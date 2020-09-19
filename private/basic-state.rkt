#lang racket/base

#|

    Copyright 2020 June Sage Rana

    This program is free software: you can redistribute it and/or modify
    it under the terms of the fuck around and find out license v0.1 as
    published in this program.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

    You should have received a copy of the the fuck around and find out
    license v0.1 along with this program.  If not, see
    <https://paste.sr.ht/blob/d581b82a39d6f36f2f4c541785cee349b2549699>.

|#

; todo: validation layers and device extensions

(require racket/hash
         racket/sequence
         ffi/unsafe
         vulkan/unsafe
         ; require custom sdl2 to correct arity issue in
         ; SDL_Vulkan_GetInstanceExtensions
         ; see: https://github.com/lockie/racket-sdl2/issues/3.
         ; AND _VkInstance is already provided by vulkan/unsafe.
         (except-in sdl2 _VkInstance)
         "allocators.rkt"
         "config.rkt"
         "utilities.rkt")

(provide setup-vulkan shutdown-vulkan)

#|

VULKAN STRUCTURE UTILS

|#

; application information is defined here, returns a VkApplicationInfo cstruct
(define (make-application-info)
  (make-VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                          #f ;pNext
                          #"Chowder SDK" ;pApplicationName
                          (VK_MAKE_VERSION 0 0 0) ;applicationVersion
                          #"Chowder" ;pEngineName
                          (VK_MAKE_VERSION 0 0 0) ;engineVersion
                          (VK_MAKE_VERSION 1 1 0))) ;apiVersion

(define (queue-priority-arr-ptr length)
  (let ([ret (A*-float length)])
    (for ([iter (in-range length)])
      (ptr-set! (ptr-add ret iter _float) _float 1.0))
    ret))

; queue create info structure
(define (gen-queue-create-info index count)
  (make-VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                #f ;pNext
                                0 ;flags
                                index
                                count
                                (queue-priority-arr-ptr count)))

; device create info structure
; TODO, accept device validation layers
(define (gen-log-dev-create-info q-create-info-count
                                 q-create-info-arr-ptr
                                 dev-feats-ptr)
  (make-VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
                           #f
                           0
                           q-create-info-count
                           q-create-info-arr-ptr
                           0
                           #f
                           0
                           #f
                           dev-feats-ptr))

#|

VULKAN INSTANCE SET-UP 

|#

; make-window-ptr initializes sdl and creates a window with vulkan support
(define (make-window-ptr)
  (begin (SDL_Init 'SDL_INIT_EVERYTHING)
         (SDL_CreateWindow "Chowder"
                           0 0 ;window pos
                           (get-config "vfx-width")
                           (get-config "vfx-height")
                           (if (get-config "vfx-fullscreen")
                               (list 'SDL_WINDOW_VULKAN 'SDL_WINDOW_FULLSCREEN)
                               'SDL_WINDOW_VULKAN))))

; composes make-window-ptr to get vulkan extension count and names
(define (make-sdl2-window-info)
  (let ([window-ptr (make-window-ptr)]
        [extcount-ptr (A-uint32)])
    (cpointer-push-tag! extcount-ptr 'uint*)
    (SDL_Vulkan_GetInstanceExtensions window-ptr extcount-ptr #f)
    (let ([extnames-ptr (A*-string (ptr-ref extcount-ptr _uint32))])
      (SDL_Vulkan_GetInstanceExtensions window-ptr extcount-ptr extnames-ptr)
      (hash 'window window-ptr
            'extcount extcount-ptr
            'extnames extnames-ptr))))

; gathers information on available validation layers if validation layers are enabled
(define (make-validation-info enabled?)
  (if (not enabled?)
      (hash 'vlayers-count vkzero
            'vlayers-names #f)
    (let ([vlayers-count-ptr (A-uint32)])
      (vkEnumerateInstanceLayerProperties vlayers-count-ptr #f)
      (let ([vlayers-props-ptr (A*-VkLayerProperties (ptr-ref vlayers-count-ptr _uint32))]
            [vlayers-names-ptr (A*-vkextname (ptr-ref vlayers-count-ptr _uint32))])
        (vkEnumerateInstanceLayerProperties vlayers-count-ptr vlayers-props-ptr)
        (for ([vlayer-iter (in-range (ptr-ref vlayers-count-ptr _uint32))])
          (let ([vkextname-t (_array _char VK_MAX_EXTENSION_NAME_SIZE)]
                [vkextname-ptr (array-ptr
                            (VkLayerProperties-layerName (ptr-ref (ptr-add
                                                                  vlayers-props-ptr
                                                                  vlayer-iter
                                                                  _VkLayerProperties)
                                                                 _VkLayerProperties)))])
            (memcpy (ptr-add vlayers-names-ptr vlayer-iter vkextname-t)
                    vkextname-ptr VK_MAX_EXTENSION_NAME_SIZE)))
        ;;; TODO separate instance layers from device layers
        (hash 'vlayers-count vlayers-count-ptr
              'vlayers-names vlayers-names-ptr)))))

; composing procedures above, adds a VkInstanceCreateInfo cstruct to state
(define (make-inst-create-info)
  (let ([state (hash-union (hash-set (make-sdl2-window-info) 'appinfo (make-application-info))
                           (make-validation-info (get-config "vk-validation")))])
    (hash-set state 'inst-create-info
              (make-VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                                         #f ;pNext
                                         0 ;flags
                                         (hash-ref state 'appinfo) ;pApplicationInfo
                                         (ptr-ref (hash-ref state 'vlayers-count)
                                                  _uint32) ;enabledLayerCount
                                         (hash-ref state 'vlayers-names) ;ppEnabledLayerNames
                                         (ptr-ref (hash-ref state 'extcount)
                                                  _uint32) ;enabledExtensionCount
                                         (hash-ref state 'extnames)))))
  

; composes procedures above to add a vkinstance to the state
(define (make-instance)
  (let* ([vkinst-ptr (A-VkInstance)]
         [state (make-inst-create-info)])
    (let ([vkresult-createinst  (vkCreateInstance (hash-ref state 'inst-create-info) #f vkinst-ptr)])
      (unless (= VK_SUCCESS vkresult-createinst)
        (error 'vkCreateInstance "failed with code ~a: ~a" vkresult-createinst
               (case vkresult-createinst
                 [(VK_ERROR_OUT_OF_HOST_MEMORY) 'VK_ERROR_OUT_OF_HOST_MEMORY]
                 [(VK_ERROR_OUT_OF_DEVICE_MEMORY) 'VK_ERROR_OUT_OF_DEVICE_MEMORY]
                 [(VK_ERROR_INITIALIZATION_FAILED) 'VK_ERROR_INITIALIZATION_FAILED]
                 [(VK_ERROR_LAYER_NOT_PRESENT) 'VK_ERROR_LAYER_NOT_PRESENT]
                 [(VK_ERROR_EXTENSION_NOT_PRESENT) 'VK_ERROR_EXTENSION_NOT_PRESENT]
                 [(VK_ERROR_INCOMPATIBLE_DRIVER) 'VK_ERROR_INCOMPATIBLE_DRIVER]
                 [else 'SOMETHING_ELSE?]))))
    (cpointer-push-tag! vkinst-ptr 'VkInstance_T)
    (hash-set state 'vkinst vkinst-ptr)))

; free in reverse order
(define (free-instance instdata)
  (let ([vkinst (hash-ref instdata 'vkinst)])
    (vkDestroyInstance (ptr-ref (hash-ref instdata 'vkinst) _VkInstance) #f)
    (SDL_DestroyWindow (hash-ref instdata 'window))
    (SDL_Quit)))


#|

PHYSICAL DEVICE SET-UP

(composes vulkan instance set-up procedures)

|#

; Grabs one physical device. The first one by default.
(define (get-phys-dev)
  (let* ([state (make-instance)]
         [vkinstance (ptr-ref (hash-ref state 'vkinst) _VkInstance)]
         [devcount-ptr (A-uint32)])
    ; initialize devcount bc vkEnumeratePhysicalDevices
    ; will not set to zero if devices are not found
    (ptr-set! devcount-ptr _uint32 0) 
    (vkEnumeratePhysicalDevices vkinstance devcount-ptr #f)
    ; if there are no suitable devices, shut down
    (unless (positive? (ptr-ref devcount-ptr _uint32))
      (error "Found no suitable graphics cards. Shutting down."))
    (let* ([devcount (ptr-ref devcount-ptr _uint32)]
           [phys-devices-ptr (A*-VkPhysicalDevice devcount)])
      ; grabbing handles for each physical device
      (vkEnumeratePhysicalDevices vkinstance devcount-ptr phys-devices-ptr)
      (let ([phys-devices-props-ptr (A*-VkPhysicalDeviceProperties devcount)])
        (for ([iter-dev (in-range devcount)])
          ; getting properties of each phys dev
          (vkGetPhysicalDeviceProperties
           (ptr-ref (ptr-add phys-devices-ptr
                             iter-dev
                             _VkPhysicalDevice) _VkPhysicalDevice)
           (ptr-add phys-devices-props-ptr
                    iter-dev
                    _VkPhysicalDeviceProperties)))
        ; if this is a different list than stored in config, apply new config
        (let ([vfx-dev-avail
               (build-list (ptr-ref devcount-ptr _uint32)
                           (λ (n) (get-device-name
                                   (VkPhysicalDeviceProperties-deviceName 
                                    (ptr-ref (ptr-add phys-devices-props-ptr
                                                      n
                                                      _VkPhysicalDeviceProperties)
                                             _VkPhysicalDeviceProperties)))))])
          (if (equal? (get-config "vfx-dev-avail") vfx-dev-avail)
              null
              (set-config! "vfx-dev-avail" vfx-dev-avail)))
        ; ... making sure to keep the physical device we want ...
        (let ([phys-dev-ptr (A-VkPhysicalDevice)])
          (ptr-set! phys-dev-ptr
                    _VkPhysicalDevice
                    (ptr-ref (ptr-add phys-devices-ptr
                                     (get-config "vfx-dev-sel")
                                     _VkPhysicalDevice)
                             _VkPhysicalDevice))
          ; return new state with selected physical device
          (hash-set state 'phys-dev phys-dev-ptr))))))

#|

LOGICAL DEVICE AND QUEUE SET-UP

(composes physical device set-up procedures)

|#

(define (extract-queues log-dev-create-info)
  (letrec ([qfamcount (VkDeviceCreateInfo-queueCreateInfoCount log-dev-create-info)]
           [qfamcrinfosptr (VkDeviceCreateInfo-pQueueCreateInfos log-dev-create-info)]
           [qcounts (λ (qfamrange-lst)
                      (if (zero? (length qfamrange-lst))
                          '()
                          (cons (VkDeviceQueueCreateInfo-queueCount
                                 (ptr-ref (ptr-add qfamcrinfosptr
                                                   (car qfamrange-lst)
                                                   _VkDeviceQueueCreateInfo)
                                          _VkDeviceQueueCreateInfo))
                                (qcounts (cdr qfamrange-lst)))))])
    (letrec ([qcounts-lst (qcounts (sequence->list (in-range qfamcount)))]
             [queues-alloc (λ (qcount-lst)
                             (if (zero? (length qcount-lst))
                                 '()
                                 (cons (A-VkQueue)
                                       (queues-alloc (cdr qcount-lst)))))])
      (define queues (queues-alloc (sequence->list (in-range (apply + qcounts-lst)))))
      ; cannot bind queues to these pointers here.
      (values queues qcounts-lst)))) 

(define (q-count queue-flags)
  (letrec ([flag-lst (λ (q-flags [lst '()])
                       (if (zero? queue-flags)
                           lst
                           (if (>= q-flags 16)
                               (flag-lst (- q-flags 16)
                                         (cons 'queue-protected lst))
                               (if (>= q-flags 8)
                                   (flag-lst (- q-flags 8)
                                             (cons 'queue-sparse-binding lst))
                                   (if (>= q-flags 4)
                                       (flag-lst (- q-flags 4)
                                                 (cons 'transfer lst))
                                       (if (>= q-flags 2)
                                           (flag-lst (- q-flags 2)
                                                     (cons 'compute lst))
                                           (if (>= q-flags 1)
                                               (flag-lst (- q-flags 1)
                                                         (cons 'graphics lst))
                                               lst)))))))])
    (let ([qf (flag-lst queue-flags)])
      (if (and (member 'compute qf) (member 'graphics qf))
          4 ; versatile queue families have more queues
          (if (or (member 'compute qf) (member 'graphics qf))
              2 ; exclusive queue families have fewer queues
              1))))) ; if queue family doesn't have compute graphics or transfer, no queues

; composes the above to add a physical device and handles to its queues to the state
(define (make-log-dev)
  ; grab number of queue families and supported features on selected device 
  (let* ([state (get-phys-dev)]
         [phys-dev (ptr-ref (hash-ref state 'phys-dev) _VkPhysicalDevice)]
         [queue-family-count-ptr (A-uint32)] ;saved in state
         [phys-dev-feats-ptr (A-VkPhysicalDeviceFeatures)]) ;saved in state
    (vkGetPhysicalDeviceQueueFamilyProperties phys-dev queue-family-count-ptr #f)
    (vkGetPhysicalDeviceFeatures phys-dev phys-dev-feats-ptr)
    ; grab properties of queue families on selected device
    (let* ([queue-family-count (ptr-ref queue-family-count-ptr _uint32)]
           [queue-families-props-ptr (A*-VkQueueFamilyProperties queue-family-count)]
           ; queue create info structures allocated here
           [queue-create-infos-ptr (A*-VkDeviceQueueCreateInfo queue-family-count)]) ;saved in state
      (vkGetPhysicalDeviceQueueFamilyProperties phys-dev
                                                queue-family-count-ptr
                                                queue-families-props-ptr)
      ; set up number of queues and priorities
      ; queue families have same priority, and none will have more than 4 queues
      (for ([queue-fam-iter (in-range queue-family-count)])
        ; determine appropo queue count
        (let* ([q-flags
                (VkQueueFamilyProperties-queueFlags
                 (ptr-ref (ptr-add queue-families-props-ptr
                                   queue-fam-iter
                                   _VkQueueFamilyProperties)
                          _VkQueueFamilyProperties))]
               [count (q-count q-flags)])
          ; create queue create info structure for queue family
          (ptr-set! (ptr-add queue-create-infos-ptr queue-fam-iter _VkDeviceQueueCreateInfo)
                    _VkDeviceQueueCreateInfo
                    (gen-queue-create-info queue-fam-iter count))))
        (let ([log-dev-create-info-ptr (A-VkDeviceCreateInfo)] ;saved in state
              [log-dev-ptr (A-VkDevice)]) ;saved in state 
          (cpointer-push-tag! queue-create-infos-ptr 'VkDeviceQueueCreateInfo)
          (cpointer-push-tag! phys-dev-feats-ptr 'VkPhysicalDeviceFeatures)
          (ptr-set! log-dev-create-info-ptr
                    _VkDeviceCreateInfo
                    (gen-log-dev-create-info queue-family-count
                                             queue-create-infos-ptr
                                             phys-dev-feats-ptr))
          (let ([vkresult-createdev (vkCreateDevice phys-dev log-dev-create-info-ptr #f log-dev-ptr)])
            (unless (= VK_SUCCESS vkresult-createdev)
              (error 'vkCreateDevice "fails with code ~a: ~a."
                     vkresult-createdev
                     (case vkresult-createdev
                       [(VK_ERROR_OUT_OF_HOST_MEMORY) 'VK_ERROR_OUT_OF_HOST_MEMORY]
                       [(VK_ERROR_OUT_OF_DEVICE_MEMORY) 'VK_ERROR_OUT_OF_DEVICE_MEMORY]
                       [(VK_ERROR_INITIALIZATION_FAILED) 'VK_ERROR_INITIALIZATION_FAILED]
                       [(VK_ERROR_EXTENSION_NOT_PRESENT) 'VK_ERROR_EXTENSION_NOT_PRESENT]
                       [(VK_ERROR_FEATURE_NOT_PRESENT) 'VK_ERROR_FEATURE_NOT_PRESENT]
                       [(VK_ERROR_TOO_MANY_OBJECTS) 'VK_ERROR_TOO_MANY_OBJECTS]
                       [(VK_ERROR_DEVICE_LOST) 'VK_ERROR_DEVICE_LOST]
                       [else 'SOMETHING_ELSE?]))))
          (cpointer-push-tag! log-dev-ptr 'VkDevice_T)
          ; queues are presently allocated but not assigned
          (define-values (queues qcounts) (extract-queues (ptr-ref log-dev-create-info-ptr
                                                                   _VkDeviceCreateInfo)))
          (for ([q-binding-args
                 (for/list ([q-ref (for*/list ([qfam-iter (in-range (length qcounts))]
                                               [q-iter (in-range (list-ref qcounts qfam-iter))])
                                     (cons qfam-iter (cons q-iter '())))]
                            [q queues])
                   (cons q q-ref))])
            (vkGetDeviceQueue (ptr-ref log-dev-ptr _VkDevice)
                              (list-ref q-binding-args 1)
                              (list-ref q-binding-args 2)
                              (list-ref q-binding-args 0)))
          (hash-union state
                      (hash 'log-dev log-dev-ptr
                            'log-dev-create-info log-dev-create-info-ptr
                            'qfam-create-info queue-create-infos-ptr
                            'phys-dev-feats phys-dev-feats-ptr
                            'qfam-count queue-family-count-ptr
                            'queues queues
                            'qcounts qcounts))))))

#|

provided set-up and shutdown procedure

|#

(define (setup-vulkan) (make-log-dev))
(define (shutdown-vulkan state) (free-instance state))
