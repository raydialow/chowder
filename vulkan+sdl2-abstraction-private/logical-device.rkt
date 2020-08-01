#lang racket

#|

    Copyright 2020 June Sage Rana

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

|#

(require racket/hash
         ffi/unsafe
         vulkan/unsafe
         "allocators.rkt"
         "instance.rkt"
         "physical-device.rkt")

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
      (display (format "Found a queue family with these flags: ~a \n" qf)) 
      (if (and (member 'compute qf) (member 'graphics qf) (member 'transfer qf))
          4 ; versatile queue families have more queues
          (if (or (member 'compute qf) (member 'graphics qf) (member 'transfer qf))
              2 ; exclusive queue families have fewer queues
              0)))))

(define (gen-queue-create-info index count priority)
  (make-VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                #f ;pNext
                                0 ;flags
                                index
                                count
                                priority))

(define (gen-log-dev-create-info q-create-info-count
                                 q-create-info-arr-ptr
                                 ext-count
                                 ext-names-arr-ptr
                                 dev-feats-ptr)
  (make-VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
                      #f
                      0
                      q-create-info-count
                      q-create-info-arr-ptr
                      0
                      #f
                      ext-count
                      ext-names-arr-ptr
                      dev-feats-ptr))

;this procedure assumes state is a valid state with a physical device in it
(define (make-log-dev state)
  ; grab number of queue families and supported features on selected device 
  (let ([phys-dev (ptr-ref (hash-ref state 'phys-dev) _VkPhysicalDevice)]
        [queue-family-count-ptr (A-uint32)] ;saved in state
        [phys-dev-feats-ptr (A-VkPhysicalDeviceFeatures)]) ;saved in state
    (vkGetPhysicalDeviceQueueFamilyProperties phys-dev queue-family-count-ptr #f)
    (vkGetPhysicalDeviceFeatures phys-dev phys-dev-feats-ptr)
    ; grab properties of queue families on selected device
    (let* ([queue-family-count (ptr-ref queue-family-count-ptr _uint32)]
           [queue-families-props-ptr (A*-VkQueueFamilyProperties queue-family-count)]
           ; queue create info structures allocated here
           [queue-create-infos-ptr (A*-VkDeviceQueueCreateInfo queue-family-count)] ;saved in state
           [queue-families-priorities-ptr (A-float)]) ;saved in state
      (vkGetPhysicalDeviceQueueFamilyProperties phys-dev
                                                queue-family-count-ptr
                                                queue-families-props-ptr)
      ; set up number of queues and priorities
      ; queue families have same priority...
      (ptr-set! queue-families-priorities-ptr _float 1.0)
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
                    (gen-queue-create-info queue-fam-iter
                                           count
                                           queue-families-priorities-ptr))
          (display (format "A queue family with queue flag ~b will get ~a queues.\n"
                           q-flags count))))
        (let ([extcount (ptr-ref (hash-ref state 'extcount) _uint32)]
              [extnames (hash-ref state 'extnames)]
              [log-dev-create-info-ptr (A-VkDeviceCreateInfo)] ;saved in state
              [log-dev-ptr (A-VkDevice)]) ;saved in state
          (cpointer-push-tag! queue-create-infos-ptr 'VkDeviceQueueCreateInfo)
          (cpointer-push-tag! phys-dev-feats-ptr 'VkPhysicalDeviceFeatures)
          (ptr-set! log-dev-create-info-ptr
                    _VkDeviceCreateInfo
                    (gen-log-dev-create-info queue-family-count
                                             queue-create-infos-ptr
                                             extcount
                                             extnames
                                             phys-dev-feats-ptr))
          (vkCreateDevice phys-dev log-dev-create-info-ptr #f log-dev-ptr)
          (hash-union state
                      (hash 'log-dev log-dev-ptr
                            'log-dev-create-info log-dev-create-info-ptr
                            'qfam-priority queue-families-priorities-ptr
                            'qfam-create-info queue-create-infos-ptr
                            'phys-dev-feats phys-dev-feats-ptr
                            'qfam-count queue-family-count-ptr))))))
        ; TODO pass queue family info array ptr to create logical device
        ; keep new logical device and whatever else needs to be preserved
        ; free everything else!

;test
(define state (make-log-dev (get-phys-dev (make-instance))))
(free-instance state)