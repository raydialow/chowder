#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         vulkan/unsafe)

(provide (all-defined-out))

; deallocator
(define D ((deallocator) (λ (v) (free v))))

; single allocators
(define (A ctype) ((allocator D)
                   (λ ()
                     (let ([ptr (malloc 'raw ctype)])
                       (cpointer-push-tag! ptr (ctype->layout ctype))
                       ptr))))

;basic types
(define (A-uint32) ((A _uint32)))
(define (A-float) ((A _float)))
;vulkan types
(define (A-VkDevice) ((A _VkDevice)))
(define (A-VkDeviceCreateInfo) ((A _VkDeviceCreateInfo)))
(define (A-VkInstance) ((A _VkInstance)))
(define (A-VkPhysicalDevice) ((A _VkPhysicalDevice)))
(define (A-VkPhysicalDeviceFeatures) ((A _VkPhysicalDeviceFeatures)))
(define (A-VkQueue) ((A _VkQueue)))

; array allocators
(define (A* ctype length) ((allocator D)
                           (λ () (malloc 'raw (_array ctype length)))))

;basic types
(define (A*-float length) ((A* _float length)))
(define (A*-string length) ((A* _string length)))
(define (A*-vkextname length) ((A* (_array _char VK_MAX_EXTENSION_NAME_SIZE) length)))
;vulkan types
(define (A*-VkDeviceQueueCreateInfo length) ((A* _VkDeviceQueueCreateInfo length)))
(define (A*-VkLayerProperties length) ((A* _VkLayerProperties length)))
(define (A*-VkPhysicalDevice length) ((A* _VkPhysicalDevice length)))
(define (A*-VkPhysicalDeviceProperties length) ((A* _VkPhysicalDeviceProperties length)))
(define (A*-VkQueue length) ((A* _VkQueue length)))
(define (A*-VkQueueFamilyProperties length) ((A* _VkQueueFamilyProperties length)))

; initializing allocators
; produces a ptr to a uint32 containing zero
(define (zero) (let ([ret (A-uint32)]) (ptr-set! ret _uint32 0) ret))
