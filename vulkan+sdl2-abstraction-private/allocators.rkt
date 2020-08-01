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

(define (A-uint32) ((A _uint32)))
(define (A-VkInstance) ((A _VkInstance)))
(define (A-VkPhysicalDevice) ((A _VkPhysicalDevice)))
(define (A-VkPhysicalDeviceFeatures) ((A _VkPhysicalDeviceFeatures)))
(define (A-float) ((A _float)))
(define (A-VkDeviceCreateInfo) ((A _VkDeviceCreateInfo)))
(define (A-VkDevice) ((A _VkDevice)))

; array allocators
(define (A* ctype length) ((allocator D)
                           (λ () (malloc 'raw (_array ctype length)))))

(define (A*-string length) ((A* _string length)))
(define (A*-VkPhysicalDevice length) ((A* _VkPhysicalDevice length)))
(define (A*-VkPhysicalDeviceProperties length) ((A* _VkPhysicalDeviceProperties length)))
(define (A*-VkQueueFamilyProperties length) ((A* _VkQueueFamilyProperties length)))
(define (A*-VkDeviceQueueCreateInfo length) ((A* _VkDeviceQueueCreateInfo length)))