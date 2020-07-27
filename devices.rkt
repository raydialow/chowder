#lang racket

(require ffi/unsafe
         vulkan/unsafe
         "instance.rkt")

(define (get-phys-device-count status)
  (let ([devcount-ptr (malloc 'raw _uint32)])
    (begin
      (ptr-set! devcount-ptr _uint32 0)
      (vkEnumeratePhysicalDevices (ptr-ref (hash-ref status 'vkinst)
                                           _VkInstance)
                                  devcount-ptr #f)
      (display (format "Found ~a physical devices.\n"
                            (ptr-ref devcount-ptr _uint32)))
      (unless (not (zero? (ptr-ref devcount-ptr _uint32)))
        (free-instance status))
      (hash-set status 'phys-dev-count devcount-ptr))))

(define (get-phys-devices status)
  (let ([vkinstance (ptr-ref (hash-ref status 'vkinst) _VkInstance)]
        [devcount-ptr (malloc 'raw _uint32)])
    (begin
      (vkEnumeratePhysicalDevices vkinstance devcount-ptr #f)
      (let* ([devcount
             (ptr-ref devcount-ptr _uint32)]
            [phys-devices-ptr
             (malloc 'raw (_array _VkPhysicalDevice devcount))])
        (begin
          (vkEnumeratePhysicalDevices vkinstance devcount-ptr phys-devices-ptr)
          (let ([phys-devices-props-ptr
                 (malloc 'raw (_array _VkPhysicalDeviceProperties devcount))]
                [phys-devices-feats-ptr
                 (malloc 'raw (_array _VkPhysicalDeviceFeatures devcount))])
            (begin
              (for ([iter (in-range devcount)])
                (begin
                  (vkGetPhysicalDeviceProperties
                   (ptr-ref (ptr-add phys-devices-ptr
                                     iter
                                     _VkPhysicalDevice) _VkPhysicalDevice)
                   (ptr-add phys-devices-props-ptr
                            iter
                            _VkPhysicalDeviceProperties))
                  (vkGetPhysicalDeviceFeatures
                   (ptr-ref (ptr-add phys-devices-ptr
                                     iter
                                     _VkPhysicalDevice) _VkPhysicalDevice)
                   (ptr-add phys-devices-feats-ptr
                            iter
                            _VkPhysicalDeviceFeatures))))
              )))))))

      
#|
(define (get-phys-devices-properties status)
  (let* ([status (get-phys-devices status)]
         [phys-devices-props (malloc 'raw (_array _VkPhysicalDeviceProperties ])
    (for ([iter (in-range (ptr-ref (hash-ref status 'phys-dev-count) _uint32))])
      
    ))

(define (grab-phys-device) #f)
|#
