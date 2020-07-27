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

(require ffi/unsafe
         vulkan/unsafe
         "instance.rkt")

(define (get-phys-devs status)
  (let ([vkinstance (ptr-ref (hash-ref status 'vkinst) _VkInstance)]
        [devcount-ptr (malloc 'raw _uint32)])
    (begin
      (vkEnumeratePhysicalDevices vkinstance devcount-ptr #f)
      (display (format "Found ~a physical devices."
                       (ptr-ref devcount-ptr _uint32)))
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
              (hash-set
               (hash-set
                (hash-set
                 (hash-set status
                           'phys-dev-count devcount-ptr)
                 'phys-devs phys-devices-ptr)
                'phys-devs-props phys-devices-props-ptr)
               'phys-devs-feats phys-devices-feats-ptr))))))))

;;test succeeds
;(free-instance (get-phys-devs (make-instance)))