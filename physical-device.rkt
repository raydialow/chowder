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
         "chowder-config.rkt"
         "instance.rkt")

(provide get-phys-dev)

(define (get-device-name carray)
  (string-trim 
   (build-string VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                 (λ (n)
                   (let ([ch (integer->char (array-ref carray n))])
                     (if (char-graphic? ch)
                         ch
                         #\space))))))

; Grabs one physical device. The first one by default.
; this procedure assumes it has been passed a state created by make-instance
(define (get-phys-dev state)
  (let ([vkinstance (ptr-ref (hash-ref state 'vkinst) _VkInstance)]
        [devcount-ptr (malloc 'raw _uint32)])
    ; initialize devcount bc vkEnumeratePhysicalDevices
    ; will not set to zero if devices are not found
    (ptr-set! devcount-ptr _uint32 0) 
    (vkEnumeratePhysicalDevices vkinstance devcount-ptr #f)
    (display (format "Found ~a physical devices.\n"
                     (ptr-ref devcount-ptr _uint32)))
    ; if there are no suitable devices, shut down
    (unless (positive? (ptr-ref devcount-ptr _uint32))
      (begin (display "Found no suitable graphics cards. Shutting down.")
             (free-instance state)))
    (let* ([devcount
            (ptr-ref devcount-ptr _uint32)]
           [phys-devices-ptr
            (malloc 'raw (_array _VkPhysicalDevice devcount))])
      ; grabbing handles for each physical device
      (vkEnumeratePhysicalDevices vkinstance devcount-ptr phys-devices-ptr)
      (let ([phys-devices-props-ptr
             (malloc 'raw (_array _VkPhysicalDeviceProperties devcount))])
        (for ([iter-dev (in-range devcount)])
          ; getting properties of each phys dev
          (vkGetPhysicalDeviceProperties
           (ptr-ref (ptr-add phys-devices-ptr
                             iter-dev
                             _VkPhysicalDevice) _VkPhysicalDevice)
           (ptr-add phys-devices-props-ptr
                    iter-dev
                    _VkPhysicalDeviceProperties))
          (display (format "Found physical device called: ~a.\n"
                           (get-device-name
                            (VkPhysicalDeviceProperties-deviceName 
                             (ptr-ref (ptr-add phys-devices-props-ptr
                                               iter-dev
                                               _VkPhysicalDeviceProperties)
                                      _VkPhysicalDeviceProperties))))))
        ; if this is a different list than stored in settings, apply new setting
        (let ([vfx-dev-avail
               (build-list
                (ptr-ref devcount-ptr _uint32)
                (λ (n) (get-device-name
                        (VkPhysicalDeviceProperties-deviceName 
                         (ptr-ref (ptr-add phys-devices-props-ptr
                                           n
                                           _VkPhysicalDeviceProperties)
                                  _VkPhysicalDeviceProperties)))))])
          (if (equal? (hash-ref current-config "vfx-dev-avail") vfx-dev-avail)
              null
              (begin
                (hash-set! current-config "vfx-dev-avail" vfx-dev-avail)
                (save-config current-config))))
        ; free the memory we dont need anymore
        (free devcount-ptr)
        (free phys-devices-props-ptr)
        ; ... making sure to keep the physical device we want ...
        (let ([phys-dev-ptr (malloc 'raw _VkPhysicalDevice)])
          (ptr-set! phys-dev-ptr
                    _VkPhysicalDevice
                    (ptr-ref (ptr-add phys-devices-ptr
                                     (hash-ref current-config "vfx-dev-sel")
                                     _VkPhysicalDevice)
                             _VkPhysicalDevice))
          (free phys-devices-ptr)
          ; return new state with selected physical device
          (hash-set state 'phys-dev phys-dev-ptr))))))


