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
         "chowder-config.rkt"
         "instance.rkt"
         "physical-devices.rkt")


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

;this procedure assumes state is a valid state with a physical device in it
(define (make-queues state)
  ; grab number of queue families and supported features on selected device 
  (let ([phys-dev (ptr-ref (hash-ref state 'phys-dev) _VkPhysicalDevice)]
        [queue-family-count-ptr (malloc 'raw _uint32)]
        [phys-dev-feats-ptr (malloc 'raw _VkPhysicalDeviceFeatures)])
    (vkGetPhysicalDeviceQueueFamilyProperties phys-dev queue-family-count-ptr #f)
    (vkGetPhysicalDeviceFeatures phys-dev phys-dev-feats-ptr)
    ; grab properties of queue families on selected device
    (let* ([queue-family-count (ptr-ref queue-family-count-ptr _uint32)]
           [queue-families-props-ptr
            (malloc 'raw (_array _VkQueueFamilyProperties queue-family-count))]
           [queue-create-infos-ptr ; queue create info structures allocated here
            (malloc 'raw (_array _VkDeviceQueueCreateInfo queue-family-count))])
      (vkGetPhysicalDeviceQueueFamilyProperties phys-dev
                                                queue-family-count-ptr
                                                queue-families-props-ptr)
      ; set up number of queues and priorities
      (let ([queue-families-priorities-ptr (malloc 'raw _float)])
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
        ; TODO pass queue family info array ptr to create logical device
        ; keep new logical device and whatever else needs to be preserved
        ; free everything else!
        ))))

;test
(define state (get-phys-dev (make-instance)))
(make-queues state)
(free-instance state)