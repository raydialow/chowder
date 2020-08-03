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
         "allocators.rkt")

(provide (all-defined-out))

#|

IMMUTABLE HASH UTILS

|#

; removes multiple key-val pairs from an immutable hash recursively
(define (hash-remove* hash . keys)
  (letrec ([remover (λ (hash lst)
                      (if (zero? (length lst))
                          hash
                          (remover (hash-remove hash (car lst)) (cdr lst))))])
    (remover hash keys)))

#|

FORIEGN CHAR ARRAY UTILS

|#

; makes string from extension name char array
(define (printable-carray carray)
  (string-trim 
   (build-string VK_MAX_EXTENSION_NAME_SIZE
                 (λ (n)
                   (let ([ch (integer->char (array-ref carray n))])
                     (if (char-graphic? ch)
                         ch
                         #\space))))))

; makes string from device name char array
(define (get-device-name carray)
  (string-trim 
   (build-string VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                 (λ (n)
                   (let ([ch (integer->char (array-ref carray n))])
                     (if (char-graphic? ch)
                         ch
                         #\space))))))

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
                          (VK_MAKE_VERSION 1 0 0) ;apiVersion
                          ))

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

