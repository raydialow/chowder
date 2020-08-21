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

; returns a char array to string procedure for char arrays of size
(define ((carray->string size) carray)
  (string-trim 
   (build-string size
                 (λ (n)
                   (let ([ch (integer->char (array-ref carray n))])
                     (if (char-graphic? ch)
                         ch
                         #\space))))))

; makes string from extension name char array
(define get-extension-name
  (carray->string VK_MAX_EXTENSION_NAME_SIZE))

; makes string from device name char array
(define get-device-name
  (carray->string VK_MAX_PHYSICAL_DEVICE_NAME_SIZE))
