#lang racket

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
