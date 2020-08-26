#lang typed/racket

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

(require/typed yaml
  [read-yaml (->* () (Input-Port #:allow-undefined? Boolean) (HashTable Any Any))]
  [write-yaml (->* ((HashTable Any Any))
                   (Output-Port #:canonical? Boolean
                                #:indent Positive-Integer
                                #:width Positive-Integer
                                #:explicit-start? Boolean
                                #:explicit-end? Boolean
                                #:scalar-style (U #\" #\' #\| #\> 'plain)
                                #:style (U 'block 'flow 'best)
                                #:sort-mapping (Option (-> Any Any Any))
                                #:sort-mapping-key (-> Any Any))
                   Void)])

(provide (all-defined-out))

(: load-config (-> (HashTable Any Any)))
(define (load-config)
  (with-input-from-file "config.yaml" read-yaml))

(: save-config (-> (HashTable Any Any) Void))
(define (save-config config)
  (with-output-to-file "config.yaml" #:exists 'replace
    (λ () (write-yaml config))))

(: current-config (HashTable Any Any))
(define current-config (cast (load-config) (HashTable Any Any)))

(: get-config (-> Any Any))
(define (get-config key-string)
  (hash-ref current-config key-string))

(: set-config! (-> Any Any Void))
(define (set-config! key-string value)
    (hash-set! current-config key-string value)
    (save-config current-config))
