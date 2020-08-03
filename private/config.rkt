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

(require yaml)

(provide (all-defined-out))

(define (load-config)
  (with-input-from-file "config.yaml" read-yaml))

(define (save-config config)
  (with-output-to-file "config.yaml" #:exists 'replace
    (λ () (write-yaml config))))

(define current-config (load-config))

(define (get-config key-string)
  (hash-ref current-config key-string))

(define (set-config! key-string value)
  (begin
    (hash-set! current-config key-string value)
    (save-config current-config)))