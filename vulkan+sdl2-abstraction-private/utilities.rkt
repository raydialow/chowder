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

(require ffi/unsafe)

(provide (all-defined-out))

(define (hash-remove* hash . keys)
  (letrec ([remover (λ (hash lst)
                      (if (zero? (length lst))
                          hash
                          (remover (hash-remove hash (car lst)) (cdr lst))))])
    (remover hash keys)))

(define (free* . cpointers)
  (display (format "~a" (cdr cpointers))))