#lang typed/racket

(require "typed-ffi-unsafe.rkt")

(ffi-lib (string->path "libvulkan"))
