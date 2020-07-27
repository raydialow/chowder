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
         ; require custom sdl2 to correct arity issue in
         ; SDL_Vulkan_GetInstanceExtensions
         ; see: https://github.com/lockie/racket-sdl2/issues/3.
         ; AND _VkInstance is already provided by vulkan/unsafe.
         (except-in "racket-sdl2-vlk-edition/main.rkt" _VkInstance))

(provide make-instance free-instance)

(define (make-application-info)
  (make-VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                          #f ;pNext
                          #"Chowder SDK" ;pApplicationName
                          (VK_MAKE_VERSION 0 0 0) ;applicationVersion
                          #"Chowder" ;pEngineName
                          (VK_MAKE_VERSION 0 0 0) ;engineVersion
                          (VK_MAKE_VERSION 1 2 0) ;apiVersion
                          ))

; make-window-ptr initializes sdl and creates a window with vulkan support
(define (make-window-ptr)
  (begin (SDL_Init 'SDL_INIT_EVERYTHING)
         (display "Initialized SDL2.\n")
         (SDL_CreateWindow "Chowder" 0 0 800 640 'SDL_WINDOW_VULKAN)))

; composes make-window-ptr to get vulkan extension count and names
(define (make-sdl2-window-info)
  (let ([window-ptr (make-window-ptr)]
        [extcount-ptr (malloc 'raw _uint32)])
    (begin
      (display "Created SDL2 window.\n")
      (cpointer-push-tag! extcount-ptr 'uint*)
      (SDL_Vulkan_GetInstanceExtensions window-ptr extcount-ptr #f)
      (let ([extnames-ptr (malloc 'raw
                                  (_array _string
                                          (ptr-ref extcount-ptr _uint32)))])
        (begin
          (SDL_Vulkan_GetInstanceExtensions window-ptr
                                            extcount-ptr
                                            extnames-ptr)
          (hash 'window window-ptr
                'extcount extcount-ptr
                'extnames extnames-ptr))))))

(define (make-inst-create-info)
  (let ([status (hash-set (make-sdl2-window-info)
                          'appinfo (make-application-info))])
        (hash-set
         status
         'inst-create-info
         (make-VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                                    #f ;pNext
                                    0 ;flags
                                    (hash-ref status 'appinfo) ;pApplicationInfo
                                    0 ;enabledLayerCount
                                    #f ;ppEnabledLayerNames
                                    (ptr-ref
                                     (hash-ref status 'extcount)
                                     _uint32) ;enabledExtensionCount
                                    (hash-ref status 'extnames)
                                    ;ppEnabledExtensionNames
                                    ))))
  

; composes make-proto-instance-data to create vkinstance with associated window
; returns a hash containing pointers to the required data
(define (make-instance)
  (let* ([vkinst-ptr (malloc 'raw _VkInstance)]
         [status (make-inst-create-info)])
    (begin
      (vkCreateInstance (hash-ref status 'inst-create-info) #f vkinst-ptr)
      (cpointer-push-tag! vkinst-ptr 'VkInstance_T)
      (display "Created Vulkan instance.\n")
      (hash-set status 'vkinst vkinst-ptr))))

; free everything else first!
(define (free-instance instdata)
  (let ([vkinst (hash-ref instdata 'vkinst)])
    (begin
      (vkDestroyInstance (ptr-ref (hash-ref instdata 'vkinst) _VkInstance) #f)
      (display "Freed Vulkan instance.\n")
      (SDL_DestroyWindow (hash-ref instdata 'window))
      (display "Freed SDL2 window.\n")
      (SDL_Quit)
      (display "Shutdown SDL2.\n")
      (hash-for-each instdata (λ (k v)
                                (unless (or
                                         (equal? k 'vkinst)
                                         (equal? k 'window)
                                         (equal? k 'inst-create-info)
                                         (equal? k 'appinfo))
                                  (begin (display
                                          (format "Freeing ~a.\n"
                                                  (symbol->string k)))
                                         (free v) )))))))

