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

(require "typed-ffi-unsafe.rkt")

(require/typed/provide sdl2
                       ;; Basic Initialization and Shutdown
                       [SDL_Init (-> Number Number)]
                       [SDL_Quit (-> Void)]
                       ;; Hint Getter + Setter
                       [SDL_GetHint (-> String String)]
                       [SDL_GetHintBoolean (-> String Boolean Boolean)]
                       [SDL_SetHint (-> String String Boolean)]
                       ;; Error Getter
                       [SDL_GetError (-> String)]
                       ;; Window Management
                       [SDL_CreateWindow (-> String Integer Integer Integer Integer Number CPointer)]
                       )