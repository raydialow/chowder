#lang typed/racket

;;; Let's Type the upstream ffi module instead...

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

; ISSUES:
;  array-set! penultimate arg is variadic: unsupported afaik in require/typed!
;    EFFECTS: array-set! only. This is the only procedure with a second-to-last variadic arg.
;  CType values raise "unable to protect opaque value" deprecation warning
;    EFFECTS: All values of type CType. 
;  Procedures returning late-weak-box or late-weak-hasheq are afaik unsupported!
;    EFFECTS: make-late-weakbox and make-late-hasheq. These contracts are insufficient!
;  Interface insists make-late-hasheq takes zero arguments!
;    EFFECTS: make-late-hasheq only. This contract is insufficient.

;; Convenience Types
(define-type ObjName (U String Bytes Symbol))
(define-type Layout (U Symbol (Listof Symbol) (Vector Symbol Nonnegative-Integer)))
(define-type ABI (U False 'default 'stdcall 'sysv))
(define-type AsyncApply (U (-> (-> Any) Any) BoxTop))
(define-type SaveErrno (U 'posix 'windows))
(define-type Wrapper (-> Procedure Procedure))
(define-type Keeper (U Boolean BoxTop (-> Any Any)))
(define-type Alignment (U False 1 2 4 8 16))
(define-type MallocMode (U 'raw 'atomic 'nonatomic 'tagged 'atomic-interior
                           'interior 'stubborn 'uncollectable 'eternal))
(define-type NonZeroInteger (U Negative-Integer Positive-Integer))

(require/typed/provide ffi/unsafe
  
  ;; Opaque Types
  [#:opaque CType ctype?]
  [#:opaque FFI-Lib ffi-lib?]
  [#:opaque CPointer cpointer?]
  [#:opaque CArray array?]
  [#:opaque CUnion union?]
  [#:opaque CPointerPred cpointer-predicate-procedure?]

  ;; Loading Foreign Libraries
  [ffi-lib (->* ((Option Path))
                ((U (Option String) (Listof (Option String)))
                 #:get-lib-dirs (-> (Listof Path))
                 #:global? Any
                 #:custodian (U 'place Custodian False))
                FFI-Lib)]
  [get-ffi-obj (->* (ObjName (Option (U FFI-Lib Path)) CType)
                    ((Option (-> Any)))
                    Any)]
  [set-ffi-obj! (-> ObjName (Option (U FFI-Lib Path)) CType Any Void)]
  [make-c-parameter (-> ObjName
                        (Option (U FFI-Lib Path))
                        CType
                        (case-> [-> Any] [-> Any Void]))]
  [ffi-obj-ref (->* (ObjName (Option (U FFI-Lib Path))) ((Option (-> Any))) Any)]
  
  ;; Type Constructors
  [make-ctype (-> CType (Option (-> Any Any)) (Option (-> Any Any)) CType)]
  [ctype-sizeof (-> CType Nonnegative-Integer)]
  [ctype-alignof (-> CType Nonnegative-Integer)]
  [ctype->layout (-> CType Layout)]
  [compiler-sizeof (-> (U Symbol (Listof Symbol)) Nonnegative-Integer)]

  ;; Numeric Types
  [_int8 CType]
  [_sint8 CType]
  [_uint8 CType]
  [_int16 CType]
  [_uint16 CType]
  [_sint16 CType]
  [_int32 CType]
  [_uint32 CType]
  [_sint32 CType]
  [_int64 CType]
  [_uint64 CType]
  [_sint64 CType]
  [_byte CType]
  [_sbyte CType]
  [_ubyte CType]
  [_wchar CType]
  [_word CType]
  [_sword CType]
  [_uword CType]
  [_short CType]
  [_sshort CType]
  [_ushort CType]
  [_int CType]
  [_sint CType]
  [_uint CType]
  [_long CType]
  [_slong CType]
  [_ulong CType]
  [_llong CType]
  [_sllong CType]
  [_ullong CType]
  [_intptr CType]
  [_sintptr CType]
  [_uintptr CType]
  [_size CType]
  [_ssize CType]
  [_ptrdiff CType]
  [_intmax CType]
  [_uintmax CType]
  [_fixnum CType]
  [_ufixnum CType]
  [_fixint CType]
  [_ufixint CType]
  [_float CType]
  [_double CType]
  [_double* CType]
  [_longdouble CType]
  
  ;; Other Atomic Types
  [_stdbool CType]
  [_bool CType]
  [_void CType]
  
  ;; String Types
  [_string/ucs-4 CType]
  [_string/utf-16 CType]
  [_path CType]
  [_symbol CType]
  [_string/utf-8 CType]
  [_string/latin-1 CType]
  [_string/locale CType]
  [_string*/utf-8 CType]
  [_string*/latin-1 CType]
  [_string*/locale CType]
  [_string CType]
  [default-_string-type (Parameterof CType)]
  [_file CType]
  [_bytes/eof CType]
  [_string/eof CType]
  
  ;; Pointer Types
  [_pointer CType]
  [_gcpointer CType]
  [_racket CType]
  [_scheme CType]
  [_fpointer CType]
  [_or-null (-> CType CType)]
  [_gcable (-> CType CType)]

  ;; Function Types
  [_cprocedure (->* ((Listof CType) CType)
                    (#:abi ABI
                     #:atomic? Any
                     #:async-apply (Option AsyncApply)
                     #:lock-name (Option String)
                     #:in-original-place? Any
                     #:blocking? Any
                     #:save-errno (Option SaveErrno)
                     #:wrapper (Option Wrapper)
                     #:keep Keeper)
                    Any)]
  [function-ptr (-> (U CPointer Procedure) CType CPointer)]
  
  ;; C Struct Types
  [make-cstruct-type (-> (Listof CType) ABI Alignment MallocMode CType)]
  [_list-struct (-> Alignment MallocMode CType * CType)]
  [compute-offsets (-> (Listof CType)
                       Alignment
                       (Listof (U False Integer))
                       (Listof Integer))]
  
  ;; C Array Types
  [make-array-type (-> CType Nonnegative-Integer CType)]
  [_array (-> CType Nonnegative-Integer Nonnegative-Integer * CType)]
  [array-ref (-> CArray Nonnegative-Integer * Any)]
  [array-set! (-> CArray Nonnegative-Integer Any Any * Void)]
  [array-ptr (-> CArray CPointer)]
  [array-length (-> CArray Nonnegative-Integer)]
  [array-type (-> CArray CType)]
  [in-array (->* (CArray)
                 (Nonnegative-Integer (Option Integer) NonZeroInteger)
                 SequenceTop)]
  [_array/list (-> CType Nonnegative-Integer Nonnegative-Integer * CType)]
  [_array/vector (-> CType Nonnegative-Integer Nonnegative-Integer * CType)]
  
  ;; C Union Types
  [make-union-type (-> CType CType * CType)]
  [_union (-> CType CType * CType)]
  [union-ref (-> CUnion Nonnegative-Integer Any)]
  [union-set! (-> CUnion Nonnegative-Integer Any Void)]
  [union-ptr (-> CUnion CPointer)]
  
  ;; Enumerations and Masks
  [_enum (->* ((Listof Any)) (CType #:unknown Any) CType)]
  [_bitmask (-> Symbol (Listof Any) CType * CType)]
  
  ;; Pointer Functions
  [ptr-equal? (-> CPointer CPointer Boolean)]
  [ptr-add (->* (CPointer Integer) (CType) CPointer)]
  [offset-ptr? (-> CPointer Boolean)]
  [ptr-offset (-> CPointer Integer)]
  [cpointer-gcable? (-> CPointer Boolean)]
  [set-ptr-offset! (->* (CPointer Integer) (CType) Void)]
  [ptr-add! (->* (CPointer Integer) (CType) Void)]
  [ptr-ref (case-> [->* (CPointer CType) (Nonnegative-Integer) Any]
                   [-> CPointer CType 'abs Nonnegative-Integer Any])]
  [ptr-set! (case-> [-> CPointer CType Any Void]
                    [-> CPointer CType Nonnegative-Integer Any Void]
                    [-> CPointer CType 'abs Nonnegative-Integer Any Void])]
  [memmove (->* (CPointer CPointer Nonnegative-Integer)
                (Integer Integer CType)
                Void)]
  [memcpy (->* (CPointer CPointer Nonnegative-Integer)
               (Integer Integer CType)
               Void)]
  [memset (->* (CPointer Byte Nonnegative-Integer) (Integer CType) Void)]
  [cpointer-tag (-> CPointer Any)]
  [set-cpointer-tag! (-> CPointer Any Void)]
  [malloc (->* ((U Nonnegative-Integer CType))
               ((U Nonnegative-Integer CType) CPointer MallocMode 'failok)
               CPointer)]
  [free (-> CPointer Void)]
  [end-stubborn-change (-> CPointer Void)]
  [malloc-immobile-cell (-> Any CPointer)]
  [free-immobile-cell (-> CPointer Void)]
  [register-finalizer (-> Any (-> Any Any) Void)]
  [make-late-weak-box (-> Any Any)] ;TODO return weak box contract not supported
  [make-late-weak-hasheq (-> Any)] ; if. insists this procedure takes 0 args?
  [make-sized-byte-string (-> CPointer Nonnegative-Integer Bytes)]
  [void/reference-sink (-> Any * Void)]
  [prop:cpointer Struct-Type-Property]
  
  ;; Tagged C Pointer Types
  [_cpointer (case-> [-> Any CType]
                     [-> Any (Option CType) CType]
                     [-> Any
                         (Option CType)
                         (Option (-> Any Any))
                         (Option (-> Any Any))
                         CType])]
  [_cpointer/null (case-> [-> Any CType]
                          [-> Any (Option CType) CType]
                          [-> Any
                              (Option CType)
                              (Option (-> Any Any))
                              (Option (-> Any Any))
                              CType])]
  [cpointer-has-tag? (-> CPointer Any Boolean)]
  [cpointer-push-tag! (-> CPointer Any Void)]
  
  ;; Miscellaneous Support
  [list->cblock (->* ((Listof Any) CType)
                     ((Option Nonnegative-Integer)
                      #:malloc-mode (Option MallocMode))
                     CPointer)] 
  [vector->cblock (->* ((Vectorof Any) CType)
                       ((Option Nonnegative-Integer)
                        #:malloc-mode (Option MallocMode))
                       CPointer)] 
  [vector->cpointer (-> (Vectorof Any) CPointer)]
  [flvector->cpointer (-> FlVector CPointer)]
  [saved-errno (case-> [-> Integer] [-> Integer Void])]
  [lookup-errno (-> Symbol (Option Integer))]
  [cast (-> Any CType CType Any)]
  [cblock->list (-> Any CType Nonnegative-Integer (Listof Any))]
  [cblock->vector (-> Any CType Nonnegative-Integer (Vectorof Any))])

(require/typed/provide ffi/unsafe/define
  [make-not-available (-> Symbol (-> Any * Any))])

;;
;; Custom Typed Internals...
;;


;;
;; Syntax requires
;;
(require (only-in ffi/unsafe
                  define-c
                  define-cstruct
                  _fun
                  [-> -fun>]
                  define-fun-syntax
                  _?
                  _ptr
                  _box
                  _list
                  _vector
                  _bytes
                  _bytes/nul-terminated
                  define-cpointer-type))

(require (only-in ffi/unsafe/define
                  define-ffi-definer
                  provide-protected))

(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))


(provide (all-defined-out))

