
#|everything is under the license LICENSE in the root of this repository
  copyright June Rana 2020|#

(import (prefix sdl2 "sdl:") (prefix sdl2-image "img:") coops)

#| general utilities |#
(define (^2 b) (expt b 2.0))
(define (slot-set! object slotname value) (set! (slot-value object slotname) value))

#| math utilities |#
(define (matrix r0 r1) (vector (vector-ref r0 0) (vector-ref r0 1)
								  (vector-ref r1 0) (vector-ref r1 1)))
(define (matrix-ref M r c) (vector-ref M (+ (* r 2) c)))
(define (matrix-row-ref M r) (vector (matrix-ref M r 0) (matrix-ref M r 1)))
(define (matrix-col-ref M c) (vector (matrix-ref M 0 c) (matrix-ref M 1 c)))
(define (matrix-print M) (print "[" (matrix-row-ref M 0) "]") (print "[" (matrix-row-ref M 1) "]"))
(define (matrix-scalar M s) (matrix (vector (* s (matrix-ref M 0 0)) (* s (matrix-ref M 0 1)))
									   (vector (* s (matrix-ref M 1 0)) (* s (matrix-ref M 1 1)))))
(define matrix+ (lambda M
				  (let ((matrix-add
						 (lambda (M1 M2)
						   (matrix (vector (+ (matrix-ref M1 0 0) (matrix-ref M2 0 0))
											  (+ (matrix-ref M1 0 1) (matrix-ref M2 0 1)))
									  (vector (+ (matrix-ref M1 1 0) (matrix-ref M2 1 0))
											  (+ (matrix-ref M1 1 1) (matrix-ref M2 1 1)))))))
					(foldl matrix-add (matrix #(0 0) #(0 0)) M))))
(define (dot-prod r c) (+ (* (vector-ref r 0) (vector-ref c 0)) (* (vector-ref r 1) (vector-ref c 1)))) 
(define (matrix*matrix M1 M2) (matrix (vector (dot-prod (matrix-row-ref M1 0) (matrix-col-ref M2 0))
												 (dot-prod (matrix-row-ref M1 0) (matrix-col-ref M2 1)))
										 (vector (dot-prod (matrix-row-ref M1 1) (matrix-col-ref M2 0))
												 (dot-prod (matrix-row-ref M1 1) (matrix-col-ref M2 1)))))
(define (vector*matrix v M)
  (let ((col1 (map (lambda (x) (* x (vector-ref v 0))) (vector->list (matrix-col-ref M 0))))
		(col2 (map (lambda (y) (* y (vector-ref v 1))) (vector->list (matrix-col-ref M 1)))))
	(vector (+ (car col1) (car col2)) (+ (cadr col1) (cadr col2)))))
(define (vec-stretch-x v k) (vector*matrix v (matrix (vector k 0) (vector 0 1))))
(define (vec-stretch-y v k) (vector*matrix v (matrix (vector 1 0) (vector 0 k))))
(define (vec-rotate-cw v theta) (vector*matrix v (matrix (vector (cos theta) (sin theta))
															(vector (* -1 (sin theta)) (cos theta)))))
(define (vec-rotate-ccw v theta) (vector*matrix v (matrix (vector (cos theta) (* -1 (sin theta)))
															 (vector (sin theta) (cos theta)))))
(define (vec-shear-x v k) (vector*matrix v (matrix (vector 1 k) (vector 0 1))))
(define (vec-shear-y v k) (vector*matrix v (matrix (vector 1 0) (vector k 1))))
(define (vec-reflect-o v) (vector*matrix v (matrix (vector 0 1) (vector 1 0))))
(define (vec-reflect-x v) (vector*matrix v (matrix (vector -1 0) (vector 0 1))))
(define (vec-reflect-y v) (vector*matrix v (matrix (vector 1 0) (vector 0 -1))))
(define (vec-reflect-xy v) (vector*matrix v (matrix (vector -1 0) (vector 0 -1))))
 
#| game objects |#
(define-class <object> () ((id)))
(define-method (get-id (obj <object>)) (slot-value obj 'id))
(define-method (set-id (obj <object>) sym) (if (symbol? sym) (slot-set! obj 'id sym) '()))
(define-class <asset> (<object>) ((path)))
(define-method (get-path (obj <object>)) (slot-value obj 'path))
(define-method (set-path (obj <object>) path) (if (string? path) (slot-set! obj 'path sym) '()))
(define-class <image> (<asset>) ((size)(surface)))
(define-method (load-image (img <image>) path)
  (set-path img path)
  (slot-set! img 'surface (img:load path))
  (slot-set! img 'size (vector (sdl:surface-w (slot-value img 'surface))
							   (sdl:surface-h (slot-value img 'surface))))
  img)

#| parameters |#
(define main-window-args (make-parameter (list "Chowder v0.0.0" 0 0 1280 720 '(maximized))))

#| initialization block |#
(sdl:set-main-ready!)
(sdl:init!)
(img:init!)
(define main-window (apply sdl:create-window! (main-window-args)))
(define main-renderer (sdl:create-renderer! main-window -1 '(accelerated)))
(define main-surface (sdl:make-surface* (cadddr (main-window-args)) (car (cddddr (main-window-args))) 32))
(define main-texture (sdl:create-texture-from-surface* main-renderer main-surface))

#| main SDK loop block |#
(do ((quitting #f (sdl:quit-requested?)))
	(quitting '())
  #| present step |#
  (sdl:render-clear! main-renderer)
  (sdl:render-copy! main-renderer main-texture)
  (sdl:render-present! main-renderer))

#| shutdown block |#
(sdl:destroy-texture! main-texture)
(sdl:free-surface! main-surface)
(sdl:destroy-renderer! main-renderer)
(sdl:destroy-window! main-window)
(img:quit!)
(sdl:quit!)
