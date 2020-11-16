
#|everything is under the license LICENSE in the root of this repository
  copyright June Rana 2020|#

(import (prefix sdl2 "sdl:") (prefix sdl2-image "img:") coops)

#| parameters: todo these should load from a file |#
(define main-window-title (make-parameter "Innerlands v0.0.0"))
(define main-window-origin (make-parameter (cons 0 0)))
(define main-window-dimens (make-parameter (cons 1280 720)))
(define main-window-flags (make-parameter '(maximized)))
(define main-window-args (let ((title (main-window-title))
								 (xy (main-window-origin))
								 (wh (main-window-dimens))
								 (flags (main-window-flags)))
							 (list title (car xy) (cdr xy) (car wh) (cdr wh) flags)))

#| general utilities |#
(define (slot-set! object slotname value) (set! (slot-value object slotname) value))

#| math utilities |#
(define (^2 b) (expt b 2.0))
(define (matrix r0 r1) (vector (vector-ref r0 0) (vector-ref r0 1)
								  (vector-ref r1 0) (vector-ref r1 1)))
(define (matrix-ref M r c) (vector-ref M (+ (* r 2) c)))
(define (matrix-row-ref M r) (vector (matrix-ref M r 0) (matrix-ref M r 1)))
(define (matrix-col-ref M c) (vector (matrix-ref M 0 c) (matrix-ref M 1 c)))
(define (matrix-print M) (print "[" (matrix-row-ref M 0) "," (matrix-row-ref M 1) "]"))
(define (matrix-scalar M s) (matrix (vector (* s (matrix-ref M 0 0)) (* s (matrix-ref M 0 1)))
									   (vector (* s (matrix-ref M 1 0)) (* s (matrix-ref M 1 1)))))
(define matrix+matrix (lambda M
						(let ((matrix-add
							   (lambda (M1 M2)
								 (matrix (vector (+ (matrix-ref M1 0 0) (matrix-ref M2 0 0))
												 (+ (matrix-ref M1 0 1) (matrix-ref M2 0 1)))
										 (vector (+ (matrix-ref M1 1 0) (matrix-ref M2 1 0))
												 (+ (matrix-ref M1 1 1) (matrix-ref M2 1 1)))))))
						  (foldl matrix-add (matrix #(0 0) #(0 0)) M))))
(define (matrix*matrix M1 M2) (matrix (vector (vector*vector (matrix-row-ref M1 0) (matrix-col-ref M2 0))
											  (vector*vector (matrix-row-ref M1 0) (matrix-col-ref M2 1)))
									  (vector (vector*vector (matrix-row-ref M1 1) (matrix-col-ref M2 0))
											  (vector*vector (matrix-row-ref M1 1) (matrix-col-ref M2 1)))))
(define (vector*matrix v M)
  (let ((col1 (map (lambda (x) (* x (vector-ref v 0))) (vector->list (matrix-col-ref M 0))))
		(col2 (map (lambda (y) (* y (vector-ref v 1))) (vector->list (matrix-col-ref M 1)))))
	(vector (+ (car col1) (car col2)) (+ (cadr col1) (cadr col2)))))
(define (vec-x v) (vector-ref v 0))
(define (vec-y v) (vector-ref v 1))
(define (vector*scalar a v) (vector (* a (vec-x v)) (* a (vec-y v))))
(define (vector+vector a b) (vector (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))
(define (vector*vector r c) (+ (* (vec-x r) (vec-x c)) (* (vec-y r) (vec-y c))))
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
(define-method (set-id! (obj <object>) sym) (if (symbol? sym) (slot-set! obj 'id sym) '()))

(define-class <asset> (<object>) ((path)))
(define-method (get-path (obj <object>)) (slot-value obj 'path))
(define-method (set-path! (obj <object>) path) (if (string? path) (slot-set! obj 'path path) '()))

(define-class <image> (<asset>) ((vis #f)(from-rect)(to-rect)(surface)))
(define-method (get-vis (img <image>)) (slot-value img 'vis))
(define-method (set-vis! (img <image>) value) (if (boolean? value) (slot-set! img 'vis value) '()))
(define-method (get-from-rect (img <image>)) (slot-value img 'from-rect))
(define-method (get-to-rect (img <image>)) (slot-value img 'to-rect))
(define-method (get-surface (img <image>)) (slot-value img 'surface))
(define-method (load-image! (img <image>) path)
  (set-path! img path)
  (slot-set! img 'surface (img:load path))
  (slot-set! img 'from-rect (sdl:make-rect 0 0
										   (sdl:surface-w (slot-value img 'surface))
										   (sdl:surface-h (slot-value img 'surface))))
  (slot-set! img 'to-rect (sdl:make-rect 0 0
										 (sdl:surface-w (slot-value img 'surface))
										 (sdl:surface-h (slot-value img 'surface))))
  img)
(define-method (move-image! (img <image>) vec)
  (sdl:rect-move! (slot-value img 'to-rect) (vec-x vec) (vec-y vec)))


#| initialization block |#
(sdl:set-main-ready!)
(sdl:init!)
(img:init! '(png))
(define main-window (apply sdl:create-window! main-window-args))
(define main-renderer (sdl:create-renderer! main-window -1 '(accelerated)))
(define main-surface (sdl:make-surface* (car (main-window-dimens)) (cdr (main-window-dimens)) 32))
(define main-texture '())
(define object-list '())

#| testing... |#
#;(define test-image (make <image>))
#;(load-image! test-image "assets/square-color-256x256.png")
#;(set-vis! test-image #t)
#;(sdl:blit-surface! (get-surface test-image) (get-from-rect test-image) 
main-surface (get-to-rect test-image))

#| main SDK loop block |#
(do ((quitting #f (sdl:quit-requested?))
	 (ticks (sdl:get-ticks))
	 (big-delta 0))
	#| exit cases |#
	(quitting '())
  #| update loop parameters |#
  (let* ((new-ticks (sdl:get-ticks))
		 (lil-delta (- new-ticks ticks)))
	(set! ticks new-ticks)
	(set! big-delta (+ big-delta lil-delta)))
  #| executes each frame (~60fps) |#
  (if (> big-delta 16)
	  (begin (set! main-texture (sdl:create-texture-from-surface* main-renderer main-surface))
			 (sdl:render-copy! main-renderer main-texture)
			 (sdl:destroy-texture! main-texture)
			 (sdl:render-present! main-renderer)
			 (set! big-delta (- big-delta 16))))
  #| every loop |#
  '())

#| shutdown block |#
(sdl:free-surface! main-surface)
(sdl:destroy-renderer! main-renderer)
(sdl:destroy-window! main-window)
(img:quit!)
(sdl:quit!)
0
