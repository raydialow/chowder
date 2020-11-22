
#|everything is under the license LICENSE in the root of this repository
  copyright June Rana 2020|#

(import (prefix sdl2 "sdl:") (prefix sdl2-image "img:") coops (chicken random))

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
(define (slot-get object slotname) (slot-value object slotname))
(define (slot-set! object slotname value) (set! (slot-get object slotname) value))

#| math utilities |#
(define (^2 b) (expt b 2.0))
(define (vec-x v) (vector-ref v 0))
(define (vec-y v) (vector-ref v 1))
(define (matrix r0 r1) (vector (vec-x r0) (vec-y r0) (vec-x r1) (vec-y r1)))
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
  (let ((col1 (map (lambda (x) (* x (vec-x v))) (vector->list (matrix-col-ref M 0))))
		(col2 (map (lambda (y) (* y (vec-y v))) (vector->list (matrix-col-ref M 1)))))
	(vector (+ (car col1) (car col2)) (+ (cadr col1) (cadr col2)))))
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
(define-class <named> () ((id)))
(define-method (get-id (obj <named>)) (slot-get obj 'id))
(define-method (set-id! (obj <named>) sym) (if (symbol? sym) (slot-set! obj 'id sym) '()))

(define-class <image> () ((from-rect)(to-rect)(surface)))
(define-method (get-from-rect (img <image>)) (slot-get img 'from-rect))
(define-method (get-to-rect (img <image>)) (slot-get img 'to-rect))
(define-method (get-surface (img <image>)) (slot-get img 'surface))
(define-method (load-image! (img <image>) path)
  (slot-set! img 'surface (img:load path))
  (slot-set! img 'from-rect (sdl:make-rect 0 0
										   (sdl:surface-w (slot-get img 'surface))
										   (sdl:surface-h (slot-get img 'surface))))
  (slot-set! img 'to-rect (sdl:make-rect 0 0
										 (sdl:surface-w (slot-get img 'surface))
										 (sdl:surface-h (slot-get img 'surface))))
  img)
(define-method (draw-image! (img <image>) to-surface) (sdl:blit-surface! (get-surface img)
																		 (get-from-rect img)
																		 to-surface
																		 (get-to-rect img)))
(define-method (move-image! (img <image>) vec)
  (sdl:rect-move! (slot-get img 'to-rect) (vec-x vec) (vec-y vec)))

(define-class <sprite> (<named>) ((keys (vector))(pick 0)(animode 'oneshot)));modes are 'static, 'oneshot, 'pingpong, or 'random
(define-method (get-sprite-pick (sprt <sprite>)) (abs (slot-get sprt 'key)))
(define-method (load-sprite! (sprt <sprite>) . paths)
  (letrec ((load-images! (lambda (paths) (if (empty? paths)
											 '()
											 (let ((img (make <image>)))
											   (load-image! img (car path))
											   (cons img (load-images! (cdr path))))))))
	(slot-set! sprt (list->vector (load-images! paths)))))
(define-method (draw-sprite! (sprt <sprite>) to-surface)
  (draw-image! (vector-ref (slot-get sprt 'keys) (get-sprite-pick sprt))) to-surface))
(define-method (move-sprite! (sprt <sprite>) vec)
  (do ((idx 0 (+ idx 1)) (keys (slot-get sprt 'keys)))
	  ((>= idx (vector-length keys)) '())
	(move-image! (vector-ref keys idx) vec)))
(define-method (animate-sprite! (sprt <sprite>))
  (slot-set! sprt 'pick
			 (let ((mode (slot-get sprt 'animode)))
			   (cond ((eqv? mode 'static) 0)
					 ((eqv? mode 'oneshot)
					  (let ((old-pick (get-sprite-pick sprt))
							(max-pick (- (vector-length (slot-get sprt 'keys)) 1)))
						(if (eqv? old-pick max-pick)
							0
							(+ old-pick 1))))
					 ((eqv? mode 'pingpong)
					  (let ((old-pick (slot-get sprt 'pick))
							(max-pick (- (vector-length (slot-get sprt 'keys)) 1)))
						(if (< old-pick 0)
							(if (eqv? old-pick (* -1 max-pick))
								(- max-pick 1)
								(- old-pick 1)))))
					 ((eqv? mode 'random)
					  (pseudo-random-integer (vector-length (slot-get sprt 'keys))))
					 (else 0))))

(define-class <tile> (<sprite>) ((collide? #f)))
(define-method (collides? (tile <tile>)) (slot-get tile 'collide?))
(define-method (collision-set! (tile <tile>) value) (slot-set! tile 'collide? value))

#| initialization block |#
(sdl:set-main-ready!)
(sdl:init!)
(img:init! '(png))
(define main-window (apply sdl:create-window! main-window-args))
(define main-renderer (sdl:create-renderer! main-window -1 '(accelerated)))
(define main-surface (sdl:make-surface* (car (main-window-dimens)) (cdr (main-window-dimens)) 32))
(define main-texture '())
(define object-list '())

#| main loop block |#
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
