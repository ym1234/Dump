#!/usr/bin/env racket
#lang racket/gui

;; TODO(ym): Fix this mess when you actually learn racket properly lol
;; UHHHH why did i use this instead of the old graphics api?
;; https://docs.racket-lang.org/graphics/index.html

(struct vec2d (x y) #:transparent #:mutable)

(define frame (new frame% [label "Pong"] [width 500] [height 500] ))
(define pong%
  (class canvas%

    (define-values (players keys circle-dir circle)
	  (values (vector 30 30)
			  (mutable-set)
			  (vec2d 10 10)
			  (vec2d 0 0)))

    (super-new [min-height 500] [min-width 500])
	(inherit get-dc set-canvas-background get-height get-width refresh-now)

    (send* (get-dc)
	  (set-brush "white" 'solid)
      (set-pen (new pen% [style 'transparent]))
      (set-smoothing 'aligned))
	(set-canvas-background (make-object color% 0 0 0))

	;; Super inefficient lol
    (define (move-player player y)
		(vector-set! players player
					 (modulo (+ (vector-ref players player) y) (get-height))))

    ;; TODO(ym): actually do the collisions lol
    (define/public (update)
	  ;; Kinda wasteful
	  (for ([key (in-set keys)])
		(match key
		  ['up   (move-player 0 -10)]
		  ['down (move-player 0  10)]
		  [#\k   (move-player 1 -10)]
		  [#\j   (move-player 1  10)]))
      (set-vec2d-x! circle (+ (vec2d-x circle) (vec2d-x circle-dir)))
      (set-vec2d-y! circle (+ (vec2d-y circle) (vec2d-y circle-dir)))
	  (check-ball)
      (refresh-now))

	;; Wew this looks awful
	(define/private (check-ball)
	  (cond
		[(<= (vec2d-x circle) 0) (set-vec2d-x! circle-dir (- (vec2d-x circle-dir))) (set-vec2d-x! circle 0)]
		[(>= (vec2d-x circle) (- (get-width) 20)) (set-vec2d-x! circle-dir (- (vec2d-x circle-dir))) (set-vec2d-x! circle (- (get-width) 50))]
		[(<= (vec2d-y circle) 0) (set-vec2d-y! circle-dir (- (vec2d-y circle-dir)))(set-vec2d-y! circle 0)]
		[(>= (vec2d-y circle) (- (get-height) 20)) (set-vec2d-y! circle-dir (- (vec2d-y circle-dir))) (set-vec2d-y! circle (- (get-height) 50))]))

    (define/override (on-size width height)
      (move-player 0 0)
      (move-player 1 0))

    (define/override (on-char event)
	  (define key-code (send event get-key-code))
      (match key-code
        [(or 'up 'down #\k #\j)  (set-add! keys (send event get-key-code))]
        ['release (set-remove! keys (send event get-key-release-code))]
        [(or 'escape #\q) (exit)]
        [else void]))

    (define/override (on-paint)
      (send* (get-dc)
	    (draw-ellipse (vec2d-x circle) (vec2d-y circle) 20 20)
	    (draw-rectangle 30 (vector-ref players 0) 10 100)
	    (draw-rectangle (- (get-width) 30) (vector-ref players 1) 10 100)))))

(define pong (new pong% [parent frame]))
(send frame show #t)

;; Not sure if this is the right way // Pretty sure this isn't the right way
(thread
 (lambda ()
   (define (game-loop)
     (send pong update)
     (sleep 0.016)
     (game-loop))
   (game-loop)))
