#!/usr/bin/env racket
#lang racket/gui

;; TODO(ym): Fix this mess when you actually learn racket properly lol

(struct vector2d (x y) #:transparent #:mutable)

(define frame (new frame% [label "Pong"]))
(define pong%
  (class canvas%
	;; apparently needs to be the first thing
	(super-new)

	(define dc (send this get-dc))

	(send dc set-brush "white" 'solid)
	(send dc set-pen (new pen% [style 'transparent]))
	(send dc set-smoothing 'aligned)
	(send this set-canvas-background (make-object color% 0 0 0))

	;; For some reason changing this doesn't have any effect?
	(define player-one 10)
	(define player-two 10)
	(define circle-dir (vector2d 5 5))
	(define circle (vector2d (/ (send this get-height) 2)  (/ (send this get-width) 2)))
	(println circle)
	(define keys (make-hash))

	;; Better way to do this? macro? Box? match?
	(define (move-player-one y) (set! player-one (modulo (+ player-one y) (send this get-height))))
	(define (move-player-two y) (set! player-two (modulo (+ player-two y) (send this get-height))))

	;; uhhh
	;; aside from that, TODO(ym): actually do the collisions lol
	(define/public (update)
	  (if (hash-ref! keys 'up #f) (move-player-one -10) '())
	  (if (hash-ref! keys 'down #f) (move-player-one 10) '())
	  (if (hash-ref! keys #\t #f) (move-player-two -10) '())
	  (if (hash-ref! keys #\h #f) (move-player-two 10) '())
	  (set-vector2d-x! circle (+ (vector2d-x circle) (vector2d-x circle-dir)))
	  (set-vector2d-y! circle (+ (vector2d-y circle) (vector2d-y circle-dir)))
	  (send this refresh-now))

	;; you know the drill
	(define/override (on-size width height)
	  (set! player-one (modulo player-one height))
	  (set! player-two (modulo player-two height)))

	;; Again, need a a better way to do this
	(define/override (on-char event)
	  (match (send event get-key-code)
		['up  (hash-set! keys 'up #t)]
		['down  (hash-set! keys 'down #t)]
		[#\h  (hash-set! keys #\h  #t)]
		[#\t  (hash-set! keys #\t #t)]
		['release (hash-set! keys (send event get-key-release-code) #f)]
		['escape (exit)]
		[else void]))


	;; TODO(ym): More complex drawing, this is a one off so i don't really care, but it looks really ugly when the rects clip
	(define/override (on-paint)
	 (send dc draw-ellipse (vector2d-x circle) (vector2d-y circle) 50 50)
	 (send dc draw-rectangle 5 player-one 10 100)
	 (send dc draw-rectangle (- (send this get-width) 15) player-two 10 100))))

(define pong (new pong% [parent frame]))
(send frame show #t)

(thread
  (lambda ()
	(define (game-loop)
	  (send pong update)
	  (sleep 0.016)
	  (game-loop))
	(game-loop)))
