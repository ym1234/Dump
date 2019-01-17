#!/usr/bin/env racket
#lang racket/gui

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

	(define player-one 0)
	(define player-two 0)

	;; Better way to do this lol? macro? Box? match?
	(define (move-player-one y) (set! player-one (modulo (+ player-one y) (send this get-height))))
	(define (move-player-two y) (set! player-two (modulo (+ player-two y) (send this get-height))))

	(define/override (on-size width height)
	  (set! player-one (modulo player-one height))
	  (set! player-two (modulo player-two height))
	  (send this refresh-now))

	(define/override (on-char event)
	  (match (send event get-key-code)
		['up  (move-player-one -5)(send this refresh-now)]
		['down  (move-player-one 5) (send this refresh-now)]
		[#\h  (move-player-two 5) (send this refresh-now)]
		[#\t  (move-player-two -5) (send this refresh-now)]
		['escape (exit)]
		[else void]))

	(define/override (on-paint)
	  (println 'repaint)
	  (send dc draw-ellipse 50 50 100 100)
	  (send dc draw-rectangle 0 player-one 10 50)
	  (send dc draw-rectangle (- (send this get-width) 10)  player-two 10 50))))

(new pong% [parent frame])
(send frame show #t)
