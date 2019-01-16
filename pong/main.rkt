#lang racket/gui

(define frame (new frame% [label "Pong"]))

(define pong%
  (class canvas%
	#| apparently needs to be the first thing |#
	(super-new)

	(define dc (send this get-dc))
	(define player-one 10)
	(define player-two 10)

	(define (move-player-one y) (set! player-one (+ player-one y)))
	(define (move-player-two y) (set! player-two (+ player-two y)))

	(define/override (on-char event)
	  (match (send event get-key-code)
		['up  (move-player-one -1)(send this refresh-now)]
		['down  (move-player-one 1) (send this refresh-now)]
		[#\h  (move-player-two 1) (send this refresh-now)]
		[#\t  (move-player-two -1) (send this refresh-now)]
		[else '()]))

	(define/override (on-paint)
	  (println 'repaint)
	  (send dc set-smoothing 'aligned)
	  (send dc draw-rectangle 1 player-one 25 50)
	  (send dc draw-rectangle (( λ () (print (send this get-client-size)) 10)) player-two 25 50 ))))

(new pong% [parent frame])
(send frame show #t)
