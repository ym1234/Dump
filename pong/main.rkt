#!/usr/bin/env racket
#lang racket/gui

;; TODO(ym): Fix this mess when you actually learn racket properly lol
;; UHHHH why did i use this instead of the old graphics api?
;; https://docs.racket-lang.org/graphics/index.html

(struct vector2d (x y) #:transparent #:mutable)

(define frame (new frame% [label "Pong"] [width 500] [height 500] ))
(define pong%
  (class canvas%
    (define player-one 10)
    (define player-two 10)
    (define circle-dir (vector2d 1 1))
    (define keys (make-hash))

    (super-new [min-height 500] [min-width 500])
	(inherit get-dc set-canvas-background get-height get-width refresh-now)

    (define dc (get-dc))

    (send dc set-brush "white" 'solid)
    (send dc set-pen (new pen% [style 'transparent]))
    (send dc set-smoothing 'aligned)
    (set-canvas-background (make-object color% 0 0 0))

    (define circle (vector2d (/ (get-height) 2.0)  (/ (get-width) 2.0)))

    ;; Better way to do this? macro? Box? match? or just use a list of players lol
    (define (move-player-one y) (set! player-one (modulo (+ player-one y) (get-height))))
    (define (move-player-two y) (set! player-two (modulo (+ player-two y) (get-height))))

    ;; aside from that, TODO(ym): actually do the collisions lol
    (define/public (update)
      (if (hash-ref! keys 'up #f) (move-player-one -10) '())
      (if (hash-ref! keys 'down #f) (move-player-one 10) '())
      (if (hash-ref! keys #\k #f) (move-player-two -10) '())
      (if (hash-ref! keys #\j #f) (move-player-two 10) '())
      (set-vector2d-x! circle (+ (vector2d-x circle) (vector2d-x circle-dir)))
      (set-vector2d-y! circle (+ (vector2d-y circle) (vector2d-y circle-dir)))
      (refresh-now))

    #| (define/private (move-ball) |#
    #|   '()) |#

    ;; you know the drill
    (define/override (on-size width height)
	  (printf "width: ~a, height: ~a" width height)
      (set! player-one (modulo player-one height))
      (set! player-two (modulo player-two height)))

    (define/override (on-char event)
	  (define key-code (send event get-key-code))
      (match key-code
        [(or 'up 'down #\k #\j)  (hash-set! keys (send event get-key-code) #t)]
        ['release (hash-set! keys (send event get-key-release-code) #f)]
        ['escape (exit)]
        [else void]))


    ;; TODO(ym): More complex drawing, this is a one off so i don't really care, but it looks really ugly when the rects clip
    (define/override (on-paint)
      (send dc draw-ellipse (vector2d-x circle) (vector2d-y circle) 50 50)
      (send dc draw-rectangle 5 player-one 10 100)
      (send dc draw-rectangle (- (get-width) 15) player-two 10 100))))

(define pong (new pong% [parent frame]))
(send frame show #t)

(thread
 (lambda ()
   (define (game-loop)
     (send pong update)
     (sleep 0.016)
     (game-loop))
   (game-loop)))
