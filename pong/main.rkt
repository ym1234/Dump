#!/usr/bin/env racket
#lang racket/gui

;; TODO(ym): Fix this mess when you actually learn racket properly lol
;; UHHHH why did i use this instead of the old graphics api?
;; https://docs.racket-lang.org/graphics/index.html

(struct vec2d (x y) #:transparent #:mutable)

(define (vec+! x y)
  (set-vec2d-x! x (+ (vec2d-x x) (vec2d-x y)))
  (set-vec2d-y! x (+ (vec2d-y x) (vec2d-y y))))

(define frame (new frame% [label "Pong"] [width 500] [height 500] ))
(define pong%
  (class canvas%

    (define-values (players keys circle-dir circle)
      (values
        (vector 30 30)
        (mutable-set)
        (vec2d 5 5)
        (vec2d 0 0)))

    (super-new [min-height 500] [min-width 500])
    (inherit flush get-dc set-canvas-background get-height get-width refresh-now refresh)

    (send* (get-dc)
           (set-brush "white" 'solid)
           (set-pen (new pen% [style 'transparent]))
           (set-smoothing 'aligned))
    (set-canvas-background (make-object color% 0 0 0))

    (define (move-player player y)
      (vector-set! players player
                   (modulo (+ (vector-ref players player) y) (get-height))))

    (define/public (update)
      (for ([key (in-set keys)])
        (match key
          ['up   (move-player 0 -10)]
          ['down (move-player 0  10)]
          [#\k   (move-player 1 -10)]
          [#\j   (move-player 1  10)]
          [else  void]))

      (vec+! circle circle-dir)
      (check-ball)
      (refresh-now))

    ;; i mean it works, but still looks ugly af
    (define/private (check-ball)
      (for ([i `((,<=  0 0) (,>=  ,(- (get-width) 20) ,(- (get-height) 20)))])
        (match-define (list op x y) i)
        (cond
          [(op (vec2d-x circle) x) (set-vec2d-x! circle-dir (- (vec2d-x circle-dir))) (set-vec2d-x! circle x)]
          [(op (vec2d-y circle) y) (set-vec2d-y! circle-dir (- (vec2d-y circle-dir))) (set-vec2d-y! circle y)])))

    (define/override (on-size width height)
      (move-player 0 0)
      (move-player 1 0))

    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (match key-code
        ['release (set-remove! keys (send event get-key-release-code))]
        [(or 'escape #\q) (exit)]
        [else (set-add! keys key-code)]))

    (define/override (on-paint)
      (send* (get-dc)
             (draw-ellipse (vec2d-x circle) (vec2d-y circle) 20 20)
             (draw-rectangle 30 (vector-ref players 0) 10 100)
             (draw-rectangle (- (get-width) 40) (vector-ref players 1) 10 100)
             (draw-rectangle (/ (get-width) 2) 0 10 (get-height))))))

(define pong (new pong% [parent frame]))
(send frame show #t)

;; Not sure if this is the right way // Pretty sure this isn't the right way
(define (game-loop)
  (send pong update)
  (sleep/yield 0.016)
  (game-loop))
(game-loop)
