#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require racket/set)

; Posn
; A posn is a two dimensional point with components 'x' and 'y'.
(define-struct posn (x y) #:transparent)

; posn number -> posn
; Adds number to x component of posn
(check-expect (posn-addx (posn 0 0) 2) (posn 2 0))
(check-expect (posn-addx (posn 0 0) -3) (posn -3 0))

(define (posn-addx pnt n)
  (struct-copy posn pnt [x (+ (posn-x pnt) n)]))

; posn number -> posn
; Adds number to y component of posn
(check-expect (posn-addy (posn 0 0) 2) (posn 0 2))
(check-expect (posn-addy (posn 0 0) -3) (posn 0 -3))

(define (posn-addy pnt n)
  (struct-copy posn pnt [y (+ (posn-y pnt) n)]))

; posn char -> posn
; Moves the posn based on the direction
(check-expect (posn-move (posn 0 0) #\<) (posn -1 0))
(check-expect (posn-move (posn 0 0) #\>) (posn 1 0))
(check-expect (posn-move (posn 0 0) #\^) (posn 0 1))
(check-expect (posn-move (posn 0 0) #\v) (posn 0 -1))

(define (posn-move pnt direction)
  (cond [(char=? direction #\^) (posn-addy pnt 1)]
        [(char=? direction #\v) (posn-addy pnt -1)]
        [(char=? direction #\>) (posn-addx pnt 1)]
        [(char=? direction #\<) (posn-addx pnt -1)]
        [else (error "Unknown direction")]))

; string -> number
; Determines the number of houses visited based on the
; given route instructions
(check-expect (houses-visited ">") 2)
(check-expect (houses-visited "^>v<") 4)
(check-expect (houses-visited "^v^v^v^v^v") 2)

(define (houses-visited directions [visited (set (posn 0 0))] [i 0] [pos (posn 0 0)])
  (cond [(>= i (string-length directions)) (set-count visited)]
        [else (define moved (posn-move pos (string-ref directions i)))
              (houses-visited directions (set-add visited moved) (+ i 1) moved)]))

; string -> number
; Determines the number of houses visited by either Santa or Robo-Santa
(check-expect (houses-visited-withrobo "^v") 3)
(check-expect (houses-visited-withrobo "^>v<") 3)
(check-expect (houses-visited-withrobo "^v^v^v^v^v") 11)

(define (houses-visited-withrobo directions)
  (visit-houses-santa directions (set (posn 0 0)) 0 (posn 0 0) (posn 0 0)))

; Moves Santa and updates 'visited'.
(define (visit-houses-santa directions visited i santa-pos robo-pos)
  (cond [(>= i (string-length directions)) (set-count visited)]
        [else (define santa-moved (posn-move santa-pos (string-ref directions i)))
              (visit-houses-robo directions (set-add visited santa-moved) (+ i 1) santa-moved robo-pos)]))

; Moves Robo-Santa and updates 'visited'.
(define (visit-houses-robo directions visited i santa-pos robo-pos)
  (cond [(>= i (string-length directions)) (set-count visited)]
        [else (define robo-moved (posn-move robo-pos (string-ref directions i)))
              (visit-houses-santa directions (set-add visited robo-moved) (+ i 1) santa-pos robo-moved)]))

; Execution
(test)

(define day3-input (read-file "day3.txt"))
(houses-visited day3-input) ; Part 1
(houses-visited-withrobo day3-input) ; Part 2
