#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; int int int -> int
; Determines the surface area of a right rectangular prism
(check-expect (surface-area 2 3 4) 52)
(check-expect (surface-area 1 1 10) 42)

(define (surface-area l w h)
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)))

; int int int -> int
; Calculates the amount of wrapping paper required for the given box.
; Wrapping paper amount is the surface area + the area of the smallest side
(check-expect (calc-wrappingpaper 2 3 4) 58)
(check-expect (calc-wrappingpaper 1 1 10) 43)

(define (calc-wrappingpaper l w h)
  (define lw (* l w))
  (define wh (* w h))
  (define hl (* h l))
  (+ (* 2 lw) (* 2 wh) (* 2 hl) (min lw wh hl)))

; int int int -> int
; Calculates the amount of ribbon needed to wrap the present of the given dimensions
(check-expect (calc-ribbon 2 3 4) 34)
(check-expect (calc-ribbon 1 1 10) 14)

(define (calc-ribbon l w h)
  (define lw (* 2 (+ l w)))
  (define wh (* 2 (+ w h)))
  (define hl (* 2 (+ h l)))
  (+ (min lw wh hl) (* l w h)))

; string -> [[int]]
; Reads and parses the input
(define (read-input fp)
  (for/fold ([acc empty])
            ([line (read-lines fp)])
    (cons (map string->number (string-split line "x")) acc)))

; Execution
(test)

(define day2-input (read-input "day2.txt"))

; Part 1 - Total Wrapping Paper Amount
(foldr + 0 (map (lambda (x) (calc-wrappingpaper (first x) (second x) (third x))) day2-input))

; Part 2 - Total Ribbon Length
(foldr + 0 (map (lambda (x) (calc-ribbon (first x) (second x) (third x))) day2-input))