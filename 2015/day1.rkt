#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; string -> int
; Calculates the resulting floor from the directions
(check-expect (eval-instructions "(())") 0)
(check-expect (eval-instructions "()()") 0)
(check-expect (eval-instructions "(((") 3)
(check-expect (eval-instructions "(()(()(") 3)
(check-expect (eval-instructions "))(((((") 3)
(check-expect (eval-instructions "())") -1)
(check-expect (eval-instructions "))(") -1)
(check-expect (eval-instructions ")))") -3)
(check-expect (eval-instructions ")())())") -3)

(define (eval-instructions instructions [floor 0])
  (cond [(= (string-length instructions) 0) floor]
        [(char=? (string-ref instructions 0) #\() (eval-instructions (substring instructions 1) (+ floor 1))]
        [(char=? (string-ref instructions 0) #\)) (eval-instructions (substring instructions 1) (- floor 1))]))

; string -> int
; Calculates the position within the instructions where santa first enters the
; basement
(check-expect (basement-position ")") 1)
(check-expect (basement-position "()())") 5)

(define (basement-position instructions [i 0] [floor 0])
  (cond [(= floor -1) i]
        [(>= i (string-length instructions)) (error "Does not enter basement.")]
        [(char=? (string-ref instructions i) #\() (basement-position instructions (+ i 1) (+ floor 1))]
        [(char=? (string-ref instructions i) #\)) (basement-position instructions (+ i 1) (- floor 1))]))


; Execution
(test)

(define day1-input (read-file "day1.txt"))
(eval-instructions day1-input) ; Part 1
(basement-position day1-input) ; Part 2