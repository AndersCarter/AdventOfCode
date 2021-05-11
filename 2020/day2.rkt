#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; string char num num -> bool
; Verifies that the given 'pass' contains 'c' at least 'min' times and at most 'max' times
(check-expect (verify-password "abcde" #\a 1 3) #t)
(check-expect (verify-password "cdefg" #\b 1 3) #f)
(check-expect (verify-password "ccccccccc" #\c 2 9) #t)

(define (verify-password pass c min max)
  (define c-count (for/sum ([i (string-length pass)])
                    (if (char=? c (string-ref pass i)) 1 0)))
  (<= min c-count max))

; string char num num -> bool
; Verifies the password witht he toboggan system
(define (verify-toboggan-password pass c i j)
  (xor (char=? (string-ref pass (- i 1)) c)
       (char=? (string-ref pass (- j 1)) c)))

; string -> [[str char num num]]
; Parses the input file
(define (read-input fp)
  (define input-regex #px"^(\\d+)\\-(\\d+)\\s+(.):\\s+(.+)$")

  (for/fold ([acc empty])
            ([line (read-lines fp)])
    (define match (regexp-match input-regex line))
    (cons (list (string->number (second match)) (string->number (third match)) (string-ref (fourth match) 0) (fifth match)) acc))
  )

; Execution
(test)

(define day2-inputs (read-input "day2.txt"))

; Part 1
(for/sum ([input day2-inputs])
  (define min (first input))
  (define max (second input))
  (define c (third input))
  (define pass (fourth input))
  (if (verify-password pass c min max) 1 0))

; Part 2
(for/sum ([input day2-inputs])
  (define min (first input))
  (define max (second input))
  (define c (third input))
  (define pass (fourth input))
  (if (verify-toboggan-password pass c min max) 1 0))