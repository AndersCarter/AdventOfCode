#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; vector -> vector
; Evaluates the intcode for 'v'. 'i' is the current index of command
(check-expect (eval-intcode (vector 1 9 10 3 2 3 11 0 99 30 40 50)) (vector 3500 9 10 70 2 3 11 0 99 30 40 50))
(check-expect (eval-intcode (vector 1 0 0 0 99)) (vector 2 0 0 0 99))
(check-expect (eval-intcode (vector 2 3 0 3 99)) (vector 2 3 0 6 99))
(check-expect (eval-intcode (vector 2 4 4 5 99 0)) (vector 2 4 4 5 99 9801))
(check-expect (eval-intcode (vector 1 1 1 4 99 5 6 0 99)) (vector 30 1 1 4 2 5 6 0 99))

(define (eval-intcode v [i 0])
  
  (define command (vector-ref v i))
  (cond [(= command 99) v] ; End intcode execution
        [(or (= command 1)
             (= command 2)) (eval-intcode (math-opcode v command i) (+ i 4))]
        [else (error "Unknown intcode command")]))

; vector int int -> vector
; Evaluates the math opcode. 'i' is the index of the opcode
(define (math-opcode v opcode i)
  (define left-index (vector-ref v (+ i 1)))   ; Index of the left operand
  (define right-index (vector-ref v (+ i 2)))  ; Index of the right operand
  (define result-index (vector-ref v (+ i 3))) ; Index of the result position

  (define op (cond [(= opcode 1) +]
                   [(= opcode 2) *]
                   [else (error "Unknown math opcode.")]))

  (vector-set! v result-index (op (vector-ref v left-index) (vector-ref v right-index)))
  v)

; vector int -> vector
; Solves the intcode by modifying the first and second commands until 'desired' is the result.
(define (solve-intcode v desired [i 0] [j 0])
  (define copy (vector-copy v))
  (vector-set! copy 1 i)
  (vector-set! copy 2 j)
  (define evaled (eval-intcode copy))

  (cond [(= (vector-ref evaled 0) desired) (vector i j)]
        [(= j 99) (solve-intcode v desired (+ i 1) 0)]
        [(> i 99) (error "No solution found")]
        [else (solve-intcode v desired i (+ j 1))]))

; Helper Functions

; string -> vector
; Parses the input file
(define (read-input fp)
  (list->vector (map string->number (string-split (read-file fp) ","))))

; Execution
(test)

; Part 1
(define intcode-input (read-input "day2.txt"))
(vector-set! intcode-input 1 12)
(vector-set! intcode-input 2 2)
(vector-ref (eval-intcode intcode-input) 0)

; Part 2
(set! intcode-input (read-input "day2.txt"))
(define solved-input (solve-intcode intcode-input 19690720))
(define noun (vector-ref solved-input 0))
(define verb (vector-ref solved-input 1))
(+ (* 100 noun) verb)