#lang racket

(require test-engine/racket-tests)
(require file/md5)

; string -> int
; Returns lowest integer needed to append to 'key' to get a
; md5 hash with five leading zeros
(check-expect (mine-adventcoin "abcdef") 609043)
(check-expect (mine-adventcoin "pqrstuv") 1048970)

(define (mine-adventcoin key leading-zeros [i 0])
  (define hash-string (bytes->string/utf-8 (md5 (string-append key (number->string i)))))
  (cond [(= (modulo i 1000) 0) (displayln i)])
  (if (string=? (make-string leading-zeros #\0) (substring hash-string 0 5))
      i
      (mine-adventcoin key leading-zeros (+ i 1))))

; Execution
;(test)
(mine-adventcoin "iwrupvqb" 5) ; Part 1
(mine-adventcoin "iwrupvqb" 6) ; Part 2