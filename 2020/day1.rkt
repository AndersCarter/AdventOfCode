#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; [number] -> number
; Fixes the expense report by finding two entries that sum to 2020 and then
; multiplying them together
(check-expect (fix-expense-report (list 1721 979 366 299 675 1456)) 514579)

(define (fix-expense-report entries)
  (fix-expense-report-recur (sort entries <) 0 (- (length entries) 1)))

(define (fix-expense-report-recur entries start stop)

  (define start-entry (list-ref entries start))
  (define stop-entry (list-ref entries stop))
  (define entry-sum (+ start-entry stop-entry))
  
  (cond [(<= stop start) (error "No value sums to 2020")]
        [(= entry-sum 2020) (* start-entry stop-entry)]
        [(< entry-sum 2020) (fix-expense-report-recur entries (+ 1 start) stop)]
        [(> entry-sum 2020) (fix-expense-report-recur entries start (- stop 1))]))

; [number] number -> number
; Fixes the expense report by finding three entries that sum to 2020 and then
; multiplying them together
(check-expect (fix-expense-report-slow (list 1721 979 366 299 675 1456)) 241861950)

(define (fix-expense-report-slow entries [i 0] [j 1] [k 2])
  (define i-entry (list-ref entries i))
  (define j-entry (list-ref entries j))
  (define k-entry (list-ref entries k))

  (cond [(= (+ i-entry j-entry k-entry) 2020) (* i-entry j-entry k-entry)]
        [else (define new-k (+ k 1))
              (define new-j j)
              (define new-i i)
              (cond [(>= new-k (length entries)) (set! new-k 0) (set! new-j (+ j 1))])
              (cond [(>= new-j (length entries)) (set! new-j 0) (set! new-i (+ i 1))])
              (cond [(>= new-i (length entries)) (error "No three values sum to 2020")]
                    [else (fix-expense-report-slow entries new-i new-j new-k)])]))


; Execution
(test)

(define day1-input (map string->number (read-lines "day1.txt")))
(fix-expense-report day1-input)      ; Part 1
(fix-expense-report-slow day1-input) ; Part 2