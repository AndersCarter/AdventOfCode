#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; string -> bool
; Determines if the string is nice.;
; A nice string has,
; 1. At least three vowels (aeiou)
; 2. At least one letter that appears twice in a row,
;    like (aa)
; 3. It does not contain one of the following substrings
;    'ab', 'cd', 'pq', or 'xy'.

(check-expect (is-nice? "ugknbfddgicrmopn") #t)
(check-expect (is-nice? "aaa") #t)
(check-expect (is-nice? "jchzalrnumimnmhp") #f) ; No double letter
(check-expect (is-nice? "haegwjzuvuyypxyu") #f) ; Contains 'xy'
(check-expect (is-nice? "dvszwmarrgswjxmb") #f) ; Only one vowel

(define (is-nice? str [vowel-count 0] [has-double? #f] [has-excluded? #f] [i 0])
  (cond [(>= i (string-length str)) (and (>= vowel-count 3) has-double? (not has-excluded?))]

        ; Checks the vowel for the first character/iteration
        [(= i 0) (if (is-vowel? (string-ref str i))
                     (is-nice? str (+ vowel-count 1) has-double? has-excluded? (+ i 1))
                     (is-nice? str vowel-count has-double? has-excluded? (+ i 1)))]
        
        [else (define current-char (string-ref str i))
              (define prev-char (string-ref str (- i 1)))
              (is-nice? str
                        (if (is-vowel? current-char) (+ vowel-count 1) vowel-count) ; Updates vowel count
                        (or has-double? (char=? current-char prev-char))            ; Checks for double
                        (or has-excluded? (member (string prev-char current-char) (list "ab" "cd" "pq" "xy"))) ; Checks for excluded substrings
                        (+ i 1))]))

; string -> bool
; Determines if the string is nice.
; A nice string has,
; 1. A pair of any two letters that appears at least twice in the string without overlapping,
;    Such as, "xyxy" (xy) or "aabcdefgaa" (aa) but not "aaa".
; 2. At least one letter which repreasts with exactly one letter between them.
;    Such as, "xyx", or "abcdefeghi" (efe).

(check-expect (is-nice?-part2 "qjhvhtzxzqqjkmpb") #t) ; 'qj' appears twice and 'zxz'.
(check-expect (is-nice?-part2 "xxyxx") #t)
(check-expect (is-nice?-part2 "uurcxstgmygtbstg") #f) ; No repeat with a single letter between
(check-expect (is-nice?-part2 "ieodomkazucvgmuy") #f) ; No pair appears twice

(define (is-nice?-part2 str)
  (cond [(< (string-length str) 3) #f] ; Can't satisfy either condition
        [else (define first-pair (substring str 0 2))
              (define second-pair (substring str 1 3))
              (define pair-set (set first-pair))
              (is-nice?-part2-recur str pair-set second-pair #f (char=? (string-ref str 0) (string-ref str 2)) 3)]))

(define (is-nice?-part2-recur str pair-set pair-hold has-pair? has-repeat? i)
  (cond [(>= i (string-length str)) (and has-pair? has-repeat?)]
        [else (define new-pair (substring str (- i 1) (+ i 1)))
              (is-nice?-part2-recur str
                                    (set-add pair-set pair-hold) ; Add held pair to set
                                    new-pair ; Current pair becomes held pair
                                    (or has-pair? (set-member? pair-set new-pair))
                                    (or has-repeat? (char=? (string-ref str (- i 2)) (string-ref str i)))
                                    (+ i 1))]))

; char -> bool
; Returns true if the character is a vowel (aeiou).
(check-expect (is-vowel? #\a) #t)
(check-expect (is-vowel? #\e) #t)
(check-expect (is-vowel? #\i) #t)
(check-expect (is-vowel? #\o) #t)
(check-expect (is-vowel? #\u) #t)
(check-expect (is-vowel? #\y) #f)
(define (is-vowel? c)
  (foldr (lambda (x y) (or y (char=? c x))) #f (list #\a #\e #\i #\o #\u)))

; Execution
(test)
(define day5-input (read-lines "day5.txt"))

; Part 1
(for/sum ([str day5-input])
  (if (is-nice? str) 1 0))

; Part 2
(for/sum ([str day5-input])
  (if (is-nice?-part2 str) 1 0))
