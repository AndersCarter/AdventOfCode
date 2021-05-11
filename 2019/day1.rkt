#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; int -> int
; Calculates the amount of fuel needed based on the given 'mass'. Fuel
; calculation is fuel = floor(mass / 3) - 2
(check-expect (calc-fuel 12) 2)
(check-expect (calc-fuel 14) 2)
(check-expect (calc-fuel 1969) 654)
(check-expect (calc-fuel 100756) 33583)

(define (calc-fuel mass)
  (- (floor (/ mass 3)) 2))

; int -> int
; Calculates the amount of fuel required for the module based on its mass
; including the mass added by the fuel.
(check-expect (calc-module-fuel 14) 2)
(check-expect (calc-module-fuel 1969) 966)
(check-expect (calc-module-fuel 100756) 50346)

(define (calc-module-fuel mass [acc 0])
  (define fuel-mass (calc-fuel mass))
  (if (<= fuel-mass 0)
      acc
      (calc-module-fuel fuel-mass (+ acc fuel-mass))))

; Helper Functions

; str -> [int]
; Reads and parses the input file, 'fp'.
(define (read-input fp)
  (map string->number (read-lines fp)))


; Execution
(test)
(define module-masses (read-input "day1.txt"))

; Part 1 - Sum of all fuel calculation
(foldr + 0 (map calc-fuel module-masses))

; Part 2 - Sum of the fuel calculations with fuel mass
(foldr + 0 (map calc-module-fuel module-masses))
