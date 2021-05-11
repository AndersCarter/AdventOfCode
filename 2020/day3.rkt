#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; Constants
(define TEST-MAP (list
                  "..##......."
                  "#...#...#.."
                  ".#....#..#."
                  "..#.#...#.#"
                  ".#...##..#."
                  "..#.##....."
                  ".#.#.#....#"
                  ".#........#"
                  "#.##...#..."
                  "#...##....#"
                  ".#..#...#.#"))

; Posn
; A posn represents a two dimensional point with components 'x', and 'y'.
(define-struct posn (x y) #:transparent)

; posn posn -> posn
; Adds the two posns together
(define (posn-add p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))

; [string] posn -> number
; Counts the number of trees hit by moving 'step' from (0,0) as defined by 'tree-map'.
; 'tree-map' tiles infinitely to the right.
(check-expect (count-tree-hits TEST-MAP (posn 1 1)) 2)
(check-expect (count-tree-hits TEST-MAP (posn 3 1)) 7)
(check-expect (count-tree-hits TEST-MAP (posn 5 1)) 3)
(check-expect (count-tree-hits TEST-MAP (posn 7 1)) 4)
(check-expect (count-tree-hits TEST-MAP (posn 1 2)) 2)

(define (count-tree-hits tree-map step [pos (posn 0 0)] [count 0])
  (define map-y-size (length tree-map))
  (define map-x-size (string-length (first tree-map)))

  (define new-count (if (char=? (string-ref (list-ref tree-map (posn-y pos)) (posn-x pos)) #\#) (+ count 1) count))
  (define moved-pos (posn-add pos step))
  (define new-pos (struct-copy posn moved-pos [x (modulo (posn-x moved-pos) map-x-size)]))

  (cond [(>= (posn-y new-pos) map-y-size) new-count]
        [else (count-tree-hits tree-map step new-pos new-count)]))

; Execution
(test)

; Part 1
(define input-map (read-lines "day3.txt"))
(count-tree-hits input-map (posn 3 1))

; Part 2
(* (count-tree-hits input-map (posn 1 1))
   (count-tree-hits input-map (posn 3 1))
   (count-tree-hits input-map (posn 5 1))
   (count-tree-hits input-map (posn 7 1))
   (count-tree-hits input-map (posn 1 2)))
