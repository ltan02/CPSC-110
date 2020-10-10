;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2016W1-F-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@problem 1)
;; The following signature purpose, stub, and check-expects for the function 
;; count-trues are provided for you to use with Problems 1A, 1B, 1C and 1D.
;;
;; Read Problems 1A, 1B, 1C and 1D before you start.


(@htdf count-trues)
(@signature (listof Boolean) -> Natural)
;; produce the number of trues in the list
(check-expect (count-trues empty) 0)
(check-expect (count-trues (list false true true true false false))
              3)
(check-expect (count-trues (list true true false true true true true))
              6)

#;
(define (count-trues x) 0) ;stub


;; Problem 1A:
;;
;; Write a non-tail recursive function definition for count-trues using the
;; (listof X) template.
;;
;; Include a template tag.
#;#;
(@template (listof Boolean))
(define (count-trues x)
  (cond [(empty? x) 0]
        [else
         (if (first x)
             (+ 1 (count-trues (rest x)))
             (count-trues (rest x)))]))

;; Problem 1B:
;;
;; Write a tail recursive function definition for count-trues using the
;; (listof X) template.
;;
;; Include a template tag.
#;#;
(@template (listof X) encapsulated accumulator)
(define (count-trues x0)
  ;; acc is Natural ; the number of trues seen so far
  (local [(define (fn-for-x x acc)
            (cond [(empty? x) acc]
                  [else
                   (fn-for-x (rest x)
                             (if (first x)
                                 (add1 acc)
                                 acc))]))]
    (fn-for-x x0 0)))

;; Problem 1C:
;;
;; Write a a function definition for count-trues using built-in abstract list
;; functions.
;;
;; Include a template tag.
;; DO NOT use the built-in length function.

(@template use-abstract-fn)
(define (count-trues x)
  (foldr + 0 (map (λ (e) (if e 1 0)) x)))

;; Problem 1D:
;;
;; Write a function definition for count-trues using for-each. 
;;
;; Include a template tag.

; DID NOT LEARN

(@problem 2)
;; In a list of items, the term run-length describes how many of a given item
;; appear in a row. In this list of numbers:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/16w1frun.PNG
;;
;; There are 2 separate runs of the number 2. The first has run length 1, the
;; second has run length 3. So the maximum run length of 2 is 3.
;;
;; Below are the signature, purpose and check-expects for the function max-run.
;;
;; Write the template tag and the function definition for max-run.
;; Do not use for-each. 

(@htdf max-run)
(@signature Number (listof Number) -> Natural)
;; Produce the maximum run length of n in the list. 0 if n does not appear.
(check-expect (max-run 1 (list)) 0)
(check-expect (max-run 2 (list 1 2 3 4 2 2 2 1)) 3)
(check-expect (max-run 5 (list 5 4 5 6 5 5 5 5 3)) 4)
#;
(define (max-run n lon) 0) ;stub

(@template (listof Number) encapsulated accumulator)
(define (max-run n lon0)
  ;; curr-run-n is Natural ; the current number of consecutive n seen so far
  ;; max-run-n is Natural ; the highest number of consecutive n seen so far
  (local [(define (fn-for-lon lon curr-run-n max-run-n)
            (cond [(empty? lon) max-run-n]
                  [else
                   (fn-for-lon (rest lon)
                               (if (= (first lon) n)
                                   (add1 curr-run-n)
                                   0)
                               (max curr-run-n max-run-n))]))]
    (fn-for-lon lon0 0 0)))

(@problem 3)
;; Design a function that consumes a natural number >= 2 and
;; produces a "chess board" of that size. 
;;
;; For coloring:
;;  - assume the upper left corner is row,column 0,0
;;  - follow the rule that if the sum of the row and column number is odd,
;;    the square should be black, otherwise it should be white
;;
;; Do not worry about placing a border around your board.
;;
;; For example (board 4) should produce something like this:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/16w1fboard.PNG
;;
;; HINT: If you are stuck, first sketch out a function that produces
;; a single row of squares all the same colour. 

(define BLK (square 20 "solid" "black"))
(define WHT (square 20 "solid" "white"))

(@htdf board)
(@signature Natural -> Image)
;; produces a "chess board" of that size
;; ASSUME: n >= 2
(check-expect (board 4) (above (beside WHT BLK WHT BLK)
                               (beside BLK WHT BLK WHT)
                               (beside WHT BLK WHT BLK)
                               (beside BLK WHT BLK WHT)))
(check-expect (board 2) (above (beside WHT BLK)
                               (beside BLK WHT)))

;(define (board n) empty-image) ;stub

(@template use-abstract-fn)
(define (board n)
  (foldr above empty-image
         (build-list n
                     (λ (r)
                       (foldr beside empty-image
                              (build-list n
                                          (λ (c)
                                            (if (odd? (+ r c))
                                                BLK
                                                WHT))))))))

(@problem 4)
;; As you know, the template for using a for-each loop is: 
;;
;;  (define (fn-using-for-each lox)
;;   ;; acc is:      ;
;;   (local [(define acc ...)]
;;     (begin
;;       (for-each (lambda (x)
;;                   (set! acc (... acc x)))
;;                 lox)
;;       (... acc))))
;;
;; Of course given any template you can produce an abstract function definition 
;; by simply replacing each set of dots with a parameter. We have done that for
;; you below and named the function foldfe for fold with for-each.
;;
;; Please complete this abstract function by adding its signature and the
;; following 3 check expects:
;;
;;   - one that uses foldfe to sum (list 2 3 4) to get 9
;;   - one that uses foldfe to multiply (list 2 3 4) to get 24
;;   - one that uses foldfe to average (list 2 3 4) to get 3
;;
;; Each individual check-expect must only call for-each one time.  If you can't
;; fit your check-expects here there is plenty of room on the next page.

#;
(define (foldfe f1 f2 b lox)
  (local [(define acc b)]
    (begin
      (for-each (lambda (x)
                  (set! acc (f1 acc x)))
                lox)
      (f2 acc))))

(@problem 5)
;; Below is a function that consumes the representation of a simple map, and
;; produces the total combined number of ponds and islands on that map.
;; 
;; In this exam problem you need to modify the function design so that instead
;; of producing a single number which is the sum of the number of ponds and
;; the number of islands, it instead produces a (listof (listof Pos)). The
;; (listof (listof Pos)) has one (listof Pos) for each pond or island. The
;; positions in the list are all the positions in that pond or island.
;; 
;; For example, calling your revised function as follows:
;;
;; (count-pi (list W W W
;;                 W L W
;;                 W W W))
;;
;; Should produce:
;;
;; (list (list (make-posn 1 1))
;;      
;;       (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)
;;             (make-posn 1 2) (make-posn 2 2)
;;             (make-posn 2 1) (make-posn 2 0) (make-posn 1 0)))
;;
;; Do not worry about the order of elements in the lists! If your function
;; produces lists with the same positions as your check-expects we will consider
;; it correct.
;;
;; Be sure to follow all recipe elements correctly.
;;
;;
;; NOTE: Spend some time carefully reading the existing design before you start.
;; The changes you need to make, especially to the function definition, are
;; relatively minor. You can make the changes by making edits on the existing
;; design. So writing the changes shouldn't take very long. Working out exactly
;; what the changes should be is likely to take longer.


;; =================
;; Data definitions:


(@htdd Surface)
;; Surface is Boolean
;; interp. true is land, false is water
(define W #f)
(define L #t)


(@htdd Map)
;; Map is (listof Surface)
;; interp. A square map. 
;;         For any given map its RANK is (sqrt (length <map>))
;;         A position on a map must have 0 <= x < RANK and 0 <= y < RANK.
;;         0, 0 is upper left
(define M1 (list L L   
                 W W))


(@htdd Pos)
;; Pos is (make-posn Natural Natural)
;; interp. the x, y coordinates of a position on a map. See Map.

(define P1 (make-posn 0 0))


;; =================
;; Functions:


(@htdf count-pi)
(@signature Map -> Natural)
;; count the number of ponds and islands (distinct subgraphs with same Surface)
(check-expect (count-pi (list W W W
                              W L W
                              W W W))
              (list (list (make-posn 1 1))      
                    (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)
                          (make-posn 0 1) (make-posn 2 1)
                          (make-posn 0 2) (make-posn 1 2) (make-posn 2 2))))
              
(check-expect (count-pi (list W W L
                              W W W
                              L W W))
              (list (list (make-posn 2 0) (make-posn 0 2))
                    (list (make-posn 0 0) (make-posn 1 0)
                          (make-posn 0 1) (make-posn 1 1) (make-posn 2 1)
                          (make-posn 1 2) (make-posn 2 2))))

(check-expect (count-pi (list W L W L W
                              W W W W W
                              W W W W W
                              W L W L W
                              W W W W W))
              (list (list (make-posn 1 0) (make-posn 3 0)
                          (make-posn 1 3) (make-posn 3 3))
                    (list (make-posn 0 0) (make-posn 2 0) (make-posn 4 0)
                          (make-posn 0 1) (make-posn 1 1) (make-posn 2 1)
                          (make-posn 3 1) (make-posn 4 1) (make-posn 0 2)
                          (make-posn 1 2) (make-posn 3 2) (make-posn 4 2)
                          (make-posn 0 3) (make-posn 2 3) (make-posn 4 3)
                          (make-posn 0 4) (make-posn 1 4) (make-posn 2 4)
                          (make-posn 3 4) (make-posn 4 4))))

(@template Pos (listof Pos) genrec accumulator encapsulated)

(define (count-pi m)
  (local [(define RANK (sqrt (length m)))
          
          ;; Ordinary args:
          ;;  p is Pos; position the current search traversal is at
          ;;  looking is Surface; the surface the current traversal is looking
          ;;                      for 
          
          ;; Accumulators:
          ;;  c(ount) is Natural; number of contiguous ponds/islands seen so far
          ;;  unvisited is (listof Pos); every pos not colored by color/one
          ;;  todo is (listof Pos); worklist for color/one and color/todo
          
          ;; Termination:
          ;; scan, color/one, color/todo visit cells of the map one at
          ;; a time, stopping when unvisited is empty.
          ;;  Since the map is finite, this will terminate.

          ;; initiate a new graph traversal at next unvisited cell
          ;; template on (listof Pos), and 2 accumulators
          (define (scan c unvisited)
            (cond [(empty? unvisited) (list (reverse (first c))
                                            (reverse (second c)))]
                  [else
                   (local [(define p (first unvisited))]
                     (color/one p (map-ref m p) c unvisited empty))]))

          ;; generate and traverse the graph of connected cells matching looking
          ;; next two sub-functions templated as traversal of generated graph
          (define (color/one p looking c unvisited todo)
            (cond [(not (member? p unvisited))
                   (color/todo looking c unvisited todo)]
                  ;[(not (surface=? (map-ref m p) looking)) 
                  ; (color/todo looking c unvisited todo)]
                  [else
                   (color/todo looking
                               (if (map-ref m p) ;; L is true
                                   (list (cons p (first c))
                                         (second c))
                                   (list (first c)
                                         (cons p (second c))))
                               (remove p unvisited)
                               (append (next-ps p) todo))]))

          (define (color/todo looking c unvisited todo)
            (cond [(empty? todo) (scan c unvisited)]
                  [else
                   (color/one (first todo) looking c unvisited (rest todo))]))

          ;; produce the neighbours of p
          ;; template on Pos
          (define (next-ps p)            
            (local [(define x (posn-x p))
                    (define y (posn-y p))]
              
              (filter on-map?
                      (list (make-posn (sub1 x)       y)      ;L
                            (make-posn (add1 x)       y)      ;R
                            (make-posn       x  (sub1 y))     ;U
                            (make-posn       x  (add1 y)))))) ;D

          (define (on-map? p)
            (local [(define x (posn-x p))
                    (define y (posn-y p))]
              (and (<= 0 x) (< x RANK)
                   (<= 0 y) (< y RANK))))

          ;; produce true if both are land or both are water
          (define (surface=? b1 b2)
            (boolean=? b1 b2))
     
          ;; produce contents of map cell at p
          ;; Template from Pos
          (define (map-ref m p)
            (list-ref m (+ (* (posn-y p) RANK) (posn-x p))))]

    (scan (list (list) (list))
          ;; Build a list of every position in the map
          (build-list (sqr RANK)
                      (lambda (i)
                        (make-posn (remainder i RANK)
                                   (quotient  i RANK)))))))


















































