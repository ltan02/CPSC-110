;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2016W1-F-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

;(define (count-trues x) 0) ;stub


;; Problem 1A:
;;
;; Write a non-tail recursive function definition for count-trues using the
;; (listof X) template.
;;
;; Include a template tag.


;; 5, 1 each for
;;
;;  - template tag
;;  - template is apparent
;;  - base case result is correct (0)
;;  - combination is correct (+)
;;  - selection behaviour is correct (true and false cases)


(@template (listof Boolean))

#;
(define (count-trues lob)
  (cond [(empty? lob) 0]
        [else
         (if (first lob)
             (add1 (count-trues (rest lob)))
             (count-trues (rest lob)))]))


(@template (listof Boolean))

#;
(define (count-trues lob)
  (cond [(empty? lob) 0]
        [else
         (+ (if (first lob) 1 0)
            (count-trues (rest lob)))]))



;; Problem 1B:
;;
;; Write a tail recursive function definition for count-trues using the
;; (listof X) template.
;;
;; Include a template tag.


;; 7, 1 each for
;;
;;  - template tag
;;  - template is apparent
;;  - accumulator type (0 if no accumulator comment)
;;  - accumulator invariant (0 if no accumulator comment)
;;  - accumulator initial value is correct
;;  - accumulator value update is correct
;;  - final result is correct (the accumulator)


(@template (listof Boolean) accumulator)

#;
(define (count-trues lob0)
  ;; rsf is Natural -  number of trues seen so far
  (local [(define (count-trues lob rsf)
            (cond [(empty? lob) rsf]
                  [else
                   (if (first lob)
                       (count-trues (rest lob) (add1 rsf))
                       (count-trues (rest lob) rsf))]))]
    (count-trues lob0 0)))


(@template (listof Boolean) accumulator)

#;
(define (count-trues lob0)
  ;; rsf is Natural -  number of trues seen so far
  (local [(define (count-trues lob rsf)
            (cond [(empty? lob) rsf]
                  [else
                   (count-trues (rest lob) (+ (if (first lob) 1 0) rsf))]))]
    (count-trues lob0 0)))



;; Problem 1C:
;;
;; Write a a function definition for count-trues using built-in abstract list
;; functions.
;;
;; Include a template tag.
;; DO NOT use the built-in length function.


;; 5, 1 each for
;;
;;  - template tag
;;  - template is apparent
;;  - selection behaviour is correct (1 / 0 for true / false)
;;  - combination behaviour is correct +
;;  - function is correct


(@template use-abstract-fn fn-composition)

#;
(define (count-trues lob)
  (foldr (lambda (t n) (add1 n))
         0
         (filter identity lob)))


(@template use-abstract-fn)

#; 
(define (count-trues lob)
  (foldr (lambda (b n) (if b (add1 n) n))
         0
         lob))


(@template use-abstract-fn)

#; 
(define (count-trues lob)
  (foldr (lambda (b n) (+ (if b 1 0) n))
         0
         lob))

(@template use-abstract-fn fn-composition)

(define (count-trues lob)
  (foldr + 0 (map (lambda (t) 1) (filter identity lob))))



;; Problem 1D:
;;
;; Write a function definition for count-trues using for-each. 
;;
;; Include a template tag.


;; 7, 1 each for
;;
;;  - template tag
;;  - template is apparent
;;  - accumulator type (0 if no accumulator comment)
;;  - accumulator invariant (0 if no accumulator comment)
;;  - accumulator initial value is correct
;;  - accumulator new value is correct
;;  - final result is correct (the accumulator)


(@template for-each)

#;
(define (count-trues lob)
  ;; cnt is Natural - count of the number of trues seen so far
  (local [(define cnt 0)]
    (begin
      (for-each (λ (b)
                  (if b
                      (set! cnt (add1 cnt))
                      (void)))
                lob)
      cnt)))


(@template for-each)

#;
(define (count-trues lob)
  ;; cnt is Natural - count of the number of trues seen so far
  (local [(define cnt 0)]
    (begin
      (for-each (λ (b)
                  (set! cnt (+ cnt (if b 1 0))))
                lob)
      cnt)))



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

;(define (max-run n lon) 0) ;stub


;; 7, 1 each for
;;
;; 
;;  list template is apparent
;;  accumulator template is apparent
;;    (outer fn, inner fn, accumulator parameter and operands)
;;  for curr
;;    initial value is correct
;;    value update is correct in add1 case
;;    value update is correct in reset to 0 case
;;  for rsf
;;    initial value is correct
;;    value update is correct
;;    (can always be max, but only has to max in add1 case of curr


(@template (listof Number) accumulator)   

(define (max-run n lon0)
  ;; curr is Natural, length of current run
  ;; rsf is Natural,  longest run seen so far
  (local [(define (max-run lon curr rsf)
            (cond [(empty? lon) rsf]
                  [else
                   (local [(define new-curr (if (= (first lon) n)
                                                (add1 curr) 0))]
                     (max-run (rest lon)
                              new-curr
                              (max rsf new-curr)))]))]
    
    (max-run lon0 0 0)))



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


;; Note: The whole thing can go the other way around, putting columns beside
;; each other.
;;
;;  1- signature (both types correct)
;;
;;  1- for outer fn operating on Natural
;;      (either list template or foldr + build list)
;;      template is apparent, either self-referential Natural,
;;      or composition of built-ins
;;  1- composition in outer is correct (above)
;;  1- contribution in outer is correct (a row)
;;  1- base in outer is correct empty-image
;;
;;  1- for inner fn operating on Natural
;;      (either list template or foldr + build list)
;;      template is apparent, either self-referential Natural,
;;      or composition of built-ins
;;  1- composition in inner is correct (beside)
;;  1- contribution in inner is correct (a cell)
;;  1- base in outer is correct empty-image
;;
;;  2- color alternating behaviour is correct (all or nothing)


(@htdf chessboard)
(@signature Natural -> Image)
;; produce chessboard of given size, upper left corner is white
(check-expect (chessboard 0) empty-image)
(check-expect (chessboard 2) (above (beside WHT BLK) (beside BLK WHT)))

(@template use-abstract-fn fn-composition)

(define (chessboard n)
  (foldr above
         empty-image
         (build-list n
                     (lambda (r)
                       (foldr beside
                              empty-image
                              (build-list n
                                          (lambda (c)
                                            (if (odd? (+ r c)) BLK WHT))))))))



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


(@template for-each)

(define (foldfe f1 f2 b lox)
  (local [(define acc b)]
    (begin
      (for-each (lambda (x)
                  (set! acc (f1 acc x)))
                lox)
      (f2 acc))))



;;  13 signature
;;   1 for each correct type parameter
;;   1 for each function type
;;   1 for the (listof  ) type
;;
;;
;;  12 first 2 ce
;;   2 for each correct first 3 operands to foldfe (each is all or nothing) 
;;
;;  8 third ce
;;   2 for realization that compound data is required (all or nothing)
;;   2 for each of first 3 operands to foldfe
;;     1 for working with compound data
;;     1 for correct


(@htdf foldfe)
(@signature (Y X -> Y) (Y -> Z) Y (listof X) -> Z)
;; Accumulate f1 over elements of lox. B is init acc, (f2 acc) produces result.


(check-expect (foldfe + identity 0 (list 2 3 4))  9)
(check-expect (foldfe * identity 1 (list 2 3 4)) 24)

(check-expect (local [(define-struct sc (s c))]
                ;; SC is (make-sc Number Natural); the sum and count so far
                (foldfe (lambda (sc n)
                          (make-sc (+ (sc-s sc) n)
                                   (+ (sc-c sc) 1)))
                        (lambda (sc)
                          (/ (sc-s sc) (sc-c sc)))
                        (make-sc 0 0)
                        (list 2 3 4)))
              3)



(@problem 5)
;; Below is a function that consumes the representation of a simple map, and
;; produces the total combined number of ponds and islands on that map.
;;
;; A pond is a continuous area of water on the map, and an island is a
;; continuous area of land on the map.
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


;;  2 for correct change to signature (must be correct)
;;  2 for good purpose (be somewhat lenient in phrasing, but must be correct)
;;
;;  for each check-expect
;;  2 if correct (be very lenient with order, be lenient with missing points,
;;                but must be basically correct)
;;
;; If signature purpose and tests go in a different direction than we asked for
;; take off all of the points above and grade the rest of the problem with
;; respect to what the signature, purpose and tests said, provided what they
;; said was reasonably close.  Cannot be just return a list of every position.
;; Returning a list of the p/i sizes might be enough.


;;  One accumulator needs to go away, c.
;;  2 points if done properly, include in below if it is renamed.
;; 
;; Two accumulators need to be added, points below are 1 each.
;;
;;  2 type and invariant
;;  2 properly threaded through different functions (one goes through all 3,
;;     one just goes through 2 but OK if it goes through all 3)
;;  2 initial accumulator values correct
;;  2 accumulator update values correct
;;     (composition and contribution - cons and identity)
;;  2 final result correct


(@htdf count-pi)
;(@signature Map -> Natural) ;!!!
(@signature Map -> (listof (listof Pos)))
;; Produce lists of all the positions grouped together into ponds or islands
(check-expect (count-pi (list W W W
                              W L W
                              W W W))
              ;2 ;!!!
              (list (list (make-posn 1 1))
                    (list (make-posn 0 1)
                          (make-posn 0 2)
                          (make-posn 1 2)
                          (make-posn 2 2)
                          (make-posn 2 1)
                          (make-posn 2 0)
                          (make-posn 1 0)
                          (make-posn 0 0))))




(check-expect (count-pi (list W W L
                              W W W
                              L W W))
              ;3 ;!!!
              (list (list (make-posn 0 2))
                    (list (make-posn 2 0))
                    (list (make-posn 1 2)
                          (make-posn 2 2)
                          (make-posn 2 1)
                          (make-posn 0 1)
                          (make-posn 1 1)
                          (make-posn 1 0)
                          (make-posn 0 0))))


(check-expect (count-pi (list W L W L W
                              W W W W W
                              W W W W W
                              W L W L W
                              W W W W W))
              ;5 ;!!!
              (list (list (make-posn 3 3))
                    (list (make-posn 1 3))
                    (list (make-posn 3 0))
                    (list (make-posn 1 0))
                    (list (make-posn 2 0) (make-posn 2 3)
                          (make-posn 4 3) (make-posn 4 4)
                          (make-posn 3 4) (make-posn 2 4)
                          (make-posn 1 4) (make-posn 0 4)
                          (make-posn 0 3) (make-posn 0 2)
                          (make-posn 1 2) (make-posn 2 2)
                          (make-posn 3 2) (make-posn 4 2)
                          (make-posn 4 0) (make-posn 4 1)
                          (make-posn 3 1) (make-posn 2 1)
                          (make-posn 1 1) (make-posn 0 1)
                          (make-posn 0 0))))


(@template Pos (listof Pos) genrec accumulator encapsulated)

(define (count-pi m)  
  (local [(define RANK (sqrt (length m)))
          
          ;; Ordinary args:
          ;;  p is Pos; position the current search traversal is at
          ;;  looking is Surface; the surface the current traversal is looking
          ;;                      for 
          
          ;; Accumulators:
          ;;  !!! delete the first accumulator
          ;;  c(ount) is Natural; number of contiguous ponds/islands seen so far
          ;;  rsf  is (listof (listof Pos)); result so far
          ;;  curr is (listof Pos); all the positions in the current pond or
          ;;                        island
          ;;  unvisited is (listof Pos); every pos not colored by color/one
          ;;  todo is (listof Pos); worklist for color/one and color/todo
          
          ;; Termination:
          ;;  scan, color/one, color/todo visit cells of the map one at
          ;;  a time, stopping when unvisited is empty.
          ;;  Since the map is finite, this will terminate.
          
          
          ;; initiate a new graph traversal at next unvisited cell
          ;; template on (listof Pos), and 2 accumulators
          (define (scan rsf unvisited)
            (cond [(empty? unvisited) rsf]
                  [else
                   (local [(define p (first unvisited))]
                     (color/one p (map-ref m p) rsf empty unvisited empty))]))
          
          ;; generate and traverse the graph of connected cells matching looking
          ;; next two sub-functions templated as traversal of generated graph
          (define (color/one p looking rsf curr unvisited todo)
            (cond [(not (member? p unvisited))
                   (color/todo looking rsf curr unvisited todo)]
                  [(not (surface=? (map-ref m p) looking))
                   (color/todo looking rsf curr unvisited todo)]
                  [else
                   (color/todo looking
                               rsf
                               (cons p curr)
                               (remove p unvisited)
                               (append (next-ps p) todo))]))
          
          (define (color/todo looking rsf curr unvisited todo)
            (cond [(empty? todo) (scan (cons curr rsf) unvisited)]
                  [else
                   (color/one (first todo) looking
                              rsf curr unvisited (rest todo))]))
          
          
          
          
          
          
          
          
          
          
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
    
    
    
    
    
    
    (scan empty
          ;; Build a list of every position in the map.
          (build-list (sqr RANK)
                      (lambda (i)
                        (make-posn (remainder i RANK)
                                   (quotient  i RANK)))))))

