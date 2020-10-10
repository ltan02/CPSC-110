;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2019W2-MT1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)



(@problem 1)
;;
;; PUT YOUR CS ID HERE ON THIS LINE: ____________________________
;;



(@problem 2)
;;
;; Given the following definitions:
;;

(define LEN 10)

(define (foo a b)
  (cond [(and (> (string-length a) LEN) (< b LEN)) b]
        [(< b LEN) (sqr b)]
        [else 0]))

;; Show the next 7 steps of the evaluation for the following expression,
;; not counting the expression itself

(foo (string-append "cat" "sings") 4)   

































;; this page is blank for the solution to problem 2 































































(@problem 3)
;;
;; The following function design may have one or more errors and/or other
;; design flaws.  Please fix the error(s) and/or flaw(s) that you find.
;; PLEASE NOTE THAT:
;;
;;   - Any changes you make should preserve the existing design intent.
;;   - Don't change things that do not need to change.  Do not entirely
;;     rewrite the function to fix a small problem or small problems.
;;

(@htdf bigger)
(@signature Number Image -> Boolean)
;; produce true if img perimeter is bigger than given value, false otherwise

(check-expect (bigger (rectangle 10 5 "solid" "blue") 20) true)
(check-expect (bigger (rectangle 10 5 "solid" "blue") 40) false)


;(define (bigger img n) img)   ;stub

(@template Image)

(define (bigger img n)
  (> ((* (image-width img) 2) + (* (image-height img) 2))
     n))




































(@problem 4)
;;
;; Please write the @dd-template-rules tag and template for the following
;; type comment.  You do not need to simplify questions, but you do need
;; to include all required guards.


(@htdd Gift)
(define-struct outfit (size designer))
;; Gift is one of:
;;  - (make-outfit Natural String)
;;  - "Dinner"
;;  - Number
;; interp. a gift for a person
;; CONSTRAINT: If Number must be greater than 0

(define G1 (make-outfit 10 "Gucci"))
(define G2 "Dinner")
(define G3 50)











































(@problem 5)
;;
;; The HtDW program below is a variation of the Balloon program from Lab 3.
;; Complete the function definitions for tick and handle-key based on the
;; partial designs below.
;; While our check-expects are not exhaustive you do not need to write any
;; additional check-expects. 



;; Balloon program with key control


(@htdw Balloon)
;; =================
;; Constants:
(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))
(define POP-IMAGE (overlay (text "POP!" 80 "black")
                           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2)
                                        "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define MIN 10)
(define MAX (/ WIDTH 2))

;; =================
;; Data definitions:
(@htdd Balloon)
(define-struct balloon (r c dr))
;; Balloon is one of:
;; - false
;; - (make-balloon Natural String Integer)
;; interp. a balloon is either popped (false) or has a r(adius) in pixels,
;;         c(olor), and dr is the rate per tick at which the radius changes,
;;         dr is positive if the balloon is getting bigger or negative if
;;         the balloon is shrinking. 
;; CONSTRAINT: Balloon radius is in [MIN, MAX], rate is never 0.

(define B0 false)
(define B1 (make-balloon MIN "red" -3))
(define B2 (make-balloon MAX "green" 2))

(@dd-template-rules one-of
                    atomic-distinct
                    compound)

(define (fn-for-balloon b)
  (cond [(false? b) (...)]
        [else (... (balloon-r b)
                   (balloon-c b)
                   (balloon-dr b))]))








;; =================
;; Functions:

(@htdf main)
(@signature Balloon -> Balloon)
;; starts the world program with (main B1)
;; no tests for main function

(@template htdw-main)
(define (main b)
  (big-bang b
    (on-tick tick)          ; Balloon -> Balloon
    (to-draw render)        ; Balloon -> Image
    (on-key handle-key)))   ; Balloon KeyEvent -> Balloon


(@htdf tick)
(@signature Balloon -> Balloon)
;; change balloon radius by its rate. At MIN, stops shrinking, above MAX pops.
              
(check-expect (tick B1) B1) 
(check-expect (tick B2) false)
(check-expect (tick false) false)
(check-expect (tick (make-balloon (+ MIN 3) "blue" -4))
              (make-balloon MIN "blue" -4))

(check-expect (tick (make-balloon (+ MIN 3) "blue" -2)) 
              (make-balloon (+ MIN 1) "blue" -2))

(check-expect (tick (make-balloon (- MAX 2) "green" 3))
              false)

(check-expect (tick (make-balloon (- MAX 2) "green" 1))
              (make-balloon (- MAX 1) "green" 1))

(check-expect (tick (make-balloon (- MAX 2) "green"  2))
              (make-balloon  MAX "green" 2))


(define (tick b) b)   ; stub

;; put the template tag and function definition for tick on the next page




















;; put the template tag and function definition for tick on this page






























































(@htdf render)
(@signature Balloon -> Image)
;; draws the balloon as a circle with its size and color on MTS, unless popped

(check-expect (render B1) (place-image (circle MIN "solid" "red")
                                       CTR-X
                                       CTR-Y
                                       MTS))
(check-expect (render false) (place-image POP-IMAGE CTR-X CTR-Y MTS))

; (define (render b) MTS)   ; stub

(@template Balloon)
(define (render b)
  (cond [(false? b) (place-image POP-IMAGE CTR-X CTR-Y MTS)]
        [else
         (place-image (circle (balloon-r b) "solid" (balloon-c b))
                      CTR-X
                      CTR-Y
                      MTS)]))
       




(@htdf handle-key)
(@signature Balloon KeyEvent -> Balloon)
;; on space flip rate from + to - or vice versa; on p change color to purple

(check-expect (handle-key B0 " ") B0)
(check-expect (handle-key B0 "p") B0)
(check-expect (handle-key B0 "down") B0)
(check-expect (handle-key B1 " ") (make-balloon MIN "red" 3))
(check-expect (handle-key B2 " ") (make-balloon MAX "green" -2))
(check-expect (handle-key B1 "p") (make-balloon MIN "purple" -3))
(check-expect (handle-key B2 "up") B2)

(define (handle-key b ke) b);  ; stub


;; put the template tag and function definition for handle-key on the next page





















;; put the template tag and function definition for handle-key on this page































































(@problem 6)
;;
;; Consider the following data definitions.
;;

(@htdd Item)
(define-struct item (n c ft?))
;; Item is (make-item String Number Boolean)
;; interp. a store item, with its n(ame), c(ost) (in Canadian dollars),
;;         and whether it is fair trade
;; CONSTRAINT: cost must always be greater than 0

(define I0 (make-item "Mug" 8 true))
(define I1 (make-item "Comb" 14 false))
(define I2 (make-item "Kite" 100 true))

(@dd-template-rules compound)

(define (fn-for-item i)
  (... (item-n i)
       (item-c i)
       (item-ft? i)))


(@htdd ListOfItem)
;; ListOfItem is one of:
;;  - empty
;;  - (cons Item ListOfItem)
;; interp. a list of items

(define LOI0 empty)
(define LOI1 (cons I0 empty))
(define LOI2 (cons I0 (cons I1 empty)))
(define LOI3 (cons I0 (cons I1 (cons I2 empty))))


(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-item (first loi))
              (fn-for-loi (rest loi)))]))
  














;;
;; Due to changes on the global market, the Canadian-based store has to change
;; the cost of all fair trade items by a certain factor. Please design a
;; function, name it update, that consumes a list of items and a factor and
;; produces a list of items with the updated prices.
;;
;; The factor is represented as a positive number and the new price of a fair
;; trade item is calculated as follows
;;     new-price = (current-price * factor)
;;
;; For example 
;; If the factor is 2 and the current price of a fair trade item is 10 dollars
;; The new price will be = (10 * 2) = 20
;; If the factor is 0.5 and the current price of a fair trade item is 10 dollars
;; The new price will be = (10 * 0.5) = 5
;;
;; You must follow all applicable design rules and include all
;; appropriate @tags.  Please use the Item and ListOfItem example constants
;; defined above in your check-expects.
;;










































;; this page is blank for the solution to problem 6





























































;; this page is blank for the solution to problem 6











































