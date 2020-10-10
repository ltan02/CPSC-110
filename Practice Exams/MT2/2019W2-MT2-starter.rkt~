;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2019W2-MT2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment mt-2)

(@cwl ???) ;<<< replace ??? with your cwl.


(@problem 1)
;;
;; Write the signature for the following abstract function.
;;   Do your SCRATCH WORK SOMEWHERE ELSE, and then 
;;   put your FINAL ANSWER in the @signature ABOVE the function definition.
;;

;; Uncomment the following line and put your signature in the tag as usual.
;(@signature )


(define (bar p q r s a b)
  (cond [(zero? a) r] 
        [(empty? b) s] 
        [else
         (q (p a (first b))
            (bar p q r s (sub1 a) (rest b)))]))





;; ---- PLEASE REMEMBER TO USE SPD HANDIN ABOUT EVERY 20 MINUTES ----

(@problem 2)
;;
;; Consider the following data definition and fold function
;;

(@htdd Dir)
(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String (listof Dir) (listof Image))
;; interp. a directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

(define I1 (square 20 "solid" "red"))
(define I2 (square 15 "solid" "green"))
(define I3 (square 10 "solid" "blue"))
(define I4 (square 25 "solid" "black"))
(define I5 (square 30 "solid" "purple"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) (list I4 I5)))

#;
(define (fn-for-dir d)
  (local [(define (fn-for-dir d)
            (... (dir-name d)
                 (fn-for-lod (dir-sub-dirs d))
                 (fn-for-loi (dir-images d))))

          (define (fn-for-lod lod)
            (cond [(empty? lod) (...)]
                  [else
                   (... (fn-for-dir (first lod))
                        (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)
            (cond [(empty? loi) (...)]
                  [else
                   (... (first loi)
                        (fn-for-loi (rest loi)))]))]
    (fn-for-dir d)))


(@htdf fold-dir)
(@signature (String Y Z -> X) (X Y -> Y) (Image Z -> Z) Y Z -> X)    
;; abstract fold for Dir

(check-expect (fold-dir make-dir cons cons empty empty D6) D6)

(@template Dir (listof Dir) (listof Image) encapsulated)

(define (fold-dir c1 c2 c3 b1 b2 d)
  (local [(define (fn-for-dir d)                          
            (c1  (dir-name d)
                 (fn-for-lod (dir-sub-dirs d)) 
                 (fn-for-loi (dir-images d))))  

          (define (fn-for-lod lod)                       
            (cond [(empty? lod) b1]
                  [else
                   (c2 (fn-for-dir (first lod))
                       (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)                       
            (cond [(empty? loi) b2]
                  [else
                   (c3 (first loi)
                       (fn-for-loi (rest loi)))]))]
    (fn-for-dir d)))

;;
;; Complete the design of the function below.
;; The body of your function MUST USE the abstract fold function for Dir.
;; Solutions that do not use fold-dir will NOT be graded.

(@htdf count-imgs-with-height>n)
(@signature Dir Natural -> Natural)
;; produce the count of images in dir with image-height > n

(check-expect (count-imgs-with-height>n D5 10) 0)
(check-expect (count-imgs-with-height>n D6 15) 3)
(check-expect (count-imgs-with-height>n D6 20) 2)

(define (count-imgs-with-height>n dir n) 0) ;stub








;; ---- PLEASE REMEMBER TO USE SPD HANDIN ABOUT EVERY 20 MINUTES ----

(@problem 3)
;;
;; Complete the design for a function called pyramid that consumes a natural
;; number and produces a pyramid of circles that high.
;;
;; The body of your function definition MUST USE built-in abstract functions.
;; Solutions which use part of the list template will NOT be graded
;;
;; Make sure that you take some time to understand the given check-expects.
;;

(@htdf pyramid)
(@signature Natural -> Image)
;; produce a pyramid of n circles

(check-expect (pyramid 0) empty-image)
(check-expect (pyramid 3) (above (circle  10 "solid" "black")
                                 (above (beside (circle  10 "solid" "black")
                                                (circle  10 "solid" "black"))
                                        (beside (circle  10 "solid" "black")
                                                (circle  10 "solid" "black")
                                                (circle  10 "solid" "black")))))

(define (pyramid n)  empty-image) ; stub




;; ---- PLEASE REMEMBER TO USE SPD HANDIN ABOUT EVERY 20 MINUTES ----

(@problem 4)
;;
;; You are asked to implement the four abstract list functions andmap, ormap,
;; map, and filter.  Because foldr is the abstract function for the (listof X)
;; template itself, you know that some of these could be implemented using
;; foldr.
;;
;; For each of the 4 function definitions below, you should do one and only
;; one of two things:
;;   - Code the body of the function so that it calls foldr. Your
;;     implementation using foldr must have the complete correct behaviour
;;     of the built-in you are implementing.
;; 
;;   - Leave the function definition as is, but write a comment above the
;;     function explaining why calling foldr will not work properly.
;;



(@htdf andmap2)
(@signature (X -> Boolean) (listof X) -> Boolean)
;; produce false as soon as p produces false for one element of lox
(check-expect (andmap2 odd?  empty) true)
(check-expect (andmap2 odd?  (list 1 3)) true)
(check-expect (andmap2 even? (list 1 2 3)) false)

(@template use-abstract-fn)

;; Either revise the function definition below to call foldr, OR leave
;; the function definition as is and put a comment above the function
;; definition explaining why calling foldr will not work properly.

(define (andmap2 p lox) false)





(@htdf ormap2)
(@signature (X -> Boolean) (listof X) -> Boolean)
;; produce true as soon as p produces true for one element of lox
(check-expect (ormap2 odd?  empty) false)
(check-expect (ormap2 even? (list 1 3))   false)
(check-expect (ormap2 even? (list 1 2 3)) true)

(@template use-abstract-fn)

;; Either revise the function definition below to call foldr, OR leave
;; the function definition as is and put a comment above the function
;; definition explaining why calling foldr will not work properly.

(define (ormap2 p lox) false)




(@htdf filter2)
(@signature (X -> Boolean) (listof X) -> (listof X))  
;; produce list containing only those items for which p produces true
(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 negative? (list 1 -2 3 -4)) (list -2 -4))
(check-expect (filter2 positive? (list 1 -2 3 -4)) (list 1 3))


(@template use-abstract-fn)

;; Either revise the function definition below to call foldr, OR leave
;; the function definition as is and put a comment above the function
;; definition explaining why calling foldr will not work properly.

(define (filter2 p lox) empty)


(@htdf map2)
(@signature (X ->  Y) (listof X) -> (listof Y))
;; given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 2 4)) (list 4 16))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))
(check-expect (map2 abs (list 2 -3 4)) (list 2 3 4))
(check-expect (map2 string-length (list "a" "bc" "def")) (list 1 2 3))

(@template use-abstract-fn)

;; Either revise the function definition below to call foldr, OR leave
;; the function definition as is and put a comment above the function
;; definition explaining why calling foldr will not work properly.

(define (map2 p lox) empty)




;; ---- PLEASE REMEMBER TO USE SPD HANDIN ABOUT EVERY 20 MINUTES ----

(@problem 5)
;;
;; Design a function called group that consumes a list of numbers and groups
;; numbers that are the same into ordered sublists. For example:
;;
;;  (group (list 4 1 6 1 6)) --> (list (list 1 1) (list 4) (list 6 6))
;;
;; IMPORTANT: Your design must include all relevant design elements.
;;
;; NOTE: We have provided you with the list-min helper, which was useful in
;;       our solution.

;;

;(@htdf group) ;uncomment this line when you start








(@signature (listof Number) -> Number)
;; produce minimum number in list
;; CONSTRAINT: lon must have at least one element
(check-expect (list-min (list 1)) 1)
(check-expect (list-min (list 0)) 0)
(check-expect (list-min (list  3 2 1 5 9 0)) 0)

(@template use-abstract-fn)

(define (list-min lon)
  (foldr min (first lon) (rest lon)))




;; ---- PLEASE REMEMBER TO USE SPD HANDIN ABOUT EVERY 20 MINUTES ----

(@problem 6)
;;
;; Design a function called sq-frac that consumes a positive number and
;; produces a fractal image like the ones shown in the exam supplement.
;;
;; To save writing we are giving you a simple function called sq that consumes
;; a number and produces a black solid square of that side-length.  This will
;; save you having to write "solid" "black" multiple times.
;;
;; You must include all relevant design recipe elements, including a three part
;; termination argument.
;;

(define CUTOFF 10)

;(@htdf sq-frac) ; Uncomment this line and complete the design of sq-frac.


(@htdf sq)
(@signature Number -> Image)
;; produce solid black square of given side-length

(check-expect (sq 10) (square 10 "solid" "black"))
(check-expect (sq 20) (square 20 "solid" "black"))

(@template Number)

(define (sq s)
  (square s "solid" "black"))


































