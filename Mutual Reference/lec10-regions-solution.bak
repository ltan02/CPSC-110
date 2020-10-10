;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lec10-regions-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

;;
;; Region and ListOfRegion data definitions provided.  
;; 
(@htdd Region ListOfRegion)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given single region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ss and Gs are Regions
(define S1 (make-single "one" 20 "red"))
(define S2 (make-single "two" 40 "blue"))
(define S3 (make-single "three" 60 "orange"))
(define S4 (make-single "four" 30 "black"))
(define S5 (make-single "five" 50 "purple"))
(define S6 (make-single "six" 80 "black"))

(define G1 (make-group "red"  (list S1 S2 S3)))
(define G2 (make-group "blue" (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black" (list G2 G3)))

(define LORE empty)
(define LOR123 (list S1 S2 S3))

(define (fn-for-region r)
  (cond [(single? r)
         (... (single-label r)
              (single-weight r)
              (single-color r))]
        [(group? r)
         (... (group-color r)
              (fn-for-lor (group-subs r)))]))

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))



(@htdd ListOfString)

(@problem 1)
;; Design a function that produces the total weight of a region

(@htdf total-weight--region total-weight--lor)
(@signature Region -> Integer)
(@signature ListOfRegion -> Integer)
;; produce total weight of region / list of region
(check-expect (total-weight--region S1) 20)
(check-expect (total-weight--region S2) 40)
(check-expect (total-weight--lor empty) 0)
(check-expect (total-weight--lor (list S1 S2 S3)) (+ 20 40 60))             
(check-expect (total-weight--region (make-group "red" empty)) 0)
(check-expect (total-weight--region G1) (+ 20 40 60))
(check-expect (total-weight--region G4) (+ 20 40 60 30 50 80))

(@template Region)

(define (total-weight--region r)
  (cond [(single? r) (single-weight r)]
        [(group?  r) (total-weight--lor (group-subs r))]))

(@template ListOfRegion)

(define (total-weight--lor lor)
  (cond [(empty? lor) 0]
        [else
         (+ (total-weight--region (first lor))
            (total-weight--lor (rest lor)))]))




(@problem 2)
;; Design a function that consumes a region and a string
;; and produces a list of all contained regions with the given color.
;; Include the root if it has that color.   Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@htdf all-with-color--region all-with-color--lor)
(@signature String Region -> ListOfRegion)
(@signature String ListOfRegion -> ListOfRegion)
;; produce all regions with given color
(check-expect (all-with-color--lor "red" empty) '())
(check-expect (all-with-color--region "red" S1) (list S1))
(check-expect (all-with-color--region "blue" S1) '())
(check-expect (all-with-color--lor "red" LOR123) (list S1))
(check-expect
 (all-with-color--region "red"
                         (make-group "blue"
                                     (list G4
                                           (make-single "X" 90 "red"))))
 (list G1 S1 (make-single "X" 90 "red")))

(@template Region)

(define (all-with-color--region c r)
  (cond [(single? r) (if (string=? (single-color r) c) (list r) '())]
        [(group?  r) (if (string=? (group-color r) c)
                         (cons r (all-with-color--lor c (group-subs r)))
                         (all-with-color--lor c (group-subs r)))]))

(@template ListOfRegion)

(define (all-with-color--lor c lor)
  (cond [(empty? lor) empty]
        [else
         (append (all-with-color--region c (first lor))
                 (all-with-color--lor c (rest lor)))]))




(@problem 3)
;; Design a function that consumes a region and a string
;; and looks for a region with the given label.  If there is one
;; the function should produce the first one it finds.  If there is
;; not one it should signal failure by producing false.
;; The signature for the function is given below.   Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@signature String Region -> Region or false)
(@signature String ListOfRegion  -> Region or false)

(@htdf find-region--region find-region--lor)
(@signature Region String -> Region or false)
(@signature ListOfRegion String -> Region or false)
;; find region w/ given label
(check-expect (find-region--lor "one" empty) false)
(check-expect (find-region--region "one" S1) S1)
(check-expect (find-region--region "one" S2) false)
(check-expect (find-region--lor "two" LOR123) S2)
(check-expect (find-region--region "three" G4) S3)

(@template Region backtracking)

(define (find-region--region l r)
  (cond [(single? r) (if (string=? (single-label r) l) r false)]
        [(group?  r) (find-region--lor l (group-subs r))]))

(@template ListOfRegion backtracking)

(define (find-region--lor c lor)
  (cond [(empty? lor) false]
        [else
         (if (not (false? (find-region--region c (first lor))))
             (find-region--region c (first lor))
             (find-region--lor c (rest lor)))]))






(@problem 4)
;; Design a function that consumes a region and
;; produces a list of all the labels in the region.  Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@htdf all-labels--region all-labels--lor)
(@signature Region -> ListOfString)
(@signature ListOfRegion -> ListOfString)
;; produce labels of all regions in region (including root)
(check-expect (all-labels--lor empty) '())
(check-expect (all-labels--region S1) (list "one"))
(check-expect (all-labels--lor LOR123) (list "one" "two" "three"))
(check-expect (all-labels--region G4)
              (list "one" "two" "three"
                    "four" "five" "six"))

(@template Region)

(define (all-labels--region r)
  (cond [(single? r) (list (single-label r))]
        [(group?  r) (all-labels--lor (group-subs r))]))

(@template ListOfRegion)

(define (all-labels--lor lor)
  (cond [(empty? lor) empty]
        [else
         (append (all-labels--region (first lor))
                 (all-labels--lor (rest lor)))]))



(@problem 5)
;; Design a function that renders a region and its subregions as
;; nested boxes. The rendering does not have to be pretty, but it must somehow
;; reflect the coloring, the labels and the weights.  The border function below
;; may be helpful to you.   Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@htdf render--region render--lor)
(@signature Region -> Image)
(@signature ListOfRegion -> Image)
;; produce simple nested rendering of region
(check-expect (render--lor empty) empty-image)
(check-expect (render--region S1) (text "one" 20 "red"))
(check-expect (render--lor LOR123) (beside (render--region S1)
                                           (render--region S2)
                                           (render--region S3))) 

(check-expect (render--region G1)
              (border "red" (render--lor (group-subs G1))))


(@template Region)


(define (render--region r)
  (cond [(single? r)
         (text (single-label r)
               (single-weight r)
               (single-color r))]
        [(group? r)
         (border (group-color r)
                 (render--lor (group-subs r)))])) 

(@template ListOfRegion)



(define (render--lor lor)
  (cond [(empty? lor) empty-image]
        [else
         (beside (render--region (first lor))
                 (render--lor (rest lor)))]))


(define BORDER-THICKNESS 10)

(@htdf border)
(@signature Color Image -> Image)
;; add a border of the given color around img
(check-expect (border "red" (rectangle 50 100 "solid" "blue"))
              (overlay (rectangle 50 100 "solid" "blue")
                       (rectangle 50 100 "solid" "white")
                       (rectangle (+ 50 BORDER-THICKNESS)
                                  (+ 100 BORDER-THICKNESS)
                                  "solid"
                                  "red")))
(check-expect (border "orange" (rectangle 60 70 "solid" "blue"))
              (overlay (rectangle 60 70 "solid" "blue")
                       (rectangle 60 70 "solid" "white")
                       (rectangle (+ 60 BORDER-THICKNESS)
                                  (+ 70 BORDER-THICKNESS)
                                  "solid"
                                  "orange")))

(define (border c img)
  (overlay img
           (rectangle (image-width img)
                      (image-height img)
                      "solid"
                      "white")
           (rectangle (+ (image-width img) BORDER-THICKNESS)
                      (+ (image-height img) BORDER-THICKNESS)
                      "solid"
                      c)))
           








