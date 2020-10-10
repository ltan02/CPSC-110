;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-09-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Search Lab

(@assignment lab-09)
(@cwl atan12)

(@problem 1)

(@htdd Clown)
;; Clown is String
;; interp.  The name of a clown
(define M "Marny")
(define N "Nifty")
(define O "Oscar")
(define P "Porky")
(define R "Rascal")
(define S "Sunny")
(define V "Vinny")

(@dd-template-rules atomic-non-distinct)
(define (fn-for-clown c)
  (... c))

;; On-ground students:
;; PRELAB: Define examples of ChoiceState that correspond to the two scenarios
;; (on the lab page)

(@htdd ChoiceState)
(define-struct chst (picked options))
;; ChoiceState is (make-chst (listof Clown) (listof Clown))
;; interp. (make-chst picked options) represents a moment during selection when:
;; - we have selected all of the clowns in picked
;; - we are still deciding whether to pick or skip the clowns in options
;; INVARIANT: all of the clowns in picks and options are unique.
(define CH0 (make-chst (list)       (list M N O P R S V))) ; no selections yet
(define CH1 (make-chst (list M)     (list O P R S V)))     ; M picked, N skipped
(define CH2 (make-chst (list M P V) (list)))               ; picking is complete

(@dd-template-rules compound ;; 2 fields
                    ref      ;; (listof Clown)
                    ref)     ;; (listof Clown)

(define (fn-for-chst c)
  (... (fn-for-loc (chst-picked c))
       (fn-for-loc (chst-options c))))



;; ========================================================
;; Functions

;; On-ground Students:
;; PRE-LAB:
;; Our main task in lab will be to design a function, called solve,
;; that takes a choice state and tries to find a list of clowns that satisfy
;; the criteria based from our choice state. Start the design of solve by
;; writing its wish list entry.

;; You should also identify which template elements you will need to blend
;; together for the solve function.

;; Online Students:
;; Follow the instructions on edX


(@htdf solve)
(@signature ChoiceState -> (listof Clown) or false)
;; produce a list of clowns that satisfy the criteria based from choice state
(check-expect (solve CH0) (list S R P O M))
(check-expect (solve CH2) false)

;(define (solve cs) false) ;stub

(@template genrec backtracking bin-tree)
(define (solve cs0)
  (local [(define (fn-for-cs c)
            (cond [(all-crits? (chst-picked c)) (chst-picked c)]
                  [(empty? (chst-options c)) false]
                  [else
                   (fn-for-locs (next-stage c))]))

          (define (fn-for-locs locs)
            (cond [(empty? locs) false]
                  [else
                   (local [(define try (fn-for-cs (first locs)))]
                     (if (not (false? try))
                         try
                         (fn-for-locs (rest locs))))]))]
    (fn-for-cs cs0)))
            
(@htdf next-stage)
(@signature ChoiceState -> (listof ChoiceState))
;; produces the picked and skipped choice state
(check-expect (next-stage (make-chst (list) (list V R O)))
              (list (make-chst (list V) (list R O))
                    (make-chst (list) (list R O))))
(check-expect (next-stage (make-chst (list V) (list R O)))
              (list (make-chst (list R V) (list O))
                    (make-chst (list V) (list O))))
(check-expect (next-stage (make-chst (list V) (list O)))
              (list (make-chst (list O V) (list))
                    (make-chst (list V) (list))))

;(define (next-stage cs) empty) ;stub

(@template fn-composition)
(define (next-stage cs)
  (list (pick cs)
        (skip cs)))

(@htdf pick)
(@signature ChoiceState -> ChoiceState)
;; produce the next pick
;; ASSUME: cs is not empty
(check-expect (pick (make-chst (list R O) (list M)))
              (make-chst (list M R O) (list)))

;(define (pick cs) cs) ;stub

(@template ChoiceState)
(define (pick cs)
  (make-chst (cons (first (chst-options cs)) (chst-picked cs))
             (rest (chst-options cs))))

(@htdf skip)
(@signature ChoiceState -> ChoiceState)
;; produce the next skip
;; ASSUME: cs is not empty
(check-expect (skip (make-chst (list R O) (list M)))
              (make-chst (list R O) (list)))

;(define (skip cs) cs) ;stub

(@template ChoiceState)
(define (skip cs)
  (make-chst (chst-picked cs) (rest (chst-options cs))))











;; Completed Helper Functions that you will use for the lab

(@htdf all-crits?)
(@signature (listof Clown) -> Boolean)
;; produce true if loc satisfies all the clown crits, otherwise false
(check-expect (all-crits? empty) false)
(check-expect (all-crits? (list "Vinny" "Rascal" "Oscar" "Marny")) true)

(@template use-abstract-fn)
(define (all-crits? loc) 
  (andmap (λ (crit?) (crit? loc))
          (list crit-0? crit-1? crit-2? crit-3? crit-4? crit-5?)))

(@htdf crit-0?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 0: There must be at least four clowns
(check-expect (crit-0? empty) false)
(check-expect (crit-0? (list V R)) false)
(check-expect (crit-0? (list V R S O)) true)
(check-expect (crit-0? (list M V R S O)) true)

(@template (listof Clown))
(define (crit-0? loc)
  (>= (length loc) 4))


(@htdf crit-1?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 1: Exactly two of Rascal, Sunny, and Vinny are selected.
(check-expect (crit-1? empty) false)
(check-expect (crit-1? (list R S V)) false)
(check-expect (crit-1? (list V)) false)
(check-expect (crit-1? (list V R)) true)

(@template (listof Clown))
(define (crit-1? loc)
  (or (and (member? R loc)
           (member? S loc)
           (not (member? V loc)))
      (and (member? R loc)
           (member? V loc)
           (not (member? S loc)))
      (and (member? S loc)
           (member? V loc)
           (not (member? R loc)))))


(@htdf crit-2?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 2: Either Nifty is selected, Oscar is selected, or both
(check-expect (crit-2? empty) false)
(check-expect (crit-2? (list N)) true)
(check-expect (crit-2? (list N O)) true)
(check-expect (crit-2? (list M O)) true)

(@template (listof Clown))
(define (crit-2? loc)
  (if (not (member? N loc))
      (member? O loc)
      true))


(@htdf crit-3?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 3: Oscar cannot be selected unless Rascal is selected.
(check-expect (crit-3? empty) true)
(check-expect (crit-3? (list O)) false)
(check-expect (crit-3? (list O R)) true)

(@template (listof Clown))
(define (crit-3? loc)
  (if (member? O loc)
      (member? R loc)
      true))

(@htdf crit-4?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 4: Porky cannot be selected unless Sunny is selected.
(check-expect (crit-4? empty) true)
(check-expect (crit-4? (list P)) false)
(check-expect (crit-4? (list P S)) true)

(@template (listof Clown))
(define (crit-4? loc)
  (if (member? P loc)
      (member? S loc)
      true))

(@htdf crit-5?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 5: If Marny is selected, then Nifty cannot be selected.
(check-expect (crit-5? empty) true)
(check-expect (crit-5? (list M)) true)
(check-expect (crit-5? (list N)) true)
(check-expect (crit-5? (list M N)) false)

(@template (listof Clown))
(define (crit-5? loc)
  (if (member? M loc)
      (not (member? N loc))
      true))
