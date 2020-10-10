;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lec14-hp-family-tree-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

;; lec14-hp-family-tree-solution.rkt

;; Data definitions:

(@htdd Wizard ListOfWizard)

(define-struct wiz (name wand patronus kids))
;; Wizard is (make-wiz String String String ListOfWizard)
;; interp. a wizard in a descendant family tree
;;         name is the first name
;;         wand is the wood their primary wand is made of ("" if unknown)
;;         patronus is a string  ("" if unknown)
;;         kids is their immediate children

;;
;; ListOfWizard is one of:
;;  - empty
;;  - (cons Wizard ListOfWizard)
;; interp. a list of wizards

(define ARTHUR
  (make-wiz "Arthur" "" "Weasel"
            (list (make-wiz "Bill" "" ""
                            (list (make-wiz "Victoire"  "" "" empty)
                                  (make-wiz "Dominique" "" "" empty)  ;optional
                                  (make-wiz "Louis"     "" "" empty)));optional
                  (make-wiz "Charlie" "ash" "" empty)
                  (make-wiz "Fred"    ""    "" empty)
                  (make-wiz "George"  ""    "" empty)
                  (make-wiz "Ron"     "ash" "Jack Russell Terrier" empty)
                  (make-wiz "Ginny"   ""    "horse" 
                            (list (make-wiz "James" "" "" empty)
                                  (make-wiz "Albus" "" "" empty)
                                  (make-wiz "Lily"  "" "" empty))))))

#| PROBLEM 3: Use local to encapsulate these templates. |#

(@template Wizard ListOfWizard encapsulated)

(define (fn-for-wizard w)
  (local [(define (fn-for-wizard w)
            (... (wiz-name w)
                 (wiz-wand w)
                 (wiz-patronus w)
                 (fn-for-low (wiz-kids w))))

          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wizard (first low))
                        (fn-for-low (rest low)))]))]

    (fn-for-wizard w)))



;; ListOfPair is one of:
;;  - empty
;;  - (cons (list String String) ListOfPair)
;; interp. used to represent an arbitrary number of pairs of strings
(define LOP1 empty)
(define LOP2 (list (list "Harry" "stag") (list "Hermione" "otter")))
#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (first (first lop))
              (second (first lop)) ;(first (rest (first lop)))
              (fn-for-lop (rest lop)))]))


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (list "a" "b"))

(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
(@htdf patroni)
(@signature Wizard -> ListOfPair)
;; produce every wizard in tree together with their patronus
(check-expect (patroni (make-wiz "a" "b" "c" empty))
              (list (list "a" "c")))
(check-expect (patroni ARTHUR)
              (list (list "Arthur" "Weasel")
                    (list "Bill" "")
                    (list "Victoire" "")
                    (list "Dominique" "")
                    (list "Louis" "")
                    (list "Charlie" "")
                    (list "Fred" "")
                    (list "George" "")
                    (list "Ron" "Jack Russell Terrier")
                    (list "Ginny" "horse")
                    (list "James" "")
                    (list "Albus" "")
                    (list "Lily" "")))

#| PROBLEM 1: Refactor using local to encapsulate. |#

(@template Wizard ListOfWizard encapsulated)

(define (patroni w)
  (local [(define (patroni--wiz w)
            (cons (list (wiz-name w)
                        (wiz-patronus w))
                  (patroni--low (wiz-kids w))))
          (define (patroni--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (patroni--wiz (first low))
                           (patroni--low (rest low)))]))]

    (patroni--wiz w)))


(@htdf has-wand-of-wood)
(@signature Wizard String -> ListOfString)
;; produce names of all descendants with wand of given wood (including w)
(check-expect (has-wand-of-wood (make-wiz "a" "b" "c" empty) "x") empty)
(check-expect (has-wand-of-wood (make-wiz "a" "b" "c" empty) "b")
              (list "a"))                                           
(check-expect (has-wand-of-wood ARTHUR "ash")
              (list "Charlie" "Ron"))

#| PROBLEM 2: Refactor using local to encapsulate. |#

(@template Wizard ListOfWizard encapsulated)

(define (has-wand-of-wood w wood)
  (local [(define (has-wand-of-wood--wiz w)
            (if (string=? (wiz-wand w) wood)
                (cons (wiz-name w)
                      (has-wand-of-wood--low (wiz-kids w)))
                (has-wand-of-wood--low (wiz-kids w))))
          (define (has-wand-of-wood--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (has-wand-of-wood--wiz (first low))
                           (has-wand-of-wood--low (rest low)))]))]

    (has-wand-of-wood--wiz w)))


#|
PROBLEM 4:

Design a function that consumes a wizard family tree and a string and
searches the tree for a wizard with the given name. If found it should
produce the wizard, if not found it should produce false.

Use these encapsulated templates.
|#

(@htdf find-wiz)
(@signature Wizard String -> Wizard or false)
;; search for wizard with given name, if found produce it, otherwise false
(check-expect (find-wiz (make-wiz "James" "" "" empty) "James")
              (make-wiz "James" "" "" empty))
(check-expect (find-wiz (make-wiz "James" "" "" empty) "Ron")
              false)
(check-expect (find-wiz ARTHUR "Ron") 
              (make-wiz "Ron"     "ash" "Jack Russell Terrier" empty))
(check-expect (find-wiz ARTHUR "Sally") 
              false)

(@template Wizard ListOfWizard encapsulated backtracking)

(define (find-wiz w n)
  (local [(define (fn-for-wizard w)
            (if (string=? (wiz-name w) n)
                w
                (fn-for-low (wiz-kids w))))
          
          (define (fn-for-low low)
            (cond [(empty? low) false]
                  [else
                   
                   (local [(define try (fn-for-wizard (first low)))]
                     (if (not (false? try))
                         try
                         (fn-for-low (rest low))))]))]
    
    (fn-for-wizard w)))



