;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 2017W2-MT2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@problem 1)
;; Structure definitions, type comments, interpretations, and
;; examples have been provided for Person and FamilyTree below.
;;
;; Write the function templates for both Person and FamilyTree.
;;
;; You do not need to provide the template rules used.


(@htdd Person)
(define-struct person (name age location))
;; Person is (make-person String Natural String)
;; interp. a person with a name, age, and city they were born
(define P1 (make-person "Austin"    12 "Vancouver"))
(define P2 (make-person "Genevieve" 38 "Paris"))
(define P3 (make-person "Pierre"    63 "Paris"))
(define P4 (make-person "Anna"      59 "Geneva"))
(define P5 (make-person "Will"      35 "Seattle")) 
(define P6 (make-person "Haruto"    68 "Tokyo"))


(@htdd FamilyTree)
(define-struct node (p l r))
;; FamilyTree is one of:
;;  - false
;;  - (make-node Person FamilyTree FamilyTree)
;; interp. false means there is no information about the family member
;;         p is the information on a person in the family tree
;;         l and r are left and right subtrees
(define ANNA (make-node P4 false false))
(define PIERRE (make-node P3 false false))
(define GENEVIEVE (make-node P2 ANNA PIERRE))

(define AUSTIN (make-node P1
                          GENEVIEVE
                          (make-node P5
                                     false
                                     (make-node P6 false false))))


;; COMPLETE NECESSARY FUNCTION TEMPLATES HERE:

(define (fn-for-person p)
  (... (person-name p)
       (person-age p)
       (person-location p)))

(define (fn-for-ft ft)
  (cond [(false? ft) (...)]
        [else
         (... (fn-for-person (ft-p ft))
              (fn-for-ft (ft-l ft))
              (ft-for-ft (ft-r ft)))]))

(@problem 2)
;; Design a function that consumes a list of strings and a list of natural
;; numbers.
;;
;; The function produces a single string by appending some of the strings from
;; the list together.
;; Each number in the list of numbers indicates the maximum length of a string
;; that will be kept.
;; If a string exceeds the maximum length it will not be included.
;;
;; For example:
;; Given the lists: (list "ab" "cdef" "ghi") (list 2 3 5)
;; the function would produce the string "abghi" because:
;;  - the length of "ab" is not greater than 2  (included)
;;  - the length of "cdef" is greater than 3    (not included)
;;  - the length of "ghi" is not greater than 5 (included)
;;
;; If there are no remaining numbers in the list of numbers, all remaining
;; strings are appended.
;;
;; For example: 
;; Given the lists: (list "ab" "cdef" "ghi") (list 1)
;; the function would produce: "cdefghi"
;;
;; If there are no remaining strings in the list of strings, then no more
;; strings are appended.
;;
;; For example:
;; Given the lists: (list "ab" "cdef" "ghi") (list 1 6 5 8 9)
;; the function would produce: "cdefghi"
;;
;; If there are no strings in the list of strings, or no strings have a small
;; enough length, then an empty string with no characters is produced: "".
;;
;; 
;; Provide a full function design including all steps.
;; We have provided this page and the next page for you to write your solution.

(@htdf combine)
(@signature (listof String) (listof Natural) -> String)
;; produces strings from the list together when length !> number in list
(check-expect (combine empty empty) "")
(check-expect (combine empty (list 1)) "")
(check-expect (combine (list "ad") (list 1)) "")
(check-expect (combine (list "adc" "abc") empty) "adcabc")
(check-expect (combine (list "ab" "cdef" "ghi") (list 1)) "cdefghi")
(check-expect (combine (list "ab" "cdef" "ghi") (list 2 3 5)) "abghi")
(check-expect (combine (list "ab" "cdef" "ghi") (list 1 6 5 8 9)) "cdefghi")

;(define (combine los lon) "") ;stub

(define (combine los lon)
  (cond [(empty? los) ""]
        [(empty? lon)
         (string-append (first los)
                 (combine (rest los) empty))]
        [else
         (if (not (> (string-length (first los)) (first lon)))
             (string-append (first los)
                     (combine (rest los) (rest lon)))
             (combine (rest los) (rest lon)))]))
        
;; This page has been provided to give you extra space to complete Problem 1.  

;; Problem 3 setup:
;;
;; A University degree program has a name a list of courses required to
;; graduate.
;;
;; Each course has a department name, a course number, and a list of textbooks.
;;
;; Each textbok has a title, an author, and a number of pages.
;;
;; A full data definition is provided for you to represent all the necessary
; ;information about a university degree program below. 


;; =================
;; Constants:

(define FONT-SIZE 14)
(define FONT-COLOUR "red")
(define PAGE-COUNT-THRESHOLD 100)



;; =================
;; Data definitions:

(@htdd Textbook)
(define-struct textbook (title author pages))
;; Textbook is (make-textbook String String Natural)
;; interp. a textbook for a university course with a title, author, and number
;;         of pages
(define T1 (make-textbook "Organization Behaviour"
                          "Henry Angus Sauder"            85))
(define T2 (make-textbook "Interpersonal Processes"
                          "David Lam"                     60))
(define T3 (make-textbook "How to Design Programs"
                          "Felleisen, Findler, and Flatt" 100))
(define T4 (make-textbook "Systematic Program Design"
                          "Hugh Dempster"                 111))
(define T5 (make-textbook "Course Companion"
                          "Anonymous"                     83))
(define T6 (make-textbook "Differential Calculus"
                          "Feldman, Rechnizter, Yeager"   200))


(@htdd Course)
(define-struct course (dept num textbooks))
;; Course is (make-course String Natural (listof Textbook))
;; interp. a university course with a department name, number, and
;;         list of required textbooks
(define C0 (make-course "CPSC" 121 empty))
(define C1 (make-course "COMM" 329 (list T1 T2)))
(define C2 (make-course "CPSC" 110 (list T3 T4 T5)))
(define C3 (make-course "MATH" 100 (list T6)))


(@htdd Program)
(define-struct program (name courses))
;; Program is (make-program String (listof Course))
;; interp. a degree program at a University with the name of the degree program,
;;         and list of courses needed to graduate
(define P-MT (make-program "undeclared" empty))
(define P-BUCS (make-program "BUCS" (list C0 C1 C2 C3)))

#;
(define (fn-for-program p)
  (local [(define (fn-for-textbook t)
            (... (textbook-title t)
                 (textbook-author t)
                 (textbook-pages t)))
          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-textbook (first lot))
                        (fn-for-lot (rest lot)))]))       
          (define (fn-for-course c)
            (... (course-dept c)
                 (course-num c)
                 (fn-for-lot (course-textbooks c))))
          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-course (first loc))
                        (fn-for-loc (rest loc)))]))
          (define (fn-for-program p)
            (... (program-name p)
                 (fn-for-loc (program-courses p))))]
    (fn-for-program p)))


(@problem 3)
;; Refactor the following complete function design WHEREVER POSSIBLE using     
;; BUILT-IN abstract functions. You must not define any new abstract functions,
;; we want you to use built-in abstract functions.
;; Marks will be deducted for attempts to refactor functions that cannot be
;; refactored.
;;
;; Do not rewrite everything.  Cross out the fuction you are choosing to
;; refactor and write your version clearly beside it.


(@htdf without-large-textbook)
(@signature Program -> Image)
;; produce an image of courses in p without a textbook with pages > threshold
(check-expect (without-large-textbook P-MT) empty-image)
(check-expect (without-large-textbook P-BUCS)
              (above (text "CPSC121" FONT-SIZE FONT-COLOUR)
                     (text "COMM329" FONT-SIZE FONT-COLOUR)))

(@template Course (listof Course) Textbook (listof Textbook) Program
           encapsulated)

(define (without-large-textbook p)
  (local [(define (fn-for-textbook t)
            (< (textbook-pages t) PAGE-COUNT-THRESHOLD))
          
          
          
          (define (fn-for-lot lot)
            (andmap fn-for-textbook lot))
          

          
          
          (define (fn-for-course c)
            (if (fn-for-lot (course-textbooks c))
                (text (string-append (course-dept c)
                                     (number->string (course-num c)))
                      FONT-SIZE FONT-COLOUR)
                empty-image))
          


          
          
          
          (define (fn-for-loc loc)
            (foldr above empty-image (map fn-for-course loc)))
            


          
          
          (define (fn-for-program p)
            (fn-for-loc (program-courses p)))]


    
    
    (fn-for-program p)))


;; Problem 4 and 5 Setup:
;;
;; For these problems you will be working with a frog family tree.
;;
;; A given frog has a name, a habitat the frog lives in, a list of the
;; highest jumps the frog has ever jumped (in inches), and a list of
;; children (which are themselves frogs).
;;
;; A habitat is the place a frog lives. A frog's habitat can be either:
;; a marsh, in the trees, in a cave, in a desert, or in a city.



(@problem 4)
;; - On the TYPE COMMENTS, draw any appropriate reference, self-reference, and
;; mutual reference arrows and label each arrow with R, SR, or MR.
;;
;; - On the TEMPLATES, draw any appropriate natural helper, natural recursion,
;; and natural mutual recursion arrows and label NH, NR, or NMR.


(@htdd Habitat)
;; Habitat is one of:
;; - "marsh"
;; - "trees"
;; - "cave"
;; - "desert"
;; - "city"
;; interp. an environment for a frog to live
;;<examples are redundant for enumeration>


(@htdd ListOfNatural)
;; ListOfNatural is one of:
;; - empty
;; - (cons Natural ListOfNatural)
;; interp. a list of natural numbers
(define LON0 empty)
(define LON1 (cons 14 (cons 20 (cons 8 empty))))


(@htdd Frog)
(define-struct frog (name habitat jumps children))
;; Frog is (make-frog String Habitat ListOfNatural ListOfFrog)
;; interp. a frog with a name,
;;         habitat the frog lives in,
;;         list of the highest jumps the frog has jumped in inches,
;;         and list of children


(@htdd ListOfFrog)
;; ListOfFrog is one of:
;; - empty
;; - (cons Frog ListOfFrog)
;; interp. a list of frogs

(define LOF0    empty)
(define KERMIT (make-frog "Kermit" "city"  (list 2 3)      empty)) 
(define HOPPY  (make-frog "Hoppy"  "marsh" (list 30 28 38) empty))
(define LOF1   (list KERMIT HOPPY))
(define GREEN  (make-frog "Green"  "trees" (list 8 12 9)   LOF1))

(define PEPE   (make-frog "Pepe"   "cave"  empty
                          (list
                           (make-frog "Charming" "cave"   (list 5)       empty)
                           (make-frog "Toad"     "desert" (list 20 7 13) empty)
                           GREEN)))

(define TIANA  (make-frog "Tiana"
                          "trees"
                          (list 7 3)
                          (list
                           (make-frog
                            "Ribbit"
                            "marsh"
                            (list 15 18)
                            (list (make-frog "Croak" "cave" empty empty)
                                  (make-frog "LilyPad" "marsh" LON1 empty))))))

(define FREDDO (make-frog "Freddo" "marsh" (list 8 9 7)
                          (list
                           PEPE 
                           (make-frog "Frogger" "city" (list 41 45 38) empty)
                           TIANA)))

#;
(define (fn-for-frog f)
  (local [(define (fn-for-habitat h)
            (cond [(string=? "marsh" h)  (...)]
                  [(string=? "trees" h)  (...)]
                  [(string=? "cave" h)   (...)]
                  [(string=? "desert" h) (...)]
                  [(string=? "city" h)   (...)]))

          (define (fn-for-lon lon)
            (cond [(empty? lon) (...)]
                  [else
                   (... (first lon)
                        (fn-for-lon (rest lon)))]))

          (define (fn-for-frog f)
            (... (frog-name f)
                 (fn-for-habitat (frog-habitat f))
                 (fn-for-lon (frog-jumps f))
                 (fn-for-lof (frog-children f))))

          (define (fn-for-lof lof)
            (cond [(empty? lof) (...)]
                  [else
                   (... (fn-for-frog (first lof))
                        (fn-for-lof (rest lof)))]))]

    (fn-for-frog f)))


(@problem 5)
;; Design an abstract fold function for Frog. You must include the signature,
;; purpose, two check-expects (described below), and the function definition.
;; 
;; We have provided this page and the next page for you to write your solution;
;; there is an encapsulated copy of the template on the next page.
;; You should neatly edit the template provided. Do not rewrite the template.
;;
;; The first check-expect should produce a copy of the given frog.
;;
;; The second check-expect should produce a count of the number of frogs who
;; live in a city or trees that have jumped at least 12 inches in the air.

(@htdf fold-frog)
(@signature (Natural X -> X) (String W X Z -> Y) (Y Z -> Z) X Z W W W W W Frog
            -> Y)
;; an abstract fold function for Frog
(check-expect (fold-frog cons make-frog cons
                         empty empty
                         "marsh" "trees" "cave" "desert" "city" FREDDO)
              FREDDO)
(check-expect (local [(define (c1 n lon)
                        (if (>= n 12)
                            true
                            lon))

                      (define (c2 name hab rlon rlof)
                        (if (and rlon hab)
                            (+ 1 rlof)
                            rlof))]
                (fold-frog c1 c2 + false 0
                           false true false false true FREDDO))
              2)

(define (fold-frog c1 c2 c3 b1 b2 a1 a2 a3 a4 a5 f)
  (local [(define (fn-for-habitat h)  ; -> W
            (cond [(string=? "marsh" h)  a1]
                  [(string=? "trees" h)  a2]
                  [(string=? "cave" h)   a3]
                  [(string=? "desert" h) a4]
                  [(string=? "city" h)   a5]))

          (define (fn-for-lon lon)    ; -> X
            (cond [(empty? lon) b1]
                  [else
                   (c1 (first lon)
                        (fn-for-lon (rest lon)))]))

          (define (fn-for-frog f)     ; -> Y
            (c2 (frog-name f)
                 (fn-for-habitat (frog-habitat f))
                 (fn-for-lon (frog-jumps f))
                 (fn-for-lof (frog-children f))))

          (define (fn-for-lof lof)    ; -> Z
            (cond [(empty? lof) b2]
                  [else
                   (c3 (fn-for-frog (first lof))
                        (fn-for-lof (rest lof)))]))]

    (fn-for-frog f)))