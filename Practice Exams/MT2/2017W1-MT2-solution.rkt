;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2017W1-MT2-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@problem 1)
;; You are provided with a signature, purpose, stub, and check-expects for a
;; function design. Complete the function design, including the template tag.
;;
;; You have been provided with data defitions for a Natural and ListOfNatural,
;; in case you choose to use them. You may also choose to use encapsulation or
;; built-ins.
;;
;; Here is a visual representation of what the completed function design should
;; produce:
;;
;; (n-circles-list (list 3 5 2)) produces a list of the following three images,
;; in this order:

;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2cir3.PNG
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2cir5.PNG
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2cir2.PNG


(define CIRCLE-IMG (circle 4 "solid" "blue"))


(@htdd Natural)
;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. a natural number
(define N0 0)
(define N1 (add1 0))
(define N2 (add1 (add1 0)))

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-natural (sub1 n)))]))


(@htdd ListOfNatural)
;; ListOfNatural is one of:
;; - empty
;; - (cons Natural ListOfNatural)
;; interp. a list of naturals
(define LON0 empty)
(define LON1 (cons 5 (cons 1 (cons 3 empty))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))


(@htdf n-circles-list)
(@signature ListOfNatural -> ListOfImage)
;; produce list of imgs, where each img is a number of CIRCLE-IMGs based on lon
(check-expect (n-circles-list empty) empty)
(check-expect (n-circles-list (list 2 3))
              (list (beside CIRCLE-IMG CIRCLE-IMG)
                    (beside CIRCLE-IMG CIRCLE-IMG CIRCLE-IMG)))
(check-expect (n-circles-list (list 3 5 2))
              (list (beside CIRCLE-IMG CIRCLE-IMG CIRCLE-IMG)
                    (beside CIRCLE-IMG CIRCLE-IMG
                            CIRCLE-IMG CIRCLE-IMG CIRCLE-IMG)
                    (beside CIRCLE-IMG CIRCLE-IMG)))

;(define (n-circles-list lon) empty) ;stub


;; [9 marks]:
;;
;; ; function that operates on ListOfNatural:
;; 1 origin of template comment
;; 1 ListOfNatural template is apparent (or map/foldr)
;; 1 base-case result 
;; 1 combination function 
;; 1 arguments passed to combination function correctly
;;   (or correct fn passed to map)
;;
;; ; helper function
;; (-2 if helper function does not contain htdf)
;; 1 Natural template is apparent (or build-list)
;; 1 base case
;; 1 combination function
;; 1 NR called correctly


;; POSSIBLE SOLUTION 1:

(@template ListOfNatural)

(define (n-circles-list lon)
  (cond [(empty? lon) empty]
        [else
         (cons (n-circles-img (first lon))
               (n-circles-list (rest lon)))]))


(@htdf n-circles-img)
(@signature Natural -> Image)
;; produce n circles beside each other
(check-expect (n-circles-img 0) empty-image)
(check-expect (n-circles-img 2) (beside CIRCLE-IMG CIRCLE-IMG empty-image))

;(define (circles-list n) empty-image) ;stub

(@template Natural)

(define (n-circles-img n)
  (cond [(zero? n) empty-image]
        [else
         (beside CIRCLE-IMG
                 (n-circles-img (sub1 n)))]))


;; POSSIBLE SOLUTION 2:

(@template Natural ListOfNatural encapsulated)

#;
(define (n-circles-list lon0)
  (local [(define (n-circles-img n)
            (cond [(zero? n) empty-image]
                  [else
                   (beside CIRCLE-IMG
                           (n-circles-img (sub1 n)))]))
          
          (define (n-circles-lon lon)
            (cond [(empty? lon) empty]
                  [else
                   (cons (n-circles-img (first lon))
                         (n-circles-lon (rest lon)))]))]
    (n-circles-lon lon0)))


;; POSSIBLE SOLUTION 3:

(@template use-abstract-fn fn-composition encapsulated)

#;
(define (n-circles-list lon)
  (local [(define (n-to-img n) CIRCLE-IMG)
          (define (row-of-circles n)
            (foldr beside empty-image (build-list n n-to-img)))]
    (map row-of-circles lon)))


;; POSSIBLE SOLUTION 4:

(@template use-abstract-fn fn-composition)

#;
(define (n-circles-list lon)
  (map (λ(n) (foldr beside
                    empty-image
                    (build-list n (λ(x) CIRCLE-IMG))))
       lon))


;; Possible solution

(@template Natural use-abstract-fn)

#;
(define (n-circles-list lon)
  (local [(define (n-circles-img n)
            (cond [(zero? n) empty-image]
                  [else
                   (beside CIRCLE-IMG
                           (n-circles-img (sub1 n)))]))]
    (map n-circles-img lon)))


(@problem 2)
;; You are provided with a signature, purpose, stub, check-expects, and
;; cross-product of type comments table. Complete the function design, including
;; origin of template comment.
;;
;; You must reduce the cross-product of type comments table to the MINIMUM
;; number of cases.
;; You must label each cell and corresponding cond Q/A pair in your function
;; with a number.
;;
;; NOTE: There is a helper function called node-image that you may use in your
;; solution.


(define FONT-SIZE 14)
(define FONT-COLOR "black")


(@htdd BinaryTree)
(define-struct node (key l r))
;; BinaryTree is one of:
;;  - false
;;  - (make-node Natural BinaryTree BinaryTree)
;; interp. a binary tree, each node has a key and left/right children
(define BT0 false)
(define BT16 (make-node 16 false false))
(define BT40 (make-node 40 BT16 false))
(define BT45 (make-node 45 BT40 (make-node 80
                                           (make-node 66 false false)
                                           (make-node 98 false false))))
#;
(define (fn-for-bt bt)
  (cond [(false? bt) (...)]
        [else
         (... (node-key bt)
              (fn-for-bt (node-l bt))
              (fn-for-bt (node-r bt)))]))


(@htdd Path)
;; Path is one of:
;; - empty
;; - (cons "L" Path)
;; - (cons "R" Path)
;; interp. A sequence of left and right 'turns' down through a BinaryTree.
;;         (list "L" "R" "R") means take the left child of the root,
;;         then the right child of that node, and the right child again.
(define P1 empty)
(define P2 (list "L" "R"))
#;
(define (fn-for-path p)
  (cond [(empty? p) (...)]
        [(string=? (first p) "L") (... (fn-for-path (rest p)))]
        [(string=? (first p) "R") (... (fn-for-path (rest p)))]))


(@htdf node-image)
(@signature BinaryTree -> Image)
;; produce an image representing the node of a BT
(check-expect (node-image false) empty-image)
(check-expect (node-image BT16) (overlay (text "16" FONT-SIZE FONT-COLOR)
                                         (circle 16 "outline" "red")))
(check-expect (node-image BT40) (overlay (text "40" FONT-SIZE FONT-COLOR)
                                         (circle 40 "outline" "red")))

(@template BinaryTree)

(define (node-image bt)
  (cond [(false? bt) empty-image]
        [else
         (overlay (text (number->string (node-key bt)) FONT-SIZE FONT-COLOR)
                  (circle (node-key bt) "outline" "red"))]))


(@htdf bt-path-images)
(@signature BinaryTree Path -> (listof Image))
;; produce a list of images representing the path taken through t
(check-expect (bt-path-images false empty) empty)
(check-expect (bt-path-images false (cons "L" empty)) empty)
(check-expect (bt-path-images false (cons "R" empty)) empty)
(check-expect (bt-path-images BT16 empty)
              (list (overlay (text "16" FONT-SIZE FONT-COLOR)
                             (circle 16 "outline" "red"))))
(check-expect (bt-path-images BT45 (cons "R" (cons "L" (cons "L" empty))))
              (list (overlay (text "45" FONT-SIZE FONT-COLOR)
                             (circle 45 "outline" "red"))
                    (overlay (text "80" FONT-SIZE FONT-COLOR)
                             (circle 80 "outline" "red"))
                    (overlay (text "66" FONT-SIZE FONT-COLOR)
                             (circle 66 "outline" "red"))))

;(define (bt-path-images t p) empty)   ;stub

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;; ╔════════════════╦═══════════╦════════════════════════════════════════════╗
;; ║   \       t -> ║           ║                                            
;; ║  p     \       ║   false   ║ (make-node Natural BinaryTree BinaryTree)   
;; ║             \  ║           ║                                            
;; ╠════════════════╬═══════════╬════════════════════════════════════════════╣
;; ║                ║           ║                  (2)
;; ║      empty     ║           ║ (list <image representing BT-node>)
;; ║                ║           ║
;; ╠════════════════╬           ╬════════════════════════════════════════════╣
;; ║                ║   (1)     ║ (cons <image representing BT-node> onto
;; ║(cons "L" Path) ║  empty    ║   (bt-path-images <left child> <rest p>)
;; ║                ║           ║                  (3)
;; ╠════════════════╬           ╬════════════════════════════════════════════╣
;; ║                ║           ║(cons <image representing BT-node> onto
;; ║(cons "R" Path) ║           ║   (bt-path-images <right child> <rest p>)
;; ║                ║           ║                  (4)
;; ╚                ╩           ╩


;; [18 marks]:
;;
;; 1 template from comment (cross product table)
;;
;; 4 table has 4 cases labeled (-1 for extra cases, or missing label)
;; 4 cond cases match labels from table (-1 for missing label)
;;
;; 1 - first cond case Q and A correct
;; 2 - second cond case Q and A correct (1 for Q, 1 for A (helper called))
;; 3 - third cond case Q and A correct
;;     (1 for 1, 1 for cons helper, 1 for recursion)
;; 3 - fourth cond case Q and A correct
;;     (1 for 1, 1 for cons helper, 1 for recursion)
     

(@template 2-one-of)

(define (bt-path-images t p)
  (cond [(false? t) empty]                    ;(1)
        [(empty? p) (list (node-image t))]    ;(2)
        [(string=? "L" (first p))             ;(3)
         (cons (node-image t) (bt-path-images (node-l t) (rest p)))]
        [else                                 ;(4)
         (cons (node-image t) (bt-path-images (node-r t) (rest p)))]))



                         
;; Problem 3 and 4 Setup:
;;
;; This problem will use a directory unit structure similar to the examples used
;; in the videos found in Module 6b: Mutual Reference.
;;
;; A given directory has a name, a list of files, and a list of sub-directories
;; (which are themselves directories) that the given directory contains.
;;
;; A file has a name, and a size in kilobytes.
;;
;; The following image illustrates this relationship:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2dir.PNG
;;
;; NOTE:
;;
;; In the diagram in the link, the larger boxes are directories
;;   (Documents, ENGL 100, Essays).
;; The smaller boxes are files with name and size
;;   (Ch1-review 98, mt1-marks 112, etc).
;;
;; A full data definition is provided for you on the following page.




(@problem 3)
;; - On the TYPE COMMENTS, draw any appropriate reference, self-reference, and
;; mutual reference arrows and label each arrow with R, SR, or MR.
;;
;; - On the TEMPLATES, draw any appropriate natural helper, natural recursion,
;; and natural mutual recursion arrows and label NH, NR, or NMR.


(@htdd File)
(define-struct file (name size))
;; File is (make-file String Natural)
;; interp. A file in a directory with name and size in kb
(define F1 (make-file "ps1-starter" 32))
(define F2 (make-file "htdf-starter" 14))
(define F3 (make-file "yoshi-eggs" 65))
(define F4 (make-file "tamagotchi" 58))
(define F5 (make-file "ref-rule" 8))
(define F6 (make-file "Ch1-review" 98))
(define F7 (make-file "mt1-marks" 112))
(define F8 (make-file "csid" 2))


(@htdd ListOfFile)
;; ListOfFile is one of:
;;  - empty
;;  - (cons File ListOfFile)
;; interp. a list of files
(define LOF0 empty)
(define LOF1 (list F1 F2))
(define LOF2 (list F3 F4 F5))


(@htdd Dir)
(define-struct dir (name files sub-dirs))
;; Dir is (make-dir String ListOfFile ListOfDir)
;; interp. A directory with a name, list of files, and list of sub-directories.


(@htdd ListOfDir)
;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories

(define D1 (make-dir "Essays"    empty        empty))
(define D2 (make-dir "ENGL100"   (list F6)    (list D1)))

(define D3 (make-dir "Week1"     LOF1         empty))
(define D4 (make-dir "Module 4b" LOF2         empty))

(define LOD1 (list D3 D4))
(define D5 (make-dir "CPSC110"   (list F7 F8) LOD1))
(define D6 (make-dir "Extra Curricular" empty empty))

(define LOD2 (list D2 D5 D6))
(define D7 (make-dir "Documents" empty        LOD2))


;; PROBLEM 3: 24 points
;;
;; Notes: an arrow is incorrect if it's pointing in the wrong direction
;;
;; = 6 correct type reference arrows: from reference to definition, right
;;     arrowhead
;;
;; = 6 correct type reference arrow labels
;;
;; = 6 correct template function call arrows: from call to fn-definition, right
;;     arrowhead
;;
;; = 6 correct template function call arrow labels
;;  
;; take away 2 for each extra incorrect arrow


;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2type.PNG
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w1m2temp.PNG


#;#;#;#;
(define (fn-for-file f)
  (... (file-name f)
       (file-size f)))

(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (fn-for-file (first lof))
              (fn-for-lof (rest lof)))]))

(define (fn-for-dir d)
  (... (dir-name d)
       (fn-for-lof (dir-files d))
       (fn-for-lod (dir-sub-dirs d))))

(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))


(@problem 4)
;; Design an abstract fold function for Directory. You must include the
;; signature, purpose, two check-expects (described below), and the function
;; definition.
;;
;; We have provided this page and the next page for you to write your solution;
;; there is an encapsulated copy of the template on the next page.
;; You should neatly edit the template provided. Do not rewrite the template.
;;
;; The first check-expect should produce a copy of the given directory.
;;
;; The second check-expect should produce a list containing the names of only
;; directories for which the sum of their file sizes is greater than 100kb.
;;
;; Hint: Use the picture in the "Problem 3 and 4 Setup" box to help identify the
;; TWO directories containing files that have sizes that add up to greater than
;; 100kb.


;; [37 marks]:
;; signature [15]:
;; (in all cases below, deduct 1 each error unless otherwise noted)
;; 3 - signature of c1 - fn-for-file combination fn
;;     no marks if number of parameters is incorrect
;; 2 - signature of c2 - fn-for-lof combination fn
;;     no marks if number of parameters is incorrect
;; 4 - signature of c3 - fn-for-dir combination fn
;;     no marks if number of parameters is incorrect
;; 2 - signature of c4 - fn-for-lod combination fn
;;     no marks if number of parameters is incorrect
;; 2 - remaining parameters
;; 1 - Dir as final parameter
;; 1 - type produced by fold-for-project is correct
;;
;; purpose [1]: 1 clear purpose statement
;;
;; check-expects [15]:
;; 6 first c-e  - deduct 1 for each error
;; 2 second c-e - c1 correct (just produces second argument)
;; 1 second c-e - c2 correct (+) -> okay to locally defined
;; 3 second c-e - c3 correct (1 mark if, 1 mark true case, 1 mark false case) 
;; 1 second c-e - c4 correct (append) -> okay if locally defined
;; 2 second c-e - remaining arguments - deduct 1 for each error
;;
;; parameterization [6]:
;; 4 combination functions correctly parameterized
;; 2 base case values correctly parameterized


(@htdf fold-dir)
(@signature (String Natural -> W) (W X -> X) (String X Z -> Y) (Y Z -> Z) X Z
            Dir -> Y)
;; abstract fold for Directory
(check-expect (fold-dir make-file cons make-dir cons empty empty D7) D7)

(check-expect (local [(define (c1 name size)
                        size)
                      (define (c3 name rfnlof rfnlod)
                        (if (>= rfnlof 100)
                            (cons name rfnlod)
                            rfnlod))]
                (fold-dir c1 + c3 append 0 empty D7))
              (list "CPSC110" "Module 4b"))

(@template File ListOfFile Dir ListOfDir encapsulated)

(define (fold-dir c1 c2 c3 c4 b1 b2 d)
  
  (local [(define (fn-for-file f) ; -> W            
            (c1 (file-name f)                 
                (file-size f)))
          
          (define (fn-for-lof lof) ; -> X            
            (cond [(empty? lof) b1]                  
                  [else                   
                   (c2 (fn-for-file (first lof))                        
                       (fn-for-lof (rest lof)))]))
          
          (define (fn-for-dir d) ; -> Y            
            (c3 (dir-name d)                 
                (fn-for-lof (dir-files d))                 
                (fn-for-lod (dir-sub-dirs d))))
          
          (define (fn-for-lod lod) ; -> Z            
            (cond [(empty? lod) b2]                  
                  [else                   
                   (c4 (fn-for-dir (first lod))                        
                       (fn-for-lod (rest lod)))]))]
    
    (fn-for-dir d)))


(@problem 5)
;; Over the next two pages there are THREE function designs. For each function
;; design, you must use built-in abstract functions to refactor the function
;; design wherever possible.
;;
;; For full marks your refactored solutions must not use the list template at
;; all, and you CANNOT use the built-in functions member or length.
;;
;; You do not need to repeat signatures, purposes, and check-expects for the
;; functions. Provide your solution by crossing out the existing template from
;; comments and function bodies, and then write your new template from comments
;; and function bodies.


(@htdd Midterm)
(define-struct mt (course weight buildings))
;; Midterm is (make-mt String Natural (listof String))
;; interp. a midterm exam at UBC, with a course name,
;;         weight of overall course grade, in percent
;;         and list of the names of the buildings where the exam takes place
(define MT-110 (make-mt "CPSC110" 20 (list "CIRS" "WOOD" "HEBB")))
(define MT-121 (make-mt "CPSC121" 12 (list "CIRS" "HEBB")))
(define MT-210 (make-mt "CPSC210" 15 (list "CIRS" "SWNG" "WESB")))
(define MT-221 (make-mt "CPSC221" 20 (list "SWNG")))

#;
(define (fn-for-mt m)
  (... (mt-course m)
       (mt-weight m)
       (fn-for-los (mt-buildings m))))



;; FUNCTION DESIGN 1

(@htdf label-with-semester)
(@signature String (listof Midterm) -> (listof String))
;; produce a list of semester-course labels corresponding to given s and lom
(check-expect (label-with-semester "2017W1" empty) empty)
(check-expect (label-with-semester "2017W1" (list MT-110 MT-121 MT-210))
              (list "2017W1-CPSC110" "2017W1-CPSC121" "2017W1-CPSC210"))
(check-expect (label-with-semester "2015S" (list MT-121 MT-221))
              (list "2015S-CPSC121" "2015S-CPSC221"))

;; (@template (listof Midterm))
;;
;; (define (label-with-semester s lom)
;;   (cond [(empty? lom) empty]
;;         [else
;;          (cons (combine-strings s (first lom))
;;                (label-with-semester s (rest lom)))]))
;;
;;
;; (@htdf combine-strings)
;; (@signature String Midterm-> String)
;; ;; produce a string with s concatenated onto m's course name
;; (check-expect (combine-strings "cde" MT-110) "cde-CPSC110")
;;
;; (@template Midterm)
;;
;; (define (combine-strings s m)
;;   (string-append s "-" (mt-course m)))


;; [5 marks]:
;;
;; 1 origin of template comment
;; 1 template is apparent
;; 2 correct fn argument 
;;   - deduct 2 if two parameters defined for fn consumed by map
;;     or if they call top level function, this must be a closure



;; POSSIBLE SOLUTION 1:

(@template use-abstract-fn)

#;
(define (label-with-semester s lom)
  (local [(define (label m)
            (string-append s "-" (mt-course m)))]
    (map label lom)))


;; POSSIBLE SOLUTION 2:

(@template use-abstract-fn fn-composition)

#;
(define (label-with-semester s lom)
  (local [(define (label c)
            (string-append s "-" c))]
    (map label (map mt-course lom))))


;; POSSIBLE SOLUTION 3

(@template use-abstract-fn)

(define (label-with-semester s lom)
  (map (λ(m) (string-append s "-" (mt-course m))) lom))




;; FUNCTION DESIGN 2                                                     

(@htdf contains?)
(@signature String (listof String) -> Boolean)
;; produce true if str is in los, false otherwise
(check-expect (contains? "test" empty) false)
(check-expect (contains? "abc" (list "abc" "de" "fgh")) true)
(check-expect (contains? "CPSC110" (list "CPSC121" "CPSC110")) true)
(check-expect (contains? "def" (list "abcd" "efg" "hij")) false)

;; (@template (listof String))
;;
;; (define (contains? str los)
;;   (cond [(empty? los) false]
;;         [else
;;          (or (string=? str (first los))
;;              (contains? str (rest los)))]))

;; [5 marks]:
;;
;; 1 origin of template comment
;; 1 template for ormap (or foldr) is evident
;; 3 correct fn argument for ormap (or foldr)
;;   - deduct 2 if two parameters defined for fn consumed by ormap
;;     (or deduct 2 if one parameter defined for fn consumed by foldr)

(@template use-abstract-fn)

(define (contains? str los)
  (local [(define (matches? s)
            (string=? str s))]
    (ormap matches? los)))

;; a solution using foldr will produce the same answer, but it will
;; not have the short circuiting behaviour of stopping as soon as
;; it gets to the first true





;; FUNCTION DESIGN 3

(@htdf count-midterms-in)
(@signature String (listof Midterm) -> Natural)
;; count the number courses with a midterm in building b
(check-expect (count-midterms-in "CIRS" empty) 0)
(check-expect (count-midterms-in "WOOD" (list MT-110 MT-121 MT-210)) 1)
(check-expect (count-midterms-in "SWNG" (list MT-110 MT-121 MT-210 MT-221)) 2)

;; (@template fn-composition Midterm (listof Midterm) encapsulated)
;;
;; (define (count-midterms-in b lom)
;;   (local [(define (in-building? m) (contains? b (mt-buildings m)))
;;           (define (mts-in-building lom)
;;             (cond [(empty? lom) empty]
;;                   [else
;;                    (if (in-building? (first lom))
;;                        (cons (first lom) (mts-in-building (rest lom)))
;;                        (mts-in-building (rest lom)))]))
;;           (define (count lom)
;;             (cond [(empty? lom) 0]
;;                   [else
;;                    (+ 1
;;                       (count (rest lom)))]))]
;;     (count (mts-in-building lom))))


;; [9 marks]:
;;
;; 1 origin of template comment
;; 1 template for filter is evident
;; 3 lom correctly filtered to produce list of midterms in building b
;;   - deduct 2 marks if two arguments passed to p?
;;   - deduct 2 if closure not used
;; 1 template for foldr is apparent
;; 3 foldr correctly produces count of items in result of filter
;;   (-2 if fn does not consumes 2 arguments (cannot just put + or add1)


;; POSSIBLE SOLUTION 1:

(@template use-abstract-fn fn-composition)

#;
(define (count-midterms-in b lom)
  (local [(define (in-b? m)
            (contains? b (mt-buildings m)))
          (define (count f rnr)
            (+ 1 rnr))]
    (foldr count 0 (filter in-b? lom))))


;; POSSIBLE SOLUTION 2:

(@template use-abstract-fn)

(define (count-midterms-in b lom)
  (local [(define (in-building? m) (contains? b (mt-buildings m)))
          (define (count-in-b f rnr)
            (if (in-building? f)
                (+ 1 rnr)
                rnr))]
    (foldr count-in-b 0 lom)))
