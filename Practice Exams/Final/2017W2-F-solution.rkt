;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2017W2-F-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@problem 1)
;; An online item selling system (like eBay) allows sellers to list
;; items to be sold for up to 30 days and track the status of these items.
;;
;; The status of an item is either:
;; still listed with the number of days left in the listing, reserved by an
;; interested buyer, or sold.
;;
;; Provide a complete data definition for the status of an item listing.



;; SOLUTION

;; (/14)
;;
;; Listing DD
;; type comments and examples
;; = 1 one of 
;; = 3 types comment (1 for each type in itemization)
;; = 2 clear interp with all cases (1) + units (1)
;; = 1 example for each case
;; template:
;; = 1 # of cases match type comment
;; = 3 "answers" in template correct
;; = 3 "questions" in template correct



(@htdd Listing)
;; Listing is one of:
;; - Natural
;; - "reserved"
;; - "sold"
;; interp. a listing which can be either
;;         - number of days left in listing between 0 and 30, in days
;;         - reserved by a buyer
;;         - sold to a buyer
(define L-NUM 20)
(define L-SOLD "reserved")
(define L-PAID "sold")

(define (fn-for-listing l)
  (cond [(number? l) (... l)]
        [(string=? l "reserved") (...)]
        [else        (...)]))







(@problem 2)
;; This problem is broken into four parts. Read all four parts before you start.
;;
;; You are provided with a signature, purpose, stub, and check-expects for
;; the function strings-of-len. 
;;
;; You must include the function implementation
;; for strings-of-len in each of Parts A-D.


(@htdf strings-of-len)
(@signature (listof String) Natural -> (listof String))
;; produce a list of strings from strings in los that have length greater than n
(check-expect (strings-of-len empty 5) empty)
(check-expect (strings-of-len (list "ab" "cde" "ghih") 3) (list "ghih"))
(check-expect (strings-of-len (list "ab" "cde" "ghih") 2) (list "cde" "ghih"))

;(define (strings-of-len los n) empty)


;; Problem 2 (PART A)
;;
;; Write a non-tail recursive function definition for strings-of-len. 
;; Do not use function composition or built-in abstract functions.
;;
;; A grade of 0 will be given for tail recursive solutions or
;; solutions that use built-in abstract functions.




;; SOLUTION

;; (/5)
;;
;; = 1 template is apparent
;; = 1 base result is correct
;; = 1 recursive call is correct with add'l arg
;; = 2 overall correct logic (0 or 2)
;; (lenient with string-length fn name)


(@template (listof String))

#;
(define (strings-of-len los n)
  (cond [(empty? los) empty]
        [else
         (if (> (string-length (first los)) n)
             (cons (first los)
                   (strings-of-len (rest los) n)) 
             (strings-of-len (rest los) n))]))




;; Problem 2 (PART B)
;;
;; Write a new function definition using built-in abstract list functions.
;;
;; A grade of 0 will be given for solutions that do not use built-in abstract
;; list functions.



;; SOLUTION

;; (/5)
;;
;; = 1 template is apparent
;; = 2 filtering logic (0 or 2) (lenient with string-length fn name)
;; = 2 function arguments are correct (1 for los, 1 for remaining args)

(@template use-abstract-fn)

#;
(define (strings-of-len los n)
  (local [(define (fn s)
            (> (string-length s) n))]
    (filter fn los)))


(@template use-abstract-fn)

#;
(define (strings-of-len los n)
  (local [(define (fn s ror)
            (if (> (string-length s) n)
                (cons s ror)
                ror))]
    (foldr fn empty los)))






























;; Problem 2 (PART C)
;;
;; Write a tail recursive function.
;; Do not use function composition or built-in abstract functions.
;;
;; A grade of 0 will be given for solutions that are not tail recursive,
;; or solutions that use built-in abstract functions.


;; SOLUTION

;; (/7)
;;
;; = 1 template is apparent
;; = 1 accumulator type + invariant
;; = 1 trampoline call is correct
;; = 1 base result is correct (order correct)
;; = 1 recursive call is correct with add'l arg
;; = 2 overall correct logic (0 or 2)
;;
;; (lenient with string-length fn name)


(@template (listof String) accumulator)

#;
(define (strings-of-len los0 n)
  ;; rsf is (listof String) - a list of strings with length >n seen so far
  (local [(define (strings-of-len los rsf)
            (cond [(empty? los) rsf]
                  [else
                   (if (> (string-length (first los)) n) 
                       (strings-of-len (rest los)
                                       (append rsf (list (first los))))
                       (strings-of-len (rest los) rsf))]))]
    (strings-of-len los0 empty)))


(@template (listof String) accumulator)

;;alternate solution
(define (strings-of-len los0 n)
  ;; rsf is (listof String) - a list of strings with length >n seen so far
  (local [(define (strings-of-len los rsf)
            (cond [(empty? los) (reverse rsf)]
                  [else
                   (if (> (string-length (first los)) n) 
                       (strings-of-len (rest los) (cons (first los) rsf))
                       (strings-of-len (rest los) rsf))]))]
    (strings-of-len los0 empty)))



















;; Problem 2 (PART D)
;;
;; Write a new version of the function definition using for-each.
;;
;; A grade of 0 will be given for solutions not using for-each.


;; SOLUTION

;; (/7)
;;
;; = 1 template is apparent
;; = 1 accumulator type +  invariant
;; = 1 initial value of accumulator is correct
;; = 2 update correct
;; = 2 overall correct logic (0 or 2)


(@template for-each)

#;
(define (strings-of-len los n)
  ;; rsf is (listof String) - a list of strings with length >n seen so far
  (local [(define rsf empty)]
    (begin
      (for-each
       (λ (s) (if (> (string-length s) n)
                  (set! rsf (append rsf (list s)))
                  void))
       los)
      rsf)))


(@template for-each)

#;
(define (strings-of-len los n)
  ;; rsf is (listof String) - a list of strings with length >n seen so far
  (local [(define rsf empty)]
    (begin
      (for-each
       (λ (s) (if (> (string-length s) n)
                  (set! rsf (append rsf (list s)))
                  (set! rsf rsf)))
       los)
      rsf)))













(@problem 3)
;; Design a function that consumes a list of images and produces a number
;; representing the length of the longest sequence of images that increase in
;; area. 
;;
;; For example:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w2flist3.PNG
;;
;; Calling the function with the list in the above link produces 3, as the first
;; 3 images in a row increase in area. There is another sequence where images
;; increase in area (the last two images), but the sequence of 3 is longest of
;; the two sequences in the given list.
;;
;; Note: the first image in the list is compared to empty-image to determine the
;; length of the first sequence of images that increases in size.
;;
;; For example:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w2flist1.PNG
;;
;; Calling the function with the list in the above link produces 1, as the first
;; image has a larger area than empty-image, but the second image is the same
;; size as the first.


(define LOI0 empty)
(define LOI1 (list (square 30 "solid" "blue")
                   (square 40 "solid" "red")
                   (square 70 "solid" "purple")
                   (square 50 "solid" "orange")
                   (square 60 "solid" "pink")))

(define LOI2 (list (square 40 "solid" "yellow")
                   (square 40 "solid" "purple")))

(define LOI3 (list (square 30 "solid" "black")
                   (square 25 "solid" "brown")
                   (square 20 "solid" "grey")))

(define LOI4 (list (square 50 "solid" "red")
                   (square 20 "solid" "yellow")
                   (square 30 "solid" "purple")))







;; SOLUTION:

;; (/26)
;; = 1 signature (0 or 1)
;; = 1 purpose
;; = 3 check-expects cover each of these cases (can be done in 2 c-es)
;;     - base case
;;     - list with length >= 2
;;     - check for equal area (not just strictly greater)
;;
;; = 1 template apparent
;; = 3 accumulator types
;; = 3 accumulator invariants
;;
;; = 3 initialize accs correctly
;; = 2 logic exploiting prev acc (if)
;; = 3 update accs correctly in true case
;; = 3 update accs correctly in false case
;; = 2 base case produces correct result
;; = 1 use helper function


(@htdf largest-increasing-seq)
(@signature (listof Image) -> Natural)
;; produce the size of the largest sequence of growing images in loi0, in images
(check-expect (largest-increasing-seq empty) 0)
(check-expect (largest-increasing-seq (list (square 1 "solid" "black"))) 1)
(check-expect (largest-increasing-seq LOI1) 3)
(check-expect (largest-increasing-seq LOI2) 1)
(check-expect (largest-increasing-seq LOI3) 1)
(check-expect (largest-increasing-seq LOI4) 2)

(@template (listof Image) accumulator encapsulated)

(define (largest-increasing-seq loi0)
  ;; rsf is Natural: largest sequence found so far
  ;; cur is Natural: number of images of increasing size found in current run
  ;; prev is Image: previous first image in loi
  (local [(define (fn-for-loi loi prev cur rsf)
            (cond [(empty? loi) rsf]
                  [else
                   (if (area>? (first loi) prev)
                       (fn-for-loi (rest loi) (first loi)
                                   (add1 cur) (max (add1 cur) rsf))
                       (fn-for-loi (rest loi) (first loi)
                                   1          (max cur rsf)))]))   
          (define (area>? i1 i2)
            (> (* (image-width i1) (image-height i1))
               (* (image-width i2) (image-height i2))))]   
    (fn-for-loi loi0 empty-image 0 0)))








;; Problem 4 setup
;;
;; This problem will use words and mutations on words. A mutation occurs
;; when a letter is added or removed from a word to form a new word.
;;
;; A given word has a name and a list of mutations that can be applied to it.
;;
;; A mutation can either add or remove a single letter from a word to form
;; a new word.
;;
;; The following image illustrates the relationship between words and mutations:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17w2fwords.PNG
;;
;; In the image above words are shown in the boxes, and mutations are shown as
;; arrows. The letter being added or removed in the mutation to form a new word
;; is shown beside the arrow. The arrow points to the new word formed.
;;
;; Note: Dashed (- - -) arrows denote a mutation where a letter is being
;;       removed.
;;       Solid arrows denote a mutation where a letter is being added.
;;
;; In the English language, there are more relationships between words and
;; mutations than shown in the image above.
;; To simplify the problem, use only the examples that are shown in the image
;; above.
;;
;; A data definition to represent this information has been provided on the next
;; page.











;; =================
;; Data Definitions:

(@htdd Word)
(define-struct word (name mutations))
;; Word is (make-word String (listof Mutation))
;; interp. a word with a name composed of lower-case English letters, and
;;         a list of mutations that can be applied to form new words


(@htdd Mutation)
(define-struct mut (add? letter result))
;; Mutation is (make-mut Boolean String Word)
;; interp. a mutation on a word that adds or removes a letter to form a new word
;;         add? is true when the mututation adds a letter, false for a removal
;;         letter is the letter being added or removed
;;         result is the new word that is formed
;; ASSUME: letter is always a one-letter lowercase String

(define WORDS-GRAPH
  (shared [(FEAST (make-word "feast" (list (make-mut false "e" FAST)
                                           (make-mut false "f" EAST))))
           (BEAST (make-word "beast" (list (make-mut false "b" EAST))))
           (EAST  (make-word "east"  (list (make-mut true  "f" FEAST)
                                           (make-mut false "s" EAT))))
           (BEAT  (make-word "beat"  empty))
           (FEAT  (make-word "feat"  (list (make-mut false "f" EAT))))
           (FAST  (make-word "fast"  (list (make-mut false "s" FAT))))
           (FAT   (make-word "fat"   (list (make-mut true  "e" FEAT))))
           (EAT   (make-word "eat"   (list (make-mut true  "b" BEAT)
                                           (make-mut false "e" AT))))
           (AT    (make-word "at"    (list (make-mut true  "f" FAT)
                                           (make-mut true  "e" EAT)
                                           (make-mut true  "b" BAT))))
           (BAT   (make-word "bat"   (list (make-mut true  "e" BEAT))))]
    (list FEAST BEAST EAST BEAT FEAT FAST FAT EAT AT BAT)))

(define FEAST (first WORDS-GRAPH))
(define BEAST (list-ref WORDS-GRAPH 1))
(define EAST (list-ref WORDS-GRAPH 2))
(define BEAT (list-ref WORDS-GRAPH 3))
(define EAT (list-ref WORDS-GRAPH 7))
(define BAT (list-ref WORDS-GRAPH 9))

#;
(define (fn-for-word w0)
  (local [(define (fn-for-word w)
            (... (word-name w)
                 (fn-for-lom (word-mutations w))))

          (define (fn-for-lom lom)
            (cond [(empty? lom) (...)]
                  [else
                   (... (fn-for-mut (first lom))
                        (fn-for-lom (rest lom)))]))

          (define (fn-for-mut m)
            (... (mut-add? m)
                 (mut-letter m)
                 (fn-for-word (mut-result m))))]

    (fn-for-word w0)))



(@problem 4)
;; Design a tail-recursive function that consumes a Word from the words graph  
;; and a single letter. 
;;
;; The function should produce true if the letter is part of MORE add mutations
;; than remove mutations.
;;
;; For example, starting at FEAST, the letter "f" is in the following mutations:
;;  - a removal mutation going from FEAST to EAST
;;  - an add mutation going from EAST to FEAST
;;  - a removal mutation going from FEAT to EAT
;;  - an add mutation going from AT to FAT
;;
;; The function should produce false since "f" is not in more add than
;; remove mutations (there are 2 of each) in the graph.
;;
;; On the other hand, the function would produce true if given the letter "e",
;; as there are 3 add mutations with "e", and only 2 remove mutations.
;;
;; You must provide a tail-recursive function definition.
;; The signature, purpose, and check-expects have been provided for you.


(@htdf more-add-than-remove?)
(@signature Word String -> Boolean)
;; produce true if x is in more add than remove mutations reachable from w0
;; ASSUME: x is a String that is only one lower-case letter long
(check-expect (more-add-than-remove? BEAT "e") false)
(check-expect (more-add-than-remove? BAT "e") true)
(check-expect (more-add-than-remove? FEAST "f") false)
(check-expect (more-add-than-remove? FEAST "e") true)
(check-expect (more-add-than-remove? BEAST "b") true)
(check-expect (more-add-than-remove? FEAST "s") false)

;(define (more-add-than-remove? w x) true)




;;SOLUTION

;; (/27)
;;
;; = 1 template apparent
;; = 6 accumulator types, invariants, (4 todo/visited accs + 2 rsf acc)
;;     (-1 for each missing)
;; = 3 initialize accs correctly (2 todo/visited accs + 1 rsf acc)
;;     (-1 for each error)
;;
;; fn-for-word
;; = 1 parameter list correct
;; = 1 checks if member
;; = 1 logic for if member true
;; = 2 logic for if member false: updates todo and visited correctly
;;
;; fn-for-lom or fn-for-todo
;; = 1 parameter list correct
;; = 2 base case (determines if more add than remove based on acc) (0 or 2)
;; = 2 else case calls fn-for-mut and handles todo correctly (0 or 2)
;;
;; fn-for-mut
;; = 1 parameter list correct
;; = 2 letter is x and it is an add
;;     (1 question, 1 for correspond call to fn-for-word)
;; = 2 letter is x and it is not an add
;;     (1 question, 1 for correspond call to fn-for-word)
;; = 2 letter is not x  (1 question, 1 for correspond call to fn-for-word)


(@template Mutation (listof Mutation) Word encapsulated accumulator)

#;
(define (more-add-than-remove? w0 x)
  ;; todo is (listof Word): worklist accumulator
  ;; visited is (listof String): names of words visited so far
  ;; rsf is Integer: count of how many more add than remove mutations so far
  (local [(define (fn-for-word w todo visited rsf)
            (if (member? (word-name w) visited)
                (fn-for-lom todo visited rsf)
                (fn-for-lom (append (word-mutations w) todo)
                            (cons (word-name w) visited)
                            rsf)))

          (define (fn-for-lom todo visited rsf)
            (cond [(empty? todo) (> rsf 0)]
                  [else
                   (fn-for-mut (first todo) (rest todo) visited rsf)]))

          (define (fn-for-mut m todo visited rsf)
            (if (string=? (mut-letter m) x)
                (if (mut-add? m)
                    (fn-for-word (mut-result m) todo visited (add1 rsf))
                    (fn-for-word (mut-result m) todo visited (sub1 rsf)))
                (fn-for-word (mut-result m) todo visited rsf)))]

    (fn-for-word w0 empty empty 0)))








; ALTERNATE SOLUTION: 2 accumulators

(@template Mutation (listof Mutation) Word encapsulated accumulator)

#;
(define (more-add-than-remove? w0 x)
  ;; todo is (listof Mutation): worklist accumulator
  ;; visited is (listof String): names of words visited so far
  ;; adds is Natural: count of how many add mutations with letter x so far
  ;; rems is Natural: count of how many remove mutations with letter x so far
  (local [(define (fn-for-word w todo visited adds rems)
            (if (member? (word-name w) visited)
                (fn-for-lom todo visited adds rems)
                (fn-for-lom (append (word-mutations w) todo)
                            (cons (word-name w) visited)
                            adds
                            rems)))

          (define (fn-for-lom todo visited adds rems)
            (cond [(empty? todo) (> adds rems)]
                  [else
                   (fn-for-mut (first todo) (rest todo) visited adds rems)]))

          (define (fn-for-mut m todo visited adds rems)
            (if (string=? (mut-letter m) x)
                (if (mut-add? m)
                    (fn-for-word (mut-result m) todo visited (add1 adds) rems)
                    (fn-for-word (mut-result m) todo visited adds (add1 rems)))
                (fn-for-word (mut-result m) todo visited adds rems)))]

    (fn-for-word w0 empty empty 0 0)))








; ALTERNATE SOLUTION: conds instead of nested if, 2 accumulators

(@template Mutation (listof Mutation) Word encapsulated accumulator)

(define (more-add-than-remove? w0 x)
  ;; todo is (listof Mutation): worklist accumulator
  ;; visited is (listof String): names of words visited so far
  ;; adds is Natural: count of how many add mutations with letter x so far
  ;; rems is Natural: count of how many remove mutations with letter x so far
  (local [(define (fn-for-word w todo visited adds rems)
            (if (member? (word-name w) visited)
                (fn-for-lom todo visited adds rems)
                (fn-for-lom (append (word-mutations w) todo)
                            (cons (word-name w) visited)
                            adds
                            rems)))

          (define (fn-for-lom todo visited adds rems)
            (cond [(empty? todo) (> adds rems)]
                  [else
                   (fn-for-mut (first todo) (rest todo) visited adds rems)]))

          (define (fn-for-mut m todo visited adds rems)
            (cond [(and (string=? (mut-letter m) x) (mut-add? m))
                    (fn-for-word (mut-result m) todo visited (add1 adds) rems)]
                  [(string=? (mut-letter m) x)
                    (fn-for-word (mut-result m) todo visited adds (add1 rems))]
                  [else
                   (fn-for-word (mut-result m) todo visited adds rems)]))]

    (fn-for-word w0 empty empty 0 0)))















