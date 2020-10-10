;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-09-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER
;;

(require spd/tags)

(@assignment pset-09);Do not edit or remove this tag
(@cwl ??? ???)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


;;
;; THIS IS THE MOST CHALLENGING PROBLEM SET SO FAR THIS TERM.  PLEASE BE
;; SURE YOU WORK THROUGH IT CAREFULLY THOUGH.  110 FINAL EXAMS OFTEN INCLUDE
;; PROBLEMS BASED ON PROBLEM SETS 9, 10, OR 11.
;;
;; ALSO NOTE THAT THE AUTOGRADER WILL ONLY GRADE ONE SUBMISSION PER HOUR FOR
;; THIS PROBLEM SET. SO START EARLY AND CAREFULLY TEST YOUR WORK ON YOUR OWN
;; EACH TIME BEFORE SUBMITTING TO THE AUTOGRADER.
;;
;; THERE WILL BE A SPECIAL PINNED THREAD ON PIAZZA IN WHICH WE WILL ANSWER
;; QUESTIONS ABOUT THIS PROBLEM SET.
;;
;; In this problem set you are going to work on one of the toughest problems
;; we face running 110 - scheduling of TAs.  As you may know, we have about
;; 45 TAs, and we have to schedule them for many labs, 3 lectures, and office
;; hours.  We solved this by writing a schedule solver, and that's what you
;; are going to do for this problem set.
;;
;; We are making it a little easier for you, in that all you will be having
;; to deal with is labs. 
;;
;; We are giving you two key data definitions, as well as some examples of
;; that data.  We are also giving you a wish list entry for the main solve
;; function you have to design.  The function consumes a list of TAs and a
;; list of lab slots to fill.  It produces a list of assignments.  So, for
;; example, in the following very simple case, where there are two slots,
;; and two TAs, you get an assignment of the TAs to those slots.
;;
;; (solve (list (make-slot "A" 1) (make-slot "B" 1))
;;        (list (make-ta "Ali" (list "B"))
;;              (make-ta "Azi" (list "A"))))  ==>
;;
;; (list (list "Ali" "B") (list "Azi" "A"))
;;
;; In this simple example there was only one possible assignment. But in
;; general there might be more than one assignment, or it might be impossible
;; to generate an assignment.

;; By now you know enough about search to know that the first thing you need
;; to do is figure out the search space.  What does the tree look like? What
;; information do you have to represent at each node in the tree?  Even though
;; The function ends up producing just a list of assignments, it need more than
;; just the assignments so far at each node in the tree?  What other information
;; do you need to represent at each node?
;;
;; As you consider the search tree, note that a TA can work more than one slot,
;; but a slot is filled by one TA.  So once you assign a TA to a slot that slot
;; is done.
;;
;; As usual, anything we give you you must use without changing.  The autograder
;; will want to call solve with arguments as described below.  

;; Constants:

(define MAX-SLOTS-PER-TA 3) ;this is the max number of labs a TA can work

;; Data definitions:

(@htdd Slot)
(define-struct slot (lab n))
;; Slot is (make-slot String Natural)
;; interp. the name of a lab - "A", "B", etc.
;;         the lab's slot number - 1, 2, 3 etc.
;; CONSTRAINT:
;;  a list of slots representing positions that need to be filled
;;  should not contain duplicate slots (slots with same lab and n)
;;  
(define SLOTS1
  (list (make-slot "A" 1) (make-slot "A" 2)   ;A needs 2 TAs
        (make-slot "B" 1)                     ;B needs 1
        (make-slot "C" 1) (make-slot "C" 2))) ;C needs 2

(@htdd TA)
(define-struct ta (name avail))
;; TA is (make-ta String (listof String))
;; interp. A TA with their name and the labs times they are free to work.
;; CONSTRAINT: 
;;  a list of TAs should not contain TAs with duplicate names

(define TAS1
  (list (make-ta "Ali" (list "A" "B"))
        (make-ta "Ari" (list     "B" "C"))
        (make-ta "Azi" (list "A"     "C"))))



;;!!! below here is solution

(@htdf solve)
(@signature (listof Slot) (listof TA) -> (listof (listof String)) or false)
;; produce list of lab assignments of form: (list "ta name" "lab name")

(check-expect (solve empty empty) empty) 
(check-expect (solve empty (list (make-ta "Ali" (list "A"))))
              empty)
(check-expect (solve (list (make-slot "A" 1)) empty) false) 
(check-expect (solve (list (make-slot "A" 1) 
                           (make-slot "A" 2))
                     (list (make-ta "Ali" (list "A" "B"))))
              false)
(check-expect (solve (list (make-slot "B" 1) 
                           (make-slot "C" 1))
                     (list (make-ta "Ali" (list "A"))
                           (make-ta "Azi" (list "B"))))
              false)
(check-expect (solve (list (make-slot "A" 1)
                           (make-slot "B" 1)
                           (make-slot "C" 1)
                           (make-slot "D" 1))
                     (list (make-ta "Ali" (list "A" "B" "C" ))))
              false)
(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "C" 1))
                        (list (make-ta "Ali" (list "A" "B" "C" ))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "C" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" )))
                                    pairs)))
(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "C" 1)
                              (make-slot "D" 1))
                        (list (make-ta "Ali" (list "A" "B" "C" "D"))
                              (make-ta "Sam" (list "D"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "C" 1)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" "D"))
                                          (make-ta "Sam" (list "D")))
                                    pairs)))
(check-satisfied (solve SLOTS1 TAS1)
                 (lambda (pairs)
                   (valid-schedule? SLOTS1 TAS1 pairs)))
(check-satisfied (solve (list (make-slot "A" 1)
                              (make-slot "B" 1)
                              (make-slot "C" 1)
                              (make-slot "D" 1))
                        (list (make-ta "Ali"  (list "A" "B" "C" "D"))
                              (make-ta "Sam"  (list "B" "C"))
                              (make-ta "Alex" (list "C"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1)
                                          (make-slot "B" 1)
                                          (make-slot "C" 1)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali"
                                                   (list "A" "B" "C" "D"))
                                          (make-ta "Sam"
                                                   (list "B" "C"))
                                          (make-ta "Alex"
                                                   (list "C")))
                                    pairs)))
(check-satisfied (solve (list (make-slot "A" 1)
                              (make-slot "A" 2)
                              (make-slot "B" 1)
                              (make-slot "B" 2)
                              (make-slot "C" 1)
                              (make-slot "C" 2)
                              (make-slot "D" 1))
                        (list (make-ta "Ali"
                                       (list "A" "B" "C"))
                              (make-ta "Sam"
                                       (list "A" "B" "C" "D"))
                              (make-ta "Ari" (list "A"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "A" 2)
                                          (make-slot "B" 1)
                                          (make-slot "B" 2)
                                          (make-slot "C" 1)
                                          (make-slot "C" 2)
                                          (make-slot "D" 1))
                                    (list (make-ta "Ali"
                                                   (list "A" "B" "C"))
                                          (make-ta "Sam" 
                                                   (list "A" "B" "C" "D"))
                                          (make-ta "Ari" (list "A")))
                                    pairs)))

(check-satisfied (solve (list (make-slot "A" 1) 
                              (make-slot "B" 1)
                              (make-slot "B" 2)
                              (make-slot "C" 1)
                              (make-slot "C" 2)
                              (make-slot "D" 1)
                              (make-slot "E" 1))
                        (list (make-ta "Ali"
                                       (list "A" "B" "C" "D"))
                              (make-ta "Azi"
                                       (list "A" "B" "C" "E"))
                              (make-ta "Sam" (list "A"))))
                 (lambda (pairs)
                   (valid-schedule? (list (make-slot "A" 1) 
                                          (make-slot "B" 1)
                                          (make-slot "B" 2)
                                          (make-slot "C" 1)
                                          (make-slot "C" 2)
                                          (make-slot "D" 1)
                                          (make-slot "E" 1))
                                    (list (make-ta "Ali" (list "A" "B" "C" "D"))
                                          (make-ta "Azi" (list "A" "B" "C" "E"))
                                          (make-ta "Sam" (list "A")))
                                    pairs)))
(check-expect (solve (list (make-slot "A" 1) 
                           (make-slot "B" 1)
                           (make-slot "B" 2)
                           (make-slot "C" 1)
                           (make-slot "C" 2)
                           (make-slot "D" 1)
                           (make-slot "E" 1)
                           (make-slot "E" 1))
                     (list (make-ta "Ali" (list "A" "B" "C" "D" "E" ))
                           (make-ta "Azi" (list "A" "B" "C" "D" "E"))
                           (make-ta "Sam" (list "A"))))
              false)

(@template backtracking genrec arb-tree encapsulated)

(define (solve slots tas)
  (local [;;
          ;; trivial case: slots remaining to be scheduled is empty
          ;; reduction step: schedule first slot (or fail to do so)
          ;; argument: slots to schedule is finite, so reducing it by
          ;;           one each time will reach empty

          (define-struct ss (pairs slots))
          ;; SearchState is (make-ss (listof (listof String)) (listof Slot))
          ;; pairs is (list (list "ta name" "lab name")...)  ;a schedule
          ;; slots is the slots that remain to be scheduled
	  
          (define (search/one ss)
            (if (empty? (ss-slots ss))                 ;trivial?
                (ss-pairs ss)                          ;trivial-answer
                (search/list (next-search-states ss))))
          
          (define (search/list loss)
            (cond [(empty? loss) false]
                  [else
                   (local [(define try (search/one (first loss)))]
                     (if (not (false? try))
                         try
                         (search/list (rest loss))))]))

          ;;(@template fn-composition use-abstract-fn)
          (define (next-search-states ss)
            (local [(define pairs (ss-pairs ss))
                    (define slots (ss-slots ss))
                    (define slot  (first slots))]
              (map (lambda (ta)
                     ;; assign each available ta to the first slot
                     (make-ss (cons (list (ta-name ta) (slot-lab slot)) pairs)
                              (rest slots)))
                   (filter (lambda (ta)
                             (and (ta-listed-slot?         ta slot)
                                  (ta-has-more-time?       ta      pairs)
                                  (ta-not-already-working? ta slot pairs)))
                           tas))))

          ;; true if TA listed the slot as available time
          (define (ta-listed-slot? ta slot)
            (member (slot-lab slot) (ta-avail ta))) 

          ;; true if TA can work more given the current pairs
          (define (ta-has-more-time? ta pairs)
            (< (length (filter (lambda (p) (ta-pair? ta p))
                               pairs))
               MAX-SLOTS-PER-TA))

          ;; true if TA not already assigned to this lab given current pairs
          (define (ta-not-already-working? ta slot pairs)
            (empty?
             (filter (lambda (p) (ta-pair? ta p))
                     (filter (lambda (p) (slot-pair? slot p))
                             pairs))))
	  
          ;; true if the ta/slot is part of the pair
          (define (ta-pair?   ta   p) (string=? (first  p) (ta-name  ta)))
          (define (slot-pair? slot p) (string=? (second p) (slot-lab slot)))]
    
    (search/one (make-ss empty slots))))



;; code below here is to support autograder testing of solutions

(define (valid-schedule? slots tas pairs)
  (local [;(@template 2-one-of)
          (define (same-elements? l1 l2)
            (cond [(empty? l1) (empty? l2)]
                  [(empty? l2) false]
                  [else
                   (and (member (first l1) l2)
                        (same-elements? (rest l1) (remove (first l1) l2)))]))

          (define (listed-all-slots? ta pairs)
            (andmap (lambda (lab)
                      (member lab (ta-avail ta)))
                    (map second
                         (ta-pairs ta pairs))))

          (define (not-over-time? ta pairs)
            (<= (length (ta-pairs ta pairs))  MAX-SLOTS-PER-TA))

          (define (not-double-booked? ta pairs)
            (not (contains-duplicates? (map second (ta-pairs ta pairs)))))

          (define (contains-duplicates? lox)
            (cond [(empty? lox) false]
                  [else
                   (or (member (first lox) (rest lox))
                       (contains-duplicates? (rest lox)))]))

          (define (ta-pairs ta pairs)
            (filter (lambda (pair)
                      (string=? (first pair) (ta-name ta)))
                    pairs))]

    
    (and (not (false? pairs))
         (same-elements? (map slot-lab slots) ;lab names required
                         (map second pairs))  ;lab names filled
         (andmap (lambda (ta)
                   (and (listed-all-slots?  ta pairs)
                        (not-over-time?     ta pairs)
                        (not-double-booked? ta pairs)))
                 tas))))


