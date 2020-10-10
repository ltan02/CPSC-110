;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2018W1-F-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)

(@problem 1) ; cs id

(@htdd Movie)
(define-struct movie (title revenue genre rating))
;; Movie is (make-movie String Natural String Number)
;; interp. movie with English title, box office revenue
;;         (in millions of US dollars), genre, and
;;         Rotten Tomatoes percentage rating.


(define M1 (make-movie "Titanic" 659 "Romance" 89))
(define M2 (make-movie "Black Panther" 700 "Action" 97))
(define M3 (make-movie "Tomb Raider"  57 "Action" 20))
(define M4 (make-movie "Sound of Music" 163 "Musical" 86))
(define M5 (make-movie "Avengers Infinity War" 678 "Action" 84))

#;
(define (fn-for-movie m)
  (... (movie-title m)
       (movie-revenue m)
       (movie-genre m)
       (movie-rating m)))

;; Here is a helper function that is used below.  This helper function
;; is correct and you must not change it.

(@htdf match-genre?)
(@signature Movie String -> Boolean)
;; produce true if m has the genre g
(check-expect (match-genre? M1 "Action") false)
(check-expect (match-genre? M1 "Romance") true)
(check-expect (match-genre? M4 "Musical") true)

(@template Movie)

(define (match-genre? m g)
  (string=? (movie-genre m) g))

;; Problems 2 - 4 all involve different definitions of the genre-only function. 

(@htdf genre-only)
(@signature (listof Movie) String -> (listof String))
;; produce a list of movie titles from lom with g as the genre
(check-expect (genre-only empty "Romance") empty)
(check-expect (genre-only (list M1 M2 M3 M4 M5) "Action")
              (list "Black Panther" "Tomb Raider" "Avengers Infinity War"))
(check-expect (genre-only (list M1 M2 M3 M4 M5) "Musical")
              (list "Sound of Music"))



(@problem 2)
;; Below is a structurally recursive function definition.
;; As written it has a bug. Fix it so that it works properly.

(@template (listof Movie))
#;
(define (genre-only lom g)
  (cond [(empty? lom) empty]
        [else
         (if (match-genre? (first lom) g)
             (cons (movie-title (first lom))
                   (genre-only (rest lom) g))  ;<<< add g
             (genre-only (rest lom) g))]))     ;<<< add g



(@problem 3)
;; Below is a function definition using built-in abstract functions.
;; As written it has a bug. Fix it so that it works properly.

(@template fn-composition use-abstract-fn)
#;
(define (genre-only lom g)
  ;; close over g, with lambda or local
  (map movie-title (filter  (lambda (m) (match-genre? m g)) lom)))




(@problem 4)
;; Below is a tail recursive function definition.
;; As written it has a bug. Fix it so that it works properly.

(@template (listof Movie) accumulator)

(define (genre-only lom0 g)
  ;; rsf is (listof String); titles of movies with g as genre seen so far
  (local [(define (fn-for-lom lom rsf)
            (cond [(empty? lom) (reverse rsf)] ;(reverse rsf) instead of empty
                  [else
                   (if (match-genre? (first lom) g)
                       (fn-for-lom (rest lom)
                                   (cons (movie-title (first lom)) rsf))
                       (fn-for-lom (rest lom)
                                   rsf))]))]

    (fn-for-lom lom0 empty)))



(@problem 5)
;;
;; Please draw and label reference arrows on the type comments below.
;;          
;; 4 R  arrows
;; 2 SR arrows
;;
;;   Aisle is (make-aisle Natural ListOfCart)
;;
;;   ListOfCart is one of:
;;
;;     - empty
;;
;;     - (cons Cart ListOfCart)
;;
;;
;;   Cart is (make-cart ListOfItem)
;;
;;   ListOfItem is one of:
;;
;;     - empty
;;
;;     - (cons Item ListOfItem)
;;
;;
;;    Item is (make-item Image Number Number)



(@problem 6)
;;
;; In this problem you will complete a function definition using for-each.
;;
(@htdf skips)
(@signature (listof X) -> (listof X))
;; Skip 1, pick 1, skip 2, pick 1, skip 3, pick 1 ...
(check-expect (skips (list)) (list))
(check-expect (skips (list 1 2 3 4 5 6 7 8 9)) (list 2 5 9))

;; Complete the following function definition below. You MUST use for-each.
;; You MUST include type and invariant for any accumulators you use.
(@template for-each)

(define (skips lox)
  (local [(define cskip 1)      ;Natural, number to skip for current interval
          (define sls   0)      ;Natural, number skipped since last include
          (define rsf   empty)] ;(listof X), result so far (reversed)
    (begin (for-each (lambda (x)
                       (if (= sls cskip)
                           (begin (set! rsf (cons x rsf))
                                  (set! cskip (add1 cskip))
                                  (set! sls 0))
                           (begin (set! sls (add1 sls)))))
                     lox)
           (reverse rsf))))

(@problem 7)
;;
;; Design a function called rainfall that consumes a list of numbers
;; representing daily rainfall amounts as entered by a user. The list
;; may contain the number -999 indicating the end of the data of interest. 
;; Produce the average of the non-negative values in the list up to the
;; first -999 (if it shows up). There may be negative numbers other than
;; -999 in the list.

(@htdf rainfall)
(@signature (listof Number) -> Number)
;; produce average of non-negative numbers before -999; 0 if no such numbers
(check-expect (rainfall empty) 0)
(check-expect (rainfall (list -1 -999 2)) 0)
(check-expect (rainfall (list 2 3 -4 -999 1)) 2.5)
#;#;
(@template for-each)

(define (rainfall lon)
  ;; sum is Number; sum of non-negative numbers before -999 so far
  ;; count is Natural; number of non-negative numbers before -999 so far
  ;; sentinel? is Boolean; whether we have seen -999 so far
  (local [(define sum   0)
          (define count 0)
          (define sentinel? false)]
    (begin
      (for-each (lambda (n)
                  (cond [sentinel? (void)]
                        [(= n -999) (set! sentinel? true)]
                        [(negative? n) (void)]
                        [else
                         (begin (set! sum (+ sum n))
                                (set! count (add1 count)))]))
                lon)
      (if (zero? count) 0 (/ sum count))))) 


(@template fn-composition use-abstract-fn)

(define (rainfall lon)
  (local [(define (sentinel lon)
            (cond [(empty? lon) empty]
                  [else
                   (if (= (first lon) -999)
                       empty
                       (cons (first lon) (sentinel (rest lon))))]))

          (define (non-negative lon) (filter (compose not negative?) lon))
          (define (sum   lon)        (foldr + 0 lon))          
          (define (count lon)        (foldr (lambda (n c) (add1 c)) 0 lon))]

    (local [(define cleaned (non-negative (sentinel lon)))]    
      (if (empty? cleaned)
          0
          (/ (sum cleaned)
             (count cleaned))))))
          

(@problem 8)

;; In this problem you will be working with a simple representation of a secret
;; castle.  The figure below shows a small secret castle with 5 rooms named A,
;; B, C, D, and E.  A has exits that lead to rooms B, D and E.  B has a single
;; exit that leads to C, and so on.  The numbers in ovals (long circles) are
;; locks. The lock at room E requires key # 1 to open it.  The lock at room D
;; requires key number 2 to open it.  The underlined numbers are keys.  After
;; you get into room B you automatically pickup key 2.  After you get into room
;; C you automatically pickup key 1.

;; <image removed>

;; In this castle the only legal path from A to the room named E is A, B, C,
;; D,  E.  
;;
;; Note that in general a door might have multiple locks, and a room might
;; provide multiple keys.
;;
;; Here are the data definitions we use.

;; Data definitions:

(@htdd Room)
(define-struct room (name locks keys exits))
;; Room is (make-room String (listof Natural) (listof Natural) (listof Room))
;; interp. each room has
;;           - a Name
;;           - locks that require a key of the same number to open
;;           - keys that open locks of the same number
;;           - doors to other rooms
;;
;; NOTE: The keys can be for any rooms in the house, they do not have
;;       to be for one of the rooms in exits.
;;

(define HOUSE
  (shared ([A (make-room "A" (list)   (list)   (list B D E))]
           [B (make-room "B" (list)   (list 2) (list C))]
           [C (make-room "C" (list)   (list 1) (list A D))]
           [D (make-room "D" (list 2) (list)   (list B E))]
           [E (make-room "E" (list 1) (list)   (list))])
           
    (list A B C D E)))

(define A (list-ref HOUSE 0))
(define B (list-ref HOUSE 1))
(define C (list-ref HOUSE 2))
(define D (list-ref HOUSE 3))
(define E (list-ref HOUSE 4))

;; Design a function that consumes a Room and a String and tries to find
;; a path from the room to a room with the given name.  A path
;;   - cannot go into a room more than once
;;   - cannot go into a room unless the required keys are being held
;; You can only pick up the keys in a room once you are inside it.  You may
;; carry more than one key at a time. 
;;
;; If a path is possible your function should produce the list of room names
;; traversed in order.
;;    (find-path A "E") should produce (list "A" "B" "C" "D" "E")
;;    (find-path D "E") should produce false
;;
;; Your design must include signature, purpose, appropriate tests, @template
;; tag, and working function definition.  Your design must follow templating
;; and all other applicable design rules.  Any accumulators must have type and
;; invariant comments.
;;
;; Your function should NOT use tail recursion.  Just use ordinary structural
;; recursion.  We will mark solutions that use tail recursion, but the function
;; is harder to write that way so we advise against it.
;;
;; PLEASE TAKE YOUR TIME AND WRITE NEATLY.


(@htdf find-path)
(@signature Room String -> (listof String) or false)
;; try to find path between from and to, while enforcing door lock rules
(check-expect (find-path A "B") '("A" "B"))
(check-expect (find-path A "C") '("A" "B" "C"))
(check-expect (find-path A "D") '("A" "B" "C" "D"))
(check-expect (find-path A "E") '("A" "B" "C" "D" "E"))
(check-expect (find-path B "E") '(    "B" "C" "A" "D" "E"))
(check-expect (find-path C "E") '("C" "A" "E"))
(check-expect (find-path D "E") false)

(@template Room (listof Room) backtracking accumulator)
(define (find-path from to)
  ;; Path is (listof String);  names of rooms on this recursive path
  ;; keys is (listof Natural); keys collected on this recursive path
  (local [(define (fn-for-r r path keys)
            (cond [(member (room-name r) path) false]
                  [(not (andmap (lambda (k) (member k keys)) ;MUST precede next 
                                (room-locks r)))             ;string=? clause
                   false]
                  [(string=? (room-name r) to)
                   (reverse (cons (room-name r) path))]
                  [else
                   (fn-for-lor (room-exits r)
                               (cons (room-name r) path)
                               (append (room-keys r) keys))]))

          (define (fn-for-lor lor path keys)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (fn-for-r (first lor) path keys))]
                     (if (not (false? try))
                         try
                         (fn-for-lor (rest lor) path keys)))]))]

    (fn-for-r from empty empty)))


