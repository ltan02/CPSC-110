;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pset-11-solution-tr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-11); Do not edit or remove this tag
(@cwl ??? ???)       ; Replace ??? with your cwl,
;;                   ; second ??? is replaced with partner cwl if you have one

(@problem 1)

;;
;; Please read through the MODIFIED data definition introduced in Problem Set 6
;; for Treasure that can be found in a Scavenger Hunt. It has been modified
;; to add route durations to travel between treasure boxes.
;;

(@htdd Status)
;; Status is one of:
;; - "buried"
;; - "sunken"
;; - "locked"
;; interp. the status of an unopened treasure box
;;<examples are redundant for enumeration>

(@htdd Treasure)
(define-struct treasure (label amount difficulty status routes))
;; Treasure is (make-treasure String Natural Natural Status (listof Route))
;; interp. a treasure box with a label name,
;;         the number of gold coins contained in the treasure box,
;;         a rating of difficulty to find and open the treasure box between 1
;;         and 5, where 1 is very easy to find and open and 5 is very difficult,
;;         the status of the treasure box before it was opened,
;;         and a list of routes leading from this treasure box
;;         to other treasure boxes

(@htdd Route)
(define-struct route (duration destination))
;; Route is (make-route Natural Treasure)
;; interp. a route leading from one treasure box to another       
;;         destination is the treasure box the route leads to. and
;;         duration is the time in hours it will take to travel to it

(define TREASURE-MAP
  (shared [(-T1- (make-treasure "E" 32 3 "buried" (list (make-route 3 -T10-))))
           (-T2- (make-treasure "F" 10 2 "locked" (list (make-route 7 -T9-))))
           (-T3- (make-treasure "B" 6 5 "locked" (list (make-route 9 -T1-)
                                                       (make-route 15 -T2-))))
           (-T4- (make-treasure "J" 1 1 "sunken" (list (make-route 6 -T7-))))
           (-T5- (make-treasure "H" 17 2 "sunken" (list (make-route 15 -T4-)
                                                        (make-route 4 -T7-))))
           (-T6- (make-treasure "G" 52 3 "buried" (list (make-route 2 -T8-))))
           (-T7- (make-treasure "I" 100 5 "locked" empty))
           (-T8- (make-treasure "D" 21 1 "sunken" (list (make-route 8 -T6-)
                                                        (make-route 13 -T5-)
                                                        (make-route 9 -T7-)
                                                        (make-route 11 -T10-))))
           (-T9- (make-treasure "C" 41 4 "buried" (list (make-route 6 -T6-))))
           (-T10-(make-treasure "A" 7 1 "locked" (list (make-route 12 -T3-)
                                                       (make-route 7 -T9-)
                                                       (make-route 27 -T8-))))]
    (list -T1- -T2- -T3- -T4- -T5- -T6- -T7- -T8- -T9- -T10-)))

(define T1  (list-ref TREASURE-MAP 0))
(define T2  (list-ref TREASURE-MAP 1))
(define T3  (list-ref TREASURE-MAP 2))
(define T4  (list-ref TREASURE-MAP 3))
(define T5  (list-ref TREASURE-MAP 4))
(define T6  (list-ref TREASURE-MAP 5))
(define T7  (list-ref TREASURE-MAP 6))
(define T8  (list-ref TREASURE-MAP 7))
(define T9  (list-ref TREASURE-MAP 8))
(define T10 (list-ref TREASURE-MAP 9))
                        
(define (fn-for-treasure t)
  (local [(define (fn-for-status s)
            (cond [(string=? s "buried") (...)]
                  [(string=? s "sunken") (...)]
                  [(string=? s "locked") (...)]))

          (define (fn-for-treasure t)
            (... (treasure-label t)
                 (treasure-amount t)
                 (treasure-difficulty t)
                 (fn-for-status (treasure-status t))
                 (fn-for-lor (treasure-routes t))))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-route (first lor))
                        (fn-for-lor (rest lor)))]))

          (define (fn-for-route r)
            (... (route-duration r)
                 (fn-for-treasure (route-destination r))))]
    
    (fn-for-treasure t)))


;;
;; Design a function that consumes a treasure and produces the total amount
;; of gold that can be obtained by opening that treasure, and all treasures
;; reachable from that treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf reachable-gold)
(@signature Treasure -> Natural)
;; produce the total amount of reachable gold
(check-expect (reachable-gold T7) 100)
(check-expect (reachable-gold T5) (+ 17 1 100))
(check-expect (reachable-gold T10) (+ 7 6 32 10 41 52 21 17 1 100))

(@template Treasure (listof Route) Route accumulator)

(define (reachable-gold t)
  ;; rsf is Natural: total gold found so far
  ;; todo is (listof Route): worklist accumulator of routes to visit
  ;; visited is (listof String): labels of treasures already visited
  (local [(define (fn-for-treasure t todo visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor todo visited rsf)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) todo)
                               (cons (treasure-label t) visited)
                               (+ (treasure-amount t) rsf))]))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-route (first todo) (rest todo) visited rsf)]))

          (define (fn-for-route r todo visited rsf)
            (fn-for-treasure (route-destination r) todo visited rsf))]
    
    (fn-for-treasure t empty empty 0)))


(@problem 2)
;;
;; Complete the function that lists the label names of all reachable treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf all-labels)
(@signature Treasure -> (listof String))
;; produce the label names of all reachable treasures
(check-expect (all-labels T7) (list "I"))
(check-expect (all-labels T5) (list "H" "J" "I"))
(check-expect (all-labels T10) (list "A" "B" "E" "F" "C"
                                     "G" "D" "H" "J" "I"))

(@template Treasure (listof Route) Route accumulator)

(define (all-labels t)
  ;; todo is (listof Route): worklist accumulator of routes to visit
  ;; visited is (listof String): labels of treasures already visited
  (local [(define (fn-for-treasure t todo visited)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor todo visited)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) todo)
                               (cons (treasure-label t) visited))]))

          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (reverse visited)]
                  [else
                   (fn-for-route (first todo) (rest todo) visited)]))

          (define (fn-for-route r todo visited)
            (fn-for-treasure (route-destination r) todo visited))]
    
    (fn-for-treasure t empty empty)))


(@problem 3)
;;
;; Complete the function that lists the labels names of all reachable treasures
;; when following only routes with a duration less than n hours long.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf short-dur-reachable)
(@signature Treasure Number -> (listof String))
;; produce labels of all treasures reachable by following routes < n hr long
(check-expect (short-dur-reachable T7 12) (list "I"))
(check-expect (short-dur-reachable T5 4)  (list "H"))
(check-expect (short-dur-reachable T5 5)  (list "H" "I"))
(check-expect (short-dur-reachable T5 16) (list "H" "J" "I"))
(check-expect (short-dur-reachable T10 9) (list "A" "C" "G" "D"))

(@template Treasure (listof Route) Route accumulator)

(define (short-dur-reachable t n)
  ;; todo is (listof Route): worklist accumulator of routes to visit
  ;; visited is (listof String): labels of treasures already visited
  (local [(define (fn-for-treasure t todo visited)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor todo visited)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) todo)
                               (cons (treasure-label t) visited))]))

          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (reverse visited)]
                  [else
                   (fn-for-route (first todo) (rest todo) visited)]))

          (define (fn-for-route r todo visited)
            (if (< (route-duration r) n)
                (fn-for-treasure (route-destination r) todo visited)
                (fn-for-lor todo visited)))]
    
    (fn-for-treasure t empty empty)))


(@problem 4)
;;
;; Complete the design of a function that consumes two treasures and counts
;; the number of routes reachable from t1 that lead to t2.
;;
;; Note: This is counting the number of routes found in treasure boxes where t2
;; is the destination, NOT the total number of paths between the two treasure
;; boxes. It is asking how many routes have t2 as their destination (how many
;; arrows are pointing to t2).
;;
;; Examples:
;;
;; (num-lead-to T10 T7) produces 3. This is because there are three routes that
;; are reachable from T10 that lead to T7. These routes are the route leading
;; from T5 to T7, the route leading from T4 to T7, and the route leading from
;; T8 to T7.
;;
;; (num-lead-to T7 T10) produces 0. Even though two routes lead to T10 (the
;; route from T8 to T10 and the route from T1 to T10), neither route can be
;; reached from T7 so the function produces 0.
;;
;; Note that you can use the built-in function equals? to compare if two
;; treasures (or any define-structs) are equal. For example:
;;   - (equals? T1 T2) produces false
;;   - (equals? T3 T3) produces true
;;
;; Your solution MUST be tail recursive.
;;

(@htdf num-lead-to)
(@signature Treasure Treasure -> Natural)
;; count the number of reachable routes that lead to t2
(check-expect (num-lead-to T7 T10) 0)
(check-expect (num-lead-to T4 T7) 1)
(check-expect (num-lead-to T5 T7) 2)
(check-expect (num-lead-to T10 T7) 3)
(check-expect (num-lead-to T2 T10) 2)
(check-expect (num-lead-to T8 T9) 2)

(@template Treasure (listof Route) Route accumulator)

(define (num-lead-to t1 t2)
  ;; rsf is Natural: number of routes leading to t2 found so far
  ;; todo is (listof Route): worklist accumulator of routes to visit
  ;; visited is (listof String): label names of treasures already visited
  (local [(define (fn-for-treasure t todo visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor todo visited rsf)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) todo)
                               (cons (treasure-label t) visited)
                               rsf)]))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-route (first todo) (rest todo) visited rsf)]))

          (define (fn-for-route p todo visited rsf)
            (if (equal? (route-destination p) t2)
                (fn-for-treasure (route-destination p) todo visited (add1 rsf))
                (fn-for-treasure (route-destination p) todo visited rsf)))]
    
    (fn-for-treasure t1 empty empty 0)))


(@problem 5)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the time in hours of route durations
;; it would take to travel from t to the treasure labeled s. This function
;; produces the total duration of the routes followed as soon as it finds a
;; treasure labeled s. The function produces false if there is no way of
;; reaching a treasure with the given label.
;;

(@htdf route-to)
(@signature Treasure String -> Natural or false)
;; produce the total duration traveled on route to s from t, false if not found
(check-expect (route-to T1 "X") false)
(check-expect (route-to T1 "E") 0)
(check-expect (route-to T1 "A") 3)
(check-expect (route-to T5 "I") 21)
(check-expect (route-to T10 "G") 40)
(check-expect (route-to T10 "J") 70)

(@template Treasure (listof Route) Route accumulator backtracking) 
#;
(define (route-to t s)
  ;; rsf is Natural: duration of travel on route so far
  ;; visited is (listof String): labels of treasures visited so far
  (local [(define (fn-for-treasure t visited rsf)
            (cond [(member? (treasure-label t) visited) false]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (treasure-routes t)
                                    (cons (treasure-label t) visited)
                                    rsf)]))

          (define (fn-for-lor lor visited rsf)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (fn-for-route (first lor) visited rsf))]
                     (if (not (false? try))
                         try
                         (fn-for-lor (rest lor) visited rsf)))]))

          (define (fn-for-route r visited rsf)
            (fn-for-treasure (route-destination r)
                             visited
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty 0)))

(define (route-to t s)
  ;; rsf is Natural: duration of travel on route so far
  ;; visited is (listof String): labels of treasures visited so far
  ;; todo is (listof WLE): worklist accumulator
  (local [;; WLE is (make-wle Route Natural)
          ;; interp. worklist entry
          (define-struct wle (r rsf))
          
          (define (fn-for-treasure t todo visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor todo visited)]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (append (map (λ (r) (make-wle r rsf))
                                                 (treasure-routes t))
                                            todo)
                                    (cons (treasure-label t) visited))]))

          (define (fn-for-lor todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-route (wle-r (first todo))
                                 (rest todo)
                                 visited
                                 (wle-rsf (first todo)))]))

          (define (fn-for-route r todo visited rsf)
            (fn-for-treasure (route-destination r)
                             todo
                             visited
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty empty 0)))

(@problem 6)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the MINIMUM time in hours of all of the
;; possible routes that could be taken to travel from t to the treasure
;; labeled s. The function produces false if there is no way of reaching the
;; a treasure with the given label.
;;

(@htdf min-route-to)
(@signature Treasure String -> Natural or false)
;; produce the min duration traveled on route to s from t, false if not found
(check-expect (min-route-to T1 "X") false)
(check-expect (min-route-to T1 "E") 0)
(check-expect (min-route-to T1 "A") 3)
(check-expect (min-route-to T5 "I") 4)
(check-expect (min-route-to T10 "G") 13)
(check-expect (min-route-to T10 "J") 43)

(@template Treasure (listof Route) Route accumulator backtracking)

(define (min-route-to t s)
  ;; rsf is Natural: duration of travel on route so far
  ;; visited is (listof String): labels of treasures visited so far
  (local [(define (fn-for-treasure t visited rsf)
            (cond [(member? (treasure-label t) visited) false]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (treasure-routes t)
                                    (cons (treasure-label t) visited)
                                    rsf)]))

          (define (fn-for-lor lor visited rsf)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (fn-for-route (first lor) visited rsf))
                           (define try2 (fn-for-lor (rest lor) visited rsf))]
                     (if (not (false? try))
                         (if (not (false? try2))
                             (min try try2)
                             try)
                         try2))]))

          (define (fn-for-route r visited rsf)
            (fn-for-treasure (route-destination r)
                             visited
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty 0)))
