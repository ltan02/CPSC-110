;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |count-rooms-solution - union recipe|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require spd/tags)

(@assignment graphs-p2)
(@cwl ??? ???)

(@problem 1)
;; Using the following data definition, design a function that consumes a room 
;; and produces the total number of rooms reachable from the given room. Include
;; the starting room itself. Your function should be tail recursive, but you
;; should not use the primitive length function.


;; =================
;; Data Definitions: 

(@htdd Room)
(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

;; Link to image of H1:
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/H1.PNG

(define H1 (make-room "A" (list (make-room "B" empty))))

;; Link to image of H2:
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/H2.PNG

(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 

;; Link to image of H3:
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/H3.PNG

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
           
;; Link to image of H4:
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/H4.PNG

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

(@template Room (listof Room) accumulator encapsulated)

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms
  ;;                             already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ;(... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))



;; =================
;; Functions:

(@htdf count-rooms)
(@signature Room -> Natural)
;; produces the number of rooms reachable from the given room
(check-expect (count-rooms H1) 2)
(check-expect (count-rooms H3) 3)
(check-expect (count-rooms H4) 6)

;structural recursion only (not tail recursion)
(@template Room (listof Room) accumulator encapsulated)

#;
(define (count-rooms r0)
  ;; path is (listof String); context preserving accumulator, names of rooms
  (local [(define (fn-for-room r  path) 
            (if (member (room-name r) path)
                empty
                (cons (room-name r)
                      (fn-for-lor (room-exits r) 
                            (cons (room-name r) path)))))
          (define (fn-for-lor lor path)
            (cond [(empty? lor) empty]
                  [else
                   (union (fn-for-room (first lor) path)
                          (fn-for-lor (rest lor) path))]))]
    (length (fn-for-room r0 empty))))

(@template Room (listof Room) accumulator encapsulated)

(define (count-rooms r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms
  ;;                             already visited
  ;; rsf is Natural; the number of rooms seen so far
  (local [(define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited) (add1 rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited rsf)]))] 
    (fn-for-room r0 empty empty 0)))


(@htdf union)
(@signature (listof X) (listof X) -> (listof X))
;; produce list containing all elements of l1 and l2 w/ no duplicates
;; ASSUME: l1 and l2 have no duplicates themselves
(check-expect (union empty empty) empty)
(check-expect (union empty (list "a")) (list "a"))
(check-expect (union (list "b") empty) (list "b"))
(check-expect (union (list "a") (list "b")) (list "a" "b"))
(check-expect (union (list "a") (list "a")) (list "a"))
(check-expect (union (list "a" "b" "c" "e") (list "b" "c" "d" "e"))
              (list "a" "b" "c" "e" "d"))

;;Cross Product of Types Table:
;;
;; ╔════════════════╦═══════════════╦═════════════════════════╗
;; ║                ║               ║                         ║
;; ║            l1  ║     empty     ║   (cons x (listof X))   ║
;; ║                ║               ║                         ║
;; ║    l2          ║               ║                         ║
;; ╠════════════════╬═══════════════╬═════════════════════════╣
;; ║                ║               ║                         ║
;; ║     empty      ║               ║           l1 [2]        ║
;; ║                ║               ║                         ║
;; ╠════════════════╣               ╠═════════════════════════╣  
;; ║                ║               ║                         ║
;; ║                ║     l2 [1]    ║(cons (first l1)      [3]║
;; ║(cons X         ║               ║      (union (rest l1)   ║
;; ║      (listof X)║               ║             (remove     ║
;; ║                ║               ║              (first l1) ║
;; ║                ║               ║              l2)))      ║ 
;; ║                ║               ║                         ║
;; ╚════════════════╩═══════════════╩═════════════════════════╝

(@template 2-one-of)

(define (union l1 l2)
  (cond [(empty? l1) l2]  ;(1)
        [(empty? l2) l1]  ;(2)
        [else
         (cons (first l1) ;(3)
               (union (rest l1)
                      (remove
                       (first l1) l2)))]))