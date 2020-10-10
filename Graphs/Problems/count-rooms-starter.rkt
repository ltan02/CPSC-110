;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname count-rooms-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
;; produce the number of rooms reachable from a given room
(check-expect (count-rooms H1) 2)
(check-expect (count-rooms H4) 6)

;(define (count-rooms r) 1) ;stub

(@template Room (listof Room) encapsulated accumulator)
(define (count-rooms r0)
  ;; todo is (listof Room) ; worklist accumulator
  ;; visited is (listof Room) ; a list of rooms visited already
  ;; rsf is Natural ; the number of rooms seen so far
  (local [(define (fn-for-room r todo visited rsf)
            (if (member r visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons r visited)
                            (add1 rsf))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                rsf)]))]
    (fn-for-room r0 empty empty 0)))