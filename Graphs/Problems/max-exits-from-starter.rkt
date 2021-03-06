;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname max-exits-from-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require spd/tags)

(@assignment graphs-p4)
(@cwl ??? ???)

(@problem 1)
;; Using the following data definition, design a function that produces the room
;; with the most exits (in the case of a tie you can produce any of the rooms in
;; the tie).


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
(@htdf max-exits)
(@signature Room -> Room)
;; produce the room with the most exits
(check-expect (max-exits H1) H1)
(check-expect (max-exits H2) H2)
(check-expect (max-exits (shared ((-A- (make-room "A" (list -B- -D-)))
                                  (-B- (make-room "B" (list -C- -E- -F-)))
                                  (-C- (make-room "C" (list -B-)))
                                  (-D- (make-room "D" (list -E-)))
                                  (-E- (make-room "E" (list -F- -A-)))
                                  (-F- (make-room "F" (list))))
                           -A-))
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E- -F-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -B-))

;(define (max-exits r) r) ;stub

(@template Room (listof Room) encapsulated accumulator)
(define (max-exits r0)
  ;; todo is (listof Room) ; worklist accumulator
  ;; visited is (listof Room) ; rooms visited already
  ;; rsf is Room ; room with most exits seen so far
  (local [(define (fn-for-room r todo visited rsf)
            (if (member r visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons r visited)
                            (if (> (length (room-exits r))
                                   (length (room-exits rsf)))
                                r
                                rsf))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                rsf)]))]
    (fn-for-room r0 empty empty r0)))
