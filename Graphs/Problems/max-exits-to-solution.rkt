;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname max-exits-to-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require spd/tags)

(@assignment graphs-p5)
(@cwl ??? ???)

(@problem 1)
;; Using the following data definition, design a function that produces the room
;; to which the greatest number of other rooms have exits (in the case of a tie
;; you can produce any of the rooms in the tie).


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

#;
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


;; Helpful data definition:

(@htdd RoomsFrom)
;; RoomsFrom is (listof (list Room (listof String)))
;; interp. each element of the list (list Room (listof String)) is 
;;         a room and a list of room names that lead to it

(define RF (list (list (make-room "B" empty) (list "C" "A"))
                 (list (make-room "A" empty) (list "C"))))
;; RF says that rooms A and C lead to room B, and room C leads to A



;; =================
;; Functions:

(@htdf max-exits-to)
(@signature Room -> Room)
;; produce the room to which the greatest number of other rooms have exits

(check-expect (max-exits-to H1) (first (room-exits H1)))
(check-expect (max-exits-to H2) (shared ((-A- (make-room "A" (list -B-)))
                                         (-B- (make-room "B" (list -A-))))
                                  -B-))

(check-expect (max-exits-to H4) 
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -E-))

(@template Room (listof Room) accumulator encapsulated)
;(could add use-abstract-fn and fn-composition)

(define (max-exits-to r0)
  (local [;; RoomNExits is (make-rne Room Natural)
          ;; interp. the number of exits recorded for a given room
          (define-struct rne (r n))
          
          ;; todo is (listof Room); a worklist accumulator
          ;; visited is (listof String); context preserving accumulator, names
          ;;                             of rooms already visited
          ;; rsf is (listof RoomNExits); the number of exits leading to rooms so
          ;;                             far
          (define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (merge-room r rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo)  
                                (rest todo)
                                visited
                                rsf)]))
          ;; add one new room to rsf
          (define (merge-room r rsf)
            (foldr merge-exit rsf (room-exits r)))
          ;; add one new exit to rsf
          (define (merge-exit r lorne)
            (cond [(empty? lorne) (list (make-rne r 1))]
                  [else
                   (if (string=? (room-name r)
                                 (room-name (rne-r (first lorne))))
                       (cons (make-rne r
                                       (add1 (rne-n (first lorne))))
                             (rest lorne))
                       (cons (first lorne)
                             (merge-exit r (rest lorne))))]))
          ;; pick the room from rsf that has the most exits to it
          (define (pick-max rsf)
            (rne-r
             (foldr (λ (e1 e2)
                      (if (> (rne-n e1) (rne-n e2))
                          e1
                          e2))
                    (first rsf)
                    (rest rsf))))]
    ;; function composition
    (pick-max (fn-for-room r0 empty empty empty))))