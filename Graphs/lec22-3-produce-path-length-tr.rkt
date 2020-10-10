;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec22-produce-path-length) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

;; Solve simple square mazes

;; Data definitions:

(@htdd Maze)
;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;;
(define (open? v) v)
(define (wall? v) (not v))

(define O #t) ;Open
(define W #f) ;Wall

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        W O W W W
        W O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3             ;forces backtracking in this solver
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))


(define M5
  (list O O O O O
        O W O W O
        O O O O O
        O W O W O
        W O O W W))

(define M6
  (list O O O O O O O O O O
        W W O W W O W W W O
        O O O W W O W O O O
        O W O O W O W O W W
        O W W O W O W O O O
        O W W O W O W W W O
        O W W O W O W O O O
        O W W O O O W O W W
        O O O O W W W O O O
        W W W W W O O W W O))



(@htdd Pos)
(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         the SIZE of a maze is (sqrt (length m))
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 SIZE))
;;            - (<= 0 y (sub1 SIZE))
;;            - there is a true in the given cell
;;                         ;in a 5x5 maze:
(define P0 (make-pos 0 0)) ;upper left
(define P1 (make-pos 4 0)) ;upper right
(define P2 (make-pos 0 4)) ;lower left
(define P3 (make-pos 4 4)) ;lower right

;; Functions

;; You must NOT have path accumulator, you can never call length
;; or your own version of length, you MUST counts steps as you go
;; it MUST be tail recursive

(@htdf solve)
(@signature Maze -> Boolean)
;; produce LENGTH OF FIRST PATH IN MAZE if maze is solvable, false otherwise
;; CONSTRAINT: maze has a true at least in the upper left
(check-expect (solve M1) 9)
(check-expect (solve M2) 9)
(check-expect (solve M3) 9) 
(check-expect (solve M4) 13)
(check-expect (solve M5) #f)
(check-expect (solve M6) 41)


(@template encapsulated backtracking genrec arb-tree accumulator)

(define (solve m)
  (local [(define R (sqrt (length m)))
          (define-struct wle (p c))
          ;; WLE is (make-wle Pos Natural)
          ;; interp. position to call solve/p with, and number of steps
          ;;         IN THE MAZE to that position

          ;; tail recursion, with visited accumulator, compound wles
          ;; todo    is (listof WLE); worklist
          ;; visited is (listof Pos); every position ever visited
          ;; count   is Natural; number of positions on path before p
          (define (solve/p p todo visited c)
            (cond [(solved? p) (add1 c)]
                  [(member p visited) (solve/lop todo visited)]
                  [else
                   (solve/lop (append (map (lambda (p1) (make-wle p1 (add1 c)))
                                           (next-ps p))
                                      todo)
                              (cons p visited))]))

          (define (solve/lop todo visited)
            (cond [(empty? todo) false]
                  [else
                   (solve/p (wle-p (first todo))
                            (rest todo)
                            visited
                            (wle-c (first todo)))]))
          
          ;; Pos -> Boolean          
          ;; produce true if pos is at the lower right
          (define (solved? p)
            (and (= (pos-x p) (sub1 R))
                 (= (pos-y p) (sub1 R))))


          ;; Pos -> (listof Pos)
          ;; produce next  legal positions
          (define (next-ps p)
            (local [(define x (pos-x p))
                    (define y (pos-y p))]
              (filter (lambda (p1)
                        (and (<= 0 (pos-x p1) (sub1 R))  ;legal x
                             (<= 0 (pos-y p1) (sub1 R))  ;legal y
                             (open? (maze-ref m p1))))   ;open?
                      (list (make-pos x (sub1 y))        ;up
                            (make-pos x (add1 y))        ;down
                            (make-pos (sub1 x) y)        ;left
                            (make-pos (add1 x) y)))))    ;right

          ;; Maze Pos -> Boolean
          ;; produce contents of maze at location p
          ;; assume p is within bounds of maze
          (define (maze-ref m p)
            (list-ref m (+ (pos-x p) (* R (pos-y p)))))]
    
    (solve/p (make-pos 0 0) empty empty 0)))

(require 2htdp/image)

(define SQUARE-SZ 20)
(define GOAL-SZ 8)
(define DOT-SZ 2)

(define DOT (circle DOT-SZ "solid" "black"))

(define OS (square SQUARE-SZ "outline" "white"))
(define WS (square SQUARE-SZ "solid" "black"))

(@htdf render-maze)
(@signature Maze -> Image)
;; produce simple rendering of MAZE using above constants
(check-expect
 (render-maze (list O W W O))
 (place-image OS (* .5 SQUARE-SZ) (* .5 SQUARE-SZ)
              (place-image WS (* 1.5 SQUARE-SZ) (* 0.5 SQUARE-SZ)
                           (place-image WS (* 0.5 SQUARE-SZ) (* 1.5 SQUARE-SZ)
                                        (place-image
                                         OS (* 1.5 SQUARE-SZ) (* 1.5 SQUARE-SZ)
                                         (square (* 2 SQUARE-SZ)
                                                 "outline" "black"))))))
                           

(define (render-maze m)
  (local [(define S (sqrt (length m)))

          (define BKGRD (square (* S SQUARE-SZ) "outline" "black"))

          ;; foldr w/ extra accumulator
          ;; i is Integer; index number of (first lov) in original m          
          (define (fold lov i img)
            (cond [(empty? lov) img]
                  [else
                   (place-image (if (first lov) OS WS)
                                (i->x i)
                                (i->y i)
                                (fold (rest lov) (add1 i) img))]))

          (define (i->x i) (+ (* (remainder i S) SQUARE-SZ) (/ SQUARE-SZ 2)))
          (define (i->y i) (+ (* (quotient  i S) SQUARE-SZ) (/ SQUARE-SZ 2)))]
    
    (fold m 0 BKGRD)))