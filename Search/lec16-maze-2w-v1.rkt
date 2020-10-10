;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec18-maze-2w-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

;; maze-v1.rkt

;; Solve simple square mazes

;; Constants:


;; Data definitions:

(@htdd Maze)
;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;; CONSTRAINT: maze is square, so (sqrt (length <maze>)) is a Natural
;;

(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

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

(@htdd Position)

(define-struct pos (x y))
;; Position is (make-pos Natural Natural)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower right  "  "   "

;; Functions:

(@htdf mref)
(@signature Maze Position -> Boolean)
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(@template Position)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))












(require 2htdp/image)

(define CELL-SIZE 30)
(define OPEN (square CELL-SIZE "solid" "light gray"))
(define WALL (square CELL-SIZE "solid" "black"))

(@htdf render)
(@signature Maze -> Image)
;; produce simple rendering of a square maze.
(check-expect (render empty) empty-image)
(check-expect (render (list #t #f #t
                            #t #f #f
                            #f #f #f))
              (above (beside OPEN WALL OPEN)
                     (beside OPEN WALL WALL)
                     (beside WALL WALL WALL)))

(@template encapsulated fn-composition use-abstract-fn)

(define (render m)
  (local [(define SIZE (sqrt (length m)))         ;in 4x4 this is 4
          (define IDXS (build-list SIZE identity));in 4x4 this is (list 0 1 2 3)
          
          (define (cv->image cv)
            (cond [cv   OPEN]
                  [else WALL]))]
    
    (foldr (lambda (i img)
             (above (foldr (lambda (j img)
                             (beside (cv->image (mref m (make-pos j i)))
                                     img))
                           empty-image
                           IDXS)
                    img))
           empty-image
           IDXS)))
