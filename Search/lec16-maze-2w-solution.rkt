;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec16-maze-2w-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

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

(@htdd solvable?)
(@signature Maze -> Boolean)
;; produce true if maze is solvable, false otherwise
;; CONSTRAINT: maze has an empty square at least in the upper left
(check-expect (solvable? M1) #t)
(check-expect (solvable? M2) #t)
(check-expect (solvable? M3) #t) 
(check-expect (solvable? M4) #f)

(@template encapsulated backtracking genrec arb-tree)

#|
YOU WOULD NOT NORMALLY LEAVE THIS STEP BY STEP BLENDING OF THE
TEMPLATES IN YOUR WORK, BUT I AM PUTTING IT HERE TO HELP UNDERSTAND
HOW WE GOT THERE.

(define (solvable? m)              ;encapsulated
  (local []

    (... (make-pos 0 0))))

(define (solvable? m)              ;+arb-tree
  (local [(define (fn-for-pos p)
            (... (pos-x p)
                 (pos-y p)
                 (fn-for-lop (pos-sub-poses p)))) ;this selector doesn't exist

          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-pos (first lop))
                        (fn-for-lop (rest lop)))]))]

    (... (make-pos 0 0))))


(define (solvable? m)              ;+genrec
  ;; Base case:
  ;; reduction:
  ;; argument:
  (local [(define (fn-for-pos p)
            (if (solved? p)
                true
                (fn-for-lop (next-valid-positions p))))


          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-pos (first lop))
                        (fn-for-lop (rest lop)))]))]

    (... (make-pos 0 0))))

|#
(define (solvable? m)            ;+backtracking
  ;; Base case: maze is solved, or there are no valid moves
  ;; reduction step: right and down, if valid
  ;; argument: we only ever move right and down, and maze has
  ;;           finite size, so we will run out of moves to make
  (local [(define (fn-for-pos p)
            (if (solved? p m)
                true
                (fn-for-lop (valid-next-positions p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) false]
                  [else
                   (local [(define try (fn-for-pos (first lop)))]
                     (if (not (false? try))
                         try		       
                         (fn-for-lop (rest lop))))]))

          (define W/H (sub1 (sqrt (length m))))  ;width and height

          ;; (@signature Position -> Boolean)
          ;; produce true if p represents the lower right corner of a maze
          ;; (@template Position add-param)
          (define (solved? p m)
            (= (pos-x p) (pos-y p) W/H))

          ;; (@signature Position Maze -> (listof Position))
          ;; Produce a list of up to two valid next positions (right and down)
          ;; (@template fn-composition encapsulated)
          (define (valid-next-positions p)
            (filter valid? (all-next-positions p)))

          (define (all-next-positions p)
            (local [(define x (pos-x p))
                    (define y (pos-y p))]
              (list (make-pos (add1 x)      y)
                    (make-pos       x (add1 y)))))
          
          (define (valid? p)
            (and (<= 0 (pos-x p) W/H)
                 (<= 0 (pos-y p) W/H)
                 (mref m p)))]

    (fn-for-pos (make-pos 0 0))))





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
  (local [(define SIZE (sqrt (length m)))          ;4x4 this is 4
          (define IDXS (build-list SIZE identity)) ;4x4 this is (list 0 1 2 3)
          
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
