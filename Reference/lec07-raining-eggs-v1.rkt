;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec08-raining-eggs-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

;;  Yoshi Eggs
(@htdw WS)

;; Constants:

(define WIDTH 400)
(define HEIGHT 600)

(define MARIO
  (bitmap/url "https://cs110.students.cs.ubc.ca/lectures/gk/lec07-mario.png"))

(define MTS
  (place-image MARIO
               (/ WIDTH 2)
               (- HEIGHT (/ (image-height MARIO) 2) 5)
               (empty-scene WIDTH HEIGHT)))

(define YOSHI-EGG 
  (bitmap/url "https://cs110.students.cs.ubc.ca/lectures/gk/lec07-egg.png"))

(define FALL-SPEED 5) ;pixels  per tick
(define SPIN-SPEED 5) ;degrees per tick


;; Data Definitions:

(@htdd WS)
;; WS is ... (give WS a better name)



;; =================
;; Functions:

(@htdf main)
(@signature WS -> WS)
;; start the world with ...
;;

(@template htdw-main)

(define (main ws)
  (big-bang ws           ; WS
    (on-tick   tock)     ; WS -> WS
    (to-draw   render)   ; WS -> Image
    (stop-when ...)      ; WS -> Boolean
    (on-mouse  ...)      ; WS Integer Integer MouseEvent -> WS
    (on-key    ...)))    ; WS KeyEvent -> WS

(@htdf tock)
(@signature WS -> WS)
;; produce the next ...
;; !!!
(define (tock ws) ...)


(@htdf render)
(@signature WS -> Image)
;; render ... 
;; !!!
(define (render ws) ...)
