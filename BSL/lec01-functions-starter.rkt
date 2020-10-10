;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec01-functions-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)


;;
;; Already we can start to build up complex results, such as:
;;

(above (circle 40 "solid" "red")         
       (circle 40 "solid" "yellow")
       (circle 40 "solid" "green"))






;; 
;; PROBLEM:
;; 
;; Write a function that will help eliminate the
;; redundancy in the following expressions:
;; 

(* 3.14 (sqr 3))
(* 3.14 (sqr 6))
(* 3.14 (sqr 2.5))

;; 
;; It is always OK to use paper and pencil rather than DrRacket in lecture. 

(define (area radius)
  (* 3.14 (sqr radius)))

(area 3)
(area 6)
(area 2.5)
;; 
;; PROBLEM:
;; 
;; Consider this function:

(define (silly x)
  (* 2 (sqr x)))

;; Write out step-by-step evaluation of: 
;; (silly (* 1 3))
;; 
;; 

(silly (* 1 3))
(silly 3)
(* 2 (sqr 3))
(* 2 9)
18


;;
;; Some optional fun:
;;
;; Given:
;;

(require 2htdp/image)
(require 2htdp/universe)

(define MTS (empty-scene 100 100))
(define STAR (radial-star 8 8 48 "solid" "darkslategray")) 
               
;; Write the definition for a function that will help eliminate the
;; redundancy in the following expressions:

(overlay (rotate 10 STAR) MTS)
(overlay (rotate 20 STAR) MTS)

;; Call your function star-at-angle and when you are done
;; try evaluating (animate star-at-angle)

(define (star-at-angle t)
  (overlay (rotate t STAR) MTS))

(animate star-at-angle)

