;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lecture 4 - Notes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)

;Interactive Programs

;Big bang is polymorphic: it works for any type of world state

#;
(big-bang <world-state>               ; X
          (on-tick <function>)        ; X -> X
          (to-draw <function>))       ; X -> Image

; All the X needs to be the same type

;Domain Analysis: cat-starter.rkt
;Program through main Function: cat-starter.rkt