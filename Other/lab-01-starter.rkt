;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-01-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Intro Lab
(require spd/tags)
(require 2htdp/image)

(@assignment lab-01)
(@cwl atan12)  ;; !!! REPLACE ??? with your cwl to hand this file in


(@problem 1)
;; Complete Problem 1 below using the following constants

(define PREFIX "hello")
(define SUFFIX "world")

(string-append PREFIX "_" SUFFIX)



(@problem 2)
;; Complete Problem 2 below using the following constants


(define STR "helloworld")
(define i 5)

(string-append (substring STR 0 i) "_" (substring STR i (string-length STR)))





(@problem 3)
;; Complete Problem 3 below using the following constant

(define CAT
  (bitmap/url
   "https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/cat.png"))









(@problem 4)
;; Complete Problem 4 below using CAT as defined above






(@problem 5)
;; Complete Problem 5 below using STR as defined above




