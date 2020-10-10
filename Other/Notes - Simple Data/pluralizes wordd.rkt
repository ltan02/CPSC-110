;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |pluralizes wordd|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Problem: Design a function that pluralizes a given word. (Pluralize means to convert to its plural form.) For simplicity you may assume that just adding s is enough to pluralize a word

(check-expect (pluralize "open") "opens")
(check-expect (pluralize "men") "mens")

(define (pluralize x)
  (string-append x "s"))