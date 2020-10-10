;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

(@assignment lab-06)
(@cwl atan12) 

;; DATA DEFINITIONS ===============
;; Data definition for sentence tree
(@problem 1)
(@htdd SentenceTree)
(define-struct stree (prefix subs))
;; SentenceTree is (make-stree String (listof SentenceTree))
;; interp: an stree with:
;;             prefix the current phrase
;;             subs a list of stree

(define ST1 (make-stree "KISS ME"
                        (list (make-stree "JOKING ABOUT JEALOUSY" empty)
                              (make-stree "LIKE"
                                          (list (make-stree "YOU REALLY MEAN IT" empty)
                                                (make-stree "WE ARE"
                                                            (list (make-stree "IN A BACK TO SCHOOL ABOUT MONO" empty)
                                                                  (make-stree "PERCHED ON THE TOP OF A SINKING SHIP" empty)))))
                              (make-stree "TO"
                                          (list (make-stree "FREEZE TIME" empty)
                                                (make-stree "MY FAVOURITE SONG ON REPEAT" empty))))))
#;
(define (fn-for-stree st)
  (local [(define (fn-for-stree st)
            (... (stree-prefix st)
                 (fn-for-lost (stree-subs st))))

          (define (fn-for-lost lost)
            (cond [(empty? lost) (...)]
                  [else
                   (... (fn-for-stree (first lost))
                        (fn-for-lost (rest lost)))]))]
    (fn-for-stree st)))

;; FUNCTIONS ======================
;; Introspection function (stree->list)
(@problem 2)
(@htdf sentence-count)
(@signature SentenceTree -> Natural)
;; produces the number of sentences in a sentence tree
(check-expect (sentence-count ST1) 6)

;(define (sentence-count st) 1) ;stub

(define (sentence-count st0)
  (local [(define (fn-for-stree st)
            (if (empty? (stree-subs st))
                (+ 1 (fn-for-lost (stree-subs st)))
                (fn-for-lost (stree-subs st))))

          (define (fn-for-lost lost)
            (cond [(empty? lost) 0]
                  [else
                   (+ (fn-for-stree (first lost))
                      (fn-for-lost (rest lost)))]))]
    (fn-for-stree st0)))

(@problem 3)

(define (render st0)
  (local [(define (fn-for-stree st)
            (beside (text (stree-prefix st) 15 "black")
                 (fn-for-lost (stree-subs st))))

          (define (fn-for-lost lost)
            (cond [(empty? lost) empty-image]
                  [else
                   (above/align "left"
                                (fn-for-stree (first lost))
                                (fn-for-lost (rest lost)))]))]
    (fn-for-stree st0)))



(@problem 4)
