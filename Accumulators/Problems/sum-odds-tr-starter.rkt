;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sum-odds-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p7)
(@cwl ??? ???)

(@problem 1)
;; Consider the following function that consumes a list of numbers and produces
;; the sum of all the odd numbers in the list.
;;
;; Use an accumulator to design a tail-recursive version of sum-odds.


(@htdf sum-odds)
(@signature (listof Number) -> Number)
;; produce sum of all odd numbers of lon
(check-expect (sum-odds empty) 0) 
(check-expect (sum-odds (list 1 2 5 6 11)) 17) 
#;#;
(@template (listof Number))

(define (sum-odds lon)
  (cond [(empty? lon) 0]
        [else
         (if (odd? (first lon))
             (+ (first lon)
                (sum-odds (rest lon)))
             (sum-odds (rest lon)))])) 

(@template (listof Number) accumulator)
(define (sum-odds lon0)
  ;; acc is Number ; the sum of odd naturals seen so far
  (local [(define (solve lon acc)
            [cond [(empty? lon) acc]
                  [else
                   (solve (rest lon)
                          (if (odd? (first lon))
                              (+ (first lon) acc)
                              acc))]])]
    (solve lon0 0)))

