;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname circle-fractal-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment genrec-p1)
(@cwl ??? ???)

(@problem 1)
;; Design a function that will create the following fractal:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/circle-fractal.PNG
;; 
;; Each circle is surrounded by circles that are two-fifths smaller. 
;;
;; You can build these images using the convenient beside and above functions
;; if you make your actual recursive function be one that just produces the
;; top leaf shape. You can then rotate that to produce the other three shapes.
;;
;; You don't have to use this structure if you are prepared to use more
;; complex place-image functions and do some arithmetic. But the approach
;; where you use the helper is simpler.
;;
;; Include a termination argument for your design.


;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

(@htdf draw)
(@signature Number -> Image)
;; produces each circle surrounded by circles that are STEP smaller
(check-expect (draw TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue"))
(check-expect (draw (/ TRIVIAL-SIZE STEP))
              (local [(define current (circle (/ TRIVIAL-SIZE STEP) "solid"
                                              "blue"))
                      (define next (circle TRIVIAL-SIZE "solid" "blue"))]
                (beside next
                        (above next current next)
                        next)))

;(define (draw n) empty-image) ;stub

(define (draw n0)
  ;; base case: (<= n TRIVIAL-SIZE)
  ;; reduction step: (* n STEP)
  ;; termination arguement: Since n is always being multiplied by a fraction,
  ;;                        it will always get smaller
  (local [(define (fn-for-main n)
            (cond [(<= n TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue")]
                  [else
                   (local [(define next-n (* n STEP))]
                     (above (fn-for-next next-n)
                            (beside (rotate 90 (fn-for-next next-n))
                                    (circle n "solid" "blue")
                                    (rotate -90 (fn-for-next next-n)))
                            (rotate 180 (fn-for-next next-n))))]))

          (define (fn-for-next n1)
            (cond [(<= n1 TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue")]
                  [else
                   (local [(define current (circle n1 "solid" "blue"))
                           (define next (fn-for-next (* n1 STEP)))]
                     (above next
                            (beside (rotate 90 next)
                                    current
                                    (rotate -90 next))))]))]
    (fn-for-main n0)))



