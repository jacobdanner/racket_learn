#lang slideshow
(define c (circle 10))
(define r (rectangle 10 2))
(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  ; let* for local binding
  ; binds identifiers at once, 
  ; instead of requiring a separate define for each
  ; like inline variable definitions of functions of functions
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))