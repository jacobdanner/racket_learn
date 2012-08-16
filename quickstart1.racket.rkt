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

; lambda == anonymous function
; long form of the above
(define verbose_series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))


; lexical scope - identifier == expression/binding
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

; use map to iterate over a list/apply
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

; apply works on a list, but all at once instead of iteratively
(define (vert_rainbow p)
  (apply vc-append (rainbow p)))

(require slideshow/flash)
(define (simple_flash x y)
  (filled-flash x y))

(require slideshow/code)
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))