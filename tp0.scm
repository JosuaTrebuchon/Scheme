(define (puis2 x)
  (* x x))

(define (puis4 x)
  (puis2 (puis2 x))
)

(define (divisible x y)
  (if (= (remainder x y) 0) #t #f))

(define (test x)
  (cond
    [(= x 3) 3]
    [(>= x 8) (puis2 x)]
    [(= 6 x) "toto"]
    [(= 7 x) #t]
    [else #f]))

; remainder()
; floor et ceiling pour les parties entières
; http://aqualonne.free.fr/Teaching/csc/Scheme1.pdf