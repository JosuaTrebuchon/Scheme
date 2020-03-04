;TP 01 exercice 2
(define x 5)
(define (f x) (* x x)); x dans la fonction différent de variable x
(f 10)

;exercice 3
(define d 1)
(define (plusd x) (+ x d)); d dans la fonction = variable d
(plusd 10)

;exercice 4
(define d 5)
(plusd 10); renvoie 15 car valeur de d modifiée

;exercice 5
(define (g x) (+ 1 (h x)))
(define (h x) (* x x))
(g 10); Il ne se passe rien si on change l'ordre de déclaration

;exercice 7
(define (monabs x)
  (if (< x 0) (* -1 x) x))
;exercice 8
(define care-div (lambda(x y)
                   (if (= y 0) (display "Erreur: le diviseur est nul") (/ x y))))
;exercice 9
(define signal (lambda(t)
                 (cond
                   [(and (> t -3) (> -1 t)) 1]
                   [

