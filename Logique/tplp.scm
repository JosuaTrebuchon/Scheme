#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 Representation des propositions

(define F 'p)
(define G '(! toto))
(define H '(<-> (^ a c) (v (! b) (-> c (^ Bot Top)))))

(define (neg? F) (eq? F '!))
(define (and? F) (eq? F '^))
(define (or? F) (eq? F 'v))
(define (imp? F) (eq? F '->))
(define (equ? F) (eq? F '<->))
(define (top? F) (eq? F 'Top))
(define (bot? F) (eq? F 'Bot))
(define (symbLog? F) (or (top? F) (bot? F) (and? F) (or? F) (neg? F) (imp? F) (equ? F)))
(define (conBin? F) (or (and? F) (or? F) (imp? F) (equ? F)))
(define (symbProp? F) (and (symbol? F) (not (symbLog? F))))
(define (atomicFbf? F) (or (symbProp? F) (top? F) (bot? F)))
(define (fbf? F)
  (cond ((atomicFbf? F) 					   #t )
        ((list? F) (cond ((and (= (length F) 2) (neg? (car F)))    (fbf? (cadr F)))
                         ((and (= (length F) 3) (conBin? (car F))) (and (fbf? (cadr F)) (fbf? (caddr F))) )
                         (else #f)))
        (else #f)))
(define (conRac F) (car F))
(define (fils F) (cadr F))
(define (filsG F) (cadr F))
(define (filsD F) (caddr F))
(define (negFbf? F) (and (not (atomicFbf? F)) (neg? (conRac F))))
(define (conjFbf? F) (and (not (atomicFbf? F)) (and? (conRac F))))
(define (disjFbf? F) (and (not (atomicFbf? F)) (or? (conRac F))))
(define (implFbf? F) (and (not (atomicFbf? F)) (imp? (conRac F))))
(define (equiFbf? F) (and (not (atomicFbf? F)) (equ? (conRac F))))
 
; Q1
(display "\nQ1\n")
(display "F => ") F
(display "G => ") G
(display "H => ") H
(define F1 '(<-> (^ a b) (v (! a) b)))
(define F2 '(v (! (^ a (! b))) (! (-> a b))))
(define F3 '(^ (! (-> a (v a b))) (! (! (^ a (v b (! c)))))))
(define F4 '(^ (^
                (v (v (! a) b) d)
                (^ (v (! d) c) (v c a)))
               (^
                (v (! c) b)
                (^ (v (! c) (! b)) (v (! b) d)))))

               

; Q2
(display "\nQ2\n")
(display "(fbf? F) => ") (fbf? F)
(display "(fbf? G) => ") (fbf? G)
(display "(fbf? H) => ") (fbf? H)
(display "(fbf? '(! a b)) => ") (fbf? '(! a b))
(display "(fbf? F1) => ") (fbf? F1)
(display "(fbf? F2) => ") (fbf? F2)
(display "(fbf? F3) => ") (fbf? F3)
(display "(fbf? F4) => ") (fbf? F4)
;(fbf? F1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2 Syntaxe des propositions

;Q3
(define (nbc F)
  (cond [(atomicFbf? F) 0]
        [(negFbf? F) (+ 1 (nbc (fils F)))]
        [else (+ 1 (nbc (filsG F)) (nbc (filsD F)))]))
(nbc F1)
(nbc F2)
(nbc F3)
(nbc F4)

;Q4
(define (prof f)
  (cond
    [(atomicFbf? f) 0]
    [(negFbf? f) (+ 1 (prof (fils f)))]
    [else (let ((gauche (+ 1 (prof (filsG f))))
                (droit (+ 1 (prof (filsD f)))))
            (if (> gauche droit)
                gauche
                droit))]))
(prof F1)
(prof F2)
(prof F3)
(prof F4)

;Q5
(define (ensSP F)
  (cond
    [(and (atomicFbf? F)  (symbProp? F)) (list F) ]
    [(or (top? F) (bot? F)) '()]
    [(negFbf? F) (ensSP (fils F))]
    [else (set-union (ensSP  (filsG F)) (ensSP  (filsD F)))]))

(ensSP F1)
(ensSP F2)
(ensSP F3)
(ensSP F4)

;Q6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3 Semantique

(define I '((a . 0) (b . 1)))

;Q7
(define I1 '((c . (a . 1))) (b . 0))
(define I2 '(c . (b . (a . 0))))
(define I3 '(c . (b . (a . 1))))


;Q8
;(define (intSymb s I)
;  (cond ((eq? s (car (set-first I))) (cdr (set-first I)))
;        (else              ...)))

;Q9
(define (intAnd v1 v2) (* v1 v2))
;(define intTop   ...)
 
;Q10

;Q11

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4 Satisfiabilite, Validite

;Q12
; (define EI ...)

;Q13
;(define (ensInt ensSymb)
;  (if (set-empty? ensSymb) '(())
;      (let ( (EI ...) )
;                 (append (map (lambda (I) (set-add I ...)) EI)
;                         (map (lambda (I) (set-add I ...)) EI)))))
                                 
;Q14

;Q15

;Q16


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5 Equivalence, Consequence

;Q17

;Q17 bis

;Q18

;Q19

;Q20

;Q21
        
;Q22

;Q23
(define (conjonction EF) ; EF est une ensemble de fbf
  (cond  ((set-empty? EF) 'Top)
         ((set-empty? (set-rest EF)) (set-first EF))
         (else (list '^ (set-first EF) (conjonction (set-rest EF))))))

;(define (consequenceV? EH C)
   
;Q24


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6 Mise sous forme conjonctive

;Q25
;(define (oteEqu F)
;  (cond ((atomicFbf? F) ...)
;        ((negFbf? F) ...)
;        ((not (equiFbf?  F)) ...)
;        (else  ; c'est <->
;               ...)))

;Q26

;Q27

;Q28
;(define (redNeg F)
;  (cond ((symbProp? F)     ...) ; cas d'un symbole propositionnel
;        ((not (negFbf? F)) ...) ; cas d'un connecteur racine ^ ou v
;        (else  ; cas de la négation en connecteur racine ==> on regarde son fils
;         (cond ((symbProp? (fils F))  	...) ; littéral négatif
;               ((negFbf? (fils F)) 	...) ; deux négations ==> équivalence de la double négation 
;               ((conjFbf? (fils F)) 	...) ; négation d'une conjonction ==> équivalence de De Morgan
;               (else 					...))))) ; négation d'une disjonction ==> équivalence de De Morgan

;Q29
;(define (distOu F)
;  (cond ((symbProp? F)	...)
;        ((negFbf? F)	...)
;        ((conjFbf? F) 	...)
;        (else          ; c'est donc une disjonction
;         (let ( (Fg (distOu (filsG F))) (Fd (distOu (filsD F))) )  
;           (cond ((conjFbf? Fg)	(list '^   (distOu (list 'v (filsG Fg) Fd))   (distOu (list 'v (filsD Fg) Fd))))
;                 ((conjFbf? Fd)	(list '^   (distOu (list 'v Fg (filsG Fd)))   (distOu (list 'v Fg (filsD Fd)))))
;                 (else 			; il n'y a plus de ^ dans les sous-formules
;                  (list 'v   Fg   Fd)))))))

;Q30

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7 Mise sous forme clausale

; Exemple de clause et forme clausale
(define exClause '( p (! r) t)) 
(define exFormeClausale '( (p (! p))  (p q (! r))  ((! r) s)  (p (! r) t)  (p ( ! r))  (r (! t))  (s t) (p (! s))   ((! p ) (! s))))

; Fonction permettant de tester si une fbf est un littéral et d'obtenir le littéral opposé d'un littéral
(define (litteral? F) (or (symbProp? F) (and (negFbf? F) (symbProp? (fils F)))))
(define (oppose L) (if (symbProp? L) (list '! L) (fils L)))
  
; Fonctions permettant de manipuler des ensembles d'ensembles
(define (setSet-member? EC C)
  (cond ((set-empty? EC) #f)
        ((set=? (set-first EC) C) #t)
        (else (setSet-member? (set-rest EC) C))))

(define (setSet-add EC C)
  (cond ((set-empty? EC) (list C))
        ((set=? (set-first EC) C) EC)
        (else (set-add (setSet-add (set-rest EC) C) (set-first EC)))))

(define (setSet-union EC1 EC2)
  (if (set-empty? EC2) EC1
      (setSet-union (setSet-add EC1 (set-first EC2)) (set-rest EC2))))

;Q31
  
;Q32

;Q33

;Q34

;Q35

;Q36


;;;;;;;;;;;;;;;
; 8  Resolution

;Q37
  
;Q38
        
;Q39

;Q40


;;;;;;;;;;;;;;;
; 9 Application

;Q41


;;;;;;;;;;;;;;;
; 10 Evaluation

