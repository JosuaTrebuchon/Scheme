;TP partie 2 notée

; Définitions des objets de base
(define LVal '(1 2 3 4 5 6 7 8 9 10 25 50 75 100))
(define Op '(+ * - /))

;Exercice 1
(define make-cible(lambda()
             (+ (random 900) 100)))       
;Fonction qui renvoie le nième élément de la liste en paramètre
(define (pioche n L)
  (if (null? L) "pas possible"
      (if (< n 0) "N est négatif pas possible"
          (if(= n 1) (car L)
            (pioche (- n 1) (cdr L))))))
                         

(define make-tirage(lambda()
               (define aux(lambda(n); Si n = 1 alors on ranvoie la liste constiyuée d'un elem de LVal et liste vide
                            (if (= 1 n) (cons (pioche (+ (random 14) 1) LVal) '())
                                (cons (pioche (+ (random 14) 1) LVal) (aux (- n 1)))))); Sinon liste plus appel rec
               (aux 6)))

;Exercice 2

(define estDans?(lambda(x L)
                  (define (aux n)
                    (cond
                      [(< n 0) "pas possible"]; Si négatif pas possible
                      [(null? L) #f]; Si la liste est vide x n'y est pas
                      [(= n 1) (= x (pioche n L))]; Si dernier elem on teste
                      [(= x (pioche n L)) #t]; A chaque element on teste si = à x
                      [else (aux (- n 1))])); Si pas trouvé on regarde le prochain élément de la liste
                  (aux (length L))))
; Exercice 3
; L'utilisation de (eval x) est pertinente car si l'opérateur vient d'un liste
; ex : (car '(+)) sans eval, cela ne fonctionne pas (permet d'interpréter l'opérateur)

(define (estValide? op x y)
  (if (not(eq? (eval op) /)) (< 0 ((eval op) x y)); On evalue si division pas ( eq? evalue syntaxiquement)
      (if (= 0 y) #f
          (and (< 0 ((eval op) x y)) (= (remainder x y) 0))))); Si division doir être >0 et reste de la divison euclidienne = 0
                        

; Exercice 4
;Renvoie tous les résultats possible
(define (opere L a b)
  (cond
    [(null? L) "pas d'opérateur"];Si la liste est vide on fait rien
    [(= (length L) 1) (if (estValide? (car L) a b) (cons ((eval (car L)) a b) '());SI on peut faire (a op b) renvoie la liste
                          (if (estValide? (car L) b a) (cons ((eval (car L)) b a) '()) '()))];Sinon on essaie (b op a) et si pas bon renvoie rien
    [else (if (estValide? (car L) a b);SI (a op b) ok ?
              (cons ((eval (car L)) a b) (opere (cdr L) a b));Renvoie liste de l'opération et de la suite de la liste
                          (if (estValide? (car L) b a) (cons ((eval (car L)) b a) (opere (cdr L) a b)); Sinon on teste (b op a)
                                                       (opere (cdr L) a b)))]))

;Exercice 5

;Fonction renvoie la queue de la liste située après la valeur a
(define (reste-liste L a)
  (cond
    [(null? L) '()]
    [(= (car L) a) (cdr L)]
    [else (reste-liste (cdr L) a)]))

;Fonction renvoie le debut de la liste située avant la valeur a
(define (debut-liste L a)
  (cond
    [(null? L) '()]
    [(= (car L) a) '()]
    [else (cons (car L) (debut-liste (cdr L) a))]))

;Fonction qui retire un element de la liste
(define (retire-liste L a)
  (append (debut-liste L a) (reste-liste L a)))

;Fonction qui retire les deux elements de la liste
(define (retire-liste L a b)
  (let ((sans-a (retire-liste a)))
    (append (debut-liste sans-a b) (reste-liste sans-a b))))


  
  


    

