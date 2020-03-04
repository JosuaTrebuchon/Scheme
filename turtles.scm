(require (lib "turtles.ss" "graphics"))

(define carré(lambda(lgr)
             (draw lgr)(turn 90)(draw lgr)(turn 90)(draw lgr)(turn 90)(draw lgr)(turn 90)(move lgr)))

(turtles #t)

(define hexagone(lambda(lgr)
             (draw lgr)(turn 60)
             (draw lgr)(turn 60)
             (draw lgr)(turn 60)
             (draw lgr)(turn 60)
             (draw lgr)(turn 60)
             (draw lgr)(turn 60)(move lgr)))

(define figure(lambda(lgr n)
              (let ((a (if (= 0 n) 0 (/ 360 n)))); Attrubution de la valeur du if a a, jusqu'à fin du (let ...)
              (define coteRec; Définition de la fonction qui sera appelée récursivement
                (lambda(l n); Un lambda s'applique toujours à quelque chose, on ne peut réutiliser lgr que en le mettant en paramètre
                  (if (> n 0) ( begin (draw l) (turn a)(coteRec l (- n 1)))))); Fin de déf de la fonction
                (clear);Néttoyage du champ de dessin avant nouveau dessin
                (coteRec lgr n)))); Appelle du dessin
                  
                