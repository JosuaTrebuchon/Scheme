;Exercice 15 TP1 Calacule jour del'année
(define bissextile?(lambda(x)
  (if (or (and (= (remainder x 4) 0) (> (remainder x 100) 0)) (= (remainder x 400) 0))
      #t; Si x divisible par 4 mais pas par 100 sauf si x ivisible par 400
      #f)))

;Exercice 16
(define nb-annees-bissextiles(lambda(x)
                (if (>= x 1900)
                    (if (bissextile? x) (+ 1 (nb-annees-bissextiles (- x 1)))
                        (nb-annees-bissextiles (- x 1); else
                                               )) 0)))
                         
;Exercice 17
(define nb-jours-au-1-jan(lambda(m)
           (cond
             [(= m 1) 0]
             [(= m 2) 31]
             [(= m 3) (+ 28 (nb-jours-au-1-jan (- m 1)))]
             [(= m 4) (+ 31 (nb-jours-au-1-jan (- m 1)))]
             [(= m 5) (+ 30 (nb-jours-au-1-jan (- m 1)))]
             [(= m 6) (+ 31 (nb-jours-au-1-jan (- m 1)))]
             [(= m 7) (+ 30 (nb-jours-au-1-jan (- m 1)))]
             [(= m 8) (+ 31 (nb-jours-au-1-jan (- m 1)))]
             [(= m 9) (+ 31 (nb-jours-au-1-jan (- m 1)))]
             [(= m 10) (+ 30 (nb-jours-au-1-jan (- m 1)))]
             [(= m 11) (+ 31 (nb-jours-au-1-jan (- m 1)))]
             [(= m 12) (+ 30 (nb-jours-au-1-jan (- m 1)))]
             [0])))
;Exercice 18
(define nb-jours(lambda(j mm aaaa)
         (let ((bi (if (and (= mm 2)(bissextile? aaaa))
                       (if (> 29 j) (- (nb-annees-bissextiles aaaa) 1) (nb-annees-bissextiles aaaa))(nb-annees-bissextiles aaaa))))
           (define annees(
               lambda(x)
                (if (> x 1900)
                    (+ 365 (annees (- x 1))) 0)))
             (+ (nb-jours-au-1-jan mm) j bi (annees aaaa)))))
;Exercercice 19
(define jour-de-semaine(lambda(j mm aaaa)
                         (let ((nb (remainder (nb-jours j mm aaaa) 7)))
                           (cond
                             [(= 1 nb) "Lundi"]
                             [(= 2 nb) "Mardi"]
                             [(= 3 nb) "Mercredi"]
                             [(= 4 nb) "Jeudi"]
                             [(= 5 nb) "Vendredi"]
                             [(= 6 nb) "Samedi"]
                             [(= 0 nb) "Dimanche"]
                             ["Erreur"]))))


