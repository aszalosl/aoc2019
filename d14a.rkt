#lang racket
(define (state0 file-name)
  (map (λ (line) (map (λ (s) (string-split s " "))
                      (regexp-match* #px"[[:digit:]]+ [[:upper:]]+" line)))
  (file->lines file-name)))

(define (origin file-name)
  (for/hash ([item (state0 file-name)])
    (let ([meti (reverse item)])
      (values (second (first meti)) (cons (string->number (caar meti)) (rest meti))))))
;(define (less-chem? a b Ori)
;  (let ([a-orig (map second (rest (hash-ref Ori a)))]
;        [b-orig (map second (rest (hash-ref Ori b)))])
;    (cond
;      [(member a b-orig) #t]
;      [(member b a-orig) #f]
;      [else #t])))

(define (rewrite lst tsl Ori)
  ;(printf "~a\n" lst) 
  (cond
    [(empty? lst) tsl]
    [else
     (let ([chem (caar lst)]
           [nu   (cadar lst)])
       ;(printf "~a\n" nu)
       (if (hash-has-key? Ori chem)
           (rewrite (rest lst)
                    (append (map (lambda (p) (list (second p)
                                                   (* nu (string->number (first p)))))
                                 (rest (hash-ref Ori chem))) tsl)
                    Ori)
           (rewrite (rest lst) (cons (list chem nu) tsl) Ori)))])) 