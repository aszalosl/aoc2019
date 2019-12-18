#lang racket

(define (state0 file-name)
  (map (lambda (line) (append (map string->number
                                   (regexp-match* #px"-?\\d+" line)) '(0 0 0)))
       (file->lines file-name)))

(define (signum x)
  (cond
    [(zero? x) 0]
    [(< 0 x)  1]
    [(< x 0) -1]))

(define (dist this that)
  (let* ([dx (- (first that) (first this))]
         [dy (- (second that) (second this))]
         [dz (- (third that) (third this))])
    (list (signum dx) (signum dy) (signum dz))))

(define (sum3 sx sy sz lst)
  (cond
    [(empty? lst) (list sx sy sz)]
    [else (sum3 (+ sx (first (first lst))) (+ sy (second (first lst)))
                (+ sz (third (first lst))) (rest lst))]))

(define (velos state)
  (map (lambda (this) (sum3 0 0 0 
                        (map (lambda (that) (dist this that)) state))) state))

(define (new-state state)
  (define (inner states velos new)
    (cond
      [(empty? states) (reverse new)]
      [else
       (let ([hs (first states)]
             [hv (first velos)])
         (inner (rest states) (rest velos)
                (cons (list
                       (+ (first hs)  (first hv)  (fourth hs))
                       (+ (second hs) (second hv) (fifth hs))
                       (+ (third hs)  (third hv)  (sixth hs))
                       (+ (fourth hs) (first hv))
                       (+ (fifth hs)  (second hv))
                       (+ (sixth hs)  (third hv))) new)))]))
    (inner state (velos state) '()))

(define (next-state state n)
  (cond [(zero? n) state]
        [else (next-state (new-state state) (sub1 n))]))

(define (potential state)
  (map (lambda (moon) (apply + (map abs (take moon 3)))) state))

(define (kinetic state)
  (map (lambda (moon) (apply + (map abs (drop moon 3)))) state))

(define (total state n)
  (let* ([final (next-state state n)]
         [k (kinetic final)]
         [p (potential final)])
    (apply + (map * k p))))

(module+ test
  (require rackunit)
  (check-equal? (total (state0 "d12a.txt") 10) 179)
  (check-equal? (total (state0 "d12b.txt") 100) 1940)
  )

;puzzle1
; (total (state0 "d12.txt") 1000)

(define (nrg-seq state n lst)
  (cond
    [(zero? n) (reverse lst)]
    [(let ([new (new-state state)])
       (nrg-seq new (sub1 n) (cons (apply + (map * (kinetic state) (potential state))) lst)))]))

(define (primes n)
(define (inner n i lst)
  (cond
    [(= n 1) (reverse lst)]
    [(zero? (remainder n i)) (inner (quotient n i) i (cons i lst))]
    [else (inner n (add1 i) lst)]))
  (inner n 2 '()))