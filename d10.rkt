#lang racket

(define (pairs i str)
  (for/list ([j (in-range (string-length str))] #:when (equal? #\# (string-ref str j)))
    (list j i)))

(define (pair-< xy uv)
  (or (< (first xy) (first uv))
      (and (= (first xy) (first uv))
           (< (second xy) (second uv)))))

(define (triple-> xyz uvw)
  (> (first xyz) (first uvw)))

(define (count lst)
  (define (inner x lst cnt)
    (cond
      [(empty? lst) cnt]
      [(equal? x (first lst)) (inner x (rest lst) cnt)]
      [else (inner (first lst) (rest lst) (add1 cnt))]))
  (inner (first lst) (rest lst) 0))

(define (diff x y asts)
  (count (sort 
   (map (lambda (p) (let* ([dx (- (first p) x)]
                           [dy (- (second p) y)]
                           [g (gcd dx dy)])
                      (if (< 0 g)
                          (list  (quotient dx g) (quotient dy g)) (list 0 0))))
       asts) pair-<)))

(define (asteroid filename)
  (let* ([strs (file->lines filename)]
        [asts (apply append (for/list ([i (in-range (length strs))]) 
                              (pairs i (list-ref strs i))))])
     (car (sort (map (lambda (p) (list (diff (first p) (second p) asts) p)) 
                     asts) triple->))))

(module+ test
  (require rackunit)
  (check-equal? (asteroid "d10a.txt") '(8 (3 4)))
  (check-equal? (asteroid "d10b.txt") '(33 (5 8)))
  (check-equal? (asteroid "d10c.txt") '(35 (1 2)))
  (check-equal? (asteroid "d10d.txt") '(41 (6 3)))
  (check-equal? (asteroid "d10e.txt") '(210 (11 13)))
  )

(define (puzzle1 filename)
     (asteroid filename))

;(puzzle1 "d10.txt")

;13 17 
(define (pair2 row str dx dy)
  (for/list ([col (in-range (string-length str))] #:when (equal? #\# (string-ref str col)))
    (list (- col dx) (- row dy))))

(define (quarter x y)
  (cond 
    [(and (= x 0) (< y 0)) 0]
    [(and (< 0 x) (< y 0)) 1]
    [(and (< 0 x) (= 0 y)) 2]
    [(and (< 0 x) (< 0 y)) 3]
    [(and (= x 0) (< 0 y)) 4]
    [(and (< x 0) (< 0 y)) 5]
    [(and (< x 0) (= 0 y)) 6]
    [(and (< x 0) (< y 0)) 7]))

(define (pythagoras x y)
  (+ (* x x) (* y y)))

(define (less xy uv)
  (let* ([x (first xy)] 
         [y (second xy)] 
         [u (first uv)] 
         [v (second uv)]
         [qxy (quarter x y)]
         [quv (quarter u v)])
    (or (< qxy quv)  ; két térfél
        (and (= qxy quv) (even? qxy) (< (pythagoras x y) (pythagoras u v)))
        (and (= qxy quv) (odd?  qxy) (> (* (abs y) (abs u)) (* (abs v) (abs x))))
        (and (= qxy quv) (odd? qxy) (= (* (abs y) (abs u)) (* (abs v) (abs x)))
             (< (pythagoras x y) (pythagoras u v))))))

(define (laser filename dx dy)
  (let* ([strs (file->lines filename)]
        [asts (apply append (for/list ([col (in-range (length strs))]) 
                              (pair2 col (list-ref strs col) dx dy)))])
    (sort (filter (lambda (p) (< 0 (pythagoras (first p) (second p)))) 
                  asts) less)))

;(define xXx 11)
;(define yYy 13)
;(define ps (laser "d10e.txt" xXx yYy))
(define xXx 8)
(define yYy 3)
(define ps (laser "d10f.txt" xXx yYy))

(define (my-count n head tail liat)
  (cond
    [(zero? n) head]
    [(and (empty? tail) (empty? liat)) head]
    [(empty? tail)
     (let ([new-tail (reverse liat)])
       (my-count (sub1 n)  (first new-tail)(rest new-tail) '()))]
    [(and (= (* (first head) (cadar tail)) (* (second head) (caar tail)))
          (= (quarter (first head) (second head)) (quarter (caar tail) (cadar tail))))
     (my-count n head (rest tail) (cons (first tail) liat))]
    [else 
      (printf "~a. @ ~a,~a\n" (- 29 n) (+ (first head) xXx) (+ (second head) yYy))
      (my-count (sub1 n) (first tail) (rest tail) liat)]))

(my-count 30 (first ps) (rest ps) '())