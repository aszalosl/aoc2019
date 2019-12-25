#lang racket
(require racket/hash)

(define (map-points line lines x y bugs)
  (cond
    [(and (empty? line) (empty? lines)) bugs]
    [(empty? line) (map-points (first lines) (rest lines) 0 (add1 y) bugs)]
    [(equal? (first line) #\#) (map-points (rest line) lines (add1 x) y (cons (cons x y) bugs))]
    [else (map-points (rest line) lines (add1 x) y bugs)]))

(define dx '(0 1 0 -1))
(define dy '(1 0 -1 0))

(define (neighbours x y bugs)
  (length (filter (λ (xy) (member xy bugs)) (for/list ([i (in-range 4)]) (cons (+ x (list-ref dx i)) (+ y (list-ref dy i)))))))

(define (survives x y bugs)
  (and (member (cons x y) bugs) (= 1 (neighbours x y bugs))))

(define (borns x y bugs)
  (let ([n (neighbours x y bugs)])
    (and (not (member (cons x y) bugs)) (< 0 n 3))))

(define (next bugs)
  (for*/list ([i (in-range 5)] [j (in-range 5)] #:when (or (survives i j bugs) (borns i j bugs))) (cons i j)))
 
(define (generations bugs others)
  (cond
    [(member bugs others) bugs]
    [else (generations (next bugs) (cons bugs others))]))

(define (biodiversity bugs)
 (apply + (map (λ (bug) (expt 2 (+ (car bug) (* 5 (cdr bug))))) bugs)))

(define (puzzle1 file-name)
     (let* ([my-map (file->lines file-name)]   
            [lls (map string->list my-map)]    
            [pkd (map-points (first lls) (rest lls) 0 0 '())] 
            )
          (biodiversity (generations pkd '()))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-to-hole a b)
  (and (= a 2) (or (= 1 b) (= 3 b))))

(define (inner a) (cond [(= a 1) 0] [(= a 3) 4]))

(define (outer a) (cond [(= a -1) 1] [(= a  5) 3]))

(define (not-center x y) (not (= 2 x y)))

(define (3d-neighbour x y z dir)
  (let* ([u (+ x (list-ref dx dir))]
         [v (+ y (list-ref dy dir))])
    (cond
      [(member u '(-1 5)) (list (list (outer u) 2 (sub1 z)))]
      [(member v '(-1 5)) (list (list 2 (outer v) (sub1 z)))]
      [(= u v 2 x) (for/list ([i (in-range 5)]) (list i (inner y) (add1 z)))]
      [(= u v 2 y) (for/list ([i (in-range 5)]) (list (inner x) i (add1 z)))]
      [else (list (list u v z))])))

(define (3d-neighbours x y z bugs)
   (length (filter (λ (xyz) (= 1 (hash-ref bugs xyz 0))) (apply append (for/list ([i (in-range 4)]) (3d-neighbour x y z i))))))

(define (survives-or-borns x y z bugs)
  (let ([n (3d-neighbours x y z bugs)])
    (if (= 1 (hash-ref bugs (list x y z) 0)) (= n 1) (< 0 n 3))))

(define (next-3d bugs)
  (let* ([zs (limits bugs)]
         [zl (sub1 (car zs))]
         [zu (+ 2 (cdr zs))])
  (for*/hash ([k (in-range zl zu)]
              [i (in-range 5)] [j (in-range 5)]
              #:when (and (not-center i j) (survives-or-borns i j k bugs)))
    (values (list i j k) 1))))

(define (map-3dpoints line lines x y bugs)
  (cond
    [(and (empty? line) (empty? lines)) (for/hash ([i (in-list bugs)]) (values i 1))]
    [(empty? line) (map-3dpoints (first lines) (rest lines) 0 (add1 y) bugs)]
    [(equal? (first line) #\#) (map-3dpoints (rest line) lines (add1 x) y (cons (list x y 0) bugs))]
    [else (map-3dpoints (rest line) lines (add1 x) y bugs)]))

(define (limits bugs) (let ([zs (map third (hash-keys bugs))]) (cons (apply min zs) (apply max zs))))

(define (3d-generations bugs n)
  (cond
    [(zero?  n) (length (hash-keys bugs))]
    [else (3d-generations (next-3d bugs) (sub1 n))]))

(define (show-generation bugs)
  (let ([zs (limits bugs)])
    (for* ([j (in-range 5)])
      (for ([k (in-range (car zs) (add1 (cdr zs)))])
        (for ([i (in-range 5)])
          (if (= 1 (hash-ref bugs (list i j k) 0)) (printf "#") (printf ".")))
        (printf "  "))
      (printf "\n"))))
 

(define (puzzle2 file-name n)
  (let* ([my-map (file->lines file-name)] 
         [lls (map string->list my-map)]  
         [pkd (map-3dpoints (first lls) (rest lls) 0 0 '())])
    (3d-generations pkd n)))

(puzzle2 "d24.txt" 200)