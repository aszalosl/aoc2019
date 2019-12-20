#lang racket
(require pmap)

(define (string-value str l)
  (- (for/sum ([j (in-range l)]) (char->integer (string-ref str j))) (* l 48)))

(define (digit signal signal-length i)
  (let* ([m (floor (/ signal-length i 4))]
         [last (- signal-length (remainder signal-length (* 4 i)))]
         [pb (min (sub1 (+ last i)) (sub1 signal-length))]
         [pe (min (sub1 (+ last i i)) (sub1 signal-length))]
         [nb (min (sub1 (+ last i i i)) (sub1 signal-length))])
    (remainder (abs (- (+
        (for/sum ([k (in-range m)])
          (let ([b (sub1 (+ (* 4 i k) i))]) 
               (string-value (substring signal b (+ b i)) i)))
        (if (< pb pe) (string-value (substring signal pb pe) (- pe pb)) 0)) 
       (for/sum ([k (in-range m)])
         (let ([b (- (* 4 i (add1 k)) i 1)]) 
              (string-value (substring signal b (+ b i)) i)))
       (if (< nb (sub1 signal-length)) 
           (string-value (substring signal nb) (- signal-length nb 1))
           0))) 10))) 
    
(define (phrase signal)
  (let ([signal-length (add1 (string-length signal))])
  (apply string (pmapf (lambda (n) (integer->char (+ n 48)))  
       (for/list ([i (in-range 1 signal-length)])
         (digit signal signal-length  i))))))

(define (phrases signal n)
  (cond
    [(zero? n) (substring signal 0 8)]
    [else (phrases (phrase signal) (sub1 n))]))


(define (string->signal str)
   (for/list ([i (in-range (string-length str))])
     (- (char->integer (string-ref str i)) 48)))

;(module+ test
;  (require rackunit)
;  (check-equal? (phrases "80871224585914546619083218645595" 100) "24176176")
;  (check-equal? (phrases "19617804207202209144916044189917" 100) "73745418")
;  (check-equal? (phrases "69317163492948606335995924319873" 100) "52432133"))

; puzzle #1
;(phrases (first (file->lines "d16.txt")) 100)

(define (summit signal num)
  (define (inner k signal lst)
    (cond
      [(empty? signal) (reverse lst)]
      [else (let ([next (remainder (+ k (first signal)) 10)])
              (inner next (rest signal) (cons next lst)))]))
  (cond
    [(zero? num) (take (reverse signal) 8)]
    [else (summit (inner (first signal) (rest signal) (list (first signal)))
                  (sub1 num))]))

(define (puzzle2 str amount number-of-phases)
  (let* ([offset (string->number (substring str 0 7))]
        [lst (reverse (map (λ (c) (- (char->integer c) 48)) 
                           (string->list str)))]
        [ls (length lst)]
        [signal (for/list ([i (in-range (- (* ls amount) offset))] 
                           [c (in-cycle lst)]) c)])
    (summit signal number-of-phases)))

(module+ test
  (require rackunit)
  (check-equal? (puzzle2 "03036732577212944063491565474664" 10000 100) '(8 4 4 6 2 0 2 6))
  (check-equal? (puzzle2 "02935109699940807407585447034323" 10000 100) '(7 8 7 2 5 2 7 0))
  (check-equal? (puzzle2 "03081770884921959731165446850517" 10000 100) '(5 3 5 5 3 7 3 1)))

# puzzle #2
(puzzle2 (first (file->lines "d16.txt")) 10000 100) 