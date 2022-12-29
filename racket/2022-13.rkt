#lang racket
(require racket/trace)
(require rackunit)
(require racket/match)

(define sample "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(struct pair (left right) #:transparent)

(define (read-list str)
  (with-input-from-string (string-replace str "," " " #:all? #t) read))

(define (read-pair l)
  (pair (read-list (first l)) (read-list (second l))))

(define (string->pairs str)
  (map (compose read-pair (curryr string-split "\n")) (string-split str "\n\n")))

(define (evaluate p)
  (match p
    [(pair '() '()) 'undecided]
    [(pair '() _) #t]
    [(pair _ '()) #f]
    [(pair (cons l ls) (cons r rs))
     #:when (and (number? l) (number? r) (< l r)) #t]
    [(pair (cons l ls) (cons r rs))
     #:when (and (number? l) (number? r) (> l r)) #f]
    [(pair (cons l ls) (cons r rs))
     #:when (and (number? l) (number? r)) (evaluate (pair ls rs))]
    [(pair l (cons r rs))
     #:when (and (number? l)) (evaluate (pair (list l) (cons r rs)))]
    [(pair (cons l ls) r)
     #:when (and (number? r)) (evaluate (pair (cons l ls) (list r)))]
    [(pair (cons '() ls) (cons '() rs)) (evaluate (pair ls rs))]
    [(pair (cons '() ls) _) #t]
    [(pair _ (cons '() rs)) #f]
    [(pair (cons l ls) (cons r rs)) (let ([s (evaluate (pair l r))])
                                      (if (eq? s 'undecided)
                                          (evaluate (pair ls rs))
                                          s))]))

(let ([sample-pairs (string->pairs sample)])
 (check-equal? (evaluate (pair '() '())) 'undecided)
 (check-equal? (evaluate (first sample-pairs)) #t)
 (check-equal? (evaluate (second sample-pairs)) #t)
 (check-equal? (evaluate (third sample-pairs)) #f)
 (check-equal? (evaluate (fourth sample-pairs)) #t)
 (check-equal? (evaluate (fifth sample-pairs)) #f)
 (check-equal? (evaluate (sixth sample-pairs)) #t)
 (check-equal? (evaluate (seventh sample-pairs)) #f)
 (check-equal? (evaluate (eighth sample-pairs)) #f))

(define (run-task str)
  (apply + (let ([pairs (string->pairs str)])
             (map (lambda (bool index) (if bool (+ 1 index) 0))
                  (map evaluate pairs) (range 0 (length pairs))))))

(check-equal? (run-task sample) 13)

(define (string->signals input)
  (filter-map (lambda (s) (if (regexp-match? "^ *$" s)
                              #f
                              (read-list s)))
              (string-split input "\n" )))

(define (run-task-2 input)
  (let ([sorted (sort (append (string->signals input) '(((2)) ((6))))
                      (lambda (a b) (evaluate (pair a b))))])
    (apply * (map (curry + 1) (list (index-of sorted '((2))) (index-of sorted '((6)) ))))))

(check-equal? (run-task-2 sample) 140)
