#lang racket

(require rackunit)
(require math/array)

(define sample "30373
25512
65332
33549
35390")

(define (char->intValue c)
  (- (char->integer c) 48))

(define (string->trees str)
  (let ([lines (string-split str "\n")])
    (build-array
     (vector-immutable (string-length (car lines)) (length lines))
     (match-lambda [(vector x y)
                    (char->intValue (string-ref (list-ref lines y) x))]))))


(define (find-trees ary lst)
  (array->list (array-flatten (array-slice-ref ary lst))))

(define (treeline ary pos)
  (match-let* ([(vector x y) pos]
               [to-the-left   (find-trees ary (list (:: #f x 1) (:: y (+ 1  y) 1)))]
               [to-the-right  (find-trees ary (list (:: (+ 1 x) #f 1) (:: y (+ 1  y) 1)))]
               [to-the-top    (find-trees ary (list (:: x (+ 1 x) 1) (:: #f y 1)))]
               [to-the-bottom (find-trees ary (list (:: x (+ 1 x) 1) (:: (+ 1 y) #f 1)))])
    (list to-the-left to-the-right to-the-top to-the-bottom)))

(define (tree-visible? ary pos)
  (let ([height (array-ref ary pos)]
        [trees (treeline ary pos)])
    (or (ormap empty? trees)
        (ormap (curry andmap (curry > height)) trees))))

(let ([ary (string->trees sample)])
  (check-equal? (tree-visible? ary #[0 0]) #t)
  (check-equal? (tree-visible? ary #[1 0]) #t)
  (check-equal? (tree-visible? ary #[0 1]) #t)
  (check-equal? (tree-visible? ary #[4 4]) #t)
  (check-equal? (tree-visible? ary #[1 4]) #t)
  (check-equal? (tree-visible? ary #[4 1]) #t)
  (check-equal? (tree-visible? ary #[4 1]) #t)
  (check-equal? (tree-visible? ary #[1 1]) #t)
  (check-equal? (tree-visible? ary #[2 1]) #t)
  (check-equal? (tree-visible? ary #[3 1]) #f)
  (check-equal? (tree-visible? ary #[1 2]) #t)
  (check-equal? (tree-visible? ary #[1 2]) #t)
  (check-equal? (tree-visible? ary #[2 2]) #f))

(define (run-task str)
  (let ([trees (string->trees str)])
    (length
     (filter-map (curry tree-visible? trees)
                 (sequence->list (in-array-indexes (array-shape trees)))))))

(check-equal? (run-task sample) 21)
; (run-task (file->string "input-08"))

(define (treeline-centred ary pos)
  (match-let* ([(vector x y) pos]
               [to-the-left   (find-trees ary (list (:: (- x 1) #f -1) (:: y (+ 1  y) 1)))]
               [to-the-right  (find-trees ary (list (:: (+ 1 x) #f 1) (:: y (+ 1  y) 1)))]
               [to-the-top    (find-trees ary (list (:: x (+ 1 x) 1) (:: (- y 1) #f -1)))]
               [to-the-bottom (find-trees ary (list (:: x (+ 1 x) 1) (:: (+ 1 y) #f 1)))])
    (list to-the-left to-the-right to-the-top to-the-bottom)))

(define (sight-distance height lst)
  (cond
    [(empty? lst) 0]
    [(index-where lst (curry <= height)) => (curry + 1)]
    [else (length lst)]))

(check-equal? (sight-distance 3 '()) 0)
(check-equal? (sight-distance 3 '(3)) 1)
(check-equal? (sight-distance 3 '(1 2)) 2)
(check-equal? (sight-distance 3 '(1 5)) 2)
(check-equal? (sight-distance 3 '(1 3 5)) 2)
(check-equal? (sight-distance 3 '(1 2 2)) 3)


(define (run-task-2 str)
  (let ([trees (string->trees str)])
    (define (score pos)
      (apply * (map (curry sight-distance (array-ref trees pos))
                    (treeline-centred trees pos))))
    (apply max (map score (sequence->list (in-array-indexes (array-shape trees)))))))

(check-equal? (run-task-2 sample) 8)
(run-task-2 (file->string "input-08"))

