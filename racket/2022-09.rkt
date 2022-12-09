#lang racket
(require rackunit)

(struct position (x y) #:transparent)
(struct rope (points) #:transparent)
(define origin (position 0 0))

(define (drag-tip direction r)
  (match-let ([(rope (cons (position px py) tail)) r])
    (rope (cons
           (match direction
             ["U" (position px (+ py 1 ))]
             ["D" (position px (- py 1 ))]
             ["L" (position (- px 1 ) py)]
             ["R" (position (+ px 1 ) py)]) tail))))

(define (points-touch? p1 p2)
  (match-let
      ([(position hx hy) p1]
       [(position tx ty) p2])
    (or (and ((abs (- hy ty)) . <= . 1) ((abs (- hx tx)) . <= . 1)))))

(define (sign x)
  "Returns 1 or -1."
  (if (eq? x 0) 0
      (/ x (abs x))))

(define (drag-pair h t)
  (if (points-touch? h t) t
      (match-let*
          ([(position hx hy) h]
           [(position tx ty) t]
           [dx (- hx tx)]
           [dy (- hy ty)])
        (position (+ tx (sign dx)) (+ ty (sign dy))))))

(define (map-feedback f l)
  (define (iter ls res)
    (if (empty? ls) res
        (iter (cdr ls) (cons (f (car res) (car ls)) res))))
  (reverse (iter (cdr l) (list (car l)))))

(define (drag-rope r)
  (rope (map-feedback drag-pair (rope-points r))))

(define (string->moves str)
  (flatten
   (map (lambda (line)
          (match-let ([(list _ dir amnt) (regexp-match "([URDL]) ([0-9]+)" line)])
            (make-list (string->number amnt) dir)))
        (string-split str "\n"))))

(define (simulate r str)
  (set-count (apply set (map (compose last rope-points)
                             (foldl
                              (lambda (m rs) (cons (drag-rope (drag-tip m (car rs))) rs))
                              (list r)
                              (string->moves str))))))

(simulate (rope (make-list 2 origin)) (file->string "input-09"))
(simulate (rope (make-list 10 origin)) (file->string "input-09"))

(define sample "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(define sample-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")
(check-equal? (simulate (rope (make-list 2 origin)) sample) 13)
(check-equal? (simulate (rope (make-list 10 origin)) sample-2) 36)

(check-equal? (points-touch? (position 0 0) (position 0 0)) #t)
(check-equal? (points-touch? (position 0 1) (position 0 0)) #t)
(check-equal? (points-touch? (position 1 0) (position 0 0)) #t)
(check-equal? (points-touch? (position 1 1) (position 0 0)) #t)
(check-equal? (points-touch? (position 0 2) (position 0 0)) #f)
(check-equal? (points-touch? (position 2 0) (position 0 0)) #f)
(check-equal? (points-touch? (position 2 2) (position 0 0)) #f)

(check-equal? (drag-rope (rope (list (position 4 2)
                                    (position 3 0)
                                    (position 2 0)
                                    (position 1 0)
                                    (position 0 0)
                                    (position 0 0))))
              (rope (list (position 4 2)
                          (position 4 1)
                          (position 3 1)
                          (position 2 1)
                          (position 1 1)
                          (position 0 0))))

(check-equal? (map-feedback + (range 0 9))
              '(0 1 3 6 10 15 21 28 36))



