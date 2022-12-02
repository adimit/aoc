#lang racket

(define sample "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(require racket/string)

(print "first problem: ")
(print
 (cdr
       (foldl
        (lambda (item acc)
          (match acc
            [(cons current biggest)
             (cond
               [(string->number item) => (lambda (it) (cons (+ current it) biggest))]
               [else (cons 0 (max biggest current))])]))
        (cons 0 0)
        (string-split (file->string "input") "\n"))))



(print " second problem: ")
(print
 (apply
  +
  (take
   (sort
    (foldl (lambda (line acc)
             (cond
               [(string->number line) => (lambda (it) (cons (+ (car acc) it) (cdr acc)))]
               [else (cons 0 acc)]))
           (list 0)
           (string-split (file->string "input") "\n"))
    >)
   3)))
