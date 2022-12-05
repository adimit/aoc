#lang racket
(require rackunit)
(define sample
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(struct instruction (amount origin target)
  #:transparent)
(struct stack (contents)
  #:transparent)

(define (push s element)
  (stack (cons element (stack-contents s))))
(define (pop s) (if (empty? (stack-contents s))
                    (values s 'nothing)
                    (values (stack (cdr (stack-contents s))) (car (stack-contents s)))))
(define empty-stack (stack '()))
(define (peek s) (car (stack-contents s)))

(check-equal? (push empty-stack "a") (stack '("a")))
(check-equal? (push (stack '("b")) "a") (stack '("a" "b")))
(check-equal? (call-with-values (lambda () (pop (stack '("a" "b")))) list)
              (list (stack '("b")) "a"))
(check-equal? (call-with-values (lambda () (pop (stack '()))) list)
              (list (stack '()) 'nothing))
(check-equal? (call-with-values (lambda () (pop (push (stack '("b")) "a"))) list)
              (list (stack '("b")) "a"))

(define (parse-stacks str)
  (let* ([lines (cdr (reverse (string-split str "\n")))]
         [num-stacks (1 . + . (((string-length (first lines)) . - . 3) . / . 4))])
    (foldl
     (lambda (contents stacks)
       (map (lambda (c s)
              (if (not (eq? c #\space))
                  (push s c)
                  s))
            contents
            stacks))
     (map (lambda (_) empty-stack) (range 0 num-stacks))
     (map (lambda (line)
                 (map (lambda (i) (string-ref line (+ 1 i (* i 3))))
                      (range 0 num-stacks))) lines))))

(define sample-stacks
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 ")

(check-equal? (parse-stacks sample-stacks)
              (list (stack '(#\N #\Z))
                    (stack '(#\D #\C #\M))
                    (stack '(#\P))))

(define sample-instructions "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(define (parse-instructions str)
  (map (lambda (line)
         (apply instruction
                (map string->number (regexp-match* "[0-9]+" line))))
       (string-split str "\n")))

(check-equal? (parse-instructions sample-instructions)
              (list (instruction 1 2 1)
                    (instruction 3 1 3)
                    (instruction 2 2 1)
                    (instruction 1 1 2)))

(define (execute stacks instr)
  (foldl
   (lambda (_ acc)
     (let*-values ([(oi) (- (instruction-origin instr) 1)]
                   [(ti) (- (instruction-target instr) 1)]
                   [(new-origin element) (pop (list-ref acc oi))]
                   [(target) (list-ref acc ti)])
       (list-set (list-set acc oi new-origin) ti (push target element))))
   stacks
   (range 0 (instruction-amount instr))))

(check-equal? (execute (list (stack '("a")) (stack '("b" "c")))
                       (instruction 2 2 1))
              (list (stack '("c" "b" "a")) (stack '())))

(define (run-task method str)
  (match-let ([(list stacks instructions) (string-split str "\n\n")])
    (list->string
     (map peek (foldl
                (lambda (instr acc) (method acc instr))
                (parse-stacks stacks)
                (parse-instructions instructions))))))

(check-equal? (run-task execute sample) "CMZ")

(run-task execute (file->string "input-05"))

(define (stack-split s pos)
  (let-values ([(head tail) (split-at (stack-contents s) pos)])
    (values head (stack tail))))

(define (stack-prepend s lst)
  (stack (append lst (stack-contents s))))

(define (execute-2 stacks instr)
  (let*-values ([(oi) (- (instruction-origin instr) 1)]
                [(ti) (- (instruction-target instr) 1)]
                [(elements new-origin) (stack-split (list-ref stacks oi) (instruction-amount instr))]
                [(new-target) (stack-prepend (list-ref stacks ti) elements)])
    (list-set (list-set stacks oi new-origin) ti new-target)))

(check-equal? (run-task execute-2 sample) "MCD")

(run-task execute-2 (file->string "input-05"))
