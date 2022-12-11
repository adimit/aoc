#lang racket
(require rackunit)
(require racket/trace)

(define sample "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(define (divisible-by n)
  (compose (curry eq? 0) (curryr modulo n)))

(struct monkey (index items op test if-true if-false peeks) #:transparent)

(define (parse-operation str)
  (lambda (old)
    (let ([operation (match (first (regexp-match "[+*]" str))
                       ["*" *]
                       ["+" +])]
          [n (cond
               [(regexp-match "[0-9]+" str) => (compose string->number first)]
               [else old])])
      (operation old n))))

(check-equal? ((parse-operation "old * old") 3) 9)
(check-equal? ((parse-operation "old * 5") 3) 15)
(check-equal? ((parse-operation "old + 5") 3) 8)

(define (string->monkey str)
  (let* ([read-number (compose string->number first (curry regexp-match "[0-9]+"))]
         [lines (string-split str "\n")]
         [index (read-number (first lines))]
         [items (map string->number (regexp-match* "[0-9]+" (second lines)))]
         [op (parse-operation (third lines))]
         [test (read-number (fourth lines))]
         [if-true (read-number (fifth lines))]
         [if-false (read-number (sixth lines))])
    (monkey index items op test if-true if-false 0)))

(define (string->monkeys str)
  (map string->monkey (string-split str "\n\n")))

(define (monkey-move item player monkeys flipover)
  (let* ([new-item (modulo ((monkey-op player) item) flipover)]
         [target-index (if ((divisible-by (monkey-test player)) new-item)
                                  (monkey-if-true player)
                                  (monkey-if-false player))]
         [target (list-ref monkeys target-index)])

    (list-set
     (list-set monkeys target-index
               (struct-copy monkey target
                            [items (append (monkey-items target) (list new-item))]))
     (monkey-index player)
     (struct-copy monkey player
                  [items (cdr (monkey-items player))]
                  [peeks (+ 1 (monkey-peeks player))]))))

(define (monkey-turn player monkeys flipover)
  (foldl
   (lambda (item acc-monkeys)
     (monkey-move item (list-ref acc-monkeys (monkey-index player)) acc-monkeys flipover))
   monkeys
   (monkey-items player)))

(define (monkey-round monkeys flipover)
  (foldl
   (lambda (player-index acc-monkeys)
     (monkey-turn (list-ref acc-monkeys player-index) acc-monkeys flipover))
   monkeys
   (range 0 (length monkeys))))

(define (play rounds monkeys)
  (let ([flipover (apply lcm (map monkey-test monkeys))])
    (foldl
     (lambda (r acc)
       (monkey-round acc flipover))
     monkeys
     (range 0 rounds))))

(define (run-task str rounds)
  (let ([monkeys (string->monkeys str)])
    (apply * (take (sort (map monkey-peeks (play rounds monkeys)) >) 2))))

(check-equal? (run-task (file->string "input-11") 20) 54240)

; (run-task (file->string "input-11") 10000)

