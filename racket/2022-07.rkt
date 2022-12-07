#lang racket

(define sample "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(require rackunit)

(struct directory (name contents size) #:transparent)
(struct plain-file (name size) #:transparent)

(define (directory-add dir element) (directory (directory-name) (cons element (directory-contents dir))))

(define (string->directory str)
  (define
    (iter lines contents size)
    (cond
      ; reached EOI
      [(eq? lines '()) (values '() (reverse contents) size)]
      ; pop directory stack
      [(regexp-match? "\\$ cd \\.\\." (car lines))
       => (lambda (_) (values (cdr lines) (reverse contents) size))]
      ; found file
      [(regexp-match "([0-9]+) (.*)" (car lines))
       => (lambda (l)
            (let ([file-size (string->number (second l))])
              (iter (cdr lines)
                    (cons (plain-file (third l) file-size) contents)
                    (+ size file-size))))]
      ; descend into directory
      [(regexp-match "\\$ cd (.*)" (car lines))
       => (lambda (l)
            (match/values (iter (cdr lines) '() 0)
              [(rest-lines dir-contents dir-size)
               (iter rest-lines (cons (directory (second l) dir-contents dir-size) contents) (+ size dir-size))]))]
      ; ignore noisy line
      [else (iter (cdr lines) contents size)]))

  (match/values (iter (string-split str "\n") '() 0)
    [(_rest-of-input (list contents) _size) contents]))

(check-equal? (directory-size (string->directory sample)) 48381165)

(define (find p l)
  (if (eq? l '()) '()
      (append (filter p l) (find p (flatten (map directory-contents (filter directory? l)))))))

(define (run-task str)
  (apply + (filter (lambda (s) (s . < . 100000)) (map directory-size (find directory? (list (string->directory str)))))))

(check-equal? (run-task sample) 95437)

(define (run-task-2 str)
  (let* ([total-space 70000000]
         [required-space 30000000]
         [tree (string->directory str)]
         [root-size (directory-size tree)]
         [missing-space (- required-space (- total-space root-size))])
    (apply min (filter (lambda (size) (size . > . missing-space)) (map directory-size (find directory? (list tree)))))))

(check-equal? (run-task-2 sample) 24933642)
(run-task-2 (file->string "input-07"))
