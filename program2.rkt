#lang racket











(define (read-file file-name) 
  (file->string file-name #:mode 'text))

(define (clean-file input-string final-string)
  (let ([char (read-char (open-input-string input-string))])
    (cond
      [(eof-object? char) final-string]
      [(char-upper-case? char) (clean-file (substring input-string 1) (string-append final-string (make-string 1 (char-downcase char))))]
      [(equal? char #\")       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\,)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\.)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\?)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\!)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\-)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\_)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\:)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\;)       (clean-file (substring input-string 1) (string-append final-string " "))]
      [(equal? char #\newline) (clean-file (substring input-string 1) (string-append final-string " "))]
      [else                    (clean-file (substring input-string 1) (string-append final-string (make-string 1 char)))])))

(define (string-to-list input-string final-list)
    (let ([char (read-char (open-input-string input-string))])
      (cond
        [(eof-object? char) final-list]
        [(equal? char " ") (string-to-list (substring input-string 1) final-list)]
        [else (string-to-list (substring input-string 1) (cons (string-append (first final-list) (make-string 1 char))(rest final-list)))])))
       
(define (make-string-hash input-list)
  (define (list-to-hash input final-hash)
   (display "y"))

  (list-to-hash input-list (hash "!" 0)))
  
(define start (display (string-to-list (clean-file (read-file "test.txt") "")'(""))))