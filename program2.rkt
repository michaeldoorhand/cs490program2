#lang racket

(define (read-file file-name) 
  (file->string file-name #:mode 'text))

(define (clean-file input-string)
  (string-downcase (regexp-replace* #px"\\s{2,}" (regexp-replace* #px"[[\\]$#,\".?!\n_\\-:;]" input-string " ") " ")))

(define (string-to-hash input-string)
  (define (make-count-hash input-list final-hash)
    (if (empty? input-list) final-hash
    (let ([word (first input-list)])
      (if (equal? word "") (make-count-hash (rest input-list) final-hash)
      (if (hash-has-key? final-hash word)
          (make-count-hash (rest input-list) (hash-set final-hash word (+ (hash-ref final-hash word) 1)))
          (make-count-hash (rest input-list) (hash-set final-hash word 1)))))))
  (make-count-hash (regexp-split #rx" " input-string) (hash)))
  
(define (make-frequency-hash input-hash)
  (let ([total (foldl + 0 (hash-values input-hash))])
  (hash-map/copy input-hash (lambda (k v) (values k (* (log (/ v total) 10) -1))))))

(define (get-rarity-hash file-name)
  (make-frequency-hash (string-to-hash (clean-file (read-file file-name)))))

(define (make-rarity-table file-list table-hash)
  (if (empty? file-list) table-hash
  (let ([file-name (first file-list)])
    (make-rarity-table (rest file-list) (hash-set table-hash file-name (get-rarity-hash file-name))))))

(define (analyze-text control-hash input-mystery-hash)
  (let* ([mystery-hash (values input-mystery-hash)])
    
    (display mystery-words)))

 (display (analyze-text (make-rarity-table '("test.txt" "test2.txt")(hash)) (make-rarity-table '("test.txt")(hash))))

