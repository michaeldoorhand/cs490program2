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

(define (analyze-text control-table input-mystery-table)
  (define (compare-two control-hash mystery-hash results)
    (if (hash-empty? mystery-hash) results 
  (let ([mystery-word (first (hash-keys mystery-hash))]
        [mystery-value (first (hash-values mystery-hash))])
            (if (hash-has-key? control-hash mystery-word)
                (compare-two control-hash
                             (hash-remove mystery-hash mystery-word)
                             (hash-set results mystery-word (abs (- mystery-value (hash-ref control-hash mystery-word))))) 
                (compare-two control-hash (hash-remove mystery-hash mystery-word) results)))))
        
  
  (compare-two (hash-ref control-table (first (hash-keys control-table)))
               (hash-ref input-mystery-table (first (hash-keys input-mystery-table)))
               (hash)))

(define (foo 
 (display (analyze-text (make-rarity-table '("test.txt" "test2.txt")(hash)) (make-rarity-table '("mysterytest.txt" "mysterytest2.txt")(hash))))
;(display (hash-ref (make-rarity-table '("test.txt" "test2.txt")(hash)))     )
