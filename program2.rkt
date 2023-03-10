#lang racket

;opens file and returns as a string
(define (read-file file-name) 
  (file->string file-name #:mode 'text))

;gets rid of any occurance of 2 or more whitespace and any [ ] $ # , \ " ? ! _ - : ;
;returns string
(define (clean-file input-string)
  (string-downcase (regexp-replace* #px"\\s{2,}" (regexp-replace* #px"[[\\]$#,\".?!\n_\\-:;]" input-string " ") " ")))

;creates a hash from a string with key being the word and the value the count of occurences of the word
(define (string-to-hash input-string)
  (define (make-count-hash input-list final-hash)
    (if (empty? input-list) final-hash
    (let ([word (first input-list)])
      (if (equal? word "") (make-count-hash (rest input-list) final-hash)
      (if (hash-has-key? final-hash word)
          (make-count-hash (rest input-list) (hash-set final-hash word (+ (hash-ref final-hash word) 1)))
          (make-count-hash (rest input-list) (hash-set final-hash word 1)))))))
  (make-count-hash (regexp-split #rx" " input-string) (hash)))

;transforms the occurence hash into a hash of frequencies
(define (make-frequency-hash input-hash)
  (let ([total (foldl + 0 (hash-values input-hash))])
  (hash-map/copy input-hash (lambda (k v) (values k (* (log (/ v total) 10) -1))))))

;combines all of the above functions to create a rarity-hash from a file 
(define (get-rarity-hash file-name)
  (make-frequency-hash (string-to-hash (clean-file (read-file file-name)))))

;compares a known/control text to a mystery text
;if the mystery text word is in the control perform the absolute value of the difference
;else set the difference to 0
;I tried not adding the word to the results if it wasn't in the control, but was getting incorrect
;output due to the total amount of differences being lower/inaccurate
(define (analyze-text control mystery)
  (define (compare-two control-hash mystery-hash results)
    (if (hash-empty? mystery-hash) results 
  (let ([mystery-word (first (hash-keys mystery-hash))]
        [mystery-value (first (hash-values mystery-hash))])
            (if (hash-has-key? control-hash mystery-word)
                (compare-two control-hash
                             (hash-remove mystery-hash mystery-word)
                             (hash-set results mystery-word (abs (- mystery-value (hash-ref control-hash mystery-word))))) 
                (compare-two control-hash
                             (hash-remove mystery-hash mystery-word)
                             (hash-set results mystery-word 0)))))) 

  (let* ([differences (hash-values(compare-two control mystery (hash)))]
         [sum    (apply + differences)]
         [total  (length differences)])
    (/ sum total)))

;Gets the best match from two known texts, Doyle & Lovecraft, compared to the mystery text
(define (get-best-match mystery-text-list)
  (display (string-append "Analyzing " (first mystery-text-list) "\n"))
  (let* ([mystery (get-rarity-hash (first mystery-text-list))]
        [doyle (get-rarity-hash "doyle.txt")]
        [doyle-score (analyze-text doyle mystery)]
        [lovecraft (get-rarity-hash "lovecraft.txt")]
        [lovecraft-score (analyze-text lovecraft mystery)])    
    (display (string-append "\t" (first mystery-text-list) " is probably "))
    (if (> doyle-score lovecraft-score) (display "Doyle \n") (display "Lovecraft \n"))
    (if (equal? (length mystery-text-list) 1) (display "Done")
    (get-best-match (rest mystery-text-list)))))

;gets the best match for mystery texts.
(get-best-match '("mystery1.txt" "mystery2.txt"))
