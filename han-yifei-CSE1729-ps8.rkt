(define (string->symbol-list str)
  ;; takes a string, returns list of symbols
  ;; spaces are used to delimit strings representing symbols
  ;; newlines are turned into the symbol 'newline in the output list
  (define (string->symbol-list-aux char-lst)
    (cond ((null? char-lst) '())
          ((eq? (car char-lst) #\space)
           (string->symbol-list-aux (cdr char-lst)))
          ((eq? (car char-lst) #\newline)
           (cons 'newline (string->symbol-list-aux (cdr char-lst))))
          (else
           (cons (get-first-symbol char-lst)
                 (string->symbol-list-aux (remove-first-symbol char-lst))))))
  (string->symbol-list-aux (string->list str)))

(define (get-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns a symbol made from the characters up to a space newline or end of list
  (define (gfs-iter char-lst sofar)
    (cond ((or (null? char-lst)
               (eq? (car char-lst) #\space)
               (eq? (car char-lst) #\newline))
           (string->symbol (list->string (reverse sofar))))
          (else
           (gfs-iter (cdr char-lst) (cons (car char-lst) sofar)))))
  (gfs-iter char-lst '()))

(define (remove-first-symbol char-lst)
  ; given a list of characters not starting with space or newline
  ; returns the list with all characters up to a space, newline,
  ; or end of list removed
  (cond ((or (null? char-lst)
             (eq? (car char-lst) #\space)
             (eq? (car char-lst) #\newline))
         char-lst)
        (else
         (remove-first-symbol (cdr char-lst)))))

(define (symbol-list->string sym-lst)
  ; given a list of symbols, turns it into a string
  ; by adding space characters between strings representing
  ; symbols, and adds newline characters wherever there is
  ; the 'newline symbol
  (cond ((null? sym-lst) "")
        ((eq? (car sym-lst) 'newline)
         (string-append (list->string (list #\newline)) (symbol-list->string (cdr sym-lst))))
        (else (string-append (symbol->string (car sym-lst))
                             " "
                             (symbol-list->string (cdr sym-lst))))))

(define surfin-bird 
  "a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird the bird is the word
a well a bird bird bird well the bird is the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word
a well a

a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a dont you know about the bird
well everybodys talking about the bird
a well a bird bird b-birds the word
a well a bird

surfin bird
bbbbbbbbbbbbbbbbbb aaah

pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa
pa pa pa pa pa pa pa pa pa pa pa pa pa pa ooma mow mow
papa ooma mow mow

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
oom oom oom oom ooma mow mow
papa ooma mow mow papa oom oom oom
oom ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa a mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
papa ooma mow mow ooma mow mow
papa oom oom oom oom ooma mow mow
oom oom oom oom ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
well dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow")
"problem1"
(define (element-of-list? x lst)
  (if (null? lst)
      #f
      (if (equal? x (car lst))
          #t
          (element-of-list? x (cdr lst)))))
(define (num-occurs sym lst)
  (if (null? lst)
      0
      (if (eq? sym (car lst))
          (+ 1 (num-occurs sym (cdr lst)))
          (num-occurs sym (cdr lst)))))
(define (freq-list lst)
  (define (freq-lst-helper lst2 freqlst)
    (if (null? lst2)
        freqlst
        (if (element-of-list?
             (cons (car lst2) (num-occurs (car lst2) lst))
             freqlst)
            (freq-lst-helper (cdr lst2) freqlst)
            (freq-lst-helper (cdr lst2)
                             (cons  (cons (car lst2)
                                          (num-occurs (car lst2) lst)
                                          ) freqlst)))))
  (freq-lst-helper lst '()))
(define (word-frequencies str)
  (freq-list (string->symbol-list str)))
"test1"
"(word-frequencies surfin-bird)"
(word-frequencies surfin-bird)

"problem2"
(define (create-heap vw-pair
                     left
                     right)
  (list vw-pair left right))
(define (h-min heap)
  (car heap))
(define (left heap)
  (cadr heap))
(define (right heap)
  (caddr heap))
(define (heap-value heap)
  (cdr (h-min heap)))
(define (insert vw-pair heap)
  (if (null? heap)
      (list vw-pair '() '())
      (let ((child (if (< (cdr vw-pair) (heap-value heap))
                       (h-min heap)
                       vw-pair))
            (root (if (< (cdr vw-pair) (heap-value heap))
                      vw-pair
                      (h-min heap))))
        (create-heap root
                     (right heap)
                     (insert child (left heap))))))
(define (insert-list-of-pairs vw-pair-list heap)
  (if (null? vw-pair-list)
      heap
      (insert-list-of-pairs (cdr vw-pair-list)
                            (insert (car vw-pair-list) heap))))
(define (remove-min heap)
  (cond ((null? (left heap)) (right heap))
        ((null? (right heap)) (left heap))
        (else (if (< (heap-value (left heap))
                     (heap-value (right heap)))
                  (create-heap (h-min (left heap))
                               (right heap)
                               (remove-min (left heap)))
                  (create-heap (h-min (right heap))
                               (left heap)
                               (remove-min (right heap)))))))
(define (make-tree a b c)
  (list a b c))
(define (make-value-trees lst)
  (if (null? lst)
      lst
      (cons
       (cons (list (caar lst) '() '()) (cdr (car lst)))
       (make-value-trees (cdr lst)))))
(define (make-internal-node 0-tree 1-tree)
  (make-tree 'internal 0-tree 1-tree))
(define (build-huffman lst)
  (define (build-huffman-helper lst heap-pair)
    (if (null? (remove-min heap-pair))
        (car (car heap-pair))
        (build-huffman-helper lst
                              (insert (cons (make-internal-node (car (h-min heap-pair))
                                                                (car (h-min (remove-min heap-pair))))
                                            (+ (cdr (h-min heap-pair))
                                               (cdr (h-min (remove-min heap-pair)))))
                                      (remove-min (remove-min heap-pair))))))
  (build-huffman-helper lst (insert-list-of-pairs lst '())))
"test2"
(build-huffman (word-frequencies surfin-bird))

"problem3"

(define (add-1 lst)
  (append lst '(#\0)))

(define (add-0 lst)
  (append lst '(#\0)))

(define (get-encoding-list huff-tree)
  (define (get-encode-helper number-tracking tree)
    (if (null? tree)
        '()
        (cond ((not (or (equal? (car (cadr tree)) 'internal)
                        (equal? (car (caddr tree)) 'internal)))
               (list (cons (h-min (cadr tree)) (list->string (add-0 number-tracking)))
                     (cons (h-min (caddr tree)) (list->string (add-1 number-tracking)))))
              ((and (equal? (car (cadr tree)) 'internal)
                    (equal? (car (caddr tree)) 'internal))
               (append (get-encode-helper (add-0 number-tracking) (cadr tree))
                       (get-encode-helper (add-1 number-tracking) (caddr tree))))
              ((equal? (car (cadr tree)) 'internal)
               (cons (cons (car (caddr tree)) (list->string (add-1 number-tracking)))
                     (get-encode-helper (add-0 number-tracking) (cadr tree))))
              (else (cons (cons (car (cadr tree)) (list->string (add-0 number-tracking)))
                          (get-encode-helper (add-1 number-tracking) (caddr tree)))))))
  (get-encode-helper '() huff-tree))
"test3"
(get-encoding-list (build-huffman (word-frequencies surfin-bird)))

"problem4"

"problem5"
;(define (decode str huff-tree)

"problem6a"
"(word-frequencies surfin-bird)"
(word-frequencies surfin-bird)
"problem6b"
"(build-huffman (word-frequencies surfin-bird))"
(build-huffman (word-frequencies surfin-bird))
"problem6c"
