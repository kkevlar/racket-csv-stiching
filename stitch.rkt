#lang racket

(require csv-writing)
(require 2htdp/batch-io)
(require (planet neil/csv:1:=7) net/url)

(define make-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))


(define (all-rows url make-reader)
  (define next-row (make-reader (get-pure-port url)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop))))
  (loop))

(define post-url 
  (string->url
   "https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet='{sheetname}'))
(define pre-url 
  (string->url
"https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet='{sheetname}'))

(define post-data (all-rows
                   post-url
                   make-csv-reader ))

(define pre-data (all-rows
                  pre-url
                  make-csv-reader ))

(define (proc-pre row)
  (define nts (rest row))
  (define no-agreement (cons (first nts) (rest (rest nts))))
  (define no-prequiz (append (take no-agreement 6) (list-tail no-agreement 9)))
  no-prequiz)

(define (proc-post row)
  (rest row))

(define (stitch-row prerow postrow)
  (append (proc-pre prerow) (proc-post postrow)))

(define (datasort table-no-labels)
  (sort table-no-labels (λ (one two) (<= (string->number (first one)) (string->number (first two))))))

(define sorted-pre (datasort (map proc-pre (rest pre-data))))
(define sorted-post (datasort (map proc-post (rest post-data))))

(define (stitch spre spost)
  (cond
    [(empty? spre) '()]
    [(empty? spost) '()]
    [else
     (define (gimme-id r) (first (first r)))
     (printf "Matching pre ~v\n" (gimme-id spre))
     (cond
       [(equal? (gimme-id spre) (gimme-id spost))
        (define stitched-line (append (first spre) (rest (first spost))))
        (define stitched-rest (stitch (rest spre) (rest spost)))
        (cons stitched-line stitched-rest)]
       [else
        (printf "Throwing out post test id ~v\n" (gimme-id spost))
        (stitch spre (rest spost))])]))


(define (filter-pre-ids sorted-pre filter-ids)
  (cond
    ;[(empty? sorted-pre) sorted-pre]
    [(empty? filter-ids) sorted-pre]
    [else (define (mypred element) (not (equal? (first element) (first filter-ids))))
          (filter-pre-ids (filter mypred sorted-pre) (rest filter-ids))])
  )

(define answer-key-row (first (filter (λ (row) (equal? (first row) "123456")) sorted-post)))
(define (map-post-row-to-scored-row answer-key-row curr-row questions-left)
  (cond
    [(>= 0 questions-left) curr-row]
    ; [(equal? 2 questions-left) (rest curr-row)]
    [else (cons
           (cond
             [(equal? (first answer-key-row) (first curr-row)) "1"]
             [else 0])
           (map-post-row-to-scored-row (rest answer-key-row) (rest curr-row) (- questions-left 1))
           )]))

(define sorted-scored
  (map (λ (row) (cons (first row) (map-post-row-to-scored-row (rest answer-key-row) (rest row) 11)))
       sorted-post ))


(define finished (stitch
                  (filter-pre-ids sorted-pre
                                  (list "1359" "1361"))
                  sorted-scored))

(define finished-with-labels (cons (stitch-row (first pre-data) (rest (first post-data))) finished))

(define (consume-filters filters data)
  (cond
    [(empty? filters) data]
    [else (filter (first filters) (consume-filters (rest filters) data) )]))

(define x-filters (list
                 (λ (row) (not (and (equal? "3" (list-ref row 4)) (equal? "Yes" (list-ref row 34)))))
                 (λ (row) (equal? "Yes" (list-ref row 34)))))

(define finished-x (cons (first finished-with-labels)
                                (consume-filters x-filters (rest finished-with-labels))))
(define y-filters (list                 
                 (λ (row) (equal? "No" (list-ref row 34)))))

(define finished-y (cons (first finished-with-labels)
                                (consume-filters y-filters (rest finished-with-labels))))



(define (calc data)
  (define (row-to-ques row) (take (drop row 15) 11))
  (define quess (map (λ (r) (map (λ (s) (if (number? s) s (string->number s))) r))
                     (map row-to-ques data)))
  (define (plus-lists one two)
    (cond
      [(and (empty? one ) (empty? two)) '()]
      [(or (empty? one ) (empty? two)) (error 'plus-lists "length mismatch")]
      [else (cons (+ (first one) (first two))
                  (plus-lists (rest one) (rest two)))]))
  (define (consume list)
    (cond
      [(empty? (rest list)) (first list)]
      [else (plus-lists (first list) (consume (rest list)))]
      ))
  (define zip (append (consume quess) (list (length quess))))
  zip)
 

(write-file "/home/kevlar/Desktop/test.csv"
            (table->string (append finished-x (rest finished-y))))
;(write-file "/home/kevlar/Desktop/test.csv"
;            (table->string (list (calc (rest finished-x)) (calc (rest finished-y)))))



