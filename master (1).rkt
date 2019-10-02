#lang racket 

(define (zip l1 l2)
(cond [(null? l1) l2]
[(null? l2) l1]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(provide (all-defined-out))
(struct node(t1 t2) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))


(require 2htdp/batch-io)

(require "decision_functions.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings

;(define (split l1 l2)
 ; (if (null? l1) l2
 ;     (split (cdr l1) (append l2 (list (break (car l1)))))))
;(define (break x)
 ; (break-h (string->list x) '() '()))
  ;(define (break-h l1 l2 l3)
   ; (cond [(null? l1) (append l3 (list (list->string l2)))]
    ;      [(equal? (car l1) #\,) (break-h (cdr l1) '() (append l3 (list (list->string l2))))]
     ;     [else (break-h (cdr l1) (append l2 (list (car l1))) l3)]))


(provide toy-raw)
(define toy-raw
    (map (lambda (x) (map (lambda (y) (string->number y)) x)) (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw
(let ([l (cons (cddar (read-csv-file titanictrain)) (lc (cddr x) : x <- (cdr (read-csv-file titanictrain))))])
  (map (lambda (x) (map (lambda (y) (string->number y)) x)) l)))



(provide mushroom-raw)
(define mushroom-raw
  (map (lambda (x) (map (lambda (y) (string->number y)) x)) (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (cons (cdr data) (car data)))

;list of (features . result)
(provide toy)
(define toy
  (cdr (lc (format x) : x <- toy-raw)))

(provide titanic)
(define titanic
  (cdr (lc (format x) : x <- titanic-raw)))

(provide mushroom)
(define mushroom
  (cdr (lc (format x) : x <- mushroom-raw)))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (/ (length (filter (lambda (x) (equal? (cdr x) 1)) data)) (length data)))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (let ([Fp (get-leaf-prob data)]
        [Fn (- 1 (get-leaf-prob data))])
    (if (or (= 0 Fn) (= 0 Fp)) 0
    (/ (+ (* -1 Fp (log Fp)) (* -1 Fn (log Fn))) (log 2)))))

;find the difference in entropy achieved
;by applying a decision function f to the data
(define (sum l)
  (sum-h l 0))
(define (sum-h l1 total)
  (if (null? l1) total
      (sum-h (cdr l1) (+ total (car l1)))))

(define (form-groups f data)
 (let* ([a (lc (cons (f (car x)) x) : x <- data)]
        [b (sort a (lambda (x y) (< (car x) (car y))))])
   (work-on (cdr b) (list (car b)) '())))
(define (work-on l1 l2 l3)
  (cond [(null? l1) (append l3 (list (lc (cdr x) : x <- l2)))]
        [(equal? (caar l1) (caar l2)) (work-on (cdr l1) (cons (car l1) l2) l3)]
        [else (work-on (cdr l1) (list (car l1)) (append l3 (list (lc (cdr x) : x <- l2))))]))
        
  
(provide entropy-diff)
(define (entropy-diff f data)
  (- (get-entropy data) (sum (lc (/ (* (length x) (get-entropy x)) (length data)) : x <- (form-groups f data)))))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
 (let ([p (lc (cons x (entropy-diff (cdr x) data)) : x <- candidates)])
   (caar (sort p (lambda (x y) (>= (cdr x) (cdr y)))))))

(provide DTree)
(struct DTree (desc func kids) #:transparent)

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (cond [(= depth 0) (DTree (number->string (get-leaf-prob data)) "done" '())]
        [(= (length candidates) 0) (DTree (number->string (get-leaf-prob data)) "done" '())]
        [(or (= 1 (get-leaf-prob data)) (= 0 (get-leaf-prob data))) (DTree (number->string (get-leaf-prob data)) "done" '())] 
      [else (let ([chosen (choose-f candidates data)])
     (DTree (car chosen) (cons (cdr chosen) (remove-duplicates (sort (lc ((cdr chosen) (car y)) : y <- data) <)))
                                                                   (lc (build-tree (remove chosen candidates) x (- depth 1)) : x <- (form-groups (cdr chosen) data))))]))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree desc func kids) (if (null? kids) (string->number desc)
                                (if (equal? #f (index-of (cdr func) ((car func) test))) 0
                                (make-decision (list-ref kids (index-of (cdr func) ((car func) test))) test)))]))
                                       
;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================
