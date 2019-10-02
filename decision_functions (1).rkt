#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1 (cons "feature1" (lambda (x) (first x)))) ; returns the value of feature 1 for a given test sample
(define y2 (cons "feature2" (lambda (x) (second x))))
(define y3 (cons "feature3" (lambda (x) (third x))))
(define y4>62 (cons "feature4>62" (lambda (x) (if (< 62 (fourth x)) 1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass (cons "pclass" (lambda (x) (first x)))) ; returns the value of pclass for a given test sample
(define sex (cons "sex" (lambda (x) (second x))))
(define age>25 (cons "age>25" (lambda (x) (if (< 25 (third x)) 1 0))))
(define sibsp (cons "sibsp" (lambda (x) (fourth x))))
(define parch (cons "parch" (lambda (x) (fifth x))))
(define fare>50 (cons "fare>50" (lambda (x) (if (< 50 (sixth x)) 1 0))))
(define emb (cons "emb" (lambda (x) (seventh x))))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" (lambda (x) (first x))))
(define csurf (cons "csurf" (lambda (x) (second x))))
(define bruise (cons "bruise" (lambda (x) (third x))))
(define odor (cons "odor" (lambda (x) (fourth x))))
(define gatch (cons "gatch" (lambda (x) (fifth x))))
(define gspace (cons "gspace" (lambda (x) (sixth x))))
(define gsize (cons "gsize" (lambda (x) (seventh x))))
(define sshape (cons "sshape" (lambda (x) (eighth x))))
(define nring (cons "nring" (lambda (x) (ninth x))))
(define pop (cons "pop" (lambda (x) (tenth x))))
(define hab (cons "hab" (lambda (x) (tenth (cdr x)))))
