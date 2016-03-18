#lang typed/racket/base

(provide dispatch-case dispatch)

(require racket/stxparam
         (for-syntax racket/base
                     racket/list
                     syntax/parse))

(define-syntax-parameter dispatch
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside dispatch-case")))

(define-for-syntax (dispatch-transformer this-case all-cases)
  (λ (stx)
    (syntax-parse stx
      [(_ d:expr succeed:expr fail:expr)
       #:fail-unless (member (syntax->datum #'d) (syntax->datum all-cases)) "unknown dispatch case"
       (if (equal? (syntax->datum this-case) (syntax->datum #'d))
           #'succeed
           #'fail)]
      [(_ (d:expr ...) succeed:expr fail:expr)
       #:fail-unless (andmap (λ (d)
                               (member d (syntax->datum all-cases)))
                             (syntax->datum #'(d ...)))
       "unknown dispatch case"
       #:fail-when (check-duplicates (syntax->datum #'(d ...)))
       "overlapping dispatch cases"
       (if (member (syntax->datum this-case) (syntax->datum #'(d ...)))
           #'succeed
           #'fail)]
      [(_ ((d:expr succeed:expr) ...) fail:expr)
       #:fail-unless (andmap (λ (d)
                               (member d (syntax->datum all-cases)))
                             (syntax->datum #'(d ...)))
       "unknown dispatch case"
       #:fail-when (check-duplicates (syntax->datum #'(d ...)))
       "overlapping dispatch cases"
       (cond
         [(findf (λ (qclause)
                   (equal? (syntax->datum this-case) (car (syntax->datum qclause))))
                 (syntax-e #'((d succeed) ...)))
          => (λ (qclause)
               (cadr (syntax-e qclause)))]
         [else #'fail])])))

(define-syntax (dispatch-case stx)
  (syntax-parse stx
    [(_ selector:expr (case-datum ...) body:expr)
    #:fail-when (check-duplicates (syntax->datum #'(case-datum ...)))
    "overlapping dispatch cases"
     #`(case selector
         [(case-datum) (syntax-parameterize ([dispatch (dispatch-transformer #'case-datum
                                                                             #'(case-datum ...))])
                         body)] ...)]))