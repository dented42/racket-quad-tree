#lang typed/racket/base

(provide quad-case quad-switch)

(require racket/stxparam
         (for-syntax racket/base
                     syntax/parse))

(define-syntax-parameter quad-switch
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside quad-case")))

(define-for-syntax (quad-switch-selector quad)
  (define-syntax-class qname
    #:datum-literals (∨∨ ∨∧ ∧∨ ∧∧)
    (pattern ∨∨)
    (pattern ∨∧)
    (pattern ∧∨)
    (pattern ∧∧))
  (λ (stx)
    (syntax-parse stx
      [(_ q:qname succeed:expr fail:expr)
       (if (equal? quad (syntax-e #'q))
           #'succeed
           #'fail)]
      [(_ (q:qname ...) succeed:expr fail:expr)
       (if (member quad (syntax->datum #'(q ...)))
           #'succeed
           #'fail)]
      [(_ ((q:qname succeed:expr) ...) fail:expr)
       (cond
         [(findf (λ (qclause)
                   (equal? quad (car (syntax->datum qclause))))
                 (syntax-e #'((q succeed) ...)))
          => (λ (qclause)
               (cadr (syntax-e qclause)))]
         [else #'fail])])))

(define-syntax (quad-case stx)
  (syntax-parse stx
    [(_ quad-selector:expr body:expr)
     #`(case quad-selector
         [(∨∨) (syntax-parameterize ([quad-switch (quad-switch-selector '∨∨)])
                 body)]
         [(∨∧) (syntax-parameterize ([quad-switch (quad-switch-selector '∨∧)])
                 body)]
         [(∧∨) (syntax-parameterize ([quad-switch (quad-switch-selector '∧∨)])
                 body)]
         [(∧∧) (syntax-parameterize ([quad-switch (quad-switch-selector '∧∧)])
                 body)])]))