#lang typed/racket/base

(provide dispatch-case/quad dispatch)

(require quad-tree/misc/dispatch-case)

(define-syntax-rule (dispatch-case/quad selector body)
  (dispatch-case selector (∨∨ ∨∧ ∧∨ ∧∧) body))