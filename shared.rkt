#lang typed/racket/base

(provide dispatch-case/quad dispatch
         Quadrant-Name
         Quadrant-Path)

(require quad-tree/misc/dispatch-case)

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))

(define-syntax-rule (dispatch-case/quad selector body)
  (dispatch-case selector (∨∨ ∨∧ ∧∨ ∧∧) body))