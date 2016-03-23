#lang typed/racket/base

(provide Quadrant-Name
         Quadrant-Path)

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))