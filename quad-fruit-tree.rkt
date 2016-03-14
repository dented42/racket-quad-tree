#lang typed/racket/base

(provide QFTreeof qftree?
         QFTree-Branch qftree-leaf?
         QFTree-Leaf qftree-branch?
         qftree->sexp
         
         Quadrant-Name
         Quadrant-Path
         
         qftree-ref
         qftree-ref*
         
         qftree-map
         qftree-fold)

(struct (L F) quad-fruit-leaf ([value : L]))
(struct (L F) quad-fruit-branch ([fruit : F]
                                 [∨∨ : (QFTreeof L F)]
                                 [∨∧ : (QFTreeof L F)]
                                 [∧∨ : (QFTreeof L F)]
                                 [∧∧ : (QFTreeof L F)]))

(define-type (QFTree-Leaf L F) (quad-fruit-leaf L F))
(define-type (QFTree-Branch L F) (quad-fruit-branch L F))
(define-type (QFTreeof L F) (U (QFTree-Leaf L F)
                               (QFTree-Branch L F)))

(define-predicate qftree? (QFTreeof Any Any))
(define-predicate qftree-leaf? (QFTree-Leaf Any Any))
(define-predicate qftree-branch? (QFTree-Branch Any Any))

(: qftree->sexp (∀ (L F) ((QFTreeof L F) → (Sexpof Any))))
(define (qftree->sexp t)
  (if (quad-fruit-leaf? t)
      `(leaf ,(quad-fruit-leaf-value t))
      `(branch (fruit ,(quad-fruit-branch-fruit t))
               ,(qftree->sexp (quad-fruit-branch-∨∨ t))
               ,(qftree->sexp (quad-fruit-branch-∨∧ t))
               ,(qftree->sexp (quad-fruit-branch-∧∨ t))
               ,(qftree->sexp (quad-fruit-branch-∧∧ t)))))

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))

(: qftree-ref (∀ (L F) ((QFTree-Branch L F) Quadrant-Name → (QFTreeof L F))))
(define (qftree-ref t q)
  (case q
    [(∨∨) (quad-fruit-branch-∨∨ t)]
    [(∨∧) (quad-fruit-branch-∨∧ t)]
    [(∧∨) (quad-fruit-branch-∧∨ t)]
    [(∧∧) (quad-fruit-branch-∧∧ t)]))

(: qftree-ref* (∀ (L F) ((QFTreeof L F) Quadrant-Path → (QFTreeof L F))))
(define (qftree-ref* t p)
  (cond
    [(null? p) t]
    [(quad-fruit-leaf? t) (error "Cannot get children of a leaf.")]
    [(qftree-ref* (qftree-ref t (car p)) (cdr p))]))

(: qftree-map (∀ (L F L* F*) ((L → L*)
                              (F → F*)
                              (QFTreeof L F) → (QFTreeof L* F*))))
(define (qftree-map leaf-func fruit-func t)
  (if (quad-fruit-branch? t)
      (quad-fruit-branch (fruit-func (quad-fruit-branch-fruit t))
                         (qftree-map leaf-func fruit-func (quad-fruit-branch-∨∨ t))
                         (qftree-map leaf-func fruit-func (quad-fruit-branch-∨∧ t))
                         (qftree-map leaf-func fruit-func (quad-fruit-branch-∧∨ t))
                         (qftree-map leaf-func fruit-func (quad-fruit-branch-∧∧ t)))
      (quad-fruit-leaf (leaf-func (quad-fruit-leaf-value t)))))

(: qftree-fold (∀ (L F X) ((F X X X X → X) (L → X) (QFTreeof L F) → X)))
(define (qftree-fold node-f leaf-f t)
     (if (quad-fruit-branch? t)
         (node-f (quad-fruit-branch-fruit t)
                 (qftree-fold node-f leaf-f (quad-fruit-branch-∨∨ t))
                 (qftree-fold node-f leaf-f (quad-fruit-branch-∨∧ t))
                 (qftree-fold node-f leaf-f (quad-fruit-branch-∧∨ t))
                 (qftree-fold node-f leaf-f (quad-fruit-branch-∧∧ t)))
         (leaf-f (quad-fruit-leaf-value t))))