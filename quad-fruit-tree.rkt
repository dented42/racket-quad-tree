#lang typed/racket/base

(provide QFTreeof qftree?
         QFTree-Branch qftree-leaf?
         QFTree-Leaf qftree-branch?

         qfleaf
         qfbranch
         
         qftree->sexp
         
         Quadrant-Name
         Quadrant-Path

         qfleaf-value
         qfbranch-fruit
         qfbranch-ref
         qftree-ref

         qfbranch-set-fruit
         qfbranch-set
         qftree-set
         
         qftree-map
         qftree-fold

         qftree-update
         qftree-update/leaf)

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

(: qfleaf (∀ (L F) (L → (QFTree-Leaf L F))))
(define (qfleaf l)
  (quad-fruit-leaf l))

(: qfbranch (∀ (L F) (F
                      (QFTreeof L F)
                      (QFTreeof L F)
                      (QFTreeof L F)
                      (QFTreeof L F) → (QFTree-Branch L F))))
(define (qfbranch f t1 t2 t3 t4)
  (quad-fruit-branch f t1 t2 t3 t4))

(: qftree->sexp ((QFTreeof Any Any) → (Sexpof Any)))
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

(: qfleaf-value (∀ (L F) ((QFTree-Leaf L F) → L)))
(define (qfleaf-value l)
  (quad-fruit-leaf-value l))

(: qfbranch-fruit (∀ (L F) ((QFTree-Branch L F) → F)))
(define (qfbranch-fruit t)
  (quad-fruit-branch-fruit t))

(: qfbranch-ref (∀ (L F) ((QFTree-Branch L F) Quadrant-Name → (QFTreeof L F))))
(define (qfbranch-ref t q)
  (case q
    [(∨∨) (quad-fruit-branch-∨∨ t)]
    [(∨∧) (quad-fruit-branch-∨∧ t)]
    [(∧∨) (quad-fruit-branch-∧∨ t)]
    [(∧∧) (quad-fruit-branch-∧∧ t)]))

(: qftree-ref (∀ (L F) ((QFTreeof L F) Quadrant-Path → (QFTreeof L F))))
(define (qftree-ref t p)
  (cond
    [(null? p) t]
    [(quad-fruit-leaf? t) (error "Cannot get children of a leaf.")]
    [(qftree-ref (qfbranch-ref t (car p)) (cdr p))]))

(: qfbranch-set-fruit (∀ (L F) ((QFTree-Branch L F) F → (QFTree-Branch L F))))
(define (qfbranch-set-fruit tree fruit)
  (quad-fruit-branch fruit
                     (quad-fruit-branch-∨∨ tree)
                     (quad-fruit-branch-∨∧ tree)
                     (quad-fruit-branch-∧∨ tree)
                     (quad-fruit-branch-∧∧ tree)))

(: qfbranch-set (∀ (L F) ((QFTree-Branch L F) Quadrant-Name (QFTreeof L F) → (QFTreeof L F))))
(define (qfbranch-set tree quadrant update)
  (case quadrant
    [(∨∨) (quad-fruit-branch (quad-fruit-branch-fruit tree)
                             update
                             (quad-fruit-branch-∨∧ tree)
                             (quad-fruit-branch-∧∨ tree)
                             (quad-fruit-branch-∧∧ tree))]
    [(∨∧) (quad-fruit-branch (quad-fruit-branch-fruit tree)
                             (quad-fruit-branch-∨∨ tree)
                             update
                             (quad-fruit-branch-∧∨ tree)
                             (quad-fruit-branch-∧∧ tree))]
    [(∧∨) (quad-fruit-branch (quad-fruit-branch-fruit tree)
                             (quad-fruit-branch-∨∨ tree)
                             (quad-fruit-branch-∨∧ tree)
                             update
                             (quad-fruit-branch-∧∧ tree))]
    [(∧∧) (quad-fruit-branch (quad-fruit-branch-fruit tree)
                             (quad-fruit-branch-∨∨ tree)
                             (quad-fruit-branch-∨∧ tree)
                             (quad-fruit-branch-∧∨ tree)
                             update)]))

(: qftree-set (∀ (L F) ((QFTreeof L F) Quadrant-Path (QFTreeof L F) → (QFTreeof L F))))
(define (qftree-set tree path update)
  (cond
    [(null? path) update]
    [(quad-fruit-leaf? tree) (error "Cannot set children of a leaf.")]
    [(qftree-set (qfbranch-ref tree (car path)) (cdr path) update)]))

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

(: qftree-update (∀ (L F) ((QFTreeof L F)
                           (F → (U #f Quadrant-Name))
                           ((QFTreeof L F) → (QFTreeof L F)) → (QFTreeof L F))))
(define (qftree-update tree path-finder updater)
  (cond
    [(quad-fruit-leaf? tree) (updater tree)]
    [(path-finder (quad-fruit-branch-fruit tree))
     => (λ (step)
          (qfbranch-set tree step (qftree-update (qfbranch-ref tree step) path-finder updater)))]
    [else (updater tree)]))

(: qftree-update/leaf (∀ (L F) ((QFTreeof L F)
                                (F → Quadrant-Name)
                                (L → (QFTreeof L F)) → (QFTreeof L F))))
(define (qftree-update/leaf tree path-finder updater)
  (if (quad-fruit-leaf? tree)
      (updater (quad-fruit-leaf-value tree))
      (let ([step (path-finder (quad-fruit-branch-fruit tree))])
        (qfbranch-set tree step (qftree-update/leaf (qfbranch-ref tree step) path-finder updater)))))