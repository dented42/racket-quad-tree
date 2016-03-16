#lang typed/racket/base

(provide QTreeof qtree?
         QTree-Branch qtree-leaf?
         QTree-Leaf qtree-branch?

         qleaf
         qbranch
         
         qtree->sexp
         
         Quadrant-Name
         Quadrant-Path

         qleaf-value
         qbranch-ref
         qtree-ref

         qbranch-set
         qtree-set
         
         qtree-map
         qtree-fold)

(struct (L) quad-leaf ([value : L]))
(struct (L) quad-branch ([∨∨ : (QTreeof L)]
                         [∨∧ : (QTreeof L)]
                         [∧∨ : (QTreeof L)]
                         [∧∧ : (QTreeof L)]))

(define-type (QTree-Leaf L) (quad-leaf L))
(define-type (QTree-Branch L) (quad-branch L))
(define-type (QTreeof L) (U (QTree-Leaf L)
                            (QTree-Branch L)))

(define-predicate qtree? (QTreeof Any))
(define-predicate qtree-leaf? (QTree-Leaf Any))
(define-predicate qtree-branch? (QTree-Branch Any))

(: qleaf (∀ (L) (L → (QTree-Leaf L))))
(define (qleaf l)
  (quad-leaf l))

(: qbranch (∀ (L) ((QTreeof L) (QTreeof L) (QTreeof L) (QTreeof L) → (QTree-Branch L))))
(define (qbranch t1 t2 t3 t4)
  (quad-branch t1 t2 t3 t4))

(: qtree->sexp ((QTreeof Any) → (Sexpof Any)))
(define (qtree->sexp t)
  (if (qtree-leaf? t)
      `(leaf ,(quad-leaf-value t))
      `(branch ,(qtree->sexp (quad-branch-∨∨ t))
               ,(qtree->sexp (quad-branch-∨∧ t))
               ,(qtree->sexp (quad-branch-∧∨ t))
               ,(qtree->sexp (quad-branch-∧∧ t)))))

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))

(: qleaf-value (∀ (L) ((QTree-Leaf L) → L)))
(define (qleaf-value l)
  (quad-leaf-value l))

(: qbranch-ref (∀ (L) ((quad-branch L) Quadrant-Name → (QTreeof L))))
(define (qbranch-ref t q)
  (case q
    [(∨∨) (quad-branch-∨∨ t)]
    [(∨∧) (quad-branch-∨∧ t)]
    [(∧∨) (quad-branch-∧∨ t)]
    [(∧∧) (quad-branch-∧∧ t)]))

(: qtree-ref (∀ (L) ((QTreeof L) Quadrant-Path → (QTreeof L))))
(define (qtree-ref t p)
  (cond
    [(null? p) t]
    [(quad-leaf? t) (error "Cannot get children of a leaf.")]
    [(qtree-ref (qbranch-ref t (car p)) (cdr p))]))

(: qbranch-set (∀ (L) ((QTree-Branch L) Quadrant-Name (QTreeof L) → (QTreeof L))))
(define (qbranch-set tree quadrant update)
  (case quadrant
    [(∨∨) (quad-branch update
                       (quad-branch-∨∧ tree)
                       (quad-branch-∧∨ tree)
                       (quad-branch-∧∧ tree))]
    [(∨∧) (quad-branch (quad-branch-∨∨ tree)
                       update
                       (quad-branch-∧∨ tree)
                       (quad-branch-∧∧ tree))]
    [(∧∨) (quad-branch (quad-branch-∨∨ tree)
                       (quad-branch-∨∧ tree)
                       update
                       (quad-branch-∧∧ tree))]
    [(∧∧) (quad-branch (quad-branch-∨∨ tree)
                       (quad-branch-∨∧ tree)
                       (quad-branch-∧∨ tree)
                       update)]))

(: qtree-set (∀ (L) ((QTreeof L) Quadrant-Path (QTreeof L) → (QTreeof L))))
(define (qtree-set tree path update)
  (cond
    [(null? path) update]
    [(quad-leaf? tree) (error "Cannot set children of a leaf.")]
    [(qtree-set (qbranch-ref tree (car path)) (cdr path) update)]))

(: qtree-map (∀ (A B) ((A → B) (QTreeof A) → (QTreeof B))))
(define (qtree-map f t)
  (if (quad-branch? t)
      (quad-branch (qtree-map f (quad-branch-∨∨ t))
                   (qtree-map f (quad-branch-∨∧ t))
                   (qtree-map f (quad-branch-∧∨ t))
                   (qtree-map f (quad-branch-∧∧ t)))
      (quad-leaf (f (quad-leaf-value t)))))

(: qtree-fold (∀ (A B) ((B B B B → B) (A → B) (QTreeof A) → B)))
(define (qtree-fold node-f leaf-f t)
  (if (quad-branch? t)
      (node-f (qtree-fold node-f leaf-f (quad-branch-∨∨ t))
              (qtree-fold node-f leaf-f (quad-branch-∨∧ t))
              (qtree-fold node-f leaf-f (quad-branch-∧∨ t))
              (qtree-fold node-f leaf-f (quad-branch-∧∧ t)))
      (leaf-f (quad-leaf-value t))))