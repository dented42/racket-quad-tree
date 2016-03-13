#lang typed/racket/base

(provide QTreeof qtree?
         QTree-Branch qtree-leaf?
         QTree-Leaf qtree-branch?
         qtree->sexp
         
         Quadrant-Name
         Quadrant-Path
         
         qtree-ref
         qtree-ref*
         
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

(: qtree->sexp (∀ (L) ((QTreeof L) → (Sexpof L))))
(define (qtree->sexp t)
  (if (qtree-leaf? t)
      `(leaf ,(quad-leaf-value t))
      `(branch ,(qtree->sexp (quad-branch-∨∨ t))
               ,(qtree->sexp (quad-branch-∨∧ t))
               ,(qtree->sexp (quad-branch-∧∨ t))
               ,(qtree->sexp (quad-branch-∧∧ t)))))

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))

(: qtree-ref (∀ (L) ((quad-branch L) Quadrant-Name → (QTreeof L))))
(define (qtree-ref t q)
  (case q
    [(∨∨) (quad-branch-∨∨ t)]
    [(∨∧) (quad-branch-∨∧ t)]
    [(∧∨) (quad-branch-∧∨ t)]
    [(∧∧) (quad-branch-∧∧ t)]))

(: qtree-ref* (∀ (L) ((QTreeof L) Quadrant-Path → (QTreeof L))))
(define (qtree-ref* t p)
  (cond
    [(null? p) t]
    [(quad-leaf? t) (error "Cannot get children of a leaf.")]
    [(qtree-ref* (qtree-ref t (car p)) (cdr p))]))

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