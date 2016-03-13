#lang typed/racket/base

(provide Quad-Treeof quad-tree?
         Quad-Tree-Branch quad-tree-leaf?
         Quad-Tree-Leaf quad-tree-branch?
         quad-tree->s-expression

         Quadrant-Name
         Quadrant-Path

         qtree-ref
         qtree-ref*
         
         qtree-map
         qtree-fold)

(struct (L) quad-leaf ([value : L]))
(struct (L) quad-branch ([∨∨ : (Quad-Treeof L)]
                         [∨∧ : (Quad-Treeof L)]
                         [∧∨ : (Quad-Treeof L)]
                         [∧∧ : (Quad-Treeof L)]))

(define-type (Quad-Tree-Leaf L) (quad-leaf L))
(define-type (Quad-Tree-Branch L) (quad-branch L))
(define-type (Quad-Treeof L) (U (Quad-Tree-Leaf L)
                                (Quad-Tree-Branch L)))

(define-predicate quad-tree? (Quad-Treeof Any))
(define-predicate quad-tree-leaf? (Quad-Tree-Leaf Any))
(define-predicate quad-tree-branch? (Quad-Tree-Branch Any))

(: quad-tree->s-expression (∀ (L) ((Quad-Treeof L) → (Sexpof L))))
(define (quad-tree->s-expression t)
  (if (quad-leaf? t)
      `(leaf ,(quad-leaf-value t))
      `(branch ,(quad-tree->s-expression (quad-branch-∨∨ t))
               ,(quad-tree->s-expression (quad-branch-∨∧ t))
               ,(quad-tree->s-expression (quad-branch-∧∨ t))
               ,(quad-tree->s-expression (quad-branch-∧∧ t)))))

(define-type Quadrant-Name (U '∨∨ '∨∧ '∧∨ '∧∧))
(define-type Quadrant-Path (Listof Quadrant-Name))

(: qtree-ref (∀ (L) ((quad-branch L) Quadrant-Name → (Quad-Treeof L))))
(define (qtree-ref t q)
  (case q
    [(∨∨) (quad-branch-∨∨ t)]
    [(∨∧) (quad-branch-∨∧ t)]
    [(∧∨) (quad-branch-∧∨ t)]
    [(∧∧) (quad-branch-∧∧ t)]))

(: qtree-ref* (∀ (L) ((Quad-Treeof L) Quadrant-Path → (Quad-Treeof L))))
(define (qtree-ref* t p)
  (cond
    [(null? p) t]
    [(quad-leaf? t) (error "Cannot get children of a leaf.")]
    [(qtree-ref* (qtree-ref t (car p)) (cdr p))]))

(: qtree-map (∀ (A B) ((A → B) (Quad-Treeof A) → (Quad-Treeof B))))
(define (qtree-map f t)
  (if (quad-branch? t)
      (quad-branch (qtree-map f (quad-branch-∨∨ t))
                   (qtree-map f (quad-branch-∨∧ t))
                   (qtree-map f (quad-branch-∧∨ t))
                   (qtree-map f (quad-branch-∧∧ t)))
      (quad-leaf (f (quad-leaf-value t)))))

(: qtree-fold (∀ (A B) ((B B B B → B) (A → B) (Quad-Treeof A) → B)))
(define (qtree-fold node-f leaf-f t)
  (if (quad-branch? t)
      (node-f (qtree-fold node-f leaf-f (quad-branch-∨∨ t))
              (qtree-fold node-f leaf-f (quad-branch-∨∧ t))
              (qtree-fold node-f leaf-f (quad-branch-∧∨ t))
              (qtree-fold node-f leaf-f (quad-branch-∧∧ t)))
      (leaf-f (quad-leaf-value t))))