#lang typed/racket/base

(provide Quad-Treeof
         (struct-out quad-leaf)
         (struct-out quad-tree)
         quad-tree-map
         quad-tree-fold)

(define-type (Quad-Treeof A) (U (quad-leaf A) (quad-tree A)))

(struct (A) quad-leaf ([value : A]) #:transparent)
(struct (A) quad-tree ([|00| : (Quad-Treeof A)]
                       [|01| : (Quad-Treeof A)]
                       [|10| : (Quad-Treeof A)]
                       [|11| : (Quad-Treeof A)])
  #:transparent)

(: quad-tree-map (∀ (A B) ((A → B) (Quad-Treeof A) → (Quad-Treeof B))))
(define (quad-tree-map f t)
  (if (quad-tree? t)
      (quad-tree (quad-tree-map f (quad-tree-00 t))
                 (quad-tree-map f (quad-tree-01 t))
                 (quad-tree-map f (quad-tree-10 t))
                 (quad-tree-map f (quad-tree-11 t)))
      (quad-leaf (f (quad-leaf-value t)))))

(: quad-tree-fold (∀ (A B) ((B B B B → B) (A → B) (Quad-Treeof A) → B)))
(define (quad-tree-fold node-f leaf-f t)
  (if (quad-tree? t)
      (node-f (quad-tree-fold node-f leaf-f (quad-tree-00 t))
              (quad-tree-fold node-f leaf-f (quad-tree-01 t))
              (quad-tree-fold node-f leaf-f (quad-tree-10 t))
              (quad-tree-fold node-f leaf-f (quad-tree-11 t)))
      (leaf-f (quad-leaf-value t))))