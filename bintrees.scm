;; -*- mode: scheme48; scheme48-package: bintrees -*-

(define-structure bintrees (export bintree-member?
				   bintree-size
				   bintree-reverse
				   bintree-preorder
				   fast-bintree-preorder
				   bintree-postorder
				   fast-bintree-postorder
				   bintree-inorder
				   fast-bintree-inorder
				   bst-empty?
				   bst-member?
				   bst-insert
				   (bst-insert! :syntax)
				   bst-remove
				   list->tree
				   treesum
				   treesort
				   L
				   test-bintree
				   test-bst)
  (open scheme srfi-1)
  (begin
    (define L '(128 76 106 402 100 46 354 1018 112 28 396 35))
    (define test-bintree '(- (+ (128) (12)) (136.2)))
    (define test-bst '(2 (1 (1) (2)) (3 (3) (4))))

;;; BINARY TREES.
    ;; Constructors.

    (define (make-bintree-leaf E)
      "Create a leaf."
      (list E))

    (define (make-bintree-node E B1 B2)
      "Create a node with element E, left subtree B1 and right subtree B2."
      (list E B1 B2))

    ;; Selectors.

    (define (bintree-leaf-element L)
      "Retrieve the element of a leaf L."
      (first L))
					; Note the redundancy here. Might be able to economize...
    (define (bintree-node-element N)
      "Retrieve the element of a node N."
      (first N))

    (define (bintree-node-left N)
      "Retrieve the left subtree of a node N."
      (second N))

    (define (bintree-node-right N)
      "Retrieve the right subtree of a node N."
      (third N))

    ;; Recognizers.

    (define (bintree-leaf? B)
      "Test if binary tree B is a leaf."
      (and (list? B)
	   (= (length B) 1)))

    (define (bintree-node? B)
      "Test if binary tree B is a node."
      (and (list? B)
	   (= (length B) 3)))

    (define (bintree-member? E B)
      "Test if element E is a member of binary tree B."
      (if (bintree-leaf? B) ; See if B is already a leaf, e.g., a list with length of 1.
	  (equal? E (bintree-leaf-element B)) ; If so, is E equal? to B?
	  (or (equal? E (bintree-node-element B)) ; is E equal to the node element?
	      (bintree-member? E (bintree-node-left B)) ; ... otherwise, we recur down
	      (bintree-member? E (bintree-node-right B))))) ; the two halves of the tree..

    ;; Exercise: Let size(B) be the number of members of a binary tree B. Give a recursive definition of size(B), then implement a Scheme procedure (bintree-size B) that returns size(B).

    ;; If B is a leaf, its size is 1. Otherwise, add 1 to the sum of size(left subtree of B) and size(right subtree of B).

    (define (bintree-size B)
      "How many elements in binary tree B?"
      (if (bintree-leaf? B)
	  1
	  (+ 1 (+ (bintree-size (bintree-node-left B))
		  (bintree-size (bintree-node-right B))))))

    (define (bintree-reverse B)
      "Reverse binary tree B."
      (if (bintree-leaf? B)
	  B
	  (make-bintree-node
	   (bintree-node-element B)
	   (bintree-reverse (bintree-node-right B))
	   (bintree-reverse (bintree-node-left B)))))

    (define (bintree-preorder B)
      "Create a list containing keys of B in preorder."
      (if (bintree-leaf? B)
	  (list (bintree-leaf-element B))
	  (cons (bintree-node-element B)
		(append 
		 (bintree-preorder (bintree-node-left B))
		 (bintree-preorder (bintree-node-right B))))))

    (define (fast-bintree-preorder B)
      "A tail-recursive version of bin-tree-preorder."
      ;; Note that this is an example of sequential programming, and would
      ;; in theory be less efficient in parallel on multicore systems. See
      ;; Guy Steele's 'foldl and foldr considered slightly harmful.'
      (letrec ((preorder-aux
		(lambda (B A)
		  (if (bintree-leaf? B)
		      (cons (bintree-leaf-element B) A)
		      (cons (bintree-leaf-element B)
			    (preorder-aux (bintree-node-left B)
					  (preorder-aux (bintree-node-right B) A)))))))
	(preorder-aux B '())))

    ;; Exercise: Implement a function that will create a list containing
    ;; members of a given binary tree in postorder. Implement also a
    ;; tail-recursive version of the same function.

    (define (bintree-postorder-easy B)
      "Create a postorder list of keys in B, defined using
'bintree-preorder.'"
      (if (bintree-leaf? B)
	  (list (bintree-leaf-element B))
	  (reverse (bintree-preorder B))))

    (define (bintree-postorder B)
      "Create a list containing keys of B in postorder."
      (if (bintree-leaf? B)
	  (list (bintree-leaf-element B))
	  (append (bintree-postorder (bintree-node-right B))
		  (append (bintree-postorder (bintree-node-left B))
			  (cons (bintree-leaf-element B) '())))))

    (define (fast-bintree-postorder B)
      "A tail-recursive version of 'bintree-postorder'."
      (letrec ((postorder-aux
		(lambda (B A)
		  (if (bintree-leaf? B)
		      (cons (bintree-leaf-element B) A)
		      (postorder-aux
		       (bintree-node-right B)
		       (postorder-aux
			(bintree-node-left B)
			(cons (bintree-leaf-element B) A)))))))
	(postorder-aux B '())))

    ;; Exercise: Repeat the last exercise with inorder.

    (define (bintree-inorder B)
      "Create a list containing keys of B in 'inorder' (infix) notation."
      (if (bintree-leaf? B)
	  (list (bintree-leaf-element B))
	  (append
	   (append (bintree-inorder (bintree-node-left B))
		   (cons
		    (bintree-leaf-element B) '()))
	   (bintree-inorder (bintree-node-right B)))))

    (define (fast-bintree-inorder B)
      "A tail-recursive version of 'bintree-inorder'."
      (letrec ((inorder-aux
		(lambda (B A C)
		  (if (bintree-leaf? B)
		      (cons (bintree-leaf-element B) C)
		      (inorder-aux
		       (bintree-node-left B) A
		       (inorder-aux
			(cons (bintree-leaf-element B) A) C
			(inorder-aux
			 (bintree-node-right B) A C)))))))
	(inorder-aux B '() '())))

;;; SETS.

    (define (make-empty-set)
      "Creates an empty set."
      '())

    (define (set-insert E S)
      "Return a set containing all the members of the set S plus the
element E."
      (lset-adjoin equal? S E))

    (define (set-remove E S)
      "Return a set containing all the members of the set S except the
element E."
      (let ((not-E?
	     (lambda (elem)
	       (if (equal? E elem)
		   #f
		   #t))))
	(filter not-E? S)))

    (define (set-member? E S)
      "Return #t if set S contains element E."
      (if (member E S)
	  #t
	  #f))

    (define (set-empty? S)
      "Return #t if the set S is empty."
      (null? S))


;;; BINARY SEARCH TREES.
    ;; These will be implemented in terms of our above binary trees.

    (define (make-empty-bst)
      "Create an empty binary search tree."
      '())

    (define (bst-empty? B)
      "Check if the given binary search tree is empty."
      (null? B))

    (define (bst-member? E B)
      "Check if the given element E is a member of binary search tree B."
      (if (bst-empty? B)
	  #f
	  (bst-nonempty-member? E B)))

    (define (bst-nonempty-member? E B)
      "Check if element E is a member of non-empty binary search tree B."
      (if (bintree-leaf? B)
	  (= E (bintree-leaf-element B))
	  (if (<= E (bintree-node-element B))
	      (bst-nonempty-member? E (bintree-node-left B))
	      (bst-nonempty-member? E (bintree-node-right B)))))

    (define (bst-insert E B)
      "Insert element E into binary search tree B."
      (if (bst-empty? B)
	  (make-bintree-leaf E)
	  (bst-nonempty-insert E B)))
    
    (define-syntax bst-insert!
      "Destructively insert element E into binary search tree B."
      (syntax-rules ()
	((bst-insert! element tree)
	 (set! tree (bst-insert element tree)))))

    (define (list->tree xs)
      (let ((tree '()))
	(for-each (lambda (x)
		    (set! tree (bst-insert x tree)))
		  xs)
	tree))

    (define (atom? a)
      (and (not (null? a))
	   (not (pair? a))))

    (define (treesum tree)
      (cond ((null? tree) 0)
	    ((atom? (car tree))
	     (+ (car tree)
		(treesum (cdr tree))))
	    (else (+ (treesum (car tree))
		     (treesum (cdr tree))))))

    (define (treesort xs)
      (let ((result '())
	    (seen '())
	    (tree (list->tree xs)))
	(for-each
	 (lambda (x)
	   (if (member x seen)
	       (set! result (cons x result))
	       (set! seen (cons x seen))))
	 (fast-bintree-preorder tree))
	(reverse result)))

    (define (bst-nonempty-insert E B)
      "Insert element E into non-empty binary search tree B."
      (if (bintree-leaf? B)
	  (bst-leaf-insert E B)
	  (let ((this (bintree-node-element B))
		(left (bintree-node-left B))
		(right (bintree-node-right B)))
	    (if (<= E (bintree-node-element B))
		(make-bintree-node this
				   (bst-nonempty-insert E (bintree-node-left B))
				   right)
		(make-bintree-node this
				   left
				   (bst-nonempty-insert E (bintree-node-right B)))))))

    (define (bst-leaf-insert E L) ; This needs changing for Harel.
      "Insert element E into a binary search tree with only one leaf."
      (let ((this (bintree-leaf-element L)))
	(if (= E this)
	    L ; return yourself
	    (if (< E this)
		(make-bintree-node E ; Make E a node, and E the lesser leaf
				   (make-bintree-leaf E)
				   (make-bintree-leaf this)) ; Make
					; yourself the right leaf
		(make-bintree-node this ; else make yourself the node
				   (make-bintree-leaf this) ; ...and the
					; lesser leaf
				   (make-bintree-leaf E)))))) ; make E the
					; greater leaf

    ;; Removal

    (define (bst-remove E B)
      "Remove the element E from the binary search tree B."
      (if (bst-empty? B)
	  B
	  (if (bintree-leaf? B)
	      (bst-leaf-remove E B)
	      (bst-node-remove E B))))

    (define (bst-leaf-remove E L)
      "Remove element E from binary search tree leaf L."
      (if (= E (bintree-leaf-element L))
	  (make-empty-bst)
	  L))

    (define (bst-node-remove E N)
      "Remove node E from the binary search tree node N."
      (let ((this (bintree-node-element N))
	    (left (bintree-node-left N))
	    (right (bintree-node-right N)))
	(if (<= E this)
	    (if (bintree-leaf? left)
		(if (= E (bintree-leaf-element left))
		    right
		    N)
		(make-bintree-node this (bst-node-remove E left) right))
	    (if (bintree-leaf? right)
		(if (= E (bintree-leaf-element right))
		    left
		    N)
		(make-bintree-node this left (bst-node-remove E right))))))))