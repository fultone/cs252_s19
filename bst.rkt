; ------------------------- PART 1 -------------------------

; Returns true if given tree is a valid BST, meaning its length is 3,
; the first item is a number, and the second two items are lists, otherwise returns false.
(define valid-tree?
  (lambda (bst)
    (if
      ; 
       (and (list? bst)
            (equal? (length bst) 3)
            (number? (car bst))
            (list? (car (cdr bst)))
            (list? (car (cdr (cdr bst))))) #t
                                           #f)))

; Returns the root of the tree if it is a valid bst
(define entry
  (lambda (bst)
    (if (valid-tree? bst) (car bst)
        #f)))

; Returns the left subtree (of the root) if the tree is valid
(define left
  (lambda (bst)
    (if (valid-tree? bst) (car (cdr bst))
        #f)))

; Returns the right subtree (of the root) if the tree is valid
(define right
  (lambda (bst)
    (if (valid-tree? bst) (car (cdr (cdr bst)))
        #f)))

; Input: root, left subtree, right subtree
; Return: a new BST given the input, or return False if the input would not yeild a valid BST
(define make-bst
  (lambda (elt left right)
    ; check if elt is a number, then if (left is empty or if it is a valid tree) (same for right)
    (if
     (and (number? elt)
          (or (null? left) (valid-tree? left))
          (or (null? right) (valid-tree? right))) (list elt left right)
                                                #f)))

; Return a list containing all values in bst in the order obtained from a preorder traversal
(define preorder
  (lambda (bst)
    ;; assuming bst is a valid tree
    (if (null? bst) '()
        (append (list (car bst)) (preorder (car (cdr bst))) (preorder (car (cdr (cdr bst))))))))

; Return a list containing all values in bst in the order obtained from an inorder traversal
(define inorder
  (lambda (bst)
    ;; assuming bst is a valid tree
    (if (null? bst) '()
        (append (inorder (car (cdr bst))) (list (car bst)) (inorder (car (cdr (cdr bst))))))))

; Return a list containing all values in bst in the order obtained from a postorder traversal
(define postorder
  (lambda (bst)
    ;; assuming bst is a valid tree
    (if (null? bst) '()
        (append (postorder (car (cdr bst))) (postorder (car (cdr (cdr bst)))) (list (car bst))))))

; Given a new number v and an existing bst, return a new bst, containing new leaf "v"
(define insert
  (lambda (v bst)
    (cond [(null? bst) (list v '() '())]
          [(equal? (car bst) v) bst]
          [(< v (car bst)) (list (car bst) (insert v (car (cdr bst))) (car (cdr (cdr bst))))]
          [(> v (car bst)) (list (car bst) (car (cdr bst)) (insert v (car (cdr (cdr bst)))))])))
        
; ------------------------- BONUS -------------------------

; Returns a bst obtained by inserting the integers in a given list one-by-one,
; from the left of the list, into an initially empty binary search tree
(define bst-from-list
  (lambda (lst)
    (if (null? lst) '()
        (insert (car lst) (bst-from-list (cdr lst))))))

; Returns true if the tree is well-formed (ie adheres to the definition of bst, where left children
; are > their parents, and right children are < their parents), and false otherwise
(define proper-tree?
  (lambda (bst)
    (cond [(null? bst) #t]
          [(not (list? bst)) #f]
          [(not (number? (car bst))) #f]

          [(and (valid-tree? bst)
                (or (null? (car (cdr bst))) ; left child
                    (< (car (car (cdr bst))) (car bst)))
                (or (null? (car (cdr (cdr bst))))
                    (> (car (car (cdr (cdr bst)))) (car bst))) ; right child
                (proper-tree? (car (cdr bst)))
                (proper-tree? (car (cdr (cdr bst))))) #t]
          [else #f])))

; ------------------------- Test Area -------------------------
(define test
  (lambda (a b)
    (if (equal? a b) 'pass
        'FAIL)))

(define tree1 '(2 (4) ()))
(define tree2 '(8 (3 (1 () ()) (6 () ()) ) (10 () ())))
(define tree3 'e)
(define tree4 '(7 (5 (3 (1 () ()) (4 () ())) (6 () ())) (12 (9 (8 () ()) (10 () ())) (15 (13 () ()) (17 () ())))))
(define tree5 '())

(test (entry tree1) 2)
(test (entry tree2) 8)
(test (entry tree3) #f)

;(make-bst 5 '() '())
;(make-bst 5 '(4) '())
;(make-bst 5 '(3 () ()) '())
;(make-bst '8 '(2()()) '())


;(preorder tree2)
;(inorder tree2)
;(postorder tree2)

;(insert 8.5 tree5)
;(bst-from-list '(4 9 2 14 6 -3 0))

(proper-tree? tree1)
(proper-tree? tree2)
(proper-tree? tree3)
(proper-tree? tree4)
(proper-tree? tree5)

