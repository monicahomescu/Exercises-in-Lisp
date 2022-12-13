;Define a function that returns the depth of a tree represented 
;as (root list_of_nodes_subtree1 ... list_of_nodes_subtreen).
;Eg. the depth of the tree (a (b (c)) (d) (e (f))) is 3

;getDepthTree(l1l2..ln) = 0, if l1 is atom or n=0
;                         1 + max(getDepthTree(l1) U ... U getDepthTree(ln))
;getDepthTree(l: list)
(defun getDepthTree (l)
  (cond
    ((or (atom l) (null l)) 0)
    (T (+ 1 (apply #'max (mapcar #'getDepthTree l))))
  )
)

;test
(defun test ()
	(assert (equal (getDepthTree '(a (b (c)) (d) (e (f))))  3))
	(assert (equal (getDepthTree '(A (B) (C (D) (E))))  3))
	(assert (equal (getDepthTree '(A (B (F)) (C (D) (E))))  3))
	(assert (equal (getDepthTree '(1 (2) (3))) 2))
	(assert (equal (getDepthTree '(1 (2) (3 (4) (5 (6))) (7))) 4))
)

(test)
