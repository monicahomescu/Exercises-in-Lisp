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

(defun test ()
	(assert (equal (getDepthTree '(a (b (c)) (d) (e (f))))  3))
	(assert (equal (getDepthTree '(A (B) (C (D) (E))))  3))
	(assert (equal (getDepthTree '(A (B (F)) (C (D) (E))))  3))
	(assert (equal (getDepthTree '(1 (2) (3))) 2))
	(assert (equal (getDepthTree '(1 (2) (3 (4) (5 (6))) (7))) 4))
)

(defun test2 ()
	(assert (equal (extra '(a (b (c)) (d) (e (f))) e)  2))
)

(test)





(defun f (l x fg)
  (cond
    ((null l) 0)
    ((atom l) 0)
    ((equal (car l) x) (+ 1 (apply #'max (mapcar (lambda (r) (f r x T)) l))))
    (fg (+ 1 (apply #'max (mapcar (lambda (r) (f r x T)) l))))
    (T (apply #'max (mapcar (lambda (r) (f r x fg)) l)))
  )
)

(defun test ()
	(assert (equal (f '(a (b (c)) (d) (e (f))) 'b NIL)  2))
	(assert (equal (f '(A (B) (C (D) (E))) 'E NIL)  1))
	(assert (equal (f '(A (B (F)) (C (D) (E))) 'A NIL)  3))
	(assert (equal (f '(1 (2) (3)) 1 NIL) 2))
	(assert (equal (f '(1 (2) (3 (4) (5 (6))) (7)) 1 NIL) 4))
)

(test)






;(defun getDepthTree (l)
;  (cond
;	((atom l) 0)
;    ((null (cdr l)) 0)
;    (T (+ 1 (apply #'max (mapcar #'getDepthTree l))))
;  )
;)