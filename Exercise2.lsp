;Return the level of a node X in a tree of type (2). The level of the root element is 0.

;getLevel(x, l1l2..ln, r) = nil, if n=0
;                           r, if l1=x
;                           getLevel(x, l2, r+1) or getLevel(x, l3, r+1), otherwise
;getLevel(x: integer, l: list, r: integer)
(defun getLevel (x l r)
	(cond
		((null (car l)) nil)
		((equal (car l) x) r)
		(T (or (getLevel x (cadr l) (+ 1 r)) (getLevel x (caddr l) (+ 1 r))))
	)
)

(defun test_getLevel ()
	(assert (equal (getlevel 1 '(1 (2) (3 (4) (5))) 0) 0))
	(assert (equal (getlevel 2 '(1 (2) (3 (4) (5))) 0) 1))
	(assert (equal (getlevel 3 '(1 (2) (3 (4) (5))) 0) 1))
	(assert (equal (getlevel 4 '(1 (2) (3 (4) (5))) 0) 2))
	(assert (equal (getlevel 5 '(1 (2) (3 (4) (5))) 0) 2))
)

(test_getLevel)
