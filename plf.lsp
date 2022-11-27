;a)Write a function to eliminate the n-th element of a linear list.

;eliminateNthElement(l1l2..ln, p) = (), if n=0
;                                   l2l3..ln, if p=1
;                                   {l1} U eliminateNthElement(l2l3..ln, p-1), otherwise
;eliminateNthElement(l: list, n: integer)
(defun eliminateNthElement (l n)
	(cond
		((null l) nil)
		((= n 1) (cdr l))
		(T (cons (car l) (eliminateNthElement (cdr l) (- n 1))))
	)
)



;b)Write a function to determine the successor of a number represented digit by digit 
;as a list, without transforming the representation of the number from list to number. 
;Example: (1 9 3 5 9 9) --> (1 9 3 6 0 0)

;reverseDigits(l1l2..ln, r) = r, if n=0
;                             reverseDigits(l2l3..ln, {l1} U r), otherwise
;reverseDigits(l: list, r: list) 
(defun reverseDigits (l r)
	(cond
		((null l) r)
		(T (reverseDigits (cdr l) (cons (car l) r)))
	)
)

;succesorDigits(l1l2..ln) = {0} U succesorDigits(l2l3..ln), if l1=9 and n!=1
;                           {0} U {1}, if l1=9 and n=1
;                           {l1+1} U l2l3..ln, otherwise
;succesorDigits(l: list)
(defun succesorDigits (l)
	(cond
		((and (equal 9 (car l)) (not (equal nil (cdr l)))) (cons 0 (succesorDigits (cdr l))))
		((equal 9 (car l)) (cons 0 (cons 1 nil)))
		(T (cons (+ 1 (car l)) (cdr l)))
	)
)

(defun succesorNumber (l)
    (reverseDigits (succesorDigits (reverseDigits l nil)) nil)
)



;c)Write a function to return the set of all the atoms of a list.
;Example: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)

;getAtoms(l1l2..ln, r1r2..rm) = r1r2..rm, if n=0
;                               getAtoms(l1, r1r2..rm), if l1 is list
;                               getAtoms(l2l3..ln, {l1} U r1r2..rm), if l1 is atom
;getAtoms(l: list, r: list)
(defun getAtoms (l r)
	(cond
		((null l) r)
		((listp (car l)) (getAtoms (car l) r))
		((atom (car l)) (getAtoms (cdr l) (cons (car l) r)))
	)
)

;containsElement(l1l2..ln, e) = false, if n=0
;                               true, if l1=e
;                               containsElement(l2l3..ln, e), otherwise
;containsElement(l: list, e: integer)
(defun containsElement (l e)
    (cond
        ((null l) nil)
        ((equal (car l) e) T)
        (T (containsElement (cdr l) e))
    )
)

;getUnique(l1l2..ln, r1r2..rm) = r1r2..rm, if n=0
;                                getUnique(l2l3..ln, r1r2..rm), if containsElement(l1, r1r2..rm)
;                                getUnique(l2l3..ln, {l1} U r1r2..rm), otherwise
;getUnique(l: list, r: list)
(defun getUnique (l r)
    (cond
        ((null l) r)
        ((containsElement r (car l)) (getUnique (cdr l) r))
        (T (getUnique (cdr l) (cons (car l) r)))
    )
)

(defun getAtomsSet (l)
    (getUnique (getAtoms l '()) '())
)



;d)Write a function to test whether a linear list is a set.

;testIfSet(l1l2..ln) = true, if n=0
;                      false, if containsElement(l2l3..ln, l1)
;                      testIfSet(l2l3..ln), otherwise
;testIfSet(l: list)
(defun testIfSet (l)
	(cond
		((null l) T)
		((containsElement (cdr l) (car l)) nil)
		(T (testIfSet (cdr l)))
	)
)



;Tests

(defun test_eliminateNthElement ()
	(assert (equal (eliminateNthElement '() 3)  nil))
	(assert (equal (eliminateNthElement '(5) 1)  nil))
	(assert (equal (eliminateNthElement '(1 2 3 3 4 5) 3)  '(1 2 3 4 5)))
)

(defun test_succesorNumber ()
	(assert (equal (succesorNumber '(0))  '(1)))
	(assert (equal (succesorNumber '(9 9))  '(1 0 0)))
	(assert (equal (succesorNumber '(1 9 3 5 9 9))  '(1 9 3 6 0 0)))
)

(defun test_getAtomsSet ()
	(assert (equal (getAtomsSet '())  '()))
	(assert (equal (getAtomsSet '(1))  '(1)))
	(assert (equal (getAtomsSet '(1 (2 (1 3 (2 4) 3) 1) (1 4)))  '(1 3 2 4)))
)

(defun test_testIfSet ()
	(assert (equal (testIfSet '())  T))
	(assert (equal (testIfSet '(3 1 2))  T))
	(assert (equal (testIfSet '(3 1 2 3))  nil))
)

(defun test_all ()
	(test_eliminateNthElement)
	(test_succesorNumber)
	(test_getAtomsSet)
	(test_testIfSet)
)

(test_all)
