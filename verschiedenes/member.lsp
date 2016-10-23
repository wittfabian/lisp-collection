;MEMBER

(DEFUN MEMB (X LISTE)
(COND ((EQUAL X (CAR LISTE)) 'T)
	  ((NULL (CDR LISTE)) 'NIL)
	  (T (MEMB X (CDR LISTE)))
	)
)

;(MEMB 1 '(7 3 9 1 4));-> T
;(MEMB 10 '(7 3 9 1 4));-> NIL
;(MEMB 4 '(7 3 9 1 4));-> T