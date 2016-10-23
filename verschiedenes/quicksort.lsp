;Ausgabe
(defun AUSG (L1 E L2)
	(print `(,L1 ,E ,L2))
)

;listen zusammenfuegen
(DEFUN BIND (L1 L2)
	(COND ((NULL (CAR L2)) L1)
		(T (CONS (CAR L2) L1))
	)
)


;part funktion
(DEFUN PART (L E &OPTIONAL (L1 NIL) (L2 NIL))
	;(COND ((NULL (CAR L)) (AUSG L1 E L2))
	 (COND ((NULL (CAR L)) (BIND (CONS E L1) L2))
		((< E (CAR L)) (CONS (CAR L) L2) (PART (CDR L) E L1 (CONS (CAR L) L2)))
		(T (PART (CDR L) E (CONS (CAR L) L1) L2))
	)
)

(PART '(1 5 3 7 9 2) 4); -> ((1 3 2) 5 7 9)

;quicksort funktion
(DEFUN QUICKSORT (L)
	(COND ((NULL (CAR L)) 'KeineListe)
		(T (QUICKSORT (PART (CDR L) (CAR L))))
	
	

	)
)

(QUICKSORT '(3 2 5 1 4)); -> (1 2 3 4 5)