;umkehr
(DEFUN UMKEHR (L AKKU) 
	(COND ((NULL L) AKKU)
		(T (UMKEHR (CDR L) (CONS (CAR L) AKKU)))
	)
)

(UMKEHR '(A B C) NIL);-> (C B A)