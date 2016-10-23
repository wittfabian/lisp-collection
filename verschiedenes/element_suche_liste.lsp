; Suche eines Elementes in einer ungeordneten Liste
(DEFUN ELRE (E L); => {T, NIL}
	(COND ((NULL L) NIL)
      ((EQUAL E (CAR L)) T)
      (T (ELRE E (CDR L)))
	)
)
;------------------------------------------------------------------------------------
(ELRE 'C '(A B C D));-> T
(ELRE 'C '());-> NIL
(ELRE 'X '(A B C D));-> NIL
(ELRE '(A . 1) '((A . 1) (B . 2) (C . 3) (D . 4)));-> T