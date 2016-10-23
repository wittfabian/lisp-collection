;ueberpruefung auch MONOTON STEIGENDE ZAHLENLIST

(DEFUN MONSTEI (XL)
(COND ((NULL (CDR XL)) 'T)
	  ((< (CAR XL) (CADR XL)) (MONSTEI (CDR XL)))
	  (T 'NIL)
	)
)
(MONSTEI '(1 2 3 7));-> T
(MONSTEI '(1 2 3 10 34 57));-> T
(MONSTEI '(1 2 7 5));-> NIL
(MONSTEI '(1 2 4 5 6 8 2 4));-> NIL
(MONSTEI '(10 2 7 5));-> NIL