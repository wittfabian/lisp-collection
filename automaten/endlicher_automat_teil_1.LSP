(SETF TACC1 '(((0 . a) . 1) ((0 . b) . 2)
	      ((1 . a) . 3) ((1 . b) . 2)
	      ((2 . a) . 1) ((2 . b) . 4)
	      ((3 . a) . 5) ((3 . b) . 2)
	      ((4 . a) . 1) ((4 . b) . 5)
	      ((5 . a) . 5) ((5 . b) . 5)
	     )
)


(SETF AACC1 '((0 . ACCEPT) 
	      (1 . ACCEPT)
	      (2 . ACCEPT)
	      (3 . ACCEPT)
	      (4 . ACCEPT)
	      (5 . NOACC)
	     )
)

(DEFUN Z1-SUCHE (Z X L)
(COND ((NULL L) 'ERRORTACC)
      ((EQUAL (CONS Z X) (CAAR L)) (CDAR L))
      (T (Z1-SUCHE Z X (CDR L)))
      )

)


;(Z1-SUCHE 1 'B TACC1);-> 2


(DEFUN A-SUCHE (Z L)
(COND ((NULL L) 'ERRORAACC)
      ((EQUAL Z (CAAR L)) (CDAR L))
      (T (A-SUCHE Z (CDR L)))
      )
)


;(A-SUCHE 1 AACC1);-> ACCEPT



(DEFUN ACCEPTOR-S (XL Z0 TACC AACC);-> {ACCEPT, NOACC}
(COND ((NULL XL) (A-SUCHE Z0 AACC))
      (T (ACCEPTOR-S (CDR XL) (Z1-SUCHE Z0 (CAR XL) TACC) TACC AACC))
      )
)


(ACCEPTOR-S '(A B A A B B) 0 TACC1 AACC1);-> ACCEPT
(ACCEPTOR-S '(A B A A B B B) 0 TACC1 AACC1);-> NOACC
