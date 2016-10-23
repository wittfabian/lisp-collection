;von unaer zu 4-adische Zahl 

(SETF TAB '( ( I . 1 ) ( (0 I) . 0 ) ( (1 I) . 2 )	( (2 I) . 3 )	( (3 I) . (I 0) ) ))

(DEFUN SUCHE (EING STAB)
;(FORMAT T "~%; ~A" EING)
(COND ((NULL EING) NIL)
	  ((NULL STAB) NIL)
	  ((EQUAL (CAAR STAB) EING) (CAAR STAB))
	  (T (SUCHE EING (CDR STAB)))
	)
)
;(SUCHE '() TAB);->NIL
;(SUCHE 'I TAB);-> I
;(SUCHE 2 TAB);-> NIL
;(SUCHE '(2 I) TAB);-> (2 I)
;(SUCHE '(1 I) TAB);-> (1 I)

(DEFUN RUECK (EING STAB)
;(FORMAT T "~%; ~A" EING)
(COND ((NULL EING) NIL)
	  ((NULL STAB) NIL)
	  ((EQUAL (CAAR STAB) EING) (CDAR STAB))
	  (T (RUECK EING (CDR STAB)))
	)
)
;(RUECK '() TAB);->NIL
;(RUECK 'I TAB);-> 1
;(RUECK 2 TAB);-> NIL
;(RUECK '(2 I) TAB);-> 3
;(RUECK '(1 I) TAB);-> 2

(DEFUN UMW (ZAHL STAB)
(FORMAT T "~%; ~A" ZAHL)
(COND ((NULL ZAHL) NIL)
	  ((EQUAL (CAR ZAHL) (SUCHE (CAR ZAHL) STAB)) (UMW (CONS (RUECK (CAR ZAHL) STAB) (CDR ZAHL)) STAB))
	  ((EQUAL (LIST (CAR ZAHL) (CADR ZAHL))  (SUCHE (LIST (CAR ZAHL) (CADR ZAHL))  TAB)) (UMW (CONS (RUECK (LIST (CAR ZAHL) (CADR ZAHL)) STAB) (CDDR ZAHL)) STAB))
	  (T (CAR ZAHL))
	)
) 
;(UMW '(I I I I I I I) TAB);-> 13
;(UMW '(I) TAB);-> 1
;(UMW '(I I) TAB);-> 2
;(UMW '(I I I) TAB);-> 2
;(UMW '() TAB);-> NIL