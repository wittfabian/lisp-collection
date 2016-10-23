(DEFUN KONJUNKTION (&REST LWL)
(COND ((NULL LWL) 1); NEUTRALER WERT DER KONJUNKTION
      ((LISTP (CAR LWL)) (* (LOG-INT (CAR LWL)) (APPLY 'KONJUNKTION (CDR LWL)))) 
      ((= (CAR LWL) 0) 0)
      (T (APPLY 'KONJUNKTION (CDR LWL)))))

;(KONJUNKTION);->1
;(KONJUNKTION 1);->1
;(KONJUNKTION 1 0);->0
;(KONJUNKTION 1 1 0 1);->0
;(KONJUNKTION 1 1 1 1 1);->1

(DEFUN DISJUNKTION (&REST LWL)
(COND ((NULL LWL) 0); NEUTRALER WERT DER DISJUNKTION
      ((LISTP (CAR LWL)) (APPLY 'DISJUNKTION (LIST (LOG-INT (CAR LWL)) (APPLY 'DISJUNKTION (CDR LWL)))))
      ((= (CAR LWL) 1) 1)
      (T (APPLY 'DISJUNKTION (CDR LWL)))))

;(DISJUNKTION);->0
;(DISJUNKTION 1);->1
;(DISJUNKTION 1 0);->1
;(DISJUNKTION 1 1 1);->1

(DEFUN ANTIVALENZ (&REST LWL) 
(COND ((NULL LWL) 0); NEUTRALER WERT DER ANTIVALENZ  
      ((LISTP (CAR LWL)) (APPLY 'ANTIVALENZ (CONS (LOG-INT (CAR LWL)) (CDR LWL))))
      ((= (CAR LWL) 0) (APPLY 'ANTIVALENZ (CDR LWL)))
      ((= (CAR LWL) 1) (- 1 (APPLY 'ANTIVALENZ (CDR LWL))))
      (T 'ERROR)))

;(ANTIVALENZ);->0
;(ANTIVALENZ 1);->1
;(ANTIVALENZ 1 1);->0
;(ANTIVALENZ 1 1 1);->1
;(ANTIVALENZ 1 1 1 1);->0
;(ANTIVALENZ 0 0 0 0);->0

(DEFUN NEGATION (EXPR) 
(COND ((LISTP EXPR) (NEGATION (LOG-INT EXPR))) 
      ((ATOM EXPR) (- 1 EXPR))))

;(NEGATION 1);->0
;(NEGATION 0);->1

(DEFUN IMPLIKATION (X Y)
(COND ((LISTP X) (IMPLIKATION (LOG-INT X) Y))
      ((LISTP Y) (IMPLIKATION X (LOG-INT Y)))
      ((= X Y) 1)
      ((= X 1) 0)
      ((= X 0) 1)
      (T 'ERROR)
))

;(IMPLIKATION 0 0);->1
;(IMPLIKATION 0 1);->1
;(IMPLIKATION 1 0);->0
;(IMPLIKATION 1 1);->1

(DEFUN NAND (&REST LWL) 
(COND ((LISTP (CAR LWL)) (APPLY 'NAND (CONS (LOG-INT (CAR LWL)) (CDR LWL))))
      (T (NEGATION (APPLY 'KONJUNKTION LWL)))))

;(NAND 0 0);->1
;(NAND 0 1);->1
;(NAND 1 0);->1
;(NAND 1 1);->0

(DEFUN NOR (&REST LWL) 
(COND ((LISTP (CAR LWL)) (APPLY 'NAND (CONS (LOG-INT (CAR LWL)) (CDR LWL))))
      (T (NEGATION (APPLY 'DISJUNKTION LWL)))))

;(NOR 0 0);->1
;(NOR 0 1);->0
;(NOR 1 0);->0
;(NOR 1 1);->0

(DEFUN LOG-INT (L-EXPR)
(CASE (CAR L-EXPR)
      (v (APPLY 'DISJUNKTION (CDR L-EXPR)))
      (& (APPLY 'KONJUNKTION (CDR L-EXPR)))
      (\# (APPLY 'ANTIVALENZ (CDR L-EXPR)))
      (-> (APPLY 'IMPLIKATION (CDR L-EXPR)))
      (! (APPLY 'NEGATION (CDR L-EXPR)))
      (^ (APPLY 'NAND (CDR L-EXPR)))
      (\| (APPLY 'NOR (CDR l-EXPR)))
))

;(LOG-INT '(v (& 1 0) (& 1 1)));-> 1
;(LOG-INT '(\# (& 1 0) (& 1 1)));-> 1
;(LOG-INT '(\# (& 1 1) (& 1 1)));-> 0
;(LOG-INT '(-> (& 1 0) (& 1 1)));-> 1
;(LOG-INT '(! (v (& 1 0) (& 1 1))));->0
;(LOG-INT '(^ (^ 0 0) (^ 0 0)));->0
;(LOG-INT '(! 0));->1


;(SORT '(Z X Y) 'STRING< :KEY 'SYMBOL-NAME)
(DEFUN EXTRAKT-VAR (EXPR OPZ-L &OPTIONAL (EXPR-AKKU NIL) (ERG-AKKU NIL))
(COND ((AND (NULL EXPR) (NULL EXPR-AKKU)) (SORT ERG-AKKU 'STRING< :KEY 'SYMBOL-NAME))
      ((NULL EXPR) (EXTRAKT-VAR EXPR-AKKU OPZ-L NIL ERG-AKKU))
      ((MEMBER (CAR EXPR) OPZ-L) (EXTRAKT-VAR (CDR EXPR) OPZ-L EXPR-AKKU ERG-AKKU))
      ((LISTP (CAR EXPR)) (EXTRAKT-VAR (CAR EXPR) OPZ-L (APPEND (CDR EXPR) EXPR-AKKU) ERG-AKKU)) 
      ((AND (ATOM (CAR EXPR)) (NOT (MEMBER (CAR EXPR) ERG-AKKU))) (EXTRAKT-VAR (CDR EXPR) OPZ-L EXPR-AKKU (APPEND ERG-AKKU (LIST (CAR EXPR)))))
      (T (EXTRAKT-VAR (CDR EXPR) OPZ-L EXPR-AKKU ERG-AKKU))))


;(EXTRAKT-VAR '(& X Y) '(v & \# ^ $ -> !));->(X Y)
;(EXTRAKT-VAR '(v (& X Y) (& X Z)) '(v & \# ^ $ -> !));-> (X Z Y)
;(EXTRAKT-VAR '(v (& Y X) (& X Z)) '(v & \# ^ $ -> !));-> (X Z Y)
;(EXTRAKT-VAR '(\# (& X Y) (& Z A)) '(v & \# ^ $ -> !));-> (A X Y Z)
;(EXTRAKT-VAR '(\# (& X X) (& X X)) '(v & \# ^ $ -> !));-> (X)
;(EXTRAKT-VAR '(-> (& X Y) (& Z A)) '(v & \# ^ $ -> !));-> (A X Y Z)
;(EXTRAKT-VAR '(! (v (& A B (& C D)) (& E F (& G H)))) '(v & \# ^ $ -> !));-> (A B C D E F G H)
;(EXTRAKT-VAR '(! (v (& H B (& E C)) (& D F (& G A)))) '(v & \# ^ $ -> !));-> (A B C D E F G H)
;(EXTRAKT-VAR '(! (v (& X Y) (& A B))) '(v & \# ^ $ -> !));->(A B X Y)
;(EXTRAKT-VAR '(^ (^ Y Y) (^ Y Y)) '(v & \# ^ $ -> !));->(Y)
;(EXTRAKT-VAR '($ Y Y) '(v & \# ^ $ -> !));->(Y)
;(EXTRAKT-VAR '($ ($ Y Y) ($ Y Y)) '(v & \# ^ $ -> !));->(Y)
;(EXTRAKT-VAR '(v (& X Y) (& X Z)) '(v & \# ^ $ -> !));-> (X Y Z)

;(DEFUN ASSO (VAR ASSLISTE)
;(COND ((NULL ASSLISTE) NIL)
;      ((EQUAL VAR (CAAR ASSLISTE)) (CAR ASSLISTE))
;      (T (ASSO VAR (CDR ASSLISTE)))
;))

;(DEFUN ASSLISTE (VARI KONS)
;(COND ((AND (NULL VARI) (NULL KONS)) NIL)
;      (T (CONS (CAR VARI) (CAR KONS)) (ASSLISTE (CDR VARI) (CDR KONS)))
;))

(DEFUN BAUM-SUBSTITUTION (BAUM SUB-L)
(COND ((NULL BAUM) NIL)
      ((AND (ATOM BAUM) (ASSOC BAUM SUB-L)) (CDR (ASSOC BAUM SUB-L)))
      ((ATOM BAUM) BAUM)
      (T (CONS (BAUM-SUBSTITUTION (CAR BAUM) SUB-L) (BAUM-SUBSTITUTION (CDR BAUM) SUB-L)))))

;(BAUM-SUBSTITUTION '(+ X (* Y Z)) '((X . 1) (Y . 2) (Z . 3)));-> (+ 1 (* 2 3))
;(BAUM-SUBSTITUTION '(v (& X Y) (& X Z)) '((X . 1) (Y . 0) (Z . 1))):-> (V (& 1 0) (& 1 1)) 
;(EVAL (BAUM-SUBSTITUTION '(+ X (* Y Z)) '((X . 1) (Y . 2) (Z . 3))));-> 7


(DEFUN B-INCR (B-VECTOR) (REVERSE (B-INCR-H (REVERSE B-VECTOR))))

(DEFUN B-INCR-H (REV-VEC)
(COND ((NULL REV-VEC) (LIST 1)); EXIT
      ((= 0 (CAR REV-VEC)) (CONS 1 (CDR REV-VEC)))
      (T (CONS 0 (B-INCR-H (CDR REV-VEC))))))

;(B-INCR '(0 0 0)); -> (0 0 1) 
;(B-INCR '(1 1 1)); -> (1 0 0 0) 


;(DEFUN PAAR (Z B &OPTIONAL (ERG-AKKU NIL))
;(COND ((NULL Z) ERG-AKKU)
;      (T (PAAR (CDR Z) (CDR B) (APPEND ERG-AKKU (LIST (CONS (CAR B) (CAR Z))))))))
(DEFUN PAAR (Z B &OPTIONAL (ERG-AKKU NIL))
(MAPCAR #'(LAMBDA (X Y) (CONS X Y)) B Z))

;(PAAR '(1 2 3 4) '(A B C D));-> '((A . 1) (B . 2) (C . 3) (D . 4))
;(PAAR '(1 0 1) '(X Y Z));->'((X . 1) (Y . 0) (Z . 1))

(DEFUN PRINT-TABLE (ERG VAR)
(FRESH-LINE) (FORMAT T "~S | F~S" VAR VAR)
(FRESH-LINE) (FORMAT T "---------------------")
(FRESH-LINE)
(MAPCAR #'(LAMBDA (E) (FORMAT T "~S | ~S" (CAR E) (CDR E)) (FRESH-LINE)) ERG))


;(DEFUN ERG (L-EXPR OPZ-L PAR &OPTIONAL (ERG-AKKU NIL))
;(SETF ERG-LOG-INT (LOG-INT (BAUM-SUBSTITUTION L-EXPR (PAAR PAR (EXTRAKT-VAR L-EXPR OPZ-L)))))
;(COND ((> (LENGTH PAR) (LENGTH (EXTRAKT-VAR L-EXPR OPZ-L))) ERG-AKKU)
;      (T (ERG L-EXPR OPZ-L (B-INCR PAR) (APPEND ERG-AKKU (LIST (CONS PAR ERG-LOG-INT)))))))

(DEFUN ERG (L-EXPR OPZ-L PAR &OPTIONAL (ERG-AKKU NIL))
(LET ((ERG-LOG-INT (LOG-INT (BAUM-SUBSTITUTION L-EXPR (PAAR PAR (EXTRAKT-VAR L-EXPR OPZ-L))))))

(COND ((> (LENGTH PAR) (LENGTH (EXTRAKT-VAR L-EXPR OPZ-L))) ERG-AKKU)
      (T (ERG L-EXPR OPZ-L (B-INCR PAR) (APPEND ERG-AKKU (LIST (CONS PAR ERG-LOG-INT))))))
))


(DEFUN LOG-ANALYSE (L-EXPR OPZ-L)
(LET* ((Z (MAKE-LIST (LENGTH (EXTRAKT-VAR L-EXPR OPZ-L)):INITIAL-ELEMENT 0))
      (TEST (MAPCAR 'CDR (ERG L-EXPR OPZ-L Z))))

(PRINT-TABLE (ERG L-EXPR OPZ-L Z) (EXTRAKT-VAR L-EXPR OPZ-L))

(COND ((NOT (MEMBER '1 TEST)) 'WIDERSPRUCH)
      ((NOT (MEMBER '0 TEST)) 'TAUTOLOGIE)
      (T (MAPCAN #'(LAMBDA (L) (WHEN (= 1 (CDR L)) (LIST L))) (ERG L-EXPR OPZ-L Z))))
))

(LOG-ANALYSE '(v (& X Y) (& X Z)) '(v & \# ^ $ -> !));->
; (X Z Y) | F(X Z Y)
; --------+---
; (0 0 0) | 0
; (0 0 1) | 0
; (0 1 0) | 0
; (0 1 1) | 0
; (1 0 0) | 0
; (1 0 1) | 1
; (1 1 0) | 1
; (1 1 1) | 1
;(((1 0 1) . 1) ((1 1 0) . 1) ((1 1 1) . 1))

(LOG-ANALYSE '(& X (! X) Y Z) '(v & \# ^ $ -> !));->
; (Z Y X) | F(Z Y X)
; --------+---
; (0 0 0) | 0
; (0 0 1) | 0
; (0 1 0) | 0
; (0 1 1) | 0
; (1 0 0) | 0
; (1 0 1) | 0
; (1 1 0) | 0
; (1 1 1) | 0
;WIDERSPRUCH 

(LOG-ANALYSE '(v (& X Y) (& (! X) Y)  (& X (! Y)) (& (! X) (! Y))) '(v & \# ^ $ -> !));->
; (Y X) | F(Y X)
; --------+---
; (0 0) | 1
; (0 1) | 1
; (1 0) | 1
; (1 1) | 1
;TAUTOLOGIE 

(LOG-ANALYSE '(-> X X)  '(v & \# ^ $ -> !));->
; (X) | F(X)
; --------+---
; (0) | 1
; (1) | 1
;TAUTOLOGIE

(LOG-ANALYSE '(-> (-> X Y) Z)  '(v & \# ^ $ -> !));->
; (X Z Y) | F(X Z Y)
; --------+---
; (0 0 0) | 0
; (0 0 1) | 0
; (0 1 0) | 1
; (0 1 1) | 1
; (1 0 0) | 1
; (1 0 1) | 0
; (1 1 0) | 1
; (1 1 1) | 1
;(((0 1 0) . 1) ((0 1 1) . 1) ((1 0 0) . 1) ((1 1 0) . 1) ((1 1 1) . 1))


(DEFUN AUTOTEST (FKT TFL)
(LET ((TESTERG (REMOVE T (MAPCAR #'(LAMBDA (TESTFALL) 
                                     (IF (EQUAL (CDR TESTFALL) (APPLY FKT (CAR TESTFALL))) T (CONS (CDR TESTFALL) (APPLY FKT (CAR TESTFALL))))) TFL))))

(IF (NULL TESTERG) 'OK TESTERG)))

(AUTOTEST 'LOG-ANALYSE '(
( ((-> X X) (v & \# ^ $ -> !)) . TAUTOLOGIE );OK

( ((& X (! X) Y Z) (v & \# ^ $ -> !)) . WIDERSPRUCH );OK

( ((v (& X Y) (& (! X) Y)  (& X (! Y)) (& (! X) (! Y))) (v & \# ^ $ -> !)) . TAUTOLOGIE );OK

( ((v (& X Y) (& X Z)) (v & \# ^ $ -> !)) . (((1 0 1) . 1) ((1 1 0) . 1) ((1 1 1) . 1)) );OK
))









