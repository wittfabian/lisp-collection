;***********************************************************************************************
;  programm zur ableitung und vereinfachung von funktionen
;***********************************************************************************************
;-----------------------------------------------------------------------------------------------
;  prüfung, ob variable in term enthalten ist
;-----------------------------------------------------------------------------------------------
(defun enth (x e)
  (cond
    ((null e) nil)
    ((and (atom e) (eql x e)) t)
    ((listp e) (or (enth x (car e)) (enth x (cdr e))))
  )
)
;-----------------------------------------------------------------------------------------------
(enth 'u '(u)) ;->T
(enth 'u '(a b u c)) ;->T
(enth 'u '(a (u) b)) ;->T
(enth 'u '(a b ((c d) (x () z)) b)) ;->NIL
(enth 'u '(a b ((c d) (x (u) z)) b)) ;->T
(enth 'u '(a b ((c d) (x z)) u)) ;->T
;-----------------------------------------------------------------------------------------------
;  differentationsfunktion
;-----------------------------------------------------------------------------------------------
(defun diff (e x)
  (cond 
    ((and (atom e) (not (eql e x))) 0)
    ((and (atom e) (eql e x)) 1)
    ((eql (length e) 2)
       (case (car e)
			(+ (cond
				((and (atom (cadr e)) (not (equal x (cadr e)))) 0)
				((and (atom (cadr e)) (equal x (cadr e))) 1)
				((enth x (cadr e)) (diff (cadr e) x))
				)
			 )
			(- (cond
				((and (atom (cadr e)) (not (equal x (cadr e)))) 0)
				((and (atom (cadr e)) (equal x (cadr e))) -1)
				((enth x (cadr e)) (- (diff (cadr e) x)))
				)
			)
			(* (cond
				((and (atom (cadr e)) (not (equal x (cadr e)))) 0)
				((and (atom (cadr e)) (equal x (cadr e))) 1)
				((enth x (cadr e)) (diff (cadr e) x))
				)
			)
			(/ (cond
				((and (atom (cadr e)) (equal 0 (cadr e))) 'DIVISIONSFEHLER)
				((and (atom (cadr e)) (not (equal x (cadr e)))) 0)
				((and (atom (cadr e)) (equal x (cadr e))) `(- (/ (expt ,x 2))))
				((enth x (cadr e)) `(- (/ ,(diff (cadr e) x) (expt ,(cadr e) 2))))
			   )  
                        )
			(sin (cond
				  ((and (atom (cadr e)) (not (equal x (cadr e)))) 0) 
				  ((and (atom (cadr e)) (equal x (cadr e))) `(cos ,x))
				  ((enth x (cadr e)) `(* (cos ,(cadr e)) ,(diff (cadr e) x)))
				 )
			)
			(cos (cond
				  ((and (atom (cadr e)) (not (equal x (cadr e)))) 0) 
				  ((and (atom (cadr e)) (equal x (cadr e))) `(- (sin ,x)))
				  ((enth x (cadr e)) `(- (* (sin ,(cadr e)) ,(diff (cadr e) x))))
				 )
			)
			(tan (cond
				  ((and (atom (cadr e)) (not (equal x (cadr e)))) 0) 
				  ((and (atom (cadr e)) (equal x (cadr e))) `(/ (expt (cos ,x) 2)))
				  ((enth x (cadr e)) `(/ ,(diff (cadr e) x) (expt (cos ,(cadr e)) 2)))
				 )
			)
			(cot (cond
				  ((and (atom (cadr e)) (not (equal x (cadr e)))) 0) 
				  ((and (atom (cadr e)) (equal x (cadr e))) `(- (/ (expt (sin ,x) 2))))
				  ((enth x (cadr e)) `(- (/ ,(diff (cadr e) x) (expt (sin ,(cadr e)) 2))))
				 )
			)
			(exp (cond
				  ((and (atom (cadr e)) (not (equal x (cadr e)))) 0) 
				  ((and (atom (cadr e)) (equal x (cadr e))) `(exp ,x))
				  ((enth x (cadr e)) `(* (exp ,(cadr e)) ,(diff (cadr e) x)))
				 )
			)
        )
    )
	((eql (length e) 3)
		(case (car e)
			(+ (cond
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (not (equal x (caddr e)))) 0)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (not (equal x (caddr e)))) 1)
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e))) 1)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (equal x (caddr e))) 2)
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e)))) (diff (caddr e) x))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e)))) (diff (cadr e) x))
				((and (atom (cadr e)) (enth x (caddr e)) (equal x (cadr e))) `(+ 1 ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (atom (caddr e)) (equal x (caddr e))) `(+ ,(diff (cadr e) x) 1))
				((and (enth x (cadr e)) (enth x (caddr e))) `(+ ,(diff (cadr e) x) ,(diff (caddr e) x)))
				((and (not (enth x (cadr e))) (enth x (caddr e))) (diff (caddr e) x))
				((and (enth x (cadr e)) (not (enth x (caddr e)))) (diff (cadr e) x))
				((and (not (enth x (cadr e))) (not (enth x (caddr e)))) 0)
				)
			 )
			 (- (cond
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (not (equal x (caddr e)))) 0)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (not (equal x (caddr e)))) 1)
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e))) -1)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (equal x (caddr e))) 0)
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e)))) `(- ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e)))) (diff (cadr e) x))
				((and (atom (cadr e)) (enth x (caddr e)) (equal x (cadr e))) `(- 1 ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (atom (caddr e)) (equal x (caddr e))) `(- ,(diff (cadr e) x) 1))
				((and (enth x (cadr e)) (enth x (caddr e))) `(- ,(diff (cadr e) x) ,(diff (caddr e) x)))
				((and (not (enth x (cadr e))) (enth x (caddr e))) `(- ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (not (enth x (caddr e)))) (diff (cadr e) x))
				((and (not (enth x (cadr e))) (not (enth x (caddr e)))) 0)
				)
			 )
			 (* (cond
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (not (equal x (caddr e)))) 0)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (not (equal x (caddr e)))) (caddr e))
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e))) (cadr e))
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (equal x (caddr e))) `(* 2 ,x))
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e)))) `(* ,(cadr e) ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e)))) `(* ,(diff (cadr e) x) ,(caddr e)))
				((and (atom (cadr e)) (enth x (caddr e)) (equal x (cadr e))) `(+ ,(caddr e) (* ,(cadr e) ,(diff (caddr e) x))))
				((and (enth x (cadr e)) (atom (caddr e)) (equal x (caddr e))) `(+ ,(cadr e) (* ,(caddr e) ,(diff (cadr e) x))))
				((and (enth x (cadr e)) (enth x (caddr e))) `(+ (* ,(caddr e) ,(diff (cadr e) x)) (* ,(cadr e) ,(diff (caddr e) x))))
				((and (not (enth x (cadr e))) (enth x (caddr e))) `(* ,(cadr e) ,(diff (caddr e) x)))
				((and (enth x (cadr e)) (not (enth x (caddr e)))) `(* ,(caddr e) ,(diff (cadr e) x)))
				((and (not (enth x (cadr e))) (not (enth x (caddr e)))) 0)
				)
			 )
			 (/ (cond
			    ((and (atom (caddr e)) (equal 0 (caddr e))) 'DIVISIONSFEHLER)
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (not (equal x (caddr e)))) 0)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (not (equal x (caddr e)))) `(/ 1 ,(caddr e)))
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e))) `(/ (- ,(cadr e)) (expt ,x 2)))
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (equal x (caddr e))) 0)
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e)))) `(- (/ (* ,(cadr e) ,(diff (caddr e) x)) (expt ,(caddr e) 2))))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e)))) `(/ ,(diff (cadr e) x) ,(caddr e)))
				((and (atom (cadr e)) (enth x (caddr e)) (equal x (cadr e))) `(/ (- ,(caddr e) (* ,(cadr e) ,(diff (caddr e) x))) (expt ,(caddr e) 2)))
				((and (enth x (cadr e)) (atom (caddr e)) (equal x (caddr e))) `(/ (- (* ,(caddr e) ,(diff (cadr e) x) ,(cadr e))) (expt ,(caddr e) 2)))
				((and (enth x (cadr e)) (enth x (caddr e))) `(/ (- (* ,(caddr e) ,(diff (cadr e) x)) (* ,(diff (caddr e) x) ,(cadr e))) (expt ,(caddr e) 2)))
				((and (not (enth x (cadr e))) (enth x (caddr e))) `(- (/ (* ,(cadr e) ,(diff (caddr e) x)) (expt ,(caddr e) 2))))
				((and (enth x (cadr e)) (not (enth x (caddr e)))) `(/ ,(diff (cadr e) x) ,(caddr e)))
				((and (not (enth x (cadr e))) (not (enth x (caddr e)))) 0)
				
				)
			 )
			 (expt (cond
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (not (equal x (caddr e)))) 0)
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (not (equal x (caddr e)))) `(* ,(caddr e) (expt ,(cadr e) ,(- (caddr e) 1))))
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e)) (numberp (cadr e))) `(* ,(log (cadr e)) (expt ,(cadr e) ,(caddr e))))
				((and (atom (cadr e)) (atom (caddr e)) (not (equal x (cadr e))) (equal x (caddr e)) (symbolp (cadr e))) `(* (log ,(cadr e)) (expt ,(cadr e) ,(caddr e))))
				((and (atom (cadr e)) (atom (caddr e)) (equal x (cadr e)) (equal x (caddr e))) `(* (+ 1 (log ,(caddr e))) (expt ,(cadr e) ,(caddr e))))
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e))) (numberp (cadr e))) `(* ,(log (cadr e)) ,(diff (caddr e) x) (expt ,(cadr e) ,(caddr e))))
				((and (atom (cadr e)) (enth x (caddr e)) (not (equal x (cadr e))) (symbolp (cadr e))) `(* (log ,(cadr e)) ,(diff (caddr e) x) (expt ,(cadr e) ,(caddr e))))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e))) (numberp (caddr e))) `(* ,(caddr e) ,(diff (cadr e) x) (expt ,(cadr e) ,(- (caddr e) 1))))
				((and (enth x (cadr e)) (atom (caddr e)) (not (equal x (caddr e))) (symbolp (caddr e))) `(* ,(caddr e) ,(diff (cadr e) x) (expt ,(cadr e) (- ,(caddr e) 1))))
				((and (atom (cadr e)) (enth x (caddr e)) (equal x (cadr e))) `(* (+ (* (log ,(cadr e)) ,(diff (caddr e) x)) (/ ,(caddr e) ,(cadr e))) (expt ,(cadr e) ,(caddr e))))
				((and (atom (cadr e)) (not (enth x (caddr e))) (equal x (cadr e))) `(* ,(caddr e) (expt ,(cadr e) (- ,(caddr e) 1))))
				((and (enth x (cadr e)) (atom (caddr e)) (equal x (caddr e))) `(* (+ (log ,(cadr e)) (* (/ ,(caddr e) ,(cadr e)) ,(diff (cadr e) x))) (expt ,(cadr e) ,(caddr e))))
				((and (enth x (cadr e)) (enth x (caddr e))) `(* (+ (* (log ,(cadr e)) ,(diff (caddr e) x)) (* (/ ,(caddr e) ,(cadr e)) ,(diff (cadr e) x))) (expt ,(cadr e) ,(caddr e))))
				((and (not (enth x (cadr e))) (enth x (caddr e))) `(* (log ,(cadr e)) ,(diff (caddr e) x) (expt ,(cadr e) ,(caddr e))))
				((and (enth x (cadr e)) (not (enth x (caddr e)))) `(* ,(caddr e) (/ ,(diff (cadr e) x) (expt ,(cadr e) ,(caddr e)))))
				((and (not (enth x (cadr e))) (not (enth x (caddr e)))) 0)
				((and (not (enth x (cadr e))) (atom (caddr e)) (not (equal x (caddr e)))) 0)
				)
			 )
		)
	)
  )
)
;---------------------------------------3-----------------------------------------------------------------------------------------------
(DIFF 1 'X);-> 				0
(DIFF 'X 'Y);->				0
(DIFF 'X 'X);->				1
(princ `( 1 --------------------------------------))
;--------------------------------------11-------------------------------------------------------------------------------------------------
(DIFF '(+ X) 'X);-> 			1
(DIFF '(+ Y) 'X);-> 			0
(DIFF '(+ 1) 'X);-> 			0
(DIFF '(* X) 'X);-> 			1
(DIFF '(* Y) 'X);-> 			0
(DIFF '(* 1) 'X);-> 			0
(DIFF '(/ X) 'X);-> 			(- (/ (EXPT X 2)))
(DIFF '(/ Y) 'X);-> 			0
(DIFF '(/ 0) 'X);->             DIVISIONSFEHLER
(DIFF '(/ 1) 'X);->             0
(DIFF '(/ (SIN X)) 'X);->      (- (/ (COS X) (* (SIN X) (SIN X))))
(princ `( 2 --------------------------------------))
;---------------------------------------5-------------------------------------------------------------------------------------------------
(DIFF '(SIN X) 'X);-> 			(COS X)
(DIFF '(SIN Y) 'Y);-> 			(COS Y)
(DIFF '(SIN X) 'Y);-> 			0
(DIFF '(SIN 1) 'X);-> 			0
(DIFF '(SIN (* X X)) 'X);->    (* (COS (* X X)) (* 2 X))
(princ `( 3 --------------------------------------))
;---------------------------------------5-------------------------------------------------------------------------------------------------
(DIFF '(COS X) 'X);-> 			(- (SIN X))
(DIFF '(COS Y) 'Y);-> 			(- (SIN Y))
(DIFF '(COS X) 'Y);-> 			0
(DIFF '(COS 1) 'X);-> 			0
(DIFF '(COS (+ X X)) 'X);->             (- (* (SIN (+ X X)) 2))
(princ `( 4 --------------------------------------))
;---------------------------------------5-------------------------------------------------------------------------------------------------
(DIFF '(TAN X) 'X);-> 			(/ (* (COS X) (COS X)))
(DIFF '(TAN Y) 'Y);-> 			(/ (* (COS Y) (COS Y)))
(DIFF '(TAN X) 'Y);-> 			0
(DIFF '(TAN 1) 'X);-> 			0
(DIFF '(TAN (/ X)) 'X);->               (/ (- (/ (* X X))) (* (COS (/ X)) (COS (/ X)))) = (/ (- (* X X)) (* (COS (/ X)) (COS (/ X))))
(princ `( 5 --------------------------------------))
;---------------------------------------5-------------------------------------------------------------------------------------------------
(DIFF '(COT X) 'X);-> 			(- (/ (* (SIN X)) (SIN X)))
(DIFF '(COT Y) 'Y);-> 			(- (/ (* (SIN Y)) (SIN Y)))
(DIFF '(COT X) 'Y);-> 			0
(DIFF '(COT 1) 'X);-> 			0
(DIFF '(COT (EXP X)) 'X);->             (- (/ (EXP X) (* (SIN (EXP X)) (SIN (EXP X)))))
(princ `( 6 --------------------------------------))
;---------------------------------------5-------------------------------------------------------------------------------------------------
(DIFF '(EXP X) 'X);-> 			(EXP X)
(DIFF '(EXP X) 'Y);-> 			0
(DIFF '(EXP 3) 'X);-> 			0
(DIFF '(EXP (SIN X)) 'X);-> 		(* (EXP (SIN X)) (COS X))
(DIFF '(EXP (* X X)) 'X);-> 		(* (EXP (* X X)) (* 2 X))
(princ `( 7 --------------------------------------))
;--------------------------------------13------------------------------------------------------------------------------------------------
(DIFF '(+ 2 2) 'X);-> 			0		
(DIFF '(+ X 2) 'X);-> 			1		
(DIFF '(+ 2 X) 'X);-> 			1		
(DIFF '(+ Y X) 'X);-> 			1		
(DIFF '(+ X X) 'X);-> 			2
(DIFF '(+ 2 (SIN X)) 'X);-> 		(COS X)
(DIFF '(+ (SIN X) 2) 'X);-> 		(COS X)
(DIFF '(+ X (SIN X)) 'X);-> 		(+ 1 (COS X))
(DIFF '(+ (SIN X) X) 'X);-> 		(+ (COS X) 1)
(DIFF '(+ (SIN X) (SIN X)) 'X);-> 	(+ (COS X) (COS X))
(DIFF '(+ (SIN X) (SIN Y)) 'X);-> 	(COS X)
(DIFF '(+ (SIN Y) (SIN X)) 'X);-> 	(COS X)
(DIFF '(+ (SIN Y) (SIN Y)) 'X);-> 	0
(princ `( 8 --------------------------------------))
;---------------------------------------15-----------------------------------------------------------------------------------------------
(DIFF '(- 2 2) 'X);-> 			0		
(DIFF '(- X 2) 'X);-> 			1		
(DIFF '(- 2 X) 'X);-> 			-1		
(DIFF '(- X X) 'X);-> 			0	
(DIFF '(- X Y) 'X);-> 			1	
(DIFF '(- Y X) 'X);-> 			-1	
(DIFF '(- Y Y) 'X);-> 			0	
(DIFF '(- 2 (SIN X)) 'X);-> 		(- (COS X))
(DIFF '(- (SIN X) 2) 'X);-> 		(COS X)
(DIFF '(- X (SIN X)) 'X);-> 		(- 1 (COS X))
(DIFF '(- (SIN X) X) 'X);-> 		(- (COS X) 1)
(DIFF '(- (SIN X) (SIN X)) 'X);-> 	0
(DIFF '(- (SIN X) (SIN Y)) 'X);-> 	(- (COS X)) 
(DIFF '(- (SIN Y) (SIN X)) 'X);-> 	(COS X)
(DIFF '(- (SIN Y) (SIN Y)) 'X);-> 	0
(princ `( 9 --------------------------------------))
;--------------------------------------13------------------------------------------------------------------------------------------------
(DIFF '(* 2 2) 'X);-> 			0
(DIFF '(* X 2) 'X);-> 			2
(DIFF '(* 2 X) 'X);-> 			2
(DIFF '(* X X) 'X);-> 			(* 2 X)
(DIFF '(* X Y) 'X);-> 			Y
(DIFF '(* 2 (SIN X)) 'X);-> 		(* 2 (COS X))
(DIFF '(* (SIN X) 2) 'X);-> 		(* (COS X) 2)
(DIFF '(* X (SIN X)) 'X);-> 		(+ (SIN X) (* X (COS X)))
(DIFF '(* (SIN X) X) 'X);-> 		(+ (SIN X) (* X (COS X)))
(DIFF '(* (SIN X) (SIN X)) 'X);-> 	(+ (* (SIN X) (COS X)) (* (SIN X) (COS X)))
(DIFF '(* (SIN X) (SIN Y)) 'X);-> 	(* (SIN Y) (COS X))
(DIFF '(* (SIN Y) (SIN X)) 'X);-> 	(* (SIN Y) (COS X))
(DIFF '(* (SIN Y) (SIN Y)) 'X);-> 	0
(princ `( 10 --------------------------------------))
;--------------------------------------13-------------------------------------------------------------------------------------------------
(DIFF '(/ 2 2) 'X); -> 			0
(DIFF '(/ X 2) 'X); -> 			(/ 2)
(DIFF '(/ 2 X) 'X); -> 			(/ (- 2) (* X X))
(DIFF '(/ X X) 'X); -> 			0
(DIFF '(/ 1 (SIN X)) 'X);-> 		(- (/ (* 1 (COS X)) (* (SIN X) (SIN X))))
(DIFF '(/ 2 (SIN X)) 'X);-> 		(- (/ (* 2 (COS X)) (* (SIN X) (SIN X))))
(DIFF '(/ (SIN X) 2) 'X);-> 		(/ (COS X) 2)
(DIFF '(/ X (SIN X)) 'X);-> 		(/ (- (SIN X) (* X (COS X))) (* (SIN X) (SIN X)))
(DIFF '(/ (SIN X) X) 'X);-> 		(/ (- (* X (COS X)) (SIN X)) (* X X))
(DIFF '(/ (SIN X) (SIN X)) 'X);-> 	0   ; nach vereinfachung 0
(DIFF '(/ (SIN X) (SIN Y)) 'X);-> 	(/ (COS X) (SIN Y))
(DIFF '(/ (SIN Y) (SIN X)) 'X);-> 	(- (/ (* (SIN Y) (COS X)) (* (SIN X) (SIN X))))
(DIFF '(/ (SIN Y) (SIN Y)) 'X);-> 	0
(princ `( 11 --------------------------------------))
;--------------------------------------16------------------------------------------------------------------------------------------------
(DIFF '(EXPT 2 3) 'X);->                0
(DIFF '(EXPT X 3) 'X);-> 		        (* 3 (EXPT X 2))
(DIFF '(EXPT Y 3) 'X);->                0
(DIFF '(EXPT 3 X) 'X);-> 		        (* 1.09861231 (EXPT 3 X))
(DIFF '(EXPT 3 Y) 'X);->                0
(DIFF '(EXPT X X) 'X);-> 		        (* (+ 1 (LOG X)) (EXPT X X)) 
(DIFF '(EXPT Y Y) 'X);->                0
(DIFF '(EXPT X (SIN Y)) 'X);->          (* (SIN Y) (EXPT X) (- (SIN Y) 1))   OK
(DIFF '(EXPT Y (SIN X)) 'X);->          (* (LOG Y) (COS X) (EXPT Y (SIN X))) OK
(DIFF '(EXPT 3 (SIN X)) 'X);->          (* 1.09861231 (COS X) (EXPT 3 (SIN X))) OK
(DIFF '(EXPT X (* X X)) 'X);->          (* (+ (* (LOG X) (* 2 X)) (/ (* X X) X)) (EXPT X (* X X))) = (* (+ X (* 2 X (LOG X)) (EXPT X (* X X))))
(DIFF '(EXPT Y (* X X)) 'X);->          (* (LOG Y) (* 2 X) (EXPT Y (* X X)))
(DIFF '(EXPT (SIN X) X) 'X);->          (* (+ (LOG (SIN X)) (* (/ X (SIN X)) (COS X))) (EXPT (SIN X) X))
(DIFF '(EXPT (SIN X) (* X X)) 'X);->    (* (+ (* (LOG (SIN X)) (* 2 X)) (* (/ (* X X) (SIN X)) (COS X))) (EXPT (SIN X) (* X X))) = 
;                                       (* (+ (* (LOG (SIN X)) 2) (* X (COT X))) X (EXPT (SIN X) (* X X)))
(DIFF '(EXPT (SIN Y) (* X X)) 'X);->    (* (LOG (SIN Y)) (* 2 X) (EXPT (SIN Y) (* X X)))
(DIFF '(EXPT (SIN X) (* Y Y)) 'X);->    (* (* Y Y) (/ (COS X) (SIN X)) (EXPT (SIN X) (* Y Y))) = (* (* Y Y) (COS X) (EXPT (SIN X) (- (* Y Y) 1)))  
(DIFF '(EXPT (SIN X) 5) 'X);->			(* 5 (COS X) (EXPT (SIN X) 4))
(DIFF '(EXPT (SIN Y) 5) 'X);->			0
(DIFF '(EXPT (SIN X) Y) 'X);->          (* Y (COS X) (EXPT (SIN X) (- Y 1)))
(DIFF '(EXPT (SIN Y) Y) 'X);->		    0
(DIFF '(EXPT (SIN X) (* Y Y)) 'X);->    (* (* Y Y) (/ (COS X) (SIN X)) (EXPT (SIN X) (* Y Y))) = (* (* Y Y) (COS X) (EXPT (SIN X) (- (* Y Y) 1)))  
(princ `( 12 --------------------------------------))
;----------------------------------------------------------------------------------------------------------------------------------------
(DIFF (DIFF '(* X Y) 'X) 'Y);-> 1
(DIFF (DIFF '(* X (* Y Z)) 'X) 'Y);-> (+ Z 0)
(DIFF '(+ (* (sin x) (sin x)) (* (cos x) (cos x))) 'x)
;-----------------------------------------------------------------------------------------------
;  vereinfachungsfunktion
;-----------------------------------------------------------------------------------------------
(defun vereinf (expr) 
(cond	
        ((atom expr) expr)
        ((equal (cdr expr) nil) (car expr))
		((member (car expr) '(cos sin tan cot log)) expr)
        ((and (equal (car expr) '/) (equal (caddr expr) 0)) 'FEHLER-Div0)
		((and (numberp (cadr expr)) (numberp (caddr expr))) (eval expr))
		(T	(case	(car expr)
				(+ (cond	
						((equal (cadr expr) 0) (caddr expr))
						((equal (caddr expr) 0) (cadr expr))
						((null (cddr expr)) (cadr expr) )
                        ((and (listp (cadr expr)) (listp (caddr expr)) (or (and (equal (cadr (cadr expr)) (cadr (caddr expr))) (equal (caddr (cadr expr)) (caddr (caddr expr))))    (and (equal (cadr (cadr expr)) (caddr (caddr expr))) (equal (caddr (cadr expr)) (cadr (caddr expr)))))) `(* 2 ,(cadr expr)))    ;	(+ (* (SIN X) (COS X)) (* (SIN X) (COS X)))
						(T (list '+ (vereinf (cadr expr)) (vereinf (caddr expr))))
					)
				)
				(- (cond
                        ((and (listp (cadr expr)) (equal (car (cadr expr)) '-) (null (caddr expr))) (cadr (cadr expr)))
                        ((equal (caddr expr) nil) (if (equal (vereinf (cadr expr)) (cadr expr)) `(- ,(cadr expr)) `(- ,(vereinf (cadr expr)))))
						((and (equal (cadr expr) 0) (equal (car (caddr expr)) '-)) `(+ ,(cadr expr) ,(cdaddr expr)))                      ;`(- ,(caddr expr)))
						((equal (cadr expr) 0) `(- ,(caddr expr)))
					 	((equal (cadr expr) (caddr expr)) 0)
                        ((and (listp (caddr expr)) (equal (car (caddr expr)) '-)) `(+ ,(cadr expr) ,(cdaddr expr)))
						(T (list '- (vereinf (cadr expr)) (vereinf (caddr expr))))
					)
				)
               (*  (cond
                        ((equal (cadr expr) 0) 0)
                        ((equal (caddr expr) 0) 0)
                        ((equal (caddr expr) nil) (cadr expr))
                        ((equal (cadr expr) 1) (caddr expr))
                        ((equal (cadr expr) '(- 1)) `(- ,(caddr expr)))
                        ((equal (caddr expr) 1) (cadr expr))
                        ((equal (caddr expr) '(- 1)) `(- ,(cadr expr)))
                        ((null (cddr expr)) (cadr expr))
                        ((equal (cadr expr) (caddr expr)) `(expt ,(cadr expr) 2))
                        (T (list '* (vereinf (cadr expr)) (vereinf (caddr expr))))
                   )
               )
               (/  (cond
                        ((equal (caddr expr) nil) (if (equal (vereinf (cadr expr)) (cadr expr)) `(/ ,(cadr expr)) `(/ ,(vereinf (cadr expr)))))
                        ((equal (cadr expr) 0) 0)
                        ((equal (caddr expr) 1) (cadr expr))
                        ((and (listp (cadr expr)) (listp (caddr expr)) (or (and (equal (cadr (cadr expr)) (cadr (caddr expr))) (equal (caddr (cadr expr)) (caddr (caddr expr)))) (and (equal (cadr (cadr expr)) (caddr (caddr expr))) (equal (caddr (cadr expr)) (cadr (caddr expr)))))) 1)
                        (T (list '/ (vereinf (cadr expr)) (vereinf (caddr expr))))
                   ) 
               ) 
               (expt (list 'expt (vereinf (cadr expr)) (vereinf (caddr expr))))
			)
		)
)
)
;-----------------------------------------------------------------------------------------------
;  prüfung, ob ein fehlercode in einem ausdruck (baum) enthalten ist
;    falls ja, fehlercode zurückgeben, ansonsten nil
;-----------------------------------------------------------------------------------------------
(defun fehler (expr code &optional (expr-akku nil) (erg-akku nil))
  (cond
   ((and (null expr) (null expr-akku)) erg-akku)
   ((null expr) (fehler expr-akku code nil erg-akku))
   ((and (atom (car expr)) (equal (car expr) code)) (fehler (cdr expr) code expr-akku code))
   ((listp (car expr)) (fehler (car expr) code (append (cdr expr) expr-akku) erg-akku))
   (t (fehler (cdr expr) code expr-akku erg-akku))    
  )  
)
;-----------------------------------------------------------------------------------------------
(PRINT '---------------------------------------------fehlerfunktion---------------------------------------------)
(fehler '(+ 6) 'd);->NIL
(fehler '(+ (- 3 (COS X)) (* 4 FEHLER-DIV0)) 'FEHLER-DIV0);->FEHLER-DIV0
(fehler '(+ (- 3 (COS X)) (+ (* 4 FEHLER-DIV0) 5)) 'FEHLER-DIV0);->FEHLER-DIV0
(fehler '(+ (- 3 (+ 5 FEHLER-DIV0)) (* 4 5)) 'FEHLER-DIV0);->FEHLER-DIV0
(fehler '(+ (- 3 FEHLER-DIV0) (* 4 5)) 'FEHLER-DIV0);->FEHLER-DIV0
(fehler '(+ (- 3 5) FEHLER-DIV0) 'FEHLER-DIV0);->FEHLER-DIV0
;-----------------------------------------------------------------------------------------------
;  vereinfachung solange aufrufen, bis nicht weiter vereinfacht werden kann + fehlerausgabe
;-----------------------------------------------------------------------------------------------
(defun vereinf2 (expr)
(cond   ((fehler (list (vereinf expr)) 'FEHLER-DIV0) (fehler (list (vereinf expr)) 'FEHLER-DIV0)) 
        ((equal expr (vereinf expr)) expr)
		(T (vereinf2 (vereinf expr)))))
;-----------------------------------------------------------------------------------------------
(PRINT '---------------------------------------------PLUS-----------------------------------------------------)
(vereinf2 '(+ 5));->5
(vereinf2 '(+ (+ 5)));->5
(vereinf2 '(+ (+ 4) (- (sin x) (sin x))));->4
(vereinf2 '(+ 4 (- (tan x) (tan x))));->4
(vereinf2 '(+ (* (SIN X) (COS X)) (* (SIN X) (COS X))));->(* 2 (* (SIN X) (COS X)))
(vereinf2 '(+ (* (COS X) (SIN X)) (* (SIN X) (COS X))));->(* 2 (* (COS X) (SIN X)))
(PRINT '---------------------------------------------MINUS-----------------------------------------------------)

(vereinf2 '(- 4 0));->4
(vereinf2 '(- 4 4));->0
(vereinf2 '(- 0 5));->-5
(vereinf2 '(- 0 0));->0
(vereinf2 '(- (1) (- cos x)));->(+ 1 (COS X))
(vereinf2 '(- (1) (cos x)));->(- 1 (COS X))
(vereinf2 '(- 1));->(- 1)
(vereinf2 '(- (- 1)));->1
(PRINT '---------------------------------------------MAL-----------------------------------------------------)
(vereinf2 '(* 4 0));->0
(vereinf2 '(* 4 4));->16
(vereinf2 '(* 0 5));->0
(vereinf2 '(* 0 0));->0
(vereinf2 '(* (- 1) (cos x)));->(- (COS X))
(vereinf2 '(* 1));->1
(vereinf2 '(* (- 1)));->(- 1)
(vereinf2 '(* 5 1));->5
(vereinf2 '(* 1 (cos x)));->(COS X)
(vereinf2 '(* (cos x) 1));->(COS X) 
(vereinf2 '(* (cos x)));->(COS X)
(vereinf2 '(* (cos x) (cos x)));->(EXPT (COS X) 2) 
(PRINT '---------------------------------------------DIVISION-----------------------------------------------------)
(vereinf2 '(/ 4 0));->FEHLER-DIV0
(vereinf2 '(/ 4 4));->1
(vereinf2 '(/ 0 5));->0
(vereinf2 '(/ 0 0));->FEHLER-DIV0
(vereinf2 '(/ (- 1) (cos x)));->(/ (- 1) (COS X))
(vereinf2 '(/ 1));->(/ 1)
(vereinf2 '(/ (- 1)));->(/ (- 1))
(vereinf2 '(/ 5 1));->5
(vereinf2 '(/ 1 (cos x)));->(/ 1 (COS X)) 
(vereinf2 '(/ (cos x) 1));->(COS X)  
(vereinf2 '(/ (cos x)));->(/ (COS X))
(vereinf2 '(/ (cos x) (cos x)));->1 
(vereinf2 '(+ (- 3 (cos x)) (* 4 (/ (sin x) 0))));->FEHLER-DIV0
(vereinf2 '(/ (cos x) 0));->FEHLER-DIV0
(vereinf2 '(/ (* (sin x) (sin x))));->(/ (EXPT (SIN X) 2))
(vereinf2 '(- (/ (* (SIN X)) (SIN X))));->(- 1)
(vereinf2 '(+ (/ (* (SIN X) (SIN X)))));->(/ (EXPT (SIN X) 2))
(vereinf2 '(- (/ (* (SIN X) (SIN X)))));->(- (/ (EXPT (SIN X) 2)))
(vereinf2 '(* (/ (* (SIN X) (SIN X)))));->(/ (EXPT (SIN X) 2))
(vereinf2 '(/ (/ (* (SIN X) (SIN X)))));->(/ (/ (EXPT (SIN X) 2)))
(vereinf2 '(/ (* (SIN X) (SIN X))));->(/ (EXPT (SIN X) 2))
(vereinf2 '(- (- (* (SIN X) (SIN X)))));->(EXPT (SIN X) 2)
(vereinf2 '(/ (- (/ (* X X))) (* (COS (/ X)) (COS (/ X)))));->(/ (- (/ (EXPT X 2))) (EXPT (COS (/ X)) 2))
(PRINT '---------------------------------------------Vereinfachung-nach-diff------------------------------------------)
;-----------------------------------------------------------------------------------------------
; geht teils nicht, da operationen (z.B. *) mit einer Arität von n > 2 vorkommen
;     der rest wird dann jeweils verworfen
;-----------------------------------------------------------------------------------------------
(vereinf2 (DIFF '(EXPT (SIN Y) (* X X)) 'X));-> Arität > 2 -> fehlerhafte vereinfachung
(vereinf2 (DIFF '(EXPT (SIN X) (* Y Y)) 'X));-> (* (EXPT Y 2) (/ (COS X) (EXPT (SIN X) (EXPT Y 2))))    
(vereinf2 (DIFF '(EXPT (SIN X) 5) 'X));-> Arität > 2 -> fehlerhafte vereinfachung			
(vereinf2 (DIFF '(EXPT (SIN Y) 5) 'X));-> 0			
(vereinf2 (DIFF '(EXPT (SIN X) Y) 'X));-> Arität > 2 -> fehlerhafte vereinfachung         
(vereinf2 (DIFF '(EXPT (SIN Y) Y) 'X));-> 0	    

