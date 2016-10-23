(SETF MOORET1 '(((0 . a) . 1) ((0 . b) . 2)
	      ((1 . a) . 3) ((1 . b) . 2)
	      ((2 . a) . 1) ((2 . b) . 4)
	      ((3 . a) . 5) ((3 . b) . 2)
	      ((4 . a) . 1) ((4 . b) . 5)
	      ((5 . a) . 5) ((5 . b) . 5)
	     )
)


(SETF AUSMOORE1 '((0 . X) 
		 (1 . Y)
	         (2 . X)
	         (3 . Y)
	         (4 . X)
	         (5 . Y)
	        )
)



(DEFUN MOORE (XL Z0 TMOORE AUSMOORE)


)




(MOORE '(A B A A B B) 0 TMOORE1 AUSMOORE1);->   (X Y X Y Y X X)
(MOORE '(A B A A B B B) 0 TMOORE1 AUSMOORE1);-> (X Y X Y Y X X Y)