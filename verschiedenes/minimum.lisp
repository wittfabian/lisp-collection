;minimum
(defun MINI (X Y)
	(cond ; ( (> X Y) (mini Y X))
	( (<= X 0) X )
	( (<= Y 0) Y )
	(t (+ 1 (MINI (- X 1) (- Y 1) ) ))))

(MINI 3 4)
(MINI 8 5)
(mini -2 -4)