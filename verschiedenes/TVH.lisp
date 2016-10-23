; Turm von Hanoi
(defun HF (von nach)
	(print `(tausche ,von mit ,nach))
1)

(defun TVH (n A B C)
	(COND ((= n 1) (HF A C))
		(T  (+ (TVH (- n 1) A C B)
			(HF A C)
			(TVH (- n 1) B A C))		
		)
	)
)


(TVH 10 'A 'B 'C)