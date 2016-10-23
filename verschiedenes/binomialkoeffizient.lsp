; Funktion zur Berechnung der Werte der Binomialkoeffizienten
; Ohne Benutzung der Fakultätsfunktion - rekursiv 
(DEFUN BINOMIAL (N K); N über K
(COND ((< K 0) "K ist NEGATIV")
	  ((< N 0) "N ist NEGATIV")
	  ((> K N) "K ist größer als N")
      ((ZEROP K) 1)
      ((ZEROP N) 1)
      ((= N K) 1)
      (T (+ (BINOMIAL (- N 1) (- K 1)) (BINOMIAL (- N 1) K)))))
;----------------------------------------------------------------------------------
(BINOMIAL 4 2);-> 6
(BINOMIAL 5 2);-> 10
(BINOMIAL 10 5);-> 252
(BINOMIAL 2 5);-> "K ist größer als N"
(BINOMIAL 5 -2);-> "K ist NEGATIV"
(BINOMIAL -5 2);-> "N ist NEGATIV"