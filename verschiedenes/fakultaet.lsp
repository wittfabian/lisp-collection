;Fakultät
(DEFUN FAKULTAET (N); N!
(COND ((< N 0) "NEGATIVER WERT")
      ((ZEROP N) 1)
      (T (* N (FAKULTAET (- N 1))))))
;----------------------------------------------------------------------------------
(FAKULTAET 5);-> 120
(FAKULTAET -5);-> "NEGATIVER WERT"