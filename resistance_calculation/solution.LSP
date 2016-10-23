;********************************************************************************************************
;********************************************************************************************************
;*******                   Übung 2: Widerstandsberechnung von Gleichstromnetzwerken		   ******
;********************************************************************************************************
;********************************************************************************************************
(DEFUN EINHEIT (E &OPTIONAL F)
(COND ((EQL (CAR E) 'UNENDLICH) 'UNENDLICH)
      ((EQL (CDR E) 'OHM) (CAR E))
      ((EQL (CDR E) 'KILOOHM) (* (CAR E) 1000))
      ((EQL (CDR E) 'MKF) (COMPLEX 0 (/ (- 1000000) (* 2 PI F (CAR E)))))
      ((EQL (CDR E) 'MLH) (COMPLEX 0 (/ (* 2 PI F (CAR E)) 1000)))
      ((EQL (CDR E) 'MKH) (COMPLEX 0 (/ (* 2 PI F (CAR E)) 1000000)))
      )
)
;(EINHEIT '(200 . OHM));-> 200
;(EINHEIT '(200 . KILOOHM));-> 200000


(DEFUN REIWI (&REST WILI)
;(FORMAT T "REIWI: ~S~%" WILI)
(COND ((NULL WILI) '(0 . OHM))
      ((MEMBER (CAAR WILI) '(+ =)) (REIPARWI (CAR WILI)))
      (T (CONS (+ (EINHEIT (CAR WILI)) (CAR (APPLY 'REIWI (CDR WILI)))) 'OHM))
))
;(REIWI '(1 . OHM) '(200 . OHM) '(1 . KILOOHM));-> (1201 . OHM)
;(REIWI);-> (0 . OHM)
;(REIWI '(200 . OHM));-> (200 . OHM)


;Berechnung fuer PARWI
(DEFUN PAR2 (R1 R2)
(COND ((EQL R1 'UNENDLICH) R2)
      ((EQL R2 'UNENDLICH) R1)
      (T (/ (* R1 R2) (+ R1 R2)))
))

;(R1 * R2) / (R1 + R2)
(DEFUN PARWI (&REST WILI)
;(FORMAT T "PARWI: ~S~%" WILI)
(COND ((NULL WILI) '(UNENDLICH . OHM))
      ((MEMBER (CAAR WILI) '(+ =)) (REIPARWI (CAR WILI)))
      (T (CONS (PAR2 (EINHEIT (CAR WILI)) (CAR (APPLY 'PARWI (CDR WILI)))) 'OHM))
))      
;(PARWI '(1 . OHM) '(200 . OHM) '(1 . KILOOHM));-> (500/503 . OHM)
;(PARWI);-> (UNENDLICH . OHM)
;(PARWI '(200 . OHM));-> (200 . OHM)



(DEFUN REIPARWI (SCHEX)
;(FORMAT T "REIPARWI: ~S~%" SCHEX)
(COND ((EQL (CAR SCHEX) '+) (APPLY 'REIWI (CDR SCHEX)))
      ((EQL (CAR SCHEX) '=) (APPLY 'PARWI (CDR SCHEX)))
))
;(REIPARWI '(= (100 . OHM) (+ (200 . OHM) (300 . OHM))));->(250/3 . OHM)
;(REIPARWI '(=));-> (UNENDLICH . OHM)
;(REIPARWI '(+));-> (0 . OHM)
;(REIPARWI '(+ (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM)))) (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))))));->
;-> (1/2 . OHM)


;Kondensatorreihenschaltung
(DEFUN KOREI (F &REST WILI)
;(FORMAT T "KOREI: ~S~%" WILI)
(COND ((NULL WILI) '(0 . OHM))
      (T (CONS (+ (EINHEIT (CAR WILI) F) (CAR (APPLY 'KOREI F (CDR WILI)))) 'OHM))
))
;(KOREI 100 '(1 . MKF) '(1 . MLH) '(1 . KILOOHM) '(100 . MKH));->( #C(1000.0D0 -1590.8582805351634D0) . OHM)



;********************************************************************************************************
;Klausurteil im Wechelstromkreis
;********************************************************************************************************
;--------------------------------------------------------------------------------------------------------
; Hinweis1: Die Kapazität von gebräuchlichen Kondensatoren wird in PIKOFARAD, NANOFARAD und MIKROFARAD
;           gemessen. 1 FARAD = 10^6 MIKROFARAD  = 10^9 NANOFARAD = 10^12 PIKOFARAD
;           Die Indukivität von gebräuchlichen Spulen wird in MIKROHENRY, MILLIHENRY und HENRY
;           gemessen. 1 HENRY = 10^6 MIKROHENRY  = 10^3 MILLIHENRY
;--------------------------------------------------------------------------------------------------------
; Hinweis2: Der komplexe Widerstand eines Kondensators mit der Kapazität C ist -j/2*pi*f*C.
;           Der komplexe Widerstand einer Spule mit der Induktivität L ist j*2*pi*f*L.
;           Hierbei ist j die imaginäre Einheit und f die Frequenz.
;--------------------------------------------------------------------------------------------------------
; Hinweis3: Komplexe Zahlen werden LISP mit der Funktion COMPLEX erzeugt.
;           (COMPLEX <RealTeil> &OPTIONAL <ImaginärTeil).
;           So erhält man den Widerstand einer Spule mit 0.5 H bei 100 Hz mit dem Ausdruck:
;           (COMPLEX 0 (* 2 PI 100 0.5)) -> #C(0.0D0 314.15926535897933D0)
;           Da alle arithmetischen Funktionen in LISP mit komplexen Zahlen rechnen können bleiben alle
;           Funktionen, die für die Berechnung von Gleichstromkreisen gelten auch für die Berechnung von
;           Wechselstromkreisen gültig.
;--------------------------------------------------------------------------------------------------------
;Ueberpruefen ob F in der Liste ist
(DEFUN F-IN (L &OPTIONAL F)
(COND ((NULL L) NIL)
      ((MEMBER (CAR L) '(+ =)) (F-IN (CDR L)))
      ((EQL (CDAR L) 'F) T)
      (T (F-IN (CDR L)))
))
;(F-IN '(+ (1 . OHM) (1 . H) (1 . F)));-> T
;(F-IN '(+ (= (3 . OHM) (2 . H) (1 . H)) (2 . F) (= (10 . OHM) (10 . OHM))));-> NIL


(DEFUN EINHEIT (E &KEY (F 0 FF))
(COND ((EQL (CAR E) 'UNENDLICH) 'UNENDLICH)
      ((EQL (CDR E) 'OHM) (CAR E))
      ((EQL (CDR E) 'KILOOHM) (* (CAR E) 1000))

      ((AND FF (EQL (CDR E) 'F)) (COMPLEX 0 (/ (- 1) (* 2 PI F (CAR E)))))
      ((AND FF (EQL (CDR E) 'MF)) (COMPLEX 0 (/ (- 1000) (* 2 PI F (CAR E)))))
      ((AND FF (EQL (CDR E) 'MKF)) (COMPLEX 0 (/ (- 1000000) (* 2 PI F (CAR E)))))
      ((AND FF (EQL (CDR E) 'NF)) (COMPLEX 0 (/ (- 1000000000) (* 2 PI F (CAR E)))))
      ((AND FF (EQL (CDR E) 'PF)) (COMPLEX 0 (/ (- 1000000000000) (* 2 PI F (CAR E)))))
      
      ((AND FF (EQL (CDR E) 'H))   (COMPLEX 0 (* 2 PI F (CAR E))))
      ((AND FF (EQL (CDR E) 'MLH)) (COMPLEX 0 (/ (* 2 PI F (CAR E)) 1000)))
      ((AND FF (EQL (CDR E) 'MKH)) (COMPLEX 0 (/ (* 2 PI F (CAR E)) 1000000)))
      (T 'FEHLER)
))
;(EINHEIT '(200 . OHM));-> 200
;(EINHEIT '(200 . KILOOHM));-> 200000
;(EINHEIT '(0.5 . H) :F 100);-> #C(0.0D0 314.15926535897933D0)


(DEFUN REIWI (F &REST WILI)
(COND ((NOT (NUMBERP F)) NIL)
      ((NULL WILI) '(0 . OHM))
      ;((AND (= 0 F) (F-IN WILI)) '(UNENDLICH . OHM));DIV/0 abfangen
      ((MEMBER (CAAR WILI) '(+ =)) (REIPARWI (CAR WILI) :F F))
      (T (CONS (+ (EINHEIT (CAR WILI) :F F) (CAR (APPLY 'REIWI F (CDR WILI)))) 'OHM))
))
;(REIWI 100 '(1 . MKF) '(1 . MLH) '(1 . KILOOHM) '(100 . MKH));->( #C(1000.0D0 -1590.8582805351634D0) . OHM )


;Berechnung fuer PARWI
(DEFUN PAR2 (R1 R2)
(COND ((EQL R1 'UNENDLICH) R2)
      ((EQL R2 'UNENDLICH) R1)
      (T (/ (* R1 R2) (+ R1 R2)))
))

;(R1 * R2) / (R1 + R2)
(DEFUN PARWI (F &REST WILI)
(COND ((NOT (NUMBERP F)) NIL)
      ((NULL WILI) '(UNENDLICH . OHM))
      ((MEMBER (CAAR WILI) '(+ =)) (REIPARWI (CAR WILI) :F F))
      (T (CONS (PAR2 (EINHEIT (CAR WILI) :F F) (CAR (APPLY 'PARWI F (CDR WILI)))) 'OHM))
)) 
;(PARWI 100 '(1 . MKF) '(1 . MLH) '(1 . KILOOHM) '(100 . MKH));->( #C(3.2629133344713836D-6 0.057121916317861586D0) . OHM )


(DEFUN REIPARWI (SCHEX &KEY (F 0))
(COND ((NULL SCHEX) 'FEHLER)
      ((EQL (CAR SCHEX) '+) (APPLY 'REIWI F (CDR SCHEX)))
      ((EQL (CAR SCHEX) '=) (APPLY 'PARWI F (CDR SCHEX)))
))
;(REIPARWI '(= (100 . OHM) (+ (200 . OHM) (300 . OHM))));->(250/3 . OHM)
;(REIPARWI '(=));-> (UNENDLICH . OHM)
;(REIPARWI '());-> FEHLER
;(REIPARWI '(+));-> (0 . OHM)
;(REIPARWI '(+ (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM)))) (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))))));->
;-> (1/2 . OHM)

;(REIPARWI '(+ (0.5 . H) (0.5 . H)) :F 100);->( #C(0.0D0 628.3185307179587D0) . OHM )
;(REIPARWI '(+ (0.5 . H) (0.5 . H)) :F '());->NIL
;(REIPARWI '(= (100 . OHM) (+ (200 . OHM) (1 . F))) :F 100);->(#C(66.66666666760483D0 -1.7683882565268448D-4) . OHM)

;(REIPARWI '(+ (1 . OHM) (1 . F)) :F 0);->(UNENDLICH . OHM)
;DIV DURCH 0
;frequenz 0 und kondensator = unendlich ohm in reiwi prüfen
;(REIPARWI '(+ (1 . OHM) (1 . H) (1 . F)) :F 0);->(UNENDLICH . OHM)














