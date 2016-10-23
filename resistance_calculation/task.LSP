;********************************************************************************************************
;********************************************************************************************************
;*******                   Übung 2: Widerstandsberechnung von Gleichstromnetzwerken		   ******
;********************************************************************************************************
;********************************************************************************************************
; Gegeben ist ein Gleichstromnetzwerk aus Reihen- und Parallelschaltungen von Widerständen; gesucht ist 
; der Gesamtwiderstand.
;
;		   +------+			
;	 +---------| 100  |----------+
;	 |	   +------+ 	     |
; A -----+			     +------- B
;	 |   +-------+	 +-------+   |
;	 +---|  200  |---|  300  |---+
;	     +-------+	 +-------+
;		
; Beispiel für ein Gleichstromnetzwerk aus Reihen- und Parallelschaltungen von Widerständen zwischen A und B.
;
;
; Die Bestimmung des Gesamtwiderstandes einer beliebigen Schaltung wird auf die Bestimmung des Widerstandes
; der Reihenschaltung und Parallelschaltung zurückgeführt.
;
; Formel für die Bestimmung des Gesamtwiderstandes einer Reihenschaltung: RG=R1+R2+...+Rn 
;
; Formel für die Bestimmung des Gesamtwiderstandes einer Parallelschaltung: 
;
; RG= 1/(1/R1+1/R2+...+1/Rn )
;********************************************************************************************************
; Aufgabe 1: Definieren Sie eine Funktion REIWI, die für beliebig viele Widerstände den Gesamtwiderstand 
;            der Reihenschaltung dieser Widerstände bestimmt. Die einzelnen Widerstände sind durch 
;            Punktpaare gegeben. die den Zahlenwert und die Maßeinheit enthalten. 
;            Z.B. (100 . KILOOHM) bedeuten einen Widerstand von 100 Kiloohm.
;            Die Reihenschaltung von 0 Widerständen ergibt den Gesamtwiderstand 0.
;--------------------------------------------------------------------------------------------------------
; Hinweis: Da die Anzahl der Parameter variabel ist benutzen Sie die Option &REST für die Lambdaliste
;--------------------------------------------------------------------------------------------------------
(REIWI '(1 . OHM) '(200 . OHM) '(1 . KILOOHM));-> (1201 . OHM)
(REIWI);-> (0 . OHM)
(REIWI '(200 . OHM));-> (200 . OHM)
;********************************************************************************************************
; Aufgabe 2: Definieren Sie eine Funktion PARWI, die für beliebig viele Widerstände den Gesamtwiderstand 
;            der Parallelschaltung dieser Widerstände bestimmt. Die einzelnen Widerstände sind durch 
;            Punktpaare gegeben. die den Zahlenwert und die Maßeinheit enthalten. 
;            Die Parallelschaltung von 0 Widerständen ergibt den Gesamtwiderstand UNENDLICH
;--------------------------------------------------------------------------------------------------------
(PARWI '(1 . OHM) '(200 . OHM) '(1 . KILOOHM));-> (500/503 . OHM)
(PARWI);-> (UNENDLICH . OHM)
(PARWI '(200 . OHM));-> (200 . OHM)
;********************************************************************************************************
; Aufgabe 3: Aufbauend auf Erweiterungen der Funktionen REIWI und PARWI, definieren Sie eine Funktion 
;            REIPARWI, die für eine beliebige Schaltung, die aus Reihen- und Parallelschaltungen bestehtden
;            Gesamtwiderstand bestimmt. 
;            Die einzelnen Widerstände sind durch Punktpaare gegeben; ihre Verknüpfung durch einen 
;            entsprechenden verschachtelten Ausdruck in Präfixnotation mit den Symbolen + für 
;            Reihenschaltung und = für Parallelschaltung.
;
;					  +------+			
;				+---------| 100  |----------+
;				|	  +------+ 	    |
;		           -----+ 		            +-------
;				|   +-------+	+-------+   |
;				+---|  200  |---|  300  |---+
;				    +-------+	+-------+
;
;            Der Ausdruck, der diese Schaltung beschreibt ist: (= (100 . OHM) (+ (200 . OHM) (300 . OHM)))
;            Die Funktion REIPARWI wertet diesen Ausdruck aus und gibt das Ergebnis zurück.
;--------------------------------------------------------------------------------------------------------
; Hinweis: Die  Funktionen REIWI und PARWI sind so zu erweitern, daß sie nicht nur Widerstandswerte 
;          verarbeiten sondern auch ganze Ausdrücke. Dazu sind die COND Ausdrücke lediglich um eine 
;          Klausel zu erweitern
;--------------------------------------------------------------------------------------------------------
(REIPARWI '(= (100 . OHM) (+ (200 . OHM) (300 . OHM))));->(250/3 . OHM)
(REIPARWI '(=));-> (UNENDLICH . OHM)
(REIPARWI '(+));-> (0 . OHM)
(REIPARWI '(+ (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM)))) (= (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))) (+ (= (1 . OHM) (1 . OHM)) (= (1 . OHM) (1 . OHM))))));->
;-> (1 . OHM)
