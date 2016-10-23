; Aufgabe: Endlicher Automat
;------------------------------------------------------------------------------------------------
; Gegeben ist folgen Steuertabelle eines Akzeptors
;
; Z | a | b |   Akz
;---+---+---+-------
; 0 | 1 | 2 | ACCEPT
; 1 | 3 | 2 | ACCEPT
; 2 | 1 | 4 | ACCEPT
; 3 | 5 | 2 | ACCEPT
; 4 | 1 | 5 | ACCEPT
; 5 | 5 | 5 | NOACC
;
;------------------------------------------------------------------------------------------------
; 1. Kodieren Sie die Zustand�bergangstabelle TACC als Liste von zweifach geschachtelten Punktpaaren:
;    (((Zn . Xn) . Zn+1) ...). F�r die gegebenen Automatentabelle der erste Eintrag Ausgangszustand = 0,
;    Eingangsbuchstabe = a und Folgezustand = 1 den Eintrag  (((0 . a) . 1) ...)
;    F�r das Beispiel enth�lt die Liste ZustandsanzahlxEingangsbuchstabenanzahl = 12 Eintr�ge
;    Aus dieser Liste kann mit Hilfe einer Suchfunktion1 f�r einen gegebenen Zustand und einnen
;    gegebenen Eingangsbuchstaben einfach der Folgezustand ermittelt werden
;------------------------------------------------------------------------------------------------
; 2. Kodieren Sie die Akzeptierungstabelle AACC als Liste von Punktpaaren
;     ((0 . ACCEPT) ...(5 . NOACCEPT))
;    Aus dieser Liste kann mit Hilfe einer Suchfunktion2 f�r einen gegebenen Zustand ermittelt werden
;    ob in diesem Zustand die bisherige Eingabe akzeptiert wird 
;    F�r das Beispiel enth�lt die Liste Zustandsanzahl = 6 Eintr�ge
; Die durch die Kodierung erhaltenen Listen werden mit SETF an die Symbole (Variablen) TACC bzw. AACC
; gebunden: 
; (SETF TACC1 '(((0 . a) . 1) ...))
; (SETF AACC1 '((0 . ACCEPT) ...))    
;------------------------------------------------------------------------------------------------
; 3. Definieren Sie die SUCHFUNKTION1 f�r FOLGEZUSTAND 
;     (DEFUN Z1-SUCHE (Z0 X TACC) ...)
; Ber�cksichtigen Sie das Nichtfinden des Zustands-Eingabewort-Paares. Das kann durch Schreibfehler 
; bei der Kodierung der Steuertabelle verursacht werden.
;------------------------------------------------------------------------------------------------
(Z1-SUCHE 1 'B TACC);-> 2
;------------------------------------------------------------------------------------------------
; 4. Definieren Sie die  SUCHFUNKTION2 f�r ACCEPTANZ 
;     (DEFUN A-SUCHE (Z AACC)
; Ber�cksichtigen Sie das Nichtfinden des Zustands-Eingabewort-Paares. Das kann durch Schreibfehler 
; bei der Kodierung der Steuertabelle verursacht werden.
;------------------------------------------------------------------------------------------------
(A-SUCHE 1 AACC);-> ACCEPT
;------------------------------------------------------------------------------------------------
; 5. Durch fortlaufende Anwendung der Suchfunktion1 Z1-SUCHE kann der jeweilige Folgezustand f�r 
;    die Folge von Eingabebuchstaben (Eingabewort) ermittelt werden. Das Eingabewort ist als Liste 
;    der Eingabebuchstaben gegeben. Wenn die Liste abgearbeitet ist, dann kann mit Hilfe der 
;    Suchfunktion2 A-SUCHE aus dem erhaltenen Endzustand bestimmt werden, ob das Eingabewort
;    von dem Automaten akzeptiert wird oder nicht.
;    Definieren Sie die Funktionen ACCEPTOR-S zur Interpretation eines Akzeptors auf der Basis der 
;    vorher definierten Funktionen 
;    Z1-SUCHE und A-SUCHE mit den Eingabeparametern:
; 	XL = eingabewort
; 	Z0 = Anfangszustand
; 	TACC = Zustand�bergangstabelle 
; 	AACC = Akzeptierungstabelle 
; 	Der R�ckgabewert der Funktion ist ACCEPT wenn XL akzeptiert wird sonst NOACC
(DEFUN ACCEPTOR-S (XL Z0 TACC AACC);-> {ACCEPT, NOACC}
(?????????????????????))
;------------------------------------------------------------------------------------------------
, Beispiele f�r die Anwendung der Funktion ACCEPTOR-S
;------------------------------------------------------------------------------------------------
(ACCEPTOR-S '(A B A A B B) 0 TACC AACC);-> ACCEPT
(ACCEPTOR-S '(A B A A B B B) 0 TACC AACC);-> NOACC

; Hinweis: F�r die Ausgabe der Zwischenergebnisse kann die formatierte Ausgabe benutzt werden z.b: (FORMAT T "~%; Z = ~A   X = ~A" Z (CAR XL))
; Die Formatfunktion ist genauso aufgebaut wie die formatierte Ausgabe in C printf:
;(FORMAT T <controlstring> <arg1> <arg2> ... )
; 
;Dann erh�lt man die Ausgaben:
(ACCEPTOR-S '(A B A A B B) 0 TACC AACC)
; Z = 0   X = A
; Z = 1   X = B
; Z = 2   X = A
; Z = 1   X = A
; Z = 3   X = B
; Z = 2   X = B
; Z = 4   X = NIL
; ACCEPT
(ACCEPTOR-S '(A B A A B B B) 0 TACC AACC)
; Z = 0   X = A
; Z = 1   X = B
; Z = 2   X = A
; Z = 1   X = A
; Z = 3   X = B
; Z = 2   X = B
; Z = 4   X = B
; Z = 5   X = NIL
;NOACC


