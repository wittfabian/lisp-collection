; Aufgabe: Endliche Automaten Teil 2
;------------------------------------------------------------------------------------------------
; Nachdem im Teil 1 der Interpreter für Akzeptoren implemetiert wurde
; sollen im Teil 2 Interpreter für Transduktoren implementiert werden.
; Der wesentliche Unterschied zwischen der IMplemetierung eines 
; Akzeptors und der eines  Transduktors besteht darin, daß der Akzeptor
; einen Wert zurückgibt der Transduktor eine Wertesequenz - ein Wort.
;------------------------------------------------------------------------------------------------
; Gegeben ist folgen Steuertabelle eines  MOORE AUTOMATen
;************************************************************************************************/
;  | a | b |
;--+---+---+---
;0 | 1 | 2 | X
;1 | 3 | 2 | Y
;2 | 1 | 4 | X
;3 | 5 | 2 | Y
;4 | 1 | 5 | X
;5 | 5 | 5 | Y
;------------------------------------------------------------------------------------------------
; 1. Kodieren Sie die Zustandübergangstabelle MOORET als Liste von zweifach geschachtelten Punktpaaren:
;    (((Zn . Xn) . Zn+1) ...). Für die gegebenen Automatentabelle der erste Eintrag Ausgangszustand = 0,
;    Eingangsbuchstabe = a und Folgezustand = 1 den Eintrag  (((0 . a) . 1) ...)
;    Für das Beispiel enthält die Liste ZustandsanzahlxEingangsbuchstabenanzahl = 12 Einträge
;    Aus dieser Liste kann mit Hilfe einer Suchfunktion1 für einen gegebenen Zustand und einnen
;    gegebenen Eingangsbuchstaben einfach der Folgezustand ermittelt werden
;------------------------------------------------------------------------------------------------
; 2. Kodieren Sie die Ausgabefunktion AUSMOORE als Liste von Punktpaaren
;     ((Zn . Yn) ...)
;    Aus dieser Liste kann mit Hilfe einer Suchfunktion2 der Ausgabebuschstaben für einen gegebenen 
;    Zustand ermittelt werden.
;    Für das Beispiel enthält die Liste = 6 Einträge
; Die durch die Kodierung erhaltenen Listen werden mit SETF an die Symbole (Variablen) TACC bzw. AACC
; gebunden: 
; (SETF MOORET  '(((0 . a) . 1) ...))
; (SETF AUSMOORE  '((0 . X) ...))    
;------------------------------------------------------------------------------------------------
; 3. Definieren Sie die Funktionen MOORE zur Interpretation eines Moore-Automaten 
; 	XL = eingabewort
; 	Z0 = Anfangszustand
; 	MOORET = Zustandübergangstabelle 
; 	AUSMOORE = Tabelle der Ausgabefunktion
; 	Der Rückgabewert der Funktion ist das durch den Automaten erzeugte Ausgabewort
;------------------------------------------------------------------------------------------------
; Beispiele für die Anwendung der Funktion ACCEPTOR-S
;------------------------------------------------------------------------------------------------
(MOORE '(A B A A B B) 0 TMOORE AUSMOORE);->   (X Y X Y Y X X)
(MOORE '(A B A A B B B) 0 TMOORE AUSMOORE);-> (X Y X Y Y X X Y)
;------------------------------------------------------------------------------------------------
; Hinweis: Für die Ausgabe der Zwischenergebnisse kann die formatierte Ausgabe benutzt werden z.b: (FORMAT T "~%; Z = ~A   X = ~A" Z (CAR XL))
; Die Formatfunktion ist genauso aufgebaut wie die formatierte Ausgabe in C printf:
;(FORMAT T <controlstring> <arg1> <arg2> ... )
; 
;Dann erhält man die Ausgaben:
(PMOORE '(A B A A B B) 0 TMOORE AUSMOORE);->   
; Z = 0  Y = X  X = A
; Z = 1  Y = Y  X = B
; Z = 2  Y = X  X = A
; Z = 1  Y = Y  X = A
; Z = 3  Y = Y  X = B
; Z = 2  Y = X  X = B
;(X Y X Y Y X X)

(PMOORE '(A B A A B B B) 0 TMOORE AUSMOORE);-> 
; Z = 0  Y = X  X = A
; Z = 1  Y = Y  X = B
; Z = 2  Y = X  X = A
; Z = 1  Y = Y  X = A
; Z = 3  Y = Y  X = B
; Z = 2  Y = X  X = B
; Z = 4  Y = X  X = B
;(X Y X Y Y X X Y)
;------------------------------------------------------------------------------------------------
;4. Entwerfen Sie die Automatentabelle des in der Vorlesung behandelten Mealy-Automaten
;    zur Verteilung der Münzen aus 2 Eingabeschächten A, B in 2 Ausgabeschächte C, D.

; Z | Z0 | Z1 | Z2
;---+----+----+----
;Z0 | 0  | 0  | 0
;Z1 | 1  | 0  | 0
;Z2 | 0  | 1  | 0
;Z3 | 1  | 1  | 0
;------------------------------------------------------------------------------------------------
;5. Kodieren Sie die Automatentabelle als Liste von zweifach geschachtelten Punktpaaren:
;    (((Zn . Xn) . (Zn+1 . Yn))  ...).
;------------------------------------------------------------------------------------------------
; 6. Definieren Sie die Funktionen MEALY zur Interpretation eines Mealy-Automaten 
; 	XL = eingabewort
; 	Z0 = Anfangszustand
; 	TMEALY = Zustandübergangstabelle mit Ausgabe
; 	Der Rückgabewert der Funktion ist das durch den Automaten erzeugte Ausgabewort
;------------------------------------------------------------------------------------------------
; Testen Sie die konstruierte Automatentabelle mit der Funktion MEALYT
;
; (MEALYT '(A B B B A A B B A B A) 0 MUAU);-> 
; Z = 0 X = A  Y = C
; Z = 1 X = B  Y = C
; Z = 7 X = B  Y = D
; Z = 3 X = B  Y = D
; Z = 5 X = A  Y = C
; Z = 6 X = A  Y = C
; Z = 7 X = B  Y = D
; Z = 3 X = B  Y = D
; Z = 5 X = A  Y = C
; Z = 6 X = B  Y = D
; Z = 2 X = A  Y = C
;(C C D D C C D D C D C)

