;****************************************************************************************************************
;****************************************************************************************************************
; AUFGABE: DEFINIEREN SIE EINEN UNIVERSELLEN tabellengesteuerten INTERPRETER für Turingmaschinen 
;****************************************************************************************************************
; DAZU BENÖTIGT MAN EINE DATENSTRUKTUR ZUR DARSTELLUNG DER STEUERTABELLE UND EINE DATENSTRUKTUR 
; ZUR DARSTELLUNG DES ARBEITSBANDES DER TM
;****************************************************************************************************************
; BEISPIEL: STEUERTABELLE einer TM zur Translation unärer Zahlendarstellungen in binäre
;----------------------------------------------------------------------------------------------------------------
;        |         Eingabesymbole          
;    Zu- +------+------+------+------+------+
;   stand|  '$' |  'I' |  'X' |  '0' | '1'  |
;   -----+------+------+------+------+------+
;     0  | H,-,-| 1,X,L| 0,-,R| 0,-,R| 0,-,R|
;   -----+------+------+------+------+------+
;     1  | 0,1,R|   -  | 1,-,L| 0,1,R| 1,0,L|
;----------------------------------------------------------------------------------------------------------------
; ALS DATENSTRUKTUR ZUR DARSTELLUNG DER QUINTUPEL (Z X Z' Y D) DER STEUERTABELLE WERDEN EINE LISTE 
; VON GESCHACHTELTEN PUNKTPAARE (SIEHE INTERPRETER KA)BENUTZT: ((Z . X) . (Z' Y D)): 
; FÜR DIE BEISPIEL TM ERHÄLT MAN DAMIT:
;----------------------------------------------------------------------------------------------------------------
(SETF UNBIN '(((0 . $) H - -) ((0 . I) 1 X L) ((0 . X) 0 - R) ((0 . 0) 0 - R) ((0 . 1) 0 - R)
              ((1 . $) 0 1 R) ((1 . I) - - -) ((1 . X) 1 - L) ((1 . 0) 0 1 R) ((1 . 1) 1 0 L)))
;****************************************************************************************************************
;DATENSTRUKTUR ZUR DARSTELLUNG DES BANDES  ((LINKS-VOM-KOPF) . (UNTER-DEM-KOPF . (RECHTS-VOM-KOPF)))
;Dabei ist der Zusammenhang zwischen Punktpaaren und Listen zu beachten: ((A B C) . (D E F)) = ((A B C) D E F)
;Damit steht der Kopf auf dem ersten Zeichen nach der Liste, die das CAR des Bandes ist 
;----------------------------------------------------------------------------------------------------------------
; BEISPIEL: BAND = ((1 2 3 4) 5 6 7 8 9):  KOPF STEHT AUF POSITION 5
;----------------------------------------------------------------------------------------------------------------
; Mit diese Datenstruktur ist der ZUgriff aus das vom (simulierten) Schreib-und Leskopf gelesene Zeichen sehr 
; einfach mit (CADR BAND) = (CAR (CDR BAND))
; (CADR '(($ $ $ $) X I I I I I I $));-> X 
;****************************************************************************************************************
;****************************************************************************************************************
; Die zu definierende interpreterfunktion TMINT benutzt weitere Funktionen
;
;                                            TMINT
;						|
;				+---------------+---------------+
;				|				|
;			     TRANS			     AUSGABE
;						+---------------+---------------+
;						|				|
;                			     AUSGABEL			    AUSGABER
;****************************************************************************************************************
; FUNKTION TRANS ZUR BESTIMMUNG DES FOLGEZUSTANDES, DES ZU SCHREIBENDEN ZEICHENS UND DER BANDBEWEGUNG
; (Entspricht den analogen Funktionen für Atomat und Kellerautomat)
;----------------------------------------------------------------------------------------------------------------
(DEFUN TRANS (ZUSTAND BAND STEUERTABELLE)
?????????????????????????????????????????????????????????????)
;----------------------------------------------------------------------------------------------------------------
(TRANS 0 '(($ $ $ $) I I I I I I I $) UNBIN);-> (1 X L)
;****************************************************************************************************************
; FUNKTION AUSGABE ZUR BANDMANIPULATION: X= zu schreibendes Zeichen, D = {L,R} = Bandbewegung
;----------------------------------------------------------------------------------------------------------------
(DEFUN AUSGABE (BAND X D)
??????????????????????????????????????????????????????????????????????????????????)
;----------------------------------------------------------------------------------------------------------------
(AUSGABE '(($ $ $ $) I I I I I I I $) 'X 'L);-> (($ $ $) $ X I I I I I I $)
;----------------------------------------------------------------------------------------------------------------
; Die Funktion zur Bandmanipulation (Schreiben eines neuen Zeichens und Bewegung des Schreib-Lese-Kopfes) benutzt
; zwei weitere Funktionen in Abhängigkeit, ob die Kopfbewegung nach links oder rechts erfolgt.
;----------------------------------------------------------------------------------------------------------------
; Beispiel: Zum Schreiben von X und Bewegen des Kopfes nach links dient der folgende Ausdruck:
;----------------------------------------------------------------------------------------------------------------
(CONS (REVERSE (CDR (REVERSE (CAR BAND)))) (CONS (CAR (REVERSE (CAR BAND))) (CONS 'X (CDDR BAND))))
; bzw. bei einem konkreten Bandinhalt
(CONS (REVERSE (CDR (REVERSE (CAR '((1 2 3 4) 5 6 7))))) (CONS (CAR (REVERSE (CAR '((1 2 3 4) 5 6 7)))) (CONS 'X (CDDR '((1 2 3 4) 5 6 7)))))
;-> ((1 2 3) 4 X 6 7) 
;----------------------------------------------------------------------------------------------------------------
; Das Schreiben eines Zeichens und Bewegung des Kopfes nach Rechts ist einFacher - an das CAR des Bandes wird
; das zu schreibende Zeichen angehängt durch UMdrehung mit REVERSE und CONS und danach wieder Umdrehen: 
;----------------------------------------------------------------------------------------------------------------
; Beispiel: Zum Schreiben von X und Bewegen des Kopfes nach rechts:
(REVERSE (CONS 'X (REVERSE (CAR '((1 2 3 4) 5 6 7))))); -> (1 2 3 4 X)
;Jetzt muß nur noch das CDR des CDR des Bandes angehangen werden:
(CONS (REVERSE (CONS 'X (REVERSE (CAR '((1 2 3 4) 5 6 7))))) (CDDR '((1 2 3 4) 5 6 7))); -> ((1 2 3 4 X) 6 7)
;****************************************************************************************************************
; INTERPRETERFUNKTION
;----------------------------------------------------------------------------------------------------------------
(DEFUN TMINT (ZUSTAND BAND STEUERTAB)
(FORMAT T "~%;Z:~A   ~A" ZUSTAND BAND)
????????????????????????????????????????????????????)
;----------------------------------------------------------------------------------------------------------------
(TMINT 0 '(($ $ $ $ $ $) I I I I I I $) UNBIN)

;Z:0   (($ $ $ $ $ $) I I I I I I $)
;Z:1   (($ $ $ $ $) $ X I I I I I $)
;Z:0   (($ $ $ $ $ 1) X I I I I I $)
;Z:0   (($ $ $ $ $ 1 X) I I I I I $)
;Z:1   (($ $ $ $ $ 1) X X I I I I $)
;Z:1   (($ $ $ $ $) 1 X X I I I I $)
;Z:1   (($ $ $ $) $ 0 X X I I I I $)
;Z:0   (($ $ $ $ 1) 0 X X I I I I $)
;Z:0   (($ $ $ $ 1 0) X X I I I I $)
;Z:0   (($ $ $ $ 1 0 X) X I I I I $)
;Z:0   (($ $ $ $ 1 0 X X) I I I I $)
;Z:1   (($ $ $ $ 1 0 X) X X I I I $)
;Z:1   (($ $ $ $ 1 0) X X X I I I $)
;Z:1   (($ $ $ $ 1) 0 X X X I I I $)
;Z:0   (($ $ $ $ 1 1) X X X I I I $)
;Z:0   (($ $ $ $ 1 1 X) X X I I I $)
;Z:0   (($ $ $ $ 1 1 X X) X I I I $)
;Z:0   (($ $ $ $ 1 1 X X X) I I I $)
;Z:1   (($ $ $ $ 1 1 X X) X X I I $)
;Z:1   (($ $ $ $ 1 1 X) X X X I I $)
;Z:1   (($ $ $ $ 1 1) X X X X I I $)
;Z:1   (($ $ $ $ 1) 1 X X X X I I $)
;Z:1   (($ $ $ $) 1 0 X X X X I I $)
;Z:1   (($ $ $) $ 0 0 X X X X I I $)
;Z:0   (($ $ $ 1) 0 0 X X X X I I $)
;Z:0   (($ $ $ 1 0) 0 X X X X I I $)
;Z:0   (($ $ $ 1 0 0) X X X X I I $)
;Z:0   (($ $ $ 1 0 0 X) X X X I I $)
;Z:0   (($ $ $ 1 0 0 X X) X X I I $)
;Z:0   (($ $ $ 1 0 0 X X X) X I I $)
;Z:0   (($ $ $ 1 0 0 X X X X) I I $)
;Z:1   (($ $ $ 1 0 0 X X X) X X I $)
;Z:1   (($ $ $ 1 0 0 X X) X X X I $)
;Z:1   (($ $ $ 1 0 0 X) X X X X I $)
;Z:1   (($ $ $ 1 0 0) X X X X X I $)
;Z:1   (($ $ $ 1 0) 0 X X X X X I $)
;Z:0   (($ $ $ 1 0 1) X X X X X I $)
;Z:0   (($ $ $ 1 0 1 X) X X X X I $)
;Z:0   (($ $ $ 1 0 1 X X) X X X I $)
;Z:0   (($ $ $ 1 0 1 X X X) X X I $)
;Z:0   (($ $ $ 1 0 1 X X X X) X I $)
;Z:0   (($ $ $ 1 0 1 X X X X X) I $)
;Z:1   (($ $ $ 1 0 1 X X X X) X X $)
;Z:1   (($ $ $ 1 0 1 X X X) X X X $)
;Z:1   (($ $ $ 1 0 1 X X) X X X X $)
;Z:1   (($ $ $ 1 0 1 X) X X X X X $)
;Z:1   (($ $ $ 1 0 1) X X X X X X $)
;Z:1   (($ $ $ 1 0) 1 X X X X X X $)
;Z:1   (($ $ $ 1) 0 0 X X X X X X $)
;Z:0   (($ $ $ 1 1) 0 X X X X X X $)
;Z:0   (($ $ $ 1 1 0) X X X X X X $)
;Z:0   (($ $ $ 1 1 0 X) X X X X X $)
;Z:0   (($ $ $ 1 1 0 X X) X X X X $)
;Z:0   (($ $ $ 1 1 0 X X X) X X X $)
;Z:0   (($ $ $ 1 1 0 X X X X) X X $)
;Z:0   (($ $ $ 1 1 0 X X X X X) X $)
;Z:0   (($ $ $ 1 1 0 X X X X X X) $)
;Z:H   (($ $ $ 1 1 0 X X X X X X) ($))
";HALTEZUSTAND"
