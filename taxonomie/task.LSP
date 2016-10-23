;************************************************************************************************************************************
;************************************************************************************************************************************
;************************************************************************************************************************************
;************************************							           **********************************
;********************************                          UEBUNG 6: TAXONOMIE 	                        *****************************
;********************************                             	                                        *****************************
;************************************							           **********************************
;************************************************************************************************************************************
;************************************************************************************************************************************
;************************************************************************************************************************************
(SETQ *PRINT-LENGTH*)
;************************************************************************************************************************************
; Aufgabe 1: Gegeben ist eine Klassenhierarchie (Taxonomie) von Lebewesen mit Einfachvererbung = Baum (siehe uebung6.doc)
;            Diese Taxonomie ist mit einer Eigenschaftsliste darzustellen.  Die Eigenschaftsliste benutzt die Zuordnung:
;                       Attribut = Unterklasse
;                       Attributwert = Oberklasse
;************************************************************************************************************************************
; Test der Eigenschaftsliste:
;------------------------------------------------------------------------------------------------------------------------------------
(SYMBOL-PLIST 'LEBEWESEN); -> (MEHRZELLER LEBEWESEN EINZELLER ...)
(GET 'LEBEWESEN 'MENSCH);-> PRIMAT
;************************************************************************************************************************************
; Aufgabe 2: Suche in der Taxonomie
;            Es ist eine Funktion (DEFUN ISA (TAXONOMIE U O N) ... ) 
;            zu definieren, die in der gegebenen Taxonomie TAXONOMIE zu einer gegebenen Unterklasse U feststellt, ob diese zu der
;            Oberklasse O gehört und dabei die Anzahl N der Ebenen zwischen U und O nicht überschritten wird.
;            Existiert eine solche Beziehung zwischen U und O dann wird T zurückgegeben ansonsten NIL.
;            Wenn U oder O nicht in der Taxonomie vorkommen so ist eine entsprechende Meldung auszugeben.
;************************************************************************************************************************************
; Test der Funktion:
;------------------------------------------------------------------------------------------------------------------------------------
(ISA 'LEBEWESEN 'MENSCH 'SAEUGETIER 3); -> T
(ISA 'LEBEWESEN 'MENSCH 'TIER 3); -> NIL
(ISA 'LEBEWESEN 'MENSCH 'TIER 5); -> T
(ISA 'LEBEWESEN 'SCHMETTERLING 'FRAU 3); -> UNBEKANNTE-OBERKLASSE
(ISA 'LEBEWESEN 'MANN 'AMEISE 3); -> UNBEKANNTE-UNTERKLASSE
;************************************************************************************************************************************
; Aufgabe 3: Taxonomie der Klassen mit Eigenschaften
;            Die Klassen der Taxonomie sollen mit Eigenschaften ergänzt werden. Siehe dazu Tabelle in uebung6.doc.
;            Ergänzen Sie die für einige Lebewesen fehlenden Eigenschaften aus dem Netz
;            Dabei soll bei Abfrage die Eigenschaften einer Klasse aus den speziellen Eigenschaften der Klasse und den vererbten
;            Eigenschaften der Oberklasse zurückgegeben werden.
;            Dazu erhält jedes Attribut als Wert eine Liste, die an erster Stelle die Oberklasse enthält und an zweiter Stelle die
;            Eigenschaften in einer Liste. 
;            Konstruieren Sie die Eigenschaftsliste LW-M-MERKMAL.
;************************************************************************************************************************************
; Eine natürlichsprachliche Ausgabe der ersten Eigenaschaft der unmittelbar der jeweiligen Klasse zugeschriebenen Eigenschaftsliste
; nimmt die Funktiom EINFACHE-EIGENSCHAFT vor.  Diese Funktion kann zum Testen verwendet werden.  
;------------------------------------------------------------------------------------------------------------------------------------
(DEFUN EINFACHE-EIGENSCHAFT (TAXONOMIE KLASSE)
(FORMAT T "DER/DIE/DAS ~S IST EIN(E) ~S UND HAT ~S."
             KLASSE (CAR (GET TAXONOMIE KLASSE '(UNBEKANNTE-KLASSE))) (CAADR (GET TAXONOMIE KLASSE '(X (UNBEKANNTE-EIGENSCHAFTEN))))))
;------------------------------------------------------------------------------------------------------------------------------------
(EINFACHE-EIGENSCHAFT 'LW-M-MERKMAL 'MENSCH);-> DER/DIE/DAS MENSCH IST EIN PRIMAT UND HAT INTELLIGENZ.
(EINFACHE-EIGENSCHAFT 'LW-M-MERKMAL 'AMEISE);-> DER/DIE/DAS AMEISE IST EIN INSEKT UND HAT FAEHIGKEIT-ZUR-STAATEN-BILDUNG.
(EINFACHE-EIGENSCHAFT 'LW-M-MERKMAL 'MEISE);->  DER/DIE/DAS MEISE IST EIN(E) UNBEKANNTE-KLASSE UND HAT UNBEKANNTE-EIGENSCHAFTEN.
;************************************************************************************************************************************
;************************************************************************************************************************************
; Aufgabe 4: Definieren Sie für die Beantwortung von Frage nach den Eigenschaften einer Klasse einschließlich den Eigenschaften der
;            Oberklassen eine Funktion (DEFUN HAS (TAXOMIE K) ... ), 
;            die die in der gegebenen Taxonomie mit Eigenschaften TAXOMIE für eine gegebenen Klasse K deren Eigenschaften
;            einschließlich den Eigenschaften ihrer Oberklassen zurückgibt.
;************************************************************************************************************************************
; Lösung:
;------------------------------------------------------------------------------------------------------------------------------------
(DEFUN HAS (TAXONOMIE K)
  (COND ((NOT (MEMBER K (SYMBOL-PLIST TAXONOMIE))) 'UNBEKANNTE-KLASSE)
        ((NOT (GET TAXONOMIE (CAR (GET TAXONOMIE K)))) (CADR (GET TAXONOMIE K))); 
        (T (APPEND (CADR (GET TAXONOMIE K)) (HAS TAXONOMIE (CAR (GET TAXONOMIE K)))))))
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
(HAS 'LW-M-MERKMAL 'MENSCH);-> (INTELLIGENZ ALLESFRESSER LUNGEN MAMMA SKELETT WIRBELSAEULE BEWEGUNG ZELLSPEZIALISIERUNG WACHSTUM VEMEHRUNG) 
(HAS 'LW-M-MERKMAL 'AMEISE);-> (6-BEINE AUSSENSKELETT KEINE-WIRBELSAEULE BEWEGUNG ZELLSPEZIALISIERUNG WACHSTUM VEMEHRUNG)
(HAS 'LW-M-MERKMAL 'MANN);-> UNBEKANNTE-KLASSE
;************************************************************************************************************************************
; Aufgabe 5: Definieren Sie eine Funktion (DEFUN ANTWORT (FRAGE TAXONOMIE) ...), die unter Benutzung der Funktion ISA (Aufgabe 2)
; Fragen in Form von Listen der beiden folgenden Typen beantwortet: 
; 1. (WAS IST EIN X)
; 2: (IST EIN X EIN Y)
;************************************************************************************************************************************
; TESTBEISPIELE
;--------------------------------------------------------------------------------------------------------
(ANTWORT '(WAS IST EIN HAI) 'LEBEWESEN); --> (EIN HAI IST EIN FISCH)            
(ANTWORT '(WAS IST EIN FUCHS) 'LEBEWESEN); --> (EIN FUCHS IST EIN UNBEKANNTES-LEBEWESEN)            
(ANTWORT '(WAS IST EIN PILZ) 'LEBEWESEN); --> (EIN PILZ IST EIN MEHRZELLER)
(ANTWORT '(WAS IST EIN X) 'LEBEWESEN); --> (EIN X IST EIN UNBEKANNTES-LEBEWESEN)             
(ANTWORT '(WAS IST EIN HAI FISCH) 'LEBEWESEN); --> (FRAGE IST UNVERSTAENDLICH)  
(ANTWORT '() 'LEBEWESEN); --> (FRAGE IST UNVERSTAENDLICH)
(ANTWORT '(EIN HAI IST WAS) 'LEBEWESEN); --> (FRAGE IST UNVERSTAENDLICH)            
(ANTWORT '(IST EIN HAI EIN FISCH) 'LEBEWESEN); --> (JA EIN HAI IST EIN FISCH)   
(ANTWORT '(IST EIN HAI EIN WIRBELTIER) 'LEBEWESEN); --> (JA EIN HAI IST EIN WIRBELTIER)   
(ANTWORT '(IST EIN PILZ EIN FISCH) 'LEBEWESEN); -->  (NEIN EIN PILZ IST KEIN FISCH) 
(ANTWORT '(IST EIN PILZ EIN PILZ) 'LEBEWESEN); --> (JA EIN PILZ IST EIN PILZ)
(ANTWORT '(IST EIN PILZ EIN Y) 'LEBEWESEN); --> (EIN Y IST EIN UNBEKANNTES LEBEWESEN)
(ANTWORT '(IST EIN X EIN PILZ) 'LEBEWESEN); --> (EIN X IST EIN UNBEKANNTES LEBEWESEN) 
(ANTWORT '(IST EIN X EIN Y) 'LEBEWESEN); --> (X UND Y SIND UNBEKANNTE LEBEWESEN) 
