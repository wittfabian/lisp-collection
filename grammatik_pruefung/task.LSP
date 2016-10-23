;****************************************************************************************************************
;****************************************************************************************************************
;****************************************************************************************************************
;*********************************                                              ********************************* 
;*********************************                 NL-SYNTAXANALYSE             *********************************
;*********************************                                              ********************************* 
;****************************************************************************************************************
;****************************************************************************************************************
;****************************************************************************************************************
; GEGEBEN IST DIE FOLGENDE LL(1)-GRAMMATIK EINFACHER DEUTSCHER AUSSAGESÄTZE
;----------------------------------------------------------------------------------------------------------------
;                  GRAMMATIKREGELN                                               ENTSCHEIDUNGSMENGEN
;----------------------------------------------------------------------------------------------------------------
;<satz> 			--> <nominalphrase> <verbalphrase>	{der, den, die, ein, einen, eine, max, moritz, informatik, student,  hase, hasen, mohrruebe}
;<nominalphrase> 	--> <artikel> <nomem>		        {der, den, die, ein, einen, eine}
;<nominalphrase> 	--> <nomen>			 				{informatik, student,  hase, hasen, mohrruebe}
;<nominalphrase> 	--> <eigenname> 					{max, moritz}
;<verbalphrase>		--> <verb> <rest> 					{mag, studiert, isst, SCHLAEFT, SIEHT}
;<rest>				--> <nominalphrase>	                {der, den, die, ein, einen, eine, max, moritz}
;<rest>				-->					{EKZ}
;----------------------------------------------------------------------------------------------------------------
;                  LEXIKALISCHE REGELN                                               
;----------------------------------------------------------------------------------------------------------------
;<artikel>		--> der 
;<artikel>		--> den
;<artikel>		--> die
;<artikel>		--> ein
;<artikel>		--> einen
;<artikel>		--> eine
;<nomen>		--> student 
;<nomen>		--> studenten 
;<nomen>		--> informatik
;<nomen>		--> hase 
;<nomen>		--> hasen 
;<nomen>		--> mohrruebe
;<nomen>		--> mohrrueben
;<eigenname>	--> max
;<eigenname> 	--> moritz
;<verb> 		--> mag
;<verb> 		--> moegen
;<verb> 		--> studiert
;<verb> 		--> studieren
;<verb> 		--> isst
;<verb> 		--> essen
;****************************************************************************************************************
; Die lexikalische Regeln werden zur Überprüfungen bezüglich der Entscheidungsmengen einer Regel benötigt.
; Deswegen werden die Worte gleicher Wortarten in Listen zusammengefasst:
;----------------------------------------------------------------------------------------------------------------
;  LEXIKALISCHE DEFINITIONEN
;----------------------------------------------------------------------------------------------------------------
(SETF LEX-ARTIKEL '(DER DEN DIE EIN EINE EINEN))
(SETF LEX-NOMEN '(STUDENT STUDENTEN INFORMATIK HASE HASEN MOHRRUEBE MOHRRUEBEN))
(SETF LEX-EIGENNAME '(MAX MORITZ))
(SETF LEX-VERB '(MAG MOEGEN STUDIERT STUDIEREN ISST ESSEN SCHLAEFT SCHLAFEN SIEHT SEHEN))
;****************************************************************************************************************
; AUFGABE 1: DEFINIEREN SIE EINE FUNKTION PARSER, DIE UNTER BENUTZUNG DER FUNKTIONEN FÜR DIE NT-SYMBOLE NACH DER
;            MÉTHODE DES REKURSIVEN ABSTIEGS EINE SYNTAXANALYSE MIT FEHLERMELDUNGEN REALISIERT. 
;----------------------------------------------------------------------------------------------------------------
; HINWEIS 1: ZUR GENERIERUNG DER FEHLERMELDUNGEN KÖNNEN SIE DIE FUNKTION STRING-APPEND BENUTZEN, DIE BELIEBIG
;            VIELE STRINGS VERKETTET.
; HINWEIS 2: UM EIN SYMBOL WIE EIN TERMINALSYMBOL IN EINEN STRING UMZUWANDELN KÖNNEN SIE DIE FUNKTION 
;            SYMBOL-NAME BENUTZEN, DIE DEN NAMEN DES SYMBOLS, DAS QUOTIERT SEIN MUSS, ZURÜCKGIBT.
;****************************************************************************************************************
;                                  LÖSUNG
;****************************************************************************************************************
(DEFUN PARSER (L) ....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN SATZ (L) ....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN NOMINALPHRASE (L) ....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN VERBALPHRASE (L)....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN RESTPHRASE (L)....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN ARTIKEL (L)....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN nomen (L)....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN EIGENNAME (L)....)
;----------------------------------------------------------------------------------------------------------------
(DEFUN VERB (L)....)
;----------------------------------------------------------------------------------------------------------------
;Wandelt Liste in String um:
;----------------------------------------------------------------------------------------------------------------
(DEFUN LIST-TO-STRING (LIST)
(COND ((NULL (CDR LIST)) (PRINC-TO-STRING (CAR LIST)))
      (T (STRING-APPEND (PRINC-TO-STRING (CAR LIST)) " " (LIST-TO-STRING (CDR LIST))))))
;----------------------------------------------------------------------------------------------------------------
(LIST-TO-STRING '(A B C));-> "A B C"
;****************************************************************************************************************
;                                                    TESTEN
;****************************************************************************************************************
(UNTRACE PARSER SATZ NOMINALPHRASE VERBALPHRASE RESTPHRASE ARTIKEL nomen EIGENNAME VERB)
;----------------------------------------------------------------------------------------------------------------
(PRINT '---------------------------------------------KORREKT-----------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER '(EIN STUDENT STUDIERT INFORMATIK))	;-> ACCEPT
(PARSER '(DER STUDENT STUDIERT INFORMATIK))	;-> ACCEPT
(PARSER '(MAX STUDIERT INFORMATIK))		;-> ACCEPT
(PARSER '(EIN STUDENT STUDIERT DIE INFORMATIK))	;-> ACCEPT
(PARSER '(DER STUDENT STUDIERT DIE INFORMATIK))	;-> ACCEPT
(PARSER '(MAX STUDIERT DIE INFORMATIK))		;-> ACCEPT
(PARSER '(MORITZ MAG INFORMATIK))		;-> ACCEPT
(PARSER '(MAX MAG MORITZ))			;-> ACCEPT
(PARSER '(DER HASE STUDIERT DIE MOHRRUEBE))	;-> ACCEPT
(PARSER '(MAX STUDIERT DEN HASEN))		;-> ACCEPT
(PARSER '(MORITZ SCHLAEFT))                     ;-> ACCEPT
;----------------------------------------------------------------------------------------------------------------
(PRINT '-----------------------------------------KONGRUENZFEHLER------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER '(DIE HASE STUDIERT DIE STUDENT))	;-> ACCEPT
(PARSER '(DIE HASE SCHLAEFT DIE STUDENT))	;-> ACCEPT
(PARSER '(STUDENT STUDIERT DEN MOHRRUEBE));-> ACCEPT
(PARSER '(DEN STUDENT STUDIERT DEN MOHRRUEBE));-> ACCEPT
(PARSER '(DER MOHRRUEBE SIEHT DEN STUDENT));-> ACCEPT
(PARSER '(DIE MOHRRUEBE SIEHT DER STUDENT));-> ACCEPT
(PARSER '(DEN MOHRRUEBE SIEHT DIE STUDENT));-> ACCEPT
;----------------------------------------------------------------------------------------------------------------
(PRINT '------------------------------------------SYNTAXFEHLER--------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER '())				;-> nomen-FEHLT
(PARSER '(EIN))				;-> nomen-FEHLT
(PARSER '(STUDENT))			;-> VERBALPHRASE-FEHLT
(PARSER '(STUDENT HASE))		;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: HASE" 
(PARSER '(EIN STUDENT STUDIERT DER))	;-> nomen-FEHLT
(PARSER '(DER STUDENT MAG STUDIERT))	;-> "NOMINALPHRASE BEGINNT NICHT MIT ARTIKEL nomen ODER EIGENNAME: STUDIERT"
(PARSER '(STUDIERT MAX INFORMATIK))	;-> "SATZ BEGINNT NICHT MIT EINER NOMINALPHRASE: STUDIERT"
(PARSER '(MORITZ INFORMATIK MAG))	;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: INFORMATIK"
(PARSER '(MAX MAG MORITZ STUDIERT))	;-> "Das Wort STUDIERT ist ueberfluessig"
(PARSER '(MAX STUDIERT DIE ASTRONOMIE)) ;-> "DAS WORT ASTRONOMIE IST KEIN nomen"
(PARSER '(MAX INFORMATIK SCHLAEFT))     ;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: INFORMATIK" 
;****************************************************************************************************************
; AUFGABE 2: Diese Lösung mit rekursivem Abstieg, bei dem die einzelnen Funktionen jeweils nur die nach der
;            Bearbeitung übriggebliebene Liste zurückgeben ruft einige Probleme hervor:
;----------------------------------------------------------------------------------------------------------------
; 1. Schwierigkeiten bei der Erzeugung der Fehlermeldung 
; 2. Unmöglichkeit der Kontrolle der Kongruenz
; 3. Unmöglichkeit der Rückgabe eines Syntaxbaumes
; 4. Unmöglichkeit der Rückgabe ders Semantik
;****************************************************************************************************************
; Diese Mängel lassen sich beheben, wenn als Rückgabewert keine Liste sondern ein STRUCT benutzt wird, dessen
; Slots unterschiedliche Werte zurückgeben können. 
; Um Fehlermelduungserzeugung leichter zu machen und eine Kontrolle der Kongruenz zu ermöglichen wird eine 
; STRUCT-Definition mit 3 Slots empfohlen. Eine Erweiterung zur Erzeungung von Syntaxbaum und Semantik ist dann
; leicht möglich.
; Die Slots sind PHRASE CGN ERROR. CGN ist eine Liste die die Werte von CASUS, GENUS und NUMERUS enthält, Die 
; Zusammenfassung von CASUS, GENUS und NUMERUS gestattet Mehrfachkombinationen ihrer Werte, was durch die 
; Nichteindeutigkeit vieler Worte, insbesondere der Artikel und Pronomen notwendig ist. So gehören zu dem Artikel
; DIE die folgenden 6 Kombinationen, die Wert deas Slots CGN sind:
; '((NOM FEM SING) (AKK FEM SING) (NOM FEM PLUR) (AKK FEM PLUR) (NOM MAS PLUR) (AKK MAS PLUR)))
;----------------------------------------------------------------------------------------------------------------
; Fuer die Analyse der einfachen Satze der Uebung werden fuer CASUS nur Nominativ und Akkusativ beruecksichtigt.
;****************************************************************************************************************
; DEFINIEREN SIE EINE FUNKTION PARSER2, DIE
; 1. GENAUERE FEHLERMELDUNGEN ERZEUGT
; 2: EINE KONGRUENZPRUEFUNG VORNIMMT
;----------------------------------------------------------------------------------------------------------------
; HINWEIS 1: Wenn eine syntaktische Kategorie an der gegebenen Stelle keine Bedeutung hat, so erhaelt sie einen
;            speziellen Wert: BEL. Deswegen ist fuer den Vergleich der Werte von CGN der verschiedenen
;            Grammatiksymbole ist eine zu definierende Funktion erforderlich, da das einfache EQL nicht ausreicht.
; HINWEIS 2: Beim Uebergang von einem NT zum anderen muessen bestimmte syntaktische Kategorien verglichen werden,
;            andere dagegen nicht. Um das zu gewaehrleisten muessen zwei entsprechende Filterfunktionen definiert
;            werden.
; HINWEIS 3: Fuer die Loesung muessen alle Funktionen von Aufgabe 1 modifiziert werden, da jetzt der STRUCT als
;            Rueckgabeparameter dient. Weiterhin sind Funktionen fuer den Vergleich und fuer die Filterung der
;            CNG-Komponente des STRUCTS erforderlich.
;****************************************************************************************************************
; VORGEGEBENE TEILLOESUNG
;****************************************************************************************************************
(DEFSTRUCT PARSTYP PHRASE CGN ERROR)
;****************************************************************************************************************
; Allen Deklinations- und Konjugationsformen der Lexikoneintraege wird als Wert ein STRUCT zugeordnet, der die
; CGN-Werte fuer die jeweilige Form enthaelt
;----------------------------------------------------------------------------------------------------------------
(SETF 
;----------------------------------------------------------------------------------------------------------------
MAX    (MAKE-PARSTYP :CGN '((NOM MAS SING) (AKK MAS SING)))
MORITZ (MAKE-PARSTYP :CGN '((NOM MAS SING) (AKK MAS SING)))
;----------------------------------------------------------------------------------------------------------------
DER (MAKE-PARSTYP :CGN '((NOM MAS SING)))
DEN (MAKE-PARSTYP :CGN '((AKK MAS SING))); AUCH DATIV PLURAL
DIE (MAKE-PARSTYP :CGN '((NOM FEM SING) (AKK FEM SING) (NOM FEM PLUR) (AKK FEM PLUR) (NOM MAS PLUR) (AKK MAS PLUR)))
EIN (MAKE-PARSTYP :CGN '((NOM MAS SING))); AUCH MOM NEU SING
EINE (MAKE-PARSTYP :CGN '((NOM FEM SING) (AKK FEM SING)))
EINEN (MAKE-PARSTYP :CGN '((AKK MAS SING)))
;----------------------------------------------------------------------------------------------------------------
INFORMATIK  (MAKE-PARSTYP :CGN '((NOM FEM SING) (AKK FEM SING)))
STUDENT     (MAKE-PARSTYP :CGN '((NOM MAS SING)))
STUDENTEN   (MAKE-PARSTYP :CGN '((AKK MAS SING) (NOM MAS PLUR) (AKK MAS PLUR)))
HASE        (MAKE-PARSTYP :CGN '((NOM MAS SING)))
HASEN       (MAKE-PARSTYP :CGN '((AKK MAS SING) (NOM MAS PLUR) (AKK MAS PLUR)))
MOHRRUEBE   (MAKE-PARSTYP :CGN '((NOM FEM SING) (AKK FEM SING)))
MOHRRUEBEN  (MAKE-PARSTYP :CGN '((NOM FEM PLUR) (AKK FEM PLUR)))
;----------------------------------------------------------------------------------------------------------------
STUDIERT    (MAKE-PARSTYP :CGN '((AKK BEL SING))); BEL STEHT FUER BELIEBIG
STUDIEREN   (MAKE-PARSTYP :CGN '((AKK BEL PLUR)))
SIEHT       (MAKE-PARSTYP :CGN '((AKK BEL SING)))
SEHEN       (MAKE-PARSTYP :CGN '((AKK BEL PLUR)))
MAG         (MAKE-PARSTYP :CGN '((AKK BEL SING)))
MOEGEN      (MAKE-PARSTYP :CGN '((AKK BEL PLUR)))
ISST        (MAKE-PARSTYP :CGN '((AKK BEL SING)))
ESSEN	    (MAKE-PARSTYP :CGN '((AKK BEL PLUR)))
SCHLAEFT    (MAKE-PARSTYP :CGN '((BEL BEL SING)))
SCHLAFEN    (MAKE-PARSTYP :CGN '((BEL BEL PLUR))) 
) 
;****************************************************************************************************************
; Funktion vergleicht Einzelwerte der CNG-Komponente
;----------------------------------------------------------------------------------------------------------------
(DEFUN EINZEL-VERGL (SOLL IST) ...)
;----------------------------------------------------------------------------------------------------------------
(EINZEL-VERGL 'BEL 'NOM);-> NOM
(EINZEL-VERGL 'NOM 'NOM);-> NOM
(EINZEL-VERGL 'AKK 'NOM);-> NIL
(EINZEL-VERGL 'AKK 'BEL);-> AKK
(EINZEL-VERGL 'BEL 'BEL);-> BEL
;****************************************************************************************************************
; Funktion vergleicht 2 Listen mit allen CNG-Komponenten
;----------------------------------------------------------------------------------------------------------------
(DEFUN CGN-VERGL (SOLL-CGN IST-CGN) ...)
;----------------------------------------------------------------------------------------------------------------
(CGN-VERGL '(AKK MAS SING) '(AKK BEL SING));-> (AKK MAS SING)
(CGN-VERGL '(AKK MAS BEL) '(AKK BEL SING));-> (AKK MAS SING)
(CGN-VERGL '(AKK MAS SING) '(AKK MAS SING));-> (AKK MAS SING)
(CGN-VERGL '(AKK MAS SING) '(AKK BEL SING));-> (AKK MAS SING)
(CGN-VERGL '(BEL MAS BEL) '(AKK BEL SING));-> (AKK MAS SING)
(CGN-VERGL '(AKK MAS SING) '(AKK FEM SING));-> NIL
;****************************************************************************************************************
; Funktion vergleicht eine Liste mit allen CNG-Komponenten = SOLL-CGN mit einer Liste von Listen der CNG-Komponenten = IST-CGN-SET
;----------------------------------------------------------------------------------------------------------------
(DEFUN CGN-SET-VERGL (SOLL-CGN IST-CGN-SET) ...)
;----------------------------------------------------------------------------------------------------------------
(CGN-SET-VERGL '(NOM MAS SING) '((NOM MAS SING)));-> (NOM MAS SING)
(CGN-SET-VERGL '(NOM MAS SING) '((NOM MAS SING) (AKK MAS SING)));-> (NOM MAS SING)
(CGN-SET-VERGL '(AKK MAS SING) '((NOM MAS SING) (AKK MAS SING)));-> (AKK MAS SING)
(CGN-SET-VERGL '(AKK MAS SING) '((NOM MAS SING) (AKK BEL SING)));-> (AKK MAS SING)
(CGN-SET-VERGL '(AKK MAS SING) '((NOM FEM SING) (AKK FEM SING) (NOM FEM PLUR) (AKK FEM PLUR) (NOM MAS PLUR) (AKK MAS PLUR)));-> NIL
(CGN-SET-VERGL '(AKK MAS PLUR) '((NOM FEM SING) (AKK FEM SING) (NOM FEM PLUR) (AKK FEM PLUR) (NOM MAS PLUR) (AKK MAS PLUR)));-> (AKK MAS PLUR)
(CGN-SET-VERGL '(AKK MAS PLUR) '((BEL BEL SING)));->NIL
(CGN-SET-VERGL '(AKK MAS SING) '((BEL BEL SING)));->(AKK MAS SING)
;****************************************************************************************************************
; FUNKTION FILTERT BEIM UEBERGANG NP -> VP CASUS UND GENUS HERAUS
;----------------------------------------------------------------------------------------------------------------
(DEFUN CAS-GEN-FILTER (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
(CAS-GEN-FILTER (MAKE-PARSTYP :PHRASE '(SIEHT DEN HASEN) :CGN '(NOM MAS SING)));-> 
;                                            #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (BEL BEL SING) :ERROR NIL)
;****************************************************************************************************************
; FUNKTION FILTERT BEIM UEBERGANG VERB -> NP den NUMERUS HERAUS
;----------------------------------------------------------------------------------------------------------------
(DEFUN NUM-FILTER (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
(NUM-FILTER (MAKE-PARSTYP :PHRASE '(DEN HASEN) :CGN '(AKK BEL SING)));->  
;                                            #S(PARSTYP :PHRASE (DEN HASEN) :CGN (AKK BEL BEL) :ERROR NIL)

;****************************************************************************************************************
; PARSER
;----------------------------------------------------------------------------------------------------------------
(DEFUN PARSER2 (WORT-L) ...)

;****************************************************************************************************************
; Funktion fuer  
; <satz> --> <nominalphrase> <verbalphrase>    {der, den, die, ein, einen, eine, max, moritz, informatik,
;                                               student,  hase, hasen, mohrruebe}
;----------------------------------------------------------------------------------------------------------------
(DEFUN SATZ2 (PARS-STRUCT)  ...)
;****************************************************************************************************************
; Funktion fuer  
; <nominalphrase> --> <artikel> <nomen>	{der, den, die, ein, einen, eine}
; <nominalphrase> --> <nomen>			{informatik, student,  hase, hasen, mohrruebe}
; <nominalphrase> --> <eigenname>       {max, moritz}
;----------------------------------------------------------------------------------------------------------------
(DEFUN NOMINALPHRASE2 (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
;(NOMINALPHRASE2 (MAKE-PARSTYP :PHRASE '(MAX SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                                           #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
;(NOMINALPHRASE2 (MAKE-PARSTYP :PHRASE '(DER STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                                           #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
;(NOMINALPHRASE2 (MAKE-PARSTYP :PHRASE '(STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                                           #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
;****************************************************************************************************************
; Funktion fuer 
; <verbalphrase> --> <verb> <rest> 		{mag, studiert, isst, SCHLAEFT, SIEHT}
;----------------------------------------------------------------------------------------------------------------
(DEFUN VERBALPHRASE2 (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
;(VERBALPHRASE2 (MAKE-PARSTYP :PHRASE '(STUDIEREN INFORMATIK) :CGN '(BEL BEL PLUR) :ERROR NIL));->
;                                                         #S(PARSTYP :PHRASE NIL :CGN (AKK FEM SING) :ERROR NIL)
;****************************************************************************************************************
; Funktion fuer 
;<rest>			--> <nominalphrase>	                {der, den, die, ein, einen, eine, max, moritz}
;<rest>			-->					{EKZ}
;----------------------------------------------------------------------------------------------------------------
(DEFUN RESTPHRASE2 (PARS-STRUCT) ...)
;****************************************************************************************************************
; Funktion fuer NT <ART>
;----------------------------------------------------------------------------------------------------------------
(DEFUN ARTIKEL2 (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
(ARTIKEL2 (MAKE-PARSTYP :PHRASE '(DER STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                                                      #S(PARSTYP :PHRASE (STUDENT SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
(ARTIKEL2 (MAKE-PARSTYP :PHRASE '(DER STUDENT SIEHT DEN HASEN) :CGN '(AKK MAS SING)));->
;                                                      #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "DER steht im falschen Fall")
(ARTIKEL2 (MAKE-PARSTYP :PHRASE '(DAS HAUS STEHT IM GARTEN) :CGN '(AKK MAS SING)));->
;                                                     #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "DAS ist kein Artikel oder ist unbekannt")
;****************************************************************************************************************
; Funktion fuer NT <nomen>
;----------------------------------------------------------------------------------------------------------------
(DEFUN NOMEN2 (PARS-STRUCT)  ...)
;----------------------------------------------------------------------------------------------------------------
(NOMEN2 (MAKE-PARSTYP :PHRASE '(DER STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));-> 
;                             #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "DER ist kein Nomen oder ist unbekannt")
(NOMEN2 (MAKE-PARSTYP :PHRASE '(STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                             #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
(NOMEN2 (MAKE-PARSTYP :PHRASE '(STUDENTEN SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                             #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS PLUR) :ERROR NIL)
(NOMEN2 (MAKE-PARSTYP :PHRASE '(STUDENTEN SIEHT DEN HASEN) :CGN '(NOM BEL SING)));->
;                             #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "STUDENTEN ist nicht kongruent zu Artikel oder den Vorgaben")
;****************************************************************************************************************
; Funktion fuer NT <eigenname>
;----------------------------------------------------------------------------------------------------------------
(DEFUN EIGENNAME2 (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
(EIGENNAME2 (MAKE-PARSTYP :PHRASE '(STUDENT SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));-> 
;                             #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "student ist kein EIGENNAME oder ist unbekannt")
(EIGENNAME2 (MAKE-PARSTYP :PHRASE '(MAX SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                             #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
(EIGENNAME2 (MAKE-PARSTYP :PHRASE '(MORITZ SIEHT DEN HASEN) :CGN '(NOM BEL BEL)));->
;                             #S(PARSTYP :PHRASE (SIEHT DEN HASEN) :CGN (NOM MAS SING) :ERROR NIL)
;****************************************************************************************************************
; Funktion fuer NT <verb>
;----------------------------------------------------------------------------------------------------------------
(DEFUN VERB2 (PARS-STRUCT) ...)
;----------------------------------------------------------------------------------------------------------------
(VERB2 (MAKE-PARSTYP :PHRASE '(SIEHT DEN HASEN) :CGN '(BEL BEL SING)));->
;                                                      #S(PARSTYP :PHRASE (DEN HASEN) :CGN (AKK BEL SING) :ERROR NIL)
(VERB2 (MAKE-PARSTYP :PHRASE '(SIEHT DEN HASEN) :CGN '(AKK MAS SING)));->
;                                                      #S(PARSTYP :PHRASE (DEN HASEN) :CGN (AKK MAS SING) :ERROR NIL)
(VERB2 (MAKE-PARSTYP :PHRASE '(SIEHT DEN HASEN) :CGN '(BEL BEL PLUR)));->
;                                                      #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "SIEHT ist nicht kongruent zu den Vorgaben")
(VERB2 (MAKE-PARSTYP :PHRASE '(HOERT DEN HASEN) :CGN '(BEL BEL SING)));->
;                                                     #S(PARSTYP :PHRASE NIL :CGN NIL :ERROR "HOERT ist kein Verb oder ist unbekannt")
(VERB2 (MAKE-PARSTYP :PHRASE '(ISST DEN HASEN) :CGN '(NOM MAS SING) :ERROR NIL))
;****************************************************************************************************************
; TESTs
;----------------------------------------------------------------------------------------------------------------
;(TRACE PARSER2 SATZ2 NOMINALPHRASE2 VERBALPHRASE2 RESTPHRASE2 ARTIKEL2 nomen2 EIGENNAME2 VERB2 NUM-FILTER)
;----------------------------------------------------------------------------------------------------------------
(PRINT '---------------------------------------------KORREKT-----------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER2 '(DER STUDENT ISST DEN HASEN))         ;-> "ACCEPT"
(PARSER2 '(DIE MOHRRUEBE SIEHT DEN HASEN))      ;-> "ACCEPT"
(PARSER2 '(STUDENT STUDIERT INFORMATIK))        ;-> "ACCEPT"
(PARSER2 '(STUDENTEN STUDIEREN INFORMATIK))     ;-> "ACCEPT" 
(PARSER2 '(EIN STUDENT STUDIERT INFORMATIK))	;-> "ACCEPT"
(PARSER2 '(DER STUDENT STUDIERT INFORMATIK))	;-> "ACCEPT"
(PARSER2 '(MAX STUDIERT INFORMATIK))			;-> "ACCEPT"
(PARSER2 '(EIN STUDENT STUDIERT DIE INFORMATIK));-> "ACCEPT"
(PARSER2 '(DER STUDENT STUDIERT DIE INFORMATIK));-> "ACCEPT"
(PARSER2 '(MAX STUDIERT DIE INFORMATIK))		;-> "ACCEPT"
(PARSER2 '(MORITZ MAG INFORMATIK))				;-> "ACCEPT"
(PARSER2 '(MAX MAG MORITZ))						;-> "ACCEPT"
(PARSER2 '(DER HASE STUDIERT DIE MOHRRUEBE))	;-> "ACCEPT"
(PARSER2 '(MAX STUDIERT DEN HASEN))				;-> "ACCEPT"
(PARSER2 '(MORITZ SCHLAEFT))                    ;-> "ACCEPT"
;----------------------------------------------------------------------------------------------------------------
(PRINT '-----------------------------------------KONGRUENZFEHLER------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER2 '(DIE MOHRRUEBE SIEHT DER HASEN))      ;-> "DER steht im falschen Fall"
(PARSER2 '(DIE HASE STUDIERT DIE STUDENT))		;-> "HASE ist nicht kongruent zu Artikel oder den Vorgaben"
(PARSER2 '(DIE HASE SCHLAEFT DIE STUDENT))		;-> "HASE ist nicht kongruent zu Artikel oder den Vorgaben"
(PARSER2 '(STUDENT STUDIERT DEN MOHRRUEBE))     ;-> "MOHRRUEBE ist nicht kongruent zu Artikel oder den Vorgaben"
(PARSER2 '(DEN STUDENT STUDIERT DEN MOHRRUEBE))	;-> "DEN steht im falschen Fall"
(PARSER2 '(DER MOHRRUEBE SIEHT DEN STUDENT))	;-> "MOHRRUEBE ist nicht kongruent zu Artikel oder den Vorgaben"
(PARSER2 '(DIE MOHRRUEBE SIEHT DER STUDENT))	;-> "DER steht im falschen Fall"
(PARSER2 '(DEN MOHRRUEBE SIEHT DIE STUDENT))	;-> "DEN steht im falschen Fall"
;----------------------------------------------------------------------------------------------------------------
(PRINT '------------------------------------------SYNTAXFEHLER--------------------------------------------------)
;----------------------------------------------------------------------------------------------------------------
(PARSER2 '())							;-> "SATZ-FEHLT"
(PARSER2 '(EIN))						;-> "nomen-FEHLT"
(PARSER2 '(STUDENT))					;-> "VERBALPHRASE-FEHLT"
(PARSER2 '(STUDENT HASE))				;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: HASE" 
(PARSER2 '(EIN STUDENT STUDIERT DER))	;-> "DER steht im falschen Fall"
(PARSER2 '(DER STUDENT MAG STUDIERT))	;-> "RESTPHRASE BEGINNT NICHT MIT ARTIKEL nomen ODER EIGENNAME: STUDIERT"
(PARSER2 '(STUDIERT MAX INFORMATIK))	;-> "SATZ BEGINNT NICHT MIT EINER NOMINALPHRASE: STUDIERT"
(PARSER2 '(MORITZ INFORMATIK MAG))		;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: INFORMATIK"
(PARSER2 '(MAX STUDIERT DIE ASTRONOMIE));-> "ASTRONOMIE IST KEIN nomen oder ist unbekannt"
(PARSER2 '(MAX INFORMATIK SCHLAEFT))    ;-> "VERBALPHRASE BEGINNT NICHT MIT EINEM VERB: INFORMATIK" 
(PARSER2 '(MAX MAG MORITZ STUDIERT))	;-> "STUDIERT ist ueberfluessig"
(PARSER2 '(MAX MAG MORITZ STUDIERT X))	;-> "STUDIERT X ist ueberfluessig"
;****************************************************************************************************************
