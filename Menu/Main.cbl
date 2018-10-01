       program-id. Main as "Menu.Main".
       author. Olivier DOSSMANN.

       data division.
       working-storage section.

      * Structure pour accueillir la date système
       01 DATE-SYSTEME.
           10 ANNEE PIC 99.
           10 MOIS PIC 99.
           10 JOUR PIC 99.

       77 OPTION PIC 9 VALUE 9.
       77 FIN-MENU PIC 9.
       77 LIGNE PIC X(79) VALUE ALL "-".

       77 COULEURFOND PIC 99 VALUE 15.
       77 COULEURTEXTE PIC 99 VALUE 0.

       screen section.
      *****************
      * Menu Principal
      *****************
       01 MenuPrincipal Background-color COULEURFOND Foreground-color COULEURTEXTE.
           10 line  1 Col 1  BLANK SCREEN.
           10 line  3 Col 31 VALUE "Gestion de la banque".
           10 line  5 Col 2  VALUE "Date systeme :".
           10 line  5 Col 18 FROM JOUR OF DATE-SYSTEME.
           10 line  5 Col 20 VALUE "/".
           10 line  5 Col 21 FROM MOIS OF DATE-SYSTEME.
           10 line  5 Col 23 VALUE "/".
           10 line  5 Col 24 FROM ANNEE OF DATE-SYSTEME.
           10 line  5 Col 70 VALUE "Option :".
           10 line  5 col 79 PIC 9 FROM Option.
           10 line  8 Col 4 VALUE "- 1 - Importation des comptes ............................... : ".
           10 line  9 Col 4 VALUE "- 2 - Liste des banques ..................................... : ".
           10 line 10 Col 4 VALUE "- 3 - Liste des comptes ..................................... : ".
           10 line 11 Col 4 VALUE "- 4 - Controle des cles RIB ................................. : ".
           10 line 12 Col 4 VALUE "- 5 - Gestion des clients ................................... : ".
           10 line 14 Col 4 VALUE "- 0 - Retour au menu appelant ............................... : ".

           10 line 5 col 79.

       procedure division.
      ******************
      * Affiche un MENU
      ******************

       MENU.
           PERFORM MENU-INIT.
           PERFORM MENU-TRAITEMENT UNTIL FIN-MENU = 1.
           PERFORM MENU-FIN.

       MENU-INIT.
           MOVE 0 TO FIN-MENU.
           MOVE 9 TO OPTION.

       MENU-TRAITEMENT.
           DISPLAY MenuPrincipal.
      * Contrôle d'entrée de boucle
           ACCEPT OPTION.
           EVALUATE OPTION
               WHEN 0 MOVE 1 TO FIN-MENU
               WHEN OTHER
                   DISPLAY "Mauvaise option"
           END-EVALUATE.

       MENU-FIN.

           STOP run.
           
       end program Main.
