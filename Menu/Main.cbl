       program-id. Main as "Menu.Main".
       author. Olivier DOSSMANN.

       environment division.
       configuration section.
       source-computer.
           SRF-EN2-07.

       input-output section.
       file-control.
           SELECT FICHIER-COMPTE
               ASSIGN "C:\Users\Olivier\Documents\Public\Client.csv"
               LINE SEQUENTIAL.


       data division.
       file section.
       FD FICHIER-COMPTE record varying from 0 to 255.
       01 EnrFichierCompte PIC X(255).

       working-storage section.

      * Structure pour accueillir la date système
       01 DATE-SYSTEME.
           10 ANNEE PIC 99.
           10 MOIS PIC 99.
           10 JOUR PIC 99.

      * Structure pour accueillir une partie de la ligne du fichier compte
       01 COMPTE.
           10 CodeBanque PIC X(5).
           10 CodeGuichet PIC X(5).
           10 RacineCompte PIC X(9).
           10 TypeCompte PIC XX.
           10 CleRib PIC 99.
           10 Debit PIC 9(10).
           10 Credit PIC 9(10).

       01 CLIENT.
           10 Intitule PIC X(10).
           10 Prenom PIC X(50).
           10 Nom PIC X(50).

       77 OPTION PIC 9 VALUE 9.
       77 FIN-FICHIER PIC 9.
       77 LIGNE PIC X(79) VALUE ALL "-".

       77 DernierChamp PIC X(12).
       77 Solde PIC ZZZZZZ9.

       77 NAWAK pic 9.

       77 COULEURFOND PIC 99 VALUE 15.
       77 COULEURTEXTE PIC 99 VALUE 0.

       77 CNXDB String.
           exec sql
               include sqlca
           end-exec.
           exec sql
               include sqlda
           end-exec.

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
           10 line  8 Col 4 VALUE "- 1 - Importation des comptes ..................................... : ".
           10 line  9 Col 4 VALUE "- 2 - Liste des banques ........................................... : ".
           10 line 10 Col 4 VALUE "- 3 - Liste des comptes ........................................... : ".
           10 line 11 Col 4 VALUE "- 4 - Controle des cles RIB ....................................... : ".
           10 line 12 Col 4 VALUE "- 5 - Gestion des clients ......................................... : ".
           10 line 14 Col 4 VALUE "- 0 - Retour au menu appelant ..................................... : ".

       procedure division.

      ******************
      * Affiche un MENU
      ******************
       MENU.
           PERFORM MENU-INIT.
           PERFORM MENU-TRAITEMENT UNTIL OPTION = 0.
           PERFORM MENU-FIN.

       MENU-INIT.
      * Connexion à la base de données
           MOVE "Trusted_Connection=yes;Database=CIGALES;server=SRF-EN2-07\SQLEXPRESS;factory=System.Data.SqlClient;" TO CNXDB.
           exec sql
               connect using :CNXDB
           end-exec.

      * Choix de l'option Autocommit ** NE JAMAIS FAIRE IRL **************
           exec sql
               set autocommit on
           end-exec.

      * Utilisation d'une autre valeur que 0 à 5 par défaut
           MOVE 6 TO OPTION.
           ACCEPT DATE-SYSTEME FROM date.

       MENU-TRAITEMENT.
           MOVE 0 TO OPTION.
           DISPLAY MenuPrincipal.
      * Récupération de l'option de l'utilisateur directement au bon endroit
           ACCEPT OPTION line 5 Col 79.
           EVALUATE OPTION
               WHEN 1 PERFORM IMPORT-FICHIER
               WHEN 2 continue
               WHEN 3 continue
               WHEN 4 continue
               WHEN 5 CONTINUE
           END-EVALUATE.

       MENU-FIN.
           STOP run.

      *************************************************
      * Importation des lignes de fichier "Client.csv"
      *************************************************
       IMPORT-FICHIER.
           PERFORM IMPORT-FICHIER-INIT.
           PERFORM IMPORT-FICHIER-TRAITEMENT UNTIL FIN-FICHIER = 1.
           PERFORM IMPORT-FICHIER-FIN.

       IMPORT-FICHIER-INIT.
           MOVE 0 TO FIN-FICHIER.
      * ouverture du fichier
           OPEN INPUT FICHIER-COMPTE.
           READ FICHIER-COMPTE.

       IMPORT-FICHIER-TRAITEMENT.
      * lecture de la ligne
           READ FICHIER-COMPTE
               at end move 1 to FIN-FICHIER
               not at end perform TrtLigne
           end-read.

       IMPORT-FICHIER-FIN.
      * fermeture du fichier
           CLOSE FICHIER-COMPTE.
           accept NAWAK.

      ************************************
      * Traitement d'une ligne du fichier
      ************************************
       TrtLigne.
           UNSTRING EnrFichierCompte DELIMITED ";" INTO
               CodeBanque of COMPTE
               CodeGuichet of COMPTE
               RacineCompte of COMPTE
               TypeCompte of COMPTE
               CleRib of COMPTE
               Intitule of CLIENT
               Prenom of CLIENT
               Nom of CLIENT
               DernierChamp
           END-UNSTRING.

      * Traitement du solde : la zone est cadrée à gauche. Si elle 
           DISPLAY Prenom Nom.
           UNSTRING DernierChamp delimited " " or "-" into
               Credit of COMPTE
               Debit of COMPTE
           END-UNSTRING.

      * On divise les valeurs trouvées par 100
           Divide 100 into Debit of Compte.
           Divide 100 into Credit of Compte.

       end program Main.
