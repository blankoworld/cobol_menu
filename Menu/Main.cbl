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
           10 CodeBanque   SQL CHAR(5).
           10 CodeGuichet  SQL CHAR(5).
           10 RacineCompte SQL CHAR(9).
           10 TypeCompte   SQL CHAR(2).
           10 CleRib       SQL CHAR(2).
           10 Debit        PIC 9(12)V99.
           10 Credit       PIC 9(12)V99.

       01 CLIENT.
           10 CodeClient PIC X(36).
           10 Intitule   SQL CHAR-VARYING(10).
           10 Prenom     SQL CHAR-VARYING(50).
           10 Nom        SQL CHAR-VARYING(50).

       01 BQ.
           10 CodeBanque PIC X(5).
           10 Enseigne   PIC X(151).

       77 OPTION PIC 9 VALUE 9.
       77 OPTION-BQ PIC X VALUE "S".
       77 FIN-FICHIER PIC 9.

       77 DernierChamp PIC X(12).
       77 BQ-NumeroLigne PIC 99.

       77 COULEURFOND PIC 99 VALUE 15.
       77 COULEURTEXTE PIC 99 VALUE 0.

       77 CNXDB String.
           exec sql
               include sqlca
           end-exec.
           exec sql
               include sqlda
           end-exec.
      * SQLCA et SQLDA sont des blocs de données en instructions pour COBOL.

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

      *****************
      * Menu Banque
      *****************
       01 MenuBanque Background-color COULEURFOND
           Foreground-color COULEURTEXTE.
           10 line 1 Col 1  BLANK SCREEN.
           10 line 3 Col 31 VALUE "LISTE DES BANQUES".
           10 line 5 Col 1  VALUE " Code  Nom de la banque"
               Background-color COULEURTEXTE Foreground-color COULEURFOND
               SIZE 80.

       01 MenuBanqueLigne.
           10 line BQ-Numeroligne col 1 from CodeBanque of BQ.
           10 line BQ-Numeroligne col 7 from Enseigne of BQ.

       01 MenuBanqueQuestion.
           10 line 1 Col 1 VALUE " Page [S]uivante - Retour au [M]enu :"
               Background-color COULEURTEXTE Foreground-color COULEURFOND.
           10 line 1 Col 39 FROM OPTION-BQ
               Background-color COULEURTEXTE Foreground-color COULEURFOND.

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
      * une seule instruction SQL à la fois.
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
               WHEN 2 PERFORM LISTE-BQ
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

      * Traitement du solde : la zone est cadrée à gauche. Si elle est négative
      * alors ça fait Credit NULL, et Debit. Sinon Credit rempli.
           UNSTRING DernierChamp delimited " " or "-" into
               Credit of COMPTE
               Debit of COMPTE
           END-UNSTRING.

      * On divise les valeurs trouvées par 100
           Divide 100 into Debit of Compte.
           Divide 100 into Credit of Compte.

      * Création d'un nouvel ID et envoie dans le CodeClient du présent prog.
           exec sql
               SELECT NEWID() into :Client.CodeClient
           end-exec.

      * On crée un nouveau client
           exec sql
               INSERT INTO CLIENT
                   ([CodeClient]
                   ,[Intitule]
                   ,[Prenom]
                   ,[Nom])
               VALUES
                   (:Client.CodeClient
                   ,:Client.Intitule
                   ,:Client.Prenom
                   ,:Client.Nom)
           end-exec.

      * On crée le compte de ce client
           exec sql
               INSERT INTO COMPTE
                   ([CodeBanque]
                   ,[CodeGuichet]
                   ,[RacineCompte]
                   ,[TypeCompte]
                   ,[CleRib]
                   ,[SoldeDebiteur]
                   ,[SoldeCrediteur]
                   ,[CodeClient])
               VALUES
                   (:Compte.CodeBanque
                   ,:Compte.CodeGuichet
                   ,:Compte.RacineCompte
                   ,:Compte.TypeCompte
                   ,:Compte.CleRib
                   ,:Compte.Debit
                   ,:Compte.Credit
                   ,:Client.CodeClient)
           end-exec.

      *************************************************************************
      * Affiche la liste des banques triées par ordre alphabétique
      *************************************************************************
       LISTE-BQ.
           PERFORM LISTE-BQ-INIT.
           PERFORM LISTE-BQ-TRT UNTIL (SQLCODE = 100) OR (SQLCODE = 101).
           PERFORM LISTE-BQ-FIN.

       LISTE-BQ-INIT.
      * On initialise le curseur.
           EXEC sql
             DECLARE CURSEUR-BQ CURSOR FOR
               SELECT [CodeBanque]
               ,[Enseigne]
               FROM BANQUE ORDER BY [Enseigne]
           END-EXEC.
      * Puis on l'ouvre
           EXEC sql
             OPEN CURSEUR-BQ
           END-EXEC.
      * On initialise le numéro de ligne où afficher la banque
           MOVE 5 TO BQ-NumeroLigne.
      * On affiche le début de l'écran
           DISPLAY MenuBanque.

       LISTE-BQ-TRT.
      * On récupère le résultat du curseur.
           EXEC sql
             FETCH CURSEUR-BQ INTO :BQ.CodeBanque, :BQ.Enseigne
           END-EXEC.
           PERFORM LISTE-BQ-AFFICHAGE.

       LISTE-BQ-FIN.
      * On ferme le curseur.
           EXEC sql
             CLOSE CURSEUR-BQ
           END-EXEC.

       LISTE-BQ-AFFICHAGE.
      * On affiche les lignes de banque entre la ligne 6 et 23.
      * On incrémente donc la variable de la ligne de banque.
           ADD 1 TO BQ-NumeroLigne.
           DISPLAY MenuBanqueLigne.

           IF BQ-NumeroLigne = 23
               MOVE "S" TO OPTION-BQ
               MOVE 5 TO BQ-NumeroLigne
               DISPLAY MenuBanqueQuestion
               ACCEPT OPTION-BQ LINE 1 Col 39
               IF OPTION-BQ = "M" OR OPTION-BQ = "m"
                   MOVE 101 TO SQLCODE
               END-IF
           END-IF.

       end program Main.
