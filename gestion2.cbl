       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestion2.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Section pour le contrôle des fichiers d'entrées et de sortie
      *    du programme

      *    Le fichier "input.dat" sur lequel travaille le programme
           SELECT ELEVES ASSIGN TO "input.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

      *    Le fichier "output.dat" qui contiendra les résultats
      *    du programme
           SELECT RAPPORT ASSIGN TO "output.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

      *    Un fichier intermédiaire pour trier les résultats
           SELECT RAPPORT-TRIE ASSIGN TO "rapport_trie.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
      *    Le groupe de variables du fichier ELEVES
      *    Les deux lignes sont là pour distinguer les deux longueurs
      *    différentes
       FD  ELEVES.
       01  F-LIGNE.
           05 F-LIGNE-ELEVE PIC X(17).
           05 F-LIGNE-MAT PIC X(31).

      *    Un groupe de variables pour les élèves enregistrés depuis
      *    le fichier
       01  F-ELEVE.
           05 F-NOM-ELEVE PIC X(7).
           05 F-PRENOM-ELEVE PIC X(6).
           05 F-AGE-ELEVE PIC X(02).

      *    Un groupe de variables pour les matières depuis le fichier
       01  F-MATIERES.
           05 F-NOM-MAT PIC X(21).
           05 F-COEF-MAT PIC 9V9.
           05 F-NOTE-MAT PIC 99V99.    

       FD  RAPPORT.
       01  ELEVES-OUT.
           05 NOM-ELEVE-OUT PIC X(7).
           05 PRENOM-ELEVE-OUT PIC X(6).
           05 AGE-ELEVE-OUT PIC X(02).

       SD  RAPPORT-TRIE.
       01  ELEVE-TRIE.
           05 T-NOM-ELEVE PIC X(7).
           05 T-PRENOM-ELEVE PIC X(6).
           05 T-AGE-ELEVE PIC X(02).

       WORKING-STORAGE SECTION.

       01  WS-CLASSE.
           05 WS-NBR-ELEVES PIC 9(03).
           05 WS-NBR-MAT PIC 9(03).
           05 WS-ELEVE OCCURS 1 TO 999 TIMES DEPENDING ON WS-NBR-ELEVES.
               10 WS-NOM-ELEVE PIC X(7).
               10 WS-PRENOM-ELEVE PIC X(6).
               10 WS-AGE-ELEVE PIC X(02).
               10 FILLER PIC X VALUE " ".
               10 WS-NOTES-ELEVE OCCURS 6 TIMES.
                   15 WS-NOTE PIC 99V99.

       01  WS-TEMPO-NOTE PIC 999V99.

       01  WS-TAB-MAT.
           05 WS-NOM-MAT PIC X(21).
           05 WS-COEF-MAT PIC 9V9.

       77  WS-SOMME-NOTES PIC 999V99.
       77  WS-MOYENNE PIC 99V99.

       77  WS-INDEX1 PIC 9(03) VALUE IS 1.
       77  WS-INDEX2 PIC 9(03) VALUE IS 0.
       77  WS-TAB-NOTES-INDEX PIC 9(03) VALUE IS 0.

       77  WS-MAX-TAB PIC 99 VALUE 49.
       77  WS-FIN-FICHIER PIC X VALUE "N".

       PROCEDURE DIVISION.
           PERFORM LECTURE-FICHIER.
           PERFORM CALCUL-MOYENNE.
           
           STOP RUN.

       AFFICHER-FICHIER.
           PERFORM LECTURE-FICHIER.
           PERFORM VARYING WS-INDEX1 FROM 1 BY 1
           UNTIL WS-INDEX1 > WS-NBR-ELEVES
               DISPLAY WS-ELEVE(WS-INDEX1)
           END-PERFORM.

       LECTURE-FICHIER.
           OPEN INPUT ELEVES.
           PERFORM UNTIL WS-FIN-FICHIER = "Y"
           READ ELEVES
           AT END
               MOVE "Y" TO WS-FIN-FICHIER
           NOT AT END
      *    Boucle conditionnelle pour 
      *    parcourir le fichier ligne par ligne
           IF WS-INDEX1 <= WS-MAX-TAB
      *    Boucle conditionnelle pour enregistrer les élèves 
      *    (ligne commençant par "01")
               IF F-LIGNE(1:2) EQUAL "01"
      *            Incrémentation du nombre totale d'élèves, 
      *            et de leurs index
                   ADD 1 TO WS-NBR-ELEVES
                   ADD 1 TO WS-INDEX2
      *            Enregistrement du nom de l'élève
                   MOVE F-LIGNE(3:7)
                     TO WS-NOM-ELEVE(WS-INDEX2)
      *            Enregistrement du prenom de l'eleve
                   MOVE F-LIGNE(10:6)
                       TO WS-PRENOM-ELEVE(WS-INDEX2)
      *            Enregistrement de l'age de l'élève
                   MOVE F-LIGNE(16:2)
                       TO WS-AGE-ELEVE(WS-INDEX2)
               END-IF
      *        Boucle conditionnelle pour l'enregsitrement des matières
      *        (lignes commencant par "02")
               IF F-LIGNE(1:2) EQUAL "02"
      *            Incrémentation du nombre de matières
      *            et de leurs index
                   ADD 1 TO WS-NBR-MAT
                   ADD 1 TO WS-TAB-NOTES-INDEX
      *            Si l'index dépasse le nombre totale de matières
      *            il revient à zéro
                   IF WS-TAB-NOTES-INDEX > 6
                   MOVE 1 TO WS-TAB-NOTES-INDEX
                   END-IF
      *            Enregistrement du nom de la matière
                   MOVE F-LIGNE(3:21)
                       TO WS-NOM-MAT
      *            Enregistrement du coefficient de la matière
                   MOVE F-LIGNE(24:3)
                       TO WS-COEF-MAT
      *            Enregistrement temporaire de la note
                   MOVE F-LIGNE(27:5)
                       TO WS-TEMPO-NOTE

      *            Déplacement de la note vers 
      *            celles rattachées à l'élève
                   MOVE WS-TEMPO-NOTE
                       TO WS-NOTES-ELEVE(WS-INDEX2 WS-TAB-NOTES-INDEX)

               END-IF
               ADD 1 TO WS-INDEX1
           END-IF
           END-READ
           END-PERFORM.
           CLOSE ELEVES.
       
       AFFICHER-FICHIER-TRIE.
           PERFORM LECTURE-FICHIER.          
           MOVE "N" TO WS-FIN-FICHIER.

           SORT RAPPORT-TRIE 
           ON ASCENDING KEY T-NOM-ELEVE
           INPUT PROCEDURE LECTURE-ELEVES
           GIVING RAPPORT.

       LECTURE-ELEVES.
           OPEN INPUT ELEVES.
           PERFORM UNTIL WS-FIN-FICHIER = "Y"
           READ ELEVES
           AT END 
           MOVE "Y" TO WS-FIN-FICHIER
           NOT AT END
               IF F-LIGNE(2:1) = "1"
               MOVE F-LIGNE(3:7)
                   TO T-NOM-ELEVE
               MOVE F-LIGNE(10:6)
                   TO T-PRENOM-ELEVE
               MOVE F-LIGNE(16:2)
                   TO T-AGE-ELEVE
               RELEASE ELEVE-TRIE
               END-IF
               END-READ
               END-PERFORM.

       AFFICHER-RAPPORT.
           MOVE "N" TO WS-FIN-FICHIER.
           OPEN INPUT RAPPORT
           PERFORM UNTIL WS-FIN-FICHIER = "Y"
           READ RAPPORT
           AT END
               MOVE "Y" TO WS-FIN-FICHIER
           NOT AT END
               DISPLAY "ÉLÈVE TRIÉ : " 
                   NOM-ELEVE-OUT SPACE 
                   PRENOM-ELEVE-OUT SPACE 
                   AGE-ELEVE-OUT
           END-PERFORM
           CLOSE RAPPORT.


       CALCUL-MOYENNE.
           PERFORM VARYING WS-INDEX1 FROM 1 BY 1
           UNTIL WS-INDEX1 > WS-NBR-ELEVES
               MOVE 0 TO WS-SOMME-NOTES

               PERFORM VARYING WS-INDEX2 FROM 1 BY 1
               UNTIL WS-INDEX2 > 6

                   ADD WS-NOTES-ELEVE(WS-INDEX1, WS-INDEX2)
                   TO WS-SOMME-NOTES
               END-PERFORM
               COMPUTE WS-MOYENNE 
               ROUNDED = WS-SOMME-NOTES/6

               DISPLAY "ELEVE : "
                       WS-NOM-ELEVE(WS-INDEX1) SPACE
                       WS-PRENOM-ELEVE(WS-INDEX1) SPACE
                       WS-AGE-ELEVE(WS-INDEX1) SPACE
                       "MOYENNE : " WS-MOYENNE
           
           END-PERFORM.


       ECRIRE-RAPPORT.


