       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG13.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                SELECT EMPFIL ASSIGN TO EMPFILE
                ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EMPFIL
            RECORDING MODE IS F.
       01 FL-REC.
          05 PATN-ID    PIC X(09).
          05 FILLER    PIC X(71).
       WORKING-STORAGE SECTION.

       01 WS-FL-REC.
          05 FL-PATN-ID PIC X(09).
          05 FILLER    PIC X(71).


       01 WS-EOF    PIC A VALUE SPACE.

            EXEC SQL
               INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
               INCLUDE PATIENT
            END-EXEC.

       01  DCLPATIENT.
           10 WS-PATN-ID               PIC S9(9) USAGE COMP.
           10 WS-PATN-NAME             PIC X(20).
           10 WS-PATN-AGE              PIC S9(2) USAGE COMP.
           10 WS-PATN-BG               PIC X(3).
           10 WS-PATN-NUM              PIC X(12).

       PROCEDURE DIVISION.

            OPEN INPUT EMPFIL.

            PERFORM UNTIL WS-EOF = 'Y'

               READ EMPFIL INTO WS-FL-REC
                 AT END MOVE 'Y' TO WS-EOF
                 NOT AT END PERFORM A000-WRITE-PARA
               END-READ

            END-PERFORM.

            CLOSE EMPFIL.

            STOP RUN.

       A000-WRITE-PARA.

             COMPUTE WS-PATN-ID = FUNCTION NUMVAL(FL-PATN-ID)

             EXEC SQL
               SELECT  PATN_NAME, PATN_AGE, PATN_BG, PATN_NUM
               INTO  :WS-PATN-NAME, :WS-PATN-AGE, :WS-PATN-BG,
                     :WS-PATN-NUM
               FROM PATIENT
               WHERE PATN_ID = :WS-PATN-ID
             END-EXEC.


             EVALUATE SQLCODE
             WHEN 100
                  DISPLAY 'NO SUCH RECORD FOUND'
             WHEN 0
                  DISPLAY 'PATN-NAME: ' WS-PATN-NAME
                  DISPLAY 'PATN-AGE:  ' WS-PATN-AGE
                  DISPLAY 'PATN-BG:  ' WS-PATN-BG
                  DISPLAY 'PATN-NUM: ' WS-PATN-NUM
             END-EVALUATE.