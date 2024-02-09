       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. STUD.                                                00020000
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00040000
       FILE-CONTROL.                                                    00050000
                SELECT EMPFIL ASSIGN TO EMPFILE                         00060000
                ORGANIZATION IS SEQUENTIAL.                             00070000
       DATA DIVISION.                                                   00080000
       FILE SECTION.                                                    00090000
       FD EMPFIL                                                        00100000
            RECORDING MODE IS F.                                        00110000
       01 FL-REC.                                                       00120000
          05 STUD-ID    PIC X(09).                                      00130000
          05 FILLER    PIC X(71).                                       00140000
       WORKING-STORAGE SECTION.                                         00150000
       01 WS-FL-REC.                                                    00160000
          05 FL-STUD-ID PIC X(09).                                      00170000
          05 FILLER    PIC X(71).                                       00180000
                                                                        00190000
       77 WS-STUD-ID1 PIC 9(09).                                        00200000
                                                                        00210000
       01 WS-EOF    PIC A VALUE SPACE.                                  00220000
                                                                        00230000
            EXEC SQL                                                    00240000
               INCLUDE SQLCA                                            00250000
            END-EXEC.                                                   00260000
                                                                        00270000
            EXEC SQL                                                    00280000
               INCLUDE STUDENT                                          00290000
            END-EXEC.                                                   00300000
                                                                        00310000
       01  DCLSTUDENT.                                                  00320000
           10 WS-STUD-ID               PIC S9(9) USAGE COMP.            00330000
           10 WS-STUD-NAME             PIC X(30).                       00340000
           10 WS-STUD-DEPT             PIC X(10).                       00350000
                                                                        00360000
       PROCEDURE DIVISION.                                              00370000
                                                                        00380000
            OPEN INPUT EMPFIL.                                          00390000
                                                                        00400000
            PERFORM UNTIL WS-EOF = 'Y'                                  00410000
                                                                        00420000
               READ EMPFIL INTO WS-FL-REC                               00430000
                 AT END MOVE 'Y' TO WS-EOF                              00440000
                 NOT AT END PERFORM A000-WRITE-PARA                     00450000
                 COMPUTE WS-STUD-ID = FUNCTION NUMVAL(FL-STUD-ID)       00460000
                 MOVE  WS-STUD-ID TO WS-STUD-ID1                        00470000
                 PERFORM A000-STUDENT-ID-VAL                            00480000
               END-READ                                                 00490000
                                                                        00500000
            END-PERFORM.                                                00510000
                                                                        00520000
            CLOSE EMPFIL.                                               00530000
                                                                        00540000
            GOBACK.                                                     00550000
                                                                        00560000
       A000-WRITE-PARA.                                                 00570000
                                                                        00580000
             COMPUTE WS-STUD-ID = FUNCTION NUMVAL(FL-STUD-ID)           00590000
                                                                        00600000
             EXEC SQL                                                   00610000
                  SELECT  STUD_ID, STUD_NAME, STUD_DEPT                 00620000
                    INTO  :WS-STUD-ID, :WS-STUD-NAME, :WS-STUD-DEPT     00630000
                 FROM STUD_STUDENT                                      00640000
                 WHERE STUD_ID = :WS-STUD-ID                            00650000
             END-EXEC.                                                  00660000
                                                                        00670000
                                                                        00680000
             EVALUATE SQLCODE                                           00690000
             WHEN 100                                                   00700000
                  DISPLAY 'NO RECORDS FOUND'                            00710000
                  MOVE +12  TO RETURN-CODE                              00720000
             WHEN 0                                                     00730000
                  DISPLAY 'STUD-ID: ' WS-STUD-ID                        00740000
                  DISPLAY 'STUD-NAME: ' WS-STUD-NAME                    00750000
      *           PERFORM A000-DEPARTMENT-VAL                           00760000
      *           MOVE WS-STUD-ID TO WS-STUD-ID1                        00770000
      *           PERFORM A000-STUDENT-ID-VAL                           00780000
                  MOVE 0    TO RETURN-CODE                              00790000
             END-EVALUATE.                                              00800000
                                                                        00810000
      *A000-DEPARTMENT-VAL.                                             00820000
      *      EVALUATE WS-STUD-DEPT                                      00830000
      *      WHEN 'CSE'                                                 00840000
      *           DISPLAY 'COMPUTER SCIENCE'                            00850000
      *      WHEN 'MEC'                                                 00860000
      *           DISPLAY 'MECHANICAL'                                  00870000
      *      WHEN 'ECE'                                                 00880000
      *           DISPLAY 'ELECTRONIC AND COMMUNICATION ENG'            00890000
      *      WHEN 'IT'                                                  00900000
      *           DISPLAY 'INFORMATION TECH'                            00910000
      *      WHEN OTHER                                                 00920000
      *           DISPLAY 'NOT VALID'                                   00930000
      *      END-EVALUATE.                                              00940000
                                                                        00950000
       A000-STUDENT-ID-VAL.                                             00960000
             IF WS-STUD-ID1 IS NUMERIC                                  00970000
                  DISPLAY 'VALID STUDENT ID'                            00980000
             ELSE                                                       00990000
                  DISPLAY 'NOT VALID'                                   01000000
             END-IF.                                                    01010000