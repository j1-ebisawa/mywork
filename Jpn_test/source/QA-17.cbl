      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXFQA-17
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-17.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-25.
       DATE-COMPILED.        2011-08-25.
      ******************************************************************
       ENVIRONMENT           DIVISION.
      ******************************************************************
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      PC.
       OBJECT-COMPUTER.      PC.
       SPECIAL-NAMES.
      ******************************************************************
       DATA                  DIVISION.
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE       SECTION.
      ******************************************************************
       01  OMIT-WK            PIC X.
       01  CASE-ID            PIC X(10).
       01  A        PIC X(30) VALUE "‚`‚a‚b‚c‚d".
       01  B        PIC N(05) VALUE "‚`‚a‚b‚c‚d".
       01  C        PIC N(10) VALUE "‚`‚a‚b‚c‚d".
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-17 test start".
      *ƒP[ƒX1:
            MOVE "A = B"  TO CASE-ID.
            IF A = B
               DISPLAY CASE-ID "TRUE:OK"
            ELSE
               DISPLAY CASE-ID "FALSE:NG"
            END-IF.
      *ƒP[ƒX2:
            MOVE "A = C"        TO CASE-ID.
            IF A = C
               DISPLAY CASE-ID "TRUE:NG"
            ELSE
               DISPLAY CASE-ID "FALSE:OK"
            END-IF.
      *ƒP[ƒX3:
            MOVE "B = C"        TO CASE-ID.
            IF B = C
               DISPLAY CASE-ID "TRUE:OK"
            ELSE
               DISPLAY CASE-ID "FALSE:NG"
            END-IF.
      *     
            DISPLAY "QA-17 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

