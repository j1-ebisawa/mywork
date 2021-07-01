      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXFQA-17
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-19.
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
       01  CASE-ID            PIC X(20).
       01  A        PIC X(30) VALUE "‚`‚a‚b‚c‚d".
       01  B        PIC N(05) VALUE "‚`‚a‚b‚c‚d".
       01  C        PIC N(10) VALUE "‚`‚a‚b‚c‚d".
       01  X        PIC X(16375).
       01  Y        PIC X(16376).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-19 test start".
      *ƒP[ƒX1:
            MOVE "X PIC X(16375)"  TO CASE-ID.
            MOVE ALL "*" TO X
            IF X = ALL "*"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
            END-IF.
      *ƒP[ƒX2:
            MOVE "Y PIC X(16376)"  TO CASE-ID.
            MOVE ALL "*" TO Y
            IF Y = ALL "*"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
            END-IF.
      *     
            DISPLAY "QA-19 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

