      ******************************************************************
      *    ÉeÉXÉgÉPÅ[ÉXÅFQA-27
      *    ÉvÉçÉOÉâÉÄñºÅFì˙ñ{åÍâªÉeÉXÉg QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-27.
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
       01  A        PIC 
          X/X/X/X/XBX/X/X/X/XBX/X/X/X/XB.
       01  B        PIC
          N/N/N/N/NBN/N/N/N/NBN/N/N/N/NB.
       01  C        PIC 
          X/X/X/X/XBX/X/X/X/XBX/X/X/X/XBX/X/X/X/XBX/X/X/X/XB.
       01  D        PIC
          N/N/N/N/NBN/N/N/N/NBN/N/N/N/NBN/N/N/N/NBN/N/N/N/NB.
      *01  E        PIC                                      *>Pic string >50
      *   X/X/X/X/XBX/X/X/X/XBX/X/X/X/XBX/X/X/X/XBX/X/X/X/XBX.
      *01  F        PIC
      *   N/N/N/N/NBN/N/N/N/NBN/N/N/N/NBN/N/N/N/NBN/N/N/N/NBN.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-27 test start".
      *ÉPÅ[ÉX1:
            MOVE "PIC X string <= 30"  TO CASE-ID.
            MOVE "123451234512345" TO A
            IF A = ALL "1/2/3/4/5 "
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "A=" A
            END-IF.
      *ÉPÅ[ÉX2:
            MOVE "PIC N string <= 30"  TO CASE-ID.
            MOVE "ÇPÇQÇRÇSÇTÇPÇQÇRÇSÇTÇPÇQÇRÇSÇT" TO B
            IF B = ALL "ÇPÅ^ÇQÅ^ÇRÅ^ÇSÅ^ÇTÅ@"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "B=" B
            END-IF.
      *ÉPÅ[ÉX3:
            MOVE "PIC X string > 30"  TO CASE-ID.
            MOVE "1234512345123451234512345" TO C
            IF C = ALL "1/2/3/4/5 "
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "C=" C
            END-IF.
      *ÉPÅ[ÉX4:
            MOVE "PIC N string > 30"  TO CASE-ID.
            MOVE "ÇPÇQÇRÇSÇTÇPÇQÇRÇSÇTÇPÇQÇRÇSÇTÇPÇQÇRÇSÇTÇPÇQÇRÇSÇT" 
                                      TO D
            IF D = ALL "ÇPÅ^ÇQÅ^ÇRÅ^ÇSÅ^ÇTÅ@"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "D=" D
            END-IF.
      *     
      *ÉPÅ[ÉX5:
            MOVE "PIC X string <= 30"  TO CASE-ID.
            MOVE ALL "12345" TO A
            IF A = ALL "1/2/3/4/5 "
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "A=" A
            END-IF.
      *ÉPÅ[ÉX6:
            MOVE "PIC N string <= 30"  TO CASE-ID.
            MOVE ALL "ÇPÇQÇRÇSÇT" TO B
            IF B = ALL "ÇPÅ^ÇQÅ^ÇRÅ^ÇSÅ^ÇTÅ@"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "B=" B
            END-IF.
            DISPLAY "QA-27 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

