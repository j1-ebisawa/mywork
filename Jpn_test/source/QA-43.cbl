      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-43.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-24.
       DATE-COMPILED.        2011-08-24.
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
       01  W-LENG      PIC S9(5).

       01  X-01          PIC X(10).
       01  A-01          PIC A(5).
       01  ZONE-01       PIC 9(3).
       01  PACK-01       PIC S9(3)V9(3)  COMP-3.
       01  BIN-01        PIC S9(5)       COMP.
       01  G-01          PIC N(8).
       01  GE-01         PIC N/NNN/N.
      *
       01  GRP-03.
           05  GRP-03-1  PIC X(5).                       *>5
           05  GRP-03-2  PIC A(5).                       *>5
           05  GRP-03-3  PIC 9(5).                       *>5
           05  GRP-03-4  PIC N(5)  OCCURS 1 TO 10        *>10*n
                                   DEPENDING ON GRP-03-3.
       01  GRP-01.
           05  GRP-01-1  PIC X(5).                       *>5
           05  GRP-01-2  PIC A(5).                       *>5
           05  GRP-01-3  PIC 9(5).                       *>5
           05  GRP-01-4  PIC N(5)  OCCURS 0 TO 10        *>10*n
                                   DEPENDING ON GRP-01-3.

      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (QA-43)".
      *ケース7：エラーチェック
            MOVE "P-070-01"        TO CASE-ID.
            MOVE 0 TO GRP-01-3. 
            MOVE FUNCTION LENGTH(GRP-01) TO W-LENG.
            IF W-LENG = 15           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-070-01"        TO CASE-ID.
            MOVE 0 TO GRP-03-3. 
            MOVE FUNCTION LENGTH(GRP-03) TO W-LENG.
            IF W-LENG = 15           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            DISPLAY "TEST END   (QA-43)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

