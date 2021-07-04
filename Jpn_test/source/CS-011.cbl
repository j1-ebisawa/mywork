      ******************************************************************
      *    テストケース：CS-011
      *    プログラム名：通貨記号テスト 
      *    処理概要　　：通貨記号「￥」および、
      *                  CURRENCY-SIGN句が使えるかチェックする。
      *  --------------------------------------------------------------
      *   テストケース:CURRENCY SIGNに「￥」指定なしでも「\」が使える。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           CS-011.
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
              *>#001 CURRENCY SIGN IS "\".
      ******************************************************************
       DATA                  DIVISION.
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE       SECTION.
      ******************************************************************
       01  OMIT-WK            PIC X.
       01  CASE-ID            PIC X(10).
      *
       01  NUM-01        PIC S9(5).
       01  NUM-02        PIC S9(5).
       01  NUM-03        PIC S9(5).
      *
       01  NEDIT-01X.
           05 NEDIT-01   PIC \\\,\\9.   *>#001 Default \
       01  XXX           PIC XX.
       01  NEDIT-02X.
           05  NEDIT-02  PIC \\\,\\9.   *>#001 Default \
       01  WK-I          PIC S9(3).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (CS-011)".
      *ケース1.
            MOVE "P-010-01"        TO CASE-ID.
            MOVE      1 TO NUM-01.
            MOVE NUM-01 TO NEDIT-01.
            IF NEDIT-01X = "     \1"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-01X
            END-IF.
      *
            MOVE "P-010-02"        TO CASE-ID.
            MOVE   1234 TO NUM-01.
            MOVE NUM-01 TO NEDIT-01.
            IF NEDIT-01X = " \1,234"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-01X
            END-IF.
      *
            MOVE "P-010-03"        TO CASE-ID.
            MOVE   1234   TO NUM-01.
            MOVE NUM-01   TO NEDIT-01.
            MOVE NEDIT-01 TO NUM-02.
            IF NUM-02 = 1234
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NUM-02
            END-IF.
      *
            DISPLAY "TEST END   (CS-011)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

