      ******************************************************************
      *    テストケース：CS-07
      *    プログラム名：通貨記号テスト 
      *    処理概要　　：通貨記号「￥」および、CURRENCY-SIGN句により
      *                  「￥」が編集記号として機能するかチェックする。
      *  --------------------------------------------------------------
      *   テストケース:CURRENCY SIGNに「￥」指定すると「$」が使えない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           CS-07.
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
              CURRENCY SIGN IS "\".
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
      *固定挿入の場合、
       01  NEDIT-01X.
           05 NEDIT-01   PIC $B0/99,999.99-.
       01  NEDIT-02X.
           05 NEDIT-02   PIC $B0/99,999.99+.
       01  NEDIT-03X.
           05 NEDIT-03   PIC $B0/99,999.99CR.
       01  NEDIT-04X.
           05 NEDIT-04   PIC -$B0/99,999.99.
       01  NEDIT-05X.
           05 NEDIT-05   PIC +$B0/99,999.99.
       01  NEDIT-06X.
           05 NEDIT-06   PIC $B0/99,999.99DB.
       01  NEDIT-07X.
           05 NEDIT-07   PIC -$B0/99,999.99.
       01  NEDIT-08X.
           05 NEDIT-08   PIC +$B0/99,999.99.
      *浮動挿入の場合、小数点の左側
       01  NEDIT-11X.
           05 NEDIT-11   PIC $$$B0/99,999.99-.
       01  NEDIT-12X.
           05 NEDIT-12   PIC $$$B0/99,999.99+.
       01  NEDIT-13X.
           05 NEDIT-13   PIC $$$B0/99,999.99CR.
       01  NEDIT-14X.
           05 NEDIT-14   PIC -$$$B0/99,999.99.
       01  NEDIT-15X.
           05 NEDIT-15   PIC +$$$B0/99,999.99.
       01  NEDIT-16X.
           05 NEDIT-16   PIC $$$B0/99,999.99DB.
       01  NEDIT-17X.
           05 NEDIT-17   PIC -$$$B0/99,999.99.
       01  NEDIT-18X.
           05 NEDIT-18   PIC +$$$B0/99,999.99.
      *
       01  WK-I          PIC S9(3).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (CS-07)".
      *ケース1.
            MOVE "P-010-01"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-01.
            IF NEDIT-01X = "$ 0/00,123.45-"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-01X
            END-IF.
      *
            MOVE "P-010-02"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-02.
            IF NEDIT-02X = "$ 0/00,123.45-"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-02X
            END-IF.
      *
            MOVE "P-010-03"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-03.
            IF NEDIT-03X = "$ 0/00,123.45CR"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-03X
            END-IF.
      *
            MOVE "P-010-04"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-04.
            IF NEDIT-04X = "-$ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-04X
            END-IF.
      *
            MOVE "P-010-05"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-05.
            IF NEDIT-05X = "-$ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-05X
            END-IF.
      *
            MOVE "P-010-06"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-06.
            IF NEDIT-06X = "$ 0/00,123.45DB"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-06X
            END-IF.
      *
            MOVE "P-010-07"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-07.
            IF NEDIT-07X = "-$ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-07X
            END-IF.
      *
            MOVE "P-010-08"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-08.
            IF NEDIT-08X = "-$ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-08X
            END-IF.
      *ケース2.
            MOVE "P-010-11"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-11.
            IF NEDIT-11X = "  $ 0/00,123.45-"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-11X
            END-IF.
      *
            MOVE "P-010-12"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-12.
            IF NEDIT-12X = "  $ 0/00,123.45-"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-12X
            END-IF.
      *
            MOVE "P-010-13"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-13.
            IF NEDIT-13X = "  $ 0/00,123.45CR"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-13X
            END-IF.
      *
            MOVE "P-010-14"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-14.
            IF NEDIT-14X = "-  $ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-14X
            END-IF.
      *
            MOVE "P-010-15"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-15.
            IF NEDIT-15X = "-  $ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-15X
            END-IF.
      *
            MOVE "P-010-16"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-16.
            IF NEDIT-16X = "  $ 0/00,123.45DB"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-16X
            END-IF.
      *
            MOVE "P-010-17"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-17.
            IF NEDIT-17X = "-  $ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-17X
            END-IF.
      *
            MOVE "P-010-18"        TO CASE-ID.
            MOVE -123.456 TO NEDIT-18.
            IF NEDIT-18X = "-  $ 0/00,123.45"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" NEDIT-18X
            END-IF.
      *
      *
            DISPLAY "TEST END   (CS-07)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

