      ******************************************************************
      *    テストケース：5-1-1F
      *    プログラム名：日本語化テスト （手続き部）条件 比較条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:４９～５０
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-1F.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-18.
       DATE-COMPILED.        2011-08-18.
      ******************************************************************
       ENVIRONMENT           DIVISION.
      ******************************************************************
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      PC.
       OBJECT-COMPUTER.      PC
              PROGRAM COLLATING SEQUENCE IS EBC.
       SPECIAL-NAMES.
              ALPHABET EBC IS EBCDIC.
      ******************************************************************
       DATA                  DIVISION.
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE       SECTION.
      ******************************************************************
       01  OMIT-WK            PIC X.
       01  CASE-ID            PIC X(10).
       01  W-L-GRP.
           05  L-A            PIC A(10).
           05  L-AN           PIC X(10).
           05  L-ANE          PIC XX/XX/XX.
           05  L-ZONE         PIC 9(5).
           05  L-ZONE-DEC     PIC S9(3)V9(2).
           05  L-PACK         PIC S9(5)       COMP-3.
           05  L-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  L-BIN          PIC S9(5)       COMP.
           05  L-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  L-NE           PIC ----9.
           05  L-G            PIC N(5).
           05  FILLER         REDEFINES L-G.
               10  L-G-3      PIC N(3).
               10  L-G-2      PIC N(2).
           05  L-GE           PIC N/N/N.
           05  FILLER         REDEFINES L-GE.
               10  L-GE-3     PIC N/N.
               10  L-GE-2     PIC /N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC N(5).
           05  FILLER         REDEFINES R-G.
               10  R-G-3      PIC N(3).
               10  R-G-2      PIC N(2).
           05  R-GE           PIC N/N/N.
           05  FILLER         REDEFINES R-GE.
               10  R-GE-3     PIC N/N.
               10  R-GE-2     PIC /N.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-490. 
            DISPLAY "TEST START (EX5-1-1F)".
      *  ケース49.比較表の日本語関係以外の欄
      *  数字＋英数字
            MOVE 12345             TO L-ZONE.
            MOVE "12345"           TO R-AN.
      *
            MOVE "P-490-01"        TO CASE-ID.
            IF L-ZONE = R-AN         DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *  英数字＋文字定数
            MOVE "ABCDE"           TO L-AN.
      *
            MOVE "P-490-02"        TO CASE-ID.
            IF L-AN = "ABCDE"        DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *  数字＋数字定数
            MOVE 123               TO L-ZONE.
            MOVE 123.45            TO L-ZONE-DEC.
            MOVE 321               TO L-PACK.
            MOVE 321.54            TO L-PACK-DEC.
            MOVE 789               TO L-BIN.
            MOVE 789.12            TO L-BIN-DEC.
      *
            MOVE "P-490-03"        TO CASE-ID.
            IF L-ZONE = 123          DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-490-04"        TO CASE-ID.
            IF L-ZONE-DEC = 123.45   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-490-05"        TO CASE-ID.
            IF L-PACK = 321          DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-490-06"        TO CASE-ID.
            IF L-PACK-DEC = 321.54   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-490-07"        TO CASE-ID.
            IF L-BIN = 789           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-490-08"        TO CASE-ID.
            IF L-BIN-DEC = 789.12    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-500. 
      *  ケース50.（左辺）表意定数＋（右辺）日本語編集
            MOVE "あああああ"         TO L-G.
            MOVE "０００００"         TO R-G.
      *
            MOVE "P-500-01"        TO CASE-ID.
            IF L-G = R-G             DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-500-02"        TO CASE-ID.
            IF L-G > R-G             DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-500-03"        TO CASE-ID.
            IF L-G < R-G             DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            DISPLAY "TEST END   (EX5-1-1F)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

