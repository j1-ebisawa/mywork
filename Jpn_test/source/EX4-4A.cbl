      ******************************************************************
      *    テストケース：4-4A
      *    プログラム名：日本語化テスト （データ部）VALUE句
      *    処理概要　　：VALUE句指定が正しく実行できるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-4A.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-18.
       DATE-COMPILED.        2011-08-18.
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
       78  C-01      VALUE "あいう".
       78  C-02      VALUE "１２３".
       78  C-03      VALUE "１／２／３".
       78  C-04      VALUE "ＡＢＣ123".

       01  G-01      PIC NNN    VALUE C-02.
       01  GE-01     PIC N/N/N  VALUE "Ａ／Ｂ／Ｃ".
       01  X-03      PIC X(9).
       01  x-99      pic x(9).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-4A)".
      *  ケース1.78定数名指定のVALUE句
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE C-01 TO G-01.
            IF G-01 = "あいう" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE C-02               TO GE-01.
            IF GE-01 = C-03
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE C-04 TO X-03.
            IF X-03 = "ＡＢＣ123"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX4-4A)".
            ACCEPT OMIT-WK.
      *
            GOBACK
            .

