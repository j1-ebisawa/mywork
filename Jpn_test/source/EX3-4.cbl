      ******************************************************************
      *    テストケース：3-4
      *    プログラム名：日本語化テスト （言語要素）英数字データ
      *    処理概要　　：全角半角混合の英数字データが正しく実行されるか
      *    　　　　　　　をチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～５
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-4.
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
       01  W-I                PIC 999.
       01  P                  PIC 999.
       01  L                  PIC 999.
       01  AN-01              PIC X(10)    VALUE "あいa".
       01  AN-02              PIC X(2)9(8) VALUE "あ1234".
       01  AN-03              PIC X(2)A(8) VALUE "あabcd".
       01  AN-04              PIC X(10).
       01  G-01               PIC N(5).
       01  G-02               PIC N(5).
       01  G-03               PIC N(5).
       01  G-04               PIC N(5).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX3-4)".
      *  ケース1.英数字データ項目の定義とVALUE句
      *
            MOVE "P-010-01"             TO CASE-ID.
            IF AN-01 = "あいa"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            IF AN-02 = "あ1234"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            IF AN-03 = "あabcd"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-020. 
      *  ケース2.英数字データ項目の定義と混合文字定数の転記
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE "あいx" TO AN-01.
            IF AN-01 = "あいx"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE "あ9876" TO AN-02.
            IF AN-02 = "あ9876"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-03"             TO CASE-ID.
            MOVE "あxyzw" TO AN-03.
            IF AN-03 = "あxyzw"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-030. 
      *  ケース3.英数字データの日本語への転記（１B→２B変換）
            MOVE "P-030-01"             TO CASE-ID.
            MOVE "あいx" TO AN-01.
            MOVE AN-01 TO G-01.
            IF G-01 = "あいｘ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            MOVE "あ9876" TO AN-02.
            MOVE AN-02 TO G-02.
            IF G-02 = "あ９８７６"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-02
            END-IF.
      *
            MOVE "P-030-03"             TO CASE-ID.
            MOVE "あxyzw" TO AN-03.
            MOVE AN-03 TO G-03.
            IF G-03 = "あｘｙｚｗ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-03
            END-IF.
       P-040. 
      *  ケース4.英数字データの日本語への転記（１B→２B変換、ILLEGAL DATA）
      *
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "あ" & X"00" & "い" TO AN-01.
            MOVE AN-01 TO G-01.
            IF G-01 = "あ" & X"0000" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-040-02"        TO CASE-ID.
            MOVE "あ" & X"FF" & "い" TO AN-02.
            MOVE AN-02 TO G-02.
            IF G-02 = "あ" & X"FFFF" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-02
            END-IF.
      *
            MOVE "P-040-03"        TO CASE-ID.
            MOVE "あ" & X"20" & "い" TO AN-03.
            MOVE AN-03 TO G-03.
            IF G-03 = "あ" & X"8140" & "い"     
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-03
            END-IF.
      *
            MOVE "P-040-04"        TO CASE-ID.
            MOVE "あ" & X"03" & "い" TO AN-04.
            MOVE AN-04 TO G-04.
            IF G-04 = "あ" & X"8140" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-04
            END-IF.
      *
       P-050. 
      *  ケース5.英数字データの日本語への転記（１B→２B変換、ILLEGAL DATA）
      *
            MOVE "P-050-01"        TO CASE-ID.
            MOVE "あ" & X"0000" & "い" TO AN-01.
            MOVE AN-01 TO G-01.
            IF G-01 = "あ" & X"0000" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE "あ" & X"FFFF" & "い" TO AN-02.
            MOVE AN-02 TO G-02.
            IF G-02 = "あ" & X"FFFF" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-02
            END-IF.
      *
            MOVE "P-050-03"        TO CASE-ID.
            MOVE "あ" & X"2020" & "い" TO AN-03.
            MOVE AN-03 TO G-03.
            IF G-03 = "あ" & X"81408140" & "い"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-03
            END-IF.
      *
            MOVE "P-050-04"        TO CASE-ID.
            MOVE "あ" & X"0303" & "い" TO AN-04.
            MOVE AN-04 TO G-04.
            IF G-04 = "あ" & X"81408140" & "い"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" G-04
            END-IF.
      *
            DISPLAY "TEST END   (EX3-4)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

