      ******************************************************************
      *    テストケース：3-5A
      *    プログラム名：日本語化テスト （言語要素）行のつなぎ
      *    処理概要　　：行のつなぎが正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～４
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-5A.
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
       01  データ名１         PIC X(10).
       01  データ名-abc       PIC X(10).
       01  原価ＡＢＣ         PIC X(10).
       01  G-01               PIC N(5).
       01  G-02               PIC N(5).
       01  G-03               PIC N(5).
       01  G-04               PIC N(5).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX3-5A)".
      *  ケース1.日本語利用者語の継続
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE "ABC" TO   データ
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE "ABC" TO   データ名-a
      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE "ABC" TO            原価
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-020. 
      *  ケース2.日本語定数の継続
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE                                                 "日本語
      -     "ＡＢ"   TO G-01
            IF G-01 = "日本語ＡＢ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE                                              "日本語”"
            & "Ａ"   TO G-01
            IF G-01 = "日本語”Ａ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:"  G-01
            END-IF.
      *
       P-030. 
      *  ケース3.日本語利用者語の継続(継続行の連続）
            MOVE "P-030-01"             TO CASE-ID.
            MOVE "ABC" TO   デー
      -                         タ
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            MOVE "ABC" TO   データ
      -                          名-a
      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-03"             TO CASE-ID.
            MOVE "ABC" TO            原
      -                                価
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
       P-040. 
      *  ケース4.日本語利用者語の継続(間にコメント行）
      *
            MOVE "P-040-01"             TO CASE-ID.
            MOVE "ABC" TO   データ
      *
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-02"             TO CASE-ID.
            MOVE "ABC" TO   データ名-a
      *
      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-03"             TO CASE-ID.
            MOVE "ABC" TO            原価
      *
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX3-5A)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

