      ******************************************************************
      *    テストケース：3-5B
      *    プログラム名：日本語化テスト （言語要素）行のつなぎ
      *    処理概要　　：行のつなぎが、正しく処理されるか。
      *  --------------------------------------------------------------
      *   テストケース：４～８
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-5B.
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
       P-050. 
            DISPLAY "TEST START (EX3-5B)".
      *  ケース5.日本語利用者語の継続+空白行
      *
            MOVE "P-050-01"             TO CASE-ID.
            MOVE "ABC" TO   データ
            
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-050-02"             TO CASE-ID.
            MOVE "ABC" TO   データ名-a

      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-050-03"             TO CASE-ID.
            MOVE "ABC" TO            原価

      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-060. 
      *  ケース6.日本語定数の継続+空白行
      *
            MOVE "P-060-01"             TO CASE-ID.
            MOVE                                                 "日本語

      -     "ＡＢ"   TO G-01
            IF G-01 = "日本語ＡＢ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-060-02"             TO CASE-ID.
            MOVE                                              "日本語”"

            & "Ａ"   TO G-01
            IF G-01 = "日本語”Ａ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-070. 
      *  ケース3.日本語利用者語の継続(継続行の連続）+空白行
            MOVE "P-070-01"             TO CASE-ID.
            MOVE "ABC" TO   デー

      -                         タ
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-070-02"             TO CASE-ID.
            MOVE "ABC" TO   データ
      -                           名-a

      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-070-03"             TO CASE-ID.
            MOVE "ABC" TO            原

      -                                価
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
       P-080. 
      *  ケース8.日本語利用者語の継続(間にコメント行）+空白行
      *
            MOVE "P-080-01"             TO CASE-ID.
            MOVE "ABC" TO   データ

      *
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-080-02"             TO CASE-ID.
            MOVE "ABC" TO   データ名-a
      *

      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-080-03"             TO CASE-ID.
            MOVE "ABC" TO            原価

      *
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
      *
            DISPLAY "TEST END   (EX3-5B)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

