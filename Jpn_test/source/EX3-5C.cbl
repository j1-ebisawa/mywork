      ******************************************************************
      *    テストケース：3-5C
      *    プログラム名：日本語化テスト （言語要素）行のつなぎ
      *    処理概要　　：行のつなぎが正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *     意図に反してエラーとなってしまった。
      *   テストケース:９
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-5C.
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
            DISPLAY "TEST START (EX3-5C)".
      *
      *  ケース9.日本語利用者語の継続（インラインコメントとの組み合わせ）
      *
            MOVE "P-090-01"             TO CASE-ID.
            MOVE "ABC" TO   データ                             *>comment
      -                          名１.
            IF データ名１ = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-090-02"             TO CASE-ID.
            MOVE "ABC" TO   データ名-a                       *>コメント
      -                              bc.
            IF データ名-abc = "ABC"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-090-03"             TO CASE-ID.
            MOVE "ABC" TO            原価                      *>コメント
      -                                  ＡＢＣ.
            IF 原価ＡＢＣ = "ABC"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX3-5C)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

