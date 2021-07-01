      ******************************************************************
      *    テストケース：4-1A
      *    プログラム名：日本語化テスト （データ部）JUSTIFIED句
      *    処理概要　　：JUST指定が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～４
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-1A.
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
       01  G-01               PIC N(5).
       01  GE-01              PIC N/N/N.
       01  G-J-01             PIC N(5) JUST.
       01  L-J-10             PIC N(10).
       01  L-J-3              PIC N(3).
       01  L-X-10             PIC X(10).
       01  L-X-3              PIC X(3).
       
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-1A)".
      *  ケース1.日本語データ（JUST句なし）
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            IF G-01 = "あいうえお"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE "あいう"               TO G-01.
            IF G-01 = "あいう　　"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE "あいうえお"           TO GE-01.
            IF GE-01 = "あ／い／う"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *
            MOVE "P-010-04"             TO CASE-ID.
            MOVE "あ"                   TO GE-01.
            IF GE-01 = "あ／　／　"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *
       P-020. 
      *  ケース2.日本語定数→日本語データ（JUST句あり）
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-J-01
            IF G-J-01 = "かきくけこ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE "あいう"               TO G-J-01.  
            IF G-J-01 = "　　あいう"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-030. 
      *  ケース3.日本語データ→日本語データ（JUST句あり）
            MOVE "P-030-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-J-10.
            MOVE L-J-10 TO G-J-01.
            IF G-J-01 = "かきくけこ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            MOVE "あいう"               TO L-J-3.
            MOVE L-J-3 TO G-J-01  
            IF G-J-01 = "　　あいう"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
       P-040. 
      *  ケース4.英数字データ→日本語データ（JUST句あり）
      *
            MOVE "P-040-01"             TO CASE-ID.
            MOVE "ｱｲｳｴｵｶｷｸｹｺ" TO L-X-10.
            MOVE L-X-10 TO G-J-01.
            IF G-J-01 = "カキクケコ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-J-01
            END-IF.
      *
            MOVE "P-040-02"             TO CASE-ID.
            MOVE "ｱｲｳ"               TO L-X-3.
            MOVE L-X-3 TO G-J-01
            IF G-J-01 = "　　アイウ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-J-01
            END-IF.
      *
      *
            DISPLAY "TEST END   (EX4-1A)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

