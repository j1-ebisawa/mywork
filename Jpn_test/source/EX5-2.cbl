      ******************************************************************
      *    テストケース：5-2
      *    プログラム名：日本語化テスト （手続き部）ACCEPT命令
      *    処理概要　　：ACCEPT命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～７
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-2.
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
       01  W-G.
           05  A-01.
               10  A-01-1     PIC N(5).
           05  A-02           PIC N(5).
           05  A-03           PIC N(5).
           05  A-04           PIC N(5).
           05  A-05           PIC N(5).
           05  A-06.
               10  A-06-1     PIC N(5).
           05  A-07.
               10  A-07-1     PIC N/N/N.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-2)".
      *ケース1.日本語を指定した場合、1バイト文字が入力できるか
            MOVE "P-010-01"        TO CASE-ID.
            MOVE SPACE       TO A-01-1.
            DISPLAY "INPUT:'ABCDEFGHIJ'".
            ACCEPT  A-01-1.
            IF A-01 = "ABCDEFGHIJ"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-01=" A-01
            END-IF.
      *ケース2：日本語を指定した場合、2バイト文字が入力できるか
            MOVE "P-020-01"        TO CASE-ID.
            MOVE SPACE       TO A-02.
            DISPLAY "INPUT:'あいうえお'".
            ACCEPT A-02.
            IF A-02 = "あいうえお"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-02=" A-02
            END-IF.
      *ケース3：日本語を指定した場合、2バイト+1バイト文字混在が入力できるか
            MOVE "P-030-01"        TO CASE-ID.
            MOVE SPACE       TO A-03.
            DISPLAY "INPUT:'あい1234お'".
            ACCEPT A-03.
            IF A-03 = "あい1234お"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-03=" A-03
            END-IF.
      *ケース4：受け取り側項目より、長いデータを入力したとき、切り捨てられるか。
            MOVE "P-040-01"        TO CASE-ID.
            MOVE SPACE       TO A-04.
            DISPLAY "INPUT:'あいうえおかき'".
            ACCEPT A-04.
            IF A-04 = "あいうえお"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-04=" A-04
            END-IF.
      *ケース5：受け取り側項目より、短い2バイト文字データを入力したとき、
      *         サイズ分だけ送られて、以降は2バイト空白が埋められるか。  20110921修正
            MOVE "P-050-01"        TO CASE-ID.
            MOVE ALL "＊"       TO A-05.                                 *>20110921修正
            DISPLAY "INPUT:'あい'".
            ACCEPT A-05.
            IF A-05 = "あい　　　"                                      *>20110921修正
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-05=" A-05
            END-IF.
      *ケース6：受け取り側項目より、短い1バイト文字データを入力したとき、
      *         サイズ分だけ送られて、以降は2バイト空白が埋められるか。  20110921修正
            MOVE "P-060-01"        TO CASE-ID.
            MOVE ALL "＊"      TO A-06-1.                                *>20110921修正
            DISPLAY "INPUT:'ABCD'".
            ACCEPT A-06-1.                                              *>20110921修正
            IF A-06 = "ABCD　　　"                                      *>20110921修正
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-06=" A-06
            END-IF.
      *ケース7：日本語編集項目が受け取り側のとき、編集はされないか。
      *         サイズ分だけ送られて、以降は2バイト空白が埋められるか。  20110921修正
            MOVE "P-070-01"        TO CASE-ID.
            MOVE ALL "＊"       TO A-07-1.                               *>20110921修正
            DISPLAY "INPUT:'あいう'".
            ACCEPT A-07-1.                                              *>20110921修正
            IF A-07 = "あいう　　"                                      *>20110921修正
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-07=" A-07
            END-IF.
      *ケース8：日本語項目以外が受け取り側のとき、短いデータを入力すると  20110921追加
      *         サイズ分だけ送られて、以降は1バイト空白が埋められるか。   20110921追加
            MOVE "P-080-01"        TO CASE-ID.                          *>20110921追加
            MOVE ALL "*"       TO A-06.                                 *>20110921追加
            DISPLAY "INPUT:'あいう12'".                                 *>20110921追加
            ACCEPT A-06.                                                *>20110921追加
            IF A-06 = "あいう12  "                                      *>20110921追加
                                     DISPLAY CASE-ID "OK"               *>20110921追加
               ELSE                  DISPLAY CASE-ID "NG" "A-06=" A-06  *>20110921追加
            END-IF.                                                     *>20110921追加
      *
            DISPLAY "TEST END   (EX5-2)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

