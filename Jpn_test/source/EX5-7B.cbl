      ******************************************************************
      *    テストケース：5-7B
      *    プログラム名：日本語化テスト （手続き部）UNSTRING命令
      *    処理概要　　：UNSTRING命令が正しく構文エラーチェックされるか
      *    ワーニングエラーとし実行できる
      *  --------------------------------------------------------------
      *   テストケース:２０
      *                 エラーチェックのため、実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-7B.
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
       01  OVER-SW            PIC 9.
       01  CASE-ID            PIC X(10).
       01  G-01        PIC N(10).
       01  W-PTR       PIC 999.
       01  W-TALLY     PIC 999.
       01  W-CNT-1     PIC 999.
       01  W-CNT-2     PIC 999.
       01  W-DEL-1     PIC N.
       01  W-DEL-2     PIC N.
       01  W-DEL2-1    PIC NN.
       01  W-DEL2-2    PIC NN.
       01  W-INTO.
           05  W-INTO-1    PIC N(10).
           05  W-INTO-2    PIC N(10).
           05  W-INTO-3    PIC N(10).
           05  W-INTO-4    PIC N(10).
           05  W-INTO-5    PIC N(10).
           05  W-DELIN-1   PIC N.
           05  W-DELIN-2   PIC N.
           05  W-DELIN2-1  PIC NN.
           05  W-DELIN2-2  PIC NN.
       01  X-01     PIC X(5).
       01  N-01     PIC 9(5).
       01  W-INTO-X PIC X(20).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START  (EX5-7B)".
      *ケース20.エラーチェック
           MOVE "あいうえおかきくけこ" TO G-01.
           MOVE ALL "＊" TO W-INTO.
           MOVE 1 TO W-PTR.
           MOVE 0 TO W-TALLY.
           
           UNSTRING G-01 DELIMITED BY "A" OR "B"
                INTO W-INTO-1 DELIMITER IN W-DELIN2-1 
                              COUNT IN W-CNT-1
           END-UNSTRING.
           DISPLAY "EX5-7B-01".
           DISPLAY "W-INTO-1:" W-INTO-1
           DISPLAY "W-DELIN2-1:" W-DELIN2-1 
           DISPLAY "W-CNT-1:" W-CNT-1
           
           
           UNSTRING G-01 DELIMITED BY X-01
                INTO W-INTO-1 DELIMITER IN W-DELIN-1 
                              COUNT IN W-CNT-1
                ON OVERFLOW MOVE 1 TO OVER-SW
                NOT ON OVERFLOW MOVE 0 TO OVER-SW
           END-UNSTRING.
           DISPLAY "EX5-7B-02".
           DISPLAY "W-INTO-1:" W-INTO-1
           DISPLAY "W-DELIN-1:" W-DELIN-1 
           DISPLAY "W-CNT-1:" W-CNT-1
           DISPLAY "OVER-SW:" OVER-SW
           
           UNSTRING G-01 DELIMITED BY "あ"
                INTO X-01 DELIMITER IN W-DELIN-1 
                          COUNT IN W-CNT-1
                ON OVERFLOW MOVE 1 TO OVER-SW
                NOT ON OVERFLOW MOVE 0 TO OVER-SW
           END-UNSTRING.
           DISPLAY "EX5-7B-03".
           DISPLAY "X-01:" X-01
           DISPLAY "W-DELIN-1:" W-DELIN-1 
           DISPLAY "W-CNT-1:" W-CNT-1
           DISPLAY "OVER-SW:" OVER-SW
           
           UNSTRING G-01 DELIMITED BY "か"
                INTO W-INTO-1 DELIMITER IN X-01 
                              COUNT IN W-CNT-1
                ON OVERFLOW MOVE 1 TO OVER-SW
                NOT ON OVERFLOW MOVE 0 TO OVER-SW
           END-UNSTRING.
           DISPLAY "EX5-7B-04".
           DISPLAY "W-INTO-1:" W-INTO-1
           DISPLAY "X-01:" X-01
           DISPLAY "W-CNT-1:" W-CNT-1
           DISPLAY "OVER-SW:" OVER-SW
           
           UNSTRING X-01 DELIMITED BY W-DEL-1
                INTO W-INTO-1 DELIMITER IN W-DELIN-1 
                              COUNT IN W-CNT-1
                ON OVERFLOW MOVE 1 TO OVER-SW
                NOT ON OVERFLOW MOVE 0 TO OVER-SW
           END-UNSTRING.
           DISPLAY "EX5-7B-05".
           DISPLAY "W-INTO-1:" W-INTO-1
           DISPLAY "W-DELIN-1:" W-DELIN-1
           DISPLAY "W-CNT-1:" W-CNT-1
           DISPLAY "OVER-SW:" OVER-SW
      ***
           DISPLAY "EX5-7B NG".
            DISPLAY "TEST END    (EX5-7B)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

