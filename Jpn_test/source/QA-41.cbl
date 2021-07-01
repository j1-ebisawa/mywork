      ******************************************************************
      *    テストケース：QA-33
      *    プログラム名：日本語化テスト QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-41.
       AUTHOR.               TSH.
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
       01  CASE-ID            PIC X(30).
      *
       01  G-01               PIC N(10).
       01  X-01               PIC X(20).
      *
       01  X-02               PIC X(2) VALUE "あ".
       01  X-03               PIC X(2) VALUE "＊".
       01  WK-TALLY           PIC 999.
      *
      *****************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-41 test start".
      *
            MOVE "QA41-01"        TO CASE-ID.
            MOVE "あいうえおあいうえお" TO G-01.
            INSPECT G-01 REPLACING ALL "あ" BY "＊".
            IF G-01 = "＊いうえお＊いうえお"   
                             DISPLAY CASE-ID "OK"
               ELSE          DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *     
            MOVE "QA41-02"        TO CASE-ID.
            MOVE "あいうえおあいうえお" TO X-01.
            INSPECT X-01 REPLACING ALL "あ" BY "＊".
            IF X-01 = "＊いうえお＊いうえお"
                              DISPLAY CASE-ID "OK"
               ELSE           DISPLAY CASE-ID "NG:" X-01
            END-IF.
      *     
            MOVE "QA41-03"        TO CASE-ID.
            MOVE "aあいうえおあいうえb" TO X-01.
            INSPECT X-01 REPLACING ALL "あ" BY "＊".
            IF X-01 = "a＊いうえお＊いうえb"
                              DISPLAY CASE-ID "OK"
               ELSE           DISPLAY CASE-ID "NG:" X-01
            END-IF.
      *
            MOVE "QA41-04"        TO CASE-ID.
            MOVE "あいうえおあいうえお" TO G-01.
            MOVE 0 TO WK-TALLY.
            INSPECT G-01 TALLYING WK-TALLY FOR ALL "あ".
            IF WK-TALLY = 2
                             DISPLAY CASE-ID "OK"
               ELSE          DISPLAY CASE-ID "NG:" WK-TALLY
            END-IF.
      *     
            MOVE "QA41-05"        TO CASE-ID.
            MOVE "あいうえおあいうえお" TO X-01.
            MOVE 0 TO WK-TALLY.
            INSPECT X-01 TALLYING WK-TALLY FOR ALL "あ".
            IF WK-TALLY = 2
                              DISPLAY CASE-ID "OK"
               ELSE           DISPLAY CASE-ID "NG:" WK-TALLY
            END-IF.
      *     
            MOVE "QA41-06"        TO CASE-ID.
            MOVE "aあいうえおあいうえb" TO X-01.
            MOVE 0 TO WK-TALLY.
            INSPECT X-01 TALLYING WK-TALLY FOR ALL "あ".
            IF WK-TALLY = 2
                              DISPLAY CASE-ID "OK"
               ELSE           DISPLAY CASE-ID "NG:" WK-TALLY
            END-IF.
      *
            MOVE "QA41-07"        TO CASE-ID.
            MOVE "あいうえおあいうえお" TO G-01.
            INSPECT G-01 REPLACING ALL X-02 BY X-03.
            IF G-01 = "＊いうえお＊いうえお"   
                             DISPLAY CASE-ID "OK"
               ELSE          DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            DISPLAY "QA-41 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

