      ******************************************************************
      *    テストケース：QA-33
      *    プログラム名：日本語化テスト QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-35.
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
       01  AN-01              PIC X(10).
       01  AN-02-G.
           05  AN-02-V.
             10  FILLER         PIC X(2) VALUE "あ".
             10  FILLER         PIC X    VALUE X"00".
             10  FILLER         PIC X(2) VALUE "い".
           05  AN-02            REDEFINES AN-02-V
                                PIC X(5).
      *
       01  G-01               PIC N(5).
       01  G-02               PIC N(5).
       01  G-03-G.
           05  G-03V.
             10  FILLER         PIC X(2) VALUE "あ".
             10  FILLER         PIC X(2) VALUE X"0000".
             10  FILLER         PIC X(2) VALUE "い".
             10  FILLER         PIC N(2) VALUE "　　".
           05  G-03             REDEFINES G-03V
                                PIC N(5).
      *
      *****************************************************
       LINKAGE SECTION.
       01  BASE-CHAR    PIC X(10).
      *****************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-35 test start".
      *  ケース4.英数字データの日本語への転記（１B→２B変換、ILLEGAL DATA）
      *
            MOVE "QA35-01"        TO CASE-ID.
            MOVE "あ" & X"00" & "い" TO AN-01.
            MOVE AN-01 TO G-01.
            IF G-01 = "あ" & X"0000" & "い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *     
            MOVE "QA35-02"        TO CASE-ID.
            MOVE AN-02 TO G-02.
            IF G-02 = G-03 
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "QA-35 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

