      ******************************************************************
      *    テストケース：3-3-6B
      *    プログラム名：日本語化テスト （言語要素）部分参照
      *    処理概要　　：部分参照が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１２～１４
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-3-6B.
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
      *>my comment
      ******************************************************************
       WORKING-STORAGE       SECTION.
      ******************************************************************
       01  OMIT-WK            PIC X.
       01  CASE-ID            PIC X(10).
       01  W-I                PIC 999.
       01  P                  PIC 999.
       01  L                  PIC 999.
       01  W-L-GRP.
           05  L-A            PIC A(10).
           05  W-L-GRP-10     REDEFINES L-A.
               10  W-L-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  L-AN           PIC X(10).
           05  L-ANE          PIC XX/XX/XX.
           05  L-ZONE         PIC 9(5).
           05  L-ZONE-DEC     PIC S9(3)V9(2).
           05  L-PACK         PIC S9(5)       COMP-3.
           05  L-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  L-BIN          PIC S9(5)       COMP.
           05  L-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  L-NE           PIC ----9.
           05  L-G            PIC N(10).
           05  FILLER         REDEFINES L-G.
               10  L-G-5      PIC N(5).
               10  L-G-3      PIC N(3).
               10  L-G-2      PIC N(2).
           05  L-GE           PIC N/N/N.
           05  FILLER         REDEFINES L-GE.
               10  L-GE-3     PIC N/N.
               10  L-GE-2     PIC /N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  R-GRP-10       REDEFINES R-A.
               10  R-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  FILLER         REDEFINES R-ANE.
               10  R-ANE-3    PIC XX/XX/.
               10  R-ANE-2    PIC XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC N(10).
           05  FILLER         REDEFINES R-G.
               10  R-G-5      PIC N(5).
               10  R-G-3      PIC N(3).
               10  R-G-2      PIC N(2).
           05  R-GJ           PIC N(10) JUST.
           05  FILLER         REDEFINES R-GJ.
               10  R-GJ-5     PIC N(5)  JUST.
               10  R-GJ-3     PIC N(3)  JUST.
               10  R-GJ-2     PIC N(2)  JUST.
           05  R-GE           PIC N/N/N.
           05  FILLER         REDEFINES R-GE.
               10  R-GE-3     PIC N/N.
               10  R-GE-2     PIC /N.
           05  R-GE-NB        PIC NNN/NBN/.
           05  R-GE-NBZ       PIC NNN/N0BN.
        01  W-OCC-GRP.
               10  W-OCC      OCCURS 50 INDEXED BY IDX-01.
                 15  W-OCC-G  PIC N(5).
                 15  W-OCC-GE PIC N/N/N.
                 15  W-OCC-X  PIC X(5).

      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX3-3-6B)".
      *  ケース12.日本語/日本語編集 (ACCEPT/DISPLAY)
      *
            MOVE "P-120-01"             TO CASE-ID.
            MOVE ALL "＊" TO L-G.
            DISPLAY "KEY-IN=あいうえお".
            *>ACCEPT L-G(2:5).
            move "あいうえお" to L-G(2:5). 
            
            IF L-G = "＊あいうえお＊＊＊＊"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-G                                                
                                        display "＊あいうえお＊＊＊＊"
                                        display L-G
            END-IF.
      *
            MOVE "P-120-02"             TO CASE-ID.
            DISPLAY L-G.
            DISPLAY L-G(1:1)  L-G(2:5)  L-G(7:4)
            DISPLAY "It's OK if above 2 lines are same".
            DISPLAY "同じならばOK".
            *>ACCEPT OMIT-WK.
      *
       P-130. 
      *  ケース13.日本語/日本語編集 （EVALUATE/SEARCH WHEN）
      *
            MOVE "P-130-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            MOVE 4 TO P.
            MOVE 2 TO L.
            EVALUATE L-G(P:L) 
              WHEN "えお"               DISPLAY CASE-ID "OK"
              WHEN OTHER              DISPLAY CASE-ID "NG"
            END-EVALUATE.
      *
            MOVE "P-130-02"             TO CASE-ID.
            MOVE "あああああ"      TO W-OCC-G(1).
            MOVE "aaaaa"           TO W-OCC-X(1).
            MOVE "わわわわわ"      TO W-OCC-G(2).
            MOVE "wwwww"           TO W-OCC-X(2).
            MOVE "かかかかか"      TO W-OCC-G(3).
            MOVE "kkkkk"           TO W-OCC-X(3).
            MOVE "ぱぱぱぱぱ"      TO W-OCC-G(4).
            MOVE "ppppp"           TO W-OCC-X(4).
            MOVE LOW-VALUE         TO W-OCC-G(5).
            MOVE "xxxxx"           TO W-OCC-X(5).
            SET IDX-01 TO 1.
            SEARCH W-OCC VARYING IDX-01
                   AT END     DISPLAY CASE-ID "NG"
                   WHEN W-OCC-G(IDX-01)(1:3) = "かかか"
                              DISPLAY CASE-ID "OK"
            END-SEARCH.
      *
      *
       P-140. 
      *  ケース14.長さ省略
      *
            MOVE "P-140-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            MOVE 3 TO P.
            MOVE 2 TO L.
            MOVE ALL "＊" TO L-G(P:).
            
            IF L-G = "あい＊＊＊＊＊＊＊＊"
                                        DISPLAY CASE-ID "OK"
              ELSE                      DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-02"             TO CASE-ID.
            MOVE "あいう" TO L-GE.
            MOVE ALL "＊"  TO L-GE(3:).
            
            IF L-GE = "あ／＊＊＊"
                                        DISPLAY CASE-ID "OK"
              ELSE                      DISPLAY CASE-ID "NG"
            END-IF.
      *
             DISPLAY "TEST END   (EX3-3-6B)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

