      ******************************************************************
      *    テストケース：3-3-6A
      *    プログラム名：日本語化テスト （言語要素）部分参照
      *    処理概要　　：部分参照が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～１１
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-3-6A.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-18.
       DATE-COMPILED.        2011-08-18.
      ******************************************************************
       ENVIRONMENT          DIVISION.
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
            DISPLAY "TEST START (EX3-3-6A)".
      *  ケース1.日本語/日本語編集 (1:3)
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            
            IF L-G(3:2) = "うえ"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG;" L-G
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE "あいう" TO L-GE.
            
            IF L-GE(3:2) = "い／"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE "＊＊＊＊＊＊＊＊＊＊"               TO L-G.
            MOVE "あいう"               TO L-G(3:3).
            
            IF L-G = "＊＊あいう＊＊＊＊＊"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-010-04"             TO CASE-ID.
            MOVE "＊＊＊"                TO L-GE.
            MOVE "？"                   TO L-GE(3:1).
            
            IF L-GE = "＊／？／＊"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:"  L-GE
            END-IF.
      *
       P-020. 
      *  ケース2.日本語/日本語編集 (P:L)
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            MOVE 4 TO P.
            MOVE 2 TO L.
            IF L-G(P:L) = "えお"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:"  L-G
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE "あいう"                  TO L-GE.
            MOVE 3 TO P.
            MOVE 2 TO L.
            IF L-GE(P:L) = "い／"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-020-03"             TO CASE-ID.
            MOVE "＊＊＊＊＊＊＊＊＊＊"    TO L-G.
            MOVE 3 TO P.
            MOVE 3 TO L.
            MOVE "あいう"               TO L-G(P:L).
            IF L-G = "＊＊あいう＊＊＊＊＊"    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-020-04"             TO CASE-ID.
            MOVE "＊＊＊"                TO L-GE.
            MOVE 3 TO P.
            MOVE 2 TO L.
            MOVE "１２"                 TO L-GE(P:L).
            IF L-GE = "＊／１２＊"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
       P-030. 
      *  ケース3.日本語/日本語編集 (I*2:J+2)
            MOVE "P-030-01"             TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            MOVE 1 TO P.
            MOVE 2 TO L.
            IF L-G(P * 2 :  L + 3) = "いうえおか"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:"  L-G
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            MOVE "あいう" TO L-GE.
            MOVE 1 TO P.
            MOVE 1 TO L.
            IF L-GE(P * 2 : L + 1) = "／い"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-030-03"             TO CASE-ID.
            MOVE "＊＊＊＊＊＊＊＊＊＊"    TO L-G.
            MOVE 1 TO P.
            MOVE 2 TO L.
            MOVE "あいう"               TO L-G(P + 1:L * 2).
            IF L-G = "＊あいう　＊＊＊＊＊"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-030-04"             TO CASE-ID.
            MOVE "＊＊＊"                TO L-GE.
            MOVE 3 TO P.
            MOVE 1 TO L.
            MOVE "？"                   TO L-GE(P:L*2).
            IF L-GE = "＊／？　＊"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" L-GE
            END-IF.
       P-040. 
      *  ケース4.日本語→日本語
      *
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "＊＊＊＊＊"        TO L-G-5.
            MOVE "あいうえおかきくけこ" TO L-G-5(1:3).
            IF L-G-5 = "あいう＊＊"    
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:"  L-G-5
            END-IF.
      *
            MOVE "P-040-02"        TO CASE-ID.
            MOVE "＊＊＊＊＊"        TO L-G-5.
            MOVE "あい"            TO L-G-5(2:3).
            IF L-G-5 = "＊あい　＊"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:"  L-G-5
            END-IF.
      *
            MOVE "P-040-03"        TO CASE-ID.
            MOVE "＊＊＊"           TO L-GE.
            MOVE "あいうえお"       TO L-GE(1:2).
            IF L-GE = "あい＊／＊"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-040-04"        TO CASE-ID.
            MOVE "＊＊＊"          TO L-GE.
            MOVE "あい"            TO L-GE(1:3).
            IF L-GE = "あい　／＊"    DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-040-05"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO R-G-5.
            MOVE "＊＊"                 TO R-G-5(2:2).
            IF R-G-5 = "あ＊＊えお"   DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG:" R-G-5
            END-IF.
      *
            MOVE "P-040-06"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO R-GJ-5.
            MOVE "＊＊"                 TO R-GJ-5(2:2).
            IF R-GJ-5 = "か＊＊けこ"   DISPLAY CASE-ID "OK"
               ELSE                    DISPLAY CASE-ID "NG:" R-GJ-5
            END-IF.
      *
       P-050. 
      *  ケース5.日本語/日本語編集 日本語転記(1B->２B変換）
      *
            MOVE "P-050-01"        TO CASE-ID.
            MOVE "＊＊＊＊＊"   TO R-G-5.
            MOVE "ABCDE"    TO R-G-5(3:3).
            IF R-G-5 = "＊＊ＡＢＣ"    DISPLAY CASE-ID "OK"
               ELSE                 DISPLAY CASE-ID "NG:" R-G-5
            END-IF.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE "＊＊＊＊＊" TO R-G-5.
            MOVE "ｱｲ"     TO R-G-5(1:3).
            IF R-G-5 = "アイ　＊＊"   DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG:" R-G-5
            END-IF.
      *
            MOVE "P-050-03"        TO CASE-ID.
            MOVE "＊＊＊" TO R-GE.
            MOVE "ABCDE"  TO R-GE(1:3).
            IF R-GE = "ＡＢＣ／＊"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-GE
            END-IF.
      *
            MOVE "P-050-04"        TO CASE-ID.
            MOVE "＊＊＊" TO R-GE.
            MOVE "ｱｲ"     TO R-GE(1:3).
            IF R-GE = "アイ　／＊"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-GE
            END-IF.
      *
            *>ACCEPT OMIT-W K.   
      *
       P-060. 
      *  ケース6.日本語/日本語編集 日本語転記（表意定数、ALL 定数）
      *
            MOVE "P-060-01"        TO CASE-ID.
            MOVE ALL "＊"   TO R-G-5.
            MOVE SPACE      TO R-G-5(3:3).
            IF R-G-5 = "＊＊　　　"    DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" R-G-5
            END-IF.
      *
            MOVE "P-060-02"        TO CASE-ID.
            MOVE ALL "＊"  TO R-G-5.
            MOVE ALL "？"  TO R-G-5(1:3).
            IF R-G-5 = "？？？＊＊"   DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG:" R-G-5
            END-IF.
      *
            MOVE "P-060-03"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE.
            MOVE QUOTES   TO R-GE(1:3).
            IF R-GE = "”””／＊"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-GE
            END-IF.
      *
            MOVE "P-060-04"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE.
            MOVE ALL ZERO TO R-GE(1:3).
            IF R-GE = "０００／＊"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-GE
            END-IF.
      *
       P-070. 
      *  ケース7.日本語/日本語編集 日本語比較
      *
            MOVE "P-070-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            IF L-G(1:3) = "あいう　　　　　　　"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-070-02"        TO CASE-ID.
            MOVE "あいう" TO L-G.
            IF L-G(1:5) = "あいう"    
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-070-03"        TO CASE-ID.
            MOVE ALL "＊"          TO L-GE.
            IF L-GE(1:3) = "＊／＊　　　　"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-070-04"        TO CASE-ID.
            MOVE "あい"            TO L-GE(1:5).
            IF L-GE(1:5) = "あい"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
       P-080. 
      *  ケース8.日本語/日本語編集 日本語比較(表意定数、ALL 定数）
      *
            MOVE "P-080-01"        TO CASE-ID.
            MOVE SPACE TO L-G.
            IF L-G(1:3) = SPACE
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-080-02"        TO CASE-ID.
            MOVE ALL "あいう" TO L-G.
            IF L-G(1:5) = ALL "あいう"    
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-G
            END-IF.
      *
            MOVE "P-080-03"        TO CASE-ID.
            MOVE QUOTE             TO L-GE.
            IF L-GE(1:3) = "”／”"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            MOVE "P-080-04"        TO CASE-ID.
            MOVE ALL QUOTE         TO L-GE(1:5).
            IF L-GE(1:5) = ALL QUOTE
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:" L-GE
            END-IF.
      *
            *>ACCEPT OMIT-WK.
       P-090. 
      *  ケース09.日本語/日本語編集 日本語比較(修飾）
            MOVE "P-090-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO L-G.
            IF L-G OF W-L-GRP(1:3) = "あいう　　　　　　　"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:"
                                           L-G OF W-L-GRP(1:3)
            END-IF.
      *
            MOVE "P-090-02"        TO CASE-ID.
            MOVE "あいう" TO L-G.
            IF L-G OF W-L-GRP(1:5) = "あいう"    
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:"
                                           L-G OF W-L-GRP(1:5)
            END-IF.
      *
            MOVE "P-090-03"        TO CASE-ID.
            MOVE ALL "＊"          TO L-GE OF W-L-GRP(1:3).
            IF L-GE OF W-L-GRP(1:3) = "＊＊＊　　　　"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:"
                                           L-GE OF W-L-GRP(1:3)
            END-IF.
      *
            MOVE "P-090-04"        TO CASE-ID.
            MOVE "あい"            TO L-GE OF W-L-GRP(1:5).
            IF L-GE OF W-L-GRP(1:5) = "あい"
                                   DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG:"
                                           L-GE OF W-L-GRP(1:5) 
            END-IF.
      *
       P-100. 
      *  ケース10.日本語/日本語編集 日本語比較(添え字）
            
      *
            MOVE "P-100-01"        TO CASE-ID.
            MOVE "あいうえお"      TO W-OCC-G(1).
            IF W-OCC-G(1)(3:2) = "うえ"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                             W-OCC-G(1)(3:2)
            END-IF.
      *
            MOVE "P-100-02"        TO CASE-ID.
            MOVE 5 TO W-I.
            MOVE "あいうえお"      TO W-OCC-G(W-I).
            IF W-OCC-G(W-I)(2:4) = "いうえお"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                             W-OCC-G(W-I)(2:4)
            END-IF.
      *
            MOVE "P-100-03"        TO CASE-ID.
            MOVE "あいう"          TO W-OCC-GE(1)(3:3).
            IF W-OCC-GE(1)(3:3) = "あいう"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                             W-OCC-GE(1)(3:3)
            END-IF.
      *
            MOVE "P-100-04"        TO CASE-ID.
            MOVE 5 TO W-I.
            MOVE "あい"            TO W-OCC-GE(W-I)(1:3).
            IF W-OCC-GE(W-I)(1:3) = "あい　"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                             W-OCC-GE(W-I)(1:3)
            END-IF.
      *
      *  ケース11.日本語/日本語編集 日本語比較(修飾+添え字）
            
      *
            MOVE "P-110-01"        TO CASE-ID.
            MOVE "あいうえお"      TO W-OCC-G(1).
            IF W-OCC-G OF W-OCC-GRP(1)(3:2) = "うえ"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                     W-OCC-G OF W-OCC-GRP(1)(3:2)
            END-IF.
      *
            MOVE "P-110-02"        TO CASE-ID.
            MOVE 5 TO W-I.
            MOVE "あいうえお"      TO W-OCC-G(W-I).
            IF W-OCC-G OF W-OCC-GRP(W-I)(2:4) = "いうえお"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                     W-OCC-G OF W-OCC-GRP(W-I)(2:4)
            END-IF.
      *
            MOVE "P-110-03"        TO CASE-ID.
            MOVE "あいう"          TO W-OCC-GE(1)(3:2).
            IF W-OCC-GE OF W-OCC-GRP(1)(3:2) = "あい"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                     W-OCC-GE OF W-OCC-GRP(1)(3:2)
            END-IF.
      *
            MOVE "P-110-04"        TO CASE-ID.
            MOVE 5 TO W-I.
            MOVE "あい"            TO W-OCC-GE(W-I)(1:3).
            IF W-OCC-GE OF W-OCC-GRP(W-I)(1:3) = "あい　"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
                                     W-OCC-GE OF W-OCC-GRP(W-I)(1:3) 
            END-IF.
      *
      *
             DISPLAY "TEST END   (EX3-3-6A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

