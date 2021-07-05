      ******************************************************************
      *    テストケース：5-5A
      *    プログラム名：日本語化テスト （手続き部）MOVE命令
      *    処理概要　　：日本語転記が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～１０
      *   　　　　　　　ケース３，８はコンパイルエラーとしたい。
      *   　　　　　　　確認後、コメント化して、実行テストする。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-5A.
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
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-5A)".
      *  ケース1.日本語→集団項目
            MOVE "あいうえおかきくけこ" TO L-G.
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE ALL "*"                TO W-R-GRP.
            MOVE L-G-5 TO R-GRP-10.
            
            IF R-GRP-10 = "あいうえお"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE ALL "*"                TO W-R-GRP.
            MOVE L-G  TO R-GRP-4.
            
            IF R-GRP-4 = "あい"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE ALL "*"                TO W-R-GRP.
            MOVE L-G-3 TO R-GRP-10.
            
            IF R-GRP-10 = "かきく    "    
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-020. 
      *  ケース2.日本語→英数字・英数字編集
            MOVE "あいうえおかきくけこ" TO L-G.
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE ALL "*" TO R-AN.
            MOVE L-G-5   TO R-AN.
            
            IF R-AN = "あいうえお"      DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"        TO CASE-ID.
            MOVE ALL "*" TO R-ANE.
            MOVE L-G-5   TO R-ANE.
            
            IF R-ANE = "あ/い/う"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-03"        TO CASE-ID.
            MOVE ALL "*" TO R-AN.
            MOVE L-G-2   TO R-AN.
            IF R-AN = "けこ      "   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-030. 
      *  ケース3.日本語→英字・数字・数字編集
      *           コンパイルエラーとなるか
            MOVE "あいうえおかきくけこ" TO L-G.
      *
           MOVE "P-030-01"        TO CASE-ID.
           MOVE L-G-5 TO R-A.
           DISPLAY CASE-ID "NG"
      *
           MOVE "P-030-02"        TO CASE-ID.
           MOVE L-G-5 TO R-ZONE.
           MOVE L-G-5 TO R-ZONE-DEC.
           DISPLAY CASE-ID "NG"
      *
           MOVE "P-030-03"        TO CASE-ID.
           MOVE L-G-5 TO R-PACK.
           MOVE L-G-5 TO R-PACK-DEC.
           DISPLAY CASE-ID "NG"
      *
           MOVE "P-030-04"        TO CASE-ID.
           MOVE L-G-5 TO R-BIN.
           MOVE L-G-5 TO R-BIN-DEC.
           DISPLAY CASE-ID "NG"
      *
           MOVE "P-030-05"        TO CASE-ID.
           MOVE L-G-5 TO R-NE.
           DISPLAY CASE-ID "NG".
      *
       P-040. 
      *  ケース4.日本語→日本語
            MOVE "あいうえおかきくけこ" TO L-G.
      *
            MOVE "P-040-01"        TO CASE-ID.
            MOVE ALL "＊"          TO R-G.
            MOVE L-G TO R-G.
            IF R-G = "あいうえおかきくけこ"    
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-02"        TO CASE-ID.
            MOVE ALL "＊"          TO R-G.
            MOVE L-G TO R-G-5.
            
            IF R-G-5 = "あいうえお"  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-03"        TO CASE-ID.
            MOVE ALL "＊"          TO R-G.
            MOVE L-G-3 TO R-G-5.
            IF R-G-5 = "かきく　　"  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-04"        TO CASE-ID.
            MOVE ALL "＊"          TO R-GJ.
            MOVE L-G TO R-GJ-5.
            
            IF R-GJ-5 = "かきくけこ"  DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-05"        TO CASE-ID.
            MOVE ALL "＊"          TO R-G.
            MOVE L-G-3 TO R-GJ-5.
            IF R-GJ-5 = "　　かきく"  DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-050. 
      *  ケース5.日本語→日本語編集
            MOVE "あいうえおかきくけこ" TO L-G.
      *
            MOVE "P-050-01"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE.
            MOVE L-G-3    TO R-GE.
            IF R-GE = "か／き／く"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE.
            MOVE L-G   TO R-GE.
            IF R-GE = "あ／い／う"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-050-03"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE.
            MOVE L-G-2    TO R-GE.
            IF R-GE = "け／こ／　"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-050-04"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE-NB.
            MOVE L-G-5    TO R-GE-NB.
            IF R-GE-NB = "あいう／え　お／" DISPLAY CASE-ID "OK"
               ELSE                         DISPLAY CASE-ID "NG:"
                                             
            END-IF.
      *
            MOVE "P-050-05"        TO CASE-ID.
            MOVE ALL "＊" TO R-GE-NBZ.
            MOVE L-G-5    TO R-GE-NBZ.
            IF R-GE-NBZ = "あいう／え０　お" DISPLAY CASE-ID "OK"        *>20111121
               ELSE                          
                   DISPLAY CASE-ID "NG:"R-GE-NBZ
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-060. 
      *  ケース6.日本語編集→集団項目
            MOVE "あいう"            TO L-GE.
      *
            MOVE "P-060-01"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE L-GE TO R-GRP-10.
            IF R-GRP-10 = "あ／い／う" DISPLAY CASE-ID "OK"
               ELSE                    DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-060-02"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE L-GE TO R-GRP-4.
            IF R-GRP-4 = "あ／"      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-060-03"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE L-GE TO W-R-GRP.
            IF W-R-GRP = "あ／い／う" DISPLAY CASE-ID "OK"
               ELSE                   DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-070. 
      *  ケース7.日本語編集→英数字・英数字編集
            MOVE "あいう"            TO L-GE.
      *
            MOVE "P-070-01"        TO CASE-ID.
            MOVE ALL "*"           TO R-AN.
            MOVE L-GE TO R-AN.
            IF R-AN = "あ／い／う" DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-070-02"        TO CASE-ID.
            MOVE ALL "*"           TO R-ANE-3.
            MOVE L-GE TO R-ANE-3.
            IF R-ANE-3 = "あ/／/"  DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-070-03"        TO CASE-ID.
            MOVE ALL "*"           TO R-AN.
            MOVE L-GE-3 TO R-AN.
            IF R-AN = "あ／い    " DISPLAY CASE-ID "OK"
               ELSE                DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-080. 
      *  ケース8.日本語編集→英字・数字・数字編集
      *           コンパイルエラーとなるか
           MOVE "あいう"            TO L-GE.
           MOVE "P-080-01"        TO CASE-ID.
           MOVE L-GE TO R-A.
           DISPLAY CASE-ID "NG".
      *
      *     MOVE "P-080-02"        TO CASE-ID.
      *     MOVE L-GE TO R-ZONE.            *>エラー確認した
      *     MOVE L-GE TO R-ZONE-DEC.        *>エラー確認した
      *
      *     MOVE "P-080-03"        TO CASE-ID.
      *     MOVE L-GE TO R-PACK.            *>エラー確認した
      *     MOVE L-GE TO R-PACK-DEC.        *>エラー確認した
      *
      *     MOVE "P-080-04"        TO CASE-ID.
      *     MOVE L-GE TO R-BIN.              *>エラー確認した
      *     MOVE L-GE TO R-BIN-DEC.          *>エラー確認した
      *
      *     MOVE "P-080-05"        TO CASE-ID.
      *     MOVE L-GE TO R-NE.               *>エラー確認した
       P-090. 
      *  ケース09.日本語編集→日本語
            MOVE "あいう"            TO L-GE.
      *
            MOVE "P-090-01"        TO CASE-ID.
            MOVE ALL "＊" TO R-G.
            MOVE L-GE TO R-G.
            IF R-G = "あ／い／う"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-090-02"        TO CASE-ID.
            MOVE ALL "＊" TO R-G-3.
            MOVE L-GE TO R-G-3.
            IF R-G-3 = "あ／い"      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-090-03"        TO CASE-ID.
            MOVE ALL "＊" TO R-G.
            MOVE L-GE-3 TO R-G.
            IF R-G = "あ／い　　"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-100. 
      *  ケース10.日本語編集→日本語編集
            MOVE "あいう"            TO L-GE.
            MOVE ALL "＊"            TO R-GE.
            
      *
            MOVE "P-100-01"        TO CASE-ID.
            MOVE L-GE TO R-GE.
            IF R-GE = "あ／／／い"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
            END-IF.
      *
            MOVE "P-100-02"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE L-GE TO R-GE-3.
            IF R-GE-3 = "あ／／"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
            END-IF.
      *
            MOVE "P-100-03"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE L-GE-2 TO R-GE.
            IF R-GE = "／／う／　"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" 
            END-IF.
      *
      *
             DISPLAY "TEST END   (EX5-5A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

