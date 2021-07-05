      ******************************************************************
      *    テストケース：5-5B
      *    プログラム名：日本語化テスト （手続き部）MOVE命令
      *    処理概要　　：日本語転記が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１１～２０
      *   　　　　　　　ケース１３、１８はコンパイルエラーとしたい。
      *   　　　　　　　確認後、コメント化して、実行テストする。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-5B.
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
           05  L-G-10         PIC N(10).
           05  FILLER         REDEFINES L-G-10.
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
       MAIN-00.
            DISPLAY "TEST START (EX5-5B)".
       P-110. 
      *  ケース11.日本語定数→集団項目
      *
            MOVE "P-110-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "あいうえお"        TO R-GRP-10.
            IF  R-GRP-10 = "あいうえお"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-110-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "あいうえお"        TO R-GRP-4.
            IF  R-GRP-4 = "あい"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-110-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "あいうえお"        TO W-R-GRP.
            IF "あいうえお" = W-R-GRP         
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-120. 
      *  ケース12.日本語定数→英数字・英数字編集
      *
            MOVE "P-120-01"        TO CASE-ID.
            MOVE ALL "*"      TO R-AN.
            MOVE "あいうえお" TO R-AN.
            IF R-AN = "あいうえお"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-120-02"        TO CASE-ID.
            MOVE ALL "*"      TO R-ANE.
            MOVE "あいうえおかきく" TO R-ANE.
            IF R-ANE = "あ/い/う"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-120-03"        TO CASE-ID.
            MOVE ALL "*"      TO R-ANE.
            MOVE "あい"       TO R-ANE.
            IF R-ANE = "あ/い/  "    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-130. 
      *  ケース13.日本語定数→英字・数字・数字編集
      *           コンパイルエラーとなるか
           MOVE "P-130-01"        TO CASE-ID.
           MOVE "あいうえお" TO R-A.
           MOVE "あいうえお" TO R-ZONE.
           MOVE "あいうえお" TO R-ZONE-DEC.
           MOVE "あいうえお" TO R-PACK.
           MOVE "あいうえお" TO R-PACK-DEC.
           MOVE "あいうえお" TO R-BIN.
           MOVE "あいうえお" TO R-BIN-DEC.
           MOVE "あいうえお" TO R-NE.
           DISPLAY CASE-ID "NG".
           
      *
       P-140. 
      *  ケース14.日本語定数→日本語
      * 
      *
            MOVE "P-140-01"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいうえお"        TO R-G.
            IF R-G = "あいうえお"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-02"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいうえお"  TO R-G-3.
            IF R-G-3 = "あいう"      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-03"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいう"  TO R-G.
            IF R-G = "あいう　　"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-04"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいうえお"        TO R-GJ-5.
            IF R-GJ-5 = "あいうえお" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-05"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいうえお"        TO R-GJ-3.
            IF R-GJ-3 = "うえお"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-140-06"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE "あいう"        TO R-GJ-5.
            IF R-GJ-5 = "　　あいう" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-150. 
      *  ケース15.日本語定数→日本語編集
            MOVE "あいう"            TO R-GE.
      *
            MOVE "P-150-01"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE "あいう"            TO R-GE.
            IF "あ／い／う" = R-GE   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-150-02"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE "あいう"            TO R-GE-3.
            IF R-GE-3 = "あ／い"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-150-03"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE "あい"              TO R-GE.
            IF R-GE = "あ／い／　"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-150-04"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE-NB.
            MOVE "あいうえお"        TO R-GE-NB.
            IF R-GE-NB = "あいう／え　お／"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-150-05"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE-NBZ.
            MOVE "あいうえお"        TO R-GE-NBZ.
            IF R-GE-NBZ = "あいう／え０　お"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-160. 
      *  ケース16.ALL 日本語定数→集団項目
      *
            MOVE "P-160-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE ALL "あいうえお"    TO R-GRP-10.
            IF  R-GRP-10 = "あいうえお"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-160-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE ALL "あいう"        TO R-GRP-10.
            IF  R-GRP-10 = "あいうあい"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-160-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE ALL "あい"          TO R-GRP-10.
            IF R-GRP-10 = "あいあいあ" 
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-170. 
      *  ケース17.ALL 日本語定数→英数字・英数字編集
      *
            MOVE "P-170-01"        TO CASE-ID.
            MOVE ALL "*"          TO R-AN.
            MOVE ALL "あいうえお" TO R-AN.
            IF R-AN = "あいうえお"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-170-02"        TO CASE-ID.
            MOVE ALL "*"      TO R-ANE.
            MOVE ALL "あいう" TO R-ANE.
            IF R-ANE = "あ/い/う"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-170-03"        TO CASE-ID.
            MOVE ALL "*"      TO R-ANE.
            MOVE ALL "あい"   TO R-ANE.
            IF R-ANE = "あ/い/あ"    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-180. 
      *  ケース18.ALL 日本語定数→英字・数字・数字編集
      *           コンパイルエラーとなるか
           MOVE "P-180-01"        TO CASE-ID.
           MOVE ALL "あいうえお" TO R-A.
           MOVE ALL "あいうえお" TO R-ZONE.
           MOVE ALL "あいうえお" TO R-ZONE-DEC.
           MOVE ALL "あいうえお" TO R-PACK.
           MOVE ALL "あいうえお" TO R-PACK-DEC.
           MOVE ALL "あいうえお" TO R-BIN.
           MOVE ALL "あいうえお" TO R-BIN-DEC.
           MOVE ALL "あいうえお" TO R-NE.
           DISPLAY CASE-ID "NG".
      *
       P-190. 
      *  ケース19.ALL 日本語定数→日本語
      * 
            MOVE "P-190-01"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あいうえお"    TO R-G.
            IF R-G = "あいうえおあいうえお"    
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-02"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あ"  TO R-G-3.
            IF R-G-3 = "あああ"      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-03"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あいう"  TO R-G.
            IF R-G = "あいうあいうあいうあ"    
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-04"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あいうえお"   TO R-GJ-5.
            IF R-GJ-5 = "あいうえお" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-05"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あい"        TO R-GJ-3.
            IF R-GJ-3 = "あいあ"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-06"        TO CASE-ID.
            MOVE ALL "*" TO W-R-GRP.
            MOVE ALL "あいう"        TO R-GJ-5.
            IF R-GJ-5 = "あいうあい" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-200. 
      *  ケース20.ALL 日本語定数→日本語編集
      *
            MOVE "P-200-01"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE ALL "あいう"        TO R-GE.
            IF R-GE = "あ／い／う"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-02"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE ALL "あ"            TO R-GE.
            IF R-GE = "あ／あ／あ"     
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-03"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE.
            MOVE ALL "あい"         TO R-GE-3.
            IF R-GE-3 = "あ／い"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-04"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE-NB.
            MOVE ALL "あ"            TO R-GE-NB.
            IF R-GE-NB = "あああ／あ　あ／"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-05"        TO CASE-ID.
            MOVE ALL "＊"            TO R-GE-NBZ.
            MOVE ALL "あいう"        TO R-GE-NBZ.
            IF R-GE-NBZ = "あいう／あ０　い"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
      *
             DISPLAY "TEST END   (EX5-5B)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

