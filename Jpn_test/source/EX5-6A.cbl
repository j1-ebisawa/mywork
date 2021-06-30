      ******************************************************************
      *    テストケース：5-6A
      *    プログラム名：日本語化テスト （手続き部）STRING命令
      *    処理概要　　：STRING命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～２５
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-6A.
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
       01  W-PTR    PIC 999.
       01  W-INTO   PIC N(10).
       01  W-INTO-2 PIC N(20).
       01  G-01     PIC N(5).
       01  G-01-1   PIC N(5).
       01  G-01-2   PIC N(5).
       01  G-02-1   PIC N.
       01  G-02-2   PIC NN.

       01  X-01     PIC X(5).
       01  N-01     PIC 9(5).
       01  W-INTO-X PIC X(20).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-6A)".
      *ケース1.ケース1.一意名１、定数1、DELIMITED指定なし
            MOVE "P-010-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            STRING G-01 INTO W-INTO.
            IF W-INTO = "あいう　　＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

            MOVE "P-010-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            STRING "あいう" INTO W-INTO.
            IF W-INTO = "あいう＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース2：一意名１、定数1、DELIMITED BY SIZE
            MOVE "P-020-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            STRING G-01 DELIMITED BY SIZE
                        INTO W-INTO.
            IF W-INTO = "あいうえお＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

            MOVE "P-020-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            STRING "あいうえお" DELIMITED BY SIZE
                        INTO W-INTO.
            IF W-INTO = "あいうえお＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース3：一意名１、定数1、DELIMITED BY 定数２
            MOVE "P-030-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            STRING G-01 DELIMITED BY "お"
                        INTO W-INTO.
            IF W-INTO = "あいうえ＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

            MOVE "P-030-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            STRING "あいうえお" DELIMITED BY "お"
                        INTO W-INTO.
            IF W-INTO = "あいうえ＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース4：一意名１、定数1、DELIMITED BY 表意定数.ALL定数
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            STRING G-01 DELIMITED BY SPACES
                        INTO W-INTO.
            IF W-INTO = "あいう＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

            MOVE "P-040-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
      *     STRING "あい”””" DELIMITED BY ALL QUOTE
            STRING "あい”””" DELIMITED BY     QUOTE
                        INTO W-INTO.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース5：一意名１、定数1、DELIMITED BY 一意名２、.ALL 一意名２
            MOVE "P-050-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE SPACE TO G-02-1.
            STRING G-01 DELIMITED BY G-02-1
                        INTO W-INTO.
            IF W-INTO = "あいう＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-050-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE QUOTE TO G-02-1.
      *     STRING "あい”””" DELIMITED BY ALL G-02-1
            STRING "あい”””" DELIMITED BY     G-02-1
                        INTO W-INTO.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース6：一意名１、定数1、DELIMITED BY 2文字定数２
            MOVE "P-060-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            STRING G-01 DELIMITED BY "うえ"
                        INTO W-INTO.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

            MOVE "P-060-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            STRING "あいうえお" DELIMITED BY "うお"
                        INTO W-INTO.
            IF W-INTO = "あいうえお＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース7：一意名１、定数1、DELIMITED BY ALL 2文字定数
            MOVE "P-070-01"        TO CASE-ID.
            MOVE "あいいえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
      *     STRING G-01 DELIMITED BY ALL "いえ"
            STRING G-01 DELIMITED BY     "いえ"
                        INTO W-INTO.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-070-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
      *     STRING "あいうえお" DELIMITED BY ALL "いえ"
            STRING "あいうえお" DELIMITED BY     "いえ"
                        INTO W-INTO.
            IF W-INTO = "あいうえお＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース8：一意名１、定数1、DELIMITED BY 一意名２、ALL 一意名２
            MOVE "P-080-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "えお" TO G-02-2.
            STRING G-01 DELIMITED BY G-02-2
                        INTO W-INTO.
            IF W-INTO = "あいう＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-080-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "うえ" TO G-02-2.
      *     STRING "あいうえお" DELIMITED BY ALL G-02-2
            STRING "あいうえお" DELIMITED BY     G-02-2
                        INTO W-INTO.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース9：一意名１、定数1、DELIMITED指定なし、WITH POINTER
            MOVE "P-090-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            STRING G-01 INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "あいう　　＊＊＊＊＊"
               AND W-PTR = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-090-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
            STRING "あいう" INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊あいう＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース10：一意名１、定数1、DELIMITED BY SIZE、WITH POINTER
            MOVE "P-100-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 3 TO W-PTR.
            STRING G-01 DELIMITED BY SIZE
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊あいうえお＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-100-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
            STRING "あいうえお" DELIMITED BY SIZE
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊あいうえお＊"
               AND W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース11：一意名１、定数1、DELIMITED BY 定数２、WITH POINTER
            MOVE "P-110-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 3 TO W-PTR.
            STRING G-01 DELIMITED BY "お"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊あいうえ＊＊＊＊"
               AND W-PTR = 7
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-110-01"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 2 TO W-PTR.
            STRING "あいうえお" DELIMITED BY "お"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊あいうえ＊＊＊＊＊"
               AND W-PTR = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース12：一意名１、定数1、DELIMITED BY 表意定数.ALL定数、WITH POINTER
            MOVE "P-120-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 7 TO W-PTR.
            STRING G-01 DELIMITED BY SPACES
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊＊＊あいう＊"
               AND W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-120-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
      *     STRING "あい”””" DELIMITED BY ALL QUOTE
            STRING "あい”””" DELIMITED BY     QUOTE
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
               AND W-PTR = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            *>ACCEPT OMIT-WK.
      *ケース13：一意名１、定数1、DELIMITED BY 一意名２、.ALL 一意名２、WITH POINTER
            MOVE "P-130-01"        TO CASE-ID.
            MOVE "あいう" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE SPACE TO G-02-1.
            MOVE 7 TO W-PTR.
            STRING G-01 DELIMITED BY G-02-1
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊＊＊あいう＊"
               AND W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-130-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE QUOTE TO G-02-1.
            MOVE 1 TO W-PTR.
      *     STRING "あい”””" DELIMITED BY ALL G-02-1
            STRING "あい”””" DELIMITED BY     G-02-1
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "あい＊＊＊＊＊＊＊＊"
               AND W-PTR = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース14：一意名１、定数1、DELIMITED BY 2文字定数２、WITH POINTER
            MOVE "P-140-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 4 TO W-PTR.
            STRING G-01 DELIMITED BY "うえ"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊あい＊＊＊＊＊"
               AND W-PTR = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-140-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 6 TO W-PTR.
            STRING "あいうえお" DELIMITED BY "うお"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊＊あいうえお"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース15：一意名１、定数1、DELIMITED BY ALL 2文字定数、WITH POINTER
            MOVE "P-150-01"        TO CASE-ID.
            MOVE "あいいえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
      *     STRING G-01 DELIMITED BY ALL "いえ"
            STRING G-01 DELIMITED BY     "いえ"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊あい＊＊＊＊"
               AND W-PTR = 7
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-150-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 8 TO W-PTR.
      *     STRING "あいうえお" DELIMITED BY ALL "いえ"
            STRING "あいうえお" DELIMITED BY     "いえ"
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊＊＊＊あいう"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース16：一意名１、定数1、DELIMITED BY 一意名２、ALL　一意名２、WITH POINTER
            MOVE "P-160-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "えお" TO G-02-2.
            MOVE 5 TO W-PTR.
            STRING G-01 DELIMITED BY G-02-2
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊あいう＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-160-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "うえ" TO G-02-1.
            MOVE 8 TO W-PTR.
      *     STRING "あいうえお" DELIMITED BY ALL G-02-2
            STRING "あいうえお" DELIMITED BY     G-02-2
                        INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊＊＊＊あいう"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース17：一意名１、定数1、繰り返し、DELIMITED指定なし、WITH POINTER
            MOVE "P-170-01"        TO CASE-ID.
            MOVE "あいう" TO G-01-1.
            MOVE "かきく" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            STRING G-01-1 G-01-2  INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "あいう　　かきく　　"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-170-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
            STRING "あいう"  "かきく" INTO W-INTO
                        WITH POINTER W-PTR.
            IF W-INTO = "＊＊＊＊あいうかきく"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース18：一意名１、定数1、繰り返し、DELIMITED BY SIZE、WITH POINTER
            MOVE "P-180-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01-1.
            MOVE "かきくけこ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 3 TO W-PTR.
            STRING G-01-1 DELIMITED BY SIZE
                   G-01-2 DELIMITED BY SIZE
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊あいうえおかきくけこ＊＊＊＊＊＊＊＊"
               AND W-PTR = 13
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-180-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 5 TO W-PTR.
            STRING "あいうえお" DELIMITED BY SIZE
                   "かきくけこ" DELIMITED BY SIZE
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊あいうえおかきくけこ＊＊＊＊＊＊"
               AND W-PTR = 15
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース19：一意名１、定数1、繰り返し、DELIMITED BY 定数２、WITH POINTER
            MOVE "P-190-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01-1.
            MOVE "かきくけこ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 3 TO W-PTR.
            STRING G-01-1 DELIMITED BY "お"
                   G-01-2 DELIMITED BY "き"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊あいうえか＊＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-190-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 2 TO W-PTR.
            STRING "あいうえお" DELIMITED BY "お"
                   "かきくけこ" DELIMITED BY "き"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊あいうえか＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 7
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース20：一意名１、定数1、繰り返し、DELIMITED BY 表意定数.ALL定数、WITH POINTER
            MOVE "P-200-01"        TO CASE-ID.
            MOVE "あいう" TO G-01-1.
            MOVE "かきく" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 7 TO W-PTR.
            STRING G-01-1 DELIMITED BY SPACES
                   G-01-2 DELIMITED BY SPACES
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊＊＊あいうかきく＊＊＊＊＊＊＊＊"
               AND W-PTR = 13
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
            
            MOVE "P-200-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 1 TO W-PTR.
      *     STRING "あい”””" DELIMITED BY ALL QUOTE
      *            "かきく””" DELIMITED BY ALL QUOTE
            STRING "あい”””" DELIMITED BY     QUOTE
                   "かきく””" DELIMITED BY     QUOTE
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "あいかきく＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース21：一意名１、定数1、繰り返し、DELIMITED BY 一意名２、.ALL　一意名２、WITH POINTER
            MOVE "P-210-01"        TO CASE-ID.
            MOVE "あいう" TO G-01-1.
            MOVE "かきく" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE SPACE TO G-02-1.
            MOVE 7 TO W-PTR.
            STRING G-01-1 DELIMITED BY G-02-1
                   G-01-2 DELIMITED BY G-02-1
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊＊＊あいうかきく＊＊＊＊＊＊＊＊"
               AND W-PTR = 13
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-210-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE QUOTE TO G-02-1.
            MOVE 1 TO W-PTR.
      *     STRING "あい”””" DELIMITED BY ALL G-02-1
      *            "かきくけこ" DELIMITED BY ALL G-02-1
            STRING "あい”””" DELIMITED BY     G-02-1
                   "かきくけこ" DELIMITED BY     G-02-1
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "あいかきくけこ＊＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
      *ケース22：一意名１、定数1、繰り返し、DELIMITED BY 2文字定数２、WITH POINTER
            MOVE "P-220-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01-1.
            MOVE "かきくけこ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 4 TO W-PTR.
            STRING G-01-1 DELIMITED BY "うえ"
                   G-01-2 DELIMITED BY "けこ"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊あいかきく＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 9
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-220-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 6 TO W-PTR.
            STRING "あいうえお" DELIMITED BY "うお"
                   "かきくけこ" DELIMITED BY "けこ"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊＊あいうえおかきく＊＊＊＊＊＊＊"
               AND W-PTR = 14
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
      *ケース23：一意名１、定数1、繰り返し、DELIMITED BY ALL 2文字定数、WITH POINTER
            MOVE "P-230-01"        TO CASE-ID.
            MOVE "あいいえお" TO G-01-1.
            MOVE "かきくくけ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 5 TO W-PTR.
      *     STRING G-01-1 DELIMITED BY ALL "いえ"
      *            G-01-2 DELIMITED BY ALL "きく"
            STRING G-01-1 DELIMITED BY     "いえ"
                   G-01-2 DELIMITED BY     "きく"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊あいか＊＊＊＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-230-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE 8 TO W-PTR.
      *     STRING "あいいえお" DELIMITED BY ALL "いえ"
      *            "かきくくけ" DELIMITED BY ALL "きく"
            STRING "あいいえお" DELIMITED BY     "いえ"
                   "かきくくけ" DELIMITED BY     "きく"
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊＊＊＊あいか＊＊＊＊＊＊＊＊＊＊"
               AND W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
      *ケース24：一意名１、定数1、繰り返し、DELIMITED BY 一意名２、ALL　一意名２、WITH POINTER
            MOVE "P-240-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01-1.
            MOVE "かきくけこ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE "えお" TO G-02-2.
            MOVE 5 TO W-PTR.
            STRING G-01-1 DELIMITED BY G-02-2
                   G-01-2 DELIMITED BY G-02-2
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊あいうかきくけこ＊＊＊＊＊＊＊＊"
               AND W-PTR = 13
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-240-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE "うえ" TO G-02-2.
            MOVE 8 TO W-PTR.
      *     STRING "あいうえお" DELIMITED BY ALL G-02-2
      *            "かきくけこ" DELIMITED BY ALL G-02-2
            STRING "あいうえお" DELIMITED BY     G-02-2
                   "かきくけこ" DELIMITED BY     G-02-2
                        INTO W-INTO-2
                        WITH POINTER W-PTR.
            IF W-INTO-2 = "＊＊＊＊＊＊＊あいかきくけこ＊＊＊＊＊＊"
               AND W-PTR = 15
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
      *ケース25：ON OVERFLOW句
            MOVE "P-250-01"        TO CASE-ID.
            MOVE "あいうえお" TO G-01-1.
            MOVE "かきくけこ" TO G-01-2.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE "えお" TO G-02-2.
            MOVE 5 TO W-PTR.
            MOVE 0 TO OVER-SW.
            STRING G-01-1 DELIMITED BY G-02-2
                   G-01-2 DELIMITED BY G-02-2
                        INTO W-INTO-2
                        WITH POINTER W-PTR
                   ON OVERFLOW MOVE 1 TO OVER-SW
            END-STRING.
            IF W-INTO-2 = "＊＊＊＊あいうかきくけこ＊＊＊＊＊＊＊＊"
               AND OVER-SW = 0 AND W-PTR = 13
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
            
            MOVE "P-250-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO-2.
            MOVE "うえ" TO G-02-2.
            MOVE 15 TO W-PTR.
            MOVE 0 TO OVER-SW.
            STRING "あいうえお" DELIMITED BY G-02-2
                   "かきくけこ" DELIMITED BY G-02-2
                        INTO W-INTO-2
                        WITH POINTER W-PTR
                   ON OVERFLOW MOVE 1 TO OVER-SW
            END-STRING.
            IF W-INTO-2 = "＊＊＊＊＊＊＊＊＊＊＊＊＊＊あいかきくけ"
               AND OVER-SW = 1 AND W-PTR = 21
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-PTR " " W-INTO-2
            END-IF.
      ***
            DISPLAY "TEST END    (EX5-6A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

