      ******************************************************************
      *    テストケース：5-7A
      *    プログラム名：日本語化テスト （手続き部）UNSTRING命令
      *    処理概要　　：UNSTRING命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～１９
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-7A.
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
            DISPLAY "TEST START  (EX5-7A)".
      *ケース1.DELIMITED BY 定数1/一意名2
            MOVE "P-010-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "か" TO W-DEL-1.
            
            UNSTRING G-01 DELIMITED BY "か"
                     INTO W-INTO-1.
            IF W-INTO-1 = "あいうえお"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-010-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL-1
                     INTO W-INTO-1.
            IF W-INTO-1 = "あいうえお"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース2：DELIMITED BY [ALL] 定数1
            MOVE "P-020-01"        TO CASE-ID.
            MOVE "あいうえおかかかけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "か"
                     INTO W-INTO-1
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえお"
               AND W-PTR = 7
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-020-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY ALL "か"
                     INTO W-INTO-1
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえお"
               AND W-PTR = 9
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース3：DELIMITED BY 定数1/一意名２ OR ・・・
            MOVE "P-030-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "い" TO W-DEL-1.
            MOVE "き" TO W-DEL-2.
            
            UNSTRING G-01 DELIMITED BY "い" OR "き"
                     INTO W-INTO-1 W-INTO-2.
            IF W-INTO-1 = "あ"  AND
               W-INTO-2 = "うえおか"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-030-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL-1 OR W-DEL-2
                     INTO W-INTO-1 W-INTO-2.
            IF W-INTO-1 = "あ"  AND
               W-INTO-2 = "うえおか"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース4：DELIMITED BY ALL 定数1　OR  ALL 定数2
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "あいうううかきくくこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            
            UNSTRING G-01 DELIMITED BY "う" OR "く"
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = SPACES AND
               W-PTR = 5
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-040-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY ALL "う" OR ALL "く"
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = "かき" AND
               W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース5：DELIMITED BY 表意定数　OR  ALL 表意定数
            MOVE "P-050-01"        TO CASE-ID.
            MOVE "あい　　　かき””こ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            
            UNSTRING G-01 DELIMITED BY SPACE OR QUOTE
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = SPACES AND
               W-PTR = 5
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY ALL SPACES OR ALL QUOTE
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = "かき" AND
               W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース6：INTO 一意名4、DELIMITER IN 一意名5 COUNT IN 一意名６ ・・・
            MOVE "P-060-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "い" TO W-DEL-1.
            MOVE "き" TO W-DEL-2.
            
            UNSTRING G-01 DELIMITED BY "い" OR "き"
                     INTO W-INTO-1 DELIMITER IN W-DELIN-1 
                                   COUNT IN W-CNT-1.
            
            IF W-INTO-1 = "あ" AND W-DELIN-1 = "い" AND W-CNT-1 = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-060-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL-1 OR W-DEL-2
                     INTO W-INTO-1 DELIMITER IN W-DELIN-1 
                                   COUNT IN W-CNT-1
                          W-INTO-2 DELIMITER IN W-DELIN-2 
                                   COUNT IN W-CNT-2.
            IF  W-INTO-1 = "あ"       AND 
                W-DELIN-1 = "い" AND W-CNT-1 = 1
            AND W-INTO-2 = "うえおか" AND 
                W-DELIN-2 = "き" AND W-CNT-2 = 4
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース7：WITH POINTER句
            MOVE "P-070-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "こ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえおかきくけ" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-070-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "こ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = "おかきくけ" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-070-03"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 11 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "こ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = ALL "＊" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-070-04"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 0 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "こ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = ALL "＊" AND
               W-PTR = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース8：TALLYING句
            MOVE "P-080-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 
                 DELIMITED BY "い" OR "え" OR "か" OR "く" OR "こ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY.
            IF W-INTO-1 = "あ" AND W-INTO-2 = "う" AND
               W-INTO-3 = "お" AND
               W-INTO-4 = "き" AND W-INTO-5 = "け" AND
               W-TALLY = 5
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-080-02"        TO CASE-ID.
            MOVE ALL "＊" TO W-INTO.
            MOVE 3 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "い" OR "か" OR "こ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY.
            IF W-INTO-1 = "あ" AND W-INTO-2 = "うえお" AND 
               W-INTO-3 = "きくけ" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

      *ケース9：OVERFLOW
            MOVE "P-090-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 
                 DELIMITED BY "い" OR "え" OR "か" OR "く" OR "こ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "あ" AND W-INTO-2 = "う" AND 
               W-INTO-3 = "お" AND
               W-INTO-4 = "き" AND W-INTO-5 = "け" AND
               W-TALLY = 5 AND OVER-SW = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-090-02"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 
                 DELIMITED BY "い" OR "え" OR "か" OR "く" OR "こ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "あ" AND W-INTO-2 = "う" AND 
               W-INTO-3 = "お" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 3 AND OVER-SW = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース10：総合テスト１
            MOVE "P-100-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            MOVE 1 TO W-PTR.
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 
                 DELIMITED BY "い" OR "え" OR "か" OR "く" OR "こ"
                 INTO W-INTO-1 DELIMITER IN W-DELIN-1 COUNT IN W-CNT-1
                      W-INTO-2 DELIMITER IN W-DELIN-2 COUNT IN W-CNT-2
                      W-INTO-3
                      W-INTO-4
                      W-INTO-5
                 WITH POINTER W-PTR
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "あ" AND W-DELIN-1 = "い" AND W-CNT-1 = 1 AND
               W-INTO-2 = "う" AND W-DELIN-2 = "え" AND W-CNT-1 = 1 AND
               W-INTO-3 = "お" AND
               W-INTO-4 = "き" AND
               W-INTO-5 = "け" AND
               W-TALLY = 5 AND W-PTR = 11 AND OVER-SW = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-100-02"        TO CASE-ID.
            MOVE "あいうえお　　くけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            MOVE 2 TO W-PTR.
            MOVE 10 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "え" OR ALL SPACE OR "く"
                 INTO W-INTO-1 DELIMITER IN W-DELIN-1 COUNT IN W-CNT-1
                      W-INTO-2 DELIMITER IN W-DELIN-2 COUNT IN W-CNT-2
                      W-INTO-3
                 WITH POINTER W-PTR
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF  W-INTO-1 = "いう" AND  W-DELIN-1 = "え" AND W-CNT-1 = 2 
            AND W-INTO-2 = "お"   AND  W-DELIN-2 = "　" AND W-CNT-2 = 1 
            AND W-INTO-3 = "　"      AND
                W-INTO-4 = ALL "＊"  AND
                W-INTO-5 = ALL "＊"  AND
                W-TALLY = 13 AND W-PTR = 9 AND OVER-SW = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *ケース11：DELIMITED BY 定数1/一意名2（2バイトDELIMITER)
            MOVE "P-110-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "かき" TO W-DEL2-1.
            
            UNSTRING G-01 DELIMITED BY "かき"
                     INTO W-INTO-1.
            IF W-INTO-1 = "あいうえお"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-110-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL2-1
                     INTO W-INTO-1.
            IF W-INTO-1 = "あいうえお"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース12：DELIMITED BY [ALL] 定数1（2バイトDELIMITER)
            MOVE "P-120-01"        TO CASE-ID.
            MOVE "あいうえおかかかけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "かか"
                     INTO W-INTO-1
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえお"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-120-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY ALL "かか"
                     INTO W-INTO-1
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえお"
               AND W-PTR = 8
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース13：DELIMITED BY 定数1/一意名２　OR ・・・（2バイトDELIMITER)
            MOVE "P-130-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "いう" TO W-DEL2-1.
            MOVE "きく" TO W-DEL2-2.
            
            UNSTRING G-01 DELIMITED BY "いう" OR "きく"
                     INTO W-INTO-1 W-INTO-2.
            IF W-INTO-1 = "あ"     AND
               W-INTO-2 = "えおか"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-130-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL2-1 OR W-DEL2-2
                     INTO W-INTO-1 W-INTO-2.
            IF W-INTO-1 = "あ"     AND
               W-INTO-2 = "えおか"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース14：DELIMITED BY ALL 定数1　OR  ALL 定数2（2バイトDELIMITER)
            MOVE "P-140-01"        TO CASE-ID.
            MOVE "あいうううかきくくこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            
            UNSTRING G-01 DELIMITED BY "うう" OR "くく"
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = "うかき" AND
               W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-140-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY ALL "うう" OR ALL "くく"
                     INTO W-INTO-1 W-INTO-2
                     WITH POINTER W-PTR.
            IF W-INTO-1 = "あい" AND
               W-INTO-2 = "うかき" AND
               W-PTR = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース15：INTO 一意名4、DELIMITER IN 一意名5 COUNT IN 一意名６ ・・・（2バイトDELIMITER)
            MOVE "P-150-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            MOVE "いう" TO W-DEL2-1.
            MOVE "かき" TO W-DEL2-2.
            
            UNSTRING G-01 
                     DELIMITED BY "いう" OR "かき"
                     INTO W-INTO-1 DELIMITER IN W-DELIN2-1 
                                   COUNT IN W-CNT-1.
            
            IF W-INTO-1 = "あ" AND W-DELIN2-1 = "いう" AND W-CNT-1 = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-150-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            UNSTRING G-01 DELIMITED BY W-DEL2-1 OR W-DEL2-2
                     INTO W-INTO-1 DELIMITER IN W-DELIN2-1 
                                   COUNT IN W-CNT-1
                          W-INTO-2 DELIMITER IN W-DELIN2-2 
                                   COUNT IN W-CNT-2.
            IF  W-INTO-1 = "あ"   AND W-DELIN2-1 = "いう" AND
                W-CNT-1 = 1
            AND W-INTO-2 = "えお" AND W-DELIN2-2 = "かき" AND 
                W-CNT-2 = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース16：WITH POINTER句（2バイトDELIMITER)
            MOVE "P-160-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊"   TO W-INTO.
            
            MOVE 1 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "こさ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = "あいうえおかきくけこ" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-160-02"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 5 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "けこ"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = "おかきく" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-160-03"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 11 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "えお"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = ALL "＊" AND
               W-PTR = 11
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-160-04"        TO CASE-ID.
            MOVE ALL "＊"   TO W-INTO.
            MOVE 0 TO W-PTR.
            UNSTRING G-01 DELIMITED BY "あい"
                   INTO W-INTO-1
                   WITH POINTER W-PTR.
            IF W-INTO-1 = ALL "＊" AND
               W-PTR = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.

      *ケース17：TALLYING句（2バイトDELIMITER)
            MOVE "P-170-01"        TO CASE-ID.
            MOVE "あいいいいいきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "いい" OR "けこ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY.
            IF W-INTO-1 = "あ" AND W-INTO-2 = "　" AND 
               W-INTO-3 = "いきく" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-170-02"        TO CASE-ID.
            MOVE ALL "＊" TO W-INTO.
            MOVE 3 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY ALL "いい"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY.
            IF W-INTO-1 = "あ" AND W-INTO-2 = "いきくけこ" AND 
               W-INTO-3 = ALL "＊" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 5
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース18：OVERFLOW（2バイトDELIMITER)
            MOVE "P-180-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "いう" OR "おか" OR "けこ"
                 INTO W-INTO-1 W-INTO-2 W-INTO-3 W-INTO-4 W-INTO-5
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            IF W-INTO-1 = "あ" AND W-INTO-2 = "え" AND 
               W-INTO-3 = "きく" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 3 AND OVER-SW = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-180-02"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "いう" OR "おか" OR "けこ"
                 INTO W-INTO-1 W-INTO-2 
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "あ" AND W-INTO-2 = "え" AND 
               W-INTO-3 = ALL "＊" AND
               W-INTO-4 = ALL "＊" AND W-INTO-5 = ALL "＊" AND
               W-TALLY = 2 AND OVER-SW = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *ケース19：総合テスト２（2バイトDELIMITER)
            MOVE "P-190-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            MOVE 1 TO W-PTR.
            MOVE 0 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "うえお" OR "けこ"
                 INTO W-INTO-1 DELIMITER IN W-DELIN2-1 
                               COUNT IN W-CNT-1
                      W-INTO-2 DELIMITER IN W-DELIN2-2 
                               COUNT IN W-CNT-2
                      W-INTO-3
                      W-INTO-4
                      W-INTO-5
                 WITH POINTER W-PTR
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "あい"   AND W-DELIN2-1 = "うえ" AND 
               W-CNT-1 = 2 AND
               W-INTO-2 = "かきく" AND W-DELIN2-2 = "けこ" AND 
               W-CNT-2 = 3 AND
               W-INTO-3 = ALL "＊"  AND
               W-INTO-4 = ALL "＊"  AND
               W-INTO-5 = ALL "＊"  AND
               W-TALLY = 2 AND W-PTR = 11 AND OVER-SW = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      *
            MOVE "P-190-02"        TO CASE-ID.
            MOVE "あいうえお　　くけこ" TO G-01.
            MOVE ALL "＊" TO W-INTO.
            MOVE 2 TO W-PTR.
            MOVE 10 TO W-TALLY.
            UNSTRING G-01 DELIMITED BY "うえ" OR ALL SPACE
                 INTO W-INTO-1 DELIMITER IN W-DELIN-1 
                               COUNT IN W-CNT-1
                      W-INTO-2 DELIMITER IN W-DELIN-2 
                               COUNT IN W-CNT-2
                 WITH POINTER W-PTR
                 TALLYING IN W-TALLY
                 ON OVERFLOW MOVE 1 TO OVER-SW
                 NOT ON OVERFLOW MOVE 0 TO OVER-SW
            END-UNSTRING.
            
            IF W-INTO-1 = "い"  AND 
               W-DELIN-1 = "う" AND W-CNT-1 = 1 AND
               W-INTO-2 = "お"   AND 
               W-DELIN-2 = "　"   AND W-CNT-2 = 1 AND
               W-INTO-3 = ALL "＊" AND
               W-INTO-4 = ALL "＊" AND
               W-INTO-5 = ALL "＊" AND
               W-TALLY = 12 AND W-PTR = 8 AND OVER-SW = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:"
            END-IF.
      ***
            DISPLAY "TEST END    (EX5-7A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

