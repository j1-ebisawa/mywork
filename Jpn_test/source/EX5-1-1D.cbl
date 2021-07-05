      ******************************************************************
      *    テストケース：5-1-1D
      *    プログラム名：日本語化テスト （手続き部）条件 比較条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:３３～４４
      *   　　　　　　　ケース４１～４４はコンパイルエラーとしたい。
      *   　　　　　　　確認後、コメント化して、実行テストする。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-1D.
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
           05  L-G            PIC N(5).
           05  FILLER         REDEFINES L-G.
               10  L-G-3      PIC N(3).
               10  L-G-2      PIC N(2).
           05  L-GE           PIC N/N/N.
           05  FILLER         REDEFINES L-GE.
               10  L-GE-3     PIC N/N.
               10  L-GE-2     PIC /N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  W-R-GRP-10     REDEFINES R-A.
               10  W-R-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC N(5).
           05  FILLER         REDEFINES R-G.
               10  R-G-3      PIC N(3).
               10  R-G-2      PIC N(2).
           05  R-GE           PIC N/N/N.
           05  FILLER         REDEFINES R-GE.
               10  R-GE-3     PIC N/N.
               10  R-GE-2     PIC /N.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-330. 
            DISPLAY "TEST START (EX5-1-1D)".
      *  ケース33.（左辺）集団項目＋（右辺）日本語
            MOVE "あいうえお"        TO W-L-GRP.
            MOVE "あいうえお"        TO R-G.
      *
            MOVE "P-330-01"        TO CASE-ID.
            IF W-L-GRP-10 = R-G      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-330-02"        TO CASE-ID.
            IF W-L-GRP    > R-G      DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-330-03"        TO CASE-ID.
            IF W-L-GRP-4  < R-G      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-340. 
      *  ケース34.（左辺）集団＋（右辺）日本語編集
            MOVE "あ／い／う"        TO W-L-GRP.
            MOVE "あいう"            TO R-GE.
      *
            MOVE "P-340-01"        TO CASE-ID.
            IF W-L-GRP = R-GE        DISPLAY CASE-ID "OK"     *>20110912修正
               ELSE                  DISPLAY CASE-ID "NG"     *>20110921修正
            END-IF.
      *
            MOVE "P-340-02"        TO CASE-ID.
            IF W-L-GRP > R-GE        DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-340-03"        TO CASE-ID.
            IF W-L-GRP-4 < R-GE      DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-350. 
      *  ケース35.（左辺）集団＋（右辺）日本語定数
            MOVE "あいうえお"        TO W-L-GRP.
      
            MOVE "P-350-01"        TO CASE-ID.
            IF W-L-GRP-10 = "あいうえお"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-350-02"        TO CASE-ID.
            IF W-L-GRP-4 >  "あいうえお"         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-350-03"        TO CASE-ID.
            IF W-L-GRP  > "あいうえお"         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-360. 
      *  ケース36.（左辺）集団＋（右辺）ALL 日本語定数
            MOVE "あいう"          TO W-L-GRP.
      
            MOVE "P-360-01"        TO CASE-ID.
            IF W-L-GRP = ALL "あいう"
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-360-02"        TO CASE-ID.
            IF W-L-GRP >  ALL "あい"         
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-360-03"        TO CASE-ID.
            IF W-L-GRP > ALL "うえお"         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-370. 
      *  ケース37.（左辺）英数字・英数字編集・数字編集＋（右辺）日本語
            MOVE "あいうえお"        TO L-AN.
            MOVE "ABCDE"             TO L-ANE.
            MOVE 12345               TO L-NE.
            MOVE "あいうえお"        TO R-G.
      *
            MOVE "P-370-01"        TO CASE-ID.
            IF L-AN = R-G            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-370-02"        TO CASE-ID.
            IF L-ANE > R-G-3         DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-370-03"        TO CASE-ID.
            IF L-NE < R-G            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-380. 
      *  ケース38.（左辺）英数字・英数字編集・数字編集＋（右辺）日本語
            MOVE "あ／い／う"        TO L-AN.
            MOVE "ABCDE"             TO L-ANE.
            MOVE 12345               TO L-NE.
            MOVE "あいう"            TO R-GE.
      *
            MOVE "P-380-01"        TO CASE-ID.
            IF L-AN = R-GE           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-380-02"        TO CASE-ID.
            IF L-ANE > R-GE-3        DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-380-03"        TO CASE-ID.
            IF L-NE < R-GE           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-390. 
      *  ケース39.（左辺）英数字・英数字編集・数字編集＋（右辺）日本語定数
            MOVE "あ／い／う"        TO L-AN.
            MOVE "ABCDE"             TO L-ANE.
            MOVE 12345               TO L-NE.
      *
            MOVE "P-390-01"        TO CASE-ID.
            IF L-AN = "あ／い／う"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-390-02"        TO CASE-ID.
            IF L-ANE > "あ／い／う"  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-390-03"        TO CASE-ID.
            IF L-NE < "あ／い／う"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P40-0. 
      *  ケース40.（左辺）英数字・英数字編集・数字編集＋（右辺）ALL 日本語定数
            MOVE ALL "あいう"        TO L-AN.
            MOVE "ABCDE"             TO L-ANE.
            MOVE 12345               TO L-NE.
      *
            MOVE "P-400-01"        TO CASE-ID.
            IF L-AN = ALL "あいう"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-400-02"        TO CASE-ID.
            IF L-ANE > ALL "あいう"  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-400-03"        TO CASE-ID.
            IF L-NE < ALL "あいう"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-410. 
      *  ケース41.（左辺）英字・数字＋（右辺）日本語
      *  コンパイルエラー
           MOVE "ABCDE"             TO L-A.
           MOVE 12345               TO L-ZONE.
           MOVE "あいうえお"        TO R-G.
      *
           MOVE "P-410-01"        TO CASE-ID.
           IF L-A = R-G             DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
           MOVE "P-410-02"        TO CASE-ID.
           IF L-ZONE > R-G          DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
           MOVE "P-410-03"        TO CASE-ID.
           IF L-PACK < R-G          DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
           MOVE "P-410-04"        TO CASE-ID.
           IF 12345  < R-G          DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
      *     MOVE "P-410-05"        TO CASE-ID.
      *     IF 123.45 < R-G          DISPLAY CASE-ID "NG"  *>エラーになった
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
       P-420. 
      *  ケース42.（左辺）英字・数字＋（右辺）日本語編集
      *  コンパイルエラー
      *     MOVE "ABCDE"             TO L-A.
      *     MOVE 12345               TO L-ZONE.
      *     MOVE "あいう"            TO R-GE.
      *
      *     MOVE "P-420-01"        TO CASE-ID.
      *     IF L-A = R-GE            DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-420-02"        TO CASE-ID.
      *     IF L-ZONE > R-GE         DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-420-03"        TO CASE-ID.
      *     IF L-PACK < R-GE         DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-420-04"        TO CASE-ID.
      *     IF 12345  < R-GE         DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-420-04"        TO CASE-ID.
      *     IF 123.45 < R-GE         DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
       P-430. 
      *  ケース43.（左辺）英字・数字＋（右辺）日本語定数
      *  コンパイルエラー
      *     MOVE "ABCDE"             TO L-A.
      *     MOVE 12345               TO L-ZONE.
      *
      *     MOVE "P-430-01"        TO CASE-ID.
      *     IF L-A = "あいう"        DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-430-02"        TO CASE-ID.
      *     IF L-ZONE-DEC > "あいう" DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-430-03"        TO CASE-ID.
      *     IF L-PACK-DEC < "あいう" DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
       P-440. 
      *  ケース44.（左辺）英字・数字＋（右辺）ALL 日本語定数
      *  コンパイルエラー
      *     MOVE "ABCDE"             TO L-A.
      *     MOVE 12345               TO L-ZONE.
      *
      *     MOVE "P-440-01"        TO CASE-ID.
      *     IF L-A = ALL "あいう"    DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-440-02"        TO CASE-ID.
      *     IF L-ZONE-DEC > ALL "あいう" 
      *                              DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *     MOVE "P-440-03"        TO CASE-ID.
      *     IF L-PACK-DEC < ALL "あいう" 
      *                              DISPLAY CASE-ID "NG"
      *        ELSE                  DISPLAY CASE-ID "NG"
      *     END-IF.
      *
      *
            DISPLAY "TEST END   (EX5-1-1D)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

