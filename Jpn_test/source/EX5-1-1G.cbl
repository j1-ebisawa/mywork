      ******************************************************************
      *    テストケース：5-1-1G
      *    プログラム名：日本語化テスト （手続き部）条件 比較条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:３、１１
      *   　　コンパイルエラーのチェックのため、実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-1G.
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
       P-010. 
            DISPLAY "TEST START (EX5-1-1G)".
      *
       P-030. 
      *  ケース3.（左辺）日本語＋（右辺）英字・数字
      *           コンパイルエラーとなるか
            MOVE "P-030-01"        TO CASE-ID.
            IF L-G = R-A             DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-02"        TO CASE-ID.
            IF L-G > R-ZONE          DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-03"        TO CASE-ID.
            IF L-G > 123             DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
      *      MOVE "P-030-04"        TO CASE-ID.
      *      IF L-G > 123.45          DISPLAY CASE-ID "NG"  *>エラーとなった
      *         ELSE                  DISPLAY CASE-ID "NG"
      *      END-IF.
      *
       P-110. 
      *  ケース11.（左辺）日本語編集＋（右辺）英字・数字
      *           コンパイルエラーとなるか
            MOVE "P-110-01"        TO CASE-ID.
            IF L-GE = R-A            DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-110-02"        TO CASE-ID.
            IF L-GE > R-ZONE         DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-110-03"        TO CASE-ID.
            IF L-GE = 123            DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
      *      MOVE "P-110-04"        TO CASE-ID.
      *      IF L-GE > 123.45         DISPLAY CASE-ID "NG"  *>エラーとなった
      *         ELSE                  DISPLAY CASE-ID "NG"
      *      END-IF.
      *
       P-019. 
      *  ケース19.（左辺）日本語定数＋（右辺）英字・数字
      *           コンパイルエラーとなるか
            MOVE "P-190-01"        TO CASE-ID.
            IF "あいうえお" = R-A    DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-190-02"        TO CASE-ID.
            IF "あいうえお" > R-ZONE DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-270. 
      *  ケース27.（左辺）ALL 日本語定数＋（右辺）英字・数字
      *           コンパイルエラーとなるか
            MOVE "P-270-01"        TO CASE-ID.
            IF ALL "あいう" = R-A    DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-270-02"        TO CASE-ID.
            IF ALL "あいう" > R-ZONE DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
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
      *      MOVE "P-410-05"        TO CASE-ID.
      *      IF 123.45 < R-G          DISPLAY CASE-ID "NG"  *>エラーとなった
      *         ELSE                  DISPLAY CASE-ID "NG"
      *      END-IF.
      *
       P-420. 
      *  ケース42.（左辺）英字・数字＋（右辺）日本語編集
      *  コンパイルエラー
            MOVE "ABCDE"             TO L-A.
            MOVE 12345               TO L-ZONE.
            MOVE "あいう"            TO R-GE.
      *
            MOVE "P-420-01"        TO CASE-ID.
            IF L-A = R-GE            DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-420-02"        TO CASE-ID.
            IF L-ZONE > R-GE         DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-420-03"        TO CASE-ID.
            IF L-PACK < R-GE         DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-420-04"        TO CASE-ID.
            IF 12345  < R-GE         DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
      *      MOVE "P-420-04"        TO CASE-ID.
      *      IF 123.45 < R-GE         DISPLAY CASE-ID "NG"  *>エラーとなった
      *         ELSE                  DISPLAY CASE-ID "NG"
      *      END-IF.
      *
       P-430. 
      *  ケース43.（左辺）英字・数字＋（右辺）日本語定数
      *  コンパイルエラー
            MOVE "ABCDE"             TO L-A.
            MOVE 12345               TO L-ZONE.
      *
            MOVE "P-430-01"        TO CASE-ID.
            IF L-A = "あいう"        DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-430-02"        TO CASE-ID.
            IF L-ZONE-DEC > "あいう" DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-430-03"        TO CASE-ID.
            IF L-PACK-DEC < "あいう" DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-440. 
      *  ケース44.（左辺）英字・数字＋（右辺）ALL 日本語定数
      *  コンパイルエラー
            MOVE "ABCDE"             TO L-A.
            MOVE 12345               TO L-ZONE.
      *
            MOVE "P-440-01"        TO CASE-ID.
            IF L-A = ALL "あいう"    DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-440-02"        TO CASE-ID.
            IF L-ZONE-DEC > ALL "あいう" 
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-440-03"        TO CASE-ID.
            IF L-PACK-DEC < ALL "あいう" 
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX5-1-1G)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

