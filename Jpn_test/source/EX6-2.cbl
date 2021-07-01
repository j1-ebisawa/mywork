      ******************************************************************
      *    テストケース：6-2
      *    プログラム名：日本語化テスト （関数）BYTE-LENGTH
      *    処理概要　　：LENGTH関数が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～７
      ******************************************************************
      * REPLACE ==BYTE-LENGTH== BY ==LENGTH-AN==.
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX6-2.
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
       01  CASE-ID            PIC X(10).
       01  W-LENG      PIC S9(5).

       01  X-01          PIC X(10).
       01  A-01          PIC A(5).
       01  ZONE-01       PIC 9(3).
       01  PACK-01       PIC S9(3)V9(3)  COMP-3.
       01  BIN-01        PIC S9(5)       COMP.
       01  G-01          PIC N(8).
       01  GE-01         PIC N/NNN/N.

       01  GRP-01.
           05  GRP-01-1  PIC X(5).                       *>5
           05  GRP-01-2  PIC A(5).                       *>5
           05  GRP-01-3  PIC 9(5).                       *>5
           05  GRP-01-4  PIC N(5).                       *>10
           05  GRP-01-5  PIC S9(5)V9(3)  COMP-3.         *>5

       01  GRP-02.
           05  GRP-02-1  PIC X(5).                       *>5
           05  FILLER    PIC A(5).                       *>5
           05  GRP-02-3  PIC 9(5)  OCCURS 4.             *>20
           05  GRP-02-4  PIC N(5).                       *>10
           05  GRP-02-5  PIC S9(5)V9(3)  COMP-3.         *>5

       01  GRP-03.
           05  GRP-03-1  PIC X(5).                       *>5
           05  GRP-03-2  PIC A(5).                       *>5
           05  GRP-03-3  PIC 9(5).                       *>5
           05  GRP-03-4  PIC N(5)  OCCURS 1 TO 10        *>10*n
                                   DEPENDING ON GRP-03-3.

        01  GRP-04.
           05  GRP-04-1  PIC X(1).                       *>1
           05  GRP-04-2  USAGE SIGNED-SHORT.             *>2
           05  GRP-04-3  PIC N.                          *>2
           05  GRP-04-4  USAGE SIGNED-LONG.              *>4
           05  GRP-04-5  PIC N.                          *>2
           05  GRP-04-6  PIC X(1).                       *>1
           05  GRP-04-7  USAGE DOUBLE.            *>8

       01  GRP-05.
           05  GRP-05-1  PIC X(1).                       *>1
                                                         *>1  FILLER
           05  GRP-05-2  USAGE SIGNED-SHORT SYNC.        *>2
           05  GRP-05-3  PIC N.                          *>2
                                                         *>2  FILLER
           05  GRP-05-4  USAGE SIGNED-LONG SYNC.         *>4
           05  GRP-05-5  PIC N.                          *>2
           05  GRP-05-6  PIC X(1).                       *>1
                                                         *>1  FILLER
           05  GRP-05-7  USAGE DOUBLE SYNC.       *>8

      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX6-2)".
      *ケース1.基本データ項目
            MOVE "P-010-01"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(X-01)     TO W-LENG.
            IF W-LENG = 10           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-02"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(A-01)     TO W-LENG.
            IF W-LENG = 5            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-03"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(ZONE-01)     TO W-LENG.
            IF W-LENG = 3            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-04"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(PACK-01)     TO W-LENG.
            IF W-LENG = 4            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-05"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(BIN-01)     TO W-LENG.
            IF W-LENG = 4            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-06"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(G-01)     TO W-LENG.
            IF W-LENG = 16           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-010-07"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(GE-01)     TO W-LENG.
            IF W-LENG = 14           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.

      *ケース2：定数
            MOVE "P-020-01"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH("ABC")     TO W-LENG.
            IF W-LENG = 3            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-020-02"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH("あいうえお")     TO W-LENG.
            IF W-LENG = 10           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-020-03"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH("あいうXYお")     TO W-LENG.
            IF W-LENG = 10           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-020-04"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(12345)     TO W-LENG.
            IF W-LENG = 5            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-020-05"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(X"01020304")     TO W-LENG.
            IF W-LENG = 4            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-020-06"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH("ABC" & "あいう")     TO W-LENG.
            IF W-LENG = 9            DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.

      *ケース3：集団項目
            MOVE "P-030-01"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(GRP-01)     TO W-LENG.
            IF W-LENG = 30           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *ケース4：FILLER
            MOVE "P-040-01"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(GRP-01)     TO W-LENG.
            IF W-LENG = 30           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *ケース5：可変繰り返し集団
            MOVE "P-050-01"        TO CASE-ID.
            MOVE 3 TO GRP-03-3. 
            MOVE FUNCTION BYTE-LENGTH(GRP-03) TO W-LENG.
            IF W-LENG = 45           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE 1 TO GRP-03-3. 
            MOVE FUNCTION BYTE-LENGTH(GRP-03) TO W-LENG.
            IF W-LENG = 25           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-050-03"        TO CASE-ID.
            MOVE 5 TO GRP-03-3. 
            MOVE FUNCTION BYTE-LENGTH(GRP-03) TO W-LENG.
            IF W-LENG = 65           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *ケース6：SYNC
            MOVE "P-060-01"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(GRP-04) TO W-LENG.
            IF W-LENG = 20           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            MOVE "P-060-02"        TO CASE-ID.
            MOVE FUNCTION BYTE-LENGTH(GRP-05) TO W-LENG.
            IF W-LENG = 24           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *ケース7：エラーチェック
            MOVE "P-070-01"        TO CASE-ID.
            MOVE 0 TO GRP-03-3. 
            MOVE FUNCTION BYTE-LENGTH(GRP-03) TO W-LENG.
            IF W-LENG = 15           DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:W-LENG=" W-LENG
            END-IF.
      *
            DISPLAY "TEST END   (EX6-2)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

