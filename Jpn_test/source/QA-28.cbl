      ******************************************************************
      *    テストケース：QA-27
      *    プログラム名：日本語化テスト QA
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-28.
       AUTHOR.               TSH.
       DATE-WRITTEN.         2011-08-25.
       DATE-COMPILED.        2011-08-25.
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
       01  P        PIC S9(5).
       01  L        PIC S9(5).
       01  X-GRP.
           05  X-01     PIC X(10).
           05  X-02     PIC X(10).
           05  X-03     PIC X(10).
       01  N-GRP-X.
           05  N-DEF.
             10  N-01     PIC N(10).
             10  N-02     PIC N(10).
             10  N-03     PIC N(10).
           05  N-GRP      REDEFINES N-DEF  PIC N(30).
       01  PTR          USAGE POINTER.
      *****************************************************
       LINKAGE SECTION.
       01  BASE-CHAR    PIC X(10).
      *****************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "QA-28 test start".
            MOVE ALL "A" TO X-01.
            MOVE ALL "B" TO X-02.
            MOVE ALL "C" TO X-03.
            MOVE ALL "A" TO N-01.
            MOVE ALL "B" TO N-02.
            MOVE ALL "C" TO N-03.
            MOVE SPACE TO CASE-ID.
            DISPLAY "CASE_ID" UPON ENVIRONMENT-NAME.
            ACCEPT   CASE-ID  FROM ENVIRONMENT-VALUE.
            EVALUATE CASE-ID
              WHEN "01"    PERFORM TEST-01
              WHEN "02"    PERFORM TEST-02
              WHEN "03"    PERFORM TEST-03
              WHEN "04"    PERFORM TEST-04
              WHEN "05"    PERFORM TEST-05
              WHEN "06"    PERFORM TEST-06
              WHEN "07"    PERFORM TEST-07
              WHEN "08"    PERFORM TEST-08
              WHEN "09"    PERFORM TEST-09
              WHEN OTHER   GO TO TEST-01
            END-EVALUATE.
      *
            DISPLAY "QA-28 test end".
            *>ACCEPT OMIT-WK.
            GOBACK
            .
      *ケース1:
       TEST-01.
            MOVE "Case1:X-data P>0 and L>0"  TO CASE-ID.
            display CASE-ID.
            MOVE 11 TO P.
            MOVE 10 TO L.
            IF X-GRP(P:L) = ALL "B"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "X-GRP(11:10)" X-GRP(11:10)
            END-IF.
      *ケース2:
       TEST-02.
            MOVE "Case2:X-data P=0 and L>0"  TO CASE-ID.
            display CASE-ID.
            MOVE 0  TO P.
            MOVE 10 TO L.
            IF X-02(P:L) = "ABBBBBBBBB"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "X-02(0:10)" X-02(0:10)
               display "X-02(0:10)" X-02(P:L)
            END-IF.
      *ケース3:
       TEST-03.
            MOVE "Case3:X-data P<0 and L>10"  TO CASE-ID.
            display CASE-ID.
            MOVE -4  TO P.
            MOVE 10  TO L.
            IF X-02(P:L) = "AAAAABBBBB"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "X-02(-4:10)=" X-02(-4:10)
               display "X-02(-4:10)=" X-02(P:L)
            END-IF.
      *ケース4:
       TEST-04.
            MOVE "Case4:X-data P=1 and L=0"  TO CASE-ID.
            display CASE-ID.
            MOVE  1  TO P.
            MOVE  0  TO L.
            IF X-02(P:L) = " "
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "X-02(1:0)=" X-02(1:0)
               display "X-02(1:0)=" X-02(P:L)
            END-IF.
      *ケース5:
       TEST-05.
            MOVE "Case5:N-data P>0 and L>0"  TO CASE-ID.
            display CASE-ID.
            MOVE 11 TO P.
            MOVE 10 TO L.
            IF N-GRP(P:L) = ALL "Ｂ"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "N-GRP(11:10)" N-GRP(11:10)
            END-IF.
      *ケース6:
       TEST-06.
            MOVE "Case6:N-data P=0 and L>0"  TO CASE-ID.
            display CASE-ID.
            MOVE 0  TO P.
            MOVE 10 TO L.
            IF N-02(P:L) = "ＡＢＢＢＢＢＢＢＢＢ"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "N-02(0:10)" N-02(0:10)
               display "N-02(0:10)" N-02(P:L)
            END-IF.
      *ケース7:
       TEST-07.
            MOVE "Case7:N-data P<0 and L>10"  TO CASE-ID.
            display CASE-ID.
            MOVE -4  TO P.
            MOVE 10  TO L.
            IF N-02(P:L) = "ＡＡＡＡＡＢＢＢＢＢ"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "N-02(-4:10)=" N-02(-4:10)
               display "N-02(-4:10)=" N-02(P:L)
            END-IF.
      *ケース8:
       TEST-08.
            MOVE "Case8:N-data P=1 and L=0"  TO CASE-ID.
            display CASE-ID.
            MOVE  1  TO P.
            MOVE  0  TO L.
            IF N-02(P:L) = "　"
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
      *        display "N-02(1:0)=" N-02(1:0)
               display "N-02(1:0)=" N-02(P:L)
            END-IF.
      *ケース9:
       TEST-09.
            MOVE "Case9:X-data P=1 and L<0"  TO CASE-ID.
            display CASE-ID.
            MOVE   1   TO P.
            MOVE  -10  TO L.
            IF X-02(P:L) = " "
               DISPLAY CASE-ID "OK"
            ELSE
               DISPLAY CASE-ID "NG"
               display "X-02(1:-10)=" X-02(P:L)
               display "X-02(1:-10)= X-02(P:L) 画面が壊れる"
            END-IF.
      *
       TEST-OWARI.
            DISPLAY "QA-28 test end".
            *>ACCEPT OMIT-WK.
            GOBACK.

