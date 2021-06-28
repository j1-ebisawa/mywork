      ******************************************************************
      *    テストケース：3-3-6D
      *    プログラム名：日本語化テスト （言語要素）部分参照
      *    処理概要　　：部分参照の実行時エラーを検査する。
      *  --------------------------------------------------------------
      *   このプログラムは実行時エラーの確認のため、正常に実行できない。
      *   テストケース:１８（実行時のエラー発生）
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX3-3-6D.
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
       01  W-I                PIC 999.
       01  P                  PIC S999.
       01  L                  PIC S999.
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
       P-150. 
            DISPLAY "TEST START (EX3-3-6D)".
      *  ケース18.コンパイルエラー (左端位置、長さ)
           set configuration "iscobol.array_check" to 1.
           set configuration "iscobol.substring.check" to 1.
      *
            MOVE "P-180-01"             TO CASE-ID.
      *
            MOVE 0 TO P.
            MOVE "＊" TO L-G(P:L).
      *
            MOVE 20 TO L.
            MOVE "＊" TO L-G(1:L).
      *
            MOVE -3 TO P.
            MOVE "＊" TO L-G(P:5).
      *
            MOVE -2 TO L.
            MOVE "＊" TO L-G(3:L).
      *
            MOVE 100  TO P.
            MOVE "＊" TO L-G(P:5).
      *
            MOVE 7    TO P.
            MOVE 7    TO L.
            MOVE "＊" TO L-G(P:L).
            
      *
             DISPLAY "TEST END   (EX3-3-6D)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

