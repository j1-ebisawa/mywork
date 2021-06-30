      ******************************************************************
      *    テストケース：EX5-6B-4
      *    プログラム名：日本語化テスト （手続き部）STRING命令
      *    処理概要　　：STRING命令の構文チェックでエラーとなるかを
      *    　　　　　　　チェックする。
      *  --------------------------------------------------------------
      *   テストケース:1  このプログラムは実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-6B-4.
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
       01  N-01     PIC 9(5).  
       01  N-02     PIC 9.       
       01  N-03     PIC 9/9.    
       01  N-04     PIC 9(8).
       01  N-05     PIC S9(4) COMP.
       01  N-06     PIC S9(4) COMP-3.
       01  W-INTO-X PIC X(20).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
      * ケース1.数字編集タイプと数字項目タイプの場合で、
           MOVE 12345 TO N-01.
           MOVE 5 TO N-02.
           MOVE 34 TO N-03.
           MOVE 1 TO W-PTR.
      * コンパイルエラー
           MOVE ZERO TO N-04
           STRING N-01 DELIMITED BY N-02
                       INTO N-04
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY N-04.

           STRING N-03 DELIMITED BY N-02
                       INTO N-04
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY N-04.

           STRING N-02 DELIMITED BY N-03
                       INTO N-04
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY N-04.
           
           STRING N-05 DELIMITED BY N-02
                       INTO N-04
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY N-04.
           
           STRING N-06 DELIMITED BY N-02
                       INTO N-04
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY N-04.
      ****
           
           *>ACCEPT OMIT-WK.
           GOBACK
           .
