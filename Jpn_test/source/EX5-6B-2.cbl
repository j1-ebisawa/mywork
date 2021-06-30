      ******************************************************************
      *    テストケース：EX5-6B-2
      *    プログラム名：日本語化テスト （手続き部）STRING命令
      *    処理概要　　：STRING命令の構文チェックでエラーとなるかを
      *    　　　　　　　チェックする。
      *  --------------------------------------------------------------
      *   テストケース:1  このプログラムは実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-6B-2.
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
       01  G-02-2   PIC N/N.

       01  X-01     PIC X(5).
       01  X-02     PIC X.       
       01  X-03     PIC X/X.
       01  N-01     PIC 9(5).
       01  W-INTO-X PIC X(20).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
      * ケース1.一意名３が編集タイプの場合
           MOVE "ABCDE" TO X-01.
           MOVE "A" TO X-02.
           MOVE "あ" TO G-02-1.
           MOVE ALL "＊"   TO G-02-2.
           MOVE "あいうえお" TO G-01.
           MOVE 1 TO W-PTR
           
      * コンパイルエラー
           STRING G-01 DELIMITED BY G-02-1
                       INTO G-02-2
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.
           DISPLAY G-02-2.
           
      * コンパイルエラー
           STRING X-01 DELIMITED BY X-02
                       INTO X-03
                       WITH POINTER W-PTR
                  ON OVERFLOW MOVE 1 TO OVER-SW
           END-STRING.        
           DISPLAY X-03.  
      ****
           
           *>ACCEPT OMIT-WK.
           GOBACK
           .
