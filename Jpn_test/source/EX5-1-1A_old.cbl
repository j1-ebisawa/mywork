      ******************************************************************
      *    テストケース：5-1-1A
      *    プログラム名：日本語化テスト （手続き部）条件 比較条件
      *    処理概要　　：日本語比較が正しく比較表にしたがって
      *                  正しくエラーチェックされるかをチェックする。
      *  --------------------------------------------------------------
      *    このプログラムはエラーチェックを調べるので、実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-1A.
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
       01  W-L-GRP.
           05  L-A            PIC A(10).
           05  L-AN           PIC X(10).
           05  L-ANE          PIC XX/XX/XX.
           05  L-ZONE         PIC 9(5).
           05  L-ZONE-DEC     PIC S9(3)V9(2).
           05  L-PACK         PIC S9(5)       COMP-3.
           05  L-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  L-BIN          PIC S9(5)       COMP.
           05  L-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  L-NE           PIC ----9.
           05  L-G            PIC NNN.
           05  L-GE           PIC N/N/N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC NNN.
           05  R-GE           PIC N/N/N.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-1-1A)".
      *   集団
      *     ＋集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY W-L-GRP 
                                      R-ITEM BY QUOTES.
      *
       P-020. 
      *   英字
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A 
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY QUOTES.
      *
      *
       P-030. 
      *   英数字
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-AN
                                      R-ITEM BY QUOTES.
      *
      *
       P-040. 
      *   英数字編集
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ANE
                                      R-ITEM BY QUOTES.
      *
       P-050. 
      *   数字編集
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY QUOTES.
      *
      *
       P-060. 
      *   英字
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-A
                                      R-ITEM BY QUOTES.
      *
      *
       P-070. 
      *   ZONE整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE
                                      R-ITEM BY QUOTES.
      *
      *
       P-080. 
      *   ZONE非整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-ZONE-DEC
                                      R-ITEM BY QUOTES.
      *
       P-090. 
      *   PACK整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK
                                      R-ITEM BY QUOTES.
      *
      *
       P-100. 
      *   PACK非整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-PACK-DEC
                                      R-ITEM BY QUOTES.
      *
      *
       P-110. 
      *   BIN整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN
                                      R-ITEM BY QUOTES.
      *
      *
       P-120. 
      *   BIN非整数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-BIN-DEC
                                      R-ITEM BY QUOTES.
      *
      *
       P-130. 
      *   数字編集
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-NE
                                      R-ITEM BY QUOTES.
      *
      *
       P-140. 
      *   日本語
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-G
                                      R-ITEM BY QUOTES.
      *
       P-150. 
      *   日本語編集
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY L-GE
                                      R-ITEM BY QUOTES.
      *
      *
       P-160. 
      *   日本語定数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY "日本語"
                                      R-ITEM BY QUOTES.
      *
       P-170. 
      *   ALL 日本語定数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY ==ALL "日本語"==
                                      R-ITEM BY QUOTES.
      *
      *
       P-170. 
      *   表意定数
      *     +集団
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY W-R-GRP.
      *     +英字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-A.
      *     +英数字
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-AN.
      *     +数字(ZONE整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-ZONE.
      *     +数字(ZONE非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-ZONE-DEC.
      *     +数字(PACK整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-PACK.
      *     +数字(PACK非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-PACK-DEC.
      *     +数字(BIN整数)
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-BIN.
      *     +数字(BIN非整数）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-BIN-DEC.
      *     +数字(数字編集）
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-NE.
      *     +日本語
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-G.
      *     +日本語編集
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY R-GE.
      *     +日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY "日本語".
      *     +ALL 日本語定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY ==ALL "日本語"==.
      *     +表意定数
            COPY "IF_CHECK.cpy" REPLACING L-ITEM BY SPACES
                                      R-ITEM BY QUOTES.
      *
            DISPLAY "TEST END   (EX5-1-1A)".
           ACCEPT OMIT-WK.
           GOBACK
           .

