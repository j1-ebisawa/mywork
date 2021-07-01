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
       PROGRAM-ID.           IF_TEST.
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
            DISPLAY "TEST START (IF_TEST)".
      *   集団
      *     ＋集団
            IF W-L-GRP = W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF W-L-GRP = R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF W-L-GRP = R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF W-L-GRP = R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF W-L-GRP = R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF W-L-GRP = R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF W-L-GRP =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF W-L-GRP = R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF W-L-GRP = R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF W-L-GRP =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF W-L-GRP =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF W-L-GRP =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF W-L-GRP =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF W-L-GRP = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF W-L-GRP = QUOTES
               CONTINUE
            END-IF.
      *
       P-020. 
      *   英字
      *     +集団
            IF L-A = W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-A =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-A =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-A = R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-A = R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-A = R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-A = R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-A =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-A =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-A =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-A =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-A =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-A =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-A = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-A =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-030. 
      *   英数字
      *     +集団
            IF L-AN =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-AN =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-AN =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-AN =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-AN =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-AN =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-AN =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-AN =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-AN =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-AN = R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-AN =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-AN =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-AN =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-AN = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-AN =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-040. 
      *   英数字編集
      *     +集団
            IF L-ANE =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-ANE =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-ANE =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-ANE =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-ANE =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-ANE =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-ANE =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-ANE =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-ANE =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-ANE =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-ANE =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-ANE =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-ANE =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-ANE = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-ANE =  QUOTES
               CONTINUE
            END-IF.
      *
       P-050. 
      *   数字編集
      *     +集団
            IF L-NE =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-NE =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-NE =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-NE =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-NE = R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-NE = R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-NE =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-NE =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-NE =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-NE =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-NE =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-NE =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-NE =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-NE = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-NE = QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-060. 
      *   英字
      *     +集団
            IF L-A = W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-A =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-A =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-A =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-A = R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-A =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-A = R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-A = R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-A =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-A =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-A =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-A =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-A = "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-A = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-A =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-070. 
      *   ZONE整数
      *     +集団
            IF L-ZONE =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-ZONE =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-ZONE =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-ZONE =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-ZONE =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-ZONE =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-ZONE =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-ZONE =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-ZONE = R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-ZONE =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-ZONE =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-ZONE =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-ZONE =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-ZONE = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-ZONE =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-080. 
      *   ZONE非整数
      *     +集団
            IF L-ZONE-DEC =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-ZONE-DEC =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-ZONE-DEC =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-ZONE-DEC =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-ZONE-DEC =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-ZONE-DEC =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-ZONE-DEC = R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-ZONE-DEC =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-ZONE-DEC =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-ZONE-DEC =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-ZONE-DEC =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-ZONE-DEC =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-ZONE-DEC =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-ZONE-DEC = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-ZONE-DEC =  QUOTES
               CONTINUE
            END-IF.
      *
       P-090. 
      *   PACK整数
      *     +集団
            IF L-PACK =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-PACK =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-PACK =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-PACK =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-PACK =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-PACK =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-PACK =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-PACK =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-PACK =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-PACK =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-PACK =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-PACK =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-PACK =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-PACK = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-PACK =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-100. 
      *   PACK非整数
      *     +集団
            IF L-PACK-DEC =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-PACK-DEC =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-PACK-DEC =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-PACK-DEC =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-PACK-DEC =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-PACK-DEC =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-PACK-DEC =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-PACK-DEC =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-PACK-DEC =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-PACK-DEC =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-PACK-DEC =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-PACK-DEC =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-PACK-DEC =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-PACK-DEC = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-PACK-DEC =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-110. 
      *   BIN整数
      *     +集団
            IF L-BIN =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-BIN =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-BIN =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-BIN =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-BIN =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-BIN =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-BIN =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-BIN =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-BIN =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-BIN =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-BIN =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-BIN =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-BIN =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-BIN = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-BIN =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-120. 
      *   BIN非整数
      *     +集団
            IF L-BIN-DEC =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-BIN-DEC =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-BIN-DEC =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-BIN-DEC =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-BIN-DEC =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-BIN-DEC =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-BIN-DEC =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-BIN-DEC =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-BIN-DEC =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-BIN-DEC =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-BIN-DEC =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-BIN-DEC =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-BIN-DEC =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-BIN-DEC = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-BIN-DEC =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-130. 
      *   数字編集
      *     +集団
            IF L-NE =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-NE =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-NE =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-NE =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-NE =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-NE =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-NE =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-NE =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-NE =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-NE =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-NE =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-NE =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-NE =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-NE = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-NE =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-140. 
      *   日本語
      *     +集団
            IF L-G =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-G =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-G =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-G =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-G =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-G =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-G =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-G =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-G =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-G =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-G =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-G =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-G =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-G = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-G =  QUOTES
               CONTINUE
            END-IF.
      *
       P-150. 
      *   日本語編集
      *     +集団
            IF L-GE =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF L-GE =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF L-GE =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF L-GE =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF L-GE =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF L-GE =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF L-GE =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF L-GE =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF L-GE =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF L-GE =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF L-GE =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF L-GE =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF L-GE =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF L-GE = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF L-GE =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-160. 
      *   日本語定数
      *     +集団
            IF "日本語" =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF "日本語" =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF "日本語" =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF "日本語" =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF "日本語" =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF "日本語" =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF "日本語" =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF "日本語" =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF "日本語" =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF "日本語" =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF "日本語" =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF "日本語" =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF "日本語" =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF "日本語" = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF "日本語" =  QUOTES
               CONTINUE
            END-IF.
      *
       P-170. 
      *   ALL 日本語定数
      *     +集団
            IF ALL "日本語" =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF ALL "日本語" =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF ALL "日本語" =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF ALL "日本語" =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF ALL "日本語" =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF ALL "日本語" =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF ALL "日本語" =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF ALL "日本語" =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF ALL "日本語" =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF ALL "日本語" =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF ALL "日本語" =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF ALL "日本語" =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF ALL "日本語" =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF ALL "日本語" = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF ALL "日本語" =  QUOTES
               CONTINUE
            END-IF.
      *
      *
       P-170. 
      *   表意定数
      *     +集団
            IF SPACES =  W-R-GRP
               CONTINUE
            END-IF.
      *     +英字
            IF SPACES =  R-A
               CONTINUE
            END-IF.
      *     +英数字
            IF SPACES =  R-AN
               CONTINUE
            END-IF.
      *     +数字(ZONE整数)
            IF SPACES =  R-ZONE
               CONTINUE
            END-IF.
      *     +数字(ZONE非整数）
            IF SPACES =  R-ZONE-DEC
               CONTINUE
            END-IF.
      *     +数字(PACK整数)
            IF SPACES =  R-PACK
               CONTINUE
            END-IF.
      *     +数字(PACK非整数）
            IF SPACES =  R-PACK-DEC
               CONTINUE
            END-IF.
      *     +数字(BIN整数)
            IF SPACES =  R-BIN
               CONTINUE
            END-IF.
      *     +数字(BIN非整数）
            IF SPACES =  R-BIN-DEC
               CONTINUE
            END-IF.
      *     +数字(数字編集）
            IF SPACES =  R-NE
               CONTINUE
            END-IF.
      *     +日本語
            IF SPACES =  R-G
               CONTINUE
            END-IF.
      *     +日本語編集
            IF SPACES =  R-GE
               CONTINUE
            END-IF.
      *     +日本語定数
            IF SPACES =  "日本語"
               CONTINUE
            END-IF.
      *     +ALL 日本語定数
            IF SPACES = ALL "日本語"
               CONTINUE
            END-IF.
      *     +表意定数
            IF SPACES =  QUOTES
               CONTINUE
            END-IF.
      *
            DISPLAY "TEST END   (IF_TEST)".
           *>ACCEPT OMIT-WK.
           GOBACK
           .

