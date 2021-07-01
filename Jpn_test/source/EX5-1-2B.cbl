      ******************************************************************
      *    テストケース：5-1-2
      *    プログラム名：日本語化テスト （手続き部）条件 条件名条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:２０、２１
      *   　　　このプログラムは、エラーチェックのため実行できない。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-2B.
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
       01  W-G.
      *ケース1：日本語＋日本語定数1個
           05  A-01           PIC N(5).
               88  C-01       VALUE "あああああ".
      *ケース2：日本語＋日本語定数2個（THRUなし)
           05  A-02           PIC N(5).
               88  C-02       VALUE "あああああ" "いいいいい".
      *ケース3：日本語＋日本語定数2個（THRUあり)
           05  A-03           PIC N(1).
               88  C-03       VALUE "０" THRU "９".
      *ケース4：日本語＋日本語定数n個（組み合わせ)
           05  A-04           PIC N(1).
               88  C-04       VALUE "１" "３" "５" "７" THRU "９".
      *ケース5：日本語＋ALL 日本語定数
           05  A-05           PIC N(5).
               88  C-05       VALUE ALL "あいう".
      *ケース6：日本語＋表意定数
           05  A-06           PIC N(5).
               88  C-06-1     VALUE SPACE.
               88  C-06-2     VALUE QUOTE.
               88  C-06-3     VALUE ZEROES.
               88  C-06-4     VALUE HIGH-VALUE.
               88  C-06-5     VALUE LOW-VALUE.
      *ケース7：日本語＋日本語定数（長さが小さい）
           05  A-07           PIC N(5).
               88  C-07       VALUE "あいう".
      *ケース8：日本語＋日本語定数（長さが大きい）
           05  A-08           PIC N(5).
               88  C-08       VALUE "あいうえおか".
       01  W-GE.
      *ケース9：日本語編集＋日本語定数1個
           05  A-09           PIC N/N/N.
               88  C-09       VALUE "あ／あ／あ".
      *ケース10：日本語編集＋日本語定数2個（THRUなし)
           05  A-10           PIC N/N/N.
               88  C-10       VALUE "あ／あ／あ"  "い／い／い".
      *ケース11：日本語編集＋日本語定数2個（THRUあり)
           05  A-11           PIC N/N/N.
               88  C-11       VALUE "あ／あ／あ" THRU "う／う／う".
      *ケース12：日本語編集＋日本語定数n個（組み合わせ)
           05  A-12           PIC N/N/N.
               88  C-12       VALUE  "１／１／１"      "３／３／３"
                                     "７／７／７" THRU "９／９／９".
      *ケース13：日本語編集＋ALL 日本語定数
           05  A-13           PIC N/N/N.
               88  C-13       VALUE ALL "あ／".
      *ケース14：日本語編集＋表意定数
           05  A-14           PIC N/N/N.
               88  C-14-1     VALUE SPACE.
               88  C-14-2     VALUE QUOTE.
               88  C-14-3     VALUE ZEROES.
               88  C-14-4     VALUE HIGH-VALUE.
               88  C-14-5     VALUE LOW-VALUE.
      *ケース15：日本語編集＋日本語定数（長さが小さい）
           05  A-15           PIC N/N/N.
               88  C-15       VALUE "あ／".
      *ケース16：日本語編集＋日本語定数（長さが大きい）
           05  A-16           PIC N/N/N.
               88  C-16       VALUE "あ／い／う／え／お／か".
      *ケース17：(参照箇所）PERFORM命令のUNTIL
        01  A-17           PIC N(5).
               88  C-17       VALUE HIGH-VALUE.
      *ケース18：(参照箇所）EVALUATE命令のWHEN
        01  A-18           PIC N(5).
               88  C-18-1     VALUE "１".
               88  C-18-2     VALUE "２".
               88  C-18-3     VALUE "３".
      *ケース19：(参照箇所）SEARCH命令のWHEN
        01  A-19-GRP.
               10  A-19       OCCURS 50 INDEXED BY IDX-01.
                 15  A-19-1   PIC N(5).
                     88  C-19-1     VALUE LOW-VALUE.
                 15  A-19-2   PIC X(5).
      *ケース20：(エラー）日本語＋数字定数
        01  A-20              PIC NNN.
               88 C-20-1      VALUE 123.
               88 C-20-2      VALUE 123.45. 
      *ケース21：(エラー）日本語編集＋数字定数
        01  A-21              PIC N/N/N.
               88 C-21-1      VALUE 123.
               88 C-21-2      VALUE 123.45. 
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-020. 
            DISPLAY "TEST START (EX5-1-2B)".
      *ケース20：(エラー）日本語＋数字定数
      *      コンパイル時エラー、ならOK
            MOVE "P-200-01"        TO CASE-ID.
            MOVE "６"         TO A-20.
            IF C-20-1                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-200-02"        TO CASE-ID.
            MOVE "Ａ"         TO A-03.
            IF C-20-2                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.

      *ケース21：(エラー）日本語編集＋数字定数
      *      コンパイル時エラー、ならOK
            MOVE "P-210-01"        TO CASE-ID.
            MOVE "６"         TO A-20.
            IF C-21-1                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-210-02"        TO CASE-ID.
            MOVE "Ａ"         TO A-03.
            IF C-21-2                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END    (EX5-1-2B)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

