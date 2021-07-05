      ******************************************************************
      *    テストケース：5-1-2
      *    プログラム名：日本語化テスト （手続き部）条件 条件名条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～２１
      *   　　　　　　　ケース２０、１１はコンパイルエラーとしたい。
      *   　　　　　　　確認後、コメント化して、実行テストする。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-2A.
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
              88 C-20-1      VALUE 123.
              88 C-20-2      VALUE 123.45. 
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-1-2A)".
      *ケース1：日本語＋日本語定数1個
            MOVE "P-010-01"        TO CASE-ID.
            MOVE "あああああ" TO A-01.
            IF C-01                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース2：日本語＋日本語定数2個（THRUなし)
            MOVE "P-020-01"        TO CASE-ID.
            MOVE "ううううう" TO A-02.
            IF C-02                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース3：日本語＋日本語定数2個（THRUあり)
            MOVE "P-030-01"        TO CASE-ID.
            MOVE "６"         TO A-03.
            IF C-03                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-030-02"        TO CASE-ID.
            MOVE "Ａ"         TO A-03.
            IF C-03                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース4：日本語＋日本語定数n個（組み合わせ)
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "６"          TO A-04.
            IF C-04                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-040-02"        TO CASE-ID.
            MOVE "３"          TO A-04.
            IF C-04                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース5：日本語＋ALL 日本語定数
            MOVE "P-050-01"        TO CASE-ID.
            MOVE ALL "あいう"  TO A-05.
            IF C-05                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース6：日本語＋表意定数
            MOVE "P-060-01"        TO CASE-ID.
            MOVE SPACE         TO A-06.
            IF C-06-1                DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-060-02"        TO CASE-ID.
            MOVE QUOTE         TO A-06.
            IF C-06-2                DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-060-03"        TO CASE-ID.
            MOVE ZERO          TO A-06.
            IF C-06-3                DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-060-04"        TO CASE-ID.
            MOVE HIGH-VALUE    TO A-06.
            IF C-06-4                DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-060-05"        TO CASE-ID.
            MOVE LOW-VALUE     TO A-06.
            IF C-06-5                DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース7：日本語＋日本語定数（長さが小さい）
            MOVE "P-070-01"        TO CASE-ID.
            MOVE "あいう"     TO A-07.
            IF C-07                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
               
      *
            *>ACCEPT OMIT-WK.
      *ケース8：日本語＋日本語定数（長さが大きい）
            MOVE "P-080-01"        TO CASE-ID.
            MOVE "あいうえおか" TO A-08.
            IF C-08                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース9：日本語編集＋日本語定数1個
            MOVE "P-090-01"        TO CASE-ID.
            MOVE "あああ    " TO A-09.
            IF C-09                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース10：日本語編集＋日本語定数2個（THRUなし)
            MOVE "P-100-01"        TO CASE-ID.
            MOVE "いいい"     TO A-10.
            IF C-10                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース11：日本語編集＋日本語定数2個（THRUあり)
            MOVE "P-110-01"        TO CASE-ID.
            MOVE "いいい"     TO A-11.
            IF C-11                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-110-02"        TO CASE-ID.
            MOVE "かかか"     TO A-11.
            IF C-11                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース12：日本語編集＋日本語定数n個（組み合わせ)
            MOVE "P-120-01"        TO CASE-ID.
            MOVE "２２２"     TO A-12.
            IF C-12                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-120-02"        TO CASE-ID.
            MOVE "８８８"     TO A-12.
            IF C-12                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース13：日本語編集＋ALL 日本語定数
            MOVE "P-130-01"        TO CASE-ID.
            MOVE ALL "あ"     TO A-13.
            IF C-13                  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *ケース14：日本語編集＋表意定数
            MOVE "P-140-01"        TO CASE-ID.
            MOVE SPACE         TO A-14.
            IF C-14-1                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-140-02"        TO CASE-ID.
            MOVE QUOTE         TO A-14.
            IF C-14-2                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-140-03"        TO CASE-ID.
            MOVE ZERO          TO A-14.
            IF C-14-3                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-140-04"        TO CASE-ID.
            MOVE HIGH-VALUE    TO A-14.
            IF C-14-4                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
            MOVE "P-140-05"        TO CASE-ID.
            MOVE LOW-VALUE     TO A-14.
            IF C-14-5                DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース15：日本語編集＋日本語定数（長さが小さい）
            MOVE "P-150-05"        TO CASE-ID.
            MOVE "あああ"       TO A-15.
            IF C-15                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース16：日本語編集＋日本語定数（長さが大きい）
            MOVE "P-160-01"        TO CASE-ID.
            MOVE "あいう"       TO A-16.
            IF C-16                  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *ケース17：(参照箇所）PERFORM命令のUNTIL
            MOVE "P-170-01"        TO CASE-ID.
            MOVE HIGH-VALUE     TO A-17.
            PERFORM UNTIL C-17
                                    DISPLAY CASE-ID "NG"
            END-PERFORM.
            IF A-17 = HIGH-VALUE
                                    DISPLAY CASE-ID "OK"
            END-IF.
      *ケース18：(参照箇所）EVALUATE命令のWHEN
            MOVE "P-180-01"        TO CASE-ID.
            MOVE "３"            TO A-18.
            EVALUATE TRUE
              WHEN C-18-1           DISPLAY CASE-ID "NG"
              WHEN C-18-2           DISPLAY CASE-ID "NG"
              WHEN C-18-3           DISPLAY CASE-ID "OK"
              WHEN OTHER            DISPLAY CASE-ID "NG"
            END-EVALUATE.
      *ケース19：(参照箇所）SEARCH命令のWHEN
            MOVE "P-190-01"        TO CASE-ID.
            MOVE "あああああ"      TO A-19-1(1).
            MOVE "aaaaa"           TO A-19-2(1).
            MOVE "わわわわわ"      TO A-19-1(2).
            MOVE "wwwww"           TO A-19-2(2).
            MOVE "かかかかか"      TO A-19-1(3).
            MOVE "kkkkk"           TO A-19-2(3).
            MOVE "ぱぱぱぱぱ"      TO A-19-1(4).
            MOVE "ppppp"           TO A-19-2(4).
            MOVE LOW-VALUE         TO A-19-1(5).
            MOVE "xxxxx"           TO A-19-2(5).
            SET IDX-01 TO 1.
            SEARCH A-19 VARYING IDX-01
                   AT END     DISPLAY CASE-ID "NG"
                   WHEN C-19-1(IDX-01)
                              DISPLAY CASE-ID "OK"
            END-SEARCH.
      *ケース20：(エラー）日本語＋数字定数
      *      コンパイル時エラー、ならOK

      *ケース21：(エラー）日本語編集＋数字定数
      *      コンパイル時エラー、ならOK
            DISPLAY "TEST END   (EX5-1-2A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

