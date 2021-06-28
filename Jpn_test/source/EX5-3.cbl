      ******************************************************************
      *    テストケース：5-3
      *    プログラム名：日本語化テスト （手続き部）INITIALIZE命令
      *    処理概要    ：INITIALIZE命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～１２
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-3.
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
       01  A-01               PIC N(5) VALUE "あいうえお".
       01  A-02               PIC N(5) VALUE "あいうえお".
       01  A-03.
           05  A-03-1         PIC N(5).
           05  A-03-2         PIC N(5) VALUE "かかかかか".
       01  A-04               PIC N(5).
       01  A-05               PIC N/N/N.
       01  A-06               PIC N/N/N VALUE "１／２／３".
       01  A-07.
           05  A-07-1         PIC N/N/N.
           05  A-07-2         PIC NNN  VALUE "あいう".
       01  A-08               PIC N/N/N.
       01  A-09.
           05  FILLER         PIC N(5).
           05  FILLER         PIC N/N/N.
       01  A-10.
           05  A-10-1         PIC X(5)          VALUE "ABCDE".
           05  A-10-2         PIC NNN           VALUE "あいう".
           05  A-10-3         PIC 999           VALUE 123.
           05  A-10-4         PIC N/N/N         VALUE "１／２／３".
           05  A-10-5         PIC XX/XX/XX      VALUE "11/22/33".
       01  A-11.
           05  A-11-1         PIC X(5)          VALUE "ABCDE".
           05  A-11-2         PIC NNN           VALUE "あいう".
           05  A-11-3         PIC 999           VALUE 123.
           05  A-11-4         PIC N/N/N         VALUE "１／２／３".
           05  A-11-5         PIC XX/XX/XX      VALUE "11/22/33".
       01  A-12.
           05  A-12-1         PIC X(5)          VALUE "ABCDE".
           05  A-12-2         PIC NNN           VALUE "あいう".
           05  A-12-3         PIC 999           VALUE 123.
           05  A-12-4         PIC N/N/N         VALUE "１／２／３".
           05  A-12-5         PIC XX/XX/XX      VALUE "11/22/33".
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-3)".
      *ケース1.一意名１が日本語項目のとき（何も指定がない、またはDEFAULT指定のとき）
            MOVE "P-010-01"        TO CASE-ID.
            INITIALIZE A-01.
            IF A-01 = "　　　　　"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-01=" A-01
            END-IF.
      *ケース2：一意名１が日本語項目のとき（TO VALUE指定のとき）
            MOVE "P-020-01"        TO CASE-ID.
            MOVE SPACE       TO A-02.
            INITIALIZE A-02 replacing NATIONAL TO VALUE.
            IF A-02 = "あいうえお"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-02=" A-02
            END-IF.
      *ケース3：一意名１が日本語項目のとき（REPLACING一意名指定のとき）
            MOVE "P-030-01"        TO CASE-ID.
            MOVE SPACE       TO A-03-1.
            INITIALIZE A-03-1 REPLACING NATIONAL BY A-03-2.
            IF A-03-1 = "かかかかか" DISPLAY CASE-ID "OK"
               ELSE               DISPLAY CASE-ID "NG" "A-03-1=" A-03-1
            END-IF.
      *ケース4：一意名１が日本語項目のとき（REPLACING定数指定のとき）
            MOVE "P-040-01"        TO CASE-ID.
            MOVE SPACE       TO A-04.
            INITIALIZE A-04 REPLACING NATIONAL BY "かきくけこ".
            IF A-04 = "かきくけこ"   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-04=" A-04
            END-IF.
      *ケース5：一意名１が日本語編集項目のとき（何も指定がない、
      *         またはDEFAULT指定のとき）
            MOVE "P-050-01"        TO CASE-ID.
            INITIALIZE A-05.
            IF A-05 = "　／　／　"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-05=" A-05
            END-IF.
      *ケース6：一意名１が日本語編集項目のとき（TO VALUE指定のとき）
            MOVE "P-060-01"        TO CASE-ID.
            MOVE SPACE       TO A-06.
            INITIALIZE A-06 replacing NATIONAL-EDITED TO VALUE.
            IF A-06 = "１／２／３"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-06=" A-06
            END-IF.
      *ケース7：一意名１が日本語編集項目のとき（REPLACING一意名指定のとき）
            MOVE "P-070-01"        TO CASE-ID.
            INITIALIZE A-07-1 REPLACING NATIONAL-EDITED BY A-07-2.
            IF A-07-1 = "あ／い／う"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-07=" A-07
            END-IF.
      *ケース8：一意名１が日本語編集項目のとき（REPLACING定数指定のとき）
            MOVE "P-080-01"        TO CASE-ID.
            INITIALIZE A-08 REPLACING NATIONAL-EDITED BY "さしす".
            IF A-08 = "さ／し／す"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-08=" A-08
            END-IF.
      *ケース9：無名項目が日本語項目のとき（WITH FILLERで初期化されるか）
            MOVE "P-090-01"        TO CASE-ID.
            INITIALIZE A-09 WITH FILLER
                       REPLACING NATIONAL        BY "あいうえお"
                                 NATIONAL-EDITED BY "１２３".
            IF A-09 = "あいうえお１／２／３"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-09=" A-09
            END-IF.
      *ケース10：集団項目が指定されたとき(DEFAULTまたは何も指定されないとき）
            MOVE "P-100-01"        TO CASE-ID.
            INITIALIZE A-10.
            IF A-10-1 = SPACE       AND
               A-10-2 = SPACE       AND
               A-10-3 = 0           AND
               A-10-4 = "　／　／　" AND
               A-10-5 = "  /  /  "
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-10=" A-10
            END-IF.
      *ケース11：集団項目が指定されたとき(TO　VALUEが指定されたとき）
            MOVE "P-110-01"        TO CASE-ID.
            MOVE ALL "*" TO A-11.
            INITIALIZE A-11 ALL TO VALUE..
            IF A-11-1 = "ABCDE"      AND
               A-11-2 = "あいう"     AND
               A-11-3 = 123          AND
               A-11-4 = "１／２／３" AND
               A-11-5 = "11/22/33"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-11=" A-11
            END-IF.
      *ケース12：集団項目が指定されたとき(REPLACINGが指定されたとき）
            MOVE "P-120-01"        TO CASE-ID.
            MOVE ALL "*" TO A-12.
            INITIALIZE A-12 REPLACING NATIONAL        BY "かきく"
                                      NATIONAL-EDITED BY "あいう".
            IF A-12-1 = "*****"      AND
               A-12-2 = "かきく"     AND
               A-12-3(1:3) = "***"   AND
               A-12-4 = "あ／い／う" AND
               A-12-5 = "********"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG" "A-12=" A-12
            END-IF.
      *
            DISPLAY "TEST END   (EX5-3)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

