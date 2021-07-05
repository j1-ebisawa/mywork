      ******************************************************************
      *    テストケース：5-4A
      *    プログラム名：日本語化テスト （手続き部）INSPECT命令
      *    処理概要    ：INSPECT命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１～２１
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-4A.
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
       01  CASE-ID            PIC X(10).
       01  W-TALLY  PIC 999.
       01  G-01     PIC N(10).
       01  GE-01    PIC NN/NNNN/NN.
       01  G-03-1   PIC N.
       01  G-03-2   PIC NN.
       01  G-04-1   PIC N.
       01  G-04-2   PIC NN.
       01  G-05-1   PIC N.
       01  G-05-2   PIC NN.
       01  G-06-1   PIC N.
       01  G-06-2   PIC NN.
       01  G-08-1   PIC N.
       01  G-08-2   PIC NN.

      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            display "TEST START(EX5-4A)".
      *ケース1.一意名１（TALLYING,CHARACTERS指定）
            MOVE "P-010-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR CHARACTERS.
            IF W-TALLY = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-010-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR CHARACTERS.
            IF W-TALLY = 10
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース2.一意名１、定数２（TALLYING,ALL指定）
            MOVE "P-020-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おかき" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL "＊".
            IF W-TALLY = 4
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-020-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL "＊".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース3：一意名１、定数２（TALLYING,LEADING指定）
            MOVE "P-030-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいうえかき" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR LEADING "あ".
            IF W-TALLY = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-030-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR LEADING "あ".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース4：一意名１、定数２（TALLYING,TRAILING指定）
            MOVE "P-040-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あああい＊＊＊＊" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR TRAILING "＊".
            IF W-TALLY = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-040-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR TRAILING "＊".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース5：一意名１、定数２（TALLYING,ALL,2文字指定）
            MOVE "P-050-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL "＊＊".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
       P-050-02.
      *
            MOVE "P-050-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL "＊＊".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース6：一意名１、定数７（TALLYING,CHARACTERS,BEFORE指定）
            MOVE "P-060-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR CHARACTERS BEFORE "え".
            IF W-TALLY = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-060-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR CHARACTERS BEFORE "か".
            IF W-TALLY = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース7：一意名１、定数２、定数７（TALLYING,ALL,AFTER指定）
            MOVE "P-070-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おか＊き" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL "＊" AFTER "か".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-070-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL "＊" AFTER "お".
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース8：一意名１、定数２、定数７（TALLYING,LEADING,AFTER指定）
            MOVE "P-080-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいあああき" TO GE-01.

            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR LEADING "あ" AFTER "い".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-080-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR LEADING "あ" AFTER "い".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース9：一意名１、定数２、定数７（TALLYING,TRAILING,BEFORE指定）
            MOVE "P-090-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊え" TO G-01.
            MOVE "あ＊い＊う＊＊え" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR TRAILING "＊" BEFORE "え".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-090-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR
                                           TRAILING "＊" BEFORE "え".
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース10：一意名１、定数２、定数７（TALLYING,ALL,2文字指定,BEFORE）
            MOVE "P-100-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL "＊＊" BEFORE "き＊".
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-100-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL "＊＊" BEFORE "／＊".
            IF W-TALLY = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース11：一意名１、定数２、定数７（TALLYING,ALL,2文字指定,AFTER）
            MOVE "P-110-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.

            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL "＊＊" AFTER "き＊".
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-110-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL "＊＊" AFTER "／＊".
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース12.一意名１、一意名３（TALLYING,ALL指定）
            MOVE "P-120-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おかき" TO GE-01.
            MOVE "＊" TO G-03-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-1.
            IF W-TALLY = 4
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-120-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース13：一意名１、一意名3（TALLYING,LEADING指定）
            MOVE "P-130-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいうえかき" TO GE-01.
            MOVE "あ" TO G-03-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR LEADING G-03-1.
            IF W-TALLY = 6
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-130-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR LEADING G-03-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース14：一意名１、一意名3（TALLYING,TRAILING指定）
            MOVE "P-140-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あああい＊＊＊＊" TO GE-01.
            MOVE "＊" TO G-03-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR TRAILING G-03-1.
            IF W-TALLY = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-140-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR TRAILING G-03-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース15：一意名１、一意名3（TALLYING,ALL,2文字指定）
            MOVE "P-150-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "＊＊" TO G-03-2.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-2.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-150-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-2.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース16：一意名１、一意名8（TALLYING,CHARACTERS,BEFORE指定）
            MOVE "P-160-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            MOVE "え" TO G-08-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR CHARACTERS BEFORE G-08-1.
            IF W-TALLY = 3
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-160-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR CHARACTERS BEFORE G-08-1.
            IF W-TALLY = 4
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース17：一意名１、一意名3、一意名8（TALLYING,ALL,AFTER指定）
            MOVE "P-170-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おか＊き" TO GE-01.
            MOVE "＊" TO G-03-1.
            MOVE "か" TO G-08-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-1 AFTER G-08-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-170-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-1 AFTER G-08-1.
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース18：一意名１、一意名3、一意名8（TALLYING,LEADING,AFTER指定）
            MOVE "P-180-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいあああき" TO GE-01.
            MOVE "あ" TO G-03-1.
            MOVE "い" TO G-08-1.

            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR 
                                          LEADING G-03-1 AFTER G-08-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-180-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR 
                                           LEADING G-03-1 AFTER G-08-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース19：一意名１、一意名3、一意名8（TALLYING,TRAILING,BEFORE指定）
            MOVE "P-190-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊え" TO G-01.
            MOVE "あ＊い＊う＊＊え" TO GE-01.
            MOVE "＊" TO G-03-1.
            MOVE "え" TO G-08-1.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR
                                          TRAILING G-03-1 BEFORE G-08-1.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-190-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR
                                          TRAILING G-03-1 BEFORE G-08-1.
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース20：一意名１、一意名3、一意名8（（TALLYING,ALL,2文字指定,BEFORE）
            MOVE "P-200-01"        TO CASE-ID.
            MOVE "あい＊＊おかきき＊＊" TO G-01.
            MOVE "＊＊＊＊きき＊＊" TO GE-01.
            MOVE "＊＊" TO G-03-2.
            MOVE "きき" TO G-08-2.
            
            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-2 BEFORE G-08-2.
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-200-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-2 BEFORE G-08-2.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース21：一意名１、一意名3、一意名8（TALLYING,ALL,2文字指定,AFTER）
            MOVE "P-210-01"        TO CASE-ID.
            MOVE "あい＊＊おか／＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "／＊" TO G-03-2.
            MOVE "＊＊" TO G-08-2.
            

            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-2 AFTER G-08-2.
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-210-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-2 AFTER G-08-2.
            IF W-TALLY = 1
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *ケース21追加.：一意名１、一意名3、一意名8
      *    （AFTERで一意名8が見つからないとき）
      *    （BEFOREで一意名8が見つからないとき）
            MOVE "P-210-03"        TO CASE-ID.
            MOVE "あい＊＊おか／＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "／＊" TO G-03-2.
            MOVE "ＡＢ" TO G-08-2.


            MOVE 0 TO W-TALLY.
            INSPECT G-01 TALLYING W-TALLY FOR ALL G-03-2 AFTER G-08-2.
            IF W-TALLY = 0
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      *
            MOVE "P-210-04"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 TALLYING W-TALLY FOR ALL G-03-2 BEFORE G-08-2.
            IF W-TALLY = 2
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" W-TALLY
            END-IF.
      
      *
            display "TEST END  (EX5-4A)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

