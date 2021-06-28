      ******************************************************************
      *    テストケース：5-4B
      *    プログラム名：日本語化テスト （手続き部）INSPECT命令
      *    処理概要    ：INSPECT命令が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:２２～４７
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-4B.
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
            display "TEST START(EX5-4B)".
      *ケース22.一意名１、定数4（REPLACING,CHARACTERS指定）
            MOVE "P-220-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            
            INSPECT G-01 REPLACING CHARACTERS BY "＊".
            IF G-01 = "＊＊＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-220-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING CHARACTERS BY "＊".
            IF GE-01 = "＊＊＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース23.一意名１、定数3、定数4（REPLACING,ALL指定）
            MOVE "P-230-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おかきく" TO GE-01.
            INSPECT G-01 REPLACING ALL "＊" BY "＠".
            IF G-01 = "あい＠え＠かき＠＠こ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-230-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL "＊" BY "＠".
            IF GE-01 = "あい／＠＠おか／きく"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース24：一意名１、定数3、定数4（REPLACING,LEADING指定）
            MOVE "P-240-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいうえかき" TO GE-01.
            INSPECT G-01 REPLACING LEADING "あ" BY "＠".
            IF G-01 = "＠＠＠＠＠＠いああう"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-240-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING LEADING "あ" BY "＠".
            IF GE-01 = "＠＠／あいうえ／かき"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース25：一意名１、定数3、定数4（REPLACING,TRAILING指定）
            MOVE "P-250-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あああい＊＊＊＊" TO GE-01.
            INSPECT G-01 REPLACING TRAILING "＊" BY "＠".
            IF G-01 = "あい＊＊おかき＠＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-250-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING TRAILING "＊" BY "＠".
            IF GE-01 = "ああ／あい＊＊／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース26：一意名１、定数3、定数4（REPLACING,FIRST指定）
            MOVE "P-260-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            INSPECT G-01 REPLACING FIRST "＊" BY "＠".
            IF G-01 = "あい＠＊おかき＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-260-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING FIRST "＊" BY "＠".
            IF GE-01 = "あ＠／＊い＊＊／＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース27：一意名１、定数3、定数4（REPLACING,ALL,2文字指定）
            MOVE "P-270-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            INSPECT G-01 REPLACING ALL "＊＊" BY "＠＠".
            IF G-01 = "あい＠＠おかき＠＠＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-270-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL "＊＊" BY "＠＠".
            IF GE-01 = "あ＊／＊い＠＠／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース28：一意名１、定数3、定数4（REPLACING,CHARACTERS,BEFORE指定）
            MOVE "P-280-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            INSPECT G-01 REPLACING CHARACTERS BY "＠" BEFORE "え".
            IF G-01 = "＠＠＠えおかきくけこ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-280-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING CHARACTERS BY "＠" BEFORE "え".
            IF GE-01 = "＠＠＠＠えおか／きく"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース29：一意名１、定数3、定数4、定数７（REPLACING,ALL,AFTER指定）
            MOVE "P-290-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おか＊き" TO GE-01.
            INSPECT G-01 REPLACING ALL "＊" BY "＠" AFTER "か".
            IF G-01 = "あい＊え＊かき＠＠こ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-290-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL "＊" BY "＠"  AFTER "か".
            IF GE-01 = "あい／＊＊おか／＠き"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.

      *ケース30：一意名１、定数3、定数4、定数７（REPLACING,LEADING,AFTER指定）
            MOVE "P-300-01"        TO CASE-ID.
            MOVE "あああああいああうあ" TO G-01.
            MOVE "あああいあああき" TO GE-01.

            INSPECT G-01 REPLACING LEADING "あ" BY "＠" AFTER "い".
            IF G-01 = "あああああい＠＠うあ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-300-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING LEADING "あ" BY "＠" AFTER "い".
            IF GE-01 = "ああ／あい＠＠／あき"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース31：一意名１、定数3、定数4、定数７（REPLACING,TRAILING,BEFORE指定）
            MOVE "P-310-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊え" TO G-01.
            MOVE "あ＊い＊う＊＊え" TO GE-01.
            INSPECT G-01 REPLACING TRAILING "＊" BY "＠"  BEFORE "え".
            IF G-01 ="あい＊＊おかき＠＠え"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-310-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING TRAILING "＊" BY "＠" BEFORE "え".
            IF GE-01 = "あ＊／い＊う＊／＠え"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース32：一意名１、定数3、定数4、定数７（REPLACING,FIRST,AFTER指定）
            MOVE "P-320-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊え" TO G-01.
            MOVE "あ＊＊いお＊＊＊" TO GE-01.
            INSPECT G-01 REPLACING FIRST "＊" BY "＠"  AFTER "お".
            IF G-01 ="あい＊＊おかき＠＊え"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-320-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING FIRST "＊" BY "＠" AFTER "お".
            IF GE-01 = "あ＊／＊いお＠／＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース33：一意名１、定数3、定数4、定数７（REPLACING,ALL,2文字指定,BEFORE）
            MOVE "P-330-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "＊＊＊い＊＊き＊" TO GE-01.
            INSPECT G-01 REPLACING ALL "＊＊" BY "＠＠" BEFORE "き＊".
            IF G-01 = "あい＠＠おかき＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-330-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL "＊＊" BY "＠＠"  BEFORE "き＊".
            IF GE-01 = "＠＠／＊い＠＠／き＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース34：一意名１、定数3、定数4、定数７（REPLACING,ALL,2文字指定,AFTER）
            MOVE "P-340-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あき＊＊き＊＊＊" TO GE-01.
            INSPECT G-01 REPLACING ALL "＊＊" BY "＠＠" AFTER "き＊".
            IF G-01 = "あい＊＊おかき＊＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-340-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL "＊＊" BY "＠＠"  AFTER "き＊".
            IF GE-01 = "あき／＊＊き＊／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.

      *ケース35.一意名１、一意名5（REPLACING,CHARACTERS指定）
            MOVE "P-350-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            MOVE "＊" TO G-05-1.
            
            INSPECT G-01 REPLACING CHARACTERS BY G-05-1.
            IF G-01 = "＊＊＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      
            MOVE "P-350-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING CHARACTERS BY G-05-1.
            IF G-01 = "＊＊＊＊＊＊＊＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース36.一意名１、一意名4、一意名5（REPLACING,ALL指定）
            MOVE "P-360-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おかきく" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            
            INSPECT G-01 REPLACING ALL G-04-1 BY G-05-1.
            IF G-01 = "あい＠え＠かき＠＠こ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-360-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL G-04-1 BY G-05-1.
            IF GE-01 = "あい／＠＠おか／きく"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース37：一意名１、一意名4、一意名5（REPLACING,LEADING指定）
            MOVE "P-370-01"        TO CASE-ID.
            MOVE "ああああああいああう" TO G-01.
            MOVE "あああいうえかき" TO GE-01.
            MOVE "あ" TO G-04-1.
            MOVE "＠" TO G-05-1.
            
            INSPECT G-01 REPLACING LEADING G-04-1 BY G-05-1.
            IF G-01 = "＠＠＠＠＠＠いああう"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-370-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING LEADING G-04-1 BY G-05-1.
            IF GE-01 = "＠＠／あいうえ／かき"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース38：一意名１、一意名4、一意名5（REPLACING,TRAILING指定）
            MOVE "P-380-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あああい＊＊＊＊" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            
            INSPECT G-01 REPLACING TRAILING G-04-1 BY G-05-1.
            IF G-01 = "あい＊＊おかき＠＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-380-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING TRAILING G-04-1 BY G-05-1.
            IF GE-01 = "ああ／あい＊＊／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース39：一意名１、一意名4、一意名5（REPLACING,FIRST指定）
            MOVE "P-390-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            
            INSPECT G-01 REPLACING FIRST G-04-1 BY G-05-1.
            IF G-01 = "あい＠＊おかき＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-390-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING FIRST G-04-1 BY G-05-1.
            IF GE-01 = "あ＠／＊い＊＊／＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース40：一意名１、一意名4、一意名5(REPLACING,ALL,2文字指定）
            MOVE "P-400-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "＊＊" TO G-04-2.
            MOVE "＠＠" TO G-05-2.
            
            INSPECT G-01 REPLACING ALL G-04-2 BY G-05-2.
            IF G-01 = "あい＠＠おかき＠＠＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-400-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL G-04-2 BY G-05-2.
            IF GE-01 = "あ＊／＊い＠＠／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース41：一意名１、一意名5、一意名8（REPLACING,CHARACTERS,BEFORE指定）
            MOVE "P-410-01"        TO CASE-ID.
            MOVE "あいうえおかきくけこ" TO G-01.
            MOVE "あいうえおかきく" TO GE-01.
            MOVE "＠" TO G-05-1.
            MOVE "え" TO G-08-1.
            
            INSPECT G-01 REPLACING CHARACTERS BY G-05-1 BEFORE G-08-1.
            IF G-01 = "＠＠＠えおかきくけこ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-410-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING CHARACTERS BY G-05-1 BEFORE G-08-1.
            IF GE-01 = "＠＠＠＠えおか／きく"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース42：一意名１、一意名4、一意名5、一意名8（REPLACING,ALL,AFTER指定）
            MOVE "P-420-01"        TO CASE-ID.
            MOVE "あい＊え＊かき＊＊こ" TO G-01.
            MOVE "あい＊＊おか＊き" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            MOVE "か" TO G-08-1.
            
            INSPECT G-01 REPLACING ALL G-04-1 BY G-05-1 AFTER G-08-1.
            IF G-01 = "あい＊え＊かき＠＠こ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-420-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL G-04-1 BY G-05-1 AFTER G-08-1.
            IF GE-01 = "あい／＊＊おか／＠き" 
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース43：一意名１、一意名4、一意名5、一意名8（REPLACING,LEADING,AFTER指定）
            MOVE "P-430-01"        TO CASE-ID.
            MOVE "あああああいああうあ" TO G-01.
            MOVE "あああいあああき" TO GE-01.
            MOVE "あ" TO G-04-1.
            MOVE "＠" TO G-05-1.
            MOVE "い" TO G-08-1.

            INSPECT G-01 REPLACING 
                         LEADING G-04-1 BY G-05-1 AFTER G-08-1.
            IF G-01 = "あああああい＠＠うあ"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-430-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING 
                          LEADING G-04-1 BY G-05-1 AFTER G-08-1.
            IF GE-01 = "ああ／あい＠＠／あき"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース44：一意名１、一意名4、一意名5、一意名8（REPLACING,TRAILING,BEFORE指定）
            MOVE "P-440-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊え" TO G-01.
            MOVE "あ＊い＊う＊＊え" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            MOVE "え" TO G-08-1.
            
            INSPECT G-01 REPLACING
                         TRAILING G-04-1 BY G-05-1  BEFORE G-08-1.
            IF G-01 = "あい＊＊おかき＠＠え"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-440-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING
                          TRAILING G-04-1 BY G-05-1  BEFORE G-08-1
            IF GE-01 = "あ＊／い＊う＊／＠え"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース45：一意名１、一意名4、一意名5、一意名8（REPLACING,FIRST,AFTER指定）
            MOVE "P-450-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あ＊＊いお＊＊＊" TO GE-01.
            MOVE "＊" TO G-04-1.
            MOVE "＠" TO G-05-1.
            MOVE "お" TO G-08-1.
            
            INSPECT G-01 REPLACING 
                         FIRST G-04-1 BY G-05-1  AFTER G-08-1.
            IF G-01 = "あい＊＊おかき＠＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-450-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING
                          FIRST G-04-1 BY G-05-1  AFTER G-08-1
            IF GE-01 = "あ＊／＊いお＠／＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース46：一意名１、一意名4、一意名5、一意名8（（REPLACING,ALL,2文字指定,BEFORE）
            MOVE "P-460-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "＊＊＊い＊＊き＊" TO GE-01.
            MOVE "＊＊" TO G-04-2.
            MOVE "＠＠" TO G-05-2.
            MOVE "き＊" TO G-08-2.
            
            INSPECT G-01 REPLACING ALL G-04-2 BY G-05-2 BEFORE G-08-2.
            IF G-01 = "あい＠＠おかき＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-460-02"        TO CASE-ID.
            MOVE 0 TO W-TALLY.
            INSPECT GE-01 REPLACING ALL G-04-2 BY G-05-2 BEFORE G-08-2.
            IF GE-01 = "＠＠／＊い＠＠／き＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース47：一意名１、一意名4、一意名5、一意名8（REPLACING,ALL,2文字指定,AFTER）
            MOVE "P-470-01"        TO CASE-ID.
            MOVE "あい＊＊おかき＊＊＊" TO G-01.
            MOVE "あき＊＊き＊＊＊" TO GE-01.
            MOVE "＊＊" TO G-04-2.
            MOVE "＠＠" TO G-05-2.
            MOVE "き＊" TO G-08-2.
            
            INSPECT G-01 REPLACING ALL G-04-2 BY G-05-2 AFTER G-08-2.
            IF G-01 = "あい＊＊おかき＊＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-470-02"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL G-04-2 BY G-05-2 AFTER G-08-2.
            IF GE-01 = "あき／＊＊き＊／＠＠"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ケース47追加.：一意名１、一意名4、一意名5、一意名8
      *    （AFTERで一意名8が見つからないとき）
      *    （BEFOREで一意名8が見つからないとき）
            MOVE "P-470-03"        TO CASE-ID.
            MOVE "あい＊＊おか／＊＊＊" TO G-01.
            MOVE "あ＊＊い＊＊＊＊" TO GE-01.
            MOVE "／＊" TO G-04-2.
            MOVE "＠＠" TO G-05-2
            MOVE "ＡＢ" TO G-08-2.

            INSPECT G-01 REPLACING ALL G-04-2 BY G-05-2 AFTER G-08-2.
            IF G-01 = "あい＊＊おか／＊＊＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-470-04"        TO CASE-ID.
            INSPECT GE-01 REPLACING ALL G-04-2 BY G-05-2 BEFORE G-08-2.
            IF GE-01 = "あ＊＠＠い＊＊＠＠＊"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *
            display "TEST END  (EX5-4B)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

