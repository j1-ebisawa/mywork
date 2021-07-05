      ******************************************************************
      *    テストケース：5-1-1C
      *    プログラム名：日本語化テスト （手続き部）条件 比較条件
      *    処理概要　　：日本語比較が正しく実行されるかをチェックする。
      *  --------------------------------------------------------------
      *   テストケース:１７～３２
      *   　　　　　　　ケース１９、２７はコンパイルエラーとしたい。
      *   　　　　　　　確認後、コメント化して、実行テストする。
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-1-1C.
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
           05  L-G            PIC N(5).
           05  FILLER         REDEFINES L-G.
               10  L-G-3      PIC N(3).
               10  L-G-2      PIC N(2).
           05  L-GE           PIC N/N/N.
           05  FILLER         REDEFINES L-GE.
               10  L-GE-3     PIC N/N.
               10  L-GE-2     PIC /N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  W-R-GRP-10     REDEFINES R-A.
               10  W-R-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC N(5).
           05  FILLER         REDEFINES R-G.
               10  R-G-3      PIC N(3).
               10  R-G-2      PIC N(2).
           05  R-GE           PIC N/N/N.
           05  FILLER         REDEFINES R-GE.
               10  R-GE-3     PIC N/N.
               10  R-GE-2     PIC /N.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-017. 
            DISPLAY "TEST START (EX5-1-1C)".
      *  ケース17.（左辺）日本語定数＋（右辺）集団項目
            MOVE "あいうえお"        TO W-R-GRP.
      *
            MOVE "P-170-01"        TO CASE-ID.
            IF "あいうえお" = W-R-GRP-10
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-170-02"        TO CASE-ID.
            IF "あいうえお" > W-R-GRP-4   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-170-03"        TO CASE-ID.
            IF "あいうえお" < W-R-GRP         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-018. 
      *  ケース18.（左辺）日本語定数＋（右辺）英数字・英数字編集・数字編集
            MOVE "あい12えお"        TO R-AN.
            MOVE "ABCDEF"            TO R-ANE.
            MOVE 12345               TO R-NE.
      *
            MOVE "P-180-01"        TO CASE-ID.
            IF "あいうえお" = R-AN   DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-180-02"        TO CASE-ID.
            IF "あいうえお" > R-ANE  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-180-03"        TO CASE-ID.
            IF "あいうえお" < R-NE   DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-019. 
      *  ケース19.（左辺）日本語定数＋（右辺）英字・数字
      *           コンパイルエラーとなるか
           MOVE "P-190-01"        TO CASE-ID.
           IF "あいうえお" = R-A    DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
           MOVE "P-190-02"        TO CASE-ID.
           IF "あいうえお" > R-ZONE DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
       P-200. 
      *  ケース4.（左辺）日本語定数＋（右辺）日本語
            MOVE "あいうえお"        TO R-G.
      *
            MOVE "P-200-01"        TO CASE-ID.
            IF "あいうえお" = R-G    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-02"        TO CASE-ID.
            IF "あいうえお" > R-G-3  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-200-03"        TO CASE-ID.
            IF "あいう" < R-G        DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-210. 
      *  ケース21.（左辺）日本語定数＋（右辺）日本語編集
            MOVE "あいう"            TO R-GE.
      *
            MOVE "P-210-01"        TO CASE-ID.
            IF "あ／い／う" = R-GE   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-210-02"        TO CASE-ID.
            IF "あ／い／う" > R-GE-3 DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-210-03"        TO CASE-ID.
            IF "あ／い" < R-GE       DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-220. 
      *  ケース22.（左辺）日本語定数＋（右辺）日本語定数
            MOVE "あいうえお"      TO L-G.
      *
            MOVE "P-220-01"        TO CASE-ID.
            IF "あいうえお" =  "あいうえお"       
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-220-02"        TO CASE-ID.
            IF "あいうえお" > "あい" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-220-03"        TO CASE-ID.
            IF "あい" < "あいうえお" DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
      *
       P-230. 
      *  ケース23.（左辺）日本語定数＋（右辺）ALL 日本語定数
            MOVE "あいあいあ"      TO L-G.
      *
            MOVE "P-230-01"        TO CASE-ID.
            IF "あいあいあ" =  ALL "あい"     
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-02"        TO CASE-ID.
            IF "あいあ" > ALL "あい" DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-230-03"        TO CASE-ID.
            IF "あいあいあい" < ALL "あい"      
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-240. 
      *  ケース8.（左辺）日本語定数＋（右辺）表意定数
      *
            MOVE "P-240-01"        TO CASE-ID.
            IF "’" =  QUOTES        DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-02"        TO CASE-ID.
            IF "’" > SPACES         DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-03"        TO CASE-ID.
            IF "’" < ZEROES         DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-250. 
      *  ケース25.（左辺）ALL 日本語定数＋（右辺）集団項目
            MOVE ALL "あいう"        TO W-R-GRP.
      *
            MOVE "P-250-01"        TO CASE-ID.
            IF ALL "あいう" = W-R-GRP-10        
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-02"        TO CASE-ID.
            IF ALL "あいう" > W-R-GRP-4   
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-250-03"        TO CASE-ID.
            IF ALL "あいう" < W-R-GRP        
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-260. 
      *  ケース26.（左辺）ALL 日本語定数＋（右辺）英数字・英数字編集・数字編集
            MOVE ALL "あいう"        TO R-AN.
            MOVE "ABCDEF"            TO R-ANE.
            MOVE 123                 TO R-NE.
            
      *
            MOVE "P-260-01"        TO CASE-ID.
            IF ALL "あいう" = R-AN   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-02"        TO CASE-ID.
            IF ALL "あいう" > R-ANE  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-03"        TO CASE-ID.
            IF ALL "あいう" < R-NE   DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-270. 
      *  ケース27.（左辺）ALL 日本語定数＋（右辺）英字・数字
      *           コンパイルエラーとなるか
           MOVE "P-270-01"        TO CASE-ID.
           IF ALL "あいう" = R-A    DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
           MOVE "P-270-02"        TO CASE-ID.
           IF ALL "あいう" > R-ZONE DISPLAY CASE-ID "NG"
              ELSE                  DISPLAY CASE-ID "NG"
           END-IF.
      *
       P-280. 
      *  ケース28.（左辺）ALL 日本語定数＋（右辺）日本語
            MOVE ALL "あいう"      TO R-G.
            
      *
            MOVE "P-280-01"        TO CASE-ID.
            IF ALL "あいう" = R-G    DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-280-02"        TO CASE-ID.
            IF ALL "あいう" > R-G-3  DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-280-03"        TO CASE-ID.
            IF ALL "あいう" < R-G    DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-290. 
      *  ケース29.（左辺）ALL 日本語定数＋（右辺）日本語編集
            MOVE ALL "あいう"      TO R-GE.
            
      *
            MOVE "P-290-01"        TO CASE-ID.
            IF ALL "あ／い／う" = R-GE           
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-290-02"        TO CASE-ID.
            IF ALL "あ／い／う" > R-GE-3         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-290-03"        TO CASE-ID.
            IF ALL "あ／い／う" < R-GE         
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-300. 
      *  ケース30.（左辺）ALL 日本語定数＋（右辺）日本語定数
            
      *
            MOVE "P-300-01"        TO CASE-ID.
            IF ALL "あいう" = "あいうあいう"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-300-02"        TO CASE-ID.
            IF ALL "あいう" > "あいうあい"       
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            MOVE "P-300-03"        TO CASE-ID.
            IF ALL "あいう" < "あい" 
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-310. 
      *  ケース31.（左辺）ALL 日本語定数＋（右辺）ALL 日本語定数
            
      *
            MOVE "P-310-01"        TO CASE-ID.
            IF ALL "あいう" = ALL "あいう"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-310-02"        TO CASE-ID.
            IF ALL "あいう" > ALL "あい"   
                                     DISPLAY CASE-ID "OK"      *>20110921修正
               ELSE                  DISPLAY CASE-ID "NG"      *>20110921修正
            END-IF.
      *
            MOVE "P-310-03"        TO CASE-ID.
            IF ALL "あいう" < ALL "あい" 
                                     DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
       P-320. 
      *  ケース32.（左辺）ALL 日本語定数＋（右辺）表意定数
            
      *
            MOVE "P-320-01"        TO CASE-ID.
            IF ALL "”" = QUOTES     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-320-02"        TO CASE-ID.
            IF ALL "あ" > QUOTES     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-320-03"        TO CASE-ID.
            IF ALL "０" < ZERO       DISPLAY CASE-ID "NG"
               ELSE                  DISPLAY CASE-ID "OK"
            END-IF.
      *
            DISPLAY "TEST END   (EX5-1-1C)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

