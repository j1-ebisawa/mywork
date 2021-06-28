      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF4-2A
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jPICTURE‹å
      *    ˆ—ŠT—v@@FPICTURE‹åŽw’è‚ª³‚µ‚­ŽÀs‚³‚ê‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚P`‚T
      ******************************************************************
      * REPLACE ==BYTE-LENGTH== BY ==LENGTH-AN==.
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-2A.
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
       01  G-01               PICTURE N.
       01  G-02               PIC NNN.
       01  G-03               PIC N(10).
       01  G-04               PIC N(5)NNN(5).
       01  G-05               PIC NNNNNNNNNNNNNNNNNNNNNNNNNNNNNN.
       01  G-06               PIC N(2)N(2)N(2)N(2)N(2)N(2)N(2)NN.
       01  G-07               PIC N(20)N(20)N(20)N(20)N(20)N(20).
       01  G-08               PIC N(20)N(20)NNNNNNNNNNNNNNNN(20).
       01  G-09               PIC N(16383).
       
       01  GE-01              PICTURE NBN.
       01  GE-02              PIC N/N.
       01  GE-03              PIC N0N.
       01  GE-04              PIC N/NBN0N.
       01  GE-05              PIC NBNBNBNBNBNBNBNBNBNBNBNBNBNBNB.
       01  GE-06              PIC N(3)/N(3)/N(3)/N(3)/N(3)/N(3)/.
       01  GE-07              PIC N(20)0N(20)0N(20)0N(20)0N(20)0.
       01  GE-08              PIC N(09)0BN(09)0BN(09)0BN(09)0BNN.
       01  GE-09              PIC N(16383)/.
       
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-2A)".
      *  ƒP[ƒX1.“ú–{Œêƒf[ƒ^iNj
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE "‚ " TO G-01.
            IF G-01 = "‚ " 
               AND FUNCTION BYTE-LENGTH(G-01) = 2
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE "‚ ‚¢‚¤"               TO G-02.
            IF G-02 = "‚ ‚¢‚¤" 
               AND FUNCTION BYTE-LENGTH(G-02) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE "‚ ‚¢‚¤‚¦‚¨‚©‚«‚­‚¯‚±" TO G-03.
            IF G-03 = "‚ ‚¢‚¤‚¦‚¨‚©‚«‚­‚¯‚±"
               AND FUNCTION BYTE-LENGTH(G-03) = 20
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-04"             TO CASE-ID.
            MOVE "‚P‚Q‚R‚S‚T‚U‚V‚W‚X‚O‚P‚Q" TO G-04.
            IF G-04 = "‚P‚Q‚R‚S‚T‚U‚V‚W‚X‚O‚P‚Q" 
               AND FUNCTION BYTE-LENGTH(G-04) = 24
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-020. 
      *  ƒP[ƒX2.“ú–{Œê•ÒWƒf[ƒ^(N,B,/,0)
      *
            MOVE "P-020-01"             TO CASE-ID.
            MOVE "‚ ‚¢" TO GE-01
            IF GE-01 = "‚ @‚¢"
               AND FUNCTION BYTE-LENGTH(GE-01) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE "‚ ‚¢"  TO GE-02.  
            IF GE-02 = "‚ ^‚¢"
               AND FUNCTION BYTE-LENGTH(GE-02) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-03"             TO CASE-ID.
            MOVE "‚ ‚¢"  TO GE-03.  
            IF GE-03 = "‚ ‚O‚¢"
               AND FUNCTION BYTE-LENGTH(GE-03) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-04"             TO CASE-ID.
            MOVE "‚P‚Q‚R‚S"  TO GE-04.  
            IF GE-04 = "‚P^‚Q@‚R‚O‚S"
               AND FUNCTION BYTE-LENGTH(GE-04) = 14
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-030. 
      *  ƒP[ƒX3.“ú–{Œêƒf[ƒ^iPIC•¶Žš—ñ30Žšj
            MOVE "P-030-01"             TO CASE-ID.
            MOVE ALL "‚ ‚¢" TO G-05.
            IF G-05 = ALL "‚ ‚¢"
               AND FUNCTION BYTE-LENGTH(G-05) = 60
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            MOVE ALL "‚ ‚¢‚¤" TO G-06.
            IF G-06 = ALL "‚ ‚¢‚¤"
               AND FUNCTION BYTE-LENGTH(G-06) = 32
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-03"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO G-07.
            IF G-07 = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X"
               AND FUNCTION BYTE-LENGTH(G-07) = 240
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-04"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO G-08.
            IF G-08 = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X"
               AND FUNCTION BYTE-LENGTH(G-08) = 150
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
       P-040. 
      *  ƒP[ƒX4.“ú–{Œê•ÒWƒf[ƒ^iPIC•¶Žš—ñ30Žšj
      *
            MOVE "P-040-01"             TO CASE-ID.
            MOVE ALL "‚ ‚¢‚¤‚¦‚¨‚©‚«‚­‚¯‚±‚³‚µ‚·‚¹‚»" TO GE-05.
            IF GE-05 = 
        "‚ @‚¢@‚¤@‚¦@‚¨@‚©@‚«@‚­@‚¯@‚±@‚³@‚µ@‚·@‚¹@‚»@"
               AND FUNCTION BYTE-LENGTH(GE-05) = 60
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-02"             TO CASE-ID.
            MOVE ALL "‚ ‚¢‚¤"          TO GE-06.
            IF GE-06 = ALL "‚ ‚¢‚¤^"
               AND FUNCTION BYTE-LENGTH(GE-06) = 48
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-03"             TO CASE-ID.
            MOVE ALL "‚`‚a‚b‚c‚d‚e‚f‚g‚h‚i"   TO GE-07.
            IF GE-07 = ALL "‚`‚a‚b‚c‚d‚e‚f‚g‚h‚i‚`‚a‚b‚c‚d‚e‚f‚g‚h‚i‚O"       *>20111012 QA-25
               AND FUNCTION BYTE-LENGTH(GE-07) = 210
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-040-04"             TO CASE-ID.
            MOVE ALL "‚P‚Q‚R‚S‚T‚U‚V‚W‚X"   TO GE-08.
            IF GE-08 = ALL "‚P‚Q‚R‚S‚T‚U‚V‚W‚X‚O@"
               AND FUNCTION BYTE-LENGTH(GE-08) = 92
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-050. 
      *  ƒP[ƒX5.ŒÀŠEŒniƒf[ƒ^ƒTƒCƒYj
      *
            MOVE "P-050-01"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO G-09.
            IF G-09 = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X"
               AND FUNCTION BYTE-LENGTH(G-09) = 32766
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-050-02"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO GE-09.
            IF GE-09(1:16383) = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" AND
               GE-09(16384:1) = "^"
               AND FUNCTION BYTE-LENGTH(GE-09) = 32768
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX4-2A)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

