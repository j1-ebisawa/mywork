      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF4-3A
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jUSAGE‹å
      *    ˆ—ŠT—v@@FUSAGE‹å‚ðŽw’è‚Å‚«‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚P`‚Q
      ******************************************************************
      * REPLACE ==BYTE-LENGTH== BY ==LENGTH-AN==.
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-3A.
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
       01  G-01               PICTURE N        USAGE NATIONAL.
       01  G-02               PIC NNN          USAGE NATIONAL.
       01  G-03               PIC N(10)        USAGE NATIONAL.
       01  G-04               PIC N(5)NNN(5)   USAGE NATIONAL.
       
       01  GE-01              PICTURE NBN      USAGE NATIONAL.
       01  GE-02              PIC N/N          USAGE NATIONAL.
       01  GE-03              PIC N0N          USAGE NATIONAL.
       01  GE-04              PIC N/NBN0N      USAGE NATIONAL.
       01  wk-len             pic 999.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-3A)".
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
            move FUNCTION BYTE-LENGTH(GE-01) to wk-len
            IF GE-01 = "‚ @‚¢"
               AND FUNCTION BYTE-LENGTH(GE-01) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-01 
                                                        "WK-L=" wk-len
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            MOVE "‚ ‚¢"  TO GE-02.  
            IF GE-02 = "‚ ^‚¢"
               AND FUNCTION BYTE-LENGTH(GE-02) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-02
            END-IF.
      *
            MOVE "P-020-03"             TO CASE-ID.
            MOVE "‚ ‚¢"  TO GE-03.  
            IF GE-03 = "‚ ‚O‚¢"
               AND FUNCTION BYTE-LENGTH(GE-03) = 6
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-03
            END-IF.
      *
            MOVE "P-020-04"             TO CASE-ID.
            MOVE "‚P‚Q‚R‚S"  TO GE-04.  
            IF GE-04 = "‚P^‚Q@‚R‚O‚S"
               AND FUNCTION BYTE-LENGTH(GE-04) = 14
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" GE-04
            END-IF.
      *
      *
            DISPLAY "TEST END   (EX4-3A)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

