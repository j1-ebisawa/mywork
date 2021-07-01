      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXFQA-24
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jPICTURE‹å
      *    ˆ—ŠT—v@@FPICTURE‹åŽw’è‚ª³‚µ‚­ŽÀs‚³‚ê‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚P`‚T
      ******************************************************************
      *  REPLACE ==BYTE-LENGTH== BY ==LENGTH-AN==.
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-24.
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
       01  G-09               PIC N(16383).
      * 01  GE-09              PIC N(16382)/.     *>N(16383)/.
       01  G-99               PIC N(16384).
      * 01  GE-99              PIC N(16383)/.     *>N(16383)/.
       01  W-L                PIC 99999999.

       
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (QA-24)".
      *
       P-050. 
      *  ƒP[ƒX5.ŒÀŠEŒniƒf[ƒ^ƒTƒCƒYj
      *
            MOVE "P-050-01"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO G-09.
            MOVE FUNCTION BYTE-LENGTH(G-09) TO W-L.
            IF G-09 = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X"
               AND W-L = 32766
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"  W-L
            END-IF.
      *
      *      MOVE "P-050-02"             TO CASE-ID.
      *      MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO GE-09.
      *      IF GE-09(1:16382) = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" AND
      *         GE-09(16383:1) = "^"
      *         AND FUNCTION BYTE-LENGTH(GE-09) = 32766
      *                                  DISPLAY CASE-ID "OK"
      *         ELSE                     DISPLAY CASE-ID "NG"
      *      END-IF.
      *
      *
       P-051.
      *  ƒP[ƒX5.ŒÀŠEŒniƒf[ƒ^ƒTƒCƒYj
      *
            MOVE "P-051-01"             TO CASE-ID.
            MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO G-99.
            MOVE FUNCTION BYTE-LENGTH(G-99) TO W-L.
            IF G-99 = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X"
               AND W-L = 32768
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"  W-L
            END-IF.
      *
      *      MOVE "P-051-02"             TO CASE-ID.
      *      MOVE ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" TO GE-99.
      *      IF GE-09(1:16383) = ALL "‚O‚P‚Q‚R‚S‚T‚U‚V‚W‚X" AND
      *         GE-09(16384:1) = "^"
      *         AND FUNCTION BYTE-LENGTH(GE-99) = 32768
      *                                  DISPLAY CASE-ID "OK"
      *         ELSE                     DISPLAY CASE-ID "NG"
      *      END-IF.
      *
            DISPLAY "TEST END   (QA-24)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

