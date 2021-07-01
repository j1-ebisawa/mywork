      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF5-4C
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iŽè‘±‚«•”jINSPECT–½—ß
      *    ˆ—ŠT—v    FINSPECT–½—ß‚ª³‚µ‚­ŽÀs‚³‚ê‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚S‚W`‚U‚P
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-4C.
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
       01  G-07-1   PIC N.
       01  G-07-2   PIC NN.
       01  G-08-1   PIC N.
       01  G-08-2   PIC NN.

      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX5-4C)".
      *ƒP[ƒX48.ˆêˆÓ–¼‚PA’è”5A’è”6iCONVERTING,ALLŽw’èj
            MOVE "P-480-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚«–––" TO GE-01.
            INSPECT G-01 CONVERTING "–" TO "—".
            IF G-01 = "‚ ‚¢——‚¨‚©‚«———"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-480-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING "–" TO "—".
            IF GE-01 = "‚ ‚«^——‚«—^——"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX49FˆêˆÓ–¼‚PA’è”5A’è”6A’è”7iCONVERTING,BEFOREŽw’èj
            MOVE "P-490-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚©–––" TO GE-01.
            INSPECT G-01 CONVERTING "–" TO "—" BEFORE "‚©".
            IF G-01 = "‚ ‚¢——‚¨‚©‚«–––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-490-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING "–" TO "—" BEFORE "‚©".
            IF GE-01 = "‚ ‚«^——‚©–^––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX50FˆêˆÓ–¼‚PA’è”5A’è”6A’è”‚ViCONVERTING,AFTERŽw’èj
            MOVE "P-500-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚©–––" TO GE-01.
            INSPECT G-01 CONVERTING "–" TO "—" AFTER "‚©".
            IF G-01 = "‚ ‚¢––‚¨‚©‚«———"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-500-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING "–" TO "—"  AFTER "‚©".
            IF GE-01 = "‚ ‚«^––‚©—^——"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX51FˆêˆÓ–¼‚PAˆêˆÓ–¼6AˆêˆÓ–¼‚ViCONVERTINGj
            MOVE "P-510-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚«–––" TO GE-01.
            MOVE "–" TO G-06-1.
            MOVE "—" TO G-07-1.
            
            INSPECT G-01 CONVERTING G-06-1 TO G-07-1.
            IF G-01 = "‚ ‚¢——‚¨‚©‚«———"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
            
            MOVE "P-510-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING G-06-1 TO G-07-1.
            IF GE-01 = "‚ ‚«^——‚«—^——"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX52FˆêˆÓ–¼‚PAˆêˆÓ–¼6AˆêˆÓ–¼7AˆêˆÓ–¼8iCONVERTING,BEFOREŽw’èj
            MOVE "P-520-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚©–––" TO GE-01.
            MOVE "–" TO G-06-1.
            MOVE "—" TO G-07-1
            MOVE "‚©" TO G-08-1.
            
            INSPECT G-01 CONVERTING G-06-1 TO G-07-1 BEFORE G-08-1.
            IF G-01 = "‚ ‚¢——‚¨‚©‚«–––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-520-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING G-06-1 TO G-07-1 BEFORE G-08-1.
            IF GE-01 = "‚ ‚«^——‚©–^––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX53FˆêˆÓ–¼‚PAˆêˆÓ–¼6AˆêˆÓ–¼7AˆêˆÓ–¼8iCONVERTING,AFTERŽw’èj
            MOVE "P-530-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚©–––" TO GE-01.
            MOVE "–" TO G-06-1.
            MOVE "—" TO G-07-1
            MOVE "‚©" TO G-08-1.
            
            INSPECT G-01 CONVERTING G-06-1 TO G-07-1 AFTER G-08-1.
            IF G-01 = "‚ ‚¢––‚¨‚©‚«———"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-530-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING G-06-1 TO G-07-1 AFTER G-08-1.
            IF GE-01 = "‚ ‚«^––‚©—^——" 
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX54FˆêˆÓ–¼‚PA’è”3A’è”4A’è”7iCONVERTING,2•¶ŽšŽw’è,BEFOREŽw’èj
            MOVE "P-540-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "–––‚¢––‚«–" TO GE-01.

            INSPECT G-01 CONVERTING "–‚¨" TO "——" BEFORE "‚«–".
            IF G-01 = "‚ ‚¢–——‚©‚«–––"                              *>20111107
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-540-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING  "‚¢–" TO "——" BEFORE "‚«–".
            IF GE-01 = "——^————^‚«–"                             *>20111107
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
      *ƒP[ƒX55FˆêˆÓ–¼‚PAˆêˆÓ–¼4AˆêˆÓ–¼5AˆêˆÓ–¼8iCONVERTING,2•¶ŽšŽw’è,AFTERŽw’èj
            MOVE "P-550-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«–––" TO G-01.
            MOVE "‚ ‚«––‚«–––" TO GE-01.
            MOVE "––" TO G-04-2.
            MOVE "——" TO G-05-2
            MOVE "‚«–" TO G-08-2.

            INSPECT G-01 CONVERTING G-04-2 TO G-05-2 AFTER G-08-2.
            IF G-01 = "‚ ‚¢––‚¨‚©‚«–——"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            MOVE "P-540-02"        TO CASE-ID.
            INSPECT GE-01 CONVERTING G-04-2 TO G-05-2 AFTER G-08-2.
            IF GE-01 = "‚ ‚«^––‚«–^——"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" GE-01
            END-IF.
            
      *ƒP[ƒX56FŒJ‚è•Ô‚µ\•¶1
            MOVE "P-560-01"        TO CASE-ID.
            MOVE "‚ ‚¢––‚¨‚©‚«––‚¦" TO G-01.
            
            INSPECT G-01 REPLACING ALL "‚ " BY "‚©"
                                   ALL "‚¢" BY "‚«".
            IF G-01 = "‚©‚«––‚¨‚©‚«––‚¦"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *ƒP[ƒX57FŒJ‚è•Ô‚µ\•¶2
            MOVE "P-570-01"        TO CASE-ID.
            MOVE "‚ –‚¢––‚©‚«–––" TO G-01.
            
            INSPECT G-01 REPLACING ALL "–" BY "—"
                                   AFTER "‚¢" BEFORE "‚©".
            IF G-01 = "‚ –‚¢——‚©‚«–––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *ƒP[ƒX58FŒJ‚è•Ô‚µ\•¶‚R
            MOVE "P-580-01"        TO CASE-ID.
            MOVE "–‚¢–‚©‚«––‚©‚«–" TO G-01.
            
            INSPECT G-01 TALLYING W-TALLY FOR ALL "–" AFTER "‚¢"
                         REPLACING ALL "‚©‚«" BY "‚³‚µ" BEFORE "––".
            IF G-01 = "–‚¢–‚³‚µ––‚©‚«–"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *ƒP[ƒX59F‚»‚Ì‘¼‚P
            MOVE "P-590-01"        TO CASE-ID.
            MOVE "–—––––––––" TO G-01.
            
            INSPECT G-01 REPLACING ALL "–––" BY "”””"
            IF G-01 = "–—””””””––"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *ƒP[ƒX60F‚»‚Ì‘¼‚Qi•\ˆÓ’è”j
            MOVE "P-600-01"        TO CASE-ID.
            MOVE "‚P‚Q‚R‚S‚T‚U‚V‚W‚X‚O" TO G-01.
            
            INSPECT G-01 REPLACING ALL "‚T‚U" BY SPACE
            IF G-01 = "‚P‚Q‚R‚S@@‚V‚W‚X‚O"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *ƒP[ƒX61F‚»‚Ì‘¼‚Ri•\ˆÓ’è”j
            MOVE "P-610-01"        TO CASE-ID.
            MOVE "‚P‚Q‚R‚S‚T‚U‚V‚W‚X‚O" TO G-01.
            
            INSPECT G-01 REPLACING ALL "‚T" BY QUOTE
            IF G-01 = "‚P‚Q‚R‚Sh‚U‚V‚W‚X‚O"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.

      *
            DISPLAY "TEST END   (EX5-4C)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

