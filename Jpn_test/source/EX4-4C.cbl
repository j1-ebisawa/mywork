      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF4-4C
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jVALUE‹å
      *    ˆ—ŠT—v@@FVALUE‹åŽw’è‚ªŒë‚Á‚Ä‚¢‚é‚Æ‚«AƒGƒ‰[•\Ž¦‚³‚ê‚é‚©
      *                  ‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚W`‚X
      *   ‚±‚ÌƒvƒƒOƒ‰ƒ€‚ÍƒGƒ‰[ƒ`ƒFƒbƒN‚Ì‚½‚ßAŽÀs‚Å‚«‚È‚¢
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-4C.
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
       01  G-08      PIC NNN    VALUE "‚P‚Q‚R‚S‚T".
       01  G-09      PIC NNN    VALUE "ABC".
       01  GE-08     PIC N/N/N  VALUE "‚P^‚Q^‚R^‚S^‚T".
       01  GE-09     PIC N/N/N  VALUE "ABC".
       01  ERR-01     PIC NNN    VALUE 123.
       01  ERR-02     PIC NNN    VALUE -123.5.
       01  ERR-03.
           05  ERR-03-1          
           VALUE "01@‚PŒŽ02@‚QŒŽ03@‚RŒŽ04@‚SŒŽ05@‚TŒŽ06@‚UŒŽ" &
                 "07@‚VŒŽ08@‚WŒŽ09@‚XŒŽ10‚P‚OŒŽ11‚P‚PŒŽ12‚P‚QŒŽ".
            10  FILLER          OCCURS 10.
              15  ERR-03-11     PIC 99.
              15  ERR-03-12     PIC NNN.
       01  A-01           PIC AAA            VALUE "‚P‚Q‚R‚S‚T".
       01  X-01           PIC XXX            VALUE "‚`‚a‚b".
       01  N-ZONE         PIC 9999           VALUE "‚P^‚Q^‚R^‚S^‚T".
       01  N-ZONE-DEC     PIC 999V99         VALUE "‚ ‚¢‚¤".
       01  N-PACK         PIC 9999   COMP-3  VALUE "‚P‚Q‚R".
       01  N-PACK-DEC     PIC 99V999 COMP-3  VALUE "‚X‚W‚V".
       01  N-BIN          PIC 9999   COMP    VALUE "‚P‚Q‚R".
       01  N-BIN-DEC      PIC 99V999 COMP    VALUE "‚X‚W‚V".
       01  NE-01          PIC --99.999       VALUE "‚X‚W‚V".
       
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-4C)".
      *  ƒP[ƒX9.VALUE‹å‚ÌƒGƒ‰[ƒ`ƒFƒbƒN(‚»‚Ì‚Pj
      *
            MOVE "P-010-01"             TO CASE-ID.
            DISPLAY  G-08.
            DISPLAY  G-09.
            DISPLAY  GE-08.
            DISPLAY  GE-09.
            DISPLAY  ERR-01.
            DISPLAY  ERR-02.
            DISPLAY  ERR-03-1.
      *  ƒP[ƒX10.VALUE‹å‚ÌƒGƒ‰[ƒ`ƒFƒbƒN(‚»‚Ì‚Qj
      *
            MOVE "P-010-02"             TO CASE-ID.
            DISPLAY  A-01.
            DISPLAY  X-01.
            DISPLAY  N-ZONE.
            DISPLAY  N-ZONE-DEC.
            DISPLAY  N-PACK.
            DISPLAY  N-PACK-DEC.
            DISPLAY  N-BIN.
            DISPLAY  N-BIN-DEC.
            DISPLAY  NE-01.
      *
            DISPLAY "TEST END   (EX4-4C)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

