      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF4-4A
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jVALUE‹å
      *    ˆ—ŠT—v@@FVALUE‹åŽw’è‚ª³‚µ‚­ŽÀs‚Å‚«‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚P
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-4A.
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
       78  C-01      VALUE "‚ ‚¢‚¤".
       78  C-02      VALUE "‚P‚Q‚R".
       78  C-03      VALUE "‚P^‚Q^‚R".
       78  C-04      VALUE "‚`‚a‚b123".

       01  G-01      PIC NNN    VALUE C-02.
       01  GE-01     PIC N/N/N  VALUE "‚`^‚a^‚b".
       01  X-03      PIC X(9).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-010. 
            DISPLAY "TEST START (EX4-4A)".
      *  ƒP[ƒX1.78’è”–¼Žw’è‚ÌVALUE‹å
      *
            MOVE "P-010-01"             TO CASE-ID.
            MOVE C-01 TO G-01.
            IF G-01 = "‚ ‚¢‚¤" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-02"             TO CASE-ID.
            MOVE C-02               TO GE-01.
            IF GE-01 = C-03
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-010-03"             TO CASE-ID.
            MOVE C-04 TO X-03.
            IF X-03 = "‚`‚a‚b123"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX4-4A)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

