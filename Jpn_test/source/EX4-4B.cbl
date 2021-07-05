      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF4-4B
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iƒf[ƒ^•”jVALUE‹å
      *    ˆ—ŠT—v@@FVALUE‹åŽw’è‚ª³‚µ‚­ŽÀs‚Å‚«‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚Q`‚W
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX4-4B.
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
       78  C-01      VALUE "‚ ‚¢‚¤".
       78  C-02      VALUE "‚P‚Q‚R".
       78  C-03      VALUE "‚P‚Q‚R".
       78  C-04      VALUE "‚`‚a‚b123".
       01  OMIT-WK            PIC X.
       01  CASE-ID            PIC X(10).
       01  G-01      PIC NNN    VALUE C-02.
       01  G-02      PIC NNN    VALUE "‚©‚«‚­".
       01  G-03      PIC NNN    VALUE "‚³‚µ".
       01  G-04      PIC NNN    VALUE "‚½‚¿" JUSTIFIED RIGHT.
       01  G-05      PIC NNN    VALUE SPACE.
       01  G-06      PIC NNN    VALUE ALL "–".
       01  G-07      PIC N(10)  VALUE ALL "‚P‚Q‚R".
      *01  G-08      PIC NNN    VALUE "‚P‚Q‚R‚S‚T".              *>20111019
       01  G-08      PIC NNN    VALUE "‚P‚Q‚R".
       01  G-09      PIC NNN    VALUE "ABC".
       
       01  GE-01      PIC N/N/N    VALUE C-03.
       01  GE-02      PIC N/N/N    VALUE "‚©‚«‚­".
       01  GE-03      PIC N/N/N    VALUE "‚³‚µ".
       01  GE-05      PIC N/N/N    VALUE SPACE.
       01  GE-06      PIC N/N/N    VALUE ALL "–".
       01  GE-07      PIC N(5)/N(5)  VALUE ALL "‚P‚Q‚R".
      *01  GE-08      PIC N/N/N    VALUE "‚P^‚Q^‚R^‚S^‚T".    *>20111019
       01  GE-08      PIC N/N/N    VALUE "‚P‚Q‚R".
       01  GE-09      PIC N/N/N    VALUE "ABC".
       
       01  GRP-DAT-1    VALUE "‚ ‚¢‚¤‚P^‚Q^‚Rabcdef1234567890".
           05  GRP-01.
               10  GRP-G1-1       PIC NNN.
               10  GRP-G1-2       PIC N/N/N.
           05  GRP-02.
               10  GRP-X1-1       PIC X(6).
               10  GRP-X1-2       PIC X(10).
       01  OCC-DAT-1.
           05  OCC-01          OCCURS 5.
               10  OCC-X1       PIC XXX  VALUE "123".
               10  OCC-G1       PIC NNN  VALUE "‚ ‚¢‚¤".
       01  OCV-DAT-1.
           05  OCV-01          OCCURS 1 TO 100 DEPENDING ON W-OCV-CNT .
               10  OCV-X1       PIC XXX  VALUE "123".
               10  OCV-G1       PIC NNN  VALUE "‚ ‚¢‚¤".
       01  W-OCV-CNT            PIC 999.
       01  W-I                  PIC 999.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       P-020. 
            DISPLAY "TEST START (EX4-4B)".
      *  ƒP[ƒX‚Q.“ú–{Œê€–Ú‚Ö‚Ì‰Šú’l(³íŒnj
      *
            MOVE "P-020-01"             TO CASE-ID.
            IF G-01 = "‚P‚Q‚R" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-02"             TO CASE-ID.
            IF G-02 = "‚©‚«‚­"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-03"             TO CASE-ID.
            IF G-03 = "‚³‚µ@"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-04"             TO CASE-ID.
            IF G-04 = "@‚½‚¿"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-05"             TO CASE-ID.
            IF G-05 = "@@@"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-06"             TO CASE-ID.
            IF G-06 = ALL "–"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-020-07"             TO CASE-ID.
            IF G-07 = ALL "‚P‚Q‚R"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-030. 
      *  ƒP[ƒX‚R.“ú–{Œê€–Ú‚Ö‚Ì‰Šú’l(“ÁŽêŒnj
      *
            MOVE "P-030-01"             TO CASE-ID.
            IF G-08 = "‚P‚Q‚R" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-030-02"             TO CASE-ID.
            IF G-09 = "‚`‚a‚b"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG:" G-09
            END-IF.
       P-040. 
      *  ƒP[ƒX‚S.“ú–{Œê•ÒW€–Ú‚Ö‚Ì‰Šú’l(³íŒnj
      *
            MOVE "P-040-01"             TO CASE-ID.
            IF GE-01 = "‚P^‚Q^‚R" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-01
            END-IF.
      *
            MOVE "P-040-02"             TO CASE-ID.
            IF GE-02 = "‚©^‚«^‚­"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-02
            END-IF.
      *
            MOVE "P-040-03"             TO CASE-ID.
            IF GE-03 = "‚³^‚µ^@"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-03
            END-IF.
      *
            MOVE "P-040-05"             TO CASE-ID.
            IF GE-05 = "@^@^@"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-05
            END-IF.
      *
            MOVE "P-040-06"             TO CASE-ID.
            IF GE-06 = "–^–^–"                          *>20111019
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-06
            END-IF.
      *
            MOVE "P-040-07"             TO CASE-ID.
            IF GE-07 = "‚P‚Q‚R‚P‚Q^‚R‚P‚Q‚R‚P"              *>20111019
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-07
            END-IF.
      *
       P-050. 
      *  ƒP[ƒX‚T.“ú–{Œê•ÒW€–Ú‚Ö‚Ì‰Šú’l(“ÁŽêŒnj
      *
            MOVE "P-050-01"             TO CASE-ID.
            IF GE-08 = "‚P^‚Q^‚R" 
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-08
            END-IF.
      *
            MOVE "P-050-02"             TO CASE-ID.
            IF GE-09 = "‚`‚a‚b"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
                                        display GE-09
            END-IF.
      *
       P-060. 
      *  ƒP[ƒX‚U.W’c€–Ú‚Ö‚Ì‰Šú’l
      *
            MOVE "P-060-01"             TO CASE-ID.
            IF GRP-G1-1 = "‚ ‚¢‚¤" AND GRP-G1-2 = "‚P^‚Q^‚R" AND
               GRP-X1-1 = "abcdef" AND GRP-X1-2 = "1234567890"
                                        DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-070. 
      *  ƒP[ƒX‚V.ŒJ‚è•Ô‚µ€–Ú‚Ö‚Ì‰Šú’l
      *
            MOVE "P-070-01"             TO CASE-ID.
            PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 5
               IF OCC-X1(W-I) = "123" AND 
                  OCC-G1(W-I) = "‚ ‚¢‚¤"
                                CONTINUE
               ELSE             EXIT PERFORM
               END-IF                          *>20111019
            END-PERFORM.
            IF W-I > 5                  DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-080. 
      *  ƒP[ƒX‚W.ŒJ‚è•Ô‚µ€–Ú‚Ö‚Ì‰Šú’l
      *
            MOVE "P-080-01"             TO CASE-ID.
            PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 5
               IF OCV-X1(W-I) = "123" AND 
                  OCV-G1(W-I) = "‚ ‚¢‚¤"
                                CONTINUE
               ELSE             EXIT PERFORM
               END-IF                              *>20111019
            END-PERFORM.
            IF W-I > 5                  DISPLAY CASE-ID "OK"
               ELSE                     DISPLAY CASE-ID "NG"
            END-IF.
      *
            DISPLAY "TEST END   (EX4-4B)".
            *>ACCEPT OMIT-WK.
      *
            GOBACK
            .

