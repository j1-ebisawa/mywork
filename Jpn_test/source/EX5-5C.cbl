      ******************************************************************
      *    ƒeƒXƒgƒP[ƒXF5-5C
      *    ƒvƒƒOƒ‰ƒ€–¼F“ú–{Œê‰»ƒeƒXƒg iŽè‘±‚«•”jMOVE–½—ß
      *    ˆ—ŠT—v@@F“ú–{Œê“]‹L‚ª³‚µ‚­ŽÀs‚³‚ê‚é‚©‚ðƒ`ƒFƒbƒN‚·‚éB
      *  --------------------------------------------------------------
      *   ƒeƒXƒgƒP[ƒX:‚Q‚P`‚R‚P
      *   @@@@@@@ƒP[ƒX‚Q‚VA‚Q‚W‚ÍƒRƒ“ƒpƒCƒ‹ƒGƒ‰[‚Æ‚µ‚½‚¢B
      *   @@@@@@@Šm”FŒãAƒRƒƒ“ƒg‰»‚µ‚ÄAŽÀsƒeƒXƒg‚·‚éB
      ******************************************************************
       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           EX5-5C.
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
           05  FILLER         REDEFINES L-A.
               10  L-A-5      PIC X(5).
               10  L-A-3      PIC X(3).
               10  L-A-2      PIC X(2).
           05  L-GRP-10       REDEFINES L-A.
               10  L-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  L-AN           PIC X(10).
           05  FILLER         REDEFINES L-AN.
               10  L-AN-5     PIC X(5).
               10  L-AN-3     PIC X(3).
               10  L-AN-2     PIC X(2).
           05  L-ANE          PIC XX/XX/XX.
           05  L-ZONE         PIC 9(5).
           05  L-ZONE-DEC     PIC S9(3)V9(2).
           05  L-PACK         PIC S9(5)       COMP-3.
           05  L-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  L-BIN          PIC S9(5)       COMP.
           05  L-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  L-NE           PIC ----9.
           05  L-G-10         PIC N(10).
           05  FILLER         REDEFINES L-G-10.
               10  L-G-5      PIC N(5).
               10  L-G-3      PIC N(3).
               10  L-G-2      PIC N(2).
           05  L-GE           PIC N/N/N.
           05  FILLER         REDEFINES L-GE.
               10  L-GE-3     PIC N/N.
               10  L-GE-2     PIC /N.
           
       01  W-R-GRP.
           05  R-A            PIC A(10).
           05  R-GRP-10       REDEFINES R-A.
               10  R-GRP-4.
                   15  FILLER PIC X(4).
               10  FILLER     PIC X(6).
           05  R-AN           PIC X(10).
           05  R-ANE          PIC XX/XX/XX.
           05  FILLER         REDEFINES R-ANE.
               10  R-ANE-3    PIC XX/XX/.
               10  R-ANE-2    PIC XX.
           05  R-ZONE         PIC 9(5).
           05  R-ZONE-DEC     PIC S9(3)V9(2).
           05  R-PACK         PIC S9(5)       COMP-3.
           05  R-PACK-DEC     PIC S9(3)V9(2)  COMP-3.
           05  R-BIN          PIC S9(5)       COMP.
           05  R-BIN-DEC      PIC S9(3)V9(2)  COMP.
           05  R-NE           PIC ----9.
           05  R-G            PIC N(10).
           05  FILLER         REDEFINES R-G.
               10  R-G-5      PIC N(5).
               10  R-G-3      PIC N(3).
               10  R-G-2      PIC N(2).
           05  R-GJ           PIC N(10) JUST.
           05  FILLER         REDEFINES R-GJ.
               10  R-GJ-5     PIC N(5)  JUST.
               10  R-GJ-3     PIC N(3)  JUST.
               10  R-GJ-2     PIC N(2)  JUST.
           05  R-GE           PIC N/N/N.
           05  FILLER         REDEFINES R-GE.
               10  R-GE-3     PIC N/N.
               10  R-GE-2     PIC /N.
           05  R-GE-NB        PIC NNN/NBN/.
           05  R-GE-NBZ       PIC NNN/N0BN.
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
       MAIN-00.
            DISPLAY "TEST START (EX5-5C)".
       P-210. 
      *  ƒP[ƒX21.W’c€–Ú¨“ú–{Œê
      *
            MOVE "P-210-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²‚ $&"        TO L-GRP-10.
            MOVE L-GRP-10 TO R-G
            IF  R-G = "ab12±²‚ $&"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-G
            END-IF.
      *
            MOVE "P-210-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²‚ $&"        TO L-GRP-10.
            MOVE L-GRP-10 TO R-G-3
            IF  R-G-3 = "ab12±²"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-210-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12"              TO L-GRP-4.
            MOVE L-GRP-4 TO R-G
            IF R-G = "ab12      "
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-G
            END-IF.
      *
       P-220. 
      *  ƒP[ƒX22.W’c¨“ú–{Œê•ÒW
      *
            MOVE "P-220-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²‚ $&"        TO L-GRP-10.
            MOVE L-GRP-10 TO R-GE
            IF  R-GE = "ab12±²‚ $&"  DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-220-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²‚ $&"        TO L-GRP-10.
            MOVE L-GRP-10 TO R-GE-3
            IF R-GE-3 = "ab12±²"     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-220-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12"              TO L-GRP-4.
            MOVE L-GRP-4 TO R-GE
            IF R-GE = "ab12      "   DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-230. 
      *  ƒP[ƒX23.‰p”ŽšE‰p”Žš•ÒWE”Žš•ÒW¨“ú–{Œê
      *           “ú–{Œê“]‹Li1B->‚QB)‚Æ‚È‚é‚©
            MOVE "P-230-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN TO R-G
            IF  R-G = "‚‚‚‚P‚QƒAƒC•{"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG:" R-G
            END-IF.
      *
            MOVE "P-230-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-5 TO R-G
            IF  R-G = "‚‚‚‚P‚QƒA@@@@@"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-5 TO R-G-3
            IF  R-G-3 = "‚‚‚‚P"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-04"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²"          TO L-ANE.
            MOVE L-ANE TO R-G-3
            IF  R-G-3 = "‚‚‚^"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-05"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE -123               TO L-NE.
            MOVE L-NE TO R-G
            IF  R-G = "@|‚P‚Q‚R@@@@@"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-06"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN TO R-GJ-3
            IF  R-GJ-3 = "•{"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-230-07"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-3 TO R-GJ
            IF  R-GJ = "@@@@@@@ƒC•"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-240. 
      *  ƒP[ƒX24.“ú–{Œê’è”¨“ú–{Œê
      *           “ú–{Œê“]‹Li1B->‚QB)‚Æ‚È‚é‚©
            MOVE "P-240-01"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-3 TO R-GE
            IF  R-GE = "ƒC^^•"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-02"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-5 TO R-GE
            IF  R-GE = "‚^‚‚^‚P"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-03"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-2 TO R-GE
            IF  R-GE = "{^^@"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-04"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²"          TO L-ANE.
            MOVE L-ANE TO R-GE
            IF  R-GE = "‚^‚‚^^"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-05"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE -123               TO L-NE.
            MOVE L-NE TO R-GE
            IF  R-GE = "@^|^‚P"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-06"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN.
            MOVE L-AN-5 TO R-GE-NB
            IF  R-GE-NB = "‚‚‚‚P^‚Q@ƒA^"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-240-07"        TO CASE-ID.
            MOVE ALL "*"             TO W-R-GRP.
            MOVE "ab12±²$&+="        TO L-AN-5.                         *>20111102
            MOVE L-AN-5 TO R-GE-NBZ                                     *>20111102
            IF  R-GE-NBZ = "‚‚‚‚P^‚Q‚O@ƒA"  
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            *>ACCEPT OMIT-WK.
      *
       P-250. 
      *  ƒP[ƒX25.‰pŽšA”Žš(®”jA”Žš’è”i®”j¨“ú–{Œê
      *
            MOVE "P-250-01"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "ABCDEabcde"      TO L-A.
            MOVE L-A TO R-G.
            IF R-G = "‚`‚a‚b‚c‚d‚‚‚‚ƒ‚„‚…" 
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-02"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-ZONE.
            MOVE L-ZONE TO R-G.
            IF R-G = "‚O‚O‚P‚Q‚R@@@@@"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-03"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-PACK.
            MOVE L-PACK TO R-G.
            IF R-G = "‚O‚O‚P‚Q‚R@@@@@"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-04"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-BIN.
            MOVE L-BIN TO R-G.
            IF R-G = "‚O‚O‚P‚Q‚R@@@@@"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-05"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123 TO R-G.
            IF R-G = "‚P‚Q‚R@@@@@@@"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-06"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "ABCDEabcde"      TO L-A.
            MOVE L-A TO R-GJ-5.
            IF R-GJ-5 = "‚‚‚‚ƒ‚„‚…"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-250-07"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "abc"      TO L-A-3.
            MOVE L-A-3 TO R-GJ-5.
            IF R-GJ-5 = "@@‚‚‚‚ƒ"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-260. 
      *  ƒP[ƒX26.‰pŽšA”Žš(®”jA”Žš’è”i®”j¨“ú–{Œê•ÒW
      *
            MOVE "P-260-01"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "abc"      TO L-A-3.
            MOVE L-A-3 TO R-GE.
            IF R-GE = "‚^‚‚^‚ƒ"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-02"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-ZONE.
            MOVE L-ZONE TO R-GE.
            IF R-GE = "‚O^‚O^‚P"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-03"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-PACK.
            MOVE L-PACK TO R-GE.
            IF R-GE = "‚O^‚O^‚P"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-04"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123          TO L-BIN.
            MOVE L-BIN TO R-GE.
            IF R-GE = "‚O^‚O^‚P"
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-05"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE 123 TO R-GE.
            IF R-GE = "‚P^‚Q^‚R"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-06"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "ABCDEabcde"      TO L-A.
            MOVE L-A-5 TO R-GE-NB.
            IF R-GE-NB = "‚`‚a‚b^‚c@‚d^"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-260-07"        TO CASE-ID.
            MOVE ALL "*"           TO W-R-GRP.
            MOVE "ABCDEabcde"      TO L-A.
            MOVE L-A-5 TO R-GE-NBZ.
            IF R-GE-NBZ = "‚`‚a‚b^‚c‚O@‚d"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
       P-270. 
      *  ƒP[ƒX27.”Žši¬”jA”Žš’è”(¬”j¨“ú–{Œê
      *           ƒRƒ“ƒpƒCƒ‹ƒGƒ‰[‚Æ‚È‚é‚©
           MOVE "P-270-01"        TO CASE-ID.
           MOVE 123.456 TO L-ZONE-DEC.
           MOVE 123.456 TO L-PACK-DEC.
           MOVE 123.456 TO L-BIN-DEC.
           
           MOVE L-ZONE-DEC TO R-G.
           MOVE L-PACK-DEC TO R-G.
           MOVE L-BIN-DEC  TO R-G.
           DISPLAY CASE-ID "NG".
            
       P-280. 
      *  ƒP[ƒX28.”Žši¬”jA”Žš’è”(¬”j¨“ú–{Œê•ÒW
      *           ƒRƒ“ƒpƒCƒ‹ƒGƒ‰[‚Æ‚È‚é‚©
           MOVE "P-280-01"        TO CASE-ID.
           MOVE 123.456 TO L-ZONE-DEC.
           MOVE 123.456 TO L-PACK-DEC.
           MOVE 123.456 TO L-BIN-DEC.
           MOVE L-ZONE-DEC TO R-GE.
           MOVE L-PACK-DEC TO R-GE.
           MOVE L-BIN-DEC  TO R-GE.
           DISPLAY CASE-ID "NG".
      *
       P-290. 
      *  ƒP[ƒX29.•\ˆÓ’è”¨“ú–{Œê
      *           “ú–{Œê“]‹L‚Æ‚È‚é‚©
            MOVE "P-290-01"        TO CASE-ID.
            MOVE SPACE TO R-G.
            IF R-G = "@@@@@@@@@@"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-290-02"        TO CASE-ID.
            MOVE QUOTE TO R-G.
      *     IF R-G = ""                             *>20111102
            IF R-G = "hhhhhhhhhh"                             *>20111102
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
            MOVE "P-290-03"        TO CASE-ID.
            MOVE ZERO TO R-G.
            IF R-G = "‚O‚O‚O‚O‚O‚O‚O‚O‚O‚O"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *  ƒP[ƒX30.ALL •¶Žš’è”¨“ú–{Œê
      *           “ú–{Œê“]‹L‚Æ‚È‚é‚©
            MOVE "P-300-01"        TO CASE-ID.
            MOVE ALL "abc" TO R-G.
            IF R-G = "‚‚‚‚ƒ‚‚‚‚ƒ‚‚‚‚ƒ‚"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *  ƒP[ƒX31.ALL •¶Žš’è”¨“ú–{Œê•ÒW
      *           “ú–{Œê“]‹L‚Æ‚È‚é‚©
            MOVE "P-310-01"        TO CASE-ID.
            MOVE ALL"a" TO R-GE.
            IF R-GE = "‚^‚^‚"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-310-02"        TO CASE-ID.
            MOVE ALL"a" TO R-GE-NB.
            IF R-GE-NB = "‚‚‚^‚@‚^"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
            MOVE "P-310-03"        TO CASE-ID.
            MOVE ALL"a" TO R-GE-NBZ.
            IF R-GE-NBZ = "‚‚‚^‚‚O@‚"   
                                     DISPLAY CASE-ID "OK"
               ELSE                  DISPLAY CASE-ID "NG"
            END-IF.
      *
             DISPLAY "TEST END   (EX5-5C)".
            *>ACCEPT OMIT-WK.
            GOBACK
            .

