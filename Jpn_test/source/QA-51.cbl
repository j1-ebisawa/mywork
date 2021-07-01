       IDENTIFICATION        DIVISION.
      ******************************************************************
       PROGRAM-ID.           QA-51.
       AUTHOR.               LA.
       DATE-WRITTEN.         2011-11-09.
       DATE-COMPILED.        2011-11-09.
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
       01  OMIT-WK           PIC X.
       01  CASE-ID           PIC X(10).
       01  X-01              PIC X(10).
       01  G-01              PIC N(10).
      ******************************************************************
       PROCEDURE             DIVISION.
      ******************************************************************
       MAIN                  SECTION.
      * 
            MOVE "QA51-1"         TO CASE-ID.
            MOVE "AB+C D E"       TO X-01.
            INSPECT X-01 REPLACING ALL SPACES BY "-"
                AFTER "+".
      *      
            IF X-01 = "AB+C-D-E--"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" X-01
            END-IF.
      * 
            MOVE "QA51-2"         TO CASE-ID.
            MOVE "AB+C D E"       TO G-01.
            INSPECT G-01 REPLACING ALL SPACES BY "|"
                AFTER "{".
      *      
            IF G-01 = "‚`‚a{‚b|‚c|‚d||"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      * 
            MOVE "QA51-3"         TO CASE-ID.
            MOVE "AB+C D E"       TO G-01.
            INSPECT G-01 REPLACING ALL "@" BY "|"
                AFTER "{".
      *      
            IF G-01 = "‚`‚a{‚b|‚c|‚d||"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      * 
            MOVE "QA51-4"         TO CASE-ID.
            MOVE "AB+C0D0E"       TO G-01.
            INSPECT G-01 REPLACING ALL ZERO BY "|"
                AFTER "{".
      *      
            IF G-01 = "‚`‚a{‚b|‚c|‚d@@"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      * 
            MOVE "QA51-5"         TO CASE-ID.
            MOVE "AB+C0D0E"       TO G-01.
            INSPECT G-01 REPLACING ALL "‚O" BY "|"
                AFTER "{".
      *      
            IF G-01 = "‚`‚a{‚b|‚c|‚d@@"
                  DISPLAY CASE-ID "OK"
            ELSE  DISPLAY CASE-ID "NG:" G-01
            END-IF.
      *
            *>ACCEPT OMIT-WK.
            GOBACK
            .
      
                  
      
       