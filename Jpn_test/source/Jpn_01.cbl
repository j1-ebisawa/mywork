       IDENTIFICATION             DIVISION.
       PROGRAM-ID.                Jpn_01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N-01    PIC N(10) VALUE ALL "‚ ".
       01  N-02    PIC N(4)  VALUE "‚¢".
       01  NE-01   PIC NN/NN/NN VALUE "‚`‚`‚a‚a‚b‚b".
       01  LENG    PIC 9(3).
       01  omit-wk pic x.
       PROCEDURE                  DIVISION.
         P-01.
           DISPLAY "TEST START  (JPN_TEST)".
           DISPLAY "X_CONFIG"   UPON ENVIRONMENT-NAME.
           DISPLAY "‚ ‚¢‚¤‚¦‚¨" UPON ENVIRONMENT-VALUE.
           .       
           MOVE SPACE TO N-01.
           DISPLAY "X_CONFIG" UPON ENVIRONMENT-NAME.
           ACCEPT N-01        FROM ENVIRONMENT-VALUE.
           DISPLAY N-01.
           *>go to P-EXIT.
      *
           MOVE FUNCTION LENGTH(N-01) TO LENG.
           DISPLAY LENG ":" N-01.
      *
           MOVE FUNCTION LENGTH(N-02) TO LENG.
           DISPLAY LENG ":" N-02.
      *
           MOVE FUNCTION LENGTH(NE-01) TO LENG.
           DISPLAY LENG ":" NE-01.
        P-02.
           MOVE ALL "‚w" TO N-01.
           MOVE FUNCTION LENGTH(N-01) TO LENG.
           DISPLAY LENG ":" N-01.
      *
           MOVE "‚¤‚¦"   TO N-02.
           MOVE FUNCTION LENGTH(N-02) TO LENG.
           DISPLAY LENG ":" N-02.
      *
           MOVE "‚P‚Q‚R‚S‚T‚U" TO NE-01.
           MOVE FUNCTION LENGTH(NE-01) TO LENG.
           DISPLAY LENG ":" NE-01.
      *
        P-EXIT.
           DISPLAY "TEST END    (JPN_TEST)".
           *>accept omit-wk.
           GOBACK.
