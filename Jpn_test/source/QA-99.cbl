001400 IDENTIFICATION    DIVISION.
001500 PROGRAM-ID.       QA-99.
001600*
001700 ENVIRONMENT       DIVISION.
001800 CONFIGURATION     SECTION.
001900 OBJECT-COMPUTER.  PC.
002300 INPUT-OUTPUT      SECTION.
002400 FILE-CONTROL.
003300*
003400 DATA              DIVISION.
005400*
005500 WORKING-STORAGE        SECTION.
000080 01  MONITOR-WORK.
000200     03  M-MNT-DATA               PIC  N(52).
       01  RS2-RECORD.
          03  RS2-MNT-DATA              PIC  N(52).
005600******************************************
005700*   スタンダード　コーディング　ワーク   *
005800******************************************
006400*
017500*
017600 PROCEDURE         DIVISION.
017700**************
017800*  ﾚ ﾍﾞ ﾙ 1  *
017900**************
018000 100-RTN.
000530     MOVE      M-MNT-DATA         TO        RS2-MNT-DATA.
058100*
