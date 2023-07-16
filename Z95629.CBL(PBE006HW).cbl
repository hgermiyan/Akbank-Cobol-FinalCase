       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBE006HW.
       AUTHOR.     Halim Germiyan.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS OUT-ST.
           SELECT INP-FILE   ASSIGN TO INPFILE
                             STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-FFUNC           PIC X(6).
           03 OUT-SUB-TYPE        PIC X(1).
           03 OUT-FID             PIC X(5).
           03 OUT-ID              PIC 9(5).
           03 OUT-DVZ             PIC 9(3).
           03 OUT-FRC             PIC X(5).
           03 OUT-RETURN-CODE     PIC 9(2).
           03 OUT-FDESC           PIC X(7).
           03 OUT-DESCRIPTION     PIC X(30).
           03 OUT-FDATA           PIC X(7).
           03 OUT-DATA.
              05 OUT-FNAME-FROM   PIC X(15).
              05 OUT-FNAME-TO     PIC X(15).
              05 OUT-LNAME-FROM   PIC X(15).
              05 OUT-LNAME-TO     PIC X(15).

       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           03 INP-SUB-TYPE        PIC X(1).
           03 INP-ID              PIC 9(5).
           03 INP-DVZ             PIC 9(3).
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           03 WS-PBEGIDX          PIC X(7) VALUE 'PBEGIDX'.
           03 OUT-ST              PIC 9(2).
              88 OUT-SUCCESS            VALUE 00 97.
           03 INP-ST              PIC 9(2).
              88 INP-SUCCESS            VALUE 00 97.
              88 INP-EOF                VALUE 10.
           03 WS-SUB-TYPE         PIC X(1).
              88 WS-SUB-TYPE-VALID      VALUE 'R' 'U' 'W' 'D'.
           03 WS-SUB-AREA.
              05 WS-SUB-FUNC      PIC X(1).
                 88 WS-FUNC-READ        VALUE 'R'.
                 88 WS-FUNC-UPDATE      VALUE 'U'.
                 88 WS-FUNC-WRITE       VALUE 'W'.
                 88 WS-FUNC-DELETE      VALUE 'D'.
              05 WS-SUB-ID        PIC 9(5).
              05 WS-SUB-DVZ       PIC 9(3).
              05 WS-SUB-RC        PIC 9(2).
              05 WS-SUB-DESC      PIC X(30).
              05 WS-SUB-DATA      PIC X(60).
      *--------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF (NOT OUT-SUCCESS)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' OUT-ST
           MOVE OUT-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT INP-SUCCESS)
           DISPLAY 'UNABLE TO READ INPFILE: ' INP-ST
           MOVE INP-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
      * INPUT DOSYASINDAN GELEN KAYITLARIN HER BIRI ICIN
      * PBEGIDX ALT PROGRAMINI CAGIRIR VE SONUCU OUTFILE'A YAZAR.
           READ INP-FILE
             AT END SET INP-EOF TO TRUE
           END-READ.
           IF INP-EOF
               PERFORM H999-PROGRAM-EXIT
           END-IF .
           MOVE INP-SUB-TYPE TO WS-SUB-TYPE.
           MOVE INP-ID TO WS-SUB-ID.
           MOVE INP-DVZ TO WS-SUB-DVZ.
           MOVE ZEROS TO WS-SUB-RC.
           MOVE SPACES TO WS-SUB-DESC.
           MOVE SPACES TO WS-SUB-DATA.
           EVALUATE WS-SUB-TYPE
              WHEN 'R'
                 SET WS-FUNC-READ TO TRUE
              WHEN 'U'
                 SET WS-FUNC-UPDATE TO TRUE
              WHEN 'W'
                 SET WS-FUNC-WRITE TO TRUE
              WHEN 'D'
                 SET WS-FUNC-DELETE TO TRUE
              WHEN OTHER
                 MOVE 99 TO WS-SUB-RC
                 MOVE 'INVALID SUB-TYPE: ' TO WS-SUB-DESC
                 PERFORM H300-PROCESS-OUTPUT
                 PERFORM H999-PROGRAM-EXIT
           END-EVALUATE.
           CALL WS-PBEGIDX USING WS-SUB-AREA.
           PERFORM H300-PROCESS-OUTPUT.
       H200-END. EXIT.

       H300-PROCESS-OUTPUT.
           MOVE WS-SUB-TYPE TO OUT-SUB-TYPE.
           MOVE WS-SUB-ID   TO OUT-ID.
           MOVE WS-SUB-DVZ  TO OUT-DVZ.
           MOVE WS-SUB-RC   TO OUT-RETURN-CODE.
           MOVE WS-SUB-DESC TO OUT-DESCRIPTION.
           IF (WS-SUB-DATA NOT = SPACES)
              MOVE WS-SUB-DATA TO OUT-DATA
           ELSE
              MOVE 'FUNCTION NOT SUPPORTED' TO OUT-DATA
           END-IF.
      * BURADA OUTFILE'A YAZILACAK KAYITLARIN FORMATI BELIRLENIR.
           MOVE 'FUNC:'   TO OUT-FFUNC.
           MOVE ' ID: '   TO OUT-FID.
           MOVE ' RC: '   TO OUT-FRC.
           MOVE ' DESC: ' TO OUT-FDESC.
           MOVE ' DATA: ' TO OUT-FDATA.
      * EĞER OUTFILE'A YAZILACAK KAYITLARDA LOW-VALUES VARSA
      * ONLARIN YERINE SPACES YAZILIR.
           INSPECT OUT-REC REPLACING ALL LOW-VALUES BY SPACES.
           WRITE OUT-REC.
           IF (NOT OUT-SUCCESS)
             DISPLAY 'UNABLE TO WRITE OUTFILE: ' OUT-ST
             MOVE OUT-ST TO RETURN-CODE
             PERFORM H999-PROGRAM-EXIT
           END-IF.
       H300-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE OUT-FILE.
           CLOSE INP-FILE.
           DISPLAY 'PROGRAM EXIT WITH RETURN-CODE: ' WS-SUB-RC.
           STOP RUN.
       H999-END. EXIT.
      *
