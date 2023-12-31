//SORTEG02 JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE Z95629.QSAM.SA NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC=0
//SORT0200 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
10003848RU SE N        GERMIYAN       20230601
10002949H AL IM        GERMIYAN       20230601
10001978KAM IL E       GERMIYAN       20230601
10001921KAMI LE        GERMIYAN       20230601
10001943KA MI LE       GERMIYAN       20230601
//SORTOUT  DD DSN=Z95629.QSAM.SA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=60)
//SYSIN    DD *
  SORT FIELDS=(1,7,CH,A)
  OUTREC FIELDS=(1,38,39,8,Y4T,TOJUL=Y4T,15C'0')
//*
//DELET300 EXEC PGM=IEFBR14
//FILE01    DD DSN=&SYSUID..QSAM.SB,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//SORT0400  EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSN=Z95629.QSAM.SA,DISP=SHR
//SORTOUT   DD DSN=&SYSUID..QSAM.SB,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=47)
//SYSIN DD *
  SORT FIELDS=COPY
    OUTREC FIELDS=(1,5,ZD,TO=PD,LENGTH=3,
                   6,3,ZD,TO=BI,LENGTH=2,
                   9,30,
                   39,7,ZD,TO=PD,LENGTH=4,
                   46,15,ZD,TO=PD,LENGTH=8)
