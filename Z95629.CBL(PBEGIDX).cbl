       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBEGIDX.
       AUTHOR.     Halim Germiyan.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS MODE RANDOM
                             RECORD KEY IDX-KEY
                             STATUS IDX-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID        PIC S9(05)  COMP-3.
              05 IDX-DVZ       PIC S9(03)  COMP.
           03 IDX-NAME         PIC X(30).
           03 IDX-DATE         PIC S9(7)  COMP-3.
           03 IDX-BALLANCE     PIC S9(15) COMP-3.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           03 IDX-ST           PIC 9(2).
              88 IDX-SUCCESS            VALUE 00 97.
              88 IDX-NOTFND             VALUE 23.
      * VERİLERİN İSİM VE SOYİSİM KISIMLARINI AYIRMAK İÇİN KULLANILAN
      * DEĞİŞKENLER
           03 WS-NAME          PIC X(15).
           03 WS-SURNAME       PIC X(15).
           03 WS-INDEX         PIC 9(2).
           03 WS-INDEXJ        PIC 9(2).
           03 WS-NEW-NAME      PIC X(15).
           03 WS-NEW-LNAME     PIC X(15).
           03 WS-FULL-NAME     PIC X(30).
      * YENİ VERİYİ YAZMAK İÇİN KULLANILAN DEĞİŞKENLER
           03 WS-REC.
              05 WS-ID          PIC S9(05)  COMP-3.
              05 WS-DVZ         PIC S9(03)  COMP.
              05 WS-TEMP-NAME   PIC X(30).
              05 WS-DATE        PIC S9(7)  COMP-3.
              05 WS-BALLANCE    PIC S9(15) COMP-3.

       LINKAGE SECTION.
      * ANA PROGRAMDAN GELEN VERİYİ TUTAN DEĞİŞKENLER
       01  WS-SUB-AREA.
           05 WS-SUB-FUNC   PIC X(1).
           05 WS-SUB-ID     PIC 9(5).
           05 WS-SUB-DVZ    PIC 9(3).
           05 WS-SUB-RC     PIC 9(2).
           05 WS-SUB-DESC   PIC X(30).
           05 WS-SUB-DATA   PIC X(60).
      *--------------------
      * ANA PROGRAMDAN GELEN VERİYİ ALARAK ALT PROGRAMI BAŞLATIYORUZ
       PROCEDURE DIVISION USING WS-SUB-AREA.
      * ALT PROGRAMIN ANA FONKSİYONU TÜM İŞLEMLERİ BURDAN YÖNETİYORUZ
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.
      * VSAM DOSYAMIZI AÇIYORUZ
       H100-OPEN-FILES.
           OPEN I-O  IDX-FILE.
           IF (NOT IDX-SUCCESS)
           MOVE IDX-ST TO WS-SUB-RC
           MOVE 'UNABLE TO OPEN IDXFILE: ' TO WS-SUB-DESC
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.
      * GELEN VERİYE GÖRE İŞLEM YAPIYORUZ
       H200-PROCESS.
           EVALUATE TRUE
              WHEN WS-SUB-FUNC = 'U'
                 PERFORM H205-UPDATE-DATA
              WHEN WS-SUB-FUNC = 'R'
                 PERFORM H300-READ-DATA
              WHEN WS-SUB-FUNC = 'W'
                 PERFORM H400-WRITE-DATA
              WHEN WS-SUB-FUNC = 'D'
                 PERFORM H500-DELETE-DATA
              WHEN OTHER
                 MOVE 99 TO WS-SUB-RC
                 MOVE 'UNKNOWN FUNCTION' TO WS-SUB-DESC
                 PERFORM H999-PROGRAM-EXIT
           END-EVALUATE.
       H200-END. EXIT.
      * GELEN VERİYİ UPDATE İŞLEMİ YAPIYORUZ
       H205-UPDATE-DATA.
      * GELEN VERİYİ OKUYORUZ
           PERFORM H300-READ-DATA.
      * GELEN VERİYİ İSİM VE SOYİSİM OLARAK AYIRIYORUZ
           PERFORM H206-PARSE-NAME.
      * YENİ İSİM VE SOYİSİM İÇİN YENİ DEĞERLERİ HAZIRLIYORUZ
           PERFORM H207-WRITE-NEWNAME.
           PERFORM H208-WRITE-NEWLNAME.
      * YENİ İSİM VE SOYİSİM ESKİ İSİM VE SOYİSİM İLE KARŞILAŞTIRIYORUZ
      * EĞER DEĞİŞİKLİK YOKSA HATA VERİYORUZ
           IF (WS-NAME = WS-NEW-NAME AND WS-SURNAME = WS-NEW-LNAME)
                MOVE 'NO CHANGE' TO WS-SUB-DESC
                MOVE IDX-NAME TO WS-SUB-DATA
                MOVE 23 TO WS-SUB-RC
                PERFORM H999-PROGRAM-EXIT
           END-IF.
      * EĞER DEĞİŞİKLİK VARSA İSİM VE SOYİSİMİ VERİMİZE UYGUN BİR
      * ŞEKİLDE BİRLEŞTİRİYORUZ VE TÜM VERİYİ ANA PROGRAMDAN GELEN
      * WS-SUB-DATA DEĞİŞKENİNE ATIYORUZ
           STRING WS-NEW-NAME WS-NEW-LNAME DELIMITED BY SIZE INTO
            WS-FULL-NAME.
           STRING WS-NAME, WS-NEW-NAME, WS-SURNAME, WS-NEW-LNAME
            DELIMITED BY SIZE INTO WS-SUB-DATA.
      * YENİ VERİYİ VSAM DOSYAMIZDA GÜNCELLEME İŞLEMİNİ YAPIYORUZ
           PERFORM H210-UPDATE-VSAM.
      * GÜNCELLEME İŞLEMİ BAŞARILIYSA OK DÖNÜYORUZ
           MOVE 'OK' TO WS-SUB-DESC.
       H205-END. EXIT.

       H206-PARSE-NAME.
      * GELEN VERİYİ İSİM VE SOYİSİM OLARAK AYIRIYORUZ
           MOVE IDX-NAME(1:15) TO WS-NAME.
           MOVE IDX-NAME(16:15) TO WS-SURNAME.
       H206-END. EXIT.

       H207-WRITE-NEWNAME.
      * BURDA 2 İNDEX KULLANMAMIZIN SEBEBİ İLK İNDEX İLE ESKİ İSİMİMİZİ
      * GEZİYORUZ VE BOŞLUK GÖRÜNCE 2. İNDEX İLE YENİ İSİMİMİZE
      * ATAMAYA BAŞLIYORUZ
           MOVE 1 TO WS-INDEX.
           MOVE 1 TO WS-INDEXJ.
           PERFORM UNTIL WS-INDEX > LENGTH OF WS-NAME
              IF WS-NAME(WS-INDEX:1) = SPACES
                 ADD 1 TO WS-INDEX
              ELSE
                 MOVE WS-NAME(WS-INDEX:1) TO WS-NEW-NAME(WS-INDEXJ:1)
                 ADD 1 TO WS-INDEX
                 ADD 1 TO WS-INDEXJ
              END-IF
           END-PERFORM.
      * YENİ İSİMİMİZİN SONUNDA LOW-VALUE VARSA ONLARI BOŞLUK İLE
      * DEĞİŞTİRİYORUZ
           INSPECT WS-NEW-NAME REPLACING ALL LOW-VALUES BY SPACES.
       H207-END. EXIT.

       H208-WRITE-NEWLNAME.
           MOVE 1 TO WS-INDEX.
           MOVE 1 TO WS-INDEXJ.
           PERFORM UNTIL WS-INDEX > LENGTH OF WS-SURNAME
              IF WS-SURNAME(WS-INDEX:1) = SPACES
                 ADD 1 TO WS-INDEX
              ELSE
                 MOVE WS-SURNAME(WS-INDEX:1) TO
                  WS-NEW-LNAME(WS-INDEXJ:1)
                 ADD 1 TO WS-INDEX
                 ADD 1 TO WS-INDEXJ
              END-IF
           END-PERFORM.
           INSPECT WS-NEW-LNAME REPLACING ALL LOW-VALUES BY SPACES.
       H208-END. EXIT.

       H210-UPDATE-VSAM.
      * YENİ VERİYİ IDX-NAME DEĞİŞKENİNE ATIYORUZ
           MOVE WS-FULL-NAME TO IDX-NAME.
      * REWRITE FONKSİYONU İLE VSAM DOSYAMIZI GÜNCELLEME İŞLEMİNİ
      * YAPIYORUZ
           REWRITE IDX-REC
      * EĞER GÜNCELLEME İŞLEMİ BAŞARISIZSA HATA VERİYORUZ
           INVALID KEY
           MOVE 23 TO WS-SUB-RC
           MOVE 'UNABLE TO UPDATE VSAM' TO WS-SUB-DESC
           PERFORM H999-PROGRAM-EXIT
           END-REWRITE.
       H210-END. EXIT.
      * GELEN VERİYİ OKUYORUZ
       H300-READ-DATA.
      * GELEN VERİYİ KEY DEĞİŞKENİNE ATIYORUZ
           MOVE WS-SUB-ID TO IDX-ID.
           MOVE WS-SUB-DVZ TO IDX-DVZ.
           READ IDX-FILE
                KEY IS IDX-KEY
      * EĞER VERİ BULUNAMAZSA HATA VERİYORUZ
                INVALID KEY
                 MOVE 23 TO WS-SUB-RC
                 MOVE 'UNABLE TO READ VSAM' TO WS-SUB-DESC
                 PERFORM H999-PROGRAM-EXIT
           END-READ.
           MOVE 'OK' TO WS-SUB-DESC.
           MOVE IDX-NAME TO WS-SUB-DATA.
       H300-END. EXIT.
      * YENİ VERİ YAZMA İŞLEMİ
       H400-WRITE-DATA.
      * YENİ VERİYİ YAZMAK İÇİN YENİ DEĞERLERİ HAZIRLIYORUZ
           MOVE WS-SUB-ID                        TO WS-ID.
           MOVE WS-SUB-DVZ                       TO WS-DVZ.
           MOVE 'H A L I M      GE RM IY AN    ' TO WS-TEMP-NAME.
           MOVE 2000331                          TO WS-DATE.
           MOVE 100000000000000                  TO WS-BALLANCE.
           MOVE WS-REC TO IDX-REC.
           WRITE IDX-REC
              INVALID KEY
                MOVE 23 TO WS-SUB-RC
                MOVE 'UNABLE TO WRITE VSAM' TO WS-SUB-DESC
                PERFORM H999-PROGRAM-EXIT
           END-WRITE.
           MOVE WS-TEMP-NAME TO WS-SUB-DATA.
           MOVE 'OK' TO WS-SUB-DESC.
       H400-END. EXIT.
      * VERİ SİLME İŞLEMİ
       H500-DELETE-DATA.
      * GELEN VERİYİ KEY DEĞİŞKENİNE ATIYORUZ
           MOVE WS-SUB-ID TO IDX-ID.
           MOVE WS-SUB-DVZ TO IDX-DVZ.
      * EĞER KEY VARSA SİLME İŞLEMİNİ YAPIYORUZ YOKSA HATA VERİYORUZ
           DELETE IDX-FILE
              INVALID KEY
                MOVE 23 TO WS-SUB-RC
                MOVE 'UNABLE TO DELETE VSAM' TO WS-SUB-DESC
                PERFORM H999-PROGRAM-EXIT
           END-DELETE.
           MOVE 'OK' TO WS-SUB-DESC.
       H500-END. EXIT.
      * PROGRAMDA AÇILAN DOSYALARI KAPATIYORUZ VE PROGRAMDAN ÇIKIYORUZ
       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           EXIT PROGRAM.
       H999-END. EXIT.
      *
