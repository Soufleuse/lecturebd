      * dbpre V 0.4: lecturebd.cob                               20230501-225733
------*-------------------------------------------------------------------------
      ****************************************************************
      * Inspiré de https://www.easysoft.com/blog/visual-cobol.html
      * Visual COBOL -> ODBC -> MySQL example.
      *
      * Retourne les enregistrements de la table Equipe de la BD
      * MySql LigueHockey. Retourne 2 champs de la table.
      *
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. lecturbd.
DBPRE *DATE-WRITTEN. 2023-01-21.
      *
      * Environment division 
       ENVIRONMENT DIVISION.
      *
      * Data division 
       DATA DIVISION.
      *
       FILE SECTION.
       WORKING-STORAGE SECTION.
       COPY PGCTBBATWS.
      *
       01  PGCTB-VERSION                    PIC  X(38) 
HISTOR     VALUE '20230430 1.0 INITIAL RELEASE'.
      *
      *01  T                                PIC  S9(9) COMP.
      *
      * The communication area for the database
      *    EXEC SQL 
      *       INCLUDE SQLCA  
        01  SQLCA.
          05 SQLCA-CID                     USAGE POINTER.
          05 SQLCA-CURSOR-CTRL-GRP.
            10 SQLCA-CURSOR-CTRL OCCURS 65 PIC X.
          05 SQLCA-RESULT-GRP OCCURS 65.
            10 SQLCA-RESULT                USAGE POINTER.
          05 SQLCA-SEQUENCE                PIC 9(08).
          05 SQLCA-COUNT                   PIC 9(08).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-RETURN-CODE             PIC 9(03).
          05 SQLCA-CROWCNT                 PIC X(08).
          05 SQLCA-ROWCNT                  PIC 9(08).
            88 SQLCA-NO-ROW                VALUE 0.
            88 SQLCA-ONE-ROW               VALUE 1.
            88 SQLCA-MORE-THAN-ONE-ROW     VALUE 2 THRU 99999999.
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-HOST                    PIC X(32).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-USER                    PIC X(32).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-PASSWD                  PIC X(32).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-DBNAME                  PIC X(32).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-PORT                    PIC 9(05).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-SOCKET                  PIC X(32).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCA-QUOTE                   PIC X VALUE "'".
          05 SQLCA-COBVERSION              PIC X(20).
          05 SQLCA-CARD                    PIC X(80).
          05 SQLCA-STATEMENT.
            10 SQLCA-STAT-LINE OCCURS 80   PIC X(25).
          05 FILLER                        PIC X VALUE LOW-VALUE.
          05 SQLCODE                       PIC 9(03).
            88 DB-OK                       VALUE 0.
            88 DB-CURSOR-ALREADY-OPEN      VALUE -1.
            88 DB-CURSOR-NOT-OPEN          VALUE -2.
            88 DB-NOT-FOUND                VALUE 100.
DBPRE *    END-EXEC.
       01 HOSTVARS.
           05  idEquipe           pic S9(9) comp-4.
           05  pNomEquipe         pic  x(50).
           05  pVilleHote         pic  x(50).
      *
      *linkage section.
      *
      * Procedure division
       PROCEDURE DIVISION.
      * 
DBPRE      MOVE 1             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *        INCLUDE PGCTBBAT REPLACING 'TTTTNNNB' BY 'lecturbd'.
                                                 
----+-*--1-!--+----2----+----3----+----4----+----5----+----6----+----7-!--+----8
      *
           MOVE 'lecturbd'                  TO PGCTB-PROGRAM-NAME
      *
           CALL "cobmysqlapi_get_cobol_version" USING SQLCA-COBVERSION
           END-CALL
      *
           CALL "read_params"         USING PGCTB-PROGRAM-NAME
                                            SQLCA-HOST
                                            SQLCA-USER
                                            SQLCA-PASSWD
                                            SQLCA-DBNAME
                                            SQLCA-PORT   
                                            SQLCA-SOCKET 
           END-CALL
      *
           INSPECT SQLCA-HOST REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-USER REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-PASSWD REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-DBNAME REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-PORT REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-SOCKET REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-COBVERSION REPLACING ALL LOW-VALUE BY SPACE
      *
      * All cursors are closed at the beginning
           MOVE ALL '0'                     TO SQLCA-CURSOR-CTRL-GRP
      *
           ACCEPT PGCTB-DATE                FROM DATE
           ACCEPT PGCTB-STARTTIME           FROM TIME
      *
           DISPLAY '*******************************************'
                   '*********'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*                ' PGCTB-PROGRAM-NAME(1:1) ' '
                                       PGCTB-PROGRAM-NAME(2:1) ' '
                                       PGCTB-PROGRAM-NAME(3:1) ' '
                                       PGCTB-PROGRAM-NAME(4:1) ' '
                                       PGCTB-PROGRAM-NAME(5:1) ' '
                                       PGCTB-PROGRAM-NAME(6:1) ' '
                                       PGCTB-PROGRAM-NAME(7:1) ' '
                                       PGCTB-PROGRAM-NAME(8:1) ' '
                   '                  *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*       Start..: 20' PGCTB-DATE(1:2) '-' 
                   PGCTB-DATE(3:2) '-' PGCTB-DATE(5:2) ' ' 
                   PGCTB-STARTTIME(1:2) ':' PGCTB-STARTTIME(3:2) ':'
                   PGCTB-STARTTIME(5:2) '    '
                   '           *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '* Version..: ' PGCTB-VERSION
                   '*'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*                       -----              '
                   '        *'
           DISPLAY '*      Gnu-Cobol VERSION ' SQLCA-COBVERSION
                   '      *'
           DISPLAY '*                       -----              '
                   '        *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*******************************************'
                   '*********'
           DISPLAY '*  DBHOST.......: ' SQLCA-HOST ' *'
           DISPLAY '*  DBUSER.......: ' SQLCA-USER ' *'
           DISPLAY '*  DBPASSWD.....: ' SQLCA-PASSWD ' *'
           DISPLAY '*  DBNAME.......: ' SQLCA-DBNAME ' *'
           DISPLAY '*  DBPORT.......: ' SQLCA-PORT 
           '                            *'
           DISPLAY '*  DBSOCKET.....: ' SQLCA-SOCKET ' *'
           DISPLAY '*******************************************'
                   '*********'
      *
      * Initialize the database connection
DBPRE      MOVE 2             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *       INIT DB
DBPRE         CALL "MySQL_init"  USING SQLCA-CID
DBPRE         END-CALL
DBPRE         MOVE RETURN-CODE    TO SQLCODE
DBPRE *    END-EXEC.
           EVALUATE TRUE
           WHEN DB-OK
              CONTINUE
           WHEN DB-NOT-FOUND
              SET DB-OK              TO TRUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE
                                                 
DBPRE      MOVE 3             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *       CONNECT DB
DBPRE         CALL "MySQL_real_connect" USING
DBPRE                                   SQLCA-HOST
DBPRE                                   SQLCA-USER
DBPRE                                   SQLCA-PASSWD
DBPRE                                   SQLCA-DBNAME
DBPRE                                   SQLCA-PORT
DBPRE                                   SQLCA-SOCKET
DBPRE         END-CALL
DBPRE         MOVE RETURN-CODE    TO SQLCODE
DBPRE *    END-EXEC.
           EVALUATE TRUE
           WHEN DB-OK
              CONTINUE
           WHEN DB-NOT-FOUND
              SET DB-OK              TO TRUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE
      *
      * Now execute the user's code
           PERFORM PGCTB-ACTION
      *
      * Any errors?
           PERFORM DB-STATUS
      *
      * Commit the work
DBPRE      MOVE 4             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *       COMMIT
DBPRE         CALL "MySQL_commit"
DBPRE         END-CALL

DBPRE         MOVE RETURN-CODE    TO SQLCODE
DBPRE         IF RETURN-CODE NOT = 0 THEN
DBPRE            PERFORM DB-STATUS
DBPRE         END-IF
DBPRE *    END-EXEC.
      *
      * We're done, now close the database and stop the program
DBPRE      MOVE 5             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *        CLOSE DB
DBPRE          CALL "MySQL_close"

DBPRE          END-CALL

DBPRE          MOVE RETURN-CODE    TO SQLCODE

DBPRE *    END-EXEC.
           PERFORM DB-STATUS
      *
           ACCEPT PGCTB-DATE                FROM DATE
           ACCEPT PGCTB-ENDTIME             FROM TIME
           DISPLAY '*******************************************'
                   '*********'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*       Start..: 20' PGCTB-DATE(1:2) '-' 
                   PGCTB-DATE(3:2) '-' PGCTB-DATE(5:2) ' ' 
                   PGCTB-STARTTIME(1:2) ':' PGCTB-STARTTIME(3:2) ':'
                   PGCTB-STARTTIME(5:2) '    '
                   '           *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*       End....: 20' PGCTB-DATE(1:2) '-' 
                   PGCTB-DATE(3:2) '-' PGCTB-DATE(5:2) ' ' 
                   PGCTB-ENDTIME(1:2) ':' PGCTB-ENDTIME(3:2) ':'
                   PGCTB-ENDTIME(5:2) '    '
                   '           *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*                                          '
                   '        *'
           DISPLAY '*******************************************'
                   '*********'
      *
      * No error, return zero
      *
           MOVE 0                       TO RETURN-CODE
           .
       PGCTB-MAIN-EXIT.    
           STOP RUN.
      /
      *************************************************************************
       PGCTB-STATUS SECTION.
           IF PGCTB-ERROR
              IF PGCTB-ERROR-MESSAGE = SPACES
                 STRING PGCTB-PROGRAM-NAME    DELIMITED BY SIZE
                        ': PGCTB-STATUS-FLD ' DELIMITED BY SIZE
                         PGCTB-STATUS-FLD     DELIMITED BY SIZE
                         ' is set!'         DELIMITED BY SIZE
                                   INTO PGCTB-ERROR-MESSAGE
              END-IF
      *
      * Rollback the work
DBPRE         MOVE 6             TO SQLCA-SEQUENCE
      *       EXEC SQL
      *          ROLLBACK
DBPRE          CALL "MySQL_rollback"
DBPRE          END-CALL
DBPRE          MOVE RETURN-CODE    TO SQLCODE
DBPRE *       END-EXEC.
              MOVE 2                       TO RETURN-CODE
              STOP RUN
           END-IF
           .
       PGCTB-STATUS-EXIT.    
           EXIT.    
      *************************************************************************
       PGCTB-DISPLAY-ERROR SECTION.
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '* E R R O R * E R R O R * E R R O R * E R R'
                   ' O R * E R R O R * E R R O R *'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '***                                        '
                   '                           ***'
           DISPLAY '** ' PGCTB-ERROR-MESSAGE ' **'
           DISPLAY '***                                        '
                   '                           ***'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '* E R R O R * E R R O R * E R R O R * E R R'
                   ' O R * E R R O R * E R R O R *'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '*      D A T A B A S E   W O R K   U N I T '
                   '  R O L L E D    B A C K     *'
           DISPLAY '*******************************************'
                   '******************************'
           .
       PGCTB-DISPLAY-ERROR-EXIT.    
           EXIT.    
      *************************************************************************
       DB-STATUS SECTION.
           IF SQLCODE NOT = 0
              CALL "MySQL_errno" USING PGCTB-ERRNO
              END-CALL
              DISPLAY 'ERRNO: ' PGCTB-ERRNO
              CALL "MySQL_error" USING PGCTB-ERROR-MESSAGE
              END-CALL
              DISPLAY PGCTB-ERROR-MESSAGE
              MOVE SPACES                      TO PGCTB-ERROR-MESSAGE
              STRING  'DB-STATUS: Program '     DELIMITED BY SIZE
                      PGCTB-PROGRAM-NAME         DELIMITED BY SIZE
                      ' SQLCODE='             DELIMITED BY SIZE
                      SQLCODE                  DELIMITED BY SIZE
                      '   SQLCA-SEQUENCE='     DELIMITED BY SIZE
                      SQLCA-SEQUENCE           DELIMITED BY SIZE
                      ' '                      DELIMITED BY SIZE
                                               INTO PGCTB-ERROR-MESSAGE
              PERFORM PGCTB-DISPLAY-ERROR
      *
      * Rollback the work
DBPRE         MOVE 7             TO SQLCA-SEQUENCE
      *       EXEC SQL
      *          ROLLBACK
DBPRE          CALL "MySQL_rollback"
DBPRE          END-CALL
DBPRE          MOVE RETURN-CODE    TO SQLCODE
DBPRE *       END-EXEC.
              MOVE 3                       TO RETURN-CODE
              STOP RUN
           END-IF
           .
       DB-STATUS-EXIT.    
           EXIT.    
DBPRE *    END-EXEC.
      ******************************************************************
      *  P G C T B - A C T I O N   S E C T I O N                       *
      ******************************************************************
       PGCTB-ACTION SECTION. 
      *
           DISPLAY 'In PGCTB-ACTION.'
      *
DBPRE      MOVE 8             TO SQLCA-SEQUENCE
      *    EXEC SQL
DBPRE *       DECLARE MONCURSEUR CURSOR FOR
DBPRE *    END-EXEC.
      *
           EVALUATE TRUE
           WHEN DB-OK
              CONTINUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE.
      *
DBPRE      MOVE 9             TO SQLCA-SEQUENCE
      *    EXEC SQL 
DBPRE *         OPEN MONCURSEUR
DBPRE       IF SQLCA-CURSOR-CTRL (1) = 1
DBPRE          SET DB-CURSOR-ALREADY-OPEN TO TRUE
DBPRE          PERFORM DB-STATUS
DBPRE       END-IF

DBPRE       MOVE 1 TO SQLCA-CURSOR-CTRL (1)
DBPRE       MOVE LOW-VALUES TO SQLCA-STATEMENT
DBPRE       MOVE 'SELECT NoEquipe, NomEquip' TO SQLCA-STAT-LINE (1)
DBPRE       MOVE 'e, Ville FROM Equipe ' TO SQLCA-STAT-LINE (2)
DBPRE       CALL 'MySQL_query' USING SQLCA-STATEMENT
DBPRE       END-CALL
DBPRE       MOVE RETURN-CODE TO SQLCODE
DBPRE       IF DB-OK
DBPRE          CALL 'MySQL_use_result' USING SQLCA-RESULT (1)
DBPRE          END-CALL
DBPRE          IF SQLCA-RESULT (1) = NULL
DBPRE             MOVE 100 TO SQLCODE
DBPRE          ELSE
DBPRE             MOVE 0 TO SQLCODE
DBPRE          END-IF
DBPRE       END-IF
DBPRE       IF DB-OK
DBPRE          CALL 'MySQL_fetch_row' USING SQLCA-RESULT (1)
DBPRE                                          idEquipe,
DBPRE                                          pNomEquipe,
DBPRE                                          pVilleHote
DBPRE          END-CALL
DBPRE          IF SQLCA-RESULT (1) = NULL
DBPRE             MOVE 100 TO SQLCODE
DBPRE          ELSE
DBPRE             MOVE 0 TO SQLCODE
DBPRE          END-IF
DBPRE       END-IF
DBPRE *    END-EXEC
      *
           EVALUATE TRUE
           WHEN DB-OK
              CONTINUE
           WHEN DB-NOT-FOUND
              CONTINUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE.
      *
           DISPLAY "No Equipe=" idEquipe "; Nom Equipe=" pNomEquipe
              "; Ville=" pVilleHote.
      *
           PERFORM UNTIL NOT DB-OK
DBPRE         MOVE 10             TO SQLCA-SEQUENCE
      *       EXEC SQL
DBPRE *       END-EXEC
DBPRE       IF SQLCA-CURSOR-CTRL (1) = 0
DBPRE          SET DB-CURSOR-NOT-OPEN TO TRUE
DBPRE          PERFORM DB-STATUS
DBPRE       END-IF
DBPRE          CALL 'MySQL_fetch_row' USING SQLCA-RESULT (1)
DBPRE                                          idEquipe
DBPRE                                          pNomEquipe
DBPRE                                          pVilleHote
DBPRE          END-CALL
DBPRE          IF SQLCA-RESULT (1) = NULL
DBPRE             MOVE 100 TO SQLCODE
DBPRE          ELSE
DBPRE             MOVE 0 TO SQLCODE
DBPRE          END-IF
              EVALUATE TRUE
              WHEN DB-OK
                  DISPLAY "No Equipe=" idEquipe "; Nom Equipe="
                  pNomEquipe "; Ville=" pVilleHote
              WHEN DB-NOT-FOUND
                 MOVE ZEROES            TO idEquipe
                 MOVE SPACE             TO pNomEquipe
                 MOVE SPACE             TO pVilleHote
              WHEN OTHER
                 PERFORM DB-STATUS
              END-EVALUATE
           END-PERFORM.
      *
DBPRE      MOVE 11             TO SQLCA-SEQUENCE
      *    EXEC SQL
      *       CLOSE MONCURSEUR
DBPRE       IF SQLCA-CURSOR-CTRL (1) = 0
DBPRE          SET DB-CURSOR-NOT-OPEN TO TRUE
DBPRE          PERFORM DB-STATUS
DBPRE       END-IF
DBPRE       MOVE 0 TO SQLCA-CURSOR-CTRL (1)
DBPRE *    END-EXEC.
           EVALUATE TRUE
           WHEN DB-OK
               CONTINUE
           WHEN OTHER
               PERFORM DB-STATUS
           END-EVALUATE.
           DISPLAY '-------------------------------------------'.
      *
       PGCTB-ACTION-EXIT.
           EXIT.
      *    STOP RUN.
