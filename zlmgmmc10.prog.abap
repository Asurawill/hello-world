REPORT ZLMGMMC10.
TABLES: MARC, QMAT.

SELECT-OPTIONS: MATERIAL FOR MARC-MATNR,
                WERK FOR MARC-WERKS.
DATA: L_QMAT_TAB LIKE QMAT OCCURS 15,
      L_QMAT_WA LIKE QMAT,
      L_MARC_TAB LIKE MARC OCCURS 1000,
      L_MARC_WA LIKE MARC.

START-OF-SELECTION.
* Selektiert alle aktive Prüfarten von der QMAT
  SELECT    * FROM QMAT INTO TABLE L_QMAT_TAB
         WHERE  WERKS  IN WERK
         AND    MATNR  IN MATERIAL
         AND    AKTIV  = 'X'.
* MARC muß nur einmal gelesen wreden, daher werden doppelte Einträge
* gelöscht.
  DELETE ADJACENT DUPLICATES FROM L_QMAT_TAB COMPARING WERKS MATNR .
* Bearbeiten der Sätze
  LOOP AT L_QMAT_TAB INTO L_QMAT_WA.
* Lesen der MARC Sätze ohne Kennzeichen Prüfdaten. Daten sind egal.
* Nur der Sy-subrc zählt
    SELECT SINGLE * FROM MARC INTO L_MARC_WA
    WHERE  WERKS EQ L_QMAT_WA-WERKS
    AND    MATNR EQ L_QMAT_WA-MATNR
    AND    QMATV EQ ''.
* Gefunden ?
    CHECK SY-SUBRC IS INITIAL.
* Ja !
    MOVE 'X' TO L_MARC_WA-QMATV.
* Sperren der MARC
    CALL FUNCTION 'ENQUEUE_EMMARCE'
         EXPORTING
              MATNR          = L_MARC_WA-MATNR
              WERKS          = L_MARC_WA-WERKS
         EXCEPTIONS
              FOREIGN_LOCK   = 2
              SYSTEM_FAILURE = 3.
* MARC erfolgreich gesperrt
    IF SY-SUBRC IS INITIAL.
* Update des bearbeiteten Satzes
      UPDATE MARC SET QMATV = L_MARC_WA-QMATV
      WHERE MATNR EQ L_MARC_WA-MATNR
      AND WERKS EQ L_MARC_WA-WERKS.
    ENDIF.
    COMMIT WORK.
* Entsperren der MARC
    CALL FUNCTION 'DEQUEUE_EMMARCE'
         EXPORTING
              MATNR = L_MARC_WA-MATNR
              WERKS = L_MARC_WA-WERKS.
  ENDLOOP.
* Init der MARC Tabellen.
  CLEAR L_MARC_WA.
  REFRESH L_MARC_TAB.
* Gegencheck: Gibt es Sätze mit MARC Kennzeichen ohne aktive Prüfart ?
  SELECT        * FROM  MARC INTO TABLE L_MARC_TAB
         WHERE  WERKS  IN WERK
         AND    MATNR  IN MATERIAL
         AND    QMATV  = 'X'.
* Prüfen der QMAT Sätze
  LOOP AT L_MARC_TAB INTO L_MARC_WA.
    SELECT SINGLE * FROM  QMAT
           WHERE  WERKS  EQ L_MARC_WA-WERKS
           AND    MATNR  EQ L_MARC_WA-MATNR
           AND    AKTIV  = 'X'.
* Wenn Sy-subrc nicht null ist, gibt es keine Sätze, obwohl das MARC
* Kennzeichen sitzt
    IF NOT SY-SUBRC IS INITIAL.
* Löschen MARC Kennzeichen
      CLEAR L_MARC_WA-QMATV.
* Update der MARC
      UPDATE MARC SET QMATV = L_MARC_WA-QMATV
      WHERE MATNR EQ L_MARC_WA-MATNR
      AND WERKS EQ L_MARC_WA-WERKS.
    ENDIF.
    COMMIT WORK.
* Entsperren der MARC
    CALL FUNCTION 'DEQUEUE_EMMARCE'
         EXPORTING
              MATNR = L_MARC_WA-MATNR
              WERKS = L_MARC_WA-WERKS.
  ENDLOOP.
