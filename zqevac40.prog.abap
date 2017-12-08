REPORT ZQEVAC40.
*report zqevac40.note:74638
*----------------------------------------------------------------------*
*  Datendefinitionen
*----------------------------------------------------------------------*
* Tabellen
*&You can cancel the usage decision by implementing a customer-specific program. This will allow you to record results.
*&As of release 3.0E, make sure you use the customer exit QEVA0008.
*&Create the program "ZQEVAC40" with transaction SE38.
*&Go to the ABAP/4 Text Elements. You have to type in the text symbols "S01" and "S02".
*&Text symbol "S01": "Find insp.lot"
*&Text symbol "S02": "Show insp.lot"
*&Further you have to type in the selection text of the selection parameter "PRUEFLOS": Text "Inspection Lot".
*----------------------------------------------------------------------*
TABLES sscrfields.
TABLES qals.
TABLES qave.
*----------------------------------------------------------------------*
* Konstanten
CONSTANTS:
  c_rc_0        LIKE sy-subrc           VALUE 0,
  c_rc_4        LIKE sy-subrc           VALUE 4,
  c_rc_20       LIKE sy-subrc           VALUE 20,
  c_kreuz       LIKE qm00-qkz VALUE 'X'.
*----------------------------------------------------------------------*
* Eingabebildschirm
SELECTION-SCREEN SKIP 2.
PARAMETERS prueflos  LIKE qals-prueflos MATCHCODE OBJECT qals MEMORY ID qls .
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK search WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 3(20) text-s01 USER-COMMAND sear.
SELECTION-SCREEN PUSHBUTTON 40(20) text-s02 USER-COMMAND show.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK search.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'SEAR'  OR prueflos IS INITIAL.
    CALL FUNCTION 'QELA_START_SELECTION_OF_LOTS'
      EXPORTING
        i_selid          = ' '
        i_stat_aenderung = 'X'
        i_stat_ero       = 'X'
        i_stat_frei      = 'X'
        i_stat_ve        = ' '
      IMPORTING
        e_prueflos       = prueflos
      EXCEPTIONS
        no_entry         = 1
        no_selected      = 2
        OTHERS           = 3.

  ENDIF.

  IF sscrfields-ucomm EQ 'SHOW'.

    CALL FUNCTION 'QSS1_LOT_SHOW'
      EXPORTING
        i_prueflos = prueflos.

  ENDIF.

  CHECK sscrfields-ucomm EQ 'ONLI'.

* ab hier muß Prüflosnummer gefüllt sein.

  IF prueflos IS INITIAL.

    MESSAGE e164(qa).

  ENDIF.

* Lesen Los

  CALL FUNCTION 'ENQUEUE_EQQALS1'
    EXPORTING
      prueflos = prueflos.

  CALL FUNCTION 'QPSE_LOT_READ'
    EXPORTING
      i_prueflos = prueflos
    IMPORTING
      e_qals     = qals
    EXCEPTIONS
      no_lot     = 1.

  IF NOT sy-subrc IS INITIAL.

    MESSAGE e102(qa).


  ENDIF.
*-----------------

* Prüfen Status

  CALL FUNCTION 'QAST_STATUS_CHECK'
    EXPORTING
      i_objnr          = qals-objnr
      i_status         = 'I0218' "Status VE getroffen
    EXCEPTIONS
      status_not_activ = 1.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e102(qv) WITH qals-prueflos.
  ENDIF.

*

  CALL FUNCTION 'QEVA_UD_READ'
    EXPORTING
      i_prueflos = qals-prueflos
    IMPORTING
      e_qave     = qave.
*---------------------------------------------------------------------*

START-OF-SELECTION.

* Vorgaben sind ok.   1. Material Umlagern und Los ändern

  PERFORM qals_aendern.

************************************************************************

*----------------------------------------------------------------------*

*       FORM QALS_aendern

*----------------------------------------------------------------------*

FORM qals_aendern.

*

  PERFORM status_fix_setzen USING 'I0002' c_kreuz.
  PERFORM status_fix_setzen USING 'I0216' space.
  PERFORM status_fix_setzen USING 'I0217' space.
  PERFORM status_fix_setzen USING 'I0218' space.

  CLEAR: qals-stat14.

  CLEAR: qals-stat35.

  CLEAR: qave-vauswahlmg,

       qave-vwerks,
       qave-versionam,
       qave-vcodegrp,
       qave-vcode,
       qave-vbewertung,
       qave-versioncd,
       qave-vfolgeakti,
       qave-qkennzahl.
*--... verbuchen
  CALL FUNCTION 'QEVA_UD_UPDATE' IN UPDATE
    TASK
    EXPORTING
      qals_new = qals
      qave_new = qave.
  COMMIT WORK.
  MESSAGE s101(qa) WITH qals-prueflos.
ENDFORM.

*---------------------------------------------------------------------*
*       Form  STATUS_FIX_SETZEN
*----------------------------------------------------------------------*

*   Setzen eines Status aufgrund von Voreinstellungen wie QMAT etc.    *

*---------------------------------------------------------------------*

*  -->  STATUS    Status der gesetzt werden soll

*  -->  AKTIV     Status wird aktiviert sonst deaktiviert

*----------------------------------------------------------------------*

FORM status_fix_setzen USING
            VALUE(status) LIKE tj02-istat
            VALUE(aktiv) LIKE c_kreuz.

* lokale Tabelle fuer Statusfortschreibung

  DATA: BEGIN OF l_stattab OCCURS 0.
          INCLUDE STRUCTURE jstat.
  DATA  END OF l_stattab.

* Falls Objektnr. nicht gefüllt. --> Fehlermeldung !!!



  IF qals-objnr EQ space.
    MESSAGE e013(qv).
*   Fehlende Objektnr.: Problem fü
  ENDIF.

  MOVE status TO l_stattab-stat.

  IF aktiv EQ space.
    MOVE c_kreuz TO l_stattab-inact.
  ENDIF.

  APPEND l_stattab.

  CALL FUNCTION 'STATUS_CHANGE_INTERN'
    EXPORTING
      check_only = space
      objnr      = qals-objnr
    TABLES
      status     = l_stattab.


ENDFORM.                               " STATUS_FIX_SETZEN
