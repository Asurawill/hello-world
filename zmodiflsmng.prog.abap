*&---------------------------------------------------------------------*
*& Report  ZMODIFLSMNG
*& from SAP Note 1905684
*&---------------------------------------------------------------------*
*& Report is able to edit the Schedule line quantity (LSMNG) in order
*& to overcome dumps, occuring because the total of all LSMNG would
*& exceed the maximum of 9999999999.999
*&
*& How to use:
*& Execute for a PO (or range of POs) and double click the line you
*& wish to change. If you confirm the change in the popup, the
*& change is written to the database immediately
*& In the popup, the quantity to be entered in the internal format:
*& Decimal separation is always the DOT (.), no separators for thousands.
*&
*& Please try this report on your development/test system first to verify
*& that it will work before using it on your production system.
*&---------------------------------------------------------------------*
REPORT ZMODIFLSMNG .

TABLES: EKBE, MSEG.

DATA: BEGIN OF XEKBE OCCURS 0.
        INCLUDE STRUCTURE EKBE.
DATA: END OF XEKBE.

SELECT-OPTIONS: EBELN FOR EKBE-EBELN OBLIGATORY,
                EBELP FOR EKBE-EBELP.

START-OF-SELECTION.
  PERFORM LIST_OUT.

AT LINE-SELECTION.
  DATA: L_ANSWER,
        L_VALUE(18) TYPE C.
  READ TABLE XEKBE WITH KEY EBELN = XEKBE-EBELN
                            EBELP = XEKBE-EBELP
                            ZEKKN = XEKBE-ZEKKN
                            VGABE = XEKBE-VGABE
                            GJAHR = XEKBE-GJAHR
                            BELNR = XEKBE-BELNR
                            BUZEI = XEKBE-BUZEI.
  IF SY-SUBRC <> 0.
    CLEAR XEKBE.
    EXIT.
  ENDIF.
  L_VALUE = XEKBE-LSMNG.
  CALL FUNCTION 'POPUP_TO_GET_VALUE'
    EXPORTING
      FIELDNAME           = 'LSMNG'
      TABNAME             = 'EKBE'
      TITEL               = ''
      VALUEIN             = L_VALUE
    IMPORTING
      ANSWER              = L_ANSWER
      VALUEOUT            = L_VALUE.

  CHECK L_ANSWER IS INITIAL.
  XEKBE-LSMNG = L_VALUE.
  UPDATE EKBE FROM XEKBE.
  SELECT SINGLE * FROM MSEG WHERE MBLNR = XEKBE-BELNR
                              AND MJAHR = XEKBE-GJAHR
                              AND ZEILE = XEKBE-BUZEI.
  IF SY-SUBRC = 0 AND '167' CS XEKBE-VGABE.
    MSEG-LSMNG = XEKBE-LSMNG.
    MODIFY MSEG.
  ENDIF.
  COMMIT WORK AND WAIT.
  SY-LSIND = 0.
  PERFORM LIST_OUT.

FORM LIST_OUT.
  SELECT * FROM EKBE INTO TABLE XEKBE
    WHERE EBELN IN EBELN
      AND EBELP IN EBELP.
  SORT XEKBE BY EBELN EBELP GJAHR BELNR BUZEI.
  LOOP AT XEKBE.
    WRITE: / XEKBE-EBELN, XEKBE-EBELP, XEKBE-GJAHR,
             XEKBE-BELNR, XEKBE-BUZEI, XEKBE-MENGE,
             XEKBE-LSMNG, XEKBE-LSMEH.
    HIDE: XEKBE.
  ENDLOOP.
  CLEAR XEKBE.
ENDFORM.
