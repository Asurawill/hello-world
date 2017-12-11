*&---------------------------------------------------------------------*
*&  包含                ZXMG0U02
*&---------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(WMARA) LIKE  MARA STRUCTURE  MARA
*"     VALUE(WMARC) LIKE  MARC STRUCTURE  MARC
*"     VALUE(WMARD) LIKE  MARD STRUCTURE  MARD
*"     VALUE(WMBEW) LIKE  MBEW STRUCTURE  MBEW
*"     VALUE(WMLGN) LIKE  MLGN STRUCTURE  MLGN
*"     VALUE(WMLGT) LIKE  MLGT STRUCTURE  MLGT
*"     VALUE(WMVKE) LIKE  MVKE STRUCTURE  MVKE
*"     VALUE(WSTAT) LIKE  MGSTAT STRUCTURE  MGSTAT
*"     VALUE(WMFHM) LIKE  MFHM STRUCTURE  MFHM
*"     VALUE(WMPOP) LIKE  MPOP STRUCTURE  MPOP
*"  TABLES
*"      STEXT STRUCTURE  SHORT_DESC
*"      SSTEUERTAB STRUCTURE  MG03STEUER
*"      SSTEUMMTAB STRUCTURE  MG03STEUMM
*"      WMEINH STRUCTURE  SMEINH
*"      SMEAN_ME_TAB STRUCTURE  MEAN
*"  CHANGING
*"     VALUE(CMARA) LIKE  MARU STRUCTURE  MARU
*"  EXCEPTIONS
*"      APPLICATION_ERROR
*"----------------------------------------------------------------------

*物料描述重复性校验增强

*ADD BY HANDWY 2015-8-12
IF SY-TCODE = 'MM01' OR SY-TCODE = 'ZMM001'.
  BREAK HAND.
  DATA LS_STEXT TYPE SHORT_DESC.
  DATA L_MATNR  TYPE MAKT-MATNR.

  CLEAR L_MATNR.

  SELECT SINGLE MATNR FROM MARA
    INTO L_MATNR
    WHERE MATNR = WMARA-MATNR.
  IF SY-SUBRC <> 0.
    LOOP AT STEXT[] INTO LS_STEXT
    WHERE SPRAS = SY-LANGU.

      SELECT SINGLE MATNR FROM MAKT
       INTO L_MATNR
       WHERE MAKTX = LS_STEXT-MAKTX
       AND   SPRAS = SY-LANGU.

      IF SY-SUBRC = 0.
        MESSAGE E024(ZMM01) WITH L_MATNR.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.



*END ADD
