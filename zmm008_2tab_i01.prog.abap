
*&SPWIZARD: INPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TAB_CONTR_MODIFY INPUT.
  MODIFY IT_ZMM002_2
    FROM WA_ZMM002_2
    INDEX TAB_CONTR-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_CONTR_MARK INPUT.
  DATA: G_TAB_CONTR_WA2 LIKE LINE OF IT_ZMM002_2.
  IF TAB_CONTR-LINE_SEL_MODE = 1
  AND WA_ZMM002_2-ZBOX = 'X'.
    LOOP AT IT_ZMM002_2 INTO G_TAB_CONTR_WA2
      WHERE ZBOX = 'X'.
      G_TAB_CONTR_WA2-ZBOX = ''.
      MODIFY IT_ZMM002_2
        FROM G_TAB_CONTR_WA2
        TRANSPORTING ZBOX.
    ENDLOOP.
  ENDIF.
  MODIFY IT_ZMM002_2
    FROM WA_ZMM002_2
    INDEX TAB_CONTR-CURRENT_LINE
    TRANSPORTING ZBOX.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_CONTR_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_CONTR'
                              'IT_ZMM002_2'
                              'ZBOX'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LGORT INPUT.
  IF WA_ZMM002_2-UMLGO <> ''.
    SELECT COUNT(*) FROM T001L WHERE LGORT = WA_ZMM002_2-UMLGO
                        AND   WERKS = WA_ZMM002_2-WERKS.
    IF SY-SUBRC <> 0.
      MESSAGE E019 WITH   WA_ZMM002_2-WERKS
                             '目标库位' WA_ZMM002_2-UMLGO.
    ENDIF.
  ENDIF.
  IF WA_ZMM002_2-LGORT <> ''.
    SELECT COUNT(*) FROM T001L WHERE LGORT = WA_ZMM002_2-LGORT
                        AND   WERKS = WA_ZMM002_2-WERKS.
    IF SY-SUBRC <> 0.
      MESSAGE E019 WITH   WA_ZMM002_2-WERKS
                             '来源库位' WA_ZMM002_2-LGORT.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_AUTO_OUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_AUTO_OUT INPUT.
  SELECT SINGLE MAKTX INTO WA_ZMM002_2-MAKTX
    FROM MAKT
    WHERE SPRAS = SY-LANGU
    AND MATNR = WA_ZMM002_2-MATNR
    .
  SELECT SINGLE MEINS INTO WA_ZMM002_2-MEINS
    FROM MARA
    WHERE MATNR = WA_ZMM002_2-MATNR.

  WA_ZMM002_2-XCHPF = ''.
  SELECT SINGLE XCHPF INTO WA_ZMM002_2-XCHPF
    FROM MARC
    WHERE MATNR = WA_ZMM002_2-MATNR
    AND   WERKS = WA_ZMM002_2-WERKS.

* DELETE BY HANDWY 2015-10-26
*    SELECT COUNT( * ) FROM mvke
*      WHERE MATNR = wa_zmm002_2-MATNR
*      AND   VKORG = wa_zmm002_2-WERKS
*      AND   MTPOS = 'Z003'.
*      IF sy-subrc = 0.
*        wa_zmm002_2-SPCID = 'E'.
*       ELSE.
*         wa_zmm002_2-SPCID = ''.
*      ENDIF.
*ENDADD.
  WA_ZMM002_2-BWART = '311'. "2015.4.8修改为默认311 调库
  WA_ZMM002_2-ZUSER = SY-UNAME.
  WA_ZMM002_2-ZDATUM = SY-DATUM.
ENDMODULE.
