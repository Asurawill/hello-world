
*&SPWIZARD: INPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TAB_CONTR_MODIFY INPUT.
  MODIFY IT_ZMM002_1
    FROM WA_ZMM002_1
    INDEX TAB_CONTR-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_CONTR_MARK INPUT.
  DATA: G_TAB_CONTR_WA2 LIKE LINE OF IT_ZMM002_1.
  IF TAB_CONTR-LINE_SEL_MODE = 1
  AND WA_ZMM002_1-ZBOX = 'X'.
    LOOP AT IT_ZMM002_1 INTO G_TAB_CONTR_WA2
      WHERE ZBOX = 'X'.
      G_TAB_CONTR_WA2-ZBOX = ''.
      MODIFY IT_ZMM002_1
        FROM G_TAB_CONTR_WA2
        TRANSPORTING ZBOX.
    ENDLOOP.
  ENDIF.
  MODIFY IT_ZMM002_1
    FROM WA_ZMM002_1
    INDEX TAB_CONTR-CURRENT_LINE
    TRANSPORTING ZBOX.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_CONTR_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_CONTR'
                              'IT_ZMM002_1'
                              'ZBOX'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZJWUNAME_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZJWUNAME_CHECK INPUT.
  DATA L_UNAME TYPE ZJWYG.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = WA_ZMM002_1-ZJWUNAME
    IMPORTING
      OUTPUT = L_UNAME.
  IF L_UNAME NP '3*'.
    MESSAGE  E000(SV)  WITH '借物员工编号必须为3开头' .
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_VBDESCRIB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_VBDESCRIB INPUT.
  PERFORM FRM_READ_TEXT USING WA_ZMM002_1-VBELN SY-LANGU 'Z001' 'VBBK'
                        CHANGING WA_ZMM002_1-VBDESCRIB.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_AUTO_OUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_AUTO_OUT INPUT.
  SELECT SINGLE MAKTX INTO WA_ZMM002_1-MAKTX
    FROM MAKT
    WHERE SPRAS = SY-LANGU
    AND MATNR = WA_ZMM002_1-MATNR
    .
  SELECT SINGLE MEINS INTO WA_ZMM002_1-MEINS
    FROM MARA
    WHERE MATNR = WA_ZMM002_1-MATNR.
  SELECT SINGLE NAME1 INTO WA_ZMM002_1-ZJWUNAMEMC
    FROM LFA1                             "IT02 add 150710
    WHERE  LIFNR = WA_ZMM002_1-ZJWUNAME .
  WA_ZMM002_1-XCHPF = ''.
  SELECT SINGLE XCHPF INTO WA_ZMM002_1-XCHPF
    FROM MARC
    WHERE MATNR = WA_ZMM002_1-MATNR
    AND   WERKS = WA_ZMM002_1-WERKS.

*DELETE BY HANDWY 2015-10-26  不检查销售订单
*  SELECT COUNT( * ) FROM mvke
*    WHERE matnr = wa_zmm002_1-matnr
*    AND   vkorg = wa_zmm002_1-werks
*    AND   mtpos = 'Z003'.
*  IF sy-subrc = 0.
*    wa_zmm002_1-spcid = 'E'.
*  ELSE.
*    wa_zmm002_1-spcid = ''.
*  ENDIF.
*ENDADD

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREAT_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREAT_DATE INPUT.
  WA_ZMM002_1-ZUSER = SY-UNAME.
  WA_ZMM002_1-ZDATUM = SY-DATUM.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_FIELD INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LGORT_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LGORT_CHECK INPUT.
  IF WA_ZMM002_1-LGORT <> ''.
    SELECT COUNT(*) FROM T001L WHERE LGORT = WA_ZMM002_1-LGORT
                        AND   WERKS = WA_ZMM002_1-WERKS.
    IF SY-SUBRC <> 0.
      MESSAGE E019 WITH   WA_ZMM002_1-WERKS
                             '库位' WA_ZMM002_1-LGORT.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VBELN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VBELN INPUT.
  IF WA_ZMM002_1-VBELN <> ''.
    SELECT COUNT(*) FROM VBAK
      WHERE VBELN = WA_ZMM002_1-VBELN.
    IF SY-SUBRC <> 0.
      MESSAGE E019 WITH '订单号：'  WA_ZMM002_1-VBELN.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ADD_LEAD_ZERO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ADD_LEAD_ZERO INPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_ZMM002_1-ZJWUNAME
    IMPORTING
      OUTPUT = WA_ZMM002_1-ZJWUNAME.
ENDMODULE.
