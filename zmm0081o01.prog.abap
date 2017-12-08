*&---------------------------------------------------------------------*
*&  包含                ZMM0081O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STATE_9000'.
*   SET TITLEBAR 'xxx'.

  IF G_INIT9000 IS INITIAL.
    PERFORM FRM_CREATE_ALV9000.
    PERFORM FRM_DISPLAY_ALV9000.
  ELSE.
    PERFORM FRM_REFRESH_ALV USING G_ALV_1.
    PERFORM FRM_REFRESH_ALV USING G_ALV_2.
  ENDIF.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_STOCK'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_STOCK_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_SEL_STOCK LINES TAB_STOCK-LINES.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_STOCK'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TAB_STOCK_GET_LINES OUTPUT.
  G_TAB_STOCK_LINES = SY-LOOPC.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STATE_9001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LGORT_REQUIRED  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LGORT_REQUIRED OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'WA_SEL_STOCK-LGORTO'.
      IF WA_SEL_STOCK-LABST > 0.
        SCREEN-REQUIRED = '1'.
      ELSE.
        SCREEN-REQUIRED = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LGORTO_DROP_DOWN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LGORTO_DROP_DOWN OUTPUT.
  TYPE-POOLS VRM.
  DATA :NAME  TYPE VRM_ID,
        LIST  TYPE VRM_VALUES,
        VALUE LIKE LINE OF LIST.
  NAME = 'WA_SEL_STOCK-LGORTO'. "屏幕上绑定的下拉框也是这个名字。
  REFRESH LIST.
  VALUE-KEY = '3002'.
  VALUE-TEXT = '深圳光电整备仓/北京整备仓'.
  APPEND VALUE TO LIST.

  VALUE-KEY = '3003'.
  VALUE-TEXT = '物流仓'.
  APPEND VALUE TO LIST.

  IF WA_SEL_STOCK-WERKS <> '1000'.
    VALUE-KEY = '3004'.
    VALUE-TEXT = 'TV整备仓'.
    APPEND VALUE TO LIST.

    VALUE-KEY = '3006'.
    VALUE-TEXT = '铁路整备仓'.
    APPEND VALUE TO LIST.
  ENDIF.

  VALUE-KEY = '3012'.
  VALUE-TEXT = '渠道整备仓'.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = LIST.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'STA_9002'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
