*&---------------------------------------------------------------------*
*&  包含                ZMM045_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
*  DATA: DYNPFIELDS TYPE TABLE OF DYNPREAD WITH HEADER LINE.
*  CLEAR: DYNPFIELDS, DYNPFIELDS[].
*
*  DYNPFIELDS-FIELDNAME = 'P_EKORG1'. "填入需要读值的字段名
*  APPEND DYNPFIELDS.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      DYNAME             = SY-REPID
*      DYNUMB             = SY-DYNNR
*      TRANSLATE_TO_UPPER = 'X'
*    TABLES
*      DYNPFIELDS         = DYNPFIELDS[] "在哪定义的啊  这个好像是系统本来就有的
*    EXCEPTIONS
*      OTHERS             = 9.
*  IF SY-SUBRC = 0.
*    READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'P_EKORG1'.  "你在测试急这里打断点跑下好
*    P_EKORG1 = DYNPFIELDS-FIELDVALUE. "备注
*  ENDIF.

  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BUT1' .
      MYTAB-ACTIVETAB = 'BUT1'.
      G_SUB = '0100'.
    WHEN 'BUT2' .
      MYTAB-ACTIVETAB = 'BUT2'.
      G_SUB = '0200'.
    WHEN 'BACK' OR 'CANCEL' .
      PERFORM FRM_GLOBAL_REFRESH .  "全局参数刷新
      PERFORM FRM_ALV_REFRESH .     "ALV参数刷新
      LEAVE TO SCREEN 0 .
    WHEN 'EXCU' .
      PERFORM FRM_CHECK_EMPTY .
      CALL SCREEN 9002 .
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  LEAVE PROGRAM  .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT2 INPUT.
  LEAVE PROGRAM .
ENDMODULE.
