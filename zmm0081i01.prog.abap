*&---------------------------------------------------------------------*
*&  包含                ZMM0081I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       屏幕通用USER_COMMAND
*----------------------------------------------------------------------*

MODULE USER_COMMAND INPUT.
  SAVE_OK = OK_CODE.
  PERFORM FRM_USER_COMMOND.
  CLEAR OK_CODE.

ENDMODULE. " USER_COMMAND_9000 INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE USER_COMMAND_9000 INPUT.
  CASE SAVE_OK.
    WHEN  '&CALCULATE'.
    WHEN '&SAVE_DATE'.
    WHEN OTHERS.
  ENDCASE.

  CLEAR SAVE_OK.

*CALL METHOD g_alv_1->check_changed_data
**  IMPORTING
**    e_valid   =
**  CHANGING
**    c_refresh = 'X'

  .

ENDMODULE. " USER_COMMAND_9000 INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_STOCK'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TAB_STOCK_MODIFY INPUT.
  MODIFY IT_SEL_STOCK
    FROM WA_SEL_STOCK
    INDEX TAB_STOCK-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_STOCK'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_STOCK_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_STOCK'
                              'IT_SEL_STOCK'
                              ' '
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  DATA: L_NUMTEMP TYPE KWMENG. "临时用于统计调拨单创建数量
  DATA: L_ERROR TYPE C.
  DATA:L_NUM TYPE KWMENG.
  DATA:LT_DATA3 LIKE TABLE OF IT_DATA3 WITH HEADER LINE.

  CASE SY-UCOMM.
    WHEN  '&CANCLE'.
      LEAVE TO SCREEN 0.
    WHEN  '&ENTER'.

      CLEAR: L_NUM,LT_DATA3,LT_DATA3[],L_ERROR.

      LOOP AT IT_SEL_STOCK INTO WA_SEL_STOCK WHERE LABST > 0.
        IF WA_SEL_STOCK-LGORTO = ''."填入过账数量的库位不能为空
          CLEAR LT_DATA3.
          MESSAGE S015 DISPLAY LIKE 'E'.
          L_ERROR = 'X'.
          EXIT.
        ENDIF.

        IF WA_SEL_STOCK-LABST > WA_SEL_STOCK-KALAB.
          CLEAR LT_DATA3.
          MESSAGE S014 DISPLAY LIKE 'E'."DISPLAY LIKE 'E'.
          L_ERROR = 'X'.
          EXIT.
        ENDIF.
        L_NUM = L_NUM + WA_SEL_STOCK-LABST.
        CLEAR LT_DATA3.
        LT_DATA3 = IT_DATA1.
        LT_DATA3-VGBEL  = WA_SEL_STOCK-VBELN.
        LT_DATA3-VGPOS  = WA_SEL_STOCK-POSNR.
        LT_DATA3-SLOCFR = WA_SEL_STOCK-LGORT.
        LT_DATA3-STOCTO = WA_SEL_STOCK-LGORTO.
        LT_DATA3-CHARG  = WA_SEL_STOCK-CHARG.
        LT_DATA3-QTYBDB = WA_SEL_STOCK-LABST.
        LT_DATA3-ZDY    = SY-UNAME.
        LT_DATA3-ZDDT   = SY-DATUM.
        APPEND LT_DATA3.
      ENDLOOP.

      CHECK L_ERROR = ''.
      IF L_NUM > G_DBD_MARGIN.
        CLEAR: LT_DATA3,LT_DATA3[].
        CLEAR L_NUM.
        MESSAGE S004 DISPLAY LIKE 'E'.
      ELSE.
        DELETE IT_DATA3 WHERE VBELN  = IT_DATA1-VBELN
                          AND  EBELN = IT_DATA1-EBELN
                          AND  POSNR = IT_DATA1-POSNR
                          AND  EBELP = IT_DATA1-EBELP
                          AND  MATNR = IT_DATA1-MATNR
                          .
        APPEND LINES OF LT_DATA3 TO IT_DATA3.
        CLEAR:LT_DATA3, LT_DATA3[].
        CLEAR L_NUM.
        LEAVE TO SCREEN 0.
      ENDIF.
*  EDDADD.

      CHECK L_NUM <= G_DBD_MARGIN .
    WHEN OTHERS.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LGORT INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  IF SY-UCOMM = 'FC_OK' OR SY-UCOMM = 'FC_CANCEL'.
    IF SY-UCOMM = 'FC_CANCEL'.
      GV_CANCEL = 'X'.
    ENDIF.
    CLEAR SY-UCOMM.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
