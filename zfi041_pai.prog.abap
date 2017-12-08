*&---------------------------------------------------------------------*
*&  包含                ZFI041_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BACK' OR 'CANCEL' .
      LEAVE TO SCREEN 0 .
    WHEN 'BUT1' .
      G_BUT1 = 'X' .
      PERFORM FRM_INITIAL_STATE .
    WHEN 'BUT2' .
      G_BUT2 = 'X' .
      PERFORM FMR_STORAGE .
    WHEN 'EXCU' .
      CLEAR: G_BUT1,G_BUT2 .
      PERFORM FRM_SHOW_ALV_BEFORE_POST .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  LEAVE PROGRAM .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.
  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BACK' OR 'CANCEL' .
      LEAVE TO SCREEN 0 .
    WHEN 'EXCU2' .
      PERFORM FRM_SHOW_SCREEN . " 展示明细或汇总数据的选择界面
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003 INPUT.
  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BACK' OR 'CANCEL' .
      LEAVE TO SCREEN 0 .
    WHEN 'EXCU3' .
      PERFORM FRM_SHOW_DETAIL . " 展示明细数据
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9004 INPUT.
  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BACK' OR 'CANCEL' .
      LEAVE TO SCREEN 0 .
    WHEN 'EXCU4' .
      PERFORM FRM_SHOW_COLLECT . " 展示汇总数据
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9005 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'OK'.
      IF G_STGRD IS INITIAL.
        MESSAGE '冲销原因必输！' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        GS_REVERSAL-REASON_REV = G_STGRD.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: GS_REVERSAL-REASON_REV,
             GS_REVERSAL-PSTNG_DATE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
