*&---------------------------------------------------------------------*
*&  包含                ZFI043_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.
  SET TITLEBAR 'TIT9001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_INIT OUTPUT.
  IF GR_CONTAINER IS INITIAL.
    PERFORM FRM_CREATE_CONTAINER.
    PERFORM FRM_ALV_DISPLAY.
  ELSE.
    PERFORM FRM_REFRESH_ALV.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'MOD'.
      IF G_EDIT_MOD = GC_EDITABLE.
        SCREEN-INPUT = 1.
      ELSE.
        SCREEN-INPUT = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  DATA L_ALV_EDIT TYPE INT4.
  L_ALV_EDIT = GR_ALVGRID->IS_READY_FOR_INPUT( ).
  IF ( L_ALV_EDIT = 1 AND G_EDIT_MOD NE GC_EDITABLE )
    OR ( L_ALV_EDIT = 0 AND G_EDIT_MOD NE GC_READONLY ).
    L_ALV_EDIT = 1 - L_ALV_EDIT.
    PERFORM FRM_CHANGE_EDIT_MODE USING L_ALV_EDIT.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'STA9002'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
