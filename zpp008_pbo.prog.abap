*&---------------------------------------------------------------------*
*&  包括                ZPP005_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  包含                ZPP005_PBO
*&---------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STA_9001'.
  SET TITLEBAR 'TIT_9001'.


ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_9001 OUTPUT.
  IF WCL_CONTAINER IS INITIAL.
    PERFORM FRM_CREATE_CONTAINER USING 'CONT_9001' '9001' CHANGING WCL_CONTAINER GCL_ALV.
    PERFORM FRM_EXCLUDE.
    PERFORM FRM_PRE_LAYOUT USING '' SPACE CHANGING GS_LAYOUT.
    PERFORM FRM_PRE_FIELDCAT.
    PERFORM FRM_UPLOAD_EVENT CHANGING GCL_ALV. " 注册事件
    PERFORM FRM_SET_TAB_DISPLAY.

  ELSE.
    PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV.

  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'STA_9002'.
*  SET TITLEBAR 'xxx'.
  SET TITLEBAR 'TIT_9002'.
ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_9002 OUTPUT.

  IF WCL_CONTAINER2 IS INITIAL.
    PERFORM FRM_CREATE_CONTAINER USING 'CONT_9002' '9002' CHANGING WCL_CONTAINER2 GCL_ALV2.
    PERFORM FRM_EXCLUDE.
    PERFORM FRM_PRE_LAYOUT USING '' SPACE CHANGING GS_LAYOUT2.
    PERFORM FRM_PRE_FIELDCAT2.
*    PERFORM frm_upload_event CHANGING gcl_alv2. " 注册事件
    PERFORM FRM_SET_TAB_DISPLAY2.

  ELSE.
    PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV2.

  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_9002  OUTPUT
