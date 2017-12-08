*&---------------------------------------------------------------------*
*&  包含                ZMM045_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.
  SET TITLEBAR 'STT9001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'STA9002'.
  SET TITLEBAR 'STT9002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_INIT OUTPUT.

  IF GR_CONTAINER1 IS INITIAL .
    PERFORM FRM_CREATE_CONTAINER1.
*  ELSE.
*    PERFORM FRM_REFRESH_ALV1 .
  ENDIF.

* 抬头
  IF GT_POSO1 IS INITIAL AND GT_POPS2 IS INITIAL .
    IF MYTAB-ACTIVETAB = 'BUT1' .
      PERFORM FRM_GET_AND_SET_DATA_FOR_POSO .
    ELSEIF MYTAB-ACTIVETAB = 'BUT2' .
      PERFORM FRM_GET_AND_SET_DATA_FOR_POPS .
    ENDIF.
    PERFORM FRM_ALV_DISPLAY1.
  ELSE.
    PERFORM FRM_REFRESH_ALV1 .
  ENDIF.

* 左边
  IF GR_CONTAINER2 IS INITIAL .
    PERFORM FRM_CREATE_CONTAINER2.
*  ELSE.
*    PERFORM FRM_REFRESH_ALV2 .
  ENDIF.
  IF GT_PO1 IS INITIAL AND GT_PO2 IS INITIAL .
    PERFORM FRM_ALV_DISPLAY2 .
  ELSE .
    PERFORM FRM_REFRESH_ALV2 .
  ENDIF.

* 右边
  IF GR_CONTAINER3 IS INITIAL .
    PERFORM FRM_CREATE_CONTAINER3.
*  ELSE.
*    PERFORM FRM_REFRESH_ALV3 .
  ENDIF.
  IF GT_SO1 IS INITIAL AND GT_PS2 IS INITIAL .
    PERFORM FRM_ALV_DISPLAY3 .
  ELSE .
    PERFORM FRM_REFRESH_ALV3 .
  ENDIF.

ENDMODULE.
