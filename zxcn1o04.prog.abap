*----------------------------------------------------------------------*
***INCLUDE ZXCN1O04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0700 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  LOOP AT SCREEN .
    IF G_DISPLAY = 'X'..
      SCREEN-INPUT = '0' .
      MODIFY SCREEN .
    ENDIF.
  ENDLOOP.
ENDMODULE.
