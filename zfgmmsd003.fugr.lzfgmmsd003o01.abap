*----------------------------------------------------------------------*
***INCLUDE LZFGMMSD003O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CLEAR_SN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_sn OUTPUT.
  IF STATUS = 'EULG'.
    CLEAR g_sn.
  ENDIF.
ENDMODULE.