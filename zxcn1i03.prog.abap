*----------------------------------------------------------------------*
***INCLUDE ZXCN1I03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GINCLUDE_PROJ-ZHBM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZHBM_F4 INPUT.
DATA: BEGIN OF GT_TCURT OCCURS 0 .
   DATA: WAERS TYPE WAERS ,
         LTEXT TYPE LTEXT ,
  END OF GT_TCURT.
 SELECT WAERS LTEXT
    INTO CORRESPONDING FIELDS OF TABLE GT_TCURT
    FROM TCURT  WHERE  SPRAS = SY-LANGU.
 CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WAERS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GIN_KUNNRNAME-ZHBM'
      value_org   = 'S'
    TABLES
      value_tab   = GT_TCURT.
ENDMODULE.
