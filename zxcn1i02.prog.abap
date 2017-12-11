*----------------------------------------------------------------------*
***INCLUDE ZXCN1I02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GINCLUDE_PROJ-ZKUNNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GINCLUDE_PROJ-ZKHBM INPUT.

IF GINCLUDE_PROJ-ZKHBM NE ''.
  CLEAR :GIN_ZKHBMNAME .
  SELECT SINGLE NAME1 INTO GIN_ZKHBMNAME FROM KNA1 WHERE KUNNR = GINCLUDE_PROJ-ZKHBM .
  IF GIN_ZKHBMNAME EQ '' .
    CONCATENATE '客户编码：'  GINCLUDE_PROJ-ZKHBM '系统不存在，请重新输入!' INTO GIN_ZKHBMNAME .
    CLEAR GINCLUDE_PROJ-ZKHBM .
    MESSAGE '请输入系统中系统已存在的客户' TYPE 'S' DISPLAY LIKE 'E'.

 "   SET CURSOR FIELD GINCLUDE_PROJ-ZKUNNR  LINE GIN_CURSOR .
  ENDIF.
ENDIF.
ENDMODULE.
