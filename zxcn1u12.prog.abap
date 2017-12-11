*&---------------------------------------------------------------------*
*&  包含                ZXCN1U12
*&---------------------------------------------------------------------*
"break it02.
 IF GINCLUDE_PROJ-ZHBM  IS INITIAL.
   GINCLUDE_PROJ-ZHBM = 'CNY'."若货币码为空，就默认为CNY
  ENDIF.
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
MOVE-CORRESPONDING GINCLUDE_PROJ TO CNCI_PROJ_EXP.
