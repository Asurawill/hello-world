FUNCTION ZOA_PROJ_RFC.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_PSPID) TYPE  PS_PSPID OPTIONAL
*"  EXPORTING
*"     VALUE(O_MSG) TYPE  CHAR0256
*"  TABLES
*"      O_PROJXX STRUCTURE  ZRFC_SEL_PROJXX OPTIONAL
*"----------------------------------------------------------------------
DATA: LEN TYPE I .

IF I_PSPID IS NOT INITIAL .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE O_PROJXX
    FROM PROJ WHERE PSPID = I_PSPID.
ELSE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE O_PROJXX
    FROM PROJ .

ENDIF.

 DESCRIBE TABLE O_PROJXX LINES LEN .
 IF LEN = 0 .

 O_MSG = '数据读取失败,请重新更新检索条件' .
 ELSE.
  O_MSG = '据读取成功,请查看O_PROJXX表' .

 ENDIF.


ENDFUNCTION.
