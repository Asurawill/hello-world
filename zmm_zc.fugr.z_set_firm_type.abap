FUNCTION Z_SET_FIRM_TYPE.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_LFA1) TYPE  LFA1 OPTIONAL
*"----------------------------------------------------------------------


  " LFA1-ZZFIRM_TYPE is the screen field name.
*    G_LFA1-ZSFZH = IV_LFA1. "set here

  MOVE-CORRESPONDING IV_LFA1 TO G_LFA1.
ENDFUNCTION.
