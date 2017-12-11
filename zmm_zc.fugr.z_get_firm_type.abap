FUNCTION Z_GET_FIRM_TYPE.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(IV_LFA1) TYPE  LFA1
*"----------------------------------------------------------------------


  "LFA1-ZZFIRM_TYPE is the screen field name.
*  IV_ZSFZH = G_LFA1-ZSFZH.

  MOVE-CORRESPONDING G_LFA1 TO IV_LFA1.

ENDFUNCTION.
