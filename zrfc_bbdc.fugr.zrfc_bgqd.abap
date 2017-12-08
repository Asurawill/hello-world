FUNCTION ZRFC_BGQD.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D DEFAULT 2110
*"  TABLES
*"      T_ZBGQD STRUCTURE  ZBGQD
*"----------------------------------------------------------------------
*EXPORT P_WERKS TO MEMORY ID 'ZWERKS'.
*
*SUBMIT ZPP014 WITH s_werks-low  = P_WERKS
*AND RETURN .
*
*IMPORT T_ZBGQD[] FROM MEMORY ID 'Z_ZBGQD'.




ENDFUNCTION.
