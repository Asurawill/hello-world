FUNCTION ZFI_FB01.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      T_ZFIFB01 STRUCTURE  ZFIFB01 OPTIONAL
*"      T_ZFIFB01OUT STRUCTURE  ZFIFB01OUT
*"----------------------------------------------------------------------

EXPORT T_ZFIFB01[]  TO MEMORY ID 'ZFIFB01'.
SUBMIT ZFI029W WITH P_MODE = 'N' AND RETURN.
IMPORT T_ZFIFB01OUT[] FROM  MEMORY ID 'ZFIFB01OUT'.




ENDFUNCTION.
