FUNCTION ZFM_SD_SAVE_SO_DATA.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN OPTIONAL
*"----------------------------------------------------------------------
DATA: LV_VKORG TYPE VKORG.
SELECT SINGLE VKORG INTO LV_VKORG
  FROM VBAK
  WHERE VBELN = IV_VBELN.
CHECK SY-SUBRC = 0.
EXPORT G_BACK = 'X' TO MEMORY ID 'ZSD010'.
SUBMIT ZSD010 WITH s_vkorg EQ LV_VKORG
              WITH s_vbeln EQ IV_VBELN.
ENDFUNCTION.
