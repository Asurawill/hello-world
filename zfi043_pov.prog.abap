*&---------------------------------------------------------------------*
*&  包含                ZFI043_POV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZYWLX_VALUELIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZYWLX_VALUELIST INPUT.
  CLEAR: GT_VRMLIST, GS_VRMVALUE.

  G_VRMNAME = 'GS_HEAD-ZYWLX'.

  " 填充下拉内表
  GS_VRMVALUE-KEY = '1'.
  GS_VRMVALUE-TEXT = '产品销售开票'.
  APPEND GS_VRMVALUE TO GT_VRMLIST.
*  GS_VRMVALUE-KEY = '2'.
*  GS_VRMVALUE-TEXT = '开票（无调整）'."IT02 modify 150623 repalce "开金税发票后调整金额" with "开票（无调整）"
*  APPEND GS_VRMVALUE TO GT_VRMLIST.
*
**add by handwy 2014-04-15
*  GS_VRMVALUE-KEY = '3'.
*  GS_VRMVALUE-TEXT = '国际业务开票'.
*  APPEND GS_VRMVALUE TO GT_VRMLIST.
*
**add by IT02 2015-06-23
*  GS_VRMVALUE-KEY = '4'.
*  GS_VRMVALUE-TEXT = '开票（只有调整）'.
*  APPEND GS_VRMVALUE TO GT_VRMLIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = G_VRMNAME
      VALUES = GT_VRMLIST.
ENDMODULE.
