*----------------------------------------------------------------------*
***INCLUDE ZFI008_POV.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BSTYP_VALUELIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BSTYP_VALUELIST INPUT.
  CLEAR: IT_VRMLIST, WA_VRMVALUE.

  G_VRMNAME = 'WA_HEAD-BSTYP'.

  " 填充下拉内表
  WA_VRMVALUE-KEY = '1'.
  WA_VRMVALUE-TEXT = '开金税发票'.
  APPEND WA_VRMVALUE TO IT_VRMLIST.
  WA_VRMVALUE-KEY = '2'.
  WA_VRMVALUE-TEXT = '开票（无调整）'."IT02 modify 150623 repalce "开金税发票后调整金额" with "开票（无调整）"
  APPEND WA_VRMVALUE TO IT_VRMLIST.

*add by handwy 2014-04-15
  WA_VRMVALUE-KEY = '3'.
  WA_VRMVALUE-TEXT = '国际业务开票'.
  APPEND WA_VRMVALUE TO IT_VRMLIST.

*add by IT02 2015-06-23
  WA_VRMVALUE-KEY = '4'.
  WA_VRMVALUE-TEXT = '开票（只有调整）'.
  APPEND WA_VRMVALUE TO IT_VRMLIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = G_VRMNAME
      VALUES = IT_VRMLIST.
ENDMODULE.
