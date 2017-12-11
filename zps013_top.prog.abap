*&---------------------------------------------------------------------*
*&  包含                ZPS013_TOP
*&---------------------------------------------------------------------*

TABLES: T001,PRPS.

DATA: BEGIN OF GS_ALV ,
        SEL          TYPE C,
        NUM          TYPE I,  "序号
        ZBMMC        TYPE PRPS-ZBMMC, "部门
        ZJBR         TYPE PRPS-ZJBR,  "铁三角
        NAME1        TYPE LFA1-NAME1, "客户名称
        LJHKL        TYPE STRING,     "累计回款率
        ZHTJR        TYPE PRPS-ZHTJR, "合同金额
        HSL          TYPE ZFI005-HSL, "累计收款
        ZMYSK        TYPE STRING,     "账面应收款
        BQYSK        TYPE STRING,     "按合同约定本期应收款
        ZYSRQ        TYPE STRING,     "按约定应开始收款时间
        ZL           TYPE STRING,     "账龄（月）
        ZJZYLX_MONTH TYPE STRING,     "资金占用利息（累计）
        ZJZYLX_DATE  TYPE STRING,     "资金占用利息（每天）
        ZYYKXX       TYPE PRPS-ZYYKXX, "收款条件
        ZZXWT        TYPE PRPS-ZZXWT, "最新进展及问题
        PSPID        TYPE PROJ-PSPID, "项目编号
        POST1        TYPE PROJ-POST1, "项目名称
      END OF GS_ALV .
DATA GT_ALV LIKE TABLE OF GS_ALV .

DATA:GS_LAYOUT        TYPE LVC_S_LAYO ,      "alv的格式
     GS_GRID_SETTINGS TYPE LVC_S_GLAY,
     GS_LVC           TYPE LVC_S_FCAT,
     GT_LVC           TYPE LVC_T_FCAT,
     GT_SORT          TYPE LVC_T_SORT,
     GS_VARIANT       TYPE DISVARIANT,
     GT_EVENTS        TYPE SLIS_T_EVENT  , "保存AVL事件
     GS_EVENT         LIKE LINE OF GT_EVENTS.
DATA:
  GR_GRID TYPE REF TO CL_GUI_ALV_GRID,
  GS_STBL TYPE LVC_S_STBL.

DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  GS_LVC-fieldname = &1.
  GS_LVC-coltext   = &2.
  GS_LVC-scrtext_l = &2.
  GS_LVC-scrtext_m = &2.
  GS_LVC-scrtext_s = &2.
  GS_LVC-reptext   = &2.
  GS_LVC-outputlen = &3.
  IF &4 = 'X'.
    GS_LVC-key = 'X'.
  ENDIF.
  GS_LVC-checkbox = &5.
  GS_LVC-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GS_LVC-hotspot   = &7.
  GS_LVC-ref_field = &9.
  GS_LVC-ref_table = &8.

*IF gw_lvc-fieldname = 'PROJK'.
*   gw_lvc-NO_ZERO = 'X'.
*ENDIF.
  APPEND GS_LVC TO gt_lvc.
  CLEAR GS_LVC.
END-OF-DEFINITION.
