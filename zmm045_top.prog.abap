*&---------------------------------------------------------------------*
*&  包含                ZMM045_TOP
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES: T024E,LFA1,EKKO,EKKN ,
        TVKO,KNA1,VBAK ,
        T001,PROJ,PRPS .
************************************************************************
* Type Declaration
************************************************************************
***& PO+SO 部分
* PO 跟 TAB2 共用一个PO
TYPES: BEGIN OF TY_PO1 ,
         SEL         TYPE C,
         EBELN       TYPE EKPO-EBELN, "采购订单
         EBELP       TYPE EKPO-EBELP, "行项目
         MATNR       TYPE EKPO-MATNR, "物料
         TXZ01       TYPE EKPO-TXZ01, "物料描述
         MENGE_CG    TYPE EKPO-MENGE, "数量
         BRTWR       TYPE EKPO-BRTWR, "金额-含税
         RLWRT       TYPE EKPO-NETWR, "金额-净价
         MENGE_RK    TYPE EKBE-MENGE, "入库数量
         DMBTR       TYPE EKBE-DMBTR, "入库金额-含税
         DMBTR_TAX   TYPE EKBE-DMBTR, "入库金额-净价
         AREWR       TYPE EKBE-AREWR, "已发票校验金额-含税
         AREWR_TAX   TYPE EKBE-AREWR, "已发票校验金额-净价
         WFPJYJE_TAX TYPE EKPO-BRTWR, "未发票校验金额-含税价
         WFPJYJE     TYPE EKPO-BRTWR, "未发票校验金额-净价
         MWSKZ       TYPE EKPO-BRTWR, "税码
       END OF TY_PO1 .
* SO
TYPES: BEGIN OF TY_SO1 ,
         SEL       TYPE C,
         VBELN     TYPE VBAP-VBELN, "销售订单
         POSNR     TYPE VBAP-POSNR, "行项目
         MATNR     TYPE VBAP-MATNR, "物料
         ARKTX     TYPE VBAP-ARKTX, "物料描述
         KWMENG    TYPE VBAP-KWMENG, "销售数量
         NETWR     TYPE VBAP-NETWR, "订单总额净价
         DDZE      TYPE VBAP-NETWR, "订单总额
         LFIMG     TYPE LIPS-LFIMG, "发货数量
         FHJE_TAX  TYPE VBAP-NETWR, "发货金额-含税价
         FHJE      TYPE VBAP-NETWR, "发货金额-净价
         KPJE_TAX  TYPE VBRP-NETWR, "开票金额-含税价
         KPJE      TYPE VBRP-NETWR, "开票金额-净价
         WKPJE_TAX TYPE VBAP-NETWR, "未开票金额-含税价
         WKPJE     TYPE VBAP-NETWR, "未开票金额-净价
*         MWSKZ     TYPE EKPO-MWSKZ, "税码
       END OF TY_SO1 .
* PO+SO
TYPES: BEGIN OF TY_POSO1,
         "PO
         SEL         TYPE C,
         BUKRS       TYPE EKKO-BUKRS, "采购方
         EKGRP       TYPE EKKO-EKGRP, "采购组
         EBELN       TYPE EKKO-EBELN, "采购订单
         MENGE_CG    TYPE EKPO-MENGE, "采购数量
         BRTWR       TYPE EKPO-BRTWR, "订单总价
         RLWRT       TYPE EKPO-NETWR, "订单总净价(需要累计每行)
         MENGE_RK    TYPE EKBE-MENGE, "入库数量
         DMBTR       TYPE EKBE-DMBTR, "入库金额-含税
         DMBTR_TAX   TYPE EKBE-DMBTR, "入库金额-净价
         AREWR       TYPE EKBE-AREWR, "已开票校验金额-含税
         AREWR_TAX   TYPE EKBE-AREWR, "已开票校验金额-净价
         WFPJYJE     TYPE EKPO-BRTWR, "未开票校验金额-含税
         WFPJYJE_TAX TYPE EKPO-BRTWR, "未开票校验金额-净价
         MWSKZ_E     TYPE EKPO-BRTWR, "税码
         "SO
         VKORG       TYPE VBAK-VKORG, "销售方
         VBELN       TYPE VBAK-VBELN, "销售订单
         KWMENG      TYPE VBAP-KWMENG, "销售数量
         NETWR       TYPE VBAK-NETWR, "订单总额净价
         DDZE        TYPE VBAK-NETWR, "订单总额
         LFIMG       TYPE LIPS-LFIMG, "发货数量
         FHJE_TAX    TYPE VBAP-NETWR, "发货金额-含税价
         FHJE        TYPE VBAP-NETWR, "发货金额-净价
         KPJE_TAX    TYPE VBRP-NETWR, "开票金额-含税价
         KPJE        TYPE VBRP-NETWR, "开票金额-净价
         WKPJE_TAX   TYPE VBAP-NETWR, "未开票金额-含税价
         WKPJE       TYPE VBAP-NETWR, "未开票金额-净价
*         MWSKZ_V     TYPE EKPO-MWSKZ, "税码
       END OF TY_POSO1 .

* PO+PS 部分
* PO 跟 TAB1 共用一个

* PS
TYPES: BEGIN OF TY_PS2 ,
         SEL       TYPE C,
         POSID     TYPE PRPS-POSID,
         POSNR     TYPE I,
         MATNR     TYPE RESB-MATNR,
         MAKTX     TYPE MAKT-MAKTX,
         BDMNG     TYPE RESB-BDMNG,
         XXJE_TAX  TYPE CKIS-GPREIS,
         XXJE      TYPE CKIS-GPREIS,
         MENGE     TYPE MSEG-MENGE,
         FHJE      TYPE CKIS-GPREIS,
         FHJE_TAX  TYPE CKIS-GPREIS,
         DMBTR_TAX TYPE BSEG-DMBTR,
         DMBTR     TYPE BSEG-DMBTR,
         WKPJE_TAX TYPE CKIS-GPREIS,
         WKPJE     TYPE CKIS-GPREIS,
       END OF TY_PS2 .
* PO+PS
TYPES: BEGIN OF TY_POPS2,
         "PO
         SEL         TYPE C,
         BUKRS       TYPE EKKO-BUKRS, "采购方
         EKGRP       TYPE EKKO-EKGRP, "采购组
         EBELN       TYPE EKKO-EBELN, "采购订单
         MENGE_CG    TYPE EKPO-MENGE, "采购数量
         BRTWR       TYPE EKPO-BRTWR, "订单总价
         RLWRT       TYPE EKPO-NETWR, "订单总净价(需要累计每行)
         MENGE_RK    TYPE EKBE-MENGE, "入库数量
         DMBTR       TYPE EKBE-DMBTR, "入库金额-净价
         DMBTR_TAX   TYPE EKBE-DMBTR, "入库金额-含税
         AREWR       TYPE EKBE-AREWR, "已开票校验金额-净价
         AREWR_TAX   TYPE EKBE-AREWR, "已开票校验金额-含税
         WFPJYJE     TYPE EKPO-BRTWR, "未开票校验金额-净价
         WFPJYJE_TAX TYPE EKPO-BRTWR, "未开票校验金额-含税
         MWSKZ       TYPE EKPO-BRTWR, "税码
         "SO
         PBUKR       TYPE PRPS-PBUKR, "销售方
         POSID       TYPE PRPS-POSID, "销售订单
         BDMNG       TYPE RESB-BDMNG, "销售数量
         DDZE        TYPE CKIS-GPREIS, "订单总额-净价
         DDZE_TAX    TYPE CKIS-GPREIS, "订单总额-含税
         MENGE       TYPE MSEG-MENGE, "发货数量
         FHJE_TAX    TYPE CKIS-GPREIS, "发货金额-含税
         FHJE        TYPE CKIS-GPREIS, "发货金额-净价
         KPJE_TAX    TYPE BSEG-DMBTR, "开票金额-含税
         KPJE        TYPE BSEG-DMBTR, "开票金额-净价
         WKPJE_TAX   TYPE CKIS-GPREIS, "未开票金额-含税
         WKPJE       TYPE CKIS-GPREIS, "未开票金额-净价
*         MWSKZ_V     TYPE EKPO-MWSKZ, "税码
       END OF TY_POPS2 .

************************************************************************
* Internal Table & WorkArea
************************************************************************
***& PO+SO
* PO 取值 跟 TAB2 共用一个
DATA: BEGIN OF GS_EKON ,
        EBELN TYPE EKKO-EBELN,
*        EBELP      TYPE EKKN-EBELP,
*        ZEKKN      TYPE EKKN-ZEKKN,
        BUKRS TYPE EKKO-BUKRS,
        EKGRP TYPE EKKO-EKGRP,
        RLWRT TYPE EKKO-RLWRT,
*        PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
      END OF GS_EKON .
DATA GT_EKON LIKE TABLE OF GS_EKON .
DATA: BEGIN OF GS_EKPO ,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        TXZ01 TYPE EKPO-TXZ01,
        MENGE TYPE EKPO-MENGE,
        MATNR TYPE EKPO-MATNR,
        MWSKZ TYPE EKPO-MWSKZ,
        BRTWR TYPE EKPO-BRTWR,
        NETWR TYPE EKPO-NETWR,
      END OF GS_EKPO .
DATA GT_EKPO LIKE TABLE OF GS_EKPO .
DATA: BEGIN OF GS_EKBE ,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        ZEKKN TYPE EKBE-ZEKKN,
        MENGE TYPE EKBE-MENGE,
        DMBTR TYPE EKBE-DMBTR,
        AREWR TYPE EKBE-AREWR,
        BEWTP TYPE EKBE-BEWTP,
        SHKZG TYPE EKBE-SHKZG,
      END OF GS_EKBE .
DATA GT_EKBE LIKE TABLE OF GS_EKBE .

* SO 取值
DATA: BEGIN OF GS_VBAK ,
        VBELN TYPE VBAK-VBELN,
        VKORG TYPE VBAK-VKORG,
        NETWR TYPE VBAK-NETWR,
        BSTNK TYPE VBAK-BSTNK,
      END OF GS_VBAK .
DATA GT_VBAK LIKE TABLE OF GS_VBAK .
DATA: BEGIN OF GS_VRP ,
        VBELN_VBRP TYPE VBRP-VBELN,
        POSNR_VBRP TYPE VBRP-POSNR,
        VBELN_VBAP TYPE VBAP-VBELN,
        POSNR_VBAP TYPE VBAP-POSNR,
        MATNR      TYPE VBAP-MATNR,
        ARKTX      TYPE VBAP-ARKTX,
        KWMENG     TYPE VBAP-KWMENG,
        NETWR_VBAP TYPE VBAP-NETWR,
        MWSBP_VBAP TYPE VBAP-MWSBP,
        NETWR_VBRP TYPE VBRP-NETWR,
        MWSBP_VBRP TYPE VBRP-MWSBP,
      END OF GS_VRP .
DATA GT_VRP LIKE TABLE OF GS_VRP .
DATA: BEGIN OF GS_LIUP ,
        VBELN_VBUP TYPE VBUP-VBELN,
        POSNR_VBUP TYPE VBUP-POSNR,
        VBELN_LIPS TYPE LIPS-VBELN,
        POSNR_LIPS TYPE LIPS-POSNR,
        VGBEL      TYPE LIPS-VGBEL,
        VGPOS      TYPE LIPS-VGPOS,
        LFIMG      TYPE LIPS-LFIMG,
      END OF GS_LIUP .
DATA GT_LIUP LIKE TABLE OF GS_LIUP .

***& PO+PS
* PO 取值 跟 TAB1 共用一个

* PS 取值
DATA: BEGIN OF GS_PRPS ,
        PSPNR  TYPE PRPS-PSPNR,
        POSID  TYPE PRPS-POSID,
        OBJNR  TYPE PRPS-OBJNR,
        PBUKR  TYPE PRPS-PBUKR,
        ZCGDDH TYPE PRPS-ZCGDDH,
      END OF GS_PRPS .
DATA GT_PRPS LIKE TABLE OF GS_PRPS .
DATA: BEGIN OF GS_PRCKS ,
        SUBNR  TYPE PRECP2-SUBNR,
        KALNR  TYPE PRECP2-KALNR,
        MATNR  TYPE CKIS-MATNR,
        GPREIS TYPE CKIS-GPREIS,
      END OF GS_PRCKS .
DATA GT_PRCKS LIKE TABLE OF GS_PRCKS .
DATA: BEGIN OF GS_RESB ,
        RSNUM TYPE RESB-RSNUM,
        RSPOS TYPE RESB-RSPOS,
        BWART TYPE RESB-BWART,
        MATNR TYPE RESB-MATNR,
        BDMNG TYPE RESB-BDMNG,
        PSPEL TYPE RESB-PSPEL,
      END OF GS_RESB .
DATA GT_RESB LIKE TABLE OF GS_RESB .
DATA: BEGIN OF GS_MSEG ,
        MBLNR      TYPE MSEG-MBLNR,
        MJAHR      TYPE MSEG-MJAHR,
        ZEILE      TYPE MSEG-ZEILE,
        BWART      TYPE MSEG-BWART,
        MATNR      TYPE MSEG-MATNR,
        MENGE      TYPE MSEG-MENGE,
        PS_PSP_PNR TYPE MSEG-PS_PSP_PNR,
      END OF GS_MSEG .
DATA GT_MSEG LIKE TABLE OF GS_MSEG .
DATA: BEGIN OF GS_BSEG ,
        BUKRS TYPE BSEG-BUKRS,
        BELNR TYPE BSEG-BELNR,
        GJAHR TYPE BSEG-GJAHR,
        BUZEI TYPE BSEG-BUZEI,
        MATNR TYPE BSEG-MATNR,
        MENGE TYPE BSEG-MENGE,
        PROJK TYPE BSEG-PROJK,
        SHKZG TYPE BSEG-SHKZG,
        DMBTR TYPE BSEG-DMBTR,
      END OF GS_BSEG .
DATA GT_BSEG LIKE TABLE OF GS_BSEG .
DATA: GT_MAKT TYPE TABLE OF MAKT ,
      GS_MAKT TYPE MAKT .

* PO+SO
DATA: GT_POSO1 TYPE TABLE OF TY_POSO1,
      GS_POSO1 TYPE TY_POSO1.
DATA: GT_PO1 TYPE TABLE OF TY_PO1,
      GS_PO1 TYPE TY_PO1.
DATA: GT_SO1 TYPE TABLE OF TY_SO1,
      GS_SO1 TYPE TY_SO1.

* PO+PS
DATA: GT_POPS2 TYPE TABLE OF TY_POPS2,
      GS_POPS2 TYPE TY_POPS2.
DATA: GT_PO2 TYPE TABLE OF TY_PO1,  "跟TAB1 共用一个PO类型
      GS_PO2 TYPE TY_PO1.
DATA: GT_PS2 TYPE TABLE OF TY_PS2,
      GS_PS2 TYPE TY_PS2.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT1.      "  ALV Fieldcat Setting
  clear gS_lvc1.
  gS_lvc1-fieldname = &1.
  gS_lvc1-coltext = &2.
  gS_lvc1-scrtext_l = &2.
  gS_lvc1-scrtext_m = &2.
  gS_lvc1-scrtext_s = &2.
  gS_lvc1-reptext = &2.
  gS_lvc1-edit = &3.
  gS_lvc1-cfieldname = &4.
  gS_lvc1-qfieldname =  &5.
  gS_lvc1-ref_table = &6.
  gS_lvc1-ref_field = &7.
  gS_lvc1-outputlen = &8.
*  gw_lvc-intlen = &9.
*  IF &1 = 'HKONT'.
*    gS_lvc-f4availabl = 'X'.
*  ENDIF.
*  IF &1 = 'SFNCX'.
*    gS_lvc-checkbox = 'X'.
*  ENDIF.
  append gS_lvc1 to gt_lvc1.
END-OF-DEFINITION.
DEFINE INIT_FIELDCAT2.      "  ALV Fieldcat Setting
  clear gS_lvc2.
  gS_lvc2-fieldname = &1.
  gS_lvc2-coltext = &2.
  gS_lvc2-scrtext_l = &2.
  gS_lvc2-scrtext_m = &2.
  gS_lvc2-scrtext_s = &2.
  gS_lvc2-reptext = &2.
  gS_lvc2-edit = &3.
  gS_lvc2-cfieldname = &4.
  gS_lvc2-qfieldname =  &5.
  gS_lvc2-ref_table = &6.
  gS_lvc2-ref_field = &7.
  gS_lvc2-outputlen = &8.
*  gw_lvc-intlen = &9.
*  IF &1 = 'HKONT'.
*    gS_lvc-f4availabl = 'X'.
*  ENDIF.
*  IF &1 = 'SFNCX'.
*    gS_lvc-checkbox = 'X'.
*  ENDIF.
  append gS_lvc2 to gT_lvc2.
END-OF-DEFINITION.
DEFINE INIT_FIELDCAT3.      "  ALV Fieldcat Setting
  clear gS_lvc3.
  gS_lvc3-fieldname = &1.
  gS_lvc3-coltext = &2.
  gS_lvc3-scrtext_l = &2.
  gS_lvc3-scrtext_m = &2.
  gS_lvc3-scrtext_s = &2.
  gS_lvc3-reptext = &2.
  gS_lvc3-edit = &3.
  gS_lvc3-cfieldname = &4.
  gS_lvc3-qfieldname =  &5.
  gS_lvc3-ref_table = &6.
  gS_lvc3-ref_field = &7.
  gS_lvc3-outputlen = &8.
*  gw_lvc-intlen = &9.
*  IF &1 = 'HKONT'.
*    gS_lvc-f4availabl = 'X'.
*  ENDIF.
*  IF &1 = 'SFNCX'.
*    gS_lvc-checkbox = 'X'.
*  ENDIF.
  append gS_lvc3 to gT_lvc3.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
* CONTAINER1
DATA: GT_LVC1           TYPE LVC_T_FCAT,
      GS_LVC1           TYPE LVC_S_FCAT,
      GT_SORT1          TYPE LVC_T_SORT,
      GS_SORT1          TYPE LVC_S_SORT,
      GS_LAYOUT1        TYPE LVC_S_LAYO,   "alv的格式
      GS_VARIANT1       TYPE DISVARIANT,
      GS_GRID_SETTINGS1 TYPE LVC_S_GLAY,
      GT_EVENTS1        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GS_EVENTS1        LIKE LINE OF GT_EVENTS1.

DATA: GT_EXCLUDE1 TYPE SLIS_T_EXTAB,
      GS_EXCLUDE1 TYPE SLIS_EXTAB.

DATA: GT_OO_EXCLUDE1 TYPE UI_FUNCTIONS.

*CONTAINER2
DATA: GT_LVC2           TYPE LVC_T_FCAT,
      GS_LVC2           TYPE LVC_S_FCAT,
      GT_SORT2          TYPE LVC_T_SORT,
      GS_SORT2          TYPE LVC_S_SORT,
      GS_LAYOUT2        TYPE LVC_S_LAYO,   "alv的格式
      GS_VARIANT2       TYPE DISVARIANT,
      GS_GRID_SETTINGS2 TYPE LVC_S_GLAY,
      GT_EVENTS2        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GS_EVENTS2        LIKE LINE OF GT_EVENTS2.

DATA: GT_EXCLUDE2 TYPE SLIS_T_EXTAB,
      GS_EXCLUDE2 TYPE SLIS_EXTAB.

DATA: GT_OO_EXCLUDE2 TYPE UI_FUNCTIONS.

* CONTAINER3
DATA: GT_LVC3           TYPE LVC_T_FCAT,
      GS_LVC3           TYPE LVC_S_FCAT,
      GT_SORT3          TYPE LVC_T_SORT,
      GS_SORT3          TYPE LVC_S_SORT,
      GS_LAYOUT3        TYPE LVC_S_LAYO,   "alv的格式
      GS_VARIANT3       TYPE DISVARIANT,
      GS_GRID_SETTINGS3 TYPE LVC_S_GLAY,
      GT_EVENTS3        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GS_EVENTS3        LIKE LINE OF GT_EVENTS3.

DATA: GT_EXCLUDE3 TYPE SLIS_T_EXTAB,
      GS_EXCLUDE3 TYPE SLIS_EXTAB.

DATA: GT_OO_EXCLUDE3 TYPE UI_FUNCTIONS.

**&
DATA: GR_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GR_ALVGRID1   TYPE REF TO CL_GUI_ALV_GRID.
DATA: GR_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GR_ALVGRID2   TYPE REF TO CL_GUI_ALV_GRID.
DATA: GR_CONTAINER3 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GR_ALVGRID3   TYPE REF TO CL_GUI_ALV_GRID.

************************************************************************
* Global Variant
************************************************************************
DATA: SAVE_OK TYPE SY-UCOMM,
      OK_CODE TYPE SY-UCOMM.

DATA G_SUB(4) TYPE C VALUE '0100'.
DATA G_SUB101(4) TYPE C VALUE '0101'.
DATA G_SUB102(4) TYPE C VALUE '0102'.
DATA G_SUB201(4) TYPE C VALUE '0201'.
DATA G_SUB202(4) TYPE C VALUE '0202'.
************************************************************************
* Constant
************************************************************************
CONTROLS MYTAB TYPE TABSTRIP .
