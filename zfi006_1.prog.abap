REPORT ZFI006_1.
*&---------------------------------------------------------------------*
*& Report  ZFI006
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/03/2
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
** 修改日期   开发人员  请求号        描述
" 20170220   IT02     ED1K905257   品能修改银行账户信息&发票信息抬头已付款信息
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:EKKO,RBKP,ZFI006,LFM1.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        STATU      TYPE   ICONNAME,       "状态烂
        BELNR      TYPE   RBKP-BELNR,     "发票凭证
        GJAHR      TYPE   RBKP-GJAHR,     "发票会计年度
        ZSQD       TYPE   ZFI006-ZSQD,    "付款申请单
        BUKRS      TYPE   EKKO-BUKRS,     "公司代码
        LIFNR      TYPE   EKKO-LIFNR,     "供应商
        NAME1      TYPE   LFA1-NAME1,     "供应商名称
        EBELN      TYPE   EKKO-EBELN,     "采购订单号
        NETWR      TYPE   EKPO-NETWR,     "采购订单总价
        WAERS      TYPE   EKKO-WAERS,     "货币单位
        NETWR_1    TYPE   EKPO-NETWR,     "采购订单总价（外币）
        YJHJE      TYPE   EKBE-DMBTR,     "已交货金额
        YJHJE_1    TYPE   EKBE-DMBTR,     "已交货金额(外币)
        FPPZJE     TYPE   RBKP-RMWWR,     "发票总金额
        FPPZJE_1   TYPE   RBKP-RMWWR,     "发票总金额
        YFJE       TYPE   EKBE-DMBTR,     "已付金额
        ZFKXZ      TYPE   ZFI006-ZFKXZ,   "付款性质
        ZSQFKJE    TYPE   ZFI006-ZSQFKJE, "申请付款金额
        ZSQFKJEDX  TYPE C LENGTH 150, "申请付款金额大写
        WAERS_2    TYPE   ZFI006-WAERS_2, "货币
        ZSQRQ      TYPE   ZFI006-ZSQRQ,   "申请日期
        ZSQFKRQ    TYPE   ZFI006-ZSQFKRQ, "申请付款日期
        ZZY        TYPE   ZFI006-ZZY,     "摘要
        ZCLJD      TYPE   ZFI006-ZCLJD,   "处理进度
        BELNR_F    TYPE   ZFI006-BELNR_F, "付款会计凭证
        GJAHR_F    TYPE   ZFI006-GJAHR_F, "付款会计年度
        WAERS_1    TYPE   EKKO-WAERS,     "货币单位
        CELLTAB    TYPE   LVC_T_STYL,     "控制单元格属性
        FKDQ       TYPE   SY-DATUM ,      "付款到期日IT02150601
        SGTXT      TYPE   RBKP-SGTXT,     "发票抬头文本  IT02 150709
        YSQJE      TYPE   ZFI006-YSQJE,   "已申请金额
        YSQJE_ADD  TYPE   ZFI006-YSQJE,    "已申请-按订单
        YSQJE_AFP  TYPE   ZFI006-YSQJE,    "已申请-按发票
        YSQJE_TZDD TYPE  ZFI006-YSQJE,    "已申请-调整订单
        YSQJE_TZFP TYPE  ZFI006-YSQJE,    "已申请-调整发票
        EKGRP      TYPE LFM1-EKGRP, "采购组
        EKNAM      TYPE T024-EKNAM, "采购组ming
        ZBOX       TYPE   C,
*&--代码添加 BY HANDYBY 16.08.2017 21:54:05  BEGIN
        BANKN      TYPE LFBK-BANKN, " 银行账号好吗
        KOINH      TYPE LFBK-KOINH, " 账户持有人姓名
        BANKL      TYPE LFBK-BANKL, " 银行代码
        BANKA      TYPE BNKA-BANKA, " 银行名称
*&--代码添加 BY HANDYBY 16.08.2017 21:54:05  END
      END OF TY_DATA.

TYPES:BEGIN OF TY_EKKO,
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        BUKRS TYPE   EKKO-BUKRS,     "公司代码
        LIFNR TYPE   EKKO-LIFNR,     "供应商
        WAERS TYPE   EKKO-WAERS,     "货币单位
      END OF TY_EKKO.

TYPES:BEGIN OF TY_EKPO,
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        EBELP TYPE   EKPO-EBELP,     "采购订单行项目
        MATNR TYPE   EKPO-MATNR,     "物料号
        TXZ01 TYPE EKPO-TXZ01,       "物料描述
        BUKRS TYPE   EKKO-BUKRS,     "公司代码
        LIFNR TYPE   EKKO-LIFNR,     "供应商
        NETWR TYPE   EKPO-NETWR,     "采购订单总价
        WAERS TYPE   EKKO-WAERS,     "货币单位
      END OF TY_EKPO.

TYPES:BEGIN OF TY_RSEG,
        GJAHR TYPE RSEG-GJAHR,          "发票年度
        BELNR TYPE RSEG-BELNR,          "发票号
        EBELN TYPE RSEG-EBELN,          "采购订单号
        WRBTR TYPE RSEG-WRBTR,          "采购订单金额
      END OF TY_RSEG.

TYPES:BEGIN  OF TY_DELBELNR,
        BELNR TYPE RSEG-BELNR, "发票凭证号
        GJAHR TYPE RSEG-GJAHR, "发票年度
      END OF TY_DELBELNR.

"增加品能打印模板数据
TYPES:BEGIN  OF TY_PN_HD,
        ZSQD      TYPE   ZFI006-ZSQD,    "付款申请单
        BUKRS     TYPE   T001-BUKRS,     "公司代码
        BUTXT     TYPE   T001-BUTXT,     "公司代码名称
        BELNR     TYPE   RBKP-BELNR,       "系统发票号
        GJAHR     TYPE   RBKP-GJAHR ,      "发票年度
        ZKDQR     TYPE   RBKP-ZFBDT,       "账款到期日
        JZ        TYPE   RBKP-RMWWR,       "净值
        XBLNR     TYPE   RBKP-XBLNR,       "金税发票号
        BLDAT     TYPE   RBKP-BLDAT,       "发票日期
        WMWST1    TYPE   RBKP-WMWST1,      "税额
        LIFNR     TYPE   RBKP-LIFNR,       "供应商编码
        NAME1     TYPE   LFA1-NAME1,       "供应商描述
        RMWWR     TYPE   RBKP-RMWWR,       "税价合计
        YFJE      TYPE   EKBE-DMBTR,     "已付金额
        ZSQFKJE   TYPE   ZFI006-ZSQFKJE, "申请付款金额
        ZSQFKJEDX TYPE   C LENGTH 150,     "申请付款金额大写
        WFJE      TYPE   EKBE-DMBTR,       "未付
        ZLSCH     TYPE   RBKP-ZLSCH,       "付款条件
        FKMS      TYPE   T042Z-TEXT1,      "付款条件描述
        WAERS     TYPE   WAERS,             "货币码
        ZZY       TYPE   ZFI006-ZZY,     "摘要
        BANKN     TYPE   LFBK-BANKN,     "银行账户
        BANKS     TYPE   LFBK-BANKS,     "银行账户
        KOINH     TYPE   LFBK-KOINH,      "科目持有者
        BANKL     TYPE   BNKA-BANKL,      "银行代码
        BANKA     TYPE   BNKA-BANKA,      "银行名称
      END OF TY_PN_HD .

TYPES:BEGIN  OF TY_PN_MX,
        ZSQD  TYPE ZFI006-ZSQD,      "付款申请单
        XH    TYPE I,                  "序号
        BELNR TYPE RBKP-BELNR,         "系统发票号
        GJAHR TYPE RBKP-GJAHR,         "发票年度
        XBLNR TYPE RSEG-XBLNR,         "入库单
        EBELN TYPE RSEG-EBELN,         "采购订单
        MATNR TYPE RSEG-MATNR,         "物料号
        TXZ01 TYPE EKPO-TXZ01,         "物料描述
        RKSL  TYPE MSEG-MENGE,         "入库数量
        HSDJ  TYPE RSEG-WRBTR,         "含税单价
        HSJE  TYPE RSEG-WRBTR,         "含税金额
        BSTME TYPE RSEG-BSTME,         "单位
      END OF TY_PN_MX .

"发票付款明细
TYPES:BEGIN OF TY_PN_FP_MX,
        ZSQD  TYPE   ZFI006-ZSQD,      "付款申请单
        BELNR TYPE   RBKP-BELNR,       "系统发票号
        GJAHR TYPE   RBKP-GJAHR ,      "发票年度
        EBELN TYPE RSEG-EBELN,         "采购订单
        YFJE  TYPE   EKBE-DMBTR,       "已付金额
        WAERS TYPE   WAERS,            "货币码
      END OF TY_PN_FP_MX.

"发票付款汇总
TYPES:BEGIN OF TY_PN_FP_HJ,
        ZSQD  TYPE   ZFI006-ZSQD,      "付款申请单
        BELNR TYPE   RBKP-BELNR,       "系统发票号
        GJAHR TYPE   RBKP-GJAHR ,      "发票年度
        YFJE  TYPE   EKBE-DMBTR,       "已付金额
        WAERS TYPE   WAERS,            "货币码
      END OF TY_PN_FP_HJ.

"发票凭证汇总明细
TYPES:BEGIN OF TY_PN_HJ,
        BELNR TYPE RBKP-BELNR,     "系统发票号
        GJAHR TYPE C LENGTH 4,    "发票年度
        XBLNR TYPE RSEG-XBLNR,    "入库单
        EBELN TYPE RSEG-EBELN,    "采购订单
        EBELP TYPE RSEG-EBELP,   "采购明细
        RKSL  TYPE MSEG-MENGE,   "入库数量
        HSJE  TYPE BSEG-WRBTR,   "含税金额
        BSTME TYPE RSEG-BSTME,   "单位

      END OF TY_PN_HJ .

"增加品能打印采购付款模板数据
TYPES:BEGIN  OF TY_PN_EKKO,
        ZSQD      TYPE   ZFI006-ZSQD,      "付款申请单
        BUKRS     TYPE   T001-BUKRS,       "公司代码
        BUTXT     TYPE   T001-BUTXT,       "公司代码名称
        EBELN     TYPE   EKKO-EBELN,       "采购订单号
        JZ        TYPE   RBKP-RMWWR,       "净值
        SER       TYPE   KONV-KWERT,       "税额
        LIFNR     TYPE   RBKP-LIFNR,       "供应商编码
        NAME1     TYPE   LFA1-NAME1,       "供应商描述
        SJHJ      TYPE   RBKP-RMWWR,       "税价合计
        YFJE      TYPE   EKBE-DMBTR,       "已付金额
        ZSQFKJE   TYPE   ZFI006-ZSQFKJE,   "申请付款金额
        ZSQFKJEDX TYPE   C LENGTH 150,     "申请付款金额大写
        WFJE      TYPE   EKBE-DMBTR,       "未付
        ZTERM     TYPE   EKKO-ZTERM,       "付款条款
        FKMS      TYPE   T042Z-TEXT1,      "付款条件描述
        WAERS     TYPE   WAERS,             "货币码
        ZZY       TYPE   ZFI006-ZZY,        "摘要
        BANKN     TYPE   LFBK-BANKN,        "银行账户
        KOINH     TYPE   LFBK-KOINH,        "科目持有者
        BANKS     TYPE   LFBK-BANKS,
        BANKL     TYPE   BNKA-BANKL,      "银行代码
        BANKA     TYPE   BNKA-BANKA,      "银行名称
      END OF TY_PN_EKKO .

TYPES:BEGIN OF TY_KONV_HZ,
        KNUMV TYPE KONV-KNUMV,
        SJHJ  TYPE   KONV-KWERT,       "税价合计
        SER   TYPE   KONV-KWERT,       "税额

      END OF TY_KONV_HZ .


"采购组名称
TYPES:BEGIN OF TY_LFM1,
        LIFNR TYPE LFM1-LIFNR, "供应商号
        EKORG TYPE LFM1-EKORG, "采购组
        EKGRP TYPE LFM1-EKGRP, "采购组
        EKNAM TYPE T024-EKNAM, "采购组ming
      END OF  TY_LFM1 .
DATA : T_LFM1 TYPE TABLE OF TY_LFM1 WITH HEADER LINE.


DATA:GS_DELBELNR TYPE  TY_DELBELNR .
DATA:GT_DELBELNR TYPE TABLE OF TY_DELBELNR.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI006    TYPE TABLE OF ZFI006.
DATA GS_ZFI006    TYPE ZFI006.

DATA GT_ZFI006_1  TYPE TABLE OF ZFI006.
DATA GS_ZFI006_1  TYPE ZFI006.

DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.
DATA GS_DATA_2   TYPE TY_DATA.

DATA GT_RSEG     TYPE TABLE OF RSEG.
DATA GT_RSEG_4   TYPE TABLE OF TY_RSEG.
DATA GS_RSEG_4   TYPE TY_RSEG.
DATA GS_RSEG     TYPE RSEG.
DATA GS_RSEG_3   TYPE RSEG.
DATA GS_RSEG_2   TYPE RSEG.

*采购采购订单号汇总发票金额
DATA GT_RSEG_1 TYPE TABLE OF TY_RSEG.
DATA GS_RSEG_1 TYPE TY_RSEG.

DATA GT_EKKO   TYPE TABLE OF EKKO.
DATA GS_EKKO   TYPE EKKO.

DATA GT_EKPO   TYPE TABLE OF TY_EKPO.
DATA GS_EKPO   TYPE TY_EKPO.

DATA GT_EKBE   TYPE TABLE OF EKBE.
DATA GS_EKBE   TYPE EKBE.

DATA GT_RBKP   TYPE TABLE OF RBKP.
DATA GS_RBKP   TYPE RBKP.

DATA GT_LFA1   TYPE TABLE OF LFA1.
DATA GS_LFA1   TYPE LFA1.

DATA GT_T001   TYPE TABLE OF T001.
DATA GS_T001   TYPE T001.

DATA GT_KONV   TYPE TABLE OF KONV.
DATA GS_KONV   TYPE KONV.



DATA:GT_T052U TYPE TABLE OF T052U,
     GS_T052U TYPE T052U.

DATA GT_ZFI006_P TYPE TABLE OF ZFI006_P.
DATA GS_ZFI006_P TYPE ZFI006_P.

DATA GT_ZFI006_P_1 TYPE TABLE OF ZFI006_P.
DATA GS_ZFI006_P_1 TYPE ZFI006_P.

DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

"品能定义数据
DATA:GT_PN_HD TYPE TABLE OF TY_PN_HD,
     GS_PN_HD TYPE TY_PN_HD.

DATA:GT_PN_MX TYPE TABLE OF TY_PN_MX,
     GS_PN_MX TYPE TY_PN_MX.

DATA:GT_PN_HJ TYPE TABLE OF TY_PN_HJ,
     GS_PN_HJ TYPE TY_PN_HJ.

DATA:GT_PN_CGDD TYPE TABLE OF TY_PN_EKKO,
     GS_PN_CGDD TYPE TY_PN_EKKO.

DATA:GT_PN_EKKO TYPE TABLE OF EKKO,
     GS_PN_EKKO TYPE EKKO.

DATA:GT_PN_FP_MX TYPE TABLE OF TY_PN_FP_MX,
     GS_PN_FP_MX TYPE TY_PN_FP_MX.

DATA:GT_PN_FP_HJ TYPE TABLE OF TY_PN_FP_HJ,
     GS_PN_FP_HJ TYPE TY_PN_FP_HJ.


DATA GT_KONV_1   TYPE TABLE OF KONV.
DATA GS_KONV_1   TYPE KONV.

DATA GT_KONV_9   TYPE TABLE OF KONV.
DATA GS_KONV_9   TYPE KONV.

DATA: GT_DATA_01 TYPE TABLE OF TY_DATA,
      GS_DATA_01 TYPE TY_DATA.

DATA: GT_DATA_02 TYPE TABLE OF TY_DATA,
      GS_DATA_02 TYPE TY_DATA.

DATA:GT_DATA_FP TYPE TABLE OF TY_DATA,
     GS_DATA_FP TYPE TY_DATA.

DATA:GT_RBKP_01 TYPE TABLE OF RBKP,
     GS_RBKP_01 TYPE RBKP.

DATA:GT_RSEG_01 TYPE TABLE OF RSEG,
     GS_RSEG_01 TYPE RSEG.

DATA:GT_BELNR TYPE TABLE OF TY_DELBELNR,
     GS_BELNR TYPE TY_DELBELNR.

DATA:GT_T042Z TYPE TABLE OF T042Z,
     GS_T042Z TYPE T042Z.


*DATA:gt_t052u type table of t052u,
*     gs_t052u type t052u .

DATA:GT_KONV_HZ TYPE TABLE OF TY_KONV_HZ,
     GS_KONV_HZ TYPE TY_KONV_HZ.

DATA:T_WAERS TYPE RBKP-WAERS.
DATA:T_FACTOR TYPE ISOC_FACTOR.
DATA T_FTAXP    TYPE TABLE OF FTAXP.
DATA S_FTAXP    TYPE FTAXP.
DATA FACTOR TYPE P DECIMALS 3.

DATA:G_XH TYPE I.    "全局序号

DATA: CONTROL    TYPE SSFCTRLOP,
      NTOTALLINE TYPE I,
      NPAGELINE  TYPE I VALUE 9,
      P_INDEX    LIKE SY-TABIX.
DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
      NCURRLINE       TYPE I,      "中间变量
      JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA: G_NAME TYPE RS38L_FNAM.
DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI006_PN'.
DATA:L_PAGE  TYPE I.
DATA:L_BUKRS TYPE BUTXT."公司代码描述

*RANGES:R_BELNR FOR RBKP-BELNR,  "发票凭证号
*       R_EBELN FOR EKKO-EBELN.  "采购订单范围

DATA G_ANSWER     TYPE STRING. "控制弹出框
************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.

  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

*
*  IF gw_lvc-fieldname = 'ZSQFKJE'.
*  gw_lvc-NO_SIGN = ''.
*  ENDIF.

IF   gw_lvc-fieldname     = 'NETWR'
or   gw_lvc-fieldname     = 'NETWR_1'
or   gw_lvc-fieldname     = 'YJHJE'
or   gw_lvc-fieldname     = 'YJHJE_1'
or   gw_lvc-fieldname     = 'FPPZJE'
or   gw_lvc-fieldname     = 'FPPZJE_1'
or   gw_lvc-fieldname     = 'YFJE' .
"or   gw_lvc-fieldname     = 'ZSQFKJE'.
*货币不显示0
      gw_lvc-no_zero = 'X'.
ENDIF.
IF gw_lvc-fieldname =  'NETWR_1'  OR gw_lvc-fieldname = 'YJHJE_1 '  OR gw_lvc-fieldname = 'FPPZJE_1' ."OR gw_lvc-fieldname = 'YFJE'  OR gw_lvc-fieldname = 'ZSQFKJE'.
    gw_lvc-CFIELDNAME = 'WAERS_1'.
ENDIF.
  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.

END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,                      "SY-REPID 指 当前的主程序
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GW_EVENTS        LIKE LINE OF GT_EVENTS.
DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.
DATA: GW_ISTABLE TYPE LVC_S_STBL.

DATA STBL       TYPE LVC_S_STBL.
************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR EKKO-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY ,"会计年度
                S_LIFNR   FOR EKKO-LIFNR ,                                     "供应商
                S_EBELN   FOR EKKO-EBELN ,                                     "采购订单
                S_BELNR   FOR RBKP-BELNR,                                      "发票号
                S_BUDAT   FOR RBKP-BUDAT,"过账日期
                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI006-ZSQRQ,                                    "申请日期
                S_ZSQFKR  FOR ZFI006-ZSQFKRQ,                                  "申请付款日期
                S_ZCLJD   FOR ZFI006-ZCLJD,                                    "处理进度
              "  S_EKORG  FOR LFM1-EKORG,"采购组织
                S_EKGRP FOR LFM1-EKGRP."采购组
SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002."未完成付款状态
PARAMETER X_WFK  AS CHECKBOX  ."未完成付款
SELECTION-SCREEN END OF BLOCK BLK2.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.


*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF  S_EBELN  IS  INITIAL
  AND S_BELNR  IS  INITIAL .
    MESSAGE S004(ZFICO01) DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 1000.
  ENDIF.
* IF S_BUKRS-LOW = '1100' AND S_BELNR IS INITIAL.
*   MESSAGE '1100公司发票凭证编号查询不能为空' TYPE 'E'.
*   LEAVE TO SCREEN 1000.
* ENDIF.
*
*   IF S_BUKRS-LOW = '1100' AND S_EBELN IS NOT INITIAL.
*      MESSAGE '1100公司不能按采购凭证号查询' TYPE 'E'.
*    ENDIF.
* IF S_BUKRS-LOW = '1000' AND S_EBELN  IS INITIAL.
*   MESSAGE '1100公司采购凭证编号查询不能为空' TYPE 'E'.
*   LEAVE TO SCREEN 1000.
* ENDIF.
*IF S_BUKRS-LOW = '1000' AND S_BELNR  IS NOT INITIAL.
*      MESSAGE '1100公司不能按发票凭证号查询' TYPE 'E'.
*ENDIF.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD S_BUKRS-LOW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
*当选择屏幕选择的是采购订单
  IF S_EBELN IS NOT INITIAL.
**查询发票行项目
*    SELECT * FROM RSEG
*      INTO CORRESPONDING FIELDS OF TABLE GT_RSEG
*      WHERE BUKRS IN S_BUKRS
*      AND   EBELN IN S_EBELN
*      AND   LIFNR IN S_LIFNR
*      AND   EBELN IN S_EBELN
*      AND   BELNR IN S_BELNR
*      AND   GJAHR IN S_GJAHR.
*
**查询对应的采购订单
*    IF GT_RSEG IS NOT INITIAL.

*查询采购订单的抬头
    SELECT * FROM EKKO
     INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
     WHERE EBELN IN S_EBELN
     AND   BUKRS IN S_BUKRS
     AND   LIFNR IN S_LIFNR
     AND   EKGRP IN S_EKGRP.

*查询公司代码对应的货币
    SELECT * FROM T001
      INTO CORRESPONDING FIELDS OF TABLE GT_T001
      FOR ALL ENTRIES IN GT_EKKO
      WHERE BUKRS = GT_EKKO-BUKRS.
    SORT  GT_T001 BY BUKRS .

*查询供应商描述
    SELECT * FROM LFA1
     INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
     FOR ALL ENTRIES IN GT_EKKO
     WHERE LIFNR = GT_EKKO-LIFNR.
*查询采购组 及采购组名
    SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM INTO CORRESPONDING FIELDS OF TABLE T_LFM1
      FROM LFM1 AS A
      LEFT JOIN T024 AS B
      ON A~EKGRP = B~EKGRP
      WHERE  A~LIFNR IN S_LIFNR AND  A~EKORG = S_BUKRS-LOW AND A~EKGRP IN S_EKGRP.
    SORT T_LFM1 BY  LIFNR EKORG EKGRP .
*查询采购订单的行项目
    SELECT * FROM EKPO
     INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
     FOR ALL ENTRIES IN GT_EKKO
     WHERE EKPO~EBELN = GT_EKKO-EBELN AND LOEKZ <> 'L'. "150525 添加  LOEKZ <> 'L‘ 的删除标识
*     AND   EKPO~EBELP = GT_RSEG-EBELP.

*查询采购订单PB00或者PBXX的金额
    SELECT * FROM KONV
      INTO CORRESPONDING FIELDS OF TABLE GT_KONV
      FOR ALL ENTRIES IN GT_EKKO
      WHERE KNUMV = GT_EKKO-KNUMV.

*查询交货历史
    SELECT * FROM EKBE
     INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
     FOR ALL ENTRIES IN GT_EKPO
     WHERE EBELN = GT_EKPO-EBELN
     AND   EBELP = GT_EKPO-EBELP.

*查询自建表数据(审批通过)
    SELECT * FROM ZFI006
      INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006
      FOR ALL ENTRIES IN GT_EKKO
      WHERE EBELN = GT_EKKO-EBELN.
*      AND   ZCLJD = '3'.

  ELSE.

*当选择屏幕选择的是发票
*（筛选出已过账且未冲销的）
    SELECT * FROM RBKP
      INTO CORRESPONDING FIELDS OF TABLE GT_RBKP
      WHERE RBSTAT = '5'
      AND   STBLG  = ''
      AND   XRECH  = 'X'
      AND   BELNR IN S_BELNR
      AND   BUKRS IN S_BUKRS
      AND   LIFNR IN S_LIFNR
      AND   GJAHR IN S_GJAHR.

    " AND   BUDAT IN S_BUDAT.
    "IT02 UPDATE 150521begin
    IF GT_RBKP[] IS NOT  INITIAL.

      IF S_BUDAT[] IS NOT INITIAL.
        LOOP AT GT_RBKP INTO GS_RBKP .
          GS_RBKP-ZFBDT = GS_RBKP-ZFBDT + GS_RBKP-ZBD1T .
          MODIFY GT_RBKP FROM GS_RBKP.
        ENDLOOP.
        DELETE GT_RBKP WHERE ZFBDT NOT IN S_BUDAT.
      ENDIF.
    ENDIF.
    "IT02 UPDATE 150502end
    "增加采购组 并查询
    SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM INTO CORRESPONDING FIELDS OF TABLE T_LFM1
    FROM LFM1 AS A
    LEFT JOIN T024 AS B
    ON A~EKGRP = B~EKGRP
    WHERE  A~LIFNR IN S_LIFNR AND EKORG IN S_BUKRS AND A~EKGRP IN S_EKGRP.

    SORT T_LFM1 BY  LIFNR EKGRP .

    IF GT_RBKP[] IS NOT INITIAL.
      SELECT * FROM RSEG
        INTO CORRESPONDING FIELDS OF TABLE GT_RSEG
        FOR ALL ENTRIES IN GT_RBKP
        WHERE BELNR = GT_RBKP-BELNR
        AND   GJAHR = GT_RBKP-GJAHR.


**汇总发票金额
*      LOOP AT GT_RSEG INTO GS_RSEG.
*
*        MOVE-CORRESPONDING GS_RSEG TO GS_RSEG_4.
*        COLLECT GS_RSEG_4 INTO GT_RSEG_4.
*        CLEAR GS_RSEG_4.
*
*      ENDLOOP.


*采购订单抬头
      SELECT * FROM EKKO
       INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
       FOR ALL ENTRIES IN GT_RSEG
       WHERE EBELN = GT_RSEG-EBELN.

*查询公司代码对应的货币
      SELECT * FROM T001
        INTO CORRESPONDING FIELDS OF TABLE GT_T001
        FOR ALL ENTRIES IN GT_EKKO
        WHERE BUKRS = GT_EKKO-BUKRS.

*查询采购订单PB00或者PBXX的金额
      SELECT * FROM KONV
        INTO CORRESPONDING FIELDS OF TABLE GT_KONV
        FOR ALL ENTRIES IN GT_EKKO
        WHERE KNUMV = GT_EKKO-KNUMV.

*查询公司代码对应的货币
      SELECT * FROM T001
        INTO CORRESPONDING FIELDS OF TABLE GT_T001
        FOR ALL ENTRIES IN GT_EKKO
        WHERE BUKRS = GT_EKKO-BUKRS.

*查询供应商描述
      SELECT * FROM LFA1
       INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
       FOR ALL ENTRIES IN GT_EKKO
       WHERE LIFNR = GT_EKKO-LIFNR.

*查询采购订单的行项目
      SELECT * FROM EKPO
       INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
       FOR ALL ENTRIES IN GT_EKKO
       WHERE EKPO~EBELN = GT_EKKO-EBELN.

*查询交货历史
      SELECT * FROM EKBE
       INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
       FOR ALL ENTRIES IN GT_EKPO
       WHERE EBELN = GT_EKPO-EBELN
       AND   EBELP = GT_EKPO-EBELP.

*查询自建表数据
      SELECT * FROM ZFI006
        INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006
        FOR ALL ENTRIES IN GT_RBKP
        WHERE  BELNR = GT_RBKP-BELNR
         AND   GJAHR = GT_RBKP-GJAHR
         AND   STATU <> ICON_DELETE.

      "IT02 追加 RE为空，ZCLJD 为3 ，根据采购订单检索追加到 GT_ZFI006表
      IF GT_RSEG[] IS NOT INITIAL.
        SELECT * FROM ZFI006
          APPENDING CORRESPONDING FIELDS OF TABLE GT_ZFI006
          FOR ALL ENTRIES IN GT_RSEG
          WHERE EBELN = GT_RSEG-EBELN
*          AND ZCLJD = '3'
          AND BELNR = ''
          AND STATU <> ICON_DELETE.
      ENDIF.
    ENDIF.
  ENDIF.


*ADD BY HANDWY  2105-7-16 增加二分法排序
  SORT GT_RBKP    BY  BELNR GJAHR.
  SORT GT_RSEG    BY  BELNR GJAHR BUZEI EBELN EBELP.
  SORT GT_EKKO    BY  EBELN.
  SORT GT_EKPO    BY  EBELN EBELP.
  SORT GT_KONV    BY  KNUMV KPOSN.
  SORT GT_EKBE    BY  EBELN EBELP ZEKKN VGABE GJAHR BELNR BUZEI.
  SORT GT_LFA1    BY  LIFNR.
  SORT GT_T001    BY  BUKRS.
  SORT GT_ZFI006  BY  ZSQD BUKRS GJAHR EBELN EBELP BELNR.
*ENDADD.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .

  DATA L_NETWR    TYPE EKPO-NETWR.
  DATA LS_STABLE  TYPE LVC_S_STBL.
  DATA LS_CELLTAB TYPE LVC_S_STYL.
  DATA LT_CELLTAB TYPE LVC_T_STYL.
  DATA L_FIRST    TYPE C."用于得到汇总行
  DATA L_YFJE     TYPE ZFI006-YFJE.
  DATA: L_YSQJE      TYPE ZFI006-YSQJE,     "已申请金额
        L_YSQJE_ADD  TYPE ZFI006-YSQJE,     "已申请金额-按订单
        L_YSQJE_AFP  TYPE ZFI006-YSQJE, "已申请金额-按发票
        L_YSQJE_TZDD TYPE ZFI006-YSQJE, "已申请金额-调整订单
        L_YSQJE_TZFP TYPE ZFI006-YSQJE. "已申请金额-调整发票
  DATA L_ZSQFKJE  TYPE ZFI006-ZSQFKJE."申请付款金额
  DATA L_EBELN    TYPE EKKO-EBELN.
  DATA T_FTAXP    TYPE TABLE OF FTAXP.
  DATA S_FTAXP    TYPE FTAXP.
  DATA FACTOR TYPE P DECIMALS 3.
**********************************************当选择屏幕上输入采购订单
  IF S_EBELN IS NOT INITIAL.
    LOOP AT GT_EKKO INTO GS_EKKO.
      MOVE-CORRESPONDING GS_EKKO TO GS_DATA.
      "查询采购组及采购组名
      READ TABLE  T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
      IF SY-SUBRC = 0 .
        GS_DATA-LIFNR = T_LFM1-LIFNR.
        GS_DATA-EKNAM = T_LFM1-EKNAM.
      ENDIF.
*查询公司代码对应的本位币
      READ TABLE GT_T001 INTO GS_T001
      WITH KEY BUKRS = GS_EKKO-BUKRS
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-WAERS = GS_T001-WAERS.
      ENDIF.

*查询供应商名称
      READ TABLE GT_LFA1 INTO GS_LFA1
      WITH KEY LIFNR = GS_EKKO-LIFNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-NAME1 = GS_LFA1-NAME1.
      ENDIF.
      "查询采购组及采购组名
      READ TABLE T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
      IF SY-SUBRC = 0.
        GS_DATA-EKGRP = T_LFM1-EKGRP.
        GS_DATA-EKNAM = T_LFM1-EKNAM.
      ENDIF.
*汇总整张采购订单的金额
      CLEAR L_NETWR.
      LOOP AT GT_EKPO INTO GS_EKPO
       WHERE EBELN = GS_DATA-EBELN.

*读取条件类型PB00,以及PBXX对应的价格
        LOOP AT GT_KONV INTO GS_KONV
        WHERE  KNUMV = GS_EKKO-KNUMV
        AND    KPOSN = GS_EKPO-EBELP
        AND    KSCHL = 'PBXX'.
          L_NETWR = L_NETWR + GS_KONV-KWERT.
        ENDLOOP.

        LOOP AT GT_KONV INTO GS_KONV
        WHERE  KNUMV = GS_EKKO-KNUMV
        AND    KPOSN = GS_EKPO-EBELP
        AND    KSCHL = 'PB00'.
          L_NETWR = L_NETWR + GS_KONV-KWERT.
        ENDLOOP.

      ENDLOOP.

*整张采购订单金额（本币） * 汇率
      GS_DATA-NETWR   = L_NETWR * GS_EKKO-WKURS.

*整张采购订单金额
      GS_DATA-NETWR_1 = L_NETWR .

*货币
      GS_DATA-WAERS_1 = GS_EKKO-WAERS.

*默认黄灯
      GS_DATA-STATU = ICON_YELLOW_LIGHT.

*已交货金额
      LOOP AT GT_EKBE INTO GS_EKBE
      WHERE EBELN = GS_EKKO-EBELN
      AND   VGABE = '1'.
        IF GS_EKBE-SHKZG = 'S'.
          GS_DATA-YJHJE   = GS_DATA-YJHJE   + GS_EKBE-DMBTR.
          GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 + GS_EKBE-WRBTR.
        ENDIF.

        IF GS_EKBE-SHKZG = 'H' .
          GS_DATA-YJHJE   = GS_DATA-YJHJE   - GS_EKBE-DMBTR.
          GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 - GS_EKBE-WRBTR.
        ENDIF.
      ENDLOOP.

*已付款金额
      CLEAR L_YFJE.
      LOOP AT  GT_ZFI006 INTO GS_ZFI006
      WHERE EBELN = GS_DATA-EBELN
      AND   ZCLJD = '3'.
        " AND         BELNR = ''.
        "  L_YFJE = L_YFJE + GS_ZFI006-YFJE. 150527 YEFE 字段值为空 才注释
        L_YFJE = L_YFJE + GS_ZFI006-ZSQFKJE.  "150527 YEFE 字段值为空 才注释
      ENDLOOP.
      GS_DATA-YFJE = L_YFJE.

*已申请金额
      CLEAR : L_YSQJE,L_YSQJE_ADD,L_YSQJE_AFP,L_YSQJE_TZDD,L_YSQJE_TZFP.
      LOOP AT GT_ZFI006 INTO GS_ZFI006
       WHERE EBELN = GS_DATA-EBELN
       AND ( ZCLJD = '3'
       OR   ZCLJD  = '2'
       OR   ZCLJD  = '1')
       AND   STATU    <> ICON_DELETE.
        IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006-ZSQFKJE.             "已申请-按订单             "
        ENDIF.
        IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006-ZSQFKJE.             "已申请-按发票
        ENDIF.
        IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006-ZSQFKJE.         "已申请-调整订单
        ENDIF.
        IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006-ZSQFKJE.            "已申请-调整发票
        ENDIF.

      ENDLOOP.

*处理进度默认为1
      GS_DATA-ZCLJD = '1'.

*默认货币，申请日期
      GS_DATA-ZSQRQ      = SY-DATUM.
      GS_DATA-WAERS_2    = GS_DATA-WAERS_1.

* 默认申请金额     申请付款金额默认 = 采购订单金额 - 已申请金额（4个)
      GS_DATA-ZSQFKJE = GS_DATA-NETWR_1 - GS_DATA-YSQJE_ADD - GS_DATA-YSQJE_AFP - GS_DATA-YSQJE_TZDD - GS_DATA-YSQJE_TZFP .


      APPEND GS_DATA TO GT_DATA.
      CLEAR GS_DATA.
    ENDLOOP.
    "150527 begin 过滤 已付金额 大于等于应付金额的数据
    IF X_WFK = 'X'.
      LOOP AT GT_DATA INTO GS_DATA .

        READ TABLE GT_ZFI006 INTO GS_ZFI006
        WITH KEY EBELN = GS_DATA-EBELN
        BINARY SEARCH.
        IF SY-SUBRC = 0 .
          IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS . "若果已付金额的货币码等于 本币码 ，已付金额就和采购订单本币金额比较
            IF GS_DATA-YFJE >= GS_DATA-NETWR .
              DELETE GT_DATA WHERE EBELN = GS_DATA-EBELN.
            ENDIF.
          ELSE.                                         "若果已付金额的货币码不等于 本币码 ，已付金额就和采购订单凭证金额数比较
            IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS_1 .
              IF GS_DATA-YFJE >= GS_DATA-NETWR_1 .
                DELETE GT_DATA WHERE EBELN = GS_DATA-EBELN.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
                                                            "150527 end
  ELSE.
**********************************************当选择屏幕选择的是发票

    LOOP AT GT_RSEG INTO GS_RSEG.
      CLEAR L_FIRST.

      AT NEW BELNR.
        L_FIRST = 'X'.
        CLEAR L_YSQJE.
        CLEAR L_YFJE.
      ENDAT.

*抬头行
      IF L_FIRST = 'X'.
        GS_DATA-BELNR = GS_RSEG-BELNR.
        GS_DATA-GJAHR = GS_RSEG-GJAHR.
        GS_DATA-ZFKXZ = '1'.
        GS_DATA-ZCLJD = '1'.

*采购订单抬头
        READ TABLE GT_EKKO INTO GS_EKKO
        WITH KEY EBELN = GS_RSEG-EBELN
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_DATA-BUKRS   = GS_EKKO-BUKRS.
          GS_DATA-LIFNR   = GS_EKKO-LIFNR.
          GS_DATA-WAERS_1 = GS_EKKO-WAERS.
        ENDIF.
        "过滤供应商不在T_LFM1表中信息IT02 150804
        READ TABLE T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
        IF SY-SUBRC <> 0 .
          DELETE GT_RSEG WHERE BELNR = GS_RSEG-BELNR.
          CONTINUE.
        ENDIF.

*查询公司代码对应的本位币
        READ TABLE GT_T001 INTO GS_T001
        WITH KEY BUKRS = GS_EKKO-BUKRS
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_DATA-WAERS = GS_T001-WAERS.
        ENDIF.

*查询供应商名称
        READ TABLE GT_LFA1 INTO GS_LFA1
        WITH KEY LIFNR = GS_EKKO-LIFNR
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_DATA-NAME1 = GS_LFA1-NAME1.
        ENDIF.
        "查询采购组 及采购组名
        READ TABLE T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
        IF SY-SUBRC = 0.
          GS_DATA-EKGRP = T_LFM1-EKGRP.
          GS_DATA-EKNAM = T_LFM1-EKNAM.
        ENDIF.

*查询发票总金额
        READ TABLE GT_RBKP INTO GS_RBKP
        WITH KEY BELNR = GS_RSEG-BELNR
                 GJAHR = GS_RSEG-GJAHR
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_DATA-FPPZJE   = GS_RBKP-RMWWR * GS_RBKP-KURSF.
          GS_DATA-FPPZJE_1 = GS_RBKP-RMWWR .
        ENDIF.

*已付款金额
        CLEAR L_YFJE.
*        LOOP AT   GT_RSEG_4 INTO GS_RSEG_4
*        WHERE      BELNR = GS_RSEG-BELNR
*        AND        GJAHR = GS_RSEG-GJAHR.
*
*          LOOP AT GT_ZFI006 INTO GS_ZFI006
*           WHERE EBELN = GS_RSEG_4-EBELN
*           AND   ZCLJD = '3'.
*            L_YFJE = L_YFJE + GS_ZFI006-ZSQFKJE.
*          ENDLOOP.
*          CLEAR GS_ZFI006.
*
*        ENDLOOP.
        LOOP AT GT_ZFI006 INTO GS_ZFI006
          WHERE BELNR = GS_RSEG-BELNR
           AND  GJAHR = GS_RSEG-GJAHR
           AND   EBELN = ''
           AND  ZCLJD = '3'.
          L_YFJE = L_YFJE + GS_ZFI006-ZSQFKJE.
        ENDLOOP.

        GS_DATA-YFJE = L_YFJE.

*已申请金额
        CLEAR : L_YSQJE,L_YSQJE_ADD,L_YSQJE_AFP,L_YSQJE_TZDD,L_YSQJE_TZFP.
        LOOP AT GT_ZFI006 INTO GS_ZFI006
          WHERE BELNR = GS_RSEG-BELNR
          AND   GJAHR = GS_RSEG-GJAHR
          AND   EBELN = ''
          AND  ( ZCLJD = '3'
          OR    ZCLJD = '2'
          OR    ZCLJD = '1')
          AND   STATU    <> ICON_DELETE.
          IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
            GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006-ZSQFKJE.             "已申请-按订单             "
          ENDIF.
          IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
            GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006-ZSQFKJE.             "已申请-按发票
          ENDIF.
          IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
            GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006-ZSQFKJE.         "已申请-调整订单
          ENDIF.
          IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
            GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006-ZSQFKJE.            "已申请-调整发票
          ENDIF.
        ENDLOOP.

*申请金额(根据采购订单汇总发票行项目)
        REFRESH GT_RSEG_1.
        CLEAR GS_RSEG_1.





*申请付款金额 = 发票金额 - 已付金额   BY HANDWY 2015-7-16
        " GS_DATA-ZSQFKJE =  GS_DATA-FPPZJE_1 - GS_DATA-YSQJE.
*申请付款金额 = 发票金额 - 已申请金额（4） BY IT02 2015/12/24
        GS_DATA-ZSQFKJE = GS_DATA-FPPZJE_1  - GS_DATA-YSQJE_AFP - GS_DATA-YSQJE_TZFP .

*默认货币，申请日期
        GS_DATA-ZSQRQ      = SY-DATUM.
        GS_DATA-WAERS_2    = GS_DATA-WAERS_1.


        "150602 首先判断插入的发票首行的可申请金额是否大于零
*        IF GS_DATA-ZSQFKJE <= 0 .
*          GS_DELBELNR-BELNR = GS_DATA-BELNR.
*          APPEND  GS_DELBELNR TO GT_DELBELNR.
*          CONTINUE.
*
*        ENDIF.
        IF X_WFK = 'X'.
          READ TABLE GT_ZFI006 INTO GS_ZFI006
           WITH KEY BELNR = GS_DATA-BELNR
           BINARY SEARCH .
          IF SY-SUBRC = 0 .
            IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS . "若果已付金额的货币码等于 本币码 ，汇总的金额就和发票本币金额比较 出已付若已付大于应付的发票行就追加到GT_DELBELNR表作为后续明细的插入
              IF GS_DATA-YFJE >= GS_DATA-FPPZJE .
                GS_DELBELNR-BELNR = GS_DATA-BELNR.
                GS_DELBELNR-GJAHR = GS_DATA-GJAHR.
                APPEND  GS_DELBELNR TO GT_DELBELNR.
                CONTINUE.
              ENDIF.
            ELSE.                                         "若果已付金额的货币码不等于 本币码 ，汇总的金额就和发票凭金额数比较若已付大于应付的发票行就追加到GT_DELBELNR表作为后续明细的插入
              IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS_1 .
                IF GS_DATA-YFJE >= GS_DATA-FPPZJE_1 .
                  GS_DELBELNR-BELNR = GS_DATA-BELNR.
                  GS_DELBELNR-GJAHR = GS_DATA-GJAHR.
                  APPEND  GS_DELBELNR TO GT_DELBELNR.
                  CONTINUE.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        "IT02 150708 begin add 发票对应的行文本   发票文本写入备注字段 ADD BY HANDWY 2015-8-12
        READ TABLE GT_RBKP INTO GS_RBKP
        WITH KEY BELNR  = GS_DATA-BELNR
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_DATA-SGTXT = GS_RBKP-SGTXT.
          GS_DATA-ZZY      = GS_RBKP-SGTXT.
        ENDIF.
        "IT02 150708 begin end
        LS_CELLTAB-FIELDNAME = 'ZFKXZ' .
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

        LS_CELLTAB-FIELDNAME = 'ZSQFKJE' .
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

        LS_CELLTAB-FIELDNAME = 'ZSQFKJE' .
        LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

        GS_DATA-STATU = ICON_YELLOW_LIGHT.
        INSERT LINES OF LT_CELLTAB INTO TABLE GS_DATA-CELLTAB.

        APPEND GS_DATA TO GT_DATA.
        CLEAR GS_DATA.
        REFRESH LT_CELLTAB.
      ENDIF.

*明细行
      IF X_WFK = 'X'.
        "明细行插入之前先判断采购订单所在的发票凭证是否已插入 GT_DATA. 150602
        READ TABLE GT_DELBELNR INTO GS_DELBELNR WITH KEY BELNR = GS_RSEG-BELNR
                                                         GJAHR = GS_RSEG-GJAHR .
        IF SY-SUBRC = 0 .
          CONTINUE. "若插入的明细行的发票号已在删除的GT_DELBELNR表中，相应的明细行不插入
        ELSE.
        ENDIF.
      ENDIF.
      GS_DATA-BELNR = GS_RSEG-BELNR.
      GS_DATA-GJAHR = GS_RSEG-GJAHR.
      GS_DATA-ZFKXZ = '1'.
      GS_DATA-ZCLJD = '1'.

*采购订单抬头
      READ TABLE GT_EKKO INTO GS_EKKO
      WITH KEY EBELN = GS_RSEG-EBELN
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-BUKRS   = GS_EKKO-BUKRS.
        GS_DATA-EBELN   = GS_EKKO-EBELN.
        GS_DATA-LIFNR   = GS_EKKO-LIFNR.
        GS_DATA-WAERS_1 = GS_EKKO-WAERS.
      ENDIF.

*查询公司代码对应的本位币
      READ TABLE GT_T001 INTO GS_T001
      WITH KEY BUKRS = GS_EKKO-BUKRS
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-WAERS = GS_T001-WAERS.
      ENDIF.

*查询供应商名称
      READ TABLE GT_LFA1 INTO GS_LFA1
      WITH KEY LIFNR = GS_EKKO-LIFNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-NAME1 = GS_LFA1-NAME1.
      ENDIF.
      "查询采购组 及采购组名
      READ TABLE T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
      IF SY-SUBRC = 0.
        GS_DATA-EKGRP = T_LFM1-EKGRP.
        GS_DATA-EKNAM = T_LFM1-EKNAM.
      ENDIF.

*汇总整张采购订单的金额
      CLEAR L_NETWR.
      LOOP AT GT_EKPO INTO GS_EKPO
       WHERE EBELN = GS_DATA-EBELN.

*读取条件类型PB00,以及PBXX对应的价格
        LOOP AT GT_KONV INTO GS_KONV
        WHERE  KNUMV = GS_EKKO-KNUMV
        AND    KPOSN = GS_EKPO-EBELP
        AND    KSCHL = 'PBXX'.
          L_NETWR = L_NETWR + GS_KONV-KWERT.
        ENDLOOP.

        LOOP AT GT_KONV INTO GS_KONV
        WHERE  KNUMV = GS_EKKO-KNUMV
        AND    KPOSN = GS_EKPO-EBELP
        AND    KSCHL = 'PB00'.
          L_NETWR = L_NETWR + GS_KONV-KWERT.
        ENDLOOP.
      ENDLOOP.

*整张采购订单金额
      GS_DATA-NETWR   = L_NETWR *  GS_EKKO-WKURS.

*整张采购订单金额（本币） * 汇率
      GS_DATA-NETWR_1 = L_NETWR .

*采购已交货金额
      LOOP AT GT_EKBE INTO GS_EKBE
      WHERE EBELN = GS_EKKO-EBELN
      AND   VGABE = '1'.
        IF GS_EKBE-SHKZG = 'S'.
          GS_DATA-YJHJE   = GS_DATA-YJHJE   + GS_EKBE-DMBTR.
          GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 + GS_EKBE-WRBTR.
        ENDIF.

        IF GS_EKBE-SHKZG = 'H' .
          GS_DATA-YJHJE   = GS_DATA-YJHJE   - GS_EKBE-DMBTR.
          GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 - GS_EKBE-WRBTR.
        ENDIF.
      ENDLOOP.

*已付款金额
      CLEAR L_YFJE.
      LOOP AT GT_ZFI006 INTO GS_ZFI006
       WHERE  BELNR = GS_RSEG-BELNR
       AND    GJAHR = GS_RSEG-GJAHR
       AND    EBELN = GS_EKKO-EBELN                   "统计再追加已付款金额：根据采购订单号且发票号为空的已付款订单的金额
       AND    ZCLJD = '3'.

        L_YFJE = L_YFJE + GS_ZFI006-ZSQFKJE.                "150527
        CLEAR GS_ZFI006.
      ENDLOOP.

      LOOP AT GT_ZFI006 INTO GS_ZFI006
         WHERE  BELNR = ''
         AND    GJAHR = ''
         AND    EBELN = GS_EKKO-EBELN                   "统计再追加已付款金额：根据采购订单号且发票号为空的已付款订单的金额
         AND    ZCLJD = '3'.

        L_YFJE = L_YFJE + GS_ZFI006-ZSQFKJE.                "150527
        CLEAR GS_ZFI006.
      ENDLOOP.

*已付款金额
      GS_DATA-YFJE = L_YFJE.

*已申请金额
      CLEAR : L_YSQJE,L_YSQJE_ADD,L_YSQJE_AFP,L_YSQJE_TZDD,L_YSQJE_TZFP.
      LOOP AT GT_ZFI006 INTO GS_ZFI006
       WHERE  BELNR = GS_RSEG-BELNR
       AND    GJAHR = GS_RSEG-GJAHR     "统计已申请金额：根据发票号统计已开票订单的金额
       AND    EBELN = GS_EKKO-EBELN
       AND   ( ZCLJD = '3'
       OR     ZCLJD = '2'
       OR     ZCLJD = '1' )
       AND   STATU    <> ICON_DELETE.
*          IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
*           GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006-ZSQFKJE.             "已申请-按订单             "
*        ENDIF.
        IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006-ZSQFKJE.             "已申请-按发票
        ENDIF.
*         IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
*            GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006-ZSQFKJE.         "已申请-调整订单
*        ENDIF.
        IF GS_ZFI006-BELNR NE '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006-ZSQFKJE.            "已申请-调整发票
        ENDIF.
      ENDLOOP.

      LOOP AT GT_ZFI006 INTO GS_ZFI006
       WHERE  BELNR = ''
       AND    GJAHR = ''                    "统计已申请金额：根据采购订单号统计已申请订单的金额
       AND    EBELN = GS_EKKO-EBELN
       AND   ( ZCLJD = '3'
       OR     ZCLJD = '2'
       OR     ZCLJD = '1' )
       AND   STATU    <> ICON_DELETE.
        IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '1' OR GS_ZFI006-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006-ZSQFKJE.             "已申请-按订单             "
        ENDIF.

        IF GS_ZFI006-BELNR EQ '' AND ( GS_ZFI006-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006-ZSQFKJE.         "已申请-调整订单
        ENDIF.

      ENDLOOP.




*申请金额(根据采购订单汇总发票行项目)
      REFRESH GT_RSEG_1.
      CLEAR GS_RSEG_1.
      DATA:T_WAERS TYPE RBKP-WAERS.
      DATA:T_FACTOR TYPE ISOC_FACTOR.
      LOOP AT GT_RSEG INTO GS_RSEG
       WHERE EBELN = GS_DATA-EBELN .
        "AND  GJAHR = GS_DATA-GJAHR.
        CLEAR:T_WAERS , T_FACTOR .

        READ TABLE GT_RBKP INTO GS_RBKP
         WITH KEY BELNR = GS_DATA-BELNR
                  GJAHR = GS_DATA-GJAHR
         BINARY SEARCH.
        IF SY-SUBRC = 0.
          T_WAERS = GS_RBKP-WAERS."发票的货币码
          CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
            EXPORTING
              CURRENCY = T_WAERS
            IMPORTING
              FACTOR   = T_FACTOR
*           EXCEPTIONS
*             TOO_MANY_DECIMALS       = 1
*             OTHERS   = 2
            .
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.

        ENDIF.
        IF GS_RSEG-SHKZG EQ 'H'.
          GS_RSEG-WRBTR  =  GS_RSEG-WRBTR * -1.    "ADD IT02 20160202 借贷标识
        ENDIF.
        GS_RSEG_1-GJAHR = GS_RSEG-GJAHR.
        GS_RSEG_1-BELNR = GS_RSEG-BELNR.
        GS_RSEG_1-EBELN = GS_RSEG-EBELN.

*获取税率
        REFRESH T_FTAXP.
        CLEAR   S_FTAXP.
        CALL FUNCTION 'GET_TAX_PERCENTAGE'
          EXPORTING
            ALAND   = 'CN'
            DATAB   = SY-DATUM
            MWSKZ   = GS_RSEG-MWSKZ
            TXJCD   = ''
*           EXPORT  = ' '
          TABLES
            T_FTAXP = T_FTAXP.

*汇总发票行项目金额（含税）
        READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
        IF SY-SUBRC = 0.
          GS_RSEG_1-WRBTR = GS_RSEG-WRBTR * T_FACTOR + GS_RSEG-WRBTR * S_FTAXP-KBETR / 1000.
        ELSE.
          GS_RSEG_1-WRBTR = GS_RSEG-WRBTR * T_FACTOR .
        ENDIF.

        COLLECT GS_RSEG_1 INTO GT_RSEG_1.
        CLEAR GS_RSEG_1.
      ENDLOOP.

      SORT GT_RSEG_1 BY GJAHR BELNR EBELN.

*可申请金额需要减去已付金额
      READ TABLE GT_RSEG_1 INTO GS_RSEG_1
      WITH KEY GJAHR  = GS_DATA-GJAHR
               BELNR  = GS_DATA-BELNR
               EBELN  = GS_DATA-EBELN
               BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-FPPZJE_1 = GS_RSEG_1-WRBTR .   "发票凭证金额-凭证
        GS_DATA-ZSQFKJE = GS_RSEG_1-WRBTR  -  GS_DATA-YSQJE_AFP -  GS_DATA-YSQJE_TZFP -  GS_DATA-YSQJE_ADD - GS_DATA-YSQJE_TZDD. "默认申请付款金额
      ENDIF.

      "IT02 150708 begin add 发票对应的行文本
      READ TABLE GT_RBKP INTO GS_RBKP
      WITH KEY BELNR  = GS_DATA-BELNR
               GJAHR  = GS_DATA-GJAHR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_DATA-SGTXT = GS_RBKP-SGTXT.
        GS_DATA-FPPZJE = GS_DATA-FPPZJE_1 * GS_RBKP-KURSF .   "发票凭证金额-本币
      ENDIF.
      "IT02 150708 begin end
      LS_CELLTAB-FIELDNAME = 'ZFKXZ' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

      LS_CELLTAB-FIELDNAME = 'ZSQRQ' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

      LS_CELLTAB-FIELDNAME = 'ZSQFKRQ' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

      LS_CELLTAB-FIELDNAME = 'ZZY' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

      LS_CELLTAB-FIELDNAME = 'WAERS_2' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.

      GS_DATA-STATU = ICON_YELLOW_LIGHT.
      INSERT LINES OF LT_CELLTAB INTO TABLE GS_DATA-CELLTAB.

      APPEND GS_DATA TO GT_DATA.
      CLEAR GS_DATA.
      REFRESH LT_CELLTAB.

    ENDLOOP.

*删除重复的采购订单行
    DATA:GW_DATA TYPE TY_DATA.
    SORT GT_DATA BY BUKRS GJAHR BELNR EBELN.
    DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING BUKRS GJAHR BELNR EBELN.
*    "150527 begin 过滤 已付金额 大于等于应付金额的数据
*   IF X_WFK = 'X'.
*    LOOP AT GT_DATA INTO GS_DATA where  BELNR NE '' AND EBELN EQ ''.
*       CLEAR : GW_DATA.
*       LOOP AT GT_DATA INTO GW_DATA WHERE BELNR EQ GS_DATA-BELNR .
*             GS_DATA-YFJE = GS_DATA-YFJE + GW_DATA-YFJE . "汇总一个发票号下所有采购订单已付金额的总数
*       ENDLOOP.
*      READ TABLE GT_ZFI006 INTO GS_ZFI006 WITH KEY BELNR = GS_DATA-BELNR .
*      IF SY-SUBRC = 0 .
*        IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS . "若果已付金额的货币码等于 本币码 ，汇总的金额就和发票本币金额比较
*            IF GS_DATA-YFJE >= GS_DATA-FPPZJE .
*              DELETE GT_DATA WHERE BELNR = GS_DATA-BELNR.
*              ENDIF.
*        ELSE.                                         "若果已付金额的货币码不等于 本币码 ，汇总的金额就和发票凭金额数比较
*            IF GS_ZFI006-WAERS_2 = GS_DATA-WAERS_1 .
*            IF GS_DATA-YFJE >= GS_DATA-FPPZJE_1 .
*              DELETE GT_DATA WHERE BELNR = GS_DATA-BELNR.
*              ENDIF.
*
*          ENDIF.
*          ENDIF.
*        ENDIF.
*     ENDLOOP.
*    ENDIF.
*    "150527 end
    "150527 begin 读取发票到期日并赋值到ALV显示的内表中
    DATA: L2_ZSQFKJE TYPE ZFI006-ZSQFKJE.
    LOOP AT GT_DATA INTO GS_DATA WHERE EBELN = ''.
      READ TABLE GT_RBKP INTO GS_RBKP
      WITH KEY  BELNR = GS_DATA-BELNR
                GJAHR = GS_DATA-GJAHR
      BINARY SEARCH.  "根据发票凭证号查询GT_RBKP表读取
      IF SY-SUBRC  = 0 .
        IF S_BUDAT[] IS NOT INITIAL ."如果屏幕付款到期日不为空，FKDQ = ZFBDT ,若为空 FKDQ = ZFBDT + GS_RBKP-ZBD1T .
          GS_DATA-FKDQ = GS_RBKP-ZFBDT .
        ELSE.
          GS_DATA-FKDQ = GS_RBKP-ZFBDT + GS_RBKP-ZBD1T .
        ENDIF.
        MODIFY   GT_DATA FROM GS_DATA.
      ENDIF.
      CLEAR L2_ZSQFKJE  .
      CLEAR GS_DATA_2 .
      LOOP AT GT_DATA INTO GS_DATA_2 WHERE BELNR = GS_DATA-BELNR AND EBELN <> ''.
        L2_ZSQFKJE  = L2_ZSQFKJE  + GS_DATA_2-ZSQFKJE.
      ENDLOOP.
      GS_DATA-ZSQFKJE =  L2_ZSQFKJE  .
      MODIFY GT_DATA FROM GS_DATA.
    ENDLOOP.
                                                            "150527
  ENDIF.

*&--代码添加 BY HANDYBY 16.08.2017 22:06:09  BEGIN
  DATA: BEGIN OF LS_LFBK ,
          LIFNR TYPE LFBK-LIFNR,
          BANKS TYPE LFBK-BANKS,
          BANKL TYPE LFBK-BANKL,
          BANKN TYPE LFBK-BANKN,
          KOINH TYPE LFBK-KOINH,
        END OF LS_LFBK .
  DATA LT_LFBK LIKE TABLE OF LS_LFBK .
  DATA: BEGIN OF LS_BNKA ,
          BANKS TYPE BNKA-BANKS,
          BANKL TYPE BNKA-BANKL,
          BANKA TYPE BNKA-BANKA,
        END OF LS_BNKA .
  DATA LT_BNKA LIKE TABLE OF LS_BNKA .

  SELECT LIFNR
         BANKS
         BANKL
         BANKN
         KOINH
    INTO CORRESPONDING FIELDS OF TABLE LT_LFBK
    FROM LFBK
     FOR ALL ENTRIES IN GT_DATA
   WHERE LIFNR = GT_DATA-LIFNR .
  IF LT_LFBK IS NOT INITIAL .
    SELECT BANKS
           BANKL
           BANKA
      INTO CORRESPONDING FIELDS OF TABLE LT_BNKA
      FROM BNKA
       FOR ALL ENTRIES IN LT_LFBK
     WHERE BANKS = LT_LFBK-BANKS
       AND BANKL = LT_LFBK-BANKL .
  ENDIF.
  SORT LT_LFBK BY LIFNR .
  SORT LT_BNKA BY BANKL .

  LOOP AT GT_DATA INTO GS_DATA .
    READ TABLE LT_LFBK INTO LS_LFBK WITH KEY LIFNR = GS_DATA-LIFNR BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_DATA-BANKN = LS_LFBK-BANKN .
      GS_DATA-KOINH = LS_LFBK-KOINH .
      GS_DATA-BANKL = LS_LFBK-BANKL .
      READ TABLE LT_BNKA INTO LS_BNKA WITH KEY BANKL = LS_LFBK-BANKL BINARY SEARCH .
      IF SY-SUBRC = 0 .
        GS_DATA-BANKA = LS_BNKA-BANKA .
        CLEAR LS_BNKA .
      ENDIF.
      CLEAR LS_LFBK .
    ENDIF.
    MODIFY GT_DATA FROM GS_DATA .
    CLEAR GS_DATA .
  ENDLOOP.
*&--代码添加 BY HANDYBY 16.08.2017 22:06:09  END

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW .
  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_BUILD_EVENT.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA        = 'X'.
*  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  GW_LAYOUT-BOX_FNAME    = 'ZBOX'.
  GW_LAYOUT-STYLEFNAME   = 'CELLTAB'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'STATU'          '状态栏'                '' '' '' '' '' '' '' .
  INIT_FIELDCAT 'ZSQD'           '申请单号'              '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'              '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'          '供应商'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '供应商名称'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'          '采购组'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKNAM'          '采购组名'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '本位币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'        '凭证货币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR'          '采购订单含税金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR_1'        '采购订单含税金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE'          '已交货金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE_1'        '已交货金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '发票凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '发票会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPPZJE'         '发票凭证金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPPZJE_1'       '发票凭证金额'   '' '' '' '' '' '' ''.
  IF S_BELNR IS NOT INITIAL.
    INIT_FIELDCAT 'FKDQ'       '付款到期日'   '' '' '' '' '' '' ''.
  ENDIF.
  INIT_FIELDCAT 'YFJE'           '已付金额'   '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'YSQJE'          '已申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_ADD'      '已申请-按订单'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_AFP'      '已申请-按发票'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_TZDD'     '已申请-调整订单'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_TZFP'     '已申请-调整发票'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZFKXZ'          '付款性质'   '' '' '' 'X' ''  'ZFI006' 'ZFKXZ'.
  INIT_FIELDCAT 'ZSQFKJE'        '申请付款金额'   '' '' '' 'X' '' 'ZFI006' 'NETWR'.
  INIT_FIELDCAT 'WAERS_2'        '货币'   '6' '' '' 'X' '' 'EKKO' 'WAERS'.
  INIT_FIELDCAT 'ZSQRQ'          '申请日期'   '' '' '' 'X' '' 'ZFI006' 'ZSQRQ'.
  INIT_FIELDCAT 'ZSQFKRQ'        '申请付款日期'   '' '' '' 'X' '' 'ZFI006' 'ZSQFKRQ'.
  INIT_FIELDCAT 'ZZY'            '摘要'   '15' '' '' 'X' '' 'ZFI006' 'ZZY'.
  INIT_FIELDCAT 'ZCLJD'          '处理进度'   '6' '' '' '' '' 'ZFI006' 'ZCLJD' .
  INIT_FIELDCAT 'BELNR_F'        '付款会计凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR_F'        '付款会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '发票文本'   '15' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
  DATA L_ZSQD       TYPE ZFI006-ZSQD.          "查询当前的申请单
  DATA L_SUBRC TYPE C."检查输入项
  CLEAR L_ZSQD.
  CLEAR L_SUBRC.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.

*保存,保存的时候产生申请单号
    WHEN '&DATA_SAVE'.
      SORT GT_DATA BY BELNR EBELN .
      READ TABLE GT_DATA INTO GS_DATA WITH KEY ZFKXZ = '3' ZBOX = 'X' .
      IF SY-SUBRC = 0 .
        MESSAGE '此功能项不允许选择付款性质3-采购调整,请重新选择' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

*提示对话框
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION  = '是否执行保存操作'
        IMPORTING
          ANSWER         = G_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
      IF G_ANSWER <> '1'.
        EXIT.
      ENDIF.

*取出最大的申请单号
      SELECT * FROM ZFI006
        INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006_1.
      SORT GT_ZFI006_1 BY ZSQD  DESCENDING.
      READ TABLE GT_ZFI006_1 INTO GS_ZFI006_1 INDEX 1.

*当前日期
      IF GS_ZFI006_1-ZSQD+0(8) = SY-DATUM.
        L_ZSQD = GS_ZFI006_1-ZSQD.
      ENDIF.

      REFRESH GT_ZFI006_1.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变(排除申请金额为0的采购订单行)
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX     = 'X'
       AND   ZSQFKJE  <> '0'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQFKJE  <> '0'
         AND   BELNR NE ''.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

*检查必输项
      PERFORM CHECK_INPUT CHANGING L_SUBRC.
      IF L_SUBRC = '4'.
        EXIT.
      ENDIF.

*检查
      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0 .

*选择屏幕采购订单进入
        IF S_EBELN IS NOT INITIAL.

          LOOP AT GT_DATA INTO GS_DATA
          WHERE ZBOX = 'X'.

*形成申请单号 = 年月日+5位流水号
            AT NEW EBELN.
              IF L_ZSQD IS NOT INITIAL.
                L_ZSQD = L_ZSQD + 1.
              ELSE.
                CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
              ENDIF.
            ENDAT.

*付款申请单
            GS_DATA-ZSQD = L_ZSQD.

*更新屏幕
            MODIFY GT_DATA FROM GS_DATA.

            MOVE-CORRESPONDING GS_DATA TO GS_ZFI006_1.
            GS_ZFI006_1-ZNAME = SY-UNAME.
            GS_ZFI006_1-ZDATE = SY-DATUM.
            GS_ZFI006_1-ZTIME = SY-UZEIT.
*            GS_ZFI006_1-YSQJE = GS_ZFI006_1-YSQJE + GS_DATA-ZSQFKJE.

            APPEND GS_ZFI006_1 TO GT_ZFI006_1.
          ENDLOOP.

        ELSE.
          LOOP AT GT_DATA INTO GS_DATA
           WHERE ZBOX = 'X'.

*形成申请单号 = 年月日+5位流水号
            AT NEW BELNR.
              IF L_ZSQD IS NOT INITIAL.
                L_ZSQD = L_ZSQD + 1.
              ELSE.
                CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
              ENDIF.
            ENDAT.

*付款申请单
            GS_DATA-ZSQD = L_ZSQD.

*更新屏幕
            MODIFY GT_DATA FROM GS_DATA.

            MOVE-CORRESPONDING GS_DATA TO GS_ZFI006_1.
            GS_ZFI006_1-ZNAME = SY-UNAME.
            GS_ZFI006_1-ZDATE = SY-DATUM.
            GS_ZFI006_1-ZTIME = SY-UZEIT.
*            GS_ZFI006_1-YSQJE = GS_ZFI006_1-YSQJE + GS_DATA-ZSQFKJE.

            APPEND GS_ZFI006_1 TO GT_ZFI006_1.
          ENDLOOP.
        ENDIF.

*更新数据库表
        MODIFY  ZFI006 FROM TABLE GT_ZFI006_1.
        REFRESH GT_ZFI006_1.
        CLEAR GS_DATA.

*提示保存成功
        MESSAGE S002(Z001).

**更改到查看状态
*        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
*          EXPORTING
*            I_READY_FOR_INPUT = 0.

        CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&PRINT'.
      SORT GT_DATA BY BELNR EBELN .
      READ TABLE GT_DATA INTO GS_DATA
       WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        READ TABLE GT_DATA INTO GS_DATA
        WITH KEY ZSQD = ''
                 ZBOX = 'X'.
        IF SY-SUBRC = 0.
          MESSAGE S012(ZFICO01) DISPLAY LIKE 'E'.
        ELSE.
          IF S_BUKRS-LOW EQ '2110' OR S_BUKRS-LOW EQ '2100'.
            PERFORM FRM_PRINT_PN.
          ELSE.
            PERFORM FRM_PRINT.
          ENDIF.

        ENDIF.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.
ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  PERFORM FRM_DATA_ENTER USING ER_DATA_CHANGED..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA: G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL       TYPE LVC_S_STBL.
  DATA: WA_MOD_CELL TYPE LVC_S_MODI.
  DATA: LS_DATA     TYPE TY_DATA.
  DATA: L_ZSQFKJE   TYPE ZFI006-ZSQFKJE.
  DATA: L_STRING     TYPE CHAR100.

  FIELD-SYMBOLS:<L_MATNR> TYPE ANY.

  CLEAR L_ZSQFKJE.

  READ TABLE ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL INDEX 1.
  IF WA_MOD_CELL-FIELDNAME = 'ZSQFKJE'.

*汇总申请金额到抬头行
    READ TABLE GT_DATA INTO LS_DATA INDEX  WA_MOD_CELL-ROW_ID.
    IF SY-SUBRC = 0.

*去除金额的,号
      L_STRING = WA_MOD_CELL-VALUE.
      REPLACE ',' IN L_STRING WITH ''.
      L_ZSQFKJE  = L_STRING.

*汇总申请付款金额（汇总逻辑 =  修改的行 + 其他的行 （对于同一张发票而言））
      LOOP AT GT_DATA INTO GS_DATA
      WHERE BELNR = LS_DATA-BELNR
      AND   GJAHR = LS_DATA-GJAHR
      AND   EBELN <> ''
      AND   EBELN <> LS_DATA-EBELN.
        L_ZSQFKJE = L_ZSQFKJE + GS_DATA-ZSQFKJE.
      ENDLOOP.

      LOOP AT GT_DATA INTO GS_DATA
      WHERE BELNR = LS_DATA-BELNR
      AND   GJAHR = LS_DATA-GJAHR
      AND   EBELN = ''.
        GS_DATA-ZSQFKJE = L_ZSQFKJE.
        MODIFY GT_DATA FROM GS_DATA.
        CLEAR GS_DATA.
      ENDLOOP.
    ENDIF.
  ELSEIF  WA_MOD_CELL-FIELDNAME = 'WAERS_2'.

*    READ TABLE GT_DATA INTO LS_DATA INDEX  WA_MOD_CELL-ROW_ID.
*    IF SY-SUBRC = 0 .
*      TRANSLATE WA_MOD_CELL-VALUE  TO UPPER CASE.
*       IF WA_MOD_CELL-VALUE = 'RMB'.
*        MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
*         ENDIF.
*    ENDIF.
  ENDIF.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*自动定位光标
  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.


ENDFORM.                    "frm_data_enter
*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
*FORM TOP-OF-PAGE.
*
**ALV Header declarations
*  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
*        WA_HEADER     TYPE SLIS_LISTHEADER,
*        T_LINE        LIKE WA_HEADER-INFO,
*        LD_LINES      TYPE I,
*        LD_LINESC(10) TYPE C.
** Title
*  WA_HEADER-TYP  = 'H'.
*  WA_HEADER-INFO =  SY-TITLE."'装箱单维护打印平台'.
*  APPEND WA_HEADER TO T_HEADER.
*  CLEAR WA_HEADER.
** Date
*  WA_HEADER-TYP  = 'S'.
*  WA_HEADER-KEY = 'Date: '.
*  CONCATENATE  SY-DATUM+6(2) '.'
*               SY-DATUM+4(2) '.'
*               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
*  APPEND WA_HEADER TO T_HEADER.
*  CLEAR: WA_HEADER.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = T_HEADER.
*ENDFORM.                    "top-of-page
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT CHANGING L_SUBRC TYPE C.

*检查抬头
  IF S_EBELN IS NOT INITIAL .
    LOOP AT GT_DATA INTO GS_DATA
    WHERE ZBOX = 'X'.
      IF GS_DATA-WAERS_2 = 'RMB'.
        MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.
      IF GS_DATA-ZFKXZ IS INITIAL.
        MESSAGE '付款性质不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-WAERS_2 IS INITIAL.
        MESSAGE '货币不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-ZSQRQ IS INITIAL .
        MESSAGE '申请日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-ZSQFKRQ IS INITIAL .
        MESSAGE '申请付款日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT GT_DATA INTO GS_DATA
    WHERE ZBOX = 'X'
    AND EBELN = ''.
      IF GS_DATA-WAERS_2 = 'RMB'.
        MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.
      IF GS_DATA-ZFKXZ IS INITIAL.
        MESSAGE '付款性质不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-WAERS_2 IS INITIAL.
        MESSAGE '货币不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-ZSQRQ IS INITIAL .
        MESSAGE '申请日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.

      IF GS_DATA-ZSQFKRQ IS INITIAL .
        MESSAGE '申请付款日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
      ENDIF.
    ENDLOOP.
  ENDIF.

*检查不同重复保存
  LOOP AT GT_DATA INTO GS_DATA
  WHERE ZBOX = 'X'.
    IF GS_DATA-ZSQD IS NOT INITIAL.
      MESSAGE '该申请单已经保存，请勿重复操作，如需修改请进去修改功能' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 9,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI006'.
  DATA:L_PAGE  TYPE I.
  DATA:L_BUKRS TYPE BUTXT."公司代码描述

*查询打印的次数
  SELECT * FROM ZFI006_P
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006_P
    FOR ALL ENTRIES IN GT_DATA
    WHERE ZSQD = GT_DATA-ZSQD.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.


  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.

    AT NEW ZSQD .
      REFRESH LT_DATA.
    ENDAT.
    PERFORM CONV_AMOUNT USING GS_DATA-ZSQFKJE
                    CHANGING GS_DATA-ZSQFKJEDX.
    APPEND GS_DATA TO LT_DATA.

    AT END OF ZSQD.
*删除发票对应的采购订单行

      DELETE LT_DATA WHERE BELNR IS NOT INITIAL
                     AND   EBELN IS NOT INITIAL.

      READ TABLE LT_DATA INTO LS_DATA INDEX 1.

      SELECT SINGLE BUTXT FROM T001
       INTO L_BUKRS
       WHERE BUKRS = LS_DATA-BUKRS.

      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = LS_DATA-ZSQD.
      CLEAR L_PAGE.
      L_PAGE = GS_ZFI006_P-PRINT_NUM + 1.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          L_PAGE             = L_PAGE
          L_BUKRS            = L_BUKRS
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDAT.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

*当执行完打印返回参数
  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.
    REFRESH GT_ZFI006_P_1.

    LOOP AT GT_DATA INTO GS_DATA
    WHERE ZBOX = 'X'.
      GS_ZFI006_P_1-ZSQD = GS_DATA-ZSQD.

      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = GS_DATA-ZSQD.
      IF SY-SUBRC = 0.
        GS_ZFI006_P_1-PRINT_NUM = GS_ZFI006_P-PRINT_NUM + 1.
      ELSE.
        GS_ZFI006_P_1-PRINT_NUM = 1.
      ENDIF.

      APPEND GS_ZFI006_P_1 TO GT_ZFI006_P_1.
      CLEAR GS_ZFI006_P_1.
    ENDLOOP.
  ENDIF.

  MODIFY ZFI006_P FROM TABLE GT_ZFI006_P_1.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
                                                            "150508
FORM CONV_AMOUNT USING VALUE(F_SOURCE)
                 CHANGING VALUE(F_RESULT).
  DATA: SCR(30) TYPE C, RES(60) TYPE C,FEN(2) TYPE C .
  DATA: LEN TYPE I, C1 TYPE I, C2 TYPE I, C3 TYPE I, C4 TYPE I.
  DATA: D1(1) TYPE C, D2(1) TYPE C, D3 TYPE I.
  DATA: DIGIT(2)  TYPE C, WEIGHT(2) TYPE C.
  DATA: RULE1(20) TYPE C VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: RULE2(30) TYPE C VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  SCR = F_SOURCE * 100.
  CONDENSE SCR NO-GAPS.
  IF SCR = '0'.
    RES = '零元'.
  ELSE.
    LEN = STRLEN( SCR ).
    C1 = 0.
    D1 = '0'.
    CLEAR RES.
    DO LEN TIMES.
      C1 = C1 + 1.
      C2 = LEN - C1.
      D2 = SCR+C2(1) .
      IF D2 = '0'.
        D3 = 0.
      ELSE.
        D3 = D2.
      ENDIF.
      DIGIT = RULE1+D3(1) .
      C3 = ( C1 - 1 ) .
      WEIGHT = RULE2+C3(1) .
      IF D2 = '0'.
        IF C1 = 3.
          DIGIT = ''.
        ELSEIF C1 = 7.
          DIGIT = ''.
          IF LEN > 10 .
            C4 = LEN - 10.
            IF SCR+C4(4) = '0000'.
              WEIGHT = ''.
            ENDIF.
          ENDIF.
        ELSEIF C1 = 11.
          DIGIT = ''.
        ELSEIF D1 = '0'.
          DIGIT = ''.
          WEIGHT = ''.
        ELSE.
          WEIGHT = ''.
        ENDIF.
      ENDIF.
      CONCATENATE DIGIT WEIGHT RES INTO RES .
      D1 = D2.
    ENDDO.
  ENDIF.
  LEN = STRLEN( RES ) - 1.
  FEN = RES+LEN(1).
  IF FEN <> '分' .
    CONCATENATE RES '整' INTO F_RESULT.
  ELSE.
    F_RESULT = RES.
  ENDIF.
ENDFORM.                    "conv_amount
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_PN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_PN .
  REFRESH:GT_DATA_01.
  MOVE-CORRESPONDING GT_DATA TO GT_DATA_01 .

  DELETE GT_DATA_01 WHERE ZBOX NE 'X'.
  "删除发票号不为空、采购订单号不为空的记录
  DELETE GT_DATA_01 WHERE BELNR NE '' AND EBELN NE ''.
  "删除发票号为空、采购订单不为空的记录 。
  DELETE GT_DATA_01 WHERE BELNR EQ '' AND EBELN NE '' .
  SORT GT_DATA_01 BY ZSQD BELNR GJAHR .
  DELETE ADJACENT DUPLICATES FROM GT_DATA_01 COMPARING ZSQD BELNR GJAHR .

  IF GT_DATA_01 IS NOT INITIAL.

    PERFORM FRM_PRT_FP .     "按发票打印

  ENDIF.

  REFRESH:GT_DATA_02.
  MOVE-CORRESPONDING GT_DATA TO GT_DATA_02 .

  DELETE GT_DATA_02 WHERE ZBOX NE 'X'.

  "以采购订单付款维度打印

  DELETE GT_DATA_02 WHERE BELNR NE '' AND EBELN NE ''.

  DELETE GT_DATA_02 WHERE BELNR NE '' AND EBELN EQ '' .

  SORT GT_DATA_02 BY ZSQD EBELN .

  DELETE ADJACENT DUPLICATES FROM GT_DATA_02 COMPARING ZSQD EBELN .

  IF GT_DATA_02 IS NOT INITIAL.

    PERFORM FRM_PRT_CCGD .  "按采购订单打印

  ENDIF.









ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRT_FP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRT_FP .
  L_FORMNAME = 'ZSFFI006_PN_FP' .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  "统计发票申请：已付金额信息
  REFRESH:GT_DATA_FP .
  MOVE-CORRESPONDING GT_DATA TO GT_DATA_FP .

  "删除发票为空 、采购订单不为空的数据
  DELETE GT_DATA_FP WHERE BELNR EQ '' AND EBELN NE ''.

  "删除发票不为空 、采购订单为空的数据
  DELETE GT_DATA_FP WHERE BELNR NE '' AND EBELN EQ '' .

  "gt_pn_fp_mx：只包括按发票申请 的 采购明细行
  REFRESH:GT_PN_FP_MX.
  MOVE-CORRESPONDING GT_DATA_FP TO GT_PN_FP_MX.

  SORT GT_PN_FP_MX BY ZSQD BELNR GJAHR EBELN  .

  "统计发票明细行的合计数 ：按申请单号、发票号、年度 维度汇总
  REFRESH:GT_PN_FP_HJ.
  LOOP  AT GT_PN_FP_MX INTO GS_PN_FP_MX.
    CLEAR:GS_PN_FP_HJ.
    GS_PN_FP_HJ-ZSQD = GS_PN_FP_MX-ZSQD.
    GS_PN_FP_HJ-BELNR = GS_PN_FP_MX-BELNR.
    GS_PN_FP_HJ-GJAHR = GS_PN_FP_MX-GJAHR.
    GS_PN_FP_HJ-WAERS = GS_PN_FP_MX-WAERS.
    GS_PN_FP_HJ-YFJE = GS_PN_FP_MX-YFJE.
    COLLECT GS_PN_FP_HJ INTO GT_PN_FP_HJ.
  ENDLOOP.
  SORT GT_PN_FP_HJ BY  ZSQD BELNR GJAHR  .

  IF GT_DATA_01 IS NOT INITIAL.
    REFRESH:GT_RBKP_01.
    MOVE-CORRESPONDING GT_RBKP TO GT_RBKP_01 .
    SORT GT_RBKP_01 BY BELNR GJAHR  .
    REFRESH:GT_RSEG_01 .
    MOVE-CORRESPONDING GT_RSEG TO GT_RSEG_01 .
    SORT GT_RSEG_01 BY BELNR GJAHR  XBLNR EBELN EBELP  .
    REFRESH:GT_BELNR.
    MOVE-CORRESPONDING GT_DATA_01 TO GT_BELNR .
    DELETE GT_BELNR WHERE BELNR EQ ''.
    SORT GT_BELNR BY BELNR GJAHR .
    DELETE ADJACENT DUPLICATES FROM GT_BELNR COMPARING ALL FIELDS .
    "统计按 发票号、入库单、采购订单、采购订单行号汇总数量 、金额
    REFRESH:GT_PN_HJ,GT_PN_HJ .
    LOOP AT GT_BELNR INTO GS_BELNR.
      CLEAR:T_WAERS , T_FACTOR .
      READ TABLE GT_RBKP_01 INTO GS_RBKP_01
      WITH KEY BELNR = GS_DATA-BELNR
               GJAHR = GS_DATA-GJAHR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        T_WAERS = GS_RBKP_01-WAERS."发票的货币码
        CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
          EXPORTING
            CURRENCY = T_WAERS
          IMPORTING
            FACTOR   = T_FACTOR
*           EXCEPTIONS
*           TOO_MANY_DECIMALS       = 1
*           OTHERS   = 2
          .
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDIF.

      LOOP AT  GT_RSEG_01 INTO GS_RSEG_01 WHERE BELNR = GS_BELNR-BELNR AND GJAHR = GS_BELNR-GJAHR .
        CLEAR:GS_PN_HJ .
        GS_PN_HJ-BELNR = GS_RSEG_01-BELNR.
        GS_PN_HJ-GJAHR = GS_RSEG_01-GJAHR.
        GS_PN_HJ-XBLNR = GS_RSEG_01-XBLNR.
        GS_PN_HJ-EBELN = GS_RSEG_01-EBELN.
        GS_PN_HJ-EBELP = GS_RSEG_01-EBELP.
        GS_PN_HJ-BSTME =  GS_RSEG_01-BSTME.
        IF GS_RSEG_01-SHKZG EQ 'S'.
          GS_PN_HJ-RKSL = GS_RSEG_01-MENGE .
          GS_PN_HJ-HSJE = GS_RSEG_01-WRBTR .
        ELSE.
          GS_PN_HJ-RKSL = GS_RSEG_01-MENGE * -1 .
          GS_PN_HJ-HSJE = GS_RSEG_01-WRBTR * -1 .
        ENDIF.

        "  *获取税率
        REFRESH T_FTAXP.
        CLEAR   S_FTAXP.
        CALL FUNCTION 'GET_TAX_PERCENTAGE'
          EXPORTING
            ALAND   = 'CN'
            DATAB   = SY-DATUM
            MWSKZ   = GS_RSEG_01-MWSKZ
            TXJCD   = ''
*           EXPORT  = ' '
          TABLES
            T_FTAXP = T_FTAXP.

*汇总发票行项目金额（含税）
        READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
        IF SY-SUBRC = 0.
          GS_PN_HJ-HSJE = GS_PN_HJ-HSJE *  T_FACTOR + GS_PN_HJ-HSJE * S_FTAXP-KBETR / 1000.
        ELSE.
          GS_PN_HJ-HSJE = GS_PN_HJ-HSJE *  T_FACTOR .
        ENDIF.
        COLLECT GS_PN_HJ INTO GT_PN_HJ .
      ENDLOOP.
    ENDLOOP.
    DELETE GT_PN_HJ WHERE RKSL EQ 0 .
    SORT GT_PN_HJ BY BELNR GJAHR .

    REFRESH:GT_T042Z.
    SELECT * INTO TABLE GT_T042Z
      FROM T042Z
      WHERE LAND1 = 'CN'.
    SORT GT_T042Z BY ZLSCH .

*查询打印的次数
    REFRESH:GT_ZFI006_P.
    SELECT * FROM ZFI006_P
      INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006_P
      FOR ALL ENTRIES IN GT_DATA_01
      WHERE ZSQD = GT_DATA_01-ZSQD.

    SORT GT_ZFI006_P BY ZSQD .

    LOOP AT GT_DATA_01  INTO GS_DATA_01 .
      CLEAR:GS_PN_HD,G_XH .
      REFRESH:GT_PN_HD .
      GS_PN_HD-ZSQD = GS_DATA_01-ZSQD .
      GS_PN_HD-BUKRS = GS_DATA_01-BUKRS.
      READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = GS_DATA_01-BUKRS
                                      BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_PN_HD-BUTXT = GS_T001-BUTXT .
      ENDIF.
      GS_PN_HD-BELNR = GS_DATA_01-BELNR .
      GS_PN_HD-GJAHR = GS_DATA_01-GJAHR .
      READ TABLE GT_RBKP_01 INTO GS_RBKP_01 WITH KEY BELNR = GS_PN_HD-BELNR
                                                     GJAHR = GS_PN_HD-GJAHR
                                             BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_PN_HD-ZKDQR = GS_RBKP_01-ZFBDT . "账款到期日
        "净值
        GS_PN_HD-JZ = GS_RBKP_01-RMWWR - GS_RBKP_01-WMWST1 .
        "金税发票号
        GS_PN_HD-XBLNR = GS_RBKP_01-XBLNR.
        "发票日期
        GS_PN_HD-BLDAT = GS_RBKP_01-BLDAT .
        "税额
        GS_PN_HD-WMWST1 = GS_RBKP_01-WMWST1 .
        "供应商编码
        GS_PN_HD-LIFNR = GS_RBKP_01-LIFNR .
        "供应商名称
        READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_PN_HD-LIFNR
                                        BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          GS_PN_HD-NAME1 = GS_LFA1-NAME1.
        ENDIF.
        "银行信息
        CLEAR:GS_PN_HD-BANKN,GS_PN_HD-BANKL,GS_PN_HD-BANKS,GS_PN_HD-BANKA,GS_PN_HD-KOINH .

        SELECT SINGLE BANKN BANKL BANKS KOINH INTO ( GS_PN_HD-BANKN , GS_PN_HD-BANKL ,GS_PN_HD-BANKS,GS_PN_HD-KOINH ) FROM LFBK WHERE LIFNR = GS_PN_HD-LIFNR .

        IF GS_PN_HD-KOINH IS INITIAL.
          GS_PN_HD-KOINH  =   GS_PN_HD-BANKN  .
        ENDIF.
        SELECT SINGLE BANKA INTO GS_PN_HD-BANKA FROM BNKA WHERE BANKS = GS_PN_HD-BANKS AND BANKL = GS_PN_HD-BANKL .
        "税价合计
        GS_PN_HD-RMWWR = GS_RBKP_01-RMWWR .
        "付款条件
        GS_PN_HD-ZLSCH = GS_RBKP_01-ZLSCH .
        "付款条件描述
        READ TABLE GT_T042Z INTO GS_T042Z WITH KEY ZLSCH = GS_PN_HD-ZLSCH
                                          BINARY SEARCH.
        IF SY-SUBRC EQ 0 .
          GS_PN_HD-FKMS = GS_T042Z-TEXT1.

        ENDIF.
      ENDIF.
      "已付金额
      " gs_pn_hd-yfje  = gs_data_01-yfje  .
      READ TABLE  GT_PN_FP_HJ INTO GS_PN_FP_HJ WITH KEY  ZSQD = GS_PN_HD-ZSQD
                                                         BELNR = GS_PN_HD-BELNR
                                                         GJAHR = GS_PN_HD-GJAHR
                                               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_PN_HD-YFJE = GS_PN_FP_HJ-YFJE .
      ENDIF.
      "未付
      GS_PN_HD-WFJE  = GS_PN_HD-RMWWR -  GS_PN_HD-YFJE .
      "本次申请金额
      GS_PN_HD-ZSQFKJE = GS_DATA_01-ZSQFKJE .
      "大写
      PERFORM CONV_AMOUNT USING GS_DATA_01-ZSQFKJE
                   CHANGING GS_PN_HD-ZSQFKJEDX.

      "货币码
      GS_PN_HD-WAERS = GS_DATA_01-WAERS_2 .

      "摘要
      GS_PN_HD-ZZY = GS_DATA_01-ZZY .

      APPEND GS_PN_HD TO GT_PN_HD .

      REFRESH:GT_PN_MX .
      LOOP AT GT_PN_HJ INTO GS_PN_HJ WHERE BELNR = GS_PN_HD-BELNR
                                       AND  GJAHR = GS_PN_HD-GJAHR.
        CLEAR:GS_PN_MX .
        G_XH  = G_XH  + 1 .

        GS_PN_MX-XH    = G_XH .                 "序号
        GS_PN_MX-BELNR = GS_PN_HJ-BELNR .
        GS_PN_MX-GJAHR = GS_PN_HJ-GJAHR .
        GS_PN_MX-XBLNR = GS_PN_HJ-XBLNR .
        GS_PN_MX-EBELN = GS_PN_HJ-EBELN .
        READ TABLE GT_EKPO INTO GS_EKPO WITH KEY EBELN = GS_PN_HJ-EBELN
                                                 EBELP = GS_PN_HJ-EBELP
                                         BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          GS_PN_MX-MATNR = GS_EKPO-MATNR .
          GS_PN_MX-TXZ01 = GS_EKPO-TXZ01 .
        ENDIF.
        "入库数量
        GS_PN_MX-RKSL = GS_PN_HJ-RKSL .

        "单位
        GS_PN_MX-BSTME = GS_PN_HJ-BSTME .
        "含税金额
        GS_PN_MX-HSJE = GS_PN_HJ-HSJE .

        "含税单价
        GS_PN_MX-HSDJ = GS_PN_MX-HSJE /   GS_PN_MX-RKSL .


        APPEND GS_PN_MX TO GT_PN_MX .
      ENDLOOP.

      CLEAR L_PAGE .
      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = GS_DATA_01-ZSQD.
      IF SY-SUBRC EQ 0 .

        L_PAGE = GS_ZFI006_P-PRINT_NUM + 1.
      ELSE.
        L_PAGE = 1 .
      ENDIF.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          L_PAGE             = L_PAGE
          L_BUKRS            = L_BUKRS
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.

      ENDIF.


    ENDLOOP.


  ENDIF.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

*当执行完打印返回参数
  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.
    REFRESH GT_ZFI006_P_1.

    LOOP AT  GT_DATA_01  INTO GS_DATA_01 .
      CLEAR:GS_ZFI006_P .
      GS_ZFI006_P_1-ZSQD = GS_DATA_01-ZSQD.

      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = GS_DATA_01-ZSQD.
      IF SY-SUBRC = 0.
        GS_ZFI006_P_1-PRINT_NUM = GS_ZFI006_P-PRINT_NUM + 1.
      ELSE.
        GS_ZFI006_P_1-PRINT_NUM = 1.
      ENDIF.

      APPEND GS_ZFI006_P_1 TO GT_ZFI006_P_1.
      CLEAR GS_ZFI006_P_1.
    ENDLOOP.
  ENDIF.

  MODIFY ZFI006_P FROM TABLE GT_ZFI006_P_1.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRT_CCGD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRT_CCGD .

  L_FORMNAME = 'ZSFFI006_PN_CG' .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.



  IF GT_DATA_02 IS NOT INITIAL .
    "取付款条件描述
    REFRESH:GT_T052U.
    SELECT * INTO TABLE GT_T052U
      FROM T052U
      WHERE  SPRAS = SY-LANGU .

    SORT GT_T052U BY ZTERM .
    "取采购订单抬头数据
    REFRESH:GT_PN_EKKO .
    SELECT * INTO TABLE GT_PN_EKKO
      FROM EKKO
      FOR  ALL  ENTRIES IN GT_DATA_02
      WHERE EBELN = GT_DATA_02-EBELN.

    SORT GT_PN_EKKO BY EBELN .
    "取条件1 的值
    REFRESH:GT_KONV_1.
    SELECT * INTO TABLE GT_KONV_1
      FROM KONV
      FOR ALL ENTRIES IN GT_PN_EKKO
      WHERE KNUMV = GT_PN_EKKO-KNUMV
      AND STUNR = '1'.
    SORT GT_KONV_1 BY KNUMV.

    "取条件9 的值
    REFRESH:GT_KONV_9.
    SELECT * INTO TABLE GT_KONV_9
        FROM KONV
        FOR ALL ENTRIES IN GT_PN_EKKO
        WHERE KNUMV = GT_PN_EKKO-KNUMV
        AND STUNR = '9'.
    SORT GT_KONV_9 BY KNUMV KPOSN.

    IF GT_PN_EKKO IS NOT INITIAL .
      REFRESH:GT_KONV_HZ .
      LOOP AT GT_KONV_1 INTO GS_KONV_1 .
        CLEAR:GS_KONV_HZ.
        GS_KONV_HZ-KNUMV = GS_KONV_1-KNUMV .
        GS_KONV_HZ-SJHJ = GS_KONV_1-KWERT.  "税价
        READ TABLE GT_KONV_9 INTO GS_KONV_9 WITH KEY KNUMV = GS_KONV_1-KNUMV
                                                     KPOSN = GS_KONV_1-KPOSN
                                             BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          GS_KONV_9-KBETR = GS_KONV_9-KBETR / 1000 .
          GS_KONV_HZ-SER = GS_KONV_HZ-SJHJ / ( 1 + GS_KONV_9-KBETR ) * GS_KONV_9-KBETR . "税额
        ENDIF.
        COLLECT GS_KONV_HZ INTO GT_KONV_HZ .
      ENDLOOP.
      SORT GT_KONV_HZ BY KNUMV .
    ENDIF.



    LOOP AT GT_DATA_02 INTO GS_DATA_02 .
      REFRESH:GT_PN_CGDD .
      CLEAR:GS_PN_CGDD.
      GS_PN_CGDD-BUKRS = GS_DATA_02-BUKRS.
      READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = GS_DATA_02-BUKRS
                                    BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_PN_CGDD-BUTXT = GS_T001-BUTXT .
      ENDIF.
      GS_PN_CGDD-ZSQD = GS_DATA_02-ZSQD .  "申请单
      GS_PN_CGDD-EBELN = GS_DATA_02-EBELN . "采购订单
      READ TABLE GT_PN_EKKO  INTO GS_PN_EKKO WITH KEY  EBELN = GS_DATA_02-EBELN
                                             BINARY SEARCH.
      IF SY-SUBRC EQ 0 .
        GS_PN_CGDD-LIFNR = GS_PN_EKKO-LIFNR . "供应商编码
        READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_PN_EKKO-LIFNR
                                          BINARY SEARCH.
        IF SY-SUBRC EQ 0 .
          GS_PN_CGDD-NAME1 = GS_LFA1-NAME1 .    "供应商名称
        ENDIF.
        CLEAR:GS_PN_CGDD-BANKN,GS_PN_CGDD-BANKL,GS_PN_CGDD-BANKS,GS_PN_CGDD-BANKA,GS_PN_CGDD-KOINH .
        SELECT SINGLE BANKN BANKL BANKS KOINH INTO ( GS_PN_CGDD-BANKN , GS_PN_CGDD-BANKL ,GS_PN_CGDD-BANKS ,GS_PN_CGDD-KOINH ) FROM LFBK WHERE LIFNR = GS_PN_CGDD-LIFNR .
        IF GS_PN_CGDD-KOINH IS INITIAL.
          GS_PN_CGDD-KOINH  = GS_PN_CGDD-BANKN.
        ENDIF.
        SELECT SINGLE BANKA INTO GS_PN_CGDD-BANKA FROM BNKA WHERE BANKS = GS_PN_CGDD-BANKS AND BANKL = GS_PN_CGDD-BANKL .
        "货币码
        GS_PN_CGDD-WAERS = GS_PN_EKKO-WAERS .
        READ TABLE GT_KONV_HZ INTO GS_KONV_HZ WITH KEY KNUMV =  GS_PN_EKKO-KNUMV
                                              BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          GS_PN_CGDD-SJHJ = GS_KONV_HZ-SJHJ .  "税价合计
          GS_PN_CGDD-SER = GS_KONV_HZ-SER .   "税额
          GS_PN_CGDD-JZ =    GS_PN_CGDD-SJHJ - GS_PN_CGDD-SER ."净值

        ENDIF.
        "付款条件
        GS_PN_CGDD-ZTERM = GS_PN_EKKO-ZTERM.

        READ TABLE GT_T052U INTO GS_T052U WITH KEY ZTERM = GS_PN_CGDD-ZTERM
                                          BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          GS_PN_CGDD-FKMS = GS_T052U-TEXT1.
        ENDIF.
      ENDIF.
      "已付金额
      GS_PN_CGDD-YFJE  = GS_DATA_02-YFJE  .
      "未付
      GS_PN_CGDD-WFJE  = GS_PN_CGDD-SJHJ -  GS_PN_CGDD-YFJE .
      "本次申请金额
      GS_PN_CGDD-ZSQFKJE = GS_DATA_02-ZSQFKJE .
      "大写
      PERFORM CONV_AMOUNT USING GS_DATA_02-ZSQFKJE
                   CHANGING GS_PN_CGDD-ZSQFKJEDX.
      "大写

      PERFORM CONV_AMOUNT USING GS_DATA_02-ZSQFKJE
                   CHANGING GS_PN_HD-ZSQFKJEDX.

      "摘要
      GS_PN_CGDD-ZZY = GS_DATA_02-ZZY .

      APPEND GS_PN_CGDD TO GT_PN_CGDD .

      CLEAR L_PAGE .
      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = GS_DATA_02-ZSQD.
      IF SY-SUBRC EQ 0 .

        L_PAGE = GS_ZFI006_P-PRINT_NUM + 1.
      ELSE.
        L_PAGE = 1 .
      ENDIF.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          L_PAGE             = L_PAGE
          L_BUKRS            = L_BUKRS
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.

      ENDIF.

    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

*当执行完打印返回参数
  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.
    REFRESH GT_ZFI006_P_1.

    LOOP AT  GT_DATA_02  INTO GS_DATA_02 .
      CLEAR:GS_ZFI006_P .
      GS_ZFI006_P_1-ZSQD = GS_DATA_02-ZSQD.

      READ TABLE GT_ZFI006_P INTO GS_ZFI006_P
      WITH KEY ZSQD = GS_DATA_02-ZSQD.
      IF SY-SUBRC = 0.
        GS_ZFI006_P_1-PRINT_NUM = GS_ZFI006_P-PRINT_NUM + 1.
      ELSE.
        GS_ZFI006_P_1-PRINT_NUM = 1.
      ENDIF.

      APPEND GS_ZFI006_P_1 TO GT_ZFI006_P_1.
      CLEAR GS_ZFI006_P_1.
    ENDLOOP.
  ENDIF.

  MODIFY ZFI006_P FROM TABLE GT_ZFI006_P_1.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
