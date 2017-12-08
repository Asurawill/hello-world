REPORT ZFI017_1.
*&---------------------------------------------------------------------*
*& Report  ZFI017
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/6
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&不知道是不是因为是局部变量的原因，所以它没传过去
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:EKKO,RBKP,ZFI017,PRPS.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        STATU     TYPE   ICONNAME,       "状态栏
        EBELN     TYPE   EKKO-EBELN,     "采购订单号
*        EBELP     TYPE   EKPO-EBELP,     " 13074
        BELNR     TYPE   RBKP-BELNR,     "发票凭证
        GJAHR     TYPE   RBKP-GJAHR,     "发票会计年度
        ZSQD      TYPE   ZFI017-ZSQD,    "付款申请单
        BUKRS     TYPE   EKKO-BUKRS,     "公司代码
        LIFNR     TYPE   EKKO-LIFNR,     "供应商
        NAME1     TYPE   LFA1-NAME1,     "供应商名称
        LIFN2     TYPE   LFA1-LIFNR,     "供应商开票方
        NAME2     TYPE   LFA1-NAME1,     "开票方供应商描述
        NETWR     TYPE   EKPO-NETWR,     "采购订单总价
        WAERS     TYPE   EKKO-WAERS,     "货币单位
        NETWR_1   TYPE   EKPO-NETWR,     "采购订单总价（外币）
        YJHJE     TYPE   EKBE-DMBTR,     "已交货金额(净值)'
        YJHJE_1   TYPE   EKBE-DMBTR,     "已交货金额(外币)
        FPPZJE    TYPE   RBKP-RMWWR,     "发票总金额
        FPPZJE_1  TYPE   RBKP-RMWWR,     "发票总金额
        YFJE      TYPE   EKBE-DMBTR,     "已付金额
        ZFKXZ     TYPE   ZFI017-ZFKXZ,   "付款性质
        ZSQFKJE   TYPE   ZFI017-ZSQFKJE, "申请付款金额
        ZSQFKJEDX TYPE   C LENGTH 150,   "申请付款金额大写
        WAERS_2   TYPE   ZFI017-WAERS_2, "货币
        ZSQRQ     TYPE   ZFI017-ZSQRQ,   "申请日期
        ZSQFKRQ   TYPE   ZFI017-ZSQFKRQ, "申请付款日期
        ZZY       TYPE   ZFI017-ZZY,     "摘要
        ZCLJD     TYPE   ZFI017-ZCLJD,   "处理进度
        BELNR_F   TYPE   ZFI017-BELNR_F, "付款会计凭证
        GJAHR_F   TYPE   ZFI017-GJAHR_F, "付款会计年度
        WAERS_1   TYPE   EKKO-WAERS,     "货币单位
        CELLTAB   TYPE   LVC_T_STYL,     "控制单元格属性
        FKDQ      TYPE   SY-DATUM ,      "付款到期日IT02150601
        SGTXT     TYPE   RBKP-SGTXT,     "发票抬头文本  IT02 150709
        YSQJE     TYPE   ZFI017-YSQJE,   "已申请金额
        EKGRP     TYPE   LFM1-EKGRP,     "采购组
        EKNAM     TYPE   T024-EKNAM,     "采购组
*&--代码注释 BY HANDYBY 27.06.2017 21:21:56  BEGIN
*    POSID     TYPE   ZFI017-POSID,   "项目WBS
*        POST1     TYPE   ZFI017-POST1,   "项目描述
*&--代码注释 BY HANDYBY 27.06.2017 21:21:56  END
        MWSKZ     TYPE   ZFI017-MWSKZ,   "是否含税
        ZTERM     TYPE   ZFI017-ZTERM,   "子付款条件
        TEXT1     TYPE   ZFI017-TEXT1,   "子付款条件描述
        RATPZ     TYPE   ZFI017-RATPZ,   "应付比例
        YFJE1     TYPE   ZFI017-YFJE1,   "应付金额
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  BEGIN
        YJFKRQ    TYPE EKKO-YJFKRQ1,
        YJFKJE    TYPE EKKO-YJFKJE1,
*        YJFKRQ2   TYPE EKKO-YJFKRQ2,
*        YJFKJE2   TYPE EKKO-YJFKJE2,
*        YJFKRQ3   TYPE EKKO-YJFKRQ3,
*        YJFKJE3   TYPE EKKO-YJFKJE3,
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  END
        YFBL      TYPE   ZFI017-YFBL,    "已付比例
        WFJE      TYPE   ZFI017-WFJE,    "未付金额
        HTBH      TYPE   STRING ,        "合同编号
        ZCH       TYPE   ZFI017-ZCH,     "拆行
        FPJE      TYPE   EKBE-WRBTR,     "发票金额（净值）
        FPJE_1    TYPE   EKBE-WRBTR,     "发票金额（含税）
        YZFP      TYPE   EKBE-WRBTR,     "预制发票（净值）
        YZFP_1    TYPE   EKBE-WRBTR,      "预制发票（含税）
        JHJE_1    TYPE   EKBE-WRBTR,     "交货金额（含税）
        ZBOX      TYPE   C,
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  BEGIN
        POSID     TYPE PRPS-POSID , "WBS元素外码
        POST1     TYPE PRPS-POST1 , "WBS描述
        PSPID     TYPE PROJ-PSPID , "项目定义
        POST2     TYPE PROJ-POST1 , "项目描述
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  END

*&--代码添加 BY HANDHJD 11.07.2017   BEGIN
        BXQX      TYPE C LENGTH 20,
        BXQSRQ    TYPE ZFI017-ZSQRQ,
        YEAR      TYPE C LENGTH 4, "保修年
        MONTH     TYPE C LENGTH 2,  "保修月
        DAY       TYPE C LENGTH 2, "保修日

        YEAR2     TYPE C LENGTH 4, "保修年
        FPHM      TYPE C LENGTH 20,           "发票号码
*&--代码添加 BY HANDHJD 11.07.2017   END
      END OF TY_DATA.

*&--代码添加 BY HANDHJD 12.07.2017   BEGIN
TYPES:
  BEGIN OF TY_BSEG,
    EBELN TYPE BSEG-EBELN,
    BUKRS TYPE BSEG-BUKRS,
    BELNR TYPE BSEG-BELNR,
    BUZID TYPE BSEG-BUZID,
    ZUONR TYPE BSEG-ZUONR,
  END OF TY_BSEG.
DATA:
  GT_BSEG TYPE TABLE OF TY_BSEG,
  GS_BSEG LIKE LINE  OF GT_BSEG.

DATA:
  GT_BSEG_B TYPE TABLE OF TY_BSEG,
  GS_BSEG_B LIKE LINE OF GT_BSEG_B.
*&--代码添加 BY HANDHJD 12.07.2017   END
TYPES:BEGIN OF TY_EKKO,
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        BUKRS TYPE   EKKO-BUKRS,     "公司代码
        LIFNR TYPE   EKKO-LIFNR,     "供应商
        WAERS TYPE   EKKO-WAERS,     "货币单位
      END OF TY_EKKO.

TYPES:BEGIN OF TY_EKPO,
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        EBELP TYPE   EKPO-EBELP,     "采购订单行项目
        BUKRS TYPE   EKKO-BUKRS,     "公司代码
        LIFNR TYPE   EKKO-LIFNR,     "供应商
        NETWR TYPE   EKPO-NETWR,     "采购订单总价
        WAERS TYPE   EKKO-WAERS,     "货币单位
      END OF TY_EKPO.

TYPES:BEGIN OF TY_RSEG,
        GJAHR TYPE RSEG-GJAHR,       "发票年度
        BELNR TYPE RSEG-BELNR,       "发票号
        EBELN TYPE RSEG-EBELN,       "采购订单号
        WRBTR TYPE RSEG-WRBTR,       "采购订单金额
      END OF TY_RSEG.

TYPES:BEGIN  OF TY_DELBELNR,
        BELNR TYPE RSEG-BELNR,       "发票凭证号
      END OF TY_DELBELNR.

"采购组名称
TYPES:BEGIN OF TY_LFM1 ,
        LIFNR TYPE LFM1-LIFNR, "供应商号
        EKORG TYPE LFM1-EKORG, "采购组
        EKGRP TYPE LFM1-EKGRP, "采购组
        EKNAM TYPE T024-EKNAM, "采购组ming
      END OF  TY_LFM1 .

"发票合计
TYPES:BEGIN OF TY_FP,
        EBELN TYPE EBELN,
        MWSKZ TYPE MWSKZ,
        WRBTR TYPE EKBE-WRBTR,
      END OF TY_FP.

TYPES:BEGIN OF TY_JH,
        EBELN TYPE EBELN,
        WRBTR TYPE WRBTR,
      END OF TY_JH.

TYPES:BEGIN OF TY_SE,
        MWSKZ TYPE EKBE-MWSKZ,
        KBETR TYPE FTAXP-KBETR,
      END OF TY_SE.
* -------------- 13074-------
TYPES:BEGIN OF TY_EKPO1,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        TXZ01 TYPE EKPO-TXZ01,
      END OF TY_EKPO1.
*   --------------------------

DATA:GT_SE_1 TYPE TABLE OF TY_SE,
     GT_SE_2 TYPE TABLE OF TY_SE,
     GT_SE   TYPE TABLE OF TY_SE,
     GS_SE   TYPE TY_SE.

DATA:GT_FP_2 TYPE TABLE OF TY_FP,
     GS_FP_2 TYPE TY_FP.

DATA:GT_FP_P TYPE TABLE OF TY_FP,
     GS_FP_P TYPE TY_FP.

DATA:GT_JH TYPE TABLE OF TY_JH,
     GS_JH TYPE TY_JH.

*---------------13074 -----------

DATA : GT_NORD TYPE TABLE OF ZNEWORDER,
       GS_NORD TYPE ZNEWORDER.

*--------------------------------

DATA: GT_FTAXP LIKE  TABLE OF FTAXP .
DATA: GS_FTAXP LIKE FTAXP.
*DATA : T_LFM1 TYPE TABLE OF TY_LFM1 WITH HEADER LINE.

DATA:GS_DELBELNR TYPE  TY_DELBELNR .
DATA:GT_DELBELNR TYPE TABLE OF TY_DELBELNR.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI017    TYPE TABLE OF ZFI017.
DATA GS_ZFI017    TYPE ZFI017.

DATA GT_ZFI017_1  TYPE TABLE OF ZFI017.
DATA GS_ZFI017_1  TYPE ZFI017.

DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.
DATA GS_DATA_2 TYPE TY_DATA.

"HANDLH ADD
DATA:GT_DATA3 TYPE TABLE OF TY_DATA.



DATA GT_RSEG     TYPE TABLE OF RSEG.
DATA GS_RSEG     TYPE RSEG.
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

DATA:GT_EKBE_2 TYPE TABLE OF EKBE,
     GS_EKBE_2 TYPE EKBE.

DATA:GT_EKBE_P TYPE TABLE OF EKBE,
     GS_EKBE_P TYPE EKBE.

DATA:GT_EKBE_1 TYPE TABLE OF EKBE,
     GS_EKBE_1 TYPE EKBE.


DATA GT_RBKP   TYPE TABLE OF RBKP.
DATA GS_RBKP   TYPE RBKP.

DATA GT_LFA1   TYPE TABLE OF LFA1.
DATA GS_LFA1   TYPE LFA1.

DATA GT_LFA2   TYPE TABLE OF LFA1.
DATA GS_LFA2   TYPE LFA1.

DATA GT_T001   TYPE TABLE OF T001.
DATA GS_T001   TYPE T001.

DATA GT_KONV   TYPE TABLE OF KONV.
DATA GS_KONV   TYPE KONV.

DATA GT_ZFI017_P TYPE TABLE OF ZFI017_P.
DATA GS_ZFI017_P TYPE ZFI017_P.

DATA GT_ZFI017_P_1 TYPE TABLE OF ZFI017_P.
DATA GS_ZFI017_P_1 TYPE ZFI017_P.

*  ------------------------ 13074 ---------------

*DATA LT_DATA  TYPE TABLE OF TY_DATA.
*DATA LS_DATA  TYPE TY_DATA.

*   -------------------------------------------

*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
* -------------------------- 13074 ---------------
DATA: LT_DATA1 TYPE TABLE OF TY_DATA,
      LS_DATA1 LIKE LINE OF LT_DATA1.
* --------------------------------------------

FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA.

TYPES: BEGIN OF TY_EBELN,
         EBELN TYPE EKKO-EBELN,
       END OF TY_EBELN.
DATA: LT_EBELN TYPE TABLE OF TY_EBELN,
      LS_EBELN LIKE LINE OF LT_EBELN.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

DATA GT_T052  TYPE TABLE OF T052.
DATA GS_T052  TYPE T052.

DATA GT_T052S TYPE TABLE OF T052S.
DATA GS_T052S TYPE T052S.

DATA GT_T052U TYPE TABLE OF T052U.
DATA GS_T052U TYPE T052U.

DATA GT_T052U1 TYPE TABLE OF T052U.
DATA GS_T052U1 TYPE T052U.

DATA GT_EKKN  TYPE TABLE OF EKKN.
DATA GS_EKKN  TYPE EKKN.

DATA GS_AUFK  TYPE AUFK.

DATA GT_EKPO1 TYPE TABLE OF EKPO.
DATA GS_EKPO1 TYPE EKPO.

DATA GT_T024 TYPE TABLE OF T024.
DATA GS_T024 TYPE T024.

DATA GT_WYT3 TYPE TABLE OF WYT3.
DATA GS_WYT3 TYPE WYT3.

DATA G_ANSWER     TYPE STRING. "控制弹出框
DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.
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

  IF   gw_lvc-fieldname     = 'NETWR'
  or   gw_lvc-fieldname     = 'NETWR_1'
  or   gw_lvc-fieldname     = 'YJHJE'
  or   gw_lvc-fieldname     = 'YJHJE_1'
  or   gw_lvc-fieldname     = 'FPPZJE'
  or   gw_lvc-fieldname     = 'FPPZJE_1'
  or   gw_lvc-fieldname     = 'YFJE'
  or   gw_lvc-fieldname     = 'ZSQFKJE'
.
*  货币不显示0
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
SELECT-OPTIONS: S_BUKRS   FOR EKKO-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY ,"公司代码
                S_LIFNR   FOR EKKO-LIFNR ,                                     "供应商
                S_EBELN   FOR EKKO-EBELN ,                                     "采购订单
*                S_BELNR   FOR RBKP-BELNR,                                      "发票号
*                S_BUDAT   FOR RBKP-BUDAT,"过账日期
                S_WBS     FOR PRPS-POSID,                                       "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI017-ZSQRQ,                                    "申请日期
                S_ZSQFKR  FOR ZFI017-ZSQFKRQ,                                  "申请付款日期
                S_ZCLJD   FOR ZFI017-ZCLJD,                                    "处理进度
              "  S_EKORG  FOR LFM1-EKORG,"采购组织
                S_EKGRP FOR EKKO-EKGRP."采购组
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
*  IF  S_EBELN  IS  INITIAL.
*    MESSAGE '请在选择屏幕中输入采购订单' TYPE 'S' DISPLAY LIKE 'E'. "S004(ZFICO01) DISPLAY LIKE 'E'.
*    LEAVE TO SCREEN 1000.
*  ENDIF.
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

*&--代码添加 BY HANDYBY 27.06.2017 18:11:06  BEGIN

  DATA L_FLAG TYPE C .
  READ TABLE S_BUKRS WITH KEY LOW = '1700' .
  IF SY-SUBRC = 0 .
    L_FLAG = 'X'.
  ENDIF.
  READ TABLE S_BUKRS WITH KEY LOW = '1710'.
  IF SY-SUBRC = 0 .
    L_FLAG = 'X'.
  ENDIF.

  IF L_FLAG = 'X'. "励丰的单独弄
*&--代码添加 BY HANDYBY 23.06.2017 15:58:45  BEGIN

    DATA: BEGIN OF LS_EKKN,
            EBELN      TYPE EKKN-EBELN,
            EBELP      TYPE EKKN-EBELP,
            ZEKKN      TYPE EKKN-ZEKKN,
            PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
          END OF LS_EKKN .
    DATA LT_EKKN LIKE TABLE OF LS_EKKN .

    IF S_WBS[] IS NOT INITIAL .

      DATA: BEGIN OF LS_PROJH,
              PSPNR  TYPE PROJ-PSPNR,
              PSPNR2 TYPE PRPS-PSPNR,
            END OF LS_PROJH .
      DATA LT_PROJH LIKE TABLE OF LS_PROJH .
      SELECT A~PSPNR AS PSPNR
             B~PSPNR AS PSPNR2
        FROM PROJ AS A
       INNER JOIN PRPS AS B
          ON A~PSPNR = B~PSPHI
        INTO CORRESPONDING FIELDS OF TABLE LT_PROJH
       WHERE A~PSPID IN S_WBS
         AND B~STUFE IN ('1','2') .

      IF LT_PROJH IS NOT INITIAL .
        SELECT EBELN
               EBELP
               ZEKKN
               PS_PSP_PNR
          INTO CORRESPONDING FIELDS OF TABLE LT_EKKN
          FROM EKKN
           FOR ALL ENTRIES IN LT_PROJH
         WHERE PS_PSP_PNR = LT_PROJH-PSPNR2
           AND EBELN IN S_EBELN .

        IF LT_EKKN IS NOT INITIAL .
*查询采购订单的抬头
          SELECT * FROM EKKO
            INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
            FOR ALL ENTRIES IN LT_EKKN
           WHERE BUKRS IN S_BUKRS
             AND LIFNR IN S_LIFNR
             AND EKGRP IN S_EKGRP
            AND EBELN = LT_EKKN-EBELN .
        ENDIF.
      ENDIF.

    ELSE .

*查询采购订单的抬头
      SELECT * FROM EKKO
        INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
       WHERE BUKRS IN S_BUKRS
         AND LIFNR IN S_LIFNR
         AND EKGRP IN S_EKGRP
        AND EBELN IN S_EBELN .

    ENDIF.
*&--代码添加 BY HANDYBY 23.06.2017 15:58:45  END

  ELSE .

*查询采购订单的抬头
    SELECT * FROM EKKO
      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
     WHERE EBELN IN S_EBELN
      AND BUKRS IN S_BUKRS
     AND   LIFNR IN S_LIFNR
     AND   EKGRP IN S_EKGRP .

  ENDIF.
*&--代码添加 BY HANDHJD 12.07.2017 8:48:06  BEGIN
  "BSEG表发票号码
  SELECT
      BUKRS
      EBELN
      BELNR
   FROM BSEG INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
    FOR ALL ENTRIES IN LT_EKKN
   WHERE
    EBELN = LT_EKKN-EBELN AND
    BUKRS IN S_BUKRS.

  IF GT_BSEG IS NOT INITIAL.
    SELECT
        BUKRS
        BELNR
        BUZID
        ZUONR
     FROM BSEG INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_B
      FOR ALL ENTRIES IN GT_BSEG
     WHERE
      BELNR = GT_BSEG-BELNR AND
      BUZID EQ 'T'          AND
      BUKRS IN S_BUKRS.

  ENDIF.

*&--代码添加 BY HANDHJD 12.06.2017 8:48:06  END

*&--代码添加 BY HANDYBY 27.06.2017 18:11:06  END


  CHECK GT_EKKO IS NOT INITIAL.

*查询出票方供应商名称
  SELECT * FROM WYT3
   INTO CORRESPONDING FIELDS OF TABLE GT_WYT3
   FOR ALL ENTRIES IN GT_EKKO
   WHERE LIFNR = GT_EKKO-LIFNR
   AND   PARVW = 'RS'.

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

*查询开票方供应商描述
  SELECT * FROM LFA1
   INTO CORRESPONDING FIELDS OF TABLE GT_LFA2
   FOR ALL ENTRIES IN GT_WYT3
   WHERE LIFNR = GT_WYT3-LIFN2.

**查询采购组 及采购组名
*  SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM INTO CORRESPONDING FIELDS OF TABLE T_LFM1
*    FROM LFM1 AS A
*    LEFT JOIN T024 AS B
*    ON A~EKGRP = B~EKGRP
*    WHERE  A~LIFNR IN S_LIFNR AND  A~EKORG = S_BUKRS-LOW AND A~EKGRP IN S_EKGRP.
*  SORT T_LFM1 BY  LIFNR EKORG EKGRP .

*查询采购组描述
  SELECT * FROM T024
   INTO CORRESPONDING FIELDS OF TABLE GT_T024
   FOR ALL ENTRIES IN GT_EKKO
   WHERE EKGRP = GT_EKKO-EKGRP.

*查询采购订单的行项目
  SELECT * FROM EKPO
   INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
   FOR ALL ENTRIES IN GT_EKKO
   WHERE EKPO~EBELN = GT_EKKO-EBELN
   AND LOEKZ <> 'L'. "150525 添加  LOEKZ <> 'L‘ 的删除标识
*     AND   EKPO~EBELP = GT_RSEG-EBELP.

*查询订单税码
  SELECT * FROM EKPO
   INTO CORRESPONDING FIELDS OF TABLE GT_EKPO1
   FOR ALL ENTRIES IN GT_EKKO
   WHERE EKPO~EBELN = GT_EKKO-EBELN
   AND  LOEKZ <> 'L'.

*查询科目分配页签
  SELECT * FROM EKKN
   INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
   FOR ALL ENTRIES IN GT_EKPO
   WHERE EBELN  = GT_EKPO-EBELN
   AND   EBELP  = GT_EKPO-EBELP .

*查询采购订单PB00或者PBXX的金额
  SELECT * FROM KONV
    INTO CORRESPONDING FIELDS OF TABLE GT_KONV
    FOR ALL ENTRIES IN GT_EKKO
    WHERE KNUMV = GT_EKKO-KNUMV.

*查询交货历史
  IF GT_EKPO IS NOT INITIAL.
    SELECT * FROM EKBE
     INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
     FOR ALL ENTRIES IN GT_EKPO
     WHERE EBELN = GT_EKPO-EBELN
     AND   EBELP = GT_EKPO-EBELP.

    MOVE-CORRESPONDING GT_EKBE TO GT_EKBE_2.

    DELETE GT_EKBE_2 WHERE VGABE NE '2'.
    SORT GT_EKBE_2 BY EBELN EBELP .

    MOVE-CORRESPONDING GT_EKBE TO GT_EKBE_P.

    DELETE GT_EKBE_P WHERE VGABE NE 'P'.
    SORT GT_EKBE_P BY EBELN EBELP .

    DELETE GT_EKBE_1 WHERE VGABE NE '1'.
    SORT GT_EKBE_1 BY EBELN EBELP .
  ENDIF.
*查询自建表数据(审批通过)
  SELECT * FROM ZFI017
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017
    FOR ALL ENTRIES IN GT_EKKO
    WHERE EBELN = GT_EKKO-EBELN.
*      AND   ZCLJD = '3'.

*查询子付款条件
  SELECT * FROM T052
    INTO CORRESPONDING FIELDS OF TABLE GT_T052
    FOR ALL ENTRIES IN GT_EKKO
    WHERE ZTERM = GT_EKKO-ZTERM.

  IF GT_T052 IS NOT INITIAL.

    SELECT * FROM T052U
  INTO CORRESPONDING FIELDS OF TABLE GT_T052U1
  FOR ALL ENTRIES IN GT_T052
  WHERE ZTERM = GT_T052-ZTERM.

    SELECT * FROM T052S
      INTO CORRESPONDING FIELDS OF TABLE GT_T052S
      FOR ALL ENTRIES IN GT_T052
      WHERE ZTERM = GT_T052-ZTERM.

    SELECT * FROM T052U
      INTO CORRESPONDING FIELDS OF TABLE GT_T052U
      FOR ALL ENTRIES IN GT_T052S
      WHERE ZTERM = GT_T052S-RATZT.
  ENDIF.

*ADD BY HANDWY  210-7-16 增加二分法排序
  SORT GT_RBKP    BY  BELNR GJAHR.
  SORT GT_RSEG    BY  BELNR GJAHR BUZEI EBELN EBELP.
  SORT GT_EKKO    BY  EBELN.
  SORT GT_EKPO    BY  EBELN EBELP.
  SORT GT_KONV    BY  KNUMV KPOSN.
  SORT GT_EKBE    BY  EBELN EBELP ZEKKN VGABE GJAHR BELNR BUZEI.
  SORT GT_LFA1    BY  LIFNR.
  SORT GT_T001    BY  BUKRS.
  SORT GT_ZFI017  BY  ZSQD BUKRS GJAHR EBELN EBELP BELNR.
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
  DATA L_YFJE     TYPE ZFI017-YFJE.
  DATA L_YSQJE    TYPE ZFI017-YSQJE."已申请金额
  DATA L_ZSQFKJE  TYPE ZFI017-ZSQFKJE."申请付款金额
  DATA L_EBELN    TYPE EKKO-EBELN.
  DATA T_FTAXP    TYPE TABLE OF FTAXP.
  DATA S_FTAXP    TYPE FTAXP.
  DATA FACTOR     TYPE P DECIMALS 3.
  DATA L_TABIX    TYPE SY-TABIX.

  "统计发票金额。
  LOOP AT GT_EKBE_2  INTO GS_EKBE_2.
    CLEAR:GS_FP_2.
    GS_FP_2-EBELN = GS_EKBE_2-EBELN.
    GS_FP_2-MWSKZ = GS_EKBE_2-MWSKZ.
    IF GS_EKBE_2-SHKZG EQ 'H'.
      GS_FP_2-WRBTR = GS_EKBE_2-WRBTR * -1 .
    ELSE.
      GS_FP_2-WRBTR = GS_EKBE_2-WRBTR .
    ENDIF.
    COLLECT GS_FP_2 INTO GT_FP_2.
  ENDLOOP.
  SORT GT_FP_2 BY EBELN .

  MOVE-CORRESPONDING GT_FP_2 TO GT_SE_1.
  APPEND LINES OF GT_SE_1 TO GT_SE .

  "统计预制发票金额。
  LOOP AT GT_EKBE_P  INTO GS_EKBE_P.
    CLEAR:GS_FP_P.
    GS_FP_P-EBELN = GS_EKBE_P-EBELN.
    GS_FP_P-MWSKZ = GS_EKBE_P-MWSKZ.
    IF GS_EKBE_P-SHKZG EQ 'H'.
      GS_FP_P-WRBTR = GS_EKBE_P-WRBTR * -1 .
    ELSE.
      GS_FP_P-WRBTR = GS_EKBE_P-WRBTR .
    ENDIF.
    COLLECT GS_FP_P INTO GT_FP_P.
  ENDLOOP.
  SORT GT_FP_P BY EBELN .

  MOVE-CORRESPONDING GT_FP_P TO GT_SE_2.
  APPEND LINES OF GT_SE_2 TO GT_SE .

  SORT GT_SE BY MWSKZ .
  DELETE ADJACENT DUPLICATES FROM GT_SE COMPARING MWSKZ .

  LOOP AT GT_SE INTO GS_SE .
    REFRESH:GT_FTAXP.
    CLEAR:GS_FTAXP.
    CALL FUNCTION 'GET_TAX_PERCENTAGE'
      EXPORTING
        ALAND   = 'CN'
        DATAB   = '20000101'
        MWSKZ   = GS_SE-MWSKZ
        TXJCD   = 'TAXCN'
        "*     EXPORT        = ' '
      TABLES
        T_FTAXP = GT_FTAXP.
    READ TABLE GT_FTAXP INTO GS_FTAXP INDEX 1.
    IF SY-SUBRC EQ 0.
      GS_SE-KBETR = GS_FTAXP-KBETR.
    ENDIF.
    MODIFY GT_SE FROM GS_SE.

  ENDLOOP.

*  "统计交货金额(含税)
  LOOP AT GT_EKBE_1 INTO GS_EKBE_1.
    CLEAR:GS_JH.
    GS_JH-EBELN = GS_EKBE_1-EBELN.
    GS_JH-WRBTR = GS_EKBE_1-WRBTR.
    IF GS_EKBE_1-SHKZG EQ 'H'.
      GS_JH-WRBTR = GS_JH-WRBTR * -1 .
    ENDIF.
    COLLECT GS_JH INTO GT_JH.
  ENDLOOP.
  SORT GT_JH BY EBELN .

*&--代码添加 BY HANDYBY 19.07.2017 12:17:15  BEGIN
  SORT GT_EKKN BY EBELN .
*&--代码添加 BY HANDYBY 19.07.2017 12:17:15  END

**********************************************当选择屏幕上输入采购订单
  LOOP AT GT_EKKO INTO GS_EKKO.
    MOVE-CORRESPONDING GS_EKKO TO GS_DATA.

*    GS_DATA-ZTERM = ''.

*    "查询采购组及采购组名
*    READ TABLE  T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
*    IF SY-SUBRC = 0 .
*      GS_DATA-LIFNR = T_LFM1-LIFNR.
*      GS_DATA-EKNAM = T_LFM1-EKNAM.
*    ENDIF.
* 根据采购订单号 调用函数 Read_text 读取文本 ，文本对象-EKKO ，文本标识-F06 ADD IT02 151120

    G_OBJNAME = GS_DATA-EBELN.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'F06'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'EKKO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        LINES                   = IT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC = 0.
      READ TABLE IT_LINES INTO WA_LINES INDEX 1.
      IF SY-SUBRC = 0.
        CONDENSE WA_LINES-TDLINE NO-GAPS.
        GS_DATA-HTBH = WA_LINES-TDLINE.
      ENDIF.
    ENDIF.
*项目编号
    READ TABLE GT_EKKN INTO GS_EKKN
    WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH .
    IF SY-SUBRC = 0.
      IF GS_EKKN-PS_PSP_PNR IS  NOT INITIAL.

*WBS内码转换WBS元素
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = GS_EKKN-PS_PSP_PNR
          IMPORTING
*&--代码注释 BY HANDYBY 23.06.2017 11:14:07  BEGIN
*           output = gs_data-posid+0(12).
*&--代码注释 BY HANDYBY 23.06.2017 11:14:07  END
*&--代码添加 BY HANDYBY 23.06.2017 11:14:12  BEGIN
            OUTPUT = GS_DATA-POSID.
*&--代码添加 BY HANDYBY 23.06.2017 11:14:12  END


      ELSEIF GS_EKKN-NPLNR IS NOT INITIAL.
        CLEAR GS_AUFK.
        SELECT SINGLE * FROM AUFK
        INTO CORRESPONDING FIELDS OF GS_AUFK
        WHERE AUFNR = GS_EKKN-NPLNR.

        IF SY-SUBRC = 0.
*WBS内码转换WBS元素
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_AUFK-PSPEL
            IMPORTING
*&--代码注释 BY HANDYBY 23.06.2017 11:14:39  BEGIN
*             OUTPUT = GS_DATA-POSID+0(12).
*&--代码注释 BY HANDYBY 23.06.2017 11:14:39  END
*&--代码添加 BY HANDYBY 23.06.2017 11:14:44  BEGIN
              OUTPUT = GS_DATA-POSID.
*&--代码添加 BY HANDYBY 23.06.2017 11:14:44  END

        ENDIF.
      ENDIF.
    ENDIF.

*当输入了WBS元素时，如果没有查询，
*&--代码添加 BY HANDYBY 27.06.2017 18:06:08  BEGIN
    IF S_BUKRS-LOW NE '1700' AND S_BUKRS-LOW NE '1710' .

*&--代码添加 BY HANDYBY 27.06.2017 18:06:08  END
      IF S_WBS IS NOT INITIAL.
*&--代码注释 BY HANDYBY 23.06.2017 11:15:12  BEGIN
*    IF  GS_DATA-POSID+0(12) NOT IN S_WBS.
*&--代码注释 BY HANDYBY 23.06.2017 11:15:12  END
*&--代码添加 BY HANDYBY 23.06.2017 11:15:18  BEGIN
        IF  GS_DATA-POSID NOT IN S_WBS.
*&--代码添加 BY HANDYBY 23.06.2017 11:15:18  END
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDIF.

*项目名称
    IF GS_DATA-POSID IS NOT INITIAL.
      SELECT SINGLE POST1 FROM PRPS
        INTO  GS_DATA-POST1
        WHERE POSID = GS_DATA-POSID.
*&--代码添加 BY HANDYBY 27.06.2017 21:24:44  BEGIN
      SELECT SINGLE A~PSPID INTO GS_DATA-PSPID
          FROM PROJ AS A
        INNER JOIN PRPS AS B
         ON A~PSPNR = B~PSPHI
        WHERE B~POSID = GS_DATA-POSID .
      SELECT SINGLE A~POST1 INTO GS_DATA-POST2
          FROM PROJ AS A
        INNER JOIN PRPS AS B
         ON A~PSPNR = B~PSPHI
        WHERE B~POSID = GS_DATA-POSID .
*&--代码添加 BY HANDYBY 27.06.2017 21:24:44  END
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

*出票方的供应商
    READ TABLE GT_WYT3 INTO GS_WYT3
    WITH KEY LIFNR = GS_EKKO-LIFNR
             EKORG = GS_EKKO-EKORG
             PARVW = 'RS'.
    IF SY-SUBRC = 0.
      GS_DATA-LIFN2 = GS_WYT3-LIFN2.
    ENDIF.

*出票方供应商描述
    READ TABLE GT_LFA2 INTO GS_LFA2
    WITH KEY LIFNR = GS_WYT3-LIFN2.
    IF SY-SUBRC = 0.
      GS_DATA-NAME2 = GS_LFA2-NAME1.
    ENDIF.

*    "查询采购组及采购组名
*    READ TABLE T_LFM1 WITH KEY LIFNR = GS_EKKO-LIFNR.
*    IF SY-SUBRC = 0.
*      GS_DATA-EKGRP = T_LFM1-EKGRP.
*      GS_DATA-EKNAM = T_LFM1-EKNAM.
*    ENDIF.

    READ TABLE GT_T024 INTO GS_T024
    WITH KEY EKGRP = GS_DATA-EKGRP.
    IF SY-SUBRC = 0.
      GS_DATA-EKNAM = GS_T024-EKNAM.
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

*处理进度默认为1
    GS_DATA-ZCLJD = '1'.

*默认货币，申请日期
    GS_DATA-ZSQRQ      = SY-DATUM.
    GS_DATA-WAERS_2    = GS_DATA-WAERS_1.



*是否含税
    READ TABLE GT_EKPO1 INTO GS_EKPO1
    WITH KEY EBELN = GS_DATA-EBELN.
    IF SY-SUBRC = 0.
      GS_DATA-MWSKZ = GS_EKPO1-MWSKZ.
    ENDIF.

    "交货金额（含税）
    READ TABLE GT_JH INTO GS_JH
     WITH KEY EBELN = GS_DATA-EBELN .
    IF SY-SUBRC EQ 0.

      REFRESH:GT_FTAXP.
      CLEAR:GS_FTAXP.
      CALL FUNCTION 'GET_TAX_PERCENTAGE'
        EXPORTING
          ALAND   = 'CN'
          DATAB   = '20000101'
          MWSKZ   = GS_DATA-MWSKZ
          TXJCD   = 'TAXCN'
          "*     EXPORT        = ' '
        TABLES
          T_FTAXP = GT_FTAXP.
      READ TABLE GT_FTAXP INTO GS_FTAXP INDEX 1.
      IF SY-SUBRC EQ 0.
        GS_DATA-JHJE_1 = GS_JH-WRBTR  * ( 1 + GS_FTAXP-KBETR / 1000 ) .

      ENDIF.
    ENDIF.

*子付款条件 *应付比例 *应付金额
    READ TABLE GT_T052 INTO GS_T052
    WITH KEY ZTERM = GS_EKKO-ZTERM.
    IF SY-SUBRC = 0.
      IF GS_T052-XSPLT IS INITIAL.
        GS_DATA-YFJE1     = GS_DATA-NETWR_1.
        GS_DATA-RATPZ     = 100.

*增加付款条件  ADD BY HANDWY  2015-10-15
        GS_DATA-ZTERM     = GS_EKKO-ZTERM.

*子付款条件描述
        READ TABLE GT_T052U1 INTO GS_T052U1
        WITH KEY ZTERM = GS_DATA-ZTERM.
        IF SY-SUBRC = 0.
          GS_DATA-TEXT1 = GS_T052U1-TEXT1.
        ENDIF.

*已付款金额
        CLEAR L_YFJE.
        LOOP AT  GT_ZFI017 INTO GS_ZFI017
        WHERE EBELN = GS_DATA-EBELN
        AND   ZCLJD = '3'.
          " AND         BELNR = ''.
          "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
          L_YFJE = L_YFJE + GS_ZFI017-ZSQFKJE.  "150527 YEFE 字段值为空 才注释
        ENDLOOP.
        GS_DATA-YFJE = L_YFJE.

*已申请金额
        CLEAR L_YSQJE.
        LOOP AT GT_ZFI017 INTO GS_ZFI017
         WHERE EBELN = GS_DATA-EBELN
         AND  EBELN = GS_DATA-EBELN
         AND ( ZCLJD = '3'
         OR   ZCLJD  = '2'
         OR   ZCLJD  = '1')
         AND   STATU    <> ICON_DELETE.
          L_YSQJE = L_YSQJE + GS_ZFI017-ZSQFKJE.
        ENDLOOP.
        GS_DATA-YSQJE = L_YSQJE.

*未付金额
        GS_DATA-WFJE = GS_DATA-YFJE1 - GS_DATA-YFJE.

**申请付款金额
*        GS_DATA-ZSQFKJE = GS_DATA-WFJE.

*已付比例
        IF GS_DATA-YFJE1 NE  0.
          GS_DATA-YFBL  = ( GS_DATA-YFJE / GS_DATA-YFJE1 ) * 100.
        ENDIF.
        APPEND GS_DATA TO GT_DATA.
        CLEAR GS_DATA.
      ELSE.
        CLEAR L_TABIX.
        LOOP AT GT_T052S INTO GS_T052S
        WHERE ZTERM = GS_EKKO-ZTERM.
          GS_DATA-YFJE1 = GS_DATA-NETWR_1 * GS_T052S-RATPZ / 100.
          GS_DATA-RATPZ = GS_T052S-RATPZ.
          GS_DATA-ZTERM = GS_T052S-RATZT.

          L_TABIX = L_TABIX + 1.
          GS_DATA-ZCH   = L_TABIX.

*子付款条件描述
          READ TABLE GT_T052U INTO GS_T052U
          WITH KEY ZTERM = GS_T052S-RATZT.
          IF SY-SUBRC = 0.
            GS_DATA-TEXT1 = GS_T052U-TEXT1.
          ENDIF.

*已付款金额
          CLEAR L_YFJE.
          LOOP AT  GT_ZFI017 INTO GS_ZFI017
          WHERE EBELN = GS_DATA-EBELN
          AND   ZCLJD = '3'
          AND   ZCH   = GS_DATA-ZCH.
            " AND         BELNR = ''.
            "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
            L_YFJE = L_YFJE + GS_ZFI017-ZSQFKJE.  "150527 YEFE 字段值为空 才注释
          ENDLOOP.
          GS_DATA-YFJE = L_YFJE.

*已申请金额
          CLEAR L_YSQJE.
          LOOP AT GT_ZFI017 INTO GS_ZFI017
           WHERE EBELN = GS_DATA-EBELN
           AND  EBELN = GS_DATA-EBELN
           AND ( ZCLJD = '3'
           OR   ZCLJD  = '2'
           OR   ZCLJD  = '1')
           AND  STATU  <> ICON_DELETE
           AND  ZCH    = GS_DATA-ZCH.
            L_YSQJE = L_YSQJE + GS_ZFI017-ZSQFKJE.
          ENDLOOP.
          GS_DATA-YSQJE = L_YSQJE.

*未付金额
          GS_DATA-WFJE = GS_DATA-YFJE1 - GS_DATA-YFJE.

**申请付款金额
*          GS_DATA-ZSQFKJE = GS_DATA-WFJE.

*已付比例
          IF GS_DATA-YFJE1 NE 0 .
            GS_DATA-YFBL  = GS_DATA-YFJE / GS_DATA-YFJE1 * 100.
          ENDIF.

          APPEND GS_DATA TO GT_DATA.
        ENDLOOP.
      ENDIF.
    ELSE.

*增加付款条件  ADD BY HANDWY  2015-10-15
      GS_DATA-ZTERM     = GS_EKKO-ZTERM.

*子付款条件描述
      READ TABLE GT_T052U1 INTO GS_T052U1
      WITH KEY ZTERM = GS_DATA-ZTERM.
      IF SY-SUBRC = 0.
        GS_DATA-TEXT1 = GS_T052U1-TEXT1.
      ENDIF.


      GS_DATA-YFJE1     = GS_DATA-NETWR_1.
      GS_DATA-RATPZ     = 100.

*已付款金额
      CLEAR L_YFJE.
      LOOP AT  GT_ZFI017 INTO GS_ZFI017
      WHERE EBELN = GS_DATA-EBELN
      AND   ZCLJD = '3'.
        " AND         BELNR = ''.
        "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
        L_YFJE = L_YFJE + GS_ZFI017-ZSQFKJE.  "150527 YEFE 字段值为空 才注释
      ENDLOOP.
      GS_DATA-YFJE = L_YFJE.

*已申请金额
      CLEAR L_YSQJE.
      LOOP AT GT_ZFI017 INTO GS_ZFI017
       WHERE EBELN = GS_DATA-EBELN
       AND  EBELN = GS_DATA-EBELN
       AND ( ZCLJD = '3'
       OR   ZCLJD  = '2'
       OR   ZCLJD  = '1')
       AND   STATU    <> ICON_DELETE.
        L_YSQJE = L_YSQJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.
      GS_DATA-YSQJE = L_YSQJE.

*未付金额
      GS_DATA-WFJE = GS_DATA-YFJE1 - GS_DATA-YFJE.

**申请付款金额
*        GS_DATA-ZSQFKJE = GS_DATA-WFJE.

*已付比例
      IF GS_DATA-YFJE1 NE 0 .
        GS_DATA-YFBL  = ( GS_DATA-YFJE / GS_DATA-YFJE1 ) * 100.
      ENDIF.

      APPEND GS_DATA TO GT_DATA.
      CLEAR GS_DATA.
    ENDIF.

    CLEAR GS_DATA.
  ENDLOOP.

  "150527 begin 过滤 已付金额 大于等于应付金额的数据
  IF X_WFK = 'X'.
    LOOP AT GT_DATA INTO GS_DATA .
      READ TABLE GT_ZFI017 INTO GS_ZFI017
      WITH KEY EBELN = GS_DATA-EBELN
      BINARY SEARCH.
      IF SY-SUBRC = 0 .
        IF GS_ZFI017-WAERS_2 = GS_DATA-WAERS . "若果已付金额的货币码等于 本币码 ，已付金额就和采购订单本币金额比较
          IF GS_DATA-YFJE >= GS_DATA-NETWR .
            DELETE GT_DATA WHERE EBELN = GS_DATA-EBELN.
          ENDIF.
        ELSE.                                         "若果已付金额的货币码不等于 本币码 ，已付金额就和采购订单凭证金额数比较
          IF GS_ZFI017-WAERS_2 = GS_DATA-WAERS_1 .
            IF GS_DATA-YFJE >= GS_DATA-NETWR_1 .
              DELETE GT_DATA WHERE EBELN = GS_DATA-EBELN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "add:发票金额（净值）、发票金额（含税）、预制发票（净值）、预制发票（含税）
  LOOP AT GT_DATA INTO GS_DATA.
    READ TABLE GT_FP_2 INTO GS_FP_2 WITH KEY EBELN = GS_DATA-EBELN
                                BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      GS_DATA-FPJE = GS_FP_2-WRBTR .
      READ TABLE GT_SE INTO GS_SE WITH KEY GS_FP_2-MWSKZ
                                     BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_DATA-FPJE_1 = GS_DATA-FPJE  * ( 1 + GS_SE-KBETR / 1000 ).

      ENDIF.
    ENDIF.
    READ TABLE GT_FP_P INTO GS_FP_P WITH KEY EBELN = GS_DATA-EBELN
                                    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DATA-YZFP = GS_FP_P-WRBTR.
      READ TABLE GT_SE INTO GS_SE WITH KEY GS_FP_P-MWSKZ
                                    BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_DATA-YZFP_1 = GS_DATA-YZFP * ( 1 + GS_SE-KBETR / 1000 ).
      ENDIF.
    ENDIF.
    MODIFY GT_DATA FROM GS_DATA.
  ENDLOOP.

*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  BEGIN
  DATA: BEGIN OF LS_EKKO,
          EBELN   TYPE EKKO-EBELN,
          YJFKRQ1 TYPE EKKO-YJFKRQ1,
          YJFKJE1 TYPE EKKO-YJFKJE1,
          YJFKRQ2 TYPE EKKO-YJFKRQ2,
          YJFKJE2 TYPE EKKO-YJFKJE2,
          YJFKRQ3 TYPE EKKO-YJFKRQ3,
          YJFKJE3 TYPE EKKO-YJFKJE3,
          ZBD1T   TYPE EKKO-ZBD1T,
        END OF LS_EKKO.
  DATA LT_EKKO LIKE TABLE OF LS_EKKO .
  SELECT EBELN
         YJFKRQ1
         YJFKJE1
         YJFKRQ2
         YJFKJE2
         YJFKRQ3
         YJFKJE3
         ZBD1T
    INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
    FROM EKKO
     FOR ALL ENTRIES IN GT_DATA
   WHERE EBELN = GT_DATA-EBELN .
  SORT LT_EKKO BY EBELN .

  DATA: BEGIN OF LS_EKBE ,
          EBELN TYPE EKBE-EBELN,
          EBELP TYPE EKBE-EBELP,
          BUDAT TYPE EKBE-BUDAT,
        END OF LS_EKBE .
  DATA LT_EKBE LIKE TABLE OF LS_EKBE .
  SELECT EBELN
         EBELP
         BUDAT
    INTO CORRESPONDING FIELDS OF TABLE LT_EKBE
    FROM EKBE
     FOR ALL ENTRIES IN GT_DATA
   WHERE EBELN = GT_DATA-EBELN
     AND BEWTP = 'E'
     AND BWART IN ('101','105') .
  SORT LT_EKBE BY EBELN EBELP BUDAT DESCENDING .

  DATA: LT_DATA2 LIKE GT_DATA,
        LS_DATA2 LIKE LINE OF LT_DATA2.
  LOOP AT GT_DATA INTO GS_DATA .
    IF GS_DATA-BUKRS = '1700' OR GS_DATA-BUKRS = '1710'.

      READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH .
      IF SY-SUBRC = 0 .

        IF LS_EKKO-YJFKRQ1 IS INITIAL AND LS_EKKO-YJFKJE1 IS INITIAL AND
           LS_EKKO-YJFKRQ2 IS INITIAL AND LS_EKKO-YJFKJE2 IS INITIAL AND
            LS_EKKO-YJFKRQ3 IS INITIAL AND LS_EKKO-YJFKJE3 IS INITIAL .
          MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
          READ TABLE LT_EKBE INTO LS_EKBE WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH .
          IF SY-SUBRC = 0 .
            LS_DATA2-YJFKRQ = LS_EKBE-BUDAT + LS_EKKO-ZBD1T .
            CLEAR LS_EKBE .
          ENDIF.
          LS_DATA2-YJFKJE = GS_DATA-WFJE .
          INSERT LS_DATA2 INTO TABLE LT_DATA2  .
          CONTINUE .
        ENDIF.

        IF LS_EKKO-YJFKRQ1 IS NOT INITIAL .
          MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
          LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ1 .
          LS_DATA2-YJFKJE = LS_EKKO-YJFKJE1 .
          INSERT LS_DATA2 INTO TABLE LT_DATA2  .
          CLEAR LS_DATA2-YJFKRQ .
          CLEAR LS_DATA2-YJFKJE .
        ENDIF.

        IF LS_EKKO-YJFKRQ2 IS NOT INITIAL .
          LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ2 .
          LS_DATA2-YJFKJE = LS_EKKO-YJFKJE2 .
          INSERT LS_DATA2 INTO TABLE LT_DATA2  .
          CLEAR LS_DATA2-YJFKRQ .
          CLEAR LS_DATA2-YJFKJE .
        ENDIF.

        IF LS_EKKO-YJFKRQ3 IS NOT INITIAL ..
          LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ3 .
          LS_DATA2-YJFKJE = LS_EKKO-YJFKJE3 .
          INSERT LS_DATA2 INTO TABLE LT_DATA2  .
          CLEAR LS_DATA2-YJFKRQ .
          CLEAR LS_DATA2-YJFKJE .
        ENDIF.

      ENDIF.

    ELSE .

      MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
      INSERT LS_DATA2 INTO TABLE LT_DATA2  .

    ENDIF.
    CLEAR GS_DATA .
    CLEAR LS_EKKO .
    CLEAR LS_DATA2 .
  ENDLOOP.

  REFRESH GT_DATA .
  MOVE-CORRESPONDING LT_DATA2 TO GT_DATA .
*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  END


**删除重复的采购订单行
*  DATA:GW_DATA TYPE TY_DATA.
*  SORT GT_DATA BY BUKRS GJAHR BELNR EBELN.
*  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING BUKRS GJAHR BELNR EBELN.

  "150527 begin 读取发票到期日并赋值到ALV显示的内表中
*  DATA: L2_ZSQFKJE TYPE ZFI017-ZSQFKJE.
*  LOOP AT GT_DATA INTO GS_DATA WHERE EBELN = ''.
*    READ TABLE GT_RBKP INTO GS_RBKP
*    WITH KEY  BELNR = GS_DATA-BELNR
*    BINARY SEARCH.  "根据发票凭证号查询GT_RBKP表读取
*    IF SY-SUBRC  = 0 .
*      IF S_BUDAT[] IS NOT INITIAL ."如果屏幕付款到期日不为空，FKDQ = ZFBDT ,若为空 FKDQ = ZFBDT + GS_RBKP-ZBD1T .
*        GS_DATA-FKDQ = GS_RBKP-ZFBDT .
*      ELSE.
*        GS_DATA-FKDQ = GS_RBKP-ZFBDT + GS_RBKP-ZBD1T .
*      ENDIF.
*      MODIFY   GT_DATA FROM GS_DATA.
*    ENDIF.
*    CLEAR L2_ZSQFKJE  .
*    CLEAR GS_DATA_2 .
*    LOOP AT GT_DATA INTO GS_DATA_2 WHERE BELNR = GS_DATA-BELNR AND EBELN <> ''.
*      L2_ZSQFKJE  = L2_ZSQFKJE  + GS_DATA_2-ZSQFKJE.
*    ENDLOOP.
*    GS_DATA-ZSQFKJE =  L2_ZSQFKJE  .
*    MODIFY GT_DATA FROM GS_DATA.
*  ENDLOOP.
*                                                            "150527
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
  INIT_FIELDCAT 'LIFN2'          '供应商（开票）'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME2'          '供应商名称（开票）'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HTBH'          '合同编号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'          '采购组'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKNAM'          '采购组名'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PSPID'          '项目编号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST2'          '项目名称'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '本位币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'        '凭证货币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR'          '采购订单含税金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR_1'        '采购订单含税金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MWSKZ'          '是否含税'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM'          '子付款条件'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT1'          '子付款条件描述'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RATPZ'          '应付比例%'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE1'          '应付金额'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  BEGIN
  INIT_FIELDCAT 'YJFKRQ'          '预计付款日期'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJFKJE'          '预计付款金额'   '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'YJFKRQ2'          '预计付款日期2'   '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'YJFKJE2'          '预计付款金额2'   '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'YJFKRQ3'          '预计付款日期3'   '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'YJFKJE3'          '预计付款金额3'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  END
  INIT_FIELDCAT 'YJHJE'          '已交货金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE_1'        '已交货金额(净值)'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE'           '已付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFBL'           '已付比例%'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE'          '已申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WFJE'           '未付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZFKXZ'          '付款性质'   '' '' '' 'X' ''  'ZFI017' 'ZFKXZ'.
  INIT_FIELDCAT 'ZSQFKJE'        '申请付款金额'   '' '' '' 'X' '' 'ZFI017' 'ZSQFKJE'.
  INIT_FIELDCAT 'WAERS_2'        '货币'   '6' '' '' 'X' '' 'EKKO' 'WAERS'.
  INIT_FIELDCAT 'ZSQRQ'          '申请日期'   '' '' '' 'X' '' 'ZFI017' 'ZSQRQ'.
  INIT_FIELDCAT 'ZSQFKRQ'        '申请付款日期'   '' '' '' 'X' '' 'ZFI017' 'ZSQFKRQ'.
  INIT_FIELDCAT 'ZZY'            '摘要'   '15' '' '' 'X' '' 'ZFI017' 'ZZY'.
  INIT_FIELDCAT 'ZCLJD'          '处理进度'   '6' '' '' '' '' 'ZFI017' 'ZCLJD' .
  INIT_FIELDCAT 'BELNR_F'        '付款会计凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR_F'        '付款会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '发票文本'   '15' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 27.06.2017 21:28:12  BEGIN
  INIT_FIELDCAT 'POSID'          'WBS元素'   '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'          'WBS描述'   '15' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 27.06.2017 21:28:12  END

*&--代码添加 BY HANDSSY 11.07.2017  BEGIN
  INIT_FIELDCAT 'BXQX'          '保修期限'   '10' '' '' 'X' '' 'ZFI017' 'BXQX'.
  INIT_FIELDCAT 'BXQSRQ'        '保修起始日期'   '10' '' '' 'X' '' 'ZFI017' 'ZSQRQ'.
*&--代码添加 BY HANDSSY 11.07.2017  END


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
  DATA L_ZSQD       TYPE ZFI017-ZSQD.          "查询当前的申请单
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
*----------当公司1700 1710 1720时，以上两列：保修期限不允许为空
      IF S_BUKRS-LOW EQ '1700' OR S_BUKRS-LOW EQ '1710' OR S_BUKRS-LOW EQ '17 20'.
        READ TABLE GT_DATA INTO GS_DATA WITH  KEY ZBOX = 'X'.
*------------------------------------------------------------------------康迪注释--------*
*        IF SY-SUBRC EQ 0.
*           IF GS_DATA-BXQX IS INITIAL OR GS_DATA-BXQSRQ IS INITIAL OR GS_DATA-BXQX EQ '0'.
*              MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
*              EXIT.
*          ENDIF.
*----------------------------------------------------------------------------------------*
        IF SY-SUBRC EQ 0.

*          IF GS_DATA-BXQSRQ IS INITIAL.
*            IF GS_DATA-BXQX IS INITIAL OR GS_DATA-BXQX NE '不保修'..
*              MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
*            ENDIF.
*           else.
*
*          ENDIF.
          IF GS_DATA-BXQX EQ '不保修'.

            IF NOT GS_DATA-BXQSRQ IS INITIAL.
              MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.
          ELSEIF GS_DATA-BXQX IS INITIAL.
            IF GS_DATA-BXQSRQ IS INITIAL.
              MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.
          ELSE.
            IF GS_DATA-BXQSRQ IS INITIAL.
              MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

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
      SELECT * FROM ZFI017
        INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017_1.
      SORT GT_ZFI017_1 BY ZSQD  DESCENDING.
      READ TABLE GT_ZFI017_1 INTO GS_ZFI017_1 INDEX 1.

*当前日期
      IF GS_ZFI017_1-ZSQD+0(8) = SY-DATUM.
        L_ZSQD = GS_ZFI017_1-ZSQD.
      ENDIF.

      REFRESH GT_ZFI017_1.

**财务审批的时候，只针对发票进行审批，采购订单状态会自动改变(排除申请金额为0的采购订单行)
*      LOOP AT GT_DATA INTO GS_DATA
*       WHERE ZBOX     = 'X'
*       AND   ZSQFKJE  <> '0'.
*        LOOP AT GT_DATA INTO GS_DATA
*         WHERE BELNR = GS_DATA-BELNR
*         AND   GJAHR = GS_DATA-GJAHR
*         AND   BUKRS = GS_DATA-BUKRS
*         AND   ZSQFKJE  <> '0'.
*
*          GS_DATA-ZBOX = 'X'.
*          MODIFY GT_DATA FROM GS_DATA.
*        ENDLOOP.
*      ENDLOOP.

*检查必输项
      PERFORM CHECK_INPUT CHANGING L_SUBRC.
      IF L_SUBRC = '4'.
        EXIT.
      ENDIF.

*检查
      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0 .

**改变选中行
*        LOOP AT GT_DATA INTO GS_DATA
*         WHERE ZBOX = 'X'.
*          LOOP AT GT_DATA INTO GS_DATA
*           WHERE EBELN = GS_DATA-EBELN.
*            GS_DATA-ZBOX = 'X'.
*            MODIFY GT_DATA FROM GS_DATA.
*          ENDLOOP.
*        ENDLOOP.
*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
*备注： 针对于1700,1710,1720公司，不管选中几行都只产生一个单号  康迪—注释1760-1770
*        IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
*
*          IF L_ZSQD IS NOT INITIAL.
*            L_ZSQD = L_ZSQD + 1.
*          ELSE.
*            CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
*          ENDIF.
*        ENDIF.
*------------------------------------------------------------------------------------*
*备注：针对1700，1710，1720公司，选中几行产生不同的单号  康迪-新添代码

*      LOOP AT GT_DATA into GS_DATA where zbox = 'X'.
*        IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
*          IF L_ZSQD is not initial.
*            L_ZSQD = L_ZSQD + 1.
*           else.
*             concatenate sy-datum '00001' into L_ZSQD.
*          ENDIF.
*        endif.
*        modify GT_DATA from GS_DATA.
*      ENDLOOP.
*------------------------------------------------------------------------------------*
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

*选择屏幕采购订单进入
        SORT GT_DATA BY EBELN ZBOX DESCENDING.
        LOOP AT GT_DATA INTO GS_DATA
        WHERE ZBOX = 'X'.

*形成申请单号 = 年月日+5位流水号
*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
*备注： 针对于1700,1710,1720公司，不管选中几行都只产生一个单号
*          IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
*
*          ELSE.
*            AT NEW EBELN.
*              IF L_ZSQD IS NOT INITIAL.
*                L_ZSQD = L_ZSQD + 1.
*              ELSE.
*                CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
*              ENDIF.
*            ENDAT.
*          ENDIF.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

*备注： 针对于1700,1710,1720公司，选中几行产生几个单号    康迪-新添代码

*      LOOP AT GT_DATA into GS_DATA where zbox = 'X'.
          IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
            IF L_ZSQD IS NOT INITIAL.
              L_ZSQD = L_ZSQD + 1.
            ELSE.
              CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
            ENDIF.
*&--代码添加 BY HANDYBY 19.07.2017 16:12:44  BEGIN
          ELSE .  "非励丰的情况
            IF L_ZSQD IS INITIAL .
              CONCATENATE SY-DATUM '00001' INTO L_ZSQD.
            ELSE.
              L_ZSQD = L_ZSQD + 1 .
            ENDIF.
*&--代码添加 BY HANDYBY 19.07.2017 16:12:44  END
          ENDIF.

*      ENDLOOP.

*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

*&--------------------------ADD BY HANDHJD 20170711 15:34 ---------------------------*
          "ZFI017表中新增保修期限和保修起始时间
          IF GS_DATA-BXQX EQ '不保修'.
            GS_DATA-BXQSRQ = ''.

          ENDIF.

*&--------------------------END BY HANDHJD 20170711 15:34 ---------------------------*
*付款申请单
          GS_DATA-ZSQD = L_ZSQD.

*更新屏幕
          MODIFY GT_DATA FROM GS_DATA.

          MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
          GS_ZFI017_1-ZNAME = SY-UNAME.
          GS_ZFI017_1-ZDATE = SY-DATUM.
          GS_ZFI017_1-ZTIME = SY-UZEIT.
          GS_ZFI017_1-YSQJE = GS_ZFI017_1-YSQJE + GS_DATA-ZSQFKJE.

          APPEND GS_ZFI017_1 TO GT_ZFI017_1.
        ENDLOOP.

*更新数据库表
        MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
        REFRESH GT_ZFI017_1.
        CLEAR GS_DATA.

*提示保存成功
        MESSAGE S002(Z001).

*重新排序
        SORT GT_DATA BY  EBELN ZCH.

**更改到查看状态
*        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
*          EXPORTING
*            I_READY_FOR_INPUT = 0.

        CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&PRINT'.
*--------------------------------------------------康迪注释--------------------------------*
*      READ TABLE GT_DATA INTO GS_DATA
*       WITH KEY ZBOX = 'X'.
*      IF SY-SUBRC = 0.
*        READ TABLE GT_DATA INTO GS_DATA
*        WITH KEY ZSQD = ''
*                 ZBOX = 'X'.
*        IF SY-SUBRC = 0.
*          MESSAGE S012(ZFICO01) DISPLAY LIKE 'E'.
*        ELSE.
*          PERFORM FRM_PRINT.
*        ENDIF.
*      ELSE.
*        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
*      ENDIF.
*-------------------------------------------------------------------------------------------*

*-------------------------------------------------------new print---------------------------*

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.

        READ TABLE GT_DATA INTO GS_DATA
        WITH KEY ZSQD = ''
                 ZBOX = 'X'.
        IF SY-SUBRC = 0.
          MESSAGE '请输入必输字段' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          PERFORM FRM_PRINT.
        ENDIF.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

*-------------------------------------------------------------------------------------------*
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
  DATA: L_ZSQFKJE   TYPE ZFI017-ZSQFKJE.
  DATA: L_STRING     TYPE CHAR100.

  FIELD-SYMBOLS:<L_MATNR> TYPE ANY.

  CLEAR L_ZSQFKJE.

*  READ TABLE ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL INDEX 1.
*  IF WA_MOD_CELL-FIELDNAME = 'ZSQFKJE'.
*
**汇总申请金额到抬头行
*    READ TABLE GT_DATA INTO LS_DATA INDEX  WA_MOD_CELL-ROW_ID.
*    IF SY-SUBRC = 0.
*
**去除金额的,号
*      L_STRING = WA_MOD_CELL-VALUE.
*      REPLACE ',' IN L_STRING WITH ''.
*      L_ZSQFKJE  = L_STRING.
*
**汇总申请付款金额（汇总逻辑 =  修改的行 + 其他的行 （对于同一张发票而言））
*      LOOP AT GT_DATA INTO GS_DATA
*      WHERE BELNR = LS_DATA-BELNR
*      AND   GJAHR = LS_DATA-GJAHR
*      AND   EBELN <> ''
*      AND   EBELN <> LS_DATA-EBELN.
*        L_ZSQFKJE = L_ZSQFKJE + GS_DATA-ZSQFKJE.
*      ENDLOOP.
*
*      LOOP AT GT_DATA INTO GS_DATA
*      WHERE BELNR = LS_DATA-BELNR
*      AND   GJAHR = LS_DATA-GJAHR
*      AND   EBELN = ''.
*        GS_DATA-ZSQFKJE = L_ZSQFKJE.
*        MODIFY GT_DATA FROM GS_DATA.
*        CLEAR GS_DATA.
*      ENDLOOP.
*    ENDIF.
*  ELSEIF  WA_MOD_CELL-FIELDNAME = 'WAERS_2'.
*
**    READ TABLE GT_DATA INTO LS_DATA INDEX  WA_MOD_CELL-ROW_ID.
**    IF SY-SUBRC = 0 .
**      TRANSLATE WA_MOD_CELL-VALUE  TO UPPER CASE.
**       IF WA_MOD_CELL-VALUE = 'RMB'.
**        MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
**         ENDIF.
**    ENDIF.
*  ENDIF.
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
  LOOP AT GT_DATA INTO GS_DATA
  WHERE ZBOX = 'X'.
    IF GS_DATA-WAERS_2 = 'RMB'.
      MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
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

    IF GS_DATA-ZSQD IS NOT INITIAL.
      MESSAGE '该申请单已经保存，请勿重复操作，如需修改请进去修改功能' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
      EXIT.
    ENDIF.

    IF GS_DATA-ZSQFKJE + GS_DATA-YSQJE > GS_DATA-NETWR.
      MESSAGE '申请金额不能大于采购订单总金额' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
    ENDIF.
  ENDLOOP.

**检查不同重复保存
*  LOOP AT GT_DATA INTO GS_DATA
*  WHERE ZBOX = 'X'.
*    IF GS_DATA-ZSQD IS NOT INITIAL.
*      MESSAGE '该申请单已经保存，请勿重复操作，如需修改请进去修改功能' TYPE 'S' DISPLAY LIKE 'E'.
*      L_SUBRC = 4.
*      EXIT.
*    ENDIF.
*  ENDLOOP.

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
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI017_1'.
  DATA:L_FORMNAME1 TYPE TDSFNAME VALUE 'ZSFFI017_NEW'.  "13074
  DATA:L_PAGE  TYPE I.
*  DATA:L_PAGE1 TYPE I.                                      " 13074
  DATA:L_GD    TYPE I VALUE 10.
  DATA:L_BUKRS TYPE BUTXT."公司代码描述
  DATA:L_TEMP TYPE STRING.  " 康迪新增
  DATA:L_ZSQD1(20) TYPE C.   " 康迪新增
*  DATA:L_NUM TYPE I.                                        " 13074

* -------------------13074  -------------------------

  DATA : BEGIN OF TY_RBKP,
           BELNR TYPE RBKP-BELNR,
         END OF TY_RBKP.

  DATA : LT_DATA TYPE TABLE OF TY_DATA,
         LS_DATA TYPE TY_DATA.
  DATA : LT_DATA11 TYPE TABLE OF TY_DATA,
         LS_DATA11 TYPE TY_DATA.
  DATA : LT_EKPO TYPE TABLE OF TY_EKPO,
         LS_EKPO TYPE TY_EKPO.
  DATA : LT_EKPO1 TYPE TABLE OF TY_EKPO,
         LS_EKPO1 TYPE TY_EKPO.
  DATA : LT_NORD TYPE TABLE OF ZNEWORDER,
         LS_NORD TYPE ZNEWORDER.
*  DATA : L_TABIX1 TYPE SY-TABIX.
*  DATA : L_EBELN TYPE STRING.
  DATA : LT_RBKP LIKE TABLE OF TY_RBKP,
         LS_RBKP LIKE LINE OF LT_RBKP.

  DATA : L_FPHM TYPE STRING,   " 发票号码连接字段
         L_JETJ TYPE WRBTR.     " 含税金额总额
  DATA : LT_EKBE TYPE TABLE OF EKBE,
         LS_EKBE TYPE EKBE.

  DATA : L_ZZ TYPE I.  " LOOP  自增int

*--------------------------------------------------

*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
  IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
    L_FORMNAME = 'ZSFFI017_1_1'.
  ENDIF.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*
*查询打印的次数
  SELECT * FROM ZFI017_P
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017_P
    FOR ALL ENTRIES IN GT_DATA
    WHERE ZSQD = GT_DATA-ZSQD.

*---------------- 康迪新添--------------------------------*
  L_TEMP = GS_DATA-ZSQD+8(5).
  L_ZSQD1 = GS_DATA-ZSQD.
  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
    IF GS_DATA-ZSQD = L_ZSQD1.
      CONTINUE.
    ELSE.
      L_TEMP = L_TEMP + 1.
      L_ZSQD1 = L_ZSQD1 && '-' && L_TEMP.
    ENDIF.

  ENDLOOP.


*---------------------------------------------------------*

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

  FREE: LT_EBELN.
*  -------------------------- 13074 ----------------------

  LT_DATA11 = GT_DATA.




  DELETE LT_DATA11 WHERE ZBOX <> 'X'.
  DELETE ADJACENT DUPLICATES FROM LT_DATA11 COMPARING BELNR.

  CLEAR LS_EKBE.
  LOOP AT LT_DATA11 INTO LS_DATA11.
    READ TABLE GT_EKBE_2 INTO GS_EKBE_2 WITH KEY EBELN = LS_DATA11-EBELN.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING GS_EKBE_2 TO LS_EKBE.
      APPEND LS_EKBE TO LT_EKBE.
    ENDIF.
  ENDLOOP.

  IF LT_EKBE IS NOT INITIAL.
    SELECT
      RBKP~BELNR
      FROM RBKP
      INTO CORRESPONDING FIELDS OF TABLE LT_RBKP
      FOR ALL ENTRIES IN LT_EKBE
      WHERE RBKP~BELNR = LT_EKBE-BELNR
      AND RBKP~RBSTAT = 5
      AND RBKP~STBLG = ''
      AND RBKP~XRECH = 'X'.
  ENDIF.





*  --------------------------------------------------------

*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*

  IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.
    CLEAR: LS_DATA.
    LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
*&--------------------------ADD BY HANDHJD 20170712 09:05 ---------------------------*

      READ TABLE GT_BSEG_B INTO GS_BSEG_B INDEX 1.
      IF SY-SUBRC EQ 0.
        LS_DATA-FPHM = GS_BSEG_B-ZUONR.   "发票号码

      ENDIF.
*&------金额汇总

      LS_DATA-YFJE = LS_DATA-YFJE + GS_DATA-YFJE.
      LS_DATA-WFJE = LS_DATA-WFJE + GS_DATA-WFJE.
      LS_DATA-ZSQFKJE = LS_DATA-ZSQFKJE + GS_DATA-ZSQFKJE.
*&------收集所有采购订单
      CLEAR:LS_EBELN.
      LS_EBELN-EBELN = GS_DATA-EBELN.
*-----------保修期限和保修起始日期----------------

      LS_DATA-BXQX =  GS_DATA-BXQX.
      LS_DATA-BXQSRQ =  GS_DATA-BXQSRQ.

      APPEND LS_EBELN TO LT_EBELN.
    ENDLOOP.
    LS_DATA1 = GS_DATA.
    LS_DATA1-YFJE = LS_DATA-YFJE.
    LS_DATA1-WFJE = LS_DATA-WFJE.
    LS_DATA1-ZSQFKJE = LS_DATA-ZSQFKJE.
    LS_DATA1-FPHM = LS_DATA-FPHM.   "发票号码
**-----------保修期限和保修起始日期----------------
    LS_DATA1-BXQX =  LS_DATA-BXQX.

    IF LS_DATA1-BXQX EQ '不保修'.
      LS_DATA1-BXQSRQ = ''.
      LS_DATA1-YEAR2 = ''.
      LS_DATA1-MONTH = ''.
      LS_DATA1-DAY = ''.
    ELSE.
      LS_DATA1-BXQSRQ =  LS_DATA-BXQSRQ.
      LS_DATA1-YEAR = LS_DATA1-BXQSRQ+0(4).   "年
      LS_DATA1-MONTH = LS_DATA1-BXQSRQ+4(2).  "月
      LS_DATA1-DAY = LS_DATA1-BXQSRQ+6(2).   "日

      LS_DATA1-YEAR2 =  LS_DATA1-YEAR +  LS_DATA1-BXQX.   "年
    ENDIF.




    PERFORM CONV_AMOUNT USING LS_DATA1-ZSQFKJE
                     CHANGING LS_DATA1-ZSQFKJEDX.
    APPEND LS_DATA1 TO LT_DATA1.
*删除发票对应的采购订单行

    DELETE LT_DATA1 WHERE BELNR IS NOT INITIAL
                    AND   EBELN IS NOT INITIAL.

    SELECT SINGLE BUTXT FROM T001
     INTO L_BUKRS
     WHERE BUKRS = S_BUKRS-LOW.

    READ TABLE GT_ZFI017_P INTO GS_ZFI017_P
    WITH KEY ZSQD = LS_DATA-ZSQD.
    CLEAR L_PAGE.
    L_PAGE = GS_ZFI017_P-PRINT_NUM + 1.


*    --------------------------------13074 ------------------------------------new insert
*    CLEAR P_INDEX.


    LT_DATA = GT_DATA.

    DELETE LT_DATA WHERE ZBOX <> 'X'.
    DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING EBELN.

    SELECT
       EBELN
       EBELP
       BUKRS
       NETWR
      FROM EKPO
      INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
      FOR ALL ENTRIES IN LT_DATA
      WHERE EKPO~EBELN = LT_DATA-EBELN.

    SELECT
      RSEG~XBLNR
      RSEG~EBELN
      RSEG~MATNR
      RSEG~SHKZG
      RSEG~WRBTR
      RSEG~MENGE
      RSEG~BELNR
      EKPO~TXZ01
      EKPO~MWSKZ
      EKPO~EBELP
      FROM RSEG
      INNER JOIN EKPO ON
      EKPO~EBELN = RSEG~EBELN AND EKPO~EBELP = RSEG~EBELP
      INTO CORRESPONDING FIELDS OF TABLE LT_NORD
      FOR ALL ENTRIES IN LT_EKPO
      WHERE RSEG~EBELN = LT_EKPO-EBELN AND RSEG~EBELP = LT_EKPO-EBELP.

    LOOP AT LT_NORD INTO LS_NORD.
      READ TABLE LT_RBKP INTO LS_RBKP WITH KEY BELNR = LS_NORD-BELNR.

      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LS_NORD TO GS_NORD.
        IF LS_NORD-MWSKZ = 'A0'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 100 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'A1'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 117 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'A2'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 102 ) / 100.
*      ELSEIF LS_NORD-MWSKZ = 'A1'.
*        LS_NORD-WRBTR = ( LS_NORD-WRBTR * 100 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J0'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 100 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J1'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 117 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J2'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 110 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J3'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 111 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J5'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 106 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J6'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 103 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J7'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 100 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J8'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 117 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'J9'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 113 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'JA'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 100 ) / 100.
        ELSEIF LS_NORD-MWSKZ = 'JB'.
          LS_NORD-WRBTR = ( LS_NORD-WRBTR * 117 ) / 100.

        ENDIF.
        GS_NORD-WRBTR = LS_NORD-WRBTR.
        GS_NORD-ZHSDJ = LS_NORD-WRBTR / GS_NORD-MENGE.
        IF LS_NORD-SHKZG = 'S'.
          GS_NORD-MENGE = LS_NORD-MENGE.
        ELSE.
          GS_NORD-MENGE = LS_NORD-MENGE * ( -1 ).
        ENDIF.

        L_JETJ = L_JETJ + GS_NORD-WRBTR.


        APPEND GS_NORD TO GT_NORD.
        CLEAR GS_NORD.
*        L_NUM = L_NUM + 1.
      ENDIF.

    ENDLOOP.



    L_ZZ = 1.
    LOOP AT LT_RBKP INTO LS_RBKP.
      IF L_ZZ = 1.
        CONCATENATE L_FPHM LS_RBKP-BELNR INTO L_FPHM.
      ELSE.
        CONCATENATE L_FPHM '、' LS_RBKP-BELNR INTO L_FPHM.
      ENDIF.
      L_ZZ = L_ZZ + 1.

    ENDLOOP.



    CLEAR LS_NORD.
    FREE : LT_NORD.

*    ------------------------------------------------------------------------


    EXPORT LT_EBELN TO MEMORY ID 'EBELN'.



*    FREE: lt_ebeln.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*
    CALL FUNCTION G_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL
        L_PAGE             = L_PAGE
        L_BUKRS            = L_BUKRS
        L_ZSQD1            = L_ZSQD1
        L_FPHM             = L_FPHM
        L_JETJ             = L_JETJ
*        L_EBELN            = L_EBELN
*       npage              = npageline
*       w_head             = lw_prt
*         TABLES
*       t_item             = lt_prt[]
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
  ELSE.

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

        READ TABLE GT_ZFI017_P INTO GS_ZFI017_P
        WITH KEY ZSQD = LS_DATA-ZSQD.
        CLEAR L_PAGE.
        L_PAGE = GS_ZFI017_P-PRINT_NUM + 1.

"HANDLH ADD
         APPEND LINES OF LT_DATA TO GT_DATA3.
  "GT_DATA3 = LT_DATA .

        CALL FUNCTION G_NAME
          EXPORTING
            CONTROL_PARAMETERS = CONTROL
            L_PAGE             = L_PAGE
            L_BUKRS            = L_BUKRS
*           npage              = npageline
*           w_head             = lw_prt
*         TABLES
*           t_item             = lt_prt[]
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
  ENDIF.

*  ---------------------------------------new insert -13074------------------
  IF L_FPHM IS NOT INITIAL OR L_JETJ <> 0 OR L_JETJ <> '0.00'. "


  IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = L_FORMNAME1         "smartforms的名字
      IMPORTING
        FM_NAME            = G_NAME              "对应的smartforms的函数
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
*    WAIT UP TO 1 SECONDS.
    IF SY-SUBRC <> 0.
*     error handling
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      EXIT.
    ENDIF.




    CALL FUNCTION G_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL
*        L_NUM              = L_NUM
      TABLES
        GT_NORD            = GT_NORD
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


  ENDIF.

  ENDIF.
* ---------------------------------------------------------------------------
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
    REFRESH GT_ZFI017_P_1.

    LOOP AT GT_DATA INTO GS_DATA
    WHERE ZBOX = 'X'.
      GS_ZFI017_P_1-ZSQD = GS_DATA-ZSQD.

      READ TABLE GT_ZFI017_P INTO GS_ZFI017_P
      WITH KEY ZSQD = GS_DATA-ZSQD.
      IF SY-SUBRC = 0.
        GS_ZFI017_P_1-PRINT_NUM = GS_ZFI017_P-PRINT_NUM + 1.
      ELSE.
        GS_ZFI017_P_1-PRINT_NUM = 1.
      ENDIF.

      APPEND GS_ZFI017_P_1 TO GT_ZFI017_P_1.
      CLEAR GS_ZFI017_P_1.
    ENDLOOP.
  ENDIF.

  MODIFY ZFI017_P FROM TABLE GT_ZFI017_P_1.

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
