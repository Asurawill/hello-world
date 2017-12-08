REPORT ZFI017_2_KD.
*&---------------------------------------------------------------------*
*& Report  ZFI017
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/6
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
*&
*& Modify by     : IT02-魏云
*& Modify date   :20160701
*& Request       :
*& Descriptions  :过账抬头文本及明细文本更改为：需过账明细的合同编号
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:EKKO,RBKP,ZFI017,PRPS.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        ZBOX      TYPE   C,
        STATU     TYPE   ICONNAME,       "状态烂
        EBELN     TYPE   EKKO-EBELN,     "采购订单号
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
        YJHJE     TYPE   EKBE-DMBTR,     "已交货金额
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
        YFBL      TYPE   ZFI017-YFBL,    "已付比例
        WFJE      TYPE   ZFI017-WFJE,    "未付金额
        ZCH       TYPE   ZFI017-ZCH,     "拆行
        ZNAME1    TYPE   ZFI006-ZNAME1,  "审批人
        ZNAME     TYPE   ZFI017-ZNAME,   "创建人
        ZDATE     TYPE   ZFI017-ZDATE,   "创建日期
        ZTIME     TYPE   ZFI017-ZTIME,   "创建时间
        HTBH      TYPE   STRING ,        "合同编号
        FPJE      TYPE   EKBE-WRBTR,     "发票金额（净值）
        FPJE_1    TYPE   EKBE-WRBTR,     "发票金额（含税）
        YZFP      TYPE   EKBE-WRBTR,     "预制发票（净值）
        YZFP_1    TYPE   EKBE-WRBTR,      "预制发票（含税）
        JHJE_1    TYPE   EKBE-WRBTR,     "交货金额（含税）
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  BEGIN
        YJFKRQ    TYPE EKKO-YJFKRQ1,
        YJFKJE    TYPE EKKO-YJFKJE1,
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  END
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  BEGIN
        POSID     TYPE PRPS-POSID , "WBS元素外码
        POST1     TYPE PRPS-POST1 , "WBS描述
        PSPID     TYPE PROJ-PSPID , "项目定义
        POST2     TYPE PROJ-POST1 , "项目描述
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  END

*&---------------------13074 ----insert- -------0803----------
        ZJDFKJH   TYPE C,
        ZJDYFJE   TYPE ZFI017-ZSQFKJE,
        ZJDYSQJE  TYPE ZFI017-ZSQFKJE,
        ZCEJE     TYPE DMBTR,
        ZKSQJE    TYPE DMBTR,
*&--------------------------------------------------------
      END OF TY_DATA.

TYPES:BEGIN OF TY_HEAD,
        BLDAT TYPE BKPF-BLDAT, "凭证日期
        BUDAT TYPE BKPF-BUDAT, "过账日期
        BUKRS TYPE BKPF-BUKRS, "公司代码
        WAERS TYPE BKPF-WAERS, "公司代码
        BKTXT TYPE BKPF-BKTXT, "抬头凭证
        BELNR TYPE BKPF-BELNR, "会计凭证
        GJAHR TYPE BKPF-GJAHR, "会计年度
        BLART TYPE BKPF-BLART,
      END OF TY_HEAD.

TYPES:BEGIN OF TY_ITEM,
        BUZEI     TYPE BSEG-BUZEI, "项目号
        BSCHL     TYPE BSEG-BSCHL, "记账码
        HKONT     TYPE BSEG-HKONT, "科目
        HKTXT     TYPE SKAT-TXT20, "科目描述
        LIFNR     TYPE BSEG-LIFNR, "供应商
        KUNNR     TYPE BSEG-KUNNR, "客户
        WRBTR     TYPE BSEG-WRBTR, "金额
        PSWSL     TYPE BSEG-PSWSL, "货币
        RSTGR     TYPE BSEG-RSTGR, "原因代码
        ZUONR     TYPE BSEG-ZUONR, "汇票号
        ZUONR_1   TYPE BSEG-ZUONR, "付款申请单号
        SGTXT     TYPE BSEG-SGTXT, "行项目摘要
        PSPID     TYPE PROJ-PSPID, "项目编号
        CELLSTYLE TYPE LVC_T_STYL, "单元格状态
      END OF TY_ITEM.

TYPES:BEGIN OF TY_SUMYF ,
        ZSQD  TYPE   ZFI017-ZSQD,    "付款申请单
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        YFJE  TYPE   EKBE-DMBTR,     "已付金额
      END OF TY_SUMYF.

"发票合计
TYPES:BEGIN OF TY_FP,
        EBELN TYPE EBELN,
        MWSKZ TYPE MWSKZ,
        WRBTR TYPE EKBE-WRBTR,
      END OF TY_FP.

TYPES:BEGIN OF TY_SE,
        MWSKZ TYPE EKBE-MWSKZ,
        KBETR TYPE FTAXP-KBETR,
      END OF TY_SE.

TYPES:BEGIN OF TY_JH,
        EBELN TYPE EBELN,
        WRBTR TYPE WRBTR,
      END OF TY_JH.

TYPES:BEGIN OF TY_EBELN,
        EBELN TYPE EKBE-EBELN,
      END OF TY_EBELN.

*& ---------------------13074  INSERT 0804 -----------------

TYPES : BEGIN OF TY_MIDEBELN,
        EBELN     TYPE EBELN,
        ZJDFKJH   TYPE ZFI017-ZJDFKJH,
        YSQJE     TYPE ZFI017-YSQJE,
        YFJE      TYPE EKBE-DMBTR,     "已付金额
        YFJE1     TYPE ZFI017-YFJE1,
        ZJDYFJE   TYPE ZFI017-ZSQFKJE,
        ZJDYSQJE  TYPE ZFI017-ZJDYSQJE,
        ZCEJE     TYPE DMBTR,
        ZKSQJE    TYPE DMBTR,
        YFBL      TYPE ZFI017-YFBL,    "已付比例
        WFJE      TYPE ZFI017-WFJE,    "未付金额
        NETWR   TYPE ZFI017-NETWR,
        END OF TY_MIDEBELN.
DATA : GT_MIDEBELN TYPE TABLE OF TY_MIDEBELN,
      GS_MIDEBELN LIKE LINE OF GT_MIDEBELN.
*& ---------------------------------------------------------


DATA:GT_EBELN TYPE TABLE OF TY_EBELN,
     GS_EBELN TYPE TY_EBELN.

DATA:GT_SE_1 TYPE TABLE OF TY_SE,
     GT_SE_2 TYPE TABLE OF TY_SE,
     GT_SE   TYPE TABLE OF TY_SE,
     GS_SE   TYPE TY_SE.

DATA:GT_FP_2 TYPE TABLE OF TY_FP,
     GS_FP_2 TYPE TY_FP.

DATA:GT_FP_P TYPE TABLE OF TY_FP,
     GS_FP_P TYPE TY_FP.

DATA GT_RBKP    TYPE TABLE OF RBKP.
DATA GS_RBKP    TYPE RBKP.
DATA:GS_SUMYF   TYPE TY_SUMYF.
DATA:GT_SUMYF   TYPE TABLE OF TY_SUMYF.

DATA: GT_FTAXP LIKE  TABLE OF FTAXP .
DATA: GS_FTAXP LIKE FTAXP.

DATA:GT_JH TYPE TABLE OF TY_JH,
     GS_JH TYPE TY_JH.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI017    TYPE TABLE OF ZFI017.
DATA GS_ZFI017    TYPE ZFI017.

DATA GT_DATA      TYPE TABLE OF TY_DATA.
DATA GS_DATA      TYPE TY_DATA.

DATA GT_ZFI017_1  TYPE TABLE OF ZFI017.
DATA GS_ZFI017_1  TYPE ZFI017.

DATA GT_BSEG      TYPE TABLE OF BSEG.
DATA GS_BSEG      TYPE BSEG.

DATA GT_BKPF      TYPE TABLE OF BKPF.
DATA GS_BKPF      TYPE BKPF.

DATA GT_HEAD      TYPE TABLE OF TY_HEAD.
DATA GS_HEAD      TYPE TY_HEAD.

DATA GT_ITEM      TYPE TABLE OF TY_ITEM.
DATA GS_ITEM      TYPE TY_ITEM.

DATA G_WRBTR      TYPE BSEG-WRBTR.

DATA G_ANSWER     TYPE STRING. "控制弹出框

DATA G_EDIT_MOD   TYPE C.
DATA G_CHANGED    TYPE C.


*& --------------------     13074 - 0803 -----------------
DATA : GT_ZFI017KD TYPE TABLE OF ZFI017_KD,
      GS_ZFI017KD LIKE LINE OF GT_ZFI017KD.
*& --------------------------------------------------------

CONSTANTS: GC_EDITABLE TYPE C VALUE 'X',
           GC_READONLY TYPE C VALUE ''.

DATA GT_SKAT TYPE TABLE OF SKAT.
DATA GS_SKAT TYPE SKAT.

DATA: G_SUC.
DATA L_STRING TYPE STRING.
FIELD-SYMBOLS: <FS_ITEM> TYPE TY_ITEM.

* BAPI_ACC_DOCUMENT_REV_POST
DATA: WA_REVERSAL TYPE BAPIACREV,
      WA_BUS      TYPE BAPIACHE09.

DATA: G_STGRD TYPE BKPF-STGRD.

DATA GT_EKBE   TYPE TABLE OF EKBE.
DATA GS_EKBE   TYPE EKBE.

DATA:GT_EKBE_2 TYPE TABLE OF EKBE,
     GS_EKBE_2 TYPE EKBE.

DATA:GT_EKBE_P TYPE TABLE OF EKBE,
     GS_EKBE_P TYPE EKBE.

DATA:GT_EKBE_1 TYPE TABLE OF EKBE,
     GS_EKBE_1 TYPE EKBE.


DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

DATA GT_EKPO1 TYPE TABLE OF EKPO.
DATA GS_EKPO1 TYPE EKPO.
************************************************************************
*      DEFINE CLASS
************************************************************************
INCLUDE ZFI017_CLS_KD.
*INCLUDE ZFI017_CLS.

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

   IF &1 = 'HKONT' OR &1 = 'RSTGR'.
    gw_lvc-f4availabl = 'X'.
  ENDIF.
IF gw_lvc-fieldname =  'NETWR_1' OR gw_lvc-fieldname = 'YJHJE_1 '  OR gw_lvc-fieldname = 'FPPZJE_1' .
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

DATA: GT_OO_EXCLUDE TYPE UI_FUNCTIONS.
DATA: GR_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE TYPE SY-UCOMM,
      SAVE_OK TYPE SY-UCOMM.
************************************************************************
* BAPI
************************************************************************
DATA: WA_DOCUMENTHEADER TYPE BAPIACHE09,
      IT_ACCOUNTGL      TYPE TABLE OF BAPIACGL09,
      WA_ACCOUNTGL      TYPE BAPIACGL09,
      IT_ACCOUNTPAYABLE TYPE TABLE OF BAPIACAP09,
      WA_ACCOUNTPAYABLE TYPE BAPIACAP09,
      IT_CURRENCYAMOUNT TYPE TABLE OF BAPIACCR09,
      WA_CURRENCYAMOUNT TYPE BAPIACCR09,
      IT_CRITERIA       TYPE TABLE OF BAPIACKEC9,
      WA_CRITERIA       TYPE BAPIACKEC9,
      IT_VALUEFIELD     TYPE TABLE OF BAPIACKEV9,
      WA_VALUEFIELD     TYPE BAPIACKEV9,
      IT_EXTENSION2     TYPE TABLE OF BAPIPAREX,
      WA_EXTENSION2     TYPE BAPIPAREX,
      IT_RETURN         TYPE TABLE OF BAPIRET2,
      WA_RETURN         TYPE BAPIRET2.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: WA_ZACCDOCUEXT TYPE ZACCDOCUEXT.

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
*                S_BELNR   FOR RBKP-BELNR,                                      "发票号
               " S_BUDAT   FOR RBKP-BUDAT,"过账日期
                S_WBS     FOR PRPS-POSID,                                    "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI017-ZSQRQ ,                                   "申请日期
                S_ZSQFKR  FOR ZFI017-ZSQFKRQ,                                  "申请付款日期
                S_ZCLJD   FOR ZFI017-ZCLJD,                                    "处理进度
                S_ZSQD    FOR ZFI017-ZSQD.                                     "申请单
SELECTION-SCREEN END OF BLOCK BLK1.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

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

*&--代码添加 BY HANDYBY 13.07.2017 15:38:43  BEGIN
  IF S_WBS[] IS NOT INITIAL .
    LOOP AT S_WBS .
      IF S_WBS-LOW IS NOT INITIAL .
        S_WBS-OPTION = 'CP'.
        CONCATENATE S_WBS-LOW '*' INTO S_WBS-LOW .
        MODIFY S_WBS .
      ENDIF.
      IF S_WBS-HIGH IS NOT INITIAL .
*        CONCATENATE S_WBS-HIGH '%' INTO S_WBS-HIGH .
*        MODIFY S_WBS .
      ENDIF.
    ENDLOOP.
  ENDIF.
*&--代码添加 BY HANDYBY 13.07.2017 15:38:43  END

*当有输入采购订单号码
*查询自建表数据
  SELECT * FROM ZFI017
   INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017
   WHERE BUKRS    IN S_BUKRS
   AND   LIFNR    IN S_LIFNR
   AND   EBELN    IN S_EBELN
*   AND   GJAHR    IN S_GJAHR
   AND   ZSQRQ    IN S_ZSQRQ
   AND   ZSQFKRQ  IN S_ZSQFKR
   AND   ZCLJD    IN S_ZCLJD
   AND   ZSQD     IN S_ZSQD
   AND   POSID    IN S_WBS
   AND   STATU    <> ICON_DELETE.

*查询清帐凭证
  IF GT_ZFI017[] IS NOT INITIAL.
    MOVE-CORRESPONDING GT_ZFI017 TO GT_EBELN .
    SORT GT_EBELN BY EBELN .
    DELETE ADJACENT DUPLICATES FROM GT_EBELN COMPARING EBELN .

    "*查询订单税码
    SELECT * FROM EKPO
     INTO CORRESPONDING FIELDS OF TABLE GT_EKPO1
     FOR ALL ENTRIES IN GT_EBELN
     WHERE EKPO~EBELN = GT_EBELN-EBELN
     AND  LOEKZ <> 'L'
     AND  MWSKZ NE ''.

    SORT GT_EKPO1 BY EBELN EBELP.

    SELECT * FROM BSEG
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
     FOR ALL ENTRIES IN GT_ZFI017
     WHERE BUKRS = GT_ZFI017-BUKRS
*   AND   GJAHR = GT_ZFI017-GJAHR
     AND   LIFNR = GT_ZFI017-LIFNR
*     AND   ZUONR = GT_ZFI017-ZSQD
     AND   BSCHL = '21'.
    "*查询交货历史  IT02150520
    SELECT * FROM EKBE
     INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
     FOR ALL ENTRIES IN GT_ZFI017
     WHERE EBELN = GT_ZFI017-EBELN
   .

    MOVE-CORRESPONDING GT_EKBE TO GT_EKBE_2.

    DELETE GT_EKBE_2 WHERE VGABE NE '2'.
    SORT GT_EKBE_2 BY EBELN EBELP .

    MOVE-CORRESPONDING GT_EKBE TO GT_EKBE_P.

    DELETE GT_EKBE_P WHERE VGABE NE 'P'.
    SORT GT_EKBE_P BY EBELN EBELP .

    MOVE-CORRESPONDING GT_EKBE TO GT_EKBE_1.

    DELETE GT_EKBE_1 WHERE VGABE NE '1'.
    SORT GT_EKBE_1 BY EBELN EBELP .
  ENDIF.
  IF GT_BSEG IS NOT INITIAL .
*查询是否冲销
    SELECT * FROM BKPF
      INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
      FOR ALL ENTRIES IN GT_BSEG
      WHERE GJAHR = GT_BSEG-GJAHR
      AND   BUKRS = GT_BSEG-BUKRS
      AND   BELNR = GT_BSEG-BELNR.
  ENDIF.

*查询总账科目描述
  SELECT *
    INTO TABLE GT_SKAT
    FROM SKAT
    WHERE SPRAS = SY-LANGU
      AND KTOPL = '1000'.

  SORT GT_SKAT BY SAKNR.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_RBKP
    FROM RBKP
    WHERE BUKRS = S_BUKRS-LOW.
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

  "统计交货金额(含税)
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

  LOOP AT  GT_ZFI017 INTO GS_ZFI017.
*    GS_ZFI017-YJHJE = 0.
*    GS_ZFI017-YJHJE_1 = 0.

*    LOOP AT GT_EKBE INTO  GS_EKBE
*    WHERE EBELN = GS_ZFI017-EBELN
*    AND VGABE = '1'.
*      IF GS_EKBE-BWART = '101'.
*        GS_ZFI017-YJHJE   = GS_ZFI017-YJHJE   + GS_EKBE-DMBTR.
*        GS_ZFI017-YJHJE_1 = GS_ZFI017-YJHJE_1 + GS_EKBE-WRBTR.
*      ENDIF.
*      IF GS_EKBE-BWART = '122' OR GS_EKBE-BWART = '102'.
*        GS_ZFI017-YJHJE   = GS_ZFI017-YJHJE   - GS_EKBE-DMBTR.
*        GS_ZFI017-YJHJE_1 = GS_ZFI017-YJHJE_1 - GS_EKBE-WRBTR.
*      ENDIF.
*    ENDLOOP.

    MOVE-CORRESPONDING GS_ZFI017 TO GS_DATA.
    READ TABLE GT_RBKP INTO GS_RBKP WITH KEY BELNR = GS_DATA-BELNR.
    IF SY-SUBRC = 0.
      GS_DATA-SGTXT = GS_RBKP-SGTXT.  "发票抬头文本 "IT02 150709 添加 发票文本
    ENDIF .
**初始化凭证
*    GS_DATA-BELNR_F = ''.
*    GS_DATA-GJAHR_F = ''.

*查询清帐凭证
*    LOOP AT GT_BSEG INTO GS_BSEG
*    WHERE BUKRS = GS_DATA-BUKRS
*    AND   LIFNR = GS_DATA-LIFNR
*    AND   ZUONR = GS_DATA-ZSQD
*    AND   BSCHL = '21'.
*
**排除冲销凭证
*      READ TABLE GT_BKPF INTO GS_BKPF
*      WITH KEY GJAHR = GS_BSEG-GJAHR
*               BUKRS = GS_BSEG-BUKRS
*               BELNR = GS_BSEG-BELNR.
*      IF SY-SUBRC = 0.
*        IF GS_BKPF-STBLG IS INITIAL.
*          GS_DATA-BELNR_F = GS_BSEG-BELNR.
*          GS_DATA-GJAHR_F = GS_BSEG-GJAHR.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*当初始状态，审批人为空
    IF GS_DATA-ZCLJD = '1'.
      GS_DATA-ZNAME1 = ''.
    ENDIF.

**把确认的结账，但是没有找到凭证的，更改其状态.（为了排除确认付款后，冲销的情况）
*    IF GS_DATA-ZCLJD = '3'
*     AND GS_DATA-BELNR_F IS INITIAL
*     AND GS_DATA-GJAHR_F IS INITIAL.
*      GS_DATA-ZCLJD = '2'.
*      GS_DATA-STATU = ICON_GREEN_LIGHT.
*    ENDIF.
    "读取合同编号
    "  * 根据采购订单号 调用函数 Read_text 读取文本 ，文本对象-EKKO ，文本标识-F06 ADD IT02 151120

    G_OBJNAME = GS_DATA-EBELN.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'F06'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'EKKO'
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
    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

*按照申请单号进行排序
  SORT GT_DATA BY ZSQD BELNR EBELN.
  "150601 BEGIN 补充漏掉计算 已付款金额的数据
  SORT GT_ZFI017 BY ZSQD EBELN.
*  LOOP AT GT_ZFI017 INTO GS_ZFI017 WHERE ZCLJD = '3'.
*     clear GS_SUMYF .
*     GS_SUMYF-ZSQD = GS_ZFI017-ZSQD .
*     GS_SUMYF-EBELN = GS_ZFI017-EBELN.
*     GS_SUMYF-YFJE = GS_ZFI017-ZSQFKJE.
*     COLLECT GS_SUMYF INTO GT_SUMYF.
*  ENDLOOP.
*  LOOP AT GT_DATA INTO GS_DATA WHERE ZCLJD = '3' .
*     READ TABLE GT_SUMYF INTO GS_SUMYF WITH KEY ZSQD = GS_DATA-ZSQD EBELN = GS_DATA-EBELN .
*     IF SY-SUBRC = 0.
*       GS_DATA-YFJE = GS_SUMYF-YFJE .
*       MODIFY GT_DATA FROM GS_DATA.
*      ENDIF.
*
*   ENDLOOP.
                                                            "150601 END
**更新自建表
*  MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
*  MODIFY ZFI017 FROM TABLE GT_ZFI017.
*  REFRESH GT_ZFI017.
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

    MODIFY GT_DATA FROM GS_DATA.
  ENDLOOP.

*& ----------------------------13074 --- insert 0804 --------------

  DATA : LT_MIDEBELN TYPE TABLE OF TY_MIDEBELN,
        LS_MIDEBELN LIKE LINE OF LT_MIDEBELN.
*  DATA : LT_DATA TYPE TABLE OF GS_DATA,
*        LS_DATA LIKE LINE OF LT_DATA.


  DATA : L_YSQJE        TYPE   ZFI017-YSQJE,
         L_YFJE         TYPE   EKBE-DMBTR,
         L_ZJDYFJE      TYPE   ZFI017-ZSQFKJE,
         L_ZJDYSQJE     TYPE   ZFI017-ZJDYSQJE,
         L_CEJE         TYPE   DMBTR,
         L_KSQJE        TYPE   DMBTR,
         L_YFBL         TYPE   ZFI017-YFBL,
         L_WFJE         TYPE   ZFI017-WFJE.

  CLEAR : L_YSQJE , L_YFJE , L_ZJDYFJE , L_ZJDYSQJE ,
          L_CEJE , L_KSQJE , L_YFBL , L_WFJE.

  CLEAR : LT_MIDEBELN.
  LOOP AT GT_DATA INTO GS_DATA.

    MOVE-CORRESPONDING GS_DATA TO LS_MIDEBELN.
    APPEND LS_MIDEBELN TO LT_MIDEBELN.

  ENDLOOP.

  SORT LT_MIDEBELN BY EBELN ZJDFKJH.
  DELETE ADJACENT DUPLICATES FROM LT_MIDEBELN COMPARING EBELN ZJDFKJH.
*  CLEAR : GS_MIDEBELN.
  LOOP AT LT_MIDEBELN INTO LS_MIDEBELN.
*节点已付金额

    CLEAR : GS_MIDEBELN , GS_ZFI017.
    IF LS_MIDEBELN-ZJDFKJH = '1'.
      LOOP AT  GT_ZFI017 INTO GS_ZFI017
                          WHERE EBELN = LS_MIDEBELN-EBELN
                          AND   ZCLJD = '3'
                          AND   ZJDFKJH = '1'.
        GS_MIDEBELN-ZJDYFJE = GS_MIDEBELN-ZJDYFJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.
*节点已申请金额

      LOOP AT GT_ZFI017 INTO GS_ZFI017
       WHERE EBELN = LS_MIDEBELN-EBELN
       AND   ZJDFKJH = '1'
       AND ( ZCLJD = '3'
       OR   ZCLJD  = '2'
       OR   ZCLJD  = '1')
       AND   STATU    <> ICON_DELETE.
        GS_MIDEBELN-ZJDYSQJE = GS_MIDEBELN-ZJDYSQJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.
    ELSEIF LS_MIDEBELN-ZJDFKJH = '2'.

      LOOP AT  GT_ZFI017 INTO GS_ZFI017
                          WHERE EBELN = LS_MIDEBELN-EBELN
                          AND   ZCLJD = '3'
                          AND   ZJDFKJH = '2'.
        GS_MIDEBELN-ZJDYFJE = GS_MIDEBELN-ZJDYFJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.
*节点已申请金额

      LOOP AT GT_ZFI017 INTO GS_ZFI017
                         WHERE EBELN = LS_MIDEBELN-EBELN
                         AND ( ZCLJD = '3'
                         OR   ZCLJD  = '2'
                         OR   ZCLJD  = '1')
                         AND   ZJDFKJH = '2'
                         AND   STATU    <> ICON_DELETE.
        GS_MIDEBELN-ZJDYSQJE = GS_MIDEBELN-ZJDYSQJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.

    ELSE.
      LOOP AT  GT_ZFI017 INTO GS_ZFI017
                          WHERE EBELN = LS_MIDEBELN-EBELN
                          AND   ZCLJD = '3'
                          AND   ZJDFKJH = '3'.
        GS_MIDEBELN-ZJDYFJE = GS_MIDEBELN-ZJDYFJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.

*节点已申请金额
      LOOP AT GT_ZFI017 INTO GS_ZFI017
                         WHERE EBELN = LS_MIDEBELN-EBELN
                         AND ( ZCLJD = '3'
                         OR   ZCLJD  = '2'
                         OR   ZCLJD  = '1')
                         AND   ZJDFKJH = '3'
                         AND   STATU    <> ICON_DELETE.
        GS_MIDEBELN-ZJDYSQJE = GS_MIDEBELN-ZJDYSQJE + GS_ZFI017-ZSQFKJE.
      ENDLOOP.
    ENDIF.


*已付款金额
     CLEAR L_YFJE.
      LOOP AT  GT_ZFI017 INTO GS_ZFI017
                          WHERE EBELN = LS_MIDEBELN-EBELN
                          AND   ZCLJD = '3'.
          " AND         BELNR = ''.
          "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
          L_YFJE = L_YFJE + GS_ZFI017-ZSQFKJE.  "150527 YEFE 字段值为空 才注释
      ENDLOOP.
     GS_MIDEBELN-YFJE = L_YFJE.

*已申请金额
     CLEAR L_YSQJE.
        LOOP AT GT_ZFI017  INTO GS_ZFI017
                            WHERE EBELN = LS_MIDEBELN-EBELN
                            AND  EBELN = LS_MIDEBELN-EBELN
                            AND ( ZCLJD = '3'
                            OR   ZCLJD  = '2'
                            OR   ZCLJD  = '1')
                            AND   STATU    <> ICON_DELETE.
          L_YSQJE = L_YSQJE + GS_ZFI017-ZSQFKJE.
        ENDLOOP.
     GS_MIDEBELN-YSQJE = L_YSQJE.
*应付金额
     GS_MIDEBELN-YFJE1     = LS_MIDEBELN-NETWR.
*未付金额
     GS_MIDEBELN-WFJE = GS_MIDEBELN-YFJE1 - GS_MIDEBELN-YFJE.


*已付比例
     IF GS_MIDEBELN-YFJE1 NE  0.
        GS_MIDEBELN-YFBL  = ( GS_MIDEBELN-YFJE / LS_MIDEBELN-NETWR ) * 100.
     ENDIF.


*   订单超额金额
     GS_MIDEBELN-ZCEJE = GS_MIDEBELN-YFJE - LS_MIDEBELN-NETWR.
*   订单可申请金额
     GS_MIDEBELN-ZKSQJE  = LS_MIDEBELN-NETWR - GS_MIDEBELN-YSQJE.
     GS_MIDEBELN-ZJDFKJH = LS_MIDEBELN-ZJDFKJH.
     GS_MIDEBELN-EBELN   = LS_MIDEBELN-EBELN.
     GS_MIDEBELN-NETWR   = LS_MIDEBELN-NETWR.
     APPEND GS_MIDEBELN TO GT_MIDEBELN.

  ENDLOOP.

LOOP AT GT_DATA INTO GS_DATA.
  READ TABLE GT_MIDEBELN INTO GS_MIDEBELN WITH KEY
                                EBELN   = GS_DATA-EBELN
                                ZJDFKJH = GS_DATA-ZJDFKJH.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING GS_MIDEBELN TO GS_DATA.
  ENDIF.

  MODIFY GT_DATA FROM GS_DATA.
ENDLOOP.


*& ----------------------------------------------------------------


*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  BEGIN
*  DATA: BEGIN OF LS_EKKO,
*          EBELN   TYPE EKKO-EBELN,
*          YJFKRQ1 TYPE EKKO-YJFKRQ1,
*          YJFKJE1 TYPE EKKO-YJFKJE1,
*          YJFKRQ2 TYPE EKKO-YJFKRQ2,
*          YJFKJE2 TYPE EKKO-YJFKJE2,
*          YJFKRQ3 TYPE EKKO-YJFKRQ3,
*          YJFKJE3 TYPE EKKO-YJFKJE3,
*        END OF LS_EKKO.
*  DATA LT_EKKO LIKE TABLE OF LS_EKKO .
*  SELECT EBELN
*         YJFKRQ1
*         YJFKJE1
*         YJFKRQ2
*         YJFKJE2
*         YJFKRQ3
*         YJFKJE3
*    INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
*    FROM EKKO
*     FOR ALL ENTRIES IN GT_DATA
*   WHERE EBELN = GT_DATA-EBELN .
*  SORT LT_EKKO BY EBELN .
*
*  DATA: LT_DATA2 LIKE GT_DATA,
*        LS_DATA2 LIKE LINE OF LT_DATA2.
*  LOOP AT GT_DATA INTO GS_DATA .
*    IF GS_DATA-BUKRS = '1700' OR GS_DATA-BUKRS = '1710'.
*
*      READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH .
*      IF SY-SUBRC = 0 .
*        MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ1 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE1 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ2 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE2 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ3 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE3 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*      ENDIF.
*
*    ELSE .
*
*      MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
*      INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*
*    ENDIF.
*    CLEAR GS_DATA .
*    CLEAR LS_EKKO .
*    CLEAR LS_DATA2 .
*  ENDLOOP.
*
*  REFRESH GT_DATA .
*  MOVE-CORRESPONDING LT_DATA2 TO GT_DATA .
*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  END

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
  INIT_FIELDCAT 'HTBH'           '合同编号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'          '采购组'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKNAM'          '采购组名'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'          '项目编号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'          '项目名称'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MWSKZ'          '是否含税'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '本位币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'        '凭证货币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR'          '采购订单含税金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR_1'        '采购订单含税金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MWSKZ'          '是否含税'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM'          '子付款条件'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT1'          '子付款条件描述'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RATPZ'          '应付比例%'   '' '' '' '' '' '' ''.

  INIT_FIELDCAT 'YFJE1'          '订单应付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE'          '已交货金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE_1'        '已交货金额（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE'           '订单已付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFBL'           '订单已付比例%'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE'          '订单已申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
*&--------------------------13074  insert -----------------------------
  INIT_FIELDCAT 'ZJDFKJH'        '节点付款计划'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJDYFJE'        '节点已付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJDYSQJE'        '节点已申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZKSQJE'          '订单可申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZCEJE'          '订单超额金额'   '' '' '' '' '' '' ''.
*&----------------------------------------------------------------------
  INIT_FIELDCAT 'WFJE'           '订单未付金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZFKXZ'          '付款性质'   '' '' '' '' ''  'ZFI017' 'ZFKXZ'.
  INIT_FIELDCAT 'ZSQFKJE'        '申请付款金额'   '' '' '' '' '' 'ZFI017' 'ZSQFKJE'.
  INIT_FIELDCAT 'WAERS_2'        '货币'   '6' '' '' '' '' 'EKKO' 'WAERS'.
  INIT_FIELDCAT 'ZSQRQ'          '申请日期'   '' '' '' '' '' 'ZFI017' 'ZSQRQ'.
  INIT_FIELDCAT 'ZSQFKRQ'        '申请付款日期'   '' '' '' '' '' 'ZFI017' 'ZSQFKRQ'.
  INIT_FIELDCAT 'ZZY'            '摘要'   '15' '' '' '' '' 'ZFI017' 'ZZY'.
  INIT_FIELDCAT 'ZCLJD'          '处理进度'   '6' '' '' '' '' 'ZFI017' 'ZCLJD' .
  INIT_FIELDCAT 'BELNR_F'        '付款会计凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR_F'        '付款会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '发票文本'   '15' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  BEGIN
  INIT_FIELDCAT 'YJFKRQ'          '预计付款日期'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJFKJE'          '预计付款金额'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  END
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
*     I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
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
  DATA L_CHECK TYPE C.


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

*审批通过
    WHEN '&OK'.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQD  = GS_DATA-ZSQD.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.

*提示对话框
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            TEXT_QUESTION  = '是否执行审批通过'
          IMPORTING
            ANSWER         = G_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF G_ANSWER <> '1'.
          EXIT.
        ENDIF.

        LOOP AT GT_DATA INTO GS_DATA
         WHERE ZBOX = 'X'.

*检查处理状态
          IF GS_DATA-ZCLJD <> '1'.
            MESSAGE S006(ZFICO01) DISPLAY LIKE 'E'.
            REFRESH GT_ZFI017_1.
            EXIT.
          ENDIF.

*检查锁对象
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              ZSQD           = GS_DATA-ZSQD
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC <> 0.
            CLEAR L_STRING.
            CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
            MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          GS_DATA-ZCLJD = '2'. "同意，未付款
          GS_DATA-ZNAME1 = SY-UNAME. "审批人
          GS_DATA-STATU = ICON_GREEN_LIGHT.
          MODIFY GT_DATA FROM GS_DATA.
          MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
          APPEND GS_ZFI017_1 TO GT_ZFI017_1.
          CLEAR GS_DATA.
        ENDLOOP.

        MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
        REFRESH GT_ZFI017_1.
        CLEAR GS_DATA.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

*付款过账
    WHEN '&POST'.
*增加付款过账功能  ADD BY HANDWY 2015-3-23
      DATA LT_DATA TYPE TABLE OF TY_DATA.
      DATA LS_DATA TYPE TY_DATA.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQD  = GS_DATA-ZSQD.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.

*检查处理状态
        CLEAR L_CHECK.
        REFRESH LT_DATA.

        LOOP AT GT_DATA INTO GS_DATA
         WHERE ZBOX = 'X'.
          IF GS_DATA-ZCLJD <> '2'.
            MESSAGE S007(ZFICO01) DISPLAY LIKE 'E'.
            L_CHECK = 'X'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              ZSQD           = GS_DATA-ZSQD
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC <> 0.
            L_CHECK = 'X'.
            CLEAR L_STRING.
            CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
            MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*记录选中的行
          MOVE-CORRESPONDING GS_DATA TO LS_DATA.
          APPEND LS_DATA TO LT_DATA.
          CLEAR LS_DATA.
        ENDLOOP.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        L_CHECK = 'X'.
      ENDIF.

*删除基于发票提的付款申请的行项目
      DELETE LT_DATA WHERE BELNR IS NOT INITIAL
                     AND   EBELN IS NOT INITIAL.

*当检查无误后，(初始化数据)进入过账界面
      IF L_CHECK <> 'X'.
        PERFORM INIT_ALV_9000 TABLES LT_DATA.
        CALL SCREEN 9000.
      ENDIF.

*冲销
    WHEN '&REV'.
      CLEAR G_ANSWER.
      BREAK HANDWY.
*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQD  = GS_DATA-ZSQD.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION  = '确认要执行冲销操作'
        IMPORTING
          ANSWER         = G_ANSWER
*   TABLES
*         PARAMETER      =
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
      IF G_ANSWER <> '1'.
        EXIT.
      ENDIF.

      PERFORM FRM_ACC_REVERSAL.

*审批拒绝
    WHEN '&REJECT'.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQD  = GS_DATA-ZSQD.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.

*弹出框提示
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            TEXT_QUESTION  = '是否执行审批拒绝'
          IMPORTING
            ANSWER         = G_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF G_ANSWER <> '1'.
          EXIT.
        ENDIF.

        LOOP AT GT_DATA INTO GS_DATA
        WHERE ZBOX = 'X'.

*检查处理状态
          IF GS_DATA-ZCLJD <> '1'.
            MESSAGE S006(ZFICO01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              ZSQD           = GS_DATA-ZSQD
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC <> 0.
            CLEAR L_STRING.
            CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
            MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          GS_DATA-ZCLJD  = '4'. "同意，未付款
          GS_DATA-STATU  = ICON_RED_LIGHT.
          GS_DATA-ZNAME1 = SY-UNAME. "审批人
          MODIFY GT_DATA FROM GS_DATA.
          MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
          APPEND GS_ZFI017_1 TO GT_ZFI017_1.
          CLEAR GS_DATA.
        ENDLOOP.

        MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
        REFRESH GT_ZFI017_1.
        CLEAR GS_DATA.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.


    WHEN '&BACK'.
      LOOP AT GT_DATA INTO GS_DATA
        WHERE ZBOX = 'X'.
        LOOP AT GT_DATA INTO GS_DATA
         WHERE BELNR = GS_DATA-BELNR
         AND   GJAHR = GS_DATA-GJAHR
         AND   BUKRS = GS_DATA-BUKRS
         AND   ZSQD  = GS_DATA-ZSQD.

          GS_DATA-ZBOX = 'X'.
          MODIFY GT_DATA FROM GS_DATA.
        ENDLOOP.
      ENDLOOP.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.

*弹出框提示
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            TEXT_QUESTION  = '是否执反行审批操作'
          IMPORTING
            ANSWER         = G_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF G_ANSWER <> '1'.
          EXIT.
        ENDIF.

        LOOP AT GT_DATA INTO GS_DATA
        WHERE ZBOX = 'X'.

*检查处理状态
          IF GS_DATA-ZCLJD = '1'.
            MESSAGE S013(ZFICO01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          IF GS_DATA-ZCLJD = '3'.
            MESSAGE S014(ZFICO01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              ZSQD           = GS_DATA-ZSQD
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC <> 0.
            CLEAR L_STRING.
            CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
            MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          GS_DATA-ZCLJD  = '1'. "同意，未付款
          GS_DATA-STATU  = ICON_YELLOW_LIGHT.
          GS_DATA-ZNAME1 = ''. "审批人
          MODIFY GT_DATA FROM GS_DATA.
          MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
          APPEND GS_ZFI017_1 TO GT_ZFI017_1.
          CLEAR GS_DATA.
        ENDLOOP.

        MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
        REFRESH GT_ZFI017_1.
        CLEAR GS_DATA.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&F03' OR '&F15' OR  '&F12'.
      PERFORM FRM_UNLOCK.
  ENDCASE.

  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.
"ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_ALV_9000 TABLES LT_DATA.
  DATA LS_DATA TYPE  TY_DATA.

*初始化抬头

  READ TABLE LT_DATA INTO LS_DATA INDEX 1.
  GS_HEAD-BLDAT = SY-DATUM.       "凭证日期
  GS_HEAD-BUDAT = SY-DATUM.       "过账日期
  GS_HEAD-BUKRS = LS_DATA-BUKRS.  "公司代码
  GS_HEAD-WAERS = LS_DATA-WAERS_2."货币
  GS_HEAD-BKTXT = LS_DATA-HTBH.   "合同编号


  GS_HEAD-BLART = 'SA'.

*初始化行项目
  REFRESH GT_ITEM.
  CLEAR G_WRBTR.
  LOOP AT LT_DATA INTO LS_DATA.
    GS_ITEM-BUZEI = SY-TABIX.
    GS_ITEM-BSCHL = '21'.

*&--代码注释 BY HANDYBY 23.06.2017 11:00:02  BEGIN
* *付款性质决定科目
*    IF ls_data-zfkxz = '1'.     "预付
*      gs_item-hkont = '2202010101'.
*    ELSEIF ls_data-zfkxz = '2'. "应付
*      gs_item-hkont = '1123010101'.
*    ENDIF.
*&--代码注释 BY HANDYBY 23.06.2017 11:00:02  END
*&--代码添加 BY HANDYBY 23.06.2017 10:59:35  BEGIN
*付款性质决定科目
    "所有付款性质1、2 的总账科目默认值都是从供应商设置的统御科目
    IF LS_DATA-ZFKXZ = '1' OR  LS_DATA-ZFKXZ = '2'.     "应付 "预付

      SELECT SINGLE AKONT INTO GS_ITEM-HKONT
           FROM LFB1
           WHERE LIFNR = LS_DATA-LIFNR
            AND  BUKRS = LS_DATA-BUKRS .

    ENDIF.
*&--代码添加 BY HANDYBY 23.06.2017 10:59:35  END

*获取科目描述
    READ TABLE GT_SKAT INTO GS_SKAT
    WITH KEY SAKNR = GS_ITEM-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_ITEM-HKTXT = GS_SKAT-TXT20.
    ENDIF.

*供应商
    GS_ITEM-LIFNR   = LS_DATA-LIFN2.

*金额
    GS_ITEM-WRBTR   = LS_DATA-ZSQFKJE.

*货币
    GS_ITEM-PSWSL   = LS_DATA-WAERS_2.

*统计借方的总金额
    G_WRBTR  = G_WRBTR + GS_ITEM-WRBTR.

*付款单号
    GS_ITEM-ZUONR_1 = LS_DATA-ZSQD.

**付款申号
*    GS_HEAD-BKTXT   = LS_DATA-ZSQD.

*行项目摘要
    " GS_ITEM-SGTXT   = LS_DATA-ZZY.
    GS_ITEM-SGTXT = LS_DATA-HTBH.

*WBS元素
    GS_ITEM-PSPID   = LS_DATA-POSID.

*设置不可以编辑状态
    PERFORM FRM_CELL_STYLE USING 'BSCHL'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.
*    PERFORM FRM_CELL_STYLE USING 'HKONT'
*                           ''
*                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'LIFNR'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'KUNNR'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'WRBTR'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'RSTGR'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'ZUONR_1'
                           ''
                          CHANGING GS_ITEM-CELLSTYLE.
*    PERFORM FRM_CELL_STYLE USING 'SGTXT'
*                           ''
*                          CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'PSPID'
                           ''
                           CHANGING GS_ITEM-CELLSTYLE.

    APPEND GS_ITEM TO GT_ITEM.
    CLEAR GS_ITEM.
  ENDLOOP.

ENDFORM.
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
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STATU_9000'.
  SET TITLEBAR '采购付款记账'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPALY_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_DISPALY_9000 OUTPUT.
  IF GR_CONTAINER IS INITIAL.
    PERFORM FRM_CREATE_CONTAINER.
    PERFORM FRM_ALV_DISPLAY.
  ELSE.
    PERFORM FRM_REFRESH_ALV.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  DATA: L_SUBRC TYPE SY-SUBRC.

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'BACK'.
      PERFORM FRM_CHECK_CHANGED CHANGING L_SUBRC.
      IF L_SUBRC = 0.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXIT'.
      PERFORM FRM_CHECK_CHANGED CHANGING L_SUBRC.
      IF L_SUBRC = 0.
        LEAVE PROGRAM.
      ENDIF.
    WHEN 'POST'.
      PERFORM FRM_POST_ACCDOC.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER .
  CREATE OBJECT GR_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'CONTAINER'
*     lifetime       = cl_gui_custom_container=>lifetime_dynpro
    .

  CREATE OBJECT GR_ALVGRID
    EXPORTING
      I_APPL_EVENTS = 'X'
      I_PARENT      = GR_CONTAINER.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY .
  PERFORM INIT_OO_LAYOUT.              "设置输出格式
  PERFORM INIT_SORT_1.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'. "设置变式控制
  PERFORM FRM_INIT_LVC_1.             " 初始化内表结构/ALV显示结构
  PERFORM EXCLUDE_TB_FUNCTIONS TABLES GT_OO_EXCLUDE.
  PERFORM FRM_OO_BUILD_EVENT.
  PERFORM FRM_OO_OUTPUT TABLES GT_LVC              "输出
                               GT_SORT
                               GT_OO_EXCLUDE
                               GT_ITEM
                        USING GW_LAYOUT
                              GW_VARIANT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_OO_LAYOUT .
  GW_LAYOUT-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  GW_LAYOUT-SEL_MODE = 'A'.
  GW_LAYOUT-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  GW_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT_1 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC_1 .

  REFRESH GT_LVC.
  INIT_FIELDCAT 'BUZEI'        '项目号'   8 '' '' '' '' 'BSEG' 'BUZEI'.
  INIT_FIELDCAT 'BSCHL'        '记账码'   8 '' '' 'X' '' 'BSEG' 'BSCHL'.
  INIT_FIELDCAT 'HKONT'        '科目'   '' '' '' 'X' ''  '' ''.
  INIT_FIELDCAT 'HKTXT'        '科目描述'   '' '' '' '' '' 'SKAT' 'TXT20'.
  INIT_FIELDCAT 'LIFNR'        '供应商'   '' '' '' 'X' '' 'BSEG' 'LIFNR'.
  INIT_FIELDCAT 'KUNNR'        '客户'   '' '' '' 'X' '' 'BSEG' 'KUNNR'.
  INIT_FIELDCAT 'WRBTR'        '金额'   '' '' '' 'X' '' 'BSEG' 'WRBTR'.
  INIT_FIELDCAT 'PSWSL'        '货币'   '' '' '' '' '' 'BSEG' 'PSWSL'.
  INIT_FIELDCAT 'RSTGR'        '原因代码'   8 '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'ZUONR'        '汇票号'   '' '' '' '' '' 'BSEG' 'ZUONR'.
  INIT_FIELDCAT 'ZUONR_1'      '付款申请单号'   15 '' '' 'X' '' 'BSEG' 'ZUONR'.
  INIT_FIELDCAT 'SGTXT'        '行项目摘要'   '' '' '' 'X' '' 'BSEG' 'SGTXT'.
  INIT_FIELDCAT 'PSPID'        '项目WBS编号'   '' '' '' 'X' '' 'PROJ' 'PSPID'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       弃掉标准按钮（增加、插入）
*----------------------------------------------------------------------*
*      -->P_GT_EXCLUDE  text
*      -->P_ELSE  text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS TABLES PT_EXCLUDE TYPE UI_FUNCTIONS.

  DATA: LS_EXCLUDE TYPE UI_FUNC.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OO_BUILD_EVENT .

  DATA LR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.
  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.

  CREATE OBJECT LR_EVENT_HANDLER.
  SET HANDLER : LR_EVENT_HANDLER->HANDLE_TOOLBAR               FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_BEFORE_USER_COMMAND   FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_USER_COMMAND          FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_AFTER_USER_COMMAND    FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_ONF4                  FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED          FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED_FINISHED FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK          FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_BUTTON_CLICK          FOR GR_ALVGRID.

*F4对应的栏位
  LS_F4-FIELDNAME = 'HKONT'.
  LS_F4-REGISTER = 'X'.
*  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

*F4对应的栏位
  LS_F4-FIELDNAME = 'RSTGR'.
  LS_F4-REGISTER = 'X'.
*  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

  CALL METHOD GR_ALVGRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = LT_F4[].

  CALL METHOD GR_ALVGRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
FORM FRM_OO_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_EXCLUDE TYPE UI_FUNCTIONS
                       PT_DATA
                USING PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT.

  CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
      IS_VARIANT                    = PW_VARIANT
      I_SAVE                        = 'A'
*     i_default                     = 'X'
      IS_LAYOUT                     = PW_LAYOUT
*     is_print                      =
*     it_special_groups             =
      IT_TOOLBAR_EXCLUDING          = PT_EXCLUDE[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      IT_OUTTAB                     = PT_DATA[]
      IT_FIELDCATALOG               = PT_LVC[]
      IT_SORT                       = PT_SORT[]
*     it_filter                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.                    " FRM_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REFRESH_ALV .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
*     i_soft_refresh =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_CHANGED  CHANGING P_SUBRC TYPE SY-SUBRC.
  P_SUBRC = 0.
  IF G_CHANGED = ABAP_TRUE.
    DATA L_ANS.
    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
*       DEFAULTOPTION       = 'Y'
        TEXTLINE1 = TEXT-M01
        TEXTLINE2 = TEXT-M02
        TITEL     = TEXT-M03
*       START_COLUMN        = 25
*       START_ROW = 6
      IMPORTING
        ANSWER    = L_ANS.
    IF L_ANS = 'N'.
      P_SUBRC = 2.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_ACCDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_POST_ACCDOC .
  DATA L_ANS.
  DATA L_SUBRC TYPE SY-SUBRC.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      TEXT_QUESTION         = '确定要保存并过账吗？'
      TEXT_BUTTON_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      ANSWER                = L_ANS
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK L_ANS EQ '1'.

  PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

  CHECK L_SUBRC EQ 0.

*付款记账数据准备
  PERFORM FRM_BAPI_DATA_PREP .

*调用记账BAPI
  PERFORM FRM_CALL_BAPI CHANGING GS_HEAD-BELNR GS_HEAD-GJAHR.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM HANDLE_TOOLBAR  USING    P_E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                                     P_E_INTERACTIVE.
  DATA: LW_TOOLBAR LIKE LINE OF P_E_OBJECT->MT_TOOLBAR.

  READ TABLE P_E_OBJECT->MT_TOOLBAR INTO LW_TOOLBAR
  WITH KEY FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  IF SY-SUBRC = 0.
*加入新增行按钮
    CLEAR LW_TOOLBAR.
    LW_TOOLBAR-FUNCTION    = 'APPEND_ROW '.
*    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    LW_TOOLBAR-QUICKINFO   = '新增行'.
    LW_TOOLBAR-ICON        = ICON_CREATE.
    LW_TOOLBAR-DISABLED    = SPACE.
    INSERT LW_TOOLBAR INTO P_E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING    P_UCOMM.
  SAVE_OK = P_UCOMM.
  CLEAR P_UCOMM.

  CASE SAVE_OK.
    WHEN 'APPEND_ROW'.
      PERFORM FRM_APPEND_ROW.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED_FINISHED  USING    P_E_MODIFIED
                                            P_ET_GOOD_CELLS TYPE  LVC_T_MODI.
  DATA: LW_CELL TYPE LVC_S_MODI.

  " 输入记账码时，更改相应字段的可输入属性
  READ TABLE P_ET_GOOD_CELLS INTO LW_CELL
  WITH KEY FIELDNAME = 'BSCHL'.
  IF SY-SUBRC = 0.
    READ TABLE GT_ITEM ASSIGNING <FS_ITEM> INDEX LW_CELL-ROW_ID.
    IF SY-SUBRC = 0.
*记账为50 ,现金,银行科目,
      IF LW_CELL-VALUE = '50'.
        PERFORM FRM_CELL_STYLE USING 'LIFNR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'KUNNR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR'
                               ''
                            CHANGING GS_ITEM-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR_1'
                               ''
                             CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'RSTGR'
                              'X'
                            CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'PSPID'
                            'X'
                          CHANGING <FS_ITEM>-CELLSTYLE.

*记账码31,应付票据
      ELSEIF LW_CELL-VALUE = '31'.
        PERFORM FRM_CELL_STYLE USING 'KUNNR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'RSTGR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR_1'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'LIFNR'
                               'X'
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR'
                               'X'
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'PSPID'
                                  'X'
                                CHANGING <FS_ITEM>-CELLSTYLE.
*记账码为11,应收票据背书
      ELSEIF LW_CELL-VALUE = '11'.
        PERFORM FRM_CELL_STYLE USING 'LIFNR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'RSTGR'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'PSWSL'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR_1'
                               ''
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'KUNNR'
                               'X'
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'ZUONR'
                               'X'
                               CHANGING <FS_ITEM>-CELLSTYLE.
        PERFORM FRM_CELL_STYLE USING 'PSPID'
                          'X'
                        CHANGING <FS_ITEM>-CELLSTYLE.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM FRM_REFRESH_ALV.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_APPEND_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_APPEND_ROW .
  DATA: L_INDEX TYPE I.
  DATA: L_ROW_ID TYPE LVC_S_ROW,
        L_COL_ID TYPE LVC_S_COL,
        L_ROW_NO TYPE LVC_S_ROID.

  DATA: L_BUZEI TYPE BSEG-BUZEI.
  L_BUZEI = 1.
  DO.
    READ TABLE GT_ITEM TRANSPORTING NO FIELDS WITH KEY BUZEI = L_BUZEI.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    L_BUZEI = L_BUZEI + 1.
  ENDDO.

  CLEAR GS_ITEM.
  GS_ITEM-BUZEI = L_BUZEI.

*贷方金额
  GS_ITEM-WRBTR = G_WRBTR.

*贷方货币
  GS_ITEM-PSWSL = GS_HEAD-WAERS.

  APPEND GS_ITEM TO GT_ITEM.
  CLEAR G_WRBTR.
  CLEAR GS_ITEM.

  PERFORM FRM_REFRESH_ALV.

  " 设置光标在过账码
  L_INDEX = LINES( GT_ITEM ).

  L_ROW_ID = L_INDEX.
  L_COL_ID = 'BSCHL'.
  L_ROW_NO-ROW_ID = L_INDEX.

  CALL METHOD GR_ALVGRID->SET_CURRENT_CELL_VIA_ID
    EXPORTING
      IS_ROW_ID    = L_ROW_ID
      IS_COLUMN_ID = L_COL_ID
      IS_ROW_NO    = L_ROW_NO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_cell_style
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_CELLSTYLE  text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM FRM_CELL_STYLE  USING    P_FIELDNAME
                              P_EDITABLE
                     CHANGING PT_CELLSTYLE TYPE LVC_T_STYL.
  DATA: LW_CELLSTYLE TYPE LVC_S_STYL.

  READ TABLE PT_CELLSTYLE INTO LW_CELLSTYLE WITH KEY FIELDNAME = P_FIELDNAME.
  IF SY-SUBRC = 0.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    MODIFY TABLE PT_CELLSTYLE FROM LW_CELLSTYLE.
  ELSE.
    LW_CELLSTYLE-FIELDNAME = P_FIELDNAME.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    INSERT LW_CELLSTYLE INTO TABLE PT_CELLSTYLE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'MOD'.
*      IF G_EDIT_MOD = GC_EDITABLE.
*        SCREEN-INPUT = 1.
*      ELSE.
*        SCREEN-INPUT = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

*  DATA L_ALV_EDIT TYPE INT4.
*  L_ALV_EDIT = GR_ALVGRID->IS_READY_FOR_INPUT( ).
*  IF ( L_ALV_EDIT = 1 AND G_EDIT_MOD NE GC_EDITABLE )
*    OR ( L_ALV_EDIT = 0 AND G_EDIT_MOD NE GC_READONLY ).
*    L_ALV_EDIT = 1 - L_ALV_EDIT.
*    PERFORM FRM_CHANGE_EDIT_MODE USING L_ALV_EDIT.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ALV_EDIT  text
*----------------------------------------------------------------------*
FORM FRM_CHANGE_EDIT_MODE  USING    P_ALV_EDIT TYPE INT4.
  CALL METHOD GR_ALVGRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_ALV_EDIT.

  "切换模式后刷新ALV
  PERFORM FRM_REFRESH_ALV.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.
  " 凭证日期
  IF GS_HEAD-BLDAT IS INITIAL.
    MESSAGE TEXT-M04 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF GS_HEAD-BUDAT IS INITIAL.
    MESSAGE TEXT-M05 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF GS_HEAD-BUKRS IS INITIAL.
    MESSAGE TEXT-M06 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 货币
  IF GS_HEAD-WAERS IS INITIAL.
    MESSAGE TEXT-M07 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 行项目
  IF GT_ITEM IS INITIAL.
    MESSAGE TEXT-M09 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  IF GS_HEAD-BLART IS INITIAL.
    MESSAGE '请输入凭证类型！' TYPE 'W' DISPLAY LIKE 'E'. "凭证类型
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  LOOP AT GT_ITEM INTO GS_ITEM.
    " 记账码
    IF  GS_ITEM-BSCHL NE '21' AND GS_ITEM-BSCHL NE '50'
    AND GS_ITEM-BSCHL NE '31'  AND GS_ITEM-BSCHL NE '11'.
      MESSAGE TEXT-M10 TYPE 'W' DISPLAY LIKE 'E'.
      P_SUBRC = 8.
      EXIT.
    ENDIF.

    " 科目
    IF GS_ITEM-HKONT IS INITIAL.
      MESSAGE TEXT-M12 TYPE 'W' DISPLAY LIKE 'E'.
      P_SUBRC = 8.
      EXIT.
    ENDIF.

    "记账码为50
    IF GS_ITEM-BSCHL EQ '50'
      AND GS_ITEM-RSTGR IS INITIAL.
      MESSAGE TEXT-M11 TYPE 'W' DISPLAY LIKE 'E'.
    ENDIF.

    "记账码为31
    IF GS_ITEM-BSCHL EQ '31'
      AND GS_ITEM-LIFNR IS INITIAL.
      MESSAGE TEXT-M14 TYPE 'W' DISPLAY LIKE 'E'.
    ENDIF.

    "记账码为11
    IF GS_ITEM-BSCHL EQ '11'.
      IF GS_ITEM-KUNNR IS INITIAL.
        MESSAGE TEXT-M15 TYPE 'W' DISPLAY LIKE 'E'.
      ENDIF.
      IF GS_ITEM-ZUONR IS INITIAL.
        MESSAGE TEXT-M16 TYPE 'W' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_PREP .
*清空BAPI变量
  PERFORM FRM_BAPI_DATA_CLEAR.

  DATA: L_KATYP TYPE CSKB-KATYP.

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  WA_DOCUMENTHEADER-DOC_DATE     =  GS_HEAD-BLDAT.
*过账日期
  WA_DOCUMENTHEADER-PSTNG_DATE   =  GS_HEAD-BUDAT.
*凭证类型
  WA_DOCUMENTHEADER-DOC_TYPE     =  GS_HEAD-BLART.
*公司代码
  WA_DOCUMENTHEADER-COMP_CODE    =  GS_HEAD-BUKRS.
*凭证抬头文本
  WA_DOCUMENTHEADER-HEADER_TXT   =  GS_HEAD-BKTXT.
*创建人员
  WA_DOCUMENTHEADER-USERNAME     =  SY-UNAME.

**********************************************************************
* 凭证行
**********************************************************************
  LOOP AT GT_ITEM INTO GS_ITEM.
*当记账码是21
    IF GS_ITEM-BSCHL = '21' .
      CLEAR WA_ACCOUNTPAYABLE.
*       行项目号
      WA_ACCOUNTPAYABLE-ITEMNO_ACC  = GS_ITEM-BUZEI.
*       项目文本
      WA_ACCOUNTPAYABLE-ITEM_TEXT   = GS_ITEM-SGTXT.
*       供应商编码
      WA_ACCOUNTPAYABLE-VENDOR_NO   = GS_ITEM-LIFNR.
*       根据付款性质确定总账科目
      WA_ACCOUNTPAYABLE-GL_ACCOUNT  = GS_ITEM-HKONT.
*       分配号 = 付款申请单号 （WBS元素）
      WA_ACCOUNTPAYABLE-ALLOC_NMBR  = GS_ITEM-PSPID.

      APPEND WA_ACCOUNTPAYABLE TO IT_ACCOUNTPAYABLE.
      CLEAR WA_ACCOUNTPAYABLE.
    ENDIF.

    CLEAR WA_CURRENCYAMOUNT.
    WA_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
    WA_CURRENCYAMOUNT-CURRENCY   = GS_ITEM-PSWSL.

*当记账码为50，*当记账码为31，*当记账码为11
    IF GS_ITEM-BSCHL = '50'
      OR GS_ITEM-BSCHL = '31'
      OR GS_ITEM-BSCHL = '11'.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR * -1.
    ENDIF.

*当记账码为21
    IF GS_ITEM-BSCHL = '21'.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
    ENDIF.

    APPEND WA_CURRENCYAMOUNT TO IT_CURRENCYAMOUNT.
    CLEAR WA_CURRENCYAMOUNT.
    CLEAR WA_ACCOUNTGL.

*应收票据
    IF GS_ITEM-BSCHL = '11' OR  GS_ITEM-BSCHL = '31'.
      WA_ACCOUNTGL-CUSTOMER   = GS_ITEM-KUNNR.
*      WA_ACCOUNTGL-ALLOC_NMBR = GS_ITEM-ZUONR. "汇票号
      WA_ACCOUNTGL-ALLOC_NMBR = GS_ITEM-PSPID. "汇票号
    ENDIF.

*记账码50，写进WBS元素字段
    IF GS_ITEM-BSCHL = '50'.
      WA_ACCOUNTGL-WBS_ELEMENT  = GS_ITEM-PSPID.
    ENDIF.

*应付票据
    IF GS_ITEM-BSCHL = '31'.
      WA_ACCOUNTGL-VENDOR_NO  = GS_ITEM-LIFNR.
    ENDIF.

*总帐科目项
    IF GS_ITEM-BSCHL = '50'
      OR GS_ITEM-BSCHL = '31'
      OR GS_ITEM-BSCHL = '11'.
      WA_ACCOUNTGL-ITEMNO_ACC       = GS_ITEM-BUZEI.
      WA_ACCOUNTGL-GL_ACCOUNT       = GS_ITEM-HKONT.    "记账科目
      WA_ACCOUNTGL-ITEM_TEXT        = GS_ITEM-SGTXT.    "摘要
      APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL.
      CLEAR WA_ACCOUNTGL.
    ENDIF.

* 记账码 & 付款原因
    CLEAR WA_EXTENSION2.
    CLEAR WA_ZACCDOCUEXT.
    WA_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
    WA_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL."记账码
    WA_ZACCDOCUEXT-RSTGR = GS_ITEM-RSTGR."付款原因

    WA_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
    WA_EXTENSION2-VALUEPART1 = WA_ZACCDOCUEXT.
    APPEND WA_EXTENSION2 TO IT_EXTENSION2.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_HEAD_BELNR  text
*      <--P_GS_HEAD_GJAHR  text
*----------------------------------------------------------------------*
FORM FRM_CALL_BAPI  CHANGING P_BELNR TYPE BELNR_D
                             P_GJAHR TYPE GJAHR.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER = WA_DOCUMENTHEADER
*     CUSTOMERCPD    =
*     CONTRACTHEADER =
    IMPORTING
      OBJ_TYPE       = WA_OBJ-OBJ_TYPE
      OBJ_KEY        = WA_OBJ-OBJ_KEY
      OBJ_SYS        = WA_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL      = IT_ACCOUNTGL
*     ACCOUNTRECEIVABLE = IT_ACCOUNTRECEIVABLE
      ACCOUNTPAYABLE = IT_ACCOUNTPAYABLE
*     ACCOUNTTAX     =
      CURRENCYAMOUNT = IT_CURRENCYAMOUNT
      CRITERIA       = IT_CRITERIA
      VALUEFIELD     = IT_VALUEFIELD
*     EXTENSION1     =
      RETURN         = IT_RETURN
*     PAYMENTCARD    =
*     CONTRACTITEM   =
      EXTENSION2     = IT_EXTENSION2
*     REALESTATE     =
*     ACCOUNTWT      =
    .

  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

* 回写生成的会计凭证号与会计凭证年度
    GS_HEAD-BELNR = WA_OBJ-OBJ_KEY(10).
    GS_HEAD-GJAHR = WA_OBJ-OBJ_KEY+14(4).

*根据过账的借方行项目，更新会计凭证和状态
    LOOP AT GT_ITEM INTO GS_ITEM.
      LOOP AT  GT_DATA INTO GS_DATA
      WHERE ZSQD = GS_ITEM-ZUONR_1.
        GS_DATA-BELNR_F = GS_HEAD-BELNR.
        GS_DATA-GJAHR_F = GS_HEAD-GJAHR.
        GS_DATA-ZCLJD = '3'.
        GS_DATA-STATU = ICON_OKAY.
        GS_DATA-YFJE = GS_DATA-YFJE + GS_DATA-ZSQFKJE ."更新已付款金额数据 ：原已保存已付款数据 + 已记账成功的申请金额数据
        GS_DATA-WFJE = GS_DATA-WFJE - GS_DATA-YFJE.
        MODIFY  GT_DATA FROM GS_DATA.
        CLEAR GS_DATA.
      ENDLOOP.
    ENDLOOP.

*更新自建表状态
    REFRESH GT_ZFI017.
    MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
    MODIFY ZFI017 FROM TABLE GT_ZFI017.

**更新会计凭证和状态
*    LOOP AT GT_DATA INTO GS_DATA
*     WHERE ZBOX = 'X'.
*      GS_DATA-BELNR = GS_HEAD-BELNR.
*      GS_DATA-GJAHR = GS_HEAD-GJAHR.
*      GS_DATA-ZCLJD = '3'.
*      GS_DATA-STATU = ICON_OKAY.
*      MODIFY GT_DATA FROM GS_DATA.
*      CLEAR GS_DATA.
*    ENDLOOP.

    MESSAGE S312(F5) WITH  GS_HEAD-BELNR S_BUKRS-LOW.
    CLEAR GS_HEAD.
    CLEAR GS_ITEM.
    REFRESH GT_ITEM.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_DISPLAY .
  PERFORM FRM_MESSAGE_INITIAL.

  LOOP AT IT_RETURN INTO WA_RETURN.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.
*
*    write: / g_msg.

    "append all error warning messages to below function module
    PERFORM STORE_MESSAGES USING WA_RETURN-ID
                                 WA_RETURN-TYPE
                                 WA_RETURN-MESSAGE_V1
                                 WA_RETURN-MESSAGE_V2
                                 WA_RETURN-MESSAGE_V3
                                 WA_RETURN-MESSAGE_V4
                                 WA_RETURN-NUMBER.
  ENDLOOP.

  PERFORM FRM_MESSAGE_SHOW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_INITIAL .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      LOG_NOT_ACTIVE       = 1
      WRONG_IDENTIFICATION = 2
      OTHERS               = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETURN_ID  text
*      -->P_WA_RETURN_TYPE  text
*      -->P_WA_RETURN_MESSAGE_V1  text
*      -->P_WA_RETURN_MESSAGE_V2  text
*      -->P_WA_RETURN_MESSAGE_V3  text
*      -->P_WA_RETURN_MESSAGE_V4  text
*      -->P_WA_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM STORE_MESSAGES USING P_MSGID
                          P_MSGTY
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                          P_TXTNR.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      ARBGB                  = P_MSGID
      MSGTY                  = P_MSGTY
      MSGV1                  = P_MSGV1
      MSGV2                  = P_MSGV2
      MSGV3                  = P_MSGV3
      MSGV4                  = P_MSGV4
      TXTNR                  = P_TXTNR
    EXCEPTIONS
      MESSAGE_TYPE_NOT_VALID = 1
      NOT_ACTIVE             = 2
      OTHERS                 = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_SHOW .
  "at lsat call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA L_EXIT_COMMAND TYPE BAL_S_EXCM.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO         = SPACE
    IMPORTING
      E_EXIT_COMMAND     = L_EXIT_COMMAND
    EXCEPTIONS
      INCONSISTENT_RANGE = 1
      NO_MESSAGES        = 2
      OTHERS             = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_CLEAR .
  REFRESH: IT_ACCOUNTGL, IT_ACCOUNTPAYABLE, IT_CURRENCYAMOUNT, IT_CRITERIA, IT_VALUEFIELD, IT_EXTENSION2, IT_RETURN.
  CLEAR: WA_DOCUMENTHEADER, WA_OBJ.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING     P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                    P_E_ONF4
                                    P_E_ONF4_BEFORE
                                    P_E_ONF4_AFTER
                                    P_E_UCOMM TYPE SY-UCOMM.

  DATA:LS_MOD_CELL TYPE LVC_S_MODI ,  "应用的修改的单元格
       LV_VALUE    TYPE LVC_VALUE ,   "单元格内容
       LS_STABLE   TYPE LVC_S_STBL.   "刷新稳定性

  DATA:LS_CELLTAB TYPE LVC_S_STYL,
       LT_CELLTAB TYPE LVC_T_STYL.

  SORT P_ER_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID.

*ITEM
  LOOP AT P_ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
    AT NEW ROW_ID.
      READ TABLE GT_ITEM INTO GS_ITEM INDEX LS_MOD_CELL-ROW_ID.
    ENDAT.

    CASE LS_MOD_CELL-FIELDNAME.
      WHEN 'HKONT'.  "科目
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'HKONT'
          IMPORTING
            E_VALUE     = GS_ITEM-HKONT.

*获取科目描述
        READ TABLE GT_SKAT INTO GS_SKAT
        WITH KEY SAKNR = GS_ITEM-HKONT BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_ITEM-HKTXT = GS_SKAT-TXT20.
        ENDIF.

      WHEN 'RSTGR'.  "原因代码
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'RSTGR'
          IMPORTING
            E_VALUE     = GS_ITEM-RSTGR.

      WHEN 'WRBTR'.  "金额
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'WRBTR'
          IMPORTING
            E_VALUE     = GS_ITEM-WRBTR.

      WHEN 'SGTXT'.  "摘要
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'RSTGR'
          IMPORTING
            E_VALUE     = GS_ITEM-SGTXT.

      WHEN 'KUNNR'.  "客户
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'KUNNR'
          IMPORTING
            E_VALUE     = GS_ITEM-KUNNR.

      WHEN 'LIFNR'.  "供应商
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'LIFNR'
          IMPORTING
            E_VALUE     = GS_ITEM-LIFNR.

      WHEN 'ZUONR'.  "汇票号
        "获取指定单元格改动后内容
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELL-ROW_ID
            I_FIELDNAME = 'ZUONR'
          IMPORTING
            E_VALUE     = GS_ITEM-ZUONR.
    ENDCASE.

*刷新数据到ALV
    MODIFY GT_ITEM FROM GS_ITEM INDEX LS_MOD_CELL-ROW_ID.
    CLEAR GS_ITEM.
  ENDLOOP.

*输入检查：1. 记账码限制
  DATA: LT_MOD_DATA TYPE LVC_T_MODI,
        WA_MOD_DATA TYPE LVC_S_MODI.

  DATA: L_TYPE TYPE C,
        L_MSG  TYPE C LENGTH 100.

  LT_MOD_DATA = P_ER_DATA_CHANGED->MT_MOD_CELLS.

  LOOP AT LT_MOD_DATA INTO WA_MOD_DATA.
    IF WA_MOD_DATA-FIELDNAME = 'BSCHL'
      AND WA_MOD_DATA-VALUE IS NOT INITIAL.
      CLEAR: L_TYPE, L_MSG.
      IF WA_MOD_DATA-VALUE NE '21' AND WA_MOD_DATA-VALUE NE '11'
          AND WA_MOD_DATA-VALUE NE '31' AND WA_MOD_DATA-VALUE NE '50'.
        L_TYPE = 'E'.
        L_MSG = '记账码只能为''11''或''21''或 ''31''或''50'' '.
      ENDIF.

      IF L_TYPE = 'E'.
        CALL METHOD P_ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
            I_MSGID     = '00'
            I_MSGTY     = 'E'
            I_MSGNO     = '001'
            I_MSGV1     = L_MSG
            I_FIELDNAME = WA_MOD_DATA-FIELDNAME
*           i_row_id    = wa_mod_data-row_id
          .
      ENDIF.
    ELSE.
*      call method p_er_data_changed->refresh_protocol.
*      clear p_e_ucomm.
      IF P_ER_DATA_CHANGED->MT_PROTOCOL IS NOT INITIAL.
        CALL METHOD P_ER_DATA_CHANGED->DISPLAY_PROTOCOL
*        exporting
*          i_container        =
*          i_display_toolbar  =
*          i_optimize_columns =
          .
      ENDIF.
    ENDIF.
  ENDLOOP.            " handle_data_changed

  PERFORM FRM_REFRESH_ALV. "刷新
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM HANDLE_ONF4  USING    P_FIELDNAME TYPE LVC_FNAME
                          PS_ROW_NO TYPE LVC_S_ROID
                          PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                          PT_BAD_CELLS TYPE LVC_T_MODI
                          P_DISPLAY.
  FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.
  DATA: GT_T053S TYPE TABLE OF T053S.
  DATA: LT_DDSHRETVAL TYPE STANDARD TABLE OF DDSHRETVAL,
        LW_DDSHRETVAL TYPE DDSHRETVAL.

  CASE P_FIELDNAME.
*科目
    WHEN 'HKONT'.
      READ TABLE GT_ITEM INTO GS_ITEM INDEX PS_ROW_NO-ROW_ID.
      IF SY-SUBRC = 0.
        PERFORM SUB_HELP_HKONT CHANGING GS_ITEM-HKONT.
        IF GS_ITEM-HKONT IS NOT INITIAL.
          MODIFY GT_ITEM FROM GS_ITEM INDEX PS_ROW_NO-ROW_ID.
          " Trigger data changed event
          ASSIGN PR_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
          LW_MOD_CELL-ROW_ID     = PS_ROW_NO-ROW_ID.
          LW_MOD_CELL-SUB_ROW_ID = PS_ROW_NO-SUB_ROW_ID.
          LW_MOD_CELL-FIELDNAME  = 'HKONT'.
          LW_MOD_CELL-VALUE      = GS_ITEM-HKONT.
          APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.
        ENDIF.
      ENDIF.

*原因代码
    WHEN 'RSTGR'.
      REFRESH GT_T053S.

*根据公司代码取付款原因
      SELECT * FROM T053S
        INTO CORRESPONDING FIELDS OF TABLE GT_T053S
        WHERE BUKRS = GS_HEAD-BUKRS
        AND   SPRAS = SY-LANGU.

      IF GT_T053S IS NOT INITIAL .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            RETFIELD        = 'RSTGR'    "指定ALV用F4的字段
            DYNPPROG        = SY-REPID
            VALUE_ORG       = 'S'
          TABLES
            VALUE_TAB       = GT_T053S
            RETURN_TAB      = LT_DDSHRETVAL
          EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.
      ENDIF.

      READ TABLE GT_ITEM INTO GS_ITEM INDEX PS_ROW_NO-ROW_ID.
      IF SY-SUBRC = 0.
        READ TABLE LT_DDSHRETVAL INTO LW_DDSHRETVAL INDEX 1.
        IF SY-SUBRC = 0.
          GS_ITEM-RSTGR = LW_DDSHRETVAL-FIELDVAL.
        ENDIF.
        IF GS_ITEM-RSTGR IS NOT INITIAL.
          MODIFY GT_ITEM FROM GS_ITEM INDEX PS_ROW_NO-ROW_ID.
          " Trigger data changed event
          ASSIGN PR_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
          LW_MOD_CELL-ROW_ID     = PS_ROW_NO-ROW_ID.
          LW_MOD_CELL-SUB_ROW_ID = PS_ROW_NO-SUB_ROW_ID.
          LW_MOD_CELL-FIELDNAME  = 'RSTGR'.
          LW_MOD_CELL-VALUE      = GS_ITEM-RSTGR.
          APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.
        ENDIF.
      ENDIF.

  ENDCASE.

**  Inform ALV Grid that event 'onf4' has been processed
  PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.           "告知F4动作结束
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_ITEM_HKONT  text
*----------------------------------------------------------------------*
FORM SUB_HELP_HKONT  CHANGING P_HKONT.
  SET PARAMETER ID 'BUK' FIELD GS_HEAD-BUKRS.

*搜索帮助
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_HKONT
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ACC_REVERSAL .
  READ TABLE GT_DATA  TRANSPORTING NO FIELDS WITH KEY ZBOX = 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE '请先选择需要冲销的凭证！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA L_BELNR    TYPE BSEG-BELNR.
  DATA L_GJAHR    TYPE BSEG-GJAHR.
  DATA L_CHECK_1  TYPE C.

  CLEAR L_BELNR.
  CLEAR L_GJAHR.
  CLEAR L_CHECK_1.

*自动选中相同凭证的行
  LOOP AT GT_DATA INTO GS_DATA
   WHERE ZBOX = 'X'.

    LOOP AT GT_DATA INTO GS_DATA
     WHERE BELNR_F = GS_DATA-BELNR_F
     AND   GJAHR_F = GS_DATA-GJAHR_F.
*当遇到不同的凭证，退出
      IF GS_DATA-BELNR_F IS INITIAL.
        L_BELNR = GS_DATA-BELNR_F.
      ELSEIF L_BELNR <> GS_DATA-BELNR_F
        OR     L_GJAHR <> GS_DATA-GJAHR_F.
        L_CHECK_1 = 'X'.
        EXIT.
      ENDIF.

      GS_DATA-ZBOX = 'X'.
      MODIFY GT_DATA FROM GS_DATA.
    ENDLOOP.
  ENDLOOP.

  IF L_CHECK_1 <> 'X'.
    MESSAGE '请仅选择相同的凭证进行冲销！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*取冲销原因和过账日期
  CLEAR WA_REVERSAL.
  PERFORM FRM_GET_REV_REASON.

  IF WA_REVERSAL-REASON_REV IS INITIAL.
    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM FRM_MESSAGE_INITIAL.
  CLEAR G_SUC.

  READ TABLE GT_DATA INTO GS_DATA WITH KEY ZBOX = 'X'.
  IF SY-SUBRC = 0.

    CLEAR GS_BKPF.
*冲销凭证
    SELECT SINGLE *
      INTO GS_BKPF
      FROM BKPF
      WHERE BUKRS = GS_DATA-BUKRS
        AND BELNR = GS_DATA-BELNR_F
        AND GJAHR = GS_DATA-GJAHR_F.

*冲销数据准备
    PERFORM FRM_REV_PREP.

*调用冲销BAPI
    PERFORM FRM_REVERSAL_BAPI.

  ENDIF.

  PERFORM FRM_MESSAGE_SHOW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_REV_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_REV_REASON .
  CALL SCREEN 9001 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REV_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REV_PREP .
  WA_REVERSAL-OBJ_TYPE  = GS_BKPF-AWTYP.
  WA_REVERSAL-OBJ_KEY   = GS_BKPF-AWKEY.
  WA_REVERSAL-OBJ_KEY_R = GS_BKPF-AWKEY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSAL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REVERSAL_BAPI .

*   取得系统 LOGICAL SYSTEM
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      OWN_LOGICAL_SYSTEM = WA_REVERSAL-OBJ_SYS.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      REVERSAL = WA_REVERSAL
      BUS_ACT  = 'RFBU'
    IMPORTING
      OBJ_TYPE = WA_OBJ-OBJ_TYPE
      OBJ_KEY  = WA_OBJ-OBJ_KEY
      OBJ_SYS  = WA_OBJ-OBJ_SYS
    TABLES
      RETURN   = IT_RETURN.

  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR WA_REVERSAL.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    CLEAR WA_REVERSAL.
    G_SUC = 'X'.

*更新状态
    LOOP AT GT_DATA INTO GS_DATA
     WHERE ZBOX = 'X'.
      GS_DATA-ZCLJD   = '2'.
      GS_DATA-STATU   = ICON_GREEN_LIGHT.
      GS_DATA-BELNR_F = ''.
      GS_DATA-GJAHR_F = ''.
      GS_DATA-YFJE  = GS_DATA-YFJE - GS_DATA-ZSQFKJE ."更新已付款金额数据 ：原已保存已付款数据 + 已记账成功的申请金额数据
      GS_DATA-YSQJE = GS_DATA-YSQJE - GS_DATA-ZSQFKJE.

      MODIFY GT_DATA FROM GS_DATA.
      CLEAR GS_DATA.
    ENDLOOP.

*更新自建表状态
    REFRESH GT_ZFI017.
    MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
    MODIFY ZFI017 FROM TABLE GT_ZFI017.

  ENDIF.

  LOOP AT IT_RETURN INTO WA_RETURN.
    PERFORM STORE_MESSAGES USING WA_RETURN-ID
                                 WA_RETURN-TYPE
                                 WA_RETURN-MESSAGE_V1
                                 WA_RETURN-MESSAGE_V2
                                 WA_RETURN-MESSAGE_V3
                                 WA_RETURN-MESSAGE_V4
                                 WA_RETURN-NUMBER.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'OK'.
      IF G_STGRD IS INITIAL.
        MESSAGE '冲销原因必输！' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        WA_REVERSAL-REASON_REV = G_STGRD.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: WA_REVERSAL-REASON_REV,
             WA_REVERSAL-PSTNG_DATE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
FORM FRM_UNLOCK.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.
