REPORT ZFI006_4.
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
        ZBOX       TYPE   C,
        STATU      TYPE   ICONNAME,       "状态烂
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
        BELNR      TYPE   RBKP-BELNR,     "发票凭证
        GJAHR      TYPE   RBKP-GJAHR,     "发票会计年度
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
        ZNAME1     TYPE   ZFI006-ZNAME1,  "审批人
        CELLTAB    TYPE   LVC_T_STYL,     "控制单元格属性
        SGTXT      TYPE   RBKP-SGTXT,     "发票文本  IT02 150709
        YSQJE      TYPE   ZFI006-YSQJE,   "已申请金额
        YSQJE_ADD  TYPE   ZFI006-YSQJE,    "已申请-按订单
        YSQJE_AFP  TYPE   ZFI006-YSQJE,    "已申请-按发票
        YSQJE_TZDD TYPE  ZFI006-YSQJE,    "已申请-调整订单
        YSQJE_TZFP TYPE  ZFI006-YSQJE,    "已申请-调整发票
        EKGRP      TYPE LFM1-EKGRP, "采购组
        EKNAM      TYPE T024-EKNAM, "采购组ming
*&--代码添加 BY HANDYBY 26.06.2017 09:36:30  BEGIN
        SE_BB      TYPE EKBE-DMBTR,  "税额（本币）
        SE         TYPE EKBE-DMBTR,  "税额
*&--代码添加 BY HANDYBY 26.06.2017 09:36:30  END

*&--代码添加 BY HANDYBY 16.08.2017 21:54:05  BEGIN
        BANKN      TYPE LFBK-BANKN, " 银行账号好吗
        KOINH      TYPE LFBK-KOINH, " 账户持有人姓名
        BANKL      TYPE LFBK-BANKL, " 银行代码
        BANKA      TYPE BNKA-BANKA, " 银行名称
*&--代码添加 BY HANDYBY 16.08.2017 21:54:05  END

      END OF TY_DATA.
TYPES:BEGIN OF TY_SUMYF ,
        ZSQD  TYPE   ZFI006-ZSQD,    "付款申请单
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        YFJE  TYPE   EKBE-DMBTR,     "已付金额
      END OF TY_SUMYF.
DATA:GS_SUMYF    TYPE TY_SUMYF.
DATA:GT_SUMYF   TYPE TABLE OF TY_SUMYF.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI006 TYPE TABLE OF ZFI006.
DATA GS_ZFI006 TYPE ZFI006.

DATA GT_ZFI006_1  TYPE TABLE OF ZFI006.
DATA GS_ZFI006_1  TYPE ZFI006.

DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.

DATA GT_BSEG   TYPE TABLE OF BSEG.
DATA GS_BSEG   TYPE BSEG.

DATA GT_BKPF   TYPE TABLE OF BKPF.
DATA GS_BKPF   TYPE BKPF.

DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

DATA G_ANSWER     TYPE STRING. "控制弹出框

DATA L_STRING TYPE STRING.

DATA GT_RBKP   TYPE TABLE OF RBKP.
DATA GS_RBKP   TYPE RBKP.

DATA:GT_EKBE    TYPE TABLE OF EKBE,
     GT_EKBE_02 TYPE TABLE OF EKBE,
     GS_EKBE    TYPE EKBE.

DATA :GT_ZFI006_02 TYPE TABLE OF ZFI006.
DATA  GS_ZFI006_02 TYPE ZFI006.

DATA :GT_ZFI006_03 TYPE TABLE OF ZFI006.
DATA  GS_ZFI006_03 TYPE ZFI006.

DATA:GT_ZFI006_BELNR TYPE TABLE OF ZFI006,
     GT_ZFI006_EBELN TYPE TABLE OF ZFI006.

"采购组名称
TYPES:BEGIN OF TY_LFM1 ,
        LIFNR TYPE LFM1-LIFNR, "供应商号
        EKORG TYPE LFM1-EKORG, "采购组
        EKGRP TYPE LFM1-EKGRP, "采购组
        EKNAM TYPE T024-EKNAM, "采购组ming
      END OF  TY_LFM1 .
DATA : T_LFM1 TYPE TABLE OF TY_LFM1 WITH HEADER LINE.
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
  IF gw_lvc-fieldname =  'NETWR_1' OR  gw_lvc-fieldname = 'YJHJE_1 '  OR gw_lvc-fieldname = 'FPPZJE_1' .
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

DATA LS_CELLTAB TYPE LVC_S_STYL.
DATA LT_CELLTAB TYPE LVC_T_STYL.
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
                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI006-ZSQRQ ,                                   "申请日期
                S_ZSQFKR  FOR ZFI006-ZSQFKRQ,                                  "申请付款日期
*                S_ZCLJD   FOR ZFI006-ZCLJD,                                    "处理进度
                S_ZSQD    FOR ZFI006-ZSQD,                                     "申请单
                S_EKGRP FOR LFM1-EKGRP,"采购组
                S_ZFKXZ  FOR ZFI006-ZFKXZ.                                      "付款性质
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

*查询自建表数据
  SELECT * FROM ZFI006
   INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006
   WHERE BUKRS    IN S_BUKRS
   AND   LIFNR    IN S_LIFNR
   AND   EBELN    IN S_EBELN
   AND   BELNR    IN S_BELNR
   AND   GJAHR    IN S_GJAHR
   AND   ZSQRQ    IN S_ZSQRQ
   AND   ZSQFKRQ  IN S_ZSQFKR
*   AND   ZCLJD    IN S_ZCLJD
   AND   ZSQD     IN S_ZSQD
   AND  ( ZCLJD    = '1' OR  ( ZFKXZ = '3' AND  ZCLJD    = '3' ) )
   AND   ZFKXZ    IN S_ZFKXZ .
  IF GT_ZFI006[] IS NOT INITIAL.

    "查询采购凭证历史记录 数据
    SELECT * FROM EKBE
  INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
  FOR ALL ENTRIES IN GT_ZFI006
  WHERE EBELN = GT_ZFI006-EBELN .
    SORT GT_EKBE BY EBELN EBELP.

    "查询采购组名称
    "*查询采购组 及采购组名
    SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM
    INTO CORRESPONDING FIELDS OF TABLE T_LFM1
    FROM LFM1 AS A
    LEFT JOIN T024 AS B
    ON A~EKGRP = B~EKGRP
    FOR ALL ENTRIES IN GT_ZFI006
    WHERE  A~LIFNR IN S_LIFNR AND A~EKORG = S_BUKRS-LOW AND LIFNR = GT_ZFI006-LIFNR AND  A~EKGRP IN S_EKGRP.
    SORT T_LFM1 BY  LIFNR EKGRP .
    " *查询清帐凭证
    SELECT * FROM BSEG
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
     FOR ALL ENTRIES IN GT_ZFI006
     WHERE BUKRS = GT_ZFI006-BUKRS
*   AND   GJAHR = GT_ZFI006-GJAHR
     AND   LIFNR = GT_ZFI006-LIFNR
*   AND   ZUONR = GT_ZFI006-ZSQD
     AND   BSCHL = '21'.

    IF GT_BSEG IS NOT INITIAL .
*查询是否冲销
      SELECT * FROM BKPF
        INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
        FOR ALL ENTRIES IN GT_BSEG
        WHERE GJAHR = GT_BSEG-GJAHR
        AND   BUKRS = GT_BSEG-BUKRS
        AND   BELNR = GT_BSEG-BELNR.
    ENDIF.
  ENDIF.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_RBKP
    FROM RBKP
    WHERE BUKRS = S_BUKRS-LOW.

  "增加已付款、已申请 读取ZFI006申请付款数据 IT02 151228 BEGIN
  GT_ZFI006_BELNR = GT_ZFI006.
  DELETE GT_ZFI006_BELNR WHERE BELNR EQ ''.
  SORT GT_ZFI006_BELNR BY BELNR GJAHR EBELN .
  DELETE ADJACENT DUPLICATES FROM GT_ZFI006_BELNR COMPARING BELNR GJAHR.
  "先按发票号查询已存储的数据
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006_02
    FROM ZFI006
    FOR ALL ENTRIES IN GT_ZFI006_BELNR
    WHERE BUKRS = GT_ZFI006_BELNR-BUKRS
    AND   BELNR = GT_ZFI006_BELNR-BELNR
    AND   GJAHR = GT_ZFI006_BELNR-GJAHR
    AND   STATU    <> ICON_DELETE
    AND   ZCLJD IN ('1','2','3')..
  SORT GT_ZFI006_02 BY GJAHR BELNR EBELN .

  "采购过号 且发票号为空的存储数据
  GT_ZFI006_EBELN = GT_ZFI006.
  DELETE GT_ZFI006_EBELN WHERE EBELN EQ ''.
  SORT GT_ZFI006_EBELN BY EBELN .
  DELETE ADJACENT DUPLICATES FROM GT_ZFI006_EBELN COMPARING EBELN.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZFI006_03
   FROM ZFI006
   FOR ALL ENTRIES IN GT_ZFI006_EBELN
   WHERE BUKRS = GT_ZFI006_EBELN-BUKRS
    AND  BELNR = ''
    AND  EBELN = GT_ZFI006_EBELN-EBELN
    AND  STATU    <> ICON_DELETE
    AND  ZCLJD IN ('1','2','3')..

  SORT GT_ZFI006_03 BY EBELN .


  " 增加已付款、已申请 读取ZFI006申请付款数据END

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
  DATA L1_TABIX TYPE SY-TABIX.

  CLEAR L1_TABIX.
  LOOP AT  GT_ZFI006 INTO GS_ZFI006.
    L1_TABIX = SY-TABIX .
    READ TABLE T_LFM1 WITH KEY LIFNR = GS_ZFI006-LIFNR.
    IF SY-SUBRC <> 0 .
      " DELETE GT_ZFI006 INDEX L1_TABIX .     "删除供应商不在T_LFM1表的 GT_ZFI006数据不追加到GT_DATA 150804 IT02
      CONTINUE.
    ENDIF.

    "查询交货数据 IT02 151225 BEGIN
    IF GS_ZFI006-EBELN NE ''.
      LOOP AT GT_EKBE INTO  GS_EKBE WHERE EBELN = GS_ZFI006-EBELN AND VGABE = '1'.
        IF GS_EKBE-SHKZG = 'S'.
          GS_ZFI006-YJHJE   = GS_ZFI006-YJHJE   + GS_EKBE-DMBTR.
          GS_ZFI006-YJHJE_1 = GS_ZFI006-YJHJE_1 + GS_EKBE-WRBTR.
        ENDIF.
        IF GS_EKBE-SHKZG = 'H' .
          GS_ZFI006-YJHJE   = GS_ZFI006-YJHJE   - GS_EKBE-DMBTR.
          GS_ZFI006-YJHJE_1 = GS_ZFI006-YJHJE_1 - GS_EKBE-WRBTR.
        ENDIF.
      ENDLOOP.
    ENDIF.
    MOVE-CORRESPONDING GS_ZFI006 TO GS_DATA.
    "控制单元格发票号不空、采购订单号为空的行 申请付款金额不能编辑修改
    IF GS_ZFI006-BELNR NE '' AND GS_ZFI006-EBELN EQ ''.
      LS_CELLTAB-FIELDNAME = 'ZSQFKJE' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
      INSERT LINES OF LT_CELLTAB INTO TABLE GS_DATA-CELLTAB.

    ENDIF.
    "添加采购组及采购组名150804 IT02
    READ TABLE T_LFM1 WITH KEY LIFNR = GS_DATA-LIFNR.
    IF SY-SUBRC = 0.
      GS_DATA-EKGRP = T_LFM1-EKGRP. "
      GS_DATA-EKNAM = T_LFM1-EKNAM.
    ENDIF.
    READ TABLE GT_RBKP INTO GS_RBKP WITH KEY BELNR = GS_DATA-BELNR.
    IF SY-SUBRC = 0.
      GS_DATA-SGTXT = GS_RBKP-SGTXT.  "发票文本
    ENDIF .
*初始化凭证
    GS_DATA-BELNR_F = ''.
    GS_DATA-GJAHR_F = ''.

*当初始状态，审批人为空
    IF GS_DATA-ZCLJD = '1'.
      GS_DATA-ZNAME1 = ''.
    ENDIF.

*查询清帐凭证
    LOOP AT GT_BSEG INTO GS_BSEG
     WHERE  BUKRS = GS_DATA-BUKRS
     AND    LIFNR = GS_DATA-LIFNR
     AND    ZUONR = GS_DATA-ZSQD
     AND    BSCHL = '21'.

*排除冲销凭证
      READ TABLE GT_BKPF INTO GS_BKPF
      WITH KEY GJAHR = GS_BSEG-GJAHR
               BUKRS = GS_BSEG-BUKRS
               BELNR = GS_BSEG-BELNR.
      IF SY-SUBRC = 0.
        IF GS_BKPF-STBLG IS  INITIAL.
          GS_DATA-BELNR_F = GS_BSEG-BELNR.
          GS_DATA-GJAHR_F = GS_BSEG-GJAHR.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

**把确认的结账，但是没有找到凭证的，更改其状态.（为了排除确认付款后，冲销的情况）
*    IF GS_DATA-ZCLJD = '3'
*     AND GS_DATA-BELNR_F IS INITIAL
*     AND GS_DATA-GJAHR_F IS INITIAL.
*      GS_DATA-ZCLJD = '2'.
*      GS_DATA-STATU = ICON_GREEN_LIGHT.
*    ENDIF.

    "已付款金额 、已申请-按订单、已申请-按发票、已申请-调整订单、已申请-调整发票 add by it02 151230
    CLEAR:GS_DATA-YFJE,GS_DATA-YSQJE_ADD,GS_DATA-YSQJE_AFP, GS_DATA-YSQJE_TZDD,GS_DATA-YSQJE_TZFP.

    ""先查询汇总 按发票号 、年度、采购订单号
    IF  GS_DATA-GJAHR NE ''.
      LOOP AT GT_ZFI006_02 INTO GS_ZFI006_02 WHERE BELNR = GS_DATA-BELNR AND GJAHR = GS_DATA-GJAHR AND  EBELN = GS_DATA-EBELN..
        " 已付款金额
        IF GS_ZFI006_02-ZCLJD EQ '3'.
          GS_DATA-YFJE = GS_DATA-YFJE + GS_ZFI006_02-ZSQFKJE.
        ENDIF.
        IF GS_ZFI006_02-BELNR EQ '' AND ( GS_ZFI006_02-ZFKXZ = '1' OR GS_ZFI006_02-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006_02-ZSQFKJE.             "已申请-按订单             "
        ENDIF.
        IF GS_ZFI006_02-BELNR NE '' AND ( GS_ZFI006_02-ZFKXZ = '1' OR GS_ZFI006_02-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006_02-ZSQFKJE.             "已申请-按发票
        ENDIF.
        IF GS_ZFI006_02-BELNR EQ '' AND ( GS_ZFI006_02-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006_02-ZSQFKJE.         "已申请-调整订单
        ENDIF.
        IF GS_ZFI006_02-BELNR NE '' AND ( GS_ZFI006_02-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006_02-ZSQFKJE.            "已申请-调整发票
        ENDIF.
      ENDLOOP.
    ELSEIF GS_DATA-EBELN NE ''.
      LOOP AT GT_ZFI006_02 INTO GS_ZFI006_02 WHERE  EBELN = GS_DATA-EBELN..
        " 已付款金额
        IF GS_ZFI006_02-ZCLJD EQ '3'.
          GS_DATA-YFJE = GS_DATA-YFJE + GS_ZFI006_02-ZSQFKJE.
        ENDIF.
        IF GS_ZFI006_02-BELNR EQ '' AND ( GS_ZFI006_02-ZFKXZ = '1' OR GS_ZFI006_02-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006_02-ZSQFKJE.             "已申请-按订单             "
        ENDIF.
        IF GS_ZFI006_02-BELNR NE '' AND ( GS_ZFI006_02-ZFKXZ = '1' OR GS_ZFI006_02-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006_02-ZSQFKJE.             "已申请-按发票
        ENDIF.
        IF GS_ZFI006_02-BELNR EQ '' AND ( GS_ZFI006_02-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006_02-ZSQFKJE.         "已申请-调整订单
        ENDIF.
        IF GS_ZFI006_02-BELNR NE '' AND ( GS_ZFI006_02-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006_02-ZSQFKJE.            "已申请-调整发票
        ENDIF.
      ENDLOOP.


    ENDIF.

    "再根据发票号为空 、采购订单号汇总 已付、已申请金额
    IF GS_DATA-EBELN NE ''.
      LOOP AT GT_ZFI006_03 INTO GS_ZFI006_03 WHERE   EBELN = GS_DATA-EBELN..
        " 已付款金额
        IF GS_ZFI006_03-ZCLJD EQ '3'.
          GS_DATA-YFJE = GS_DATA-YFJE + GS_ZFI006_03-ZSQFKJE.
        ENDIF.
        IF GS_ZFI006_03-BELNR EQ '' AND ( GS_ZFI006_03-ZFKXZ = '1' OR GS_ZFI006_03-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_ADD = GS_DATA-YSQJE_ADD + GS_ZFI006_03-ZSQFKJE.             "已申请-按订单             "
        ENDIF.
        IF GS_ZFI006_03-BELNR NE '' AND ( GS_ZFI006_03-ZFKXZ = '1' OR GS_ZFI006_03-ZFKXZ = '2' ) .
          GS_DATA-YSQJE_AFP = GS_DATA-YSQJE_AFP + GS_ZFI006_03-ZSQFKJE.             "已申请-按发票
        ENDIF.
        IF GS_ZFI006_03-BELNR EQ '' AND ( GS_ZFI006_03-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZDD =  GS_DATA-YSQJE_TZDD + GS_ZFI006_03-ZSQFKJE.         "已申请-调整订单
        ENDIF.
        IF GS_ZFI006_03-BELNR NE '' AND ( GS_ZFI006_03-ZFKXZ = '3'  ) .
          GS_DATA-YSQJE_TZFP  = GS_DATA-YSQJE_TZFP  + GS_ZFI006_03-ZSQFKJE.            "已申请-调整发票
        ENDIF.

      ENDLOOP.
    ENDIF.


    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

*&--代码添加 BY HANDYBY 26.06.2017 09:39:32  BEGIN
  DATA: BEGIN OF LS_KONV,
          KNUMV TYPE KONV-KNUMV,
          KPOSN TYPE KONV-KPOSN,
          STUNR TYPE KONV-STUNR,
          ZAEHK TYPE KONV-ZAEHK,
          KSCHL TYPE KONV-KSCHL,
          KWERT TYPE KONV-KWERT,
          EBELN TYPE EKKO-EBELN,
          WAERS TYPE EKKO-WAERS,
          WKURS TYPE EKKO-WKURS,
        END OF LS_KONV .
  DATA LT_KONV LIKE TABLE OF LS_KONV .
  SELECT A~KNUMV
         A~KPOSN
         A~STUNR
         A~ZAEHK
         A~KSCHL
         A~KWERT
         B~EBELN
         B~WAERS
         B~WKURS
    INTO CORRESPONDING FIELDS OF TABLE LT_KONV
    FROM KONV AS A
   INNER JOIN EKKO AS B
      ON A~KNUMV = B~KNUMV
     FOR ALL ENTRIES IN GT_DATA
   WHERE B~EBELN = GT_DATA-EBELN .
  SORT LT_KONV BY EBELN .
  LOOP AT GT_DATA INTO GS_DATA .
    IF GS_DATA-WAERS_1 EQ 'CNY'.
      READ TABLE LT_KONV WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_KONV INTO LS_KONV FROM SY-TABIX .
          IF LS_KONV-EBELN = GS_DATA-EBELN .
            IF LS_KONV-KSCHL = 'Z010'.
              GS_DATA-SE_BB = GS_DATA-SE_BB + LS_KONV-KWERT .
            ENDIF.
            CLEAR LS_KONV .
          ELSE .
            CLEAR LS_KONV .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.
      GS_DATA-SE = GS_DATA-SE_BB .
    ELSE .
      GS_DATA-SE_BB = ''.
      GS_DATA-SE = ''.
    ENDIF.
    MODIFY GT_DATA FROM GS_DATA .
    CLEAR GS_DATA .
    CLEAR LS_KONV .
  ENDLOOP.
*&--代码添加 BY HANDYBY 26.06.2017 09:39:32  END

  SORT GT_DATA BY ZSQD BELNR EBELN.

**更新自建表
*  MOVE-CORRESPONDING GT_DATA TO GT_ZFI006.
*  MODIFY ZFI006 FROM TABLE GT_ZFI006.
*  REFRESH GT_ZFI006.

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
  INIT_FIELDCAT 'ZNAME1'         '审批人'              '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'              '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'          '供应商'                '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '供应商名称'            '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'          '采购组'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKNAM'          '采购组名'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单号'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '本位币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'        '凭证货币'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR'          '采购订单含税金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR_1'        '采购订单含税金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE'          '已交货金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJHJE_1'        '已交货金额'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 26.06.2017 10:18:57  BEGIN
  INIT_FIELDCAT 'SE_BB'          '税额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SE'        '税额'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 26.06.2017 10:18:57  END
  INIT_FIELDCAT 'BELNR'          '发票凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '发票会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPPZJE'         '发票凭证金额（本币）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPPZJE_1'       '发票凭证金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE'           '已付金额'   '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'YSQJE'          '已申请金额'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_ADD'      '已申请-按订单'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_AFP'      '已申请-按发票'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_TZDD'     '已申请-调整订单'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE_TZFP'     '已申请-调整发票'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZFKXZ'          '付款性质'   '' '' '' '' ''  'ZFI006' 'ZFKXZ'.
  INIT_FIELDCAT 'ZSQFKJE'        '申请付款金额'   '' '' '' 'X' '' 'ZFI006' 'ZSQFKJE'.
  INIT_FIELDCAT 'WAERS_2'        '货币'   '6' '' '' 'X' '' 'EKKO' 'WAERS'.
  INIT_FIELDCAT 'ZSQRQ'          '申请日期'   '8' '' '' '' '' 'ZFI006' 'ZSQRQ'.
  INIT_FIELDCAT 'ZSQFKRQ'        '申请付款日期'   '' '' '' '' '' 'ZFI006' 'ZSQFKRQ'.
  INIT_FIELDCAT 'ZZY'            '摘要'   '15' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'ZCLJD'          '处理进度'   '6' '' '' '' '' 'ZFI006' 'ZCLJD' .
  INIT_FIELDCAT 'BELNR_F'        '付款会计凭证'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR_F'        '付款会计年度'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '发票文本'   '' '' '' '' '' '' ''.

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
  DATA L_SUBRC TYPE SY-SUBRC."检查输入项
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
        LOOP AT GT_DATA INTO GS_DATA
         WHERE ZBOX = 'X'.

*更新屏幕
          LOOP AT GT_DATA INTO GS_DATA
              WHERE BELNR = GS_DATA-BELNR
              AND   GJAHR = GS_DATA-GJAHR
              AND   BUKRS = GS_DATA-BUKRS
              AND   ZSQD  = GS_DATA-ZSQD.

            GS_DATA-ZBOX = 'X'.
            MODIFY GT_DATA FROM GS_DATA.
            MOVE-CORRESPONDING GS_DATA TO GS_ZFI006_1.
            GS_ZFI006_1-ZNAME = SY-UNAME.
            GS_ZFI006_1-ZDATE = SY-DATUM.
            GS_ZFI006_1-ZTIME = SY-UZEIT.
            APPEND GS_ZFI006_1 TO GT_ZFI006_1.
          ENDLOOP.

        ENDLOOP.

*更新数据库表
        MODIFY  ZFI006 FROM TABLE GT_ZFI006_1.
        REFRESH GT_ZFI006_1.
        CLEAR GS_DATA.

*提示保存成功
        MESSAGE S002(Z001).
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.


    WHEN '&PRINT'.
      READ TABLE GT_DATA INTO GS_DATA
       WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        PERFORM FRM_PRINT.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&DEL'.
*提示对话框
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION  = '是否执行删除操作'
        IMPORTING
          ANSWER         = G_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
      IF G_ANSWER <> '1'.
        EXIT.
      ENDIF.

*更新状态
      LOOP AT GT_DATA INTO GS_DATA
      WHERE ZBOX = 'X'.
        GS_DATA-STATU = ICON_DELETE.
        MODIFY GT_DATA FROM GS_DATA.

        MOVE-CORRESPONDING GS_DATA TO GS_ZFI006_1.
        GS_ZFI006_1-ZNAME = SY-UNAME.
        GS_ZFI006_1-ZDATE = SY-DATUM.
        GS_ZFI006_1-ZTIME = SY-UZEIT.
        APPEND GS_ZFI006_1 TO GT_ZFI006_1.
      ENDLOOP.

*更新数据库表
      MODIFY  ZFI006 FROM TABLE GT_ZFI006_1.
      REFRESH GT_ZFI006_1.
      CLEAR GS_DATA.

    WHEN '&F03' OR '&F15' OR  '&F12'.
      PERFORM FRM_UNLOCK.
  ENDCASE.

  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

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

* 先判断若付款性质为3且编辑的申请付款金额为负数就提示不允许输入负数 ，并清空当前值
      IF LS_DATA-ZFKXZ NE '3' AND  WA_MOD_CELL-VALUE < 0 .
        LS_DATA-ZSQFKJE  = 0 .  "清空付款申请金额值为0
        WA_MOD_CELL-VALUE = 0. "当前编辑金额为0.
        L_ZSQFKJE  = 0 .       "累计金额为0
        MODIFY GT_DATA FROM LS_DATA INDEX WA_MOD_CELL-ROW_ID.
        "   *汇总申请付款金额（汇总逻辑 =  修改的行 + 其他的行 （对于同一张发票而言））
        IF LS_DATA-BELNR NE ''.

          LOOP AT GT_DATA INTO GS_DATA
          WHERE ZSQD  = LS_DATA-ZSQD
          AND   BELNR = LS_DATA-BELNR
          AND   GJAHR = LS_DATA-GJAHR
          AND   EBELN <> ''
          AND   EBELN <> LS_DATA-EBELN
          .
            L_ZSQFKJE = L_ZSQFKJE + GS_DATA-ZSQFKJE.
          ENDLOOP.

          LOOP AT GT_DATA INTO GS_DATA
          WHERE ZSQD  = LS_DATA-ZSQD
          AND   BELNR = LS_DATA-BELNR
          AND   GJAHR = LS_DATA-GJAHR
          AND   EBELN = ''.
            GS_DATA-ZSQFKJE = L_ZSQFKJE.
            MODIFY GT_DATA FROM GS_DATA.
            CLEAR GS_DATA.
          ENDLOOP.
        ENDIF.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            E_GRID = G_REF_GRID.

        "*  自动定位光标
        STBL-COL = 'X'.
        STBL-ROW = 'X'.
        CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = STBL.
        MESSAGE '此处付款性质为1、2不允许输入负数 ' TYPE 'I'.
        LEAVE TO SCREEN SY-DYNNR .
        .
      ENDIF.

*去除金额的,号
      L_STRING = WA_MOD_CELL-VALUE.
      REPLACE ',' IN L_STRING WITH ''.
      L_ZSQFKJE  = L_STRING.

*汇总申请付款金额（汇总逻辑 =  修改的行 + 其他的行 （对于同一张发票而言））
      IF LS_DATA-BELNR NE ''.
        LOOP AT GT_DATA INTO GS_DATA
        WHERE ZSQD  = LS_DATA-ZSQD
        AND   BELNR = LS_DATA-BELNR
        AND   GJAHR = LS_DATA-GJAHR
        AND   EBELN <> ''
        AND   EBELN <> LS_DATA-EBELN
       .
          L_ZSQFKJE = L_ZSQFKJE + GS_DATA-ZSQFKJE.
        ENDLOOP.

        LOOP AT GT_DATA INTO GS_DATA
        WHERE ZSQD  = LS_DATA-ZSQD
        AND   BELNR = LS_DATA-BELNR
        AND   GJAHR = LS_DATA-GJAHR
        AND   EBELN = ''.
          GS_DATA-ZSQFKJE = L_ZSQFKJE.
          MODIFY GT_DATA FROM GS_DATA.
          CLEAR GS_DATA.
        ENDLOOP.
      ELSE.

      ENDIF.
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
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI006_4'.
  DATA:L_PAGE TYPE I VALUE 1.
  DATA:L_BUKRS TYPE BUTXT."公司代码描述

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

*根据订单号，行项目号进行排序
  SORT LT_DATA  BY ZSQD.

  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.

    AT NEW ZSQD .
      REFRESH LT_DATA.
    ENDAT.

    PERFORM CONV_AMOUNT USING GS_DATA-ZSQFKJE
                    CHANGING GS_DATA-ZSQFKJEDX.

    APPEND GS_DATA TO LT_DATA.

    AT END OF ZSQD.

      READ TABLE LT_DATA INTO LS_DATA INDEX 1.

      SELECT SINGLE BUTXT FROM T001
       INTO L_BUKRS
       WHERE BUKRS = LS_DATA-BUKRS.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          L_PAGE             = L_PAGE
          L_BUKRS            = L_BUKRS
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

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT CHANGING L_SUBRC TYPE SY-SUBRC.

*检查抬头
  IF S_EBELN IS NOT INITIAL .
    LOOP AT GT_DATA INTO GS_DATA
    WHERE ZBOX = 'X'.

*锁对象检查，加锁
      CALL FUNCTION 'ENQUEUE_EZFI006'
        EXPORTING
*         MODE_ZFI006    = 'E'
*         MANDT          = SY-MANDT
          ZSQD           = GS_DATA-ZSQD
*         X_ZSQD         = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC <> 0.
        CLEAR L_STRING.
        CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
        MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
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

*锁对象检查，加锁
      CALL FUNCTION 'ENQUEUE_EZFI006'
        EXPORTING
*         MODE_ZFI006    = 'E'
*         MANDT          = SY-MANDT
          ZSQD           = GS_DATA-ZSQD
*         X_ZSQD         = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC <> 0.
        CONCATENATE '订单' GS_DATA-ZSQD '已被其他人锁定' INTO L_STRING.
        MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
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

*检查是否有删除标识
  LOOP AT GT_DATA INTO GS_DATA
  WHERE ZBOX = 'X'.
    IF GS_DATA-STATU = ICON_DELETE.
      MESSAGE '请不要保存已经打上删除标记的行项目' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM FRM_UNLOCK.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.


*&---------------------------------------------------------------------*
*     金额大写
*----------------------------------------------------------------------*
FORM CONV_AMOUNT USING VALUE(F_SOURCE)
                 CHANGING VALUE(F_RESULT).
  DATA: SCR(30) TYPE C, RES(60) TYPE C,FEN(2) TYPE C .
  DATA: LEN TYPE I, C1 TYPE I, C2 TYPE I, C3 TYPE I, C4 TYPE I.
  DATA: D1(1) TYPE C, D2(1) TYPE C, D3 TYPE I.
  DATA: DIGIT(2)  TYPE C, WEIGHT(2) TYPE C.
  DATA: RULE1(20) TYPE C VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: RULE2(30) TYPE C VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  IF F_SOURCE < 0.
    F_SOURCE = -1 * F_SOURCE.
  ENDIF.
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
