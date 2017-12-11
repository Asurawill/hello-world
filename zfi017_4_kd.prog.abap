REPORT ZFI017_4_KD.
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
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:EKKO,RBKP,ZFI017,LFM1,PRPS.
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
        POSID     TYPE   ZFI017-POSID,   "项目WBS
        POST1     TYPE   ZFI017-POST1,   "项目描述
        MWSKZ     TYPE   ZFI017-MWSKZ,   "是否含税
        ZTERM     TYPE   ZFI017-ZTERM,   "子付款条件
        TEXT1     TYPE   ZFI017-TEXT1,   "子付款条件描述
        RATPZ     TYPE   ZFI017-RATPZ,   "应付比例
        YFJE1     TYPE   ZFI017-YFJE1,   "应付金额
        YFBL      TYPE   ZFI017-YFBL,    "已付比例
        WFJE      TYPE   ZFI017-WFJE,    "未付金额
        ZCH       TYPE   ZFI017-ZCH,     "拆行
        ZNAME1    TYPE   ZFI006-ZNAME1,  "审批人
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

*&---------------------13074 ----insert- 0804-----------------
        ZJDFKJH   TYPE   ZFI017-ZJDFKJH,
        ZJDYFJE   TYPE   ZFI017-ZJDYFJE,
        ZJDYSQJE  TYPE   ZFI017-ZJDYSQJE,
        ZCEJE     TYPE   ZFI017-ZCEJE,
        ZKSQJE    TYPE   ZFI017-ZKSQJE,
*&--------------------------------------------------------
      END OF TY_DATA.
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

TYPES:BEGIN OF TY_EBELN,
        EBELN TYPE EKBE-EBELN,
      END OF TY_EBELN.

TYPES:BEGIN OF TY_JH,
        EBELN TYPE EBELN,
        WRBTR TYPE WRBTR,
      END OF TY_JH.

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

DATA:GT_JH TYPE TABLE OF TY_JH,
     GS_JH TYPE TY_JH.

DATA:GS_SUMYF    TYPE TY_SUMYF.
DATA:GT_SUMYF   TYPE TABLE OF TY_SUMYF.

DATA:GT_EKBE_1 TYPE TABLE OF EKBE,
     GS_EKBE_1 TYPE EKBE.

DATA:GT_EKBE_2 TYPE TABLE OF EKBE,
     GS_EKBE_2 TYPE EKBE.

DATA:GT_EKBE_P TYPE TABLE OF EKBE,
     GS_EKBE_P TYPE EKBE.

DATA GT_EKPO1 TYPE TABLE OF EKPO.
DATA GS_EKPO1 TYPE EKPO.

DATA: GT_FTAXP LIKE  TABLE OF FTAXP .
DATA: GS_FTAXP LIKE FTAXP.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI017 TYPE TABLE OF ZFI017.
DATA GS_ZFI017 TYPE ZFI017.

DATA GT_ZFI017_1  TYPE TABLE OF ZFI017.
DATA GS_ZFI017_1  TYPE ZFI017.

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


DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

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
                S_WBS     FOR PRPS-POSID,                                       "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI017-ZSQRQ ,                                   "申请日期
                S_ZSQFKR  FOR ZFI017-ZSQFKRQ,                                  "申请付款日期
*                S_ZCLJD   FOR ZFI017-ZCLJD,                                    "处理进度
                S_ZSQD    FOR ZFI017-ZSQD,                                     "申请单
                S_EKGRP FOR LFM1-EKGRP."采购组
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
  SELECT * FROM ZFI017
   INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017
   WHERE BUKRS    IN S_BUKRS
   AND   LIFNR    IN S_LIFNR
   AND   EBELN    IN S_EBELN
*   AND   BELNR    IN S_BELNR
*   AND   GJAHR    IN S_GJAHR
   AND   ZSQRQ    IN S_ZSQRQ
   AND   ZSQFKRQ  IN S_ZSQFKR
*   AND   ZCLJD    IN S_ZCLJD
   AND   ZSQD     IN S_ZSQD
   AND   POSID    IN S_WBS
   AND   ( ZCLJD    = '1' OR ZCLJD = '2' OR ZCLJD = '3' ) .
  IF GT_ZFI017[] IS NOT INITIAL.
    "查询采购组名称
    "*查询采购组 及采购组名
    SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM
    INTO CORRESPONDING FIELDS OF TABLE T_LFM1
    FROM LFM1 AS A
    LEFT JOIN T024 AS B
    ON A~EKGRP = B~EKGRP
    FOR ALL ENTRIES IN GT_ZFI017
    WHERE  A~LIFNR IN S_LIFNR AND A~EKORG = S_BUKRS-LOW AND LIFNR = GT_ZFI017-LIFNR AND  A~EKGRP IN S_EKGRP.
    SORT T_LFM1 BY  LIFNR EKGRP .
    " *查询清帐凭证
    SELECT * FROM BSEG
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
     FOR ALL ENTRIES IN GT_ZFI017
     WHERE BUKRS = GT_ZFI017-BUKRS
*   AND   GJAHR = GT_ZFI017-GJAHR
     AND   LIFNR = GT_ZFI017-LIFNR
*   AND   ZUONR = GT_ZFI017-ZSQD
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
    MOVE-CORRESPONDING GT_ZFI017 TO GT_EBELN .
    SORT GT_EBELN BY EBELN .
    DELETE ADJACENT DUPLICATES FROM GT_EBELN COMPARING EBELN .
    IF GT_EBELN IS NOT INITIAL.
      SELECT * FROM EKBE
        INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_2
        FOR ALL ENTRIES IN GT_EBELN
        WHERE EBELN = GT_EBELN-EBELN
        AND  VGABE EQ '2'.
      SORT GT_EKBE_2 BY EBELN .

      SELECT * FROM EKBE
        INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_P
        FOR ALL ENTRIES IN GT_EBELN
        WHERE EBELN = GT_EBELN-EBELN
        AND VGABE EQ 'P'.
      SORT GT_EKBE_P BY EBELN .

      "*查询订单税码
      SELECT * FROM EKPO
       INTO CORRESPONDING FIELDS OF TABLE GT_EKPO1
       FOR ALL ENTRIES IN GT_EBELN
       WHERE EKPO~EBELN = GT_EBELN-EBELN
       AND  LOEKZ <> 'L'
       AND  MWSKZ NE ''.
      SORT GT_EKPO1 BY EBELN EBELP.

      SELECT * FROM EKBE
      INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_1
      FOR ALL ENTRIES IN GT_EBELN
      WHERE EBELN = GT_EBELN-EBELN
      AND VGABE EQ '1'.
      SORT GT_EKBE_1 BY EBELN .

    ENDIF.
  ENDIF.
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
  DATA L1_TABIX TYPE SY-TABIX.
  CLEAR L1_TABIX.
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
  SORT GT_JH BY EBELN  .

  LOOP AT  GT_ZFI017 INTO GS_ZFI017.
    L1_TABIX = SY-TABIX .
    READ TABLE T_LFM1 WITH KEY LIFNR = GS_ZFI017-LIFNR.
    IF SY-SUBRC <> 0 .
      " DELETE GT_ZFI017 INDEX L1_TABIX .     "删除供应商不在T_LFM1表的 GT_ZFI006数据不追加到GT_DATA 150804 IT02
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING GS_ZFI017 TO GS_DATA.
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
*
**查询清帐凭证
*    LOOP AT GT_BSEG INTO GS_BSEG
*     WHERE  BUKRS = GS_DATA-BUKRS
*     AND    LIFNR = GS_DATA-LIFNR
*     AND    ZUONR = GS_DATA-ZSQD
*     AND    BSCHL = '21'.
*
**排除冲销凭证
*      READ TABLE GT_BKPF INTO GS_BKPF
*      WITH KEY GJAHR = GS_BSEG-GJAHR
*               BUKRS = GS_BSEG-BUKRS
*               BELNR = GS_BSEG-BELNR.
*      IF SY-SUBRC = 0.
*        IF GS_BKPF-STBLG IS  INITIAL.
*          GS_DATA-BELNR_F = GS_BSEG-BELNR.
*          GS_DATA-GJAHR_F = GS_BSEG-GJAHR.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

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

  SORT GT_DATA BY ZSQD BELNR EBELN.
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

**更新自建表
*  MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
*  MODIFY ZFI017 FROM TABLE GT_ZFI017.
*  REFRESH GT_ZFI017.
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
  INIT_FIELDCAT 'YFBL'           '订单已付比例'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJE'          '订单已申请金额'   '' '' '' '' '' '' ''.
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
  INIT_FIELDCAT 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
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
          MODIFY GT_DATA FROM GS_DATA.
          MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
          GS_ZFI017_1-ZNAME = SY-UNAME.
          GS_ZFI017_1-ZDATE = SY-DATUM.
          GS_ZFI017_1-ZTIME = SY-UZEIT.
          APPEND GS_ZFI017_1 TO GT_ZFI017_1.
        ENDLOOP.

*更新数据库表
        MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
        REFRESH GT_ZFI017_1.
        CLEAR GS_DATA.

*提示保存成功
        MESSAGE S002(Z001).
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.


    WHEN '&PRINT'.
*      READ TABLE GT_DATA INTO GS_DATA
*       WITH KEY ZBOX = 'X'.
*      IF SY-SUBRC = 0.
*        PERFORM FRM_PRINT.
*      ELSE.
*        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
*      ENDIF.

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

        MOVE-CORRESPONDING GS_DATA TO GS_ZFI017_1.
        GS_ZFI017_1-ZNAME = SY-UNAME.
        GS_ZFI017_1-ZDATE = SY-DATUM.
        GS_ZFI017_1-ZTIME = SY-UZEIT.
        APPEND GS_ZFI017_1 TO GT_ZFI017_1.
      ENDLOOP.

*更新数据库表
      MODIFY  ZFI017 FROM TABLE GT_ZFI017_1.
      REFRESH GT_ZFI017_1.
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
  DATA: L_ZSQFKJE   TYPE ZFI017-ZSQFKJE.
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
  DATA:L_PAGE TYPE I.

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

    APPEND GS_DATA TO LT_DATA.

    AT END OF ZSQD.
      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
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
