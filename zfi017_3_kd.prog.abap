REPORT ZFI017_3_KD.
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
DATA:
      BEGIN OF TY_DATA,
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
        EKGRP     TYPE   ZFI017-EKGRP,     "采购组
        EKNAM     TYPE   ZFI017-EKNAM,     "采购组
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
        FPJE      TYPE   EKBE-WRBTR,     "发票金额（净值）
        FPJE_1    TYPE   EKBE-WRBTR,     "发票金额（含税）
        YZFP      TYPE   EKBE-WRBTR,     "预制发票（净值）
        YZFP_1    TYPE   EKBE-WRBTR,      "预制发票（含税）
        ZNAME1    TYPE   ZFI006-ZNAME1,   "审批人
        HTBH      TYPE   STRING ,         "合同编号
        JHJE_1    TYPE   EKBE-WRBTR,     "交货金额（含税）
*&---------------------13074 ----insert- 0804-----------------
        ZJDFKJH   TYPE   ZFI017-ZJDFKJH,
        ZJDYFJE   TYPE   ZFI017-ZJDYFJE,
        ZJDYSQJE  TYPE   ZFI017-ZJDYSQJE,
        ZCEJE     TYPE   ZFI017-ZCEJE,
        ZKSQJE    TYPE   ZFI017-ZKSQJE,
*&--------------------------------------------------------

*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  BEGIN
        YJFKRQ    TYPE EKKO-YJFKRQ1,
        YJFKJE    TYPE EKKO-YJFKJE1,
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  END

*&--代码添加 BY HANDHJD 12.07.2017 09:12:53  BEGIN
        BXQX       TYPE C LENGTH 20,
        BXQSRQ     TYPE ZFI017-ZSQRQ,
        YEAR       TYPE C LENGTH 4, "保修年
        MONTH      TYPE C LENGTH 2,  "保修月
        DAY        TYPE C LENGTH 2, "保修日
        YEAR2      TYPE C LENGTH 4, "保修年
        FPHM        TYPE C LENGTH 20,           "发票号码
*&--代码添加 BY HANDHJD 12.07.2017 09:12:53  END
      END OF TY_DATA.

DATA:
        ZYEAR       TYPE C LENGTH 4, "保修年
        ZMONTH      TYPE C LENGTH 2,  "保修月
        ZDAY        TYPE C LENGTH 2, "保修日
        ZYEAR2      TYPE C LENGTH 4. "保修年
TYPES:BEGIN OF TY_SUMYF ,
        ZSQD  TYPE   ZFI017-ZSQD,    "付款申请单
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        YFJE  TYPE   EKBE-DMBTR,     "已付金额
      END OF TY_SUMYF.
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
      GT_BSEG_A TYPE TABLE OF TY_BSEG,
      GS_BSEG_A LIKE LINE OF GT_BSEG_A.

DATA:
      GT_BSEG_B TYPE TABLE OF TY_BSEG,
      GS_BSEG_B LIKE LINE OF GT_BSEG_B.
*&--代码添加 BY HANDHJD 12.07.2017   END
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
        NETWR     TYPE ZFI017-NETWR,
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
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI017 TYPE TABLE OF ZFI017.
DATA GS_ZFI017 TYPE ZFI017.

DATA GT_DATA   LIKE TABLE OF TY_DATA.
DATA GS_DATA   LIKE LINE OF  GT_DATA.

*& --------------------------13074  -------insert 0807 -----------------
DATA GT_DATA3 LIKE TABLE OF TY_DATA.
*& ----------------------------------------------------------------------

DATA :GT_DATA_B   LIKE TABLE OF TY_DATA,
      GS_DATA_B   LIKE LINE OF  GT_DATA_B.



DATA GT_BSEG   TYPE TABLE OF BSEG.
DATA GS_BSEG   TYPE BSEG.

DATA GT_BKPF   TYPE TABLE OF BKPF.
DATA GS_BKPF   TYPE BKPF.

DATA LT_DATA LIKE TABLE OF TY_DATA.
DATA LS_DATA LIKE TY_DATA.
*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
DATA: LT_DATA1 LIKE TABLE OF TY_DATA,
      LS_DATA1 LIKE LINE OF LT_DATA1.
FIELD-SYMBOLS: <FS_DATA> LIKE TY_DATA.

*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

DATA GT_ZFI017_P TYPE TABLE OF ZFI017_P.
DATA GS_ZFI017_P TYPE ZFI017_P.

DATA GT_ZFI017_P_1 TYPE TABLE OF ZFI017_P.
DATA GS_ZFI017_P_1 TYPE ZFI017_P.

DATA GT_RBKP   TYPE TABLE OF RBKP.
DATA GS_RBKP   TYPE RBKP.


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


*   ----------------------------13074 ---------------------

DATA : GT_NORD TYPE TABLE OF ZNEWORDER,
       GS_NORD TYPE ZNEWORDER.


*   ---------------------------------------------------

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

  IF gw_lvc-fieldname =  'NETWR_1'  OR gw_lvc-fieldname = 'YJHJE_1 ' OR gw_lvc-fieldname = 'FPPZJE_1' .
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
DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.
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
               S_WBS      FOR PRPS-POSID,                                    "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                S_ZSQRQ   FOR ZFI017-ZSQRQ ,                                   "申请日期
                S_ZSQFKR  FOR ZFI017-ZSQFKRQ,                                  "申请付款日期
                S_ZCLJD   FOR ZFI017-ZCLJD,                                    "处理进度
                S_ZSQD    FOR ZFI017-ZSQD,                                     "申请单
                S_EKGRP   FOR LFM1-EKGRP."采购组
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
   AND   ZCLJD    IN S_ZCLJD
   AND   ZSQD     IN S_ZSQD
   AND   POSID    IN S_WBS.

*查询清帐凭证
  IF GT_ZFI017[] IS NOT INITIAL.
    SELECT * FROM BSEG
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
     FOR ALL ENTRIES IN GT_ZFI017
     WHERE BUKRS = GT_ZFI017-BUKRS
*   AND   GJAHR = GT_ZFI017-GJAHR
     AND   LIFNR = GT_ZFI017-LIFNR
*   AND   ZUONR = GT_ZFI017-ZSQD
     AND   BSCHL = '21'.

    "查询采购组名称
    "*查询采购组 及采购组名
    SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM
     INTO CORRESPONDING FIELDS OF TABLE T_LFM1
     FROM LFM1 AS A
     LEFT JOIN T024 AS B
     ON A~EKGRP = B~EKGRP
     FOR ALL ENTRIES IN GT_ZFI017
     WHERE A~LIFNR IN S_LIFNR AND  A~EKORG = S_BUKRS-LOW AND LIFNR = GT_ZFI017-LIFNR AND  A~EKGRP IN S_EKGRP.
    SORT T_LFM1 BY  LIFNR EKGRP .

    MOVE-CORRESPONDING GT_ZFI017 TO GT_EBELN .
    SORT GT_EBELN BY EBELN .
    DELETE ADJACENT DUPLICATES FROM GT_EBELN COMPARING EBELN .
    IF GT_EBELN IS NOT INITIAL.



      SELECT * FROM EKBE
        INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_2
        FOR ALL ENTRIES IN GT_EBELN
        WHERE EBELN = GT_EBELN-EBELN
        AND   VGABE  EQ '2'.
      SORT GT_EKBE_2 BY EBELN .

      SELECT * FROM EKBE
        INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_P
        FOR ALL ENTRIES IN GT_EBELN
        WHERE EBELN = GT_EBELN-EBELN
        AND VGABE EQ 'P'.
      SORT GT_EKBE_P BY EBELN .

      SELECT * FROM EKBE
         INTO CORRESPONDING FIELDS OF TABLE GT_EKBE_1
         FOR ALL ENTRIES IN GT_EBELN
         WHERE EBELN = GT_EBELN-EBELN
         AND VGABE EQ '1'.
      SORT GT_EKBE_1 BY EBELN .

      "*查询订单税码
      SELECT * FROM EKPO
       INTO CORRESPONDING FIELDS OF TABLE GT_EKPO1
       FOR ALL ENTRIES IN GT_EBELN
       WHERE EKPO~EBELN = GT_EBELN-EBELN
       AND  LOEKZ <> 'L'
       AND  MWSKZ NE ''.
      SORT GT_EKPO1 BY EBELN EBELP.
    ENDIF.
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

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_RBKP
    FROM RBKP                "读取发票抬头信息 IT02 150709
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
      GS_DATA-SGTXT = GS_RBKP-SGTXT.  "读取发票抬头文本 150709
    ENDIF .
**初始化凭证
*    GS_DATA-BELNR_F = ''.
*    GS_DATA-GJAHR_F = ''.

*当初始状态，审批人为空
    IF GS_DATA-ZCLJD = '1'.
      GS_DATA-ZNAME1 = ''.
    ENDIF.

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
*    ENDLOOP..

*DELETE BY HANDWY 2015-7-20
**把确认的结账，但是没有找到凭证的，更改其状态.（为了排除确认付款后，冲销的情况）
*    IF GS_DATA-ZCLJD = '3'
*     AND GS_DATA-BELNR_F IS INITIAL
*     AND GS_DATA-GJAHR_F IS INITIAL.
*      GS_DATA-ZCLJD = '2'.
*      GS_DATA-STATU = ICON_GREEN_LIGHT.
*    ENDIF.

    IF GS_DATA-ZCLJD = '2'
    AND GS_DATA-BELNR_F IS NOT INITIAL
    AND GS_DATA-BELNR_F IS NOT INITIAL.
      GS_DATA-ZCLJD = '3'.
      GS_DATA-STATU = ICON_OKAY.
    ENDIF.
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

    " *是否含税
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
  PERFORM  FRM_OUTPUT TABLES GT_LVC              "输出
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
  INIT_FIELDCAT 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
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
*&--代码添加 BY HANDSSY  23.06.2017 21:13:26  BEGIN
  INIT_FIELDCAT 'BXQX'            '保修期限'   '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BXQSRQ'          '保修起始日期'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDSSY 23.06.2017 21:13:26  END

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

    WHEN '&PRINT'.
      READ TABLE GT_DATA INTO GS_DATA
       WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        PERFORM FRM_PRINT.
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
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI017_3_KD'.
  DATA:L_PAGE  TYPE I.
  DATA:L_BUKRS TYPE BUTXT.
  DATA:L_BZ    TYPE STRING.
*  -------------------------------13074 ------------------------

  DATA : L_FORMNAME1 TYPE TDSFNAME VALUE 'ZSFFI017_NEW'.

*  DATA : L_NUM TYPE I.

  DATA : L_FPHM TYPE STRING,
         L_JETJ TYPE WRBTR.

  DATA : L_ZZ TYPE I.

  TYPES:BEGIN OF TY_EKPO,
        EBELN TYPE   EKKO-EBELN,     "采购订单号
        EBELP TYPE   EKPO-EBELP,     "采购订单行项目
        BUKRS TYPE   EKKO-BUKRS,     "公司代码
        LIFNR TYPE   EKKO-LIFNR,     "供应商
        NETWR TYPE   EKPO-NETWR,     "采购订单总价
        WAERS TYPE   EKKO-WAERS,     "货币单位
      END OF TY_EKPO.
DATA : BEGIN OF TY_RBKP,
       BELNR TYPE RBKP-BELNR,
       END OF TY_RBKP.

  DATA : LT_EKPO TYPE TABLE OF TY_EKPO,
        LS_EKPO TYPE TY_EKPO.

  DATA : LT_NORD TYPE TABLE OF ZNEWORDER,
         LS_NORD TYPE ZNEWORDER.

  DATA : LT_DATA11 LIKE TABLE OF TY_DATA,
        LS_DATA11 LIKE LINE OF LT_DATA11.

  DATA : LT_RBKP LIKE TABLE OF TY_RBKP,
        LS_RBKP LIKE LINE OF LT_RBKP.

  DATA : LT_EKBE LIKE TABLE OF EKBE,
        LS_EKBE LIKE LINE OF LT_EKBE.

*  DATA : L_EBELN TYPE STRING.

*  ---------------------------------------------------------------




*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*
  TYPES: BEGIN OF TY_EBELN,
           EBELN TYPE EKKO-EBELN,
           NETWR TYPE EKPO-NETWR,  " BY  HANDKD  13074 0820
         END OF TY_EBELN.
  DATA: LT_EBELN TYPE TABLE OF TY_EBELN,
        LS_EBELN LIKE LINE OF LT_EBELN.

*  -------------------------- 13074 ----------------------

  LT_DATA11 = GT_DATA.

  DELETE LT_DATA11 WHERE ZBOX <> 'X'.
  DELETE ADJACENT DUPLICATES FROM LT_DATA11 COMPARING EBELN.

  CLEAR LS_EKBE.
  LOOP AT LT_DATA11 INTO LS_DATA11.
    READ TABLE GT_EKBE_2 INTO GS_EKBE_2 WITH KEY EBELN = LS_DATA11-EBELN.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING GS_EKBE_2 TO LS_EKBE.
      APPEND LS_EKBE TO LT_EKBE.
    ENDIF.
    L_BZ = LS_DATA11-WAERS.
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


  IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710'." OR S_BUKRS-LOW = '1720'.  BY 13074 HANDKD 0818
    L_FORMNAME = 'ZSFFI017_3_1_KD'.
  ENDIF.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*

* --------------------------------by handkd 13074 0818 ---------

  IF S_BUKRS-LOW = '1720'.
    L_FORMNAME = 'ZSFFI017_KD_1720_3'.
  ENDIF.

* --------------------------------------------------------------
*查询打印的次数
  SELECT * FROM ZFI017_P
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017_P
    FOR ALL ENTRIES IN GT_DATA
    WHERE ZSQD = GT_DATA-ZSQD.
*---------------- 康迪新添--------------------------------*
  DATA:L_TEMP TYPE STRING.  " 康迪新增
  DATA:L_ZSQD1(20) TYPE C.   " 康迪新增
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

  FREE: LT_EBELN,LT_DATA1.
*&--------------------------ADD BY HANDZFF 20170516 15:34 ---------------------------*

  IF S_BUKRS-LOW = '1700' OR S_BUKRS-LOW = '1710' OR S_BUKRS-LOW = '1720'.

*    -------------------------------13074 new  insert -----------------------------

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
      FROM EKPO
      JOIN RSEG ON
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


*      IF L_ZZ = 1.
*        CONCATENATE L_EBELN LS_NORD-EBELN INTO L_EBELN.
*      ELSE.
*        CONCATENATE L_EBELN '、' LS_NORD-EBELN INTO L_EBELN.
*      ENDIF.


      APPEND GS_NORD TO GT_NORD.
      CLEAR GS_NORD.
*      L_NUM = L_NUM + 1.
      ENDIF.
    ENDLOOP.
    CLEAR LS_NORD.
    FREE LT_NORD.

    L_ZZ = 1.
    LOOP AT LT_RBKP INTO LS_RBKP.
      IF L_ZZ = 1.
        CONCATENATE L_FPHM LS_RBKP-BELNR INTO L_FPHM.
      ELSE.
        CONCATENATE L_FPHM '、' LS_RBKP-BELNR INTO L_FPHM.
      ENDIF.
      L_ZZ = L_ZZ + 1.

  ENDLOOP.

*   ----------------------------------------------------------------------------------


    CLEAR: LS_DATA.
    LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
*&--代码添加 BY HANDHJD 12.07.2017 8:48:06  BEGIN
 "BSEG表发票号码
  SELECT
      BUKRS
      EBELN
      BELNR
   FROM BSEG INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_A

   WHERE
    EBELN = GS_DATA-EBELN AND
    BUKRS IN S_BUKRS      .

    IF GT_BSEG_A IS NOT INITIAL.
        SELECT
            BUKRS
            BELNR
            BUZID
            ZUONR
         FROM BSEG INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_B
          FOR ALL ENTRIES IN GT_BSEG_A
         WHERE
          BELNR = GT_BSEG_A-BELNR AND
          BUZID EQ 'T'          AND
          BUKRS IN S_BUKRS.


    ENDIF.

        READ TABLE GT_BSEG_B INTO GS_BSEG_B INDEX 1 .
         IF SY-SUBRC EQ 0.
              LS_DATA-FPHM = GS_BSEG_B-ZUONR.
         ENDIF.


*&--代码添加 BY HANDHJD 12.06.2017 8:48:06  END
*&------金额汇总

      LS_DATA-YFJE = LS_DATA-YFJE + GS_DATA-YFJE.
      LS_DATA-WFJE = LS_DATA-WFJE + GS_DATA-WFJE.
      LS_DATA-ZSQFKJE = LS_DATA-ZSQFKJE + GS_DATA-ZSQFKJE.
*&------收集所有采购订单
      CLEAR:LS_EBELN.
      LS_EBELN-EBELN = GS_DATA-EBELN.
      LS_EBELN-NETWR = GS_DATA-NETWR.
      APPEND LS_EBELN TO LT_EBELN.
*&--------------------------ADD BY HANDHJD 20170711 15:34 ---------------------------*
*------------取出保修期限和保修起始时间-------------
      LS_DATA-BXQX =  GS_DATA-BXQX.
      LS_DATA-BXQSRQ =  GS_DATA-BXQSRQ.


*&--------------------------END BY HANDHJD 20170516 15:34 ---------------------------*

    ENDLOOP.

    LS_DATA1 = GS_DATA.
    LS_DATA1-YFJE = LS_DATA-YFJE.
    LS_DATA1-WFJE = LS_DATA-WFJE.
    LS_DATA1-ZSQFKJE = LS_DATA-ZSQFKJE.
    PERFORM CONV_AMOUNT USING LS_DATA1-ZSQFKJE
                     CHANGING LS_DATA1-ZSQFKJEDX.
    LS_DATA1-FPHM = LS_DATA-FPHM.   "发票号码
    APPEND LS_DATA1 TO LT_DATA1.
*&--------------------------ADD BY HANDHJD 20170711 15:34 ---------------------------*

*------------取出保修期限和保修起始时间-------------

**-----------保修期限和保修起始日期----------------
    LS_DATA1-BXQX =  LS_DATA-BXQX.
  IF LS_DATA1-BXQX EQ '不保修'.
          LS_DATA1-BXQSRQ =  ''.
          ZYEAR = ''.   "年
          ZMONTH = ''.  "月
          ZDAY = ''.   "日
          ZYEAR2 =  ''.   "年
        ELSE.
          LS_DATA1-BXQSRQ =  LS_DATA-BXQSRQ.
          ZYEAR = LS_DATA1-BXQSRQ+0(4).   "年
          ZMONTH = LS_DATA1-BXQSRQ+4(2).  "月
          ZDAY = LS_DATA1-BXQSRQ+6(2).   "日
          ZYEAR2 =  ZYEAR +  LS_DATA1-BXQX.   "年
  ENDIF.



*&--------------------------END BY HANDHJD 20170516 15:34 ---------------------------*
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



    EXPORT LT_EBELN TO MEMORY ID 'EBELN'.

*    FREE: lt_ebeln.
*&--------------------------END BY HANDZFF 20170516 15:34 ---------------------------*
    CALL FUNCTION G_NAME
      EXPORTING
        L_BZ               = L_BZ
        CONTROL_PARAMETERS = CONTROL
        L_PAGE             = L_PAGE
        L_BUKRS            = L_BUKRS
        L_YEAR              = ZYEAR
        L_MONTH             = ZMONTH
        L_DAY               = ZDAY
        L_YEAR2             = ZYEAR2
        L_ZSQD1             = L_ZSQD1
        L_FPHM              = L_FPHM
        L_JETJ              = L_JETJ
*        L_EBELN             = L_EBELN
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

*查询公司代码描述
        SELECT SINGLE BUTXT FROM T001
         INTO L_BUKRS
         WHERE BUKRS = LS_DATA-BUKRS.

        READ TABLE GT_ZFI017_P INTO GS_ZFI017_P
        WITH KEY ZSQD = LS_DATA-ZSQD.
        CLEAR L_PAGE.
        L_PAGE = GS_ZFI017_P-PRINT_NUM + 1.

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
      "MODIFY GT_DATA FROM GS_DATA .
    ENDLOOP.
  ENDIF.

*  -----------------------------new insert -13074----------------
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

    CLEAR : GT_NORD , GT_NORD[].

 ENDIF.
 ENDIF.

*  ----------------------------------------------------------------
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
