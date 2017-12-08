*Description: 物料基本单位变更影响数据分析报表
*&---------------------------------------------------------------------*
*& Report  ZDQ_MM_084
*&
*&---------------------------------------------------------------------*
*&Program ID *&
* Program Name :ZDQ_MM_084
* Date Created : 2013/12/05
* Created By   : 石文海
* Description  : 物料基本单位变更影响数据分析报表
*&---------------------------------------------------------------------*
*&  变更记录
*&   日期            操作者         请求号
*&---------------------------------------------------------------------*

REPORT ZMM018.

TABLES:MARC.

*&---------------------------------------------------------------------*
*& 类型定义
*&---------------------------------------------------------------------*
*作为BOM父件
TYPES:BEGIN OF TY_TAB1.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV1.
TYPES:END OF TY_TAB1.

*作为BOM子件
TYPES:BEGIN OF TY_TAB2.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV2.
TYPES:END OF TY_TAB2.

*工艺路线
TYPES:BEGIN OF TY_TAB3.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV3.
TYPES:END OF TY_TAB3.

*序列号参数文件
TYPES:BEGIN OF TY_TAB4.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV4.
TYPES:END OF TY_TAB4.

*货盘化数据
TYPES:BEGIN OF TY_TAB5.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV5.
TYPES:END OF TY_TAB5.

*库存
TYPES:BEGIN OF TY_TAB6.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV6.
TYPES:END OF TY_TAB6.

*采购申请
TYPES:BEGIN OF TY_TAB7.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV7.
TYPES:END OF TY_TAB7.

*采购订单
TYPES:BEGIN OF TY_TAB8.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV8.
TYPES:END OF TY_TAB8.

*预留
TYPES:BEGIN OF TY_TAB9.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV9.
TYPES:END OF TY_TAB9.

*生产订单
TYPES:BEGIN OF TY_TAB10.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV10.
TYPES:END OF TY_TAB10.

*销售订单
TYPES:BEGIN OF TY_TAB11.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV11.
TYPES:END OF TY_TAB11.

*成本核算数据
TYPES:BEGIN OF TY_TAB12.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV12.
TYPES:END OF TY_TAB12.

*计划订单数据
TYPES:BEGIN OF TY_TAB13.
        INCLUDE STRUCTURE ZDQ_MM_084_ALV13.
TYPES:END OF TY_TAB13.

TYPES:BEGIN OF TY_MAST,
        MATNR TYPE MAST-MATNR, "物料
        WERKS TYPE MAST-WERKS, "工厂
        STLAN TYPE MAST-STLAN, "BOM 类别
        STLNR TYPE MAST-STLNR, "物料单
      END OF TY_MAST.

TYPES:BEGIN OF TY_PRST,
        PSPNR TYPE PRST-PSPNR, "WBS
        MATNR TYPE PRST-MATNR, "物料
        WERKS TYPE PRST-WERKS, "工厂
        STLAN TYPE PRST-STLAN, "BOM 类别
        STLNR TYPE PRST-STLNR, "物料单
      END OF TY_PRST.

TYPES:BEGIN OF TY_STLNR,
        STLTY TYPE STPO-STLTY, "BOM 类别
        STLNR TYPE STPO-STLNR, "物料单
      END OF TY_STLNR.

*  BOM项目
TYPES:BEGIN OF TY_STPO,
        STLTY TYPE STPO-STLTY, "BOM 类别
        STLNR TYPE STPO-STLNR, "物料单
        STLKN TYPE STPO-STLKN, "BOM 项目节点号
        STPOZ TYPE STPO-STPOZ, "内部计数器
        IDNRK TYPE STPO-IDNRK, "组件
        MEINS TYPE STPO-MEINS,
      END OF TY_STPO.

TYPES:BEGIN OF TY_MATNR,
        MATNR TYPE MARA-MATNR,
      END OF TY_MATNR.

*物料号
TYPES:BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,
        MEINS TYPE MARA-MEINS,
        ZWLMS TYPE MAKT-MAKTX, "物料长描述
      END OF TY_MARA.

TYPES:BEGIN OF TY_MARV,
        BUKRS TYPE MARV-BUKRS , "公司代码
        VMGJA TYPE MARV-VMGJA , "上期的会计年度
        VMMON TYPE MARV-VMMON , "前期的月份
      END OF TY_MARV.

TYPES: BEGIN OF TY_MBEW,
         MATNR TYPE MBEW-MATNR,
         BWKEY TYPE MBEW-BWKEY,
         BWTAR TYPE MBEW-BWTAR,
         STPRS TYPE MBEW-STPRS,
         PEINH TYPE MBEW-PEINH,
       END OF TY_MBEW.
*&---------------------------------------------------------------------*
*& 内表定义
*&---------------------------------------------------------------------*
DATA: IT_TAB1  TYPE STANDARD TABLE OF TY_TAB1,
      IT_TAB2  TYPE STANDARD TABLE OF TY_TAB2,
      IT_TAB3  TYPE STANDARD TABLE OF TY_TAB3,
      IT_TAB4  TYPE STANDARD TABLE OF TY_TAB4,
      IT_TAB5  TYPE STANDARD TABLE OF TY_TAB5,
      IT_TAB6  TYPE STANDARD TABLE OF TY_TAB6,
      IT_TAB7  TYPE STANDARD TABLE OF TY_TAB7,
      IT_TAB8  TYPE STANDARD TABLE OF TY_TAB8,
      IT_TAB9  TYPE STANDARD TABLE OF TY_TAB9,
      IT_TAB10 TYPE STANDARD TABLE OF TY_TAB10,
      IT_TAB11 TYPE STANDARD TABLE OF TY_TAB11,
      IT_TAB12 TYPE STANDARD TABLE OF TY_TAB12,
      IT_TAB13 TYPE STANDARD TABLE OF TY_TAB13.


DATA: IT_PRST  TYPE STANDARD TABLE OF TY_PRST,
      IT_MAST  TYPE STANDARD TABLE OF TY_MAST,
      IT_STPO  TYPE STANDARD TABLE OF TY_STPO,
      IT_STLNR TYPE STANDARD TABLE OF TY_STLNR,
      IT_MATNR TYPE STANDARD TABLE OF TY_MATNR,
      IT_MARA  TYPE STANDARD TABLE OF TY_MARA,
      IT_MAKT  TYPE STANDARD TABLE OF MAKT,
      IT_MBEW  TYPE STANDARD TABLE OF TY_MBEW,
      IT_MARDH TYPE STANDARD TABLE OF MARDH.

*&---------------------------------------------------------------------*
*& 工作区
*&---------------------------------------------------------------------*
DATA:
  WA_TAB1  TYPE TY_TAB1,
  WA_TAB2  TYPE TY_TAB2,
  WA_TAB3  TYPE TY_TAB3,
  WA_TAB4  TYPE TY_TAB4,
  WA_TAB5  TYPE TY_TAB5,
  WA_TAB6  TYPE TY_TAB6,
  WA_TAB7  TYPE TY_TAB7,
  WA_TAB8  TYPE TY_TAB8,
  WA_TAB9  TYPE TY_TAB9,
  WA_TAB10 TYPE TY_TAB10,
  WA_TAB11 TYPE TY_TAB11,
  WA_TAB12 TYPE TY_TAB12,
  WA_TAB13 TYPE TY_TAB13.

DATA:
  WA_PRST  TYPE TY_PRST,
  WA_MAST  TYPE TY_MAST,
  WA_STPO  TYPE TY_STPO,
  WA_STLNR TYPE TY_STLNR,
  WA_MATNR TYPE TY_MATNR,
  WA_MARV  TYPE TY_MARV,
  WA_MBEW  TYPE TY_MBEW,
  WA_MARDH TYPE MARDH,
  WA_MARA  TYPE TY_MARA,
  WA_MAKT  TYPE MAKT.

*&  begin of alv
TYPE-POOLS: SLIS.

DATA:
  GT_FIELDCATALOG TYPE LVC_T_FCAT,
  GW_FIELDCATALOG TYPE LVC_S_FCAT,

  GT_EVENTS       TYPE SLIS_T_EVENT,
  GW_EVENTS       TYPE SLIS_ALV_EVENT,

  GW_LAYOUT       TYPE LVC_S_LAYO,

  GT_EXCLUDE      TYPE SLIS_T_EXTAB,
*      IT_EXCLUDE           TYPE SLIS_EXTAB,

  G_REPID         TYPE SY-REPID VALUE SY-REPID,
  G_REF_GRID      TYPE REF TO CL_GUI_ALV_GRID.
*----------------------------------------------------------------------*
* 选择屏幕定义
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001 .
SELECT-OPTIONS:S_MATNR FOR MARC-MATNR,
               S_WERKS FOR MARC-WERKS.
PARAMETERS:
  P1  RADIOBUTTON GROUP GR1 ,            "作为BOM父件
  P2  RADIOBUTTON GROUP GR1 ,            "作为BOM子件
  P3  RADIOBUTTON GROUP GR1 ,            "工艺路线
  P4  RADIOBUTTON GROUP GR1 ,            "序列号参数文件
  P5  RADIOBUTTON GROUP GR1 ,            "货盘化数据
  P6  RADIOBUTTON GROUP GR1 ,            "库存
  P7  RADIOBUTTON GROUP GR1 ,            "采购申请
  P8  RADIOBUTTON GROUP GR1 ,            "采购订单
  P9  RADIOBUTTON GROUP GR1 ,            "预留
  P10 RADIOBUTTON GROUP GR1 ,            "生产订单
  P11 RADIOBUTTON GROUP GR1 ,            "销售订单
  P12 RADIOBUTTON GROUP GR1 ,            "成本核算数据
  P13 RADIOBUTTON GROUP GR1 .            "计划订单
SELECTION-SCREEN END OF BLOCK BLK1.

INITIALIZATION.

  PERFORM INIT_DATA.

START-OF-SELECTION.

END-OF-SELECTION.


  PERFORM GET_DATA.

  PERFORM FRM_PLATFORM_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       取上期会计年度和期间
*----------------------------------------------------------------------*
FORM INIT_DATA.
  SELECT SINGLE
    BUKRS
    VMGJA
    VMMON
    INTO WA_MARV
    FROM MARV
    WHERE BUKRS = '8300'
    .
ENDFORM.                    "init_data
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.

  IF P1 = 'X'.
    SELECT
      PSPNR
      MATNR
      WERKS
      STLAN
      STLNR
      INTO TABLE IT_PRST
      FROM PRST
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      .

    SELECT
      MATNR
      WERKS
      STLAN
      STLNR
      INTO TABLE IT_MAST
      FROM MAST
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      .

    LOOP AT IT_PRST INTO WA_PRST.
      CLEAR WA_TAB1.
      MOVE-CORRESPONDING WA_PRST TO WA_TAB1.
      APPEND WA_TAB1 TO IT_TAB1.
    ENDLOOP.

    LOOP AT IT_MAST INTO WA_MAST.
      CLEAR WA_TAB1.
      MOVE-CORRESPONDING WA_MAST TO WA_TAB1.
      APPEND WA_TAB1 TO IT_TAB1.
    ENDLOOP.

    IF IT_TAB1[] IS NOT INITIAL.
      SELECT
        MATNR
        MEINS
*        ZWLMS
        INTO CORRESPONDING FIELDS OF TABLE IT_MARA
        FROM MARA
        FOR ALL ENTRIES IN IT_TAB1
        WHERE MATNR = IT_TAB1-MATNR
        .
      SORT IT_MARA BY MATNR.

      SELECT *
       INTO CORRESPONDING FIELDS OF TABLE IT_MAKT
       FROM MAKT
        FOR ALL ENTRIES IN IT_MARA
        WHERE MATNR = IT_MARA-MATNR.

    ENDIF.

    LOOP AT IT_TAB1 INTO WA_TAB1.
      READ TABLE IT_MARA INTO WA_MARA WITH KEY
      MATNR = WA_TAB1-MATNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_TAB1-MEINS = WA_MARA-MEINS.
        READ TABLE IT_MAKT INTO WA_MAKT
        WITH KEY  MATNR = WA_TAB1-MATNR
                  SPRAS = SY-LANGU.
        IF SY-SUBRC = 0.
          WA_TAB1-ZWLMS = WA_MAKT-MAKTX.
        ENDIF.
        MODIFY IT_TAB1 FROM WA_TAB1.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF P2 = 'X'.

    SELECT
      PSPNR
      MATNR
      WERKS
      STLAN
      STLNR
      INTO TABLE IT_PRST
      FROM PRST
      WHERE WERKS IN S_WERKS
      .
    SORT IT_PRST BY STLNR.

    SELECT
      MATNR
      WERKS
      STLAN
      STLNR
      INTO TABLE IT_MAST
      FROM MAST
      WHERE WERKS IN S_WERKS
      .
    SORT IT_MAST BY STLNR.

    LOOP AT IT_PRST INTO WA_PRST.
      CLEAR WA_STLNR.
      WA_STLNR-STLTY = 'P'.
      WA_STLNR-STLNR = WA_PRST-STLNR.
      APPEND WA_STLNR TO IT_STLNR.
    ENDLOOP.

    LOOP AT IT_MAST INTO WA_MAST.
      CLEAR WA_STLNR.
      WA_STLNR-STLTY = 'M'.
      WA_STLNR-STLNR = WA_MAST-STLNR.
      APPEND WA_STLNR TO IT_STLNR.
    ENDLOOP.

    IF IT_STLNR[] IS NOT INITIAL.
      SELECT
        STLTY
        STLNR
        STLKN
        STPOZ
        IDNRK
        MEINS
        INTO CORRESPONDING FIELDS OF TABLE IT_STPO
        FROM STPO
        FOR ALL ENTRIES IN IT_STLNR
        WHERE STLTY = IT_STLNR-STLTY
        AND   STLNR = IT_STLNR-STLNR
        .
    ENDIF.

    IF S_MATNR[] IS NOT INITIAL.
      DELETE IT_STPO WHERE IDNRK NOT IN S_MATNR.
    ENDIF.

    LOOP AT IT_STPO INTO WA_STPO.
      CLEAR  WA_TAB2.
      WA_TAB2-STLTY     = WA_STPO-STLTY.
      WA_TAB2-STLNR     = WA_STPO-STLNR.
      WA_TAB2-ZJ_MATNR  = WA_STPO-IDNRK."子件物料
      WA_TAB2-ZJFH_MEINS = WA_STPO-MEINS."子件发货单位

      IF WA_STPO-STLTY = 'P'.
        READ TABLE IT_PRST INTO WA_PRST WITH KEY
        STLNR = WA_STPO-STLNR
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_TAB2-FJ_MATNR = WA_PRST-MATNR."父件物料
          WA_TAB2-PSPNR    = WA_PRST-PSPNR.
          WA_TAB2-WERKS    = WA_PRST-WERKS."工厂
        ENDIF.
      ENDIF.

      IF WA_STPO-STLTY = 'M'.
        READ TABLE IT_MAST INTO WA_MAST WITH KEY
        STLNR = WA_STPO-STLNR
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_TAB2-FJ_MATNR = WA_MAST-MATNR."父件物料
          WA_TAB2-WERKS    = WA_MAST-WERKS."工厂
        ENDIF.
      ENDIF.

      CLEAR WA_MATNR.
      WA_MATNR-MATNR = WA_TAB2-FJ_MATNR.
      APPEND WA_MATNR TO IT_MATNR.

      CLEAR WA_MATNR.
      WA_MATNR-MATNR = WA_TAB2-ZJ_MATNR.
      APPEND WA_MATNR TO IT_MATNR.

      APPEND WA_TAB2 TO IT_TAB2.
    ENDLOOP.

    IF IT_MATNR[] IS NOT INITIAL.
      SELECT
        MATNR
        MEINS
*        ZWLMS
        INTO CORRESPONDING FIELDS OF TABLE IT_MARA
        FROM MARA
        FOR ALL ENTRIES IN IT_MATNR
        WHERE MATNR = IT_MATNR-MATNR
        .
      SORT IT_MARA BY MATNR.

      SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_MAKT
      FROM MAKT
      FOR ALL ENTRIES IN IT_MARA
      WHERE MATNR = IT_MARA-MATNR.

    ENDIF.

    LOOP AT IT_TAB2 INTO WA_TAB2.

      READ TABLE IT_MARA INTO WA_MARA WITH KEY
      MATNR = WA_TAB2-FJ_MATNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_TAB2-FJ_MEINS = WA_MARA-MEINS.
        READ TABLE IT_MAKT INTO WA_MAKT
       WITH KEY  MATNR = WA_TAB2-FJ_MATNR
                 SPRAS = SY-LANGU.
        IF SY-SUBRC = 0.
          WA_TAB2-FJ_WLMS = WA_MAKT-MAKTX.
        ENDIF.
      ENDIF.

      READ TABLE IT_MARA INTO WA_MARA WITH KEY
      MATNR = WA_TAB2-ZJ_MATNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_TAB2-ZJ_MEINS = WA_MARA-MEINS.
        READ TABLE IT_MAKT INTO WA_MAKT
      WITH KEY  MATNR = WA_TAB2-ZJ_MATNR
                SPRAS = SY-LANGU.
        IF SY-SUBRC = 0.
          WA_TAB2-ZJ_WLMS = WA_MAKT-MAKTX.
        ENDIF.
      ENDIF.

      MODIFY IT_TAB2 FROM WA_TAB2.
    ENDLOOP.

  ENDIF.

  IF P3 = 'X'.
    SELECT
      MAPL~PLNTY
      MAPL~PLNNR
      MAPL~PLNAL
      MAPL~MATNR
      MAPL~WERKS
      PLKO~KTEXT
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB3
      FROM MAPL
      INNER JOIN PLKO
      ON  MAPL~PLNTY = PLKO~PLNTY
      AND MAPL~PLNNR = PLKO~PLNNR
      AND MAPL~PLNAL = PLKO~PLNAL
      WHERE MATNR IN S_MATNR
      AND   MAPL~WERKS IN S_WERKS
*      AND   mapl~loekz = ''
*      AND   plko~loekz = ''
      .
    .
  ENDIF.


  IF P4 = 'X'.
    SELECT
      MATNR
      WERKS
      SERNP
      INTO CORRESPONDING FIELDS OF TABLE  IT_TAB4
      FROM MARC
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   SERNP <> ''
      .
  ENDIF.

  IF P5 = 'X'.
    SELECT
      MATNR
      LGNUM
      LHMG1
      LHME1
      LETY1
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB5
      FROM MLGN
      WHERE MATNR IN S_MATNR
      AND
      (  LHMG1 <> ''
      OR LHME1 <> ''
      OR LETY1 <> ''
      )
      .

  ENDIF.

  IF P6 = 'X'.
*    取库存
    SELECT
      MATNR
      WERKS
      LGORT
      LVORM
      LFGJA
      LFMON
      SPERR
      LABST
      UMLME
      INSME
      EINME
      SPEME
      RETME
      FROM MARD
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB6
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      .
    IF IT_TAB6[] IS NOT INITIAL.
*      取上期库存
      SELECT
        *
        FROM MARDH
        INTO TABLE IT_MARDH
        FOR ALL ENTRIES IN IT_TAB6
        WHERE MATNR = IT_TAB6-MATNR
        AND   WERKS = IT_TAB6-WERKS
        AND   LGORT = IT_TAB6-LGORT
        AND   LFGJA = WA_MARV-VMGJA
        AND   LFMON = WA_MARV-VMMON
      .
      SORT IT_TAB6 BY MATNR WERKS LGORT.


*    取价格
      SELECT
        MATNR
        BWKEY
        BWTAR
        STPRS
        PEINH
        INTO CORRESPONDING FIELDS OF TABLE IT_MBEW
        FROM MBEW
        FOR ALL ENTRIES IN IT_TAB6
        WHERE MATNR = IT_TAB6-MATNR
          AND BWKEY = IT_TAB6-WERKS
        .
      SORT IT_MBEW BY MATNR BWKEY.

    ENDIF.

    LOOP AT IT_TAB6 INTO WA_TAB6.
      WA_TAB6-VMGJA = WA_MARV-VMGJA.
      WA_TAB6-VMMON = WA_MARV-VMMON.
      READ TABLE IT_MARDH INTO WA_MARDH WITH KEY
      MATNR = WA_TAB6-MATNR
      WERKS = WA_TAB6-WERKS
      LGORT = WA_TAB6-LGORT
      BINARY SEARCH
      .
      IF SY-SUBRC = 0.
        WA_TAB6-LABST1 = WA_MARDH-LABST.
        WA_TAB6-UMLME1 = WA_MARDH-UMLME.
        WA_TAB6-INSME1 = WA_MARDH-INSME.
        WA_TAB6-EINME1 = WA_MARDH-EINME.
        WA_TAB6-SPEME1 = WA_MARDH-SPEME.
        WA_TAB6-RETME1 = WA_MARDH-RETME.
      ENDIF.

      READ TABLE IT_MBEW INTO WA_MBEW WITH KEY
      MATNR = WA_TAB6-MATNR
      BWKEY = WA_TAB6-WERKS
      BINARY SEARCH
      .
      IF SY-SUBRC = 0.
        WA_TAB6-STPRS = WA_MBEW-STPRS .
        IF WA_MBEW-PEINH <> 0 .
          WA_TAB6-STPRS = WA_MBEW-STPRS / WA_MBEW-PEINH.
        ENDIF.
      ENDIF.

      MODIFY IT_TAB6 FROM WA_TAB6.
    ENDLOOP.

    DELETE IT_TAB6 WHERE LABST = 0
                     AND UMLME = 0
                     AND INSME = 0
                     AND EINME = 0
                     AND SPEME = 0
                     AND RETME = 0
*                     AND vmlab = 0
                     AND LABST1 = 0
                     AND UMLME1 = 0
                     AND INSME1 = 0
                     AND EINME1 = 0
                     AND SPEME1 = 0
                     AND RETME1 = 0
                     .
  ENDIF.


  IF P7 = 'X'.
*    取采购申请
    SELECT
      BANFN
      BNFPO
      BSART
      BSTYP
      MATNR
      WERKS
      ERNAM
      ERDAT
      STATU
      ESTKZ
      FROM EBAN
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB7
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   LOEKZ <> 'X'
      .

  ENDIF.

  IF P8 = 'X'.
*    取采购订单
    SELECT
      EKPO~EBELN
      EBELP
      MATNR
      WERKS
      MEINS
      EKKO~AEDAT
      ERNAM

      INTO CORRESPONDING FIELDS OF TABLE IT_TAB8
      FROM EKPO
      INNER JOIN EKKO
      ON EKPO~EBELN = EKKO~EBELN
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   EKPO~LOEKZ = ''
      AND   EKKO~LOEKZ = ''
      .
  ENDIF.

  IF P9 = 'X'.
*    取预留
    SELECT
      RSNUM
      RSPOS
      MATNR
      WERKS
      PLNUM
      AUFNR
      BAUGR
      POSNR
      BDMNG
      ENMNG
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB9
      FROM RESB
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   XLOEK = ''
      .

  ENDIF.

  IF P10 = 'X'.
*    取生产订单
    SELECT
      AFPO~AUFNR
      AFPO~POSNR
      AFPO~MATNR
      AFPO~DWERK
      AUFK~ERNAM
      AUFK~ERDAT
      FROM AFPO
      INNER JOIN AUFK
      ON AUFK~AUFNR = AFPO~AUFNR
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB10
      WHERE MATNR IN S_MATNR
      AND   DWERK IN S_WERKS
      .
  ENDIF.

  IF P11 = 'X'.
*    取销售订单
    SELECT
      VBELN
      POSNR
      MATNR
      WERKS
      ERNAM
      ERDAT
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB11
      FROM VBAP
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      .

  ENDIF.

  IF P12 = 'X'.
    SELECT
      KALNR
      KALKA
      KADKY
      BWVAR
      MATNR
      WERKS
      BWKEY
      FROM KEKO
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB12
      WHERE MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      .
  ENDIF.

  IF P13 = 'X'.
    SELECT
       PLNUM
       MATNR
       PLWRK
       PWWRK
      INTO CORRESPONDING FIELDS OF TABLE IT_TAB13
      FROM PLAF
      WHERE MATNR IN S_MATNR
      AND   PWWRK IN S_WERKS
     .
  ENDIF.

ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  frm_platform_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_DISPLAY .
  PERFORM PRE_LAYOUT.
  IF P1 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV1'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB1.
  ENDIF.
  IF P2 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV2'.
    PERFORM FRM_PLATFORM_OUTPUT2.
  ENDIF.
  IF P3 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV3'.
    PERFORM FRM_PLATFORM_OUTPUT3.
  ENDIF.
  IF P4 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV4'.
    PERFORM FRM_PLATFORM_OUTPUT4.
  ENDIF.
  IF P5 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV5'.
    PERFORM FRM_PLATFORM_OUTPUT5.
  ENDIF.

  IF P6 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV6'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB6.
  ENDIF.
  IF P7 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV7'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB7.
  ENDIF.
  IF P8 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV8'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB8.
  ENDIF.
  IF P9 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV9'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB9.
  ENDIF.
  IF P10 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV10'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB10.
  ENDIF.
  IF P11 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV11'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB11.
  ENDIF.
  IF P12 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV12'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB12.
  ENDIF.
  IF P13 = 'X'.
    PERFORM PRE_FIELDCAT USING 'ZDQ_MM_084_ALV13'.
    PERFORM FRM_PLATFORM_OUTPUT USING IT_TAB13.
  ENDIF.

ENDFORM.                    "frm_platform_display

*&---------------------------------------------------------------------*
*&      Form  pre_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRE_LAYOUT .
*  GW_layout-EDIT = 'X'.
  GW_LAYOUT-ZEBRA = 'X'.
  GW_LAYOUT-SEL_MODE = 'A'.
*  gw_layout-no_rowmark = 'X'.
  GW_LAYOUT-CWIDTH_OPT = 'X'.
ENDFORM.                    "pre_layout

*&---------------------------------------------------------------------*
*&      Form  pre_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRE_FIELDCAT USING P_TABNAME.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = P_TABNAME
    CHANGING
      CT_FIELDCAT      = GT_FIELDCATALOG[].
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "pre_fieldcat

*&---------------------------------------------------------------------*
*&      Form  frm_platform_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_TAB     text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT USING PT_TAB TYPE STANDARD TABLE .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = PT_TAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output
*&---------------------------------------------------------------------*
*&      Form  frm_platform_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT1.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = IT_TAB1
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output
*&---------------------------------------------------------------------*
*&      Form  frm_platform_output2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT2.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = IT_TAB2
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output2
*&---------------------------------------------------------------------*
*&      Form  frm_platform_output3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT3.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = IT_TAB3
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output3
*&---------------------------------------------------------------------*
*&      Form  frm_platform_output4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT4.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = IT_TAB4
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output4
*&---------------------------------------------------------------------*
*&      Form  frm_platform_output5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PLATFORM_OUTPUT5.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
      IS_LAYOUT_LVC      = GW_LAYOUT
      IT_FIELDCAT_LVC    = GT_FIELDCATALOG
      I_SAVE             = 'X'
    TABLES
      T_OUTTAB           = IT_TAB5
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "frm_platform_output5

*Text elements
*----------------------------------------------------------
* 001 选择条件


*Selection texts
*----------------------------------------------------------
* P1         作为BOM父件
* P10         生产订单
* P11         销售订单
* P12         成本核算数据
* P13         计划订单
* P2         作为BOM子件
* P3         工艺路线
* P4         序列号参数文件
* P5         货盘化数据
* P6         本期及上期非特殊库存
* P7         采购申请
* P8         采购订单
* P9         预留
* S_MATNR D       .
* S_WERKS D       .
