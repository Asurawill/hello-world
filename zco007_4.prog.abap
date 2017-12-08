*&---------------------------------------------------------------------*
*& Report  ZFI004
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/8/3
*& Request       :
*& Descriptions  : 公司间采购成本&收入报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZCO007_4.

************************************************************************
* Tables
************************************************************************
TABLES:BKPF,BSEG.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        WERKS TYPE BSEG-WERKS,
        MATNR TYPE BSEG-MATNR,
        GJAHR TYPE BKPF-GJAHR,
        MONAT TYPE BKPF-MONAT,
        CBCY  TYPE MLCD-ESTPRD, "期间成本差异
        EBELN TYPE BSEG-EBELN,
        EBELP TYPE BSEG-EBELP,
        MAKTX TYPE MAKT-MAKTX,
        VBEL2 TYPE BSEG-VBEL2,
        POSN2 TYPE BSEG-POSN2,
        MENGE TYPE BSEG-MENGE,
        DMBTR TYPE BSEG-DMBTR,
        KALNR TYPE MLCD-KALNR,  "成本核算号
      END OF TY_DATA.

TYPES:BEGIN OF TY_WL,
        EBELN TYPE BSEG-EBELN,
        EBELP TYPE BSEG-EBELP,
        MATNR TYPE BSEG-MATNR,
        WERKS TYPE BSEG-WERKS,
        GJAHR TYPE BKPF-GJAHR,
        MONAT TYPE BKPF-MONAT,
        MENGE TYPE BSEG-MENGE,
        DMBTR TYPE BSEG-DMBTR,
      END OF TY_WL.

TYPES:BEGIN OF TY_WL1,
        MATNR TYPE BSEG-MATNR,
        WERKS TYPE BSEG-WERKS,
        MONAT TYPE BKPF-MONAT,
        GJAHR TYPE BKPF-GJAHR,
        MENGE TYPE BSEG-MENGE,
        DMBTR TYPE BSEG-DMBTR,
      END OF TY_WL1.

TYPES:BEGIN OF TY_WL2,
        MATNR TYPE BSEG-MATNR,
        WERKS TYPE BSEG-WERKS,
        MONAT TYPE BKPF-MONAT,
        GJAHR TYPE BKPF-GJAHR,
        VBEL2 TYPE BSEG-VBEL2,
        POSN2 TYPE BSEG-POSN2,
        MENGE TYPE BSEG-MENGE,
        DMBTR TYPE BSEG-DMBTR,
      END OF TY_WL2.

TYPES:BEGIN OF TY_ML,
        WERKS TYPE BSEG-WERKS,
        GJAHR TYPE BKPF-GJAHR,
        MONAT TYPE BKPF-MONAT,
        MATNR TYPE BSEG-MATNR,
        DMBTR TYPE MLCD-ESTPRD,
      END OF TY_ML.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.
DATA GS_DATA_1 TYPE TY_DATA.

DATA GT_BKPF   TYPE TABLE OF BKPF.
DATA GS_BKPF   TYPE BKPF.

DATA GT_BSEG   TYPE TABLE OF BSEG.
DATA GS_BSEG   TYPE BSEG.

DATA GT_BKPF_1   TYPE TABLE OF BKPF.
DATA GS_BKPF_1   TYPE BKPF.

DATA GT_BSEG_1   TYPE TABLE OF BSEG.
DATA GS_BSEG_1   TYPE BSEG.

DATA GT_WL       TYPE TABLE OF TY_WL.
DATA GS_WL       TYPE TY_WL.

DATA GT_WL1      TYPE TABLE OF TY_WL1.
DATA GS_WL1      TYPE TY_WL1.

DATA GT_WL2      TYPE TABLE OF TY_WL2.
DATA GS_WL2      TYPE TY_WL2.

DATA GT_ML       TYPE TABLE OF TY_ML.
DATA GS_ML       TYPE TY_ML.

DATA GT_ML1      TYPE TABLE OF TY_ML.
DATA GS_ML1      TYPE TY_ML.

DATA GT_ML2      TYPE TABLE OF TY_ML.
DATA GS_ML2      TYPE TY_ML.


DATA GT_MAKT   TYPE TABLE OF MAKT.
DATA GS_MAKT   TYPE MAKT.

DATA GT_CKMLHD TYPE TABLE OF CKMLHD.
DATA GS_CKMLHD TYPE CKMLHD.

DATA GT_MLCD   TYPE TABLE OF MLCD.
DATA GS_MLCD   TYPE MLCD.

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
SELECT-OPTIONS: S_BUKRS  FOR BKPF-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,                      "公司代码
                S_GJAHR  FOR BKPF-GJAHR NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM+0(4),"会计年度
                S_MONAT  FOR BKPF-MONAT NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM+4(2)."记账期间
*                S_EBELN  FOR BSEG-EBELN,                                                          "采购订单号
*                S_EBELP  FOR BSEG-EBELP.
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

  SELECT * FROM BKPF
  INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
  WHERE BUKRS IN S_BUKRS
  AND   GJAHR IN S_GJAHR
  AND   MONAT IN S_MONAT
  AND   BLART = 'WL'.

  CHECK GT_BKPF IS NOT INITIAL.

  SELECT * FROM BSEG
  INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
  FOR ALL ENTRIES IN GT_BKPF
  WHERE GJAHR = GT_BKPF-GJAHR
  AND   BUKRS = GT_BKPF-BUKRS
  AND   BELNR = GT_BKPF-BELNR
  AND   HKONT BETWEEN '6401020000' AND '6401029999'
  AND  ( BSCHL <> '86'
  AND   BSCHL <> '96' ).
*  AND   EBELN IN S_EBELN
*  AND   EBELP IN S_EBELP.
*  AND   MATNR = '192PGF37C00F01'.

  CHECK GT_BSEG IS NOT INITIAL .

  SELECT * FROM BKPF
  INTO CORRESPONDING FIELDS OF TABLE GT_BKPF_1
  WHERE BUKRS IN S_BUKRS
  AND   GJAHR IN S_GJAHR
  AND   MONAT IN S_MONAT
  AND   BLART = 'ML'
  AND   AWTYP = 'MLHD'.

  CHECK GT_BKPF_1 IS NOT INITIAL.

  SELECT * FROM BSEG
  INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_1
  FOR ALL ENTRIES IN GT_BKPF_1
  WHERE GJAHR = GT_BKPF_1-GJAHR
  AND   BUKRS = GT_BKPF_1-BUKRS
  AND   BELNR = GT_BKPF_1-BELNR
  AND   HKONT BETWEEN '6401020000' AND '6401029999'.
*  AND   MATNR = '192PGF37C00F01'.

  CHECK GT_BSEG IS NOT INITIAL.

  SELECT * FROM MAKT
  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FOR ALL ENTRIES IN GT_BSEG
  WHERE MATNR = GT_BSEG-MATNR AND SPRAS = SY-LANGU.

*查询成本估算号
  SELECT * FROM CKMLHD
  INTO CORRESPONDING FIELDS OF TABLE GT_CKMLHD
  FOR ALL ENTRIES IN GT_BSEG
  WHERE BWKEY = GT_BSEG-WERKS
  AND   MATNR = GT_BSEG-MATNR
  AND   VBELN = GT_BSEG-VBEL2
  AND   POSNR = GT_BSEG-POSN2.

  SORT GT_CKMLHD BY BWKEY MATNR VBELN POSNR.

*查询差异
  SELECT * FROM MLCD
  INTO CORRESPONDING FIELDS OF TABLE GT_MLCD
  FOR ALL ENTRIES IN GT_BSEG
  WHERE BDATJ = GT_BSEG-GJAHR
  AND   POPER IN S_MONAT.

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

*汇总按单数据
  DATA L_MENGE    TYPE BSEG-MENGE.
  DATA L_ADCY     TYPE MLCD-ESTPRD.

  SORT GT_BSEG BY EBELN EBELP GJAHR BELNR.

*汇总交货数量，金额
  LOOP AT GT_BSEG INTO GS_BSEG.
    GS_WL-EBELN =  GS_BSEG-EBELN.
    GS_WL-EBELP =  GS_BSEG-EBELP.
    GS_WL-WERKS =  GS_BSEG-WERKS.
    GS_WL-MATNR =  GS_BSEG-MATNR.
*会计年度，期间
    READ TABLE GT_BKPF INTO GS_BKPF
   WITH KEY GJAHR = GS_BSEG-GJAHR
            BUKRS = GS_BSEG-BUKRS
            BELNR = GS_BSEG-BELNR.
    IF SY-SUBRC = 0.
      GS_WL-GJAHR =  GS_BKPF-GJAHR.
      GS_WL-MONAT =  GS_BKPF-MONAT.
    ENDIF.

    IF GS_BSEG-SHKZG = 'H'.
      GS_WL-MENGE = GS_BSEG-MENGE * -1.
      GS_WL-DMBTR =  GS_BSEG-DMBTR * -1.
    ELSE.
      GS_WL-MENGE =  GS_BSEG-MENGE.
      GS_WL-DMBTR =  GS_BSEG-DMBTR.
    ENDIF.

    COLLECT GS_WL INTO GT_WL.
    CLEAR GS_WL.
  ENDLOOP.

*计算按库数量
  LOOP AT GT_BSEG INTO GS_BSEG
  WHERE VBEL2 IS INITIAL.

    GS_WL1-WERKS =  GS_BSEG-WERKS.
    GS_WL1-MATNR =  GS_BSEG-MATNR.

    READ TABLE GT_BKPF INTO GS_BKPF
    WITH KEY GJAHR = GS_BSEG-GJAHR
             BUKRS = GS_BSEG-BUKRS
             BELNR = GS_BSEG-BELNR.
    IF SY-SUBRC = 0.
      GS_WL1-GJAHR =  GS_BKPF-GJAHR.
      GS_WL1-MONAT = GS_BKPF-MONAT.
    ENDIF.

    IF GS_BSEG-SHKZG = 'H'.
      GS_WL1-MENGE =  GS_BSEG-MENGE * -1.
      GS_WL1-DMBTR =  GS_BSEG-DMBTR * -1.
    ELSE.
      GS_WL1-MENGE =  GS_BSEG-MENGE.
      GS_WL1-DMBTR =  GS_BSEG-DMBTR.
    ENDIF.
    COLLECT GS_WL1 INTO GT_WL1.
    CLEAR GS_WL1.
  ENDLOOP.

*计算按单数量
  LOOP AT GT_BSEG INTO GS_BSEG
  WHERE VBEL2 IS NOT INITIAL.
    GS_WL2-WERKS =  GS_BSEG-WERKS.
    GS_WL2-MATNR =  GS_BSEG-MATNR.
    GS_WL2-VBEL2 =  GS_BSEG-VBEL2.
    GS_WL2-POSN2 =  GS_BSEG-POSN2.

    READ TABLE GT_BKPF INTO GS_BKPF
    WITH KEY GJAHR = GS_BSEG-GJAHR
             BUKRS = GS_BSEG-BUKRS
             BELNR = GS_BSEG-BELNR.
    IF SY-SUBRC = 0.
      GS_WL2-GJAHR =  GS_BKPF-GJAHR.
      GS_WL2-MONAT = GS_BKPF-MONAT.
    ENDIF.

    IF GS_BSEG-SHKZG = 'H'.
      GS_WL2-MENGE =  GS_BSEG-MENGE * -1.
      GS_WL2-DMBTR =  GS_BSEG-DMBTR * -1.
    ELSE.
      GS_WL2-MENGE =  GS_BSEG-MENGE.
      GS_WL2-DMBTR =  GS_BSEG-DMBTR.
    ENDIF.
    COLLECT GS_WL2 INTO GT_WL2.
    CLEAR GS_WL2.
  ENDLOOP.

*计算物料总差异
  SORT GT_BSEG_1 BY GJAHR MATNR WERKS.
  LOOP AT GT_BSEG_1 INTO GS_BSEG_1.
    READ TABLE GT_BKPF_1 INTO GS_BKPF_1
    WITH KEY GJAHR = GS_BSEG_1-GJAHR
             BUKRS = GS_BSEG_1-BUKRS
             BELNR = GS_BSEG_1-BELNR.
    IF  SY-SUBRC = 0.
      GS_ML-GJAHR = GS_BKPF_1-GJAHR.
      GS_ML-MONAT = GS_BKPF_1-MONAT.
      GS_ML-MATNR = GS_BSEG_1-MATNR.
      GS_ML-WERKS = GS_BSEG_1-WERKS.

      IF GS_BSEG_1-SHKZG = 'H'.
        GS_ML-DMBTR =  GS_BSEG_1-DMBTR * -1.
      ELSE.
        GS_ML-DMBTR = GS_BSEG_1-DMBTR.
      ENDIF.

      COLLECT GS_ML INTO GT_ML.
      CLEAR GS_ML.
    ENDIF.
  ENDLOOP.

*计算物料按库总差异
  LOOP AT GT_BSEG_1 INTO GS_BSEG_1
  WHERE VBEL2 IS INITIAL.
    READ TABLE GT_BKPF_1 INTO GS_BKPF_1
    WITH KEY GJAHR = GS_BSEG_1-GJAHR
             BUKRS = GS_BSEG_1-BUKRS
             BELNR = GS_BSEG_1-BELNR.
    IF  SY-SUBRC = 0.
      GS_ML1-GJAHR = GS_BKPF_1-GJAHR.
      GS_ML1-MONAT = GS_BKPF_1-MONAT.
      GS_ML1-MATNR = GS_BSEG_1-MATNR.
      GS_ML1-WERKS = GS_BSEG_1-WERKS.

      IF GS_BSEG_1-SHKZG = 'H'.
        GS_ML1-DMBTR =  GS_BSEG_1-DMBTR * -1.
      ELSE.
        GS_ML1-DMBTR = GS_BSEG_1-DMBTR.
      ENDIF.

      COLLECT GS_ML1 INTO GT_ML1.
      CLEAR GS_ML1.
    ENDIF.
  ENDLOOP.

*统计按单差异汇总
  DATA L_MENGE1  TYPE BSEG-MENGE.
  DATA L_DMBTR1  TYPE BSEG-DMBTR.

*当销售订单不为空处理按单差异************************
*期间交货数量，成本
  LOOP AT GT_BSEG INTO GS_BSEG
  WHERE VBEL2 IS NOT INITIAL.
    CLEAR L_MENGE.

    GS_DATA-EBELN = GS_BSEG-EBELN.
    GS_DATA-EBELP = GS_BSEG-EBELP.
    GS_DATA-MATNR = GS_BSEG-MATNR.
    GS_DATA-WERKS = GS_BSEG-WERKS.
    GS_DATA-VBEL2 = GS_BSEG-VBEL2.
    GS_DATA-POSN2 = GS_BSEG-POSN2.

*期间交货数量
    IF GS_BSEG-SHKZG = 'H'.
      GS_DATA-MENGE =  GS_BSEG-MENGE * -1.
      GS_DATA-DMBTR =  GS_BSEG-DMBTR * -1.
    ELSE.
      GS_DATA-MENGE =  GS_BSEG-MENGE.
      GS_DATA-DMBTR =  GS_BSEG-DMBTR.
    ENDIF.

    READ TABLE GT_BKPF INTO GS_BKPF
    WITH KEY GJAHR = GS_BSEG-GJAHR
             BUKRS = GS_BSEG-BUKRS
             BELNR = GS_BSEG-BELNR.
    IF SY-SUBRC = 0.
      GS_DATA-MONAT = GS_BKPF-MONAT.
      GS_DATA-GJAHR = GS_BKPF-GJAHR.
    ENDIF.

*期间交货数量
    READ TABLE GT_WL INTO GS_WL
    WITH KEY EBELN = GS_DATA-EBELN
             EBELP = GS_DATA-EBELP
             MATNR = GS_DATA-MATNR
             GJAHR = GS_DATA-GJAHR
             MONAT = GS_DATA-MONAT.
    IF SY-SUBRC = 0.
      L_MENGE1 = GS_WL-MENGE.
*      GS_DATA-DMBTR = GS_WL-DMBTR.
    ENDIF.

*查询物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

*期间成本差异
    DATA L_MATNR TYPE BSEG-MATNR.
    CLEAR L_MATNR.

*销售订单不为空
*    IF GS_DATA-VBEL2 IS NOT INITIAL .
*按库交货数量不为0.
    READ TABLE GT_CKMLHD INTO GS_CKMLHD
    WITH KEY MATNR = GS_DATA-MATNR
             BWKEY = GS_DATA-WERKS
             VBELN = GS_DATA-VBEL2
             POSNR = GS_DATA-POSN2.
    IF SY-SUBRC = 0.
      READ TABLE GT_MLCD INTO GS_MLCD
      WITH KEY KALNR = GS_CKMLHD-KALNR
               BDATJ = GS_DATA-GJAHR
               POPER = GS_DATA-MONAT
               CATEG = 'VN'
               PTYP  = 'V+'
               CURTP = '10'
               LBKUM = L_MENGE1."GS_DATA-MENGE.
      IF SY-SUBRC = 0.
        GS_DATA-CBCY = GS_MLCD-ESTPRD + GS_MLCD-MSTPRD + GS_MLCD-ESTKDM + GS_MLCD-MSTKDM.
      ENDIF.

      READ TABLE GT_WL2 INTO GS_WL2
      WITH KEY MATNR = GS_DATA-MATNR
               WERKS = GS_DATA-WERKS
               VBEL2 = GS_DATA-VBEL2
               POSN2 = GS_DATA-POSN2.
      IF SY-SUBRC = 0.
        GS_DATA-CBCY = GS_DATA-CBCY * GS_DATA-MENGE / GS_WL2-MENGE.
      ENDIF.

    ENDIF.
    COLLECT GS_DATA INTO GT_DATA.
    CLEAR GS_DATA.
    CLEAR GS_BSEG.
  ENDLOOP.
*当销售订单不为空处理按单差异************************

*当销售订单为空处理按库差异************************
  LOOP AT GT_BSEG INTO GS_BSEG
  WHERE VBEL2 IS INITIAL .
    CLEAR L_MENGE.

    GS_DATA-EBELN = GS_BSEG-EBELN.
    GS_DATA-EBELP = GS_BSEG-EBELP.
    GS_DATA-MATNR = GS_BSEG-MATNR.
    GS_DATA-WERKS = GS_BSEG-WERKS.
    GS_DATA-VBEL2 = GS_BSEG-VBEL2.
    GS_DATA-POSN2 = GS_BSEG-POSN2.

*期间交货数量
    IF GS_BSEG-SHKZG = 'H'.
      GS_DATA-MENGE =  GS_BSEG-MENGE * -1.
      GS_DATA-DMBTR =  GS_BSEG-DMBTR * -1.
    ELSE.
      GS_DATA-MENGE =  GS_BSEG-MENGE.
      GS_DATA-DMBTR =  GS_BSEG-DMBTR.
    ENDIF.

    READ TABLE GT_BKPF INTO GS_BKPF
    WITH KEY GJAHR = GS_BSEG-GJAHR
             BUKRS = GS_BSEG-BUKRS
             BELNR = GS_BSEG-BELNR.
    IF SY-SUBRC = 0.
      GS_DATA-MONAT = GS_BKPF-MONAT.
      GS_DATA-GJAHR = GS_BKPF-GJAHR.
    ENDIF.

*期间交货数量
    READ TABLE GT_WL INTO GS_WL
    WITH KEY EBELN = GS_DATA-EBELN
             EBELP = GS_DATA-EBELP
             MATNR = GS_DATA-MATNR
             GJAHR = GS_DATA-GJAHR
             MONAT = GS_DATA-MONAT.
    IF SY-SUBRC = 0.
      L_MENGE1 = GS_WL-MENGE.
*      GS_DATA-DMBTR = GS_WL-DMBTR.
    ENDIF.

*查询物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

*期间成本差异
    CLEAR L_MATNR.

    READ TABLE GT_ML1 INTO GS_ML1
         WITH KEY GJAHR  = GS_DATA-GJAHR
                  MONAT  = GS_DATA-MONAT
                  MATNR  = GS_DATA-MATNR.
    IF SY-SUBRC = 0.

      READ TABLE GT_WL1 INTO GS_WL1
      WITH KEY GJAHR = GS_DATA-GJAHR
               MONAT = GS_DATA-MONAT
               MATNR = GS_DATA-MATNR.
      IF SY-SUBRC = 0.
        CLEAR L_DMBTR1.
        LOOP AT GT_DATA INTO GS_DATA_1
         WHERE VBEL2 IS NOT INITIAL
         AND   GJAHR  = GS_DATA-GJAHR
         AND   MONAT  = GS_DATA-MONAT
         AND   MATNR  = GS_DATA-MATNR.
          L_DMBTR1 = L_DMBTR1 + GS_DATA_1-CBCY.
          CLEAR GS_DATA_1.
        ENDLOOP.

        GS_DATA-CBCY = ( GS_ML1-DMBTR - L_DMBTR1 ) * GS_DATA-MENGE / GS_WL1-MENGE.
      ENDIF.
    ENDIF.

    COLLECT GS_DATA INTO GT_DATA.
    CLEAR GS_DATA.
    CLEAR GS_BSEG.
  ENDLOOP.
*当销售订单为空处理按库差异************************

**删除相同行
*  SORT GT_DATA BY EBELN  EBELP GJAHR MONAT MATNR .
*  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING  EBELN  EBELP GJAHR MONAT MATNR .

*汇总按单差异
  LOOP AT GT_DATA INTO GS_DATA
  WHERE VBEL2 IS NOT INITIAL .
    GS_ML2-WERKS = GS_DATA-WERKS.
    GS_ML2-GJAHR = GS_DATA-GJAHR.
    GS_ML2-MONAT = GS_DATA-MONAT.
    GS_ML2-MATNR = GS_DATA-MATNR.
    GS_ML2-DMBTR  = GS_DATA-CBCY.

    COLLECT GS_ML2 INTO GT_ML2.
    CLEAR GS_ML2.
  ENDLOOP.

  SORT GT_DATA BY WERKS GJAHR MONAT MATNR CBCY MENGE ASCENDING .

  FIELD-SYMBOLS <LW_DATA> TYPE TY_DATA.
*处理按库为0,差异
  LOOP AT GT_DATA ASSIGNING <LW_DATA>.
    IF GS_DATA-VBEL2 IS NOT INITIAL .
      READ TABLE GT_WL1 INTO GS_WL1
      WITH KEY MATNR = <LW_DATA>-MATNR
               GJAHR = <LW_DATA>-GJAHR
               MONAT = <LW_DATA>-MONAT.
      IF GS_WL1-MENGE = 0.
        CLEAR GS_ML.
        READ TABLE GT_ML INTO GS_ML
        WITH KEY GJAHR = <LW_DATA>-GJAHR
                 MONAT = <LW_DATA>-MONAT
                 MATNR = <LW_DATA>-MATNR.

        CLEAR GS_ML1.
        READ TABLE  GT_ML2 INTO GS_ML2
        WITH KEY GJAHR = <LW_DATA>-GJAHR
                 MONAT = <LW_DATA>-MONAT
                 MATNR = <LW_DATA>-MATNR.

        AT NEW MATNR.
          IF GS_ML-DMBTR - GS_ML2-DMBTR <> 0.
            <LW_DATA>-CBCY = <LW_DATA>-CBCY + ( GS_ML-DMBTR - GS_ML2-DMBTR ).
          ENDIF.
        ENDAT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA L_CYFT_ALL2  TYPE P DECIMALS 2.
*差异分摊到金额最大的一行
  SORT GT_DATA BY WERKS  GJAHR MONAT MATNR CBCY MENGE EBELN EBELP ASCENDING .
  LOOP AT GT_DATA ASSIGNING <LW_DATA>.
    AT NEW MATNR.
      L_CYFT_ALL2 = 0.
    ENDAT.
    L_CYFT_ALL2 = L_CYFT_ALL2 + <LW_DATA>-CBCY .
    AT END OF MATNR.
      CLEAR GS_ML.
      READ TABLE GT_ML INTO GS_ML
      WITH KEY MATNR = <LW_DATA>-MATNR
               WERKS = <LW_DATA>-WERKS
               GJAHR = <LW_DATA>-GJAHR
               MONAT = <LW_DATA>-MONAT.
      IF SY-SUBRC = 0.
        <LW_DATA>-CBCY = <LW_DATA>-CBCY + GS_ML-DMBTR - L_CYFT_ALL2.
      ENDIF.
    ENDAT.
  ENDLOOP.

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
  GW_LAYOUT-CWIDTH_OPT   = 'X'.
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

  INIT_FIELDCAT 'EBELN'          '采购订单'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELP'          '采购订单行号'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'          '记账期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料'             '' '' '' '' '' 'MARA' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'          '物料描述'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'          '销售订单号'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSN2'          '销售订单行号'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'          '交货工厂'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '期间交货数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'          '期间交货成本'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CBCY'           '期间成本差异'             '' '' '' '' '' '' ''.

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
      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
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

*  CASE R_UCOMM.
** 双击
*    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*      AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM TOP-OF-PAGE.

*ALV Header declarations
  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.
* Title
  WA_HEADER-TYP  = 'H'.
  WA_HEADER-INFO =  SY-TITLE."'装箱单维护打印平台'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* Date
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = '报表日期: '.
  CONCATENATE  S_GJAHR-LOW '年'
               S_MONAT-LOW+0(2) '月' INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "top-of-page
