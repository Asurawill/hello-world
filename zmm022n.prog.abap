REPORT ZMM022N.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/9
*& Request       :
*& Descriptions  : 采购申请导出审批列表
*&
*& Modify by     :IT02_魏云
*& Modify date   :20160506
*& Request       :
*& Descriptions  :优化运行速度、新增替代折算
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:EBAN,PRPS,PROJ,ZMM024.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        BANFN     TYPE EBKN-BANFN, "采购申请
        BNFPO     TYPE EBKN-BNFPO, "采购申请行项目
        FRGZU     TYPE EBAN-FRGZU, "批准状态
        FRGST     TYPE EBAN-FRGST, "审批策略
        BANPR     TYPE EBAN-BANPR, "申请处理状态
        CLZT      TYPE STRING,     "处理状态
        PSPID     TYPE PROJ-PSPID, "项目定义
        POST1     TYPE PROJ-POST1, "项目描述
        POSID    TYPE PRPS-POSID, "WBS元素
        WERKS     TYPE EBAN-WERKS, "工厂
        EKGRP     TYPE EBAN-EKGRP, "采购组
        AFNAM     TYPE EBAN-AFNAM, "申请者
        ERDAT     TYPE EBKN-ERDAT, "创建者
        MATNR     TYPE EBAN-MATNR, "物料编码
        TXZ01     TYPE EBAN-TXZ01, "物料描述
        MATKL     TYPE MARA-MATKL, "物料组
        MEINS     TYPE EBAN-MEINS, "计量单位
        YSS       TYPE EBAN-MENGE, "预算数
        WQRLJSGSL TYPE EBAN-MENGE, "未确认累计申购数量
        YQRLJSGSL TYPE EBAN-MENGE, "已去人累计申购数量
        SYKSQSL   TYPE EBAN-MENGE, "剩余可申购数量
        MENGE     TYPE EBAN-MENGE, "本次申购数量
        PLIFZ     TYPE EBAN-PLIFZ, "计划交货天数
        BADAT     TYPE EBAN-BADAT, "需求（请求）日期
        LFDAT     TYPE EBAN-LFDAT, "项目交货日期
        CE        TYPE EBAN-MENGE, "剩余可申购数量与本次申购数量差额
        STATU     TYPE ICON-NAME,  "剩余可申购数量与本次申购数量差额(绿灯，红灯)
        WBS_TEXT  TYPE STRING,     "WBS传送文本字段
        SJDM      TYPE ZMM024-SJDM, "设计代码
        BSTRF     TYPE MARC-BSTRF, "申购数除以舍入值
        BSTRF1    TYPE MARC-BSTRF, "舍入值
        TD_MATNR  TYPE MATNR,      "被替代物料
        TD_MAKTX  TYPE MAKTX,      "被替代物料描述
        TD_MEINS  TYPE MEINS,      "被替代单位
        TD_GRPFLAG TYPE ZTDTZ_PR-TD_GRPFLAG, "替代组标识
        YJWL       TYPE MATNR,           "应计物料
        YJWLMS     TYPE MAKTX,            "应计物料描述
        YJSL       TYPE MENGE_D,          "应计数量
        TD_MENGE   TYPE MENGE_D,      "折算数量
        QTDB    TYPE MENGE_D,      "其他调拨
        LOEKZ     TYPE EBAN-LOEKZ, "删除标记      "added by it02 20161115
        SEL,
      END OF TY_DATA.

TYPES:BEGIN OF TY_EBAN,
        PS_PSP_PNR TYPE PS_PSP_PNR, "WBS号
        BANFN      TYPE EBKN-BANFN, "采购申请
        BNFPO      TYPE EBKN-BNFPO, "采购申请行项目
        FRGZU      TYPE EBAN-FRGZU, "批准状态
        FRGST      TYPE EBAN-FRGST, "审批策略
        BANPR      TYPE EBAN-BANPR, "申请处理状态
        "PS_PSP_PNR TYPE EBKN-PS_PSP_PNR, "WBS
        WERKS      TYPE EBAN-WERKS, "工厂
        EKGRP      TYPE EBAN-EKGRP, "采购组
        AFNAM      TYPE EBAN-AFNAM, "申请者
        ERDAT      TYPE EBKN-ERDAT, "创建日期
        MATNR      TYPE EBAN-MATNR, "物料编码
        TXZ01      TYPE EBAN-TXZ01, "物料描述
        MATKL      TYPE EBAN-MATKL, "物料组
        MEINS      TYPE EBAN-MEINS, "计量单位
        MENGE      TYPE EBAN-MENGE, "本次申购数量
        PLIFZ     TYPE EBAN-PLIFZ, "计划交货天数
        BADAT     TYPE EBAN-BADAT, "需求（请求）日期
        LFDAT     TYPE EBAN-LFDAT, "项目交货日期
        LOEKZ     TYPE EBAN-LOEKZ, "删除标记      "added by it02 20161115
      END OF TY_EBAN.

TYPES:BEGIN OF TY_ECP,
        PSPNR TYPE PS_INTNR,
        WERKS TYPE WERKS_D,
        MATNR TYPE MATNR,
        MENGE TYPE MENGE_POS,
      END OF TY_ECP.

TYPES:BEGIN OF TY_MARC,
        WERKS TYPE MARC-WERKS,
        MATNR TYPE MARC-MATNR,
        BSTRF TYPE MARC-BSTRF, "舍入值
      END OF TY_MARC.

 TYPES :BEGIN OF TY_MSEG,
    MJAHR TYPE MJAHR,              "物料凭证年度
    MBLNR TYPE MBLNR,      "物料凭证号
    ZEILE TYPE MBLPO,      "物料凭证行项目
    MATNR TYPE MATNR,       "物料号
    MENGE TYPE MENGE_D,     "数量
    MEINS TYPE MEINS,       "单位
    WERKS TYPE WERKS_D,     "工厂
    BWART TYPE  BWART,      "移动类型
    SHKZG TYPE SHKZG,       "借贷标识
    PSPNR TYPE PS_POSNR,    "来源项目号
    PSPNR1 TYPE PS_POSNR,    "目标项目号


   END OF TY_MSEG .

    TYPES:BEGIN OF TY_TD,
     "  PSPNR TYPE PS_INTNR,
       PSPID TYPE PS_PSPID,    "项目号
       POSID TYPE PS_POSID,    "WBS号
       WERKS TYPE WERKS_D,
       MATNR TYPE MATNR,
       MENGE TYPE MENGE_D,

       END OF TY_TD.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.

DATA GT_EBAN   TYPE TABLE OF TY_EBAN.
DATA GS_EBAN   TYPE TY_EBAN.

DATA GT_PROJ   TYPE TABLE OF PROJ.
DATA GS_PROJ   TYPE PROJ.

DATA GT_PRPS   TYPE TABLE OF PRPS.
DATA GS_PRPS   TYPE PRPS.

DATA GT_EBKN1  TYPE TABLE OF EBKN.
DATA GS_EBKN1  TYPE  EBKN.

DATA GT_EBAN1  TYPE TABLE OF EBAN.
DATA GS_EBAN1  TYPE EBAN.

DATA GT_ECP    TYPE TABLE OF TY_ECP.
DATA GS_ECP    TYPE TY_ECP.

DATA GT_DD07T  TYPE TABLE OF DD07T.
DATA GS_DD07T  TYPE DD07T.

DATA GT_ZMM009 TYPE TABLE OF ZMM009.
DATA GS_ZMM009 TYPE ZMM009.

DATA GT_ZMM024 TYPE TABLE OF ZMM024.
DATA GS_ZMM024 TYPE ZMM024.

DATA GT_MARC   TYPE TABLE OF MARC.
DATA GS_MARC   TYPE MARC.

DATA:GT_MAKT  TYPE TABLE OF MAKT,
     GS_MAKT  TYPE MAKT .

DATA:GT_ZTDTZ_PR TYPE TABLE OF ZTDTZ_PR,
     GS_ZTDTZ_PR TYPE ZTDTZ_PR.

*DATA:GT_ZTDTZ_PR_1 TYPE TABLE OF ZTDTZ_PR,
*     GS_ZTDTZ_PR_1 TYPE ZTDTZ_PR.

DATA:GT_ZS_PR  TYPE TABLE OF TY_TD,
     GS_ZS_PR  TYPE TY_TD .

DATA:GT_YS_PR  TYPE TABLE OF TY_TD,
     GS_YS_PR  TYPE TY_TD .

*DATA:GT_PO TYPE TABLE OF TY_TD,
*     GS_PO TYPE TY_TD.

DATA:GT_DB TYPE TABLE OF TY_TD,
     GS_DB TYPE TY_TD .

DATA:GT_ZTDTZ_PO TYPE TABLE OF ZTDTZ_PO,
     GS_ZTDTZ_PO TYPE ZTDTZ_PO.

DATA:GT_ZTDTZ_DB TYPE TABLE OF ZTDTZ_DB,
     GS_ZTDTZ_DB TYPE ZTDTZ_DB .

DATA:GT_MSEG TYPE TABLE OF  TY_MSEG ,
     GS_MSEG TYPE TY_MSEG .

DATA:GT_ZTDTZ_PR_Z TYPE TABLE OF ZTDTZ_PR,
     GS_ZTDTZ_PR_Z TYPE ZTDTZ_PR.

DATA:GT_ZTDTZ_PO_Z TYPE TABLE OF ZTDTZ_PO,
     GS_ZTDTZ_PO_Z TYPE ZTDTZ_PO.

DATA:GT_ZTDTZ_DB_Z TYPE TABLE OF ZTDTZ_DB,
     GS_ZTDTZ_DB_Z TYPE ZTDTZ_DB .

DATA:PSPID TYPE PS_PSPID .
DATA:POSID TYPE PS_POSID .
DATA  POST1     TYPE PROJ-POST1. "项目描述
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

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
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
SELECT-OPTIONS:S_WERKS FOR EBAN-WERKS NO-EXTENSION NO INTERVALS DEFAULT '1800'. "工厂

SELECT-OPTIONS:S_BANFN FOR EBAN-BANFN, "采购申请
               S_FRGZU FOR EBAN-FRGZU, "批准状态
               S_PSPID FOR PROJ-PSPID, "项目定义
               S_POSID FOR PRPS-POSID, "WBS
               S_EKGRP FOR EBAN-EKGRP, "采购组
               S_AFNAM FOR EBAN-AFNAM, "申请者
               S_ERDAT FOR EBAN-ERDAT, "创建日期
               S_MATNR FOR EBAN-MATNR, "物料编码
               S_MATKL FOR EBAN-MATKL, "物料组
               S_SJDM  FOR ZMM024-SJDM."设计代码
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.
*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**权限检查检查公司代码
*  " PERFORM FRM_AUTH_CHECK USING '03'.
*  PERFORM FRM_AUTH_CHECK.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA.
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  SELECT * FROM PROJ
     INTO TABLE GT_PROJ
    WHERE   PSPID IN S_PSPID.
  SORT GT_PROJ BY PSPNR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FROM PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR .
   SORT GT_PRPS BY PSPNR.

  SELECT EBKN~BANFN EBKN~BNFPO EBKN~PS_PSP_PNR
         EBAN~FRGST EBAN~BANPR EBAN~FRGZU
         EBAN~WERKS EBAN~EKGRP EBAN~AFNAM
         EBAN~ERDAT EBAN~MATNR EBAN~TXZ01
         EBAN~MATKL EBAN~MEINS EBAN~MENGE
         EBAN~PLIFZ EBAN~BADAT EBAN~LFDAT EBAN~LOEKZ
     INTO CORRESPONDING FIELDS OF TABLE GT_EBAN
      FROM EBKN
      INNER JOIN EBAN
      ON EBKN~BANFN = EBAN~BANFN
      AND EBKN~BNFPO = EBAN~BNFPO
      FOR ALL ENTRIES IN GT_PRPS
      WHERE EBKN~PS_PSP_PNR = GT_PRPS-PSPNR
       AND  EBAN~BANFN IN S_BANFN
       AND   FRGZU      IN S_FRGZU
       AND   WERKS      IN S_WERKS
       AND   EKGRP      IN S_EKGRP
       AND   AFNAM      IN S_AFNAM
       AND   EBAN~ERDAT IN S_ERDAT
       AND   MATNR      IN S_MATNR
       AND   MATKL      IN S_MATKL
       AND   EBKN~LOEKZ      <>  'X'.

  SORT GT_EBAN BY PS_PSP_PNR BANFN BNFPO.
  CHECK GT_EBAN IS NOT INITIAL .

  "查询工厂视图数据
  SELECT * FROM MARC
  INTO CORRESPONDING FIELDS OF TABLE GT_MARC
  FOR ALL ENTRIES IN GT_EBAN
  WHERE MATNR = GT_EBAN-MATNR
  AND   WERKS = GT_EBAN-WERKS.

SORT GT_MARC BY MATNR WERKS.


*  SELECT * INTO TABLE GT_ZTDTZ_PR
*    FROM ZTDTZ_PR
*    FOR ALL ENTRIES IN GT_EBAN
*    WHERE BANFN = GT_EBAN-BANFN
*  AND   BNFPO = GT_EBAN-BNFPO .
*
*  SORT GT_ZTDTZ_PR BY BANFN BNFPO MATNR .
SELECT * INTO TABLE GT_MAKT
  FROM MAKT
  WHERE  SPRAS = '1'.
  SORT GT_MAKT BY MATNR .

 "查询项目下所有WBS已做的采购申请
  SELECT * FROM EBKN
    INTO CORRESPONDING FIELDS OF TABLE GT_EBKN1
    FOR ALL ENTRIES IN GT_PRPS
    WHERE PS_PSP_PNR = GT_PRPS-PSPNR
    AND   LOEKZ <> 'X'.

  SORT GT_EBKN1 BY PS_PSP_PNR .

  CHECK GT_EBKN1 IS NOT INITIAL.

  SELECT * FROM EBAN
  INTO CORRESPONDING FIELDS OF TABLE GT_EBAN1
  FOR ALL ENTRIES IN GT_EBKN1
  WHERE BANFN = GT_EBKN1-BANFN
  AND   BNFPO = GT_EBKN1-BNFPO
  AND   LOEKZ <> 'X'
  AND   FRGKZ IN ('X','2').
  SORT GT_EBAN1 BY BANFN BNFPO .

  SELECT * FROM ZTDTZ_PR
        INTO TABLE GT_ZTDTZ_PR
        FOR ALL ENTRIES IN GT_EBKN1
        WHERE BANFN = GT_EBKN1-BANFN
        AND   BNFPO = GT_EBKN1-BNFPO
        AND   TD_GRPFLAG NE '' .
  SORT GT_ZTDTZ_PR BY PSPID POSID  TD_MATNR TD_GRPFLAG .
  GT_ZTDTZ_PR_Z = GT_ZTDTZ_PR .
  SORT GT_ZTDTZ_PR_Z BY PSPID POSID BANFN BNFPO TD_MATNR TD_GRPFLAG .
  "DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_PR COMPARING PSPID BANFN  TD_MATNR TD_GRPFLAG.
  DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_PR COMPARING PSPID POSID  TD_MATNR TD_GRPFLAG.

   SELECT * FROM ZTDTZ_PR
       APPENDING TABLE GT_ZTDTZ_PR
        FOR ALL ENTRIES IN GT_EBKN1
        WHERE BANFN = GT_EBKN1-BANFN
        AND   BNFPO = GT_EBKN1-BNFPO
        AND   TD_GRPFLAG EQ '' .
  SORT GT_ZTDTZ_PR BY BANFN BNFPO  MATNR." PSPID POSID BANFN BNFPO .

  SELECT * FROM DD07T
  INTO CORRESPONDING FIELDS OF TABLE GT_DD07T
  WHERE DOMNAME  = 'BANPR'
  AND DDLANGUAGE = SY-LANGU.

  SELECT * FROM ZMM009
  INTO CORRESPONDING FIELDS OF TABLE GT_ZMM009.

  SELECT * FROM ZMM024
  INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024.

*  "取采购申请替代维护
*  SELECT * INTO TABLE GT_ZTDTZ_PR
*    FROM ZTDTZ_PR
*    FOR ALL ENTRIES IN GT_EBKN1
*    WHERE BANFN = GT_EBKN1-BANFN
*    AND   BNFPO = GT_EBKN1-BNFPO .
* SORT GT_ZTDTZ_PR BY PSPID POSID BANFN BNFPO MATNR.


    SELECT MJAHR MBLNR ZEILE
           MATNR MENGE MEINS
           WERKS BWART SHKZG
           PS_PSP_PNR AS PSPNR
           MAT_PSPNR AS PSPNR1
      INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
  FROM MSEG
  FOR ALL  ENTRIES IN GT_PRPS
  WHERE MAT_PSPNR EQ GT_PRPS-PSPNR
  AND BWART  IN ('315','316') .

  SORT GT_MSEG BY PSPNR1  MBLNR MJAHR ZEILE .

     SELECT MJAHR MBLNR ZEILE
           MATNR MENGE MEINS
           WERKS BWART SHKZG
           MAT_PSPNR AS PSPNR
           PS_PSP_PNR AS PSPNR1
      APPENDING  CORRESPONDING FIELDS OF TABLE GT_MSEG
  FROM MSEG
  FOR ALL  ENTRIES IN GT_PRPS
  WHERE PS_PSP_PNR EQ GT_PRPS-PSPNR
  AND BWART  IN ('415')
  AND SHKZG EQ 'H' .

   SORT GT_MSEG BY PSPNR1  MBLNR MJAHR ZEILE .

 SELECT MJAHR MBLNR ZEILE
           MATNR MENGE MEINS
           WERKS BWART SHKZG
           MAT_PSPNR AS PSPNR1
           PS_PSP_PNR AS PSPNR
      APPENDING  CORRESPONDING FIELDS OF TABLE GT_MSEG
  FROM MSEG
  FOR ALL  ENTRIES IN GT_PRPS
  WHERE PS_PSP_PNR EQ GT_PRPS-PSPNR
  AND BWART  IN ('416')
  AND SHKZG EQ 'S' .

  SORT GT_MSEG BY PSPNR1  MBLNR MJAHR ZEILE .

  SELECT * INTO TABLE GT_ZTDTZ_DB
    FROM ZTDTZ_DB
    FOR ALL ENTRIES IN  GT_PROJ
    WHERE  PSPID1 EQ GT_PROJ-PSPID
    AND   TD_GRPFLAG NE ''.

   SORT GT_ZTDTZ_DB BY  PSPID POSID PSPID1 POSID1 TD_MATNR TD_GRPFLAG  .
    GT_ZTDTZ_DB_Z = GT_ZTDTZ_DB .
   SORT GT_ZTDTZ_DB_Z BY  PSPID POSID PSPID1 POSID1 MJAHR MBLNR ZEILE TD_MATNR TD_GRPFLAG  .
 "  DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_DB COMPARING PSPID POSID PSPID1 POSID1 MJAHR MBLNR TD_MATNR TD_GRPFLAG.
   DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_DB COMPARING PSPID POSID PSPID1 POSID1  TD_MATNR TD_GRPFLAG.


   SELECT * INTO TABLE GT_ZTDTZ_DB
    FROM ZTDTZ_DB
    FOR ALL ENTRIES IN  GT_PROJ
    WHERE  PSPID1 EQ GT_PROJ-PSPID
    AND   TD_GRPFLAG EQ ''.

    SORT GT_ZTDTZ_DB BY MJAHR MBLNR ZEILE   .


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
  DATA E_WBS_ECP        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES    TYPE TABLE OF KIS1.
  DATA LS_COST_LINES    TYPE KIS1.
  DATA L_LONGTEXT1 TYPE THEAD-TDNAME.
  DATA LT_TLINE    TYPE TABLE OF TLINE.
  DATA LS_TLINE    TYPE TLINE.

  "统计采购申请数量



  LOOP AT GT_EBKN1 INTO GS_EBKN1.
    AT NEW PS_PSP_PNR.
      CLEAR:PSPID,POSID .
      READ TABLE GT_PRPS INTO GS_PRPS
         WITH KEY PSPNR = GS_EBKN1-PS_PSP_PNR. .
            IF SY-SUBRC EQ 0 .
               POSID = GS_PRPS-POSID.
                READ TABLE GT_PROJ INTO GS_PROJ
                  WITH KEY PSPNR = GS_PRPS-PSPHI .
                 IF SY-SUBRC EQ 0 .
                   PSPID = GS_PROJ-PSPID .
                  ENDIF.
            ENDIF.

    ENDAT.

       READ TABLE GT_EBAN1  INTO GS_EBAN1
                      WITH KEY BANFN = GS_EBKN1-BANFN
                               BNFPO = GS_EBKN1-BNFPO
                               BINARY SEARCH.
       IF SY-SUBRC EQ 0 .
           IF GS_EBAN1-FRGKZ = 'X'.
             CLEAR:GS_ZS_PR .
             GS_ZS_PR-PSPID = PSPID.
             GS_ZS_PR-POSID = POSID.
             READ TABLE GT_ZTDTZ_PR INTO  GS_ZTDTZ_PR
              WITH KEY BANFN = GS_EBAN1-BANFN
                       BNFPO = GS_EBAN1-BNFPO
                       MATNR = GS_EBAN1-MATNR
                       BINARY SEARCH.
               IF SY-SUBRC EQ 0 .
                 GS_ZS_PR-MATNR = GS_ZTDTZ_PR-TD_MATNR .
                 GS_ZS_PR-MENGE = GS_ZTDTZ_PR-TD_MENGE.
                 COLLECT GS_ZS_PR INTO GT_ZS_PR .
                   IF GS_ZTDTZ_PR-TD_GRPFLAG NE ''.
                         LOOP AT GT_ZTDTZ_PR_Z INTO GS_ZTDTZ_PR_Z
                                  WHERE PSPID = GS_ZTDTZ_PR-PSPID
                                  AND   POSID = GS_ZTDTZ_PR-POSID
                              "    AND   BANFN  = GS_ZTDTZ_PR-BANFN
                                  AND   TD_MATNR = GS_ZTDTZ_PR-TD_MATNR
                                  AND   TD_GRPFLAG = GS_ZTDTZ_PR-TD_GRPFLAG .

                      DELETE GT_EBKN1 WHERE BANFN = GS_ZTDTZ_PR_Z-BANFN
                                       AND BNFPO = GS_ZTDTZ_PR_Z-BNFPO .
                      DELETE GT_EBAN1 WHERE BANFN = GS_ZTDTZ_PR_Z-BANFN
                                       AND BNFPO = GS_ZTDTZ_PR_Z-BNFPO .

                          ENDLOOP.
                 ENDIF.

                 CONTINUE .

                ELSE.
                  GS_ZS_PR-MATNR = GS_EBAN1-MATNR .
                 GS_ZS_PR-MENGE = GS_EBAN1-MENGE.
                 COLLECT GS_ZS_PR INTO GT_ZS_PR .
                 CONTINUE .
               ENDIF.
              ELSE.
                CLEAR:GS_YS_PR .
             GS_YS_PR-PSPID = PSPID.
             GS_YS_PR-POSID = POSID.
             READ TABLE GT_ZTDTZ_PR INTO  GS_ZTDTZ_PR
              WITH KEY BANFN = GS_EBAN1-BANFN
                       BNFPO = GS_EBAN1-BNFPO
                       MATNR = GS_EBAN1-MATNR
                       BINARY SEARCH.
               IF SY-SUBRC EQ 0 .
                 GS_YS_PR-MATNR = GS_ZTDTZ_PR-TD_MATNR .
                 GS_YS_PR-MENGE = GS_ZTDTZ_PR-TD_MENGE.
                 COLLECT GS_YS_PR INTO GT_YS_PR .
                IF GS_ZTDTZ_PR-TD_GRPFLAG NE ''.
                         LOOP AT GT_ZTDTZ_PR_Z INTO GS_ZTDTZ_PR_Z
                                  WHERE PSPID = GS_ZTDTZ_PR-PSPID
                                  AND   POSID = GS_ZTDTZ_PR-POSID
                              "    AND   BANFN  = GS_ZTDTZ_PR-BANFN
                                  AND   TD_MATNR = GS_ZTDTZ_PR-TD_MATNR
                                  AND   TD_GRPFLAG = GS_ZTDTZ_PR-TD_GRPFLAG .

                      DELETE GT_EBKN1 WHERE BANFN = GS_ZTDTZ_PR_Z-BANFN
                                       AND BNFPO = GS_ZTDTZ_PR_Z-BNFPO .
                      DELETE GT_EBAN1 WHERE BANFN = GS_ZTDTZ_PR_Z-BANFN
                                       AND BNFPO = GS_ZTDTZ_PR_Z-BNFPO .

                          ENDLOOP.
                ENDIF.
                 CONTINUE .

                ELSE.
                  GS_YS_PR-MATNR = GS_EBAN1-MATNR .
                 GS_YS_PR-MENGE = GS_EBAN1-MENGE.
                 COLLECT GS_YS_PR INTO GT_YS_PR .
                 CONTINUE .
               ENDIF.

           ENDIF.
       ENDIF.
   ENDLOOP.
   SORT  GT_ZS_PR BY PSPID POSID MATNR .
   SORT GT_YS_PR BY PSPID POSID MATNR .

      "统计从其他项目调拨数量
  LOOP  AT GT_MSEG INTO GS_MSEG .
    CLEAR:GS_DB.
     AT NEW PSPNR1.
      CLEAR:PSPID,POSID .
      READ TABLE GT_PRPS INTO GS_PRPS
         WITH KEY PSPNR = GS_MSEG-PSPNR1. .
            IF SY-SUBRC EQ 0 .
               POSID = GS_PRPS-POSID.
                READ TABLE GT_PROJ INTO GS_PROJ
                  WITH KEY PSPNR = GS_PRPS-PSPHI .
                 IF SY-SUBRC EQ 0 .
                   PSPID = GS_PROJ-PSPID .
                  ENDIF.
            ENDIF.

    ENDAT.
      GS_DB-PSPID = PSPID.
      GS_DB-POSID = POSID.
      IF GS_MSEG-BWART EQ '316' OR GS_MSEG-BWART EQ '416'.
          GS_MSEG-MENGE = GS_MSEG-MENGE * -1 .
      ENDIF.
      READ TABLE GT_ZTDTZ_DB INTO GS_ZTDTZ_DB
      WITH KEY  MJAHR = GS_MSEG-MJAHR
                MBLNR = GS_MSEG-MBLNR
                ZEILE = GS_MSEG-ZEILE
                MATNR = GS_MSEG-MATNR
                BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
           GS_DB-MATNR = GS_ZTDTZ_DB-TD_MATNR.
           GS_DB-MENGE = GS_ZTDTZ_DB-TD_MENGE.
             IF GS_ZTDTZ_DB-BWART EQ '316' OR GS_ZTDTZ_DB-BWART EQ '416'.

                GS_DB-MENGE = GS_ZTDTZ_DB-TD_MENGE * -1.

              ENDIF.
              COLLECT GS_DB INTO GT_DB .

              IF GS_ZTDTZ_DB-TD_GRPFLAG NE ''.
                  LOOP AT GT_ZTDTZ_DB_Z INTO GS_ZTDTZ_DB_Z
                          WHERE PSPID = GS_ZTDTZ_DB-PSPID
                             AND POSID = GS_ZTDTZ_DB-POSID
                             AND PSPID1 = GS_ZTDTZ_DB-PSPID1
                             AND POSID1 = GS_ZTDTZ_DB-POSID1
*                             AND MBLNR  = GS_ZTDTZ_DB-MBLNR
*                             AND MJAHR = GS_ZTDTZ_DB-MJAHR
                             AND TD_MATNR = GS_ZTDTZ_DB-TD_MATNR
                             AND TD_GRPFLAG = GS_ZTDTZ_DB-TD_GRPFLAG.

                    DELETE GT_MSEG WHERE MBLNR = GS_ZTDTZ_DB_Z-MBLNR
                                    AND  MJAHR = GS_ZTDTZ_DB_Z-MJAHR
                                    AND  ZEILE = GS_ZTDTZ_DB_Z-ZEILE.
                 ENDLOOP.

               ENDIF.
                 CONTINUE.
          ELSE.
            GS_DB-MATNR = GS_MSEG-MATNR.
            GS_DB-MENGE = GS_MSEG-MENGE .
             COLLECT GS_DB INTO GT_DB .
          CONTINUE.
        ENDIF.
  ENDLOOP.
    SORT GT_DB BY PSPID POSID MATNR .



  LOOP AT GT_EBAN INTO GS_EBAN.

*WBS元素
   AT NEW PS_PSP_PNR .
     CLEAR:PSPID,POSID ,POST1.
    READ TABLE GT_PRPS INTO GS_PRPS
    WITH KEY PSPNR = GS_EBAN-PS_PSP_PNR.
    IF SY-SUBRC = 0.
      POSID = GS_PRPS-POSID.
    "*项目
    READ TABLE GT_PROJ INTO GS_PROJ
    WITH KEY PSPNR = GS_PRPS-PSPHI.
    IF SY-SUBRC = 0.
      PSPID  = GS_PROJ-PSPID.
      POST1  = GS_PROJ-POST1.
     ENDIF.
    ENDIF.

  "  *预算数
    CLEAR:E_WBS_ECP,
     LS_E_WBS_ECP,
     LS_COST_LINES.

    REFRESH:LT_E_WBS_ECP,
            LT_COST_LINES.
    REFRESH GT_ECP.

    IF PSPID IS NOT INITIAL .
      CALL FUNCTION 'CNECP_READ'
        EXPORTING
          I_PROJ_DEF    = PSPID
          I_VERSION     = '000'
        IMPORTING
          E_WBS_ECP     = E_WBS_ECP
        EXCEPTIONS
          ERROR_MESSAGE = 1.
*      IF SY-SUBRC <> 0.
*        RAISE NOT_FOUND.
*      ENDIF.

      LT_E_WBS_ECP = E_WBS_ECP.
      IF  LT_E_WBS_ECP IS NOT INITIAL.
        READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
        INDEX 1.
        IF SY-SUBRC = 0.
          LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

          LOOP AT   LT_COST_LINES INTO LS_COST_LINES
          WHERE    TYPPS = 'M'.
            MOVE-CORRESPONDING LS_COST_LINES TO GS_ECP.
            COLLECT GS_ECP INTO GT_ECP.
            CLEAR GS_ECP.
          ENDLOOP.
        ENDIF.
      ENDIF.

*      READ TABLE GT_ECP INTO GS_ECP
*      WITH KEY MATNR = GS_DATA-MATNR.
*      IF SY-SUBRC = 0.
*        GS_DATA-YSS = GS_ECP-MENGE.
*      ENDIF.
    ENDIF.

    ENDAT.
    MOVE-CORRESPONDING GS_EBAN TO GS_DATA.
  GS_DATA-POSID = POSID.
  GS_DATA-PSPID = PSPID.
  GS_DATA-POST1 = POST1.
   "应计物料、应计物料描述、应计数量
  GS_DATA-YJWL = GS_DATA-MATNR.
  GS_DATA-YJWLMS = GS_DATA-TXZ01.
  GS_DATA-YJSL = GS_DATA-MENGE .
 "被替代物料
    READ TABLE  GT_ZTDTZ_PR INTO GS_ZTDTZ_PR
     WITH KEY  BANFN = GS_EBAN-BANFN
               BNFPO = GS_EBAN-BNFPO
               MATNR = GS_EBAN-MATNR
              BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
     GS_DATA-TD_MATNR = GS_ZTDTZ_PR-TD_MATNR .
     READ TABLE GT_MAKT INTO GS_MAKT
        WITH KEY MATNR = GS_DATA-TD_MATNR
             BINARY SEARCH .
        IF SY-SUBRC EQ 0.
          GS_DATA-TD_MAKTX = GS_MAKT-MAKTX .
         ENDIF.
     GS_DATA-TD_MEINS = GS_ZTDTZ_PR-TD_MEINS.
     GS_DATA-TD_MENGE = GS_ZTDTZ_PR-TD_MENGE.
     GS_DATA-TD_GRPFLAG = GS_ZTDTZ_PR-TD_GRPFLAG.
     "应计物料、应计数量、应计物料描述
     GS_DATA-YJWL = GS_DATA-TD_MATNR.
     GS_DATA-YJWLMS = GS_DATA-TD_MAKTX.
     GS_DATA-YJSL = GS_DATA-TD_MENGE .
      ELSE.
         READ TABLE GT_ZTDTZ_PR_Z INTO GS_ZTDTZ_PR_Z
               WITH KEY BANFN = GS_EBAN-BANFN
                        BNFPO = GS_EBAN-BNFPO
                        BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
              READ TABLE  GT_ZTDTZ_PR INTO GS_ZTDTZ_PR
              WITH KEY  BANFN = GS_EBAN-BANFN
                        BNFPO = GS_EBAN-BNFPO
                        MATNR = GS_EBAN-MATNR
                        BINARY SEARCH .
              IF SY-SUBRC NE 0 .
                 GS_DATA-TD_MATNR =  GS_ZTDTZ_PR_Z-TD_MATNR .
                 GS_DATA-TD_MEINS = GS_ZTDTZ_PR_Z-TD_MEINS.
                 GS_DATA-TD_MENGE = GS_ZTDTZ_PR_Z-TD_MENGE.
                 GS_DATA-TD_GRPFLAG = GS_ZTDTZ_PR_Z-TD_GRPFLAG.
                 READ TABLE GT_MAKT INTO GS_MAKT
                      WITH KEY MATNR = GS_DATA-TD_MATNR
                      BINARY SEARCH .
                 IF SY-SUBRC EQ 0 .
                    GS_DATA-TD_MAKTX = GS_MAKT-MAKTX.
                  ENDIF.
                   "应计物料、应计数量、应计物料描述
                  GS_DATA-YJWL = GS_DATA-TD_MATNR.
                  GS_DATA-YJWLMS = GS_DATA-TD_MAKTX.
                  GS_DATA-YJSL = 0.

                ENDIF.
           ENDIF.
    ENDIF.


*设计代码
    READ TABLE GT_ZMM024 INTO GS_ZMM024
    WITH KEY MATNR =  GS_DATA-YJWL
             POSID = GS_PROJ-PSPID.
    IF SY-SUBRC = 0.
      GS_DATA-SJDM = GS_ZMM024-SJDM.
    ENDIF.

*处理状态描述
    READ TABLE GT_DD07T INTO GS_DD07T
    WITH KEY DOMVALUE_L = GS_EBAN-BANPR.
    IF SY-SUBRC = 0.
      GS_DATA-CLZT = GS_DD07T-DDTEXT.
    ENDIF.
* 舍入值
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_EBAN-MATNR
             WERKS = GS_EBAN-WERKS.
    IF SY-SUBRC = 0.
      GS_DATA-BSTRF1 = GS_MARC-BSTRF.

*舍入值不为0，计算最小包装量
      IF GS_DATA-BSTRF1 <> 0.
        GS_DATA-BSTRF = GS_DATA-MENGE / GS_DATA-BSTRF1.
      ENDIF.
    ENDIF.

*预算数:用应计物料读取预算数
      READ TABLE GT_ECP INTO GS_ECP
      WITH KEY MATNR = GS_DATA-YJWL.
      IF SY-SUBRC = 0.
        GS_DATA-YSS = GS_ECP-MENGE.
      ENDIF.


*未确认累计申购数量
    READ TABLE GT_ZS_PR INTO GS_ZS_PR
          WITH KEY PSPID = GS_DATA-PSPID
                   POSID = GS_DATA-POSID
                   MATNR = GS_DATA-YJWL
          BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_ZS_PR-MENGE.

      ENDIF.

**期初数量(未确认累计申购数量)
*    LOOP AT GT_ZMM009 INTO GS_ZMM009
*    WHERE PSPHI = GS_DATA-PSPID
*    AND   MATNR = GS_DATA-MATNR.
*      GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_ZMM009-WQRLJSGS.
*    ENDLOOP.

*已确认累计申购数量
  READ TABLE GT_YS_PR INTO GS_YS_PR
          WITH KEY PSPID = GS_DATA-PSPID
                   POSID = GS_DATA-POSID
                   MATNR = GS_DATA-YJWL
          BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_YS_PR-MENGE.

      ENDIF.

**期初数量(已确认累计申购数量)
*    LOOP AT GT_ZMM009 INTO GS_ZMM009
*    WHERE PSPHI = GS_DATA-PSPID
*    AND   MATNR = GS_DATA-MATNR.
*      GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_ZMM009-YQRLJSGS.
*    ENDLOOP.

  "其他项目调拨数量
    READ TABLE GT_DB INTO  GS_DB
        WITH KEY PSPID = GS_DATA-PSPID
                 POSID = GS_DATA-POSID
                   MATNR = GS_DATA-YJWL.
     IF SY-SUBRC EQ 0 .
       GS_DATA-QTDB = GS_DATA-QTDB + GS_DB-MENGE .
     ENDIF.

*剩余申购数量
    GS_DATA-SYKSQSL = GS_DATA-YSS - GS_DATA-YQRLJSGSL
                      - GS_DATA-WQRLJSGSL  - GS_DATA-QTDB.

   IF  GS_DATA-SYKSQSL >= 0 .
      GS_DATA-STATU    = ICON_GREEN_LIGHT.
     ELSE.
     GS_DATA-STATU    =  ICON_RED_LIGHT.

   ENDIF.

**剩余可申购数量与本次申购数量差额
*    GS_DATA-CE       =  GS_DATA-SYKSQSL - GS_DATA-MENGE.
*
**状态
*    IF GS_DATA-CE >= 0.
*      GS_DATA-STATU    = ICON_GREEN_LIGHT.
*    ELSE.
*      GS_DATA-STATU    =  ICON_RED_LIGHT.
*    ENDIF.

*WBS传送文本
    CLEAR L_LONGTEXT1.
    CONCATENATE GS_DATA-BANFN GS_DATA-BNFPO INTO L_LONGTEXT1.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'B01'
        LANGUAGE                = SY-LANGU
        NAME                    = L_LONGTEXT1
        OBJECT                  = 'EBAN'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        LINES                   = LT_TLINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ELSE.
      LOOP AT LT_TLINE INTO LS_TLINE.
        CONCATENATE  GS_DATA-WBS_TEXT LS_TLINE-TDLINE INTO GS_DATA-WBS_TEXT SEPARATED BY SPACE.
      ENDLOOP.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

*设计代码
  IF S_SJDM IS NOT INITIAL .
    DELETE GT_DATA WHERE SJDM NOT IN S_SJDM.
  ENDIF.

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
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
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
  GW_LAYOUT-ZEBRA         = 'X'.
  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'SEL'.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .

  INIT_FIELDCAT 'BANFN'        '采购申请'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BNFPO'        '行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FRGZU'        '批准状态'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FRGST'        '审批策略'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANPR'        '申请处理状态'         '' '' '' '' '' 'EBAN' 'BANPR'.
  INIT_FIELDCAT 'CLZT'         '处理状态'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PSPID'        '项目定义'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'       'WBS元素'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'        '采购组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AFNAM'        '申请者'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ERDAT'        '创建日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
  INIT_FIELDCAT 'TXZ01'        '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'        '物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'        '计量单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'        '本次申购数量(D)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TD_MATNR'     '被替代物料'         '' '' '' '' '' 'MSEG' 'MATNR'.
  INIT_FIELDCAT 'TD_MAKTX'      '被替代物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TD_MEINS'      '被替代单位'         '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'TD_GRPFLAG'   '替代组标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TD_MENGE'      '折算数'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJWL'          '应计物料'         '' '' '' '' '' 'EBAN' 'MATNR'.
  INIT_FIELDCAT 'YJWLMS'        '应计物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJSL'          '应计数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSS'          '预算数(A)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WQRLJSGSL'    '材料单在审数量(B)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQRLJSGSL'    '材料单已审数量(C)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QTDB'         '其他项目调拨数量（G）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SYKSQSL'      '剩余可申购数量(E=A-B-C-G)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STATU'        '状态标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PLIFZ'        '计划交货天数'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BADAT'        '需求日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LFDAT'        '项目交货日期'         '' '' '' '' '' '' ''.
 " INIT_FIELDCAT 'CE'           '超量数(F=E-D)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WBS_TEXT'      'WBS传送文本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BSTRF1'        '舍入值(最小包装量)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BSTRF'         '申购数除以舍入值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LOEKZ'         '删除标记'         '' '' '' '' '' '' ''.
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
*     I_CALLBACK_TOP_OF_PAGE   = ' '
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
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "

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

*  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
*
*
*  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*    IMPORTING
*      E_GRID = G_REF_GRID.
*
*  CASE R_UCOMM.
*
*  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND
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
