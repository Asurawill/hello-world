REPORT ZTDTZ_PR.
"采购订单台账维护
" IT02
" 2016-04-08

TABLES:EKKN,EKKO,EKPO,MAKT,PROJ,PRPS .

 TYPES:BEGIN OF TY_DATA ,
    EBELN TYPE EBELN,            "采购订单
    EBELP TYPE EBELP,            "采购订单行号
    WERKS TYPE WERKS_D,            "工厂
    PSPID TYPE PS_PSPID,         "项目号
    POST1 TYPE PS_POST1,            "项目描述
    POSID TYPE PS_POSID,         "WBS
    MATNR TYPE MATNR,            "物料号
    TXZ01 TYPE TXZ01,            "物料描述
    MENGE TYPE MENGE_D,             "数量
    MEINS TYPE MEINS,             "单位
    EKGRP TYPE EKGRP,             "采购组
    EKNAM TYPE EKNAM,
    BANFN TYPE BANFN,            "对应采购申请号
    BNFPO TYPE BNFPO,            "对应采购申请行号
    BEDNR TYPE BEDNR,
    AFNAM  TYPE AFNAM,           "申请者
    BEDAT TYPE BEDAT,             "凭证日期
    AEDAT TYPE AEDAT,             "创建日期
    SJDM  TYPE ZSJDM,             "设计代码
    WBSCSWB TYPE STRING,          "WBS传送文本
    TD_MATNR TYPE MATNR,          "被替代物料
    TD_MAKTX TYPE MAKTX,          "被替代物料描述
    TD_MEINS TYPE MEINS,          "被替代物料单位
    TD_GRPFLAG TYPE   C LENGTH 25,      "替代组表识
    TD_MENGE TYPE    MENGE_D ,  "折算数
    SEL(1),
   END OF TY_DATA  .


TYPES:BEGIN OF TY_PROJ_SEL,
  EBELN TYPE EBELN,            "采购订单
  PSPID TYPE PS_PSPID,    "项目号

 END OF TY_PROJ_SEL.

 "声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_onf4 for event onf4 of cl_gui_alv_grid
     importing e_fieldname es_row_no er_event_data et_bad_cells e_display,
      handle_modify  FOR EVENT data_changed_finished OF cl_gui_alv_grid
     IMPORTING  e_modified  et_good_cells ." ER_DATA_CHANGED."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM . "data_changed_finished

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION




   DATA:GS_DATA TYPE TY_DATA,
        GT_DATA TYPE TABLE OF TY_DATA .

   DATA:GS_DATA_SEL TYPE TY_DATA,
        GT_DATA_SEL TYPE TABLE OF TY_DATA .

   DATA:GT_DATA_DEL TYPE TABLE OF TY_DATA ,
         GS_DATA_DEL TYPE TY_DATA.

   DATA:GS_PROJ TYPE PROJ,
        GT_PROJ TYPE TABLE OF PROJ.

  DATA:GT_EKPO TYPE TABLE OF EKPO,
       GS_EKPO TYPE EKPO.

  DATA:GT_EKKO TYPE TABLE OF EKKO,
       GS_EKKO TYPE EKKO.

   DATA: GT_EKKN  TYPE TABLE OF EKKN,
         GS_EKKN  TYPE EKKN.

   DATA:GT_PRPS TYPE TABLE OF PRPS,
        GS_PRPS TYPE PRPS.

   DATA:GT_T024 TYPE TABLE OF T024,
        GS_T024 TYPE T024.

   DATA:GT_ZMM024 TYPE TABLE OF ZMM024,
        GS_ZMM024 TYPE ZMM024.

   DATA: IT_LINES TYPE TABLE OF TLINE,
         WA_LINES TYPE TLINE.
   DATA: G_OBJNAME TYPE THEAD-TDNAME.

   DATA:GT_ZTDTZ_PO TYPE TABLE OF ZTDTZ_PO,
        GS_ZTDTZ_PO TYPE ZTDTZ_PO.


   DATA:GT_PROJ_SEL TYPE TABLE OF   TY_PROJ_SEL ,
        GS_PROJ_SEL TYPE TY_PROJ_SEL.

   DATA:GT_MAKT TYPE TABLE OF MAKT ,
        GS_MAKT TYPE MAKT .

   DATA:GT_PO TYPE TABLE OF ZTDTZ_PO,
        GS_PO TYPE ZTDTZ_PO.

   DATA:GT_PR TYPE TABLE OF ZTDTZ_PR,
        GS_PR TYPE  ZTDTZ_PR .

   DATA:GT_EBAN TYPE TABLE OF EBAN,
        GS_EBAN TYPE EBAN .

 DATA G_ANSWER     TYPE STRING. "控制弹出框
    FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA.

   DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  IF &4 = 'X'.
    GW_LVC-KEY = 'X'.
  ENDIF.
*   IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
   IF &1 = 'MENGE' .
   GW_LVC-QFIELDNAME = 'MEINS'.
  ENDIF.

  IF &1 = 'TD_MENGE' .
    GW_LVC-QFIELDNAME = 'TD_MEINS'.
    GW_LVC-DECIMALS = 3.
 ENDIF.



*  IF &1 = 'TD_MATNR' .
*   gw_lvc-f4availabl = 'X'.
* ENDIF.

  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "ALV的格式
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

DATA gt_event_receiver TYPE REF TO lcl_event_receiver .

************************************************************************
* CONSTANT
************************************************************************

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:
*P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
P_BUKRS TYPE BUKRS OBLIGATORY  DEFAULT '1800'.   "公司代码
"P_PSPID TYPE  PROJ-PSPID OBLIGATORY .   "项目号

SELECT-OPTIONS:  S_PSPID  FOR PROJ-PSPID ,   "过账日期
                S_EBELN  FOR EKKO-EBELN,   "采购订单编号
                S_MATNR  FOR EKPO-MATNR ,   "物料号
                S_AEDAT  FOR EKKO-AEDAT.   "创建日期

 PARAMETERS:  G_WH  TYPE CHAR1 RADIOBUTTON GROUP G1,
              G_XS  TYPE CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
*  PERFORM FRM_AUTH_CHECK USING '03'.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
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
 IF G_WH EQ 'X' .  "维护
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
     FROM PROJ
    WHERE PSPID IN S_PSPID.
  SORT GT_PROJ BY PSPNR.

     SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FROM PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR.
   SORT GT_PRPS BY PSPNR.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
       FROM EKKN
       FOR ALL ENTRIES IN GT_PRPS
       WHERE PS_PSP_PNR = GT_PRPS-PSPNR
        AND  LOEKZ <> 'L'
        AND  EBELN IN S_EBELN
        AND  AEDAT IN S_AEDAT.
     SORT GT_EKKN BY EBELN EBELP.

      CHECK GT_EKKN IS NOT INITIAL.
      SELECT EBELN EBELP WERKS MATNR TXZ01 MENGE MEINS
             BANFN BNFPO BEDNR AFNAM
         INTO CORRESPONDING FIELDS OF TABLE GT_DATA
            FROM EKPO
            FOR  ALL ENTRIES IN GT_EKKN
            WHERE EBELN = GT_EKKN-EBELN
             AND   EBELP = GT_EKKN-EBELP
             AND   MATNR IN S_MATNR
             AND   ( KNTTP = 'Q'
             OR KNTTP = '' )
             AND   LOEKZ <> 'L'.

      SORT GT_DATA BY EBELN EBELP.

   SELECT  * INTO TABLE GT_EKKO
     FROM EKKO
     WHERE EBELN IN S_EBELN
     AND   BUKRS EQ P_BUKRS .

  SORT GT_EKKO BY EBELN .


   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T024
     FROM T024
    .
   SORT GT_T024 BY EKGRP.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
     FROM ZMM024
     WHERE POSID  IN S_PSPID
      AND   MATNR IN S_MATNR.

   SORT GT_ZMM024 BY MATNR POSID.

      "查询采购订单替代台账维护信息
   SELECT * INTO TABLE GT_PO
     FROM ZTDTZ_PO
     WHERE WERKS EQ P_BUKRS
     AND EBELN IN S_EBELN
     AND PSPID IN S_PSPID
     AND MATNR IN S_MATNR .

   SORT GT_PO BY  EBELN EBELP.

    SELECT * INTO TABLE GT_MAKT
    FROM MAKT
    WHERE SPRAS = '1'.

  SORT GT_MAKT BY MATNR .

    SELECT * INTO TABLE GT_PR
    FROM ZTDTZ_PR
    WHERE  PSPID IN S_PSPID.
 SORT GT_PR BY BANFN BNFPO .


 ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_DATA
     FROM  ZTDTZ_PO
    WHERE  EBELN IN S_EBELN
    AND    PSPID IN S_PSPID
    AND    WERKS EQ  P_BUKRS
    AND    MATNR IN S_MATNR.
  SORT GT_DATA BY PSPID EBELN .

  SELECT * INTO TABLE GT_PROJ
    FROM PROJ
    WHERE PSPID IN S_PSPID .
  SORT GT_PROJ BY PSPID .

  SELECT * INTO TABLE GT_MAKT
    FROM MAKT
    WHERE SPRAS = '1'
     AND   MATNR IN S_MATNR.

  SORT GT_MAKT BY MATNR .





   ENDIF.

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
IF G_WH EQ 'X'.
 LOOP AT GT_DATA ASSIGNING <FS_DATA> .
  "项目号
  READ TABLE GT_EKKN INTO GS_EKKN
            WITH KEY EBELN = <FS_DATA>-EBELN
                     EBELP = <FS_DATA>-EBELP
                 BINARY SEARCH.
  IF SY-SUBRC EQ 0 .


      READ TABLE GT_PRPS INTO GS_PRPS
               WITH KEY PSPNR = GS_EKKN-PS_PSP_PNR
                         BINARY SEARCH .
        IF SY-SUBRC EQ 0.
          "WBS号
           <FS_DATA>-POSID = GS_PRPS-POSID.
           READ TABLE GT_PROJ INTO  GS_PROJ
             WITH KEY PSPNR = GS_PRPS-PSPHI
              BINARY SEARCH .
            IF SY-SUBRC EQ 0 .
               <FS_DATA>-PSPID = GS_PROJ-PSPID.
               <FS_DATA>-POST1 = GS_PROJ-POST1.
              ENDIF.
        ENDIF.
  ENDIF.
  "CHECK 采购申请号
  IF <FS_DATA>-BANFN EQ ''.
     <FS_DATA>-BANFN = <FS_DATA>-BEDNR+0(8).
     <FS_DATA>-BNFPO = <FS_DATA>-BANFN+8(1).

  ENDIF.
"采购组
  READ TABLE GT_EKKO INTO GS_EKKO
                WITH KEY EBELN = <FS_DATA>-EBELN BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
      <FS_DATA>-EKGRP = GS_EKKO-EKGRP.
       "创建日期
      <FS_DATA>-AEDAT = GS_EKKO-AEDAT.
       "凭证日期
      <FS_DATA>-BEDAT = GS_EKKO-BEDAT.
    ENDIF.

  READ TABLE GT_T024 INTO GS_T024
                  WITH KEY EKGRP = <FS_DATA>-EKGRP BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
        <FS_DATA>-EKNAM = GS_T024-EKNAM .
    ENDIF.
"设计代码
  READ TABLE GT_ZMM024 INTO GS_ZMM024
                      WITH KEY MATNR = <FS_DATA>-MATNR
                       POSID = <FS_DATA>-PSPID BINARY SEARCH.
   IF SY-SUBRC EQ 0 .
      <FS_DATA>-SJDM = GS_ZMM024-SJDM.

   ENDIF.
"WBS传送文本
  PERFORM SELXWBS USING <FS_DATA>-EBELN <FS_DATA>-EBELP
                  CHANGING <FS_DATA>-WBSCSWB.

  READ TABLE GT_PO INTO GS_PO WITH KEY
               EBELN = <FS_DATA>-EBELN
               EBELP = <FS_DATA>-EBELP
               MATNR = <FS_DATA>-MATNR
               BINARY SEARCH .
     IF SY-SUBRC EQ 0 .
      <FS_DATA>-TD_MATNR = GS_PO-TD_MATNR . "替代物料
      <FS_DATA>-TD_MEINS = GS_PO-TD_MEINS.  "替代单位
      <FS_DATA>-TD_GRPFLAG = GS_PO-TD_GRPFLAG. "替代组标识
      <FS_DATA>-TD_MENGE = GS_PO-TD_MENGE.    "替代数量
         "被替代物料描述
       READ TABLE GT_MAKT INTO GS_MAKT
            WITH KEY MATNR = <FS_DATA>-TD_MATNR
               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
         <FS_DATA>-TD_MAKTX = GS_MAKT-MAKTX .
       ENDIF.
     ELSE.
        "默认继承采购申请的替代关系
        READ TABLE GT_PR INTO  GS_PR
              WITH KEY BANFN = <FS_DATA>-BANFN
                       BNFPO = <FS_DATA>-BNFPO
                       BINARY SEARCH .
              IF SY-SUBRC EQ 0 .
                 <FS_DATA>-TD_MATNR = GS_PR-TD_MATNR . "替代物料
                 <FS_DATA>-TD_MEINS = GS_PR-TD_MEINS . "替代单位
                     "被替代物料描述
                  READ TABLE GT_MAKT INTO GS_MAKT
                  WITH KEY MATNR = <FS_DATA>-TD_MATNR
                   BINARY SEARCH .
                IF SY-SUBRC EQ 0 .
                    <FS_DATA>-TD_MAKTX = GS_MAKT-MAKTX .
                ENDIF.
                <FS_DATA>-TD_MENGE = GS_PR-TD_MENGE / GS_PR-MENGE * <FS_DATA>-MENGE.    "替代数量

              ENDIF.

     ENDIF.
ENDLOOP.
ELSE.
    LOOP AT GT_DATA ASSIGNING <FS_DATA> .
    "项目描述
     READ TABLE GT_PROJ INTO  GS_PROJ
                 WITH KEY PSPID = <FS_DATA>-PSPID
                  BINARY SEARCH .
              IF SY-SUBRC EQ 0 .
                <FS_DATA>-POST1 = GS_PROJ-POST1.
              ENDIF.
      "物料描述
       READ TABLE GT_MAKT INTO GS_MAKT
                WITH KEY MATNR = <FS_DATA>-MATNR
                 BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
             <FS_DATA>-TXZ01 = GS_MAKT-MAKTX .

           ENDIF.
     "被替代物料描述
       READ TABLE GT_MAKT INTO GS_MAKT
            WITH KEY MATNR = <FS_DATA>-TD_MATNR
               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
         <FS_DATA>-TD_MAKTX = GS_MAKT-MAKTX .
       ENDIF.
  ENDLOOP.


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
  GW_LAYOUT-BOX_FNAME = 'SEL'.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
 IF G_WH EQ 'X'.
    INIT_FIELDCAT 'EBELN'        '采购订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELP'        '采购订单行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'         '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'PSPID'         '项目号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'         '项目描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POSID'         'WBS元素'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'         '物料编码'         '' '' '' '' '' 'EKPO' 'MATNR'.
    INIT_FIELDCAT 'TXZ01'         '物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'         '数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'         '单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EKGRP'         '采购组'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EKNAM'         '采购组描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'AFNAM'         '申请者'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'AEDAT'         '创建日期'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BEDAT'         '凭证日期'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJDM'          '设计代码'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WBSCSWB'       'WBS传送文本'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_MATNR'       '被替代物料'         '' '' '' 'X' '' 'EBAN' 'MATNR'.
    INIT_FIELDCAT 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_GRPFLAG'     '替代组标识'         '25' '' '' 'X' '' '' ''.
    INIT_FIELDCAT 'TD_MENGE'       '折算数'         '' '' '' 'X' '' 'EKPO' 'MENGE'.
  ELSE.
    INIT_FIELDCAT 'EBELN'        '采购订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELP'        '采购订单行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'         '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'PSPID'         '项目号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'         '项目描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POSID'         'WBS元素'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'         '物料编码'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TXZ01'         '物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'         '数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'         '单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_MATNR'       '被替代物料'         '' '' '' '' '' 'EBAN' 'MATNR'.
    INIT_FIELDCAT 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_GRPFLAG'     '替代组标识'         '25' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TD_MENGE'       '折算数'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'UNAME'       '维护账户'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'UDATE'       '维护日期'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'UTIME'       '维护时间'         '' '' '' '' '' '' ''.

   ENDIF.
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
*  GW_EVENTS-NAME =  'CALLER_EXIT' .
*  GW_EVENTS-FORM =  'FRM_BUTTON'.   "f4事件
*  APPEND GW_EVENTS TO GT_EVENTS.
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0360   text
*      -->P_0361   text
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
*      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
      T_OUTTAB                 = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

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
*      IF RS_SELFIELD-FIELDNAME = 'VBELN'
*        AND GS_DATA-VBELN IS NOT INITIAL.
*        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*      ENDIF.
"保存维护数据
     WHEN '&DATA_SAVE'.

       IF G_WH EQ 'X'.
         DATA L_SUBRC TYPE C."检查输入项

      " *提示对话框
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

       READ TABLE GT_DATA INTO GS_DATA WITH KEY SEL = 'X'.
         IF SY-SUBRC EQ 0 .
         "   SORT GT_DATA BY BANFN BNFPO .
            LOOP AT GT_DATA INTO GS_DATA WHERE SEL = 'X' .
                 LOOP AT GT_DATA INTO GS_DATA WHERE EBELN =  GS_DATA-EBELN
                                      AND  PSPID = GS_DATA-PSPID
                                    AND   TD_MATNR NE '' .

                    GS_DATA-SEL = 'X'.


                    MODIFY GT_DATA FROM  GS_DATA .
                ENDLOOP.
            ENDLOOP.


          ELSE.

            MESSAGE '请选择要保存的相应采购订单替代记录保存'  TYPE 'S'   DISPLAY LIKE 'E'.

         ENDIF.

      "检查必输项

        PERFORM CHECK_INPUT CHANGING L_SUBRC .

      IF L_SUBRC = '4'.
        EXIT.
      ENDIF.
      CHECK GS_DATA_SEL IS NOT INITIAL.
*
        IF GT_PROJ_SEL IS NOT INITIAL.
       SORT GT_PROJ_SEL BY EBELN  PSPID.
       DELETE ADJACENT DUPLICATES FROM GT_PROJ_SEL COMPARING EBELN  PSPID .
        "先删除已选择物理凭证对应的记录台账信息
       LOOP AT GT_PROJ_SEL INTO GS_PROJ_SEL.

          "根据选择采购订单、项目号维度区分 删除已维护台账信息表
         DELETE FROM ZTDTZ_PO WHERE EBELN = GS_PROJ_SEL-EBELN
                              AND PSPID = GS_PROJ_SEL-PSPID
                              .

       ENDLOOP.
      ENDIF.

     CHECK L_SUBRC EQ 0.
      LOOP AT GT_DATA_SEL INTO GS_DATA_SEL .

        CLEAR GS_ZTDTZ_PO .
        MOVE-CORRESPONDING GS_DATA_SEL TO GS_ZTDTZ_PO.

        "维护账号
        GS_ZTDTZ_PO-UNAME = SY-UNAME.
        "维护日期
        GS_ZTDTZ_PO-UDATE = SY-DATUM.

        APPEND GS_ZTDTZ_PO TO GT_ZTDTZ_PO.
      ENDLOOP.


      SORT GT_ZTDTZ_PO BY EBELN EBELP .
       CHECK GT_ZTDTZ_PO IS NOT INITIAL.
      "保存数据到 ZTDTZ_PR.
       MODIFY ZTDTZ_PO FROM TABLE GT_ZTDTZ_PO .

      "删除台账表 被替代物料 、替代组标识 、替代数量 为空的记录
            "先删除已选择物理凭证对应的记录台账信息
       LOOP AT GT_PROJ_SEL INTO GS_PROJ_SEL.

          "根据选择采购订单、项目号维度区分 删除替代物料且替代组且折算数量这三项都 为空的记录已维护台账信息表
         DELETE FROM ZTDTZ_PO WHERE EBELN = GS_PROJ_SEL-EBELN
                              AND PSPID = GS_PROJ_SEL-PSPID
                              AND  TD_MATNR EQ ''
                              AND TD_GRPFLAG EQ ''
                              AND TD_MENGE EQ 0.

       ENDLOOP.


        IF SY-SUBRC EQ 0 .

           MESSAGE '数据保存成功' TYPE 'S'.
          ELSE.

            MESSAGE '数据更新失败' TYPE 'S'.
         ENDIF.

     ENDIF.

      CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXWBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_BANFN  text
*      -->P_<FS_DATA>_BNFPO  text
*      <--P_<FS_DATA>_WBSCSWB  text
*----------------------------------------------------------------------*
FORM SELXWBS  USING    P_EBELN
                       P_EBELP
              CHANGING P_WBSCSWB.
CONCATENATE P_EBELN P_EBELP INTO G_OBJNAME .

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'F05'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'EKPO'
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
      LOOP AT IT_LINES INTO WA_LINES .
       CONDENSE WA_LINES-TDLINE NO-GAPS.
      IF P_WBSCSWB IS INITIAL.
          P_WBSCSWB = WA_LINES-TDLINE.
      ELSE.
          CONCATENATE P_WBSCSWB WA_LINES-TDLINE INTO P_WBSCSWB.
        ENDIF.

      ENDLOOP.

    ENDIF.
ENDFORM.

FORM FRM_BUTTON  USING E_GRID TYPE SLIS_DATA_CALLER_EXIT.
  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.

   LS_F4-FIELDNAME = 'TD_MATNR'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
 ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.



    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

 CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->HANDLE_ONF4   FOR GR_ALVGRID.

  CALL METHOD GR_ALVGRID->register_f4_for_fields
     EXPORTING
       it_f4  = lt_f4[].



ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_modify.
    perform handle_data_changed_finished using e_modified et_good_cells.
**    PERFORM refresh.
*   DATA:LS_MOD_CELL TYPE LVC_S_MODI ,  "应用的修改的单元格
*         stbl TYPE lvc_s_stbl.
**
**    LOOP AT gt_itab INTO wa_itab.
**      wa_itab-cc = wa_itab-bb * 2 .
**      MODIFY gt_itab FROM wa_itab.
**    ENDLOOP.
*    SORT ER_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID.
*    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
*      AT NEW ROW_ID.
*        READ TABLE GT_DATA INTO GS_DATA INDEX LS_MOD_CELL-ROW_ID.
*
*      ENDAT.
*
*      CASE LS_MOD_CELL-FIELDNAME.
*         WHEN 'TD_MATNR'." "被替代物料
*           "获取指定单元格改动后内容
*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*               EXPORTING
*                  I_ROW_ID = LS_MOD_CELL-ROW_ID
*                  I_FIELDNAME = 'TD_MATNR'
*               IMPORTING
*                   E_VALUE  = GS_DATA-TD_MATNR.
*          "查询被替代物料描述
*           SELECT SINGLE MAKTX INTO GS_DATA-TD_MAKTX
*             FROM MAKT
*             WHERE MATNR = GS_DATA-TD_MATNR.
*          "查询被替代物料单位
*           SELECT SINGLE MEINS INTO GS_DATA-TD_MEINS
*            FROM MARA
*            WHERE MATNR = GS_DATA-TD_MATNR .
*
*          ENDCASE.
*      "刷新数据到ALV
*          MODIFY GT_DATA FROM GS_DATA INDEX LS_MOD_CELL-ROW_ID.
*          CLEAR GS_DATA.
*
*    ENDLOOP.
*  "  *   稳定刷新
*    stbl-row = 'X'." 基于行的稳定刷新
*    stbl-col = 'X'." 基于列稳定刷新
*    CALL METHOD GR_ALVGRID->refresh_table_display
*      EXPORTING
*        is_stable = stbl
*      EXCEPTIONS
*      FINISHED  = 1
*      OTHERS    = 2.
*  IF SY-SUBRC <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
  ENDMETHOD.                    "HANDLE_MODIFY

 METHOD handle_onf4.
  FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.

   CASE e_fieldname.
     WHEN 'TD_MATNR'.
        READ TABLE GT_DATA INTO GS_DATA INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_MATNR CHANGING GS_DATA-TD_MATNR.
            IF GS_DATA-TD_MATNR IS NOT INITIAL.
              MODIFY GT_DATA FROM GS_DATA INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'TD_MATNR'.
               LW_MOD_CELL-VALUE = GS_DATA-TD_MATNR.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.

   ENDCASE.

  "**  Inform ALV Grid that event 'onf4' has been processed
  er_event_data->M_EVENT_HANDLED = 'X'.           "告知F4动作结束

ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_DATA_BTDWL  text
*----------------------------------------------------------------------*
FORM SUB_HELP_MATNR  CHANGING P_TD_MATNR.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'MAT1_S_MPN'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_TD_MATNR
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM FRM_DATA_CHANGED USING   P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件



  CALL METHOD  GR_ALVGRID->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>MC_EVT_MODIFIED
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.



  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_modify FOR GR_ALVGRID.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC．  text
*      <--P_CALL  text
*      <--P_METHOD  text
*      <--P_G_REF_GRID_>REFRESH_TABLE_DISP  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT  CHANGING L_SUBRC TYPE C.
 REFRESH GT_DATA_SEL .
 REFRESH GT_PROJ_SEL .
   LOOP AT GT_DATA INTO GS_DATA
      WHERE SEL = 'X'.

*    IF GS_DATA-TD_MATNR EQ '' AND GS_DATA-TD_MENGE EQ '' AND  GS_DATA-TD_GRPFLAG EQ ''.
*      CONTINUE.
*     ENDIF.
*    IF  GS_DATA-TD_MATNR EQ ''.
*      MESSAGE '被替代物料不为空'  TYPE 'S' DISPLAY LIKE 'E'.
*         L_SUBRC = 4.
*    ENDIF.

    IF  GS_DATA-TD_MATNR NE ''  AND GS_DATA-TD_MENGE EQ 0 .
         MESSAGE '被替代物料不为空，对应记录的折算数不能为0'  TYPE 'S' DISPLAY LIKE 'E'.
         L_SUBRC = 4.
         EXIT .
    ENDIF.
    CLEAR:GS_DATA_SEL .
     MOVE-CORRESPONDING GS_DATA TO GS_DATA_SEL.
      APPEND GS_DATA_SEL TO GT_DATA_SEL .
 MOVE-CORRESPONDING GS_DATA TO GS_PROJ_SEL.
    APPEND GS_PROJ_SEL TO GT_PROJ_SEL.
  ENDLOOP.


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
DATA:LW_CELL TYPE LVC_S_MODI.
DATA: LS_STABLE TYPE LVC_S_STBL.
"替代物料描述
READ TABLE   P_ET_GOOD_CELLS INTO LW_CELL  WITH KEY FIELDNAME = 'TD_MATNR '.
 IF SY-SUBRC EQ 0 .
     READ TABLE GT_DATA ASSIGNING <FS_DATA> INDEX  LW_CELL-ROW_ID.
     IF SY-SUBRC EQ 0 .
       CLEAR:<FS_DATA>-TD_MAKTX,<FS_DATA>-TD_MEINS.
       SELECT SINGLE MAKTX INTO <FS_DATA>-TD_MAKTX
             FROM MAKT
             WHERE MATNR = <FS_DATA>-TD_MATNR.
          "查询被替代物料单位
           SELECT SINGLE MEINS INTO <FS_DATA>-TD_MEINS
            FROM MARA
            WHERE MATNR = <FS_DATA>-TD_MATNR .

    ENDIF.

 ENDIF.
 "  *   稳定刷新
    LS_STABLE-row = 'X'." 基于行的稳定刷新
    LS_STABLE-col = 'X'." 基于列稳定刷新
    CALL METHOD GR_ALVGRID->refresh_table_display
      EXPORTING
        is_stable = LS_STABLE
      EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
