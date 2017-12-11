REPORT ZTDTZ_PR.
"出库台账维护
" IT02
" 2016-04-18

TABLES:MSEG,MKPF,MAKT,PROJ,PRPS .

 TYPES:BEGIN OF TY_DATA ,
     PSPNR TYPE PS_POSNR,     "源PSPNR号

    MJAHR TYPE MJAHR,              "物料凭证年度
    MBLNR TYPE MBLNR,      "物料凭证号
    ZEILE TYPE MBLPO,      "物料凭证行项目
    BLDAT TYPE BLDAT,      "创建日期
    BUDAT TYPE BUDAT,       "过账日期
    WERKS TYPE WERKS_D,     "工厂
    PSPID TYPE PS_PSPID,    "项目号
    POST  TYPE PS_POST1,     "项目名称
    POSID TYPE PS_POSID,    "WBS
    SJDM  TYPE ZMM024-SJDM, "设计代码
    MATNR TYPE MATNR,       "物料号
    MAKTX TYPE MAKTX,       "物料描述
    MENGE TYPE MENGE_D,     "数量
    MEINS TYPE MEINS,       "单位
    TD_MATNR TYPE MATNR,              "被替代物料
    TD_MAKTX TYPE MAKTX,            "被替代物料描述
    TD_MEINS TYPE MEINS,            "被替代物料单位
    TD_GRPFLAG TYPE   C LENGTH 25,               "替代组表识
    TD_MENGE TYPE    EBAN-MENGE ,                "折算数
    BWART TYPE  BWART  ,  "移动类型
    SHKZG TYPE SHKZG,   "借贷标识
    UNAME TYPE SY-TCODE,            "维护账户
    UDATE TYPE DATS,                "维护日期
    UTIME TYPE TIMS,                "维护时间
    SEL(1),
   END OF TY_DATA  .


TYPES:BEGIN OF TY_PROJ_SEL,
  MBLNR TYPE MBLNR,      "物料凭证号
  MJAHR TYPE MJAHR,              "物料凭证年度
  PSPID TYPE PS_PSPID,    "来源项目号
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



   DATA:GS_PRPS TYPE PRPS,
        GT_PRPS TYPE TABLE OF PRPS.


   DATA:GT_T024 TYPE TABLE OF T024,
        GS_T024 TYPE T024.

   DATA:GT_ZMM024 TYPE TABLE OF ZMM024,
        GS_ZMM024 TYPE ZMM024.

   DATA: IT_LINES TYPE TABLE OF TLINE,
         WA_LINES TYPE TLINE.
   DATA: G_OBJNAME TYPE THEAD-TDNAME.

   DATA:GT_ZTDTZ_CK TYPE TABLE OF ZTDTZ_CK,
        GS_ZTDTZ_CK TYPE ZTDTZ_CK.



   DATA:GT_MAKT TYPE TABLE OF MAKT,
        GS_MAKT TYPE MAKT .

   DATA:GT_CK TYPE TABLE OF ZTDTZ_CK,
        GS_CK TYPE ZTDTZ_CK .

   DATA:GT_PROJ_SEL TYPE TABLE OF   TY_PROJ_SEL ,
        GS_PROJ_SEL TYPE TY_PROJ_SEL.

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



  IF &1 = 'TD_MATNR' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.

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
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY DEFAULT '1800' .                "公司代码
"P_MJAHR  TYPE MKPF-MJAHR OBLIGATORY DEFAULT SY-DATUM+0(4).           "物料凭证年度

*P_PSPID_F TYPE  PROJ-PSPID  OBLIGATORY,   "源项目号
*P_PSPID_T TYPE  PROJ-PSPID .         "目标项目号

SELECT-OPTIONS: " S_PSPID  FOR PROJ-PSPID ,   "过账日期
                S_PSPID FOR PROJ-PSPID," OBLIGATORY,  "项目号
                S_MJAHR  FOR MKPF-MJAHR ,  "物料凭证年度
                S_MBLNR  FOR MSEG-MBLNR,   "物料凭证号
                S_MATNR  FOR MSEG-MATNR,    "物料号
                S_BUDAT  FOR MKPF-BUDAT.   "过账日期

 PARAMETERS:  G_WH  TYPE CHAR1 RADIOBUTTON GROUP G1,
              G_XS  TYPE CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
   S_MJAHR-LOW = SY-DATUM+0(4).

   APPEND S_MJAHR .
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

IF  G_WH EQ 'X'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
     FROM PROJ
    WHERE PSPID IN S_PSPID
          AND WERKS EQ P_BUKRS.
  SORT GT_PROJ BY PSPNR.

     SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FROM PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR
       AND WERKS EQ P_BUKRS.
   SORT GT_PRPS BY PSPNR.


SELECT MKPF~MJAHR MKPF~MBLNR MKPF~BLDAT MKPF~BUDAT
       MSEG~ZEILE MSEG~MATNR MSEG~MENGE MSEG~MEINS
       MSEG~SHKZG MSEG~WERKS MSEG~BWART
      MSEG~MAT_PSPNR AS PSPNR
      INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  FROM MKPF
  INNER JOIN MSEG
  ON MKPF~MJAHR = MSEG~MJAHR
  AND MKPF~MBLNR = MSEG~MBLNR
  FOR ALL ENTRIES IN GT_PRPS
 WHERE MKPF~MBLNR IN S_MBLNR
  AND MKPF~MJAHR IN S_MJAHR
  AND MSEG~MATNR IN S_MATNR
   AND MSEG~WERKS EQ P_BUKRS
   AND MSEG~MAT_PSPNR EQ GT_PRPS-PSPNR
  AND MSEG~BWART  IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24') .

  SORT GT_DATA BY PSPNR  MBLNR MJAHR ZEILE .


  CHECK GT_DATA IS NOT INITIAL.

 SELECT * INTO TABLE GT_MAKT
   FROM MAKT
   FOR ALL ENTRIES IN GT_DATA
   WHERE MATNR = GT_DATA-MATNR .

   SORT GT_MAKT BY MATNR .

   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
     FROM ZMM024
     WHERE POSID  IN S_PSPID
      .

  SORT GT_ZMM024 BY MATNR POSID.

  SELECT * INTO TABLE GT_CK
    FROM ZTDTZ_CK
    WHERE MBLNR IN S_MBLNR
    AND    MJAHR IN S_MJAHR
    AND    PSPID IN S_PSPID
    AND    WERKS EQ  P_BUKRS
    AND    MATNR IN S_MATNR.

  SORT  GT_CK BY MBLNR MJAHR  ZEILE .
ELSE.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_DATA
     FROM  ZTDTZ_CK
    WHERE  MBLNR IN S_MBLNR
    AND    MJAHR IN S_MJAHR
    AND    PSPID IN S_PSPID
    AND    WERKS EQ  P_BUKRS
    AND    MATNR IN S_MATNR.
  SORT GT_DATA BY  MBLNR MJAHR  ZEILE .

  SELECT * INTO TABLE GT_PROJ
    FROM PROJ
    WHERE PSPID IN S_PSPID
  .
  SORT GT_PROJ BY PSPID .

  SELECT * INTO TABLE GT_MAKT
    FROM MAKT
    WHERE SPRAS = '1'
     AND   MATNR IN S_MATNR.

  SORT GT_MAKT BY MATNR .




ENDIF.
*   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T024
*     FROM T024
*    .
*   SORT GT_T024 BY EKGRP.
*
*   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
*     FROM ZMM024
*     WHERE POSID  EQ P_PSPID .
*
*   SORT GT_ZMM024 BY MATNR POSID.

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
 DATA: PSPID TYPE PS_PSPID,
       POST TYPE PS_POST1 ,
       POSID TYPE PS_POSID.
 IF G_WH EQ 'X'.
  LOOP AT GT_DATA ASSIGNING <FS_DATA> .

   AT NEW PSPNR .
     CLEAR:PSPID,POST,POSID.
       "项目号
   READ TABLE GT_PRPS INTO GS_PRPS
            WITH KEY PSPNR = <FS_DATA>-PSPNR
                    BINARY SEARCH.
   IF SY-SUBRC EQ 0 .
        "WBS号
           POSID = GS_PRPS-POSID.
           READ TABLE GT_PROJ INTO  GS_PROJ
             WITH KEY PSPNR = GS_PRPS-PSPHI
              BINARY SEARCH .
            IF SY-SUBRC EQ 0 .
               PSPID = GS_PROJ-PSPID.
               POST = GS_PROJ-POST1.
              ENDIF.
   ENDIF.


  ENDAT .
  "项目号
  <FS_DATA>-PSPID = PSPID.
  <FS_DATA>-POSID = POSID.
  <FS_DATA>-POST =  POST .


"物料描述
  READ TABLE GT_MAKT INTO GS_MAKT WITH KEY
                     MATNR = <FS_DATA>-MATNR BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
        <FS_DATA>-MAKTX = GS_MAKT-MAKTX.
    ENDIF.


  "设计代码
    READ TABLE GT_ZMM024 INTO GS_ZMM024
                   WITH KEY MATNR = <FS_DATA>-MATNR
                            POSID = <FS_DATA>-PSPID BINARY SEARCH .
     IF SY-SUBRC EQ 0 .
          <FS_DATA>-SJDM = GS_ZMM024-SJDM.
     ENDIF.

          READ TABLE GT_CK INTO GS_CK WITH KEY
               MBLNR = <FS_DATA>-MBLNR
               MJAHR = <FS_DATA>-MJAHR
               ZEILE = <FS_DATA>-ZEILE
               MATNR = <FS_DATA>-MATNR
               BINARY SEARCH .
     IF SY-SUBRC EQ 0 .
      <FS_DATA>-TD_MATNR = GS_CK-TD_MATNR . "替代物料
      <FS_DATA>-TD_MEINS = GS_CK-TD_MEINS.  "替代单位
      <FS_DATA>-TD_GRPFLAG = GS_CK-TD_GRPFLAG. "替代组标识
      <FS_DATA>-TD_MENGE = GS_CK-TD_MENGE.    "替代数量
         "被替代物料描述
       READ TABLE GT_MAKT INTO GS_MAKT
            WITH KEY MATNR = <FS_DATA>-TD_MATNR
               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
         <FS_DATA>-TD_MAKTX = GS_MAKT-MAKTX .
       ENDIF.

     ENDIF.

ENDLOOP.

ELSE.
      LOOP AT GT_DATA ASSIGNING <FS_DATA> .
       "源项目描述
     READ TABLE GT_PROJ INTO  GS_PROJ
                 WITH KEY PSPID = <FS_DATA>-PSPID
                  BINARY SEARCH .
              IF SY-SUBRC EQ 0 .
                <FS_DATA>-POST = GS_PROJ-POST1.
              ENDIF.
      "物料描述
       READ TABLE GT_MAKT INTO GS_MAKT
                WITH KEY MATNR = <FS_DATA>-MATNR
                 BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
             <FS_DATA>-MAKTX = GS_MAKT-MAKTX .

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
 IF  G_WH EQ 'X' .
 INIT_FIELDCAT 'MJAHR'        '物料凭证年度'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MBLNR'        '物料凭证号'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'ZEILE'         '物料凭证行项目'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'BLDAT'         '创建日期'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'BUDAT'         '过账日期'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'PSPID'        '项目号'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'POST'          '项目名称'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'POSID'         'WBS'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MATNR'         '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
 INIT_FIELDCAT 'MAKTX'         '物料描述'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MENGE'         '数量'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MEINS'          '单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'         '移动类型'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MATNR'       '被替代物料'         '' '' '' 'X' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_GRPFLAG'     '替代组标识'         '25' '' '' 'X' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MENGE'       '折算数'         '' '' '' 'X' '' 'MSEG' 'MENGE'.
 ELSE.
   INIT_FIELDCAT 'MJAHR'        '物料凭证年度'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MBLNR'        '物料凭证号'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'ZEILE'         '物料凭证行项目'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'PSPID'        '项目号'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'POST'          '项目名称'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'POSID'         'WBS'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MATNR'         '物料号'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MAKTX'         '物料描述'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MENGE'         '数量'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'MEINS'          '单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'         '移动类型'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MATNR'       '被替代物料'         '' '' '' 'X' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_GRPFLAG'     '替代组标识'         '25' '' '' 'X' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MENGE'       '折算数'         '' '' '' 'X' '' '' ''.
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

            LOOP AT GT_DATA INTO GS_DATA WHERE SEL = 'X' .
                 LOOP AT GT_DATA INTO GS_DATA WHERE  MBLNR =  GS_DATA-MBLNR
                                    AND  MJAHR = GS_DATA-MJAHR
                                    AND  PSPID = GS_DATA-PSPID
                                     AND   TD_MATNR NE '' .

                    GS_DATA-SEL = 'X'.


                    MODIFY GT_DATA FROM  GS_DATA .
                ENDLOOP.
            ENDLOOP.


          ELSE.

            MESSAGE '请选择要保存的相应物料凭证替代记录保存'  TYPE 'S'   DISPLAY LIKE 'E'.

         ENDIF.

      "检查必输项

        PERFORM CHECK_INPUT CHANGING L_SUBRC .

      IF L_SUBRC = '4'.
        EXIT.
      ENDIF.

      IF GT_PROJ_SEL IS NOT INITIAL.
       SORT GT_PROJ_SEL BY MBLNR MJAHR  PSPID .
       DELETE ADJACENT DUPLICATES FROM GT_PROJ_SEL COMPARING MBLNR MJAHR  PSPID .
        "先删除已选择物理凭证对应的记录台账信息
       LOOP AT GT_PROJ_SEL INTO GS_PROJ_SEL.

          "根据选择物料凭证、源项目号、目标项目号维度区分 删除已维护台账信息表
         DELETE FROM ZTDTZ_CK WHERE MBLNR = GS_PROJ_SEL-MBLNR
                              AND MJAHR = GS_PROJ_SEL-MJAHR
                              AND PSPID = GS_PROJ_SEL-PSPID
                            .

       ENDLOOP.
      ENDIF.


      "保存选择数据到 替代调拨台账维护表
      CHECK GS_DATA_SEL IS NOT INITIAL.

     CHECK L_SUBRC EQ 0.
      LOOP AT GT_DATA_SEL INTO GS_DATA_SEL .

        CLEAR GS_ZTDTZ_CK .
        MOVE-CORRESPONDING GS_DATA_SEL TO GS_ZTDTZ_CK.

        "维护账号
        GS_ZTDTZ_CK-UNAME = SY-UNAME.
        "维护日期
        GS_ZTDTZ_CK-UDATE = SY-DATUM.

        "维护时间
        GS_ZTDTZ_CK-UTIME = SY-UZEIT.

        APPEND GS_ZTDTZ_CK TO GT_ZTDTZ_CK.
      ENDLOOP.


      SORT GT_ZTDTZ_CK BY MBLNR MJAHR ZEILE .
       CHECK GT_ZTDTZ_CK IS NOT INITIAL.
      "保存数据到 ZTDTZ_PR.
       MODIFY ZTDTZ_CK FROM TABLE GT_ZTDTZ_CK .

           "先删除已选择物理凭证对应的记录台账信息
       LOOP AT GT_PROJ_SEL INTO GS_PROJ_SEL.

          "根据选择物料凭证、源项目号、目标项目号维度区分 删除且替代组且替代组标识且折算数量这三项都 为空已维护台账信息表
         DELETE FROM ZTDTZ_CK WHERE MBLNR = GS_PROJ_SEL-MBLNR
                              AND MJAHR = GS_PROJ_SEL-MJAHR
                              AND PSPID = GS_PROJ_SEL-PSPID
                              AND  TD_MATNR EQ ''
                              AND TD_GRPFLAG EQ ''
                              AND TD_MENGE EQ 0..

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
FORM SELXWBS  USING    P_BANFN
                       P_BNFPO
              CHANGING P_WBSCSWB.
CONCATENATE P_BANFN P_BNFPO INTO G_OBJNAME .

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'B01'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'EBAN'
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
" REFRESH GT_PROJ_PR.
  LOOP AT GT_DATA INTO GS_DATA
      WHERE SEL = 'X'.

*   IF GS_DATA-TD_MATNR EQ '' AND GS_DATA-TD_MENGE EQ '' AND  GS_DATA-TD_GRPFLAG EQ ''.
*      CONTINUE.
*     ENDIF.
*    IF  GS_DATA-TD_MATNR EQ ''.
*      MESSAGE '被替代物料不为空'  TYPE 'S' DISPLAY LIKE 'E'.
*         L_SUBRC = 4.
*    ENDIF.

    IF GS_DATA-TD_MATNR NE '' AND GS_DATA-TD_MENGE EQ 0 .
         MESSAGE '被替代物料不为空，对应记录的折算数不能为0'  TYPE 'S' DISPLAY LIKE 'E'.
         L_SUBRC = 4.
    ENDIF.
    CLEAR:GS_DATA_SEL .
     MOVE-CORRESPONDING GS_DATA TO GS_DATA_SEL.
     APPEND GS_DATA_SEL TO GT_DATA_SEL .
     MOVE-CORRESPONDING GS_DATA TO GS_PROJ_SEL.
     APPEND GS_PROJ_SEL TO GT_PROJ_SEL.
*     APPEND GS_PROJ_PR TO GT_PROJ_PR.
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
