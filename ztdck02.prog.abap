REPORT ZTDTZ_PR.
"出库替代关系报表查询
" IT02
" 2016-04-20

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
    YJWL       TYPE MATNR,           "应计物料
    YJWLMS     TYPE MAKTX,            "应计物料描述
    YJSL       TYPE MENGE_D,          "应计数量
    SEL(1),
   END OF TY_DATA  .


TYPES:BEGIN OF TY_PROJ_SEL,
  MBLNR TYPE MBLNR,      "物料凭证号
  MJAHR TYPE MJAHR,              "物料凭证年度
  PSPID TYPE PS_PSPID,    "来源项目号
 END OF TY_PROJ_SEL.





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


   DATA:GT_CK_Z TYPE TABLE OF ZTDTZ_CK,
        GS_CK_Z TYPE ZTDTZ_CK .

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
       MSEG~SHKZG MSEG~WERKS
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

  SORT GT_ZMM024 BY  MATNR POSID .

  SELECT * INTO TABLE GT_CK
    FROM ZTDTZ_CK
    WHERE MBLNR IN S_MBLNR
    AND    MJAHR IN S_MJAHR
    AND    PSPID IN S_PSPID
    AND    WERKS EQ  P_BUKRS
    AND    MATNR IN S_MATNR
    AND    TD_GRPFLAG NE ''.

  SORT  GT_CK BY PSPID POSID TD_MATNR TD_GRPFLAG.

   GT_CK_Z = GT_CK .
   SORT  GT_CK_Z BY PSPID POSID MBLNR MJAHR  ZEILE .
   DELETE ADJACENT DUPLICATES FROM GT_CK COMPARING PSPID POSID TD_MATNR TD_GRPFLAG  .


  SELECT * APPENDING TABLE GT_CK
    FROM ZTDTZ_CK
    WHERE MBLNR IN S_MBLNR
    AND    MJAHR IN S_MJAHR
    AND    PSPID IN S_PSPID
    AND    WERKS EQ  P_BUKRS
    AND    MATNR IN S_MATNR
    AND    TD_GRPFLAG EQ ''.

     SORT  GT_CK BY  MBLNR MJAHR  ZEILE .



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



     "应计物料
       <FS_DATA>-YJWL = <FS_DATA>-MATNR .
       <FS_DATA>-YJWLMS = <FS_DATA>-MAKTX.
       <FS_DATA>-YJSL = <FS_DATA>-MENGE.

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
         "应计物料
       <FS_DATA>-YJWL = <FS_DATA>-TD_MATNR .
       <FS_DATA>-YJWLMS = <FS_DATA>-TD_MAKTX.
       <FS_DATA>-YJSL = <FS_DATA>-TD_MENGE.
       ELSE.
           READ TABLE GT_CK_Z INTO GS_CK_Z WITH KEY
               MBLNR = <FS_DATA>-MBLNR
               MJAHR = <FS_DATA>-MJAHR
               ZEILE = <FS_DATA>-ZEILE
               MATNR = <FS_DATA>-MATNR
               BINARY SEARCH .
         IF SY-SUBRC EQ 0 .
              READ TABLE GT_CK INTO GS_CK WITH KEY
               MBLNR = <FS_DATA>-MBLNR
               MJAHR = <FS_DATA>-MJAHR
               ZEILE = <FS_DATA>-ZEILE
               MATNR = <FS_DATA>-MATNR
               BINARY SEARCH .
            IF SY-SUBRC NE 0 .
              "替代物料
           <FS_DATA>-TD_MATNR = GS_CK_Z-TD_MATNR . "替代物料
           <FS_DATA>-TD_MEINS = GS_CK_Z-TD_MEINS.  "替代单位
           <FS_DATA>-TD_GRPFLAG = GS_CK_Z-TD_GRPFLAG. "替代组标识
           <FS_DATA>-TD_MENGE = GS_CK_Z-TD_MENGE.    "替代数量
              "被替代物料描述
           READ TABLE GT_MAKT INTO GS_MAKT
            WITH KEY MATNR = <FS_DATA>-TD_MATNR
               BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
             <FS_DATA>-TD_MAKTX = GS_MAKT-MAKTX .
         ENDIF.
          "应计物料
           <FS_DATA>-YJWL = <FS_DATA>-TD_MATNR .
           <FS_DATA>-YJWLMS = <FS_DATA>-TD_MAKTX.
            <FS_DATA>-YJSL = 0.

             ENDIF.
        ENDIF.


     ENDIF.


  "设计代码
    READ TABLE GT_ZMM024 INTO GS_ZMM024
                   WITH KEY MATNR = <FS_DATA>-YJWL
                            POSID = <FS_DATA>-PSPID BINARY SEARCH .
     IF SY-SUBRC EQ 0 .
          <FS_DATA>-SJDM = GS_ZMM024-SJDM.
     ENDIF.

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
  INIT_FIELDCAT 'BLART'         '移动类型'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MATNR'       '被替代物料'         '' '' '' '' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
 INIT_FIELDCAT 'TD_GRPFLAG'     '替代组标识'         '25' '' '' '' '' 'EBAN' 'MATNR'.
 INIT_FIELDCAT 'TD_MENGE'       '折算数'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJWL'       '应计物料'         '' '' '' '' '' 'MSEG' 'MATNR'.
  INIT_FIELDCAT 'YJWLMS'     '应计物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJSL'       '应计数量'         '' '' '' '' '' '' ''.
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
*  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
*  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
*  APPEND GW_EVENTS TO GT_EVENTS.
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
