REPORT ZCO005_5.
"非合同项目/整改项目对应关系维护
"add it02
"date:20160512


TABLES:VBAK,CSKS,AUFK,CSKT ,COAS.

TYPES:BEGIN OF TY_DATA,
      BUKRS   TYPE        BUKRS ,         "公司代码
      VBELN   TYPE        VBELN,          "销售订单
      XMMC    TYPE        CHAR300,        "项目名称
      AUART   TYPE        AUART,          "销售订单类型
      DYXSDD  TYPE        VBELN,           "对应销售订单
      DYXSMC  TYPE        CHAR300,         "对应销售订单名称
      DYNBDD  TYPE        AUFNR ,          "对应内部订单
      DYNBMC  TYPE        STRING,          "对应内部订单名称
      DYCBZX  TYPE        KOSTL,           "对应成本中心
      DYCBMC  TYPE        STRING,          "对应成本中心名称
      JSFLX   TYPE        C,                "接收方类型
      UNAME   TYPE       SY-TCODE,          "更改ITCODE
      UDATE   TYPE       D,                 "更改日期
      UTIME   TYPE       T,                 "更改时间
      SEL,
  END  OF  TY_DATA.

DATA:BEGIN OF TY_VBELN,
      VBELN TYPE VBELN,
      XMMC  TYPE CHAR300,
      AUART TYPE AUART,
      VKORG TYPE VKORG,
  END  OF TY_VBELN .

  TYPES:BEGIN OF TY_XSDD,
        VBELN   TYPE        VBELN,
        XMMC    TYPE        STRING,
         AUART   TYPE        AUART,
      END OF TY_XSDD.

  DATA:GS_DATA TYPE TY_DATA,
       GT_DATA TYPE TABLE OF TY_DATA.

   DATA:GS_DATA_SEL TYPE TY_DATA,
       GT_DATA_SEL TYPE TABLE OF TY_DATA.


  DATA:GT_CSKT TYPE  TABLE OF CSKT,
       GS_CSKT TYPE CSKT .

  DATA:GT_COAS TYPE TABLE OF COAS,
       GS_COAS TYPE COAS.

  DATA:GT_VBFA TYPE TABLE OF VBFA,
       GS_VBFA TYPE VBFA.

  DATA:GT_ZCO005_5 TYPE TABLE  OF ZCO005_5,
       GS_ZCO005_5 TYPE  ZCO005_5.

DATA:GT_T001 TYPE TABLE OF T001,
      GS_T001 TYPE T001.

DATA:GT_VBAK LIKE TABLE OF TY_VBELN WITH HEADER LINE ,
     GS_VBAK LIKE  TY_VBELN.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

DATA:GT_SAVE TYPE TABLE OF ZCO005_5,
     GS_SAVE TYPE ZCO005_5.

 "声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_onf4 for event onf4 of cl_gui_alv_grid
     importing e_fieldname es_row_no er_event_data et_bad_cells e_display,
     handle_modify  FOR EVENT data_changed_finished OF cl_gui_alv_grid
     IMPORTING e_modified  et_good_cells ."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION


 FIELD-SYMBOLS: <FS_DATA>  LIKE GS_DATA .

  DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  GW_LVC-KEY = &4.
*  IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
*  IF &1 = 'DQCPCB' OR &1 = 'DYGCFY' ."OR  &1 = 'YZCB'.
*  GW_LVC-CFIELDNAME = 'WAERS'.
* ENDIF.
* IF &1 = 'YZCB'.
*   GW_LVC-NO_SIGN = ''.
* ENDIF.
 IF &1 = 'DYXSDD' OR &1 = 'DYNBDD' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_TABLE = &8.
  GW_LVC-REF_FIELD = &9.
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
DATA gt_event_receiver TYPE REF TO lcl_event_receiver .



DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.
DATA: GW_ISTABLE TYPE LVC_S_STBL.

  DATA: LT_RET_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        LS_RET_TAB TYPE DDSHRETVAL .


" ************************************************************************
* BAPI
************************************************************************
DATA: WA_DOCUMENTHEADER    TYPE BAPIACHE09,         "表头
      IT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,  "总账
      WA_ACCOUNTGL         TYPE BAPIACGL09,
      IT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,  "客户
      WA_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      IT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,  "货币项目
      WA_CURRENCYAMOUNT    TYPE BAPIACCR09,
      IT_CRITERIA          TYPE TABLE OF BAPIACKEC9,  "分配-科目
      WA_CRITERIA          TYPE BAPIACKEC9,
      IT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      WA_VALUEFIELD        TYPE BAPIACKEV9,
      IT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      WA_EXTENSION2        TYPE BAPIPAREX,
      IT_RETURN            TYPE TABLE OF BAPIRET2,
      WA_RETURN            TYPE BAPIRET2.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: WA_ZACCDOCUEXT TYPE ZACCDOCUEXT.

************************************************************************
* GLOBAL VARIANT
************************************************************************

************************************************************************
* CONSTANT
************************************************************************

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN.    "销售订单
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
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  "取出ZF1、ZZG的项目订单抬头信息
SELECT VBELN BUKRS_VF AS BUKRS  AUART
 INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  FROM VBAK
  WHERE VBELN IN S_VBELN
  AND BUKRS_VF EQ P_BUKRS
  AND AUART IN ('ZF1','ZZG').

SORT GT_DATA BY  BUKRS VBELN .

"取出已维护销售订单对应维护信息
  SELECT  * INTO  TABLE GT_ZCO005_5
    FROM ZCO005_5
    WHERE  BUKRS EQ P_BUKRS
    AND VBELN IN S_VBELN.
  SORT GT_ZCO005_5 BY BUKRS VBELN .
  IF GT_ZCO005_5 IS NOT INITIAL.
    "取出成本中心主数据
    SELECT * INTO TABLE GT_CSKT
       FROM CSKT
       FOR  ALL  ENTRIES IN GT_ZCO005_5
       WHERE KOSTL = GT_ZCO005_5-DYCBZX
       AND  KOKRS = '1000'
       AND  SPRAS = '1'.
     SORT  GT_CSKT BY  KOSTL .
     "取出内部订单信息
     SELECT * INTO TABLE GT_COAS
       FROM COAS
       FOR  ALL  ENTRIES IN GT_ZCO005_5
       WHERE AUFNR = GT_ZCO005_5-DYNBDD .
     SORT  GT_COAS BY AUFNR .
  ENDIF.
  "取出销售伙伴信息
  SELECT * INTO TABLE GT_VBFA
    FROM VBFA
    FOR  ALL ENTRIES IN  GT_DATA
    WHERE VBELN = GT_DATA-VBELN .
 SORT GT_VBFA BY VBELN .

 "取出销售订单抬头基本信息
   SELECT VBELN AUART VKORG
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
    FROM VBAK
    WHERE  BUKRS_VF = P_BUKRS.

  SORT GT_VBAK BY  VBELN VKORG AUART .

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
   "读取销售项目信息
 LOOP AT GT_VBAK INTO  GS_VBAK .
  "项目名称
   PERFORM SELXMMC USING GS_VBAK-VBELN CHANGING GS_VBAK-XMMC.
     MODIFY GT_VBAK FROM GS_VBAK.
 ENDLOOP.

  LOOP  AT GT_DATA ASSIGNING <FS_DATA>.
    "项目名称
   "项目名称
    READ TABLE  GT_VBAK INTO GS_VBAK
     WITH KEY VBELN = <FS_DATA>-VBELN
              BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
        <FS_DATA>-XMMC = GS_VBAK-XMMC.
    ENDIF.
   "PERFORM SELXMMC USING <FS_DATA>-VBELN CHANGING <FS_DATA>-XMMC.
   "对应销售订单
   READ TABLE GT_ZCO005_5 INTO GS_ZCO005_5
     WITH KEY BUKRS = <FS_DATA>-BUKRS
              VBELN = <FS_DATA>-VBELN
              BINARY SEARCH.
    IF SY-SUBRC EQ 0 .
      "取出对应的销售订单
       <FS_DATA>-DYXSDD = GS_ZCO005_5-DYXSDD.
       IF <FS_DATA>-DYXSDD IS NOT INITIAL.
            "取出项目名称
             READ TABLE  GT_VBAK INTO  GS_VBAK
                WITH KEY VBELN = <FS_DATA>-DYXSDD
                 BINARY SEARCH .
             IF SY-SUBRC EQ 0 .
               <FS_DATA>-DYXSMC = GS_VBAK-XMMC.
              ENDIF.
          "  PERFORM SELXMMC USING <FS_DATA>-DYXSDD CHANGING <FS_DATA>-DYXSMC.
            "取出接收方类型
            <FS_DATA>-JSFLX = GS_ZCO005_5-JSFLX.
            CONTINUE.
        ELSE.
          "读取内部订单
           <FS_DATA>-DYNBDD = GS_ZCO005_5-DYNBDD.
          READ TABLE GT_COAS INTO GS_COAS
            WITH KEY AUFNR = <FS_DATA>-DYNBDD
                     BINARY SEARCH.
          IF SY-SUBRC EQ 0 .
             <FS_DATA>-DYNBMC = GS_COAS-KTEXT .
           ENDIF.
          "读取成本中心
        <FS_DATA>-DYCBZX = GS_ZCO005_5-DYCBZX.
        READ TABLE GT_CSKT INTO  GS_CSKT
          WITH KEY KOSTL = <FS_DATA>-DYCBZX
                           BINARY SEARCH .
         IF SY-SUBRC EQ 0.
            <FS_DATA>-DYCBMC = GS_CSKT-KTEXT .
         ENDIF.
        <FS_DATA>-JSFLX = GS_ZCO005_5-JSFLX.
            CONTINUE.

       ENDIF.


    ELSE.
      "读取整改项目对应的销售伙伴信息
      READ TABLE  GT_VBFA INTO GS_VBFA
            WITH KEY VBELN = <FS_DATA>-VBELN
            BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
         <FS_DATA>-DYXSDD = GS_VBFA-VBELV .
              READ TABLE  GT_VBAK INTO  GS_VBAK
                WITH KEY VBELN = <FS_DATA>-DYXSDD
                 BINARY SEARCH .
             IF SY-SUBRC EQ 0 .
               <FS_DATA>-DYXSMC = GS_VBAK-XMMC.
              ENDIF.
        " PERFORM SELXMMC USING <FS_DATA>-DYXSDD CHANGING <FS_DATA>-DYXSMC.
         <FS_DATA>-JSFLX = 'S'.
         CONTINUE.
      ENDIF.
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
  GW_LAYOUT-BOX_FNAME    = 'SEL'.
 " GW_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
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
  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBELN'          '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'           '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'          '销售订单类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYXSDD'         '对应销售销售订单'         '' '' '' 'X' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'DYXSMC'         '对应销售销售订单名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYNBDD'         '对应内部订单'         '' '' '' 'X' '' 'COAS' 'AUFNR'.
  INIT_FIELDCAT 'DYNBMC'         '对应内部订单名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYCBZX'         '对应成本中心'         '' '' '' 'X' '' 'CSKS' 'KOSTL'.
  INIT_FIELDCAT 'DYCBMC'         '对应成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JSFLX'          '接收方类型'         '' '' '' '' '' '' ''.
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
  GW_EVENTS-NAME =  'CALLER_EXIT' .
  GW_EVENTS-FORM =  'FRM_BUTTON'.   "f4事件
  APPEND GW_EVENTS TO GT_EVENTS.
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
*      -->P_GT_TOTAL  text
*      -->P_0517   text
*      -->P_0518   text
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
ENDFORM.                    " FRM_OUTPUT
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_VBELN  text
*      <--P_<FS_DATA>_XMMC  text
*----------------------------------------------------------------------*
FORM SELXMMC  USING    P_VBELN TYPE VBELN
              CHANGING P_XMMC TYPE CHAR300.
  " 取项目名称 - 销售订单抬头文本
    G_OBJNAME = P_VBELN.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'Z001'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'VBBK'
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
      READ TABLE IT_LINES INTO WA_LINES INDEX 1.
      IF SY-SUBRC = 0.
       P_XMMC = WA_LINES-TDLINE.
      ENDIF.
    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0441   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_modify.
     perform handle_data_changed_finished using e_modified et_good_cells.
  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_onf4.
      FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.

   CASE e_fieldname.
     WHEN 'DYNBDD'.
        READ TABLE GT_DATA INTO GS_DATA INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_DYNBDD CHANGING GS_DATA-DYNBDD.
            IF GS_DATA-DYNBDD IS NOT INITIAL.
              MODIFY GT_DATA FROM GS_DATA INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'DYNBDD'.
               LW_MOD_CELL-VALUE = GS_DATA-DYNBDD.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.
      WHEN 'DYXSDD'.
       READ TABLE GT_DATA INTO GS_DATA INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
              CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                RETFIELD        = 'VBELN' "大写,可选值内表的字段名
                VALUE_ORG       = 'S' "就写'S'
         "*      DYNPPROG       = SY-REPID "返回的输入框所在的MAIN PROGRAM
         "*      DYNPNR      = SY-DYNNR "返回的输入框所在屏幕
         "*      DYNPROFIELD     = 'GS_DATA-DYXSDD' "返回的输入框名
                WINDOW_TITLE    = '销售订单'
          TABLES
               VALUE_TAB       = GT_VBAK "可选值的内表
               RETURN_TAB      = LT_RET_TAB "返回值内表
          EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
         OTHERS          = 3.
  IF SY-SUBRC = 0.
      READ TABLE LT_RET_TAB INTO LS_RET_TAB INDEX 1 .
        IF SY-SUBRC = 0.
           ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
          LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'DYXSDD'.
               LW_MOD_CELL-VALUE = LS_RET_TAB-FIELDVAL.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.
         ENDIF.
    ELSE.

  ENDIF.
 ENDIF.

   ENDCASE.

  "**  Inform ALV Grid that event 'onf4' has been processed
  er_event_data->M_EVENT_HANDLED = 'X'.           "告知F4动作结束
  ENDMETHOD.
ENDCLASS.

FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

 " DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GR_ALVGRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.

    WHEN '&DATA_SAVE'.
    DATA L_SUBRC TYPE C."检查输入项
    DATA G_ANSWER     TYPE STRING. "控制弹出框
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
      REFRESH GT_DATA_SEL .
        READ TABLE GT_DATA INTO GS_DATA WITH KEY SEL = 'X'.
         IF SY-SUBRC EQ 0 .
          "  SORT GT_DATA BY BANFN BNFPO .
            LOOP AT GT_DATA INTO GS_DATA WHERE SEL = 'X' .
               IF GS_DATA-DYXSDD NE '' AND ( GS_DATA-DYNBDD NE '' OR  GS_DATA-DYCBZX NE '') .
                   MESSAGE '对应销售订单和对应内部订单、对应成本中心不能同时维护!'  TYPE 'S'   DISPLAY LIKE 'E'.
                    L_SUBRC = 4 .
                    EXIT.
               ENDIF.
               IF ( GS_DATA-DYNBDD NE '' AND  GS_DATA-DYCBZX EQ '')  OR ( GS_DATA-DYNBDD EQ '' AND  GS_DATA-DYCBZX NE '') .
                MESSAGE '对应内部订单、对应成本中心必须同时维护!'  TYPE 'S'   DISPLAY LIKE 'E'.
                 L_SUBRC = 4 .
                 EXIT.
               ENDIF.

                IF  GS_DATA-DYXSDD EQ '' AND GS_DATA-DYNBDD EQ  '' AND   GS_DATA-DYCBZX EQ '' .

                 ELSEIF GS_DATA-JSFLX EQ ''.

                         MESSAGE '接收方类型验证不通过，请检查！'  TYPE 'S'   DISPLAY LIKE 'E'.
                         L_SUBRC = 4 .
                         EXIT.

                ENDIF.

                  CLEAR:GS_DATA_SEL.
                  MOVE-CORRESPONDING GS_DATA TO GS_DATA_SEL .
                  GS_DATA_SEL-UNAME = SY-UNAME.
                  GS_DATA_SEL-UDATE = SY-DATUM.
                  GS_DATA_SEL-UTIME = SY-UZEIT.
                  APPEND  GS_DATA_SEL TO GT_DATA_SEL .
            ENDLOOP.

          ELSE.

            MESSAGE '请选择要保存销售订单'  TYPE 'S'   DISPLAY LIKE 'E'.

         ENDIF.
   CHECK L_SUBRC EQ 0 .
   IF GT_DATA_SEL IS NOT INITIAL.
      REFRESH GT_SAVE.
      MOVE-CORRESPONDING GT_DATA_SEL TO GT_SAVE.
      SORT GT_SAVE BY BUKRS  VBELN .
      MODIFY ZCO005_5 FROM TABLE  GT_SAVE.
      DELETE FROM  ZCO005_5 WHERE BUKRS = P_BUKRS AND DYNBDD EQ '' AND DYCBZX EQ '' AND DYXSDD EQ ''.
      MESSAGE '数据保存成功!' TYPE 'S'.

     ELSE.
         MESSAGE '无数据保存!' TYPE 'E'.
   ENDIF.
         "刷新数据到内表
   CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_DYXSDD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_DATA_DYXSDD  text
*----------------------------------------------------------------------*
FORM SUB_HELP_DYNBDD  CHANGING P_DYNBDD.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'ORDE'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_DYNBDD
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM FRM_BUTTON  USING E_GRID TYPE SLIS_DATA_CALLER_EXIT.
  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.

  LS_F4-FIELDNAME = 'DYNBDD'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

  LS_F4-FIELDNAME = 'DYXSDD'.      "F4对应的栏位
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
DATA:NBDD_LX TYPE AUART ,   "内部订单类型
     CBZX_LX TYPE KOSAR.    "成本中心类型
DATA:L_SY TYPE  C .
"对应内部订单描述
*READ TABLE   P_ET_GOOD_CELLS INTO LW_CELL INDEX 1."  WITH KEY FIELDNAME = 'DYNBDD'.
* IF SY-SUBRC EQ 0 .
LOOP AT  P_ET_GOOD_CELLS INTO LW_CELL  .
  CLEAR:L_SY .
   L_SY  = 1 .
   CASE LW_CELL-FIELDNAME .
     WHEN 'DYNBDD'.
      READ TABLE GT_DATA ASSIGNING <FS_DATA> INDEX  LW_CELL-ROW_ID.
      IF SY-SUBRC EQ 0 .
       CLEAR:<FS_DATA>-DYNBMC ,<FS_DATA>-DYXSDD,<FS_DATA>-DYXSMC,NBDD_LX,CBZX_LX,<FS_DATA>-JSFLX.
       SELECT SINGLE  KTEXT AUART  INTO ( <FS_DATA>-DYNBMC ,NBDD_LX )
             FROM COAS
             WHERE AUFNR = <FS_DATA>-DYNBDD.
       "接收方类型
        IF NBDD_LX = 'Z001'.
           <FS_DATA>-JSFLX = 'E'.
         ELSEIF NBDD_LX = 'Z003' AND <FS_DATA>-DYCBZX NE ''.
           SELECT SINGLE KOSAR  INTO CBZX_LX
               FROM CSKS
               WHERE KOSTL = <FS_DATA>-DYCBZX.
            IF CBZX_LX EQ 'V'.
                <FS_DATA>-JSFLX = 'V'.
             ENDIF.

         ENDIF.

      ENDIF.
     WHEN 'DYCBZX'.
         READ TABLE GT_DATA ASSIGNING <FS_DATA> INDEX  LW_CELL-ROW_ID.
     IF SY-SUBRC EQ 0 .
       CLEAR:<FS_DATA>-DYCBMC ,<FS_DATA>-DYXSDD,<FS_DATA>-DYXSMC,<FS_DATA>-JSFLX,<FS_DATA>-DYXSMC,NBDD_LX,CBZX_LX.
       SELECT SINGLE KTEXT INTO <FS_DATA>-DYCBMC
             FROM CSKT
             WHERE KOSTL = <FS_DATA>-DYCBZX.
         "接收方类型
        IF <FS_DATA>-DYNBDD NE ''.
          SELECT SINGLE   AUART  INTO NBDD_LX
             FROM COAS
             WHERE AUFNR = <FS_DATA>-DYNBDD.
            IF  NBDD_LX  EQ 'Z003'.
               SELECT SINGLE KOSAR  INTO CBZX_LX
               FROM CSKS
               WHERE KOSTL = <FS_DATA>-DYCBZX.
            IF CBZX_LX EQ 'V'.
                <FS_DATA>-JSFLX = 'V'.
             ENDIF.

            ENDIF.
         ENDIF.

    ENDIF.
    WHEN 'DYXSDD'.
      READ TABLE GT_DATA ASSIGNING <FS_DATA> INDEX  LW_CELL-ROW_ID.
        IF SY-SUBRC EQ 0 .
           CLEAR:<FS_DATA>-DYXSMC,<FS_DATA>-DYNBDD,<FS_DATA>-DYNBMC,<FS_DATA>-DYCBZX,<FS_DATA>-JSFLX,<FS_DATA>-DYCBMC,<FS_DATA>-JSFLX.
           READ TABLE GT_VBAK INTO GS_VBAK
              WITH KEY VBELN = <FS_DATA>-DYXSDD
                       BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
              <FS_DATA>-DYXSMC = GS_VBAK-XMMC .

           ENDIF.
          "接收方类型
        <FS_DATA>-JSFLX = 'S'.
         ENDIF.
   ENDCASE.
ENDLOOP.
 IF  L_SY  EQ '1'.
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
ENDIF.
" ENDIF.


ENDFORM.


FORM FRM_DATA_CHANGED USING   ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
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
*&      Form  SUB_HELP_DYXSDD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_DATA_DYNBDD  text
*----------------------------------------------------------------------*
FORM SUB_HELP_DYXSDD  CHANGING P_DYXSDD.


*  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
*    EXPORTING
**     DISPLAY                   = ' '
**     FIELDNAME                 = ' '
**     INPUT_VALUE               = ' '
*      MATCHCODE_OBJECT          = 'ORDE'
**     TABNAME                   = ' '
*    IMPORTING
*      SELECT_VALUE              = P_DYXSDD
*    EXCEPTIONS
*      INVALID_DICTIONARY_FIELD  = 1
*      INVALID_MATCHDCODE_OBJECT = 2
*      NO_SELECTION              = 3
*      OTHERS                    = 4.
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.
* CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*     RETFIELD        = 'VBELN' "大写,可选值内表的字段名
*      VALUE_ORG       = 'S' "就写'S'
*      DYNPPROG        = SY-REPID "返回的输入框所在的MAIN PROGRAM
*      DYNPNR          = SY-DYNNR "返回的输入框所在屏幕
*      DYNPROFIELD     = 'GS_DATA-DYXSDD' "返回的输入框名
*      WINDOW_TITLE    = '销售订单'
*    TABLES
*      VALUE_TAB       = GT_VBAK "可选值的内表
*    EXCEPTIONS
*      PARAMETER_ERROR = 1
*      NO_VALUES_FOUND = 2
*      OTHERS          = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.
