*&---------------------------------------------------------------------*
*& Report  ZPP013
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/5
*& Request       :
*& Descriptions  : 物料替代明细表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZPP013.
************************************************************************
* Tables
************************************************************************
TABLES:MAST,STKO,STPO.
************************************************************************
* Type Declaration
************************************************************************
*TYPES:BEGIN OF TY_DATA,
*        WEKRS   TYPE MAST-WERKS, "工厂
*        STLAN   TYPE MAST-STLAN, "BOM用途
*        STLAL   TYPE MAST-STLAL, "可选BOM
*        STLST   TYPE STKO-STLST, "BOM状态
*        DATUV   TYPE STPO-DATUV, "有效起始日期
*        MATNR   TYPE MAST-MATNR, "物料编码
*        MAKTX_1 TYPE MAKT-MAKTX, "物料描述
*        BMENG   TYPE STKO-BMENG, "基本数量
*        BMEIN   TYPE STKO-BMEIN, "基本计量数量
*        ALPGR   TYPE STPO-ALPGR, "替代项目组
*        IDNRK   TYPE STPO-IDNRK, "组建物料号
*        MAKTX   TYPE MAKT-MAKTX, "描述
*        MENGE   TYPE STPO-MENGE, "数量
*        MEINS   TYPE STPO-MEINS, "单位
*        ALPRF   TYPE STPO-ALPRF, "优先级
*        ALPST   TYPE STPO-ALPST, "策略
*        EWAHR   TYPE STPO-EWAHR, "使用可能性
*      END OF TY_DATA.

TYPES:BEGIN OF TY_DATA.
        INCLUDE TYPE ZPP013.
TYPES:ZBOX  TYPE C,
      END OF TY_DATA.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_STAS TYPE TABLE OF STAS.
DATA GS_STAS TYPE STAS.
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
SELECT-OPTIONS: S_MATNR  FOR MAST-MATNR,"组建物料号
                S_IDNRK   FOR STPO-IDNRK,"替代物料号
                S_WERKS   FOR MAST-WERKS OBLIGATORY DEFAULT '1100',"工厂
                S_STLAL   FOR MAST-STLAL,"可选BOM
                S_STLAN   FOR MAST-STLAN,"BOM用途
                S_STLST   FOR STKO-STLST DEFAULT '1' TO '2'."BOM状态
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
  PERFORM FRM_AUTH_CHECK.
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  S_WERKS-LOW.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH S_WERKS-LOW.
  ENDIF.
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
  SELECT * FROM ZPP013
   INTO CORRESPONDING FIELDS OF TABLE GT_DATA
   WHERE MATNR IN S_MATNR
   AND   IDNRK IN S_IDNRK
   AND   WERKS IN S_WERKS
   AND   STLAL IN S_STLAL
   AND   STLAN IN S_STLAN
   AND   STLST IN S_STLST
   AND   ALPGR <>  ''. "排除替代组为空

  CHECK GT_DATA IS NOT INITIAL.

  SELECT * FROM STAS
   INTO CORRESPONDING FIELDS OF TABLE GT_STAS
   FOR ALL ENTRIES IN GT_DATA
   WHERE STLTY = GT_DATA-STLTY
   AND   STLNR = GT_DATA-STLNR
   AND   STLAL = GT_DATA-STLAL.

  SORT GT_DATA  ASCENDING BY MATNR STLAL ALPGR ALPST ALPRF.
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
*如果有打上删除标记删除 删除之前的行
  LOOP AT  GT_DATA INTO GS_DATA.

    IF GS_DATA-LKENZ = 'X' .
    DELETE GT_DATA WHERE STLTY = GS_DATA-STLTY
                   AND   STLNR = GS_DATA-STLNR
                   AND   STLKN = GS_DATA-STLKN
                   AND   LKENZ = ''.
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

  INIT_FIELDCAT 'WERKS'          '工厂'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STLAN'          'BOM用途'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STLAL'          '可选BOM'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STLST'          'BOM状态'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料编码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX_1'        '物料描述'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BMENG'          '基本数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BMEIN'          '基本计量数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ALPGR'          '替代项目组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'IDNRK'          '组件物料号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '描述'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '单位'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ALPRF'          '优先级'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ALPST'          '策略'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EWAHR'          '使用可能性'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DATUV'          '有效起始日期'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LKENZ'          '删除标识'           '' '' '' '' '' '' ''.
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

*打印
    WHEN '&PRNT'.
*需进行保存操作
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          E_GRID = G_REF_GRID.

*      et_fieldcat_lvc = gt_fieldcat
      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 1.
        MESSAGE S001(ZMM01)  DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE GT_DATA INTO GS_DATA
        WITH KEY ZBOX = 'X'.
        IF SY-SUBRC = 0.
*          PERFORM FRM_PRINT_DATA.
        ELSE.
          MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        ENDIF.
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
