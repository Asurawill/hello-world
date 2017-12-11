*&---------------------------------------------------------------------*
*& Report  ZPP013
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/5
*& Request       :
*& Descriptions  : 生产订单报工面积查询报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZPP019.
************************************************************************
* Tables
************************************************************************
TABLES:AFRU,MSEG,AUFK,AFKO.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        BUDAT  TYPE AFRU-BUDAT, "记账日期
        WERKS  TYPE AFRU-WERKS, "工厂
        RUECK  TYPE AFRU-RUECK, "确认号
        RMZHL  TYPE AFRU-RMZHL, "计数器
        GAMNG  TYPE AFKO-GAMNG, "订单总数
        GMNGA  TYPE AFRU-GMNGA, "报工数量
        BWART  TYPE MSEG-BWART, "移动类型
        MEINS  TYPE MARA-MEINS, "基本计量单位
        DDZMJ  TYPE VBAP-ZSD0107, "订单总面积
        BGZMJ  TYPE VBAP-ZSD0107, "报工总面积
        BGMJBL TYPE VBAP-ZSD0107, "报工面积比率
        ATWRT  TYPE AUSP-ATWRT, "产品类型
        MATNR  TYPE MSEG-MATNR, "物料号
        MAKTX  TYPE MAKT-MAKTX, "物料描述
        MTART  TYPE MARA-MTART, "物料类型
        HOEHE  TYPE MARA-HOEHE, "高度
        BREIT  TYPE MARA-BREIT, "宽度
        MEABM  TYPE MARA-MEABM, "尺寸单位
        KDAUF  TYPE AUFK-KDAUF, "销售订单
        KDPOS  TYPE AUFK-KDPOS, "销售订单行项目
        AUFNR  TYPE AFRU-AUFNR, "生产订单号
        VORNR  TYPE AFRU-VORNR, "订单工序
        AUART  TYPE AUFK-AUART, "生产订单类型
        GSTRP  TYPE AFKO-GSTRP, "基本开始日期
        GLTRP  TYPE AFKO-GLTRP, "基本完成日期
        WABLNR TYPE AFRU-WABLNR, "物料凭证
        ERNAM  TYPE AFRU-ERNAM,  "报工人员
      END OF TY_DATA.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_AFKO TYPE TABLE OF AFKO.
DATA GS_AFKO TYPE AFKO.

DATA GT_AUFK TYPE TABLE OF AUFK.
DATA GS_AUFK TYPE AUFK.

DATA GT_MARA TYPE TABLE OF MARA.
DATA GS_MARA TYPE MARA.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

DATA GT_AFRU TYPE TABLE OF AFRU.
DATA GS_AFRU TYPE AFRU.

DATA GT_MARC TYPE TABLE OF MARC.
DATA GS_MARC TYPE MARC.

DATA GT_MSEG TYPE TABLE OF MSEG.
DATA GS_MSEG TYPE MSEG.

DATA:L_OBJECT LIKE BAPI1003_KEY-OBJECT.
*分类视图
DATA:
  IT_NUM  LIKE BAPI1003_ALLOC_VALUES_NUM  OCCURS 0 WITH HEADER LINE,
  IT_CHAR LIKE BAPI1003_ALLOC_VALUES_CHAR OCCURS 0 WITH HEADER LINE,
  IT_CURR LIKE BAPI1003_ALLOC_VALUES_CURR OCCURS 0 WITH HEADER LINE,
  IT_RET  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
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
SELECT-OPTIONS: S_WERKS   FOR AFRU-WERKS OBLIGATORY DEFAULT '1100',"工厂
                S_MATNR   FOR MSEG-MATNR,"物料号
                S_AUFNR   FOR AFRU-AUFNR,"订单号
                S_AUART   FOR AUFK-AUART,"订单类型
                S_DISPO   FOR AFKO-DISPO,"MRP控制者
                S_FEVOR   FOR AFKO-FEVOR,"生产管理员
                S_BUDAT   FOR AFRU-BUDAT,"记账日期
                S_ERNAM   FOR AFRU-ERNAM."报工人员
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

*查询报工数据
  SELECT * FROM AFRU
   INTO CORRESPONDING FIELDS OF TABLE GT_AFRU
   WHERE WERKS IN S_WERKS
   AND   AUFNR IN S_AUFNR
   AND   BUDAT IN S_BUDAT
   AND   ERNAM IN S_ERNAM.

  CHECK GT_AFRU IS NOT INITIAL.

* 查询物料凭证
  SELECT * FROM MSEG
    INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
    FOR ALL ENTRIES IN GT_AFRU
    WHERE MJAHR = GT_AFRU-MYEAR
    AND   MBLNR = GT_AFRU-WABLNR
    AND   ( BWART = '101'
    OR    BWART = '102' ).

*查询工单
  SELECT * FROM AFKO
   INTO CORRESPONDING FIELDS OF TABLE GT_AFKO
   FOR ALL ENTRIES IN GT_AFRU
   WHERE AUFNR  = GT_AFRU-AUFNR
   AND   PLNBEZ IN S_MATNR
   AND   DISPO  IN S_DISPO
   AND   FEVOR  IN S_FEVOR.

*查询工单类型
  SELECT * FROM AUFK
   INTO CORRESPONDING FIELDS OF TABLE GT_AUFK
    FOR ALL ENTRIES IN GT_AFKO
    WHERE AUFNR = GT_AFKO-AUFNR
    AND   AUART IN S_AUART.

*查询物料
  SELECT * FROM MARA
    INTO CORRESPONDING FIELDS OF TABLE GT_MARA
    FOR ALL ENTRIES IN GT_AFKO
    WHERE MATNR = GT_AFKO-PLNBEZ.

*查询物料反冲
  SELECT * FROM MARC
    INTO CORRESPONDING FIELDS OF TABLE GT_MARC
    FOR ALL ENTRIES IN GT_MARA
    WHERE MATNR = GT_MARA-MATNR.

*查看物料描述
  SELECT * FROM MAKT
   INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
   FOR ALL ENTRIES IN GT_MARA
   WHERE MATNR = GT_MARA-MATNR.

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

  LOOP AT GT_AFRU INTO GS_AFRU.

*排除物料凭证类型为空的物料
    IF GS_AFRU-WABLNR IS INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE GT_MSEG INTO GS_MSEG
    WITH KEY  MJAHR = GS_AFRU-MYEAR
              MBLNR = GS_AFRU-WABLNR.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ELSE.
      GS_DATA-BWART = GS_MSEG-BWART.
    ENDIF.

    MOVE-CORRESPONDING GS_AFRU TO GS_DATA.

    READ TABLE GT_AFKO INTO GS_AFKO
    WITH KEY AUFNR = GS_AFRU-AUFNR.
    IF SY-SUBRC = 0.
      GS_DATA-GAMNG = GS_AFKO-GAMNG.
      GS_DATA-GSTRP = GS_AFKO-GSTRP.
      GS_DATA-GLTRP = GS_AFKO-GLTRP.
      GS_DATA-MATNR = GS_AFKO-PLNBEZ.
    ELSE.
      CONTINUE.
    ENDIF.

*移动类型102显示负数
    IF GS_DATA-BWART = '102'.
      GS_DATA-GMNGA = GS_DATA-GMNGA * -1.
    ENDIF.

    READ TABLE GT_AUFK INTO GS_AUFK
    WITH KEY AUFNR = GS_DATA-AUFNR.
    IF SY-SUBRC = 0.
      GS_DATA-KDAUF = GS_AUFK-KDAUF.
      GS_DATA-KDPOS = GS_AUFK-KDPOS.
      GS_DATA-AUART = GS_AUFK-AUART.
    ENDIF.

*排除反冲料
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_AFKO-PLNBEZ
             WERKS = GS_AUFK-WERKS.
    IF SY-SUBRC = 0 AND GS_MARC-RGEKZ = 'X'.
      CONTINUE.
    ENDIF.

    READ TABLE GT_MARA INTO GS_MARA
    WITH KEY MATNR = GS_AFKO-PLNBEZ.
    IF SY-SUBRC = 0.
      GS_DATA-MEINS = GS_MARA-MEINS.
      GS_DATA-MTART = GS_MARA-MTART.
      GS_DATA-HOEHE = GS_MARA-HOEHE.
      GS_DATA-BREIT = GS_MARA-BREIT.
      GS_DATA-MEABM = GS_MARA-MEABM.
    ENDIF.

    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

    L_OBJECT = GS_DATA-MATNR.

    REFRESH IT_CHAR[].
    CLEAR IT_CHAR.

*获取特性
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        OBJECTKEY       = L_OBJECT
        OBJECTTABLE     = 'MARA'
        CLASSNUM        = 'LYD001'
        CLASSTYPE       = '001'
      TABLES
        ALLOCVALUESNUM  = IT_NUM
        ALLOCVALUESCHAR = IT_CHAR
        ALLOCVALUESCURR = IT_CURR
        RETURN          = IT_RET.

*产品类型
    READ TABLE IT_CHAR
    WITH KEY CHARACT = 'CPLX'.
    IF SY-SUBRC = 0.
      GS_DATA-ATWRT = IT_CHAR-VALUE_CHAR.
    ENDIF.

*订单总面积
    GS_DATA-DDZMJ = GS_DATA-GAMNG * GS_DATA-HOEHE * GS_DATA-BREIT.

*报工面积
    GS_DATA-BGZMJ  = GS_DATA-GMNGA * GS_DATA-HOEHE * GS_DATA-BREIT.

*报工面积比率
    IF GS_DATA-GAMNG <> 0.
      GS_DATA-BGMJBL = GS_DATA-GMNGA / GS_DATA-GAMNG.
    ELSE.
      GS_DATA-BGMJBL = 0.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
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

  INIT_FIELDCAT 'BUDAT'          '记账日期'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'          '工厂'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RUECK'          '确认号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RMZHL'          '计数器'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GAMNG'          '订单总数'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GMNGA'          '报工数量'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'          '移动类型'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '基本计量单位'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DDZMJ'          '订单总面积'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BGZMJ'          '报工总面积'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BGMJBL'         '报工面积比率'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ATWRT'          '产品类型'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料号'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '物料描述'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MTART'          '物料类型'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HOEHE'          '高度'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BREIT'          '宽度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEABM'          '尺寸单位'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDAUF'          '销售订单'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDPOS'          '销售订单行项目'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR'          '生产订单号'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VORNR'          '订单工序'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'          '生产订单类型'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GSTRP'          '基本开始日期'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GLTRP'          '基本完成日期'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WABLNR'         '物料凭证'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ERNAM'          '报工人员'           '' '' '' '' '' '' ''.

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
