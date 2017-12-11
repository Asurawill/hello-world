REPORT ZMM021.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/9
*& Request       :
*& Descriptions  : 预算与累计申购数据对比报表
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
TABLES:PROJ,PRPS,MARA,ZMM024.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        PSPID     TYPE PROJ-PSPID,    "项目定义
        POST1     TYPE PROJ-POST1,    "项目订单描述
        WERKS     TYPE PROJ-WERKS,    "工厂
        MATNR     TYPE MARA-MATNR,    "物料号
        MAKTX     TYPE MAKT-MAKTX,    "物料描述
        MATKL     TYPE MARA-MATKL,    "物料组
        WGBEZ     TYPE T023T-WGBEZ,   "物料组描述
        MEINS     TYPE MARA-MEINS,    "计量单位
        POSID1    TYPE PRPS-POSID,    "WBS元素
        MENGE     TYPE EKPO-MENGE,    "预算数
        WQRLJSGSL TYPE EKPO-MENGE,    "未确认累计申购数量
        YQRLJSGSL TYPE EKPO-MENGE,    "已去人累计申购数量
        SYKSQSL   TYPE EKPO-MENGE,    "剩余可申购数量
        LJCGXDSL  TYPE EKPO-MENGE,    "累计采购下达数量
        CGWXDSL   TYPE EKPO-MENGE,    "采购未下达数量
        SJDM      TYPE ZMM024-SJDM,   "设计代码
*        WBS_TEXT  TYPE STRING,        "WBS传送文本字段
      END OF TY_DATA.

TYPES:BEGIN OF TY_ECP,
        PSPNR TYPE PS_INTNR,
        WERKS TYPE WERKS_D,
        MATNR TYPE MATNR,
        MEEHT TYPE MEINS,
        MENGE TYPE MENGE_POS,
      END OF TY_ECP.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_PROJ  TYPE TABLE OF PROJ.
DATA GS_PROJ  TYPE PROJ.

DATA GT_PRPS  TYPE TABLE OF PRPS.
DATA GS_PRPS  TYPE PRPS.

DATA GT_MAKT  TYPE TABLE OF MAKT.
DATA GS_MAKT  TYPE MAKT.

DATA GT_T023T TYPE TABLE OF T023T.
DATA GS_T023T TYPE T023T.

DATA GT_EBKN  TYPE TABLE OF EBKN.
DATA GS_EBKN  TYPE  EBKN.

DATA GT_EBAN  TYPE TABLE OF EBAN.
DATA GS_EBAN  TYPE EBAN.

DATA GT_EKKN  TYPE TABLE OF EKKN.
DATA GS_EKKN  TYPE EKKN.

DATA GT_EKPO  TYPE TABLE OF EKPO.
DATA GS_EKPO  TYPE EKPO.

DATA GT_ECP   TYPE TABLE OF TY_ECP.
DATA GS_ECP   TYPE TY_ECP.

DATA GT_ZMM009 TYPE TABLE OF ZMM009.
DATA GS_ZMM009 TYPE ZMM009.

DATA GT_ZMM024 TYPE TABLE OF ZMM024.
DATA GS_ZMM024 TYPE ZMM024.

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
SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID,    "项目定义
               S_WERKS FOR PROJ-WERKS,    "工厂
               S_MATNR FOR MARA-MATNR,    "预算物料
               S_MATKL FOR MARA-MATKL,    "物料组
               S_SJDM  FOR ZMM024-SJDM.   "设计代码
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
   INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
   WHERE PSPID IN S_PSPID.

  SELECT * FROM ZMM024
   INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
   FOR ALL ENTRIES IN GT_PROJ
   WHERE POSID = GT_PROJ-PSPID.

  IF GT_PROJ IS  NOT INITIAL.
    SELECT * FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR.

    CHECK GT_PRPS IS NOT INITIAL.

    SELECT * FROM EBKN
    INTO CORRESPONDING FIELDS OF TABLE GT_EBKN
    FOR ALL ENTRIES IN GT_PRPS
    WHERE PS_PSP_PNR = GT_PRPS-PSPNR
    AND   LOEKZ <> 'X'.
*
*    CHECK GT_EBKN IS NOT INITIAL.
    IF GT_EBKN IS NOT INITIAL .
      SELECT * FROM EBAN
      INTO CORRESPONDING FIELDS OF TABLE GT_EBAN
      FOR ALL ENTRIES IN GT_EBKN
      WHERE BANFN = GT_EBKN-BANFN
      AND   BNFPO = GT_EBKN-BNFPO
      AND   LOEKZ <> 'X'.
    ENDIF.

    SELECT * FROM EKKN
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
    FOR ALL ENTRIES IN GT_PRPS
    WHERE PS_PSP_PNR = GT_PRPS-PSPNR
    AND   LOEKZ <> 'L'.

*    CHECK GT_EKKN IS NOT INITIAL.

    IF GT_EKKN IS NOT INITIAL .
      SELECT * FROM EKPO
      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
      FOR ALL ENTRIES IN GT_EKKN
      WHERE EBELN = GT_EKKN-EBELN
      AND   EBELP = GT_EKKN-EBELP
      AND   ( KNTTP = 'Q'
      OR KNTTP = '' )
      AND   LOEKZ <> 'L'.
    ENDIF.

  ENDIF.

  SELECT * FROM ZMM009
    INTO CORRESPONDING FIELDS OF TABLE GT_ZMM009.
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
  DATA L_PSPID          TYPE PROJ-PSPID.
  DATA L_POST1          TYPE PROJ-POST1.
  DATA L_LONGTEXT1 TYPE THEAD-TDNAME.

  LOOP AT GT_PROJ INTO GS_PROJ.
    CLEAR:L_PSPID,
          L_POST1.

    L_PSPID   = GS_PROJ-PSPID.

    L_POST1  = GS_PROJ-POST1.

*工厂
    CLEAR:E_WBS_ECP,
       LS_E_WBS_ECP,
       LS_COST_LINES.

    REFRESH:LT_E_WBS_ECP,
            LT_COST_LINES.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        I_PROJ_DEF    = L_PSPID
        I_VERSION     = '000'
      IMPORTING
        E_WBS_ECP     = E_WBS_ECP
      EXCEPTIONS
        ERROR_MESSAGE = 1.
*    IF SY-SUBRC <> 0.
*      RAISE NOT_FOUND.
*    ENDIF.

    LT_E_WBS_ECP = E_WBS_ECP.
    IF  LT_E_WBS_ECP IS NOT INITIAL.
      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
      INDEX 1.
      IF SY-SUBRC = 0.
        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

*物料号，工厂，计量单位
        REFRESH GT_ECP.

*选择条件筛选
        DELETE LT_COST_LINES WHERE MATNR NOT IN S_MATNR.
        DELETE LT_COST_LINES WHERE MATKL NOT IN S_MATKL.

        LOOP AT   LT_COST_LINES INTO LS_COST_LINES
        WHERE    TYPPS = 'M'.
          MOVE-CORRESPONDING LS_COST_LINES TO GS_ECP.
          COLLECT GS_ECP INTO GT_ECP.
          CLEAR GS_ECP.
        ENDLOOP.

        LOOP AT  GT_ECP INTO GS_ECP.

*项目定义
          GS_DATA-PSPID = L_PSPID.

*项目描述
          GS_DATA-POST1 = L_POST1.

          GS_DATA-WERKS = GS_ECP-WERKS.
          GS_DATA-MATNR = GS_ECP-MATNR.

*设计代码
          READ TABLE GT_ZMM024 INTO GS_ZMM024
          WITH KEY MATNR = GS_DATA-MATNR
                   POSID = GS_PROJ-PSPID.
          IF SY-SUBRC = 0.
            GS_DATA-SJDM = GS_ZMM024-SJDM.
          ENDIF.

          GS_DATA-MEINS = GS_ECP-MEEHT.
          GS_DATA-MENGE = GS_ECP-MENGE.

*物料描述
*          READ TABLE GT_MAKT INTO GS_MAKT
*          WITH KEY MATNR = GS_DATA-MATNR
*                   SPRAS = SY-LANGU.
*          IF SY-SUBRC = 0.
*            GS_DATA-MAKTX = GS_MAKT-MAKTX.
*          ENDIF.
          SELECT SINGLE MAKTX FROM MAKT
           INTO  GS_DATA-MAKTX
           WHERE MATNR = GS_DATA-MATNR
           AND   SPRAS = SY-LANGU.


*物料组
          SELECT SINGLE MATKL FROM MARA
          INTO GS_DATA-MATKL
          WHERE MATNR = GS_DATA-MATNR.

*物料组描述
*          IF GS_DATA-MATKL IS NOT INITIAL.
*            READ TABLE GT_T023T INTO GS_T023T
*            WITH KEY MATKL = GS_DATA-MATKL
*                     SPRAS = SY-LANGU.
*            IF SY-SUBRC = 0.
*              GS_DATA-WGBEZ = GS_T023T-WGBEZ.
*            ENDIF.
*          ENDIF.
          SELECT SINGLE MATKL FROM T023T
          INTO GS_DATA-WGBEZ
          WHERE MATKL = GS_DATA-MATKL
          AND   SPRAS = SY-LANGU.

*WBS元素
          LOOP AT GT_PRPS INTO GS_PRPS
          WHERE PSPHI = GS_PROJ-PSPNR.

*未确认累计申购数量
            LOOP AT GT_EBKN INTO GS_EBKN
             WHERE PS_PSP_PNR = GS_PRPS-PSPNR.
              READ TABLE GT_EBAN INTO GS_EBAN
              WITH KEY   BANFN = GS_EBKN-BANFN
                         BNFPO = GS_EBKN-BNFPO
                         MATNR = GS_DATA-MATNR
                         FRGKZ = 'X'.
              IF SY-SUBRC = 0.
                GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_EBAN-MENGE.
              ENDIF.
            ENDLOOP.

*已确认累计申购数量
            LOOP AT GT_EBKN INTO GS_EBKN
             WHERE PS_PSP_PNR = GS_PRPS-PSPNR.

              READ TABLE GT_EBAN INTO GS_EBAN
              WITH KEY   BANFN = GS_EBKN-BANFN
                         BNFPO = GS_EBKN-BNFPO
                         MATNR = GS_DATA-MATNR
                         FRGKZ = '2'.
              IF SY-SUBRC = 0.
                GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_EBAN-MENGE.
              ENDIF.
            ENDLOOP.

*累计采购已下单数量
            LOOP AT GT_EKKN INTO GS_EKKN
              WHERE PS_PSP_PNR = GS_PRPS-PSPNR.
              READ TABLE GT_EKPO INTO GS_EKPO
              WITH KEY   EBELN = GS_EKKN-EBELN
                         EBELP = GS_EKKN-EBELP
                         MATNR = GS_DATA-MATNR..
              IF SY-SUBRC = 0.
                GS_DATA-LJCGXDSL = GS_DATA-LJCGXDSL + GS_EKPO-MENGE.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

*期初数量(未确认累计申购数量)
          LOOP AT GT_ZMM009 INTO GS_ZMM009
          WHERE PSPHI = GS_DATA-PSPID
          AND   MATNR = GS_DATA-MATNR.
            GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_ZMM009-WQRLJSGS.
          ENDLOOP.

*期初数量(已确认累计申购数量)
          LOOP AT GT_ZMM009 INTO GS_ZMM009
          WHERE PSPHI = GS_DATA-PSPID
          AND   MATNR = GS_DATA-MATNR.
            GS_DATA-LJCGXDSL = GS_DATA-LJCGXDSL + GS_ZMM009-LJCGXDS.
          ENDLOOP.


*期初数量(已确认累计申购数量)
          LOOP AT GT_ZMM009 INTO GS_ZMM009
          WHERE PSPHI = GS_DATA-PSPID
          AND   MATNR = GS_DATA-MATNR.
            GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_ZMM009-YQRLJSGS.
          ENDLOOP.

*剩余申购数量
          GS_DATA-SYKSQSL = GS_DATA-MENGE - GS_DATA-YQRLJSGSL - GS_DATA-WQRLJSGSL.

*采购未下单数量
          GS_DATA-CGWXDSL = GS_DATA-YQRLJSGSL - GS_DATA-LJCGXDSL.

          APPEND GS_DATA TO GT_DATA.
          CLEAR GS_DATA.
        ENDLOOP.

      ENDIF.
    ENDIF.
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
*  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
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
  INIT_FIELDCAT 'PSPID'        '项目定义'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'        '物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WGBEZ'        '物料组描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'        '计量单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'        '预算数(A)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WQRLJSGSL'    '材料单在审数量(B)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQRLJSGSL'    '材料单已审数量(C)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SYKSQSL'      '剩余申购数量(D=A-B-C)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJCGXDSL'     '采购已下单数量(E)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CGWXDSL'      '材料单已审采购未下单数量(F=C-E)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.

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

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'PSPID'
        AND GS_DATA-PSPID IS NOT INITIAL.
        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.



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
