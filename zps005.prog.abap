REPORT ZPS005.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/10
*& Request       :
*& Descriptions  : 项目成本预算报表
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
TABLES:PROJ.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        PSPID   TYPE PROJ-PSPID, "项目定义
        POST1   TYPE PROJ-POST1, "项目名称
        POSID1  TYPE PRPS-POSID, "WBS元素
        MLL     TYPE P LENGTH 5 DECIMALS 2, "毛利率
        PLFAZ   TYPE PROJ-PLFAZ, "开工日期
        PLSEZ   TYPE PROJ-PLSEZ, "完工日期
        XH      TYPE STRING,     "序号
        MATNR   TYPE MARA-MATNR, "物料号
        MAKTX   TYPE MAKT-MAKTX, "物料描述
        MEINS   TYPE MARA-MEINS, "计量单位
        MENGE   TYPE CKIS-MENGE, "合同工程量
        GPREIS  TYPE CKIS-GPREIS, "合同单价
        HJ      TYPE CKIS-GPREIS, "合价
        MENGE1  TYPE CKIS-MENGE, "计划工程量
        GPREIS1 TYPE CKIS-GPREIS, "材料单价
        FPREIS  TYPE CKIS-FPREIS, "人工单价
        CLHJ    TYPE CKIS-FPREIS, "材料合价
        RGHJ    TYPE CKIS-FPREIS, "人工合价
        LRL     TYPE P LENGTH 5 DECIMALS 2, "利润率
        ZCSXM1  TYPE ZPSCOST-ZCSXM, "措施项目
        ZXFC1   TYPE ZPSCOST-ZXFC, "小辅材
        ZGF1    TYPE ZPSCOST-ZGF,  "规费
        ZSJ1    TYPE ZPSCOST-ZSJ,  "税金
        ZZLJ1   TYPE ZPSCOST-ZZLJ, "暂列金
        ZQTFY1  TYPE ZPSCOST-ZQTFY, "其他费用
        ZCSXM2  TYPE ZPSCOST-ZCSXM, "措施项目（报价）
        ZXFC2   TYPE ZPSCOST-ZXFC, "小辅材（报价）
        ZGF2    TYPE ZPSCOST-ZGF,  "规费（报价）
        ZSJ2    TYPE ZPSCOST-ZSJ,  "税金（报价）
        ZZLJ2   TYPE ZPSCOST-ZZLJ, "暂列金（报价）
        ZQTFY2  TYPE ZPSCOST-ZQTFY, "其他费用（报价）
        ZBZ     TYPE ZPSCOST-ZBZ,  "备注
        SJDM    TYPE ZMM024-SJDM,  "设计代码
      END OF TY_DATA.

TYPES:BEGIN OF TY_ECP,
        PSPNR TYPE PS_INTNR,
        WERKS TYPE WERKS_D,
        MATNR TYPE MATNR,
        MEEHT TYPE MEINS,
        MENGE TYPE MENGE_POS,
      END OF TY_ECP.

TYPES:BEGIN OF TY_WERTN,
        PSPNR TYPE PS_INTNR,
        WERKS TYPE WERKS_D,
        MATNR TYPE MATNR,
        WERTN TYPE WERTN,
      END OF TY_WERTN.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA       TYPE TABLE OF TY_DATA.
DATA GS_DATA       TYPE TY_DATA.

DATA GT_PROJ       TYPE TABLE OF PROJ.
DATA GS_PROJ       TYPE PROJ.

DATA GT_ZPSCOST    TYPE TABLE OF ZPSCOST.
DATA GS_ZPSCOST    TYPE ZPSCOST.

DATA GT_ECP        TYPE TABLE OF TY_ECP.
DATA GS_ECP        TYPE TY_ECP.

DATA GT_ECP1       TYPE TABLE OF TY_ECP.
DATA GS_ECP1       TYPE TY_ECP.

DATA GT_WERTN      TYPE TABLE OF TY_WERTN.
DATA GS_WERTN      TYPE TY_WERTN.

DATA GT_WERTN1     TYPE TABLE OF TY_WERTN.
DATA GS_WERTN1     TYPE TY_WERTN.

DATA GT_ZMM024     TYPE TABLE OF ZMM024.
DATA GS_ZMM024     TYPE ZMM024.
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
SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID.    "项目定义
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

  IF GT_PROJ IS  NOT INITIAL.
    SELECT * FROM ZPSCOST
     INTO CORRESPONDING FIELDS OF TABLE GT_ZPSCOST
     FOR ALL ENTRIES IN GT_PROJ
     WHERE ZPSPID = GT_PROJ-PSPID.

    SELECT * FROM ZMM024
     INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
     FOR ALL ENTRIES IN GT_PROJ
     WHERE POSID = GT_PROJ-PSPID.
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
  DATA E_WBS_ECP         TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP      TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP      TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES     TYPE TABLE OF KIS1.
  DATA LS_COST_LINES     TYPE KIS1.

  DATA E_WBS_ECP1        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP1     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP1     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES1    TYPE TABLE OF KIS1.
  DATA LS_COST_LINES1    TYPE KIS1.

  DATA L_PSPID           TYPE PROJ-PSPID.
  DATA L_WERTN           TYPE WERTN.
  DATA L_WERTN1          TYPE WERTN.

  LOOP AT GT_PROJ INTO GS_PROJ.

    CLEAR:E_WBS_ECP,
      LS_E_WBS_ECP,
      LS_COST_LINES,
      L_PSPID.

    REFRESH:LT_E_WBS_ECP,
            LT_COST_LINES.


    L_PSPID  = GS_PROJ-PSPID.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        I_PROJ_DEF    = L_PSPID
        I_VERSION     = '000'
      IMPORTING
        E_WBS_ECP     = E_WBS_ECP
      EXCEPTIONS
        ERROR_MESSAGE = 1.
    IF SY-SUBRC <> 0.
*      RAISE NOT_FOUND.
    ELSE.
      LT_E_WBS_ECP = E_WBS_ECP.
      IF  LT_E_WBS_ECP IS NOT INITIAL.
        READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
        INDEX 1.
        IF SY-SUBRC = 0.
          LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

          REFRESH GT_ECP.
          LOOP AT   LT_COST_LINES INTO LS_COST_LINES
          WHERE    TYPPS = 'M'.
            MOVE-CORRESPONDING LS_COST_LINES TO GS_ECP.
            COLLECT GS_ECP INTO GT_ECP.
            CLEAR GS_ECP.
          ENDLOOP.

          CLEAR:L_WERTN,
                L_WERTN1.
*项目收入
          LOOP AT LT_COST_LINES INTO LS_COST_LINES
          WHERE KSTAR+0(4) = '6001'
          OR    KSTAR+0(4) = '6051'
          OR    KSTAR+0(4) = '6301'.
            MOVE-CORRESPONDING LS_COST_LINES TO GS_WERTN.
            COLLECT GS_WERTN INTO GT_WERTN.
            CLEAR GS_WERTN.
          ENDLOOP.

          READ TABLE GT_WERTN INTO GS_WERTN
           WITH KEY MATNR = GS_DATA-MATNR.
          IF SY-SUBRC = 0.
            L_WERTN = GS_WERTN-WERTN.
          ENDIF.

*项目成本
          LOOP AT LT_COST_LINES INTO LS_COST_LINES
          WHERE  KSTAR+0(4) <> '6001'
          AND    KSTAR+0(4) <> '6051'
          AND    KSTAR+0(4) <> '6301'.
            MOVE-CORRESPONDING LS_COST_LINES TO GS_WERTN.
            COLLECT GS_WERTN1 INTO GT_WERTN1.
            CLEAR GS_WERTN1.
          ENDLOOP.

          READ TABLE GT_WERTN1 INTO GS_WERTN1
           WITH KEY MATNR = GS_DATA-MATNR.
          IF SY-SUBRC = 0.
            L_WERTN1 = GS_WERTN-WERTN.
          ENDIF.

*毛利率
          IF L_WERTN <> 0.
            GS_DATA-MLL = ( L_WERTN - L_WERTN1 ) / L_WERTN * 100.
          ELSE.
            GS_DATA-MLL = 0.
          ENDIF.

*              CLEAR:E_WBS_ECP,
*                LS_E_WBS_ECP,
*                LS_COST_LINES.
*
*              REFRESH:LT_E_WBS_ECP,
*                      LT_COST_LINES.
          CALL FUNCTION 'CNECP_READ'
            EXPORTING
              I_PROJ_DEF    = L_PSPID
              I_VERSION     = '100'
            IMPORTING
              E_WBS_ECP     = E_WBS_ECP1
            EXCEPTIONS
              ERROR_MESSAGE = 1.
          LT_E_WBS_ECP1 = E_WBS_ECP1.
          IF  LT_E_WBS_ECP1 IS NOT INITIAL.
            READ TABLE LT_E_WBS_ECP1 INTO LS_E_WBS_ECP1
            INDEX 1.
            IF SY-SUBRC = 0.
              LT_COST_LINES1 = LS_E_WBS_ECP1-COST_LINES.

              REFRESH GT_ECP1.
              LOOP AT   LT_COST_LINES1 INTO LS_COST_LINES1
              WHERE    TYPPS = 'M'.
                MOVE-CORRESPONDING LS_COST_LINES1 TO GS_ECP1.
                COLLECT GS_ECP1 INTO GT_ECP1.
                CLEAR GS_ECP1.
              ENDLOOP.
            ENDIF.
          ENDIF.


          LOOP AT  GT_ECP INTO GS_ECP.
            READ TABLE LT_COST_LINES INTO LS_COST_LINES
            WITH KEY  MATNR = GS_ECP-MATNR.
            IF SY-SUBRC = 0.
*物料编码
              GS_DATA-MATNR   = LS_COST_LINES-MATNR.

*物料描述
              IF GS_DATA-MATNR IS NOT INITIAL.
                SELECT SINGLE MAKTX FROM MAKT
                INTO GS_DATA-MAKTX
                WHERE MATNR = GS_DATA-MATNR
                AND   SPRAS = SY-LANGU.
              ENDIF.

*设计代码
              READ TABLE GT_ZMM024 INTO GS_ZMM024
              WITH KEY POSID = L_PSPID
                       MATNR = GS_DATA-MATNR.
              IF SY-SUBRC = 0.
                GS_DATA-SJDM = GS_ZMM024-SJDM.
              ENDIF.

*单位
              GS_DATA-MEINS   = LS_COST_LINES-MEEHT.

*计量工程量
              GS_DATA-MENGE1   = LS_COST_LINES-MENGE.

*材料单价
              GS_DATA-GPREIS1  = LS_COST_LINES-GPREIS.

*人工单价
              GS_DATA-FPREIS  = LS_COST_LINES-FPREIS.

*材料合价
              GS_DATA-CLHJ    = GS_DATA-GPREIS1 * GS_DATA-MENGE1.

*人工合价
              GS_DATA-RGHJ    = GS_DATA-FPREIS * GS_DATA-MENGE1.


*合同工程量
              READ TABLE GT_ECP1 INTO GS_ECP1
              WITH KEY MATNR = GS_DATA-MATNR.
              IF SY-SUBRC = 0.
                GS_DATA-MENGE  = GS_ECP1-MENGE.
              ENDIF.

*合同单价
              READ TABLE LT_COST_LINES1 INTO LS_COST_LINES1
              WITH KEY MATNR = GS_DATA-MATNR.
              IF SY-SUBRC = 0.
                GS_DATA-GPREIS = LS_COST_LINES1-GPREIS.
              ENDIF.
*合计
              GS_DATA-HJ     = GS_DATA-MENGE * GS_DATA-GPREIS.
            ENDIF.

*利润率
            IF GS_DATA-HJ <> 0.
              GS_DATA-LRL = ( GS_DATA-HJ - GS_DATA-CLHJ - GS_DATA-RGHJ ) / GS_DATA-HJ * 100.
            ELSE.
              GS_DATA-LRL = 0.
            ENDIF.

*读取预算
            MOVE-CORRESPONDING GS_PROJ TO GS_DATA.

            READ TABLE GT_ZPSCOST INTO GS_ZPSCOST
            WITH KEY ZPSPID = GS_DATA-PSPID.
            IF SY-SUBRC = 0.
              MOVE-CORRESPONDING GS_ZPSCOST TO GS_DATA.
            ENDIF.

            APPEND GS_DATA TO GT_DATA.
            CLEAR GS_DATA.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*利润率
*            GS_DATA-LRL     =
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
  PERFORM FRM_BUILD_EVENT.
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
  INIT_FIELDCAT 'PSPID'       '项目定义'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'POST1'       '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID1'      'WBS'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MLL'         '毛利率%'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PLFAZ'       '开工日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PLSEZ'       '完工日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XH'          '序号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'       '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'       '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'       '计量单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'       '合同工程量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GPREIS'      '合同单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HJ'          '合价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE1'      '计划工程量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GPREIS1'     '材料单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPREIS'      '人工单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CLHJ'        '材料合价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RGHJ'        '人工合价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LRL'         '利润率%'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZCSXM1'       '措施项目（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZXFC1'        '小辅材（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZGF1'         '规费（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZSJ1'         '税金（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZLJ1'        '暂列金（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZQTFY1'       '其他费用（成本）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZCSXM2'       '措施项目（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZXFC2'        '小辅材（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZGF2'         '规费（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZSJ2'         '税金（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZLJ2'        '暂列金（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZQTFY2'       '其他费用（报价）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZBZ'         '备注'         '' '' '' '' '' '' ''.
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
