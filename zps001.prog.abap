REPORT ZPS001.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/8
*& Request       :
*& Descriptions  : 项目产值信息表
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
TABLES:PROJ,MSEG.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        PSPID      TYPE PROJ-PSPID,      "项目编号
        POST1      TYPE PROJ-POST1,      "项目名称
        MAT_PSPNR TYPE MSEG-MAT_PSPNR, "WBS
        SJDM       TYPE ZMM024-SJDM,     "设计代码
        MATNR      TYPE MSEG-MATNR,      "物料号
        MAKTX      TYPE MAKT-MAKTX,      "物料描述
        GPREIS     TYPE CKIS-GPREIS,     "物料综合单价
        FPREIS     TYPE CKIS-FPREIS,     "人工预算单价
        KBETR      TYPE KONV-KBETR,      "人工单价
        XMWZCK     TYPE MSEG-MENGE,      "物料凭证数量
        CZ         TYPE CKIS-GPREIS,     "产值
        LWYDJDK    TYPE CKIS-GPREIS,     "劳务月度进度款
        LWYDJDK1   TYPE CKIS-GPREIS,     "劳务月度进度款(预算)
        ZHTJE      TYPE PROJ-ZHTJE,      "合同金额
      END OF TY_DATA.

TYPES:BEGIN OF TY_MSEG,
        WERKS      TYPE MSEG-WERKS,               "工厂
        MATNR      TYPE MSEG-MATNR,               "物料凭证号
        MAT_PSPNR TYPE MSEG-MAT_PSPNR,          "WBS元素
      END OF TY_MSEG.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA    TYPE TABLE OF TY_DATA.
DATA GS_DATA    TYPE TY_DATA.
DATA GS_DATA_1  TYPE TY_DATA.

DATA GT_PROJ  TYPE TABLE OF PROJ.
DATA GS_PROJ  TYPE PROJ.

DATA GT_PRPS  TYPE TABLE OF PRPS.
DATA GS_PRPS  TYPE PRPS.

DATA GT_MSEG  TYPE TABLE OF MSEG.
DATA GS_MSEG  TYPE MSEG.

DATA GT_MSEG1 TYPE TABLE OF TY_MSEG.
DATA GS_MSEG1 TYPE TY_MSEG.

DATA GT_MAKT  TYPE TABLE OF MAKT.
DATA GS_MAKT  TYPE MAKT.

DATA GT_EKKN  TYPE TABLE OF EKKN.
DATA GS_EKKN  TYPE EKKN.

DATA GT_EKPO  TYPE TABLE OF EKPO.
DATA GS_EKPO  TYPE EKPO.

DATA GT_EKKO  TYPE TABLE OF EKKO.
DATA GS_EKKO  TYPE EKKO.

DATA GT_KONV  TYPE TABLE OF KONV.
DATA GS_KONV  TYPE KONV.

DATA GT_ZPSPROD TYPE TABLE OF ZPSPROD.
DATA GS_ZPSPROD TYPE ZPSPROD.

DATA GT_ZMM024     TYPE TABLE OF ZMM024.
DATA GS_ZMM024     TYPE ZMM024.

DATA E_WBS_ECP        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
DATA LT_E_WBS_ECP     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
DATA LS_E_WBS_ECP     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
DATA LT_COST_LINES    TYPE TABLE OF KIS1.
DATA LS_COST_LINES    TYPE KIS1.
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
SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID,      "项目定义
               S_VERNR FOR PROJ-VERNR,      "项目经理
               S_BUDAT FOR MSEG-BUDAT_MKPF. "过账日期
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
   WHERE PSPID IN S_PSPID
   AND   VERNR IN S_VERNR.

  IF GT_PROJ IS  NOT INITIAL.
    SELECT * FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR.

    SELECT * FROM ZMM024
     INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
     FOR ALL ENTRIES IN GT_PROJ
     WHERE POSID = GT_PROJ-PSPID.

*查询期初数据
    SELECT * FROM ZPSPROD
    INTO CORRESPONDING FIELDS OF TABLE GT_ZPSPROD
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPID = GT_PROJ-PSPID.

    SELECT * FROM EKKN
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
     FOR ALL ENTRIES IN GT_PRPS
     WHERE PS_PSP_PNR  = GT_PRPS-PSPNR
     AND  LOEKZ <> 'L'.

    IF GT_EKKN IS NOT INITIAL.
      SELECT * FROM EKPO
      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
      FOR ALL ENTRIES IN GT_EKKN
      WHERE EBELN = GT_EKKN-EBELN
      AND   EBELP = GT_EKKN-EBELP
      AND   LOEKZ <> 'L'
      AND   KNTTP = 'Y'.

      SELECT * FROM EKKO
      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
      FOR ALL ENTRIES IN GT_EKPO
      WHERE EBELN = GT_EKPO-EBELN.

      SELECT * FROM KONV
      INTO CORRESPONDING FIELDS OF TABLE GT_KONV
      FOR ALL ENTRIES IN GT_EKKO
      WHERE KNUMV = GT_EKKO-KNUMV.
    ENDIF.

    SELECT * FROM MSEG
    INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
    FOR ALL ENTRIES IN GT_PRPS
    WHERE MAT_PSPNR = GT_PRPS-PSPNR
    AND   BUDAT_MKPF IN S_BUDAT.

    MOVE-CORRESPONDING GT_MSEG TO GT_MSEG1.

    SORT GT_MSEG1 BY WERKS MATNR MAT_PSPNR.
    DELETE ADJACENT DUPLICATES FROM GT_MSEG1 COMPARING WERKS MATNR MAT_PSPNR.

    SELECT * FROM MAKT
    INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FOR ALL ENTRIES IN GT_MSEG1
    WHERE MATNR = GT_MSEG1-MATNR.
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
  DATA E_WBS_ECP        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES    TYPE TABLE OF KIS1.
  DATA LS_COST_LINES    TYPE KIS1.
  DATA L_XMWZCK         TYPE MSEG-MENGE.

  LOOP AT GT_MSEG1 INTO GS_MSEG1.

*WBS
    GS_DATA-MAT_PSPNR = GS_MSEG1-MAT_PSPNR.

    READ TABLE GT_PRPS INTO GS_PRPS
    WITH KEY PSPNR = GS_MSEG1-MAT_PSPNR.
    IF SY-SUBRC = 0.
*      GS_DATA-PSPID  = GS_PRPS-PSPHI.
*      GS_DATA-POST1  = GS_PRPS-POST1.
    ENDIF.

*项目定义
    READ TABLE GT_PROJ INTO GS_PROJ
    WITH KEY PSPNR = GS_PRPS-PSPHI.
    IF SY-SUBRC = 0.
      GS_DATA-PSPID  = GS_PROJ-PSPID.
      GS_DATA-POST1  = GS_PROJ-POST1.
      GS_DATA-ZHTJE  = GS_PROJ-ZHTJE.
    ENDIF.

*物料号
    GS_DATA-MATNR      = GS_MSEG1-MATNR.

*物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX   = GS_MAKT-MAKTX.
    ENDIF.

*设计代码
    READ TABLE GT_ZMM024 INTO GS_ZMM024
    WITH KEY POSID = GS_DATA-PSPID
             MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-SJDM = GS_ZMM024-SJDM.
    ENDIF.

*物料综合单价
*    CLEAR:E_WBS_ECP,
*          LS_E_WBS_ECP,
*          LS_COST_LINES.
*
*    REFRESH:LT_E_WBS_ECP,
*            LT_COST_LINES.
*
*    CALL FUNCTION 'CNECP_READ'
*      EXPORTING
*        I_PROJ_DEF    = GS_DATA-PSPID
*        I_VERSION     = '100'
*      IMPORTING
*        E_WBS_ECP     = E_WBS_ECP
*      EXCEPTIONS
*        ERROR_MESSAGE = 1.
*
*    LT_E_WBS_ECP = E_WBS_ECP.
*
*    IF  LT_E_WBS_ECP IS NOT INITIAL.
*      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*      INDEX 1.
*      IF SY-SUBRC = 0.
*        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.
*
*        READ TABLE   LT_COST_LINES INTO LS_COST_LINES
*        WITH KEY  TYPPS = 'M'
*                  MATNR = GS_DATA-MATNR.
*        IF SY-SUBRC = 0.
*          GS_DATA-GPREIS = LS_COST_LINES-GPREIS.
*        ENDIF.
*      ENDIF.
*    ENDIF.

*物料综合单价
    PERFORM WLZHDJ_JS  USING  GS_DATA-PSPID GS_DATA-MATNR CHANGING  GS_DATA-GPREIS .
    IF   GS_DATA-GPREIS = 0 .
      LOOP AT GT_ZMM024 INTO GS_ZMM024
         WHERE SJDM  = GS_DATA-SJDM
         AND   POSID = GS_DATA-PSPID
         AND   MATNR NE  GS_DATA-MATNR.
        PERFORM WLZHDJ_JS  USING  GS_DATA-PSPID GS_ZMM024-MATNR CHANGING  GS_DATA-GPREIS .
        IF GS_DATA-GPREIS NE 0 .
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

**人工预算单价
*    CLEAR:E_WBS_ECP,
*          LS_E_WBS_ECP,
*          LS_COST_LINES.
*
*    REFRESH:LT_E_WBS_ECP,
*            LT_COST_LINES.
*
*    CALL FUNCTION 'CNECP_READ'
*      EXPORTING
*        I_PROJ_DEF    = GS_DATA-PSPID
*        I_VERSION     = '000'
*      IMPORTING
*        E_WBS_ECP     = E_WBS_ECP
*      EXCEPTIONS
*        ERROR_MESSAGE = 1.
*
*    LT_E_WBS_ECP = E_WBS_ECP.
*
*    IF  LT_E_WBS_ECP IS NOT INITIAL.
*      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*      INDEX 1.
*      IF SY-SUBRC = 0.
*        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.
*
*        READ TABLE   LT_COST_LINES INTO LS_COST_LINES
*        WITH KEY  TYPPS = 'M'
*                  MATNR = GS_DATA-MATNR.
*        IF SY-SUBRC = 0.
*          GS_DATA-FPREIS = LS_COST_LINES-FPREIS.
*        ENDIF.
*      ENDIF.
*    ENDIF.

    PERFORM WLZHDJ_JS1  USING  GS_DATA-PSPID GS_DATA-MATNR CHANGING  GS_DATA-FPREIS .
    IF   GS_DATA-FPREIS = 0 .
      LOOP AT GT_ZMM024 INTO GS_ZMM024
         WHERE SJDM  =   GS_DATA-SJDM
         AND   POSID =   GS_DATA-PSPID
         AND   MATNR NE  GS_DATA-MATNR.
        PERFORM WLZHDJ_JS1  USING  GS_DATA-PSPID GS_ZMM024-MATNR CHANGING  GS_DATA-FPREIS .
        IF GS_DATA-FPREIS NE 0 .
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

*人工单价
    LOOP AT  GT_EKKN INTO GS_EKKN
    WHERE  PS_PSP_PNR = GS_PRPS-PSPNR.
      READ TABLE GT_EKPO INTO GS_EKPO
      WITH KEY EBELN = GS_EKKN-EBELN
               EBELP = GS_EKKN-EBELP
               MATNR = GS_DATA-MATNR
               KNTTP = 'Y'.
      IF SY-SUBRC = 0.
        READ TABLE GT_EKKO INTO GS_EKKO
        WITH KEY EBELN = GS_EKPO-EBELN.
        IF SY-SUBRC = 0.
          READ TABLE GT_KONV INTO GS_KONV
          WITH KEY KNUMV = GS_EKKO-KNUMV
                   KPOSN = GS_EKPO-EBELP
                   STUNR = 1.
          IF SY-SUBRC = 0.
            GS_DATA-KBETR = GS_KONV-KBETR.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*项目物资出库数量
    CLEAR L_XMWZCK.
    LOOP AT GT_MSEG INTO GS_MSEG
    WHERE MATNR      = GS_DATA-MATNR
    AND   MAT_PSPNR = GS_PRPS-PSPNR
    AND   WERKS      = GS_MSEG1-WERKS
    AND  ( BWART = '281'
    OR     BWART = '282'
    OR     BWART = '221'
    OR     BWART = '222'
    OR     BWART = 'Z19'
    OR     BWART = 'Z20'
    OR     BWART = 'Z21'
    OR     BWART = 'Z22'
    OR     BWART =  'Z23'
    OR     BWART =  'Z24').
      IF GS_MSEG-SHKZG = 'H'.
        L_XMWZCK = L_XMWZCK + GS_MSEG-MENGE.
      ELSE.
        L_XMWZCK = L_XMWZCK - GS_MSEG-MENGE.
      ENDIF.
    ENDLOOP.

*    IF L_XMWZCK < 0.
*      L_XMWZCK = L_XMWZCK * -1.
*    ENDIF.

    GS_DATA-XMWZCK = L_XMWZCK.

*产值
    GS_DATA-CZ = GS_DATA-XMWZCK * GS_DATA-GPREIS.

*劳务月度进度款(预算)
    GS_DATA-LWYDJDK = GS_DATA-XMWZCK * GS_DATA-FPREIS.

*劳务月度进度款
    GS_DATA-LWYDJDK1 = GS_DATA-XMWZCK * GS_DATA-KBETR.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

  LOOP AT GT_ZPSPROD  INTO GS_ZPSPROD.
    GS_DATA-PSPID = GS_ZPSPROD-PSPID.

    SELECT SINGLE POST1
    INTO GS_DATA-POST1 FROM PROJ
    WHERE PSPID = GS_DATA-PSPID.

    GS_DATA-MAKTX = '期初项目产值'.
    GS_DATA-CZ    = GS_ZPSPROD-ZCZ.
   " GS_DATA-LWYDJDK = GS_ZPSPROD-ZLWK.
     GS_DATA-LWYDJDK1 = GS_ZPSPROD-ZLWK.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

  SORT GT_DATA BY PSPID MATNR.
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
  INIT_FIELDCAT 'PSPID'        '项目编号'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAT_PSPNR'   'WBS'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GPREIS'       '物料综合单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FPREIS'       '人工预算单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KBETR'        '人工单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMWZCK'       '物料凭证数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CZ'           '产值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LWYDJDK'      '劳务月度进度款（预算）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LWYDJDK1'     '劳务月度进度款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHTJE'       '合同金额'         '' '' '' '' '' '' ''.
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
*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DATA_PSPID  text
*      <--P_GS_DATA_GPREIS  text
*----------------------------------------------------------------------*
FORM WLZHDJ_JS  USING    P_PSPID  LIKE PROJ-PSPID
                         P_MATNR  LIKE MSEG-MATNR
                CHANGING P_GPREIS LIKE CKIS-GPREIS.
  CLEAR:E_WBS_ECP,
            LS_E_WBS_ECP,
            LS_COST_LINES,
            P_GPREIS.

  REFRESH:LT_E_WBS_ECP,
          LT_COST_LINES.

  CALL FUNCTION 'CNECP_READ'
    EXPORTING
      I_PROJ_DEF    = P_PSPID
      I_VERSION     = '100'
    IMPORTING
      E_WBS_ECP     = E_WBS_ECP
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  LT_E_WBS_ECP = E_WBS_ECP.

  IF  LT_E_WBS_ECP IS NOT INITIAL.
    READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
    INDEX 1.
    IF SY-SUBRC = 0.
      LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

      READ TABLE   LT_COST_LINES INTO LS_COST_LINES
      WITH KEY  TYPPS = 'M'
                MATNR = P_MATNR.
      IF SY-SUBRC = 0.
       P_GPREIS = LS_COST_LINES-GPREIS.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DATA_PSPID  text
*      <--P_GS_DATA_GPREIS  text
*----------------------------------------------------------------------*
FORM WLZHDJ_JS1  USING    P_PSPID  LIKE PROJ-PSPID
                         P_MATNR  LIKE MSEG-MATNR
                CHANGING P_FPREIS LIKE CKIS-FPREIS.
  CLEAR:E_WBS_ECP,
            LS_E_WBS_ECP,
            LS_COST_LINES,
            P_FPREIS.

  REFRESH:LT_E_WBS_ECP,
          LT_COST_LINES.

  CALL FUNCTION 'CNECP_READ'
    EXPORTING
      I_PROJ_DEF    = P_PSPID
      I_VERSION     = '000'
    IMPORTING
      E_WBS_ECP     = E_WBS_ECP
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  LT_E_WBS_ECP = E_WBS_ECP.

  IF  LT_E_WBS_ECP IS NOT INITIAL.
    READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
    INDEX 1.
    IF SY-SUBRC = 0.
      LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

      READ TABLE   LT_COST_LINES INTO LS_COST_LINES
      WITH KEY  TYPPS = 'M'
                MATNR = P_MATNR.
      IF SY-SUBRC = 0.
       P_FPREIS = LS_COST_LINES-FPREIS.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
