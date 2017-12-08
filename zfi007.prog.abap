*&---------------------------------------------------------------------*
*& Report  ZFI004
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/03/09
*& Request       :
*& Descriptions  : 应收票据管理报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZFI007.
************************************************************************
* Tables
************************************************************************
TABLES:BKPF,BSEG,BSED.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA.
TYPES:ZBOX  TYPE C,
      STATU TYPE ICONNAME. "状态栏
        INCLUDE TYPE ZFI007.
TYPES:LIFNR   TYPE LFA1-LIFNR, "背书供应商
      NAME1   TYPE LFA1-NAME1, "描述
      BLDAT_1 TYPE BKPF-BLDAT, "背书日期
      BELNR_1 TYPE BSEG-BELNR, "背书会计凭证
      GJAHR_1 TYPE BSEG-GJAHR. "背书会计年度
TYPES:END OF TY_DATA.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_ZFI007 TYPE TABLE OF ZFI007.
DATA GS_ZFI007 TYPE ZFI007.

DATA GT_DATA  TYPE TABLE OF TY_DATA.
DATA GS_DATA  TYPE TY_DATA.

DATA GT_BSEG TYPE TABLE OF BSEG.
DATA GS_BSEG TYPE BSEG.

DATA GT_BKPF TYPE TABLE OF BKPF.
DATA GS_BKPF TYPE BKPF.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.
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
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME .
SELECT-OPTIONS: S_BUKRS   FOR BKPF-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_KUNNR   FOR BSEG-KUNNR,"客户
                S_WBANK   FOR BSED-WBANK,"汇票编码
                S_DMBTR   FOR BSEG-DMBTR,"汇票金额
                S_ZFBDT   FOR BSEG-ZFBDT,"到期日
                S_WNAME   FOR BSED-WNAME,"出票人
                S_BLDTA   FOR BKPF-BLDAT,"接收汇票日期
                S_SGTXT   FOR BSEG-SGTXT,"背书人
                S_LIFNR   FOR BKPF-BLDAT,"背书供应商
                S_BLDTA1 FOR BKPF-BLDAT."背书日期

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
 " *权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.

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

*查询应收票据管理报表
  SELECT * FROM ZFI007
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI007
    WHERE BUKRS IN S_BUKRS
    AND   KUNNR IN S_KUNNR
    AND   WBANK IN S_WBANK
    AND   DMBTR IN S_DMBTR
    AND   ZFBDT IN S_ZFBDT
    AND   WNAME IN S_WNAME
    AND   BLDAT IN S_BLDTA
    AND   SGTXT IN S_SGTXT
    AND   BLDAT IN S_BLDTA.

  CHECK GT_ZFI007 IS NOT INITIAL.

*查询背书凭证
  SELECT * FROM BSEG
    INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
    FOR ALL ENTRIES IN GT_ZFI007
    WHERE BUKRS = GT_ZFI007-BUKRS
*    AND   ZUONR = GT_ZFI007-WBANK
    AND   BSCHL = '11'
    AND   SGTXT IN  S_SGTXT.

  SORT GT_BSEG.

*查询供应商描述
  IF GT_BSEG IS NOT INITIAL.
    SELECT * FROM LFA1
      INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
      FOR ALL ENTRIES IN GT_BSEG
      WHERE LIFNR = GT_BSEG-LIFNR
      AND   SPRAS = SY-LANGU.

*查询背书凭证抬头日期
    SELECT * FROM BKPF
      INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
      FOR ALL ENTRIES IN GT_BSEG
      WHERE BUKRS = GT_BSEG-BUKRS
      AND   BELNR = GT_BSEG-BELNR
      AND   GJAHR = GT_BSEG-GJAHR
      AND   BLDAT IN   S_BLDTA1.
  ENDIF.
ENDFORM.
*-------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
*添加背书凭证
  LOOP AT GT_ZFI007 INTO GS_ZFI007.

    MOVE-CORRESPONDING GS_ZFI007 TO GS_DATA.
    GS_DATA-SGTXT = ''.

*背书供应商
    READ TABLE GT_BSEG INTO GS_BSEG
    WITH KEY BUKRS = GS_ZFI007-BUKRS
             ZUONR = GS_ZFI007-WBANK
             BSCHL = '11'.
*             BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_DATA-LIFNR   = GS_BSEG-LIFNR.
      GS_DATA-SGTXT   = GS_BSEG-SGTXT.
      GS_DATA-BELNR_1 = GS_BSEG-BELNR.
      GS_DATA-GJAHR_1 = GS_BSEG-GJAHR.
      READ TABLE GT_LFA1 INTO GS_LFA1
      WITH KEY LIFNR  = GS_DATA-LIFNR
               SPRAS = SY-LANGU.
      IF SY-SUBRC = 0.
        GS_DATA-NAME1 = GS_LFA1-NAME1.
      ENDIF.
    ELSE.

*选择屏幕背书人有筛选
      IF S_SGTXT IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*当有背书凭证为绿灯,否则红灯
    IF  GS_DATA-BELNR_1 IS INITIAL
    AND GS_DATA-GJAHR_1 IS INITIAL.
      GS_DATA-STATU = ICON_RED_LIGHT.
    ELSE.
      GS_DATA-STATU = ICON_GREEN_LIGHT.
    ENDIF.

*背书日期
    READ TABLE GT_BKPF INTO GS_BKPF
    WITH KEY BUKRS = GS_BSEG-BUKRS
             BELNR = GS_BSEG-BELNR
             GJAHR = GS_BSEG-GJAHR.
    IF SY-SUBRC = 0.
      GS_DATA-BLDAT_1 = GS_BKPF-BLDAT.

*排除冲销凭证
      IF GS_BKPF-STBLG IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ELSE.
      IF S_BLDTA1 IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
    CLEAR GS_BKPF.
    CLEAR GS_BSEG.
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
  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
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
  INIT_FIELDCAT 'STATU'   TEXT-017         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'   TEXT-001         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'   TEXT-002         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1_1' TEXT-003         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WBANK'   TEXT-004         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'   TEXT-005         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PSWSL'   TEXT-006         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'   TEXT-007         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'   TEXT-008         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WNAME'   TEXT-009         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WBZOG'   TEXT-010         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZFBDT'   TEXT-011         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WDATE'   TEXT-018        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WLZBP'   TEXT-019         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'   TEXT-020         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'   TEXT-021         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLDAT'   TEXT-012         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'   TEXT-013         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLDAT_1' TEXT-016         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR_1' TEXT-022         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR_1' TEXT-023         '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'LIFNR'   TEXT-014         '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'NAME1'   TEXT-015         '' '' '' '' '' '' ''.

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
*        AND GS_DATA-BSTAT <> 'V'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BSTAT = 'V'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
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
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD S_BUKRS-LOW.
ENDFORM.
