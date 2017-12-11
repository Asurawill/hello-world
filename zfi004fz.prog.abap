*&---------------------------------------------------------------------*
*& Report  ZFI004FZ
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/11
*& Request       :
*& Descriptions  : 会计凭证查询和打印
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZFI004FZFZ.

************************************************************************
* Tables
************************************************************************
TABLES:BKPF,BSEG.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA.
TYPES:ZBOX   TYPE C.
        INCLUDE TYPE ZFI004FZ.
TYPES:NAME   TYPE ADRP-NAME_LAST,
      NAME_1 TYPE ADRP-NAME_LAST,
      KMMS   TYPE CHAR100, "打印科目描述
      NAME_B TYPE CHAR100, "公司代码描述
      END OF TY_DATA.

*人员信息
TYPES:BEGIN OF TY_NAME,
        BNAME      TYPE USR21-BNAME,      "帐号
        PERSNUMBER TYPE USR21-PERSNUMBER, "人员编号
        NAME_LAST  TYPE ADRP-NAME_LAST,   "姓
      END OF TY_NAME.
************************************************************************
* Internal Table * WorkArea
************************************************************************

DATA GT_ZFI004FZ TYPE TABLE OF ZFI004FZ.
DATA GS_ZFI004FZ TYPE ZFI004FZ.

DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.

DATA LT_DATA  TYPE TABLE OF TY_DATA.
DATA LS_DATA  TYPE TY_DATA.

DATA GT_NAME TYPE TABLE OF TY_NAME.
DATA GS_NAME TYPE TY_NAME.

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
SELECT-OPTIONS: S_BUKRS FOR BKPF-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_GJAHR FOR BKPF-GJAHR OBLIGATORY NO INTERVALS NO-EXTENSION,"会计年度
                S_MONAT FOR BKPF-MONAT,"记账期间
                S_BELNR FOR BKPF-BELNR,"凭证编码
                S_BLDAT FOR BKPF-BLDAT,"凭证日期
                S_BUDAT FOR BKPF-BUDAT,"过账日期
                S_BLART FOR BKPF-BLART,"凭证类型
                S_PPNAM FOR BKPF-PPNAM,"制单人
                S_HKONT FOR BSEG-HKONT,"科目
                S_LIFNR FOR BSEG-LIFNR,"供应商
                S_KUNNR FOR BSEG-KUNNR,"客户
                S_KOSTL FOR BSEG-KOSTL,"成本中心
                S_AUFNR FOR BSEG-AUFNR,"内部订单
                S_VBEL2 FOR BSEG-VBEL2,"销售订单号
                S_DMBTR FOR BSEG-DMBTR,"行项目本位币金额
                S_SGTXT FOR BSEG-SGTXT,"文本
                S_ZUONR FOR BSEG-ZUONR,"分配
                S_MATNR FOR BSEG-MATNR."物料号

PARAMETERS:      P_YZPZ AS CHECKBOX  TYPE CHAR1, "预制凭证
                 P_CXPZ AS CHECKBOX  TYPE CHAR1 , "冲销凭证
                 P_DYPZ AS CHECKBOX  TYPE CHAR1 , "需要打印的凭证
                 P_PZQD AS CHECKBOX  TYPE CHAR1 . "凭证清单

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
 AT SELECTION-SCREEN on VALUE-REQUEST FOR  S_PPNAM-LOW.
 perform getppnam.
 AT SELECTION-SCREEN on VALUE-REQUEST FOR  S_PPNAM-high.
 perform getppnam.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*权限检查检查公司代码
 " PERFORM FRM_AUTH_CHECK USING '03'.
 PERFORM FRM_AUTH_CHECK.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

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
*      -->P_0558   text
*----------------------------------------------------------------------*
"FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
 FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD S_BUKRS-LOW.
*  IF SY-SUBRC <> 0.
*   MESSAGE s899(mm) WITH '您没有公司代码' S_BUKRS '的权限' DISPLAY LIKE 'E'.
*  LEAVE LIST-PROCESSING.
*   ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  FRM_GET_DATA .
  RANGES:R_YZPZ FOR BKPF-BSTAT,
         R_DYPZ FOR BKPF-BELNR,
         R_PZQD FOR BKPF-BELNR.

  REFRESH:R_YZPZ ,
          R_DYPZ ,
          R_PZQD.

*根据CHECKBOX勾选内容进行排除

*预制凭证
  IF P_YZPZ = 'X'.
    R_YZPZ-SIGN = 'I'.
    R_YZPZ-OPTION = 'EQ'.
    R_YZPZ-LOW = 'V'.
    R_YZPZ-HIGH = ''.
    APPEND R_YZPZ.
  ENDIF.

*需要打印的凭证
  IF P_DYPZ = 'X'.
    R_DYPZ-SIGN = 'I'.
    R_DYPZ-OPTION = 'BT'.
    R_DYPZ-LOW = '0010000000'.
    R_DYPZ-HIGH = '0041999999'.
    APPEND R_DYPZ.

    R_DYPZ-SIGN = 'I'.
    R_DYPZ-OPTION = 'BT'.
    R_DYPZ-LOW = '0070000000'.
    R_DYPZ-HIGH = '0089999999'.
    APPEND R_DYPZ.
  ENDIF.

*凭证清单
  IF P_PZQD = 'X'.
    R_PZQD-SIGN = 'I'.
    R_PZQD-OPTION = 'BT'.
    R_PZQD-LOW = '0048000000'.
    R_PZQD-HIGH = '0059999999'.
    APPEND R_PZQD.

    R_PZQD-SIGN = 'I'.
    R_PZQD-OPTION = 'BT'.
    R_PZQD-LOW = '0097000000'.
    R_PZQD-HIGH = '0098999999'.
    APPEND R_PZQD.
  ENDIF.

*冲销凭证
  IF P_CXPZ = 'X'.
    SELECT * FROM ZFI004FZ
      INTO CORRESPONDING FIELDS OF TABLE GT_DATA
      WHERE BUKRS IN S_BUKRS
      AND   GJAHR IN S_GJAHR
      AND   MONAT IN S_MONAT
      AND   BELNR IN S_BELNR
      AND   BLDAT IN S_BLDAT
      AND   BUDAT IN S_BUDAT
      AND   BLART IN S_BLART
      AND   PPNAM IN S_PPNAM
      AND   HKONT IN S_HKONT
      AND   LIFNR IN S_LIFNR
      AND   KUNNR IN S_KUNNR
      AND   KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   VBEL2 IN S_VBEL2
      AND   DMBTR IN S_DMBTR
      AND   SGTXT IN S_SGTXT
      AND   BKTXT IN S_SGTXT
      AND   BSTAT IN R_YZPZ
      AND   BELNR IN R_DYPZ
      AND   BELNR IN R_PZQD
      AND   ZUONR IN S_ZUONR
      AND   MATNR IN S_MATNR
      AND   STBLG <> ''.
      IF S_PPNAM[] IS NOT INITIAL.
    SELECT * FROM ZFI004FZ
      APPENDING CORRESPONDING FIELDS OF TABLE GT_DATA
      WHERE BUKRS IN S_BUKRS
      AND   GJAHR IN S_GJAHR
      AND   MONAT IN S_MONAT
      AND   BELNR IN S_BELNR
      AND   BLDAT IN S_BLDAT
      AND   BUDAT IN S_BUDAT
      AND   BLART IN S_BLART
      AND   PPNAM EQ ''
      AND   USNAM IN S_PPNAM
      AND   PPNAM IN S_PPNAM
      AND   HKONT IN S_HKONT
      AND   LIFNR IN S_LIFNR
      AND   KUNNR IN S_KUNNR
      AND   KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   VBEL2 IN S_VBEL2
      AND   DMBTR IN S_DMBTR
      AND   SGTXT IN S_SGTXT
      AND   BKTXT IN S_SGTXT
      AND   BSTAT IN R_YZPZ
      AND   BELNR IN R_DYPZ
      AND   BELNR IN R_PZQD
      AND   ZUONR IN S_ZUONR
      AND   MATNR IN S_MATNR
      AND   STBLG <> ''.
       ENDIF.
  ELSE.
    SELECT * FROM ZFI004FZ
      INTO CORRESPONDING FIELDS OF TABLE GT_DATA
      WHERE BUKRS IN S_BUKRS
      AND   GJAHR IN S_GJAHR
      AND   MONAT IN S_MONAT
      AND   BELNR IN S_BELNR
      AND   BLDAT IN S_BLDAT
      AND   BUDAT IN S_BUDAT
      AND   BLART IN S_BLART
      AND   PPNAM IN S_PPNAM
      AND   HKONT IN S_HKONT
      AND   LIFNR IN S_LIFNR
      AND   KUNNR IN S_KUNNR
      AND   KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   VBEL2 IN S_VBEL2
      AND   DMBTR IN S_DMBTR
      AND   SGTXT IN S_SGTXT
      AND   BKTXT IN S_SGTXT
      AND   ZUONR IN S_ZUONR
      AND   BSTAT IN R_YZPZ
      AND   BELNR IN R_DYPZ
      AND   BELNR IN R_PZQD
      AND   MATNR IN S_MATNR.
       IF S_PPNAM[] IS NOT INITIAL.
      SELECT * FROM ZFI004FZ
      APPENDING CORRESPONDING FIELDS OF TABLE GT_DATA
      WHERE BUKRS IN S_BUKRS
      AND   GJAHR IN S_GJAHR
      AND   MONAT IN S_MONAT
      AND   BELNR IN S_BELNR
      AND   BLDAT IN S_BLDAT
      AND   BUDAT IN S_BUDAT
      AND   BLART IN S_BLART
      AND   PPNAM EQ ''
      AND   USNAM IN S_PPNAM
      AND   HKONT IN S_HKONT
      AND   LIFNR IN S_LIFNR
      AND   KUNNR IN S_KUNNR
      AND   KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   VBEL2 IN S_VBEL2
      AND   DMBTR IN S_DMBTR
      AND   SGTXT IN S_SGTXT
      AND   BKTXT IN S_SGTXT
      AND   ZUONR IN S_ZUONR
      AND   BSTAT IN R_YZPZ
      AND   BELNR IN R_DYPZ
      AND   BELNR IN R_PZQD
      AND   MATNR IN S_MATNR.
      ENDIF.
  ENDIF.

*获取制单人姓名
  SELECT  * FROM USR21
   INNER JOIN ADRP
   ON USR21~PERSNUMBER = ADRP~PERSNUMBER
   INTO CORRESPONDING FIELDS OF TABLE GT_NAME.

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
  LOOP AT GT_DATA INTO GS_DATA.
*公司名
    GS_DATA-NAME_B = GS_DATA-BUTXT.

*记账人,审核人
    READ TABLE GT_NAME INTO GS_NAME
    WITH KEY BNAME = GS_DATA-USNAM.
    IF SY-SUBRC = 0.
      GS_DATA-NAME = GS_NAME-NAME_LAST.
    ENDIF.

*制单人
    READ TABLE GT_NAME INTO GS_NAME
    WITH KEY BNAME = GS_DATA-PPNAM.
    IF SY-SUBRC = 0.
      GS_DATA-NAME_1 = GS_NAME-NAME_LAST.
    ENDIF.

*如果制单人为空就取记账人
    IF GS_DATA-NAME_1 IS INITIAL.
      GS_DATA-NAME_1 = GS_DATA-NAME.
    ENDIF.

*打印科目描述
    GS_DATA-KMMS = GS_DATA-TXT20."科目描述

*科目描述除了输出科目描述外，还需输出 客户/供应商/成本中心/销售订单/内部订单
    IF  GS_DATA-KUNNR  IS NOT INITIAL.
      CONCATENATE GS_DATA-KMMS '/' GS_DATA-KUNNR GS_DATA-NAME1 INTO GS_DATA-KMMS.

    ELSEIF GS_DATA-LIFNR IS NOT INITIAL.
      CONCATENATE GS_DATA-KMMS '/' GS_DATA-LIFNR GS_DATA-NAME1_1 INTO GS_DATA-KMMS.

    ELSEIF GS_DATA-KOSTL IS NOT INITIAL.
      CONCATENATE GS_DATA-KMMS '/' GS_DATA-KOSTL GS_DATA-KTEXT INTO GS_DATA-KMMS.

    ELSEIF GS_DATA-VBEL2 IS NOT INITIAL.
      CONCATENATE GS_DATA-KMMS '/' GS_DATA-VBEL2  INTO GS_DATA-KMMS.

    ELSEIF GS_DATA-AUFNR IS NOT INITIAL.
      CONCATENATE GS_DATA-KMMS '/' GS_DATA-AUFNR '/' GS_DATA-KTEXT_1 INTO GS_DATA-KMMS.
    ENDIF.

    MODIFY GT_DATA FROM GS_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

  SORT GT_DATA BY GJAHR BELNR .
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
  INIT_FIELDCAT 'BUKRS'   TEXT-001         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'   TEXT-002         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'GJAHR'   TEXT-003         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLART'   TEXT-004         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BSTAT'   TEXT-005         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLDAT'   TEXT-006         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'   TEXT-007         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'   TEXT-008        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XBLNR'   TEXT-009         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PPNAM'   TEXT-010         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'USNAM'   TEXT-011         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKTXT'   TEXT-012         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STBLG'   TEXT-013         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'   TEXT-014         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SHKZG'   TEXT-015         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'   TEXT-016         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXT20'   TEXT-017         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'   TEXT-018         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'   TEXT-019         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'   TEXT-020         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1' TEXT-021         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'   TEXT-022         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'   TEXT-023         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'   TEXT-024         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1_1' TEXT-025         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1'   TEXT-026         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXT50'   TEXT-027         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'   TEXT-028         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT'   TEXT-029         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR'   TEXT-030         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT_1' TEXT-031         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'   TEXT-032         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSN2'   TEXT-033         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'   TEXT-034         '' '' '' '' '' 'MARA' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'   TEXT-035         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'   TEXT-036         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WGBEZ'   TEXT-037         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RSTGR'   TEXT-038         '' '' '' '' '' 'BSEG' 'RSTGR'.
  INIT_FIELDCAT 'SGTXT'   TEXT-039         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZUONR'   TEXT-040         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AWKEY'   TEXT-041         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'   TEXT-042         '' '' '' '' '' '' ''. "数量
  INIT_FIELDCAT 'MEINS'   TEXT-043         '' '' '' '' '' '' ''. "单位
  INIT_FIELDCAT 'PSTYV'   TEXT-044        '' '' '' '' '' '' ''. "数量
  INIT_FIELDCAT 'UEPOS'   TEXT-045         '' '' '' '' '' '' ''. "单位
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
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
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
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'BELNR'
        AND GS_DATA-BSTAT <> 'V'
        AND GS_DATA-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'BELNR'
        AND GS_DATA-BSTAT = 'V'
        AND GS_DATA-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLP' FIELD GS_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
      ENDIF.
*打印
    WHEN '&PRNT'.
*需进行保存操作
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          E_GRID = G_REF_GRID.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        PERFORM FRM_PRINT_DATA.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
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
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 9,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFFI004'.
  DATA L_LINE TYPE I. "统计打印的行进行补行
  DATA G_LINE TYPE I. "设定换页行数
  DATA G_ALL_S TYPE DMBTR.
  DATA G_ALL_H TYPE DMBTR.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

*根据凭证编码进行排序

*对于任凭证,选中一行打印整张凭证.
  LOOP AT  GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
    LOOP AT  GT_DATA INTO GS_DATA
      WHERE BELNR = GS_DATA-BELNR
      AND   GJAHR = GS_DATA-GJAHR
      AND   BUKRS = GS_DATA-BUKRS.

      GS_DATA-ZBOX = 'X'.
      MODIFY GT_DATA FROM GS_DATA.
    ENDLOOP.
  ENDLOOP.

  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
    AT NEW BELNR.
      REFRESH LT_DATA.
      CLEAR L_LINE.
      CLEAR G_ALL_S.
      CLEAR G_ALL_H.
    ENDAT.

*汇总借贷金额
    IF GS_DATA-SHKZG = 'S'.
      G_ALL_S = G_ALL_S + GS_DATA-DMBTR.
    ELSEIF GS_DATA-SHKZG = 'H'.
      G_ALL_H = G_ALL_H + GS_DATA-DMBTR.
    ENDIF.

    APPEND GS_DATA TO LT_DATA.
    CLEAR GS_DATA.

    AT END OF BELNR.
      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
          G_ALL_S            = G_ALL_S
          G_ALL_H            = G_ALL_H
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDAT.
  ENDLOOP.


  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GETPPNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETPPNAM .
 DATA:BEGIN OF WS_PPNAM,
       BNAME LIKE USER_ADDR-BNAME,
       NAME_TEXTC LIKE USER_ADDR-NAME_TEXTC,
        END OF WS_PPNAM.
  DATA:TS_PPNAM LIKE TABLE OF WS_PPNAM.
  DATA:L_BUKRS LIKE T001-BUKRS.
  DATA:BUKRS1_NAME TYPE  CHAR30.
  DATA:BUKRS2_NAME TYPE  CHAR30 .
  PERFORM frm_get_field_value USING 'S_BUKRS-LOW' CHANGING L_BUKRS.
  CONCATENATE L_BUKRS 'FI%' INTO BUKRS1_NAME .
  CONCATENATE L_BUKRS 'CO%' INTO BUKRS2_NAME .
  SELECT BNAME NAME_TEXTC  into corresponding fields of table TS_PPNAM
     from USER_ADDR
     where BNAME LIKE BUKRS1_NAME  OR BNAME LIKE BUKRS2_NAME.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BNAME' "大写,可选值内表的字段名
      value_org       = 'S' "就写'S'
      dynpprog        = sy-repid "返回的输入框所在的main program
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'S_PPNAM' "返回的输入框名
      WINDOW_TITLE    = '制单人'
    TABLES
      value_tab       = TS_PPNAM"可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM frm_get_field_value USING value(f_fieldname) CHANGING value(f_fieldvalue).
  DATA: dynpro_values TYPE TABLE OF dynpread WITH HEADER LINE,
        field_value LIKE LINE OF dynpro_values.

  CLEAR: field_value, dynpro_values[].
  field_value-fieldname = f_fieldname.
  APPEND field_value TO dynpro_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpro_values.

  CHECK NOT dynpro_values[]  IS INITIAL.
  READ TABLE dynpro_values INDEX 1.
  CHECK sy-subrc = 0.
  f_fieldvalue =  dynpro_values-fieldvalue.
ENDFORM.                    "frm_get_field_value
