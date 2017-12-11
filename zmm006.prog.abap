*&---------------------------------------------------------------------*
*& Report  ZMM006
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/01/20
*& Request       :
*& Descriptions  : 外协领料单打印
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZMM006.

************************************************************************
* Tables
************************************************************************
TABLES: EKKO,EKPO,ZMM006.

************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF TY_DATA,
        ZBOX    TYPE C,
        EBELN   TYPE EKPO-EBELN, "采购订单号
        LGORT   TYPE MSEG-LGORT, "发料仓库
        EBELP   TYPE EKPO-EBELP, "采购订单行项目
        MATNR_Z TYPE EKPO-MATNR, "子件编码
        MAKTX_Z TYPE MAKT-MAKTX, "物料描述
        WERKS   TYPE MSEG-WERKS, "工厂
        LIFNR   TYPE EKKO-LIFNR, "委外加工商
        MATNR   TYPE EKPO-MATNR, "母件编码
        MAKTX   TYPE MAKT-MAKTX, "物料描述
        MENGE   TYPE EKPO-MENGE, "母件数量
        BDMNG   TYPE RESB-BDMNG, "需求数量
        LJSL    TYPE RESB-BDMNG, "累计数量
        MEINS   TYPE MSEG-MEINS, "单位
        MENGE_S TYPE MSEG-MENGE, "实际领料数量
        CHARG   TYPE MSEG-CHARG, "批号
        CLSL    TYPE MSEG-MENGE, "超领数量
        CLPC    TYPE MSEG-CHARG, "超领批次
        BZ      TYPE CHAR100,    "备注
        SQRQ    TYPE ZMM006-SQRQ, "申请日期
        NAME1   TYPE LFA1-NAME1, "供应商日期
        NAME2   TYPE CHAR20,       "制单人
        STATU   TYPE ICONNAME,    "状态栏
      END OF TY_DATA.

TYPES:BEGIN OF TY_EKPO,
        BUDAT TYPE MKPF-BUDAT, "入账日期
        WERKS TYPE MSEG-WERKS, "工厂
        LGORT TYPE MSEG-LGORT, "发料仓库
        LIFNR TYPE EKKO-LIFNR, "委外加工商
        EBELN TYPE EKPO-EBELN, "采购订单号
        EBELP TYPE EKPO-EBELP, "采购订单行项目
        MATNR TYPE EKPO-MATNR, "母件编码
        MENGE TYPE EKPO-MENGE, "母件数量
      END OF TY_EKPO.

*人员信息
TYPES:BEGIN OF TY_NAME,
        BNAME      TYPE USR21-BNAME,      "帐号
        PERSNUMBER TYPE USR21-PERSNUMBER, "人员编号
        NAME_LAST  TYPE ADRP-NAME_LAST,  "姓
      END OF TY_NAME.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GT_DATA_1 TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.

DATA GT_ZMM006 TYPE TABLE OF ZMM006.
DATA GS_ZMM006 TYPE ZMM006.

DATA GT_EKPO TYPE TABLE OF TY_EKPO.
DATA GS_EKPO TYPE TY_EKPO.

DATA GT_MSEG TYPE TABLE OF MSEG.
DATA GS_MSEG TYPE MSEG.

DATA GT_RESB TYPE TABLE OF RESB.
DATA GS_RESB TYPE RESB.

DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.

DATA GT_T001W  TYPE TABLE OF T001W WITH HEADER LINE.

DATA GT_MAKT_1 TYPE TABLE OF MAKT.
DATA GS_MAKT_1 TYPE MAKT.

DATA GT_MARC   TYPE TABLE OF MARC.
DATA GS_MARC   TYPE MARC.

*制单人姓名
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

  IF gw_lvc-fieldname = 'MENGE_S'.
     gw_lvc-NO_ZERO = 'X'.
  ENDIF.

  IF  gw_lvc-fieldname = 'CHARG'
  OR  gw_lvc-fieldname = 'CLPC'.
   gw_lvc-F4AVAILABL = 'X'.
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

DATA STBL       TYPE LVC_S_STBL.

DATA G_EDIT TYPE C VALUE 'X'. "控制不可编辑
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
SELECT-OPTIONS: S_WERKS FOR EKPO-WERKS NO INTERVALS NO-EXTENSION OBLIGATORY,
                S_EBELN FOR EKPO-EBELN,
                S_MATNR FOR EKPO-MATNR,
                S_LIFNR FOR EKKO-LIFNR,
                S_EKGRP FOR EKKO-EKGRP,
                S_SQRQ  FOR ZMM006-SQRQ.
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

*查询工厂
  SELECT * FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE GT_T001W.

  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.

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
  LOOP AT GT_T001W WHERE WERKS IN S_WERKS.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
             ID 'WERKS' FIELD GT_T001W-WERKS
             .
    IF SY-SUBRC <> 0.
      MESSAGE E603(FCO) WITH GT_T001W-WERKS.
    ENDIF.
  ENDLOOP.
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
*取出外协采购订单
  SELECT * FROM EKKO
  INNER JOIN EKPO ON
  EKKO~EBELN  = EKPO~EBELN
  INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
  WHERE EKPO~EBELN IN  S_EBELN
  AND   EKPO~MATNR IN  S_MATNR
  AND   EKPO~WERKS IN  S_WERKS
  AND   EKKO~LIFNR IN  S_LIFNR
  AND   EKKO~EKGRP IN  S_EKGRP
  AND   EKPO~PSTYP =   '3'.

  CHECK GT_EKPO IS NOT INITIAL.

*查询母件物料描述
  SELECT * FROM MAKT
   INTO CORRESPONDING FIELDS OF TABLE GT_MAKT_1
   FOR ALL ENTRIES IN GT_EKPO
   WHERE MATNR = GT_EKPO-MATNR.

*查询供应商描述
  SELECT * FROM LFA1
   INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
   FOR ALL ENTRIES IN GT_EKPO
   WHERE LIFNR = GT_EKPO-LIFNR.

*取组件需求
  SELECT * FROM RESB
   INTO CORRESPONDING FIELDS OF TABLE GT_RESB
   FOR ALL ENTRIES IN GT_EKPO
   WHERE EBELN = GT_EKPO-EBELN
   AND   EBELP = GT_EKPO-EBELP.

  CHECK GT_RESB IS NOT INITIAL.

*查询物料的库存地点
  SELECT * FROM MARC
    INTO CORRESPONDING FIELDS OF TABLE GT_MARC
    FOR ALL ENTRIES IN GT_RESB
    WHERE MATNR = GT_RESB-MATNR.

*取出物料描述
  SELECT * FROM MAKT
   INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FOR ALL ENTRIES IN GT_RESB
   WHERE MATNR = GT_RESB-MATNR.

*取自建表中实际领料数量
  SELECT * FROM ZMM006
   INTO CORRESPONDING FIELDS OF TABLE GT_ZMM006
   FOR ALL ENTRIES IN GT_EKPO
   WHERE EBELN = GT_EKPO-EBELN
   AND   EBELP = GT_EKPO-EBELP.

*根据日期筛选
  IF S_SQRQ IS NOT INITIAL.
    LOOP AT GT_RESB INTO GS_RESB.
      READ TABLE GT_ZMM006 INTO GS_ZMM006
      WITH KEY EBELN   = GS_RESB-EBELN
               EBELP   = GS_RESB-EBELP
               MATNR_Z = GS_RESB-MATNR.

      IF SY-SUBRC = 0.
        IF GS_ZMM006-SQRQ NOT IN S_SQRQ.
          DELETE GT_RESB WHERE EBELN    = GS_ZMM006-EBELN
                         AND   EBELP    = GS_ZMM006-EBELP
                         AND   MATNR    = GS_ZMM006-MATNR_Z.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*获取制单人姓名
  SELECT SINGLE * FROM USR21
   INNER JOIN ADRP
   ON USR21~PERSNUMBER = ADRP~PERSNUMBER
   INTO CORRESPONDING FIELDS OF GS_NAME
   WHERE BNAME = SY-UNAME.
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
  DATA MENGE_541 TYPE MSEG-MENGE.
  DATA MENGE_542 TYPE MSEG-MENGE.

  LOOP AT  GT_EKPO INTO GS_EKPO.

    MOVE-CORRESPONDING GS_EKPO TO GS_DATA.

*制单人
    GS_DATA-NAME2    = GS_NAME-NAME_LAST.


*取出母件物料描述
    READ TABLE GT_MAKT_1 INTO GS_MAKT_1
    WITH KEY MATNR = GS_EKPO-MATNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT_1-MAKTX.
    ENDIF.

*读取供应商描述
    READ TABLE GT_LFA1 INTO GS_LFA1
    WITH KEY LIFNR = GS_DATA-LIFNR.
    IF SY-SUBRC = 0.
      GS_DATA-NAME1 = GS_LFA1-NAME1.
    ENDIF.

*组件信息
    LOOP AT GT_RESB INTO GS_RESB
    WHERE EBELN = GS_DATA-EBELN
    AND   EBELP = GS_DATA-EBELP.
      CLEAR: MENGE_541, MENGE_542.
      GS_DATA-MATNR_Z = GS_RESB-MATNR."组件物料号
      GS_DATA-BDMNG   = GS_RESB-BDMNG."组件需求数
      GS_DATA-MEINS   = GS_RESB-MEINS."单位
      GS_DATA-CHARG   = GS_RESB-CHARG."批次

*带出默认的仓储地点
      READ TABLE GT_MARC INTO GS_MARC
      WITH KEY MATNR = GS_DATA-MATNR_Z
               WERKS = GS_DATA-WERKS.
      IF SY-SUBRC = 0.
        GS_DATA-LGORT = GS_MARC-LGPRO.
      ENDIF.

*取出子件物料描述
      READ TABLE GT_MAKT INTO GS_MAKT
      WITH KEY MATNR = GS_DATA-MATNR_Z
               SPRAS = SY-LANGU.
      IF SY-SUBRC = 0.
        GS_DATA-MAKTX_Z = GS_MAKT-MAKTX.
      ENDIF.

*取出实际领料数量
      READ TABLE GT_ZMM006 INTO GS_ZMM006
      WITH KEY EBELN   = GS_DATA-EBELN
               EBELP   = GS_DATA-EBELP
               MATNR_Z = GS_DATA-MATNR_Z.
      IF SY-SUBRC = 0.
        GS_DATA-MENGE_S = GS_ZMM006-BDMNG - GS_ZMM006-LJSL. "GS_ZMM006-MENGE_S.
        IF GS_DATA-MENGE_S < 0.
          GS_DATA-MENGE_S = 0.
        ENDIF.

*        GS_DATA-CLSL    = GS_ZMM006-CLSL. "GS_DATA-MENGE_S - GS_DATA-BDMNG.
        GS_DATA-CHARG   = GS_ZMM006-CHARG.
        GS_DATA-BZ      = GS_ZMM006-BZ.
        GS_DATA-CLPC    = GS_ZMM006-CLPC.
        GS_DATA-SQRQ    = GS_ZMM006-SQRQ.
*        GS_DATA-LJSL    = GS_ZMM006-LJSL. "累计数量
        GS_DATA-LGORT   = GS_ZMM006-LGORT.
      ELSE.
        GS_DATA-MENGE_S = GS_DATA-BDMNG.
*        GS_DATA-CLSL    = ''.
        GS_DATA-CHARG   = ''.
        GS_DATA-BZ      = ''.
        GS_DATA-CLPC    = ''.
        GS_DATA-SQRQ    = SY-DATUM.
      ENDIF.

      "累计领料数量 541-542
      SELECT SUM( MENGE ) FROM MSEG INTO MENGE_541
        WHERE EBELN EQ GS_DATA-EBELN AND EBELP EQ GS_DATA-EBELP
        AND BWART EQ '541' AND LGORT NE SPACE AND MATNR EQ GS_DATA-MATNR_Z.

      SELECT SUM( MENGE ) FROM MSEG INTO MENGE_542
        WHERE EBELN EQ GS_DATA-EBELN AND EBELP EQ GS_DATA-EBELP
        AND BWART EQ '542' AND LGORT NE SPACE AND MATNR EQ GS_DATA-MATNR_Z.

      GS_DATA-LJSL = MENGE_541 - MENGE_542.

*当满足需求时候
      IF GS_DATA-LJSL >= GS_DATA-BDMNG.
        GS_DATA-MENGE_S = ''.
      ENDIF.

*当累计数量不为空，需减去累计的领料数量
      IF ( GS_DATA-LJSL < GS_DATA-BDMNG ) AND GS_DATA-LJSL <> 0.
        GS_DATA-MENGE_S =  GS_DATA-BDMNG - GS_DATA-LJSL.
      ENDIF.

      "超领数量
      GS_DATA-CLSL = GS_DATA-LJSL - GS_DATA-BDMNG.

      IF GS_DATA-CLSL < 0.
        GS_DATA-CLSL = 0.
      ENDIF.

      APPEND GS_DATA TO GT_DATA.
    ENDLOOP.

*自建表数据
    CLEAR GS_DATA.
  ENDLOOP.

  SORT  GT_DATA BY EBELN LGORT.
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
  INIT_FIELDCAT 'STATU'        TEXT-020         10 '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'        TEXT-001         '' '' '' '' '' 'ZMM006' 'WERKS'.
  INIT_FIELDCAT 'LIFNR'        TEXT-002      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'        TEXT-017      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'        TEXT-003      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELP'        TEXT-004    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'        TEXT-005       '' '' '' '' 'X' 'ZMM006' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'        TEXT-019       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'        TEXT-006       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR_Z'      TEXT-007       '' '' '' '' 'X' 'ZMM006' 'MATNR_Z'.
  INIT_FIELDCAT 'MAKTX_Z'      TEXT-019       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BDMNG'        TEXT-008       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'        TEXT-009         '' '' '' '' ''      'MCHB' 'MEINS'.
  INIT_FIELDCAT 'LJSL'         TEXT-018        '' '' '' '' ''   'ZMM006' 'LJSL'.
  INIT_FIELDCAT 'LGORT'        TEXT-010       '' '' ''  G_EDIT ''   'ZMM006' 'LGORT'.
  INIT_FIELDCAT 'MENGE_S'      TEXT-011     '' '' ''    G_EDIT ''   'ZMM006' 'MENGE'.
  INIT_FIELDCAT 'CHARG'        TEXT-012       '' '' ''  G_EDIT ''   'ZMM006' 'CHARG'.
  INIT_FIELDCAT 'CLSL'         TEXT-013       '' '' ''  '' ''   'ZMM006' 'MENGE'.
  INIT_FIELDCAT 'CLPC'         TEXT-015       '' '' ''  G_EDIT ''   'ZMM006' 'CHARG'.
  INIT_FIELDCAT 'BZ'           TEXT-014       '' '' ''  G_EDIT ''   'ZMM006' 'BZ'.
  INIT_FIELDCAT 'SQRQ'         TEXT-016       '' '' ''  G_EDIT ''   'ZMM006' 'SQRQ'.
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
  DATA L_SUBRC TYPE SY-SUBRC.
  CLEAR L_SUBRC.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
    WHEN '&DATA_SAVE'.
      DATA L_ANS.


*更新数据库表
      REFRESH GT_ZMM006.
      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.

*判断是否选中行项目
      IF SY-SUBRC = 0.

        PERFORM CHECK_INPUT CHANGING L_SUBRC.
        IF L_SUBRC = 4.
          MESSAGE S021(ZMM01) DISPLAY LIKE 'E'.
        ELSE.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR              = '确认保存'
*             DIAGNOSE_OBJECT       = ' '
              TEXT_QUESTION         = '确定要保存选中数据？'
              TEXT_BUTTON_1         = '是'(B01)
*             ICON_BUTTON_1         = ' '
              TEXT_BUTTON_2         = '否'(B02)
*             ICON_BUTTON_2         = ' '
*             DEFAULT_BUTTON        = '1'
              DISPLAY_CANCEL_BUTTON = ''
*             USERDEFINED_F1_HELP   = ' '
*             START_COLUMN          = 25
*             START_ROW             = 6
*             POPUP_TYPE            =
*             IV_QUICKINFO_BUTTON_1 = ' '
*             IV_QUICKINFO_BUTTON_2 = ' '
            IMPORTING
              ANSWER                = L_ANS
*   TABLES
*             PARAMETER             =
            EXCEPTIONS
              TEXT_NOT_FOUND        = 1
              OTHERS                = 2.
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.

          CHECK L_ANS EQ '1'.

          LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
*          GS_DATA-LJSL = GS_DATA-LJSL + GS_DATA-MENGE_S.
            MOVE-CORRESPONDING GS_DATA TO GS_ZMM006.
            APPEND GS_ZMM006 TO GT_ZMM006.
            CLEAR GS_ZMM006.

            MODIFY GT_DATA FROM GS_DATA.
            CLEAR GS_DATA.
          ENDLOOP.

          MODIFY ZMM006 FROM TABLE GT_ZMM006.
          IF SY-SUBRC = 0.
            MESSAGE S002(Z001).
          ENDIF.

*更改到查看状态
          CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
            EXPORTING
              I_READY_FOR_INPUT = 0.
        ENDIF.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

*编辑
    WHEN '&EDIT'.
      CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.

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

*检查库存数量
          PERFORM CHECK_INPUT CHANGING L_SUBRC.
          IF L_SUBRC = 4..
            MESSAGE S021(ZMM01) DISPLAY LIKE 'E'.
          ELSE.
            PERFORM FRM_PRINT_DATA.
          ENDIF.


        ELSE.
          MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'MATNR'
        AND GS_DATA-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD GS_DATA-MATNR.
        SET PARAMETER ID 'WRK' FIELD S_WERKS-LOW.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
      ENDIF.

      IF RS_SELFIELD-FIELDNAME = 'MATNR_Z'
        AND GS_DATA-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD GS_DATA-MATNR_Z.
        SET PARAMETER ID 'WRK' FIELD S_WERKS-LOW.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

*自动定位光标
  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.

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

  DATA: G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL       TYPE LVC_S_STBL.
  DATA LT_OUT TYPE  TABLE OF TY_DATA.
  DATA LS_OUT TYPE TY_DATA.

  FIELD-SYMBOLS:<L_MENGE_S> TYPE ANY.

  REFRESH LT_OUT.
  ASSIGN ER_DATA_CHANGED->MP_MOD_ROWS->* TO <L_MENGE_S>.
  LT_OUT = <L_MENGE_S>.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*更新超领数量
  LOOP AT LT_OUT INTO LS_OUT.
    READ TABLE GT_DATA INTO GS_DATA
    WITH KEY     EBELN   = LS_OUT-EBELN
                 EBELP   = LS_OUT-EBELP
                 MATNR_Z = LS_OUT-MATNR_Z.

    IF SY-SUBRC = 0.
*      GS_DATA-LJSL = LS_OUT-LJSL + LS_OUT-MENGE_S.

      GS_DATA-CLSL  = LS_OUT-MENGE_S + LS_OUT-LJSL - LS_OUT-BDMNG.
      GS_DATA-LGORT = LS_OUT-LGORT.
      GS_DATA-CHARG = LS_OUT-CHARG.

*超领数量小于0时候，等于0
      IF GS_DATA-CLSL < 0.
        GS_DATA-CLSL  = 0.
      ENDIF.

      MODIFY GT_DATA FROM GS_DATA TRANSPORTING CLSL      WHERE EBELN   = GS_DATA-EBELN
                                                         AND   EBELP   = GS_DATA-EBELP
                                                         AND   MATNR_Z = GS_DATA-MATNR_Z.
      CLEAR LS_OUT.
    ENDIF.
  ENDLOOP.

*自动定位光标
  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.



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
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFMM006'.
*  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
*  DATA:LW_PRT LIKE LINE OF IT_PRT.
  DATA L_LINE TYPE I. "统计打印的行进行补行
  DATA G_LINE TYPE I. "设定换页行数

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

*根据订单号，行项目号进行排序
  REFRESH GT_DATA_1.
  GT_DATA_1 = GT_DATA.

  SORT  GT_DATA_1 BY ZBOX EBELN LGORT.

  LOOP AT GT_DATA_1 INTO GS_DATA WHERE ZBOX = 'X'.
    AT NEW EBELN .
      REFRESH LT_DATA.
    ENDAT.

    AT NEW LGORT.
      REFRESH LT_DATA.
    ENDAT.

    APPEND GS_DATA TO LT_DATA .
    CLEAR GS_DATA.

    AT END OF LGORT.
*每页补齐8行
      DESCRIBE TABLE LT_DATA LINES L_LINE.
      WHILE L_LINE MOD 8 <> 0.
        L_LINE = L_LINE + 1.
        APPEND INITIAL LINE TO LT_DATA.
      ENDWHILE.
*设定10行自动换页
      G_LINE = 8.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
*         npage              = npageline
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
      CONTINUE.
    ENDAT.

*    AT END OF EBELP.
**每页补齐8行
*      DESCRIBE TABLE LT_DATA LINES L_LINE.
*      WHILE L_LINE MOD 8 <> 0.
*        L_LINE = L_LINE + 1.
*        APPEND INITIAL LINE TO LT_DATA.
*      ENDWHILE.
**设定10行自动换页
*      G_LINE = 8.
*
*      CALL FUNCTION G_NAME
*        EXPORTING
*          CONTROL_PARAMETERS = CONTROL
*          G_LINE             = G_LINE
**         npage              = npageline
**         w_head             = lw_prt
**         TABLES
**         t_item             = lt_prt[]
*        EXCEPTIONS
*          FORMATTING_ERROR   = 1
*          INTERNAL_ERROR     = 2
*          SEND_ERROR         = 3
*          USER_CANCELED      = 4
*          OTHERS             = 5.
*      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      CONTINUE.
*    ENDAT.


    AT END OF EBELN.
*每页补齐8行
      DESCRIBE TABLE LT_DATA LINES L_LINE.
      WHILE L_LINE MOD 8 <> 0.
        L_LINE = L_LINE + 1.
        APPEND INITIAL LINE TO LT_DATA.
      ENDWHILE.

*设定10行自动换页
      G_LINE = 8.
      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
*         npage              = npageline
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
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT  CHANGING L_SUBRC TYPE SY-SUBRC.
  DATA LT_MARD TYPE TABLE OF MARD.
  DATA LS_MARD TYPE MARD.
  DATA L_ANS.
  DATA L_STRING TYPE STRING.

  CLEAR L_ANS.

  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X' AND WERKS NE '2110'.
    CLEAR LS_MARD.

    SELECT SINGLE * FROM MARD
     INTO CORRESPONDING FIELDS OF LS_MARD
     WHERE MATNR = GS_DATA-MATNR_Z
     AND   LGORT = GS_DATA-LGORT
     AND   WERKS = S_WERKS-LOW.

    IF LS_MARD-LABST < GS_DATA-MENGE_S  OR GS_DATA-MENGE_S = ''.
*      CLEAR L_STRING.
*      CONCATENATE GS_DATA-MATNR_Z '物料在' GS_DATA-LGORT '库存地点下数量不足,是否继续打印？'  INTO L_STRING.


*库存地点清空（当库存不够时候） CHANGE BY HANDWY.
*      GS_DATA-LGORT = ''.
      GS_DATA-MENGE_S = ''.
      GS_DATA-STATU = ICON_RED_LIGHT.
      L_SUBRC = 4.
      MODIFY GT_DATA FROM GS_DATA.
      CLEAR GS_DATA.
    ELSE.
      GS_DATA-STATU = ICON_GREEN_LIGHT.
      MODIFY GT_DATA FROM GS_DATA.
      CLEAR GS_DATA.
*ENDADD.


*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          TITLEBAR              = '确认打印'
**         DIAGNOSE_OBJECT       = ' '
*          TEXT_QUESTION         = L_STRING
*          TEXT_BUTTON_1         = '是'(B01)
**         ICON_BUTTON_1         = ' '
*          TEXT_BUTTON_2         = '否'(B02)
**         ICON_BUTTON_2         = ' '
**         DEFAULT_BUTTON        = '1'
*          DISPLAY_CANCEL_BUTTON = ''
**         USERDEFINED_F1_HELP   = ' '
**         START_COLUMN          = 25
**         START_ROW             = 6
**         POPUP_TYPE            =
**         IV_QUICKINFO_BUTTON_1 = ' '
**         IV_QUICKINFO_BUTTON_2 = ' '
*        IMPORTING
*          ANSWER                = L_ANS
**   TABLES
**         PARAMETER             =
*        EXCEPTIONS
*          TEXT_NOT_FOUND        = 1
*          OTHERS                = 2.
*      IF SY-SUBRC <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      IF L_ANS EQ '1'.
*        L_SUBRC = 0.
*        CONTINUE.
*      ELSE.
*        L_SUBRC = 4.
*        EXIT.
*      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.
