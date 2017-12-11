*&---------------------------------------------------------------------*
*& REPORT  ZMM006
*&
*&---------------------------------------------------------------------*
*& CREATE BY     : 汪昱 （HAND)
*& CREATE DATE   : 2015/01/30
*& REQUEST       :
*& DESCRIPTIONS  : 部门&项目领料退料单打印

*&
*& MODIFY BY     :
*& MODIFY DATE   :
*& REQUEST       :
*& DESCRIPTIONS  :
*&
*&---------------------------------------------------------------------*
REPORT ZMM007.

************************************************************************
* TABLES
************************************************************************
TABLES: MSEG,MKPF,ZMM007,PRPS .

************************************************************************
* TYPE DECLARATION
************************************************************************
TYPES:BEGIN OF TY_DATA.
TYPES  ZBOX  TYPE C.
        INCLUDE STRUCTURE ZMM007.
TYPES:MAKTX TYPE MAKT-MAKTX,
      NAME1 TYPE CHAR20,     "制单人
      END OF TY_DATA.

TYPES:BEGIN OF TY_DATA_1,
        ZBOX  TYPE C,
        KOSTL TYPE MSEG-KOSTL, "成本中心
        AUFNR TYPE MSEG-AUFNR, "内部订单号
        BUDAT TYPE MKPF-BUDAT, "凭证日期
        WERKS TYPE MSEG-WERKS, "工厂
        MBLNR TYPE MSEG-MBLNR, "物料凭证
        MATNR TYPE MSEG-MATNR, "物料
        MAKTX TYPE MAKT-MAKTX, "物料描述
        LGORT TYPE MSEG-LGORT, "发货工厂
        MENGE TYPE MSEG-MENGE, "领料数量
        MEINS TYPE MSEG-MEINS, "领料单位
        CHARG TYPE MSEG-CHARG, "领料批次
        BKTXT TYPE MKPF-BKTXT, "凭证抬头文本
        BWART TYPE MSEG-BWART, "移动类型
        KDAUF TYPE MSEG-KDAUF, "销售订单号
        NAME1 TYPE CHAR20,     "制单人
*&--代码添加 BY HANDYBY 16.08.2017 18:27:52  BEGIN
        POSID TYPE PRPS-POSID,  " 项目WBS
*&--代码添加 BY HANDYBY 16.08.2017 18:27:52  END
      END OF TY_DATA_1.

*人员信息
TYPES:BEGIN OF TY_NAME,
        BNAME      TYPE USR21-BNAME,      "帐号
        PERSNUMBER TYPE USR21-PERSNUMBER, "人员编号
        NAME_LAST  TYPE ADRP-NAME_LAST,  "姓
      END OF TY_NAME.

************************************************************************
* INTERNAL TABLE * WORKAREA
************************************************************************
*更新数据库数据
DATA  GT_ZMM007 TYPE TABLE OF ZMM007.
DATA  GS_ZMM007 TYPE ZMM007.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

*ALV输出数据
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_DATA_1 TYPE TABLE OF TY_DATA_1.
DATA GS_DATA_1 TYPE TY_DATA_1.

*打印出入数据
DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

DATA LT_DATA_1 TYPE TABLE OF TY_DATA_1.
DATA LS_DATA_1 TYPE TY_DATA_1.

DATA GT_T001W TYPE TABLE OF T001W WITH HEADER LINE.

*制单人姓名
DATA GS_NAME TYPE TY_NAME.
************************************************************************
*      DEFINITION
************************************************************************
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
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.

  IF GW_LVC-FIELDNAME = 'MENGE'.
     GW_LVC-NO_ZERO = 'X'.
  ENDIF.
*根据打印单据类型不同，选择不同的屏幕
  IF G1 = 'X'.
    IF GW_LVC-FIELDNAME = 'BWART'.
       GW_LVC-REF_FIELD  = 'BWART'.
       GW_LVC-REF_TABLE  = 'ZMM007_1'.
      ENDIF.
  ENDIF.

  IF G3 = 'X'.
    IF GW_LVC-FIELDNAME = 'BWART'.
       GW_LVC-REF_FIELD  = 'BWART'.
       GW_LVC-REF_TABLE  = 'ZMM007_2'.
      ENDIF.
  ENDIF.

  IF G4 = 'X'.
    IF GW_LVC-FIELDNAME = 'BWART'.
       GW_LVC-REF_FIELD  = 'BWART'.
       GW_LVC-REF_TABLE  = 'ZMM007_3'.
      ENDIF.
  ENDIF.

*&--代码添加 BY HANDYBY 16.08.2017 18:55:21  BEGIN
  IF G5 = 'X'.
    IF GW_LVC-FIELDNAME = 'BWART'.
       GW_LVC-REF_FIELD  = 'BWART'.
       GW_LVC-REF_TABLE  = 'ZMM007_4'.
      ENDIF.
  ENDIF.
*&--代码添加 BY HANDYBY 16.08.2017 18:55:21  END

*ZMM007_1
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

DATA STBL       TYPE LVC_S_STBL.

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
SELECT-OPTIONS: S_KOSTL FOR MSEG-KOSTL,
                S_AUFNR FOR MSEG-AUFNR,
                S_MATNR FOR MSEG-MATNR,
                S_WERKS FOR MSEG-WERKS,
                S_SQRQ  FOR ZMM007-SQRQ MODIF ID Z1 DEFAULT SY-DATUM,
                S_MBLNR FOR MSEG-MBLNR  MODIF ID Z2,
                S_BUDAT FOR MKPF-BUDAT  MODIF ID Z2.
SELECT-OPTIONS S_WBS FOR PRPS-POSID MODIF ID Z3 .
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2.
PARAMETERS G1 TYPE CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND G_UCMD.
PARAMETERS G3 TYPE CHAR1 RADIOBUTTON GROUP G1.
PARAMETERS G4 TYPE CHAR1 RADIOBUTTON GROUP G1.
*&--代码添加 BY HANDYBY 16.08.2017 17:45:58  BEGIN
PARAMETERS G5 TYPE CHAR1 RADIOBUTTON GROUP G1.
*&--代码添加 BY HANDYBY 16.08.2017 17:45:58  END
PARAMETERS G2 TYPE CHAR1 RADIOBUTTON GROUP G1.

SELECTION-SCREEN END OF BLOCK BLK2.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'Z1'.
      IF G1 = 'X' OR G3 = 'X' OR G4 = 'X' OR G5 = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'Z2'.
      IF G2 = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'Z3'.
      IF G5 = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*查询工厂
  SELECT * FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE GT_T001W.

  PERFORM FRM_AUTH_CHECK.

*申请单打印
  IF G2 = 'X'.
    PERFORM FRM_GET_DATA_1. "取数逻辑
    PERFORM FRM_DEAL_DATA_1."处理数逻辑
    PERFORM FRM_ALV_SHOW_1. "ALV显示

  ELSE.
*物料凭证打印
    PERFORM FRM_GET_DATA. "取数逻辑
    PERFORM FRM_DEAL_DATA."处理数逻辑
    PERFORM FRM_ALV_SHOW. "ALV显示
  ENDIF.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
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
*&      FORM  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  IF G1 = 'X'.
*产看申请表
    SELECT * FROM ZMM007
      INTO CORRESPONDING FIELDS OF TABLE GT_ZMM007
      WHERE KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   SQRQ  IN S_SQRQ
      AND   NAME  = SY-UNAME
      AND   ( BWART = '201'
      OR      BWART = '202').
  ENDIF.

  IF G3 = 'X'.
*产看申请表
    SELECT * FROM ZMM007
      INTO CORRESPONDING FIELDS OF TABLE GT_ZMM007
      WHERE KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   SQRQ  IN S_SQRQ
      AND   NAME  = SY-UNAME
      AND   ( BWART = 'Z01'
      OR      BWART = 'Z02').
  ENDIF.

  IF G4 = 'X'.
*产看申请表
    SELECT * FROM ZMM007
      INTO CORRESPONDING FIELDS OF TABLE GT_ZMM007
      WHERE KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   SQRQ  IN S_SQRQ
      AND   NAME  = SY-UNAME
      AND   ( BWART = '231'
      OR      BWART = '232').
  ENDIF.

*&--代码添加 BY HANDYBY 16.08.2017 18:06:21  BEGIN
  IF G5 = 'X'.
*产看申请表
    SELECT * FROM ZMM007
      INTO CORRESPONDING FIELDS OF TABLE GT_ZMM007
      WHERE KOSTL IN S_KOSTL
      AND   AUFNR IN S_AUFNR
      AND   MATNR IN S_MATNR
      AND   WERKS IN S_WERKS
      AND   SQRQ  IN S_SQRQ
      AND   NAME  = SY-UNAME
      AND   POSID IN S_WBS
      AND   ( BWART = 'Z31'
      OR      BWART = 'Z32'
      OR      BWART = '282').
  ENDIF.
*&--代码添加 BY HANDYBY 16.08.2017 18:06:21  END

*查看物料描述
  IF GT_ZMM007 IS NOT INITIAL.
    SELECT * FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
      FOR ALL ENTRIES IN GT_ZMM007
      WHERE MATNR = GT_ZMM007-MATNR
        AND SPRAS = SY-LANGU.
  ENDIF.


*获取制单人姓名
  SELECT SINGLE * FROM USR21
   INNER JOIN ADRP
   ON USR21~PERSNUMBER = ADRP~PERSNUMBER
   INTO CORRESPONDING FIELDS OF GS_NAME
   WHERE BNAME = SY-UNAME.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .

  LOOP AT GT_ZMM007 INTO GS_ZMM007.
    MOVE-CORRESPONDING GS_ZMM007  TO GS_DATA.

*制单人
    GS_DATA-NAME1    = GS_NAME-NAME_LAST.

*取出物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_ZMM007-MATNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
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
*&      FORM  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA         = 'X'.
*  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  INIT_SORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  INIT_VARIANT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'SQRQ'         TEXT-002       '' '' '' 'X' '' 'ZMM007' 'SQRQ'.
  INIT_FIELDCAT 'MATNR'        TEXT-004       '' '' '' 'X' '' 'MCHB' 'MATNR'.
  INIT_FIELDCAT 'BWART'        TEXT-018       '8' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'WERKS'        TEXT-003       '' '' '' 'X' '' 'MCHB' 'WERKS'.
*  INIT_FIELDCAT 'MBLNR'        TEXT-014       '' '' '' 'X' '' 'MSEG' 'MBLNR'.
  INIT_FIELDCAT 'MAKTX'        TEXT-005       '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LGORT'        TEXT-006       '8' '' '' 'X' '' 'MCHB' 'LGORT'.
  INIT_FIELDCAT 'MENGE'        TEXT-007       '' '' '' 'X' '' 'MSEG' 'MENGE'.
  INIT_FIELDCAT 'MEINS'        TEXT-008       '8' '' '' '' '' 'EKPO' 'MEINS'.
  INIT_FIELDCAT 'CHARG'        TEXT-009       '10' '' '' 'X' '' 'MCHB' 'CHARG'.
  INIT_FIELDCAT 'KOSTL'        TEXT-010       '' '' '' 'X' '' 'CSKS' 'KOSTL'.
  INIT_FIELDCAT 'AUFNR'        TEXT-011       '' '' '' 'X' '' 'MSEG' 'AUFNR'.
  INIT_FIELDCAT 'VBELN'        TEXT-019       '' '' '' 'X' '' 'MSEG' 'KDAUF'.
*&--代码添加 BY HANDYBY 16.08.2017 18:10:42  BEGIN
  IF G5 = 'X'.
    INIT_FIELDCAT 'POSID'        TEXT-020       '' '' '' 'X' '' 'PRPS' 'POSID'.
  ENDIF.
*&--代码添加 BY HANDYBY 16.08.2017 18:10:42  END
  INIT_FIELDCAT 'BZ'           TEXT-012       '15' '' '' 'X' '' 'ZMM007' 'BZ'.
  INIT_FIELDCAT 'NAME'         TEXT-015       '8' '' '' '' '' 'ZMM007' 'NAME'.
  INIT_FIELDCAT 'DATE_C'       TEXT-016       '8' '' '' '' '' 'ZMM007' 'DATE_C'.
  INIT_FIELDCAT 'TIME'         TEXT-017       '8' '' '' '' '' 'ZMM007' 'TIME'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GT_LVC  TEXT
*      -->P_GT_SORT  TEXT
*      -->P_GT_ZMM007  TEXT
*      -->P_0413   TEXT
*      -->P_0414   TEXT
*      -->P_GW_LAYOUT  TEXT
*      -->P_GW_VARIANT  TEXT
*      -->P_GW_GRID_SETTINGS  TEXT
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
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      FORM  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID   TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
  DATA G_ANSWER     TYPE STRING. "控制弹出框
  DATA L_SUBRC      TYPE SY-SUBRC.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
    WHEN '&ADD'.
      APPEND INITIAL LINE TO GT_DATA.

    WHEN '&DELE'.

*弹出框提示
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION  = '是否删除选中的行'
        IMPORTING
          ANSWER         = G_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
      IF G_ANSWER <> '1'.
        EXIT.
      ENDIF.

*删除删除数据库表
      REFRESH GT_ZMM007.
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        MOVE-CORRESPONDING GS_DATA TO GS_ZMM007.
        APPEND GS_ZMM007 TO GT_ZMM007.
        CLEAR GS_ZMM007.
      ENDLOOP.

      DELETE ZMM007 FROM TABLE GT_ZMM007.
      DELETE GT_DATA WHERE ZBOX = 'X'.

    WHEN '&DATA_SAVE'.
      CLEAR L_SUBRC.

*检查输入字段（必须性检查）
      PERFORM CHECK_INPUT CHANGING L_SUBRC.

      CHECK L_SUBRC <> 4.

*更新数据库表
      REFRESH GT_ZMM007.
      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.

*判断是否选中行项目
      IF SY-SUBRC = 0.
        LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.

*如果物料号为空
          IF GS_DATA-MATNR IS INITIAL.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING GS_DATA TO GS_ZMM007.

*根据屏幕字段
          GS_DATA-NAME    = SY-UNAME.
          GS_DATA-DATE_C  = SY-DATUM.
          GS_DATA-TIME    = SY-UZEIT.

          MODIFY GT_DATA FROM GS_DATA.

*创建者赋值
          GS_ZMM007-NAME    = SY-UNAME.
          GS_ZMM007-DATE_C  = SY-DATUM.
          GS_ZMM007-TIME    = SY-UZEIT.


          APPEND GS_ZMM007 TO GT_ZMM007.
          CLEAR GS_ZMM007.
        ENDLOOP.

        MODIFY ZMM007 FROM TABLE GT_ZMM007.
        IF SY-SUBRC = 0.
          MESSAGE S002(Z001).
        ENDIF.

*      ET_FIELDCAT_LVC = GT_FIELDCAT

*更改到查看状态
        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
          EXPORTING
            I_READY_FOR_INPUT = 0.

      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.
*编辑
    WHEN '&EDIT'.

      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 0.

        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
          EXPORTING
            I_READY_FOR_INPUT = 1.

      ENDIF.

*打印
    WHEN '&PRNT'.
*需进行保存操作
*      ET_FIELDCAT_LVC = GT_FIELDCAT
      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 1.
        MESSAGE S001(ZMM01)  DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE GT_DATA INTO GS_DATA
        WITH KEY ZBOX = 'X'.
        IF SY-SUBRC = 0.
          PERFORM FRM_PRINT_DATA.
        ELSE.
          MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        ENDIF.
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
*&FORM  FRM_DATA_CHANGED
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  TEXT
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  PERFORM FRM_DATA_ENTER USING ER_DATA_CHANGED..
ENDFORM.     "FRM_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      FORM  FRM_DATA_ENTER
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL       TYPE LVC_S_STBL.
  DATA LT_OUT TYPE  TABLE OF TY_DATA.
  DATA LS_OUT TYPE TY_DATA.

  DATA:LS_MAKT TYPE MAKT .

  DATA:LS_MARA TYPE MARA.

  DATA: WA_MOD_CELL TYPE LVC_S_MODI.

  FIELD-SYMBOLS:<L_MATNR> TYPE ANY.

  CLEAR:LS_MAKT,
        LS_MARA.

  REFRESH LT_OUT.

  "MODIFIED BY:IT02 20161008 由原只针对第一行更改物料的变动扩为：现目前快速复制多个物料的更改
  "针对物料号更改
  LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL WHERE FIELDNAME = 'MATNR'.
    CLEAR:LS_MAKT .
    SELECT SINGLE * FROM MAKT
   INTO CORRESPONDING FIELDS OF LS_MAKT
   WHERE MATNR = WA_MOD_CELL-VALUE
   AND SPRAS = SY-LANGU.

    CLEAR:LS_MARA .
    SELECT SINGLE * FROM MARA
      INTO CORRESPONDING FIELDS OF LS_MARA
      WHERE MATNR = WA_MOD_CELL-VALUE.


    READ TABLE  GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
    IF SY-SUBRC = 0.
      CLEAR:GS_DATA-MAKTX,GS_DATA-MEINS .
      GS_DATA-MAKTX = LS_MAKT-MAKTX.  "*带出物料描述
      GS_DATA-MEINS = LS_MARA-MEINS.  " *带出物料主数据单位

      MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
      CLEAR GS_DATA.
    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = G_REF_GRID.

    CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

  ENDLOOP .
*  READ TABLE ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL INDEX 1.
*
*  TRANSLATE WA_MOD_CELL-VALUE TO UPPER CASE.
*
*  IF  WA_MOD_CELL-FIELDNAME = 'MATNR'.
*    IF SY-SUBRC = 0.
*      SELECT SINGLE * FROM MAKT
*        INTO CORRESPONDING FIELDS OF LS_MAKT
*        WHERE MATNR = WA_MOD_CELL-VALUE
*        AND SPRAS = SY-LANGU.
*
*      SELECT SINGLE * FROM MARA
*        INTO CORRESPONDING FIELDS OF LS_MARA
*        WHERE MATNR = WA_MOD_CELL-VALUE.
*
**带出物料描述
*      READ TABLE  GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
*      IF SY-SUBRC = 0.
*        GS_DATA-MAKTX = LS_MAKT-MAKTX.
*        MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
*        CLEAR GS_DATA.
*      ENDIF.
*
**带出物料主数据单位
*      READ TABLE GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
*      IF SY-SUBRC = 0.
*        GS_DATA-MEINS = LS_MARA-MEINS.
*        MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
*        CLEAR GS_DATA.
*      ENDIF.
*
*      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*        IMPORTING
*          E_GRID = G_REF_GRID.
*
*      CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
*    ENDIF.
*  ENDIF.
ENDFORM.                    "FRM_DATA_ENTER
*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_DATA_1
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_1 .

*读取凭证信息
  SELECT * FROM MKPF
  INNER JOIN MSEG ON
  MKPF~MBLNR = MSEG~MBLNR
  AND MKPF~MJAHR = MSEG~MJAHR
  INTO CORRESPONDING FIELDS OF TABLE GT_DATA_1
  WHERE KOSTL IN S_KOSTL
  AND   AUFNR IN S_AUFNR
  AND   MATNR IN S_MATNR
  AND   WERKS IN S_WERKS
  AND   MSEG~MBLNR IN S_MBLNR
  AND   BUDAT IN S_BUDAT
  AND   ( BWART  = 'Z01'
  OR      BWART  = 'Z02'
  OR      BWART  = '201'
  OR      BWART  = '202'
  OR      BWART  = '231'
  OR      BWART  = '232'
*&--代码注释 BY HANDYBY 16.08.2017 20:09:59  BEGIN
*    OR BWART = ).
*&--代码注释 BY HANDYBY 16.08.2017 20:09:59  END
*&--代码添加 BY HANDYBY 16.08.2017 20:09:53  BEGIN
    OR BWART = 'Z31'
    OR BWART = 'Z32'
    OR BWART = '282').
*&--代码添加 BY HANDYBY 16.08.2017 20:09:53  END


  CHECK GT_DATA_1 IS NOT INITIAL.

*查看物料描述
  IF GT_DATA_1 IS NOT INITIAL.
    SELECT * FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
      FOR ALL ENTRIES IN GT_DATA_1
      WHERE MATNR = GT_DATA_1-MATNR
        AND SPRAS = SY-LANGU.
  ENDIF.

*获取制单人姓名
  SELECT SINGLE * FROM USR21
   INNER JOIN ADRP
   ON USR21~PERSNUMBER = ADRP~PERSNUMBER
   INTO CORRESPONDING FIELDS OF GS_NAME
   WHERE BNAME = SY-UNAME.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DEAL_DATA_1
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA_1 .
  LOOP AT GT_DATA_1 INTO GS_DATA_1.

*制单人
    GS_DATA_1-NAME1    = GS_NAME-NAME_LAST.

*物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY   MATNR = GS_DATA_1-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA_1-MAKTX = GS_DATA_1-MAKTX.
    ENDIF.

    GS_DATA_1-MAKTX = GS_MAKT-MAKTX.
    MODIFY GT_DATA_1 FROM GS_DATA_1.
    CLEAR GS_DATA_1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_ALV_SHOW_1
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW_1 .
  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC_1.
  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA_1
                     USING 'ALV_PF_STATUS_1'
                           'ALV_USER_COMMAND_1'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
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
  DATA:L_FORMNAME TYPE TDSFNAME. "VALUE 'ZSFMM007'.
  DATA L_LINE TYPE I. "统计打印的行进行补行
  DATA G_LINE TYPE I. "设定换页行数

*------------------NEW ADD  BY HANDHJD 20170720-------------BEGIN-------------
*&--代码添加 BY HANDYBY 17.08.2017 14:23:08  BEGIN
  READ TABLE GT_DATA INTO GS_DATA INDEX 1 .
  IF SY-SUBRC = 0 .
    IF GS_DATA-WERKS EQ '1610' OR GS_DATA-WERKS EQ '1610'.
      L_FORMNAME = 'ZSFMM007_1'.   "新加的打印
*&--代码添加 BY HANDYBY 17.08.2017 12:26:29  BEGIN
    ELSEIF GS_DATA-WERKS EQ '1700' OR GS_DATA-WERKS EQ '1700'
        OR GS_DATA-WERKS EQ '1710' OR GS_DATA-WERKS EQ '1710'
        OR GS_DATA-WERKS EQ '1720' OR GS_DATA-WERKS EQ '1720'.
      L_FORMNAME = 'ZSFMM007_2'.
*&--代码添加 BY HANDYBY 17.08.2017 12:26:29  END
    ELSE.
      L_FORMNAME = 'ZSFMM007'.
    ENDIF.
  ENDIF.
**&--代码添加 BY HANDYBY 17.08.2017 14:23:08  END


*&--代码注释 BY HANDYBY 17.08.2017 14:23:31  BEGIN
*
*  IF S_WERKS-LOW EQ '1610' OR S_WERKS-HIGH EQ '1610'.
*    L_FORMNAME = 'ZSFMM007_1'.   "新加的打印
**&--代码添加 BY HANDYBY 17.08.2017 12:26:29  BEGIN
*  ELSEIF S_WERKS-LOW EQ '1700' OR S_WERKS-HIGH EQ '1700'
*      OR S_WERKS-LOW EQ '1710' OR S_WERKS-HIGH EQ '1710'
*      OR S_WERKS-LOW EQ '1720' OR S_WERKS-HIGH EQ '1720'.
*    L_FORMNAME = 'ZSFMM007_2'.
**&--代码添加 BY HANDYBY 17.08.2017 12:26:29  END
*  ELSE.
*    L_FORMNAME = 'ZSFMM007'.
*  ENDIF.
*
*&--代码注释 BY HANDYBY 17.08.2017 14:23:31  END
*------------------NEW ADD  BY HANDHJD 20170720-------------END-------------
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "SMARTFORMS的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的SMARTFORMS的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* START PRINTING

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
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  REFRESH:LT_DATA,
          LT_DATA_1.

*打印申请单
  IF G1 = 'X' OR G3 = 'X' OR G4 = 'X'
*&--代码添加 BY HANDYBY 16.08.2017 20:41:56  BEGIN
    OR G5 = 'X'.
*&--代码添加 BY HANDYBY 16.08.2017 20:41:56  END
*根据选中行
    LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.

      APPEND GS_DATA TO LT_DATA .
      CLEAR GS_DATA.

    ENDLOOP.

*每页补齐8行
    DESCRIBE TABLE LT_DATA LINES L_LINE.
    WHILE L_LINE MOD 8 <> 0.
      L_LINE = L_LINE + 1.
      APPEND INITIAL LINE TO LT_DATA.
    ENDWHILE.

*设定8行自动换页
    G_LINE = 8.

*&--代码添加 BY HANDYBY 17.08.2017 14:40:22  BEGIN
    EXPORT G5 = G5 TO MEMORY ID 'G5' .
*&--代码添加 BY HANDYBY 17.08.2017 14:40:22  END
    CALL FUNCTION G_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL
        G_LINE             = G_LINE
*       W_HEAD             = LW_PRT
*         TABLES
*       T_ITEM             = LT_PRT[]
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
  ELSEIF G2 = 'X'.

*打印凭证
    LOOP AT GT_DATA_1 INTO GS_DATA_1 WHERE ZBOX = 'X'.

      AT NEW MBLNR.
        REFRESH LT_DATA_1.
      ENDAT.

      APPEND GS_DATA_1 TO LT_DATA_1 .
      CLEAR GS_DATA_1.

      AT END OF MBLNR.
*每页补齐8行
        DESCRIBE TABLE LT_DATA_1 LINES L_LINE.
        WHILE L_LINE MOD 8 <> 0.
          L_LINE = L_LINE + 1.
          APPEND INITIAL LINE TO LT_DATA_1.
        ENDWHILE.

*设定8行自动换页
        G_LINE = 8.
        CALL FUNCTION G_NAME
          EXPORTING
            CONTROL_PARAMETERS = CONTROL
            G_LINE             = G_LINE
*           NPAGE              = NPAGELINE
*           W_HEAD             = LW_PRT
*         TABLES
*           T_ITEM             = LT_PRT[]
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

  ENDIF.
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS_1 USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      FORM  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND_1 USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.

*打印
    WHEN '&PRNT'.
*需进行保存操作
*      ET_FIELDCAT_LVC = GT_FIELDCAT
      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 1.
        MESSAGE S001(ZMM01)  DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE GT_DATA_1 INTO GS_DATA_1
        WITH KEY ZBOX = 'X'.
        IF SY-SUBRC = 0.
          PERFORM FRM_PRINT_DATA.
        ELSE.
          MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      FORM  FRM_INIT_LVC_1
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC_1 .
  INIT_FIELDCAT 'BUDAT'        TEXT-013       '' '' '' '' '' 'MKPF'   'BUDAT'.
  INIT_FIELDCAT 'WERKS'        TEXT-003       '' '' '' '' '' 'ZMM007' 'WERKS'.
  INIT_FIELDCAT 'MBLNR'        TEXT-014       '' '' '' '' '' 'MSEG'   'MBLNR'.
  INIT_FIELDCAT 'MATNR'        TEXT-004       '' '' '' '' '' 'ZMM007' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'        TEXT-005       '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'        TEXT-018       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LGORT'        TEXT-006       '' '' '' '' '' 'ZMM007' 'LGORT'.
  INIT_FIELDCAT 'MENGE'        TEXT-007       '' '' '' '' '' 'ZMM007' 'MENGE'.
  INIT_FIELDCAT 'MEINS'        TEXT-008       '' '' '' '' '' 'ZMM007' 'MEINS'.
  INIT_FIELDCAT 'CHARG'        TEXT-009       '' '' '' '' '' 'ZMM007' 'CHARG'.
  INIT_FIELDCAT 'KOSTL'        TEXT-010       '' '' '' '' '' 'ZMM007' 'KOSTL'.
  INIT_FIELDCAT 'AUFNR'        TEXT-011       '' '' '' '' '' 'ZMM007' 'AUFNR'.
  INIT_FIELDCAT 'KDAUF'        TEXT-019       '' '' '' '' '' 'ZMM007' 'VBELN'.
  INIT_FIELDCAT 'BKTXT'        TEXT-012       '' '' '' '' '' 'MKPF'   'BKTXT'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  CHECK_INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM CHECK_INPUT CHANGING L_SUBRC TYPE SY-SUBRC.

  DATA GS_COAS TYPE COAS.

  LOOP AT GT_DATA INTO GS_DATA
  WHERE ZBOX = 'X'.

    IF GS_DATA-BWART IS INITIAL.
      MESSAGE '请输入移动类型' TYPE 'S'  DISPLAY LIKE 'E'.
      L_SUBRC = 4.
      EXIT.
    ENDIF.

    IF GS_DATA-WERKS IS INITIAL.
      MESSAGE '请输入工厂' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
    ENDIF.

    IF GS_DATA-MENGE = 0.
      MESSAGE '请输入领料数量' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
    ENDIF.

    IF GS_DATA-BZ IS INITIAL.
      MESSAGE '请输入备注' TYPE 'S' DISPLAY LIKE 'E'.
      L_SUBRC = 4.
    ENDIF.

*如果输入的是201/202，则成本中心必输
    IF G1 = 'X'.
      IF GS_DATA-KOSTL IS INITIAL.
        MESSAGE '请输入成本中心' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        EXIT.
      ENDIF.

*如果输入的是Z01/Z02，则内部订单必输
    ELSEIF G3 = 'X'.
      IF GS_DATA-AUFNR IS INITIAL.
        MESSAGE '请输入内部订单' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        EXIT.

*判断内部订单是不是统计型内部订单，如果不是，则报错
        CLEAR GS_COAS.
        SELECT SINGLE * FROM COAS
          INTO CORRESPONDING FIELDS OF GS_COAS
          WHERE AUFNR = GS_DATA-AUFNR.

        IF GS_COAS-ASTKZ <> 'X'.
          MESSAGE '您正尝试将该笔材料费同时记到成本中心和内部订单上，请检输入是否正确' TYPE 'S' DISPLAY LIKE 'E'.
          L_SUBRC = 4.
          EXIT.
        ENDIF.
      ENDIF.

*如果输入的是231/232，则销售定的必输
    ELSEIF G4 = 'X'.
      IF GS_DATA-VBELN IS INITIAL.
        MESSAGE '请输入销售订单' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        EXIT.
      ENDIF.

*&--代码添加 BY HANDYBY 16.08.2017 18:27:34  BEGIN
    ELSEIF G5 = 'X'.
      IF GS_DATA-POSID IS INITIAL.
        MESSAGE '请输入项目WBS' TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        EXIT.
      ENDIF.
*&--代码添加 BY HANDYBY 16.08.2017 18:27:34  END
    ENDIF.

  ENDLOOP.
ENDFORM.
