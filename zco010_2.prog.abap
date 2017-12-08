REPORT ZCO010_2.
"

*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 2016-03-28
*& Request       :
*& Descriptions  : 销售类成本中心收入明细
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
************************************************************************
* TABLES
************************************************************************
TABLES:BKPF,BSEG,VBAK,T001,CSKT,ZCO010_6,TVKBT.

TYPES:BEGIN OF TY_DATA,
      BUKRS TYPE BUKRS,     "公司代码
      GJAHR TYPE GJAHR,     "会计年度
      VBELN TYPE VBELN,     "销售订单
      XMMC  TYPE STRING,     "项目名称
      VKBUR TYPE VKBUR,     "销售副总
      XSFZ  TYPE STRING,    "销售副总描述
      VKORG TYPE VKORG,     "销售组织
      KOSTL TYPE KOSTL,     "成本中心
      KOSTL_TXT TYPE STRING, "成本中心描述
      BELNR TYPE BELNR_D,      "会计凭证
      BUZEI TYPE BUZEI,      "行项目
      SHKZG TYPE SHKZG,      "借贷标识
      BUDAT TYPE BUDAT,      "过账日期
      BLART TYPE BLART,      "凭证类型
      HKONT TYPE HKONT,       "科目
      HKONT_TXT TYPE STRING,  "科目描述
      WRBTR TYPE BSEG-WRBTR,  "凭证货币金额
      WAERS TYPE WAERS,       "凭证货币码
      WAERS_1 TYPE WAERS,     "本币码
      DMBTR TYPE BSEG-DMBTR,  "本币金额
      VBEL2 TYPE BSEG-VBEL2,  "销售订单号
      XBLNR TYPE BKPF-XBLNR,  "参照
      SEL(1),
    END OF TY_DATA.

 DATA:GS_DATA TYPE TY_DATA,
      GT_DATA TYPE TABLE OF TY_DATA.

 DATA:GS_CSKT TYPE CSKT,
      GT_CSKT TYPE TABLE OF CSKT.

 DATA:GS_VBAK_XM TYPE VBAK,
      GT_VBAK_XM TYPE TABLE OF VBAK.

  DATA:GS_VBAK_CP TYPE VBAK,
       GT_VBAK_CP TYPE TABLE OF VBAK.

  DATA:GS_SKAT TYPE SKAT,
      GT_SKAT TYPE TABLE OF SKAT.

  DATA:GT_ZCO010_6 TYPE TABLE OF ZCO010_6,
       GS_ZCO010_6 TYPE ZCO010_6.

 FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA.

 DATA: IT_LINES TYPE TABLE OF TLINE,
       WA_LINES TYPE TLINE.

 DATA:GT_T001 TYPE TABLE OF T001,
      GS_T001 TYPE T001.

 DATA:GT_TVKBT TYPE TABLE OF TVKBT,
      GS_TVKBT TYPE TVKBT.


 DATA: G_OBJNAME TYPE THEAD-TDNAME.

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
*   IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
   IF &1 = 'WRBTR' .
   GW_LVC-CFIELDNAME = 'WAERS'.
  ENDIF.
  IF &1 = 'DMBTR' .
   GW_LVC-CFIELDNAME = 'WAERS_1'.
  ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
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
SELECT-OPTIONS: S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,   "过账日期
                S_KOSTL  FOR BSEG-KOSTL,   "成本中心
                S_VBELN  FOR BSEG-VBEL2.   "销售订单
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
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0347   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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
  "读取销售类财务凭证明细
 SELECT BKPF~BUKRS BKPF~GJAHR BKPF~BUDAT BKPF~BLART BKPF~WAERS BKPF~XBLNR
        BSEG~BELNR BSEG~BUZEI BSEG~HKONT BSEG~WRBTR BSEG~DMBTR BSEG~SHKZG
        BSEG~VBEL2
  INTO CORRESPONDING FIELDS OF TABLE GT_DATA
   FROM BKPF
   INNER JOIN BSEG
   ON BKPF~GJAHR  = BSEG~GJAHR
   AND BKPF~BUKRS = BSEG~BUKRS
   AND BKPF~BELNR = BSEG~BELNR
   WHERE BKPF~BUKRS EQ P_BUKRS
   AND   BKPF~BUDAT IN S_BUDAT
   AND   BKPF~BLART EQ 'RV'
   AND   BSEG~HKONT BETWEEN '6001000000' AND '6001999999'.
 SORT GT_DATA BY BUKRS GJAHR BELNR.

 CHECK GT_DATA IS NOT INITIAL.

 "读取项目订单 信息
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK_XM
   FROM VBAK
   WHERE VBELN IN S_VBELN
   AND AUART IN ('ZF1','ZPO','ZSO','ZWV','ZZG').

SORT GT_VBAK_XM BY VBELN.

 "读取产品订单信息
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK_CP
   FROM VBAK
   WHERE VBELN IN S_VBELN
   AND AUART IN ('ZF2','ZOR','ZRE','ZSD').
SORT GT_VBAK_CP BY VBELN.

"读取成本中心描述 信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CSKT
  FROM CSKT
  WHERE SPRAS = '1'
  AND KOKRS = '1000'
  .
SORT GT_CSKT BY KOSTL.

"读取科目描述信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
     FROM SKAT
     WHERE KTOPL = '1000'
         AND SAKNR BETWEEN '6001000000' AND '6001999999'.
  SORT GT_SKAT BY SAKNR.

"读取销售副总对应的成本中心维护信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZCO010_6
  FROM ZCO010_6
  WHERE BUKRS EQ P_BUKRS
    AND KOSTL IN S_KOSTL.
SORT GT_ZCO010_6 BY BUKRS ASCENDING VKORG ASCENDING VKBUR ASCENDING YXQC DESCENDING.



"读取公司代码主数据信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
  FROM T001
  WHERE BUKRS = P_BUKRS.
SORT GT_T001 BY BUKRS.

"读取销售副总信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_TVKBT
  FROM TVKBT
  WHERE SPRAS = '1'
 .

SORT GT_TVKBT BY VKBUR.


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
 DATA:L_TABIX TYPE SY-TABIX.
 DATA:L_KS  TYPE C .
 LOOP AT GT_DATA ASSIGNING <FS_DATA> .
   L_TABIX = SY-TABIX.

    "先判断是不是项目订单
    READ TABLE GT_VBAK_XM INTO GS_VBAK_XM WITH KEY VBELN = <FS_DATA>-VBEL2 BINARY SEARCH.
     IF SY-SUBRC EQ 0 .
        <FS_DATA>-VBELN = GS_VBAK_XM-VBELN . "销售订单
        <FS_DATA>-VKBUR = GS_VBAK_XM-VKBUR.  "主管副总
        <FS_DATA>-VKORG = GS_VBAK_XM-VKORG.  "销售副总
        ELSE.
         "再判断是不是产品订单
          READ TABLE GT_VBAK_CP INTO GS_VBAK_CP
          WITH KEY  VBELN = <FS_DATA>-XBLNR BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
             <FS_DATA>-VBELN = GS_VBAK_CP-VBELN .  "销售订单
             <FS_DATA>-VKBUR = GS_VBAK_CP-VKBUR.   "主管副总
             <FS_DATA>-VKORG = GS_VBAK_CP-VKORG.   "销售副总
             ELSE.
               DELETE GT_DATA INDEX L_TABIX.
               CONTINUE.
           ENDIF.
     ENDIF.
         "成本中心

*    DELETE  GT_ZCO010_6 WHERE  YXQC > <FS_DATA>-BUDAT .

    LOOP AT GT_ZCO010_6 INTO GS_ZCO010_6
           WHERE VKORG = <FS_DATA>-VKORG
            AND VKBUR = <FS_DATA>-VKBUR
            AND YXQC <= <FS_DATA>-BUDAT .
     <FS_DATA>-KOSTL = GS_ZCO010_6-KOSTL.
     " L_KS  = 1.
      EXIT.
    ENDLOOP.

    IF S_KOSTL IS NOT INITIAL AND <FS_DATA>-KOSTL  EQ ''.
      DELETE GT_DATA INDEX L_TABIX.
       CONTINUE.

    ENDIF.

*    READ TABLE GT_ZCO010_6 INTO GS_ZCO010_6
*          WITH KEY VKORG = <FS_DATA>-VKORG
*           VKBUR = <FS_DATA>-VKBUR
*           BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      <FS_DATA>-KOSTL = GS_ZCO010_6-KOSTL.
*      ELSE.
*        IF S_KOSTL IS  NOT INITIAL.
*           DELETE GT_DATA INDEX L_TABIX.
*           CONTINUE.
*         ENDIF.
*    ENDIF.

     IF <FS_DATA>-SHKZG EQ 'H'.
        <FS_DATA>-WRBTR = <FS_DATA>-WRBTR * -1.
        <FS_DATA>-DMBTR = <FS_DATA>-DMBTR * -1.

     ENDIF.

     "项目名称
      PERFORM SELXMMC USING <FS_DATA>-VBELN CHANGING <FS_DATA>-XMMC.
       "科目名称
    READ TABLE GT_SKAT INTO GS_SKAT
       WITH KEY SAKNR = <FS_DATA>-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-HKONT_TXT = GS_SKAT-TXT50.
    ENDIF.



    "成本中心描述
    READ TABLE GT_CSKT INTO GS_CSKT
     WITH KEY KOSTL = <FS_DATA>-KOSTL BINARY SEARCH.
     IF SY-SUBRC = 0.
       <FS_DATA>-KOSTL_TXT = GS_CSKT-LTEXT.

     ENDIF.

    "本币码
     READ TABLE GT_T001 INTO GS_T001
      WITH KEY BUKRS = <FS_DATA>-BUKRS BINARY SEARCH .
     IF SY-SUBRC EQ 0 .
       <FS_DATA>-WAERS_1 = GS_T001-WAERS.
     ENDIF.

    "主管副总描述
     READ TABLE GT_TVKBT INTO GS_TVKBT
      WITH KEY  VKBUR = <FS_DATA>-VKBUR BINARY SEARCH.
      IF SY-SUBRC EQ 0 .
        <FS_DATA>-XSFZ = GS_TVKBT-BEZEI.
      ENDIF.

       "科目名称
    READ TABLE GT_SKAT INTO GS_SKAT
       WITH KEY SAKNR = <FS_DATA>-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-HKONT_TXT = GS_SKAT-TXT50.
    ENDIF.

ENDLOOP.
SORT GT_DATA BY BUKRS GJAHR VBELN .

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
  GW_LAYOUT-BOX_FNAME = 'SEL'.
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
  INIT_FIELDCAT 'BUKRS'          '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBELN'          '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'           '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VKBUR'          '销售副总'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XSFZ'           '销售副总描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'          '成本中心'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL_TXT'      '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '会计凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'          '行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLART'          '凭证类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'          '科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT'      '科目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'          '凭证货币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'          '本币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'          '销售凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XBLNR'          '参考'         '' '' '' '' '' '' ''.
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
*      -->P_GT_DATA  text
*      -->P_0403   text
*      -->P_0404   text
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

*&---------------------------------------------------------------------*
*&      FORM  SELXMMC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GT_DATA_VBELN  TEXT
*      <--P_GT_DATA_XMMC  TEXT
*----------------------------------------------------------------------*
FORM SELXMMC  USING    P_VBELN TYPE VBELN
              CHANGING P_XMMC TYPE STRING.


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
        AND GS_DATA-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'VBELN'
        AND GS_DATA-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
