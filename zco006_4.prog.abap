REPORT ZCO006_4.

************************************************************************
* TABLES
************************************************************************
TABLES:BKPF,BSEG.

DATA:BEGIN OF GS_DATA,
     YWLX TYPE STRING, "业务类型
     BUKRS TYPE BKPF-BUKRS,  "公司代码
     GJAHR TYPE BKPF-GJAHR, "会计年度
     BELNR  TYPE BKPF-BELNR,"凭证编码
     BLART  TYPE BKPF-BLART,"凭证类型
     BUDAT  TYPE BKPF-BUDAT, "过账日期
     BUZEI  TYPE BSEG-BUZEI, "凭证行号
     SHKZG  TYPE BSEG-SHKZG, "借贷标识
     VBELN  TYPE VBELN,  "销售订单
     XMMC   TYPE STRING, "项目名称
     KUNNR  TYPE KUNNR, "客户
     KUNNR_NAME1 TYPE STRING,"客户名称
     DMBTR  TYPE BSEG-DMBTR,"本币金额
     WAERS  TYPE T001-WAERS,"货币码
     WRBTR  TYPE WRBTR,"凭证货币金额
     WAERS_1 TYPE WAERS, "凭证货币
     XNEGP  TYPE XNEGP,"反记账
     HKONT  TYPE HKONT,"科目
     HKONT_TXT TYPE STRING, "科目描述
     BKTXT  TYPE BKPF-BKTXT, "摘要
     XBLNR  TYPE BKPF-XBLNR, "参考
     ZUONR  TYPE BSEG-ZUONR,"分配
     EBELN  TYPE BSEG-EBELN,"采购订单
     MATNR  TYPE BSEG-MATNR,"物料号
     MAKTX   TYPE MAKTX,"物料描述
     AWTYP  TYPE BKPF-AWTYP,"参考交易
     VBEL2  TYPE BSEG-VBEL2,"销售凭证
     VBUND  TYPE BSEG-VBUND,"贸易伙伴
     SEL(1),
     END OF GS_DATA.
 DATA:  GT_DATA LIKE TABLE OF GS_DATA WITH HEADER LINE.
 DATA:  GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.
 DATA:  GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.
 DATA:  GT_KNA1 LIKE TABLE OF KNA1 WITH HEADER LINE.
 DATA:  GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.
 DATA:  GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.
 FIELD-SYMBOLS: <FS_DATA> LIKE GS_DATA.
 DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

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
  IF &1 = 'KUNNR'.
    GW_LVC-NO_ZERO = 'X'.
  ENDIF.
   IF &1 = 'WRBTR' .
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
SELECT-OPTIONS: S_BUDAT  FOR BKPF-BUDAT OBLIGATORY.    "过账日期
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
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_0558   TEXT
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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
SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1 A~BKTXT A~XBLNR A~AWTYP
     B~BUZEI B~SHKZG B~DMBTR B~WRBTR B~XNEGP B~HKONT B~ZUONR B~EBELN B~MATNR B~VBEL2
    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM BKPF AS A
    INNER JOIN BSEG AS B
  ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
  WHERE A~BUKRS = P_BUKRS AND BUDAT IN S_BUDAT AND B~HKONT EQ '1406010101'.
IF GT_DATA[] IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001  FROM T001 WHERE BUKRS = P_BUKRS.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT FROM SKAT WHERE KTOPL = '1000' AND SAKNR  EQ '1406010101' .
  SORT GT_SKAT BY SAKNR.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK FROM VBAK  .
  SORT  GT_VBAK BY VBELN.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_KNA1 FROM KNA1.
  SORT GT_KNA1 BY KUNNR .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT  FROM MAKT WHERE SPRAS = '1'..
  SORT  GT_MAKT  BY MATNR.
ENDIF.

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
  DATA: VBELN_TMP TYPE STRING.
  LOOP AT GT_DATA ASSIGNING <FS_DATA> .
    IF <FS_DATA>-SHKZG EQ 'H'.
      <FS_DATA>-DMBTR =  <FS_DATA>-DMBTR * -1 .
      <FS_DATA>-WRBTR = <FS_DATA>-WRBTR * -1.
    ENDIF.
    "物料描述
     PERFORM SELWLMS USING <FS_DATA>-MATNR CHANGING <FS_DATA>-MAKTX .
    "科目名称
    READ TABLE GT_SKAT WITH KEY SAKNR = <FS_DATA>-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-HKONT_TXT = GT_SKAT-TXT50.
    ENDIF.
     "读取本位币
    READ TABLE GT_T001 WITH KEY BUKRS = <FS_DATA>-BUKRS.
    IF SY-SUBRC = 0.
      <FS_DATA>-WAERS = GT_T001-WAERS.
     ENDIF.
    IF <FS_DATA>-BLART = 'SC'.
      <FS_DATA>-YWLX = '项目'.
      "读取销售订单
       SPLIT <FS_DATA>-ZUONR  AT '/' INTO <FS_DATA>-VBELN VBELN_TMP.
      "项目名称
      PERFORM SELXMMC USING <FS_DATA>-VBELN CHANGING <FS_DATA>-XMMC.
       READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-VBELN BINARY SEARCH.
       IF SY-SUBRC = 0.
         <FS_DATA>-KUNNR = GT_VBAK-KUNNR.
          "客户名称
       PERFORM SELKHMC USING <FS_DATA>-KUNNR CHANGING <FS_DATA>-KUNNR_NAME1.
       ENDIF.
       CONTINUE.
      ENDIF.
    IF <FS_DATA>-BLART = 'RV'.

       READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-XBLNR BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
            <FS_DATA>-YWLX = '产品'.
             "销售订单
             <FS_DATA>-VBELN = <FS_DATA>-XBLNR.
             "客户
             <FS_DATA>-KUNNR = GT_VBAK-KUNNR.
             "项目名称
            PERFORM SELXMMC USING <FS_DATA>-VBELN CHANGING <FS_DATA>-XMMC.
         "    客户名称
          PERFORM SELKHMC USING <FS_DATA>-KUNNR CHANGING <FS_DATA>-KUNNR_NAME1.
          CONTINUE.
         ENDIF.
        ENDIF.

    ENDIF.
      IF <FS_DATA>-BLART = 'WL'.

       READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-VBEL2 BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
            <FS_DATA>-YWLX = '产品'.
             "销售订单
             <FS_DATA>-VBELN = <FS_DATA>-VBEL2.
             "客户
             <FS_DATA>-KUNNR = GT_VBAK-KUNNR.
             "项目名称
            PERFORM SELXMMC USING <FS_DATA>-VBELN CHANGING <FS_DATA>-XMMC.
         "    客户名称
          PERFORM SELKHMC USING <FS_DATA>-KUNNR CHANGING <FS_DATA>-KUNNR_NAME1.
          CONTINUE.
            ENDIF.
        ENDIF.

    ENDIF.
     IF <FS_DATA>-BLART = 'ML'.
       <FS_DATA>-YWLX = '其他-材料差异'.
       "客户
        <FS_DATA>-KUNNR = <FS_DATA>-VBUND.
       "    客户名称
          PERFORM SELKHMC USING <FS_DATA>-KUNNR CHANGING <FS_DATA>-KUNNR_NAME1.
          CONTINUE.
     ENDIF.
    IF <FS_DATA>-YWLX NE  '其他-材料差异' AND <FS_DATA>-YWLX NE '项目' AND <FS_DATA>-YWLX NE '产品' ."
      <FS_DATA>-YWLX = '其他'.
     "客户
        <FS_DATA>-KUNNR = <FS_DATA>-VBUND.
       " 客户名称
          PERFORM SELKHMC USING <FS_DATA>-KUNNR CHANGING <FS_DATA>-KUNNR_NAME1.
          CONTINUE.
    ENDIF.
  ENDLOOP.
 SORT GT_DATA BY BUKRS GJAHR YWLX BELNR BUZEI.
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
*&      FORM  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA        = 'X'.
  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  GW_LAYOUT-BOX_FNAME = 'SEL'.

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
  INIT_FIELDCAT 'YWLX'          '业务类型'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '凭证编码'               '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'BLART'          '凭证类型'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'          '凭证行号'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SHKZG'          '借贷标识'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBELN'          '销售订单'             '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'XMMC'          '项目名称'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'          '客户'         '' '' '' '' '' 'KNA1' 'KUNNR'.
  INIT_FIELDCAT 'KUNNR_NAME1'    '客户名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'          '本币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '货币码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'          '凭证货币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'          '凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XNEGP'          '反记账'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'          '科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT'          '科目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKTXT'          '摘要'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XBLNR'          '参考'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZUONR'          '分配'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料号'         '' '' '' '' '' 'BSEG' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'          '物料描述'         '' '' '' '' '' 'MAKT' 'MAKTX'.
  INIT_FIELDCAT 'AWTYP'          '参考交易'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'          '销售凭证'         '' '' '' '' '' '' ''.
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
*      -->P_GT_DATA  TEXT
*      -->P_0402   TEXT
*      -->P_0403   TEXT
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
*&---------------------------------------------------------------------*
*&      FORM  SELKHMC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GT_DATA_KUNNR  TEXT
*      <--P_GT_DATA_KUNNR_NAME1  TEXT
*----------------------------------------------------------------------*
FORM SELKHMC  USING   P_KUNNR TYPE KUNNR
              CHANGING P_NAME1  TYPE STRING.
READ TABLE GT_KNA1 WITH KEY KUNNR = P_KUNNR.
IF SY-SUBRC = 0.
  P_NAME1 = GT_KNA1-NAME1.
 ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  SELWLMS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GT_DATA_MATNR  TEXT
*      <--P_GT_DATA_MAKTX  TEXT
*----------------------------------------------------------------------*
FORM SELWLMS  USING    P_MATNR
              CHANGING P_MAKTX.
READ TABLE GT_MAKT WITH KEY MATNR =  P_MATNR .
IF SY-SUBRC = 0.
  P_MAKTX = GT_MAKT-MAKTX.

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
