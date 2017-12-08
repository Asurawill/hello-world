REPORT ZCO006_10.

TABLES:BKPF,BSEG .

DATA:BEGIN OF GS_ITEM,
     BUKRS TYPE BKPF-BUKRS,  "公司代码
     GJAHR TYPE BKPF-GJAHR, "会计年度
     BELNR  TYPE BKPF-BELNR,"凭证编码
     BLART  TYPE BKPF-BLART,"凭证类型
     BUDAT  TYPE BKPF-BUDAT, "过账日期
     BUZEI  TYPE BSEG-BUZEI, "凭证行号
     SHKZG  TYPE BSEG-SHKZG, "借贷标识
     DMBTR  TYPE BSEG-DMBTR,"本币金额
     XNEGP  TYPE XNEGP,"反记账
     HKONT  TYPE HKONT,"科目
     KUNNR  TYPE KUNNR, "客户
     ZUONR  TYPE BSEG-ZUONR,
    END OF GS_ITEM .

DATA:BEGIN OF GS_TOTAL,
      BUKRS TYPE BKPF-BUKRS ,"公司代码
      WAERS TYPE WAERS ,       "货币码
      HKONT  TYPE HKONT,"科目
      HKONT_TXT TYPE STRING ,"描述
      KUNNR  TYPE KUNNR, "客户
      KUNNR_NAME1 TYPE STRING,"客户名称
      VBELN  TYPE VBELN,  "销售订单
      XMMC   TYPE STRING, "项目名称
      QCYE  TYPE TSLVT12  ,  "期初余额
      BQJF  TYPE TSLVT12 ,   "本期借方
      BQDF  TYPE TSLVT12 ,   "本期贷方
      QMYE  TYPE TSLVT12 ,    "期末余额

      SEL(1),
   END OF GS_TOTAL .

DATA: GT_ITEM  LIKE TABLE OF GS_ITEM WITH HEADER LINE .

DATA:GT_ITEM_02 LIKE TABLE OF GS_ITEM WITH HEADER LINE.

DATA: GT_TOTAL  LIKE TABLE OF GS_TOTAL WITH HEADER LINE.

DATA: GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.

DATA:  GT_KNA1 LIKE TABLE OF KNA1 WITH HEADER LINE.

DATA:  GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.

DATA:GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.

*DATA:GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.

 FIELD-SYMBOLS: <FS_DATA> LIKE GS_ITEM.
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
   IF &1 = 'VBELN'.
    GW_LVC-HOTSPOT = 'X'.
   ENDIF.
   IF &1 = 'QCYE'  OR &1 = 'BQJF'  OR &1 = 'BQDF'  OR &1 = 'QMYE'  .
   GW_LVC-CFIELDNAME = 'WAERS'.
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
RANGES:R_HKONT FOR BSEG-HKONT.
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
R_HKONT-SIGN = 'I'.
R_HKONT-OPTION = 'BT'.
R_HKONT-LOW = '1122000000'.
R_HKONT-HIGH = '1122999999'.
APPEND R_HKONT.
R_HKONT-SIGN = 'I'.
R_HKONT-OPTION = 'BT'.
R_HKONT-LOW = '2203000000'.
R_HKONT-HIGH = '2203999999'.
APPEND R_HKONT.
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
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  DATA:P_DATE TYPE D .
  P_DATE = S_BUDAT-LOW.
  IF S_BUDAT-HIGH IS NOT INITIAL.
    P_DATE = S_BUDAT-HIGH.
  ENDIF.
 "
  SELECT BKPF~BUKRS BKPF~GJAHR BKPF~BUDAT  BKPF~BLART BKPF~BELNR
    BSEG~BUZEI BSEG~SHKZG BSEG~DMBTR BSEG~XNEGP BSEG~HKONT BSEG~KUNNR BSEG~ZUONR
    INTO CORRESPONDING FIELDS OF TABLE  GT_ITEM
    FROM BKPF
    INNER JOIN BSEG
    ON BKPF~BUKRS  = BSEG~BUKRS AND BKPF~GJAHR = BSEG~GJAHR AND BKPF~BELNR = BSEG~BELNR
    WHERE BKPF~BUKRS = P_BUKRS  AND BKPF~BUDAT <= P_DATE
          AND BSEG~HKONT IN  R_HKONT ..

SORT GT_ITEM BY BUKRS BLART .

*:GT_ITEM_02[] = GT_ITEM[].
*
*DELETE GT_ITEM_02 WHERE BLART NE 'AB'.
*
*SORT GT_ITEM_02 BY BUKRS GJAHR BUDAT BELNR BUZEI  .

SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
  FROM T001
  WHERE BUKRS = P_BUKRS .
SORT GT_T001 BY BUKRS .

SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
  FROM VBAK.

SORT GT_VBAK BY VBELN.

 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT FROM SKAT WHERE KTOPL = '1000' AND SAKNR  IN  R_HKONT ...
  SORT GT_SKAT BY SAKNR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_KNA1 FROM KNA1.
  SORT GT_KNA1 BY KUNNR .
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
  DATA: VBELN_TMP TYPE STRING.
 LOOP AT GT_ITEM INTO GS_ITEM.
   CLEAR:GS_TOTAL.


 "公司代码
   GS_TOTAL-BUKRS = GS_ITEM-BUKRS.
   "货币码
   READ TABLE GT_T001 WITH KEY BUKRS = GS_ITEM-BUKRS BINARY SEARCH .
   IF SY-SUBRC = 0.
     GS_TOTAL-WAERS = GT_T001-WAERS.
    ENDIF.
  "科目
   GS_TOTAL-HKONT = GS_ITEM-HKONT.
  "客户
   GS_TOTAL-KUNNR = GS_ITEM-KUNNR.
 "销售订单
  CONDENSE GS_ITEM-ZUONR NO-GAPS.
 "IF GS_ITEM-BLART NE 'AB' .
*   CLEAR :GS_TOTAL-VBELN .
*   ELSE.
     READ TABLE GT_VBAK WITH KEY VBELN = GS_ITEM-ZUONR BINARY SEARCH .
      IF SY-SUBRC = 0.
         GS_TOTAL-VBELN = GS_ITEM-ZUONR .
        ENDIF.
 "ENDIF.
*  READ TABLE GT_VBAK WITH KEY VBELN = GS_ITEM-ZUONR BINARY SEARCH .
*   IF SY-SUBRC = 0.
*      GS_TOTAL-VBELN = GS_ITEM-ZUONR .
**    ELSE.
**       IF GS_ITEM-BLART = 'AB' .
**            LOOP AT GT_ITEM_02 WHERE BUKRS = GS_ITEM-BUKRS AND GJAHR = GS_ITEM-GJAHR AND BELNR = GS_ITEM-BELNR .
**                  READ TABLE GT_VBAK WITH KEY VBELN = GT_ITEM_02-ZUONR BINARY SEARCH .
**                   IF SY-SUBRC = 0.
**                       GS_TOTAL-VBELN = GT_ITEM_02-ZUONR .
**                       EXIT.
**                   ENDIF.
**
**            ENDLOOP.
**
**        ENDIF.
*    ENDIF.

 "期初余额
 IF GS_ITEM-BUDAT < S_BUDAT-LOW .
    IF GS_ITEM-SHKZG = 'H'.
      GS_TOTAL-QCYE = GS_ITEM-DMBTR * -1.
     ELSE.
       GS_TOTAL-QCYE = GS_ITEM-DMBTR.
    ENDIF.

  ENDIF.
 IF GS_ITEM-BUDAT >= S_BUDAT-LOW AND GS_ITEM-BUDAT <= S_BUDAT-HIGH.
    "借方
    IF ( GS_ITEM-XNEGP = '' AND GS_ITEM-SHKZG = 'S' ) OR ( GS_ITEM-XNEGP = 'X' AND GS_ITEM-SHKZG = 'H'  ) ..
      IF  GS_ITEM-XNEGP = 'X' .
        GS_TOTAL-BQJF = GS_ITEM-DMBTR * -1 .
       ELSE.
         GS_TOTAL-BQJF = GS_ITEM-DMBTR.
      ENDIF.

    ENDIF.
   "贷方
    IF ( GS_ITEM-XNEGP = '' AND GS_ITEM-SHKZG = 'H' ) OR ( GS_ITEM-XNEGP = 'X' AND GS_ITEM-SHKZG = 'S'  ) ..
      IF  GS_ITEM-XNEGP = 'X' .
         GS_TOTAL-BQDF = GS_ITEM-DMBTR * -1.
       ELSE.
          GS_TOTAL-BQDF = GS_ITEM-DMBTR  .
      ENDIF.

    ENDIF.

 ENDIF.

  "期末余额
 GS_TOTAL-QMYE = GS_TOTAL-QCYE + GS_TOTAL-BQJF -  GS_TOTAL-BQDF ..
 COLLECT GS_TOTAL INTO GT_TOTAL.
 ENDLOOP.
 SORT GT_TOTAL BY BUKRS HKONT KUNNR VBELN.
 DELETE GT_TOTAL WHERE  QCYE = 0 AND BQJF = 0 AND  BQDF = 0.
 LOOP AT GT_TOTAL INTO GS_TOTAL .
   "项目名称读取
   "科目名称
    READ TABLE GT_SKAT WITH KEY SAKNR = GS_TOTAL-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TOTAL-HKONT_TXT = GT_SKAT-TXT50.
    ENDIF.
   "项目名称
    PERFORM SELXMMC USING GS_TOTAL-VBELN CHANGING GS_TOTAL-XMMC.
    "客户名称
    PERFORM SELKHMC USING GS_TOTAL-KUNNR CHANGING GS_TOTAL-KUNNR_NAME1.
   MODIFY GT_TOTAL FROM GS_TOTAL.
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
                            GT_TOTAL
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

  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'          '科目'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT'      '科目描述'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'          '客户'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR_NAME1'     '客户名称'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBELN'          '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'           '项目名称'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QCYE'           '期初余额'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BQJF'           '本期借方'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BQDF'           '本期贷方'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QMYE'           '期末余额'      '' '' '' '' '' '' ''.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0417   text
*      -->P_0418   text
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
      T_OUTTAB                 = GT_TOTAL
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
*FORM FRM_EXCLUDE .
* REFRESH GT_EXCLUDE.
*  CLEAR GS_EXCLUDE.
*ENDFORM.
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
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'VBELN'
        AND GT_TOTAL-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GT_TOTAL-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELKHMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_KUNNR  text
*      <--P_<FS_DATA>_KUNNR_NAME1  text
*----------------------------------------------------------------------*
FORM SELKHMC  USING   P_KUNNR TYPE KUNNR
              CHANGING P_NAME1  TYPE STRING.
READ TABLE GT_KNA1 WITH KEY KUNNR = P_KUNNR.
IF SY-SUBRC = 0.
  P_NAME1 = GT_KNA1-NAME1.
 ENDIF.
ENDFORM.
