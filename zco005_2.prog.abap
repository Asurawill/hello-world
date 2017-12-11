REPORT ZCO005_2.
* DES:项目订单成本发生明细
* DATE 20160309  IT02

TABLES:BKPF,BSEG,VBAK,MAKT,SKAT.

DATA:BEGIN OF GS_DATA ,
     BUKRS TYPE BKPF-BUKRS,  "公司代码
     GJAHR TYPE BKPF-GJAHR, "会计年度
     MONAT TYPE BKPF-MONAT,  "期间
     VBEL2 TYPE BSEG-VBEL2,   "销售订单
     POSN2 TYPE BSEG-POSN2,   "销售订单行项目
     AUART TYPE VBAK-AUART,   "订单类型
     YSYEAR TYPE BSEG-GJAHR,   "验收年度
     BELNR TYPE BSEG-BELNR,   "会计凭证
     BUZEI TYPE BSEG-BUZEI,   "行项目
     SHKZG TYPE BSEG-SHKZG,    "借贷标识
     BUDAT TYPE BKPF-BUDAT,   "过账日期
     HKONT TYPE BSEG-HKONT,    "科目
     HKONT_TXT TYPE STRING, "科目描述
     WRBTR TYPE BSEG-WRBTR,  "凭证货币金额
     WAERS TYPE BKPF-WAERS,  "凭证货币
     DMBTR TYPE BSEG-DMBTR,  "本币金额
     WAERS_1 TYPE BKPF-WAERS, "本币
     BKTXT TYPE BKPF-BKTXT,  "摘要
    " SHKZG TYPE BSEG-SHKZG,  "借贷标识
     MATNR TYPE BSEG-MATNR,  "物料号
     MAKTX TYPE MAKT-MAKTX,  "物料描述
     XMMC  TYPE STRING ,       "项目名称
     SEL(1) TYPE C ,
    END OF GS_DATA.

DATA:GT_DATA LIKE TABLE  OF GS_DATA WITH HEADER LINE.
 FIELD-SYMBOLS: <FS_DATA> LIKE GS_DATA.
DATA:GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.
DATA:GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.
DATA:GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.
DATA:GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.
DATA:GT_VBRK LIKE TABLE OF VBRK WITH HEADER LINE.
DATA:GT_VBFA LIKE TABLE OF VBFA WITH HEADER LINE.
DATA:GT_VBAP LIKE TABLE OF VBAP WITH HEADER LINE.
DATA:L_TABIX TYPE I.

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
  GW_LVC-KEY = &4.
*  IF &1 = 'KUNNR'.
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
  GW_LVC-REF_TABLE = &8.
  GW_LVC-REF_FIELD = &9.
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
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY,                      "公司代码
P_GJAHR  TYPE BKPF-GJAHR OBLIGATORY,                      "年度
P_MONAT  TYPE BKPF-MONAT OBLIGATORY.                      "期间
SELECT-OPTIONS: S_VBEL2 FOR BSEG-VBEL2.    "销售订单
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

" *&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  "根据查询条件取出 过账明细数据
 SELECT BKPF~BUKRS BKPF~GJAHR BKPF~MONAT BKPF~BUDAT BKPF~WAERS  BKPF~BKTXT
        BSEG~BELNR BSEG~BUZEI BSEG~HKONT BSEG~WRBTR BSEG~SHKZG
        BSEG~DMBTR BSEG~MATNR BSEG~VBEL2 BSEG~POSN2
        INTO CORRESPONDING FIELDS OF TABLE GT_DATA
        FROM BKPF
        INNER JOIN BSEG
        ON BKPF~BUKRS = BSEG~BUKRS AND BKPF~GJAHR = BSEG~GJAHR
         AND BKPF~BELNR = BSEG~BELNR
        WHERE  BKPF~GJAHR EQ P_GJAHR
         AND  BKPF~BUKRS EQ P_BUKRS
         AND  BKPF~MONAT EQ P_MONAT
         AND  BSEG~VBEL2 IN S_VBEL2
         AND  BSEG~VBEL2 NE ''
         AND  ( ( BSEG~HKONT BETWEEN '8000000000' AND '8999999999' )
                OR
              ( BSEG~HKONT BETWEEN '6400000000' AND '6499999999' ) ).

  SORT GT_DATA BY BUKRS GJAHR MONAT BELNR BUZEI.

  CHECK GT_DATA[] IS NOT INITIAL.

  "取公司代码基础数据
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
    FROM T001
    WHERE BUKRS = P_BUKRS.
  SORT GT_T001 BY BUKRS.

 "取销售订单抬头数据
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
    FROM VBAK
    FOR  ALL ENTRIES IN GT_DATA
    WHERE VBELN = GT_DATA-VBEL2
    AND   AUART IN ( 'ZPO' ,'ZF1','ZZG' ).
  SORT GT_VBAK BY VBELN.

 "取物料主数据描述
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FROM MAKT
    FOR ALL ENTRIES IN GT_DATA
    WHERE MATNR = GT_DATA-MATNR
    AND   SPRAS = '1'.
 SORT GT_MAKT BY MATNR.

"取科目主数据描述
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
  FROM SKAT
  FOR ALL  ENTRIES IN GT_DATA
  WHERE SAKNR  = GT_DATA-HKONT
  AND  SPRAS = '1'
  AND  KTOPL = 1000.
SORT GT_SKAT BY SAKNR.

"取销售凭证流已开票信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBFA
  FROM VBFA
  FOR ALL ENTRIES IN GT_DATA
  WHERE VBELV = GT_DATA-VBEL2
  AND   VBTYP_N IN ('M','N').

SORT GT_VBFA BY VBELV ASCENDING VBELN DESCENDING.

DELETE ADJACENT DUPLICATES FROM GT_VBFA COMPARING VBELV VBELN.

"取开票抬头信息
IF GT_VBFA[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBRK
  FROM VBRK
  FOR ALL ENTRIES IN GT_VBFA
  WHERE VBELN = GT_VBFA-VBELN.
SORT GT_VBRK BY VBELN.
ENDIF.

"取销售明细信息
IF GT_VBAK[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
  FROM VBAP
  FOR ALL ENTRIES IN GT_VBAK
  WHERE VBELN = GT_VBAK-VBELN
  AND   PSTYV IN ('Z01','Z02','Z21','Z22','Z31','Z32','Z41','Z42').

 SORT GT_VBAP BY VBELN POSNR.
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
 LOOP AT  GT_DATA ASSIGNING <FS_DATA> .

   "记录当前行
   L_TABIX = SY-TABIX.
   "订单类型
   READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-VBEL2 BINARY SEARCH .
     IF SY-SUBRC EQ '0'.
          <FS_DATA>-AUART = GT_VBAK-AUART.
          READ TABLE GT_VBAP WITH KEY VBELN = <FS_DATA>-VBEL2 POSNR = <FS_DATA>-POSN2 BINARY SEARCH .
            IF SY-SUBRC NE '0'.
               DELETE GT_DATA INDEX L_TABIX .
               CONTINUE.
            ENDIF.
      ELSE.
        DELETE GT_DATA INDEX L_TABIX .
        CONTINUE.
     ENDIF.
  IF <FS_DATA>-SHKZG EQ 'H'.
    <FS_DATA>-WRBTR = <FS_DATA>-WRBTR * -1 .
    <FS_DATA>-DMBTR = <FS_DATA>-DMBTR * -1.
  ENDIF.
  "本币码
  READ TABLE GT_T001 WITH KEY BUKRS = <FS_DATA>-BUKRS BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
     <FS_DATA>-WAERS_1 = GT_T001-WAERS.
   ENDIF.
  "科目描述
  READ TABLE GT_SKAT WITH KEY SAKNR = <FS_DATA>-HKONT BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
       <FS_DATA>-HKONT_TXT = GT_SKAT-TXT50.
    ENDIF.
 "物料描述
 READ TABLE GT_MAKT WITH KEY MATNR = <FS_DATA>-MATNR BINARY SEARCH .
  IF SY-SUBRC EQ 0 .
     <FS_DATA>-MAKTX = GT_MAKT-MAKTX.
  ENDIF.
"验收年度
READ TABLE  GT_VBFA WITH KEY VBELV = <FS_DATA>-VBEL2 BINARY SEARCH .
 IF SY-SUBRC EQ 0 .
     IF GT_VBFA-VBTYP_N NE 'N'.
        READ TABLE GT_VBRK WITH KEY VBELN = GT_VBFA-VBELN BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
              <FS_DATA>-YSYEAR = GT_VBRK-FKDAT+0(4).
           ENDIF.
      ENDIF.
 ENDIF.
"项目名称
PERFORM SELXMMC USING <FS_DATA>-VBEL2 CHANGING <FS_DATA>-XMMC.

ENDLOOP.
 SORT GT_DATA BY BUKRS GJAHR MONAT VBEL2 .


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
  GW_LAYOUT-BOX_FNAME    = 'SEL'.
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
  INIT_FIELDCAT 'MONAT'          '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'          '销售订单'         '' '' '' '' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'XMMC'           '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'          '订单类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSYEAR'         '验收年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '会计凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'          '行项目'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'          '科目'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT'      '科目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'          '凭证货币金额'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'          '本币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKTXT'          '摘要'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料号'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '物料描述'         '' '' '' '' '' '' ''.


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
*      -->P_0384   text
*      -->P_0385   text
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
      IF RS_SELFIELD-FIELDNAME = 'VBEL2'
        AND GS_DATA-VBEL2 IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBEL2.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
**----------------------------------------------------------------------*
*FORM FRM_BUILD_EVENT .
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_VBEL2  text
*      <--P_<FS_DATA>_XMMC  text
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
