REPORT ZCO005_3.
"结转记录（可冲销）
"date 20160312
"add by leyard it02

TABLES:BKPF,BSEG,VBAK,MAKT,SKAT,ZFI006,ZCO005_1.

DATA:BEGIN OF GS_DATA,
     BUKRS    TYPE    BKPF-BUKRS,               "公司代码
     GJAHR    TYPE    BKPF-GJAHR,               "会计年度
     MONAT    TYPE    BKPF-MONAT,               "会计期间
     VBEL2    TYPE    BSEG-VBEL2,               "销售订单
     POSN2    TYPE    BSEG-POSN2,               "销售订单行号
     AUART    TYPE    VBAK-AUART,               "订单类型
     YSYEAR   TYPE    BSEG-GJAHR,               "验收年度
     DQCPCB   TYPE    BSEG-DMBTR,               "当前产品成本
     DYGCFY   TYPE    BSEG-DMBTR,               "当月工程费用
     LJCPCB   TYPE    COSP-WOG001,              "累计产品成本
     LJGCFY   TYPE    COSP-WOG001,              "累计工程费用
     YZCB     TYPE    BSEG-DMBTR,               "应转成本
     JZKM     TYPE    BSEG-HKONT,               "结转科目
     JZKM_TXT TYPE    STRING ,                  "结转科目描述
     DFKM     TYPE    BSEG-HKONT,               "对方科目
     DFKM_TXT TYPE    STRING,                   "对方科目描述
     PZLX     TYPE    BKPF-BLART,                "凭证类型
     GZRQ     TYPE    BKPF-BUDAT,                "过账日期
     BKTXT    TYPE    BKPF-BKTXT,                "文本
     GZPZ     TYPE    BKPF-BELNR,                "过账凭证
     GZPZND   TYPE    BSEG-GJAHR,                "过账凭证年度
     WAERS    TYPE    BKPF-WAERS,                "货币码
     CXPZ     TYPE    BKPF-BELNR,                 "冲销凭证
     CXPZND   TYPE    BKPF-GJAHR,                 "冲销凭证年度
     INFO_MSG TYPE    STRING  ,                   "消息文本
     XMMC     TYPE    STRING ,                    "项目名称
     GZ_DATE  TYPE    ZCO005_1-GZ_DATE,           "过账日期
     GZ_NAME  TYPE    ZCO005_1-GZ_NAME ,          "过账名字
     CX_DATE  TYPE    ZCO005_1-GZ_DATE,           "冲销日期
     CX_NAME  TYPE    ZCO005_1-CX_NAME,           "冲销名字
     STATU    TYPE    ICONNAME,                   "冲销状态栏
     SEL(1) TYPE C ,
 END OF GS_DATA.


DATA:GS_ZCO005_1 LIKE ZCO005_1 ,
     GT_ZCO005_1 LIKE TABLE OF ZCO005_1 .

DATA: OK_CODE TYPE SY-UCOMM,
      SAVE_OK TYPE SY-UCOMM.

DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.



  DATA:GT_DATA lIKE TABLE OF GS_DATA WITH HEADER LINE.
   FIELD-SYMBOLS: <FS_DATA> LIKE GS_DATA.
  DATA:GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.
  DATA:GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.

" * BAPI_ACC_DOCUMENT_REV_POST
DATA: WA_REVERSAL TYPE BAPIACREV,
      WA_BUS      TYPE BAPIACHE09.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: IT_RETURN         TYPE TABLE OF BAPIRET2,
      WA_RETURN         TYPE BAPIRET2.


DATA: G_STGRD TYPE BKPF-STGRD.

DATA G_ANSWER     TYPE STRING. "控制弹出框

DATA:GT_BKPF LIKE TABLE OF BKPF WITH HEADER LINE,
     GS_BKPF LIKE BKPF .


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
  IF &1 = 'DQCPCB' OR &1 = 'DYGCFY' OR  &1 = 'YZCB'.
  GW_LVC-CFIELDNAME = 'WAERS'.
 ENDIF.
 IF &1 = 'YZCB'.
   GW_LVC-NO_SIGN = ''.
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


" *&---------------------------------------------------------------------*
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

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY,                      "公司代码
P_GJAHR  TYPE BKPF-GJAHR OBLIGATORY,                      "年度
P_MONAT  TYPE BKPF-MONAT OBLIGATORY.                      "期间
SELECT-OPTIONS: S_VBEL2 FOR BSEG-VBEL2.    "销售订单
SELECTION-SCREEN END OF BLOCK BLK1.


 "*&---------------------------------------------------------------------*
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
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0361   text
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
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  FROM ZCO005_1
  WHERE BUKRS EQ P_BUKRS
  AND GJAHR EQ P_GJAHR
  AND MONAT EQ P_MONAT
  AND VBEL2 IN S_VBEL2.
 SORT GT_DATA BY BUKRS GJAHR MONAT VBEL2 GZPZ GZPZND .
 CHECK GT_DATA[] IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
    FROM VBAK
    FOR  ALL ENTRIES IN GT_DATA
    WHERE VBELN = GT_DATA-VBEL2
    AND   AUART IN ('ZF1','ZZG','ZPO','ZSO') .
  SORT GT_VBAK BY VBELN.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
  FROM SKAT
  WHERE SPRAS = '1'
  AND  KTOPL = 1000.
SORT GT_SKAT BY SAKNR.

 "取会计凭证过账的抬头信息
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_BKPF
   FROM BKPF
   FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = P_BUKRS
   AND   GJAHR = GT_DATA-GZPZND
   AND   BELNR = GT_DATA-GZPZ.

 SORT GT_BKPF BY BUKRS GJAHR BELNR.

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
  "订单类型
   READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-VBEL2 BINARY SEARCH .
     IF SY-SUBRC EQ '0'.
          <FS_DATA>-AUART = GT_VBAK-AUART.
     ENDIF.

      "结转科目默认
     READ TABLE GT_SKAT WITH KEY SAKNR = <FS_DATA>-JZKM BINARY SEARCH .
       IF SY-SUBRC EQ 0 .
          <FS_DATA>-JZKM_TXT = GT_SKAT-TXT50.
       ENDIF.

     "对方科目
    READ TABLE GT_SKAT WITH KEY SAKNR = <FS_DATA>-DFKM BINARY SEARCH .
       IF SY-SUBRC EQ 0 .
          <FS_DATA>-DFKM_TXT = GT_SKAT-TXT50.
       ENDIF.
    "项目名称
PERFORM SELXMMC USING <FS_DATA>-VBEL2 CHANGING <FS_DATA>-XMMC.
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
  INIT_FIELDCAT 'STATU'          '冲销状态'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'          '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'          '销售订单'         '' '' '' '' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'XMMC'           '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'          '订单类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSYEAR'         '验收年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQCPCB'         '当期产品成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYGCFY'         '当期工程费用'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZCB'           '应转成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJCPCB'         '累计产品成本'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJGCFY'         '累计工程费用'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JZKM'           '结转科目'         '' '' '' '' '' 'BSEG' 'HKONT'.
  INIT_FIELDCAT 'JZKM_TXT'       '结转科目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DFKM'           '对方科目'         '' '' '' '' '' 'BSEG' 'HKONT'.
  INIT_FIELDCAT 'DFKM_TXT'       '对方科目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PZLX'           '凭证类型'         '' '' '' '' '' 'BKPF' 'BLART'.
  INIT_FIELDCAT 'GZRQ'           '过账日期'         '' '' '' '' '' 'BKPF' 'BUDAT'.
  INIT_FIELDCAT 'BKTXT'          '文本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GZPZ'           '过账凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GZPZND'         '过账凭证年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CXPZ'           '冲销凭证'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CXPZND'         '冲销凭证年度'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INFO_MSG'       '消息文本'       '' '' '' '' '' '' ''.
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
*      -->P_0427   text
*      -->P_0428   text
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
    "  IT_EVENTS                = GT_EVENTS[]
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
      IF RS_SELFIELD-FIELDNAME = 'GZPZ'
        AND GS_DATA-GZPZ IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_DATA-GZPZ.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GZPZND.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'VBEL2'
        AND GS_DATA-VBEL2 IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBEL2.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
   "冲销
    WHEN '&REV'.
       CLEAR G_ANSWER.

    "check
*    READ TABLE GT_DATA  INTO GS_DATA
*    WITH KEY SEL = 'X'
*       IF SY-SUBRC = 0.
*        MESSAGE '选择项已有冲销凭证，请重新选择！' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.

*    READ TABLE GT_DATA  INTO GS_DATA
*    WITH KEY SEL = ''.
*       IF SY-SUBRC = 0.
*        MESSAGE '请选择无冲销的过账凭证项进行操作！' TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.
*      ENDIF.
       CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION  = '确认要执行冲销操作'
        IMPORTING
          ANSWER         = G_ANSWER
*   TABLES
*         PARAMETER      =
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
      IF G_ANSWER <> '1'.
        EXIT.
      ENDIF.
 "*取冲销原因和过账日期
  CLEAR WA_REVERSAL.
  CALL SCREEN 9001 STARTING AT 25 10.
   IF WA_REVERSAL-REASON_REV IS INITIAL.
    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

 "冲销凭证
 PERFORM FRM_ACC_REVERSAL.
 "保存到自建表
IF GT_ZCO005_1 IS NOT INITIAL.

  SORT GT_ZCO005_1 BY BUKRS GJAHR MONAT VBEL2 GZPZ GZPZND .

  "’修改自建表
  MODIFY ZCO005_1 FROM TABLE GT_ZCO005_1.

ENDIF.
  ENDCASE.
CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
 SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'OK'.
      IF G_STGRD IS INITIAL.
        MESSAGE '冲销原因必输！' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        WA_REVERSAL-REASON_REV = G_STGRD.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: WA_REVERSAL-REASON_REV,
             WA_REVERSAL-PSTNG_DATE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ACC_REVERSAL .

LOOP AT GT_DATA INTO GS_DATA WHERE SEL = 'X' AND CXPZ = ''.
 CLEAR:GS_BKPF,WA_RETURN,WA_REVERSAL-OBJ_TYPE,WA_REVERSAL-OBJ_KEY,WA_REVERSAL-OBJ_KEY_R,WA_OBJ.
 REFRESH IT_RETURN.
 READ TABLE GT_BKPF INTO GS_BKPF WITH KEY BUKRS = GS_DATA-BUKRS GJAHR = GS_DATA-GZPZND BELNR = GS_DATA-GZPZ BINARY SEARCH.
  IF  SY-SUBRC EQ 0 .
      WA_REVERSAL-OBJ_TYPE  = GS_BKPF-AWTYP.
      WA_REVERSAL-OBJ_KEY   = GS_BKPF-AWKEY.
      WA_REVERSAL-OBJ_KEY_R = GS_BKPF-AWKEY.
  ENDIF.
"  *   取得系统 LOGICAL SYSTEM
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      OWN_LOGICAL_SYSTEM = WA_REVERSAL-OBJ_SYS.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      REVERSAL = WA_REVERSAL
      BUS_ACT  = 'RFBU'
    IMPORTING
      OBJ_TYPE = WA_OBJ-OBJ_TYPE
      OBJ_KEY  = WA_OBJ-OBJ_KEY
      OBJ_SYS  = WA_OBJ-OBJ_SYS
    TABLES
      RETURN   = IT_RETURN.

    READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
       CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
       CLEAR WA_REVERSAL.
       LOOP AT IT_RETURN INTO WA_RETURN .
         IF GS_DATA-INFO_MSG EQ ''.
         CONCATENATE WA_RETURN-ID WA_RETURN-TYPE WA_RETURN-MESSAGE INTO GS_DATA-INFO_MSG .


         ELSE.
          CONCATENATE GS_DATA-INFO_MSG '；' WA_RETURN-ID WA_RETURN-TYPE WA_RETURN-MESSAGE INTO GS_DATA-INFO_MSG .

         ENDIF.
       ENDLOOP.
         "红灯状态：
         GS_DATA-STATU = ICON_RED_LIGHT .
         MODIFY GT_DATA FROM GS_DATA.
          CONTINUE.
    ELSE.
       CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
        WAIT = 'X'.

       "回写生成的冲销凭证号 与冲销凭证年度
       GS_DATA-CXPZ   =  WA_OBJ-OBJ_KEY(10).
       GS_DATA-CXPZND = WA_OBJ-OBJ_KEY+14(4).

        "绿灯状态：
       GS_DATA-STATU = ICON_GREEN_LIGHT.

       "消息描述
       GS_DATA-INFO_MSG = '冲销成功！'.

       MODIFY GT_DATA FROM GS_DATA.

       "冲销成功的记录保存到GT_ZCO005_1.
       MOVE-CORRESPONDING GS_DATA TO GS_ZCO005_1.
       "冲销人、冲销日期
       GS_ZCO005_1-CX_NAME = SY-UNAME.
       GS_ZCO005_1-CX_DATE = SY-DATUM.
       GS_ZCO005_1-CX_TIME = SY-UZEIT.
       APPEND GS_ZCO005_1 TO GT_ZCO005_1.
    ENDIF.
ENDLOOP.
ENDFORM.
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
