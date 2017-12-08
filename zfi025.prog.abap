*&---------------------------------------------------------------------*
*& Report  ZFIR024
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZFIR024.
TABLES: FAGLFLEXT,
        BSEG,
        BSAK,
        LFA1,
        KNB1,
        VBAK,
        RF05L,
        BSIK,
        LFM1.

TYPE-POOLS: SLIS.

*---------------------------------------------------------------------*
* 内表、全局变量 定义
*---------------------------------------------------------------------*
*ALV输出结构
TYPES:
  BEGIN OF TY_NAME,
    BUKRS     TYPE BSIK-BUKRS,              "公司代码
    HKONT     TYPE BSIK-HKONT,              "总账科目
    TXT50     TYPE SKAT-TXT50,              "科目描述
    LIFNR     TYPE BSIK-LIFNR,              "客户号
    NAME1     TYPE KNA1-NAME1,              "客户描述
    SHKZG     TYPE CHAR10,                  "借贷方向
    WAERS     TYPE BSIK-WAERS,              "货币
    BEG_WBTR  TYPE BSIK-WRBTR,           "期初余额-交易币
    BEG_DBTR  TYPE BSIK-DMBTR,           "期初余额-本位币
    WBTR_S    TYPE BSIK-WRBTR,             "开票金额-交易币-借方
    WBTR_H    TYPE BSIK-WRBTR,             "开票金额-交易币-贷方
    DBTR_S    TYPE BSIK-DMBTR,             "开票金额-本位币-借方
    DBTR_H    TYPE BSIK-DMBTR,             "开票金额-本位币-贷方
    END_WBTR  TYPE BSIK-WRBTR,           "期末余额-交易币
    END_DBTR  TYPE BSIK-DMBTR,           "期末余额-本位币
    EKGRP     TYPE LFM1-EKGRP, "采购组
    EKNAM     TYPE T024-EKNAM , "采购组名
    CELLCOLOR TYPE LVC_T_SCOL,   "单元格颜色设置
  END OF TY_NAME.

TYPES:
  BEGIN OF TY_NAME1,
    BUKRS    TYPE BSIK-BUKRS,              "公司代码
    HKONT    TYPE BSIK-HKONT,              "总账科目
    TXT50    TYPE SKAT-TXT50,              "科目描述
    LIFNR    TYPE BSIK-LIFNR,              "客户号
    NAME1    TYPE KNA1-NAME1,              "客户描述
    SHKZG    TYPE CHAR10,                  "借贷方向
    BEG_WBTR TYPE BSIK-WRBTR,           "期初余额-交易币
    BEG_DBTR TYPE BSIK-DMBTR,           "期初余额-本位币
    WBTR_S   TYPE BSIK-WRBTR,             "开票金额-交易币-借方
    WBTR_H   TYPE BSIK-WRBTR,             "开票金额-交易币-贷方
    DBTR_S   TYPE BSIK-DMBTR,             "开票金额-本位币-借方
    DBTR_H   TYPE BSIK-DMBTR,             "开票金额-本位币-贷方
    END_WBTR TYPE BSIK-WRBTR,           "期末余额-交易币
    END_DBTR TYPE BSIK-DMBTR,           "期末余额-本位币
  END OF TY_NAME1.


TYPES:
  BEGIN OF TY_SKB1,
    BUKRS TYPE SKB1-BUKRS,
    SAKNR TYPE SKB1-SAKNR,
    STEXT TYPE SKB1-STEXT,
  END OF TY_SKB1.

TYPES:
  BEGIN OF TY_BSIK,
    BUKRS TYPE BSIK-BUKRS,             "公司代码
    HKONT TYPE BSIK-HKONT,             "总账科目
    LIFNR TYPE BSIK-LIFNR,             "供应商编号
    AUGBL TYPE BSIK-AUGBL,             "清账凭证号
    BELNR TYPE BSIK-BELNR,             "凭证编号
    BUZEI TYPE BSIK-BUZEI,             "凭证行号
    GJAHR TYPE BKPF-GJAHR,             "年度
    WAERS TYPE BSIK-WAERS,             "货币码
    DMBTR TYPE BSIK-DMBTR,             "本位币金额
    WRBTR TYPE BSIK-WRBTR,             "凭证金额
    ZUMSK TYPE BSIK-ZUMSK,             "特别总账标识
    SHKZG TYPE BSIK-SHKZG,             "借贷标识
  END OF TY_BSIK.

*TYPES:
*      BEGIN OF TY_BSIS,
*      BUKRS TYPE BSIS-BUKRS,             "公司代码
*      HKONT TYPE BSIS-HKONT,             "总账科目
*      LIFNR TYPE BSIK-LIFNR,             "供应商编号
*      AUGBL TYPE BSIS-AUGBL,             "清账凭证号
*      BELNR TYPE BSIS-BELNR,             "凭证编号
*      WAERS TYPE BSIS-WAERS,             "货币码
*      DMBTR TYPE BSIS-DMBTR,             "本位币金额
*      WRBTR TYPE BSIS-WRBTR,             "凭证金额
**      ZUMSK TYPE BSIS-ZUMSK,             "特别总账标识
*      SHKZG TYPE BSIS-SHKZG,             "借贷标识
*      END OF TY_BSIS.

TYPES:
  BEGIN OF TY_SKAT,
    SAKNR TYPE SKAT-SAKNR,
    TXT50 TYPE SKAT-TXT50,
  END OF TY_SKAT.

TYPES:
  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1.

TYPES:BEGIN OF TY_BKPF,
        BUKRS TYPE BKPF-BUKRS,
        BELNR TYPE BKPF-BELNR,
        GJAHR TYPE BKPF-GJAHR,
        BLART TYPE BKPF-BLART,
        BUDAT TYPE BKPF-BUDAT,
        WAERS TYPE BKPF-WAERS,
      END OF TY_BKPF.

*凭证明细结构
TYPES:BEGIN OF TY_BSEG,
        BUKRS TYPE BSEG-BUKRS,
        BELNR TYPE BSEG-BELNR,
        GJAHR TYPE BSEG-GJAHR,
        BLART TYPE BKPF-BLART,
        BUZEI TYPE BSEG-BUZEI,
        WAERS TYPE BKPF-WAERS,           "货币码
        BUDAT TYPE BKPF-BUDAT,           "过账日期
        SHKZG TYPE BSEG-SHKZG,           "借贷标识
        DMBTR TYPE BSEG-DMBTR,           "本位币金额
        WRBTR TYPE BSEG-WRBTR,           "交易币金额
        SGTXT TYPE BSEG-SGTXT,           "摘要
        HKONT TYPE BSEG-HKONT,           "科目号
        LIFNR TYPE BSEG-LIFNR,
        XNEGP TYPE BSEG-XNEGP,           "反记账标识
        ZUONR TYPE BSEG-ZUONR,
        VBEL2 TYPE BSEG-VBEL2,
        ZTERM TYPE BSEG-ZTERM,
        VBELN TYPE BSEG-VBELN,
        ZLSCH TYPE BSEG-ZLSCH,
        ZFBDT TYPE BSEG-ZFBDT,
        UMSKZ TYPE BSEG-UMSKZ,
      END OF TY_BSEG.

TYPES:BEGIN OF TY_SUM,
        BUKRS    TYPE BSEG-BUKRS,           "公司代码
        HKONT    TYPE BSEG-HKONT,           "科目号
        LIFNR    TYPE BSEG-LIFNR,
        WAERS    TYPE BKPF-WAERS,           "货币码
        BEG_DBTR TYPE FAGLFLEXT-TSL01 , " bseg-dmbtr,        "期初本位币
        BEG_WBTR TYPE FAGLFLEXT-TSL01, "bseg-wrbtr,        "期初交易货币
        DMBTR_S  TYPE FAGLFLEXT-TSL01, "bseg-dmbtr,           "本位币金额
        DMBTR_H  TYPE FAGLFLEXT-TSL01, "bseg-dmbtr,
        WRBTR_S  TYPE FAGLFLEXT-TSL01, "bseg-wrbtr,           "交易币金额
        WRBTR_H  TYPE FAGLFLEXT-TSL01, "bseg-wrbtr,
      END OF TY_SUM.

*输出明细结构
TYPES:BEGIN OF TY_DETAIL,
        BUKRS     TYPE BKPF-BUKRS,
        GJAHR     TYPE BKPF-GJAHR,
        BLART     TYPE BKPF-BLART,
        HKONT     TYPE BSEG-HKONT,
        TXT50     TYPE SKAT-TXT50,              "科目描述
        LIFNR     TYPE BSEG-LIFNR,
        NAME1     TYPE KNA1-NAME1,              "客户描述
        WAERS     TYPE BKPF-WAERS,           "货币码
        BELNR     TYPE BSEG-BELNR,
        BUZEI     TYPE BSEG-BUZEI,            "行号
        BUDAT     TYPE BKPF-BUDAT,
        WRBTR_S   TYPE BSEG-WRBTR,
        WRBTR_H   TYPE BSEG-WRBTR,
        WRBTR_T   TYPE BSEG-WRBTR,
        DMBTR_S   TYPE BSEG-DMBTR,
        DMBTR_H   TYPE BSEG-DMBTR,
        DMBTR_T   TYPE BSEG-DMBTR,
        SHKZG     TYPE CHAR10,           "借贷标识
        SGTXT     TYPE BSEG-SGTXT,           "摘要
        DMBTR     TYPE BSEG-DMBTR,           "科目号
        WRBTR     TYPE BSEG-WRBTR,
        ZUONR     TYPE BSEG-ZUONR,
        VBEL2     TYPE BSEG-VBEL2,
        ZTERM     TYPE BSEG-ZTERM,
        VBELN     TYPE BSEG-VBELN,
        ZLSCH     TYPE BSEG-ZLSCH,
        ZFBDT     TYPE BSEG-ZFBDT,
        UMSKZ     TYPE BSEG-UMSKZ,
        EKGRP     TYPE LFM1-EKGRP, "采购组
        EKNAM     TYPE T024-EKNAM , "采购组名
        CELLCOLOR TYPE LVC_T_SCOL,   "单元格颜色设置
      END OF TY_DETAIL.
"采购组名称
TYPES:BEGIN OF TY_LFM1 ,
        LIFNR TYPE LFM1-LIFNR, "供应商号
        EKORG TYPE LFM1-EKORG, "采购组
        EKGRP TYPE LFM1-EKGRP, "采购组
        EKNAM TYPE T024-EKNAM, "采购组ming
      END OF  TY_LFM1.

DATA: IT_NAME TYPE TABLE OF TY_NAME,
      WA_NAME TYPE TY_NAME.

DATA: IT_SKAT TYPE TABLE OF TY_SKAT,
      WA_SKAT LIKE LINE OF IT_SKAT.

DATA: IT_SKB1 TYPE TABLE OF TY_SKB1,
      WA_SKB1 TYPE TY_SKB1.

DATA: IT_BSIK TYPE TABLE OF TY_BSIK,
      WA_BSIK TYPE TY_BSIK.

DATA: IT_BSIK02 TYPE TABLE OF TY_BSIK,
      WA_BSIK02 TYPE TY_BSIK.

DATA: IT_BSIS       TYPE TABLE OF TY_BSIK,
      WA_BSIS       TYPE TY_BSIK,
      IT_BSIS_LIFNR TYPE TABLE OF TY_BSIK WITH HEADER LINE.

DATA: IT_LFA1 TYPE TABLE OF TY_LFA1,
      WA_LFA1 LIKE LINE OF IT_LFA1.

DATA: IT_BKPF TYPE TABLE OF TY_BKPF,
      WA_BKPF LIKE LINE OF IT_BKPF.

DATA: IT_BSEG TYPE TABLE OF TY_BSEG,
      WA_BSEG LIKE LINE OF IT_BSEG.

DATA: IT_BSEG_ZJB TYPE TABLE OF TY_BSEG.
DATA: IT_BSEG_LIFNR TYPE TABLE OF TY_BSEG WITH HEADER LINE.
DATA: WA_BSEG_ZJB TYPE TY_BSEG.


DATA: IT_SUM TYPE TABLE OF TY_SUM,
      WA_SUM LIKE LINE OF IT_SUM.

DATA: IT_DETAIL TYPE TABLE OF TY_DETAIL,
      WA_DETAIL LIKE LINE OF IT_DETAIL.

DATA: OK_CODE LIKE SY-UCOMM,
      SAVE_OK LIKE SY-UCOMM.

DATA GW_VARIANT       TYPE DISVARIANT.
DATA GW_VARIANT1      TYPE DISVARIANT.

DATA: IT_NAME1 TYPE TABLE OF TY_NAME1,
      WA_NAME1 LIKE LINE OF IT_NAME1.

DATA: STA_SELECT  TYPE TABLE OF EDPLINE,
      LIFNR_FIELD TYPE EDPLINE,
      STA_WHERE   TYPE STRING.

DATA: W_ZFI015 TYPE ZFI025.

DATA T_LIFNR TYPE HASHED TABLE OF LIFNR WITH UNIQUE KEY TABLE_LINE WITH HEADER LINE.

DATA : T_LFM1 TYPE TABLE OF TY_LFM1 WITH HEADER LINE.
*      TREF     TYPE REF TO DATA.
*---------------------------------------------------------------------*
* ALV变量 定义
*---------------------------------------------------------------------*
TYPE-POOLS: SLIS.
DATA: GT_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GT_FIELDCAT_DETAIL TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GS_LAYOUT          TYPE SLIS_LAYOUT_ALV,
      GS_PRINT           TYPE SLIS_PRINT_ALV,    "ALV打印格式
      G_REPID            LIKE SY-REPID.
DATA: GIT_EVENTS     TYPE SLIS_T_EVENT, "ALV 事件
      GIT_LISTHEADER TYPE SLIS_T_LISTHEADER. "ALV 表头

*----------------------------------------------------------------------*
* 宏定义
*----------------------------------------------------------------------*
DEFINE DEF_FIELDCAT.
  CLEAR GT_FIELDCAT.
  GT_FIELDCAT-FIELDNAME = &1.
  GT_FIELDCAT-SELTEXT_L = &2.
  GT_FIELDCAT-NO_ZERO = &3.
  GT_FIELDCAT-JUST = &4.
  GT_FIELDCAT-OUTPUTLEN = &5.
  APPEND GT_FIELDCAT.
END-OF-DEFINITION.

DEFINE DEF_FIELDCAT_DETAIL.
  CLEAR GT_FIELDCAT_DETAIL.
  GT_FIELDCAT_DETAIL-FIELDNAME = &1.
  GT_FIELDCAT_DETAIL-SELTEXT_L = &2.
  GT_FIELDCAT_DETAIL-NO_ZERO = &3.
  GT_FIELDCAT_DETAIL-JUST = &4.
  GT_FIELDCAT_DETAIL-OUTPUTLEN = &5.
  GT_FIELDCAT_DETAIL-NO_OUT = &6.
  APPEND GT_FIELDCAT_DETAIL.
END-OF-DEFINITION.

*---------------------------------------------------------------------*
* 选择屏幕
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:P_BUKRS FOR RF05L-BUKRS OBLIGATORY.       "公司代码
SELECT-OPTIONS:S_HKONT FOR BSIK-HKONT.                 "总账科目
SELECT-OPTIONS:S_MONAT FOR SY-DATUM OBLIGATORY.        "会计期间
SELECT-OPTIONS:S_LIFNR FOR LFA1-LIFNR.                 "供应商编号
*SELECTION-SCREEN SKIP 1.
*PARAMETERS:p_chk type c as CHECKBOX .                  "显示本年累计
SELECT-OPTIONS:S_UMSKZ FOR BSIK-UMSKZ.
"SELECT-OPTIONS:s_ekgrp for lfm1-ekgrp.

SELECTION-SCREEN END OF BLOCK BLK1.

*PARAMETERS:p_all RADIOBUTTON GROUP R1,                 "显示本年累计
*           p_wrb RADIOBUTTON GROUP R1 DEFAULT 'X'.
PARAMETER: P_ALL TYPE C AS CHECKBOX."是否同时显示交易货币
PARAMETER: P_ZJB TYPE C AS CHECKBOX DEFAULT 'X'."包含自建表数据.



SELECTION-SCREEN:
  SKIP 2,
  BEGIN OF LINE,
    PUSHBUTTON 1(10) BTN1 USER-COMMAND COM_ZJB,
  END OF LINE.
*  SELECTION-SCREEN:
*    SKIP 1,
*    BEGIN OF BLOCK BLK WITH FRAME TITLE T1,
*      BEGIN OF LINE,
*        PUSHBUTTON 2(10) BTN1 USER-COMMAND COMM1,
*        PUSHBUTTON 12(10) BTN2 USER-COMMAND COMM2,
*        PUSHBUTTON 22(10) BTN3 USER-COMMAND COMM3,
*      END OF LINE,
*    END OF BLOCK BLK.
*---------------------------------------------------------------------*
* at selection-screen
*---------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*LOOP AT SCREEN.
*  IF screen-name = 'P_CHK'.
*    screen-input = '0'.
*    MODIFY SCREEN.
*  ENDIF.
*ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HKONT-LOW.
  PERFORM FRM_HELP.
  PERFORM FRM_SEARCH_HELP_HKONT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HKONT-HIGH.
  PERFORM FRM_HELP.
  PERFORM FRM_SEARCH_HELP_HKONT.

AT SELECTION-SCREEN.
  IF SY-UCOMM EQ 'COM_ZJB'.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        ACTION                       = 'S'
*       CORR_NUMBER                  = '          '
*       GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*       SHOW_SELECTION_POPUP         = ' '
        VIEW_NAME                    = 'ZFI025'
*       NO_WARNING_FOR_CLIENTINDEP   = ' '
*       RFC_DESTINATION_FOR_UPGRADE  = ' '
*       CLIENT_FOR_UPGRADE           = ' '
*       VARIANT_FOR_SELECTION        = ' '
*       COMPLEX_SELCONDS_USED        = ' '
*       CHECK_DDIC_MAINFLAG          = ' '
*       SUPPRESS_WA_POPUP            = ' '
*     TABLES
*       DBA_SELLIST                  =
*       EXCL_CUA_FUNCT               =
      EXCEPTIONS
        CLIENT_REFERENCE             = 1
        FOREIGN_LOCK                 = 2
        INVALID_ACTION               = 3
        NO_CLIENTINDEPENDENT_AUTH    = 4
        NO_DATABASE_FUNCTION         = 5
        NO_EDITOR_FUNCTION           = 6
        NO_SHOW_AUTH                 = 7
        NO_TVDIR_ENTRY               = 8
        NO_UPD_AUTH                  = 9
        ONLY_SHOW_ALLOWED            = 10
        SYSTEM_FAILURE               = 11
        UNKNOWN_FIELD_IN_DBA_SELLIST = 12
        VIEW_NOT_FOUND               = 13
        MAINTENANCE_PROHIBITED       = 14
        OTHERS                       = 15.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
*   特别总账标识判断
*  at SELECTION-SCREEN.
*  LOOP AT s_umskz.
*    IF s_umskz-option = 'EQ'.
*      IF s_umskz-low = 'W'.
*        MESSAGE '不能查询特别总账标识为W的数据' TYPE 'E'.
*      ENDIF.
*    ENDIF.
*    IF s_umskz-option = 'BT'.
*      IF s_umskz-low = 'W' or s_umskz-high = 'W'.
*        MESSAGE '不能查询特别总账标识为W的数据' TYPE 'E'.
*      ELSEIF s_umskz-high > 'W'.
*        MESSAGE '不能查询特别总账标识为W的数据' TYPE 'E'.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
INITIALIZATION.

  BTN1 = '维护自建表'.

*--------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_FETCH_DATA.

*---------------------------------------------------------------------*
* end-of-selection
*---------------------------------------------------------------------*
END-OF-SELECTION.

*IF SY-TCODE NE 'ZFI021'.  "现金流量表
  PERFORM FRM_GET_EVENT.           "设置自定义事件处理程序
  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_VARIANT.
  PERFORM INIT_FIELDCAT.           "设置输出字段
  PERFORM FRM_OUTPUT.              "输出
*  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
*  GS_LAYOUT-F2CODE = '&ETA'.
  GS_LAYOUT-DETAIL_POPUP = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  GS_LAYOUT-TOTALS_ONLY  = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  " 单元格颜色字段
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_FIELDCAT .
  IF P_ALL = ''.
    DEF_FIELDCAT 'BUKRS'           '公司代码'          ' '  'L '   '4'.
    DEF_FIELDCAT 'HKONT'           '科目编号'          ' '  'L '   '10'.
    DEF_FIELDCAT 'TXT50'           '科目描述'        'X'  'L'   '20'.
******供应商字段增加参考表和参考字段
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'LIFNR'.
    GT_FIELDCAT-REF_FIELDNAME = 'LIFNR'.
    GT_FIELDCAT-REF_TABNAME = 'LFA1'.
    GT_FIELDCAT-SELTEXT_L = '供应商编号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
    DEF_FIELDCAT 'NAME1'           '供应商描述'      'X'  'L'   '10'.
    "添加采购组、采购组名
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'EKGRP'.
    GT_FIELDCAT-REF_FIELDNAME = 'EKGRP'.
    GT_FIELDCAT-REF_TABNAME = 'T024'.
    GT_FIELDCAT-SELTEXT_L = '采购组编号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
    DEF_FIELDCAT 'EKNAM'           '采购组名称'  'X'  'L'   '10'.
    DEF_FIELDCAT 'SHKZG'           '借贷方向'  'X'  'L'   '5'.
*    DEF_FIELDCAT 'WAERS'           '货币'      'X'  'L'   '5'.
    DEF_FIELDCAT 'BEG_DBTR'           '期初余额-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_S'           '本期借方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_H'           '本期贷方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'END_DBTR'           '期末余额-本位币'      ''  'L'   '10'.
  ELSEIF P_ALL = 'X'.
    DEF_FIELDCAT 'BUKRS'           '公司代码'          ' '  'L '   '4'.
    DEF_FIELDCAT 'HKONT'           '科目编号'          ' '  'L '   '10'.
    DEF_FIELDCAT 'TXT50'           '科目描述'        'X'  'L'   '20'.
******供应商字段增加参考表和参考字段
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'LIFNR'.
    GT_FIELDCAT-REF_FIELDNAME = 'LIFNR'.
    GT_FIELDCAT-REF_TABNAME = 'LFA1'.
    GT_FIELDCAT-SELTEXT_L = '供应商编号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
    DEF_FIELDCAT 'NAME1'           '供应商描述'      'X'  'L'   '10'.
    "添加采购组、采购组名
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'EKGRP'.
    GT_FIELDCAT-REF_FIELDNAME = 'EKGRP'.
    GT_FIELDCAT-REF_TABNAME = 'T024'.
    GT_FIELDCAT-SELTEXT_L = '采购组编号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
    DEF_FIELDCAT 'EKNAM'           '采购组名称'  'X'  'L'   '10'.
*    DEF_FIELDCAT 'LIFNR'           '供应商编号'      'X'  'L'   '10'.


    DEF_FIELDCAT 'SHKZG'           '借贷方向'  'X'  'L'   '5'.
    DEF_FIELDCAT 'WAERS'           '货币'      'X'  'L'   '5'.
    DEF_FIELDCAT 'BEG_WBTR'           '期初余额-交易货币'      ''  'L'   '10'.
    DEF_FIELDCAT 'BEG_DBTR'           '期初余额-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'WBTR_S'           '本期借方-交易货币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_S'           '本期借方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'WBTR_H'           '本期贷方-交易货币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_H'           '本期贷方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'END_WBTR'           '期末余额-交易货币'      ''  'L'   '10'.
    DEF_FIELDCAT 'END_DBTR'           '期末余额-本位币'      ''  'L'   '10'.
  ENDIF.

ENDFORM.                    " INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_FIELDCAT_DETAIL .
  IF P_ALL = 'X'.
    DEF_FIELDCAT_DETAIL 'BUKRS'           '公司代码'          'X'  'L '   '4' ''.
    DEF_FIELDCAT_DETAIL 'HKONT'           '科目编号'          'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'TXT50'           '科目描述'        'X'  'L'   '20' ''.
******供应商编号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'LIFNR'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'LIFNR'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'LFA1'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '供应商编号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    GT_FIELDCAT_DETAIL-NO_OUT = ''.
    APPEND GT_FIELDCAT_DETAIL.
*    DEF_FIELDCAT_DETAIL 'LIFNR'           '供应商编号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'NAME1'           '供应商描述'      'X'  'L'   '10' ''.
    "添加采购组、采购组名
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'EKGRP'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'EKGRP'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'T024'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '采购组编号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT_DETAIL.
    DEF_FIELDCAT_DETAIL 'EKNAM'           '采购组名称'  'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WAERS'           '货币'  'X'  'L'   '5' ''.
    DEF_FIELDCAT_DETAIL 'BELNR'           '会计凭证号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'BUZEI'           '行号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'BUDAT'           '过账日期'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'ZUONR'           '分配'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'SGTXT'           '摘要'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_S'         '借方金额-交易货币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_H'         '贷方金额-交易货币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_T'         '余额-交易货币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'DMBTR_S'         '借方金额-本位币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'DMBTR_H'         '贷方金额-本位币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'DMBTR_T'         '余额-本位币'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'SHKZG'           '借贷方向'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'DMBTR'           '本位币金额'      ''  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'WRBTR'           '交易金额'      ''  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZUONR'           '分配'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'BLART'           '凭证类型'      'X'  'L'   '10' 'X'.
*    DEF_FIELDCAT_DETAIL 'VBEL2'           '销售订单'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZTERM'           '付款条件'      'X'  'L'   '10' 'X'.
*    DEF_FIELDCAT_DETAIL 'VBELN'           '开票凭证'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZLSCH'           '付款方式'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZFBDT'           '付款基准日'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'UMSKZ'           '特别总账标识'      'X'  'L'   '5' 'X'.
  ELSE.
    DEF_FIELDCAT_DETAIL 'BUKRS'           '公司代码'          'X'  'L '   '4' ''.
    DEF_FIELDCAT_DETAIL 'HKONT'           '科目编号'          'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'TXT50'           '科目描述'        'X'  'L'   '20' ''.
******供应商编号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'LIFNR'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'LIFNR'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'LFA1'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '供应商编号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    GT_FIELDCAT_DETAIL-NO_OUT = ''.
    APPEND GT_FIELDCAT_DETAIL.
*    DEF_FIELDCAT_DETAIL 'LIFNR'           '供应商编号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'NAME1'           '供应商描述'      'X'  'L'   '10' ''.
    "添加采购组、采购组名
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'EKGRP'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'EKGRP'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'T024'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '采购组编号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT_DETAIL.
    DEF_FIELDCAT_DETAIL 'EKNAM'           '采购组名称'  'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WAERS'           '货币'  'X'  'L'   '5' ''.
    DEF_FIELDCAT_DETAIL 'BELNR'           '会计凭证号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'BUZEI'           '行号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'BUDAT'           '过账日期'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'ZUONR'           '分配'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'SGTXT'           '摘要'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_S'         '借方金额'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_H'         '贷方金额'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'SHKZG'           '借贷方向'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'WRBTR_T'         '余额'      ''  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'DMBTR'           '本位币金额'      ''  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'WRBTR'           '交易金额'      ''  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZUONR'           '分配'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'BLART'           '凭证类型'      'X'  'L'   '10' 'X'.
*    DEF_FIELDCAT_DETAIL 'VBEL2'           '销售订单'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZTERM'           '付款条件'      'X'  'L'   '10' 'X'.
*    DEF_FIELDCAT_DETAIL 'VBELN'           '开票凭证'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZLSCH'           '付款方式'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZFBDT'           '付款基准日'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'UMSKZ'           '特别总账标识'      'X'  'L'   '5' 'X'.
  ENDIF.

ENDFORM.                    " INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .
  G_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = G_REPID
      IS_LAYOUT               = GS_LAYOUT
      IT_FIELDCAT             = GT_FIELDCAT[]
      I_SAVE                  = 'X'
      IS_VARIANT              = GW_VARIANT
      IT_EVENTS               = GIT_EVENTS[]
      IS_PRINT                = GS_PRINT
*     I_CALLBACK_PF_STATUS_SET = 'FRM_STATUS'
      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
    TABLES
      T_OUTTAB                = IT_NAME
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " FRM_OUTPUT

FORM FRM_OUTPUT_DETAIL .
  G_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT_DETAIL[]
      I_CALLBACK_PF_STATUS_SET = 'F_SETSTATUS'
      I_CALLBACK_USER_COMMAND  = 'F_USER_COMMAND_DETAIL'
      IS_VARIANT               = GW_VARIANT1
      I_SAVE                   = 'X'
      I_SCREEN_START_COLUMN    = 10
      I_SCREEN_START_LINE      = 1
      I_SCREEN_END_COLUMN      = 140
      I_SCREEN_END_LINE        = 25
    TABLES
      T_OUTTAB                 = IT_DETAIL
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_HELP .
  DATA: DYNAME TYPE D020S-PROG,
        DYNUMB TYPE D020S-DNUM,
*       code TYPE DYNFIELDVALUE.
        CODE   TYPE RANGE OF BSID-BUKRS,
        L_CODE LIKE LINE OF CODE.
  DATA: DYNPFIELDS TYPE DYNPREAD OCCURS 0 WITH HEADER LINE.
  CLEAR: IT_SKB1[].
  DYNAME = SY-CPROG.
  DYNUMB = SY-DYNNR.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = DYNAME
      DYNUMB               = DYNUMB
      REQUEST              = 'A'
    TABLES
      DYNPFIELDS           = DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.
  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'P_BUKRS-LOW'.
  IF SY-SUBRC = 0.
*    l_code-sign   = 'I'.
*    l_code-option = 'BT'.
    L_CODE-LOW = DYNPFIELDS-FIELDVALUE.
  ENDIF.
  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'P_BUKRS-HIGH'.
  IF SY-SUBRC = 0.
    L_CODE-HIGH = DYNPFIELDS-FIELDVALUE.
  ENDIF.
  IF L_CODE-HIGH IS INITIAL.
    L_CODE-SIGN   = 'I'.
    L_CODE-OPTION = 'EQ'.
  ELSE.
    L_CODE-SIGN   = 'I'.
    L_CODE-OPTION = 'BT'.
  ENDIF.
  APPEND L_CODE TO CODE.
  CLEAR: L_CODE.
  IF CODE IS INITIAL.
    SELECT BUKRS SAKNR STEXT
      FROM SKB1
      INTO CORRESPONDING FIELDS OF TABLE IT_SKB1
    WHERE MITKZ = 'K'.
  ELSE.
    SELECT BUKRS SAKNR STEXT
      FROM SKB1
      INTO CORRESPONDING FIELDS OF TABLE IT_SKB1
    WHERE MITKZ = 'K' AND BUKRS IN CODE.
  ENDIF.

  SELECT SAKNR TXT50
     INTO TABLE IT_SKAT
     FROM SKAT
     WHERE KTOPL = '1000'
  AND SPRAS = '1'.
  SORT IT_SKAT BY SAKNR.

  LOOP AT IT_SKB1 INTO WA_SKB1.
    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_SKB1-SAKNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SKB1-STEXT = WA_SKAT-TXT50.
      MODIFY IT_SKB1 FROM WA_SKB1 TRANSPORTING STEXT.
    ENDIF.
    CLEAR:WA_SKB1,WA_SKAT.
  ENDLOOP.
  CLEAR: IT_SKAT.
ENDFORM.                    " FRM_HELP

*&---------------------------------------------------------------------*
*&      Form  FRM_SEARCH_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*---------------uan-------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SEARCH_HELP_HKONT .

  DATA: V_FUNC LIKE HELP_INFO-DYNPROFLD.
  V_FUNC = 's_hkont'.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SAKNR'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = V_FUNC
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_SKB1
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
ENDFORM.                    " FRM_SEARCH_HELP_HKONT
*&---------------------------------------------------------------------*
*&      Form  FRM_FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FETCH_DATA .
  DATA WA_COLOR TYPE LVC_S_SCOL.  "颜色设置
  CLEAR:WA_BSIK,IT_BSIK.
  CLEAR: T_LIFNR.
  SELECT LIFNR FROM LFA1 INTO TABLE T_LIFNR.
  "1 获取BSIK过帐日期小于开始时间的期初数据到it_bsik
 "   期初余额 = 期初未清余额 = 期初过账截至目前未清 + 期初过账未清截至目前已清= BSIK（限定过账日期）  + BSAK（限定过账日期和清账日期）
*取BSIK数据
  IF S_MONAT-LOW >= SY-DATUM.
    SELECT BUKRS
           HKONT              "总账科目
           LIFNR              "供应商编号
           AUGBL
           BELNR
           BUZEI
           GJAHR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSIK
      INTO TABLE IT_BSIK
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT < S_MONAT-LOW
    AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*  and   umskz <> 'W'.
  ELSE.
    SELECT BUKRS
           HKONT              "总账科目
           LIFNR              "供应商编号
           AUGBL
           BELNR
           BUZEI
           GJAHR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSIK
      INTO TABLE IT_BSIK
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT < S_MONAT-LOW
      AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*    and umskz <> 'W'.
  ENDIF.

  SELECT A~LIFNR A~EKORG A~EKGRP  B~EKNAM INTO CORRESPONDING FIELDS OF TABLE T_LFM1
    FROM LFM1 AS A
    LEFT JOIN T024 AS B
    ON A~EKGRP = B~EKGRP
  WHERE A~EKORG IN P_BUKRS AND LIFNR IN S_LIFNR ." AND  A~EKGRP IN S_EKGRP.
  SORT T_LFM1 BY  LIFNR EKORG EKGRP .

  "1.1 获取BSEG过帐日期小于开始时间的期初数据到it_bsis
  IF P_ZJB EQ 'X'.
    "1.1.1 遍历ZFI025配置表字段、科目内容

    "取BKPF表
    CLEAR IT_BKPF[].
    SELECT BUKRS           "公司代码
           BELNR           "凭证编号
           GJAHR           "会计年度
           BLART           "凭证类型
           BUDAT           "过账日期
           WAERS           "货币码
    INTO TABLE IT_BKPF
    FROM BKPF
    WHERE BUKRS IN P_BUKRS
    AND   BUDAT < S_MONAT-LOW.
   SORT IT_BKPF BY BUKRS  BELNR GJAHR .
    SELECT * FROM ZFI025 INTO CORRESPONDING FIELDS OF W_ZFI015 WHERE BUKRS IN P_BUKRS.
      CLEAR IT_BSIS[].
      "1.1.2 设置科目Range
      RANGES R_HKONT FOR BSIS-HKONT.
      CLEAR R_HKONT[].
      R_HKONT-SIGN = W_ZFI015-SIGN.
      R_HKONT-OPTION = W_ZFI015-OPTI.
      R_HKONT-LOW = W_ZFI015-LOW.
      R_HKONT-HIGH = W_ZFI015-HIGH.
      APPEND R_HKONT.

      CLEAR STA_SELECT.
      CLEAR LIFNR_FIELD.
      CLEAR STA_WHERE.
      "1.1.3 设置动态选择字段
      CONCATENATE W_ZFI015-FIELDNAME 'AS LIFNR,' INTO LIFNR_FIELD SEPARATED BY SPACE.

      CLEAR STA_SELECT[].

      "1.1.3 设置SELECT字段清单
      PERFORM ADD_COMP USING:
            'BUKRS,', "总账科目
            'HKONT,', "科目编号
            LIFNR_FIELD, "供应商编号
            'AUGBL,', "清算单据的单据号码
            'BELNR,', "凭证号
            'BUZEI,', "凭证行号
            'GJAHR,', "年度
*            'WAERS,', "货币码
            'DMBTR,', "本位币金额
            'WRBTR,', "凭证金额
            'ZUMSK,', "特别总账标识
            'SHKZG'.  "借贷标识

      "1.1.4 设置动态WHERE语句
      CONCATENATE 'BUKRS EQ @W_ZFI015-BUKRS'
      'AND HKONT IN @S_HKONT'
      'AND HKONT IN @R_HKONT'
      'AND BUKRS EQ @IT_BKPF-BUKRS'
      'AND BELNR EQ @IT_BKPF-BELNR'
      'AND GJAHR EQ @IT_BKPF-GJAHR'
      'AND UMSKZ IN @S_UMSKZ'
*      W_ZFI015-FIELDNAME 'IN @S_LIFNR AND'
*      W_ZFI015-FIELDNAME 'NE @SPACE'
      INTO STA_WHERE SEPARATED BY SPACE.

      "1.1.5 执行动态SQL语句
      IF IT_BKPF[] IS NOT INITIAL.
        SELECT (STA_SELECT) INTO CORRESPONDING FIELDS OF TABLE @IT_BSIS
          FROM BSEG
          FOR ALL ENTRIES IN @IT_BKPF
        WHERE (STA_WHERE).
        PERFORM LIFNR_INPUT TABLES IT_BSIS.
        IF W_ZFI015-WHOLE IS INITIAL."供应商单行匹配
          DELETE IT_BSIS WHERE LIFNR NOT IN S_LIFNR.
        ELSE."供应商多行匹配
          PERFORM LIFNR_WHOLE.
        ENDIF.
        LOOP AT IT_BSIS INTO WA_BSIS.
          READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSIS-BUKRS BELNR = WA_BSIS-BELNR GJAHR = WA_BSIS-GJAHR BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            WA_BSIS-WAERS = WA_BKPF-WAERS.
            MODIFY IT_BSIS FROM WA_BSIS.
          ENDIF.
        ENDLOOP.
      ENDIF.
      PERFORM LIFNR_INPUT TABLES IT_BSIS.
      APPEND LINES OF IT_BSIS TO IT_BSIK.
    ENDSELECT.
    SORT IT_BSIK BY  GJAHR BUKRS BELNR BUZEI ASCENDING LIFNR DESCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_BSIK COMPARING  GJAHR BUKRS BELNR BUZEI.
  ENDIF.

  "2 获取BSAK过账日期小于开始时间， 清账日期 大于等于选择屏幕开始日期                          "清帐日期在期间内的数据到it_bsik
*取BSAK数据
 CLEAR :IT_BSIK02 .
  SELECT BUKRS
         HKONT              "总账科目
         LIFNR              "供应商编号
         AUGBL
         BELNR
         BUZEI
         GJAHR
         WAERS              "货币码
         DMBTR              "本位币金额
         WRBTR              "凭证金额
         ZUMSK              "特别总账标识
         SHKZG              "借贷标识
  FROM BSAK
    APPENDING TABLE IT_BSIK02
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    "AND   AUGDT < SY-DATUM
    AND   AUGDT >= S_MONAT-LOW
    AND   BUDAT < S_MONAT-LOW
    AND   LIFNR IN S_LIFNR
  AND   UMSKZ IN S_UMSKZ.

*    and umskz <> 'W'.


  LOOP AT IT_BSIK02 INTO WA_BSIK02.         "清账凭证号不等于凭证编号
    IF WA_BSIK02-AUGBL = WA_BSIK02-BELNR.
      DELETE TABLE IT_BSIK02 FROM WA_BSIK02.
    ENDIF.
  ENDLOOP.

APPEND LINES OF IT_BSIK02 TO IT_BSIK .


  "3 按公司代码、科目、供应商、货币码汇总it_bsik期初数据到it_sum
*处理汇总借贷
  LOOP AT IT_BSIK INTO WA_BSIK.
    WA_SUM-BUKRS = WA_BSIK-BUKRS.
    WA_SUM-HKONT = WA_BSIK-HKONT.
    WA_SUM-LIFNR = WA_BSIK-LIFNR.
    WA_SUM-WAERS = WA_BSIK-WAERS.
    IF WA_BSIK-SHKZG = 'H'.
      WA_SUM-BEG_DBTR = WA_BSIK-DMBTR * -1.
      WA_SUM-BEG_WBTR = WA_BSIK-WRBTR * -1.
    ELSE.
      WA_SUM-BEG_DBTR = WA_BSIK-DMBTR.
      WA_SUM-BEG_WBTR = WA_BSIK-WRBTR.
    ENDIF.
    COLLECT WA_SUM INTO IT_SUM.
    CLEAR:WA_SUM,WA_BSIK.
  ENDLOOP.
  MOVE-CORRESPONDING IT_SUM TO IT_NAME.
  CLEAR:IT_SUM.

  "4 获取BSAK/BSIK过账日期在期间内的数据到it_bsik
*  取BSIK/BSAK中budat属于选择画面的所有行项目的值
  IF S_MONAT-HIGH IS INITIAL.
    SELECT  BUKRS
         HKONT              "总账科目
         LIFNR              "供应商编号
         AUGBL
         BELNR
         BUZEI
         GJAHR
         WAERS              "货币码
         DMBTR              "本位币金额
         WRBTR              "凭证金额
         ZUMSK              "特别总账标识
         SHKZG              "借贷标识
  FROM BSIK
    APPENDING TABLE IT_BSIK
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT = S_MONAT-LOW
    AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*    and umskz <> 'W'.

    SELECT BUKRS
           HKONT              "总账科目
           LIFNR              "供应商编号
           AUGBL
           BELNR
           BUZEI
           GJAHR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSAK
      APPENDING TABLE IT_BSIK
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT = S_MONAT-LOW
      AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*    and umskz <> 'W'.
  ELSE.
    SELECT  BUKRS
         HKONT              "总账科目
         LIFNR              "供应商编号
         AUGBL
         BELNR
         BUZEI
         GJAHR
         WAERS              "货币码
         DMBTR              "本位币金额
         WRBTR              "凭证金额
         ZUMSK              "特别总账标识
         SHKZG              "借贷标识
  FROM BSIK
    APPENDING TABLE IT_BSIK
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT <= S_MONAT-HIGH
    AND   BUDAT >= S_MONAT-LOW
    AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*    and umskz <> 'W'.

    SELECT BUKRS
           HKONT              "总账科目
           LIFNR              "供应商编号
           AUGBL
           BELNR
           BUZEI
           GJAHR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSAK
      APPENDING TABLE IT_BSIK
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT <= S_MONAT-HIGH
      AND   BUDAT >= S_MONAT-LOW
      AND   LIFNR IN S_LIFNR
    AND   UMSKZ IN S_UMSKZ.
*    and umskz <> 'W'.
  ENDIF.

  "4.1 获取自建表过账日期在期间内的数据到it_bsis
  IF P_ZJB EQ 'X'.
    "4.1.1 遍历ZFI025配置表字段、科目内容
    "取BKPF表
    CLEAR IT_BKPF[].
    SELECT BUKRS           "公司代码
           BELNR           "凭证编号
           GJAHR           "会计年度
           BLART           "凭证类型
           BUDAT           "过账日期
           WAERS           "货币码
    INTO TABLE IT_BKPF
    FROM BKPF
    WHERE BUKRS IN P_BUKRS
    AND   BUDAT IN S_MONAT.
    SORT IT_BKPF BY BUKRS BELNR GJAHR.
    SELECT * FROM ZFI025 INTO CORRESPONDING FIELDS OF W_ZFI015 WHERE BUKRS IN P_BUKRS.

      CLEAR IT_BSIS[].
      "4.1.2 设置科目Range
      CLEAR R_HKONT[].
      R_HKONT-SIGN = W_ZFI015-SIGN.
      R_HKONT-OPTION = W_ZFI015-OPTI.
      R_HKONT-LOW = W_ZFI015-LOW.
      R_HKONT-HIGH = W_ZFI015-HIGH.
      APPEND R_HKONT.

      CLEAR STA_SELECT.
      CLEAR LIFNR_FIELD.
      CLEAR STA_WHERE.

      "4.1.3 设置动态选择字段
      CONCATENATE W_ZFI015-FIELDNAME 'AS LIFNR,' INTO LIFNR_FIELD SEPARATED BY SPACE.

      CLEAR STA_SELECT[].

      "4.1.3 设置SELECT字段清单
      PERFORM ADD_COMP USING:
            'BUKRS,', "总账科目
            'HKONT,', "科目编号
            LIFNR_FIELD, "供应商编号
            'AUGBL,', "清算单据的单据号码
            'BELNR,', "凭证号
            'BUZEI,', "凭证行号
            'GJAHR,', "年度
*            'WAERS,', "货币码
            'DMBTR,', "本位币金额
            'WRBTR,', "凭证金额
            'ZUMSK,', "特别总账标识
            'SHKZG'.  "借贷标识

      "4.1.4 设置动态WHERE语句
      CONCATENATE 'BUKRS EQ @W_ZFI015-BUKRS'
      'AND HKONT IN @S_HKONT'
      'AND BUKRS EQ @IT_BKPF-BUKRS'
      'AND BELNR EQ @IT_BKPF-BELNR'
      'AND GJAHR EQ @IT_BKPF-GJAHR'
      'AND HKONT IN @R_HKONT'
      'AND UMSKZ IN @S_UMSKZ'
*      W_ZFI015-FIELDNAME 'IN @S_LIFNR AND'
*      W_ZFI015-FIELDNAME 'NE @SPACE'
      INTO STA_WHERE SEPARATED BY SPACE.

      "4.1.5 执行动态SQL语句
      IF IT_BKPF[] IS NOT INITIAL.
        SELECT (STA_SELECT) INTO CORRESPONDING FIELDS OF TABLE @IT_BSIS
          FROM BSEG
          FOR ALL ENTRIES IN @IT_BKPF
        WHERE (STA_WHERE).
        PERFORM LIFNR_INPUT TABLES IT_BSIS.
        IF W_ZFI015-WHOLE IS INITIAL."供应商单行匹配
          DELETE IT_BSIS WHERE LIFNR NOT IN S_LIFNR.
        ELSE."供应商多行匹配
          PERFORM LIFNR_WHOLE.
        ENDIF.
        LOOP AT IT_BSIS INTO WA_BSIS.
          READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSIS-BUKRS BELNR = WA_BSIS-BELNR GJAHR = WA_BSIS-GJAHR BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            WA_BSIS-WAERS = WA_BKPF-WAERS.
            MODIFY IT_BSIS FROM WA_BSIS.
          ENDIF.
        ENDLOOP.
      ENDIF.
      PERFORM LIFNR_INPUT TABLES IT_BSIS.
      APPEND LINES OF IT_BSIS TO IT_BSIK.

    ENDSELECT.

    SORT IT_BSIK BY GJAHR BUKRS BELNR BUZEI ASCENDING LIFNR DESCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_BSIK COMPARING  GJAHR BUKRS BELNR BUZEI.
  ENDIF.
  "5 根据关键字段BUKRS HKONT LIFNR WAERS添加IT_BSIK内表数据到最终ALV输出内表IT_NAME.
  SORT IT_BSIK BY  BUKRS HKONT LIFNR WAERS.
  DELETE ADJACENT DUPLICATES FROM IT_BSIK COMPARING BUKRS HKONT LIFNR WAERS.

  SORT IT_NAME BY BUKRS HKONT LIFNR WAERS.
  LOOP AT IT_BSIK INTO WA_BSIK.
    READ TABLE IT_NAME INTO WA_NAME WITH KEY BUKRS = WA_BSIK-BUKRS
                                             HKONT = WA_BSIK-HKONT
                                             LIFNR = WA_BSIK-LIFNR
                                             WAERS = WA_BSIK-WAERS.
    IF SY-SUBRC = 0.
    ELSE.
      WA_NAME-BUKRS = WA_BSIK-BUKRS.
      WA_NAME-HKONT = WA_BSIK-HKONT.
      WA_NAME-LIFNR = WA_BSIK-LIFNR.
      WA_NAME-WAERS = WA_BSIK-WAERS.
      APPEND WA_NAME TO IT_NAME.
    ENDIF.
    CLEAR:WA_NAME,WA_BSIK.
  ENDLOOP.

*取科目描述
  SELECT SAKNR TXT50
    INTO TABLE IT_SKAT
    FROM SKAT
    WHERE KTOPL = '1000'
  AND SPRAS = '1'.
  SORT IT_SKAT BY SAKNR.


  LOOP AT IT_NAME INTO WA_NAME.
    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_NAME-HKONT BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_NAME-TXT50 = WA_SKAT-TXT50.
      MODIFY IT_NAME FROM WA_NAME.
    ENDIF.
  ENDLOOP.
  CLEAR: IT_SKAT.
*
**取供应商描述
  SELECT LIFNR NAME1
    INTO TABLE IT_LFA1
  FROM LFA1.
  SORT IT_LFA1 BY LIFNR.

  LOOP AT IT_NAME INTO WA_NAME.
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_NAME-LIFNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_NAME-NAME1 = WA_LFA1-NAME1.
      MODIFY IT_NAME FROM WA_NAME.
    ENDIF.
  ENDLOOP.
  CLEAR:IT_LFA1.
  "6 获取BKPF/BSEG过帐日期在范围内的会计凭证数据到IT_BKPF/IT_BSEG
**取BKPF表
  SELECT BUKRS           "公司代码
         BELNR           "凭证编号
         GJAHR           "会计年度
         BLART           "凭证类型
         BUDAT           "过账日期
         WAERS           "货币码
  INTO TABLE IT_BKPF
    FROM BKPF
  WHERE BUKRS IN P_BUKRS
  AND   BUDAT IN S_MONAT.
*
**通过BKPF、科目号、客户号取BSEG数据
  IF IT_BKPF IS NOT INITIAL.
    SELECT BUKRS
           BELNR
           GJAHR
           BUZEI
           SHKZG           "借贷标识
           DMBTR           "本位币金额
           WRBTR           "交易币金额
           SGTXT           "摘要
           HKONT           "科目号
           LIFNR           "客户号
           XNEGP
           ZUONR
           VBEL2
           ZTERM
           VBELN
           ZLSCH
           ZFBDT
           UMSKZ
    INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
    FROM BSEG
    FOR ALL ENTRIES IN IT_BKPF
    WHERE BUKRS = IT_BKPF-BUKRS
    AND   BELNR = IT_BKPF-BELNR
    AND   GJAHR = IT_BKPF-GJAHR
    AND   UMSKZ IN S_UMSKZ.
*   AND   UMSKZ <> 'W'.
    "6.1 获取自建表过账日期在期间内的数据到it_bsis
    IF P_ZJB EQ 'X'.
      "6.1.1 遍历ZFI025配置表字段、科目内容
      SELECT * FROM ZFI025 INTO CORRESPONDING FIELDS OF W_ZFI015 WHERE BUKRS IN P_BUKRS.

        CLEAR IT_BSEG_ZJB[].
        "6.1.2 设置科目Range
        CLEAR R_HKONT[].
        R_HKONT-SIGN = W_ZFI015-SIGN.
        R_HKONT-OPTION = W_ZFI015-OPTI.
        R_HKONT-LOW = W_ZFI015-LOW.
        R_HKONT-HIGH = W_ZFI015-HIGH.
        APPEND R_HKONT.

        CLEAR STA_SELECT.
        CLEAR LIFNR_FIELD.
        CLEAR STA_WHERE.
        "6.1.3 设置动态选择字段
        CONCATENATE W_ZFI015-FIELDNAME 'AS LIFNR,' INTO LIFNR_FIELD SEPARATED BY SPACE.

        CLEAR STA_SELECT[].

        "6.1.3 设置SELECT字段清单
        PERFORM ADD_COMP USING:
              'BUKRS,', "总账科目
              'HKONT,', "科目编号
              'GJAHR,', "年度
              'BUZEI,', "行项目号
              'SGTXT,', "摘要
              LIFNR_FIELD, "供应商编号
              'BELNR,', "凭证号
              'DMBTR,', "本位币金额
              'WRBTR,', "凭证金额
              'XNEGP,', "
              'VBEL2,', "
              'ZTERM,', "
              'VBELN,', "
              'ZLSCH,', "
              'ZFBDT,', "
              'UMSKZ,', "
              'SHKZG'.  "借贷标识

        "6.1.4 设置动态WHERE语句
        CONCATENATE 'BUKRS = @IT_BKPF-BUKRS'
        'AND BUKRS EQ @W_ZFI015-BUKRS'
        'AND HKONT IN @R_HKONT'
        'AND BELNR = @IT_BKPF-BELNR'
        'AND GJAHR = @IT_BKPF-GJAHR'
        'AND UMSKZ IN @S_UMSKZ'
*      W_ZFI015-FIELDNAME 'NE @SPACE'
        INTO STA_WHERE SEPARATED BY SPACE.

        "6.1.5 执行动态SQL语句
        SELECT (STA_SELECT) INTO CORRESPONDING FIELDS OF TABLE @IT_BSEG_ZJB
          FROM BSEG
          FOR ALL ENTRIES IN @IT_BKPF
        WHERE (STA_WHERE).

        PERFORM LIFNR_INPUT TABLES IT_BSEG_ZJB.
        IF W_ZFI015-WHOLE IS INITIAL."供应商单行匹配
          DELETE IT_BSEG_ZJB WHERE LIFNR NOT IN S_LIFNR.
        ELSE."供应商多行匹配
          PERFORM LIFNR_WHOLE_BSEG.
        ENDIF.
        APPEND LINES OF IT_BSEG_ZJB TO IT_BSEG.

      ENDSELECT.

      SORT IT_BSEG BY GJAHR BUKRS BELNR BUZEI ASCENDING LIFNR DESCENDING.
      DELETE ADJACENT DUPLICATES FROM IT_BSEG COMPARING GJAHR BUKRS BELNR BUZEI.
    ENDIF.
  ENDIF.
*
  "7 合并IT_BKPF/IT_BSEG的数据到IT_BSEG，设置货币码,过账日期
**凭证明细取货币码,过账日期
  SORT IT_BKPF BY BUKRS BELNR GJAHR.
  LOOP AT IT_BSEG INTO WA_BSEG.
    READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSEG-BUKRS
                                             BELNR = WA_BSEG-BELNR
                                             GJAHR = WA_BSEG-GJAHR
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_BSEG-BLART = WA_BKPF-BLART.
      WA_BSEG-WAERS = WA_BKPF-WAERS.
      WA_BSEG-BUDAT = WA_BKPF-BUDAT.
    ENDIF.
*************************************************************
    IF WA_BSEG-XNEGP = 'X' AND WA_BSEG-SHKZG = 'S'.
      WA_BSEG-SHKZG = 'H'.
      WA_BSEG-WRBTR = WA_BSEG-WRBTR * -1.
      WA_BSEG-DMBTR = WA_BSEG-DMBTR * -1.
    ELSEIF WA_BSEG-XNEGP = 'X' AND WA_BSEG-SHKZG = 'H'.
      WA_BSEG-SHKZG = 'S'.
      WA_BSEG-WRBTR = WA_BSEG-WRBTR * -1.
      WA_BSEG-DMBTR = WA_BSEG-DMBTR * -1.
    ENDIF.
************************************************************
    MODIFY IT_BSEG FROM WA_BSEG TRANSPORTING BLART WAERS BUDAT SHKZG WRBTR DMBTR.
    CLEAR:WA_BSEG,WA_BKPF.
  ENDLOOP.
*
  "8 汇总IT_BSEG的本期发生数据到IT_SUM
**本期发生额求和
  CLEAR: WA_SUM,IT_SUM.
  LOOP AT IT_BSEG INTO WA_BSEG.
    WA_SUM-BUKRS = WA_BSEG-BUKRS.
    WA_SUM-HKONT = WA_BSEG-HKONT.
    WA_SUM-LIFNR = WA_BSEG-LIFNR.
    WA_SUM-WAERS = WA_BSEG-WAERS.
    IF WA_BSEG-SHKZG = 'S'.
      WA_SUM-WRBTR_S = WA_BSEG-WRBTR.
      WA_SUM-DMBTR_S = WA_BSEG-DMBTR.
    ELSE.
      WA_SUM-WRBTR_H = WA_BSEG-WRBTR.
      WA_SUM-DMBTR_H = WA_BSEG-DMBTR.
    ENDIF.
    COLLECT WA_SUM INTO IT_SUM.
    CLEAR: WA_SUM,WA_BSEG.
  ENDLOOP.

  "9 添加IT_SUM本期发生数据到输出内表
*添加到输出内表
  SORT IT_SUM BY BUKRS HKONT LIFNR WAERS.
  LOOP AT IT_NAME INTO WA_NAME.
    READ TABLE IT_SUM INTO WA_SUM WITH KEY BUKRS = WA_NAME-BUKRS
                                           HKONT = WA_NAME-HKONT
                                           LIFNR = WA_NAME-LIFNR
                                           WAERS = WA_NAME-WAERS
                                           BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_NAME-WBTR_S = WA_SUM-WRBTR_S.
      WA_NAME-DBTR_S = WA_SUM-DMBTR_S.
      WA_NAME-WBTR_H = WA_SUM-WRBTR_H.
      WA_NAME-DBTR_H = WA_SUM-DMBTR_H.
      MODIFY IT_NAME FROM WA_NAME.
    ENDIF.
    CLEAR:WA_NAME,WA_SUM.
  ENDLOOP.

***************************************
*  处理以本位币显示时，不以货币汇总逻辑
  IF P_ALL = ''.
    CLEAR:IT_NAME1,WA_NAME1.
    LOOP AT IT_NAME INTO WA_NAME.
      WA_NAME1-BUKRS = WA_NAME-BUKRS.
      WA_NAME1-HKONT = WA_NAME-HKONT.
      WA_NAME1-TXT50 = WA_NAME-TXT50.
      WA_NAME1-LIFNR = WA_NAME-LIFNR.
      WA_NAME1-NAME1 = WA_NAME-NAME1.
      WA_NAME1-BEG_WBTR = WA_NAME-BEG_WBTR.
      WA_NAME1-BEG_DBTR = WA_NAME-BEG_DBTR.
      WA_NAME1-WBTR_S = WA_NAME-WBTR_S.
      WA_NAME1-WBTR_H = WA_NAME-WBTR_H.
      WA_NAME1-DBTR_S = WA_NAME-DBTR_S.
      WA_NAME1-DBTR_H = WA_NAME-DBTR_H.
      COLLECT WA_NAME1 INTO IT_NAME1.
    ENDLOOP.
    CLEAR:IT_NAME.
    MOVE-CORRESPONDING IT_NAME1 TO IT_NAME.
  ENDIF.
***************************************
  DATA L1_TABIX TYPE SY-TABIX.
  CLEAR L1_TABIX.
  LOOP AT IT_NAME INTO WA_NAME.
    L1_TABIX = SY-TABIX .
    "添加采购组 及过滤 BEGIN IT02 150804
    " IF S_EKGRP[] IS NOT INITIAL.
    READ TABLE T_LFM1 WITH KEY LIFNR = WA_NAME-LIFNR EKORG = WA_NAME-BUKRS.
    IF SY-SUBRC = 0.
      WA_NAME-EKGRP = T_LFM1-EKGRP.
      WA_NAME-EKNAM = T_LFM1-EKNAM.
    ELSE.
      "  DELETE IT_NAME INDEX L1_TABIX .
      "   CONTINUE.
    ENDIF.
    " ENDIF.
    "添加采购组 及过滤 END IT02 150804
**    期末余额，本位币，交易货币
*     wa_name-shkzg = '贷'.
    WA_NAME-END_WBTR = WA_NAME-BEG_WBTR - WA_NAME-WBTR_H + WA_NAME-WBTR_S.
    WA_NAME-END_DBTR = WA_NAME-BEG_DBTR - WA_NAME-DBTR_H + WA_NAME-DBTR_S.
    IF WA_NAME-END_WBTR > 0 OR WA_NAME-END_DBTR > 0.
      WA_NAME-SHKZG = '借'.
    ELSEIF WA_NAME-END_WBTR < 0 OR WA_NAME-END_DBTR < 0.
      WA_NAME-SHKZG = '贷'.
    ELSE.
      WA_NAME-SHKZG = '平'.
    ENDIF.
**   颜色设置
    IF WA_NAME-WBTR_S < 0.
      WA_COLOR-FNAME = 'WBTR_S'.
      WA_COLOR-COLOR-COL  = '6'.
      WA_COLOR-COLOR-INT    = '1'.
      WA_COLOR-COLOR-INV    = '0'.
      APPEND WA_COLOR TO WA_NAME-CELLCOLOR.
      CLEAR: WA_COLOR.
    ENDIF.
    IF WA_NAME-WBTR_H < 0.
      WA_COLOR-FNAME = 'WBTR_H'.
      WA_COLOR-COLOR-COL  = '6'.
      WA_COLOR-COLOR-INT    = '1'.
      WA_COLOR-COLOR-INV    = '0'.
      APPEND WA_COLOR TO WA_NAME-CELLCOLOR.
      CLEAR: WA_COLOR.
    ENDIF.

    IF WA_NAME-DBTR_S < 0.
      WA_COLOR-FNAME = 'DBTR_S'.
      WA_COLOR-COLOR-COL  = '6'.
      WA_COLOR-COLOR-INT    = '1'.
      WA_COLOR-COLOR-INV    = '0'.
      APPEND WA_COLOR TO WA_NAME-CELLCOLOR.
      CLEAR: WA_COLOR.
    ENDIF.

    IF WA_NAME-DBTR_H < 0.
      WA_COLOR-FNAME = 'DBTR_H'.
      WA_COLOR-COLOR-COL  = '6'.
      WA_COLOR-COLOR-INT    = '1'.
      WA_COLOR-COLOR-INV    = '0'.
      APPEND WA_COLOR TO WA_NAME-CELLCOLOR.
      CLEAR: WA_COLOR.
    ENDIF.
    MODIFY IT_NAME FROM WA_NAME.
    CLEAR:WA_NAME.
  ENDLOOP.

*  输出内表排序
  SORT IT_NAME BY BUKRS HKONT LIFNR WAERS.

ENDFORM.                    " FRM_FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_EVENT .

ENDFORM.                    " FRM_GET_EVENT

FORM F_USER_COMMAND USING P_UCOMM LIKE SY-UCOMM
                          P_RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: LW_NAME LIKE LINE OF IT_NAME.
  CASE P_UCOMM.
      WHEN '&IC1'.
      READ TABLE IT_NAME INTO LW_NAME INDEX P_RS_SELFIELD-TABINDEX.
      LW_NAME-BEG_DBTR = LW_NAME-BEG_DBTR * -1.
      LW_NAME-BEG_WBTR = LW_NAME-BEG_WBTR * -1.
      PERFORM FRM_DETAIL_ALV USING LW_NAME.
      CLEAR LW_NAME.
      WHEN OTHERS.
  ENDCASE.
ENDFORM.

FORM F_USER_COMMAND_DETAIL USING P_UCOMM LIKE SY-UCOMM
                          P_RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE P_UCOMM.
      WHEN '&IC1'.
      CLEAR:WA_DETAIL.
      READ TABLE IT_DETAIL INTO WA_DETAIL INDEX P_RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        SET PARAMETER ID 'GJR' FIELD WA_DETAIL-GJAHR.
        SET PARAMETER ID 'BUK' FIELD WA_DETAIL-BUKRS.
        SET PARAMETER ID 'BLN' FIELD WA_DETAIL-BELNR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.

FORM FRM_DETAIL_ALV  USING  FU_LW_NAME TYPE TY_NAME.
  DATA WA_COLOR TYPE LVC_S_SCOL.  "颜色设置
  CLEAR:IT_DETAIL,WA_DETAIL.
  CLEAR:GT_FIELDCAT_DETAIL[].
  IF P_ALL = 'X'.
    LOOP AT IT_BSEG INTO WA_BSEG.
      IF WA_BSEG-BUKRS = FU_LW_NAME-BUKRS
         AND  WA_BSEG-HKONT = FU_LW_NAME-HKONT
         AND  WA_BSEG-LIFNR = FU_LW_NAME-LIFNR
         AND  WA_BSEG-WAERS = FU_LW_NAME-WAERS.
        WA_DETAIL-BUKRS = WA_BSEG-BUKRS.
        WA_DETAIL-GJAHR = WA_BSEG-GJAHR.
        WA_DETAIL-BLART = WA_BSEG-BLART.
        WA_DETAIL-HKONT = FU_LW_NAME-HKONT.
        WA_DETAIL-TXT50 = FU_LW_NAME-TXT50.
        WA_DETAIL-LIFNR = FU_LW_NAME-LIFNR.
        WA_DETAIL-NAME1 = FU_LW_NAME-NAME1.
        WA_DETAIL-WAERS = WA_BSEG-WAERS.
        WA_DETAIL-BELNR = WA_BSEG-BELNR.
        WA_DETAIL-BUZEI = WA_BSEG-BUZEI.
        WA_DETAIL-BUDAT = WA_BSEG-BUDAT.
        WA_DETAIL-UMSKZ = WA_BSEG-UMSKZ.
        IF P_ALL = ''.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'..
            WA_DETAIL-WRBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ELSEIF P_ALL = 'X'.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'..
            WA_DETAIL-WRBTR_H = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ENDIF.

        WA_DETAIL-SHKZG = '贷'.
*            wa_detail-wrbtr_t = fu_lw_name-beg_wbtr + wa_detail-wrbtr_s - wa_detail-wrbtr_h .
        WA_DETAIL-SGTXT = WA_BSEG-SGTXT.
        IF WA_BSEG-SHKZG = 'S' .
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR * -1.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR * -1.
        ELSEIF WA_BSEG-SHKZG = 'H'.
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR.
        ENDIF.
        WA_DETAIL-ZUONR = WA_BSEG-ZUONR.
        WA_DETAIL-VBEL2 = WA_BSEG-VBEL2.
        WA_DETAIL-ZTERM = WA_BSEG-ZTERM.
        WA_DETAIL-VBELN = WA_BSEG-VBELN.
        WA_DETAIL-ZLSCH = WA_BSEG-ZLSCH.
        WA_DETAIL-ZFBDT = WA_BSEG-ZFBDT.

        APPEND WA_DETAIL TO IT_DETAIL.
      ENDIF.
      CLEAR:WA_BSEG,WA_DETAIL.
    ENDLOOP.
    SORT IT_DETAIL BY BUDAT.
    LOOP AT IT_DETAIL INTO WA_DETAIL.
      READ TABLE T_LFM1 WITH KEY LIFNR = WA_DETAIL-LIFNR . "添加采购组名
      IF SY-SUBRC = 0 .
        WA_DETAIL-EKGRP = T_LFM1-EKGRP."添加采购组
        WA_DETAIL-EKNAM = T_LFM1-EKNAM."添加采购组名
      ENDIF.
      CLEAR:WA_COLOR.
*       设置颜色
      IF WA_DETAIL-WRBTR_S < 0.
        WA_COLOR-FNAME = 'WRBTR_S'.
        WA_COLOR-COLOR-COL  = '6'.
        WA_COLOR-COLOR-INT    = '1'.
        WA_COLOR-COLOR-INV    = '0'.
        APPEND WA_COLOR TO WA_DETAIL-CELLCOLOR.
      ENDIF.
      IF WA_DETAIL-WRBTR_H < 0.
        WA_COLOR-FNAME = 'WRBTR_H'.
        WA_COLOR-COLOR-COL  = '6'.
        WA_COLOR-COLOR-INT    = '1'.
        WA_COLOR-COLOR-INV    = '0'.
        APPEND WA_COLOR TO WA_DETAIL-CELLCOLOR.
      ENDIF.

*       计算余额
      IF P_ALL = ''.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_DBTR - WA_DETAIL-WRBTR_S + WA_DETAIL-WRBTR_H .
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-WRBTR_T.
      ELSEIF P_ALL = 'X'.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_WBTR - WA_DETAIL-WRBTR_S + WA_DETAIL-WRBTR_H .
        WA_DETAIL-DMBTR_T = FU_LW_NAME-BEG_DBTR - WA_DETAIL-DMBTR_S + WA_DETAIL-DMBTR_H.
        FU_LW_NAME-BEG_WBTR = WA_DETAIL-WRBTR_T.
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-DMBTR_T.
      ENDIF.

      MODIFY IT_DETAIL FROM WA_DETAIL TRANSPORTING WRBTR_T DMBTR_T EKGRP EKNAM  CELLCOLOR.
      CLEAR:WA_DETAIL.
    ENDLOOP.
  ELSE.
    LOOP AT IT_BSEG INTO WA_BSEG.
      IF WA_BSEG-BUKRS = FU_LW_NAME-BUKRS
         AND  WA_BSEG-HKONT = FU_LW_NAME-HKONT
         AND  WA_BSEG-LIFNR = FU_LW_NAME-LIFNR.
*             and  wa_bseg-waers = fu_lw_name-waers.
        WA_DETAIL-BUKRS = WA_BSEG-BUKRS.
        WA_DETAIL-GJAHR = WA_BSEG-GJAHR.
        WA_DETAIL-BLART = WA_BSEG-BLART.
        WA_DETAIL-HKONT = FU_LW_NAME-HKONT.
        WA_DETAIL-TXT50 = FU_LW_NAME-TXT50.
        WA_DETAIL-LIFNR = FU_LW_NAME-LIFNR.
        WA_DETAIL-NAME1 = FU_LW_NAME-NAME1.
        WA_DETAIL-WAERS = WA_BSEG-WAERS.
        WA_DETAIL-BELNR = WA_BSEG-BELNR.
        WA_DETAIL-BUZEI = WA_BSEG-BUZEI.
        WA_DETAIL-BUDAT = WA_BSEG-BUDAT.
        WA_DETAIL-UMSKZ = WA_BSEG-UMSKZ.
        IF P_ALL = ''.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'..
            WA_DETAIL-WRBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ELSEIF P_ALL = 'X'.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'..
            WA_DETAIL-WRBTR_H = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ENDIF.

        WA_DETAIL-SHKZG = '贷'.
*            wa_detail-wrbtr_t = fu_lw_name-beg_wbtr + wa_detail-wrbtr_s - wa_detail-wrbtr_h .
        WA_DETAIL-SGTXT = WA_BSEG-SGTXT.
        IF WA_BSEG-SHKZG = 'S' .
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR * -1.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR * -1.
        ELSEIF WA_BSEG-SHKZG = 'H'.
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR.
        ENDIF.
        WA_DETAIL-ZUONR = WA_BSEG-ZUONR.
        WA_DETAIL-VBEL2 = WA_BSEG-VBEL2.
        WA_DETAIL-ZTERM = WA_BSEG-ZTERM.
        WA_DETAIL-VBELN = WA_BSEG-VBELN.
        WA_DETAIL-ZLSCH = WA_BSEG-ZLSCH.
        WA_DETAIL-ZFBDT = WA_BSEG-ZFBDT.
        APPEND WA_DETAIL TO IT_DETAIL.
      ENDIF.
      CLEAR:WA_BSEG,WA_DETAIL.
    ENDLOOP.
    SORT IT_DETAIL BY BUDAT.
    LOOP AT IT_DETAIL INTO WA_DETAIL.
      READ TABLE T_LFM1 WITH KEY LIFNR = WA_DETAIL-LIFNR . "添加采购组名
      IF SY-SUBRC = 0 .
        WA_DETAIL-EKGRP = T_LFM1-EKGRP. "添加采购组
        WA_DETAIL-EKNAM = T_LFM1-EKNAM.  "添加采购组名
      ENDIF.
      CLEAR:WA_COLOR.
*       设置颜色
      IF WA_DETAIL-WRBTR_S < 0.
        WA_COLOR-FNAME = 'WRBTR_S'.
        WA_COLOR-COLOR-COL  = '6'.
        WA_COLOR-COLOR-INT    = '1'.
        WA_COLOR-COLOR-INV    = '0'.
        APPEND WA_COLOR TO WA_DETAIL-CELLCOLOR.
      ENDIF.
      IF WA_DETAIL-WRBTR_H < 0.
        WA_COLOR-FNAME = 'WRBTR_H'.
        WA_COLOR-COLOR-COL  = '6'.
        WA_COLOR-COLOR-INT    = '1'.
        WA_COLOR-COLOR-INV    = '0'.
        APPEND WA_COLOR TO WA_DETAIL-CELLCOLOR.
      ENDIF.

*       计算余额
      IF P_ALL = ''.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_DBTR - WA_DETAIL-WRBTR_S + WA_DETAIL-WRBTR_H .
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-WRBTR_T.
      ELSEIF P_ALL = 'X'.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_WBTR - WA_DETAIL-WRBTR_S + WA_DETAIL-WRBTR_H .
        WA_DETAIL-DMBTR_T = FU_LW_NAME-BEG_DBTR - WA_DETAIL-DMBTR_S + WA_DETAIL-DMBTR_H.
        FU_LW_NAME-BEG_WBTR = WA_DETAIL-WRBTR_T.
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-DMBTR_T.
      ENDIF.

      MODIFY IT_DETAIL FROM WA_DETAIL TRANSPORTING WRBTR_T DMBTR_T EKGRP EKNAM CELLCOLOR.
      CLEAR:WA_DETAIL.
    ENDLOOP.
  ENDIF.

  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_VARIANT.
  PERFORM INIT_FIELDCAT_DETAIL.           "设置输出字段
  PERFORM FRM_OUTPUT_DETAIL.              "输出
ENDFORM.                    " FRM_DETAIL_ALV

FORM F_SETSTATUS USING PT_EXTAB TYPE SLIS_T_EXTAB. "固定参数
  SET PF-STATUS 'ZFIR024_GUI' EXCLUDING PT_EXTAB. " 排产按钮
ENDFORM.                    "setstatus

FORM INIT_VARIANT.
  CLEAR GW_VARIANT.
  GW_VARIANT-REPORT = SY-REPID.
  GW_VARIANT-HANDLE = '0001'.
ENDFORM.

FORM INIT_VARIANT1.
  CLEAR GW_VARIANT1.
  GW_VARIANT1-REPORT = SY-REPID.
  GW_VARIANT1-HANDLE = '0002'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_COMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->COMP   text
*----------------------------------------------------------------------*
FORM ADD_COMP  USING VALUE(COMP).
  APPEND COMP TO STA_SELECT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIFNR_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_TAB  text
*----------------------------------------------------------------------*
FORM LIFNR_INPUT  TABLES   T_TAB.
  FIELD-SYMBOLS <FS> TYPE ANY.
  DATA L_NAME TYPE CHAR20 VALUE 'T_TAB-LIFNR'.
  LOOP AT T_TAB.
    ASSIGN (L_NAME) TO <FS>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <FS>
      IMPORTING
        OUTPUT = <FS>.
    READ TABLE T_LIFNR WITH TABLE KEY TABLE_LINE = <FS>.
    IF SY-SUBRC NE 0.
      <FS> = 'NA'.
    ENDIF.
    MODIFY T_TAB.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIFNR_WHOLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIFNR_WHOLE .
  CLEAR IT_BSIS_LIFNR[].

  CHECK IT_BSIS[] IS NOT INITIAL.
  "设置动态WHERE语句
  CONCATENATE 'BUKRS EQ @W_ZFI015-BUKRS'
  'AND BUKRS EQ @IT_BSIS-BUKRS'
  'AND BELNR EQ @IT_BSIS-BELNR'
  'AND GJAHR EQ @IT_BSIS-GJAHR AND'
*  W_ZFI015-FIELDNAME 'IN @S_LIFNR AND'
  W_ZFI015-FIELDNAME 'NE @SPACE'
  INTO STA_WHERE SEPARATED BY SPACE.
  "记录供应商有效的行
  SELECT (STA_SELECT) INTO CORRESPONDING FIELDS OF TABLE @IT_BSIS_LIFNR
  FROM BSEG
  FOR ALL ENTRIES IN @IT_BSIS
  WHERE (STA_WHERE).
  PERFORM LIFNR_INPUT TABLES IT_BSIS_LIFNR.
  DELETE IT_BSIS_LIFNR WHERE LIFNR NOT IN S_LIFNR OR LIFNR IS INITIAL.
  SORT IT_BSIS_LIFNR BY GJAHR BUKRS BELNR.

  DATA L_TABIX TYPE SY-TABIX.

  "删除不包含有效供应商行的数据
  LOOP AT IT_BSIS INTO WA_BSIS.
    L_TABIX = SY-TABIX.
    READ TABLE IT_BSIS_LIFNR WITH KEY GJAHR = WA_BSIS-GJAHR BUKRS = WA_BSIS-BUKRS BELNR = WA_BSIS-BELNR BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE IT_BSIS INDEX L_TABIX.
    ELSE.
      WA_BSIS-LIFNR = IT_BSIS_LIFNR-LIFNR.
      MODIFY IT_BSIS FROM WA_BSIS INDEX L_TABIX.
    ENDIF.
  ENDLOOP.
ENDFORM.
*IT_BSEG_ZJB
FORM LIFNR_WHOLE_BSEG .

  CLEAR IT_BSEG_LIFNR[].
  CHECK IT_BSEG_ZJB[] IS NOT INITIAL.
  "设置动态WHERE语句
  CONCATENATE 'BUKRS EQ @W_ZFI015-BUKRS'
  'AND BUKRS EQ @it_bseg_zjb-BUKRS'
  'AND BELNR EQ @it_bseg_zjb-BELNR'
  'AND GJAHR EQ @it_bseg_zjb-GJAHR AND'
*  W_ZFI015-FIELDNAME 'IN @S_LIFNR AND'
  W_ZFI015-FIELDNAME 'NE @SPACE'
  INTO STA_WHERE SEPARATED BY SPACE.
  "记录供应商有效的行
  SELECT (STA_SELECT) INTO CORRESPONDING FIELDS OF TABLE @IT_BSEG_LIFNR
  FROM BSEG
  FOR ALL ENTRIES IN @IT_BSEG_ZJB
  WHERE (STA_WHERE).
  PERFORM LIFNR_INPUT TABLES IT_BSEG_LIFNR.
  DELETE IT_BSEG_LIFNR WHERE LIFNR NOT IN S_LIFNR OR LIFNR IS INITIAL.
  SORT IT_BSEG_LIFNR BY GJAHR BUKRS BELNR.

  DATA L_TABIX TYPE SY-TABIX.
  CLEAR L_TABIX.
  "删除不包含有效供应商行的数据
  LOOP AT IT_BSEG_ZJB INTO WA_BSEG_ZJB.
    L_TABIX = SY-TABIX..
    READ TABLE IT_BSEG_LIFNR WITH KEY GJAHR = WA_BSEG_ZJB-GJAHR BUKRS = WA_BSEG_ZJB-BUKRS BELNR = WA_BSEG_ZJB-BELNR BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE IT_BSEG_ZJB INDEX L_TABIX.
    ELSE.
      WA_BSEG_ZJB-LIFNR = IT_BSEG_LIFNR-LIFNR.
      MODIFY IT_BSEG_ZJB FROM WA_BSEG_ZJB INDEX L_TABIX.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  DATA: L_BUKRS LIKE T001-BUKRS.
  SELECT BUKRS INTO L_BUKRS FROM T001 WHERE BUKRS IN P_BUKRS.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                    ID 'BUKRS' FIELD L_BUKRS.
    IF SY-SUBRC <> 0.
      MESSAGE S899(MM) WITH '您没有公司代码' L_BUKRS '的权限' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
    ENDIF.
  ENDSELECT..
ENDFORM.
