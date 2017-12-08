*&---------------------------------------------------------------------*
*& Report  ZFIR021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZFIR021.

TABLES: FAGLFLEXT,
        BSEG,
        BSAD,
        KNA1,
        KNB1,
        VBAK,
        RF05L,
        BSID,
        PROJ.

TYPE-POOLS: SLIS.

*---------------------------------------------------------------------*
* 内表、全局变量 定义
*---------------------------------------------------------------------*

*ALV输出结构
TYPES:
  BEGIN OF TY_NAME,
    BUKRS     TYPE BSID-BUKRS,              "公司代码
    HKONT     TYPE BSID-HKONT,              "总账科目
    TXT50     TYPE SKAT-TXT50,              "科目描述
    KUNNR     TYPE BSID-KUNNR,              "客户号
    NAME1     TYPE KNA1-NAME1,              "客户描述
    SHKZG     TYPE CHAR10,                  "借贷方向
    WAERS     TYPE BSID-WAERS,              "货币
    BEG_WBTR  TYPE BSID-WRBTR,           "期初余额-交易币
    BEG_DBTR  TYPE BSID-DMBTR,           "期初余额-本位币
    WBTR_S    TYPE BSID-WRBTR,             "开票金额-交易币-借方
    WBTR_H    TYPE BSID-WRBTR,             "开票金额-交易币-贷方
    DBTR_S    TYPE BSID-DMBTR,             "开票金额-本位币-借方
    DBTR_H    TYPE BSID-DMBTR,             "开票金额-本位币-贷方
    END_WBTR  TYPE BSID-WRBTR,           "期末余额-交易币
    END_DBTR  TYPE BSID-DMBTR,           "期末余额-本位币
    CELLCOLOR TYPE LVC_T_SCOL,   "单元格颜色设置
  END OF TY_NAME.

TYPES:
  BEGIN OF TY_NAME1,
    BUKRS    TYPE BSID-BUKRS,              "公司代码
    HKONT    TYPE BSID-HKONT,              "总账科目
    TXT50    TYPE SKAT-TXT50,              "科目描述
    KUNNR    TYPE BSID-KUNNR,              "客户号
    NAME1    TYPE KNA1-NAME1,              "客户描述
    SHKZG    TYPE CHAR10,                  "借贷方向
    BEG_WBTR TYPE BSID-WRBTR,           "期初余额-交易币
    BEG_DBTR TYPE BSID-DMBTR,           "期初余额-本位币
    WBTR_S   TYPE BSID-WRBTR,             "开票金额-交易币-借方
    WBTR_H   TYPE BSID-WRBTR,             "开票金额-交易币-贷方
    DBTR_S   TYPE BSID-DMBTR,             "开票金额-本位币-借方
    DBTR_H   TYPE BSID-DMBTR,             "开票金额-本位币-贷方
    END_WBTR TYPE BSID-WRBTR,           "期末余额-交易币
    END_DBTR TYPE BSID-DMBTR,           "期末余额-本位币
  END OF TY_NAME1.

TYPES:
  BEGIN OF TY_SKB1,
    BUKRS TYPE SKB1-BUKRS,
    SAKNR TYPE SKB1-SAKNR,
    STEXT TYPE SKB1-STEXT,
  END OF TY_SKB1.

TYPES:
  BEGIN OF TY_BSID,
    BUKRS TYPE BSID-BUKRS,             "公司代码
    HKONT TYPE BSID-HKONT,             "总账科目
    KUNNR TYPE BSID-KUNNR,             "客户编号
    AUGBL TYPE BSAD-AUGBL,             "清账凭证号
    BELNR TYPE BSAD-BELNR,             "凭证编号
    WAERS TYPE BSID-WAERS,             "货币码
    DMBTR TYPE BSID-DMBTR,             "本位币金额
    WRBTR TYPE BSID-WRBTR,             "凭证金额
    ZUMSK TYPE BSID-ZUMSK,             "特别总账标识
    SHKZG TYPE BSID-SHKZG,             "借贷标识
  END OF TY_BSID.

TYPES:
  BEGIN OF TY_SKAT,
    SAKNR TYPE SKAT-SAKNR,
    TXT50 TYPE SKAT-TXT50,
  END OF TY_SKAT.

TYPES:
  BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
  END OF TY_KNA1.

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
        KUNNR TYPE BSEG-KUNNR,
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
        KUNNR    TYPE BSEG-KUNNR,
        WAERS    TYPE BKPF-WAERS,           "货币码
        BEG_DBTR TYPE BSEG-DMBTR,        "期初本位币
        BEG_WBTR TYPE BSEG-WRBTR,        "期初交易货币
        DMBTR_S  TYPE BSEG-DMBTR,           "本位币金额
        DMBTR_H  TYPE BSEG-DMBTR,
        WRBTR_S  TYPE BSEG-WRBTR,           "交易币金额
        WRBTR_H  TYPE BSEG-WRBTR,
      END OF TY_SUM.

*输出明细结构
TYPES:BEGIN OF TY_DETAIL,
        BUKRS     TYPE BKPF-BUKRS,              "公司代码
        GJAHR     TYPE BKPF-GJAHR,
        BLART     TYPE BKPF-BLART,
        HKONT     TYPE BSEG-HKONT,
        TXT50     TYPE SKAT-TXT50,              "科目描述
        KUNNR     TYPE BSEG-KUNNR,
        NAME1     TYPE KNA1-NAME1,              "客户描述
        WAERS     TYPE BKPF-WAERS,           "货币码
        BELNR     TYPE BSEG-BELNR,
        BUZEI     TYPE BSEG-BUZEI,            "行号
        BUDAT     TYPE BKPF-BUDAT,
        SGTXT     TYPE BSEG-SGTXT,           "摘要
        WRBTR_S   TYPE BSEG-WRBTR,
        WRBTR_H   TYPE BSEG-WRBTR,
        WRBTR_T   TYPE BSEG-WRBTR,
        DMBTR_S   TYPE BSEG-DMBTR,
        DMBTR_H   TYPE BSEG-DMBTR,
        DMBTR_T   TYPE BSEG-DMBTR,
        SHKZG     TYPE CHAR10,           "借贷标识
        DMBTR     TYPE BSEG-DMBTR,           "科目号
        WRBTR     TYPE BSEG-WRBTR,
        ZUONR     TYPE BSEG-ZUONR,
        VBEL2     TYPE BSEG-VBEL2,
        ZTERM     TYPE BSEG-ZTERM,
        VBELN     TYPE BSEG-VBELN,
        ZLSCH     TYPE BSEG-ZLSCH,
        ZFBDT     TYPE BSEG-ZFBDT,
        UMSKZ     TYPE BSEG-UMSKZ,
        CELLCOLOR TYPE LVC_T_SCOL,   "单元格颜色设置
        POST1     TYPE PS_POST1,     "项目名称 ADD IT02 20160223
      END OF TY_DETAIL.

DATA: IT_NAME TYPE TABLE OF TY_NAME,
      WA_NAME TYPE TY_NAME.

DATA: IT_SKAT TYPE TABLE OF TY_SKAT,
      WA_SKAT LIKE LINE OF IT_SKAT.

DATA: IT_SKB1 TYPE TABLE OF TY_SKB1,
      WA_SKB1 TYPE TY_SKB1.

DATA: IT_BSID TYPE TABLE OF TY_BSID,
      WA_BSID TYPE TY_BSID.

DATA: IT_KNA1 TYPE TABLE OF TY_KNA1,
      WA_KNA1 LIKE LINE OF IT_KNA1.

DATA: IT_BKPF TYPE TABLE OF TY_BKPF,
      WA_BKPF LIKE LINE OF IT_BKPF.

DATA: IT_BSEG TYPE TABLE OF TY_BSEG,
      WA_BSEG LIKE LINE OF IT_BSEG.

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
*PARAMETERS: p_bukrs TYPE t001-bukrs OBLIGATORY.        "公司代码
SELECT-OPTIONS:P_BUKRS FOR RF05L-BUKRS OBLIGATORY.
SELECT-OPTIONS:S_HKONT FOR BSAD-HKONT.                 "总账科目
SELECT-OPTIONS:S_MONAT FOR SY-DATUM OBLIGATORY.        "会计期间
SELECT-OPTIONS:S_KUNNR FOR KNB1-KUNNR.                 "客户编号
*               s_vbeln for vbak-VBELN MATCHCODE OBJECT VMVA.                 "销售订单号
SELECT-OPTIONS:S_UMSKZ FOR BSID-UMSKZ.
SELECTION-SCREEN END OF BLOCK BLK1.
*SELECTION-SCREEN SKIP 1.
*****交易货币与本位币显示设置
*SELECTION-SCREEN BEGIN OF BLOCK BK2 WITH FRAME TITLE text-002.
*PARAMETERS:p_all RADIOBUTTON GROUP R1,                 "显示本年累计
*           p_wrb RADIOBUTTON GROUP R1 DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK BK2.

PARAMETER: P_ALL TYPE C AS CHECKBOX.

*---------------------------------------------------------------------*
* at selection-screen
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
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

*---------------------------------------------------------------------*
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
*    *****客户号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'KUNNR'.
    GT_FIELDCAT-REF_FIELDNAME = 'KUNNR'.
    GT_FIELDCAT-REF_TABNAME = 'KNA1'.
    GT_FIELDCAT-SELTEXT_L = '客户号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
*        DEF_FIELDCAT 'KUNNR'           '客户号'      'X'  'L'   '10'.
    DEF_FIELDCAT 'NAME1'           '客户描述'      'X'  'L'   '10'.
    DEF_FIELDCAT 'SHKZG'           '借贷方向'  'X'  'L'   '5'.
*        DEF_FIELDCAT 'WAERS'           '货币'      'X'  'L'   '5'.
    DEF_FIELDCAT 'BEG_DBTR'           '期初余额-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_S'           '本期借方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'DBTR_H'           '本期贷方-本位币'      ''  'L'   '10'.
    DEF_FIELDCAT 'END_DBTR'           '期末余额-本位币'      ''  'L'   '10'.
  ELSEIF P_ALL = 'X'.
    DEF_FIELDCAT 'BUKRS'           '公司代码'          ' '  'L '   '4'.
    DEF_FIELDCAT 'HKONT'           '科目编号'          ' '  'L '   '10'.
    DEF_FIELDCAT 'TXT50'           '科目描述'        'X'  'L'   '20'.
*    *****客户号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT.
    GT_FIELDCAT-FIELDNAME = 'KUNNR'.
    GT_FIELDCAT-REF_FIELDNAME = 'KUNNR'.
    GT_FIELDCAT-REF_TABNAME = 'KNA1'.
    GT_FIELDCAT-SELTEXT_L = '客户号'.
    GT_FIELDCAT-NO_ZERO = 'X'.
    GT_FIELDCAT-JUST = 'L'.
    GT_FIELDCAT-OUTPUTLEN = '10'.
    APPEND GT_FIELDCAT.
*        DEF_FIELDCAT 'KUNNR'           '客户号'      'X'  'L'   '10'.
    DEF_FIELDCAT 'NAME1'           '客户描述'      'X'  'L'   '10'.
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
    DEF_FIELDCAT_DETAIL 'HKONT'           '科目编号'          'X'  'L '   '10' ''.
    DEF_FIELDCAT_DETAIL 'TXT50'           '科目描述'        'X'  'L'   '20' ''.
*  *****客户号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'KUNNR'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'KUNNR'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'KNA1'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '客户号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    GT_FIELDCAT_DETAIL-NO_OUT = ''.
    APPEND GT_FIELDCAT_DETAIL.

*      DEF_FIELDCAT_DETAIL 'KUNNR'           '客户号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'NAME1'           '客户描述'      'X'  'L'   '10' ''.
    IF P_BUKRS-LOW EQ '1800'.
    DEF_FIELDCAT_DETAIL 'POST1'           '项目名称'      'X'  'L'   '15' ''.
    ENDIF.
    DEF_FIELDCAT_DETAIL 'WAERS'           '货币'  'X'  'R'   'L' ''.
    DEF_FIELDCAT_DETAIL 'BELNR'           '会计凭证号'      'X'  'L'   '12' ''.
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
    DEF_FIELDCAT_DETAIL 'VBEL2'           '销售订单'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZTERM'           '付款条件'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'VBELN'           '开票凭证'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZLSCH'           '付款方式'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZFBDT'           '付款基准日'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'UMSKZ'           '特别总账标识'      'X'  'L'   '5' 'X'.
  ELSE.
    DEF_FIELDCAT_DETAIL 'BUKRS'           '公司代码'          'X'  'L '   '4' ''.
    DEF_FIELDCAT_DETAIL 'HKONT'           '科目编号'          'X'  'L '   '10' ''.
    DEF_FIELDCAT_DETAIL 'TXT50'           '科目描述'        'X'  'L'   '20' ''.
*  *****客户号字段增加参考表和参考字段
    CLEAR GT_FIELDCAT_DETAIL.
    GT_FIELDCAT_DETAIL-FIELDNAME = 'KUNNR'.
    GT_FIELDCAT_DETAIL-REF_FIELDNAME = 'KUNNR'.
    GT_FIELDCAT_DETAIL-REF_TABNAME = 'KNA1'.
    GT_FIELDCAT_DETAIL-SELTEXT_L = '客户号'.
    GT_FIELDCAT_DETAIL-NO_ZERO = 'X'.
    GT_FIELDCAT_DETAIL-JUST = 'L'.
    GT_FIELDCAT_DETAIL-OUTPUTLEN = '10'.
    GT_FIELDCAT_DETAIL-NO_OUT = ''.
    APPEND GT_FIELDCAT_DETAIL.
*      DEF_FIELDCAT_DETAIL 'KUNNR'           '客户号'      'X'  'L'   '10' ''.
    DEF_FIELDCAT_DETAIL 'NAME1'           '客户描述'      'X'  'L'   '10' ''.
    IF P_BUKRS-LOW EQ '1800'.
    DEF_FIELDCAT_DETAIL 'POST1'           '项目名称'      'X'  'L'   '15' ''.
    ENDIF.
    DEF_FIELDCAT_DETAIL 'WAERS'           '货币'  'X'  'R'   'L' ''.
    DEF_FIELDCAT_DETAIL 'BELNR'           '会计凭证号'      'X'  'L'   '12' ''.
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
    DEF_FIELDCAT_DETAIL 'VBEL2'           '销售订单'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'ZTERM'           '付款条件'      'X'  'L'   '10' 'X'.
    DEF_FIELDCAT_DETAIL 'VBELN'           '开票凭证'      'X'  'L'   '10' 'X'.
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
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT_DETAIL .
*  DATA:lw_variant type disvariant.
*       lw_variant-report = sy-repid.
  G_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT_DETAIL[]
      IS_VARIANT               = GW_VARIANT1
      I_CALLBACK_PF_STATUS_SET = 'F_SETSTATUS'
      I_CALLBACK_USER_COMMAND  = 'F_USER_COMMAND_DETAIL'
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
*&      Form  FRM_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_EVENT .

ENDFORM.                    " FRM_GET_EVENT
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
  CLEAR:WA_BSID,IT_BSID.
  "   期初余额 = 期初未清余额 = 期初过账截至目前未清 + 期初过账未清截至目前已清= BSID（限定过账日期）  + BSAD（限定过账日期和清账日期）
*取BSID数据
  IF S_MONAT-LOW >= SY-DATUM.
    SELECT BUKRS              "公司代码
           HKONT              "总账科目
           KUNNR              "客户编号
           AUGBL
           BELNR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSID
      INTO TABLE IT_BSID
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT < S_MONAT-LOW
    AND   KUNNR IN S_KUNNR
*  and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
* and umskz <> 'W'.
  ELSE.
    SELECT BUKRS              "公司代码
           HKONT              "总账科目
           KUNNR              "客户编号
           AUGBL
           BELNR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSID
      INTO TABLE IT_BSID
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT < S_MONAT-LOW
      AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.
*取BSAD数据
    SELECT BUKRS              "公司代码
           HKONT              "总账科目
           KUNNR              "客户编号
           AUGBL
           BELNR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSAD
      APPENDING TABLE IT_BSID
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      "AND   AUGDT < SY-DATUM
      AND   AUGDT >= S_MONAT-LOW
      AND   BUDAT < S_MONAT-LOW
      AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.
  ENDIF.

  "清账凭证号不等于凭证编号
  LOOP AT IT_BSID INTO WA_BSID.
    IF WA_BSID-AUGBL = WA_BSID-BELNR.
      DELETE TABLE IT_BSID FROM WA_BSID.
    ENDIF.
  ENDLOOP.

*处理汇总借贷
  LOOP AT IT_BSID INTO WA_BSID.
    WA_SUM-BUKRS = WA_BSID-BUKRS.
    WA_SUM-HKONT = WA_BSID-HKONT.
    WA_SUM-KUNNR = WA_BSID-KUNNR.
    WA_SUM-WAERS = WA_BSID-WAERS.
    IF WA_BSID-SHKZG = 'H'.
      WA_SUM-BEG_DBTR = WA_BSID-DMBTR * -1.
      WA_SUM-BEG_WBTR = WA_BSID-WRBTR * -1.
    ELSE.
      WA_SUM-BEG_DBTR = WA_BSID-DMBTR.
      WA_SUM-BEG_WBTR = WA_BSID-WRBTR.
    ENDIF.
    COLLECT WA_SUM INTO IT_SUM.
    CLEAR:WA_SUM,WA_BSID.
  ENDLOOP.

  MOVE-CORRESPONDING IT_SUM TO IT_NAME.
  CLEAR:IT_SUM.

*  取BSID/BSAD中budat属于选择画面的所有行项目的值
  IF S_MONAT-HIGH IS INITIAL.
    SELECT BUKRS              "公司代码
         HKONT              "总账科目
         KUNNR              "客户编号
         AUGBL
         BELNR
         WAERS              "货币码
         DMBTR              "本位币金额
         WRBTR              "凭证金额
         ZUMSK              "特别总账标识
         SHKZG              "借贷标识
  FROM BSID
    APPENDING TABLE IT_BSID
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT = S_MONAT-LOW
    AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.

    SELECT BUKRS              "公司代码
           HKONT              "总账科目
           KUNNR              "客户编号
           AUGBL
           BELNR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSAD
      APPENDING TABLE IT_BSID
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT = S_MONAT-LOW
      AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.
  ELSE.
    SELECT BUKRS              "公司代码
         HKONT              "总账科目
         KUNNR              "客户编号
         AUGBL
         BELNR
         WAERS              "货币码
         DMBTR              "本位币金额
         WRBTR              "凭证金额
         ZUMSK              "特别总账标识
         SHKZG              "借贷标识
  FROM BSID
    APPENDING TABLE IT_BSID
    WHERE BUKRS IN P_BUKRS
    AND   HKONT IN S_HKONT
    AND   BUDAT <= S_MONAT-HIGH
    AND   BUDAT >= S_MONAT-LOW
    AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.

    SELECT BUKRS              "公司代码
           HKONT              "总账科目
           KUNNR              "客户编号
           AUGBL
           BELNR
           WAERS              "货币码
           DMBTR              "本位币金额
           WRBTR              "凭证金额
           ZUMSK              "特别总账标识
           SHKZG              "借贷标识
    FROM BSAD
      APPENDING TABLE IT_BSID
      WHERE BUKRS IN P_BUKRS
      AND   HKONT IN S_HKONT
      AND   BUDAT <= S_MONAT-HIGH
      AND   BUDAT >= S_MONAT-LOW
      AND   KUNNR IN S_KUNNR
*    and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*   and umskz <> 'W'.
  ENDIF.


  SORT IT_BSID BY BUKRS HKONT KUNNR WAERS.
  DELETE ADJACENT DUPLICATES FROM IT_BSID COMPARING BUKRS HKONT KUNNR WAERS.
*
*    LOOP AT it_bsid INTO wa_bsid.
*    wa_name-hkont = wa_bsid-hkont.
*    wa_name-kunnr = wa_bsid-kunnr.
*    wa_name-waers = wa_bsid-waers.
*    COLLECT wa_name INTO it_name.
*    CLEAR:wa_name,wa_bsid.
*  ENDLOOP.
  SORT IT_NAME BY BUKRS HKONT KUNNR WAERS.
  LOOP AT IT_BSID INTO WA_BSID.
    READ TABLE IT_NAME INTO WA_NAME WITH KEY BUKRS = WA_BSID-BUKRS
                                             HKONT = WA_BSID-HKONT
                                             KUNNR = WA_BSID-KUNNR
                                             WAERS = WA_BSID-WAERS.
    IF SY-SUBRC = 0.
    ELSE.
      WA_NAME-BUKRS = WA_BSID-BUKRS.
      WA_NAME-HKONT = WA_BSID-HKONT.
      WA_NAME-KUNNR = WA_BSID-KUNNR.
      WA_NAME-WAERS = WA_BSID-WAERS.
      APPEND WA_NAME TO IT_NAME.
    ENDIF.
    CLEAR:WA_NAME,WA_BSID.
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
    CLEAR:WA_SKAT,WA_NAME.
  ENDLOOP.
  CLEAR: IT_SKAT.

*取客户描述
  SELECT KUNNR NAME1
    INTO TABLE IT_KNA1
  FROM KNA1.
  SORT IT_KNA1 BY KUNNR.

  LOOP AT IT_NAME INTO WA_NAME.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_NAME-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_NAME-NAME1 = WA_KNA1-NAME1.
      MODIFY IT_NAME FROM WA_NAME.
    ENDIF.
  ENDLOOP.
  CLEAR:IT_KNA1.
*取BKPF表
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

*通过BKPF、科目号、客户号取BSEG数据
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
           KUNNR           "客户号
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
*   and   vbel2 in s_vbeln
    AND   UMSKZ IN S_UMSKZ.
*  and   umskz <> 'W'.
  ENDIF.

*凭证明细取货币码,过账日期
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

*本期发生额求和
  CLEAR: WA_SUM,IT_SUM.
  LOOP AT IT_BSEG INTO WA_BSEG.
    WA_SUM-BUKRS = WA_BSEG-BUKRS.
    WA_SUM-HKONT = WA_BSEG-HKONT.
    WA_SUM-KUNNR = WA_BSEG-KUNNR.
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

*添加到输出内表
  SORT IT_SUM BY BUKRS HKONT KUNNR WAERS.
  LOOP AT IT_NAME INTO WA_NAME.
    READ TABLE IT_SUM INTO WA_SUM WITH KEY BUKRS = WA_NAME-BUKRS
                                           HKONT = WA_NAME-HKONT
                                           KUNNR = WA_NAME-KUNNR
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
      WA_NAME1-KUNNR = WA_NAME-KUNNR.
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

  LOOP AT IT_NAME INTO WA_NAME.
    .
**    期末余额，本位币，交易货币
*     wa_name-shkzg = '借'.
    WA_NAME-END_WBTR = WA_NAME-BEG_WBTR + WA_NAME-WBTR_S - WA_NAME-WBTR_H.
    WA_NAME-END_DBTR = WA_NAME-BEG_DBTR + WA_NAME-DBTR_S - WA_NAME-DBTR_H.
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
    CLEAR: WA_NAME.
  ENDLOOP.
*  输出内表排序
  SORT IT_NAME BY BUKRS HKONT KUNNR WAERS.

ENDFORM.                    " FRM_FETCH_DATA
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
    WHERE MITKZ = 'D'.
  ELSE.
*    TRANSLATE code TO UPPER CASE.
    SELECT BUKRS SAKNR STEXT
      FROM SKB1
      INTO CORRESPONDING FIELDS OF TABLE IT_SKB1
    WHERE MITKZ = 'D' AND BUKRS IN CODE.
  ENDIF.
  CLEAR: CODE.
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
ENDFORM.
" FRM_HELP
FORM F_USER_COMMAND USING P_UCOMM LIKE SY-UCOMM
                          P_RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: LW_NAME LIKE LINE OF IT_NAME.
  CASE P_UCOMM.
      WHEN '&IC1'.
      READ TABLE IT_NAME INTO LW_NAME INDEX P_RS_SELFIELD-TABINDEX.
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

*&---------------------------------------------------------------------*
*&      Form  FRM_DETAIL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_NAME  text
*----------------------------------------------------------------------*
FORM FRM_DETAIL_ALV  USING  FU_LW_NAME TYPE TY_NAME.
  DATA WA_COLOR TYPE LVC_S_SCOL.  "颜色设置
  CLEAR:IT_DETAIL,WA_DETAIL.
  CLEAR:GT_FIELDCAT_DETAIL[].
  IF P_ALL = 'X'.
    LOOP AT IT_BSEG INTO WA_BSEG.
      IF WA_BSEG-BUKRS = FU_LW_NAME-BUKRS
         AND  WA_BSEG-HKONT = FU_LW_NAME-HKONT
         AND  WA_BSEG-KUNNR = FU_LW_NAME-KUNNR
         AND  WA_BSEG-WAERS = FU_LW_NAME-WAERS.
        WA_DETAIL-BLART = WA_BSEG-BLART.
        WA_DETAIL-BUKRS = WA_BSEG-BUKRS.
        WA_DETAIL-GJAHR = WA_BSEG-GJAHR.
        WA_DETAIL-HKONT = FU_LW_NAME-HKONT.
        WA_DETAIL-TXT50 = FU_LW_NAME-TXT50.
        WA_DETAIL-KUNNR = FU_LW_NAME-KUNNR.
        WA_DETAIL-NAME1 = FU_LW_NAME-NAME1.
        WA_DETAIL-WAERS = WA_BSEG-WAERS.
        WA_DETAIL-BELNR = WA_BSEG-BELNR.
        WA_DETAIL-BUZEI = WA_BSEG-BUZEI.
        WA_DETAIL-BUDAT = WA_BSEG-BUDAT.
        WA_DETAIL-UMSKZ = WA_BSEG-UMSKZ.
        IF P_ALL = ''.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'.
            WA_DETAIL-WRBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ELSEIF P_ALL = 'X'.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'.
            WA_DETAIL-WRBTR_H = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ENDIF.
        WA_DETAIL-SHKZG = '借'.
*            wa_detail-wrbtr_t = fu_lw_name-beg_wbtr + wa_detail-wrbtr_s - wa_detail-wrbtr_h .
        WA_DETAIL-SGTXT = WA_BSEG-SGTXT.
        IF WA_BSEG-SHKZG = 'H' .
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR * -1.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR * -1.
        ELSEIF  WA_BSEG-SHKZG = 'S'.
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
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_DBTR + WA_DETAIL-WRBTR_S - WA_DETAIL-WRBTR_H .
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-WRBTR_T.
      ELSEIF P_ALL ='X'.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_WBTR + WA_DETAIL-WRBTR_S - WA_DETAIL-WRBTR_H .
        WA_DETAIL-DMBTR_T = FU_LW_NAME-BEG_DBTR + WA_DETAIL-DMBTR_S - WA_DETAIL-DMBTR_H.
        FU_LW_NAME-BEG_WBTR = WA_DETAIL-WRBTR_T.
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-DMBTR_T.
      ENDIF.
*   1800：项目名称  ADD IT02 20160223
     IF P_BUKRS-LOW = '1800'.
         SELECT SINGLE POST1 INTO WA_DETAIL-POST1 FROM PROJ WHERE PSPID = WA_DETAIL-ZUONR .
     ENDIF.
      MODIFY IT_DETAIL FROM WA_DETAIL TRANSPORTING WRBTR_T DMBTR_T CELLCOLOR POST1.
      CLEAR:WA_DETAIL.
    ENDLOOP.
  ELSE.
    LOOP AT IT_BSEG INTO WA_BSEG.
      IF WA_BSEG-BUKRS = FU_LW_NAME-BUKRS
         AND  WA_BSEG-HKONT = FU_LW_NAME-HKONT
         AND  WA_BSEG-KUNNR = FU_LW_NAME-KUNNR.
        WA_DETAIL-BLART = WA_BSEG-BLART.
        WA_DETAIL-BUKRS = WA_BSEG-BUKRS.
        WA_DETAIL-GJAHR = WA_BSEG-GJAHR.
        WA_DETAIL-HKONT = FU_LW_NAME-HKONT.
        WA_DETAIL-TXT50 = FU_LW_NAME-TXT50.
        WA_DETAIL-KUNNR = FU_LW_NAME-KUNNR.
        WA_DETAIL-NAME1 = FU_LW_NAME-NAME1.
        WA_DETAIL-WAERS = WA_BSEG-WAERS.
        WA_DETAIL-BELNR = WA_BSEG-BELNR.
        WA_DETAIL-BUZEI = WA_BSEG-BUZEI.
        WA_DETAIL-BUDAT = WA_BSEG-BUDAT.
        WA_DETAIL-UMSKZ = WA_BSEG-UMSKZ.
        IF P_ALL = ''.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'.
            WA_DETAIL-WRBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ELSEIF P_ALL = 'X'.
          IF WA_BSEG-SHKZG = 'S'.
            WA_DETAIL-WRBTR_S = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_S = WA_BSEG-DMBTR.
          ELSEIF WA_BSEG-SHKZG = 'H'.
            WA_DETAIL-WRBTR_H = WA_BSEG-WRBTR.
            WA_DETAIL-DMBTR_H = WA_BSEG-DMBTR.
          ENDIF.
        ENDIF.
        WA_DETAIL-SHKZG = '借'.
*        wa_detail-wrbtr_t = fu_lw_name-beg_wbtr + wa_detail-wrbtr_s - wa_detail-wrbtr_h .
        WA_DETAIL-SGTXT = WA_BSEG-SGTXT.
        IF WA_BSEG-SHKZG = 'H' .
          WA_DETAIL-DMBTR = WA_BSEG-DMBTR * -1.
          WA_DETAIL-WRBTR = WA_BSEG-WRBTR * -1.
        ELSEIF  WA_BSEG-SHKZG = 'S'.
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
      CLEAR:WA_COLOR.
*   设置颜色
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

*   计算余额
      IF P_ALL = ''.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_DBTR + WA_DETAIL-WRBTR_S - WA_DETAIL-WRBTR_H .
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-WRBTR_T.
      ELSEIF P_ALL ='X'.
        WA_DETAIL-WRBTR_T = FU_LW_NAME-BEG_WBTR + WA_DETAIL-WRBTR_S - WA_DETAIL-WRBTR_H .
        WA_DETAIL-DMBTR_T = FU_LW_NAME-BEG_DBTR + WA_DETAIL-DMBTR_S - WA_DETAIL-DMBTR_H.
        FU_LW_NAME-BEG_WBTR = WA_DETAIL-WRBTR_T.
        FU_LW_NAME-BEG_DBTR = WA_DETAIL-DMBTR_T.
      ENDIF.
*  *   1800：项目名称  ADD IT02 20160223
     IF P_BUKRS-LOW = '1800'.
         SELECT SINGLE POST1 INTO WA_DETAIL-POST1 FROM PROJ WHERE PSPID = WA_DETAIL-ZUONR .
     ENDIF.

      MODIFY IT_DETAIL FROM WA_DETAIL TRANSPORTING WRBTR_T DMBTR_T CELLCOLOR POST1.
      CLEAR:WA_DETAIL.
    ENDLOOP.
  ENDIF.

  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_VARIANT.
  PERFORM INIT_FIELDCAT_DETAIL.           "设置输出字段
  PERFORM FRM_OUTPUT_DETAIL.              "输出
ENDFORM.                    " FRM_DETAIL_ALV

FORM F_SETSTATUS USING PT_EXTAB TYPE SLIS_T_EXTAB. "固定参数
  SET PF-STATUS 'ZFIR021_GUI' EXCLUDING PT_EXTAB. " 排产按钮
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
    ENDIF.
  ENDSELECT..
ENDFORM.
