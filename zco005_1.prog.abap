REPORT ZCO005_1.

* DES:项目订单销售费用结转
* DATE 20160309  IT02

TABLES:BKPF,BSEG,VBAK,MAKT,SKAT,ZFI006.

DATA:BEGIN OF GS_DATA ,
     BUKRS     TYPE BKPF-BUKRS,     "公司代码
     GJAHR     TYPE BKPF-GJAHR,     "会计年度
     MONAT     TYPE BKPF-MONAT,     "期间
     VBEL2     TYPE BSEG-VBEL2,     "销售订单
     POSN2     TYPE BSEG-POSN2,     "销售订单行项目
     AUART     TYPE VBAK-AUART,     "订单类型
     YSYEAR    TYPE BSEG-GJAHR,     "验收年度
     BELNR     TYPE BSEG-BELNR,     "会计凭证
     BUZEI     TYPE BSEG-BUZEI,     "行项目
     SHKZG     TYPE BSEG-SHKZG,     "借贷标识
     BUDAT     TYPE BKPF-BUDAT,     "过账日期
     HKONT     TYPE BSEG-HKONT,     "科目
     HKONT_TXT TYPE STRING,         "科目描述
     WRBTR     TYPE BSEG-WRBTR,     "凭证货币金额
     WAERS     TYPE BKPF-WAERS,     "凭证货币
     DMBTR     TYPE BSEG-DMBTR,     "本币金额
     WAERS_1   TYPE BKPF-WAERS,     "本币
     BKTXT     TYPE BKPF-BKTXT,     "摘要
    " SHKZG TYPE BSEG-SHKZG,         "借贷标识
     MATNR     TYPE BSEG-MATNR,      "物料号
     MAKTX     TYPE MAKT-MAKTX,      "物料描述

    END OF GS_DATA.
DATA:BEGIN OF GS_SUM,
     BUKRS TYPE BKPF-BUKRS,     "公司代码
     GJAHR TYPE C   LENGTH 4 ,  "会计年度
     MONAT TYPE C   LENGTH 2,   "会计期间
     VBEL2 TYPE BSEG-VBEL2,     "销售订单
     AUART TYPE VBAK-AUART,     "订单类型
     DQCPCB TYPE BSEG-DMBTR,    "当前产品成本
     DYGCFY TYPE BSEG-DMBTR,    "当月工程费用
     END OF GS_SUM .
DATA:BEGIN OF GS_TOTAL,
     BUKRS TYPE BKPF-BUKRS,     "公司代码
     GJAHR TYPE BKPF-GJAHR,     "会计年度
     MONAT TYPE BKPF-MONAT,     "会计期间
     VBEL2 TYPE BSEG-VBEL2,     "销售订单
     POSN2 TYPE BSEG-POSN2,     "销售订单行号
     AUART TYPE VBAK-AUART,     "订单类型
     YSYEAR TYPE BSEG-GJAHR,    "验收年度
     DQCPCB TYPE BSEG-DMBTR,    "当前产品成本
     DYGCFY TYPE BSEG-DMBTR,    "当月工程费用
     YZCB   TYPE P  LENGTH 15 DECIMALS 2,    "应转成本
     JZKM   TYPE BSEG-HKONT,    "结转科目
     JZKM_TXT TYPE STRING ,     "结转科目描述
     DFKM   TYPE  BSEG-HKONT,   "对方科目
     DFKM_TXT TYPE STRING,      "对方科目描述
     PZLX   TYPE BKPF-BLART,    "凭证类型
     GZRQ   TYPE BKPF-BUDAT,    "过账日期
     BKTXT  TYPE BKPF-BKTXT,    "文本
     GZPZ   TYPE BKPF-BELNR,    "过账凭证
     GZPZND TYPE BSEG-GJAHR,    "过账凭证年度
     WAERS TYPE BKPF-WAERS,     "货币码
     INFO_MSG TYPE STRING,      "消息
     CELLSTYLE TYPE LVC_T_STYL, "单元格状态
     XMMC   TYPE STRING ,       "项目名称
     STATU    TYPE   ICONNAME,  "过账状态栏
     SEL(1) TYPE C ,
 END OF GS_TOTAL .

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA:GS_ZCO005_1 LIKE ZCO005_1 ,
     GT_ZCO005_1 LIKE TABLE OF ZCO005_1 .

DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.


 "声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_onf4 for event onf4 of cl_gui_alv_grid
     importing e_fieldname es_row_no er_event_data et_bad_cells e_display,
     handle_modify  FOR EVENT data_changed OF cl_gui_alv_grid
     IMPORTING ER_DATA_CHANGED."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

                 "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA:GT_SUM LIKE TABLE OF GS_SUM WITH HEADER LINE.

DATA:GT_DATA LIKE TABLE  OF GS_DATA WITH HEADER LINE.

DATA:GT_TOTAL LIKE TABLE OF GS_TOTAL WITH HEADER LINE.

 FIELD-SYMBOLS: <FS_DATA>  LIKE GS_DATA,
                <FS_TOTAL> LIKE GS_TOTAL.


DATA:GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.
DATA:GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.
DATA:GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE,
     GS_SKAT LIKE GT_SKAT.
DATA:GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.
DATA:GT_VBRK LIKE TABLE OF VBRK WITH HEADER LINE.
DATA:GT_VBFA LIKE TABLE OF VBFA WITH HEADER LINE.
DATA:GT_VBAP LIKE TABLE OF VBAP WITH HEADER LINE.
DATA:L_TABIX TYPE I.
DATA:F_FIRST LIKE SY-DATUM,
     F_END   LIKE SY-DATUM.

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
  IF &1 = 'DQCPCB' OR &1 = 'DYGCFY' ."OR  &1 = 'YZCB'.
  GW_LVC-CFIELDNAME = 'WAERS'.
 ENDIF.
 IF &1 = 'YZCB'.
  " GW_LVC-NO_SIGN = ''.
   GW_LVC-DECIMALS = 2.
 ENDIF.
 IF &1 = 'JZKM' OR &1 = 'DFKM' .
   gw_lvc-f4availabl = 'X'.
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
DATA gt_event_receiver TYPE REF TO lcl_event_receiver .



DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.
DATA: GW_ISTABLE TYPE LVC_S_STBL.

************************************************************************
* BAPI
************************************************************************
DATA: WA_DOCUMENTHEADER    TYPE BAPIACHE09,         "表头
      IT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,  "总账
      WA_ACCOUNTGL         TYPE BAPIACGL09,
      IT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,  "客户
      WA_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      IT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,  "货币项目
      WA_CURRENCYAMOUNT    TYPE BAPIACCR09,
      IT_CRITERIA          TYPE TABLE OF BAPIACKEC9,  "分配-科目
      WA_CRITERIA          TYPE BAPIACKEC9,
      IT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      WA_VALUEFIELD        TYPE BAPIACKEV9,
      IT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      WA_EXTENSION2        TYPE BAPIPAREX,
      IT_RETURN            TYPE TABLE OF BAPIRET2,
      WA_RETURN            TYPE BAPIRET2.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: WA_ZACCDOCUEXT TYPE ZACCDOCUEXT.

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
*SELECT-OPTIONS: S_VBEL2 FOR BSEG-VBEL2.    "销售订单
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
 "取出费用结转的过账明细信息
 SELECT BKPF~BUKRS BKPF~GJAHR BKPF~MONAT
        BSEG~BELNR BSEG~BUZEI BSEG~HKONT  BSEG~SHKZG
        BSEG~DMBTR  BSEG~VBEL2 BSEG~POSN2
        INTO CORRESPONDING FIELDS OF TABLE GT_DATA
        FROM BKPF
        INNER JOIN BSEG
        ON BKPF~BUKRS = BSEG~BUKRS AND BKPF~GJAHR = BSEG~GJAHR
         AND BKPF~BELNR = BSEG~BELNR
        WHERE BKPF~GJAHR EQ P_GJAHR
         AND  BKPF~BUKRS EQ P_BUKRS
         AND  BKPF~MONAT EQ P_MONAT
       "  AND  BSEG~VBEL2 IN S_VBEL2
         AND  BSEG~VBEL2 NE ''
         AND  ( ( BSEG~HKONT BETWEEN '8000000000' AND '8999999999' )
                 OR
               ( BSEG~HKONT BETWEEN '6400000000' AND '6499999999' ) ).

  SORT GT_DATA BY BUKRS GJAHR MONAT VBEL2 HKONT.

  CHECK GT_DATA[] IS NOT INITIAL.

  "取出公司代码主数据信息
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
    FROM T001
    WHERE BUKRS = P_BUKRS.
  SORT GT_T001 BY BUKRS.

  "取出销售订单抬头信息
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
    FROM VBAK
    FOR  ALL ENTRIES IN GT_DATA
    WHERE VBELN = GT_DATA-VBEL2
    AND   ( AUART = 'ZPO' or AUART = 'ZSO' ).
  SORT GT_VBAK BY VBELN.

"取出销售明细信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
  FROM VBAP
  FOR ALL ENTRIES IN GT_VBAK
  WHERE VBELN = GT_VBAK-VBELN.
SORT GT_VBAP BY VBELN POSNR.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
*    FROM MAKT
*    FOR ALL ENTRIES IN GT_DATA
*    WHERE MATNR = GT_DATA-MATNR
*    AND   SPRAS = '1'.
*
* SORT GT_MAKT BY MATNR.
"取出科目主数据信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
  FROM SKAT
 WHERE  SPRAS = '1'
  AND  KTOPL = 1000.
SORT GT_SKAT BY SAKNR.

"取出销售明细信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBFA
  FROM VBFA
  FOR ALL ENTRIES IN GT_DATA
  WHERE VBELV = GT_DATA-VBEL2
  AND   VBTYP_N IN ('M','N').

SORT GT_VBFA BY VBELV ASCENDING VBELN DESCENDING.

DELETE ADJACENT DUPLICATES FROM GT_VBFA COMPARING VBELV VBELN.

"取出开票抬头信息
IF GT_VBFA[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBRK
  FROM VBRK
  FOR ALL ENTRIES IN GT_VBFA
  WHERE VBELN = GT_VBFA-VBELN.

SORT GT_VBRK BY VBELN.
ENDIF.

"取出销售明细信息
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
 "月初第一天
 CONCATENATE P_GJAHR P_MONAT '01' INTO F_FIRST.
 "月末最后一天
 CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
   EXPORTING
     I_DATE        = F_FIRST
 IMPORTING
    E_DATE        = F_END
           .

"根据过账明细按照公司代码、年度、期间、销售订单、订单类型为组分类汇总当期产品成本、当前工程费用
 LOOP AT  GT_DATA ASSIGNING <FS_DATA> .

   "订单类型
   READ TABLE GT_VBAK WITH KEY VBELN = <FS_DATA>-VBEL2  AUART = 'ZPO' BINARY SEARCH .
     IF SY-SUBRC EQ '0'.
          <FS_DATA>-AUART = GT_VBAK-AUART.
          READ TABLE GT_VBAP WITH KEY VBELN = <FS_DATA>-VBEL2 POSNR = <FS_DATA>-POSN2 BINARY SEARCH .
            IF SY-SUBRC NE '0'.
              " DELETE GT_DATA INDEX L_TABIX .
               CONTINUE.
            ENDIF.
      ELSE.
       " DELETE GT_DATA INDEX L_TABIX .
        CONTINUE.
     ENDIF.
  IF <FS_DATA>-SHKZG EQ 'H'.
     <FS_DATA>-DMBTR = <FS_DATA>-DMBTR * -1.
  ENDIF.
  CLEAR:GT_SUM,GS_SUM.
  GS_SUM-BUKRS = <FS_DATA>-BUKRS.   "公司代码
  GS_SUM-GJAHR = <FS_DATA>-GJAHR.   "年度
  GS_SUM-MONAT = <FS_DATA>-MONAT.   "期间
  GS_SUM-VBEL2 = <FS_DATA>-VBEL2.   "销售订单
  GS_SUM-AUART = <FS_DATA>-AUART.   "订单类型
  IF <FS_DATA>-HKONT BETWEEN '6400000000' AND '6499999999'.
     GS_SUM-DQCPCB = <FS_DATA>-DMBTR.      "当期产品成本
  ELSEIF <FS_DATA>-HKONT BETWEEN '8000000000' AND '8999999999' .
  GS_SUM-DYGCFY = <FS_DATA>-DMBTR.        "当前工程费用
  ENDIF.
   COLLECT GS_SUM INTO GT_SUM.
ENDLOOP.
SORT GT_SUM BY BUKRS GJAHR MONAT VBEL2.
 MOVE-CORRESPONDING GT_SUM[] TO GT_TOTAL[].
SORT GT_TOTAL BY BUKRS GJAHR MONAT VBEL2.

LOOP AT GT_TOTAL ASSIGNING <FS_TOTAL> .
  "凭证类型默认
  <FS_TOTAL>-PZLX = 'SC'.

  "验收年度
READ TABLE  GT_VBFA WITH KEY VBELV = <FS_TOTAL>-VBEL2 BINARY SEARCH .
 IF SY-SUBRC EQ 0 .
     IF GT_VBFA-VBTYP_N NE 'N'.
        READ TABLE GT_VBRK WITH KEY VBELN = GT_VBFA-VBELN BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
              <FS_TOTAL>-YSYEAR = GT_VBRK-FKDAT+0(4).
           ENDIF.
      ENDIF.
 ENDIF.

 "本币码;
 READ TABLE GT_T001 WITH KEY BUKRS = <FS_TOTAL>-BUKRS BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
       <FS_TOTAL>-WAERS = GT_T001-WAERS.
    ENDIF.

 "应转成本：
 IF <FS_TOTAL>-YSYEAR EQ '0000'.
    <FS_TOTAL>-YZCB = 0 .
   ELSEIF <FS_TOTAL>-YSYEAR = <FS_TOTAL>-GJAHR .
     <FS_TOTAL>-YZCB = 0 .
   ELSEIF <FS_TOTAL>-YSYEAR < <FS_TOTAL>-GJAHR.
     <FS_TOTAL>-YZCB = <FS_TOTAL>-DQCPCB + <FS_TOTAL>-DYGCFY .

 ENDIF.



  "结转科目默认
  <FS_TOTAL>-JZKM = '8998000009'.
     READ TABLE GT_SKAT WITH KEY SAKNR = <FS_TOTAL>-JZKM BINARY SEARCH .
       IF SY-SUBRC EQ 0 .
          <FS_TOTAL>-JZKM_TXT = GT_SKAT-TXT50.
       ENDIF.
   "对方科目
      <FS_TOTAL>-DFKM = '6601010101'.
    READ TABLE GT_SKAT WITH KEY SAKNR = <FS_TOTAL>-DFKM BINARY SEARCH .
       IF SY-SUBRC EQ 0 .
          <FS_TOTAL>-DFKM_TXT = GT_SKAT-TXT50.
       ENDIF.
  "过账日期
     <FS_TOTAL>-GZRQ = F_END.
  "文本默认
     <FS_TOTAL>-BKTXT = '已验收项目销售费用结转'.

"项目名称
PERFORM SELXMMC USING <FS_TOTAL>-VBEL2 CHANGING <FS_TOTAL>-XMMC.

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
  GW_LAYOUT-BOX_FNAME    = 'SEL'.
  GW_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
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
  INIT_FIELDCAT 'STATU'          '过账状态'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'         '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'         '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'         '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBEL2'         '销售订单'         '' '' '' '' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'XMMC'          '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'         '订单类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSYEAR'        '验收年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQCPCB'        '当期产品成本'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYGCFY'        '当期工程费用'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZCB'          '应转成本'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'JZKM'          '结转科目'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'JZKM_TXT'      '结转科目描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DFKM'          '对方科目'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'DFKM_TXT'      '对方科目描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PZLX'          '凭证类型'         '' '' '' 'X' '' 'BKPF' 'BLART'.
  INIT_FIELDCAT 'GZRQ'          '过账日期'         '' '' '' 'X' '' 'BKPF' 'BUDAT'.
  INIT_FIELDCAT 'BKTXT'         '文本'             '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'GZPZ'          '过账凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GZPZND'        '过账凭证年度'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INFO_MSG'      '过账消息文本'     '' '' '' '' '' '' ''.

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

  GW_EVENTS-NAME =  'CALLER_EXIT' .
  GW_EVENTS-FORM =  'FRM_BUTTON'.   "f4事件
  APPEND GW_EVENTS TO GT_EVENTS.
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
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
      T_OUTTAB                 = GT_TOTAL
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

 " DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GR_ALVGRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
      READ TABLE GT_TOTAL INTO GS_TOTAL INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'GZPZ'
        AND GS_TOTAL-GZPZ IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_TOTAL-GZPZ.
        SET PARAMETER ID 'BUK' FIELD GS_TOTAL-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_TOTAL-GZPZND.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'VBEL2'
        AND GS_TOTAL-VBEL2 IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_TOTAL-VBEL2.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&POST'.
    DATA L_ANS.
    DATA L_SUBRC TYPE SY-SUBRC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      TEXT_QUESTION         = '确定要保存并过账吗？'
      TEXT_BUTTON_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      ANSWER                = L_ANS
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK L_ANS EQ '1'.

  PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

  CHECK L_SUBRC EQ 0.

  "根据选中行过帐
  PERFORM POST_DATA.

    "刷新数据到内表
   CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
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

* ADD

FORM FRM_DATA_CHANGED USING   P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件



  CALL METHOD  GR_ALVGRID->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>MC_EVT_MODIFIED
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.



  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_modify FOR GR_ALVGRID.

ENDFORM.

FORM FRM_BUTTON  USING E_GRID TYPE SLIS_DATA_CALLER_EXIT.
  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.

   LS_F4-FIELDNAME = 'JZKM'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
 ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

   LS_F4-FIELDNAME = 'DFKM'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

 CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->HANDLE_ONF4   FOR GR_ALVGRID.

  CALL METHOD GR_ALVGRID->register_f4_for_fields
     EXPORTING
       it_f4  = lt_f4[].



ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_modify.
*    PERFORM refresh.
   DATA:LS_MOD_CELL TYPE LVC_S_MODI ,  "应用的修改的单元格
         stbl TYPE lvc_s_stbl.
*
*    LOOP AT gt_itab INTO wa_itab.
*      wa_itab-cc = wa_itab-bb * 2 .
*      MODIFY gt_itab FROM wa_itab.
*    ENDLOOP.
    SORT ER_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID.
    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
      AT NEW ROW_ID.
        READ TABLE GT_TOTAL INTO GS_TOTAL INDEX LS_MOD_CELL-ROW_ID.

      ENDAT.

      CASE LS_MOD_CELL-FIELDNAME.
         WHEN 'JZKM'."科目
           "获取指定单元格改动后内容
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
               EXPORTING
                  I_ROW_ID = LS_MOD_CELL-ROW_ID
                  I_FIELDNAME = 'JZKM'
               IMPORTING
                   E_VALUE  = GS_TOTAL-JZKM.
          "获取科目描述
           READ TABLE GT_SKAT
              INTO GS_SKAT
             WITH KEY SAKNR = GS_TOTAL-JZKM BINARY SEARCH .
            IF SY-SUBRC EQ 0 .
              GS_TOTAL-JZKM_TXT = GS_SKAT-TXT50.
            ENDIF.

          WHEN 'DFKM'.  "对方科目
            "获取指定单元格改动后内容
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
               EXPORTING
                  I_ROW_ID = LS_MOD_CELL-ROW_ID
                  I_FIELDNAME = 'DFKM'
               IMPORTING
                   E_VALUE  = GS_TOTAL-DFKM.
          "获取科目描述
           READ TABLE GT_SKAT
            INTO GS_SKAT
             WITH KEY SAKNR = GS_TOTAL-DFKM BINARY SEARCH .
            IF SY-SUBRC EQ 0 .
              GS_TOTAL-DFKM_TXT = GS_SKAT-TXT50.
            ENDIF.

          ENDCASE.
      "刷新数据到ALV
          MODIFY GT_TOTAL FROM GS_TOTAL INDEX LS_MOD_CELL-ROW_ID.
          CLEAR GS_TOTAL.

    ENDLOOP.
  "  *   稳定刷新
    stbl-row = 'X'." 基于行的稳定刷新
    stbl-col = 'X'." 基于列稳定刷新
    CALL METHOD GR_ALVGRID->refresh_table_display
      EXPORTING
        is_stable = stbl
      EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_onf4.
      FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.

   CASE e_fieldname.
     WHEN 'JZKM'.
        READ TABLE GT_TOTAL INTO GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_HKONT CHANGING GS_TOTAL-JZKM.
            IF GS_TOTAL-JZKM IS NOT INITIAL.
              MODIFY GT_TOTAL FROM GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'JZKM'.
               LW_MOD_CELL-VALUE = GS_TOTAL-JZKM.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.

    WHEN 'DFKM'.
         READ TABLE GT_TOTAL INTO GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_HKONT CHANGING GS_TOTAL-DFKM.
            IF GS_TOTAL-DFKM IS NOT INITIAL.
              MODIFY GT_TOTAL FROM GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'DFKM'.
               LW_MOD_CELL-VALUE = GS_TOTAL-DFKM.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.

   ENDCASE.

  "**  Inform ALV Grid that event 'onf4' has been processed
  er_event_data->M_EVENT_HANDLED = 'X'.           "告知F4动作结束
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.

  "判断是否已选中过账的号
  READ  TABLE GT_TOTAL INTO GS_TOTAL WITH KEY SEL = 'X'.
    IF SY-SUBRC NE 0.
      P_SUBRC = 4.
       MESSAGE '请选中相应的行再过账' TYPE 'E'.
    ENDIF.

 LOOP AT GT_TOTAL INTO GS_TOTAL WHERE SEL = 'X'.
       "CHECK 必填项


    "结转科目
     IF GS_TOTAL-JZKM IS INITIAL.
       MESSAGE TEXT-M03 TYPE 'S' DISPLAY LIKE 'E'.
        P_SUBRC = 4.
      ENDIF.

    "对方科目
     IF GS_TOTAL-DFKM IS INITIAL.
       MESSAGE TEXT-M04 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
     ENDIF.

    "凭证类型
     IF GS_TOTAL-PZLX IS INITIAL.
       MESSAGE TEXT-M05 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
     ENDIF.

     "过账日期
     IF GS_TOTAL-GZRQ IS INITIAL.
       MESSAGE TEXT-M05 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
     ENDIF.

     "过账文本
     CONDENSE GS_TOTAL-BKTXT NO-GAPS.
     IF GS_TOTAL-BKTXT IS INITIAL.
        MESSAGE TEXT-M06 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
      ENDIF.


 ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DATA .
    DATA: YZ TYPE i.
 REFRESH GT_ZCO005_1 .

  LOOP AT GT_TOTAL INTO GS_TOTAL WHERE SEL = 'X' AND  GZPZ = ''.


  IF  GS_TOTAL-YZCB EQ 0 .
    GS_TOTAL-INFO_MSG = '应转成本为0的记录不能生成过账凭证'.
    MODIFY GT_TOTAL FROM GS_TOTAL.
    CONTINUE.
  ELSE.
    "凭证行:项目虚拟行
    CLEAR:GS_TOTAL-INFO_MSG,YZ.
    LOOP AT GT_VBAP  WHERE VBELN = GS_TOTAL-VBEL2.
       CLEAR:YZ.
       IF GS_TOTAL-AUART = 'ZPO'.
          YZ = GT_VBAP-POSNR MOD 100 .
         IF YZ EQ 0 .
            GS_TOTAL-POSN2 = GT_VBAP-POSNR.
            EXIT.
           ELSE.
             CONTINUE.
         ENDIF.
       ENDIF.
        IF GS_TOTAL-AUART = 'ZSO'.
            YZ = GT_VBAP-POSNR MOD 10.
         IF YZ EQ 0 .
            GS_TOTAL-POSN2 = GT_VBAP-POSNR.
            EXIT.
           ELSE.
             CONTINUE.
         ENDIF.
       ENDIF.

         ENDLOOP.


      "付款记账数据抬头
      REFRESH: IT_ACCOUNTGL, it_accountgl, IT_CURRENCYAMOUNT, IT_CRITERIA, IT_VALUEFIELD, IT_EXTENSION2, IT_RETURN.
  CLEAR: WA_DOCUMENTHEADER, WA_OBJ.

  "* 抬头
**********************************************************************
*凭证日期
  WA_DOCUMENTHEADER-DOC_DATE     =  GS_TOTAL-GZRQ.
*过账日期
  WA_DOCUMENTHEADER-PSTNG_DATE   =  GS_TOTAL-GZRQ.
*凭证类型
  WA_DOCUMENTHEADER-DOC_TYPE     =  GS_TOTAL-PZLX.
*公司代码
  WA_DOCUMENTHEADER-COMP_CODE    =  GS_TOTAL-BUKRS.
*凭证抬头文本
  WA_DOCUMENTHEADER-HEADER_TXT   =  GS_TOTAL-BKTXT.
*创建人员
  WA_DOCUMENTHEADER-USERNAME     =  SY-UNAME.

   ENDIF.

 "凭证行
  "记账行
 " **********************************************************************

    CLEAR :WA_ZACCDOCUEXT.
    WA_ZACCDOCUEXT-POSNR = '0000000010'.
    IF GS_TOTAL-YZCB > 0 .
    WA_ZACCDOCUEXT-BSCHL =  '50'.
    ELSE.
      WA_ZACCDOCUEXT-BSCHL =  '40'.
    ENDIF.
    WA_EXTENSION2-STRUCTURE = 'ZACCDOCUEXT'.
    WA_EXTENSION2-VALUEPART1 = WA_ZACCDOCUEXT.
    APPEND WA_EXTENSION2 TO IT_EXTENSION2.

    WA_ZACCDOCUEXT-POSNR = '0000000020'.
     IF GS_TOTAL-YZCB > 0 .
    WA_ZACCDOCUEXT-BSCHL =  '40'.
    ELSE.
      WA_ZACCDOCUEXT-BSCHL =  '50'.
    ENDIF..
    WA_EXTENSION2-STRUCTURE = 'ZACCDOCUEXT'.
    WA_EXTENSION2-VALUEPART1 = WA_ZACCDOCUEXT.
    APPEND WA_EXTENSION2 TO IT_EXTENSION2.

    "科目表
   " **********************************************************************
    CLEAR:WA_ACCOUNTGL.
    WA_ACCOUNTGL-ITEMNO_ACC = '0000000010'.
    "结转科目
    WA_ACCOUNTGL-GL_ACCOUNT = GS_TOTAL-JZKM.
    "项目文本
    WA_ACCOUNTGL-ITEM_TEXT = GS_TOTAL-BKTXT.
    "销售订单
    WA_ACCOUNTGL-SALES_ORD = GS_TOTAL-VBEL2.
    WA_ACCOUNTGL-S_ORD_ITEM = GS_TOTAL-POSN2.
    "分配
    WA_ACCOUNTGL-ALLOC_NMBR = GS_TOTAL-VBEL2.

    APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL .

    "科目表
    CLEAR:WA_ACCOUNTGL.
    WA_ACCOUNTGL-ITEMNO_ACC = '0000000020'.
    "对方科目
    WA_ACCOUNTGL-GL_ACCOUNT = GS_TOTAL-DFKM.
    "项目文本
    WA_ACCOUNTGL-ITEM_TEXT = GS_TOTAL-BKTXT.

    "分配
    WA_ACCOUNTGL-ALLOC_NMBR = GS_TOTAL-VBEL2.

    APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL .

   "金额
    " **********************************************************************
   CLEAR WA_CURRENCYAMOUNT.
   WA_CURRENCYAMOUNT-ITEMNO_ACC = '0000000010'.
  " *       货币
      WA_CURRENCYAMOUNT-CURRENCY = GS_TOTAL-WAERS.
*       金额
  IF GS_TOTAL-YZCB > 0 .
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  * -1.
      ELSE.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  * -1  .
   ENDIF.

 APPEND WA_CURRENCYAMOUNT TO IT_CURRENCYAMOUNT.

 "金额
   CLEAR WA_CURRENCYAMOUNT.
   WA_CURRENCYAMOUNT-ITEMNO_ACC = '0000000020'.
  " *       货币
      WA_CURRENCYAMOUNT-CURRENCY = GS_TOTAL-WAERS.
*       金额
      IF GS_TOTAL-YZCB > 0 .
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  .
      ELSE.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB .
       ENDIF.

 APPEND WA_CURRENCYAMOUNT TO IT_CURRENCYAMOUNT.

* "获利能力段
* CLEAR WA_CRITERIA.
* WA_CRITERIA-ITEMNO_ACC =  '0000000010'.
* WA_CRITERIA-FIELDNAME = 'KAUFN' ."销售订单
* WA_CRITERIA-CHARACTER = GS_TOTAL-VBEL2 .
* APPEND WA_CRITERIA TO IT_CRITERIA.
* WA_CRITERIA-FIELDNAME = 'KDPOS'. "销售订单行项目
* WA_CRITERIA-CHARACTER = GS_TOTAL-POSN2.
* APPEND WA_CRITERIA TO IT_CRITERIA.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = WA_DOCUMENTHEADER
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
     OBJ_TYPE          = WA_OBJ-OBJ_TYPE
     OBJ_KEY           = WA_OBJ-OBJ_KEY
     OBJ_SYS           = WA_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL         = IT_ACCOUNTGL
  "    ACCOUNTRECEIVABLE = IT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      CURRENCYAMOUNT    = IT_CURRENCYAMOUNT
     " CRITERIA          = IT_CRITERIA
    "  VALUEFIELD        = IT_VALUEFIELD
*     EXTENSION1        =
      RETURN            = IT_RETURN
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      EXTENSION2        = IT_EXTENSION2
*     REALESTATE        =
*     ACCOUNTWT         =
    .

  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      "错误消息回写到ALV
      LOOP AT IT_RETURN INTO WA_RETURN .
        IF GS_TOTAL-INFO_MSG EQ ''.
           CONCATENATE WA_RETURN-ID WA_RETURN-TYPE WA_RETURN-MESSAGE INTO GS_TOTAL-INFO_MSG .
         ELSE .
           CONCATENATE GS_TOTAL-INFO_MSG ';' WA_RETURN-ID WA_RETURN-TYPE WA_RETURN-MESSAGE INTO GS_TOTAL-INFO_MSG .
        ENDIF.
     ENDLOOP.
      "红灯状态：
      GS_TOTAL-STATU = ICON_RED_LIGHT .
      MODIFY GT_TOTAL FROM GS_TOTAL.
      CONTINUE.

  ELSE.
     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    " 回写生成的会计凭证号与会计凭证年度
     GS_TOTAL-GZPZ   =  WA_OBJ-OBJ_KEY(10).
     GS_TOTAL-GZPZND =  WA_OBJ-OBJ_KEY+14(4).
       "绿灯状态：
      GS_TOTAL-STATU = ICON_GREEN_LIGHT.
      "消息描述
       GS_TOTAL-INFO_MSG = '凭证过账成功！'.
   "设置不可以编辑状态
   "*设置不可以编辑状态
   PERFORM FRM_CELL_STYLE USING 'YZCB'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'JZKM'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'DFKM'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
   PERFORM FRM_CELL_STYLE USING 'PZLX'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'GZRQ'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'BKTXT'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    "过程成功的行写到GT_ZCO005_1
    CLEAR :GS_ZCO005_1.
    MOVE-CORRESPONDING GS_TOTAL TO GS_ZCO005_1.
      "创建人、创建日期、更改人 、更改日期
    GS_ZCO005_1-GZ_NAME = SY-UNAME.
    GS_ZCO005_1-GZ_DATE = SY-DATUM.
    GS_ZCO005_1-GZ_TIME = SY-UZEIT.
*    GS_ZCO005_1-CX_NAME = SY-UNAME.
*    GS_ZCO005_1-CX_DATE = SY-DATUM.
    APPEND GS_ZCO005_1 TO GT_ZCO005_1.


  ENDIF.
  MODIFY GT_TOTAL FROM GS_TOTAL.
  ENDLOOP.

  "保存数据到ZCO005_1物理表
   IF GT_ZCO005_1 IS NOT INITIAL.
      MODIFY ZCO005_1 FROM TABLE GT_ZCO005_1.
   ENDIF.

*  "付款记账数据抬头、明细
*  PERFORM FRM_BAPI_DATA_PREP.
*
*  "调用记账BAPI
*"  PERFORM FRM_CALL_BAPI CHANGING
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text"付款记账数据抬头、明细
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_PREP .
*清空BAPI变量
  PERFORM FRM_BAPI_DATA_CLEAR.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text"调用记账BAPI
*----------------------------------------------------------------------*
*      <--P_ENDFORM  text
*----------------------------------------------------------------------*
FORM FRM_CALL_BAPI  CHANGING P_ENDFORM.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_CLEAR .
  REFRESH: IT_ACCOUNTGL, it_accountreceivable, IT_CURRENCYAMOUNT, IT_CRITERIA, IT_VALUEFIELD, IT_EXTENSION2, IT_RETURN.
  CLEAR: WA_DOCUMENTHEADER, WA_OBJ.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CELL_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2318   text
*      -->P_2319   text
*      <--P_GS_ITEM_CELLSTYLE  text
*----------------------------------------------------------------------*
FORM FRM_CELL_STYLE  USING    P_FIELDNAME
                              P_EDITABLE
                     CHANGING PT_CELLSTYLE TYPE LVC_T_STYL.
  DATA: LW_CELLSTYLE TYPE LVC_S_STYL.

  READ TABLE PT_CELLSTYLE INTO LW_CELLSTYLE WITH KEY FIELDNAME = P_FIELDNAME.
  IF SY-SUBRC = 0.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    MODIFY TABLE PT_CELLSTYLE FROM LW_CELLSTYLE.
  ELSE.
    LW_CELLSTYLE-FIELDNAME = P_FIELDNAME.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    INSERT LW_CELLSTYLE INTO TABLE PT_CELLSTYLE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_VBELN  text
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
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_TOTAL_HKONT  text
*----------------------------------------------------------------------*
FORM SUB_HELP_HKONT   CHANGING P_HKONT..
" SET PARAMETER ID 'BUK' FIELD WA_HEAD-BUKRS.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_HKONT
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
