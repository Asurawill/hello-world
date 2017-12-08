*&---------------------------------------------------------------------*
*& Report  ZFI004
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/07/27B
*& Request       :
*& Descriptions  : 进销存报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZFI028.
************************************************************************
* Tables
************************************************************************
TABLES:T001,T001W,MARA,MARD.

************************************************************************
* Type Declaration
************************************************************************
"MB5B获取数据用类型定义
TYPES : BEGIN OF STYPE_TOTALS_FLAT,
          MATNR        LIKE      MBEW-MATNR,
          MAKTX        LIKE      MAKT-MAKTX,
          BWKEY        LIKE      MBEW-BWKEY,
          WERKS        LIKE      MSEG-WERKS,
          CHARG        LIKE      MSEG-CHARG,
          SOBKZ        LIKE      MSLB-SOBKZ,
          NAME1        LIKE      T001W-NAME1,               "n999530
          START_DATE   LIKE      SY-DATLO,
          END_DATE     LIKE      SY-DATLO,
          ANFMENGE(09) TYPE P    DECIMALS 3,
          MEINS        LIKE      MARA-MEINS,
          SOLL(09)     TYPE P    DECIMALS 3,
          HABEN(09)    TYPE P    DECIMALS 3,
          ENDMENGE(09) TYPE P    DECIMALS 3.
TYPES:    ANFWERT(09)   TYPE P    DECIMALS 2,
          WAERS         LIKE T001-WAERS,
          SOLLWERT(09)  TYPE P    DECIMALS 2,
          HABENWERT(09) TYPE P    DECIMALS 2,
          ENDWERT(09)   TYPE P    DECIMALS 2,
          COLOR         TYPE      SLIS_T_SPECIALCOL_ALV,
          END OF STYPE_TOTALS_FLAT.

TYPES : BEGIN OF STYPE_MB5B_ADD,
          DUMMY(01) TYPE C,
        END OF STYPE_MB5B_ADD.

TYPES : BEGIN OF STYPE_MSEG_LEAN,
          MBLNR        LIKE      MKPF-MBLNR,
          MJAHR        LIKE      MKPF-MJAHR,
          VGART        LIKE      MKPF-VGART,
          BLART        LIKE      MKPF-BLART,
          BUDAT        LIKE      MKPF-BUDAT,
          CPUDT        LIKE      MKPF-CPUDT,
          CPUTM        LIKE      MKPF-CPUTM,
          USNAM        LIKE      MKPF-USNAM,
* process 'goods receipt/issue slip' as hidden field        "n450596
          XABLN        LIKE      MKPF-XABLN,                "n450596
          LBBSA        LIKE      T156M-LBBSA,
          BWAGR        LIKE      T156S-BWAGR,
          BUKRS        LIKE      T001-BUKRS,
          BELNR        LIKE      BKPF-BELNR,
          GJAHR        LIKE      BKPF-GJAHR,
          BUZEI        LIKE      BSEG-BUZEI,
          HKONT        LIKE      BSEG-HKONT,
          WAERS        LIKE      MSEG-WAERS,
          ZEILE        LIKE      MSEG-ZEILE,
          BWART        LIKE      MSEG-BWART,
          MATNR        LIKE      MSEG-MATNR,
          WERKS        LIKE      MSEG-WERKS,
          LGORT        LIKE      MSEG-LGORT,
          CHARG        LIKE      MSEG-CHARG,
          BWTAR        LIKE      MSEG-BWTAR,
          KZVBR        LIKE      MSEG-KZVBR,
          KZBEW        LIKE      MSEG-KZBEW,
          SOBKZ        LIKE      MSEG-SOBKZ,
          KZZUG        LIKE      MSEG-KZZUG,
          BUSTM        LIKE      MSEG-BUSTM,
          BUSTW        LIKE      MSEG-BUSTW,
          MENGU        LIKE      MSEG-MENGU,
          WERTU        LIKE      MSEG-WERTU,
          SHKZG        LIKE      MSEG-SHKZG,
          MENGE        LIKE      MSEG-MENGE,
          MEINS        LIKE      MSEG-MEINS,
          DMBTR        LIKE      MSEG-DMBTR,
          DMBUM        LIKE      MSEG-DMBUM,
          XAUTO        LIKE      MSEG-XAUTO,
          KZBWS        LIKE      MSEG-KZBWS,
          XOBEW        LIKE      MSEG-XOBEW,
          RETAIL(01)   TYPE      C,
          OIGLCALC(01) TYPE      C,
          OIGLSKU(07)  TYPE  P  DECIMALS 3,
          INSMK        LIKE      MSEG-INSMK,
          SMBLN        LIKE      MSEG-SMBLN,    " No. doc
          SJAHR        LIKE      MSEG-SJAHR,    " Year          "n497992
          SMBLP        LIKE      MSEG-SMBLP.    " Item in doc
        INCLUDE TYPE STYPE_MB5B_ADD.
TYPES : END OF STYPE_MSEG_LEAN.

TYPES : BEGIN OF STYPE_BELEGE,
          BWKEY LIKE MBEW-BWKEY.
        INCLUDE TYPE STYPE_MSEG_LEAN.
TYPES :   FARBE_PRO_FELD      TYPE      SLIS_T_SPECIALCOL_ALV,
          FARBE_PRO_ZEILE(03) TYPE C.
TYPES : END OF STYPE_BELEGE.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA: S_DATUM  TYPE RANGE OF SY-DATUM WITH HEADER LINE,
      SS_LGORT TYPE RANGE OF MSKA-LGORT WITH HEADER LINE.

DATA: S_BUKRS TYPE RANGE  OF T001-BUKRS WITH HEADER LINE.

*抛内存给MB5B用变量
DATA: MATNR    TYPE MARA-MATNR,
      BUKRS    TYPE T001-BUKRS,
      WERKS    TYPE T001W-WERKS,
      DATUM    LIKE SY-DATUM,
      BWBST    LIKE AM07M-BWBST,
      PA_SUMFL LIKE AM07M-XSUM,
      PA_DBSTD LIKE AM07M-XSELK.

*从MB5B获取汇总数据用内表和工作区
DATA: G_T_TOTALS_FLAT TYPE STANDARD TABLE OF STYPE_TOTALS_FLAT,
      G_S_TOTALS_FLAT LIKE LINE OF G_T_TOTALS_FLAT.

"从MB5B获取物料移动详细数据用内表和工作区
DATA: G_T_BELEGE1 TYPE STANDARD TABLE OF STYPE_BELEGE,
      G_S_BELEGE1 LIKE LINE OF G_T_BELEGE1,
      G_T_BELEGE  TYPE STANDARD TABLE OF STYPE_BELEGE,
      G_S_BELEGE  LIKE LINE OF G_T_BELEGE.

"将从MB5B获取到的数据存到自建表所用内表
DATA: GT_ZFI035K     TYPE STANDARD TABLE OF ZFI035K,
      GW_ZFI035K     LIKE LINE OF GT_ZFI035K,
      GT_ZFI035K_SUM TYPE STANDARD TABLE OF ZFI035K,
      GW_ZFI035K_SUM LIKE LINE OF GT_ZFI035K_SUM,
      GT_ZFI035P     TYPE STANDARD TABLE OF ZFI035P,
      GW_ZFI035P     LIKE LINE OF GT_ZFI035P.

"汇总信息展示用变量
DATA: GT_FIELDCAT_K TYPE STANDARD TABLE OF SLIS_FIELDCAT_ALV,
      GW_FIELDCAT_K LIKE LINE OF GT_FIELDCAT_K,
      GW_LAYOUT_K   TYPE SLIS_LAYOUT_ALV.

"库存信息汇总用内表
DATA: GT_ZFI035C     TYPE STANDARD TABLE OF ZFI035C,
      GW_ZFI035C     LIKE LINE OF GT_ZFI035C,
      GW_ZFI035C_TMP LIKE LINE OF GT_ZFI035C.

DATA: GT_MBEW TYPE STANDARD TABLE OF MBEW,
      GW_MBEW LIKE LINE OF GT_MBEW.

DATA: GT_T156 TYPE STANDARD TABLE OF T156,
      GW_T156 LIKE LINE OF GT_T156.

DATA: GT_BSEG TYPE STANDARD TABLE OF BSEG,
      GW_BSEG LIKE LINE OF GT_BSEG.
************************************************************************
*      DEFINITION
************************************************************************
DEFINE SET_FIELDCAT.      "  ALV Fieldcat Setting
  CLEAR &1.
  &1-FIELDNAME = &3.
  IF &1-FIELDNAME = 'MATNR'
  OR &1-FIELDNAME = 'MAKTX'
  OR &1-FIELDNAME = 'BUKRS'
  OR &1-FIELDNAME = 'WERKS'
  OR &1-FIELDNAME = 'MVGRP'
  OR &1-FIELDNAME = 'LGORT'
  OR &1-FIELDNAME = 'CHARG'.
    &1-KEY = 'X'.
    &1-NO_ZERO = 'X'.
  ENDIF.
  &1-FIX_COLUMN = 'X'.
  &1-SELTEXT_L = &4.
  &1-SELTEXT_M = &1-SELTEXT_L.
  &1-SELTEXT_S = &1-SELTEXT_L.
  APPEND &1 TO &2.
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

DATA: OK_CODE  LIKE SY-UCOMM,
      GV_TIMES TYPE N LENGTH 6.

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************


SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_BUKRS TYPE T001-BUKRS OBLIGATORY. "公司代码

SELECT-OPTIONS: S_WERKS FOR T001W-WERKS OBLIGATORY. "工厂
SELECT-OPTIONS: S_LGORT FOR MARD-LGORT,             "库位
                S_MATNR FOR MARA-MATNR.             "物料
*                S_MATKL FOR MARA-MATKL,             "物料组
*                S_BKLAS FOR MBEW-BKLAS,             "评估类
*                S_KONTS FOR T030-KONTS.             "科目
PARAMETERS:     P_STDAT TYPE ABPER_RF OBLIGATORY,   "开始期间
                P_EDDAT TYPE ABPER_RF OBLIGATORY.   "结束期间
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN PUSHBUTTON 1(10) PB_QS USER-COMMAND QS.
*SELECTION-SCREEN PUSHBUTTON 15(10) PB_HT USER-COMMAND HT.

INITIALIZATION.
  MOVE TEXT-001 TO PB_QS."取数按钮
*  MOVE TEXT-005 TO PB_HT."后台作业按钮


AT SELECTION-SCREEN.
*  PERFORM FRM_CHECK_PLANT.
  CASE SY-UCOMM.
    WHEN 'QS'.
      PERFORM FRM_EXPORT_DATA.  "抛内存给MB5B
      PERFORM FRM_IMPORT_DATA.  "从MB5B获取结果
*    WHEN 'HT'.
*      PERFORM FRM_JOB.
  ENDCASE.

START-OF-SELECTION.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA_K.
  PERFORM FRM_SET_FIELDCAT_K.
  PERFORM FRM_ALV_DISPLAY_K.
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FRM_EXPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXPORT_DATA.
  DATA: L_DATUM LIKE SY-DATUM.
  CLEAR: S_DATUM,S_DATUM[].

  CONCATENATE P_STDAT '01' INTO S_DATUM-LOW.
  CONCATENATE P_EDDAT '01' INTO S_DATUM-HIGH.
  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = S_DATUM-HIGH
    IMPORTING
      E_DATE = S_DATUM-HIGH.
  S_DATUM-SIGN = 'I'.
  S_DATUM-OPTION = 'BT'.
  APPEND S_DATUM.

  BWBST = 'X'.
  PA_SUMFL = 'X'.
  PA_DBSTD = 'X'.

  CLEAR S_BUKRS.
  S_BUKRS-SIGN = 'I'.
  S_BUKRS-OPTION = 'EQ'.
  S_BUKRS-LOW = P_BUKRS.
  APPEND S_BUKRS.
  "汇总
**********************************************
*  EXPORT A = S_MATNR
*         B = S_BUKRS
*         C = S_WERKS
*         D = S_DATUM[]
*         E = BWBST
*         F = PA_SUMFL
*         G = PA_DBSTD TO MEMORY ID 'MB5B'.
*  SUBMIT RM07MLBD AND RETURN.
**********************************************
  SUBMIT RM07MLBD WITH BUKRS = P_BUKRS
                  WITH MATNR IN S_MATNR[]
                  WITH WERKS IN S_WERKS[]
                  WITH DATUM IN S_DATUM[]
                  WITH LGBST = ''
                  WITH BWBST = 'X'
                  WITH SBBST = ''
                  WITH PA_SUMFL = 'X'
                  WITH PA_WDZER = 'X'
                  WITH PA_WDZEW = 'X'
                  WITH PA_WDWIZ = 'X'
                  WITH PA_WDWUW = 'X'
                  WITH PA_WDWEW = 'X'
                  WITH PA_NDZER = 'X'
                  WITH PA_NDSTO = 'X'
                  WITH PA_DBSTD = 'X'
                  AND RETURN.
************************d**************************
*  CLEAR PA_SUMFL.
*  "明细
*  EXPORT A = S_MATNR
*         B = S_BUKRS
*         C = S_WERKS
*         D = S_DATUM[]
*         E = BWBST
*         F = PA_SUMFL
*         G = PA_DBSTD TO MEMORY ID 'MB5B'.
*  SUBMIT RM07MLBD AND RETURN.
************************d**************************
  SUBMIT RM07MLBD WITH BUKRS = P_BUKRS
                  WITH MATNR IN S_MATNR[]
                  WITH WERKS IN S_WERKS[]
                  WITH DATUM IN S_DATUM[]
                  WITH LGBST = ''
                  WITH BWBST = 'X'
                  WITH SBBST = ''
*                  WITH BWBST = 'X'
                  WITH PA_WDZER = 'X'
                  WITH PA_WDZEW = 'X'
                  WITH PA_WDWIZ = 'X'
                  WITH PA_WDWUW = 'X'
                  WITH PA_WDWEW = 'X'
                  WITH PA_NDZER = 'X'
                  WITH PA_NDSTO = 'X'
                  WITH PA_SUMFL = ''
                  WITH PA_DBSTD = 'X'
                  AND RETURN.
*  LW_ZFI035T-UZEIT_P_E = SY-UZEIT.
ENDFORM.                    " FRM_EXPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_IMPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_IMPORT_DATA .
  DATA: L_TIMES  TYPE N LENGTH 6 VALUE 0,
        L_STRING TYPE C LENGTH 20.

  IF S_MATNR IS NOT INITIAL.
    IMPORT A = G_T_TOTALS_FLAT FROM MEMORY ID 'ZMMR035_A'.
    IMPORT B = G_T_BELEGE1 FROM MEMORY ID 'ZMMR035_B'.
    FREE MEMORY ID 'ZMMR035_A'.
    FREE MEMORY ID 'ZMMR035_B'.
  ELSE.
    IMPORT A = G_T_TOTALS_FLAT FROM MEMORY ID 'ZMMR035_A'.
    FREE MEMORY ID 'ZMMR035_A'.
    IMPORT C = GV_TIMES FROM MEMORY ID 'ZMMR035_TIMES'.
    FREE MEMORY ID 'ZMMR035_TIMES'.
    DO GV_TIMES TIMES.
      L_TIMES = L_TIMES + 1.
      CONCATENATE 'ZMMR035_TIMES_' L_TIMES INTO L_STRING.
      IMPORT G_T_BELEGE FROM MEMORY ID L_STRING.
      FREE MEMORY ID L_STRING.
      LOOP AT G_T_BELEGE INTO G_S_BELEGE.
        APPEND G_S_BELEGE TO G_T_BELEGE1.
      ENDLOOP.
    ENDDO.
  ENDIF.

BREAK HAND.
*
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
  DATA: GT_ZFI035T LIKE GT_ZFI035P.
  CLEAR: GT_ZFI035K,GT_ZFI035P,GT_ZFI035C,GT_MBEW,GT_ZFI035T.
  SELECT *
    FROM ZFI035K
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI035K
    WHERE MATNR IN S_MATNR
      AND WERKS IN S_WERKS
*      AND MATKL IN S_MATKL
*      AND BKLAS IN S_BKLAS
*      AND KONTS IN S_KONTS
      AND LGORT IN S_LGORT
      AND STDAT = P_STDAT
      AND EDDAT = P_EDDAT.

  SELECT *
    FROM ZFI035P AS A
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI035P
    WHERE A~MATNR IN S_MATNR
      AND A~BUKRS = P_BUKRS
      AND A~WERKS IN S_WERKS
      AND A~LGORT IN S_LGORT
*      AND A~KONTS IN S_KONTS
      AND STDAT = P_STDAT
      AND EDDAT = P_EDDAT.

  SELECT *
    FROM ZFI035C AS A
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI035C
    WHERE A~MATNR IN S_MATNR
      AND A~BUKRS IN S_BUKRS
      AND A~WERKS IN S_WERKS
      AND A~LGORT IN S_LGORT
      AND ( STDAT = P_STDAT OR EDDAT = P_EDDAT ).

  IF NOT GT_ZFI035P IS INITIAL.
    SELECT
      BWART
      XSTBW
      FROM T156
      INTO CORRESPONDING FIELDS OF TABLE GT_T156
      FOR ALL ENTRIES IN GT_ZFI035P
      WHERE BWART = GT_ZFI035P-BWART.

    GT_ZFI035T = GT_ZFI035P.
    SORT GT_ZFI035T BY BUKRS BELNR GJAHR BUZEI.
    DELETE ADJACENT DUPLICATES FROM GT_ZFI035T COMPARING BUKRS BELNR GJAHR BUZEI.

    SELECT
      BUKRS
      BELNR
      GJAHR
      BUZEI
      XNEGP
      FROM BSEG
      INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
      FOR ALL ENTRIES IN GT_ZFI035T
      WHERE BUKRS = GT_ZFI035T-BUKRS
        AND BELNR = GT_ZFI035T-BELNR
        AND GJAHR = GT_ZFI035T-GJAHR
        AND BUZEI = GT_ZFI035T-BUZEI.

    SELECT *
      FROM MBEW
      INTO CORRESPONDING FIELDS OF TABLE GT_MBEW
      FOR ALL ENTRIES IN GT_ZFI035P
      WHERE MATNR = GT_ZFI035P-MATNR
        AND BWKEY = GT_ZFI035P-WERKS.
  ENDIF.

  IF GT_ZFI035C IS NOT INITIAL.
    SELECT *
      FROM MBEW
      APPENDING CORRESPONDING FIELDS OF TABLE GT_MBEW
      FOR ALL ENTRIES IN GT_ZFI035C
      WHERE MATNR = GT_ZFI035C-MATNR
        AND BWKEY = GT_ZFI035C-WERKS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA_K
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA_K .
  SORT GT_T156 BY BWART.
  SORT GT_BSEG BY BUKRS BELNR GJAHR BUZEI.

*汇总借贷数量和金额
  LOOP AT GT_ZFI035P INTO GW_ZFI035P.
    CLEAR: GW_ZFI035K_SUM,GW_T156.

    IF GW_ZFI035P-LGORT NOT IN S_LGORT.
      DELETE TABLE GT_ZFI035P FROM GW_ZFI035P.
      CONTINUE.
    ENDIF.

    GW_ZFI035K_SUM-MATNR = GW_ZFI035P-MATNR.
    GW_ZFI035K_SUM-WERKS = GW_ZFI035P-WERKS.
    GW_ZFI035K_SUM-KONTS = GW_ZFI035P-KONTS.
    GW_ZFI035K_SUM-TXT50 = GW_ZFI035P-TXT50.

    READ TABLE GT_T156 INTO GW_T156 WITH KEY
    BWART = GW_ZFI035P-BWART BINARY SEARCH.
    IF GW_ZFI035P-BWART IS NOT INITIAL.
      IF ( GW_ZFI035P-SHKZG = 'S' AND GW_T156-XSTBW <> 'X' )
      OR ( GW_ZFI035P-SHKZG = 'H' AND GW_T156-XSTBW = 'X').
        GW_ZFI035K_SUM-IMENG = GW_ZFI035P-MENGE.
        GW_ZFI035K_SUM-IDMBT = GW_ZFI035P-DMBTR.
      ELSEIF ( GW_ZFI035P-SHKZG = 'H' AND GW_T156-XSTBW <> 'X' )
      OR ( GW_ZFI035P-SHKZG = 'S' AND GW_T156-XSTBW = 'X').
        GW_ZFI035K_SUM-OMENG = GW_ZFI035P-MENGE.
        GW_ZFI035K_SUM-ODMBT = GW_ZFI035P-DMBTR.
      ENDIF.
    ELSE.
      CLEAR GW_BSEG.
      READ TABLE GT_BSEG INTO GW_BSEG WITH KEY
      BUKRS = GW_ZFI035P-BUKRS
      BELNR = GW_ZFI035P-BELNR
      GJAHR = GW_ZFI035P-GJAHR
      BUZEI = GW_ZFI035P-BUZEI BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF ( GW_ZFI035P-SHKZG = 'S' AND GW_BSEG-XNEGP <> 'X' )
          OR ( GW_ZFI035P-SHKZG = 'H' AND GW_BSEG-XNEGP = 'X' ) .
          GW_ZFI035K_SUM-IMENG = GW_ZFI035P-MENGE.
          GW_ZFI035K_SUM-IDMBT = GW_ZFI035P-DMBTR.
        ELSEIF ( GW_ZFI035P-SHKZG = 'H' AND GW_BSEG-XNEGP <> 'X' )
          OR ( GW_ZFI035P-SHKZG    = 'S' AND GW_BSEG-XNEGP = 'X' ) .
          GW_ZFI035K_SUM-OMENG = GW_ZFI035P-MENGE.
          GW_ZFI035K_SUM-ODMBT = GW_ZFI035P-DMBTR.
        ENDIF.
      ENDIF.
    ENDIF.

    COLLECT GW_ZFI035K_SUM INTO GT_ZFI035K_SUM.
  ENDLOOP.

*更新汇总表中的借贷数量和金额
  SORT GT_ZFI035K_SUM BY MATNR WERKS.
  LOOP AT GT_ZFI035K INTO GW_ZFI035K.
    CLEAR GW_ZFI035K_SUM.
    READ TABLE GT_ZFI035K_SUM INTO GW_ZFI035K_SUM WITH KEY
    MATNR = GW_ZFI035K-MATNR
    WERKS = GW_ZFI035K-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GW_ZFI035K-IMENG = GW_ZFI035K_SUM-IMENG.
      GW_ZFI035K-IDMBT = GW_ZFI035K_SUM-IDMBT.
      GW_ZFI035K-OMENG = GW_ZFI035K_SUM-OMENG * -1.
      GW_ZFI035K-ODMBT = GW_ZFI035K_SUM-ODMBT * -1.
    ENDIF.
    MODIFY GT_ZFI035K FROM GW_ZFI035K.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT_K
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_FIELDCAT_K .
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'BKLAS' '评估类'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'KONTS' '科目'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'TXT50' '科目描述'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'MATNR' '物料'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'MAKTX' '物料描述'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'MATKL' '物料组'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'WERKS' '工厂'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'SMENG' '期初数量'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'MEINS' '单位'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'SDMBT' '期初金额'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'WAERS' '货币'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'IMENG' '期间入库总数'.
*  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'IDMBT' '期间入库总金额'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'OMENG' '期间出库总数'.
*  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'ODMBT' '期间出库总金额'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'EMENG' '期末数量'.
  SET_FIELDCAT GW_FIELDCAT_K GT_FIELDCAT_K  'EDMBT' '期末金额'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY_K
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY_K .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'FRM_SET_STATUS_K'
      I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND_K'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = GW_LAYOUT_K
      IT_FIELDCAT              = GT_FIELDCAT_K
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_ZFI035K
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM FRM_SET_STATUS_K USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'PF_STATUS'.
ENDFORM.
