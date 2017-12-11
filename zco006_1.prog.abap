REPORT ZCO006_1.
************************************************************************
* TABLES
************************************************************************
TABLES:BKPF,BSEG,VBAK.

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
     POSN2 TYPE BSEG-POSN2 ,"销售凭证行号
     VBUND  TYPE BSEG-VBUND,"贸易伙伴
     END OF GS_DATA.
 DATA: BEGIN OF GS_TOTAl,
     YWLX TYPE STRING, "业务类型
     BUKRS TYPE BKPF-BUKRS,  "公司代码
     GJAHR TYPE BKPF-GJAHR, "会计年度
     VBELN  TYPE VBELN,  "销售订单
     AUART  TYPE AUART,"订单类型
     XMMC   TYPE STRING, "项目名称
     ZYYWCB TYPE TSLVT12,"主营业务成本
     ZYYWSR_01 TYPE TSLVT12, "主营业务收入-本币
     ZYYWSR_02 TYPE TSLVT12,"主营业务收入-凭证货币
     WAERS_1  TYPE WAERS,"收入货币
     GCCB  TYPE  TSLVT12,"工程成本
     GCCB_QCYE TYPE TSLVT12,"工程成本-期初余额
     CPBZCB TYPE TSLVT12,"产品标准成本
     CPCBCY TYPE TSLVT12,"产品成本差异
     GCFY  TYPE TSLVT12,"工程费用
     MLV  TYPE TSLVT12,"毛利率
     MLV01 TYPE STRING,"毛利率
     DDJE TYPE TSLVT12,"订单金额
     DDHB TYPE STRING,"订单货币
     KUNNR TYPE KUNNR,"客户编码
     KUNNR_NAME1 TYPE STRING,"客户名称
     YSQJ TYPE N  LENGTH 6,"验收期间
     YSKPZHB TYPE TSLVT12,"已收款-凭证货币
     YKJSPPZHB TYPE TSLVT12,"已开金税票-凭证货币
     YSKYE  TYPE TSLVT12 ,"应收款余额-凭证货币
     ZHFHRQ  TYPE D ,"最后发货日期
     SEL(1),
       END OF GS_TOTAL.
 DATA: BEGIN OF GS_VBFA_LIKP,
     VBELV TYPE VBELN_VON,"销售订单号
     POSNV TYPE POSNR_VON,
     VBELN  TYPE VBELN_NACH,"交换单号
     POSNN  TYPE POSNR_NACH,
     VBTYP_N TYPE VBTYP_N,
     WADAT_IST TYPE WADAT_IST,"最后发货日期

   END OF GS_VBFA_LIKP.
 DATA:BEGIN OF GS_DD_SUM,
     VBELN TYPE VBELN,"销售订单
     DMBTR TYPE TSLVT12,"本位币总和
   END OF GS_DD_SUM.
 DATA: GT_BZCB_XM LIKE TABLE OF GS_DATA WITH HEADER LINE.  "产品标准成本-项目
 DATA:GT_BZCB_CP LIKE TABLE OF GS_DATA WITH HEADER LINE."产品标准成本-产品
 "DATA: GT_BZCB_XM_CP LIKE TABLE OF GS_DATA WITH HEADER LINE."产品标准成本-产品/项目
 DATA:GT_CBCY_XM LIKE TABLE OF GS_DATA WITH HEADER  LINE."产品成本-项目
 DATA: GT_CBCY_CP LIKE TABLE OF GS_DATA WITH HEADER LINE."产品成本-产品
 DATA: GT_GCYF_XM LIKE TABLE OF GS_DATA WITH HEADER LINE."工程费用-项目
DATA: GT_BZCB_XM_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER LINE.  "产品标准成本-项目
DATA:GT_BZCB_CP_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER LINE."产品标准成本-产品
 "DATA:GT_BZCB_XM_CP_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER LINE."产品标准成本-产品
 DATA:GT_CBCY_XM_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER  LINE."产品成本-项目
 DATA: GT_CBCY_CP_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER LINE."产品成本-产品
 DATA: GT_GCYF_XM_SUM LIKE TABLE OF GS_DD_SUM WITH HEADER LINE."工程费用-项目
 DATA:  GT_VBFA_LIKP LIKE TABLE OF GS_VBFA_LIKP WITH HEADER LINE .
 DATA:  GT_MX1 LIKE TABLE OF GS_DATA WITH HEADER LINE. "明细1
 DATA:  GT_MX2 LIKE TABLE OF GS_DATA WITH HEADER LINE. "明细2
 DATA:  GT_MX3 LIKE TABLE OF GS_DATA WITH HEADER LINE. "明细3
 DATA:  GT_MX3_01 LIKE TABLE OF GS_DATA WITH HEADER LINE ."明细3-1
 DATA:  GT_MX4 LIKE TABLE OF GS_DATA WITH HEADER LINE. "明细4
 DATA:  GT_MX4_01 LIKE TABLE OF GS_DATA WITH HEADER LINE. "明细4_1
 DATA:  GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.
 DATA:  GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.
 DATA:  GT_KNA1 LIKE TABLE OF KNA1 WITH HEADER LINE.
 DATA:  GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.
 DATA:  GT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.
* DATA:  GT_VBRK_ZUONR LIKE TABLE OF VBRK WITH HEADER LINE.
* DATA:  GT_VBRK_XBLNR LIKE TABLE OF VBRK WITH HEADER LINE.
 DATA:  GT_TOTAL LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_XM LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_XM01 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_XM02 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_XM03 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_XM04 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_CP LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_CP01 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_CP02 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_CP03 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_CP04 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_GSJ LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_GSJ01 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_GSJ02 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_QT LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_QT01 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_QT02 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_QT03 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_APPEND  LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
* DATA:  GT_TOTAL_MX4 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
* DATA:  GT_TOTAL_MX4_01 LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_VBELN LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_VBELN_ALL LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
" DATA:  GT_TOTAL  LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_SUM LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_TOTAL_SUM_SEL LIKE TABLE OF GS_TOTAL WITH HEADER LINE.
 DATA:  GT_BSEG  LIKE TABLE OF BSEG WITH HEADER LINE.
 DATA:  GT_VBFA_MN LIKE TABLE OF VBFA  WITH HEADER LINE.
 DATA:  GT_VBRK_FKDAT LIKE TABLE OF VBRK  WITH HEADER LINE.
 DATA:  GT_VBAP  LIKE TABLE OF VBAP  WITH HEADER LINE.
 FIELD-SYMBOLS: <FS_DATA> LIKE GS_DATA.
  FIELD-SYMBOLS: <FS_MX1> LIKE GS_DATA,
                 <FS_MX2> LIKE GS_DATA,
                 <FS_MX3> LIKE GS_DATA,
                 <FS_MX3_01> LIKE GS_DATA,
                 <FS_MX4> LIKE GS_DATA,
                 <FS_MX4_01> LIKE GS_DATA,
                 <FS_TOTAL_VBELN> LIKE GS_TOTAL,
                 <FS_TOTAL> LIKE GS_TOTAL,
                 <FS_TOTAL_SUM> LIKE  GS_TOTAL.
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
  IF &1 = 'ZYYWSR_02' OR &1 = 'YSKPZHB' OR &1 = 'YKJSPPZHB' OR  &1 = 'YSKYE'  .
   GW_LVC-CFIELDNAME = 'WAERS_1'.
  ENDIF.
  IF &1 = 'DDJE'.
   GW_LVC-CFIELDNAME = 'DDHB'.
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
DATA:P_DATE TYPE D.
DATA:SEL_DATE_LEN TYPE I .
 RANGES:R_HKONT FOR BSEG-HKONT.
 RANGES:R2_HKONT FOR BSEG-HKONT.
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
SELECT-OPTIONS: S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,    "过账日期
                S_VBELN  FOR VBAK-VBELN.   "销售订单

SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
          P_1 AS CHECKBOX DEFAULT 'X',
          P_2 AS CHECKBOX DEFAULT 'X',
          P_3 AS CHECKBOX DEFAULT 'X',
          P_4 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BLK2.
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
R2_HKONT-SIGN = 'I'.
R2_HKONT-OPTION = 'BT'.
R2_HKONT-LOW = '6001000000'.
R2_HKONT-HIGH = '6001999999'.
APPEND R2_HKONT.
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
  "检查过账日期选择区间
  DESCRIBE TABLE S_BUDAT LINES SEL_DATE_LEN .
  IF SEL_DATE_LEN  > 1.
    MESSAGE '请选择一个区间段查询' TYPE 'S' DISPLAY LIKE 'E'.
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
IF S_VBELN IS NOT INITIAL.
  CLEAR : P_3 ,P_4."选择销售订单 不为空 ，就不处理 公司间采购、其他
ENDIF.
IF P_1 = 'X' OR  P_2 = 'X'  OR P_3 = 'X' OR  P_4 = 'X'.
   PERFORM SELGT_MX1. "查询明细表1
   PERFORM SELGT_MX2. "查询明细表2


ENDIF.
IF P_1 = 'X' OR  P_2 = 'X'   OR  P_4 = 'X'.
   PERFORM SELGT_MX3. "查询明细表3
ENDIF.

IF P_1 = 'X' OR  P_2 = 'X' .
   PERFORM SELGT_MX4. "查询明细表4
ENDIF.

 IF P_1 NE '' OR  P_2 NE '' OR P_3 NE '' OR P_4 NE ''.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK FROM VBAK  .
  SORT  GT_VBAK BY VBELN.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FROM VBRK  .
*  SORT  GT_VBRK BY VBELN.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_KNA1 FROM KNA1.
  SORT GT_KNA1 BY KUNNR .
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
  DATA: IS_DD TYPE STRING .
    CLEAR:P_DATE.
    P_DATE = S_BUDAT-LOW.
  IF S_BUDAT-HIGH IS NOT INITIAL.
    P_DATE = S_BUDAT-HIGH.
  ENDIF.

  IF GT_MX1[] IS NOT INITIAL.
    LOOP AT GT_MX1 ASSIGNING <FS_MX1>.
   IF <FS_MX1>-SHKZG EQ 'H'.
      <FS_MX1>-DMBTR =  <FS_MX1>-DMBTR * -1 .

    ENDIF.
 "   IF P_1 EQ 'X'.

      IF <FS_MX1>-BLART = 'SC'.
        <FS_MX1>-YWLX = '项目'.
        "读取销售订单
       IF P_1 EQ 'X'.
          SPLIT <FS_MX1>-ZUONR  AT '/' INTO <FS_MX1>-VBELN VBELN_TMP.
       IF <FS_MX1>-VBELN IN S_VBELN . "先根据屏幕销售订单作为条件筛选
          CLEAR GT_TOTAL_XM .
          GT_TOTAL_XM-YWLX =  <FS_MX1>-YWLX .  "业务类型       ”GT_TOTAL_XM 汇总明细表1项目 的 主营业务成本
          GT_TOTAL_XM-BUKRS = P_BUKRS.          "公司代码
          GT_TOTAL_XM-VBELN = <FS_MX1>-VBELN .  "销售订单
          GT_TOTAL_XM-ZYYWCB = <FS_MX1>-DMBTR.   "主营业务成本
          COLLECT GT_TOTAL_XM.
          CONTINUE.
        ELSE.
          CONTINUE.
        ENDIF.
       ENDIF.


       ENDIF.

   "   ENDIF.
   "   IF P_2 EQ 'X'.
       IF <FS_MX1>-BLART = 'RV'.
         "判读类型 产品还是产品订单 、项目订单
        "  PERFORM  IS_CPLX  USING GT_MX1-XBLNR CHANGING  IS_DD .
          READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX1>-XBLNR BINARY SEARCH.
           IF SY-SUBRC = 0.
             IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
                <FS_MX1>-YWLX = '产品'.
                IF P_2 EQ 'X'.
                    "销售订单
                  <FS_MX1>-VBELN = <FS_MX1>-XBLNR .
                  IF <FS_MX1>-VBELN IN S_VBELN . "先根据屏幕销售订单作为条件筛选
                    CLEAR GT_TOTAL_CP.
                   GT_TOTAL_CP-YWLX =  <FS_MX1>-YWLX .  "业务类型
                   GT_TOTAL_CP-BUKRS = P_BUKRS.          "公司代码      ”GT_TOTAL_CP 汇总明细表1产品 的 主营业务成本
                   GT_TOTAL_CP-VBELN = <FS_MX1>-VBELN .   "销售订单
                   GT_TOTAL_CP-ZYYWCB = <FS_MX1>-DMBTR.   "主营业务成本
                   COLLECT GT_TOTAL_CP.
                   CONTINUE.
                  ELSE.
                     CONTINUE.
                ENDIF.
              ENDIF.

               ENDIF.
           ENDIF.
        ENDIF.
        IF <FS_MX1>-BLART = 'ML'.
         "判读类型 产品还是产品订单 、项目订单
        "  PERFORM  IS_CPLX  USING GT_MX1-XBLNR CHANGING  IS_DD .
          READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX1>-ZUONR BINARY SEARCH.
           IF SY-SUBRC = 0.
             IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
                <FS_MX1>-YWLX = '产品'.
                IF P_2 EQ 'X'.
                     "销售订单
                  <FS_MX1>-VBELN = <FS_MX1>-ZUONR .
                 IF <FS_MX1>-VBELN IN S_VBELN .
                  CLEAR GT_TOTAL_CP.
                  GT_TOTAL_CP-YWLX =  <FS_MX1>-YWLX .    "业务类型
                  GT_TOTAL_CP-BUKRS = P_BUKRS.            "公司代码    ”GT_TOTAL_CP 汇总明细表1产品 的 主营业务成本
                  GT_TOTAL_CP-VBELN = <FS_MX1>-VBELN .    ""销售订单
                  GT_TOTAL_CP-ZYYWCB = <FS_MX1>-DMBTR.     ""主营业务成本
                  COLLECT GT_TOTAL_CP.
                  CONTINUE.
                   ELSE.
                     CONTINUE.
                   ENDIF.

                 ENDIF.

               ENDIF.
           ENDIF.
        ENDIF.

   "   ENDIF.
 "   IF P_3 EQ 'X'.
       IF <FS_MX1>-BLART = 'WL' AND <FS_MX1>-EBELN NE  ''.
          <FS_MX1>-YWLX = '公司间'.
          IF P_3 EQ 'X'.
               CLEAR GT_TOTAL_GSJ.
              GT_TOTAL_GSJ-YWLX =  <FS_MX1>-YWLX .            "业务类型
              GT_TOTAL_GSJ-BUKRS = P_BUKRS.                   "公司代码     ”GT_TOTAL_GSJ 汇总明细表1公司间的 主营业务成本
              GT_TOTAL_GSJ-ZYYWCB = <FS_MX1>-DMBTR.            ""主营业务成本
              COLLECT GT_TOTAL_GSJ.
               CONTINUE.
           ENDIF.

       ENDIF.
          IF <FS_MX1>-BLART = 'ML' AND <FS_MX1>-AWTYP  EQ 'MLHD'  AND <FS_MX1>-HKONT EQ '6401020101'.
          <FS_MX1>-YWLX = '公司间'.
            IF P_3 EQ 'X'.
                  CLEAR GT_TOTAL_GSJ.
                  GT_TOTAL_GSJ-YWLX =  <FS_MX1>-YWLX .           "业务类型
                  GT_TOTAL_GSJ-BUKRS = P_BUKRS.                  ""公司代码   ”GT_TOTAL_GSJ 汇总明细表1公司间的 主营业务成本
                  GT_TOTAL_GSJ-ZYYWCB = <FS_MX1>-DMBTR.           " ""主营业务成本
                  COLLECT GT_TOTAL_GSJ.
                  CONTINUE.

             ENDIF.

       ENDIF.

 "   ENDIF.
    IF P_4 EQ 'X'.
      IF  <FS_MX1>-YWLX NE '项目' AND  <FS_MX1>-YWLX NE '产品' AND  <FS_MX1>-YWLX NE '公司间'.
       <FS_MX1>-YWLX = '其他'.
       CLEAR GT_TOTAL_QT.
       GT_TOTAL_QT-YWLX =  <FS_MX1>-YWLX .           "业务类型
       GT_TOTAL_QT-BUKRS = P_BUKRS.                  ""公司代码    ”GT_TOTAL_QT 汇总明细表1其他的 主营业务成本
       GT_TOTAL_QT-ZYYWCB = <FS_MX1>-DMBTR.          " ""主营业务成本
       COLLECT GT_TOTAL_QT.
       CONTINUE.
     ENDIF.
    ENDIF.

   ENDLOOP.

  ENDIF.
  SORT GT_TOTAL_XM BY BUKRS  VBELN .
  SORT GT_TOTAL_CP BY BUKRS  VBELN .
  SORT GT_TOTAL_GSJ BY BUKRS.
  SORT GT_TOTAL_QT BY BUKRS.
  IF GT_MX2[] IS NOT INITIAL.
    LOOP AT GT_MX2 ASSIGNING <FS_MX2>.
     IF <FS_MX2>-SHKZG EQ 'H'.
      <FS_MX2>-DMBTR =  <FS_MX2>-DMBTR * -1 .
       <FS_MX2>-WRBTR =  <FS_MX2>-WRBTR * -1 .
     ENDIF.
   " IF P_1 EQ 'X'.

      IF <FS_MX2>-BLART = 'RV'.

        READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX2>-VBEL2 BINARY SEARCH.
       IF SY-SUBRC = 0.
         IF GT_VBAK-AUART EQ 'ZF1' OR GT_VBAK-AUART EQ 'ZPO' OR GT_VBAK-AUART EQ 'ZSO' OR GT_VBAK-AUART EQ 'ZWV' OR GT_VBAK-AUART EQ 'ZZG' .
        <FS_MX2>-YWLX = '项目'.
          IF P_1 EQ 'X'.
               "读取销售订单
             <FS_MX2>-VBELN = <FS_MX2>-VBEL2.
           IF <FS_MX2>-VBELN IN S_VBELN . ""先根据屏幕销售订单作为条件筛选
               CLEAR:GT_TOTAL_XM , GT_TOTAL_XM02.
               GT_TOTAL_XM02-YWLX =  <FS_MX2>-YWLX .  "业务类型
               GT_TOTAL_XM-YWLX =  <FS_MX2>-YWLX .  "业务类型
               GT_TOTAL_XM02-BUKRS = P_BUKRS.          "公司代码
               GT_TOTAL_XM-BUKRS = P_BUKRS.          "公司代码
               GT_TOTAL_XM02-VBELN = <FS_MX2>-VBELN .   "销售订单                      " GT_TOTAL_XM02 存储 明细表2 项目的 汇总值
               GT_TOTAL_XM-VBELN = <FS_MX2>-VBELN .   "销售订单
               GT_TOTAL_XM-ZYYWSR_01 = <FS_MX2>-DMBTR.  "主营业务收入-本币
               COLLECT GT_TOTAL_XM.    "汇总主营业务收入本币
               GT_TOTAL_XM02-WAERS_1 = <FS_MX2>-WAERS_1.  "货币吗

               GT_TOTAL_XM02-ZYYWSR_02 = <FS_MX2>-WRBTR.    "主营业务收入-凭证货币
             IF  <FS_MX2>-WRBTR > 0 OR <FS_MX2>-WRBTR < 0.
               IF ( <FS_MX2>-HKONT BETWEEN '6001010101' AND '6001999999' )  AND ( <FS_MX2>-HKONT NE '6001019901').
                     GT_TOTAL_XM02-YKJSPPZHB = <FS_MX2>-WRBTR.  "统计已开金税发票金额
               ENDIF.
              COLLECT GT_TOTAL_XM02.  "汇总主营业务收入凭证值
              CONTINUE.
            ENDIF.
         ELSE.
           CONTINUE.
        ENDIF.

           ENDIF.


*         COLLECT GT_TOTAL_XM02.
*         CONTINUE.
       ENDIF.
        ENDIF.
       ENDIF.
     " ENDIF.
     " IF P_2 EQ 'X'.
       IF <FS_MX2>-BLART = 'RV'.
           READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX2>-XBLNR BINARY SEARCH.
        IF SY-SUBRC = 0 .
           IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
             <FS_MX2>-YWLX = '产品'.
              IF P_2 EQ 'X'.
                    "销售订单
                 <FS_MX2>-VBELN = <FS_MX2>-XBLNR.
                 IF <FS_MX2>-VBELN  IN S_VBELN .""先根据屏幕销售订单作为条件筛选
                     CLEAR:GT_TOTAL_CP, GT_TOTAL_CP02.
                     GT_TOTAL_CP02-YWLX =  <FS_MX2>-YWLX .           "业务类型
                     GT_TOTAL_CP-YWLX =  <FS_MX2>-YWLX .           "业务类型
                     GT_TOTAL_CP02-BUKRS = P_BUKRS.                   "公司代码
                     GT_TOTAL_CP-BUKRS = P_BUKRS.                   "公司代码
                     GT_TOTAL_CP02-VBELN = <FS_MX2>-VBELN .           "销售订单                 " GT_TOTAL_CP02 存储 明细表2 产品的 汇总值
                     GT_TOTAL_CP-VBELN = <FS_MX2>-VBELN .           "销售订单
                     GT_TOTAL_CP-ZYYWSR_01 = <FS_MX2>-DMBTR.          "主营业务收入-本币
                     COLLECT GT_TOTAL_CP.                           "汇总主营业务收入本币值
                     GT_TOTAL_CP02-WAERS_1 = <FS_MX2>-WAERS_1.         "货币
                     GT_TOTAL_CP02-ZYYWSR_02 = <FS_MX2>-WRBTR.         ""主营业务收入-凭证货币
                    COLLECT GT_TOTAL_CP02.
                    CONTINUE.
                ELSE.
                CONTINUE.
               ENDIF.
          ENDIF.


            ENDIF.
        ENDIF.

        ENDIF.
    "   ENDIF.
    "  IF P_3 EQ 'X'.
        IF <FS_MX2>-BLART = 'RV'.
             IF <FS_MX2>-EBELN NE ''.
               <FS_MX2>-YWLX = '公司间'.
               IF P_3 EQ 'X'.
                     CLEAR GT_TOTAL_GSJ.
                     GT_TOTAL_GSJ-YWLX =  <FS_MX2>-YWLX .         "业务类型
                     GT_TOTAL_GSJ-BUKRS = P_BUKRS.                 "公司代码
                      " GT_TOTAL_GSJ02-WAERS_1 = <FS_MX2>-WAERS_1.       "货币码                  " GT_TOTAL_GSJ02 存储 明细表2 公司间的 汇总值
                     GT_TOTAL_GSJ-ZYYWSR_01 = <FS_MX2>-DMBTR.        "主营业务收入-本币
                     "  GT_TOTAL_GSJ02-ZYYWSR_02 = <FS_MX2>-WRBTR.        "主营业务收入-凭证货币
                   COLLECT GT_TOTAL_GSJ.
                   CONTINUE.

                ENDIF.

               ENDIF.
         ENDIF.
      " ENDIF.
     IF P_4 EQ 'X'.
       IF  <FS_MX2>-YWLX NE '项目' AND  <FS_MX2>-YWLX NE '产品' AND  <FS_MX2>-YWLX NE '公司间'.
       <FS_MX2>-YWLX = '其他'.
       CLEAR GT_TOTAL_QT.
       GT_TOTAL_QT-YWLX =  <FS_MX2>-YWLX .         "业务类型
       GT_TOTAL_QT-BUKRS = P_BUKRS.                "公司代码
     "  GT_TOTAL_QT02-WAERS_1 = <FS_MX2>-WAERS_1.       "货币吗                       "GT_TOTAL_QT02 存储明细表2 其他 的汇总至
       GT_TOTAL_QT-ZYYWSR_01 = <FS_MX2>-DMBTR.       "主营业务收入-本币
    "   GT_TOTAL_QT02-ZYYWSR_02 = <FS_MX2>-WRBTR.        "主营业务收入-凭证货币
       COLLECT GT_TOTAL_QT.
       CONTINUE.
     ENDIF.
     ENDIF.
   ENDLOOP.

  ENDIF.
  SORT GT_TOTAL_XM BY BUKRS VBELN .
  SORT GT_TOTAL_XM02 BY BUKRS  VBELN  WAERS_1.
  DELETE GT_TOTAL_XM02 WHERE ZYYWSR_01 EQ 0 AND ZYYWSR_02 EQ 0 AND ZYYWSR_01 EQ 0 .
  SORT GT_TOTAL_CP BY BUKRS VBELN.
  SORT GT_TOTAL_CP02 BY BUKRS  VBELN WAERS_1.
  DELETE GT_TOTAL_CP02 WHERE ZYYWSR_01 EQ 0 AND ZYYWSR_02 EQ 0  .
  SORT GT_TOTAL_GSJ BY BUKRS.
  SORT GT_TOTAL_QT BY BUKRS .
 IF GT_MX3[] IS NOT INITIAL.
     LOOP AT GT_MX3 ASSIGNING <FS_MX3>.
      IF <FS_MX3>-SHKZG EQ 'H'.
      <FS_MX3>-DMBTR =  <FS_MX3>-DMBTR * -1 .
     ENDIF.
  "  IF P_1 EQ 'X'.
    IF <FS_MX3>-BLART = 'SC'.
        <FS_MX3>-YWLX = '项目'.
           IF P_1 EQ 'X'.
                "读取销售订单
                 SPLIT <FS_MX3>-ZUONR  AT '/' INTO <FS_MX3>-VBELN VBELN_TMP.
               IF <FS_MX3>-VBELN IN S_VBELN .
                  CLEAR GT_TOTAL_XM.
                  GT_TOTAL_XM-YWLX =  <FS_MX3>-YWLX .        "业务类型
                  GT_TOTAL_XM-VBELN = <FS_MX3>-VBELN.         "销售订单
                  GT_TOTAL_XM-BUKRS = P_BUKRS.                  "公司代码                 ”GT_TOTAL_XM 存储明细表3 的汇总值
                  GT_TOTAL_XM-GCCB = <FS_MX3>-DMBTR.             "工程成本
                  COLLECT GT_TOTAL_XM.
                  CONTINUE.
              ELSE.
                 CONTINUE.
             ENDIF.
           ENDIF.


     ENDIF.
  "  ENDIF.
   " IF P_2 EQ 'X'.
      IF <FS_MX3>-BLART = 'RV'.
         READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX3>-XBLNR BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
           <FS_MX3>-YWLX = '产品'.
              IF P_2 EQ 'X'.
                 "销售订单
             <FS_MX3>-VBELN = <FS_MX3>-XBLNR.
             IF <FS_MX3>-VBELN IN S_VBELN .
               CLEAR GT_TOTAL_CP.
               GT_TOTAL_CP-YWLX =  <FS_MX3>-YWLX .           "业务类型
               GT_TOTAL_CP-VBELN = <FS_MX3>-VBELN .            "销售订单         "GT_TOTAL_CP存储明细表3 的汇总值
               GT_TOTAL_CP-BUKRS = P_BUKRS.                  "公司代码
               GT_TOTAL_CP-GCCB = <FS_MX3>-DMBTR.            "工程成本
               COLLECT GT_TOTAL_CP.
               CONTINUE.
               ELSE.
                 CONTINUE.
              ENDIF.

              ENDIF.


         ENDIF.
        ENDIF.
       ENDIF.
     IF <FS_MX3>-BLART = 'WL'.
            READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX3>-VBEL2 BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
           <FS_MX3>-YWLX = '产品'.
              IF P_2 EQ 'X'.
                   "销售订单
             <FS_MX3>-VBELN = <FS_MX3>-VBEL2.
            IF <FS_MX3>-VBELN  IN S_VBELN.
               CLEAR GT_TOTAL_CP.
               GT_TOTAL_CP-YWLX =  <FS_MX3>-YWLX .        "业务类型
               GT_TOTAL_CP-VBELN = <FS_MX3>-VBELN.          "销售订单                        "GT_TOTAL_CP 存储明细3 的汇总值
               GT_TOTAL_CP-BUKRS = P_BUKRS.                  "公司代码
               GT_TOTAL_CP-GCCB = <FS_MX3>-DMBTR.                "工程成本
               COLLECT GT_TOTAL_CP.
               CONTINUE.
              ELSE.
                CONTINUE.
             ENDIF.
               ENDIF.


         ENDIF.
        ENDIF.

     ENDIF.
   " ENDIF.
   IF P_4 EQ 'X'.
     IF  <FS_MX3>-YWLX NE '项目' AND  <FS_MX3>-YWLX NE '产品' .                       "<FS_MX3>-BLART = 'ML'.
        <FS_MX3>-YWLX = '其他'.
          CLEAR GT_TOTAL_QT.
          GT_TOTAL_QT-YWLX =  <FS_MX3>-YWLX .     "业务类型
            GT_TOTAL_QT-BUKRS = P_BUKRS.             "公司代码                    "GT_TOTAL_QT 存储明细表3 的汇总值
           GT_TOTAL_QT-GCCB = <FS_MX3>-DMBTR.       "工程成本
          COLLECT GT_TOTAL_QT.
          CONTINUE.
       ENDIF.
    ENDIF.
     ENDLOOP.
 ENDIF.
  SORT GT_TOTAL_XM BY BUKRS  VBELN .
  SORT GT_TOTAL_CP BY BUKRS  VBELN .
  SORT GT_TOTAL_GSJ BY BUKRS.
  SORT GT_TOTAL_QT BY BUKRS.
 IF GT_MX3_01[] IS NOT INITIAL.
   LOOP AT GT_MX3_01 ASSIGNING <FS_MX3_01>.
   IF <FS_MX3_01>-SHKZG EQ 'H'.
      <FS_MX3_01>-DMBTR =  <FS_MX3_01>-DMBTR * -1 .


    ENDIF.
    "IF P_1 EQ 'X'.

    IF <FS_MX3_01>-BLART = 'SC'.
        <FS_MX3_01>-YWLX = '项目'.
        IF P_1 EQ 'X'.
            "读取销售订单
        SPLIT <FS_MX3_01>-ZUONR  AT '/' INTO <FS_MX3_01>-VBELN VBELN_TMP.
        READ TABLE GT_TOTAL_XM WITH KEY VBELN = <FS_MX3_01>-VBELN BINARY SEARCH .
         IF SY-SUBRC = 0.
           CLEAR GT_TOTAL_XM.
           GT_TOTAL_XM-YWLX =  <FS_MX3_01>-YWLX .                           "业务类型
           GT_TOTAL_XM-VBELN = <FS_MX3_01>-VBELN.                           "销售订单      汇总 GT_TOTAL_XM 汇总工程项目 明细3_1的值
           GT_TOTAL_XM-BUKRS = P_BUKRS.                                     "公司代码
           GT_TOTAL_XM-GCCB_QCYE = <FS_MX3_01>-DMBTR.                       "工程成本-起初余额
           COLLECT GT_TOTAL_XM.
          CONTINUE.
        ENDIF.

         ENDIF.


     ENDIF.
 "   ENDIF.
  "  IF P_2 EQ 'X'.
      IF <FS_MX3_01>-BLART = 'RV'.
         READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX3_01>-XBLNR BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
           <FS_MX3_01>-YWLX = '产品'.
            IF P_2 EQ 'X'.
                              "销售订单
           <FS_MX3_01>-VBELN = <FS_MX3_01>-XBLNR.
           READ TABLE  GT_TOTAL_CP WITH KEY VBELN = <FS_MX3_01>-VBELN .
           IF SY-SUBRC = 0.
                CLEAR GT_TOTAL_CP.
               GT_TOTAL_CP-YWLX =  <FS_MX3_01>-YWLX .          "业务类型
               GT_TOTAL_CP-VBELN =  <FS_MX3_01>-VBELN .         "销售订单           汇总 GT_TOTAL_CP 汇总工程项目 明细3_1的值
               GT_TOTAL_CP-BUKRS = P_BUKRS.                     "公司代码
               GT_TOTAL_CP-GCCB_QCYE = <FS_MX3_01>-DMBTR.             "工程成本——期初余额
               COLLECT GT_TOTAL_CP.
               CONTINUE.
            ENDIF.
             ENDIF.


         ENDIF.
        ENDIF.
       ENDIF.
     IF <FS_MX3_01>-BLART = 'WL'.
            READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX3_01>-VBEL2 BINARY SEARCH.
       IF SY-SUBRC = 0 .
          IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
           <FS_MX3_01>-YWLX = '产品'.
              IF P_2 EQ 'X'.
                  "销售订单
             <FS_MX3_01>-VBELN = <FS_MX3_01>-VBEL2.
             READ TABLE GT_TOTAL_CP WITH KEY VBELN = <FS_MX3_01>-VBELN BINARY SEARCH.
             IF SY-SUBRC = 0.
               CLEAR GT_TOTAL_CP.
               GT_TOTAL_CP-YWLX =  <FS_MX3_01>-YWLX .  "业务类型
               GT_TOTAL_CP-VBELN = <FS_MX3_01>-VBELN .  "销售订单                 汇总 GT_TOTAL_CP 汇总工程项目 明细3_1的值
               GT_TOTAL_CP-BUKRS = P_BUKRS.            "公司代码
               GT_TOTAL_CP-GCCB_QCYE = <FS_MX3_01>-DMBTR.   "工程成本——期初余额
                COLLECT GT_TOTAL_CP.
                CONTINUE.
             ENDIF.

               ENDIF.


         ENDIF.
        ENDIF.
       ENDIF.
"     ENDIF.
   IF P_4 EQ 'X'.
     IF <FS_MX3_01>-YWLX NE '项目' AND  <FS_MX3_01>-YWLX NE '产品' .  " <FS_MX3_01>-BLART = 'ML'.
        <FS_MX3_01>-YWLX = '其他'.
        READ TABLE GT_TOTAL_QT WITH KEY BUKRS  = P_BUKRS  BINARY SEARCH.
        IF SY-SUBRC = 0.
           CLEAR GT_TOTAL_QT.
          GT_TOTAL_QT-YWLX =  <FS_MX3_01>-YWLX .
          GT_TOTAL_QT-BUKRS = P_BUKRS.
          GT_TOTAL_QT-GCCB_QCYE = <FS_MX3_01>-DMBTR.
          COLLECT GT_TOTAL_QT.
          CONTINUE.
         ENDIF.

       ENDIF.
    ENDIF.
   ENDLOOP.
 ENDIF.
   SORT GT_TOTAL_XM BY BUKRS  VBELN .
  SORT GT_TOTAL_CP BY BUKRS  VBELN .
  SORT GT_TOTAL_QT BY BUKRS.
 IF GT_MX4[] IS NOT INITIAL.
    LOOP AT GT_MX4 ASSIGNING <FS_MX4>.
     IF <FS_MX4>-SHKZG EQ 'H'.
       <FS_MX4>-WRBTR = <FS_MX4>-WRBTR * -1.
     ENDIF.
    " IF P_1 EQ 'X'.
   "  IF <FS_MX4>-BLART = 'SA' OR <FS_MX4>-BLART = 'ZA' .
     "先判断是否产品订单
     IF P_2 EQ 'X'.
        READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX4>-ZUONR BINARY SEARCH.
         IF SY-SUBRC = 0.
              IF GT_VBAK-AUART = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
                <FS_MX4>-YWLX = '产品'.
                <FS_MX4>-VBELN = <FS_MX4>-ZUONR.
                    READ TABLE GT_TOTAL_CP02 WITH KEY VBELN =  <FS_MX4>-VBELN  BINARY SEARCH.
                      IF SY-SUBRC = 0.
                        CLEAR GT_TOTAL_CP02.
                        GT_TOTAL_CP02-YWLX =  <FS_MX4>-YWLX .   "业务类型
                        GT_TOTAL_CP02-VBELN =  <FS_MX4>-VBELN .    "销售订单       "GT_TOTAL_CP02 统计产品汇总值
                        GT_TOTAL_CP02-BUKRS = P_BUKRS.              "公司代码
                        GT_TOTAL_CP02-WAERS_1 = <FS_MX4>-WAERS_1.     "货币码
                          IF <FS_MX4>-BLART = 'QC'.
                           GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  期初情况
                            COLLECT GT_TOTAL_CP02.
                            CONTINUE.
                        ENDIF.
                        READ TABLE GT_BSEG WITH KEY BUKRS = <FS_MX4>-BUKRS GJAHR = <FS_MX4>-GJAHR BELNR = <FS_MX4>-BELNR BINARY SEARCH.
                         IF SY-SUBRC EQ  0.
                            GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  开票情况
                            COLLECT GT_TOTAL_CP02.
                            CONTINUE.
                           ELSE.
                             IF <FS_MX4>-BLART = 'SA' OR <FS_MX4>-BLART = 'ZA' .
                               GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  收款情况
                               GT_TOTAL_CP02-YSKPZHB  = <FS_MX4>-WRBTR .    "已收款-凭证货币
                               COLLECT GT_TOTAL_CP02.
                              CONTINUE.
                             ENDIF.
                         ENDIF.
                       ELSE.
                         READ TABLE GT_TOTAL_CP WITH KEY VBELN =  <FS_MX4>-VBELN  BINARY SEARCH.
                        IF SY-SUBRC = 0.
                            CLEAR GT_TOTAL_CP02.
                        GT_TOTAL_CP02-YWLX =  <FS_MX4>-YWLX .   "业务类型
                        GT_TOTAL_CP02-VBELN =  <FS_MX4>-VBELN .    "销售订单       "GT_TOTAL_CP02 统计产品汇总值
                        GT_TOTAL_CP02-BUKRS = P_BUKRS.              "公司代码
                        GT_TOTAL_CP02-WAERS_1 = <FS_MX4>-WAERS_1.     "货币码
                          IF <FS_MX4>-BLART = 'QC'.
                           GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  期初情况
                            COLLECT GT_TOTAL_CP02.
                            CONTINUE.
                        ENDIF.
                        READ TABLE GT_BSEG WITH KEY BUKRS = <FS_MX4>-BUKRS GJAHR = <FS_MX4>-GJAHR BELNR = <FS_MX4>-BELNR BINARY SEARCH.
                         IF SY-SUBRC EQ  0.
                            GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  开票情况
                            COLLECT GT_TOTAL_CP02.
                            CONTINUE.
                           ELSE.
                             IF <FS_MX4>-BLART = 'SA' OR <FS_MX4>-BLART = 'ZA' .
                               GT_TOTAL_CP02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  收款情况
                               GT_TOTAL_CP02-YSKPZHB  = <FS_MX4>-WRBTR .    "已收款-凭证货币
                               COLLECT GT_TOTAL_CP02.
                              CONTINUE.
                             ENDIF.
                         ENDIF.
                         ENDIF.
                   ENDIF.
               ENDIF.
         ENDIF.
      ENDIF .
      IF P_1 EQ 'X'. "项目
          READ TABLE GT_VBAK WITH KEY VBELN = <FS_MX4>-ZUONR BINARY SEARCH.
          IF SY-SUBRC = 0.
             IF GT_VBAK-AUART = 'ZF1' OR GT_VBAK-AUART = 'ZPO' OR GT_VBAK-AUART = 'ZSO' OR  GT_VBAK-AUART = 'ZWV' OR  GT_VBAK-AUART = 'ZZG'.
                <FS_MX4>-YWLX = '项目'.
                <FS_MX4>-VBELN = <FS_MX4>-ZUONR.
                 READ TABLE GT_TOTAL_XM02 WITH KEY VBELN =  <FS_MX4>-VBELN  BINARY SEARCH.
                      IF SY-SUBRC = 0.
                        CLEAR GT_TOTAL_XM02.
                        GT_TOTAL_XM02-YWLX =  <FS_MX4>-YWLX .   "业务类型
                        GT_TOTAL_XM02-VBELN =  <FS_MX4>-VBELN .    "销售订单       "GT_TOTAL_CP02 统计产品汇总值
                        GT_TOTAL_XM02-BUKRS = P_BUKRS.              "公司代码
                           GT_TOTAL_XM02-WAERS_1 = <FS_MX4>-WAERS_1.     "货币码
                        IF <FS_MX4>-BLART = 'QC'.
                           GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  期初情况
                            COLLECT GT_TOTAL_XM02.
                            CONTINUE.
                        ENDIF.
                         READ TABLE GT_BSEG WITH KEY BUKRS = <FS_MX4>-BUKRS GJAHR = <FS_MX4>-GJAHR BELNR = <FS_MX4>-BELNR BINARY SEARCH.
                         IF SY-SUBRC EQ  0.
                            GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  开票情况
                            COLLECT GT_TOTAL_XM02.
                            CONTINUE.
                           ELSE.
                             IF <FS_MX4>-BLART = 'SA' OR <FS_MX4>-BLART = 'ZA' .
                               GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  收款情况
                               GT_TOTAL_XM02-YSKPZHB  = <FS_MX4>-WRBTR .    "已收款-凭证货币
                               COLLECT GT_TOTAL_XM02.
                              CONTINUE.
                             ENDIF.
                         ENDIF.
                       ELSE.
                          READ TABLE GT_TOTAL_XM WITH KEY VBELN =  <FS_MX4>-VBELN  BINARY SEARCH.
                          IF SY-SUBRC = 0.
                                  CLEAR GT_TOTAL_XM02.
                        GT_TOTAL_XM02-YWLX =  <FS_MX4>-YWLX .   "业务类型
                        GT_TOTAL_XM02-VBELN =  <FS_MX4>-VBELN .    "销售订单       "GT_TOTAL_CP02 统计产品汇总值
                        GT_TOTAL_XM02-BUKRS = P_BUKRS.              "公司代码
                           GT_TOTAL_XM02-WAERS_1 = <FS_MX4>-WAERS_1.     "货币码
                        IF <FS_MX4>-BLART = 'QC'.
                           GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  期初情况
                            COLLECT GT_TOTAL_XM02.
                            CONTINUE.
                        ENDIF.
                         READ TABLE GT_BSEG WITH KEY BUKRS = <FS_MX4>-BUKRS GJAHR = <FS_MX4>-GJAHR BELNR = <FS_MX4>-BELNR BINARY SEARCH.
                         IF SY-SUBRC EQ  0.
                            GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  开票情况
                            COLLECT GT_TOTAL_XM02.
                            CONTINUE.
                           ELSE.
                             IF <FS_MX4>-BLART = 'SA' OR <FS_MX4>-BLART = 'ZA' .
                               GT_TOTAL_XM02-YSKYE  = <FS_MX4>-WRBTR .   "应收款余额-凭证货币  收款情况
                               GT_TOTAL_XM02-YSKPZHB  = <FS_MX4>-WRBTR .    "已收款-凭证货币
                               COLLECT GT_TOTAL_XM02.
                              CONTINUE.
                             ENDIF.
                         ENDIF.
                            ENDIF.
                   ENDIF.
            ENDIF.

           ENDIF.
      ENDIF.

  ENDLOOP.
ENDIF.
SORT GT_TOTAL_XM BY BUKRS  VBELN .
SORT GT_TOTAL_CP BY BUKRS  VBELN .
SORT GT_TOTAL_XM BY YWLX BUKRS VBELN .
SORT GT_TOTAL_CP BY YWLX BUKRS VBELN .
SORT GT_TOTAL_GSJ BY YWLX BUKRS .
SORT GT_TOTAL_QT BY YWLX BUKRS .
SORT GT_TOTAL_XM02 BY YWLX BUKRS VBELN .
SORT GT_TOTAL_CP02 BY YWLX BUKRS VBELN .
SORT GT_TOTAL_GSJ02 BY YWLX BUKRS .
SORT GT_TOTAL_QT02 BY YWLX BUKRS .

IF P_1 EQ 'X'.
  IF GT_TOTAL_XM[] IS NOT INITIAL OR GT_TOTAL_XM02[] IS NOT INITIAL..
  CLEAR : GT_TOTAL_VBELN ,GT_TOTAL_VBELN[].
  APPEND LINES OF GT_TOTAL_XM TO  GT_TOTAL_VBELN .
  "APPEND LINES OF GT_TOTAL_XM02 TO GT_TOTAL_VBELN.
  GT_TOTAL_VBELN_ALL[] = GT_TOTAL_VBELN[].
  SORT GT_TOTAL_VBELN_ALL  BY VBELN  .
  DELETE ADJACENT DUPLICATES FROM GT_TOTAL_VBELN_ALL  COMPARING VBELN .
  DELETE GT_TOTAL_VBELN_ALL WHERE VBELN NOT IN S_VBELN.
  SELECT VBFA~VBELV VBFA~POSNV VBFA~VBELN VBFA~POSNN LIKP~WADAT_IST
         INTO CORRESPONDING FIELDS OF TABLE GT_VBFA_LIKP
         FROM VBFA
         INNER JOIN LIKP
         ON VBFA~VBELN = LIKP~VBELN
         FOR ALL ENTRIES IN GT_TOTAL_VBELN_ALL
         WHERE VBFA~VBELV = GT_TOTAL_VBELN_ALL-VBELN  AND VBFA~VBTYP_N = 'J'.
      SORT GT_VBFA_LIKP BY  VBELV ASCENDING WADAT_IST  DESCENDING ."销售订单 、日期排序
       DELETE ADJACENT DUPLICATES FROM GT_VBFA_LIKP COMPARING VBELV ."只保留同一销售订单号最大的日期

  "取项目类型的验收期间
  SELECT VBELN VBELV VBTYP_N
  INTO CORRESPONDING FIELDS OF TABLE GT_VBFA_MN
    FROM VBFA
    FOR ALL ENTRIES IN GT_TOTAL_VBELN_ALL
    WHERE  VBELV = GT_TOTAL_VBELN_ALL-VBELN AND   ( VBTYP_N = 'M' OR VBTYP_N = 'N').
    SORT GT_VBFA_MN BY VBELV ASCENDING VBELN DESCENDING.
    DELETE ADJACENT DUPLICATES FROM GT_VBFA_MN COMPARING VBELV VBELN.
  IF GT_VBFA_MN[] IS NOT INITIAL.
    SELECT VBELN FKDAT
     FROM VBRK
     INTO CORRESPONDING FIELDS OF TABLE GT_VBRK_FKDAT
     FOR ALL ENTRIES IN GT_VBFA_MN
     WHERE  VBELN = GT_VBFA_MN-VBELN .
    SORT GT_VBRK_FKDAT BY VBELN.

   ENDIF.
  "读取VBAP 的销售订单的虚拟项目行
  CLEAR :GT_VBAP,GT_VBAP[].
  SELECT VBELN POSNR PSTYV
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    FROM VBAP
    WHERE VBELN IN S_VBELN AND PSTYV IN ( 'Z01' ,'Z02' ,'Z21','Z22','Z31','Z32','Z41','Z42' ) .
   SORT GT_VBAP BY VBELN POSNR .
   SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
   B~BUZEI B~SHKZG B~HKONT B~DMBTR B~VBEL2 AS VBELN B~POSN2
  INTO CORRESPONDING FIELDS OF TABLE  GT_BZCB_XM
   FROM BKPF AS A         ""标准成本-项目
   INNER JOIN BSEG AS B
   ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
"   FOR ALL ENTRIES IN GT_TOTAL_VBELN
   WHERE A~BUKRS = P_BUKRS  AND  ( A~BLART = 'WL' OR  A~BLART = 'WA' ) AND BUDAT <= P_DATE AND B~HKONT  BETWEEN '6404010101' AND '6404999999'
   AND B~VBEL2 IN S_VBELN.
  SORT GT_BZCB_XM BY VBELN .
      "按项目汇总总金额
  LOOP AT GT_BZCB_XM .

        "判断销售凭证 、行号 是否为对应的销售订单的虚拟物料再汇总
        READ TABLE GT_VBAP WITH KEY VBELN = GT_BZCB_XM-VBELN  POSNR = GT_BZCB_XM-POSN2 .
        IF SY-SUBRC = 0 .
           CLEAR  GT_BZCB_XM_SUM.
           GT_BZCB_XM_SUM-VBELN =  GT_BZCB_XM-VBELN.
           IF GT_BZCB_XM-SHKZG = 'S'.
             GT_BZCB_XM_SUM-DMBTR =   GT_BZCB_XM-DMBTR.     "GT_BZCB_XM_SUM统计标准成本 项目的汇总值
           ENDIF.
          IF GT_BZCB_XM-SHKZG = 'H'.
             GT_BZCB_XM_SUM-DMBTR =   GT_BZCB_XM-DMBTR * -1.
          ENDIF.
          COLLECT GT_BZCB_XM_SUM.
          CONTINUE.
         ELSE.
           CONTINUE.
         ENDIF.

  ENDLOOP.
  SORT GT_BZCB_XM_SUM BY VBELN.
  SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
   B~BUZEI B~SHKZG B~HKONT B~DMBTR B~VBEL2  AS VBELN B~POSN2
  INTO CORRESPONDING FIELDS OF TABLE  GT_CBCY_XM
   FROM BKPF AS A         ""成本差异-项目
   INNER JOIN BSEG AS B
   ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
  " FOR ALL ENTRIES IN GT_TOTAL_VBELN
   WHERE A~BUKRS = P_BUKRS AND ( A~BLART NE 'WL' AND  A~BLART NE 'WA' )  AND BUDAT <= P_DATE AND B~HKONT  BETWEEN '6404010101' AND '6404999999'
   AND B~VBEL2 IN S_VBELN.
  SORT GT_CBCY_XM BY VBELN .
  LOOP AT GT_CBCY_XM .
    "判断销售凭证 、行号 是否为对应的销售订单的虚拟物料再汇总
     READ TABLE GT_VBAP WITH KEY VBELN = GT_CBCY_XM-VBELN  POSNR = GT_CBCY_XM-POSN2 .
      IF SY-SUBRC = 0 .
         CLEAR  GT_CBCY_XM_SUM.
         GT_CBCY_XM_SUM-VBELN =  GT_CBCY_XM-VBELN.
         IF GT_CBCY_XM-SHKZG = 'S'.
            GT_CBCY_XM_SUM-DMBTR =   GT_CBCY_XM-DMBTR.      "GT_CBCY_XM_SUM 统计 成本差异 项目的汇总值
         ENDIF.
         IF GT_CBCY_XM-SHKZG = 'H'.
           GT_CBCY_XM_SUM-DMBTR =   GT_CBCY_XM-DMBTR * -1.
         ENDIF.
         COLLECT  GT_CBCY_XM_SUM.
         CONTINUE.
      ELSE.
        CONTINUE.
      ENDIF.


  ENDLOOP.
  SORT GT_CBCY_XM_SUM BY VBELN.
   SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
   B~BUZEI B~SHKZG B~HKONT B~DMBTR B~VBEL2  AS VBELN B~POSN2
  INTO CORRESPONDING FIELDS OF TABLE  GT_GCYF_XM
   FROM BKPF AS A         ""工程费用
   INNER JOIN BSEG AS B
   ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
  " FOR ALL ENTRIES IN GT_TOTAL_VBELN
   WHERE A~BUKRS = P_BUKRS  AND BUDAT <= P_DATE AND B~HKONT  BETWEEN '8000000000' AND '8999999999'
   AND B~VBEL2 IN S_VBELN.
    SORT GT_GCYF_XM BY VBELN.
    LOOP AT GT_GCYF_XM .
        "判断销售凭证 、行号 是否为对应的销售订单的虚拟物料再汇总
        READ TABLE GT_VBAP WITH KEY VBELN = GT_GCYF_XM-VBELN  POSNR = GT_GCYF_XM-POSN2 .
        IF SY-SUBRC = 0 .
           CLEAR GT_GCYF_XM_SUM..
           GT_GCYF_XM_SUM-VBELN =  GT_GCYF_XM-VBELN.
           IF GT_GCYF_XM-SHKZG = 'S'.                          "GT_GCYF_XM_SUM 统计 工程费用 项目的汇总值
              GT_GCYF_XM_SUM-DMBTR =   GT_GCYF_XM-DMBTR.
           ENDIF.
           IF GT_GCYF_XM-SHKZG = 'H'.
             GT_GCYF_XM_SUM-DMBTR =   GT_GCYF_XM-DMBTR * -1.
          ENDIF.
         COLLECT  GT_GCYF_XM_SUM.
         CONTINUE.
       ELSE.
         CONTINUE.
        ENDIF.

    ENDLOOP.
    SORT GT_GCYF_XM_SUM BY VBELN.
  ENDIF.

ENDIF.
IF P_2 EQ 'X'.
  IF GT_TOTAL_CP[] IS NOT INITIAL OR GT_TOTAL_CP02[] IS NOT INITIAL.
*    CLEAR:  GT_TOTAL_VBELN , GT_TOTAL_VBELN[].
  APPEND LINES OF GT_TOTAL_CP TO GT_TOTAL_VBELN .
  " APPEND LINES OF GT_TOTAL_CP02 TO GT_TOTAL_VBELN .
  SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
   B~BUZEI B~SHKZG B~HKONT B~DMBTR B~VBEL2 AS VBELN
  INTO CORRESPONDING FIELDS OF TABLE  GT_BZCB_CP
   FROM BKPF AS A         ""产品标准成本-产品
   INNER JOIN BSEG AS B
   ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
"   FOR ALL ENTRIES IN GT_TOTAL_VBELN
   WHERE A~BUKRS = P_BUKRS AND ( A~BLART = 'WL' OR  A~BLART = 'WA' ) AND BUDAT <= P_DATE AND B~HKONT  EQ '1406010101'
  AND B~VBEL2 IN S_VBELN.
  SORT GT_BZCB_CP BY VBELN .
   LOOP AT GT_BZCB_CP .
        CLEAR GT_BZCB_CP_SUM.
        READ TABLE GT_VBAK WITH KEY VBELN = GT_BZCB_CP-VBELN BINARY SEARCH .
        IF SY-SUBRC = 0.
             IF GT_VBAK-AUART  = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
                 GT_BZCB_CP_SUM-VBELN =  GT_BZCB_CP-VBELN.
                  IF GT_BZCB_CP-SHKZG = 'S'.
                      GT_BZCB_CP_SUM-DMBTR =   GT_BZCB_CP-DMBTR.     "GT_BZCB_CP_SUM统计 产品 标准成本汇总值
                  ENDIF.
                  IF GT_BZCB_CP-SHKZG = 'H'.
                     GT_BZCB_CP_SUM-DMBTR =   GT_BZCB_CP-DMBTR * -1.
                  ENDIF.

                  COLLECT GT_BZCB_CP_SUM.

             ENDIF.
         ENDIF.

  ENDLOOP.
  SORT GT_BZCB_CP_SUM BY VBELN.
  SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
   B~BUZEI  B~SHKZG B~HKONT B~DMBTR B~ZUONR AS VBELN
  INTO CORRESPONDING FIELDS OF TABLE  GT_CBCY_CP
   FROM BKPF AS A         ""成本差异-项目-产品
   INNER JOIN BSEG AS B
   ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
  " FOR ALL ENTRIES IN GT_TOTAL_VBELN
   WHERE A~BUKRS = P_BUKRS AND A~BLART = 'ML' AND BUDAT <= P_DATE AND B~HKONT BETWEEN '6401000000' AND '6401999999'
   AND B~ZUONR IN S_VBELN.
  SORT GT_CBCY_CP BY VBELN .
   LOOP AT GT_CBCY_CP .
     CLEAR GT_CBCY_CP_SUM.

         READ TABLE GT_VBAK WITH KEY VBELN = GT_CBCY_CP-VBELN BINARY SEARCH .
        IF SY-SUBRC = 0.
             IF GT_VBAK-AUART  = 'ZF2' OR GT_VBAK-AUART = 'ZOR' OR GT_VBAK-AUART = 'ZRE' OR  GT_VBAK-AUART = 'ZSD'.
                 GT_CBCY_CP_SUM-VBELN =  GT_CBCY_CP-VBELN.
                 IF GT_CBCY_CP-SHKZG = 'S'.
                  GT_CBCY_CP_SUM-DMBTR =    GT_CBCY_CP-DMBTR.        "GT_CBCY_CP_SUM 统计 产品 成本差异汇总值
                 ENDIF.
                  IF GT_CBCY_CP-SHKZG = 'H'.
                  GT_CBCY_CP_SUM-DMBTR =    GT_CBCY_CP-DMBTR * -1.
                ENDIF.
                COLLECT GT_CBCY_CP_SUM.
             ENDIF.
        ENDIF.



  ENDLOOP.
  SORT GT_CBCY_CP_SUM BY VBELN.
  ENDIF.
ENDIF.

SORT GT_TOTAL_VBELN BY YWLX BUKRS VBELN .
DELETE ADJACENT DUPLICATES FROM GT_TOTAL_VBELN  COMPARING YWLX BUKRS VBELN .


IF P_3 EQ 'X'.
  APPEND LINES OF GT_TOTAL_GSJ TO GT_TOTAL .
ENDIF.
IF P_4 EQ 'X'.
  APPEND LINES OF GT_TOTAL_QT TO GT_TOTAL .
ENDIF.
SORT GT_TOTAL BY YWLX BUKRS .
DELETE ADJACENT DUPLICATES FROM GT_TOTAL COMPARING YWLX BUKRS .
 APPEND LINES OF GT_TOTAL_VBELN TO GT_TOTAL_SUM .
 APPEND LINES OF GT_TOTAL TO GT_TOTAL_SUM .
LOOP AT GT_TOTAL_SUM ASSIGNING <FS_TOTAL_SUM>.
   IF <FS_TOTAL_SUM>-YWLX = '项目'.
     READ TABLE GT_TOTAL_XM WITH KEY YWLX = <FS_TOTAL_SUM>-YWLX BUKRS = <FS_TOTAL_SUM>-BUKRS VBELN = <FS_TOTAL_SUM>-VBELN.
     IF SY-SUBRC = 0.
       <FS_TOTAL_SUM>-ZYYWCB  = GT_TOTAL_XM-ZYYWCB. "主营业务成本
     " <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_XM-ZYYWSR_01 * -1.  "主营业务收入-本币
      " <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_XM-ZYYWSR_02 * -1. "主营业务收入-凭证货币
       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_XM-ZYYWSR_01 * -1.  "主营业务收入-本币
       <FS_TOTAL_SUM>-GCCB = GT_TOTAL_XM-GCCB . "工程成本
       IF GT_TOTAL_XM-GCCB_QCYE  NE 0 .
          <FS_TOTAL_SUM>-GCCB_QCYE = GT_TOTAL_XM-GCCB_QCYE . "工程成本期初余额
        ENDIF.
*       IF GT_TOTAL_XM-YSKPZHB NE 0 .
*         <FS_TOTAL_SUM>-YSKPZHB = GT_TOTAL_XM-YSKPZHB * -1 ."已收款-凭证货币值
*        ENDIF.
*       IF GT_TOTAL_XM-YSKYE NE 0 .
*         <FS_TOTAL_SUM>-YSKYE = GT_TOTAL_XM-YSKYE ."应收款余额-凭证货币
*       ENDIF.


     ENDIF.
     LOOP AT GT_TOTAL_XM02 WHERE  YWLX = <FS_TOTAL_SUM>-YWLX AND BUKRS = <FS_TOTAL_SUM>-BUKRS AND  VBELN = <FS_TOTAL_SUM>-VBELN.
       IF GT_TOTAL_XM02-WAERS_1 EQ 'CNY'.
        <FS_TOTAL_SUM>-WAERS_1 = GT_TOTAL_XM02-WAERS_1.    "货币
        <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_XM02-ZYYWSR_02 * -1. "主营业务收入-凭证货币
        <FS_TOTAL_SUM>-YKJSPPZHB = GT_TOTAL_XM02-YKJSPPZHB * -1.  "统计已开金税发票金额
        <FS_TOTAL_SUM>-YSKPZHB = GT_TOTAL_XM02-YSKPZHB * -1 ."已收款-凭证货币值
        <FS_TOTAL_SUM>-YSKYE = GT_TOTAL_XM02-YSKYE ."应收款余额-凭证货币
       ELSE.
         CLEAR:GT_APPEND.
         MOVE  GT_TOTAL_XM02 TO GT_APPEND.
         GT_APPEND-WAERS_1 = GT_TOTAL_XM02-WAERS_1.    "货币
         GT_APPEND-ZYYWSR_02 = GT_TOTAL_XM02-ZYYWSR_02 * -1. "主营业务收入-凭证货币
         GT_APPEND-YKJSPPZHB = GT_TOTAL_XM02-YKJSPPZHB * -1.  "统计已开金税发票金额
         GT_APPEND-YSKPZHB = GT_TOTAL_XM02-YSKPZHB * -1 ."已收款-凭证货币值
         GT_APPEND-YSKYE = GT_TOTAL_XM02-YSKYE ."应收款余额-凭证货币
         APPEND GT_APPEND.
       ENDIF.

     ENDLOOP.

     READ TABLE GT_VBAK WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
     IF SY-SUBRC = 0.
       "订单类型
       <FS_TOTAL_SUM>-AUART = GT_VBAK-AUART.
         "项目名称
       PERFORM SELXMMC USING <FS_TOTAL_SUM>-VBELN CHANGING <FS_TOTAL_SUM>-XMMC.
       "订单金额
       <FS_TOTAL_SUM>-DDJE = GT_VBAK-NETWR.
       "订单货币
       <FS_TOTAL_SUM>-DDHB =  GT_VBAK-WAERK.
       "客户
        <FS_TOTAL_SUM>-KUNNR = GT_VBAK-KUNNR.
       "客户名称
          PERFORM SELKHMC USING <FS_TOTAL_SUM>-KUNNR CHANGING <FS_TOTAL_SUM>-KUNNR_NAME1.
     ENDIF.
    "最后发货日期
     READ TABLE GT_VBFA_LIKP WITH KEY VBELV = <FS_TOTAL_SUM>-VBELN.
    IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-ZHFHRQ = GT_VBFA_LIKP-WADAT_IST .
    ENDIF.
    "产品标准成本
    READ TABLE GT_BZCB_XM_SUM WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-CPBZCB = GT_BZCB_XM_SUM-DMBTR.
    ENDIF.
    "产品成本差异
    READ TABLE GT_CBCY_XM_SUM WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-CPCBCY = GT_CBCY_XM_SUM-DMBTR.
    ENDIF.
    "工程费用-项目
     READ TABLE GT_GCYF_XM_SUM WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
     IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-GCFY = GT_GCYF_XM_SUM-DMBTR.
     ENDIF.
    "毛利率
     IF <FS_TOTAL_SUM>-ZYYWSR_01 NE 0.
     <FS_TOTAL_SUM>-MLV = ( <FS_TOTAL_SUM>-ZYYWSR_01 -  <FS_TOTAL_SUM>-ZYYWCB ) / <FS_TOTAL_SUM>-ZYYWSR_01 .
      IF  <FS_TOTAL_SUM>-MLV NE 0 .
        <FS_TOTAL_SUM>-MLV = <FS_TOTAL_SUM>-MLV * 100 .
        <FS_TOTAL_SUM>-MLV01 = <FS_TOTAL_SUM>-MLV .
        CONCATENATE   <FS_TOTAL_SUM>-MLV01 '%' INTO   <FS_TOTAL_SUM>-MLV01.
      ENDIF.
     ENDIF.

      "验收期间
      READ TABLE GT_VBFA_MN WITH KEY VBELV = <FS_TOTAL_SUM>-VBELN ."BINARY SEARCH.
       IF SY-SUBRC = 0.
          IF GT_VBFA_MN-VBTYP_N = 'M'.
              READ TABLE GT_VBRK_FKDAT WITH KEY  VBELN = GT_VBFA_MN-VBELN ." BINARY SEARCH.
               IF SY-SUBRC = 0.
                 <FS_TOTAL_SUM>-YSQJ = GT_VBRK_FKDAT-FKDAT+0(6).
                 ENDIF.
           ENDIF.
       ENDIF.

  ENDIF.
   IF <FS_TOTAL_SUM>-YWLX = '产品'.
     READ TABLE GT_TOTAL_CP WITH KEY YWLX = <FS_TOTAL_SUM>-YWLX BUKRS = <FS_TOTAL_SUM>-BUKRS VBELN = <FS_TOTAL_SUM>-VBELN.
      IF SY-SUBRC = 0.
       <FS_TOTAL_SUM>-ZYYWCB  = GT_TOTAL_CP-ZYYWCB. "主营业务成本
    "   <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_CP-ZYYWSR_01 * -1.  "主营业务收入-本币
    "   <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_CP-ZYYWSR_02 * -1 . "主营业务收入-凭证货币
       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_CP-ZYYWSR_01 * -1.  "主营业务收入-本币
       <FS_TOTAL_SUM>-GCCB = GT_TOTAL_CP-GCCB . "工程成本
       IF GT_TOTAL_CP-GCCB_QCYE NE 0 .
       <FS_TOTAL_SUM>-GCCB_QCYE = GT_TOTAL_CP-GCCB_QCYE . "工程成本期初余额
       ENDIF.
*       IF GT_TOTAL_CP-YSKPZHB NE 0 .
*       <FS_TOTAL_SUM>-YSKPZHB = GT_TOTAL_CP-YSKPZHB * -1 ."已收款-凭证货币值
*       ENDIF.
*       IF GT_TOTAL_CP-YSKYE NE 0 .
*       <FS_TOTAL_SUM>-YSKYE = GT_TOTAL_CP-YSKYE  ."应收款余额-凭证货币
*       ENDIF.
     ENDIF.
     LOOP AT GT_TOTAL_CP02 WHERE YWLX = <FS_TOTAL_SUM>-YWLX AND  BUKRS = <FS_TOTAL_SUM>-BUKRS AND  VBELN = <FS_TOTAL_SUM>-VBELN..
        IF GT_TOTAL_CP02-WAERS_1 = 'CNY'.
           <FS_TOTAL_SUM>-WAERS_1 = GT_TOTAL_CP02-WAERS_1.    "货币
            <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_CP02-ZYYWSR_02 * -1 . "主营业务收入-凭证货币
         IF GT_TOTAL_CP02-YSKPZHB NE 0 .
          <FS_TOTAL_SUM>-YSKPZHB = GT_TOTAL_CP02-YSKPZHB * -1 ."已收款-凭证货币值
        ENDIF.
        IF GT_TOTAL_CP02-YSKYE NE 0 .
       <FS_TOTAL_SUM>-YSKYE = GT_TOTAL_CP02-YSKYE ."应收款余额-凭证货币
       ENDIF.
          ELSE.
            CLEAR :GT_APPEND.
            MOVE GT_TOTAL_CP02 TO GT_APPEND.
            GT_APPEND-WAERS_1 = GT_TOTAL_CP02-WAERS_1.    "货币
           GT_APPEND-ZYYWSR_02 = GT_TOTAL_CP02-ZYYWSR_02 * -1 . "主营业务收入-凭证货币
         IF GT_TOTAL_CP02-YSKPZHB NE 0 .
          GT_APPEND-YSKPZHB = GT_TOTAL_CP02-YSKPZHB * -1 ."已收款-凭证货币值
        ENDIF.
        IF GT_TOTAL_CP02-YSKYE NE 0 .
       GT_APPEND-YSKYE = GT_TOTAL_CP02-YSKYE ."应收款余额-凭证货币
       ENDIF.
       APPEND GT_APPEND.
         ENDIF.

     ENDLOOP.

     READ TABLE GT_VBAK WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
     IF SY-SUBRC = 0.
       "订单类型
       <FS_TOTAL_SUM>-AUART = GT_VBAK-AUART.
         "项目名称
       PERFORM SELXMMC USING <FS_TOTAL_SUM>-VBELN CHANGING <FS_TOTAL_SUM>-XMMC.
       "订单金额
       <FS_TOTAL_SUM>-DDJE = GT_VBAK-NETWR.
       "订单货币
       <FS_TOTAL_SUM>-DDHB =  GT_VBAK-WAERK.
       "客户
        <FS_TOTAL_SUM>-KUNNR = GT_VBAK-KUNNR.
       "客户名称
          PERFORM SELKHMC USING <FS_TOTAL_SUM>-KUNNR CHANGING <FS_TOTAL_SUM>-KUNNR_NAME1.
     ENDIF.


      "产品标准成本
    READ TABLE GT_BZCB_CP_SUM WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-CPBZCB = GT_BZCB_CP_SUM-DMBTR.
    ENDIF.

    "产品成本差异
    READ TABLE GT_CBCY_CP_SUM  WITH KEY VBELN = <FS_TOTAL_SUM>-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_TOTAL_SUM>-CPCBCY = GT_CBCY_CP_SUM-DMBTR.
    ENDIF.
    "毛利率
     IF <FS_TOTAL_SUM>-ZYYWSR_01 NE 0.
     <FS_TOTAL_SUM>-MLV = (  <FS_TOTAL_SUM>-ZYYWSR_01 -  <FS_TOTAL_SUM>-ZYYWCB ) / <FS_TOTAL_SUM>-ZYYWSR_01 .
      IF  <FS_TOTAL_SUM>-MLV NE 0 .
        <FS_TOTAL_SUM>-MLV = <FS_TOTAL_SUM>-MLV * 100 .
*        <FS_TOTAL_SUM>-MLV01 = <FS_TOTAL_SUM>-MLV .
*        CONCATENATE   <FS_TOTAL_SUM>-MLV01 '%' INTO   <FS_TOTAL_SUM>-MLV01.
      ENDIF.
     ENDIF.
  ENDIF.
  IF <FS_TOTAL_SUM>-YWLX = '公司间'.
     READ TABLE GT_TOTAL_GSJ WITH KEY YWLX = <FS_TOTAL_SUM>-YWLX BUKRS = <FS_TOTAL_SUM>-BUKRS VBELN = <FS_TOTAL_SUM>-VBELN.
      IF SY-SUBRC = 0.
       <FS_TOTAL_SUM>-ZYYWCB  = GT_TOTAL_GSJ-ZYYWCB. "主营业务成本
       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_GSJ-ZYYWSR_01 * -1 .  "主营业务收入-本币
*       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_GSJ-ZYYWSR_01 * -1 .  "主营业务收入-本币
*       <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_GSJ-ZYYWSR_02 * -1 . "主营业务收入-凭证货币
*       <FS_TOTAL_SUM>-WAERS_1 = GT_TOTAL_GSJ-WAERS_1.    "货币
      ENDIF.

      CONTINUE.
  ENDIF.
  IF <FS_TOTAL_SUM>-YWLX = '其他'.
     READ TABLE GT_TOTAL_QT WITH KEY YWLX = <FS_TOTAL_SUM>-YWLX BUKRS = <FS_TOTAL_SUM>-BUKRS VBELN = <FS_TOTAL_SUM>-VBELN.
      IF SY-SUBRC = 0.
       <FS_TOTAL_SUM>-ZYYWCB  = GT_TOTAL_QT-ZYYWCB. "主营业务成本
*       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_QT-ZYYWSR_01 * -1 .  "主营业务收入-本币
*       <FS_TOTAL_SUM>-ZYYWSR_02 = GT_TOTAL_QT-ZYYWSR_02 * -1 . "主营业务收入-凭证货币
*       <FS_TOTAL_SUM>-WAERS_1 = GT_TOTAL_QT-WAERS_1.    "货币
       <FS_TOTAL_SUM>-ZYYWSR_01 = GT_TOTAL_QT-ZYYWSR_01 * -1 .  "主营业务收入-本币
       <FS_TOTAL_SUM>-GCCB = GT_TOTAL_QT-GCCB . "工程成本
       <FS_TOTAL_SUM>-GCCB_QCYE = GT_TOTAL_QT-GCCB_QCYE . "工程成本期初余额
      ENDIF.
      CONTINUE.
  ENDIF.

ENDLOOP.
APPEND LINES OF GT_APPEND TO  GT_TOTAL_SUM .
SORT GT_TOTAL_SUM BY BUKRS YWLX GJAHR VBELN.

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
                            GT_TOTAL_SUM
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
  INIT_FIELDCAT 'VBELN'          '销售订单'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'AUART'          '订单类型'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'          '项目名称'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZYYWCB'          '主营业务成本'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZYYWSR_01'          '主营业务收入-本币'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZYYWSR_02'          '主营业务收入-凭证货币'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1'          '凭证货币'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCCB'          '工程成本'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCCB_QCYE'          '工程成本-期初余额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CPBZCB'    '产品标准成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CPCBCY'          '产品成本差异'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCFY'          '工程费用'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MLV'          '毛利率%'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DDJE'          '订单金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DDHB'          '订单货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'          '客户编码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR_NAME1'          '客户名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSQJ'          '验收期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSKPZHB'          '已收款-凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YKJSPPZHB'          '已开金税票-凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSKYE'          '应收款余额-凭证货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHFHRQ'          '最后发货日期'         '' '' '' '' '' '' ''.
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
      T_OUTTAB                 = PT_DATA
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
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
      READ TABLE GT_TOTAL_SUM INTO GS_TOTAL INDEX RS_SELFIELD-TABINDEX.
      IF RS_SELFIELD-FIELDNAME = 'VBELN'
        AND GS_TOTAL-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_TOTAL-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.
 RANGES:R_VBELN FOR VBAK-VBELN.
 DATA:  T_LEN TYPE I.
  CASE  SY-UCOMM .
     WHEN '&XMMXZ'.  "跳转到项目明细账 ZFI022
      GT_TOTAL_SUM_SEL[] = GT_TOTAL_SUM[].
      DELETE GT_TOTAL_SUM_SEL WHERE ( YWLX = '项目' AND SEL EQ '' ) OR YWLX NE '项目'.
      DESCRIBE TABLE GT_TOTAL_SUM_SEL LINES T_LEN .
      IF T_LEN > 0 .
        CLEAR: R_VBELN,R_VBELN[].
        LOOP AT GT_TOTAL_SUM_SEL WHERE SEL = 'X'.
           R_VBELN-SIGN = 'I'.
           R_VBELN-OPTION = 'EQ'.
           R_VBELN-LOW = GT_TOTAL_SUM_SEL-VBELN.
           APPEND R_VBELN.
        ENDLOOP.

          SUBMIT ZFI022 WITH P_BUKRS EQ P_BUKRS
                          WITH S_VBELN IN R_VBELN  AND RETURN .

       ELSE.
         MESSAGE '请选择项目订单' TYPE 'S' DISPLAY LIKE 'E'.

       ENDIF.

     WHEN '&GSJCB_SR'. "跳转到 公司间采购成本&收入报表
       SUBMIT ZCO007_5 WITH S_BUKRS EQ  P_BUKRS
                         WITH S_BUDAT  IN S_BUDAT AND RETURN.


   ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELGT_MX1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELGT_MX1 .
SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1  A~XBLNR A~AWTYP
    B~BUZEI B~SHKZG B~DMBTR  B~HKONT B~ZUONR  B~VBEL2 B~EBELN
    INTO CORRESPONDING FIELDS OF TABLE  GT_MX1
    FROM BKPF AS A
    INNER JOIN BSEG AS B
    ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
    WHERE A~BUKRS = P_BUKRS AND BUDAT IN S_BUDAT AND B~HKONT  BETWEEN '6401000000' AND '6401999999'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELGT_MX2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELGT_MX2 .
SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1 A~XBLNR
     B~BUZEI B~SHKZG B~DMBTR B~WRBTR  B~HKONT B~VBEL2 B~EBELN
    INTO CORRESPONDING FIELDS OF TABLE GT_MX2
    FROM BKPF AS A
    INNER JOIN BSEG AS B
    ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
    WHERE A~BUKRS = P_BUKRS AND BUDAT IN S_BUDAT AND B~HKONT  BETWEEN '6001000000' AND '6001999999'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELGT_MX3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELGT_MX3 .
  SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1  A~XBLNR
     B~BUZEI B~SHKZG B~DMBTR  B~HKONT B~ZUONR  B~VBEL2
    INTO CORRESPONDING FIELDS OF TABLE GT_MX3
    FROM BKPF AS A
    INNER JOIN BSEG AS B
    ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
    WHERE A~BUKRS = P_BUKRS AND BUDAT IN S_BUDAT AND B~HKONT  EQ '1406010101'.
  CLEAR :P_DATE.
  P_DATE = S_BUDAT-LOW - 1.
   SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1  A~XBLNR
     B~BUZEI B~SHKZG B~DMBTR  B~HKONT B~ZUONR  B~VBEL2
    INTO CORRESPONDING FIELDS OF TABLE GT_MX3_01
    FROM BKPF AS A
    INNER JOIN BSEG AS B
    ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
    WHERE A~BUKRS = P_BUKRS AND BUDAT <= P_DATE AND B~HKONT  EQ '1406010101'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELGT_MX4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELGT_MX4 .
  SELECT A~BUKRS A~GJAHR A~BELNR A~BLART A~BUDAT A~WAERS AS WAERS_1
     B~BUZEI B~SHKZG  B~WRBTR  B~HKONT B~ZUONR
    INTO CORRESPONDING FIELDS OF TABLE GT_MX4
    FROM BKPF AS A
    INNER JOIN BSEG AS B
    ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
    WHERE A~BUKRS = P_BUKRS AND BUDAT IN S_BUDAT   AND B~HKONT IN  R_HKONT ."AND A~BLART IN ('SA','ZA')
    IF GT_MX4[] IS NOT INITIAL.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
     FROM BSEG
     FOR ALL ENTRIES IN GT_MX4
     WHERE BUKRS = P_BUKRS AND GJAHR = GT_MX4-GJAHR AND BELNR = GT_MX4-BELNR AND HKONT IN R2_HKONT.
      SORT GT_BSEG BY BUKRS GJAHR BELNR .
    ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IS_CPLX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_MX1_XBLNR  text
*      <--P_IS_DD  text
*----------------------------------------------------------------------*
*FORM IS_CPLX  USING    P_VBELN TYPE STRING
*              CHANGING P_DD TYPE STRING .
*
*ENDFORM.
