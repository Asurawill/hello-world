REPORT ZCO008.
"CREATED BY  IT02 150806
TABLES:LIKP ,LIPS,EKKO,VBRK,EKBE,ZCO008,VBAK.
TYPE-POOLS:SLIS.
**INTERNAL TABLE DECLARTION
DATA :
  GR_ALV     TYPE REF TO CL_SALV_TABLE,
  GR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV WITH HEADER LINE,

      G_SAVE      TYPE C VALUE 'X',
      G_VARIANT   TYPE DISVARIANT,
      GX_VARIANT  TYPE DISVARIANT,
      G_EXIT      TYPE C,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.
TYPES:BEGIN OF W_OUT .
   INCLUDE STRUCTURE ZCO008 .
 TYPES:STPRS LIKE CKMLCR-STPRS,"标准价
      PEINH LIKE CKMLCR-PEINH,"价格单位
      F_WAERS LIKE CKMLCR-WAERS,"供货货币
      CG_DJ  LIKE KONV-KBETR,"采购单价
      CG_KPEIN LIKE KONV-KPEIN,"采购单位
      CG_MENGE LIKE EKPO-MENGE,"采购数量
      CG_CGJZ  LIKE KONV-KWERT,"采购净值
      CG_SH  LIKE KONV-KWERT,"税额
      CG_CGZH LIKE FAGLFLEXT-TSL01,"采购总额
    "  CG_WAERS LIKE EKKO-WAERS,"采购订单货币
      DJCY LIKE KONV-KWERT ,"单价差异百分比
      CY    LIKE KONV-KWERT,"差异
      BJ_CBDW_CJ  LIKE  KONV-KWERT,"报价&成本单位差异
      BJ_CB_CJ LIKE KONV-KWERT,"报价&成本差异
      CYHSL LIKE KONV-KWERT,"差异换算汇率
      CJCX  TYPE C LENGTH 1,"曾经发货冲销
      SHSL  LIKE VBFA-RFMNG,"收货数量
      WQSHSL LIKE VBFA-RFMNG,"未清收货数量
      ZHSHRQ LIKE SY-DATUM,"最后收货日期
      RKSL LIKE VBFA-RFMNG,"入库数量
      WQRKSL LIKE VBFA-RFMNG,"未清入库数量
      ZHRKRQ LIKE SY-DATUM,"最后入库日期
      GHKPRQ LIKE SY-DATUM,"供货公司开票日期
      GZJE  LIKE FAGLFLEXT-TSL01 ,"采购公司发票过账金额
      ZHFPJY LIKE SY-DATUM ,"最后发票检验日期
      VBELN_1 TYPE EKKN-VBELN,"销售订单
     " VBELP  TYPE EKKN-VBELP,"销售订单行号
      VBELP  TYPE STRING,"销售订单行号
      XMMC   TYPE STRING ,"项目名称
      CGDDSUM LIKE EKBE-MENGE,  "采购订单总数量
      THBS   TYPE C ,           "退货标识
      BOX TYPE C,
      END OF W_OUT .
DATA:BEGIN OF W_VBFA .
  DATA: VBELV LIKE VBFA-VBELV,
        POSNV LIKE VBFA-POSNV,
        VBTYP_N LIKE VBFA-VBTYP_N,
        BWART LIKE VBFA-BWART,
        PLMIN LIKE VBFA-PLMIN,
        RFMNG LIKE VBFA-RFMNG,
        VBELN LIKE VBFA-VBELN,
        ERDAT LIKE VBFA-ERDAT.

  DATA:END OF W_VBFA.
 DATA:BEGIN OF WSUM_VBFA .
    DATA: VBELV LIKE VBFA-VBELV,
          POSNV LIKE VBFA-POSNV,
          RFMNG LIKE VBFA-RFMNG.
      DATA:END OF WSUM_VBFA.
DATA:BEGIN OF W_MKPF.
  DATA: MBLNR LIKE MKPF-MBLNR,
        MJAHR LIKE MKPF-MJAHR,
        BUDAT LIKE MKPF-BUDAT.
  DATA: END OF W_MKPF.
DATA:BEGIN OF W_VBRK .
  DATA: VBELN LIKE VBRK-VBELN,
       FKDAT LIKE VBRK-FKDAT.
 DATA: END OF W_VBRK.
 DATA:BEGIN OF W_EKBE.
   DATA: BELNR LIKE EKBE-BELNR,
         BUZEI LIKE EKBE-BUZEI,
         EBELN LIKE EKBE-EBELN,
         EBELP LIKE EKBE-EBELP,
         SHKZG LIKE EKBE-SHKZG,
         DMBTR LIKE EKBE-DMBTR.
  DATA:END OF W_EKBE.
DATA:BEGIN OF WSUM_EKBE.
  DATA:  EBELN LIKE EKBE-EBELN,
         EBELP LIKE EKBE-EBELP,
            DMBTR LIKE EKBE-DMBTR.
 DATA:END OF WSUM_EKBE.
DATA:BEGIN OF  W_MSEGRSEG .
    DATA: VBELN_IM TYPE VBELN ,
          VBELP_IM    TYPE POSNR,
          MBLNR    TYPE MBLNR,
          ZEILE    TYPE MSEG-ZEILE,
          MJAHR    TYPE MJAHR,
          LFBNR    TYPE LFBNR,
          LFPOS    TYPE LFPOS,
          BELNR    TYPE RE_BELNR,
          GJAHR    TYPE GJAHR.
 DATA: END OF W_MSEGRSEG.
DATA:BEGIN OF W_EKBE_SUM.
    DATA:VBELN_ST TYPE EKBE-VBELN_ST,
         VBELP_ST TYPE EKBE-VBELP_ST,
         EBELN    TYPE EKBE-EBELN,
         EBELP    TYPE EKBE-EBELP,
         MENGE    TYPE EKBE-MENGE.
DATA:END OF W_EKBE_SUM.
DATA:BEGIN OF W_FPJYRQ.
  DATA:BELNR LIKE RBKP-BELNR,
       GJAHR LIKE RBKP-GJAHR,
       BUZEI LIKE RSEG-BUZEI,
       BUDAT TYPE RBKP-BUDAT,  "过账日期
       XBLNR TYPE RSEG-XBLNR,
       EBELN TYPE RSEG-EBELN,
       EBELP TYPE RSEG-EBELP.
 DATA:END OF W_FPJYRQ.
DATA: T_FPJYRQ_NLC LIKE TABLE OF W_FPJYRQ WITH HEADER LINE. "发票检验日期
DATA: T_FPJYRQ_RLN LIKE TABLE OF W_FPJYRQ WITH HEADER LINE. "退货发票检验日期
DATA: T_EKBE_RK_SUM LIKE TABLE OF W_EKBE_SUM WITH HEADER LINE ." 入库数量汇总
DATA: T_EKBE_SH_SUM LIKE TABLE OF W_EKBE_SUM WITH HEADER LINE.  "收货数量汇总
DATA: T_CGDD_SUM   LIKE TABLE OF W_EKBE_SUM WITH HEADER LINE.  "采购订单总数量
DATA: T_EKBE_RK_SUM_RLN LIKE TABLE OF W_EKBE_SUM WITH HEADER LINE ."退货入库数量汇总
DATA: T_EKBE_SH_SUM_RLN LIKE TABLE OF W_EKBE_SUM WITH HEADER LINE . "退货收货 数量汇总
DATA: T_MSEGRSEG  LIKE TABLE OF W_MSEGRSEG WITH  HEADER LINE.
DATA:T_EKBE_NLC LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:T_EKBE_RLN LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:T_EKBE_RK LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:TSUM_EKBE LIKE TABLE OF WSUM_EKBE WITH HEADER LINE.
DATA: T_VBRK LIKE TABLE OF VBRK WITH HEADER LINE.
DATA: T_MKPF LIKE TABLE OF W_MKPF WITH HEADER LINE.
DATA:T_VBFA LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA: T_VBFAI LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA: T_VBFAI2 LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA:T_VBFAMKPF LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA: T_VBFA56 LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA: T_VBFA562 LIKE TABLE OF W_VBFA WITH HEADER LINE.
DATA:T_VBFA_h  LIKE TABLE OF W_VBFA WITH HEADER LINE.

DATA:TSUM_VBFA LIKE TABLE OF WSUM_VBFA WITH HEADER LINE.
DATA:  T_OUT TYPE TABLE OF W_OUT WITH HEADER LINE.
DATA: T_OUT_NLC TYPE TABLE OF W_OUT WITH HEADER LINE.
DATA: T_OUT_RLN TYPE TABLE OF W_OUT WITH HEADER LINE.
DATA:T_CKMLHD LIKE TABLE OF CKMLHD WITH HEADER LINE.
DATA:T_CKMLCR LIKE TABLE OF CKMLCR WITH HEADER LINE.
DATA:T_KONV LIKE TABLE OF KONV WITH HEADER LINE.
DATA:T_EKPO LIKE TABLE OF EKPO WITH HEADER LINE.
DATA:T_103 LIKE TABLE OF MSEG WITH HEADER LINE.
DATA:T_RSEG   LIKE TABLE OF RSEG WITH HEADER LINE.
DATA:T_RBKP LIKE TABLE OF RBKP WITH HEADER LINE.
DATA:T_EKKN LIKE TABLE OF EKKN WITH HEADER LINE.
DATA:T_EKKN_02 LIKE TABLE OF EKKN WITH HEADER LINE."只根据 采购订单号 查找销售订单
DATA:G_CHECK  TYPE C LENGTH 1.
DATA:C_CHECK  TYPE C LENGTH 1.
DATA:G_OK   TYPE C LENGTH 1.
DATA:C_OK  TYPE C LENGTH 1.
DATA:ALL_OK  TYPE C LENGTH 1.

DATA:IS_JG_XS TYPE C LENGTH 1.
DATA: G_OBJNAME TYPE THEAD-TDNAME.
DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
   SELECT-OPTIONS:S_FROM FOR LIPS-WERKS OBLIGATORY, "供货公司
                  S_TO FOR LIKP-WERKS OBLIGATORY, "采购公司
                  S_VBELN FOR LIKP-VBELN, "交换单号
                  S_FBUDAT FOR LIKP-WADAT_IST,  "交换单发货日期
                 " S_TBUDAT FOR LIKP-WADAT_IST, " 收货日期
                  S_EKGRP  FOR EKKO-EKGRP,"采购组
                  S_VGBEL  FOR EKKO-EBELN,"采购订单
                  S_XSDD   FOR VBAK-VBELN."销售订单
SELECTION-SCREEN END OF BLOCK B1 .
  INITIALIZATION.
  AT SELECTION-SCREEN.
     PERFORM FRM_AUTH_CHECK.
   START-OF-SELECTION.
     CHECK ALL_OK = 'X'.
     PERFORM GET_DATA.
     PERFORM DEL_DATA.
     PERFORM BUILD_FIELDCATALOG.
     PERFORM DISPLAY_ALV_REPORT.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
DATA:ERR1_MSG TYPE STRING. "供货公司 错误消息提示
DATA:ERR2_MSG TYPE STRING. "采购公司 错误消息提示
DATA: LT1_WERKS LIKE TABLE OF T001W WITH HEADER LINE.
DATA: LT2_WERKS LIKE TABLE OF T001W WITH HEADER LINE.
SELECT * INTO CORRESPONDING FIELDS OF TABLE  LT1_WERKS FROM T001W WHERE WERKS IN S_FROM . "查询 供货公司的工厂数据
SELECT * INTO CORRESPONDING FIELDS OF TABLE LT2_WERKS FROM T001W WHERE WERKS IN S_TO. "查询 采购公司的工厂数据
  SORT LT1_WERKS BY WERKS.
  SORT LT2_WERKS BY WERKS.
 "检查供货公司 是否财务人员有显示价格的权限
 LOOP AT LT1_WERKS.
     AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD '03'
                                      ID 'BUKRS' FIELD LT1_WERKS-WERKS .
     IF SY-SUBRC = 0.
       IS_JG_XS = 'Y'.
       EXIT.
     ENDIF.

 ENDLOOP.
 LOOP AT LT2_WERKS.
     AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD '03'
                                      ID 'BUKRS' FIELD LT2_WERKS-WERKS .
     IF SY-SUBRC = 0.
       IS_JG_XS = 'Y'.
       EXIT.
     ENDIF.

 ENDLOOP.
 LOOP AT LT1_WERKS.
    AUTHORITY-CHECK OBJECT  'M_MSEG_WWA '
         ID 'WERKS' FIELD LT1_WERKS-WERKS .  "判断供货公司查询权限
  IF SY-SUBRC <> 0 .
    G_CHECK = 'X'.  "检查若无权限就赋无权限提示 ， G_CHEK 为 X
    CONCATENATE '您无权查询供货公司' LT1_WERKS-WERKS  '的权限' INTO ERR1_MSG.
    EXIT.
  ENDIF.
 ENDLOOP.
 IF G_CHECK NE 'X'.
   G_OK = 'X'.  "供货公司 权限检查通过  G_OK  赋为 ”X
 ENDIF.
 LOOP AT LT2_WERKS .
    AUTHORITY-CHECK OBJECT 'M_MSEG_WWE'
       ID 'WERKS' FIELD LT2_WERKS-WERKS .  "判断采购公司的查询权限
 IF SY-SUBRC <> 0 .
   C_CHECK = 'X'.
    CONCATENATE '您无权查询采购公司' LT1_WERKS-WERKS  '的权限' INTO ERR1_MSG.
   EXIT.
 ENDIF.
 ENDLOOP.
IF C_CHECK NE 'X'.
  C_OK = 'X'. "采购公司 权限检查通过  G_OK  赋为 ”X
ENDIF.
 IF C_OK = 'X' OR G_OK = 'X'.
    ALL_OK = 'X'.  "检查供货公司 或 采购公司有一项通过就  ALL_OK 为"X"
 ELSE.
    IF ERR1_MSG IS NOT INITIAL.
        MESSAGE   ERR1_MSG TYPE 'E'.
     ENDIF.
    IF ERR2_MSG IS NOT INITIAL.
        MESSAGE   ERR2_MSG TYPE 'E'.
     ENDIF.
 ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_DATA .
 SELECT * INTO CORRESPONDING FIELDS OF TABLE T_OUT
   FROM ZCO008
   WHERE WERKS IN S_FROM AND WERKS_1 IN S_TO AND VBELN IN S_VBELN
   AND WADAT_IST IN S_FBUDAT  AND EKGRP  IN S_EKGRP
   AND VGBEL IN S_VGBEL..
 SORT T_OUT BY VBELN POSNR.
IF T_OUT[] IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE T_EKPO
   FROM EKPO
    FOR ALL ENTRIES IN T_OUT
    WHERE EBELN = T_OUT-VGBEL AND EBELP = T_OUT-VGPOS+1(5).
    SORT T_EKPO BY EBELN EBELP.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE T_EKKN
    FROM EKKN
    FOR ALL ENTRIES IN T_OUT
    WHERE EBELN = T_OUT-VGBEL AND EBELP = T_OUT-VGPOS+1(5) AND ZEKKN = '01'.
  SORT T_EKKN BY  EBELN EBELP.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE T_EKKN_02
    FROM EKKN
    FOR ALL ENTRIES IN T_OUT
    WHERE EBELN = T_OUT-VGBEL ."APPENDED BY IT02 151022 BECAUSE OF 只根据采购订单查找销售订单
  SORT T_EKKN_02 BY  EBELN .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE T_CKMLHD
    FROM CKMLHD
    FOR ALL ENTRIES IN T_OUT
    WHERE MATNR = T_OUT-MATNR AND BWKEY = T_OUT-WERKS.
    SORT T_CKMLHD BY KALNR MATNR BWKEY VBELN POSNR.
  IF T_CKMLHD[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE T_CKMLCR
      FROM CKMLCR
      FOR ALL ENTRIES IN T_CKMLHD
      WHERE KALNR = T_CKMLHD-KALNR AND  CURTP = '10' .
      SORT T_CKMLCR BY KALNR BDATJ POPER .
   ENDIF.
SELECT * INTO CORRESPONDING FIELDS OF TABLE T_KONV
  FROM KONV
  FOR ALL ENTRIES IN T_OUT
  WHERE KNUMV = T_OUT-KNUMV AND KPOSN = T_OUT-VGPOS
  AND STUNR IN ('001' ,'009' ,'060') .
SORT T_KONV BY KNUMV KPOSN STUNR.
SELECT VBELV POSNV VBTYP_N  PLMIN RFMNG VBELN ERDAT BWART
  FROM VBFA
  INTO CORRESPONDING FIELDS OF TABLE T_VBFAI
  FOR ALL ENTRIES IN T_OUT
  WHERE VBELV = T_OUT-VBELN AND POSNV = T_OUT-POSNR AND VBTYP_N = 'i' AND BWART IN ('101','102','103','104','105' ,'106'  ) .
SORT T_VBFAI BY VBELV POSNV VBELN .
T_VBFAMKPF[] = T_VBFAI[].
SORT T_VBFAMKPF BY VBELN .
DELETE ADJACENT DUPLICATES FROM T_VBFAMKPF COMPARING VBELN ."根据VBELN删除重复行
SELECT VBELV POSNV VBTYP_N
  FROM VBFA
  INTO CORRESPONDING FIELDS OF TABLE T_VBFA_h
  FOR ALL ENTRIES IN T_OUT
  WHERE  VBELV = T_OUT-VBELN AND POSNV = T_OUT-POSNR AND VBTYP_N = 'h' .
SORT T_VBFA_h BY VBELV POSNV.
SELECT MBLNR MJAHR BUDAT
  INTO CORRESPONDING FIELDS OF TABLE T_MKPF
  FROM MKPF
  FOR ALL ENTRIES IN T_VBFAMKPF
  WHERE MBLNR = T_VBFAMKPF-VBELN AND MJAHR = T_VBFAMKPF-ERDAT+0(4) .
SORT T_MKPF BY MBLNR MJAHR .

SELECT VBELV POSNV VBTYP_N  PLMIN RFMNG VBELN ERDAT
  FROM VBFA
  INTO CORRESPONDING FIELDS OF TABLE T_VBFA56
  FOR ALL ENTRIES IN T_OUT
  WHERE VBELV = T_OUT-VBELN AND POSNV = T_OUT-POSNR AND VBTYP_N IN ('5','6').
SORT T_VBFA56 BY VBELV POSNV .
SELECT VBELN FKDAT
  INTO CORRESPONDING FIELDS OF TABLE T_VBRK
  FROM VBRK
  FOR ALL ENTRIES IN T_VBFA56
  WHERE  VBELN = T_VBFA56-VBELN.
SORT T_VBRK BY VBELN FKDAT.

"取发票检验日期
T_OUT_NLC[] = T_OUT[].
T_OUT_RLN[] = T_OUT[].
DELETE T_OUT_NLC WHERE PSTYV NE 'NLC'.
DELETE T_OUT_RLN WHERE PSTYV NE 'RLN' .

IF T_OUT_NLC[] IS NOT INITIAL.
SELECT RBKP~GJAHR RBKP~BELNR  RBKP~BUDAT
  RSEG~BUZEI RSEG~XBLNR RSEG~EBELN RSEG~EBELP
  INTO CORRESPONDING FIELDS OF TABLE T_FPJYRQ_NLC
  FROM RBKP
  INNER JOIN RSEG
  ON RBKP~BELNR = RSEG~BELNR AND RBKP~GJAHR = RSEG~GJAHR
  FOR ALL ENTRIES IN T_OUT_NLC
  WHERE RBKP~STBLG EQ ''AND  RSEG~EBELN = T_OUT_NLC-VGBEL
  AND RSEG~EBELP = T_OUT_NLC-VGPOS+1(5)  .

 SORT T_FPJYRQ_NLC BY XBLNR ASCENDING EBELN ASCENDING  EBELP  ASCENDING BUDAT DESCENDING . "根据交货单、采购订单、采购行项目取出最大胡过账执法犯法
SELECT EBELN EBELP ZEKKN VGABE GJAHR BELNR BUZEI  MENGE WESBS SHKZG BWART  BUDAT VBELN_ST  VBELP_ST
  INTO CORRESPONDING  FIELDS OF TABLE T_EKBE_NLC
  FROM EKBE
  FOR ALL ENTRIES IN T_OUT_NLC
  WHERE EBELN = T_OUT_NLC-VGBEL AND EBELP = T_OUT_NLC-VGPOS+1(5) AND VGABE = '1'
     "AND VBELN_ST = T_OUT_NLC-VBELN AND VBELP_ST = T_OUT_NLC-POSNR
     AND BWART IN ('101','102','105','106','161','162','103','104').

"SORT  T_EKBE_NLC BY VBELN_ST VBELP_ST.
SORT T_EKBE_NLC BY EBELN EBELP.

ENDIF.

IF T_OUT_RLN[] IS NOT INITIAL.
SELECT EBELN EBELP ZEKKN VGABE GJAHR BELNR BUZEI  MENGE WESBS SHKZG BWART  BUDAT VBELN_ST  VBELP_ST
  INTO CORRESPONDING  FIELDS OF TABLE T_EKBE_RLN
  FROM EKBE
  FOR ALL ENTRIES IN T_OUT_RLN
  WHERE EBELN = T_OUT_RLN-VGBEL AND EBELP = T_OUT_RLN-VGPOS+1(5) AND VGABE = '1'
     AND BWART IN ('161','162').

SORT  T_EKBE_RLN BY EBELN EBELP BUDAT.
SELECT RBKP~GJAHR RBKP~BELNR  RBKP~BUDAT
  RSEG~BUZEI  RSEG~EBELN RSEG~EBELP
  INTO CORRESPONDING FIELDS OF TABLE T_FPJYRQ_RLN
  FROM RBKP
  INNER JOIN RSEG
  ON RBKP~BELNR = RSEG~BELNR AND RBKP~GJAHR = RSEG~GJAHR
  FOR ALL ENTRIES IN T_OUT_RLN
  WHERE RBKP~STBLG EQ ''AND  RSEG~EBELN = T_OUT_RLN-VGBEL
  AND RSEG~EBELP = T_OUT_RLN-VGPOS+1(5)  .

 SORT T_FPJYRQ_RLN BY  EBELN ASCENDING  EBELP  ASCENDING BUDAT DESCENDING . "采购订单、采购行项目取出最大胡过账执法犯法

ENDIF.


*SELECT  EBELN EBELP VGABE SHKZG DMBTR " BELNR BUZEI
*  FROM EKBE
*  INTO CORRESPONDING FIELDS OF TABLE T_EKBE
*  FOR ALL ENTRIES IN T_OUT
*  WHERE EBELN = T_OUT-VGBEL AND EBELP = T_OUT-VGPOS+1(5) AND VGABE = '2'.     " = T_OUT-POSNR+2(4) ."AND VGABE = '2'.
*  SORT T_EKBE BY  EBELN EBELP.


 ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
CLEAR IT_FIELDCAT[].
IF IS_JG_XS  = 'Y'.
   PERFORM FRM_FILL_CAT USING :
          1  '' 'WERKS' '供货公司'   ''       , "供货公司
           2  '' 'WERKS_1' '采购公司'  ''        , "采购公司
           3  '' 'VBELN' '交货单'      ''    , "交货单
           4  '' 'POSNR' '交货单行号'   ''        , "交货单行号
           5  '' 'MATNR' '物料号'   ''       , "物料号
           6  '' 'MAKTX' '物料描述'  ''        , "物料描述
           7  '' 'LFIMG' '交货数量'   ''       , "交货数量
     "      8  '' 'BUDAT' '过账日期'             , "过账日期
           8  '' 'MEINS' '单位'     ''        , "单位
           9  '' 'STPRS' '供货公司标准成本'     'F_WAERS'        , "供货公司标准成本
           10  '' 'PEINH' '标准成本价格单位'   ''          , "标准成本价格单位
           11  '' 'F_WAERS' '供货公司货币'   ''          , "供货公司货币
          "    11  '' 'M1' '总成本'             , "总成本
         "        12  '' 'M2' '总数量'             , "总数量
           12 '' 'EKGRP' '采购组'   ''       , "采购组
           13 '' 'EKNAM' '采购组描述'   ''       , "采购组描述
           14 '' 'VGBEL' '采购订单'   ''       , "采购订单
           15 '' 'VGPOS' '采购订单行号'  ''        , "M采购订单行号
           16 '' 'CG_MENGE' '采购数量'  ''        , "采购数量
           17 '' 'CG_DJ' '采购单价'  'WAERS'        , "采购单价
           18 '' 'CG_KPEIN' '采购单位'  ''        , "采购单位
         "  16 '' 'CG_KPEIN' '采购单位'          , "净值
           19 '' 'CG_CGJZ' '采购净值'    'WAERS'      , "采购净值
           20 '' 'CG_SH' '税额'   'WAERS'        , "税额
           21 '' 'CG_CGZH' '采购总额'  'WAERS'        , "采购总额
           22 '' 'WAERS' '采购订单货币'  ''        , "采购订单货币
           23 '' 'BJ_CBDW_CJ' '报价&成本单位差异'  ''        , "报价&成本单位差异
  "         24 '' 'BJ_CB_CJ' '报价&成本差异'    ''     , "报价&成本差异
           25 '' 'CYHSL' '差异换算汇率'  ''        , "差异换算汇率
           26 '' 'CJCX' '曾经发货冲销'  ''        , "差异换算汇率
           26 '' 'WADAT_IST' '交货单发货日期' ''         , "交货单发货日期
           27 '' 'SHSL' '收货数量'   ''       , "收库数量
           28 '' 'RKSL' '入库数量'   ''       , "入库数量
           9 '' 'WQRKSL' '未清入库数量'   ''       , "未清入库数量
           30 '' 'ZHRKRQ' '最后入库日期'    ''     , "最后入库日期
           31 '' 'GHKPRQ' '供货公司开票日期'  ''        , "供货公司开票日期
       "      29 '' 'GZJE' '采购公司发票过账金额'          , "净值
*           32 '' 'KDAUF' '销售订单'  ''        , "销售订单
*           33 '' 'KDPOS' '销售订单行项目'   ''       , "销售订单行项目
           32 '' 'VBELN_1' '销售订单'  ''        , "销售订单
           33 '' 'VBELP' '销售订单行项目'   ''       , "销售订单行项目
           34 '' 'XMMC'  '项目名称' '',"项目名称
           35 '' 'ZHFPJY' '最后发票检验日期' '', "最后发票检验日期
           36 '' 'CGDDSUM' '采购订单入库总数量' '', "采购订单入库总数量
           37 '' 'THBS' '退货标识' ''. "退货标识



  ELSE.
     PERFORM FRM_FILL_CAT USING :
          1  '' 'WERKS' '供货公司'   ''       , "供货公司
           2  '' 'WERKS_1' '采购公司'  ''        , "采购公司
           3  '' 'VBELN' '交货单'      ''    , "交货单
           4  '' 'POSNR' '交货单行号'   ''        , "交货单行号
           5  '' 'MATNR' '物料号'   ''       , "物料号
           6  '' 'MAKTX' '物料描述'  ''        , "物料描述
           7  '' 'LFIMG' '交货数量'   ''       , "交货数量
     "      8  '' 'BUDAT' '过账日期'             , "过账日期
           8  '' 'MEINS' '单位'     ''        , "单位
*           9  '' 'STPRS' '供货公司标准成本'     'F_WAERS'        , "供货公司标准成本
           10  '' 'PEINH' '标准成本价格单位'   ''          , "标准成本价格单位
           11  '' 'F_WAERS' '供货公司货币'   ''          , "供货公司货币
          "    11  '' 'M1' '总成本'             , "总成本
         "        12  '' 'M2' '总数量'             , "总数量
           12 '' 'EKGRP' '采购组'   ''       , "采购组
           13 '' 'EKNAM' '采购组描述'   ''       , "采购组描述
           14 '' 'VGBEL' '采购订单'   ''       , "采购订单
           15 '' 'VGPOS' '采购订单行号'  ''        , "M采购订单行号
           16 '' 'CG_MENGE' '采购数量'  ''        , "采购数量
*           17 '' 'CG_DJ' '采购单价'  'WAERS'        , "采购单价
           18 '' 'CG_KPEIN' '采购单位'  ''        , "采购单位
         "  16 '' 'CG_KPEIN' '采购单位'          , "净值
*           19 '' 'CG_CGJZ' '采购净值'    'WAERS'      , "采购净值
*           20 '' 'CG_SH' '税额'   'WAERS'        , "税额
*           21 '' 'CG_CGZH' '采购总额'  'WAERS'        , "采购总额
           22 '' 'WAERS' '采购订单货币'  ''        , "采购订单货币
*           23 '' 'BJ_CBDW_CJ' '报价&成本单位差异'  ''        , "报价&成本单位差异
  "         24 '' 'BJ_CB_CJ' '报价&成本差异'    ''     , "报价&成本差异
*           25 '' 'CYHSL' '差异换算汇率'  ''        , "差异换算汇率
           26 '' 'CJCX' '曾经发货冲销'  ''        , "差异换算汇率
           26 '' 'WADAT_IST' '交货单发货日期' ''         , "交货单发货日期
           27 '' 'SHSL' '收货数量'   ''       , "收库数量
           28 '' 'RKSL' '入库数量'   ''       , "入库数量
           9 '' 'WQRKSL' '未清入库数量'   ''       , "未清入库数量
           30 '' 'ZHRKRQ' '最后入库日期'    ''     , "最后入库日期
           31 '' 'GHKPRQ' '供货公司开票日期'  ''        , "供货公司开票日期
       "      29 '' 'GZJE' '采购公司发票过账金额'          , "净值
*           32 '' 'KDAUF' '销售订单'  ''        , "销售订单
*           33 '' 'KDPOS' '销售订单行项目'   ''       , "销售订单行项目
           32 '' 'VBELN_1' '销售订单'  ''        , "销售订单
           33 '' 'VBELP' '销售订单行项目'   ''       , "销售订单行项目
           34 '' 'XMMC'  '项目名称' '',"项目名称
           35 '' 'ZHFPJY' '最后发票检验日期' '', "最后发票检验日期
           36 '' 'CGDDSUM' '采购订单入库总数量' '', "采购订单入库总数量
           37 '' 'THBS' '退货标识' ''. "退货标识

 ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_REPORT .
 DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.

* L_LAYOUT-CWIDTH_OPT = 'X'.
  L_LAYOUT-BOX_FIELDNAME = 'BOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
    "  I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_SAVE                   = 'X'
      I_GRID_SETTINGS          = L_GRID_SETTINGS
      IS_LAYOUT                = L_LAYOUT
      IS_VARIANT               = G_VARIANT
    TABLES
      T_OUTTAB                 = T_OUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
ENDFORM.
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME U_WAERS.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  LW_FIELDCAT-CFIELDNAME =  U_WAERS .
  IF U_FNAME = 'LFIMG'.
   LW_FIELDCAT-QFIELDNAME = 'MEINS' .
  ENDIF.
 " LW_FIELDCAT-NO_ZERO   = 'X'.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "FRM_FILL_CAT
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZCO008_STATUS'.
ENDFORM.                    "SET_PF_STATUS
FORM TOP-OF-PAGE.
*ALV HEADER DECLARATIONS
  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.
* TITLE
  WA_HEADER-TYP  = 'H'.
  WA_HEADER-INFO =  SY-TITLE."'
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* DATE
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'DATE: '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "TODAYS DATE
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "TOP-OF-PAGE


*&---------------------------------------------------------------------*
*&      FORM  DEL_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM DEL_DATA .
  DATA:EXCH_RATE TYPE TABLE OF  BAPI1093_0  WITH HEADER LINE..
  "统计退货数量汇总
LOOP AT T_EKBE_RLN.
  CLEAR :T_EKBE_RK_SUM_RLN ,T_EKBE_SH_SUM_RLN.
  T_EKBE_RK_SUM_RLN-EBELN    = T_EKBE_RLN-EBELN.
  T_EKBE_RK_SUM_RLN-EBELP    = T_EKBE_RLN-EBELP.
  T_EKBE_SH_SUM_RLN-EBELN    = T_EKBE_RLN-EBELN.
  T_EKBE_SH_SUM_RLN-EBELP    = T_EKBE_RLN-EBELP.

  IF T_EKBE_RLN-SHKZG = 'H'.
      T_EKBE_RK_SUM_RLN-MENGE = T_EKBE_RLN-MENGE * -1.
  ELSE.
      T_EKBE_RK_SUM_RLN-MENGE = T_EKBE_RLN-MENGE .
  ENDIF.
  COLLECT  T_EKBE_RK_SUM_RLN .  " 统计退货入库数量
   IF T_EKBE_RLN-SHKZG = 'H'.
      T_EKBE_SH_SUM_RLN-MENGE = T_EKBE_RLN-MENGE * -1.
      T_EKBE_SH_SUM_RLN-MENGE =  T_EKBE_SH_SUM_RLN-MENGE + T_EKBE_RLN-WESBS * -1.
  ELSE.
      T_EKBE_SH_SUM_RLN-MENGE = T_EKBE_RLN-MENGE .
       T_EKBE_SH_SUM_RLN-MENGE =  T_EKBE_SH_SUM_RLN-MENGE + T_EKBE_RLN-WESBS .
  ENDIF.
  COLLECT T_EKBE_SH_SUM_RLN.   "统计退货收货数量

ENDLOOP.
SORT T_EKBE_RK_SUM_RLN BY EBELN EBELP .
SORT T_EKBE_SH_SUM_RLN BY EBELN EBELP.
 "统计入库数量汇总
 LOOP AT T_EKBE_NLC .
   CLEAR :W_EKBE_SUM,T_EKBE_RK_SUM,T_EKBE_SH_SUM,T_CGDD_SUM .
   W_EKBE_SUM-VBELN_ST = T_EKBE_NLC-VBELN_ST .
   W_EKBE_SUM-VBELP_ST = T_EKBE_NLC-VBELP_ST.
   W_EKBE_SUM-EBELN    = T_EKBE_NLC-EBELN.
   W_EKBE_SUM-EBELP    = T_EKBE_NLC-EBELP.
   T_CGDD_SUM-EBELN    = T_EKBE_NLC-EBELN.
   T_CGDD_SUM-EBELP    = T_EKBE_NLC-EBELP.
    IF T_EKBE_NLC-BWART = '101' OR T_EKBE_NLC-BWART = '102' OR T_EKBE_NLC-BWART = '105' OR T_EKBE_NLC-BWART = '106'.
      CLEAR:W_EKBE_SUM-MENGE.
       IF T_EKBE_NLC-SHKZG = 'H'.
         T_CGDD_SUM-MENGE = T_EKBE_NLC-MENGE * -1.
        ELSE.
           T_CGDD_SUM-MENGE = T_EKBE_NLC-MENGE .
      ENDIF.

    COLLECT  T_CGDD_SUM .  "采购订单入库总数量

    ENDIF.

    IF T_EKBE_NLC-BWART = '101' OR T_EKBE_NLC-BWART = '102' OR T_EKBE_NLC-BWART = '105' OR T_EKBE_NLC-BWART = '106' OR T_EKBE_NLC-BWART = '161' OR T_EKBE_NLC-BWART = '162'.
      CLEAR:W_EKBE_SUM-MENGE.
      IF T_EKBE_NLC-SHKZG = 'H'.
         W_EKBE_SUM-MENGE = T_EKBE_NLC-MENGE * -1.
        ELSE.
           W_EKBE_SUM-MENGE = T_EKBE_NLC-MENGE .
      ENDIF.

    COLLECT  W_EKBE_SUM INTO T_EKBE_RK_SUM.  "汇总入库数量

   ENDIF.

    IF T_EKBE_NLC-BWART = '101' OR T_EKBE_NLC-BWART = '102' OR T_EKBE_NLC-BWART = '103' OR T_EKBE_NLC-BWART = '104' OR T_EKBE_NLC-BWART = '161' OR T_EKBE_NLC-BWART = '162'.
      CLEAR :W_EKBE_SUM-MENGE.
      IF T_EKBE_NLC-SHKZG = 'H'.
         T_EKBE_NLC-MENGE = T_EKBE_NLC-MENGE * -1.
         T_EKBE_NLC-WESBS = T_EKBE_NLC-WESBS * -1.
      ENDIF.
    W_EKBE_SUM-MENGE =  T_EKBE_NLC-MENGE  +  T_EKBE_NLC-WESBS .
    COLLECT  W_EKBE_SUM INTO T_EKBE_SH_SUM.   "汇总收货数量

   ENDIF.
 ENDLOOP.
 SORT T_EKBE_RK_SUM BY VBELN_ST VBELP_ST EBELN EBELP.
 SORT T_EKBE_SH_SUM BY VBELN_ST VBELP_ST EBELN EBELP.
 SORT T_CGDD_SUM    BY  EBELN EBELP.
 T_EKBE_RK[] = T_EKBE_NLC[].
 DELETE T_EKBE_RK WHERE BWART = '103' OR BWART = '104'.
 SORT T_EKBE_RK BY VBELN_ST ASCENDING VBELP_ST  ASCENDING  EBELN ASCENDING EBELP ASCENDING BUDAT DESCENDING . "取入库交货单 、采购订单、采购订单行号最大的日期
 DELETE ADJACENT DUPLICATES FROM T_EKBE_RK COMPARING VBELN_ST VBELP_ST EBELN EBELP.
 LOOP AT T_OUT .
   "判断是否为退货
   IF T_OUT-PSTYV = 'RLN'.
     T_OUT-THBS = 'X'.
     T_OUT-LFIMG = T_OUT-LFIMG * -1 .   "交货数量
   ENDIF.
    "取EKKN 表的VBELN 、VBELP 作为销售订单、销售订单行号
  READ TABLE T_EKKN WITH KEY EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5).
  IF SY-SUBRC = 0.
    T_OUT-VBELN_1 = T_EKKN-VBELN.
    T_OUT-VBELP   = T_EKKN-VBELP.
   ENDIF.
   IF T_OUT-VBELN_1 NOT IN S_XSDD.
      DELETE T_OUT.
      CONTINUE.
    ENDIF.

   "追加只根据采购订单 查找EKKN的VBELN 销售订单号 IT02 151022
   IF T_OUT-VBELN_1 IS INITIAL.
     READ TABLE T_EKKN_02 WITH KEY EBELN = T_OUT-VGBEL BINARY SEARCH.
     IF SY-SUBRC = 0.
        T_OUT-VBELN_1 = T_EKKN_02-VBELN.
        T_OUT-VBELP = 'X'.
      ENDIF.
   ENDIF.
   IF T_OUT-VBELN_1 NOT IN  S_XSDD.
      DELETE T_OUT.
      CONTINUE.
    ENDIF.
   IF  T_OUT-VBELN_1 IS NOT INITIAL.
     " 取项目名称 - 销售订单抬头文本
    G_OBJNAME = T_OUT-VBELN_1.
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
      LOOP AT  IT_LINES INTO WA_LINES .
        CONCATENATE T_OUT-XMMC  WA_LINES-TDLINE INTO T_OUT-XMMC.
      ENDLOOP.
    ENDIF.
   ENDIF.
    CLEAR EXCH_RATE[].
    IF T_OUT-SOBKZ = 'E'.
       READ TABLE T_CKMLHD WITH KEY MATNR = T_OUT-MATNR BWKEY = T_OUT-WERKS VBELN = T_OUT-KDAUF POSNR = T_OUT-KDPOS .
        IF SY-SUBRC = 0 .
          READ TABLE T_CKMLCR WITH KEY KALNR = T_CKMLHD-KALNR CURTP = '10' BDATJ = T_OUT-WADAT_IST+0(4) POPER = T_OUT-WADAT_IST+4(2).
          IF SY-SUBRC = 0.
             T_OUT-STPRS = T_CKMLCR-STPRS ."标准价
             T_OUT-PEINH = T_CKMLCR-PEINH. "价格单位
             T_OUT-F_WAERS = T_CKMLCR-WAERS. "供货货币
            ENDIF.
         ENDIF.
    ENDIF.
    IF T_OUT-SOBKZ NE 'Z' OR T_OUT-STPRS = 0 .
      READ TABLE T_CKMLHD WITH KEY MATNR = T_OUT-MATNR BWKEY = T_OUT-WERKS  .
        IF SY-SUBRC = 0 .
          READ TABLE T_CKMLCR WITH KEY KALNR = T_CKMLHD-KALNR CURTP = '10' BDATJ = T_OUT-WADAT_IST+0(4) POPER = T_OUT-WADAT_IST+4(2).
          IF SY-SUBRC = 0.
             T_OUT-STPRS = T_CKMLCR-STPRS ."标准价
             T_OUT-PEINH = T_CKMLCR-PEINH. "价格单位
             T_OUT-F_WAERS = T_CKMLCR-WAERS. "供货货币
            ENDIF.
         ENDIF.
     ENDIF.
  "曾经发货冲销
   READ TABLE T_VBFA_h WITH KEY VBELV = T_OUT-VBELN  POSNV = T_OUT-POSNR BINARY SEARCH.
   IF SY-SUBRC = 0.
     T_OUT-CJCX = 'X'. " 若子层类别 VBTYP_N有 ‘h",则显示 X ，否则为空
    ENDIF.
   "收货数量
   CLEAR: T_VBFAI2 ,T_VBFAI2[].

  "
   IF T_OUT-THBS EQ 'X'.


      "收货数量
   READ TABLE T_EKBE_SH_SUM_RLN WITH KEY  EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) BINARY SEARCH .
    IF SY-SUBRC = 0.
      T_OUT-SHSL = T_EKBE_SH_SUM_RLN-MENGE.
    ENDIF.
  "入库数量
    READ TABLE T_EKBE_RK_SUM_RLN WITH KEY  EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) BINARY SEARCH .
    IF SY-SUBRC = 0.
      T_OUT-RKSL = T_EKBE_RK_SUM_RLN-MENGE.
*     " 采购订单入库总数量
*     T_OUT-CGDDSUM = T_EKBE_RK_SUM_RLN-MENGE.
    ENDIF.
       "取最后发票检验日期
    READ TABLE T_FPJYRQ_RLN WITH KEY  EBELN = T_OUT-VGBEL  EBELP = T_OUT-VGPOS+1(5) .
     IF SY-SUBRC = 0.
       T_OUT-ZHFPJY = T_FPJYRQ_RLN-BUDAT .
     ENDIF.
       IF T_OUT-RKSL = 0 .
     "入库数量为0 ,日期为空
        CLEAR T_OUT-ZHRKRQ .
       ELSE.
        READ TABLE T_EKBE_RLN WITH KEY   EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) .
          IF SY-SUBRC = 0 .
            T_OUT-ZHRKRQ = T_EKBE_RLN-BUDAT.  "最后入货日期
          ENDIF.

      ENDIF.
     ELSE.
    " 采购订单入库总数量
    READ TABLE T_CGDD_SUM WITH KEY EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) BINARY SEARCH .
    IF SY-SUBRC = 0.
      T_OUT-CGDDSUM = T_CGDD_SUM-MENGE.
    ENDIF.
        "收货数量
   READ TABLE T_EKBE_SH_SUM WITH KEY VBELN_ST = T_OUT-VBELN VBELP_ST = T_OUT-POSNR EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) BINARY SEARCH .
    IF SY-SUBRC = 0.
      T_OUT-SHSL = T_EKBE_SH_SUM-MENGE.
    ENDIF.
  "入库数量
    READ TABLE T_EKBE_RK_SUM WITH KEY VBELN_ST = T_OUT-VBELN VBELP_ST = T_OUT-POSNR  EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) BINARY SEARCH .
    IF SY-SUBRC = 0.
      T_OUT-RKSL = T_EKBE_RK_SUM-MENGE.
    ENDIF.
      "取最后发票检验日期
    READ TABLE T_FPJYRQ_NLC WITH KEY XBLNR = T_OUT-VBELN EBELN = T_OUT-VGBEL  EBELP = T_OUT-VGPOS+1(5) .
     IF SY-SUBRC = 0.
       T_OUT-ZHFPJY = T_FPJYRQ_NLC-BUDAT .
     ENDIF.
       IF T_OUT-RKSL = 0 .
     "入库数量为0 ,日期为空
    CLEAR T_OUT-ZHRKRQ .
    ELSE.
      READ TABLE T_EKBE_RK WITH KEY  VBELN_ST = T_OUT-VBELN VBELP_ST = T_OUT-POSNR EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5) .
      IF SY-SUBRC = 0 .
        T_OUT-ZHRKRQ = T_EKBE_RK-BUDAT.  "最后入货日期
       ENDIF.

  ENDIF.
   ENDIF.



  SORT T_VBFAI2 BY VBELN DESCENDING .

    CLEAR:T_VBFA562, T_VBFA562[].
    LOOP AT T_VBFA56  WHERE VBELV = T_OUT-VBELN AND POSNV = T_OUT-POSNR  .
      APPEND T_VBFA56 TO T_VBFA562.
    ENDLOOP.
    SORT T_VBFA562 BY VBELN DESCENDING .
    READ TABLE T_VBFA562 INDEX 1.
    IF SY-SUBRC = 0.
       IF T_VBFA562-VBTYP_N = '5'.
         READ TABLE T_VBRK WITH KEY VBELN = T_VBFA562-VBELN .
         IF SY-SUBRC = 0.
            T_OUT-GHKPRQ = T_VBRK-FKDAT ."发票日期
          ENDIF.
         ELSEIF T_VBFA562-VBTYP_N = '6'.
           T_OUT-GHKPRQ = ''.
         ENDIF.
     ENDIF.
  DATA:KAWRT001 LIKE KONV-KAWRT ."条件基值
  READ TABLE T_EKPO WITH KEY EBELN = T_OUT-VGBEL  EBELP = T_OUT-VGPOS+1(5).
  IF SY-SUBRC = 0.
    T_OUT-CG_MENGE = T_EKPO-MENGE."采购数量
   ENDIF.
   "若 采购订单入库总数量＝采购订单数量，则 收货数量＝入库数量＝交货数量
  IF T_OUT-CG_MENGE = T_OUT-CGDDSUM .
     T_OUT-SHSL = T_OUT-LFIMG .   "收货数量
     T_OUT-RKSL = T_OUT-LFIMG .    "入库数量
  ENDIF.
  T_OUT-WQRKSL = T_OUT-LFIMG - T_OUT-RKSL ."未清入库数量
  DATA:KWERT_001 LIKE KONV-KWERT ."存储 KONV 的0001的KWERT价格.
  CLEAR KWERT_001.
  READ TABLE T_KONV WITH KEY KNUMV = T_OUT-KNUMV KPOSN = T_OUT-VGPOS STUNR = '001'.
  IF SY-SUBRC = 0.
    T_OUT-CG_DJ = T_KONV-KWERT * T_KONV-KPEIN / T_OUT-CG_MENGE."采购单价

    T_OUT-CG_KPEIN = T_KONV-KPEIN."采购价格单位
    T_OUT-CG_CGZH = T_KONV-KWERT * T_OUT-LFIMG / T_OUT-CG_MENGE ."采购总额

    KWERT_001 = T_KONV-KWERT .
   ENDIF.
  READ TABLE T_KONV WITH KEY KNUMV = T_OUT-KNUMV KPOSN = T_OUT-VGPOS STUNR = '009'.
  IF SY-SUBRC = 0.
     T_OUT-CG_SH = KWERT_001 * T_OUT-LFIMG / T_OUT-CG_MENGE / ( 1 + T_KONV-KBETR / 1000 ) * ( T_KONV-KBETR / 1000  ) .
    .
  ENDIF.
  T_OUT-CG_CGJZ =  T_OUT-CG_CGZH  -  T_OUT-CG_SH . "采购净值
  IF T_OUT-THBS EQ 'X'.
    T_OUT-CG_DJ   =  T_OUT-CG_DJ * -1.    "采购单价
    T_OUT-CG_CGZH =  T_OUT-CG_CGZH * -1.   "采购总额
    T_OUT-CG_CGJZ =  T_OUT-CG_CGJZ * -1.    "采购净值
    T_OUT-CG_SH   =  T_OUT-CG_SH  * -1.     "采购税额
  ENDIF.
  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      RATE_TYPE          =  'M'
      FROM_CURR         =  T_OUT-F_WAERS
      TO_CURRNCY       =  T_OUT-WAERS
      DATE                    =  SY-DATUM
   IMPORTING
     EXCH_RATE          = EXCH_RATE .

     T_OUT-CYHSL = EXCH_RATE-EXCH_RATE.

  IF T_OUT-STPRS NE 0 .
     T_OUT-BJ_CBDW_CJ = ( T_OUT-CG_DJ * T_OUT-CYHSL / T_OUT-CG_KPEIN - T_OUT-STPRS / T_OUT-PEINH ) /  ( T_OUT-STPRS / T_OUT-PEINH )."报价&成本单位差异
  "   T_OUT-BJ_CB_CJ = ( ( T_OUT-CG_DJ * T_OUT-CYHSL ) * T_OUT-LFIMG / T_OUT-CG_KPEIN - T_OUT-STPRS / T_OUT-CG_KPEIN * T_OUT-LFIMG   ) / ( T_OUT-STPRS * T_OUT-LFIMG / T_OUT-CG_KPEIN ) ."报价&成本差异
  ENDIF.



  "取EKKN 表的VBELN 、VBELP 作为销售订单、销售订单行号
  READ TABLE T_EKKN WITH KEY EBELN = T_OUT-VGBEL EBELP = T_OUT-VGPOS+1(5).
  IF SY-SUBRC = 0.
    T_OUT-VBELN_1 = T_EKKN-VBELN.
    T_OUT-VBELP   = T_EKKN-VBELP.
   ENDIF.

   MODIFY T_OUT.
 ENDLOOP.


ENDFORM.
