*&---------------------------------------------------------------------*
*& Report  ZFI009
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/03/1
*& Request       :
*& Descriptions  : 资产台帐报表
*&
*& Modify by     : IT02
*& Modify date   : 20160122
*& Request       :
*& Descriptions  :累计购置值 、累计折旧、每月折旧
*&
*&---------------------------------------------------------------------*
REPORT ZFI009.
************************************************************************
* Tables
************************************************************************
TABLES:ANLP,ANLA,CSKT,ZFI009.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA.
        INCLUDE TYPE ZFI009.
TYPES:ZBOX    TYPE C,
      LJGZZ   TYPE ANLC-KANSW, "累计购置值
      LJZJ    TYPE ANLC-KANSW, "累计折旧值
      ZMJZ    TYPE ANLC-KANSW, "账面净值
      NAME1   TYPE LFA1-NAME1, "供应商名称
      KSZJND  TYPE N LENGTH 4,   "开始折旧年度
      KSZJYF  TYPE N LENGTH 2,  "开始折旧月分
      YSYYF   TYPE I, "已使用月分
      SYYF    TYPE I, "剩余月份
      MYZJ001 TYPE ANLP-NAFAZ, "1月折旧
      MYZJ002 TYPE ANLP-NAFAZ, "2月折旧
      MYZJ003 TYPE ANLP-NAFAZ, "3月折旧
      MYZJ004 TYPE ANLP-NAFAZ, "4月折旧
      MYZJ005 TYPE ANLP-NAFAZ, "5月折旧
      MYZJ006 TYPE ANLP-NAFAZ, "6月折旧
      MYZJ007 TYPE ANLP-NAFAZ, "7月折旧
      MYZJ008 TYPE ANLP-NAFAZ, "8月折旧
      MYZJ009 TYPE ANLP-NAFAZ, "9月折旧
      MYZJ010 TYPE ANLP-NAFAZ, "10月折旧
      MYZJ011 TYPE ANLP-NAFAZ, "11月折旧
      MYZJ012 TYPE ANLP-NAFAZ, "12月折旧

*&--代码添加 BY HANDYBY 04.07.2017 10:58:50  BEGIN
      CAUFN   TYPE ANLZ-CAUFN , "内部订单号
      KTEXT2  TYPE COAS-KTEXT , "内部订单描述
      LJJHWZJ TYPE ANLP-NAFAZ , "累计计划外折旧
*&--代码添加 BY HANDYBY 04.07.2017 10:58:50  END
      "20171115增加新的折旧价值字段
      JZTZ    TYPE ANLC-NAFAV,"价值调整
      GZJZ    TYPE ANLC-NAFAV,"2、购置价值
      PTZJ    TYPE ANLC-NAFAV,"3、普通折旧
      JHWZJ   TYPE ANLC-NAFAV,"4、计划外折旧
      ZMJZ_NEW   TYPE ANLC-NAFAV,"账面净值

*      ANLUE   TYPE P DECIMALS 4, "功率
      END OF TY_DATA.

TYPES:BEGIN OF TY_LFA1,
        LIFNR TYPE LFA1-LIFNR,    "供应商号
        NAME1 TYPE LFA1-NAME1,    "供应商名称
        SPRAS TYPE LFA1-SPRAS,    "语言代码
      END OF TY_LFA1.

TYPES:BEGIN OF TY_ITEM_LJGZZ.
TYPES: BUKRS TYPE ANEK-BUKRS,    "公司代码
       GJAHR TYPE ANEK-GJAHR,    "年度
       ANLN1 TYPE ANEK-ANLN1,    "固定资产号
       ANLN2 TYPE ANEK-ANLN2,    "
       LNRAN TYPE ANEK-LNRAN,
       BUDAT TYPE ANEK-BUDAT,
       AFABE TYPE ANEP-AFABE,
       ZUJHR TYPE ANEP-ZUJHR,
       ANBTR TYPE ANEP-ANBTR,
       END OF TY_ITEM_LJGZZ .

TYPES:BEGIN OF TY_SUM_LJGZZ.
TYPES: BUKRS TYPE ANEK-BUKRS,    "公司代码
       GJAHR TYPE ANEK-GJAHR,    "年度
       ANLN1 TYPE ANEK-ANLN1,    "固定资产号
*&--代码添加 BY HANDYBY 26.07.2017 11:49:36  BEGIN
       ANLN2 TYPE ANEK-ANLN2 , " 次级资产编号
*&--代码添加 BY HANDYBY 26.07.2017 11:49:36  END
       ANBTR TYPE ANEP-ANBTR,    "本位币金额
       END OF TY_SUM_LJGZZ.

TYPES:BEGIN OF TY_SUM_MYZJ.
TYPES: BUKRS   TYPE ANLP-BUKRS,
       GJAHR   TYPE ANLP-GJAHR,
       ANLN1   TYPE ANLP-ANLN1,
*&--代码添加 BY HANDYBY 26.07.2017 11:50:59  BEGIN
       ANLN2   TYPE ANLP-ANLN2,
*&--代码添加 BY HANDYBY 26.07.2017 11:50:59  END
       MYZJ001 TYPE ANLP-NAFAZ, "1月折旧
       MYZJ002 TYPE ANLP-NAFAZ, "2月折旧
       MYZJ003 TYPE ANLP-NAFAZ, "3月折旧
       MYZJ004 TYPE ANLP-NAFAZ, "4月折旧
       MYZJ005 TYPE ANLP-NAFAZ, "5月折旧
       MYZJ006 TYPE ANLP-NAFAZ, "6月折旧
       MYZJ007 TYPE ANLP-NAFAZ, "7月折旧
       MYZJ008 TYPE ANLP-NAFAZ, "8月折旧
       MYZJ009 TYPE ANLP-NAFAZ, "9月折旧
       MYZJ010 TYPE ANLP-NAFAZ, "10月折旧
       MYZJ011 TYPE ANLP-NAFAZ, "11月折旧
       MYZJ012 TYPE ANLP-NAFAZ, "12月折旧
       END OF TY_SUM_MYZJ.





************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA   TYPE TABLE OF TY_DATA.
DATA GS_DATA   TYPE TY_DATA.

DATA GT_DATA_1 TYPE TABLE OF TY_DATA.
DATA GS_DATA_1 TYPE TY_DATA.

DATA GT_LFA1   TYPE TABLE OF TY_LFA1.
DATA GS_LFA1   TYPE TY_LFA1.

DATA GT_ITEM_LJGZZ   TYPE TABLE OF TY_ITEM_LJGZZ.
DATA GS_ITEM_LJGZZ   TYPE TY_ITEM_LJGZZ.

DATA GT_BSEG   TYPE TABLE OF BSEG.
DATA GS_BSEG   TYPE BSEG.

DATA GT_SUM_LJGZZ  TYPE TABLE OF TY_SUM_LJGZZ.
DATA GS_SUM_LJGZZ  TYPE TY_SUM_LJGZZ.

DATA GT_ANLP TYPE TABLE OF ANLP.
DATA:GS_ANLP TYPE ANLP.


DATA GT_ANLP_1 TYPE TABLE OF ANLP.
DATA:GS_ANLP_1 TYPE ANLP.

DATA:GT_SUM_MYZJ TYPE TABLE OF TY_SUM_MYZJ,
     GS_SUM_MYZJ TYPE TY_SUM_MYZJ.

DATA:P_MONAT TYPE BKPF-MONAT.    "期间
DATA:P_BEG  TYPE SY-DATUM .      "开始日期
DATA:P_END  TYPE SY-DATUM.        "结束日期

RANGES:R_ANLN1 FOR ANLA-ANLN1. "固定资产编号范围

*&--代码添加 BY HANDYBY 26.07.2017 13:28:13  BEGIN
" 取累计购置价值
DATA: BEGIN OF GS_ANLC ,
        BUKRS TYPE ANLC-BUKRS,
        ANLN1 TYPE ANLC-ANLN1,
        ANLN2 TYPE ANLC-ANLN2,
        GJAHR TYPE ANLC-GJAHR,
        "价值调整
        NAFAV TYPE ANLC-NAFAV,
        AAFAV TYPE ANLC-AAFAV,
        NAFAL TYPE ANLC-NAFAL,
        AAFAL TYPE ANLC-AAFAL,
        "价值调整
        ANSWL TYPE ANLC-ANSWL,
        KANSW TYPE ANLC-KANSW,
        "普通折旧
        KNAFA TYPE ANLC-ANSWL,
        NAFAG TYPE ANLC-KANSW,
        "计划外折旧
        KAAFA TYPE ANLC-KAAFA,
        AAFAG TYPE ANLC-AAFAG,
      END OF GS_ANLC.
DATA GT_ANLC LIKE TABLE OF GS_ANLC .

* 取资产次级编号
DATA: BEGIN OF GS_ANLA ,
        BUKRS TYPE ANLA-BUKRS,
        ANLN1 TYPE ANLA-ANLN1,
        ANLN2 TYPE ANLA-ANLN2,
      END OF GS_ANLA .
DATA GT_ANLA LIKE TABLE OF GS_ANLA .
*&--代码添加 BY HANDYBY 26.07.2017 13:28:13  END

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  IF &1 = 'X'.
   gw_lvc-no_zero = 'X'.
  ENDIF.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.
  IF gw_lvc-fieldname = 'ANLUE'.
    gw_lvc-ref_field = 'MENGE'.
    gw_lvc-ref_table = 'EKPO'.
  ENDIF.
  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
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
************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_GJAHR  FOR ANLP-GJAHR NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM+0(4),"会计年度
                S_PERAF  FOR ANLP-PERAF NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM+4(2),"折旧期间
                S_BUKRS  FOR ANLP-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,                      "公司代码
                S_ANLKL  FOR ANLA-ANLKL,                                                           "资产分类
                S_ANLN1  FOR ANLP-ANLN1,                                                           "资产编号
*&--代码添加 BY HANDYBY 26.07.2017 11:38:41  BEGIN
                S_ANLN2 FOR ANLP-ANLN2,                                                            " 次级资产编号
*&--代码添加 BY HANDYBY 26.07.2017 11:38:41  END
                S_KOSTL  FOR CSKT-KOSTL,                                                           "成本中心
                S_ORD41  FOR ANLA-ORD41,                                                           "使用状态
                S_LIFNR  FOR ANLA-LIFNR,                                                           "供应商
                S_TXT50  FOR ZFI009-TXT50.                                                        "资产描述
"PARAMETERS:     P_DEAKT  AS CHECKBOX TYPE CHAR1.                                                   "包含不活动状态

SELECTION-SCREEN END OF BLOCK BLK1.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
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
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD S_BUKRS-LOW.
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
  DATA L_DATA TYPE ANLZ-ADATU.

*根据选择屏幕设定资产时间段的有效起始日期和结束日期

  P_MONAT = S_PERAF-LOW+1(2) .

  CONCATENATE S_GJAHR-LOW P_MONAT '01' INTO P_BEG .

  "获取结束日期
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = P_BEG
    IMPORTING
      LAST_DAY_OF_MONTH = P_END
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  " CONCATENATE S_GJAHR-LOW S_PERAF-LOW+1(2) '28' INTO L_DATA.

  L_DATA = P_END .

  "*排除不活动资产
  " IF P_DEAKT = ''.
  SELECT * FROM ZFI009
    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    WHERE BUKRS IN S_BUKRS
    AND   ANLKL IN S_ANLKL
    AND   ANLN1 IN S_ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:39:53  BEGIN
    AND ANLN2 IN S_ANLN2
*&--代码添加 BY HANDYBY 26.07.2017 11:39:53  END
    AND   KOSTL IN S_KOSTL
    AND   ORD41 IN S_ORD41
*      AND   DEAKT = '00000000'
    AND   BDATU > P_END
    AND   LIFNR IN S_LIFNR
    AND   TXT50 IN S_TXT50
    AND   GJAHR EQ S_GJAHR-LOW .

*&--代码添加 BY HANDYBY 26.07.2017 15:05:55  BEGIN
* 取资产次级编号
*  DATA: LT_DATA LIKE GT_DATA,
*        LS_DATA LIKE LINE OF LT_DATA.
*  SELECT BUKRS
*         ANLN1
*         ANLN2
*    INTO CORRESPONDING FIELDS OF TABLE GT_ANLA
*    FROM ANLA
*     FOR ALL ENTRIES IN GT_DATA
*   WHERE BUKRS = GT_DATA-BUKRS
*     AND ANLN1 = GT_DATA-ANLN1
*     AND ANLN2 IN S_ANLN2 .
*  SORT GT_ANLA BY BUKRS ANLN1 ANLN2 .
*  LOOP AT GT_DATA INTO GS_DATA .
*    READ TABLE GT_ANLA WITH KEY BUKRS = GS_DATA-BUKRS
*                                ANLN1 = GS_DATA-ANLN1
*                                BINARY SEARCH TRANSPORTING NO FIELDS .
*    IF SY-SUBRC = 0 .
*      LOOP AT GT_ANLA INTO GS_ANLA FROM SY-TABIX .
*        IF GS_ANLA-BUKRS = GS_DATA-BUKRS AND
*          GS_ANLA-ANLN1 = GS_DATA-ANLN1 .
*          MOVE-CORRESPONDING GS_DATA TO LS_DATA .
*          LS_DATA-ANLN2 = GS_ANLA-ANLN2 .
*          APPEND LS_DATA TO LT_DATA .
*          CLEAR LS_DATA .
*        ELSE ..
*          EXIT .
*        ENDIF.
*        CLEAR GS_ANLA .
*      ENDLOOP.
*    ENDIF.
*    CLEAR GS_DATA .
*  ENDLOOP.
*
*  APPEND LINES OF LT_DATA TO GT_DATA .
*&--代码添加 BY HANDYBY 26.07.2017 15:05:55  END

*&--代码注释 BY HANDYBY 26.07.2017 11:43:51  BEGIN
*  SORT GT_DATA BY BUKRS ASCENDING GJAHR ASCENDING ANLN1  ASCENDING BDATU ASCENDING .
*&--代码注释 BY HANDYBY 26.07.2017 11:43:51  END
*&--代码添加 BY HANDYBY 26.07.2017 11:43:56  BEGIN
  SORT GT_DATA BY BUKRS ASCENDING GJAHR ASCENDING ANLN1 ANLN2  ASCENDING BDATU ASCENDING .
*&--代码添加 BY HANDYBY 26.07.2017 11:43:56  END

*&--代码注释 BY HANDYBY 26.07.2017 11:44:10  BEGIN
*  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING BUKRS GJAHR  ANLN1 .
*&--代码注释 BY HANDYBY 26.07.2017 11:44:10  END
*&--代码添加 BY HANDYBY 26.07.2017 11:44:28  BEGIN
  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING BUKRS GJAHR  ANLN1 ANLN2 .
  DELETE GT_DATA WHERE ANLN2 IS INITIAL .
*&--代码添加 BY HANDYBY 26.07.2017 11:44:28  END

  CHECK GT_DATA IS NOT INITIAL.

*查询指定期间的折旧数据
*    SELECT * FROM ZFI009
*      INTO CORRESPONDING FIELDS OF TABLE GT_DATA_1
*      FOR ALL ENTRIES IN GT_DATA
*      WHERE BUKRS = GT_DATA-BUKRS
*      AND   ANLKL = GT_DATA-ANLKL
*      AND   ANLN1 = GT_DATA-ANLN1
*      AND   KOSTL = GT_DATA-KOSTL
*      AND   ORD41 = GT_DATA-ORD41
**      AND   DEAKT = '00000000'
*      AND   GJAHR IN S_GJAHR.


* 资产期间价值
  SELECT * FROM ANLP
    INTO CORRESPONDING FIELDS OF TABLE GT_ANLP
    WHERE BUKRS IN S_BUKRS
    AND   GJAHR EQ S_GJAHR-LOW
    AND   ANLN1 IN S_ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:44:50  BEGIN
    AND ANLN2 IN S_ANLN2
*&--代码添加 BY HANDYBY 26.07.2017 11:44:50  END
    .

*&--代码注释 BY HANDYBY 26.07.2017 11:45:06  BEGIN
*   SORT GT_ANLP BY BUKRS ASCENDING  GJAHR ASCENDING  ANLN1  ASCENDING .
*&--代码注释 BY HANDYBY 26.07.2017 11:45:06  END
*&--代码添加 BY HANDYBY 26.07.2017 11:45:11  BEGIN
  SORT GT_ANLP BY BUKRS ASCENDING  GJAHR ASCENDING  ANLN1 ANLN2  ASCENDING  .
*&--代码添加 BY HANDYBY 26.07.2017 11:45:11  END

  MOVE GT_ANLP TO GT_ANLP_1.
  DELETE GT_ANLP_1 WHERE PERAF NE S_PERAF-LOW .

*&--代码注释 BY HANDYBY 26.07.2017 11:45:58  BEGIN
*    SORT GT_ANLP_1 BY  BUKRS ASCENDING  GJAHR ASCENDING  ANLN1  ASCENDING  AFBNR DESCENDING  .
*&--代码注释 BY HANDYBY 26.07.2017 11:45:58  END
*&--代码添加 BY HANDYBY 26.07.2017 11:46:08  BEGIN
  SORT GT_ANLP_1 BY  BUKRS ASCENDING  GJAHR ASCENDING  ANLN1 ANLN2  ASCENDING  AFBNR DESCENDING  .
*&--代码添加 BY HANDYBY 26.07.2017 11:46:08  END

*&--代码添加 BY HANDYBY 26.07.2017 11:46:18  BEGIN
* DELETE ADJACENT DUPLICATES FROM GT_ANLP_1 COMPARING BUKRS GJAHR ANLN1  .
*&--代码添加 BY HANDYBY 26.07.2017 11:46:18  END
*&--代码添加 BY HANDYBY 26.07.2017 11:46:24  BEGIN
  DELETE ADJACENT DUPLICATES FROM GT_ANLP_1 COMPARING BUKRS GJAHR ANLN1 ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 11:46:24  END

*&--代码添加 BY HANDYBY 26.07.2017 13:15:41  BEGIN

* 取累计购置价值
  SELECT BUKRS
         ANLN1
         ANLN2
         GJAHR
         NAFAV
         AAFAV
         NAFAL
         AAFAL
         ANSWL
         KANSW
         KNAFA
         NAFAG
         KAAFA
         AAFAG
    INTO CORRESPONDING FIELDS OF TABLE GT_ANLC
    FROM ANLC
   WHERE GJAHR IN S_GJAHR
     AND BUKRS IN S_BUKRS
     AND ANLN1 IN S_ANLN1
     AND ANLN2 IN S_ANLN2 .
  SORT GT_ANLC BY BUKRS GJAHR ANLN1 ANLN2  .

*&--代码添加 BY HANDYBY 26.07.2017 13:15:41  END

*  查询购置值
  SELECT  ANEK~BUKRS ANEK~GJAHR ANEK~ANLN1 ANEK~ANLN2 ANEK~LNRAN ANEK~BUDAT
          ANEP~AFABE ANEP~ZUJHR  ANEP~ANBTR
    INTO CORRESPONDING FIELDS OF TABLE GT_ITEM_LJGZZ
    FROM ANEK
    INNER JOIN ANEP
    ON ANEK~BUKRS = ANEP~BUKRS
    AND ANEK~ANLN1 = ANEP~ANLN1
    AND ANEK~ANLN2 = ANEP~ANLN2
    AND ANEK~GJAHR = ANEP~GJAHR
    AND ANEK~LNRAN = ANEP~LNRAN
    WHERE ANEK~BUKRS = S_BUKRS-LOW
    AND  ANEK~GJAHR = S_GJAHR-LOW
    AND  ANEK~ANLN1 IN S_ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:47:22  BEGIN
    AND ANEK~ANLN2 IN S_ANLN2
    AND ANEP~BWASL NOT IN ('Z10','Z11','Z20','Z21')
*&--代码添加 BY HANDYBY 26.07.2017 11:47:22  END
    AND  ANEK~BUDAT <= P_END .

*&--代码注释 BY HANDYBY 26.07.2017 11:47:45  BEGIN
*    SORT GT_ITEM_LJGZZ BY BUKRS GJAHR ANLN1  .
*&--代码注释 BY HANDYBY 26.07.2017 11:47:45  END
*&--代码添加 BY HANDYBY 26.07.2017 11:47:49  BEGIN
  SORT GT_ITEM_LJGZZ BY BUKRS GJAHR ANLN1 ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 11:47:49  END


*增加供应商描述   ADD BY HANDWY 2015-7-16
  SELECT LIFNR NAME1 SPRAS FROM LFA1
    INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
    FOR ALL ENTRIES IN GT_DATA
    WHERE LIFNR = GT_DATA-LIFNR.


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
  "统计资产总值  add by IT02 151218
  LOOP AT GT_ITEM_LJGZZ INTO GS_ITEM_LJGZZ.
    CLEAR: GS_SUM_LJGZZ.
    GS_SUM_LJGZZ-BUKRS = GS_ITEM_LJGZZ-BUKRS.
    GS_SUM_LJGZZ-GJAHR = GS_ITEM_LJGZZ-GJAHR.
    GS_SUM_LJGZZ-ANLN1 = GS_ITEM_LJGZZ-ANLN1.
*&--代码添加 BY HANDYBY 26.07.2017 11:49:59  BEGIN
    GS_SUM_LJGZZ-ANLN2 = GS_ITEM_LJGZZ-ANLN2.
*&--代码添加 BY HANDYBY 26.07.2017 11:49:59  END
    GS_SUM_LJGZZ-ANBTR = GS_ITEM_LJGZZ-ANBTR.
    COLLECT GS_SUM_LJGZZ INTO GT_SUM_LJGZZ.
  ENDLOOP.

*&--代码注释 BY HANDYBY 26.07.2017 11:50:12  BEGIN
*    SORT GT_SUM_LJGZZ BY BUKRS GJAHR ANLN1 .
*&--代码注释 BY HANDYBY 26.07.2017 11:50:12  END
*&--代码添加 BY HANDYBY 26.07.2017 11:50:19  BEGIN
  SORT GT_SUM_LJGZZ BY BUKRS GJAHR ANLN1 ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 11:50:19  END

  "根据公司代码 、年度、、资产号汇总资产各月的未过账折旧值 add by it02 160122
  LOOP AT GT_ANLP INTO GS_ANLP  .
    AT NEW  ANLN2 .
      CLEAR:GS_SUM_MYZJ.
      GS_SUM_MYZJ-BUKRS = GS_ANLP-BUKRS.
      GS_SUM_MYZJ-GJAHR = GS_ANLP-GJAHR.
      GS_SUM_MYZJ-ANLN1 = GS_ANLP-ANLN1.
*&--代码添加 BY HANDYBY 26.07.2017 11:50:36  BEGIN
      GS_SUM_MYZJ-ANLN2 = GS_ANLP-ANLN2.
*&--代码添加 BY HANDYBY 26.07.2017 11:50:36  END
    ENDAT.
    CASE GS_ANLP-PERAF.
      WHEN '001'.
        GS_SUM_MYZJ-MYZJ001 =   GS_ANLP-NAFAZ.        "1月折旧
      WHEN '002'.                              "2月折旧
        GS_SUM_MYZJ-MYZJ002 =   GS_ANLP-NAFAZ.
      WHEN '003'.                        "3月折旧
        GS_SUM_MYZJ-MYZJ003 =  GS_ANLP-NAFAZ.
      WHEN '004'.                            "4月折旧
        GS_SUM_MYZJ-MYZJ004 =  GS_ANLP-NAFAZ.
      WHEN '005'.                                "5月折旧
        GS_SUM_MYZJ-MYZJ005 =   GS_ANLP-NAFAZ.
      WHEN '006'.                                "6月折旧
        GS_SUM_MYZJ-MYZJ006 =  GS_ANLP-NAFAZ.
      WHEN '007'.                              "7月折旧
        GS_SUM_MYZJ-MYZJ007 =   GS_ANLP-NAFAZ.
      WHEN '008'.                                 "8月折旧
        GS_SUM_MYZJ-MYZJ008 =  GS_ANLP-NAFAZ.
      WHEN '009'.                              "9月折旧
        GS_SUM_MYZJ-MYZJ009 =  GS_ANLP-NAFAZ.
      WHEN '010'.                                 "10月折旧
        GS_SUM_MYZJ-MYZJ010 =  GS_ANLP-NAFAZ.
      WHEN '011'.                              "11月折旧
        GS_SUM_MYZJ-MYZJ011 = GS_ANLP-NAFAZ.
      WHEN '012'.                               "12月折旧
        GS_SUM_MYZJ-MYZJ012 =  GS_ANLP-NAFAZ.
    ENDCASE.
    COLLECT GS_SUM_MYZJ INTO GT_SUM_MYZJ.
  ENDLOOP.

*&--代码注释 BY HANDYBY 26.07.2017 11:51:21  BEGIN
*    SORT GT_SUM_MYZJ BY BUKRS GJAHR ANLN1.
*&--代码注释 BY HANDYBY 26.07.2017 11:51:21  END
*&--代码添加 BY HANDYBY 26.07.2017 11:51:25  BEGIN
  SORT GT_SUM_MYZJ BY BUKRS GJAHR ANLN1 ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 11:51:25  END


*处理累计购置值，累计折旧值，账面净值
  DATA L_DATE TYPE CHAR6.


*&--代码添加 BY HANDYBY 26.07.2017 13:19:25  BEGIN

* 取累计计划外折旧
  DATA: BEGIN OF LS_ANLB ,
          BUKRS TYPE ANLB-BUKRS,
          ANLN1 TYPE ANLB-ANLN1,
          ANLN2 TYPE ANLB-ANLN2,
          AFABG TYPE ANLB-AFABG,
        END OF LS_ANLB .
  DATA LT_ANLB LIKE TABLE OF LS_ANLB .
  SELECT BUKRS
         ANLN1
         ANLN2
         AFABG
    INTO CORRESPONDING FIELDS OF TABLE LT_ANLB
    FROM ANLB
     FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = GT_DATA-BUKRS
     AND ANLN1 = GT_DATA-ANLN1
     AND ANLN2 IN S_ANLN2 .
  SORT LT_ANLB BY BUKRS ANLN1 ANLN2 .

* XXX
  DATA: BEGIN OF LS_ANLC ,
          BUKRS TYPE ANLC-BUKRS,
          ANLN1 TYPE ANLC-ANLN1,
          ANLN2 TYPE ANLC-ANLN2,
          GJAHR TYPE ANLC-GJAHR,
          KAAFA TYPE ANLC-KAAFA,
          AAFAG TYPE ANLC-AAFAG,
        END OF LS_ANLC .
  DATA LT_ANLC LIKE TABLE OF LS_ANLC .
  DATA: BEGIN OF LS_ANLP ,
          BUKRS TYPE ANLP-BUKRS,
          ANLN1 TYPE ANLP-ANLN1,
          ANLN2 TYPE ANLP-ANLN2,
          GJAHR TYPE ANLP-GJAHR,
          AAFAG TYPE ANLP-AAFAG,
          AAFAZ TYPE ANLP-AAFAZ,
        END OF LS_ANLP .
  DATA LT_ANLP LIKE TABLE OF LS_ANLP .

  SELECT BUKRS
         ANLN1
         ANLN2
         GJAHR
         KAAFA
         AAFAG
    INTO CORRESPONDING FIELDS OF TABLE LT_ANLC
    FROM ANLC
     FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = GT_DATA-BUKRS
     AND ANLN1 = GT_DATA-ANLN1
     AND ANLN2 = GT_DATA-ANLN2
     AND GJAHR IN S_GJAHR .
  SORT LT_ANLC BY BUKRS ANLN1 ANLN2 GJAHR .

  SELECT BUKRS
         ANLN1
         ANLN2
         GJAHR
         AAFAG
         AAFAZ
    INTO CORRESPONDING FIELDS OF TABLE LT_ANLP
    FROM ANLP
     FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = GT_DATA-BUKRS
     AND ANLN1 = GT_DATA-ANLN1
     AND ANLN2 = GT_DATA-ANLN2
     AND GJAHR IN S_GJAHR
     AND PERAF IN S_PERAF .
  SORT LT_ANLP BY BUKRS ANLN1 ANLN1 GJAHR .

*&--代码添加 BY HANDYBY 26.07.2017 13:19:25  END


  LOOP AT GT_DATA INTO GS_DATA.

    CLEAR GS_DATA-KANSW .  " 清除自建表里的累计购置值

**功率转字段
*    GS_DATA-ZGL = GS_DATA-ANLUE.

*资本化日期 根据选择屏幕的会计年度 + 期间， 资本化日期 晚于该期间的资产进行排除
    CLEAR L_DATE.
    CONCATENATE S_GJAHR-LOW   S_PERAF-LOW+1(2) INTO L_DATE.
    IF GS_DATA-AKTIV+0(6) > L_DATE.
      " DELETE GT_DATA INDEX SY-TABIX.
      "  CONTINUE.
      CLEAR:GS_DATA-AKTIV.   " 更改为 空值  MODIFY BY IT02 151221
    ENDIF.

**不活动资产排除
*    IF P_DEAKT = ''.
*      IF GS_DATA-DEAKT+0(6) <= L_DATE AND GS_DATA-DEAKT <> '00000000'.
*        DELETE GT_DATA INDEX SY-TABIX.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

*不活动日期
    IF GS_DATA-DEAKT+0(6) > L_DATE.
      GS_DATA-DEAKT = '00000000'.
    ENDIF.

*计购置值=KANSW+ANSWL       *change by handwy
*    GS_DATA-LJGZZ = GS_DATA-KANSW + GS_DATA-ANSWL.

* 计算累计购置值

*&--代码注释 BY HANDYBY 26.07.2017 15:42:24  BEGIN
*    IF GS_DATA-GJAHR = S_GJAHR-LOW  .
*      GS_DATA-LJGZZ = GS_DATA-KANSW .
*    ENDIF.
*&--代码注释 BY HANDYBY 26.07.2017 15:42:24  END

    READ TABLE GT_SUM_LJGZZ INTO GS_SUM_LJGZZ WITH KEY BUKRS = S_BUKRS-LOW
                                                       GJAHR = S_GJAHR-LOW
                                                       ANLN1 = GS_DATA-ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:52:31  BEGIN
                                                 ANLN2 = GS_DATA-ANLN2
*&--代码添加 BY HANDYBY 26.07.2017 11:52:31  END
                                                           BINARY SEARCH.
    IF SY-SUBRC = 0.
*&--代码注释 BY HANDYBY 26.07.2017 15:42:53  BEGIN
*    GS_DATA-LJGZZ = GS_DATA-LJGZZ + GS_SUM_LJGZZ-ANBTR.                 "modify by it02 151221
*&--代码注释 BY HANDYBY 26.07.2017 15:42:53  END
*&--代码添加 BY HANDYBY 26.07.2017 15:43:01  BEGIN
      GS_DATA-LJGZZ = GS_SUM_LJGZZ-ANBTR.                 "modify by it02 151221
*&--代码添加 BY HANDYBY 26.07.2017 15:43:01  END
    ENDIF.
*&--代码添加 BY HANDYBY 26.07.2017 13:31:20  BEGIN
    READ TABLE GT_ANLC WITH KEY BUKRS = GS_DATA-BUKRS
                                ANLN1 = GS_DATA-ANLN1
                                ANLN2 = GS_DATA-ANLN2
                                GJAHR = S_GJAHR-LOW BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_ANLC INTO GS_ANLC FROM SY-TABIX .
        IF GS_ANLC-BUKRS = GS_DATA-BUKRS AND
            GS_ANLC-ANLN1 = GS_DATA-ANLN1 AND
            GS_ANLC-ANLN2 = GS_DATA-ANLN2 AND
            GS_ANLC-GJAHR = S_GJAHR-LOW .
          GS_DATA-LJGZZ = GS_DATA-LJGZZ + GS_ANLC-KANSW.
          "价值调整
          GS_DATA-JZTZ = GS_DATA-JZTZ + GS_ANLC-NAFAV +  GS_ANLC-AAFAV
                        + GS_ANLC-NAFAL + GS_ANLC-AAFAL.
          "购置价值
          GS_DATA-GZJZ = GS_DATA-GZJZ + GS_ANLC-KANSW + GS_ANLC-ANSWL.
          "普通折旧
          GS_DATA-PTZJ = GS_DATA-PTZJ + GS_ANLC-KNAFA +  GS_ANLC-NAFAG
                       + GS_ANLC-NAFAV + GS_ANLC-NAFAL.
          "计划外折旧
          GS_DATA-JHWZJ = GS_DATA-JHWZJ + GS_ANLC-KAAFA +  GS_ANLC-AAFAG
                        + GS_ANLC-AAFAV + GS_ANLC-AAFAL.
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_ANLC .
      ENDLOOP.
      "账面净值(新增)
      GS_DATA-ZMJZ_NEW =  GS_DATA-GZJZ +  GS_DATA-PTZJ + GS_DATA-JHWZJ.
    ENDIF.

* 计算累计计划外折旧
    READ TABLE LT_ANLB INTO LS_ANLB WITH KEY BUKRS = GS_DATA-BUKRS
                                             ANLN1 = GS_DATA-ANLN1
                                             ANLN2 = GS_DATA-ANLN2 BINARY SEARCH .
    IF SY-SUBRC = 0 .
      IF LS_ANLB-AFABG+0(6) GT L_DATE .
        GS_DATA-LJJHWZJ = 0 .
      ELSE .
        READ TABLE LT_ANLP INTO LS_ANLP WITH KEY BUKRS = GS_DATA-BUKRS
                                                 GJAHR = S_GJAHR-LOW
                                                 ANLN1 = GS_DATA-ANLN1
                                                 ANLN2 = GS_DATA-ANLN2 BINARY SEARCH .
        IF SY-SUBRC = 0 .
          READ TABLE LT_ANLC INTO LS_ANLC WITH KEY BUKRS = GS_DATA-BUKRS
                                                    ANLN1 = GS_DATA-ANLN1
                                                    ANLN2 = GS_DATA-ANLN2
                                                    GJAHR = S_GJAHR-LOW BINARY SEARCH .
          IF LS_ANLP-AAFAZ NE 0 .
            IF LS_ANLC IS NOT INITIAL .
              GS_DATA-LJJHWZJ = LS_ANLC-KAAFA + LS_ANLP-AAFAG + LS_ANLP-AAFAZ .
              CLEAR LS_ANLC .
            ENDIF.
          ELSEIF LS_ANLP-AAFAZ EQ 0 OR LS_ANLP-AAFAZ IS INITIAL .
            IF LS_ANLC IS NOT INITIAL .
              GS_DATA-LJJHWZJ = LS_ANLC-KAAFA + LS_ANLC-AAFAG  .
            ENDIF .
          ENDIF.
        ELSE .
          READ TABLE LT_ANLC INTO LS_ANLC WITH KEY BUKRS = GS_DATA-BUKRS
                                                    ANLN1 = GS_DATA-ANLN1
                                                    ANLN2 = GS_DATA-ANLN2
                                                    GJAHR = S_GJAHR-LOW BINARY SEARCH .
          IF SY-SUBRC = 0 .
            GS_DATA-LJJHWZJ = LS_ANLC-KAAFA + LS_ANLC-AAFAG  .
            CLEAR LS_ANLC .
          ENDIF.
          CLEAR LS_ANLP .
        ENDIF.
      ENDIF.
      CLEAR LS_ANLB .
    ENDIF.

    IF GS_DATA-DEAKT NE '00000000' .
      GS_DATA-LJJHWZJ = 0 .
    ENDIF.

*&--代码添加 BY HANDYBY 26.07.2017 13:31:20  END

*供应商字段取值添加判断 2015-3-16 by handwy
    IF GS_DATA-LIFNR IS INITIAL .
      GS_DATA-LIFNR = GS_DATA-LIEFE.
    ENDIF.

*读取供应商名称
    IF GS_DATA-LIFNR IS NOT INITIAL .
      READ TABLE GT_LFA1 INTO GS_LFA1
      WITH KEY LIFNR = GS_DATA-LIFNR
               SPRAS = SY-LANGU.
      IF SY-SUBRC = 0.
        GS_DATA-NAME1 = GS_LFA1-NAME1.
      ENDIF.
    ENDIF.

*累计折旧值（当期间有折旧，查询当前期间折旧值，累计折旧值 = 当前期间折旧值。当期间没有折旧，累计折旧值 = 以往累计折旧值）
*    READ TABLE GT_DATA_1 INTO GS_DATA_1
*    WITH KEY BUKRS = GS_DATA-BUKRS
*             ANLN1 = GS_DATA-ANLN1
*             GJAHR = S_GJAHR-LOW
*             PERAF = S_PERAF-LOW.
*    IF SY-SUBRC = 0.
*      GS_DATA-LJZJ = GS_DATA_1-KNAFA + GS_DATA_1-NAFAG_1 + GS_DATA_1-NAFAZ.
*    ELSE.
*      READ TABLE GT_DATA INTO GS_DATA_1
*      WITH KEY BUKRS   = GS_DATA-BUKRS
*               ANLN1   = GS_DATA-ANLN1
*               GJAHR_1 = S_GJAHR-LOW.
*      IF SY-SUBRC = 0.
*        GS_DATA-LJZJ = GS_DATA_1-KNAFA + GS_DATA_1-NAFAG.
*      ENDIF.
*    ENDIF.
    "modified by it02 20160120 需变动累计汇总 ANLP的NAFAZ 和NAFAG
    IF GS_DATA-GJAHR = S_GJAHR-LOW  .
      GS_DATA-LJZJ = GS_DATA-KNAFA  + GS_DATA-NAFAG. .
    ENDIF.
    READ TABLE GT_ANLP_1 INTO GS_ANLP_1
    WITH KEY  BUKRS = GS_DATA-BUKRS
              GJAHR = S_GJAHR-LOW
              ANLN1 = GS_DATA-ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:57:06  BEGIN
              ANLN2 = GS_DATA-ANLN2
*&--代码添加 BY HANDYBY 26.07.2017 11:57:06  END
              BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_DATA-LJZJ = GS_DATA-LJZJ -  GS_DATA-NAFAG + GS_ANLP_1-NAFAZ + GS_ANLP_1-NAFAG.
    ENDIF.


*当查询年度及期间 < 折旧开始日期，累计折旧金额为0
    IF   S_PERAF-LOW+1(2) < GS_DATA-AFABG+4(2)
    AND  S_GJAHR-LOW <= GS_DATA-AFABG+0(4).
      GS_DATA-LJZJ = 0.
    ELSEIF  S_GJAHR-LOW < GS_DATA-AFABG+0(4).
      GS_DATA-LJZJ = 0.
    ENDIF.

*不活动日期等于0 清空累计折旧日期
    IF GS_DATA-DEAKT <> '00000000'.
      "GS_DATA-LJZJ  = ''.
      CLEAR GS_DATA-LJZJ.
    ENDIF.

*账面净值
*&--代码注释 BY HANDYBY 26.07.2017 14:09:31  BEGIN
*    GS_DATA-ZMJZ = GS_DATA-LJGZZ + GS_DATA-LJZJ.
*&--代码注释 BY HANDYBY 26.07.2017 14:09:31  END
*&--代码添加 BY HANDYBY 26.07.2017 14:09:36  BEGIN
    GS_DATA-ZMJZ = GS_DATA-LJGZZ + GS_DATA-LJZJ + GS_DATA-LJJHWZJ .
*&--代码添加 BY HANDYBY 26.07.2017 14:09:36  END


*当资产已经报废，计购置值为0时候，账面值也应该为0
    IF GS_DATA-LJGZZ = 0.
      GS_DATA-ZMJZ = 0.
    ENDIF.

    " ADD BY IT02 151217 begin
    GS_DATA-KSZJND = GS_DATA-AFABG+0(4).     "开始 折旧年度
    GS_DATA-KSZJYF = GS_DATA-AFABG+4(2).     "开始折旧月份
* 剩余月份
*  若不活动日期在不等于"00000000" ,则显示为0
    IF GS_DATA-DEAKT NE '00000000' .
      GS_DATA-SYYF = 0.
    ENDIF.

    IF GS_DATA-DEAKT EQ '00000000' OR GS_DATA-DEAKT  EQ ''.
*      已使用月份
      GS_DATA-YSYYF = ( S_GJAHR-LOW - GS_DATA-KSZJND ) * 12 + S_PERAF-LOW+1(2) - GS_DATA-KSZJYF + 1.
      IF  GS_DATA-YSYYF <= 0.
        GS_DATA-SYYF = 0 .
      ELSE.
        GS_DATA-SYYF = GS_DATA-NDJAR  * 12 + GS_DATA-NDPER - GS_DATA-YSYYF .

        IF  GS_DATA-SYYF < 0 .

          GS_DATA-SYYF = 0.
        ENDIF.

      ENDIF.

    ENDIF.
    "每月折旧
    READ TABLE GT_SUM_MYZJ   INTO GS_SUM_MYZJ WITH KEY BUKRS = S_BUKRS-LOW
                                                       GJAHR = S_GJAHR-LOW
                                                       ANLN1 = GS_DATA-ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:57:39  BEGIN
                                                      ANLN2 = GS_DATA-ANLN2
*&--代码添加 BY HANDYBY 26.07.2017 11:57:39  END
                                                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_DATA-MYZJ001 =  GS_SUM_MYZJ-MYZJ001.        "1月折旧
      GS_DATA-MYZJ002 =  GS_SUM_MYZJ-MYZJ002.        "2月折旧
      GS_DATA-MYZJ003 =  GS_SUM_MYZJ-MYZJ003.        "3月折旧
      GS_DATA-MYZJ004 =  GS_SUM_MYZJ-MYZJ004.        "4月折旧
      GS_DATA-MYZJ005 =  GS_SUM_MYZJ-MYZJ005.        "5月折旧
      GS_DATA-MYZJ006 =  GS_SUM_MYZJ-MYZJ006.        "6月折旧
      GS_DATA-MYZJ007 =   GS_SUM_MYZJ-MYZJ007.        "7月折旧
      GS_DATA-MYZJ008 =   GS_SUM_MYZJ-MYZJ008.        "8月折旧
      GS_DATA-MYZJ009 =   GS_SUM_MYZJ-MYZJ009.        "9月折旧
      GS_DATA-MYZJ010 =   GS_SUM_MYZJ-MYZJ010.        "10月折旧
      GS_DATA-MYZJ011 =   GS_SUM_MYZJ-MYZJ011.        "11月折旧
      GS_DATA-MYZJ012 =   GS_SUM_MYZJ-MYZJ012.        "12月折旧
    ENDIF.


    " ADD BY IT02 151217 END
    MODIFY GT_DATA FROM GS_DATA.
    CLEAR GS_DATA.

  ENDLOOP.

*&--代码添加 BY HANDYBY 04.07.2017 11:19:00  BEGIN

* 取内部订单号和订单描述
  DATA L_LOW TYPE ANLZ-ADATU .
  DATA L_HIGH TYPE ANLZ-BDATU .
*  RANGES R_ADATU FOR ANLZ-ADATU .
*  R_ADATU-SIGN = 'I'.
*  R_ADATU-OPTION = 'BT'.
  CONCATENATE S_GJAHR-LOW  S_PERAF-LOW+1(2) '01' INTO L_LOW .
  CONCATENATE S_GJAHR-LOW  S_PERAF-LOW+1(2) '31' INTO L_HIGH .
*  R_ADATU-LOW = L_LOW .
*  R_ADATU-HIGH = L_HIGH .
*  APPEND R_ADATU .

  DATA: BEGIN OF LS_ANLZ,
          BUKRS TYPE ANLZ-BUKRS,
          ANLN1 TYPE ANLZ-ANLN1,
          ANLN2 TYPE ANLZ-ANLN2,
          BDATU TYPE ANLZ-BDATU,
          ADATU TYPE ANLZ-ADATU,
          CAUFN TYPE ANLZ-CAUFN,
        END OF LS_ANLZ .
  DATA LT_ANLZ LIKE TABLE OF LS_ANLZ .
  DATA: BEGIN OF LS_AUFK,
          AUFNR TYPE AUFK-AUFNR,
          KTEXT TYPE AUFK-KTEXT,
        END OF LS_AUFK .
  DATA LT_AUFK LIKE TABLE OF LS_AUFK .

  SELECT BUKRS
         ANLN1
         ANLN2
         BDATU
         ADATU
         CAUFN
    INTO CORRESPONDING FIELDS OF TABLE LT_ANLZ
    FROM ANLZ
     FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = GT_DATA-BUKRS
     AND ANLN1 = GT_DATA-ANLN1
*&--代码添加 BY HANDYBY 26.07.2017 11:58:22  BEGIN
     AND ADATU LE L_LOW
     AND BDATU GE L_HIGH
     AND ANLN2 IN S_ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 11:58:22  END

*&--代码注释 BY HANDYBY 26.07.2017 11:59:02  BEGIN
*  SORT LT_ANLZ BY BUKRS ANLN1 .
*&--代码注释 BY HANDYBY 26.07.2017 11:59:02  END
*&--代码添加 BY HANDYBY 26.07.2017 12:00:40  BEGIN
  SORT LT_ANLZ BY BUKRS ANLN1 ANLN2 .
*&--代码添加 BY HANDYBY 26.07.2017 12:00:40  END

  IF LT_ANLZ IS NOT INITIAL .
    SELECT AUFNR
           KTEXT
      INTO CORRESPONDING FIELDS OF TABLE LT_AUFK
      FROM AUFK
       FOR ALL ENTRIES IN LT_ANLZ
     WHERE AUFNR = LT_ANLZ-CAUFN .
    SORT LT_AUFK BY AUFNR .
  ENDIF.

  LOOP AT GT_DATA INTO GS_DATA .
    READ TABLE LT_ANLZ WITH KEY BUKRS = GS_DATA-BUKRS
                                ANLN1 = GS_DATA-ANLN1
                                ANLN2 = GS_DATA-ANLN2
                                BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT LT_ANLZ INTO LS_ANLZ FROM SY-TABIX .
        IF LS_ANLZ-BUKRS = GS_DATA-BUKRS AND
            LS_ANLZ-ANLN1 = GS_DATA-ANLN1  AND
            LS_ANLZ-ANLN2 = GS_DATA-ANLN2 .
          GS_DATA-CAUFN = LS_ANLZ-CAUFN .
          EXIT.
        ELSE.
          EXIT .
        ENDIF.
        CLEAR LS_ANLZ .
      ENDLOOP.

      IF GS_DATA-CAUFN IS INITIAL .
        READ TABLE LT_ANLZ INTO LS_ANLZ WITH KEY BUKRS = GS_DATA-BUKRS
                                                 ANLN1 = GS_DATA-ANLN1
                                                 BINARY SEARCH .
        IF SY-SUBRC = 0 .
          GS_DATA-CAUFN = LS_ANLZ-CAUFN .
          CLEAR LS_ANLZ .
        ENDIF.
      ENDIF.

      IF GS_DATA-CAUFN IS NOT INITIAL .
        READ TABLE LT_AUFK INTO LS_AUFK WITH KEY AUFNR = GS_DATA-CAUFN BINARY SEARCH .
        IF SY-SUBRC = 0 .
          GS_DATA-KTEXT2 = LS_AUFK-KTEXT .
        ENDIF.
      ENDIF.

    ENDIF.
    MODIFY GT_DATA FROM GS_DATA .
    CLEAR GS_DATA .
    CLEAR LS_ANLZ .
    CLEAR LS_AUFK .
  ENDLOOP.
*&--代码添加 BY HANDYBY 04.07.2017 11:19:00  END

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
  GW_LAYOUT-BOX_FNAME    = 'ZBOX'.
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
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .

  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1'          '资产编码'             '' '' '' '' 'X' 'ANLA' 'ANLN1'.
*&--代码添加 BY HANDYBY 26.07.2017 13:49:51  BEGIN
  INIT_FIELDCAT 'ANLN2'          '次级资产编码'             '' '' '' '' 'X' 'ANLA' 'ANLN2'.
*&--代码添加 BY HANDYBY 26.07.2017 13:49:51  END
  INIT_FIELDCAT 'ANLKL'          '资产分类'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXK50'          '资产分类描述'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXT50'          '描述1'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXA50'          '描述2'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLHTXT'        '资产主号文本'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SERNR'          '序列号'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'          '成本中心'         '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 04.07.2017 11:57:45  BEGIN
  INIT_FIELDCAT 'CAUFN'         '内部订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT2'         '内部订单描述'         '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 04.07.2017 11:57:45  END
  INIT_FIELDCAT 'KTEXT'          '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INVZU'          '位置'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RAUMN'          '使用人'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ORD41'          '使用状态'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DEAKT'          '不活动日期'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ORD42'          '盘盈资产'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLUE'            '功率'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'          '供应商'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '供应商名称'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HERST'          '制造商'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LAND1'          '原产地国家'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LEGEB'          '租金'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AKTIV'          '资本化日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJGZZ'          '累计购置值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJZJ'           '累计折旧'         '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 26.07.2017 13:50:25  BEGIN
  INIT_FIELDCAT 'LJJHWZJ'           '累计计划外折旧'         '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 26.07.2017 13:50:25  END
  INIT_FIELDCAT 'ZMJZ'           '账面净值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NDJAR'           '折旧年限'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NDPER'           '折旧月份'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SYYF'            '剩余月份'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ001'         '每月折旧-1'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ002'         '每月折旧-2'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ003'         '每月折旧-3'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ004'         '每月折旧-4'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ005'         '每月折旧-5'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ006'         '每月折旧-6'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ007'         '每月折旧-7'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ008'         '每月折旧-8'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ009'         '每月折旧-9'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ010'         '每月折旧-10'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ011'         '每月折旧-11'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MYZJ012'         '每月折旧-12'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JZTZ'            '价值调整(新增)'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GZJZ'            '购置价值(新增)'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PTZJ'            '普通折旧(新增)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JHWZJ'           '计划外折旧(新增)'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZMJZ_NEW'           '账面净值(新增)'         '' '' '' '' '' '' ''.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
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
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
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
      IF RS_SELFIELD-FIELDNAME = 'ANLN1'
        AND GS_DATA-ANLN1 IS NOT INITIAL.
        SET PARAMETER ID 'AN1' FIELD GS_DATA-ANLN1.
*        SET PARAMETER ID 'AN2' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&ZCMXB'.
      "跳转到ZFI035 固定资产明细表
      "  GET PARAMETER ID ZCMX_BUKRS FIELD S_BUKRS-LOW .
      EXPORT  S_BUKRS-LOW  TO MEMORY ID  'ZCMX_BUKRS' ."S_BUKRS-LOW
      SUBMIT ZFI035 WITH P_BUKRS EQ S_BUKRS-LOW  AND RETURN .
*     CLEAR:R_ANLN1.
*     LOOP AT GT_DATA INTO  GS_DATA WHERE  ZBOX = 'X'.
*       R_ANLN1-SIGN = 'I'.
*       R_ANLN1-OPTION = 'EQ'.
*       R_ANLN1-LOW = GS_DATA-ANLN1.
*     ENDLOOP.
*       SUBMIT ZFI035 WITH P_BUKRS EQ S_BUKRS-LOW
*                     WITH S_BUDAT BETWEEN  P_BEG  AND P_END
*                     WITH S_ANLN1 IN R_ANLN1 AND RETURN .


  ENDCASE.
ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  PERFORM FRM_DATA_ENTER USING ER_DATA_CHANGED..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDFORM.                    "frm_data_enter
*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM TOP-OF-PAGE.

*ALV Header declarations
  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.
* Title
  WA_HEADER-TYP  = 'H'.
  WA_HEADER-INFO =  SY-TITLE."'装箱单维护打印平台'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* Date
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = '报表日期: '.
  CONCATENATE  S_GJAHR-LOW '年'
               S_PERAF-LOW+1(2) '月' INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "top-of-page
