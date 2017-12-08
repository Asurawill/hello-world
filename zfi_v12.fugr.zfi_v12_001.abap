FUNCTION ZFI_V12_001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(BUDAT_L) TYPE  DATS DEFAULT 00000000
*"     VALUE(BUDAT_H) TYPE  DATS DEFAULT 99991231
*"  TABLES
*"      T_VBELN STRUCTURE  ZVBELN OPTIONAL
*"      T_ITEM STRUCTURE  ZFI_V12_004 OPTIONAL
*"----------------------------------------------------------------------
  DATA:GT_CSKT TYPE TABLE OF CSKT WITH HEADER LINE.
  DATA GT_V12_005 TYPE TABLE OF ZFI_V12_005 WITH HEADER LINE.
  DATA:GT_MAKT TYPE TABLE OF MAKT WITH HEADER LINE."IT02
 " DATA:GT_BKPF TYPE TABLE OF BKPF WITH HEADER LINE .
  DATA: BEGIN OF T_VBKD OCCURS 1,
          VBELN TYPE VBKD-VBELN,
          BSTKD TYPE VBKD-BSTKD,
        END OF T_VBKD,
        T_TGSBT TYPE TABLE OF TGSBT WITH HEADER LINE.
  DATA: BEGIN OF T_BSEG OCCURS 1,
          BLART TYPE BKPF-BLART,
          BUDAT TYPE BKPF-BUDAT,
          WAERS TYPE BKPF-WAERS,
          BUKRS TYPE BSEG-BUKRS,
          BELNR TYPE BSEG-BELNR,
          GJAHR TYPE BSEG-GJAHR,
          BUZEI TYPE BSEG-BUZEI,
          SHKZG TYPE BSEG-SHKZG,
          GSBER TYPE BSEG-GSBER,
          DMBTR TYPE BSEG-DMBTR,
          VBEL2 TYPE BSEG-VBEL2,
          HKONT TYPE BSEG-HKONT,
          MATNR TYPE BSEG-MATNR,"物料号
          MAKTX TYPE MAKT-MAKTX,"物料描述 IT02
          MENGE TYPE BSEG-MENGE,"数量 IT02
          MEINS TYPE BSEG-MEINS,"单位 IT02
          MXDJ TYPE BSEG-DMBTR ,"单价 IT02
          XNEGP TYPE BSEG-XNEGP, "反记账
          KOSTL TYPE BSEG-KOSTL,"成本中心
*          GSBER TYPE BSEG-GSBER,"业务范围
          LTEXT TYPE CSKT-LTEXT,"成本中心描述
          SGTXT TYPE MSEG-SGTXT, "项目文本
        END OF T_BSEG.
  DATA: BEGIN OF T_HKONT_RBUSA OCCURS 1,
          LINE_ID TYPE ZFI_V12_001-HKONT,
          LTEXT TYPE ZFI_V12_001-LTEXT,
          HKONT TYPE ZFI_V12_002-HKONT2,
          RBUSA TYPE ZFI_V12_002-RBUSA,
        END OF T_HKONT_RBUSA.
  DATA: BEGIN OF T_HKONT_MATNR OCCURS 1,
          LINE_ID TYPE ZFI_V12_001-HKONT,
          LTEXT TYPE ZFI_V12_001-LTEXT,
          HKONT TYPE ZFI_V12_003-HKONT2,
          MATNR TYPE ZFI_V12_003-MATNR_FROM,
        END OF T_HKONT_MATNR.
  RANGES: R_BUKRS FOR T001-BUKRS.
  RANGES: R_HKONT_TZ FOR BSEG-HKONT."调整
  CLEAR: R_BUKRS[],R_HKONT_TZ[], T_HKONT_RBUSA[],T_BSEG[], T_HKONT_MATNR[].
  R_HKONT_TZ-SIGN = 'I'.
  R_HKONT_TZ-OPTION = 'EQ'.
  R_HKONT_TZ-LOW = '2221030503'.
  APPEND R_HKONT_TZ.
  R_HKONT_TZ-LOW = '1122010301'.
  APPEND R_HKONT_TZ.
  R_HKONT_TZ-LOW = '1122020201'.
  APPEND R_HKONT_TZ.
  R_HKONT_TZ-LOW = '2203010401'.
  APPEND R_HKONT_TZ.
  R_HKONT_TZ-LOW = '2203020201'.
  APPEND R_HKONT_TZ.
  R_HKONT_TZ-LOW = '6001019901'.
  APPEND R_HKONT_TZ.

  IF BUKRS NE ''.
    R_BUKRS-SIGN = 'I'.
    R_BUKRS-OPTION = 'EQ'.
    R_BUKRS-LOW = BUKRS.
    APPEND R_BUKRS.
    CLEAR R_BUKRS.
  ENDIF.
  CLEAR: T_ITEM[],GT_V12_005[].
  LOOP AT T_VBELN.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = T_VBELN-VBELN
      IMPORTING
        OUTPUT = T_VBELN-VBELN.
    TRANSLATE T_VBELN-VBELN  TO UPPER CASE.
    MODIFY T_VBELN.
  ENDLOOP.
  "00 从表ZFI_BSEG_VBELN，根据销售订单号获取要处理的会计凭证
  DATA GT_BSEG_VBELN TYPE TABLE OF ZFI_BSEG_VBELN WITH HEADER LINE.
  DATA:
    BEGIN OF LT_BSEG_VBELN OCCURS 0,
      BUKRS TYPE BSEG-BUKRS,
      BELNR TYPE BSEG-BELNR,
      GJAHR TYPE BSEG-GJAHR,
      BUZEI TYPE BSEG-BUZEI,
      VBEL2 TYPE BSEG-VBEL2,
    END OF LT_BSEG_VBELN.

  CLEAR: GT_BSEG_VBELN[], LT_BSEG_VBELN[].
  IF T_VBELN[] IS NOT INITIAL.
    SELECT a~bukrs a~belnr b~gjahr b~buzei b~vbel2
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_VBELN
     FROM  bkpf as a
     inner join ZFI_BSEG_VBELN as b on
     a~bukrs = b~bukrs and a~belnr = b~belnr and a~gjahr = b~gjahr
     FOR ALL ENTRIES IN T_VBELN
     WHERE  a~BUKRS IN R_BUKRS
     and a~budat  BETWEEN BUDAT_L AND BUDAT_H
     AND b~VBEL2 EQ T_VBELN-VBELN .

  ELSE.
  SELECT a~bukrs a~belnr b~gjahr b~buzei b~vbel2
     INTO CORRESPONDING FIELDS OF TABLE GT_BSEG_VBELN
     FROM  bkpf as a
     inner join ZFI_BSEG_VBELN as b on
     a~bukrs = b~bukrs and a~belnr = b~belnr and a~gjahr = b~gjahr
     WHERE  a~BUKRS IN R_BUKRS
     and a~budat  BETWEEN BUDAT_L AND BUDAT_H
    .
  ENDIF.


     sort GT_BSEG_VBELN by bukrs gjahr belnr .
  LOOP AT GT_BSEG_VBELN.
    MOVE-CORRESPONDING GT_BSEG_VBELN TO LT_BSEG_VBELN.
    APPEND LT_BSEG_VBELN.
  ENDLOOP.
  SORT LT_BSEG_VBELN BY BUKRS BELNR GJAHR BUZEI.
  "01 BSEG,BKPF连接,获取会计凭证抬头、行项目字段；
   IF LT_BSEG_VBELN[] IS NOT INITIAL.
    SELECT
      A~BLART
      A~BUDAT
      A~WAERS
      B~BUKRS
      B~BELNR
      B~GJAHR
      B~BUZEI
      B~SHKZG
      B~GSBER
      B~DMBTR
*      C~VBEL2
      B~HKONT
      B~MATNR
      B~MENGE  "数量
      B~MEINS "单位
      B~KOSTL
      B~GSBER
      B~KOSTL
      B~SGTXT
      FROM BKPF AS A INNER JOIN BSEG AS B
      ON A~BUKRS EQ B~BUKRS
      AND A~BELNR EQ B~BELNR
      AND A~GJAHR EQ B~GJAHR
      INTO CORRESPONDING FIELDS OF TABLE T_BSEG
      FOR ALL ENTRIES IN LT_BSEG_VBELN
*      WHERE B~VBEL2 EQ T_VBELN-VBELN
      WHERE A~BUDAT BETWEEN BUDAT_L AND BUDAT_H
      AND  B~BUKRS EQ LT_BSEG_VBELN-BUKRS
      AND B~BELNR EQ LT_BSEG_VBELN-BELNR
      AND B~GJAHR EQ LT_BSEG_VBELN-GJAHR
      AND B~BUZEI EQ LT_BSEG_VBELN-BUZEI
      AND B~HKONT NOT IN R_HKONT_TZ
      AND A~BLART IN ('SA','QC').
    SELECT
      A~BLART
      A~BUDAT
      A~WAERS
      B~BUKRS
      B~BELNR
      B~GJAHR
      B~BUZEI
      B~SHKZG
      B~GSBER
      B~DMBTR
*      C~VBEL2
      B~HKONT
      B~MATNR
      B~MENGE  "数量 IT02
      B~MEINS "单位  IT02
      B~KOSTL
      B~GSBER
      B~SGTXT
      FROM BKPF AS A INNER JOIN BSEG AS B
      ON A~BUKRS EQ B~BUKRS
      AND A~BELNR EQ B~BELNR
      AND A~GJAHR EQ B~GJAHR
      APPENDING CORRESPONDING FIELDS OF TABLE T_BSEG
      FOR ALL ENTRIES IN LT_BSEG_VBELN
*      WHERE B~VBEL2 EQ T_VBELN-VBELN
      WHERE  A~BUDAT BETWEEN BUDAT_L AND BUDAT_H
      AND B~BUKRS EQ LT_BSEG_VBELN-BUKRS
      AND B~BELNR EQ LT_BSEG_VBELN-BELNR
      AND B~GJAHR EQ LT_BSEG_VBELN-GJAHR
      AND B~HKONT NOT IN R_HKONT_TZ
      AND A~BLART NOT IN ('SA','QC').
   LOOP AT T_BSEG.
     READ TABLE LT_BSEG_VBELN WITH KEY BUKRS = T_BSEG-BUKRS BELNR = T_BSEG-BELNR GJAHR = T_BSEG-GJAHR BUZEI = T_BSEG-BUZEI BINARY SEARCH.
     IF SY-SUBRC EQ 0.
       T_BSEG-VBEL2 = LT_BSEG_VBELN-VBEL2.
     ENDIF.
     MODIFY T_BSEG.
   ENDLOOP.
   ENDIF.

  "02 T_BSEG连接VBKD；
  IF T_BSEG[] IS NOT INITIAL.
    "IT02 ADD MAKTX BEGIN
    SELECT MATNR MAKTX
    INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FROM MAKT
    FOR ALL  ENTRIES IN T_BSEG
    WHERE MATNR = T_BSEG-MATNR
      AND SPRAS = SY-LANGU.

    "IT02 ADD END
    SELECT
      VBELN
      BSTKD
      FROM VBKD
      INTO CORRESPONDING FIELDS OF TABLE T_VBKD
      FOR ALL ENTRIES IN T_BSEG
      WHERE VBELN EQ T_BSEG-VBEL2
      AND BSTKD NE SPACE.
    SORT T_VBKD BY VBELN.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CSKT
      FROM  CSKT
      FOR ALL ENTRIES IN T_BSEG
      WHERE KOSTL = T_BSEG-KOSTL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE T_TGSBT
      FROM TGSBT
      FOR ALL ENTRIES IN T_BSEG
      WHERE GSBER = T_BSEG-GSBER
      AND SPRAS = SY-LANGU.
    SORT T_TGSBT BY GSBER.
  ENDIF.
  "03 ZFI_V12_001,ZFI_V12_002连接科目，获取项目和科目、业务范围的对应关系;连接T_VBKD，获取会计明细。
  SELECT
    A~HKONT AS LINE_ID
    A~LTEXT
    B~HKONT2 AS HKONT
    B~RBUSA
    FROM ZFI_V12_001 AS A
    INNER JOIN ZFI_V12_002 AS B ON A~HKONT EQ B~HKONT
    INTO CORRESPONDING FIELDS OF TABLE T_HKONT_RBUSA
    WHERE A~FORM EQ SPACE.
  "04 ZFI_V12_003连接MARA,获取物料组对应的物料；连接ZFI_V12_001,获取项目和物料的对应关系；连接ZANV_ZFI_V12，获取会计明细。
  SELECT
    A~HKONT AS LINE_ID
    A~LTEXT
    B~HKONT2 AS HKONT
    B~MATNR_FROM AS MATNR
    FROM ZFI_V12_001 AS A
    INNER JOIN ZFI_V12_003 AS B ON A~HKONT EQ B~HKONT
    INTO CORRESPONDING FIELDS OF TABLE T_HKONT_MATNR
    WHERE A~FORM EQ 'A'.
  LOOP AT T_BSEG.
    LOOP AT T_HKONT_MATNR.
      IF T_BSEG-HKONT CP T_HKONT_MATNR-HKONT
        AND T_BSEG-MATNR CP T_HKONT_MATNR-MATNR.
        T_ITEM-KM_NAME = T_HKONT_MATNR-LTEXT.
        EXIT.
      ENDIF.
    ENDLOOP.
    LOOP AT T_HKONT_RBUSA.
      IF T_BSEG-HKONT CP T_HKONT_RBUSA-HKONT
        AND T_BSEG-GSBER CP T_HKONT_RBUSA-RBUSA.
        T_ITEM-KM_NAME = T_HKONT_RBUSA-LTEXT.
        EXIT.
      ENDIF.
    ENDLOOP.
    READ TABLE GT_CSKT WITH KEY KOSTL = T_BSEG-KOSTL.
    IF SY-SUBRC = 0.
      T_BSEG-LTEXT = GT_CSKT-LTEXT.
      ENDIF.
    IF T_ITEM-KM_NAME IS NOT INITIAL.
      MOVE-CORRESPONDING T_BSEG TO T_ITEM.
      READ TABLE GT_MAKT WITH KEY MATNR = T_ITEM-MATNR.
      IF SY-SUBRC = 0.
        T_ITEM-MAKTX = GT_MAKT-MAKTX. "IT02 ADD 150612
       ENDIF.
      T_ITEM-VBELN = T_BSEG-VBEL2.
      T_ITEM-KM = T_BSEG-HKONT.
      READ TABLE T_VBKD WITH KEY VBELN = T_BSEG-VBEL2 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        T_ITEM-BSTKD = T_VBKD-BSTKD.
      ENDIF.
      READ TABLE T_TGSBT WITH KEY GSBER = T_BSEG-GSBER BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        T_ITEM-GTEXT = T_TGSBT-GTEXT.
      ENDIF.
      IF T_BSEG-SHKZG EQ 'S'.
        T_ITEM-DMBTR_S = T_BSEG-DMBTR.
      ELSE.
        T_ITEM-DMBTR_H = T_BSEG-DMBTR.
      ENDIF.
      "IT02 ADD BEGIN 150612 单价计算
      IF T_ITEM-MENGE <> 0 .
         IF T_ITEM-DMBTR_S <> 0 .
           T_ITEM-MXDJ = abs( T_ITEM-DMBTR_S ) / T_ITEM-MENGE.
         ENDIF.
          IF T_ITEM-DMBTR_H <> 0 .
           T_ITEM-MXDJ = abs( T_ITEM-DMBTR_H ) / T_ITEM-MENGE.
         ENDIF.
       ENDIF.
       "IT02 add end
      APPEND T_ITEM.
    ENDIF.
    CLEAR T_ITEM.
  ENDLOOP.

  SORT T_ITEM BY VBELN KM BUKRS BELNR GJAHR BUZEI.

ENDFUNCTION.