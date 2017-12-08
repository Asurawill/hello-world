*&---------------------------------------------------------------------*
*& Report  ZMM014
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/04/22
*& Request       :
*& Descriptions  :  公司间采购订单交货明细表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
** 修改日期   开发人员  请求号        描述
" 20170518   IT02     ED1K905522   追加交货已完成 、最后交货
" 20170518   IT02     ED1K905522   追加交货已完成 、最后交货
*20170519    IT02     ED1K905530   追加未交货完成的未发数量的处理逻辑
*&---------------------------------------------------------------------*
REPORT ZMM014.


TABLES : EKKN,VBAK,EKKO,EKPO,LFA1,LIKP.
TYPE-POOLS : SLIS.

TYPES:BEGIN OF TY_DATA,
        EBELN     TYPE EKPO-EBELN,      "采购订单号
        BSART     TYPE EKKO-BSART,      "公司间采购订单类型
        BATXT     TYPE T161T-BATXT,     "采购订单类型描述
        EKORG     TYPE EKKO-EKORG,      "采购组织
        WERKS     TYPE LFA1-WERKS,      "供应工厂
        MATNR     TYPE EKPO-MATNR,      "物料编号
        TXZ01     TYPE EKPO-TXZ01,      "物料描述
        MENGE     TYPE EKPO-MENGE,      "采购订单数量
        MEINS     TYPE EKPO-MEINS,      "单位
        MATKL     TYPE EKPO-MATKL,      "物料组
        VBELN     TYPE EKKN-VBELN,      "销售订单
        VBELP     TYPE EKKN-VBELP,      "销售订单行项目
        AUART     TYPE VBAK-AUART,      "销售订单类
        BEZEI     TYPE TVAKT-BEZEI,     "销售订单类型描述
        ERDAT     TYPE VBAK-ERDAT,      "凭证日期
        ZSD0302   TYPE VBAK-ZSD0302,    "销售立项号
        XMMC(200) TYPE C,               "项目名称
        VDATU     TYPE VBAK-VDATU,      "回复发货日期
        VDATU_1   TYPE VBAK-VDATU,      "回复发货日期
        NAME1     TYPE KNA1-NAME1,      "经办人
        MATNR_X   TYPE VBAP-MATNR,      "屏体虚拟物料
        ARKTX     TYPE VBAP-ARKTX,      "屏体物料描述
        PSTYV     TYPE VBAP-PSTYV,      "项目类型
        VTEXT     TYPE TVAPT-VTEXT,     "项目类别描述
        CPLX      TYPE AUSP-ATWRT,      "产品类型
        GGXH      TYPE AUSP-ATWRT,      "规格型号
        BELNR     TYPE EKBE-BELNR,      "交货单编号
        MENGE_1   TYPE EKBE-MENGE,      "交货单数量
        BUZEI     TYPE EKBE-BUZEI,      "交货单行项目
        WADAT_IST TYPE LIKP-WADAT_IST,  "实际交货日期
        ZMJ       TYPE EKPO-MENGE,      "总面积
        YFHMJ     TYPE EKPO-MENGE,      "已发货面积
        WFHMJ     TYPE EKPO-MENGE,      "未发货面积
        MEABM     TYPE CHAR4,           "面积单位
        LIFNR     TYPE EKKO-LIFNR,      "供应商
        EBELP     TYPE EKPO-EBELP,      "采购订单行项目
        UEPOS     TYPE VBAP-UEPOS,      "物料单结构中的上层项目
        WFSL      TYPE EKBE-MENGE,      "未发数量
        RETPO     TYPE EKPO-RETPO,           "退货行      "add by it02 20161122
        ELIKZ     TYPE EKPO-ELIKZ,      "交货已完成       "add by it02 20170518
        EGLKZ     TYPE EKPO-EGLKZ,     "最后交货
      END OF TY_DATA.

TYPES:BEGIN OF TY_EKPO,
        EBELN TYPE EKPO-EBELN,          "采购订单号
        EBELP TYPE EKPO-EBELP,          "采购订单行项目
        BSART TYPE EKKO-BSART,          "公司间采购订单类型
        EKORG TYPE EKKO-EKORG,          "采购组织
        MATNR TYPE EKPO-MATNR,          "物料编号
        TXZ01 TYPE EKPO-TXZ01,          "物料描述
        MENGE TYPE EKPO-MENGE,          "采购订单数量
        MEINS TYPE EKPO-MEINS,          "单位
        MATKL TYPE EKPO-MATKL,          "物料组
        LIFNR TYPE EKKO-LIFNR,          "供应工厂
        RETPO TYPE EKPO-RETPO,          "退货行
*        VBELN TYPE EKKN-VBELN,          "销售订单
*        VBELP TYPE EKKN-VBELP,          "销售订单行项目
        ELIKZ TYPE EKPO-ELIKZ,      "交货已完成       "add by it02 20170518
        EGLKZ TYPE EKPO-EGLKZ,     "最后交货
      END OF TY_EKPO.

TYPES:BEGIN OF TY_VBAP,
        AUART   TYPE VBAK-AUART,        "销售订单类
        ERDAT   TYPE VBAK-ERDAT,        "凭证日期
        ZSD0302 TYPE VBAK-ZSD0302,       "销售立项号
        VDATU   TYPE VBAK-VDATU,        "回复发货日期
        VBELN   TYPE VBAP-VBELN,        "销售订单号
        POSNR   TYPE VBAP-POSNR,        "销售订单号
        UEPOS   TYPE VBAP-UEPOS,        "屏体虚拟物料
        ARKTX   TYPE VBAP-ARKTX,        "屏体物料描述
        PSTYV   TYPE VBAP-PSTYV,        "项目类型
      END OF TY_VBAP.

TYPES:BEGIN OF TY_KNA1,
        VBELN TYPE VBPA-VBELN,
        KUNNR TYPE KNA1-KUNNR,
        NAME1 TYPE KNA1-NAME1,
        POSNR TYPE VBPA-POSNR,
        PARVW TYPE VBPA-PARVW,
        SPRAS TYPE KNA1-SPRAS,
      END OF TY_KNA1.

**INTERNAL TABLE DECLARTION
DATA  GT_DATA TYPE TABLE OF TY_DATA.
DATA  GS_DATA TYPE TY_DATA.

DATA  GT_EKPO TYPE TABLE OF TY_EKPO.
DATA  GS_EKPO TYPE TY_EKPO.

DATA GT_T161T TYPE TABLE OF T161T.
DATA GS_T161T TYPE T161T.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.

DATA GT_EKKN TYPE TABLE OF EKKN .
DATA GS_EKKN TYPE EKKN .

DATA GT_VBAP TYPE TABLE OF TY_VBAP.
DATA GS_VBAP TYPE TY_VBAP.

DATA GT_TVAKT TYPE TABLE OF TVAKT.
DATA GS_TVAKT TYPE TVAKT.

DATA GT_KNA1 TYPE TABLE OF TY_KNA1.
DATA GS_KNA1 TYPE TY_KNA1.

DATA GT_VBAP_1 TYPE TABLE OF VBAP.
DATA GS_VBAP_1 TYPE VBAP.

DATA GT_TVAPT TYPE TABLE OF TVAPT.
DATA GS_TVAPT TYPE TVAPT.

DATA GT_EKBE TYPE TABLE OF EKBE.
DATA GS_EKBE TYPE EKBE.

DATA GT_LIKP TYPE TABLE OF LIKP.
DATA GS_LIKP TYPE LIKP.

DATA GT_MARA TYPE TABLE OF MARA.
DATA GS_MARA TYPE MARA.

DATA:L_OBJECT LIKE BAPI1003_KEY-OBJECT.
*分类视图
DATA:
  IT_NUM  LIKE BAPI1003_ALLOC_VALUES_NUM  OCCURS 0 WITH HEADER LINE,
  IT_CHAR LIKE BAPI1003_ALLOC_VALUES_CHAR OCCURS 0 WITH HEADER LINE,
  IT_CURR LIKE BAPI1003_ALLOC_VALUES_CURR OCCURS 0 WITH HEADER LINE,
  IT_RET  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

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

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002 .

SELECT-OPTIONS: S_VBELN       FOR VBAK-VBELN   MATCHCODE OBJECT VMVA ,
                S_AUART       FOR VBAK-AUART,
                S_VKORG       FOR VBAK-VKORG,
                S_ERDAT       FOR VBAK-ERDAT,
                S_EBELN       FOR EKPO-EBELN MATCHCODE OBJECT MEKK,
                S_BSART       FOR EKKO-BSART,
                S_EKORG       FOR EKKO-EKORG,
                S_AEDAT       FOR EKKO-AEDAT,
                S_WERKS       FOR LFA1-WERKS,
                S_WADAT       FOR LIKP-WADAT_IST,
                S_MATKL       FOR EKPO-MATKL,
                S_MATNR       FOR EKPO-MATNR .


*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
*  s_loekz-sign = 'I'.
*  s_loekz-option = 'NE'.
*  s_loekz-low = 'L'.
*  APPEND s_loekz.
*  s_frgke-sign = 'I'.
*  s_frgke-option = 'NE'.
*  s_frgke-low = 'R'.
*  APPEND s_frgke.

  GX_VARIANT-REPORT = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    G_VARIANT = GX_VARIANT-VARIANT.
  ENDIF.

AT SELECTION-SCREEN.
  PERFORM FRM_AUTH_CHECK.

**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.

FORM FRM_AUTH_CHECK.

*检查工厂
*  DATA LT_T001W TYPE T001W OCCURS 0 WITH HEADER LINE.
*  SELECT WERKS
*    FROM T001W
*    INTO CORRESPONDING FIELDS OF TABLE LT_T001W
*    WHERE WERKS IN S_WERKS.
*  LOOP AT LT_T001W WHERE WERKS IN S_WERKS.
*    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
**            ID 'ACTVT' FIELD '__________'
*             ID 'WERKS' FIELD LT_T001W-WERKS.
*    IF SY-SUBRC <> 0.
*      MESSAGE E603(FCO) WITH LT_T001W-WERKS.
*    ENDIF.
*  ENDLOOP.

**检查销售组织
*  DATA:LT_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
*  SELECT VKORG FROM TVKO
*    INTO CORRESPONDING FIELDS OF TABLE LT_TVKO
*    WHERE VKORG IN S_VKORG
*    .
*  LOOP AT LT_TVKO WHERE VKORG IN S_VKORG.
*    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
*             ID 'VKORG' FIELD LT_TVKO-VKORG
*             .
*    IF SY-SUBRC <> 0.
*      MESSAGE E430(VELO) WITH LT_TVKO-VKORG.
*    ENDIF.
*  ENDLOOP.

*检查采购组织
*  DATA:LT_T024E TYPE T024E OCCURS 0 WITH HEADER LINE .
*  SELECT EKORG FROM T024E
*    INTO CORRESPONDING FIELDS OF TABLE LT_T024E
*    WHERE EKORG IN S_EKORG.
*
*  LOOP AT LT_T024E WHERE EKORG IN S_EKORG.
*    AUTHORITY-CHECK OBJECT 'M_LFM1_EKO'
*    ID 'EKORG' FIELD LT_T024E-EKORG.
*    IF SY-SUBRC <> 0.
*      MESSAGE E018(ZMM01) WITH LT_T024E-EKORG.
*    ENDIF.
*
*  ENDLOOP.
*检查采购组织
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

*查询采购订单信息
  SELECT * FROM EKPO
   INNER JOIN EKPV
  ON  EKPO~EBELN = EKPV~EBELN
  AND EKPO~EBELP = EKPV~EBELP
   INNER JOIN EKKO
  ON EKPO~EBELN = EKKO~EBELN
*   LEFT JOIN EKKN
*  ON  EKPO~EBELN = EKKN~EBELN
*  AND EKPO~EBELP = EKKN~EBELP
  INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
  WHERE EKPO~LOEKZ <> 'L'
  AND   EKPO~EBELN  IN S_EBELN
  AND   MATKL       IN S_MATKL
  AND   MATNR       IN S_MATNR
  AND   BSART       IN S_BSART
  AND   EKORG       IN S_EKORG
  AND   EKPO~AEDAT  IN S_AEDAT.
*  AND   EKKN~VBELN  IN S_VBELN.

  CHECK GT_EKPO IS NOT INITIAL.

*查询科目页签
  SELECT * FROM EKKN
   INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
   FOR ALL ENTRIES IN GT_EKPO
   WHERE EBELN = GT_EKPO-EBELN
   AND   EBELP = GT_EKPO-EBELP
   AND   VBELN IN S_VBELN.

  SORT GT_EKPO BY EBELN EBELP.

*查询采购订单描述
  SELECT * FROM T161T
   INTO CORRESPONDING FIELDS OF TABLE GT_T161T
   FOR ALL ENTRIES IN GT_EKPO
   WHERE BSART = GT_EKPO-BSART.

*供应工厂
  SELECT * FROM LFA1
   INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
   FOR ALL ENTRIES IN GT_EKPO
   WHERE LIFNR  = GT_EKPO-LIFNR
   AND WERKS   IN S_WERKS.



*查询销售订单抬头及行项目
  IF GT_EKKN IS NOT INITIAL.
    SELECT * FROM VBAK
     INNER JOIN VBAP
    ON VBAK~VBELN = VBAP~VBELN
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    FOR ALL ENTRIES IN GT_EKKN
    WHERE VBAP~VBELN = GT_EKKN-VBELN
    AND   VBAP~POSNR = GT_EKKN-VBELP
    .
  ENDIF.

*销售订单类型描述
  IF GT_VBAP IS NOT INITIAL.
    SELECT * FROM TVAKT
      INTO CORRESPONDING FIELDS OF TABLE GT_TVAKT
     FOR ALL ENTRIES IN GT_VBAP
     WHERE AUART = GT_VBAP-AUART.
  ENDIF.

*经办人
  SELECT * FROM KNA1
   INNER JOIN VBPA
   ON KNA1~KUNNR = VBPA~KUNNR
   INTO CORRESPONDING FIELDS OF TABLE GT_KNA1
   FOR ALL ENTRIES IN GT_VBAP
   WHERE VBELN = GT_VBAP-VBELN
   AND   POSNR = ''
   AND   PARVW = 'Z3'.

*屏体虚拟物料号
  SELECT * FROM VBAP
   INTO CORRESPONDING FIELDS OF TABLE GT_VBAP_1
   FOR ALL ENTRIES IN GT_VBAP
   WHERE VBELN = GT_VBAP-VBELN
   AND   POSNR = GT_VBAP-UEPOS.

*项目类别描述
  SELECT * FROM TVAPT
    INTO CORRESPONDING FIELDS OF TABLE GT_TVAPT
    FOR ALL ENTRIES IN GT_VBAP
    WHERE PSTYV = GT_VBAP-PSTYV.

**查询特性 （产品型号，规格型号）
*  SELECT * FROM AUSP
*  INTO CORRESPONDING FIELDS OF TABLE GT_AUSP
*   FOR ALL ENTRIES IN GT_EKPO
*   WHERE OBJEK = GT_EKPO-MATNR
*   AND  ( ATINN = CPLX
*   OR   ATINN = GGXH ).

*查询交货单号
  SELECT * FROM EKBE
   INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
   FOR ALL ENTRIES IN GT_EKPO
   WHERE EBELN = GT_EKPO-EBELN
   AND   EBELP = GT_EKPO-EBELP
   AND   BEWTP = 'L'.

*查询时间发货日期
  IF GT_EKBE IS NOT INITIAL .
    SELECT * FROM LIKP
      INTO CORRESPONDING FIELDS OF TABLE GT_LIKP
      FOR ALL ENTRIES IN GT_EKBE
      WHERE VBELN = GT_EKBE-BELNR
      AND   WADAT_IST IN S_WADAT.
  ENDIF.

*查询物料面积
  SELECT * FROM MARA
    INTO CORRESPONDING FIELDS OF TABLE GT_MARA
    FOR ALL ENTRIES IN GT_EKPO
    WHERE MATNR = GT_EKPO-MATNR.

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

  DATA L_MENGE TYPE EKPO-MENGE."汇总销售订单数量

*&--代码添加 BY HANDYBY 06.06.2017 11:41:10  BEGIN
  DATA: BEGIN OF LS_OBJEK,
          OBJEK LIKE AUSP-OBJEK,
          ATINN LIKE AUSP-ATINN,
          ATNAM LIKE CABN-ATNAM,
          ATWRT LIKE AUSP-ATWRT,
        END OF LS_OBJEK.
  DATA LT_OBJEK LIKE TABLE OF LS_OBJEK .
  DATA: BEGIN OF LS_MATNR,
          OBJEK LIKE AUSP-OBJEK,
        END OF LS_MATNR.
  DATA LT_MATNR LIKE TABLE OF LS_MATNR.
  DATA: BEGIN OF LS_ATNAM,
          ATNAM LIKE CABN-ATNAM,
          ATWRT LIKE AUSP-ATWRT,
        END OF LS_ATNAM.
  DATA LT_ATNAM LIKE TABLE OF LS_ATNAM.

  IF GT_EKPO IS NOT INITIAL .
    LOOP AT GT_EKPO INTO GS_EKPO .
      LS_MATNR-OBJEK = GS_EKPO-MATNR .
      APPEND LS_MATNR TO LT_MATNR .
      CLEAR LS_MATNR.
      CLEAR GS_EKPO .
    ENDLOOP.
    SORT LT_MATNR BY OBJEK .
    DELETE ADJACENT DUPLICATES FROM LT_MATNR COMPARING OBJEK .
    SELECT A~OBJEK
           A~ATINN
           C~ATNAM
           A~ATWRT
    INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP AS A
   INNER JOIN CABN AS C
      ON A~ATINN = C~ATINN
     FOR ALL ENTRIES IN LT_MATNR
   WHERE A~OBJEK = LT_MATNR-OBJEK.
    SORT LT_OBJEK BY OBJEK.
  ENDIF.
*&--代码添加 BY HANDYBY 06.06.2017 11:41:10  END

*&--代码添加 BY HANDYBY 06.06.2017 12:56:16  BEGIN
  SORT GT_EKKN BY EBELN EBELP .
  SORT GT_LFA1 BY LIFNR .
  SORT GT_VBAP BY VBELN .
  SORT GT_KNA1 BY VBELN POSNR PARVW SPRAS .
  SORT GT_VBAP_1 BY VBELN POSNR .
  SORT GT_EKBE BY EBELN EBELP BEWTP .
  SORT GT_LIKP BY VBELN .
  SORT GT_MARA BY MATNR .
*&--代码添加 BY HANDYBY 06.06.2017 12:56:16  END

  LOOP AT GT_EKPO INTO GS_EKPO.

    MOVE-CORRESPONDING GS_EKPO TO GS_DATA.

*销售订单号
*&--代码添加 BY HANDYBY 06.06.2017 11:13:13  BEGIN

    READ TABLE GT_EKKN INTO GS_EKKN
    WITH KEY EBELN = GS_DATA-EBELN
             EBELP = GS_DATA-EBELP BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:13:13  END
*&--代码注释 BY HANDYBY 06.06.2017 11:16:11  BEGIN
*    READ TABLE gt_ekkn INTO gs_ekkn
*    WITH KEY ebeln = gs_data-ebeln
*             ebelp = gs_data-ebelp  .
*&--代码注释 BY HANDYBY 06.06.2017 11:16:11  END

    IF SY-SUBRC = 0.
      GS_DATA-VBELN = GS_EKKN-VBELN.
      GS_DATA-VBELP = GS_EKKN-VBELP.
    ELSE.
      IF S_VBELN IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*采购类型描述
    READ TABLE GT_T161T INTO GS_T161T
    WITH KEY BSART = GS_DATA-BSART
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-BATXT = GS_T161T-BATXT.
    ENDIF.

*供应工厂
*&--代码添加 BY HANDYBY 06.06.2017 11:14:02  BEGIN

    READ TABLE GT_LFA1 INTO GS_LFA1
    WITH KEY LIFNR = GS_DATA-LIFNR BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:14:02  END
*&--代码注释 BY HANDYBY 06.06.2017 11:16:37  BEGIN
*    READ TABLE GT_LFA1 INTO GS_LFA1
*    WITH KEY LIFNR = GS_DATA-LIFNR .
*&--代码注释 BY HANDYBY 06.06.2017 11:16:37  END

    IF SY-SUBRC = 0.
      GS_DATA-WERKS = GS_LFA1-WERKS.
    ELSEIF SY-SUBRC <> 0 AND S_WERKS IS NOT INITIAL.
      CONTINUE.
    ENDIF.

*销售订单抬头数据
*&--代码添加 BY HANDYBY 06.06.2017 11:14:17  BEGIN

    READ TABLE GT_VBAP INTO GS_VBAP
    WITH KEY VBELN = GS_DATA-VBELN BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:14:17  END
*&--代码注释 BY HANDYBY 06.06.2017 11:16:56  BEGIN
*    READ TABLE GT_VBAP INTO GS_VBAP
*    WITH KEY VBELN = GS_DATA-VBELN .
*&--代码注释 BY HANDYBY 06.06.2017 11:16:56  END

    IF SY-SUBRC = 0.
      GS_DATA-AUART   = GS_VBAP-AUART.
      GS_DATA-ERDAT   = GS_VBAP-ERDAT.
      GS_DATA-ZSD0302 = GS_VBAP-ZSD0302.
      GS_DATA-VDATU   = GS_VBAP-VDATU - 2.
      GS_DATA-VDATU_1 = GS_VBAP-VDATU.
*      GS_DATA-PSTYV   = GS_VBAP-PSTYV.
*      GS_DATA-UEPOS   = GS_VBAP-UEPOS.
      IF S_AUART IS NOT INITIAL.
        IF GS_DATA-AUART NOT IN S_AUART.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    "add by  it02 20161107 begin
*&--代码添加 BY HANDYBY 06.06.2017 11:14:31  BEGIN
    READ TABLE GT_VBAP INTO GS_VBAP
     WITH KEY VBELN = GS_DATA-VBELN
              POSNR = GS_DATA-VBELP BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:14:31  END
*&--代码注释 BY HANDYBY 06.06.2017 11:17:22  BEGIN
*      READ TABLE GT_VBAP INTO GS_VBAP
*     WITH KEY VBELN = GS_DATA-VBELN
*              POSNR = GS_DATA-VBELP .
*&--代码注释 BY HANDYBY 06.06.2017 11:17:22  END

    IF SY-SUBRC EQ 0.
      GS_DATA-PSTYV   = GS_VBAP-PSTYV.
      GS_DATA-UEPOS   = GS_VBAP-UEPOS.

    ENDIF.
    "add by  it02 20161107 end.


*销售订单类型描述
    READ TABLE GT_TVAKT INTO GS_TVAKT
    WITH KEY AUART = GS_DATA-AUART
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-BEZEI = GS_TVAKT-BEZEI.
    ENDIF.

*项目名称
    PERFORM FRM_READ_TEXT USING GS_DATA-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING GS_DATA-XMMC.

*经办人
*&--代码添加 BY HANDYBY 06.06.2017 11:14:54  BEGIN

    READ TABLE GT_KNA1 INTO GS_KNA1
    WITH KEY  VBELN = GS_DATA-VBELN
              POSNR = ''
              PARVW = 'Z3'
              SPRAS = SY-LANGU BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:14:54  END
*&--代码注释 BY HANDYBY 06.06.2017 11:17:40  BEGIN
*    READ TABLE GT_KNA1 INTO GS_KNA1
*    WITH KEY  VBELN = GS_DATA-VBELN
*              POSNR = ''
*              PARVW = 'Z3'
*              SPRAS = SY-LANGU .
*&--代码注释 BY HANDYBY 06.06.2017 11:17:40  END

    IF SY-SUBRC = 0.
      GS_DATA-NAME1 = GS_KNA1-NAME1.
    ENDIF.

*项目类别描述
    READ TABLE GT_TVAPT INTO GS_TVAPT
    WITH KEY PSTYV = GS_DATA-PSTYV
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-VTEXT = GS_TVAPT-VTEXT.
    ENDIF.

*对应的虚拟件
*&--代码添加 BY HANDYBY 06.06.2017 11:18:04  BEGIN

    READ TABLE GT_VBAP_1 INTO GS_VBAP_1
    WITH KEY VBELN = GS_DATA-VBELN
             POSNR = GS_DATA-UEPOS BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:18:04  END
*&--代码注释 BY HANDYBY 06.06.2017 11:18:23  BEGIN
*     READ TABLE GT_VBAP_1 INTO GS_VBAP_1
*    WITH KEY VBELN = GS_DATA-VBELN
*             POSNR = GS_DATA-UEPOS.
*&--代码注释 BY HANDYBY 06.06.2017 11:18:23  END

    IF SY-SUBRC = 0.
      GS_DATA-MATNR_X = GS_VBAP_1-MATNR.
      GS_DATA-ARKTX   = GS_VBAP_1-ARKTX.
    ENDIF.

    L_OBJECT = GS_DATA-MATNR.

    REFRESH IT_CHAR[].
    CLEAR IT_CHAR.

*获取特性
*&--代码添加 BY HANDYBY 06.06.2017 11:40:23  BEGIN
*  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*      EXPORTING
*        OBJECTKEY       = L_OBJECT
*        OBJECTTABLE     = 'MARA'
*        CLASSNUM        = 'LYD001'
*        CLASSTYPE       = '001'
*      TABLES
*        ALLOCVALUESNUM  = IT_NUM
*        ALLOCVALUESCHAR = IT_CHAR
*        ALLOCVALUESCURR = IT_CURR
*        RETURN          = IT_RET.
*&--代码添加 BY HANDYBY 06.06.2017 11:40:23  END
*&--代码添加 BY HANDYBY 06.06.2017 11:44:05  BEGIN
    READ TABLE LT_OBJEK WITH KEY OBJEK = L_OBJECT BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      REFRESH LT_ATNAM.
      CLEAR LS_ATNAM.
      LOOP AT LT_OBJEK INTO LS_OBJEK FROM SY-TABIX .
        IF LS_OBJEK-OBJEK = L_OBJECT .
          LS_ATNAM-ATNAM = LS_OBJEK-ATNAM .
          LS_ATNAM-ATWRT = LS_OBJEK-ATWRT.
          APPEND LS_ATNAM TO LT_ATNAM .
          CLEAR LS_ATNAM .
        ELSE.
          EXIT.
        ENDIF.
        CLEAR LS_OBJEK .
      ENDLOOP.
    ENDIF.
*    SORT LT_ATNAM BY ATNAM .
*&--代码添加 BY HANDYBY 06.06.2017 11:44:05  END

*产品类型
*&--代码添加 BY HANDYBY 06.06.2017 11:45:35  BEGIN
    READ TABLE LT_ATNAM INTO LS_ATNAM
    WITH KEY ATNAM = 'CPLX' .
    IF SY-SUBRC = 0.
      GS_DATA-CPLX = LS_ATNAM-ATWRT.
    ENDIF.
*&--代码添加 BY HANDYBY 06.06.2017 11:45:35  END
*&--代码注释 BY HANDYBY 06.06.2017 11:45:51  BEGIN
*    READ TABLE IT_CHAR
*    WITH KEY CHARACT = 'CPLX'.
*    IF SY-SUBRC = 0.
*      GS_DATA-CPLX = IT_CHAR-VALUE_CHAR.
*    ENDIF.
*&--代码注释 BY HANDYBY 06.06.2017 11:45:51  END


*型号/规格
*&--代码添加 BY HANDYBY 06.06.2017 11:47:08  BEGIN
    READ TABLE LT_ATNAM INTO LS_ATNAM
    WITH KEY ATNAM = 'GGXH' .
    IF SY-SUBRC = 0.
      GS_DATA-GGXH = LS_ATNAM-ATWRT.
    ENDIF.
*&--代码添加 BY HANDYBY 06.06.2017 11:47:08  END
*&--代码注释 BY HANDYBY 06.06.2017 11:47:34  BEGIN
*     READ TABLE IT_CHAR
*    WITH KEY CHARACT = 'GGXH'.
*    IF SY-SUBRC = 0.
*      GS_DATA-GGXH = IT_CHAR-VALUE_CHAR.
*    ENDIF.
*&--代码注释 BY HANDYBY 06.06.2017 11:47:34  END


*&--代码添加 BY HANDYBY 06.06.2017 11:18:50  BEGIN

    READ TABLE GT_EKBE INTO GS_EKBE
    WITH KEY EBELN = GS_DATA-EBELN
             EBELP = GS_DATA-EBELP
             BEWTP = 'L' BINARY SEARCH TRANSPORTING NO FIELDS .
*&--代码添加 BY HANDYBY 06.06.2017 11:18:50  END
*&--代码注释 BY HANDYBY 06.06.2017 11:19:16  BEGIN
*    READ TABLE GT_EKBE INTO GS_EKBE
*    WITH KEY EBELN = GS_DATA-EBELN
*             EBELP = GS_DATA-EBELP
*             BEWTP = 'L'.
*&--代码注释 BY HANDYBY 06.06.2017 11:19:16  END

    IF SY-SUBRC = 0.
*处理交货单
      CLEAR L_MENGE.

*&--代码添加 BY HANDYBY 06.06.2017 12:35:35  BEGIN
      LOOP AT GT_EKBE INTO GS_EKBE FROM SY-TABIX .
        IF GS_EKBE-EBELN = GS_DATA-EBELN AND
            GS_EKBE-EBELP = GS_DATA-EBELP AND
            GS_EKBE-BEWTP = 'L' .

*&--代码添加 BY HANDYBY 06.06.2017 12:35:35  END
*&--代码注释 BY HANDYBY 06.06.2017 12:35:44  BEGIN
*          LOOP AT GT_EKBE INTO GS_EKBE
*      WHERE EBELN = GS_DATA-EBELN
*      AND   EBELP = GS_DATA-EBELP
*      AND   BEWTP = 'L'.
*&--代码注释 BY HANDYBY 06.06.2017 12:35:44  END

          GS_DATA-BELNR    = GS_EKBE-BELNR.
          GS_DATA-BUZEI    = GS_EKBE-BUZEI.
          GS_DATA-MENGE_1  = GS_EKBE-MENGE.
          GS_DATA-MENGE  = GS_EKBE-MENGE.
          L_MENGE        = L_MENGE + GS_EKBE-MENGE .

*交货日期
*&--代码添加 BY HANDYBY 06.06.2017 11:19:41  BEGIN

          READ TABLE GT_LIKP INTO GS_LIKP
          WITH KEY VBELN = GS_DATA-BELNR BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:19:41  END
*&--代码注释 BY HANDYBY 06.06.2017 11:20:03  BEGIN
*         READ TABLE GT_LIKP INTO GS_LIKP
*        WITH KEY VBELN = GS_DATA-BELNR.
*&--代码注释 BY HANDYBY 06.06.2017 11:20:03  END

          IF SY-SUBRC = 0.
            GS_DATA-WADAT_IST = GS_LIKP-WADAT_IST.
          ELSE.
            CONTINUE.
          ENDIF.

*交货单的总面积
*&--代码添加 BY HANDYBY 06.06.2017 11:20:16  BEGIN

          READ TABLE GT_MARA INTO GS_MARA
          WITH KEY MATNR = GS_DATA-MATNR BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 11:20:16  END
*&--代码注释 BY HANDYBY 06.06.2017 11:20:37  BEGIN
*        READ TABLE GT_MARA INTO GS_MARA
*        WITH KEY MATNR = GS_DATA-MATNR.
*&--代码注释 BY HANDYBY 06.06.2017 11:20:37  END

          IF SY-SUBRC = 0.
            GS_DATA-ZMJ = GS_MARA-HOEHE * GS_MARA-BREIT * GS_DATA-MENGE.
          ENDIF.

*已交货面积
          IF GS_DATA-WADAT_IST IS NOT INITIAL.
            GS_DATA-YFHMJ = GS_DATA-ZMJ.
          ELSE.
            GS_DATA-YFHMJ = 0.
          ENDIF.

*未发数量
          IF GS_DATA-WADAT_IST IS INITIAL.
            GS_DATA-WFSL = GS_DATA-MENGE.
          ELSE.
            GS_DATA-WFSL = 0.
          ENDIF.

*未发货面积
          GS_DATA-WFHMJ = GS_DATA-ZMJ - GS_DATA-YFHMJ.

          APPEND GS_DATA TO GT_DATA.

        ELSE .
          EXIT .
        ENDIF.
      ENDLOOP.
*当公司间采购订单数量不等于0的时候，显示一行未交货的数量
      IF L_MENGE - GS_EKPO-MENGE <> 0.
        "begin：未发数量为原采购项目中采购数量-已交货统计的数量 by it02 20170519
        GS_DATA-WFSL   = GS_EKPO-MENGE - L_MENGE.
        "end.
        GS_DATA-MENGE  = GS_EKPO-MENGE - L_MENGE.


*没有交货单的总面积
*&--代码添加 BY HANDYBY 06.06.2017 12:38:23  BEGIN
        READ TABLE GT_MARA INTO GS_MARA
        WITH KEY MATNR = GS_DATA-MATNR BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 12:38:23  END
*&--代码注释 BY HANDYBY 06.06.2017 12:38:36  BEGIN
*          READ TABLE GT_MARA INTO GS_MARA
*        WITH KEY MATNR = GS_DATA-MATNR.
*&--代码注释 BY HANDYBY 06.06.2017 12:38:36  END

        IF SY-SUBRC = 0.
          GS_DATA-ZMJ = GS_MARA-HOEHE * GS_MARA-BREIT * GS_DATA-MENGE.
        ENDIF.

*没有交货单的已交货面积
        GS_DATA-YFHMJ = 0.
        GS_DATA-ZMJ = GS_MARA-HOEHE * GS_MARA-BREIT * GS_DATA-MENGE.

*没有交货单未发货面积
        GS_DATA-WFHMJ = GS_DATA-ZMJ - GS_DATA-YFHMJ.
        "IT02 150617 清空追加行的 交货单号 、交货数量、交货单行号、实际交货日期 begin
        CLEAR : GS_DATA-BELNR ,GS_DATA-MENGE_1 ,GS_DATA-BUZEI ,GS_DATA-WADAT_IST .
        "IT02 150617 清空追加行的 交货单号 、交货数量、交货单行号、实际交货日期 end
        APPEND GS_DATA TO GT_DATA.
      ENDIF.
    ELSE.

*没有交货单的总面积
*&--代码添加 BY HANDYBY 06.06.2017 12:38:55  BEGIN
      READ TABLE GT_MARA INTO GS_MARA
      WITH KEY MATNR = GS_DATA-MATNR BINARY SEARCH .
*&--代码添加 BY HANDYBY 06.06.2017 12:38:55  END
*&--代码注释 BY HANDYBY 06.06.2017 12:39:05  BEGIN
*      READ TABLE GT_MARA INTO GS_MARA
*      WITH KEY MATNR = GS_DATA-MATNR.
*&--代码注释 BY HANDYBY 06.06.2017 12:39:05  END

      IF SY-SUBRC = 0.
        GS_DATA-ZMJ   = GS_MARA-HOEHE * GS_MARA-BREIT * GS_DATA-MENGE.
        GS_DATA-MEABM = GS_MARA-MEABM.
        IF GS_DATA-MEABM IS NOT INITIAL.
          CONCATENATE GS_DATA-MEABM '2' INTO GS_DATA-MEABM.
        ENDIF.
      ENDIF.

*没有交货单的已交货面积
      GS_DATA-YFHMJ = 0.
      GS_DATA-ZMJ = GS_MARA-HOEHE * GS_MARA-BREIT * GS_DATA-MENGE.

*没有交货单未发货面积
      GS_DATA-WFHMJ = GS_DATA-ZMJ - GS_DATA-YFHMJ.

*未发数量
      IF GS_DATA-WADAT_IST IS INITIAL.
        GS_DATA-WFSL = GS_DATA-MENGE.
      ELSE.
        GS_DATA-WFSL = 0.
      ENDIF.

      APPEND GS_DATA TO GT_DATA.
    ENDIF.

    CLEAR GS_DATA.
  ENDLOOP.

  SORT GT_DATA BY EBELN EBELP.
*删除不在交货日期
  DELETE GT_DATA WHERE WADAT_IST NOT IN S_WADAT.
  DELETE GT_DATA WHERE VBELN NOT IN S_VBELN .
  DELETE GT_DATA WHERE AUART NOT IN S_AUART .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
  CLEAR IT_FIELDCAT[].
  PERFORM FRM_FILL_CAT USING :
           1  '' 'EBELN'      '采购订单号' ,                         "采购订单号
           2  '' 'EBELP'       '采购订单行项目',
           3 '' 'BSART'      '公司间采购订单类型' ,                 "公司间采购订单类型
           4 '' 'BATXT'       '采购订单类型描述' ,                  "采购订单类型描述
           5 '' 'EKORG'       '采购组织' ,                         "采购组织
           6 '' 'WERKS'       '供应工厂' ,                        "供应工厂
           7 '' 'MATNR'       '物料编号' ,                        "物料编号
           8 '' 'TXZ01'       '物料描述' ,                        "物料描述
           9'' 'MEINS'        '单位' ,                           "单位
           10'' 'MATKL'       '物料组' ,                         "物料组
           11'' 'VBELN'       '销售订单' ,                       "销售订单
           12'' 'VBELP'       '销售订单行号',                     "销售订单行号
           13'' 'AUART'       '销售订单类型' ,                   "销售订单类
           14'' 'BEZEI'       '销售订单类型描述' ,                "销售订单类型描述
           15'' 'ERDAT'       '凭证日期' ,                      "凭证日期
           16'' 'ZSD0302'     '销售立项号' ,                    "销售立项号
           17'' 'XMMC'        '项目名称' ,                      "项目名称
           18'' 'VDATU'       '要求生产完成日期' ,               "要求生产完成日期
           19 '' 'VDATU_1'    '回复发货日期' ,                  "回复发货日期
           20 '' 'NAME1'      '经办人' ,                        "经办人
           21 '' 'MATNR_X'    '屏体虚拟物料' ,                  "屏体虚拟物料
           22 '' 'ARKTX'      '屏体物料描述' ,                 "屏体物料描述
           23 '' 'PSTYV'      '项目类型' ,                     "项目类型
           24 '' 'VTEXT'      '项目类别描述',                  "项目类别描述
           25 '' 'CPLX'       '产品类型'   ,                   "产品类型
           26 '' 'GGXH'       '规格型号'  ,                    "规格型号
           27 '' 'MENGE'      '公司间采购订单数量' ,              "采购订单数量
           28 '' 'BELNR'      '交货单编号' ,                   "交货单编号
           29 '' 'MENGE_1'    '交货单数量',                    "交货单数量
           30 '' 'BUZEI'      '交货单行号',                    "交货单行号
           31 '' 'WADAT_IST'  '实际交货日期' ,                 "实际交货日期
           32 '' 'ZMJ'        '总面积'     ,                  "总面积
           33 '' 'YFHMJ'      '已发货面积' ,                  "已发货面积
           34 '' 'WFHMJ'      '未发货面积'  ,                "未发货面积
           35 '' 'MEABM'      '面积单位'  ,                  "面积单位
           36 '' 'WFSL'       '未发数量',                    "未发数量
           37 '' 'RETPO'       '退货行',                    "退货行
           38 '' 'ELIKZ'       '交货已完成',                "交货已完成
           39 '' 'EGLKZ'       '最后交货'.

ENDFORM.                    " BUILD_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  frm_fill_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_POS      text
*      -->U_EDIT     text
*      -->U_FNAME    text
*      -->U_NAME     text
*----------------------------------------------------------------------*
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "frm_fill_cat

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_REPORT .

  DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.

* l_layout-CWIDTH_OPT = 'X'.
*  l_layout-box_fieldname = 'ZBOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'TOP-OF-PAGE'  "see FORM
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      IT_FIELDCAT            = IT_FIELDCAT[]
      I_SAVE                 = 'X'
      I_GRID_SETTINGS        = L_GRID_SETTINGS
      IS_LAYOUT              = L_LAYOUT
      IS_VARIANT             = G_VARIANT
    TABLES
      T_OUTTAB               = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_REPORT" DISPLAY_ALV_REPORT

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
  WA_HEADER-INFO =  SY-TITLE."'进料检验记录表打印'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* Date
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Date: '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING R_UCOMM LIKE SY-UCOMM
                                   RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE SY-UCOMM.
    WHEN '&ENTER'.
    WHEN '&DATA_SAVE'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_SAVE_DATA.

ENDFORM.                    "frm_save_data

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  SET PF-STATUS 'ZSD015_STATUS'.
ENDFORM.                    "set_pf_status



*&---------------------------------------------------------------------*
*&      Form  frm_read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_TDNAME   text
*      -->T_TDSPRAS  text
*      -->T_TDID     text
*      -->T_TDOBJECT text
*      -->T_TEXT     text
*----------------------------------------------------------------------*
FORM FRM_READ_TEXT USING T_TDNAME T_TDSPRAS T_TDID T_TDOBJECT CHANGING T_TEXT.
  DATA:LT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.
*  DATA:stxl LIKE stxl OCCURS 0 WITH HEADER LINE."抬头备注
  DATA L_STXL TYPE STXL.
  L_STXL-TDID     = T_TDID     .
  L_STXL-TDSPRAS  = T_TDSPRAS  .
  L_STXL-TDNAME   = T_TDNAME   .
  L_STXL-TDOBJECT = T_TDOBJECT .

*  SELECT SINGLE * FROM STXL INTO STXL
*    WHERE TDNAME = T_TDNAME AND TDID = T_TDID AND TDSPRAS = T_TDSPRAS AND TDOBJECT = T_TDOBJECT.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = L_STXL-TDID    "读取文本的id
      LANGUAGE                = L_STXL-TDSPRAS "读取文本的语言
      NAME                    = L_STXL-TDNAME    "读取文本的名字
      OBJECT                  = L_STXL-TDOBJECT
    TABLES
      LINES                   = LT_TLINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

*  DATA: itemp LIKE thead-tdname."itemp为变量无值

  LOOP AT LT_TLINE .
    CONCATENATE T_TEXT LT_TLINE-TDLINE INTO T_TEXT SEPARATED BY SPACE.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext
