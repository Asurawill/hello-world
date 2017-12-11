*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:zmm016
*内容描述           采购入库单打印
*版本       V1.0
*姓名       HANDLJ
*日期       18.03.2015 13:57:23
** 修改日期   开发人员  请求号        描述
" 20170310   IT02     ED1K905293   追加物料号为空，物料描述取采购订单短文本
*-----------------------------------------------------------------------------
REPORT ZMM016.

TABLES : MKPF,MSEG,EKKO,EKPO,KONV,RSEG,EKBE,T023.
TYPE-POOLS : SLIS.

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

TYPES:BEGIN OF TY_OUT,
        WERKS       TYPE MSEG-WERKS,
        LGORT       TYPE MSEG-LGORT,
        LIFNR       TYPE MSEG-LIFNR,
        LIFNAME     TYPE LFA1-NAME1,
        BUDAT       TYPE MKPF-BUDAT,
        BUDAT2      TYPE C LENGTH 10,
        CPUTM       TYPE MKPF-CPUTM,                        "20150513
        CPUTM2      TYPE C LENGTH 10,
        BUDAT_CPUTM TYPE CHAR30,
        EBELN       TYPE MSEG-EBELN,
        EBELP       TYPE MSEG-EBELP,
        MBLNR       TYPE MSEG-MBLNR,
        MBLNR2      TYPE MSEG-MBLNR,
        XBLNR       TYPE MKPF-XBLNR,
        MATNR       TYPE MSEG-MATNR,
        MAKTX       TYPE MAKT-MAKTX,
        CHARG       TYPE MSEG-CHARG,
        BWART       TYPE MSEG-BWART,
        SHKZG       TYPE MSEG-SHKZG,
        MENGE       TYPE MSEG-MENGE,
        MEINS       TYPE MSEG-MEINS,
        EKGRP       TYPE EKKO-EKGRP, "采购组
        EKNAM       TYPE T024-EKNAM, "采购组名
        ZEILE       TYPE QAMB-ZEILE, "物料凭证行号
        ZEILE2      TYPE QAMB-ZEILE, "质检参考凭证号
        MJAHR       TYPE MKPF-MJAHR, "年度
        ZBOX        TYPE  C  ,         "选中
*&--代码添加 BY HANDYBY 10.05.2017 11:55:57  BEGIN
        POST1       TYPE PRPS-POST1 , "项目名称
*&--代码添加 BY HANDYBY 10.05.2017 11:55:57  END
      END OF TY_OUT.
TYPES:BEGIN OF TYP3_OUT,
        WERKS       TYPE MSEG-WERKS,
        LGORT       TYPE MSEG-LGORT,
        LIFNR       TYPE MSEG-LIFNR,
        LIFNAME     TYPE LFA1-NAME1,
        BUDAT       TYPE MKPF-BUDAT,
        BUDAT2      TYPE C LENGTH 10,
        CPUTM       TYPE MKPF-CPUTM,                        "20150513
        CPUTM2      TYPE C LENGTH 10,
        BUDAT_CPUTM TYPE CHAR30,
        EBELN       TYPE MSEG-EBELN,
        EBELP       TYPE MSEG-EBELP,
        MBLNR       TYPE MSEG-MBLNR,
        MBLNR2      TYPE MSEG-MBLNR,
        XBLNR       TYPE MKPF-XBLNR,
        MATNR       TYPE MSEG-MATNR,
        MAKTX       TYPE MAKT-MAKTX,
        CHARG       TYPE MSEG-CHARG,
        BWART       TYPE MSEG-BWART,
        SHKZG       TYPE MSEG-SHKZG,
        MENGE       TYPE MSEG-MENGE,
        MEINS       TYPE MSEG-MEINS,
        NETPR       TYPE EKPO-NETPR, "不含税单价
        NETWR       TYPE EKPO-NETWR, "不含税金额(标准成本)
        NETWR_1     TYPE EKPO-NETWR, "不含税金额
        KBETR       TYPE KONV-KBETR, "含税单价
        KWERT       TYPE KONV-KWERT , "含税金额(标准成本)
        KWERT_1     TYPE KONV-KWERT , "含税金额
        WRBTR       TYPE RSEG-WRBTR, "发票不含税金额
        WRBTR_HS    TYPE RSEG-WRBTR, "发票含税金额
        WKPJE       TYPE RSEG-WRBTR, "未开票不含税金额(标准成本)
        WKPJE_1     TYPE RSEG-WRBTR, "未开票不含税金额
        WKPJE_HS    TYPE RSEG-WRBTR, "未开票含税金额(标准成本)
        WKPJE_HS_1  TYPE RSEG-WRBTR, "未开票含税金额
        BELNR       TYPE RSEG-BELNR, "采购发票号
        WAERS       TYPE EKKO-WAERS, "货币
        PEINH       TYPE  EKPO-PEINH , "价格单位：
        SLV         TYPE CHAR100, "税率
        DHRQ        TYPE EKBE-BUDAT, "到货日期
        EKGRP       TYPE EKKO-EKGRP, "采购组
        EKNAM       TYPE T024-EKNAM, "采购组名
        AFNAM       TYPE EKPO-AFNAM, "申请者
        ZEILE       TYPE QAMB-ZEILE, "质检参考凭证行号
        MJAHR       TYPE MKPF-MJAHR, "年度
        MATKL       TYPE T023-MATKL, "物料组
        WGBEZ       TYPE T023T-WGBEZ, "物料组名称
        ZEILE2      TYPE QAMB-ZEILE, "质检参考凭证号
        " WGBEZ       TYPE T023-WGBEZ,"物料组名
        ZBOX        TYPE  C  ,         "选中
*&--代码添加 BY HANDYBY 10.05.2017 11:55:57  BEGIN
        POST1       TYPE PRPS-POST1 , "项目名称
*&--代码添加 BY HANDYBY 10.05.2017 11:55:57  END
      END OF TYP3_OUT.

TYPES:BEGIN OF TY_CGMX,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE MSEG-MATNR,
      END OF TY_CGMX.

DATA:BEGIN OF GS_EKPO  .
DATA:EBELN LIKE EKPO-EBELN,
     EBELP LIKE EKPO-EBELP,
     KNUMV LIKE EKKO-KNUMV,
     WAERS LIKE EKKO-WAERS  ,     "           币种:EKKO~WAERS
     NETPR TYPE EKPO-NETPR, "不含税单价
     NETWR TYPE EKPO-NETWR, "不含税金额
     MWSKZ TYPE EKPO-MWSKZ , "税码
     EKGRP TYPE EKKO-EKGRP, "采购组
     AFNAM TYPE EKPO-AFNAM , "采购申请者
     PEINH TYPE   EKPO-PEINH  , "价格单位：
     MATKL TYPE EKPO-MATKL. "物料组
DATA: END OF GS_EKPO.
DATA: BEGIN OF GS_FP.
DATA:EBELN    LIKE RSEG-EBELN,
     EBELP    LIKE RSEG-EBELP,
     WRBTR    TYPE RSEG-WRBTR, "发票不含税金额
     WRBTR_HS TYPE RSEG-WRBTR, "含税金额
     WKPJE    TYPE RSEG-WRBTR, "未开票不含税金额
     WKPJE_HS TYPE RSEG-WRBTR. "未开票含税金额
DATA:END OF GS_FP.
DATA:IT_OUT TYPE TY_OUT OCCURS 0 WITH HEADER LINE.
DATA:IT_P3 TYPE TYP3_OUT OCCURS 0 WITH HEADER LINE.
DATA:IT_122 TYPE TYP3_OUT OCCURS 0 WITH HEADER LINE.
DATA:IT_P356 TYPE TYP3_OUT OCCURS 0 WITH HEADER LINE.
DATA:IT_PRT TYPE TABLE OF TY_OUT.
DATA:WA_PRT TYPE TY_OUT.
DATA:GT_RSEG LIKE TABLE OF RSEG WITH HEADER LINE.
DATA:GT_EKBE LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:GT_EKPO LIKE TABLE OF GS_EKPO WITH HEADER LINE.
DATA:GT_KONV LIKE TABLE OF KONV WITH HEADER LINE.
DATA:GT_FP LIKE TABLE OF GS_FP WITH HEADER LINE.
DATA T_FTAXP    TYPE TABLE OF FTAXP.
DATA S_FTAXP  TYPE FTAXP.
DATA:P_CHECK TYPE C .
DATA:T_102 LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:T_356 LIKE TABLE OF EKBE WITH HEADER LINE.
DATA:T_322 LIKE TABLE OF MSEG WITH HEADER LINE.
DATA:T_CXPZ LIKE TABLE OF MSEG WITH HEADER LINE. "作为冲销凭证判断
DATA:T_123 LIKE TABLE OF MSEG WITH HEADER LINE.
DATA:T_T024 LIKE TABLE OF T024 WITH HEADER LINE.
DATA:GT_EKKO LIKE TABLE OF EKKO WITH HEADER LINE.

DATA:GT_CGMX TYPE TABLE OF TY_CGMX WITH HEADER LINE,
     GS_CGMX TYPE TY_CGMX.

DATA:GT_CGMX_MS TYPE TABLE OF EKPO,
     GS_CGMX_MS TYPE EKPO.
*    PERFORM frm_read_text USING wa_prt-ls_kdauf sy-langu '0001' 'VBBK'
*                 CHANGING wa_prt-ls_kdname.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002 .
SELECT-OPTIONS:S_EBELN FOR MSEG-EBELN MODIF ID MP1,
               S_MATNR FOR MSEG-MATNR MODIF ID MP1,
               S_WERKS FOR MSEG-WERKS  MODIF ID MP1 OBLIGATORY,
               S_MBLNR FOR MSEG-MBLNR MODIF ID MP1,
               S_MJAHR FOR MSEG-MJAHR MODIF ID MP1 DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS:S_LIFNR FOR MSEG-LIFNR MODIF ID MP3,"供应商
               S_BUDAT FOR MKPF-BUDAT MODIF ID MP3,"入库日期
               S_XBLNR FOR RSEG-XBLNR MODIF ID MP3,"交货单
               S_BELNR FOR RSEG-BELNR MODIF ID MP3,"采购发票
               S_EKGRP FOR EKKO-EKGRP MODIF ID MP3. "采购组
PARAMETERS:YKP  AS CHECKBOX MODIF ID MP3. "已开票
PARAMETERS:WKP AS CHECKBOX MODIF ID MP3. "未开票
SELECTION-SCREEN END OF BLOCK B1.
"SELECTION-SCREEN BEGIN OF BLOCK b2  .

"SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK B3  .
PARAMETERS:P_1 RADIOBUTTON GROUP RAD1 DEFAULT 'X' USER-COMMAND UCOMM,
           P_2 RADIOBUTTON GROUP RAD1,
           P_3 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN END OF BLOCK B3.

**GETTING DEFAULT VARIANT


INITIALIZATION.
*  gx_variant-report = sy-repid.
*  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
*    EXPORTING
*      i_save     = g_save
*    CHANGING
*      cs_variant = gx_variant
*    EXCEPTIONS
*      not_found  = 2.
*  IF sy-subrc = 0.
*    g_variant = gx_variant-variant.
*  ENDIF.
AT SELECTION-SCREEN OUTPUT.
  CHECK P_CHECK EQ ''.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'MP3'.
      IF P_3 NE 'X'.
        SCREEN-INPUT = 0 .
        SCREEN-ACTIVE = 0.
      ENDIF.
      IF P_3 = 'X'.
        SCREEN-ACTIVE = 1 .
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  PERFORM FRM_AUTH_CHECK.

**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM DATA_RETRIVEL.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.

FORM FRM_AUTH_CHECK.
  CLEAR P_CHECK.
  DATA LT_T001W TYPE T001W OCCURS 0 WITH HEADER LINE.
  SELECT WERKS
    FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE LT_T001W
  WHERE WERKS IN S_WERKS.
  LOOP AT LT_T001W WHERE WERKS IN S_WERKS.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*            ID 'ACTVT' FIELD '__________'
             ID 'WERKS' FIELD LT_T001W-WERKS.
    IF SY-SUBRC <> 0.
      MESSAGE E603(FCO) WITH LT_T001W-WERKS.
    ENDIF.
  ENDLOOP.
  IF P_3 EQ 'X'. " 控制只有一定的查询权限才能查看 出入库选项
    AUTHORITY-CHECK OBJECT 'ZMM_MM016'
         ID 'ACTVT' FIELD '03' .
    IF SY-SUBRC <> 0.
      P_CHECK = 'X'.
      MESSAGE '无权限查询采购出入库选项查询' TYPE 'E'. ." TYPE 'S' DISPLAY LIKE 'E' ..
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVEL .
  RANGES LR_BWART FOR MSEG-BWART.
  DATA LT_MAKT    TYPE TABLE OF MAKT WITH HEADER LINE.
  DATA LT_OUT     TYPE TABLE OF TY_OUT WITH HEADER LINE.
  DATA LT_EKPO    TYPE TABLE OF EKPO WITH HEADER LINE.
  DATA LT_QAMB1   TYPE TABLE OF QAMB WITH HEADER LINE.
  DATA LT_QAMB2   TYPE TABLE OF QAMB WITH HEADER LINE.
  DATA LT_MKPF    TYPE TABLE OF MKPF WITH HEADER LINE.

*&--代码添加 BY HANDYBY 10.05.2017 13:01:42  BEGIN
  DATA:BEGIN OF LS_EKKN,
         EBELN      TYPE EKKN-EBELN,
         EBELP      TYPE EKKN-EBELP,
         ZEKKN      TYPE EKKN-ZEKKN,
         PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
       END OF LS_EKKN.
  DATA LT_EKKN LIKE TABLE OF LS_EKKN.
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POST1 TYPE PRPS-POST1,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .
*&--代码添加 BY HANDYBY 10.05.2017 13:01:42  END

  DATA:L_TABIX  TYPE I.
  FIELD-SYMBOLS   <LW_OUT> TYPE TY_OUT.
  CLEAR LR_BWART[].
  IF P_1 = 'X' .
    LR_BWART-SIGN = 'I'.
    LR_BWART-OPTION = 'EQ'.
    LR_BWART-LOW = '321'.
    APPEND LR_BWART.
    LR_BWART-LOW = '101'.
    APPEND LR_BWART.
    LR_BWART-LOW = '105'.
    APPEND LR_BWART.
  ENDIF.

  IF P_2 = 'X'.
    LR_BWART-SIGN = 'I'.
    LR_BWART-OPTION = 'EQ'.
    LR_BWART-LOW = '122'.
    APPEND LR_BWART.
    LR_BWART-LOW = '161'.
    APPEND LR_BWART.
    LR_BWART-LOW = '124'.
    APPEND LR_BWART.
  ENDIF.

  IF P_3 = 'X' .
    LR_BWART-SIGN = 'I'.
    LR_BWART-OPTION = 'EQ'.
    LR_BWART-LOW = '321'.
    APPEND LR_BWART.
    LR_BWART-LOW = '101'.
    APPEND LR_BWART.
    LR_BWART-LOW = '103'.
    APPEND LR_BWART.
*    LR_BWART-LOW = '105'.
*    APPEND LR_BWART.
*    LR_BWART-LOW = '106'.
*    APPEND LR_BWART.
    LR_BWART-SIGN = 'I'.
    LR_BWART-OPTION = 'EQ'.
    LR_BWART-LOW = '122'.
    APPEND LR_BWART.
    LR_BWART-LOW = '161'.
    APPEND LR_BWART.
  ENDIF.

  IF P_3 NE 'X'.
    SELECT  MKPF~BUDAT
            MKPF~CPUTM
            MKPF~XBLNR
            MKPF~MJAHR
            MSEG~WERKS
            MSEG~LGORT
            MSEG~LIFNR
            LFA1~NAME1 AS LIFNAME
            MSEG~MBLNR
            MSEG~MATNR
            MSEG~EBELN
            MSEG~EBELP
            MSEG~SHKZG
*          mseg~menge "对应采购订单物料凭证 该处数量为空
            MSEG~ERFMG AS MENGE
*          mseg~meins
            MSEG~ERFME AS MEINS
            MSEG~CHARG
            MSEG~BWART
      FROM  MSEG LEFT JOIN MKPF
      ON    MSEG~MBLNR = MKPF~MBLNR
      AND   MSEG~MJAHR = MKPF~MJAHR
      LEFT  JOIN LFA1
      ON    MSEG~LIFNR = LFA1~LIFNR
      INTO  CORRESPONDING FIELDS OF TABLE LT_OUT
      WHERE MSEG~WERKS IN S_WERKS
      AND   MSEG~MATNR IN S_MATNR
      AND   MSEG~EBELN IN S_EBELN
      AND   MSEG~MBLNR IN S_MBLNR
*    AND   mseg~shkzg  = 'H'
      AND   MSEG~BWART  IN LR_BWART
      AND   MSEG~MJAHR  IN S_MJAHR
   "   AND   mseg~mjahr  = sy-datum(4)
    .
  ELSE.  "P_3 新增查询 150618
    SELECT  MKPF~BUDAT
          MKPF~CPUTM
          MKPF~XBLNR
          MKPF~MJAHR
          MSEG~WERKS
          MSEG~LGORT
          MSEG~LIFNR
          LFA1~NAME1 AS LIFNAME
          MSEG~MBLNR
          MSEG~ZEILE
          MSEG~MATNR
          MSEG~EBELN
          MSEG~EBELP
          MSEG~SHKZG
*          mseg~menge "对应采购订单物料凭证 该处数量为空
          MSEG~ERFMG AS MENGE
*          mseg~meins
          MSEG~ERFME AS MEINS
          MSEG~CHARG
          MSEG~BWART
    FROM  MKPF INNER JOIN MSEG
    ON    MSEG~MBLNR = MKPF~MBLNR
    AND   MSEG~MJAHR = MKPF~MJAHR
    LEFT  JOIN LFA1
    ON    MSEG~LIFNR = LFA1~LIFNR
    INTO  CORRESPONDING FIELDS OF TABLE LT_OUT
    WHERE      MKPF~BUDAT IN S_BUDAT   "150618  新增 入库日期
 "   AND MKPF~XBLNR IN  S_XBLNR     " 150618 新增 交货单号
    AND    MSEG~WERKS IN S_WERKS
    AND   MSEG~MATNR IN S_MATNR
    AND   MSEG~EBELN IN S_EBELN
    AND   MSEG~MBLNR IN S_MBLNR
*    AND   mseg~shkzg  = 'H'
    AND   MSEG~BWART  IN LR_BWART
    AND   MSEG~MJAHR  IN S_MJAHR
  "  AND   MSEG~MJAHR  = SY-DATUM(4)
    AND   MSEG~LIFNR IN S_LIFNR   "150618 新增供应商
    AND  MSEG~EBELN IN S_EBELN  . "150618 新增 采购订单 .
    SELECT  MKPF~BUDAT
            MKPF~CPUTM
            MKPF~XBLNR
            MKPF~MJAHR
            MSEG~WERKS
            MSEG~LGORT
            MSEG~LIFNR
            LFA1~NAME1 AS LIFNAME
            MSEG~MBLNR
            MSEG~ZEILE
            MSEG~MATNR
            MSEG~EBELN
            MSEG~EBELP
            MSEG~SHKZG
*          mseg~menge "对应采购订单物料凭证 该处数量为空
            MSEG~ERFMG AS MENGE
*          mseg~meins
            MSEG~ERFME AS MEINS
            MSEG~CHARG
            MSEG~BWART
      FROM  MKPF INNER JOIN MSEG
      ON    MSEG~MBLNR = MKPF~MBLNR
      AND   MSEG~MJAHR = MKPF~MJAHR
      LEFT  JOIN LFA1
      ON    MSEG~LIFNR = LFA1~LIFNR
      APPENDING CORRESPONDING FIELDS OF TABLE LT_OUT
      WHERE      MKPF~BUDAT IN S_BUDAT   "150618  新增 入库日期
   "   AND MKPF~XBLNR IN  S_XBLNR     " 150618 新增 交货单号
      AND    MSEG~WERKS IN S_WERKS
      AND   MSEG~MATNR IN S_MATNR
      AND   MSEG~EBELN IN S_EBELN
      AND   MSEG~MBLNR IN S_MBLNR
*    AND   mseg~shkzg  = 'H'
      AND   MSEG~BWART  = '102'
      AND   MSEG~SMBLN  = ''
        AND   MSEG~MJAHR  IN S_MJAHR
    "  AND   MSEG~MJAHR  = SY-DATUM(4)
      AND   MSEG~LIFNR IN S_LIFNR   "150618 新增供应商
      AND  MSEG~EBELN IN S_EBELN   "150618 新增 采购订单


      .
  ENDIF.

*&--代码添加 BY HANDYBY 10.05.2017 12:51:50  BEGIN
  IF LT_OUT[] IS NOT INITIAL .
    SELECT EBELN
           EBELP
           ZEKKN
           PS_PSP_PNR
      INTO CORRESPONDING FIELDS OF TABLE LT_EKKN
      FROM EKKN
       FOR ALL ENTRIES IN LT_OUT
     WHERE EBELN = LT_OUT-EBELN
       AND EBELP = LT_OUT-EBELP .
    IF LT_EKKN IS NOT INITIAL .
      SORT LT_EKKN BY EBELN EBELP .
      SELECT PSPNR
             POST1
        INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
        FROM PRPS
         FOR ALL ENTRIES IN LT_EKKN
       WHERE PSPNR = LT_EKKN-PS_PSP_PNR .
      SORT LT_PRPS BY PSPNR .
    ENDIF.
  ENDIF.
*&--代码添加 BY HANDYBY 10.05.2017 12:51:50  END


  "移动类型321只取借方
  DELETE LT_OUT WHERE BWART = '321' AND SHKZG = 'H'.

  MOVE-CORRESPONDING LT_OUT[] TO GT_CGMX[].

  DELETE GT_CGMX WHERE MATNR  IS NOT  INITIAL.

  SORT GT_CGMX BY EBELN EBELP .
  DELETE ADJACENT DUPLICATES FROM GT_CGMX COMPARING EBELN EBELP .


  IF GT_CGMX[] IS NOT INITIAL.
    SELECT * INTO TABLE GT_CGMX_MS
       FROM EKPO
       FOR ALL ENTRIES IN GT_CGMX
       WHERE EBELN = GT_CGMX-EBELN
         AND EBELP = GT_CGMX-EBELP
        .
    SORT GT_CGMX_MS BY EBELN EBELP .

  ENDIF.


  IF P_3 EQ 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_RSEG
      FROM RBKP
     INNER JOIN RSEG ON  RBKP~BELNR = RSEG~BELNR
     FOR ALL ENTRIES IN LT_OUT
    "WHERE  RBKP~RBSTAT = '5'
       WHERE  RBKP~STBLG = ''    "GT_RSEG存放的是未冲销发票单据 根据采购订单号 、行项目号、交货单号、收货原始物料凭证
      AND RBKP~XRECH = 'X'
       AND   RSEG~EBELN = LT_OUT-EBELN
      AND RSEG~EBELP = LT_OUT-EBELP.
*       AND RSEG~LFBNR = IT_OUT-MBLNR2
*       AND RSEG~XBLNR  = IT_OUT-XBLNR
    .
    IF S_BELNR IS NOT INITIAL.
      DELETE GT_RSEG WHERE BELNR NOT IN S_BELNR.
    ENDIF.

  ENDIF.

  IF LT_OUT[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
      FROM EKKO
      FOR ALL ENTRIES IN LT_OUT
    WHERE EBELN = LT_OUT-EBELN
      AND EKGRP IN S_EKGRP.
    SORT GT_EKKO BY EBELN .

    SELECT * INTO CORRESPONDING FIELDS OF TABLE T_T024
    FROM T024 .
    SELECT PRUEFLOS
           ZAEHLER
           TYP
           MBLNR
           MJAHR
           ZEILE
      FROM QAMB
      INTO CORRESPONDING FIELDS OF TABLE LT_QAMB1
      FOR ALL ENTRIES IN LT_OUT
      WHERE MBLNR = LT_OUT-MBLNR
       AND   MJAHR = LT_OUT-MJAHR

    .
    IF LT_QAMB1[] IS NOT INITIAL.
      SELECT PRUEFLOS
             ZAEHLER
             TYP
             MBLNR
             MJAHR
             ZEILE
        FROM QAMB
        INTO CORRESPONDING FIELDS OF TABLE LT_QAMB2
        FOR ALL ENTRIES IN LT_QAMB1
        WHERE PRUEFLOS = LT_QAMB1-PRUEFLOS
        AND   TYP      = '1'
      .
    ENDIF.
    IF P_1 = 'X' OR P_3 = 'X'.
      SELECT EKPO~EBELN
             EKPO~EBELP
             EKPO~TXZ01
        FROM EKPO JOIN EKKO
        ON EKPO~EBELN = EKKO~EBELN
        INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
        FOR ALL ENTRIES IN LT_OUT
        WHERE EKPO~EBELN = LT_OUT-EBELN
        AND   EKPO~EBELP = LT_OUT-EBELP
        AND   EKKO~BSART IN ( 'Z02','Z04' )
      .
    ENDIF.

*    "z02,z04采购订单只保留101
*    LOOP AT lt_ekpo.
*      DELETE lt_out WHERE ebeln = lt_ekpo-ebeln
*                      AND ebelp = lt_ekpo-ebelp
*                      AND bwart = '321'.
*    ENDLOOP.



    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FOR ALL ENTRIES IN LT_OUT
      WHERE MATNR = LT_OUT-MATNR
      AND   SPRAS = SY-LANGU
    .

    SORT LT_OUT BY  WERKS
                    LGORT
                    LIFNR
                    LIFNAME
                    BUDAT
                    EBELN
                    EBELP
                    MBLNR
                    MATNR
                    MAKTX
                    CHARG
                    BWART
                    SHKZG .

    LOOP AT LT_OUT ASSIGNING <LW_OUT>.

      L_TABIX = SY-TABIX.
      IF P_3 EQ 'X' .
        READ TABLE GT_EKKO WITH KEY EBELN = <LW_OUT>-EBELN
                             BINARY SEARCH.
        IF SY-SUBRC NE 0 .
          DELETE LT_OUT INDEX L_TABIX.
          CONTINUE.
        ENDIF.
        IF GT_RSEG[] IS NOT INITIAL AND S_BELNR IS NOT INITIAL .
          READ TABLE GT_RSEG WITH KEY EBELN = <LW_OUT>-EBELN EBELP = <LW_OUT>-EBELP .
          IF SY-SUBRC NE 0 .
            DELETE LT_OUT INDEX L_TABIX.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      WRITE <LW_OUT>-BUDAT TO <LW_OUT>-BUDAT2 .
      WRITE <LW_OUT>-CPUTM TO <LW_OUT>-CPUTM2 .
      CONCATENATE <LW_OUT>-BUDAT2  ',' <LW_OUT>-CPUTM2 INTO <LW_OUT>-BUDAT_CPUTM.

      AT NEW BWART.
        CLEAR IT_OUT.
        CLEAR LT_MAKT.
        IF <LW_OUT>-MATNR IS NOT INITIAL.
          READ TABLE LT_MAKT WITH  KEY MATNR = <LW_OUT>-MATNR.
          IF SY-SUBRC = 0.
            <LW_OUT>-MAKTX = LT_MAKT-MAKTX.
          ENDIF.
        ELSE.
          READ TABLE GT_CGMX_MS INTO GS_CGMX_MS WITH KEY EBELN = <LW_OUT>-EBELN
                                                         EBELP = <LW_OUT>-EBELP
                                                        BINARY SEARCH.
          IF SY-SUBRC EQ 0 .
            <LW_OUT>-MAKTX = GS_CGMX_MS-TXZ01.
          ENDIF.
        ENDIF.


        CLEAR LT_QAMB1.
        READ TABLE LT_EKPO WITH KEY EBELN = <LW_OUT>-EBELN EBELP = <LW_OUT>-EBELP.
        IF SY-SUBRC NE 0 .
          IF <LW_OUT>-BWART NE '103' AND <LW_OUT>-BWART NE '161' ." AND <LW_OUT>-BWART NE '105' AND <LW_OUT>-BWART NE '106' .
            READ TABLE LT_QAMB1 WITH KEY MBLNR = <LW_OUT>-MBLNR MJAHR = <LW_OUT>-MJAHR.
            IF SY-SUBRC = 0.
              CLEAR LT_QAMB2.
              READ TABLE LT_QAMB2 WITH KEY PRUEFLOS = LT_QAMB1-PRUEFLOS.
              IF SY-SUBRC = 0.
                <LW_OUT>-MBLNR2 = LT_QAMB2-MBLNR.
                IF  <LW_OUT>-BWART <> '122'.
                  <LW_OUT>-ZEILE2 = LT_QAMB2-ZEILE."参考凭证行号 ADD IT02 150720
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.


        IF P_1 = 'X' OR P_3 = 'X'.
          "Z02和Z04的采购订单类型   就保留101的
          CLEAR LT_EKPO.
          IF <LW_OUT>-BWART NE '105' OR <LW_OUT>-BWART NE '103' OR <LW_OUT>-BWART NE '106' . "add 105 无需根据采购类型判断150731
            READ TABLE LT_EKPO WITH KEY EBELN = <LW_OUT>-EBELN EBELP = <LW_OUT>-EBELP.
            IF SY-SUBRC = 0.
              "<lw_out>-maktx = lt_ekpo-txz01.
              IF <LW_OUT>-BWART = '321'.
                CONTINUE.
              ENDIF.
            ELSEIF <LW_OUT>-WERKS NE '2110'.
              "标准的采购订单的收货入库  只保留321的移动类型
              READ TABLE LT_QAMB1 WITH KEY MBLNR = <LW_OUT>-MBLNR MJAHR = <LW_OUT>-MJAHR ZEILE = <LW_OUT>-ZEILE. "增加 只有 101做了检验批 的数据 ，移动类型并为101 的记录要过滤不显示
              IF SY-SUBRC = 0 .
                IF <LW_OUT>-BWART = '101'.
                  CONTINUE.
                ENDIF.
              ENDIF.
*               IF <LW_OUT>-BWART = '101'.
*                CONTINUE.
*              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING <LW_OUT> TO IT_OUT.
        CLEAR IT_OUT-MENGE.
      ENDAT.
      IF <LW_OUT>-SHKZG = 'S'.
        IT_OUT-MENGE = IT_OUT-MENGE + <LW_OUT>-MENGE .
      ELSEIF <LW_OUT>-SHKZG = 'H'.
        IT_OUT-MENGE = IT_OUT-MENGE - <LW_OUT>-MENGE.
      ENDIF.

*&--代码添加 BY HANDYBY 10.05.2017 12:56:51  BEGIN
* 添加项目名称字段
      READ TABLE LT_EKKN INTO LS_EKKN WITH KEY EBELN = <LW_OUT>-EBELN
                                               EBELP = <LW_OUT>-EBELP BINARY SEARCH .
      IF SY-SUBRC = 0 .
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY PSPNR = LS_EKKN-PS_PSP_PNR BINARY SEARCH .
        IF SY-SUBRC = 0 .
          IT_OUT-POST1 = LS_PRPS-POST1 .
          CLEAR LS_PRPS .
        ENDIF.
        CLEAR LS_EKKN .
      ENDIF.
*&--代码添加 BY HANDYBY 10.05.2017 12:56:51  END


      AT END OF BWART.
        IT_OUT-MENGE = ABS( IT_OUT-MENGE ).
        APPEND IT_OUT.
        CLEAR IT_OUT.
      ENDAT.
    ENDLOOP.

    IF IT_OUT[] IS NOT INITIAL.
      SELECT MBLNR
         MJAHR
         XBLNR
    FROM MKPF
    INTO CORRESPONDING FIELDS OF TABLE LT_MKPF
    FOR ALL ENTRIES IN IT_OUT
    WHERE MBLNR = IT_OUT-MBLNR2
      .
    ENDIF.
    SORT LT_MKPF BY MBLNR MJAHR.
    "  IF SY-SUBRC = 0.
    IF P_3 NE 'X'.
      LOOP AT IT_OUT ASSIGNING <LW_OUT>.
        CLEAR LT_MKPF.
        READ TABLE GT_EKKO WITH KEY EBELN = <LW_OUT>-EBELN.
        IF SY-SUBRC = 0.
          <LW_OUT>-EKGRP = GT_EKKO-EKGRP. "采购组
        ENDIF.
        READ TABLE T_T024 WITH KEY EKGRP = <LW_OUT>-EKGRP.
        IF SY-SUBRC = 0.
          <LW_OUT>-EKNAM = T_T024-EKNAM. "采购组名
        ENDIF.
        READ TABLE LT_MKPF WITH KEY MBLNR = <LW_OUT>-MBLNR2.
        IF SY-SUBRC = 0.
          <LW_OUT>-XBLNR = LT_MKPF-XBLNR.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "   ENDIF.
    IF P_3 = 'X'.
      IF IT_OUT[] IS NOT INITIAL.

        "冲销凭证筛选
        SELECT * INTO CORRESPONDING FIELDS OF TABLE T_CXPZ
          FROM MSEG
          FOR ALL ENTRIES IN IT_OUT
          WHERE MJAHR = IT_OUT-MJAHR AND SMBLN = IT_OUT-MBLNR AND SMBLP = IT_OUT-ZEILE.
        SORT T_CXPZ BY MJAHR SMBLN SMBLP .

        SELECT EKKO~EBELN EKKO~KNUMV EKKO~WAERS EKKO~EKGRP EKPO~EBELP EKPO~NETPR EKPO~NETWR EKPO~MWSKZ EKPO~PEINH EKPO~AFNAM EKPO~MATKL
          FROM EKKO
          INNER JOIN EKPO      "      GT_EKPO存放的 采购订单明细 根据采购订单号、行项目号
           ON EKKO~EBELN = EKPO~EBELN
           INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
          FOR ALL ENTRIES IN  IT_OUT
        WHERE EKPO~EBELN = IT_OUT-EBELN AND EKPO~EBELP = IT_OUT-EBELP
          AND  EKKO~EKGRP IN S_EKGRP .
        SORT GT_EKPO BY EBELN EBELP .
      ENDIF.
      IF GT_EKPO[] IS NOT INITIAL.

        SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
          FROM EKBE
          FOR ALL ENTRIES IN GT_EKPO
          WHERE  EBELN = GT_EKPO-EBELN
        AND EBELP = GT_EKPO-EBELP .

        T_356[] = GT_EKBE[] .
*        DELETE T_102 WHERE BWART <> '102' .
        SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_KONV
        FROM KONV
        FOR ALL ENTRIES IN GT_EKPO     "根据读取的采购行项目对应的 条件单号读取 条件价格
        WHERE  KNUMV = GT_EKPO-KNUMV
        AND KSCHL IN ('PBXX','PB00','Z010') .

      ENDIF.
      IF GT_RSEG[] IS NOT INITIAL.
        SORT GT_RSEG BY EBELN EBELP.
      ENDIF.
    ENDIF.
    IF P_3 = 'X'.  "以下逻辑读取 IT_OUT原数据 匹配对应的采购 发票号、采购单价 、发票金额 等数据 150618 IT02
      DATA:TMPMELNR TYPE MSEG-MBLNR.
      DATA:LFBNRTMP TYPE RSEG-LFBNR.
      DATA:ZEILETMP TYPE MSEG-ZEILE .
      LOOP AT IT_OUT ."ASSIGNING <LW_OUT>.
        READ TABLE T_CXPZ WITH KEY MJAHR = IT_OUT-MJAHR SMBLN = IT_OUT-MBLNR SMBLP = IT_OUT-ZEILE  .
        IF SY-SUBRC = 0.
          DELETE IT_OUT.                   "判断 后是否有正常的冲销凭证 ，若存在 就过滤不显示
          CONTINUE.
        ENDIF.

        READ TABLE GT_EKKO WITH KEY EBELN = IT_OUT-EBELN.
        IF SY-SUBRC = 0.
          IT_OUT-EKGRP = GT_EKKO-EKGRP. "采购组
        ENDIF.
        READ TABLE T_T024 WITH KEY EKGRP = IT_OUT-EKGRP.
        IF SY-SUBRC = 0.
          IT_OUT-EKNAM = T_T024-EKNAM. "采购组名
        ENDIF.
        READ TABLE LT_MKPF WITH KEY MBLNR = IT_OUT-MBLNR2.
        IF SY-SUBRC = 0.
          IT_OUT-XBLNR = LT_MKPF-XBLNR.
        ENDIF.
        CLEAR: TMPMELNR ,ZEILETMP,LFBNRTMP.
        IF IT_OUT-MBLNR2 NE ''.
          TMPMELNR = IT_OUT-MBLNR2 .
          LFBNRTMP = IT_OUT-MBLNR2 .
          ZEILETMP = IT_OUT-ZEILE2.
        ELSE.
          TMPMELNR = IT_OUT-MBLNR.
          LFBNRTMP = IT_OUT-MBLNR.
          ZEILETMP = IT_OUT-ZEILE.
        ENDIF.



        IF IT_OUT-BWART = '122'. "隐藏 122提行需根据 QAMB 物料凭证 年度、行号 、类型为3 即可屏蔽
          READ TABLE LT_QAMB1 WITH KEY TYP = '3' MBLNR = IT_OUT-MBLNR MJAHR = IT_OUT-MJAHR ZEILE = IT_OUT-ZEILE .
          IF SY-SUBRC = 0 .
            MOVE-CORRESPONDING IT_OUT TO IT_122. "需隐藏掉的122 追加到it_122 .到时 321 含税金额数据需减掉 122 含税数据
            APPEND IT_122.
            DELETE IT_OUT ."INDEX SY-TABIX .
            CONTINUE .

          ENDIF.
        ENDIF.



        IF  IT_OUT-BWART = '105' ." OR IT_OUT-BWART = '106' .
          READ TABLE T_356 WITH KEY GJAHR = IT_OUT-MJAHR BELNR = IT_OUT-MBLNR  .
          IF SY-SUBRC = 0 .
            IT_OUT-MBLNR2 = T_356-LFBNR .  "105 .106 读取103的凭证为参考凭证
            READ TABLE LT_OUT WITH KEY MBLNR = T_356-LFBNR .
            IF SY-SUBRC = 0 .
              IT_OUT-XBLNR = LT_OUT-XBLNR ." 105\106 de 交货单号为 103 的凭证
            ENDIF.
            "
          ENDIF.
        ENDIF.

        CLEAR IT_P3.
        MOVE-CORRESPONDING IT_OUT TO IT_P3.
        READ TABLE  GT_EKPO WITH KEY EBELN = IT_P3-EBELN EBELP = IT_P3-EBELP.
        IF SY-SUBRC = 0.
          IT_P3-MATKL = GT_EKPO-MATKL."物料组
          SELECT SINGLE WGBEZ INTO IT_P3-WGBEZ FROM T023T WHERE SPRAS = SY-LANGU AND MATKL = IT_P3-MATKL. "物料组名称
          IT_P3-WAERS = GT_EKPO-WAERS."币种
          IT_P3-PEINH = GT_EKPO-PEINH."价格单位
          IT_P3-NETPR = GT_EKPO-NETPR. "不含税单价
          "   IT_P3-EKGRP = GT_EKPO-EKGRP."采购组
          IT_P3-AFNAM = GT_EKPO-AFNAM."申请者


          IF IT_P3-BWART NE '103'.
            READ TABLE GT_EKBE WITH KEY EBELN = IT_P3-EBELN EBELP = IT_P3-EBELP BELNR = TMPMELNR BUZEI = ZEILETMP.
            IF SY-SUBRC = 0 .
              IT_P3-NETWR = GT_EKBE-WRBTR  . "不含税金额
              IT_P3-DHRQ = GT_EKBE-BUDAT . "到货日期
              "  *获取税率
              REFRESH T_FTAXP.
              CLEAR   S_FTAXP.
              CALL FUNCTION 'GET_TAX_PERCENTAGE'
                EXPORTING
                  ALAND   = 'CN'
                  DATAB   = SY-DATUM
                  MWSKZ   = GT_EKPO-MWSKZ
                  TXJCD   = ''
*                 EXPORT  = ' '
                TABLES
                  T_FTAXP = T_FTAXP.
              "*行项目金额（含税）
              READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
              IF SY-SUBRC = 0.
                IT_P3-KWERT = IT_P3-NETWR + IT_P3-NETWR * S_FTAXP-KBETR / 1000. "含税金额
              ELSE.
                IT_P3-KWERT = IT_P3-NETWR .
              ENDIF.
            ENDIF.
          ELSE.
            IT_P3-BWART = '105'.
            IT_P3-MENGE = 0.
            READ TABLE GT_EKBE WITH KEY EBELN = IT_P3-EBELN EBELP = IT_P3-EBELP BELNR = TMPMELNR BUZEI = ZEILETMP.
            IF SY-SUBRC = 0.
              IT_P3-MBLNR2 = GT_EKBE-LFBNR. "参考凭证
              IT_P3-DHRQ = GT_EKBE-BUDAT . "到货日期
              "  *获取税率
              REFRESH T_FTAXP.
              CLEAR   S_FTAXP.
              CALL FUNCTION 'GET_TAX_PERCENTAGE'
                EXPORTING
                  ALAND   = 'CN'
                  DATAB   = SY-DATUM
                  MWSKZ   = GT_EKPO-MWSKZ
                  TXJCD   = ''
*                 EXPORT  = ' '
                TABLES
                  T_FTAXP = T_FTAXP.
            ENDIF.
            " 以下LOOP根据采购订单 、采购订单行号 103凭证号、行号作为参考凭证号 、行号为基准统计 103以下的105 、106 的数据
            LOOP AT GT_EKBE WHERE EBELN = IT_P3-EBELN AND EBELP = IT_P3-EBELP AND LFBNR = IT_P3-MBLNR AND LFPOS = IT_P3-ZEILE AND ( BWART = '105' OR BWART = '106')..
              IF GT_EKBE-BWART = '105'.
                WRITE GT_EKBE-BUDAT TO IT_P3-BUDAT2 .
                WRITE GT_EKBE-CPUTM TO IT_P3-CPUTM2 .
                CONCATENATE IT_P3-BUDAT2  ',' IT_P3-CPUTM2 INTO IT_P3-BUDAT_CPUTM.
                IT_P3-MENGE = IT_P3-MENGE + GT_EKBE-MENGE. " 数量
                IT_P3-NETWR = IT_P3-NETWR + GT_EKBE-WRBTR. " 不含税金额
                READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
                IF SY-SUBRC = 0.
                  IT_P3-KWERT =  IT_P3-KWERT + ( GT_EKBE-WRBTR + GT_EKBE-WRBTR * S_FTAXP-KBETR / 1000 ). "含税金额
                ELSE.
                  IT_P3-KWERT = IT_P3-KWERT +  GT_EKBE-WRBTR .
                ENDIF.
              ENDIF.
              IF GT_EKBE-BWART = '106'.
                IT_P3-MENGE = IT_P3-MENGE - GT_EKBE-MENGE." 数量
                IT_P3-NETWR = IT_P3-NETWR - GT_EKBE-WRBTR. "不含税金额
                READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
                IF SY-SUBRC = 0.
                  IT_P3-KWERT =  IT_P3-KWERT - ( GT_EKBE-WRBTR + GT_EKBE-WRBTR * S_FTAXP-KBETR / 1000 ). "含税金额
                ELSE.
                  IT_P3-KWERT = IT_P3-KWERT -  GT_EKBE-WRBTR .
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
          DATA:TMP TYPE STRING.
          DATA:KBETRTMP LIKE EKPO-MENGE.
          READ TABLE GT_KONV WITH KEY KNUMV = GT_EKPO-KNUMV KSCHL = 'Z010' KPOSN+1(5) = GT_EKPO-EBELP.
          IF SY-SUBRC = 0.
            KBETRTMP = GT_KONV-KBETR / 10 .
            TMP = KBETRTMP .
            "   IT_P3-SLV = GT_KONV-KBETR / 10 .
            " IF GT_KONV-KBETR / 10 NE  '0.00'.
            CONCATENATE TMP  '%' INTO IT_P3-SLV . "添加税率

          ENDIF.
          "      IT_P3-NETWR = GT_EKPO-NETWR ." * GT_EKPO-MENGE . "不含税金额
          READ TABLE GT_KONV WITH KEY KNUMV = GT_EKPO-KNUMV KSCHL = 'PBXX' KPOSN+1(5) = GT_EKPO-EBELP.
          IF SY-SUBRC = 0.
            IT_P3-KBETR = GT_KONV-KBETR ."含税单价
            "  IT_P3-KWERT =  IT_P3-KBETR * IT_P3-MENGE. "GT_KONV-KWERT. "含税金额
          ENDIF.
          READ TABLE GT_KONV WITH KEY KNUMV = GT_EKPO-KNUMV KSCHL = 'PB00' KPOSN+1(5) = GT_EKPO-EBELP.
          IF SY-SUBRC = 0.
            IT_P3-KBETR = GT_KONV-KBETR ."含税单价
            "   IT_P3-KWERT = GT_KONV-KWERT. "含税金额
          ENDIF.
        ENDIF.
        " 由read 改为 loop 汇总 采购订单 采购订单行号 物料凭证 物料凭证号 交货单号 汇总 所有发票凭证金额数据 IT02 150807
        LOOP AT GT_RSEG WHERE EBELN = IT_OUT-EBELN AND EBELP = IT_OUT-EBELP AND LFBNR = LFBNRTMP AND  LFPOS = ZEILETMP AND  XBLNR  = IT_OUT-XBLNR . .
          IF GT_RSEG-SHKZG = 'H'.
            GT_RSEG-WRBTR = GT_RSEG-WRBTR * -1.   "增加借贷标识 IT02 20160202
          ENDIF.
          IT_P3-WRBTR =  IT_P3-WRBTR + GT_RSEG-WRBTR ."不含税金额。
          IT_P3-BELNR =    GT_RSEG-BELNR."发票号
          "  *获取税率
          REFRESH T_FTAXP.
          CLEAR   S_FTAXP.
          CALL FUNCTION 'GET_TAX_PERCENTAGE'
            EXPORTING
              ALAND   = 'CN'
              DATAB   = SY-DATUM
              MWSKZ   = GT_RSEG-MWSKZ
              TXJCD   = ''
*             EXPORT  = ' '
            TABLES
              T_FTAXP = T_FTAXP.
          "*汇总发票行项目金额（含税）
          READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
          IF SY-SUBRC = 0.
            IT_P3-WRBTR_HS =  IT_P3-WRBTR_HS + ( GT_RSEG-WRBTR  + GT_RSEG-WRBTR * S_FTAXP-KBETR / 1000 ). "发票含税金额
          ELSE.
            IT_P3-WRBTR_HS = IT_P3-WRBTR_HS +  GT_RSEG-WRBTR .
          ENDIF.

        ENDLOOP.
        IT_P3-NETWR_1 = IT_P3-NETPR / IT_P3-PEINH  * IT_P3-MENGE .  ""不含税金额
        IT_P3-KWERT_1 = IT_P3-KBETR / IT_P3-PEINH * IT_P3-MENGE .  "含税金额


        IF IT_P3-BWART = '122' OR IT_P3-BWART = '161' OR IT_P3-BWART = '102'. "若为退库选项 数量 、不含税金额、含税金额、发票不含税金额、发票含税金额为负数
          IT_P3-MENGE = IT_P3-MENGE * -1.
          IT_P3-NETWR = IT_P3-NETWR * -1 .  "不含税金额（标准成本）
          IT_P3-KWERT = IT_P3-KWERT * -1.    "含税金额（标准成本）
          IT_P3-NETWR_1 = IT_P3-NETWR_1 * -1 .  ""不含税金额
          IT_P3-KWERT_1 = IT_P3-KWERT_1 * -1 .  "含税金额
          IT_P3-WRBTR = IT_P3-WRBTR * -1 .
          IT_P3-WRBTR_HS = IT_P3-WRBTR_HS * -1.
        ENDIF.
        IT_P3-WKPJE = IT_P3-NETWR - IT_P3-WRBTR. "未开票不含税金额（标准成本）
        IT_P3-WKPJE_HS = IT_P3-KWERT - IT_P3-WRBTR_HS . "未开票含税金额（标准成本）
        IT_P3-WKPJE_1 = IT_P3-NETWR_1 - IT_P3-WRBTR. "未开票不含税金额
        IT_P3-WKPJE_HS_1 = IT_P3-KWERT_1 - IT_P3-WRBTR_HS. "未开票含税金额
        IF IT_P3-BWART = '321' . "321 凭证行金额需 减掉 在QA32 中122 类型的 金额
          LOOP AT IT_122 WHERE EBELN = IT_P3-EBELN AND EBELP = IT_P3-EBELP AND   MBLNR2 = IT_P3-MBLNR2 AND BWART = '122' .
            READ TABLE GT_EKBE WITH KEY EBELN = IT_122-EBELN EBELP = IT_122-EBELP BELNR = IT_122-MBLNR BUZEI = IT_122-ZEILE.
            IF SY-SUBRC = 0 .
              IT_122-NETWR = GT_EKBE-WRBTR  . "不含税金额
              "  *获取税率
              REFRESH T_FTAXP.
              CLEAR   S_FTAXP.
              READ TABLE  GT_EKPO WITH KEY EBELN = IT_122-EBELN EBELP = IT_122-EBELP.
              IF SY-SUBRC = 0.
                CALL FUNCTION 'GET_TAX_PERCENTAGE'
                  EXPORTING
                    ALAND   = 'CN'
                    DATAB   = SY-DATUM
                    MWSKZ   = GT_EKPO-MWSKZ
                    TXJCD   = ''
*                   EXPORT  = ' '
                  TABLES
                    T_FTAXP = T_FTAXP.
              ENDIF.
              "*行项目金额（含税）
              READ TABLE T_FTAXP INTO S_FTAXP INDEX 1.
              IF SY-SUBRC = 0.
                IT_122-KWERT = IT_122-NETWR + IT_122-NETWR * S_FTAXP-KBETR / 1000. "含税金额
              ELSE.
                IT_122-KWERT = IT_122-NETWR .
              ENDIF.
            ENDIF.
            IT_P3-NETWR = IT_P3-NETWR - IT_122-NETWR .
            IT_P3-KWERT = IT_P3-KWERT - IT_122-KWERT .
            IT_P3-WKPJE = IT_P3-NETWR  - IT_P3-WRBTR.
            IT_P3-WKPJE_HS = IT_P3-KWERT - IT_P3-WRBTR_HS.
            IT_P3-NETWR_1 = IT_P3-NETWR_1 - IT_122-NETWR .
            IT_P3-KWERT_1 = IT_P3-KWERT_1 - IT_122-KWERT .
            IT_P3-WKPJE_1 = IT_P3-NETWR_1  - IT_P3-WRBTR.
            IT_P3-WKPJE_HS_1 = IT_P3-KWERT_1 - IT_P3-WRBTR_HS.
          ENDLOOP.
        ENDIF.
        APPEND IT_P3 .
      ENDLOOP.
      IF S_LIFNR[] IS NOT INITIAL.
        DELETE IT_P3 WHERE LIFNR NOT IN S_LIFNR.
      ENDIF.
      IF S_BELNR[] IS NOT INITIAL.
        DELETE IT_P3 WHERE BELNR NOT IN S_BELNR.
      ENDIF.
      IF S_XBLNR[] IS NOT INITIAL.
        DELETE IT_P3 WHERE XBLNR NOT IN S_XBLNR.
      ENDIF.
      IF YKP = 'X' AND WKP = ''.
        DELETE IT_P3 WHERE BELNR EQ ''.
      ENDIF.
      IF YKP = '' AND WKP = 'X'.
        DELETE IT_P3 WHERE BELNR NE ''.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DATA_RETRIVEL

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


  "  16  '' '' 'PRTFLAG'     text-016   , "是否打印
  "1  ''  'BUDAT' '入库日期（过账日期）'          , "入库日期（过账日期）'
  IF P_3 NE 'X'.
    PERFORM FRM_FILL_CAT USING :

     1  ''  'BUDAT_CPUTM' '入库日期（过账日期）' '' ''         , "入库日期（过账日期）'
     " 2 ''  'CPUTM' '时间'                  ,  "时间
      2  ''  'WERKS' '工厂'   '' ''               , "工厂'
      3  ''  'LGORT' '仓库'     'MSEG' 'LGORT'             , "仓库'
      4  ''  'BWART' '移动类型'    'MSEG' 'BWART'            , "移动类型'
      5  ''  'LIFNR' '供货单位'     'MSEG' 'LIFNR'           , "供货单位'
      6  ''  'LIFNAME' '供货单位名称'    'LFA1' 'NAME1'           , "供货单位名称
      7  ''  'MBLNR' '物料凭证'        'MSEG' 'MBLNR'       , "物料凭证
      8  ''  'MBLNR2' '收货原始物料凭证'   'MSEG' 'MBLNR2'            , "物料凭证
      9  ''  'XBLNR' '交货单'    'MKPF' 'XBLNR'           , "miro过账参考
      10 ''  'EBELN' '采购订单号'  'MSEG' 'EBELN'             , "采购订单号'
      11 ''  'EBELP' '行项目'       'MSEG' 'EBELP'          , "行项目'
      12 ''  'MATNR' '物料编码'   'MSEG' 'MATNR'             , "物料编码'
      13 ''  'MAKTX' '规格描述'     'MAKT' 'MAKTX'          , "规格描述'
      14 ''  'MENGE' '数量'       'MSEG' 'MENGE'           , "数量'
      15 ''  'MEINS' '单位'      'MSEG' 'MEINS'            , "单位'
      16 ''  'CHARG' '批次'      'MSEG' 'CHARG'            . "批次'
  ENDIF.
  IF P_3 = 'X'.
    PERFORM FRM_FILL_CAT USING :
           1  ''  'BUDAT2' '入库日期' '' ''         ,
           1  ''  'BUDAT_CPUTM' '入库日期（时间）' '' ''         , "入库日期（过账日期）'
          " 2 ''  'CPUTM' '时间'                  ,  "时间
           2  ''  'WERKS' '工厂'   '' ''               , "工厂'
           3  ''  'LGORT' '仓库'     'MSEG' 'LGORT'             , "仓库'
           4  ''  'BWART' '移动类型'    'MSEG' 'BWART'            , "移动类型'
           5  ''  'LIFNR' '供货单位'     'MSEG' 'LIFNR'           , "供货单位'
           6  ''  'LIFNAME' '供货单位名称'    'LFA1' 'NAME1'           , "供货单位名称
           7  ''  'MBLNR' '物料凭证'        'MSEG' 'MBLNR'       , "物料凭证
           8  ''  'ZEILE' '物料凭证项目号'        'MSEG' 'ZEILE'       ,"物料凭证项目号
           8  ''  'MBLNR2' '收货原始物料凭证'   'MSEG' 'MBLNR2'            , "物料凭证
           8  ''  'ZEILE2' '收货原始物料凭证项目号'   '' ''            , "物料凭证
           9  ''  'XBLNR' '交货单'    'MKPF' 'XBLNR'           , "miro过账参考
           10 ''  'EBELN' '采购订单号'  'MSEG' 'EBELN'             , "采购订单号'
           11 ''  'EBELP' '行项目'       'MSEG' 'EBELP'          , "行项目'
           12 ''  'MATNR' '物料编码'   'MSEG' 'MATNR'             , "物料编码'
           13 ''  'MAKTX' '规格描述'     'MAKT' 'MAKTX'          , "规格描述'
           14 ''  'MENGE' '数量'       'MSEG' 'MENGE'           , "数量'
           15 ''  'MEINS' '单位'      'MSEG' 'MEINS'            , "单位'
           16 ''  'CHARG' '批次'      'MSEG' 'CHARG'            , "批次'
           17 ''  'MATKL' '物料组'      'EKPO' 'MATKL'            , "物料组'
           18 ''  'WGBEZ' '物料组名称'      'T023T' 'WGBEZ'            , "物料组'
           19 ''  'NETPR' '不含税单价'       '' ''          , "不含税单价
           20 ''  'NETWR_1' '不含税金额'    '' ''             , "不含税金额
           21 ''  'NETWR' '不含税金额(由标准价格计算)'    '' ''             , "不含税金额
           22 ''  'KBETR' '含税单价'     '' ''            , "含税单价
           23 ''  'KWERT_1' '含税金额'    '' ''             , "含税金额
           24 ''  'KWERT' '含税金额(由标准价格计算)'    '' ''             , "含税金额
           25 ''  'WRBTR' '发票不含税金额'   '' ''                , "发票不含税金额
           26 '' 'WRBTR_HS' '发票含税金额'  '' ''      ,"发票含税金额
            27 '' 'WKPJE_1' '未开票不含税金额' '' '' ,    "未开票不含税金额
           28 '' 'WKPJE' '未开票不含税金额(由标准价格计算)' '' '' ,    "未开票不含税金额
           29 '' 'WKPJE_HS_1' '未开票含税金额' '' '',"未开票含税金额
            30 '' 'WKPJE_HS' '未开票含税金额(由标准价格计算)' '' '',"未开票含税金额
           31 '' 'BELNR' '采购发票号' 'MSEG' 'BELNR',"采购发票号
           32 '' 'WAERS'  '货币' 'EKKO' 'WAERS', "
           33 '' 'SLV' '税率' '' '',
           34 '' 'PEINH' '价格单位' 'EKPO' 'PEINH',
           35 '' 'DHRQ' '到货日期' 'EKBE' 'DHQR',
           36 '' 'EKGRP' '采购组' 'EKKO' 'EKGRP',
           37 '' 'EKNAM' '采购组名' 'T024' 'EKNAM',
           38 '' 'AFNAM' '申请者' 'EKPO' 'AFNAM'.

  ENDIF.


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
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME U_TABLE U_FIELD.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  IF U_FNAME = 'EBELN' OR U_FNAME = 'BELNR' .
    LW_FIELDCAT-HOTSPOT = 'X'.
  ENDIF.
  IF P_3 = 'X'.
    IF U_FNAME = 'NETPR' OR U_FNAME = 'KBETR' OR U_FNAME = 'KWERT'  OR U_FNAME = 'WRBTR' OR U_FNAME = 'WRBTR_HS' OR U_FNAME = 'WKPJE'  OR U_FNAME = 'WKPJE_HS' .
      LW_FIELDCAT-CFIELDNAME = 'WAERS'.
    ENDIF.
  ENDIF.
  IF U_TABLE NE ''.
    LW_FIELDCAT-REF_TABNAME = U_TABLE.
  ENDIF.
  IF U_FIELD  NE ''.
    LW_FIELDCAT-REF_FIELDNAME = U_FIELD .
  ENDIF.

  "lw_fieldcat-no_zero   = 'X'.
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
  L_LAYOUT-BOX_FIELDNAME = 'ZBOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.
  IF P_3 <> 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM       = SY-REPID
        I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
        I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
        I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
        IT_FIELDCAT              = IT_FIELDCAT[]
        I_SAVE                   = 'X'
        I_GRID_SETTINGS          = L_GRID_SETTINGS
        IS_LAYOUT                = L_LAYOUT
        IS_VARIANT               = G_VARIANT
      TABLES
        T_OUTTAB                 = IT_OUT
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM       = SY-REPID
        I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
        I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
        I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
        IT_FIELDCAT              = IT_FIELDCAT[]
        I_SAVE                   = 'A'
        I_GRID_SETTINGS          = L_GRID_SETTINGS
        IS_LAYOUT                = L_LAYOUT
        IS_VARIANT               = G_VARIANT
      TABLES
        T_OUTTAB                 = IT_P3
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
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
  WA_HEADER-INFO =  SY-TITLE."'采购入库单.
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
    WHEN '&PRNT'.
      PERFORM FRM_PRINT_DATA.

    WHEN OTHERS.
  ENDCASE.
  "IT02 ADD 根据采购订单可双击跳转到ME23N BEGIN 150619
  CASE R_UCOMM .
    WHEN '&IC1'.
      "获取当前ALV所在行数据
      IF P_1 = 'X' OR P_2 = 'X'.
        READ TABLE IT_OUT INDEX RS_SELFIELD-TABINDEX.
        IF SY-SUBRC = 0.
          SET PARAMETER ID: 'BES' FIELD IT_OUT-EBELN.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
      IF P_3 = 'X' .
        IF RS_SELFIELD-FIELDNAME = 'EBELN'.
          READ TABLE IT_P3 INDEX RS_SELFIELD-TABINDEX.
          IF SY-SUBRC = 0.
            SET PARAMETER ID: 'BES' FIELD IT_P3-EBELN.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
        IF RS_SELFIELD-FIELDNAME = 'BELNR'.
          READ TABLE IT_P3 INDEX RS_SELFIELD-TABINDEX.
          IF SY-SUBRC = 0.
            SET PARAMETER ID: 'RBN' FIELD IT_P3-BELNR.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
  "IT02 ADD 根据采购订单可双击跳转到ME23N END 150619
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
  SET PF-STATUS 'ZMM016_STATUS'.
ENDFORM.                    "set_pf_status


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 5,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.

  DATA:L_FORMNAME TYPE TDSFNAME . "没批次，有备注

*&--代码添加 BY HANDYBY 05.07.2017 16:49:20  BEGIN
  IF S_WERKS-LOW EQ '1700' OR S_WERKS-LOW EQ '1710' OR S_WERKS-LOW EQ '1720' .
    L_FORMNAME = 'ZSFMM016_02'. "没批次，有备注
  ELSE.
    L_FORMNAME = 'ZSFMM016_01'.  "有批次，没备注(原来的)
  ENDIF.
*&--代码添加 BY HANDYBY 05.07.2017 16:49:20  END

  DATA:LT_SELECT LIKE  IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA:LW_SELECT LIKE LINE OF LT_SELECT.
  DATA:LT_PRT LIKE TABLE OF IT_OUT WITH HEADER LINE.
  DATA:L_TITLE TYPE STRING.

  IF P_1 = 'X'.
    L_TITLE = '采购入库单'.
  ENDIF.

  IF P_2 = 'X'.
    L_TITLE = '采购退货单'.
  ENDIF.

  IF P_3 = 'X'.
    L_TITLE = '采购入库退货单'.
  ENDIF.

  FIELD-SYMBOLS <LW_PRT> LIKE LINE OF IT_OUT.
  IF P_1 = 'X' OR P_2 = 'X'.
    LT_SELECT[] = IT_OUT[].

    CLEAR:LT_PRT[].
    LOOP AT LT_SELECT INTO LW_SELECT WHERE ZBOX = 'X'.
      MOVE-CORRESPONDING LW_SELECT TO LT_PRT.
      APPEND LT_PRT.
    ENDLOOP.
  ELSE.
    CLEAR:LT_PRT[].
    LOOP AT IT_P3  WHERE ZBOX = 'X'.
      MOVE-CORRESPONDING IT_P3 TO LT_PRT.
      APPEND LT_PRT.
    ENDLOOP.
  ENDIF.




  IF LT_PRT[] IS INITIAL.
    MESSAGE S001(Z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK LT_PRT[] IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

*  SORT lt_prt BY BUDAT werks lgort lifnr ebeln  ebelp.
  SORT LT_PRT BY WERKS LGORT LIFNR.

  LOOP AT LT_PRT ASSIGNING <LW_PRT>.
    AT NEW LIFNR.
      CLEAR WA_PRT.
      CLEAR IT_PRT[].
      WA_PRT = <LW_PRT>.
    ENDAT.
    APPEND <LW_PRT> TO IT_PRT[].

    AT END OF LIFNR.
      DESCRIBE TABLE IT_PRT LINES NTOTALLINE.
      NCURRLINE = NTOTALLINE MOD NPAGELINE.
      IF  NCURRLINE > 0.
        EMPTYCOUNT = NPAGELINE - NCURRLINE.
        DO EMPTYCOUNT TIMES.
          APPEND INITIAL LINE TO IT_PRT.
        ENDDO.
      ENDIF.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          NPAGE_LINE         = NPAGELINE
          TITLE              = L_TITLE
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.

  ENDIF.

ENDFORM. "frm_print_data


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
