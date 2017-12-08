*&---------------------------------------------------------------------*
*& Report  ZMM010
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/02
*& Request       :
*& Descriptions  : MRP批量运行

*&
*& Modify by     : IT02
*& Modify date   :20160512
*& Request       :增加2200（黑金）公司的MPR需求
*& Descriptions  :2016000
*& Modify:20160629 it02 增加2100，2110 工厂的MRP需求
*&---------------------------------------------------------------------*
REPORT ZPP010.
************************************************************************
* Tables
************************************************************************
TABLES: EKKO,VBAK,EKPO.

************************************************************************
* TYPES
************************************************************************
TYPES:BEGIN OF TY_VBAP.
        INCLUDE STRUCTURE VBAP.
TYPES:MTART TYPE MARA-MTART,
      END OF TY_VBAP.

************************************************************************
* Internal Table * WorkArea
************************************************************************

DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

" Global variable
DATA: G_MSGTXT(200).

DATA GT_VBAP TYPE TABLE OF TY_VBAP.
DATA GS_VBAP TYPE VBAP.

DATA GT_EKPO TYPE TABLE OF EKPO.
DATA GS_EKPO TYPE EKPO.

DATA GT_MARC TYPE TABLE OF MARC.
DATA GS_MARC TYPE MARC.
************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS P_WERKS TYPE EKPO-WERKS OBLIGATORY MEMORY ID ZPP010.
SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN MODIF ID Z1 ,
                S_EBELN FOR EKKO-EBELN MODIF ID Z2 ,
                S_MATNR FOR EKPO-MATNR.
PARAMETERS G1 TYPE CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND G_UCMD.
PARAMETERS G2 TYPE CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'Z1'.
      IF G1 = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'Z2'.
      IF G2 = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查
  PERFORM FRM_AUTH_CHECK.

*1000,1310工厂MRP执行

*&--代码注释 BY HANDYBY 27.05.2017 16:06:29  BEGIN
*    IF P_WERKS = '1000'     OR P_WERKS = '1310' OR P_WERKS = '1200'
*      OR P_WERKS = '1300' OR P_WERKS = '2200' OR P_WERKS = '2100'.
*&--代码注释 BY HANDYBY 27.05.2017 16:06:29  END
*&--代码添加 BY HANDYBY 27.05.2017 16:06:41  BEGIN
  IF P_WERKS = '1000'     OR P_WERKS = '1310' OR P_WERKS = '1200'
      OR P_WERKS = '1300' OR P_WERKS = '2200' OR P_WERKS = '2100'
    OR P_WERKS = '1600' OR P_WERKS = '160Y'
    OR P_WERKS = '3300' OR P_WERKS = '1360'.
*&--代码添加 BY HANDYBY 27.05.2017 16:06:41  END

    IF G1 = 'X' .
      PERFORM FRM_GET_DATA. "取数逻辑
      PERFORM FRM_DEAL_DATA."处理逻辑
    ELSE.
      PERFORM FRM_GET_DATA_1. "取数逻辑
      PERFORM FRM_DEAL_DATA_1."处理逻辑
    ENDIF.
*1100工厂MRP执行

*&--代码注释 BY HANDYBY 12.05.2017 14:13:53  BEGIN
*    ELSEIF P_WERKS = '1100' OR P_WERKS = '1500' OR P_WERKS = '2110'.
*&--代码注释 BY HANDYBY 12.05.2017 14:13:53  END

*&--代码添加 BY HANDYBY 12.05.2017 14:14:08  BEGIN
  ELSEIF P_WERKS = '1100' OR P_WERKS = '1500' OR P_WERKS = '2110'
    OR P_WERKS = '1610' OR P_WERKS = '1700' OR P_WERKS = '2900'
    OR P_WERKS = '2910' .
*&--代码添加 BY HANDYBY 12.05.2017 14:14:08  END

    IF G1 = 'X' .
      PERFORM FRM_GET_DATA_2. "取数逻辑
      PERFORM FRM_DEAL_DATA_2."处理逻辑
    ELSE.
      PERFORM FRM_GET_DATA_3. "取数逻辑
      PERFORM FRM_DEAL_DATA_3."处理逻辑
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  IF S_VBELN[] IS INITIAL.
    MESSAGE '请输入销售订单' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK S_VBELN[] IS NOT INITIAL.

*排除虚拟件
  SELECT * FROM VBAP
    INNER JOIN MARA ON
    VBAP~MATNR = MARA~MATNR
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    WHERE VBELN IN S_VBELN
    AND   VBAP~MATNR IN S_MATNR
    AND   MTART <> 'ZXNJ'.

  IF GT_VBAP IS INITIAL.
    MESSAGE S004(Z001) DISPLAY LIKE 'E'.
  ENDIF.
  CHECK GT_VBAP IS NOT INITIAL.

  SELECT * FROM MARC
   INTO CORRESPONDING FIELDS OF TABLE GT_MARC
   FOR ALL ENTRIES IN GT_VBAP
   WHERE MATNR = GT_VBAP-MATNR
   AND   WERKS = P_WERKS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_1 .

  IF S_EBELN[] IS INITIAL.
    MESSAGE '请输入采购订单' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK S_EBELN[] IS NOT INITIAL.

*排除虚拟件
  SELECT * FROM EKPO
    INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
    WHERE EBELN IN S_EBELN
    AND   MATNR IN S_MATNR
    AND   MTART <> 'ZXNJ'.
  IF GT_EKPO IS INITIAL.
    MESSAGE S004(Z001) DISPLAY LIKE 'E'.
  ENDIF.

  CHECK GT_EKPO IS NOT INITIAL.

  SELECT * FROM MARC
  INTO CORRESPONDING FIELDS OF TABLE GT_MARC
  FOR ALL ENTRIES IN GT_EKPO
  WHERE MATNR = GT_EKPO-MATNR
  AND   WERKS = P_WERKS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_2 .

  IF S_VBELN[] IS INITIAL.
    MESSAGE '请输入销售订单' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK S_VBELN[] IS NOT INITIAL.

*排除虚拟件
  SELECT * FROM VBAP
    INNER JOIN MARA ON
    VBAP~MATNR = MARA~MATNR
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    WHERE VBELN IN S_VBELN
    AND   VBAP~MATNR IN S_MATNR
    AND   MTART <> 'ZXNJ'.

  IF GT_VBAP IS INITIAL.
    MESSAGE S004(Z001) DISPLAY LIKE 'E'.
  ENDIF.
  CHECK GT_VBAP IS NOT INITIAL.

  SELECT * FROM MARC
   INTO CORRESPONDING FIELDS OF TABLE GT_MARC
   FOR ALL ENTRIES IN GT_VBAP
   WHERE MATNR = GT_VBAP-MATNR
   AND   WERKS = P_WERKS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_3 .

  IF S_EBELN[] IS INITIAL.
    MESSAGE '请输入采购订单' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK S_EBELN[] IS NOT INITIAL.

*排除虚拟件
  SELECT * FROM EKPO
    INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
    WHERE EBELN IN S_EBELN
    AND   MATNR IN S_MATNR
    AND   MTART <> 'ZXNJ'.
  IF GT_EKPO IS INITIAL.
    MESSAGE S004(Z001) DISPLAY LIKE 'E'.
  ENDIF.

  IF GT_EKPO IS INITIAL.
    MESSAGE S004(Z001) DISPLAY LIKE 'E'.
  ENDIF.

  CHECK GT_EKPO IS NOT INITIAL.

  SELECT * FROM MARC
  INTO CORRESPONDING FIELDS OF TABLE GT_MARC
  FOR ALL ENTRIES IN GT_EKPO
  WHERE MATNR = GT_EKPO-MATNR
  AND   WERKS = P_WERKS.
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
  LOOP AT GT_VBAP INTO GS_VBAP.
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_VBAP-MATNR.
    IF SY-SUBRC = 0.
*MRP类型 = MO时，执行MD41
      IF GS_MARC-DISMM = 'M0'.
        PERFORM FRM_PREPARE_BDC CHANGING GS_MARC-MATNR.
      ELSE.
*MRP类型 = PD时，执行MD02
        PERFORM FRM_PREPARE_BDC_1 CHANGING GS_MARC-MATNR.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA_2 .
  LOOP AT GT_VBAP INTO GS_VBAP.
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_VBAP-MATNR.
    IF SY-SUBRC = 0.
*MRP类型 = MO时，执行MD41
      IF GS_MARC-DISMM = 'M0'.
        PERFORM FRM_PREPARE_BDC_2 CHANGING GS_MARC-MATNR.
      ELSE.
*MRP类型 = PD时，执行MD02
        PERFORM FRM_PREPARE_BDC_3 CHANGING GS_MARC-MATNR.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA_1 .
  LOOP AT GT_EKPO INTO GS_EKPO.
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_EKPO-MATNR.
    IF SY-SUBRC = 0.
*MRP类型 = MO时，执行MD41
      IF GS_MARC-DISMM = 'M0'.
        PERFORM FRM_PREPARE_BDC CHANGING GS_MARC-MATNR.
      ELSE.
*MRP类型 = PD时，执行MD02
        PERFORM FRM_PREPARE_BDC_1 CHANGING GS_MARC-MATNR.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA_3 .
  LOOP AT GT_EKPO INTO GS_EKPO.
    READ TABLE GT_MARC INTO GS_MARC
    WITH KEY MATNR = GS_EKPO-MATNR.
    IF SY-SUBRC = 0.
*MRP类型 = MO时，执行MD41
      IF GS_MARC-DISMM = 'M0'.
        PERFORM FRM_PREPARE_BDC_2 CHANGING GS_MARC-MATNR.
      ELSE.
*MRP类型 = PD时，执行MD02
        PERFORM FRM_PREPARE_BDC_3 CHANGING GS_MARC-MATNR.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PREPARE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PREPARE_BDC CHANGING L_MATNR TYPE MATNR .

  PERFORM BDC_DYNPRO      USING 'SAPMM61X' '0150'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM61X-ANZLS'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RM61X-MATNR'
                                 L_MATNR.
  PERFORM BDC_FIELD       USING 'RM61X-WERKS'
                                 P_WERKS.
  PERFORM BDC_FIELD       USING 'RM61X-VERSL'
                                'NETCH'.
  PERFORM BDC_FIELD       USING 'RM61X-BANER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-LIFKZ'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-DISER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-PLMOD'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-TRMPL'
                                '1'.
*  PERFORM BDC_FIELD       USING 'RM61X-PLALL'
*                                'X'.
  PERFORM BDC_FIELD       USING 'RM61X-ANZLS'
                                 'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=XBAC'.

  IF BDCDATA[] IS NOT INITIAL.
    CALL TRANSACTION 'MD41' USING BDCDATA
                             MODE 'N'  " 'A'
                             UPDATE 'S'
                             MESSAGES INTO MESSTAB.
    LOOP AT MESSTAB.
      MESSAGE ID MESSTAB-MSGID
              TYPE MESSTAB-MSGTYP
              NUMBER MESSTAB-MSGNR
              INTO G_MSGTXT
              WITH MESSTAB-MSGV1
                   MESSTAB-MSGV2
                   MESSTAB-MSGV3
                   MESSTAB-MSGV4.
      WRITE: / G_MSGTXT.
    ENDLOOP.
    IF MESSTAB[] IS INITIAL.
      MESSAGE S005(Z001).
    ENDIF.

    REFRESH BDCDATA[].
    REFRESH MESSTAB[].
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PREPARE_BDC_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_MARC_MATNR  text
*----------------------------------------------------------------------*
FORM FRM_PREPARE_BDC_1  CHANGING L_MATNR TYPE MARC-MATNR.

  PERFORM BDC_DYNPRO      USING 'SAPMM61X' '0150'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM61X-ANZLS'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RM61X-MATNR'
                                 L_MATNR.
  PERFORM BDC_FIELD       USING 'RM61X-WERKS'
                                P_WERKS.
  PERFORM BDC_FIELD       USING 'RM61X-VERSL'
                                'NETCH'.
  PERFORM BDC_FIELD       USING 'RM61X-BANER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-LIFKZ'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-DISER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-PLMOD'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-TRMPL'
                                '1'.
*  PERFORM BDC_FIELD       USING 'RM61X-PLALL'
*                                'X'.
  PERFORM BDC_FIELD       USING 'RM61X-ANZLS'
                                 'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=XBAC'.
  IF BDCDATA[] IS NOT INITIAL.
    CALL TRANSACTION 'MD02' USING BDCDATA
                             MODE 'N'
                             UPDATE 'S'
                             MESSAGES INTO MESSTAB.
    LOOP AT MESSTAB .
      MESSAGE ID MESSTAB-MSGID
              TYPE MESSTAB-MSGTYP
              NUMBER MESSTAB-MSGNR
              INTO G_MSGTXT
              WITH MESSTAB-MSGV1
                   MESSTAB-MSGV2
                   MESSTAB-MSGV3
                   MESSTAB-MSGV4.
      WRITE: / G_MSGTXT.
    ENDLOOP.

    IF MESSTAB[] IS INITIAL.
      MESSAGE S005(Z001).
    ENDIF.

    REFRESH BDCDATA[].
    REFRESH MESSTAB[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PREPARE_BDC_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_MARC_MATNR  text
*----------------------------------------------------------------------*
FORM FRM_PREPARE_BDC_2  CHANGING  L_MATNR TYPE MARC-MATNR.
  PERFORM BDC_DYNPRO      USING 'SAPMM61X' '0150'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM61X-ANZLS'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RM61X-MATNR'
                                 L_MATNR.
  PERFORM BDC_FIELD       USING 'RM61X-WERKS'
                                P_WERKS.
  PERFORM BDC_FIELD       USING 'RM61X-VERSL'
                                'NETCH'.
  PERFORM BDC_FIELD       USING 'RM61X-BANER'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-LIFKZ'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-DISER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-PLMOD'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-TRMPL'
                                '1'.
*  PERFORM BDC_FIELD       USING 'RM61X-PLALL'
*                                'X'.
  PERFORM BDC_FIELD       USING 'RM61X-ANZLS'
                                 'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=XBAC'.

  IF BDCDATA[] IS NOT INITIAL.
    CALL TRANSACTION 'MD41' USING BDCDATA
                             MODE 'N'
                             UPDATE 'S'
                             MESSAGES INTO MESSTAB.
    LOOP AT MESSTAB.
      MESSAGE ID MESSTAB-MSGID
              TYPE MESSTAB-MSGTYP
              NUMBER MESSTAB-MSGNR
              INTO G_MSGTXT
              WITH MESSTAB-MSGV1
                   MESSTAB-MSGV2
                   MESSTAB-MSGV3
                   MESSTAB-MSGV4.
      WRITE: / G_MSGTXT.
    ENDLOOP.

    IF MESSTAB[] IS INITIAL.
      MESSAGE S005(Z001).
    ENDIF.

    REFRESH BDCDATA[].
    REFRESH MESSTAB[].
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PREPARE_BDC_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_MARC_MATNR  text
*----------------------------------------------------------------------*
FORM FRM_PREPARE_BDC_3  CHANGING  L_MATNR TYPE MARC-MATNR.

  PERFORM BDC_DYNPRO      USING 'SAPMM61X' '0150'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM61X-ANZLS'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RM61X-MATNR'
                                 L_MATNR.
  PERFORM BDC_FIELD       USING 'RM61X-WERKS'
                                P_WERKS.
  PERFORM BDC_FIELD       USING 'RM61X-VERSL'
                                'NETCH'.
  PERFORM BDC_FIELD       USING 'RM61X-BANER'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-LIFKZ'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-DISER'
                                '1'.
  PERFORM BDC_FIELD       USING 'RM61X-PLMOD'
                                '3'.
  PERFORM BDC_FIELD       USING 'RM61X-TRMPL'
                                '1'.
*  PERFORM BDC_FIELD       USING 'RM61X-PLALL'
*                                'X'.
  PERFORM BDC_FIELD       USING 'RM61X-ANZLS'
                                 'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=XBAC'.
  IF BDCDATA[] IS NOT INITIAL.
    CALL TRANSACTION 'MD02' USING BDCDATA
                             MODE 'N'
                             UPDATE 'S'
                             MESSAGES INTO MESSTAB.
    LOOP AT MESSTAB .
      MESSAGE ID MESSTAB-MSGID
              TYPE MESSTAB-MSGTYP
              NUMBER MESSTAB-MSGNR
              INTO G_MSGTXT
              WITH MESSTAB-MSGV1
                   MESSTAB-MSGV2
                   MESSTAB-MSGV3
                   MESSTAB-MSGV4.
      WRITE: / G_MSGTXT.
    ENDLOOP.

    IF MESSTAB[] IS INITIAL.
      MESSAGE S005(Z001).
    ENDIF.

    REFRESH BDCDATA[].
    REFRESH MESSTAB[].
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
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
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  P_WERKS.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH P_WERKS.
  ENDIF.
ENDFORM.
