*----------------------------------------------------------------------------
*模块	      SD
*请求类型   PROG:ZSD019
*内容描述   上海蓝硕奕硕出入库单打印
*版本       V1.0
*姓名       HANDZFF
*日期       2017.05.26 11：00:00
*-----------------------------------------------------------------------------
*& 摘要：
*&       程序复制于ZSD003
REPORT ZSD019.

TABLES : LIKP,LIPS,EKKO.
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

DATA:BEGIN OF IT_OUT OCCURS 0,
       VGBEL       TYPE LIPS-VGBEL    , "项目编号
       VBELN       TYPE LIKP-VBELN    , "交货单编号
       EBELN       TYPE EKKO-EBELN    , "采购订单号
       WADAT       TYPE LIKP-WADAT    , "交货日期
       LFART       TYPE LIKP-LFART    , "交货单类型
       VTEXT1      TYPE TVLKT-VTEXT   , "交货单类型描述
       VSTEL       TYPE LIKP-VSTEL    , "装运点
       VTEXT2      TYPE TVSTT-VTEXT   , "装运点描述
       VKORG       TYPE LIKP-VKORG    , "销售组织
       VTEXT3      TYPE TVKOT-VTEXT   , "销售组织描述
       POSNR       TYPE LIPS-POSNR    , "行项目编号
       ZLTEXT(255) TYPE C             , "项目描述         文本类型=‘Z001'   ,
       ZSHR        TYPE ZTSD003-ZSHR  , "收货人
       ZDH         TYPE ZTSD003-ZDH   , "电话
       ZDZ         TYPE ZTSD003-ZDZ   , "地址
       ZYSFS       TYPE ZTSD003-ZYSFS , "运输方式
       ZZJS        TYPE ZTSD003-ZZJS  , "总件数
       ZXS         TYPE ZTSD003-ZXS   , "箱数         "appended by it02 20160802
       ZFHR        TYPE ZTSD003-ZFHR  , "发货人
       ZJH         TYPE ZTSD003-ZJH   , "件号
       MATNR       TYPE LIPS-MATNR    , "物料编码
       ARKTX       TYPE LIPS-ARKTX    , "货物名称
       LFIMG       TYPE LIPS-LFIMG    , "数量
       MEINS       TYPE LIPS-MEINS    , "单位
       ZBZ         TYPE ZTSD003-ZBZ   , "备注
       KDMAT       TYPE LIPS-KDMAT    , "类别
       ZDY         TYPE ZTSD003-ZDY   , "是否打印
       ZBOX,
     END OF IT_OUT.

TYPES:BEGIN OF TY_PRINT,
        VBELN       TYPE LIKP-VBELN    , "交货单编号
        XH          TYPE I             , "序号
        VGBEL       TYPE LIPS-VGBEL    , "项目编号
        ZLTEXT(255) TYPE C             , "项目描述         文本类型=‘Z001'   ,
        ZDZ         TYPE ZTSD003-ZDZ   , "地址
        ZSHR        TYPE ZTSD003-ZSHR  , "收货人
        ZDH         TYPE ZTSD003-ZDH   , "电话
        WADAT       TYPE LIKP-WADAT    , "交货日期
        MATNR       TYPE LIPS-MATNR    , "成品编码
        ARKTX       TYPE LIPS-ARKTX    , "货物名称
        LFIMG       TYPE LIPS-LFIMG    , "数量
        ZXS         TYPE ZTSD003-ZXS   , "箱数
        ZYSFS       TYPE ZTSD003-ZYSFS , "运输方式
        ZBZ         TYPE ZTSD003-ZBZ   , "备注
      END OF TY_PRINT .

DATA:GT_PRINT TYPE TABLE OF TY_PRINT,
     GS_PRINT TYPE TY_PRINT.

DATA:GT_PRINT_2 TYPE TABLE OF TY_PRINT,
     GS_PRINT_2 TYPE TY_PRINT.


DATA:G_XH  TYPE I .     "全局序号


DATA:GT_PRINT_PR TYPE TABLE OF TY_PRINT,
     GS_PRINT_PR TYPE TY_PRINT.

DATA:IT_PRT LIKE TABLE OF IT_OUT WITH HEADER LINE.
DATA:IT_PRT2 LIKE TABLE OF IT_OUT WITH HEADER LINE.
DATA:IT_VBKD LIKE TABLE OF VBKD WITH  HEADER LINE.
DATA: LW_ZTSD003 TYPE ZTSD003,
      LT_ZTSD003 LIKE TABLE OF LW_ZTSD003,
      LW_LIKP    TYPE LIKP,
      LT_LIKP    LIKE TABLE OF LW_LIKP,

      LW_LIPS    TYPE LIPS,
      LT_LIPS    LIKE TABLE OF LW_LIPS WITH HEADER LINE,

      LW_EKKO    TYPE EKKO,
      LT_EKKO    LIKE TABLE OF LW_EKKO,

      LW_EKKN    TYPE EKKN,
      LT_EKKN    LIKE TABLE OF LW_EKKN,

      LW_TVLKT   TYPE TVLKT,
      LT_TVLKT   LIKE TABLE OF LW_TVLKT,
      LW_TVSTT   TYPE TVSTT,
      LT_TVSTT   LIKE TABLE OF LW_TVSTT,
      LW_TVKOT   TYPE TVKOT,
      LT_TVKOT   LIKE TABLE OF LW_TVKOT.

DATA: BEGIN OF LT_VBAP OCCURS 0,
        VBELN TYPE VBELN_VL,
        POSNR TYPE POSNR_VL,
        KDMAT TYPE MATNR_KU,
      END OF LT_VBAP.

DATA: BEGIN OF LT_LIPS2 OCCURS 0,
        "PSTYV TYPE PSTYV ,"150730 IT02 add
        VBELN TYPE VBELN_VL,
        POSNR TYPE POSNR_VL,
        VGBEL TYPE VGBEL,
        VGPOS TYPE EBELP,
      END OF LT_LIPS2.

DATA:G_INDEX TYPE I .

DATA: BEGIN OF LW_VBAK,
        VBELN TYPE VBELN_VA,
      END OF LW_VBAK,
      LT_VBAK LIKE TABLE OF LW_VBAK.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-025 .
SELECT-OPTIONS: S_VSTEL  FOR LIKP-VSTEL,
                S_VKORG  FOR LIKP-VKORG OBLIGATORY,
                S_VBELN  FOR LIKP-VBELN MATCHCODE OBJECT VMVL,
                S_WADAT  FOR LIKP-WADAT,
                S_LFART  FOR LIKP-LFART,
                S_VGBEL  FOR LIPS-VGBEL MATCHCODE OBJECT VMVA,
                S_EBELN  FOR EKKO-EBELN MATCHCODE OBJECT MEKK_C.
PARAMETERS :    P_SEL1 TYPE C AS CHECKBOX,
                P_SEL2 TYPE C AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.


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
  PERFORM DATA_RETRIVEL.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.


FORM FRM_AUTH_CHECK.

  DATA:LT_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
  SELECT VKORG FROM TVKO
  INTO CORRESPONDING FIELDS OF TABLE LT_TVKO
  WHERE VKORG IN S_VKORG
    .
  LOOP AT LT_TVKO WHERE VKORG IN S_VKORG.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
             ID 'VKORG' FIELD LT_TVKO-VKORG
             .
    IF SY-SUBRC <> 0.
      MESSAGE E430(VELO) WITH LT_TVKO-VKORG.
    ENDIF.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.08.2017 17:05:36  BEGIN
  LOOP AT S_VKORG .
    AUTHORITY-CHECK OBJECT 'Z_VKORG'
             ID 'ZVKORG' FIELD S_VKORG-LOW .
    IF SY-SUBRC <> 0.
      MESSAGE E430(VELO) WITH S_VKORG-LOW.
    ENDIF.
  ENDLOOP.
*&--代码添加 BY HANDYBY 23.08.2017 17:05:36  END

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


  RANGES L_PR_FLAG FOR ZTSD003-ZDY.

  "取交货单头
  SELECT  VBELN
          WADAT
          LFART
          VSTEL
          VKORG
    FROM  LIKP
    INTO CORRESPONDING FIELDS OF TABLE LT_LIKP
    WHERE VSTEL IN S_VSTEL
    AND   VKORG IN S_VKORG
    AND   VBELN IN S_VBELN
    AND   WADAT IN S_WADAT
    AND   LFART IN S_LFART
*    AND   lfart <> 'ZLDR'
*    AND   lfart <> 'RL'
    .
  IF LT_LIKP[] IS NOT INITIAL.
    SELECT * FROM TVLKT
      INTO TABLE LT_TVLKT
      FOR ALL ENTRIES IN LT_LIKP
      WHERE LFART = LT_LIKP-LFART
      AND SPRAS = SY-LANGU
      .
    SELECT * FROM TVSTT
      INTO TABLE LT_TVSTT
      FOR ALL ENTRIES IN LT_LIKP
      WHERE VSTEL = LT_LIKP-VSTEL
      AND SPRAS = SY-LANGU
      .
    SELECT * FROM TVKOT
      INTO TABLE LT_TVKOT
      FOR ALL ENTRIES IN LT_LIKP
      WHERE VKORG = LT_LIKP-VKORG
      AND SPRAS = SY-LANGU
      .
    SELECT VBELN
           POSNR
           PSTYV
           VGBEL
           VGPOS
           UECHA
           MATNR
           ARKTX
           LFIMG
           MEINS
           KDMAT
      FROM LIPS
      INTO CORRESPONDING FIELDS OF TABLE LT_LIPS
      FOR ALL ENTRIES IN LT_LIKP
      WHERE VBELN = LT_LIKP-VBELN
*      AND   vgbel IN s_vgbel
      .
    IF LT_LIPS[] IS  NOT INITIAL.
      "add it_vbkd  IT02 150730
      SELECT VBELN  POSNR BSTKD
        INTO CORRESPONDING FIELDS OF TABLE IT_VBKD
        FROM VBKD
        FOR ALL ENTRIES IN LT_LIPS
        WHERE VBELN = LT_LIPS-VGBEL.
      SORT IT_VBKD BY VBELN POSNR.
      LOOP AT LT_LIPS INTO LW_LIPS.
        MOVE-CORRESPONDING LW_LIPS TO LT_LIPS2.
        APPEND LT_LIPS2.
      ENDLOOP.

      SELECT * FROM ZTSD003
        INTO TABLE LT_ZTSD003
        FOR ALL ENTRIES IN LT_LIPS
        WHERE VBELN = LT_LIPS-VBELN
        AND   POSNR = LT_LIPS-POSNR
*      AND   zdy in l_pr_flag
        .

      SELECT VBELN FROM VBAK
        INTO TABLE LT_VBAK
        FOR ALL ENTRIES IN LT_LIPS
        WHERE VBELN = LT_LIPS-VGBEL
        .

      SELECT VBELN
             VBELP
             EBELN
             EBELP
        FROM EKKN
        INTO CORRESPONDING FIELDS OF TABLE LT_EKKN
        FOR ALL ENTRIES IN LT_LIPS2
        WHERE EBELN = LT_LIPS2-VGBEL
        AND   EBELP = LT_LIPS2-VGPOS
        .
      IF LT_EKKN[] IS NOT INITIAL.
        SELECT VBELN
               POSNR
               KDMAT
          FROM VBAP
          INTO CORRESPONDING FIELDS OF TABLE LT_VBAP
          FOR ALL ENTRIES IN LT_EKKN
          WHERE VBELN = LT_EKKN-VBELN
          AND   POSNR = LT_EKKN-VBELP
          .
      ENDIF.

    ENDIF.

    SELECT * INTO TABLE  LT_EKKO
      FROM EKKO
      WHERE EBELN  IN S_EBELN .
    SORT LT_EKKO BY EBELN .
  ENDIF.
  LOOP AT LT_LIPS INTO LW_LIPS WHERE  UECHA = ''.
    G_INDEX = SY-TABIX .   "当前表索引行
    CLEAR IT_OUT.
    CLEAR LW_LIKP.

    READ TABLE LT_LIKP INTO LW_LIKP WITH KEY VBELN = LW_LIPS-VBELN.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LW_LIKP TO IT_OUT.
      CLEAR LW_TVLKT.
      READ TABLE LT_TVLKT INTO LW_TVLKT WITH KEY LFART = IT_OUT-LFART.
      IF SY-SUBRC = 0.
        IT_OUT-VTEXT1 = LW_TVLKT-VTEXT.
      ENDIF.

      CLEAR LW_TVSTT.
      READ TABLE LT_TVSTT INTO LW_TVSTT WITH KEY VSTEL = IT_OUT-VSTEL.
      IF SY-SUBRC = 0.
        IT_OUT-VTEXT2 = LW_TVSTT-VTEXT.
      ENDIF.

      CLEAR LW_TVKOT.
      READ TABLE LT_TVKOT INTO LW_TVKOT WITH KEY VKORG = IT_OUT-VKORG.
      IF SY-SUBRC = 0.
        IT_OUT-VTEXT3 = LW_TVKOT-VTEXT.
      ENDIF.
    ENDIF.

*   it_out-VBELN  =  lw_lips-VBELN.
    IT_OUT-POSNR  =  LW_LIPS-POSNR.
    IT_OUT-MATNR  =  LW_LIPS-MATNR.
    IT_OUT-ARKTX  =  LW_LIPS-ARKTX.
    IT_OUT-LFIMG  =  LW_LIPS-LFIMG.
    IT_OUT-MEINS  =  LW_LIPS-MEINS.
    IT_OUT-KDMAT  =  LW_LIPS-KDMAT.


    CLEAR LW_VBAK.
    READ TABLE LT_VBAK INTO LW_VBAK WITH KEY VBELN = LW_LIPS-VGBEL.
    IF SY-SUBRC <> 0.
      CLEAR LW_EKKN.

      READ TABLE LT_EKKO INTO LW_EKKO WITH KEY EBELN = LW_LIPS-VGBEL BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        IT_OUT-EBELN = LW_EKKO-EBELN .
      ENDIF.

      READ TABLE LT_EKKN INTO LW_EKKN WITH KEY EBELN = LW_LIPS-VGBEL EBELP = LW_LIPS-VGPOS.
      IF SY-SUBRC = 0.
        IT_OUT-VGBEL  =  LW_EKKN-VBELN.
        IF IT_OUT-KDMAT = ''.
          CLEAR LT_VBAP.
          READ TABLE LT_VBAP WITH KEY VBELN = LW_EKKN-VBELN POSNR = LW_EKKN-VBELP.
          IF SY-SUBRC = 0.
            IT_OUT-KDMAT = LT_VBAP-KDMAT.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.


      IT_OUT-VGBEL  =  LW_LIPS-VGBEL.

    ENDIF.

    "判断参照的采购订单号是否在选择屏幕的采购订单号范围内
    IF IT_OUT-EBELN NOT IN S_EBELN .
      CONTINUE.
    ENDIF.

    IF IT_OUT-VGBEL NOT IN S_VGBEL.
      CONTINUE.
    ENDIF.
    "批次行数量计算
    LOOP AT LT_LIPS WHERE VBELN = LW_LIPS-VBELN AND UECHA = LW_LIPS-POSNR.
      IT_OUT-LFIMG = IT_OUT-LFIMG + LT_LIPS-LFIMG.
    ENDLOOP.

    "销售组织为'2110‘ 备注默认为：lips-kdmat .
    IF IT_OUT-VKORG EQ '2110' OR IT_OUT-VKORG EQ '2100' OR IT_OUT-VKORG EQ '2120'.
      IT_OUT-ZBZ = LW_LIPS-KDMAT .
    ENDIF.

    CLEAR LW_ZTSD003.
    READ TABLE LT_ZTSD003 INTO LW_ZTSD003 WITH KEY VBELN = LW_LIPS-VBELN
                                                    POSNR = LW_LIPS-POSNR.
    IF SY-SUBRC = 0.
      IT_OUT-ZDY    = LW_ZTSD003-ZDY  .
      IT_OUT-ZSHR   = LW_ZTSD003-ZSHR .
      IT_OUT-ZDH    = LW_ZTSD003-ZDH  .
      IT_OUT-ZDZ    = LW_ZTSD003-ZDZ  .
      IT_OUT-ZYSFS  = LW_ZTSD003-ZYSFS.
      IT_OUT-ZZJS   = LW_ZTSD003-ZZJS .
      IT_OUT-ZFHR   = LW_ZTSD003-ZFHR .
      IT_OUT-ZJH    = LW_ZTSD003-ZJH  .
      IT_OUT-ZBZ    = LW_ZTSD003-ZBZ  .
      IT_OUT-ZXS    = LW_ZTSD003-ZXS  .    "箱数
    ENDIF.
    CLEAR IT_OUT-ZLTEXT.
    IF P_SEL1 = 'X' AND IT_OUT-ZDY = 'X'.

      PERFORM FRM_READ_TEXT USING IT_OUT-VGBEL SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-ZLTEXT.
      APPEND IT_OUT.
    ENDIF.
    IF P_SEL2 = 'X' AND IT_OUT-ZDY = ''.
      PERFORM FRM_READ_TEXT USING IT_OUT-VGBEL SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-ZLTEXT.
      APPEND IT_OUT.
    ENDIF.
  ENDLOOP.
  DELETE IT_OUT WHERE VGBEL NOT IN S_VGBEL.
  SORT IT_OUT BY VBELN POSNR.
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


  PERFORM FRM_FILL_CAT USING :
                 1  ''  'ZDY'      TEXT-026   '8', "'打印标示'   ,
                 2  ''  'VGBEL'    TEXT-001   '10', "'项目编号'   ,
                 3  ''  'ZLTEXT'   TEXT-002   '40', "'项目描述'   ,
                 4  ''  'VBELN'    TEXT-003   '10', "'交货单编号'
                 5  ''  'EBELN'    TEXT-025   '10', "'采购订单编号'
                 6  ''  'WADAT'    TEXT-004   '10', "'交货日期'   ,
                 7  ''  'LFART'    TEXT-005   '10', "'交货单类型'
                 8  ''  'VTEXT1'   TEXT-006   '40', "'交货单类型描述'
                 9  ''  'VSTEL'    TEXT-007   '10', "'装运点'   ,
                 10  ''  'VTEXT2'   TEXT-008   '20', "'装运点描述'
                 11 ''  'VKORG'    TEXT-009   '8', "'销售组织'   ,
                 12 ''  'VTEXT3'   TEXT-010   '20', "'销售组织描述'
                 13 'X' 'ZSHR'     TEXT-011   '10', "'收货人'   ,
                 14 'X' 'ZDH'      TEXT-012   '20', "'电话'   ,
                 15 'X' 'ZDZ'      TEXT-013   '40', "'地址'   ,
                 16 'X' 'ZYSFS'    TEXT-014   '10', "'运输方式'   ,
                 17 'X' 'ZZJS'     TEXT-015   '10', "'总件数'   ,
                 17 'X' 'ZXS'      TEXT-027   '10', "'箱数'   ,
                 18 'X' 'ZFHR'     TEXT-016   '10', "'发货人'   ,
                 19 ''  'POSNR'    TEXT-017   '10', "'行项目编号'
                 20 'X' 'ZJH'      TEXT-018   '10', "'件号'   ,
                 21 ''  'MATNR'    TEXT-019   '10', "'物料编码'   ,
                 22 ''  'ARKTX'    TEXT-020   '20', "'货物名称'   ,
                 23 ''  'LFIMG'    TEXT-021   '8', "'数量'   ,
                 24 ''  'MEINS'    TEXT-022   '4', "'单位'   ,
                 25 'X' 'ZBZ'      TEXT-023   '20', "'备注'   ,
                 26 ''  'KDMAT'    TEXT-024   '40'. "'类别'   .
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
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME U_LENTH.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  LW_FIELDCAT-OUTPUTLEN   = U_LENTH.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "frm_fill_cat




*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_REPORT .
  DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE LVC_S_GLAY.

  L_LAYOUT-BOX_FIELDNAME = 'ZBOX'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.
  PERFORM FRM_CREATE_EVENTS.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_SAVE                   = 'X'
      I_GRID_SETTINGS          = L_GRID_SETTINGS
      IT_EVENTS                = GT_EVENTS
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
  WA_HEADER-INFO =  SY-TITLE."'装箱单维护打印平台'.
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
      PERFORM FRM_SAVE_DATA.
    WHEN '&PRNT'.
      PERFORM FRM_PRINT_DATA.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_SAVE_DATA.
  DATA L_OUT LIKE LINE OF IT_OUT.
  DATA LT_ZTSD003 TYPE ZTSD003 OCCURS 0 WITH HEADER LINE.
  LOOP AT  IT_OUT INTO L_OUT.
    CLEAR LT_ZTSD003.
    MOVE-CORRESPONDING L_OUT TO LT_ZTSD003.
    APPEND LT_ZTSD003.
  ENDLOOP.
  MODIFY ZTSD003 FROM TABLE LT_ZTSD003.
  IF SY-SUBRC = 0.
    MESSAGE S002(Z001).
  ENDIF.
ENDFORM.                    "frm_save_data
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZSD003_STATUS'.
ENDFORM.                    "set_pf_status


"自动按交货单第一行录入数据处理其他行项目录入信息
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL..
  DATA: L_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL   TYPE LVC_S_STBL.

  DATA L_OUT LIKE LINE OF IT_OUT.
  DATA LT_OUT LIKE TABLE OF IT_OUT WITH HEADER LINE.

  FIELD-SYMBOLS <L_CHANG> TYPE ANY.
  ASSIGN ER_DATA_CHANGED->MP_MOD_ROWS->* TO <L_CHANG>.
  LT_OUT[] = <L_CHANG>.

  FIELD-SYMBOLS <L_OUT> LIKE LINE OF IT_OUT.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.
  "call METHOD l_grid->CHECK_CHANGED_DATA.

  SORT IT_OUT BY VBELN POSNR.
  LOOP AT IT_OUT ASSIGNING <L_OUT>.
    AT NEW VBELN.
      L_OUT = <L_OUT>.
      CLEAR LT_OUT.
      READ TABLE LT_OUT WITH KEY VBELN = <L_OUT>-VBELN POSNR = <L_OUT>-POSNR.
      IF SY-SUBRC = 0.
        L_OUT-ZSHR  = LT_OUT-ZSHR .
        L_OUT-ZDH   = LT_OUT-ZDH  .
        L_OUT-ZDZ   = LT_OUT-ZDZ  .
        L_OUT-ZYSFS = LT_OUT-ZYSFS.
        L_OUT-ZZJS  = LT_OUT-ZZJS .
        L_OUT-ZFHR  = LT_OUT-ZFHR .
        L_OUT-ZXS   = LT_OUT-ZXS .
      ENDIF.
    ENDAT.
    <L_OUT>-ZSHR   = L_OUT-ZSHR .
    <L_OUT>-ZDH    = L_OUT-ZDH  .
    <L_OUT>-ZDZ    = L_OUT-ZDZ  .
    <L_OUT>-ZYSFS  = L_OUT-ZYSFS.
    <L_OUT>-ZZJS   = L_OUT-ZZJS .
    <L_OUT>-ZFHR   = L_OUT-ZFHR .
    <L_OUT>-ZXS    = L_OUT-ZXS  .
  ENDLOOP.
  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD L_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.

ENDFORM.                    "frm_data_enter


*&---------------------------------------------------------------------**&
*Form  frm_create_events
*&---------------------------------------------------------------------**
FORM FRM_CREATE_EVENTS.

  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.                    "frm_create_events


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
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 8,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFSD019'.
  DATA:LT_ZTSD003 TYPE ZTSD003 OCCURS 0 WITH HEADER LINE.
  DATA:LT_SELECT LIKE  IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA:LW_SELECT LIKE LINE OF LT_SELECT.
*  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
  DATA:LW_PRT LIKE LINE OF IT_PRT.

  DATA: LV_TITLE TYPE CHAR25. "打印标题

  LT_SELECT[] = IT_OUT[].

*   READ TABLE lt_select WITH KEY checkbox = 'X'.
*   IF sy-subrc <> 4.
*     DELETE lt_select WHERE checkbox <> 'X'.  "删除ALV中未选择的行
*   ENDIF.
*   CHECK sy-subrc = 0.
  CLEAR:LW_PRT,IT_PRT[].
  DATA:BZTMP TYPE CHAR100.
  LOOP AT LT_SELECT INTO LW_SELECT WHERE ZBOX = 'X'.
    CLEAR  BZTMP.
    MOVE-CORRESPONDING LW_SELECT TO LW_PRT.
    "150730 add begin  备注

    READ TABLE LT_LIPS WITH KEY VBELN = LW_PRT-VBELN POSNR = LW_PRT-POSNR .
    IF SY-SUBRC = 0.
      IF LT_LIPS-PSTYV = 'Z11' OR LT_LIPS-PSTYV = 'Z12' OR  LT_LIPS-PSTYV = 'Z13' OR  LT_LIPS-PSTYV = 'Z14'
         OR  LT_LIPS-PSTYV = 'Z15' OR  LT_LIPS-PSTYV = 'Z16' OR  LT_LIPS-PSTYV = 'Z17' OR  LT_LIPS-PSTYV = 'Z18'.
        READ TABLE IT_VBKD WITH KEY VBELN = LT_LIPS-VGBEL POSNR = LT_LIPS-VGPOS .
        IF SY-SUBRC = 0 . .
          BZTMP = IT_VBKD-BSTKD .
        ELSE.
          READ TABLE IT_VBKD WITH KEY VBELN = LT_LIPS-VGBEL  POSNR = '0000'.
          IF SY-SUBRC = 0.
            BZTMP = IT_VBKD-BSTKD .
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    IF LW_PRT-ZBZ NE '' .
      IF BZTMP NE '' .
        CONCATENATE  LW_PRT-ZBZ '_'  BZTMP INTO  LW_PRT-ZBZ .
      ENDIF.
    ELSE.
      LW_PRT-ZBZ = BZTMP .
    ENDIF.
    "150730 add end
    APPEND LW_PRT TO IT_PRT.
  ENDLOOP.

  IF IT_PRT[] IS INITIAL.
    MESSAGE S001(Z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK IT_PRT[] IS NOT INITIAL.

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

  SORT IT_PRT[] BY VGBEL VBELN POSNR.
  LOOP AT IT_PRT.

    AT NEW VGBEL.
      CLEAR : IT_PRT,
              NCURRLINE,
              EMPTYCOUNT,
              LW_PRT,
              IT_PRT2,
              IT_PRT2[].
      NPAGELINE = 2.

    ENDAT.
    MOVE IT_PRT TO LW_PRT.
    APPEND LW_PRT TO IT_PRT2.
    AT END OF VGBEL.
*&------打印标题
      IF LW_PRT-LFART = 'ZLDF'.
        IF LW_PRT-VKORG BETWEEN '2901' AND '2902'.
          LV_TITLE = '上海蓝硕数码科技有限公司出库单'.
        ELSEIF LW_PRT-VKORG BETWEEN '2911' AND '2912'.
          LV_TITLE = '奕硕多媒体移动科技（上海）有限公司出库单'.
        ENDIF.
      ELSEIF LW_PRT-LFART = 'ZLDR'.
        IF LW_PRT-VKORG BETWEEN '2901' AND '2902'.
          LV_TITLE = '上海蓝硕数码科技有限公司入库单'.
        ELSEIF LW_PRT-VKORG BETWEEN '2911' AND '2912'.
          LV_TITLE = '奕硕多媒体移动科技（上海）有限公司入库单'.
        ENDIF.
      ELSEIF LW_PRT-LFART = 'NLCC'.
        IF LW_PRT-VKORG BETWEEN '2901' AND '2902'.
          LV_TITLE = '上海蓝硕数码科技有限公司公司间出库单'.
        ELSEIF LW_PRT-VKORG BETWEEN '2911' AND '2912'.
          LV_TITLE = '奕硕多媒体移动科技（上海）有限公司公司间出库单'.
        ENDIF.
      ELSEIF LW_PRT-LFART = 'RL'.
        IF LW_PRT-VKORG BETWEEN '2901' AND '2902'.
          LV_TITLE = '上海蓝硕数码科技有限公司公司间入库单'.
        ELSEIF LW_PRT-VKORG BETWEEN '2911' AND '2912'.
          LV_TITLE = '奕硕多媒体移动科技（上海）有限公司公司间入库单'.
        ENDIF.
      ENDIF.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          GV_TITLE           = LV_TITLE
*         npage              = npageline
*         w_head             = lw_prt
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
      CLEAR: LV_TITLE.
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
    LOOP AT IT_PRT2 INTO LW_PRT.
      LW_PRT-ZDY = 'X'.
      CLEAR LT_ZTSD003.
      MOVE-CORRESPONDING LW_PRT TO LT_ZTSD003.
      APPEND LT_ZTSD003.
    ENDLOOP.
    MODIFY ZTSD003 FROM TABLE LT_ZTSD003.
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
