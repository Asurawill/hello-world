*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZPP005
*内容描述       生产计划内退料单
*版本       V1.0
*姓名       handlj
*日期       05.02.2015 11:27:21
*-----------------------------------------------------------------------------
REPORT ZPP005.
TABLES : AUFK,AFKO,AFPO.
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
        AUFNR   TYPE AUFK-AUFNR,
        AUART   TYPE AUFK-AUART,
        OBJNR   TYPE AUFK-OBJNR,
        MATNR_H TYPE AFPO-MATNR,
        MATNR_I TYPE ZPP005_STRU-MATNR_I,
        AUWERK  TYPE AUFK-WERKS,
        WERKS   TYPE ZPP005_STRU-WERKS,
        LGORT   TYPE ZPP005_STRU-LGORT,
        CHARG   TYPE ZPP005_STRU-CHARG,
        BWART   TYPE MSEG-BWART,
        ERFMG   TYPE MSEG-ERFMG,
        BAKMG   TYPE MSEG-ERFMG,
        KTEXT   TYPE AUFK-KTEXT,
        MAKTX   TYPE MAKT-MAKTX,
        MATKL   TYPE MARA-MATKL,
        MEINS   TYPE MSEG-MEINS,
        MBLNR   TYPE MSEG-MBLNR,
        ZBOX    TYPE C,
      END OF TY_OUT.

DATA IT_OUT    TYPE TY_OUT OCCURS 0 WITH HEADER LINE .

*DATA:BEGIN OF it_out OCCURS 0,
*       aufnr   TYPE aufk-aufnr,
*       auart   TYPE aufk-auart,
*       objnr   TYPE aufk-objnr,
*       matnr_h TYPE afpo-matnr,
*       matnr_i TYPE mseg-matnr,
*       charg   TYPE mseg-charg,
*       bwart   TYPE mseg-bwart,
*       erfmg   TYPE mseg-erfmg,
*       bakmg   TYPE mseg-erfmg,
*       auwerk  TYPE aufk-werks,
*       ktext   TYPE aufk-ktext,
*       maktx   TYPE makt-maktx,
*       matkl   TYPE mara-matkl,
*       werks   TYPE mseg-werks,
*       lgort   TYPE mard-lgort,
*       meins   TYPE mseg-meins,
*       mblnr   TYPE mseg-mblnr,
*     END OF it_out.
DATA:IT_PRT LIKE TABLE OF IT_OUT WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-015 .
PARAMETERS    : P_WERKS TYPE AUFK-WERKS DEFAULT '1100' OBLIGATORY.
SELECT-OPTIONS:
                S_AUFNR FOR AUFK-AUFNR OBLIGATORY ,
                S_MATNH FOR AFPO-MATNR,
                S_DISPO FOR AFKO-DISPO,
                S_FEVOR FOR AFKO-FEVOR,
                S_GSTRP FOR AFKO-GSTRP,
                S_GLTRP FOR AFKO-GLTRP,
                S_MATNI FOR AFPO-MATNR.
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
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '__________'
           ID 'WERKS' FIELD  P_WERKS.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH P_WERKS.
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
  DATA LT_MSEG TYPE MSEG OCCURS 0 WITH HEADER LINE.
  DATA LT_OUT LIKE IT_OUT OCCURS 0 .
  DATA LT_OUT_RESB LIKE IT_OUT OCCURS 0 .
  DATA LT_MAKT  TYPE MAKT OCCURS 0 WITH HEADER LINE.
  DATA LT_MAKTH TYPE MAKT OCCURS 0 WITH HEADER LINE.
  DATA LT_MARA TYPE MARA OCCURS 0 WITH HEADER LINE.
  DATA LT_MARD TYPE MARD OCCURS 0 WITH HEADER LINE.
  DATA LT_JEST TYPE JEST OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS <LW_OUT> LIKE LINE OF IT_OUT.
  SELECT AUFK~AUART
         AUFK~KTEXT
         AUFK~OBJNR
         AFPO~MATNR AS MATNR_H
         MSEG~MBLNR
         MSEG~MJAHR
         MSEG~ZEILE
         MSEG~BWART
         MSEG~MATNR AS MATNR_I
         AUFK~WERKS AS AUWERK
         MSEG~WERKS
         MSEG~LGORT
         MSEG~CHARG
         MSEG~ERFMG
         MSEG~MEINS
         MSEG~AUFNR
    FROM MSEG JOIN AFKO
    ON MSEG~AUFNR = AFKO~AUFNR
    JOIN AUFK
    ON MSEG~AUFNR = AUFK~AUFNR
    JOIN AFPO
    ON AFKO~AUFNR = AFPO~AUFNR
    INTO CORRESPONDING FIELDS OF TABLE LT_OUT
    WHERE MSEG~AUFNR IN S_AUFNR
    AND MSEG~WERKS = P_WERKS
    AND MSEG~MATNR IN S_MATNI
    AND AFKO~DISPO IN S_DISPO
    AND AFKO~FEVOR IN S_FEVOR
    AND AFKO~GSTRP IN S_GSTRP
    AND AFKO~GLTRP IN S_GLTRP
    AND AFPO~MATNR IN S_MATNH
    AND MSEG~RSNUM <> ''
    " 2015.4.16修改 2015/4/16变更逻辑：计划外领料的判断逻辑排除531、532
*    AND mseg~bwart IN ('261','262','531','532')
    AND MSEG~BWART IN ('261','262')
     .
  " 2015.4.16修改 2015/4/16变更逻辑：计划外领料的判断逻辑排除531、532
  "增加resb 拆解531 数据
  SELECT AUFK~AUART
         AUFK~KTEXT
         AUFK~OBJNR
         AFPO~MATNR AS MATNR_H
         RESB~BWART
         RESB~MATNR AS MATNR_I
         AUFK~WERKS AS AUWERK
         RESB~WERKS
         RESB~LGORT
         RESB~CHARG
         RESB~ENMNG AS ERFMG "需求
         RESB~BDMNG AS BAKMG "提货
         RESB~MEINS
         RESB~AUFNR
    FROM RESB JOIN AFKO
    ON RESB~AUFNR = AFKO~AUFNR
    JOIN AUFK
    ON RESB~AUFNR = AUFK~AUFNR
    JOIN AFPO
    ON AFKO~AUFNR = AFPO~AUFNR
    APPENDING CORRESPONDING FIELDS OF TABLE  LT_OUT_RESB
    WHERE RESB~AUFNR IN S_AUFNR
    AND RESB~WERKS = P_WERKS
    AND RESB~MATNR IN S_MATNI
    AND AFKO~DISPO IN S_DISPO
    AND AFKO~FEVOR IN S_FEVOR
    AND AFKO~GSTRP IN S_GSTRP
    AND AFKO~GLTRP IN S_GLTRP
    AND AFPO~MATNR IN S_MATNH
*    AND mseg~bwart IN ('261','262','531','532')
    AND RESB~BWART = '531'
    AND RESB~KZEAR = ''
    AND RESB~XLOEK = ''
     .
*  LOOP AT lt_out_resb ASSIGNING <lw_out> .
*    <lw_out>-bakmg = <lw_out>-bakmg - <lw_out>-erfmg.
*    append <lw_out> to lt_out.
*  ENDLOOP.
  SORT LT_OUT BY AUFNR
                 AUART
                 OBJNR
                 MATNR_H
                 MATNR_I
                 AUWERK
                 WERKS
                 LGORT
                 CHARG
                 BWART  .

  SORT LT_OUT_RESB BY AUFNR
                     AUART
                     OBJNR
                     MATNR_H
                     MATNR_I
                     AUWERK
                     WERKS
                     LGORT
                     CHARG
                     BWART  .
  IF LT_OUT[] IS   NOT INITIAL .
    SELECT OBJNR
            STAT
            INACT
   FROM JEST
   INTO CORRESPONDING FIELDS OF TABLE LT_JEST
    FOR ALL ENTRIES IN LT_OUT
    WHERE OBJNR = LT_OUT-OBJNR
    AND STAT IN ( 'I0013', 'I0043','I0045' )
    AND INACT <> 'X'
      .

  ENDIF.
  IF LT_OUT_RESB[] IS   NOT INITIAL .
    SELECT OBJNR
            STAT
            INACT
   FROM JEST
   APPENDING CORRESPONDING FIELDS OF TABLE LT_JEST
    FOR ALL ENTRIES IN LT_OUT_RESB
    WHERE OBJNR = LT_OUT_RESB-OBJNR
    AND STAT IN ( 'I0013', 'I0043','I0045' )
    AND INACT <> 'X'
      .
  ENDIF.

  IF LT_OUT[] IS NOT INITIAL .
    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKTH
      FOR ALL ENTRIES IN LT_OUT
      WHERE SPRAS = SY-LANGU
      AND   MATNR = LT_OUT-MATNR_H
      .

    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FOR ALL ENTRIES IN LT_OUT
      WHERE SPRAS = SY-LANGU
      AND   MATNR = LT_OUT-MATNR_I
      .

    SELECT MATNR
           MATKL
      FROM MARA
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FOR ALL ENTRIES IN LT_OUT
      WHERE MATNR = LT_OUT-MATNR_I
      .
*    SELECT matnr
*           werks
*           lgort
*      FROM mard
*      INTO CORRESPONDING FIELDS OF TABLE lt_mard
*      FOR ALL ENTRIES IN lt_out
*      WHERE matnr = lt_out-matnr_i
*      AND  werks =  lt_out-auwerk
*      .
  ENDIF.

  LOOP AT LT_OUT ASSIGNING <LW_OUT> .
    CLEAR LT_JEST.
    READ TABLE LT_JEST WITH KEY OBJNR = <LW_OUT>-OBJNR.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.
    AT NEW CHARG.
      CLEAR IT_OUT.
      IT_OUT-AUFNR     = <LW_OUT>-AUFNR   .
      IT_OUT-AUART     = <LW_OUT>-AUART   .
      IT_OUT-OBJNR     = <LW_OUT>-OBJNR   .
      IT_OUT-MATNR_H   = <LW_OUT>-MATNR_H .
      IT_OUT-KTEXT     = <LW_OUT>-KTEXT   .
      IT_OUT-MATNR_I   = <LW_OUT>-MATNR_I .
      IT_OUT-CHARG     = <LW_OUT>-CHARG   .
      IT_OUT-BWART     = <LW_OUT>-BWART   .
      IT_OUT-AUWERK    = <LW_OUT>-AUWERK  .
      IT_OUT-WERKS     = <LW_OUT>-WERKS   .
      IT_OUT-LGORT     = <LW_OUT>-LGORT   .
      IT_OUT-MEINS     = <LW_OUT>-MEINS   .
      CLEAR LT_MAKTH.
      READ TABLE LT_MAKTH WITH KEY MATNR = IT_OUT-MATNR_H.
      IF SY-SUBRC = 0.
        IT_OUT-KTEXT = LT_MAKTH-MAKTX.
      ENDIF.
      CLEAR LT_MAKT.
      READ TABLE LT_MAKT WITH KEY MATNR = IT_OUT-MATNR_I.
      IF SY-SUBRC = 0.
        IT_OUT-MAKTX = LT_MAKT-MAKTX.
      ENDIF.

      CLEAR LT_MARA.
      READ TABLE LT_MARA WITH KEY MATNR = IT_OUT-MATNR_I.
      IF SY-SUBRC = 0.
        IT_OUT-MATKL = LT_MARA-MATKL.
      ENDIF.

*      CLEAR lt_mard.
*      READ TABLE lt_mard WITH KEY matnr = it_out-matnr_i werks = it_out-auwerk.
*      IF sy-subrc = 0.
*        it_out-lgort = lt_mard-lgort.
*      ENDIF.
    ENDAT.

    CASE <LW_OUT>-BWART.
      WHEN  '261'.
        IT_OUT-ERFMG = IT_OUT-ERFMG + <LW_OUT>-ERFMG.
      WHEN  '262 '.
        IT_OUT-ERFMG = IT_OUT-ERFMG - <LW_OUT>-ERFMG.
*      WHEN  '532 '.
*        it_out-erfmg = it_out-erfmg - <lw_out>-erfmg.
      WHEN OTHERS.
    ENDCASE.

    AT END OF CHARG.
      IT_OUT-BAKMG =  IT_OUT-ERFMG.
      IF IT_OUT-ERFMG > 0.
        APPEND IT_OUT.
      ENDIF.
      CLEAR IT_OUT.
    ENDAT.
  ENDLOOP.

  IF LT_OUT_RESB[] IS NOT INITIAL .
    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKTH
      FOR ALL ENTRIES IN LT_OUT_RESB
      WHERE SPRAS = SY-LANGU
      AND   MATNR = LT_OUT_RESB-MATNR_H
      .

    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FOR ALL ENTRIES IN LT_OUT_RESB
      WHERE SPRAS = SY-LANGU
      AND   MATNR = LT_OUT_RESB-MATNR_I
      .

    SELECT MATNR
           MATKL
      FROM MARA
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FOR ALL ENTRIES IN LT_OUT_RESB
      WHERE MATNR = LT_OUT_RESB-MATNR_I
      .
*    SELECT matnr
*           werks
*           lgort
*      FROM mard
*      INTO CORRESPONDING FIELDS OF TABLE lt_mard
*      FOR ALL ENTRIES IN lt_out
*      WHERE matnr = lt_out-matnr_i
*      AND  werks =  lt_out-auwerk
*      .
  ENDIF.
*  RESB 531数据单独增加
  LOOP AT LT_OUT_RESB ASSIGNING <LW_OUT> .
    CLEAR LT_JEST.
    READ TABLE LT_JEST WITH KEY OBJNR = <LW_OUT>-OBJNR.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.
    AT NEW CHARG.
      CLEAR IT_OUT.
      IT_OUT-AUFNR     = <LW_OUT>-AUFNR   .
      IT_OUT-AUART     = <LW_OUT>-AUART   .
      IT_OUT-OBJNR     = <LW_OUT>-OBJNR   .
      IT_OUT-MATNR_H   = <LW_OUT>-MATNR_H .
      IT_OUT-KTEXT     = <LW_OUT>-KTEXT   .
      IT_OUT-MATNR_I   = <LW_OUT>-MATNR_I .
      IT_OUT-CHARG     = <LW_OUT>-CHARG   .
      IT_OUT-BWART     = <LW_OUT>-BWART   .
      IT_OUT-AUWERK    = <LW_OUT>-AUWERK  .
      IT_OUT-WERKS     = <LW_OUT>-WERKS   .
      IT_OUT-LGORT     = <LW_OUT>-LGORT   .
      IT_OUT-MEINS     = <LW_OUT>-MEINS   .
      CLEAR LT_MAKTH.
      READ TABLE LT_MAKTH WITH KEY MATNR = IT_OUT-MATNR_H.
      IF SY-SUBRC = 0.
        IT_OUT-KTEXT = LT_MAKTH-MAKTX.
      ENDIF.
      CLEAR LT_MAKT.
      READ TABLE LT_MAKT WITH KEY MATNR = IT_OUT-MATNR_I.
      IF SY-SUBRC = 0.
        IT_OUT-MAKTX = LT_MAKT-MAKTX.
      ENDIF.

      CLEAR LT_MARA.
      READ TABLE LT_MARA WITH KEY MATNR = IT_OUT-MATNR_I.
      IF SY-SUBRC = 0.
        IT_OUT-MATKL = LT_MARA-MATKL.
      ENDIF.

*      CLEAR lt_mard.
*      READ TABLE lt_mard WITH KEY matnr = it_out-matnr_i werks = it_out-auwerk.
*      IF sy-subrc = 0.
*        it_out-lgort = lt_mard-lgort.
*      ENDIF.
    ENDAT.
    IT_OUT-ERFMG = IT_OUT-ERFMG - <LW_OUT>-ERFMG. "提货数
    IT_OUT-BAKMG = IT_OUT-BAKMG  - <LW_OUT>-BAKMG + <LW_OUT>-ERFMG . "退货
    AT END OF CHARG.
      APPEND IT_OUT.
      CLEAR IT_OUT.
    ENDAT.
  ENDLOOP.

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
                 1  ''  'AUFNR'   ''        TEXT-001          , "订单号
                 2  ''  'AUART'   ''        TEXT-002          , "订单类型
                 3  ''  'AUWERK'  ''        TEXT-003          , "订单工厂
                 4  ''  'MATNR_H' ''        TEXT-004          , "抬头物料号
                 5  ''  'KTEXT'   ''        TEXT-005          , "描述
                 6  ''  'MATNR_I' 'ZPP005_STRU'   TEXT-006    , "组件物料号
                 7  ''  'MAKTX'   ''        TEXT-007          , "描述
                 8  ''  'WERKS'   'ZPP005_STRU' TEXT-008      , "组件发出工厂
                 9  ''  'MATKL'   ''        TEXT-009          , "物料组
                 10 ''  'ERFMG'   ''        TEXT-010          , "已领料数量
                 11 'X'  'BAKMG'  ''        TEXT-011          , "退料数量
                 12 'X'  'CHARG'  'ZPP005_STRU'    TEXT-012   , "批次
                 13 ''  'MEINS'   ''        TEXT-013          , "单位
                 14 'X'  'LGORT'  'ZPP005_STRU'    TEXT-014   . "库存地点


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
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_TABNAME U_NAME.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-REF_FIELDNAME = U_FNAME.

  IF LW_FIELDCAT-EDIT = 'X' OR U_TABNAME = 'ZPP005_STRU'.
    LW_FIELDCAT-REF_TABNAME = U_TABNAME.
  ENDIF.
  IF  U_FNAME = 'ERFMG' ." OR u_fname = 'BAKMG' .
    LW_FIELDCAT-REF_TABNAME   = 'MSEG'.
    LW_FIELDCAT-REF_FIELDNAME = 'ERFMG'.
    LW_FIELDCAT-QTABNAME      = 'IT_OUT'.
    LW_FIELDCAT-QFIELDNAME    = 'MEINS'.
  ENDIF.

  IF U_FNAME = 'BAKMG' .

    LW_FIELDCAT-QTABNAME      = 'IT_OUT'.
    LW_FIELDCAT-QFIELDNAME    = 'MEINS'.
    LW_FIELDCAT-DECIMALS_OUT      = 3.

  ENDIF.

  LW_FIELDCAT-SELTEXT_S   = U_NAME.
  LW_FIELDCAT-SELTEXT_M   = U_NAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  LW_FIELDCAT-REPTEXT_DDIC   = U_NAME.
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

*  l_layout-CWIDTH_OPT = 'X'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_LAYOUT-BOX_FIELDNAME     = 'ZBOX'.
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

ENDFORM.                    "frm_save_data
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZPP005_STATUS'.
ENDFORM.                    "set_pf_status


"自动按交货单第一行录入数据处理其他行项目录入信息
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL..


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

  DATA: L_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL   TYPE LVC_S_STBL.
  DATA: LS_MOD_CELL   TYPE LVC_S_MODI,
        LV_VALUE      TYPE LVC_VALUE,
        LV_VALUE_1    TYPE LVC_VALUE,
        LV_VALUE_2    TYPE LVC_VALUE,
        L_VALUE       TYPE MSEG-ERFMG,
        L_VALUE_WERKS TYPE AUFK-WERKS,
        L_VALUE_MATNR TYPE AFPO-MATNR.


  DATA L_OUT LIKE LINE OF IT_OUT.
  DATA LT_OUT LIKE TABLE OF IT_OUT WITH HEADER LINE.
*  FIELD-SYMBOLS: <l_chang> TYPE any,<l_out> LIKE LINE OF it_out.
*  ASSIGN er_data_changed->mp_mod_rows->* TO <l_chang>.
*  lt_out[] = <l_chang>.
  READ TABLE LT_OUT INDEX 1.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.
  "call METHOD l_grid->CHECK_CHANGED_DATA.

  READ TABLE  ER_DATA_CHANGED->MT_MOD_CELLS
                         INTO LS_MOD_CELL
                         WITH KEY FIELDNAME = 'BAKMG'.
  IF SY-SUBRC = 0.
    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = 'ERFMG'
      IMPORTING
        E_VALUE     = LV_VALUE.

    CONDENSE LV_VALUE NO-GAPS.
    L_VALUE = LV_VALUE.

    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = 'AUWERK'
      IMPORTING
        E_VALUE     = LV_VALUE_1.

    CONDENSE LV_VALUE_1 NO-GAPS.
    L_VALUE_WERKS = LV_VALUE_1.

    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = 'MATNR_H'
      IMPORTING
        E_VALUE     = LV_VALUE_2.

    CONDENSE LV_VALUE_2 NO-GAPS.
    L_VALUE_MATNR = LV_VALUE_2.

    IF   LS_MOD_CELL-VALUE <  0 .
      IF  L_VALUE_WERKS EQ '1500' AND L_VALUE_MATNR EQ '' .

      ELSE.

        CALL METHOD ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
            I_MSGID     = 'ZPP01'
            I_MSGNO     = '010'
            I_MSGTY     = 'E'
      "     i_msgv1     = '
        "   i_msgv2     = lv_value
*           i_msgv3     = lv_value
            I_FIELDNAME = LS_MOD_CELL-FIELDNAME
            I_ROW_ID    = LS_MOD_CELL-ROW_ID.
      ENDIF.

    ENDIF.

    IF L_VALUE < LS_MOD_CELL-VALUE.
      CALL METHOD ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
        EXPORTING
          I_MSGID     = 'ZPP01'
          I_MSGNO     = '002'
          I_MSGTY     = 'E'
          I_MSGV1     = LS_MOD_CELL-VALUE
          I_MSGV2     = LV_VALUE
*         i_msgv3     = lv_value
          I_FIELDNAME = LS_MOD_CELL-FIELDNAME
          I_ROW_ID    = LS_MOD_CELL-ROW_ID.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_MOD_CELL-ROW_ID
          I_FIELDNAME = LS_MOD_CELL-FIELDNAME
          I_VALUE     = LV_VALUE.
    ENDIF.
  ENDIF.

*  CALL METHOD l_grid->refresh_table_display
*    EXPORTING
*      is_stable = stbl.
ENDFORM.     "frm_data_changed

*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 10,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I ,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFPP005'.

*&--代码添加 BY HANDYBY 10.07.2017 17:11:03  BEGIN
  IF P_WERKS = '1610' .
    L_FORMNAME = 'ZSFPP005_1'.
  ENDIF.
*&--代码添加 BY HANDYBY 10.07.2017 17:11:03  END

  DATA L_ANS TYPE C.
*  DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
*  DATA:lw_select LIKE LINE OF lt_select.
*  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
  DATA:LW_PRT LIKE LINE OF IT_PRT.

  IT_PRT[] = IT_OUT[].

  READ TABLE IT_PRT WITH KEY ZBOX = 'X'.

  IF SY-SUBRC = 0.
    DELETE IT_PRT WHERE ZBOX <> 'X'.  "删除ALV中未选择的行
  ENDIF.

  READ TABLE IT_PRT WITH KEY ZBOX = 'X'.
  CHECK SY-SUBRC = 0.

  IF IT_PRT[] IS INITIAL.
    MESSAGE S001(Z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK IT_PRT[] IS NOT INITIAL.

  PERFORM FRM_MESSAGE USING L_ANS.

  CHECK L_ANS = 'J'.
  SORT IT_PRT[] BY AUFNR MATKL .
  DESCRIBE TABLE IT_PRT LINES NTOTALLINE.
  NCURRLINE = NTOTALLINE MOD NPAGELINE.
  IF  NCURRLINE > 0.
    EMPTYCOUNT = NPAGELINE - NCURRLINE.
    DO EMPTYCOUNT TIMES.
      APPEND INITIAL LINE TO IT_PRT.
    ENDDO.
  ENDIF.
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



**    lw_prt = it_prt.
**    AT NEW vgbel.
**      CLEAR : it_prt,
**              ncurrline,
**              emptycount.
**      npageline = 2.
**    ENDAT.

  CALL FUNCTION G_NAME
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
      NPAGE              = NPAGELINE
*     w_head             = lw_prt
*         TABLES
*     t_item             = lt_prt[]
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

FORM FRM_MESSAGE CHANGING L_ANSWER.

  DATA LT_MESAGE_TAB TYPE ESP1_MESSAGE_WA_TYPE OCCURS 5 WITH HEADER LINE.
  DATA LT_PRT LIKE TABLE OF IT_PRT WITH HEADER LINE.
  FIELD-SYMBOLS <LW_OUT> LIKE LINE OF IT_OUT.


  DATA: BEGIN OF LT_FILTER OCCURS 0,
          AUFNR TYPE MSEG-AUFNR,
          MATNR TYPE MSEG-MATNR,
          BWART TYPE MSEG-BWART,
          ERFMG TYPE MSEG-ERFMG,
        END OF LT_FILTER.
  DATA:LT_FILTER_SUM LIKE TABLE OF LT_FILTER WITH HEADER LINE.
  FIELD-SYMBOLS <LW_FILTER> LIKE LINE OF LT_FILTER.
  L_ANSWER = 'J'.
  "取对应计划外领料数据 提示用户退料存在计划外领料
  SELECT MBLNR
         MJAHR
         ZEILE
         AUFNR
         MATNR
         BWART
         ERFMG
    FROM MSEG
    INTO CORRESPONDING FIELDS OF TABLE LT_FILTER
    FOR ALL ENTRIES IN IT_PRT
    WHERE AUFNR = IT_PRT-AUFNR
    AND   MATNR = IT_PRT-MATNR_I
*    AND   bwart IN ('Z05','Z06','531','532')
    AND   BWART IN ('Z05','Z06')              " 2015.4.16修改 2015/4/16变更逻辑：计划外领料的判断逻辑排除531、532
    AND   RSNUM = ''
    .
  SORT LT_FILTER BY AUFNR MATNR BWART.

  LOOP AT LT_FILTER ASSIGNING <LW_FILTER>.
    AT NEW MATNR.
      CLEAR LT_FILTER_SUM.
      LT_FILTER_SUM-AUFNR =  <LW_FILTER>-AUFNR.
      LT_FILTER_SUM-MATNR =  <LW_FILTER>-MATNR.
    ENDAT.
    " 2015.4.16修改 2015/4/16变更逻辑：计划外领料的判断逻辑排除531、532
    CASE <LW_FILTER>-BWART.
      WHEN  'Z05'.
        LT_FILTER_SUM-ERFMG = LT_FILTER_SUM-ERFMG + <LW_FILTER>-ERFMG.
*      WHEN  '531'.
*        lt_filter_sum-erfmg = lt_filter_sum-erfmg + <lw_filter>-erfmg.
      WHEN  'Z06'.
        LT_FILTER_SUM-ERFMG = LT_FILTER_SUM-ERFMG - <LW_FILTER>-ERFMG.
*      WHEN  '532'.
*        lt_filter_sum-erfmg = lt_filter_sum-erfmg - <lw_filter>-erfmg.
      WHEN OTHERS.
    ENDCASE.

    AT END OF MATNR.
      APPEND LT_FILTER_SUM.
    ENDAT.

  ENDLOOP.
  CLEAR LT_FILTER[].
  LOOP AT IT_PRT ASSIGNING <LW_OUT>.
    CLEAR LT_MESAGE_TAB.
    LT_MESAGE_TAB-LINENO =  SY-TABIX.
    CLEAR LT_FILTER_SUM.
    READ TABLE LT_FILTER_SUM WITH KEY AUFNR = <LW_OUT>-AUFNR
                                      MATNR = <LW_OUT>-MATNR_I.
    IF SY-SUBRC = 0.
      IF LT_FILTER_SUM-ERFMG > 0.
        LT_MESAGE_TAB-MSGID  =  'ZPP01'.
        LT_MESAGE_TAB-MSGTY  =  'W'.
        LT_MESAGE_TAB-MSGNO  =  003.
        LT_MESAGE_TAB-MSGV1  =  <LW_OUT>-AUFNR.
        LT_MESAGE_TAB-MSGV2  =  <LW_OUT>-MATNR_I.
        LT_MESAGE_TAB-MSGV3  =  LT_FILTER_SUM-ERFMG.
        CONDENSE LT_MESAGE_TAB-MSGV2 NO-GAPS.
        APPEND LT_MESAGE_TAB.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF LT_MESAGE_TAB[] IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        I_MESSAGE_TAB = LT_MESAGE_TAB.


    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
*       DEFAULTOPTION  = 'Y'
        DIAGNOSETEXT1  = '选择项目存在计划外领料'
*       DIAGNOSETEXT2  = ' '
*       DIAGNOSETEXT3  = ' '
        TEXTLINE1      = '是否继续进行计划内退料打印'
*       TEXTLINE2      = ' '
        TITEL          = '请确认'
*       START_COLUMN   = 25
*       START_ROW      = 6
        CANCEL_DISPLAY = ''
      IMPORTING
        ANSWER         = L_ANSWER.
  ENDIF.


ENDFORM.


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
