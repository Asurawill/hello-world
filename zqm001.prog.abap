*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZQM001
*内容描述           进料检验记录表打印
*版本       V1.0
*姓名       HANDLJ
*日期       28.01.2015 14:26:23
*-----------------------------------------------------------------------------
REPORT ZQM001.

TABLES : QALS,EKKO,LFA1,T024.
TYPE-POOLS : SLIS.
********************************************
DATA :S_COUNT TYPE I. "行数

DATA: S_PAGE TYPE I VALUE 5. "每页记录个数
DATA:LS_ZZS_04 LIKE ZJLX_BD.
DATA: GT_TAB LIKE TABLE OF ZJLX_BD.
DATA:LS_ZZS_04_10 LIKE  ZJLX_H.
DATA: L_FM_NAME    TYPE RS38L_FNAM,
      OUTPUT       TYPE SSFCOMPOP,
      LW_SSFCRESCL TYPE SSFCRESCL.
DATA: CONTROL    TYPE SSFCTRLOP,
      NTOTALLINE TYPE I,
      NPAGELINE  TYPE I VALUE 9,
      P_INDEX    LIKE SY-TABIX.
DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
      NCURRLINE       TYPE I,      "中间变量
      JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA:LT_QALS TYPE TABLE OF QALS WITH HEADER LINE.
DATA:LT_MAKT TYPE TABLE OF MAKT WITH HEADER LINE.
DATA:LT_LFA1 TYPE TABLE OF LFA1 WITH HEADER LINE.
*DATA:lt_ekpo TYPE TABLE OF ekpo WITH HEADER LINE.
DATA:LT_JEST TYPE TABLE OF JEST WITH HEADER LINE.

DATA: BEGIN OF LT_EKPO OCCURS 0,
        EKGRP TYPE EKKO-EKGRP,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        BANFN TYPE EKPO-BANFN,
        BNFPO TYPE EKPO-BNFPO,
        EKNAM TYPE T024-EKNAM,
      END OF LT_EKPO.

**********************************************
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
       ZBOX      TYPE C     ,         "选中
       EBELN     TYPE QALS-EBELN    , "采购凭证
       MBLNR     TYPE QALS-MBLNR    , "物料凭证
       ZEILE     TYPE QALS-ZEILE    , "物料凭证项目
       LIFNR     TYPE QALS-LIFNR    , "供应商
       LIFNAME1  TYPE LFA1-NAME1    , "供应商名称
*       ebeln     TYPE qals-ebeln    , "采购凭证
       EBELP     TYPE QALS-EBELP    , "采购凭证项目
       LS_KDAUF  TYPE QALS-LS_KDAUF , "销售订单
       LS_KDPOS  TYPE QALS-LS_KDPOS , "销售订单项目
       BANFN     TYPE EKPO-BANFN , "采购申请编号
       BNFPO     TYPE EKPO-BNFPO , "采购申请项目
       LS_KDNAME TYPE CHAR40 ,        "项目名称
       WERK      TYPE QALS-WERK     , "工厂
       PRUEFLOS  TYPE QALS-PRUEFLOS , "检验批
       OBJNR     TYPE QALS-OBJNR    , "对象号
       MATNR     TYPE QALS-MATNR    , "物料
       MAKTX     TYPE MAKT-MAKTX    , "物料描述
       LOSMENGE  TYPE QALS-LOSMENGE , "检验批数量
       HERKUNFT  TYPE QALS-HERKUNFT , "检验批来源
       ENSTEHDAT TYPE QALS-ENSTEHDAT, "批量建立时间
       ERSTELLER TYPE QALS-ERSTELLER, "创建者
       ZTEXT     TYPE CHAR30,         "文本
*       prtflag   TYPE c,              "是否打印
     END OF IT_OUT.


TYPES:BEGIN OF TY_HEAD,
        PRUEFLOS  TYPE QALS-PRUEFLOS,
        MATNR     TYPE QALS-MATNR,
        MAKTX     TYPE MAKT-MAKTX,
        LIFNR     TYPE QALS-LIFNR,
        LIFNAME1  TYPE LFA1-NAME1,
        MBLNR     TYPE QALS-MBLNR,
        EBELN     TYPE EBELN,
        LS_KDAUF  TYPE QALS-LS_KDAUF,
        LS_KDNAME TYPE CHAR40        ,        "项目名称
        LOSMENGE  TYPE QALS-LOSMENGE,
      END OF TY_HEAD.
DATA:WA_PRT TYPE TY_HEAD.
DATA:G_TSTCT TYPE TSTCT.
DATA LT_T001W TYPE T001W OCCURS 0 WITH HEADER LINE.

****   项目名称长文本   *****
DATA:GT_TEXT TYPE TABLE OF TLINE,
     GS_TEXT LIKE LINE OF GT_TEXT.
DATA TMP_NAME     TYPE THEAD-TDNAME.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002 .

SELECT-OPTIONS:
****************修改********************
                S_EBELN       FOR EKKO-EBELN    ,
                S_LIFNR       FOR EKKO-LIFNR    ,
***********************************
                S_MBLNR       FOR QALS-MBLNR    ,
                S_ZEILE       FOR QALS-ZEILE    ,
                S_WERK        FOR QALS-WERK     ,
                S_HERK        FOR QALS-HERKUNFT ,
                S_PRUE        FOR QALS-PRUEFLOS ,
                S_ENST        FOR QALS-ENSTEHDAT,
                S_ERST        FOR QALS-ERSTELLER,
                S_MATNR       FOR QALS-MATNR    ,
                S_CHARG       FOR QALS-CHARG    .
*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
  "去调用tcode的文本描述
  SELECT SINGLE * FROM TSTCT
    INTO G_TSTCT
    WHERE SPRSL = SY-LANGU
    AND TCODE = SY-TCODE
    .
  SY-TITLE = G_TSTCT-TTEXT.
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
  "按用户调用tcode指定报表名称
  SY-TITLE = G_TSTCT-TTEXT.
  PERFORM DATA_RETRIVEL.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.


FORM FRM_AUTH_CHECK.

  SELECT WERKS
    FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE LT_T001W
  WHERE WERKS IN S_WERK.
  LOOP AT LT_T001W WHERE WERKS IN S_WERK.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*            ID 'ACTVT' FIELD '__________'
             ID 'WERKS' FIELD LT_T001W-WERKS.
    IF SY-SUBRC <> 0.
      MESSAGE E603(FCO) WITH LT_T001W-WERKS.
    ENDIF.
  ENDLOOP.
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
*  DATA:lt_qals TYPE TABLE OF qals WITH HEADER LINE.
*  DATA:lt_makt TYPE TABLE OF makt WITH HEADER LINE.
*  DATA:lt_lfa1 TYPE TABLE OF lfa1 WITH HEADER LINE.
*  DATA:lt_ekpo TYPE TABLE OF ekpo WITH HEADER LINE.
*  DATA:lt_jest TYPE TABLE OF jest WITH HEADER LINE.

  SELECT MBLNR
         ZEILE
         LIFNR
         EBELN
         EBELP
         KONT_KDAUF AS LS_KDAUF
         KONT_KDPOS AS LS_KDPOS
         WERK
         PRUEFLOS
         OBJNR
         MATNR
         LOSMENGE
         HERKUNFT
         ENSTEHDAT
         ERSTELLER
         MENGENEINH
         GESSTICHPR
    FROM QALS
    INTO CORRESPONDING FIELDS OF TABLE LT_QALS
    WHERE  MBLNR     IN S_MBLNR
*************修改********************8
    AND    EBELN     IN S_EBELN
    AND    LIFNR     IN S_LIFNR
************************************
    AND    ZEILE     IN S_ZEILE
    AND    WERK      IN S_WERK
    AND    HERKUNFT  IN S_HERK
    AND    PRUEFLOS  IN S_PRUE
    AND    ENSTEHDAT IN S_ENST
    AND    ERSTELLER IN S_ERST
    AND    MATNR     IN S_MATNR
    AND    CHARG     IN S_CHARG
  .
  IF LT_QALS[] IS NOT INITIAL.

    SELECT EKPO~EBELN
           EKPO~EBELP
           EKPO~BANFN
           EKPO~BNFPO
           EKKO~EKGRP
           T024~EKNAM
      FROM EKKO
      INNER JOIN EKPO ON EKKO~EBELN = EKPO~EBELN
      INNER JOIN T024 ON EKKO~EKGRP = T024~EKGRP
      INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
      FOR ALL ENTRIES IN LT_QALS
      WHERE EKPO~EBELN = LT_QALS-EBELN
      AND   EKPO~EBELP = LT_QALS-EBELP
      .

    SELECT  OBJNR
            STAT
            INACT
   FROM JEST
   INTO CORRESPONDING FIELDS OF TABLE LT_JEST
    FOR ALL ENTRIES IN LT_QALS
    WHERE OBJNR = LT_QALS-OBJNR
    AND STAT IN ( 'I0224' )
    AND INACT = ''
    .

    SELECT LIFNR
            NAME1
      FROM LFA1
      INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
      FOR ALL ENTRIES IN LT_QALS
      WHERE LIFNR = LT_QALS-LIFNR
    .

    SELECT  MATNR
            MAKTX
       FROM MAKT
       INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
       FOR ALL ENTRIES IN LT_QALS
       WHERE MATNR = LT_QALS-MATNR
       AND   SPRAS = SY-LANGU
    .
  ENDIF.
  LOOP AT LT_QALS.
    CONCATENATE LT_QALS-EBELN LT_QALS-EBELP INTO TMP_NAME.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*        CLIENT                  = SY-MANDT
        ID                      = 'F04'
        LANGUAGE                = '1'
        NAME                    = TMP_NAME
        OBJECT                  = 'EKPO'
      TABLES
        LINES                   = GT_TEXT
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF GT_TEXT IS NOT INITIAL.
      LOOP AT GT_TEXT INTO GS_TEXT.
        IT_OUT-ZTEXT = GS_TEXT-TDLINE.
      ENDLOOP.
    ENDIF.

    READ TABLE LT_JEST WITH KEY OBJNR = LT_QALS-OBJNR.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    CLEAR LT_EKPO.
    READ TABLE LT_EKPO WITH KEY EBELN = LT_QALS-EBELN EBELP = LT_QALS-EBELP.
    IF SY-SUBRC = 0.
      IT_OUT-BANFN = LT_EKPO-BANFN.
      IT_OUT-BNFPO = LT_EKPO-BNFPO.
    ENDIF.

    MOVE-CORRESPONDING LT_QALS TO IT_OUT.

    CLEAR LT_MAKT.
    READ TABLE LT_MAKT WITH KEY MATNR = IT_OUT-MATNR.
    IF SY-SUBRC = 0.
      IT_OUT-MAKTX = LT_MAKT-MAKTX.
    ENDIF.

    CLEAR LT_LFA1.
    READ TABLE LT_LFA1 WITH KEY LIFNR = IT_OUT-LIFNR.
    IF SY-SUBRC = 0.
      IT_OUT-LIFNAME1 = LT_LFA1-NAME1.
    ENDIF.
    APPEND IT_OUT.
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
       "  16  ''  'PRTFLAG'     text-016   , "是否打印
           1  ''  'MBLNR'       TEXT-001   , "物料凭证
           2  ''  'ZEILE'       TEXT-002   , "物料凭证项目
           3  ''  'LIFNR'       TEXT-003   , "供应商
           4  ''  'LIFNAME1'    TEXT-017   ,  "供应商名称
           5  ''  'EBELN'       TEXT-004   , "采购凭证
           6  ''  'EBELP'       TEXT-005   , "采购凭证项目
           7  ''  'LS_KDAUF'    TEXT-006   , "销售订单
           8  ''  'LS_KDPOS'    TEXT-007   , "销售订单项目
           9  ''  'WERK'        TEXT-008   , "工厂
           10  ''  'PRUEFLOS'    TEXT-009   , "检验批
           11 ''  'MATNR'       TEXT-010   , "物料
           12 ''  'MAKTX'       TEXT-011   , "物料描述
           13 ''  'LOSMENGE'    TEXT-012   , "检验批数量
           14 ''  'HERKUNFT'    TEXT-013   , "检验批来源
           15 ''  'ENSTEHDAT'   TEXT-014   , "批量建立时间
           16 ''  'ERSTELLER'   TEXT-015   . "创建者


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
  L_LAYOUT-BOX_FIELDNAME = 'ZBOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

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
      PERFORM FRM_SAVE_DATA.
    WHEN '&PRNT'.
      IF S_WERK = 'IEQ1610'.
        PERFORM FRM_PRINT_1610.
      ELSE.
        PERFORM FRM_PRINT_DATA.
      ENDIF.
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
  SET PF-STATUS 'ZQM001_STATUS'.
ENDFORM.                    "set_pf_status


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 9,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFQM001'.
  DATA:LT_SELECT LIKE  IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA:LW_SELECT LIKE LINE OF LT_SELECT.
*  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
  DATA:LW_PRT TYPE TY_HEAD,
       LT_PRT LIKE TABLE OF LW_PRT.
  CASE SY-TCODE.
    WHEN  'ZQM001'.
      L_FORMNAME = 'ZSFQM001'.
    WHEN  'ZQM002'.
      L_FORMNAME = 'ZSFQM002'.
    WHEN OTHERS.
  ENDCASE.
  LT_SELECT[] = IT_OUT[].

*   READ TABLE lt_select WITH KEY checkbox = 'X'.
*   IF sy-subrc <> 4.
*     DELETE lt_select WHERE checkbox <> 'X'.  "删除ALV中未选择的行
*   ENDIF.
*   CHECK sy-subrc = 0.
  CLEAR:LW_PRT,LT_PRT[].

  LOOP AT LT_SELECT INTO LW_SELECT WHERE ZBOX = 'X'.
    MOVE-CORRESPONDING LW_SELECT TO LW_PRT.
    IF LW_PRT-LS_KDAUF = ''.
      IF LW_SELECT-BANFN <> ''.
        CONCATENATE LW_SELECT-BANFN LW_SELECT-BNFPO INTO LW_PRT-LS_KDNAME.
        PERFORM FRM_READ_TEXT USING LW_PRT-LS_KDNAME SY-LANGU 'B03' 'EBAN'
                     CHANGING LW_PRT-LS_KDNAME.
      ENDIF.
    ELSE.
      PERFORM FRM_READ_TEXT USING LW_PRT-LS_KDAUF SY-LANGU 'Z001' 'VBBK'
                 CHANGING LW_PRT-LS_KDNAME.
    ENDIF.

    IF LW_PRT-LS_KDNAME = ''.
      LW_PRT-LS_KDNAME = '备库'.
    ENDIF.
    APPEND LW_PRT TO LT_PRT.
    IF   SY-TCODE = 'ZQM002'.
      IF LW_SELECT-WERK EQ '1000' OR LW_SELECT-WERK EQ '1500' . ""新增1000 、1500 公司调用新增模板  ZSFQM002_1 需打印整张检验报告单 IT02 150916
        L_FORMNAME = 'ZSFQM002_1' .
      ENDIF.
    ENDIF.
  ENDLOOP.

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
*    EXIT.
  ENDIF.

  LOOP AT LT_PRT INTO WA_PRT.

    CALL FUNCTION G_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL
*       npage              = npageline
*       w_head             = lw_prt
*         TABLES
*       t_item             = lt_prt[]
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
  CLEAR T_TEXT.
  LOOP AT LT_TLINE .
    CONCATENATE T_TEXT LT_TLINE-TDLINE INTO T_TEXT SEPARATED BY SPACE.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_1610
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_1610 .

  DATA: ZXH TYPE INT4 VALUE 0.
  DATA: GT_DATA_2 LIKE TABLE OF IT_OUT WITH HEADER LINE.
  DATA: GT_DATA_3 LIKE TABLE OF IT_OUT WITH HEADER LINE.
  DATA: GT_DATA LIKE TABLE OF IT_OUT WITH HEADER LINE.
  DATA: GT_DATA_1 LIKE TABLE OF IT_OUT WITH HEADER LINE.
  "获取打印名称
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZJLX_001'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = L_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
  "   打印设置
  CONTROL-NO_OPEN   = 'X'.
  CONTROL-NO_CLOSE  = 'X'.
*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.


  OUTPUT-TDDEST = 'LP01'.
*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
  OUTPUT-RQPOSNAME = ''.
  OUTPUT-TDDATASET = ''.
  OUTPUT-TDSUFFIX1 = ''.
  OUTPUT-TDSUFFIX2 = ''.
  OUTPUT-TDIMMED   = 'X'.
  OUTPUT-TDDELETE  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
      OUTPUT_OPTIONS     = OUTPUT
*    IMPORTING
*     JOB_OUTPUT_OPTIONS = OPTION
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  READ TABLE IT_OUT WITH  KEY ZBOX = 'X'.
  APPEND LINES OF IT_OUT TO GT_DATA.
  DELETE GT_DATA WHERE ZBOX NE 'X'.
  APPEND LINES OF GT_DATA TO GT_DATA_1.
  APPEND LINES OF GT_DATA TO GT_DATA_2.
  SORT GT_DATA_1 BY EBELN.
  SORT GT_DATA_2 BY EBELN.
  LOOP AT GT_DATA_1 WHERE ZBOX = 'X'.
    GT_DATA_3-LIFNR = GT_DATA_1-LIFNR.
    GT_DATA_3-EBELN = GT_DATA_1-EBELN.
    GT_DATA_3-LIFNAME1 = GT_DATA_1-LIFNAME1.
*    GT_DATA_3-EBELP = GT_DATA_1-EBELP.
    AT NEW EBELN.
      LS_ZZS_04_10-LIFNR = GT_DATA_3-LIFNR.
      LS_ZZS_04_10-EBELN = GT_DATA_3-EBELN.
      LS_ZZS_04_10-NAME1 = GT_DATA_3-LIFNAME1.
      READ TABLE LT_EKPO WITH KEY EBELN = LT_QALS-EBELN EBELP = LT_QALS-EBELP.
      IF SY-SUBRC = 0.
        LS_ZZS_04_10-EKNAM = LT_EKPO-EKNAM.
      ENDIF.
    ENDAT.

    AT END OF EBELN.
      S_COUNT = 0.
      LOOP AT GT_DATA_2  WHERE EBELN = GT_DATA_3-EBELN .
        S_COUNT = S_COUNT + 1.
        LS_ZZS_04-MBLNR  = GT_DATA_2-MBLNR.
        LS_ZZS_04-ZEILE  = GT_DATA_2-ZEILE.
        LS_ZZS_04-PRUEFLOS = GT_DATA_2-PRUEFLOS.
        LS_ZZS_04-MATNR = GT_DATA_2-MATNR.
        LS_ZZS_04-LOSMENGE = GT_DATA_2-LOSMENGE.
        READ TABLE GT_DATA_1 WITH KEY EBELP = GT_DATA_2-EBELP.
        IF SY-SUBRC = 0.
        LS_ZZS_04-ZTEXT = GT_DATA_2-ZTEXT.
        ENDIF.
        READ TABLE LT_QALS  WITH KEY EBELN = GT_DATA_3-EBELN EBELP = GT_DATA_2-EBELP.
        IF SY-SUBRC = 0.
          LS_ZZS_04-MENGENEINH = LT_QALS-MENGENEINH.
          LS_ZZS_04-GESSTICHPR = LT_QALS-GESSTICHPR.
        ENDIF.
        ZXH = ZXH + 1.
        LS_ZZS_04-ZXH        =  ZXH.
        READ TABLE LT_MAKT WITH KEY MATNR = GT_DATA_2-MATNR.
        IF SY-SUBRC = 0.
          LS_ZZS_04-MAKTX  = LT_MAKT-MAKTX.
        ENDIF.
*      MODIFY GT_EKKO FROM WA_EKKO.
        APPEND LS_ZZS_04 TO GT_TAB.
        CLEAR LS_ZZS_04.
      ENDLOOP.
      CLEAR:ZXH.
*  判断空行
      S_COUNT = S_COUNT MOD S_PAGE.
      IF S_COUNT NE 0.
        S_COUNT = S_PAGE - S_COUNT.
        CLEAR LS_ZZS_04.
        DO S_COUNT TIMES.
          APPEND LS_ZZS_04 TO GT_TAB.
        ENDDO.
      ENDIF.

      "#  调用Smartforms的Function Module打印
      CALL FUNCTION L_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          OUTPUT_OPTIONS     = OUTPUT
          ZJLX_H             = LS_ZZS_04_10
          I_NUM              = S_PAGE
        TABLES
          GT_ZJLX            = GT_TAB
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.

      CLEAR GT_TAB.
    ENDAT.
  ENDLOOP.

  "#  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.
