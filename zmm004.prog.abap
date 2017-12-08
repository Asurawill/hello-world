*----------------------------------------------------------------------------
*模块	MM
*
*请求类型      PROG:ZMM004
*内容描述    	库存调拨平台
*版本       V1.0
*姓名       HANDLJ
*日期       10.02.2015 16:01:38
*-----------------------------------------------------------------------------
" modify  20160113 it02
 "ADD 过账到Z999


REPORT ZMM004 MESSAGE-ID ZMM01.
INCLUDE <ICON>.
TABLES:ZMM002I.
**INTERNAL TABLE DECLARTION
DATA :
  GR_ALV     TYPE REF TO CL_SALV_TABLE,
  GR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.


 "声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
     handle_modify  FOR EVENT data_changed OF cl_gui_alv_grid
     IMPORTING ER_DATA_CHANGED ."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.


DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      G_SAVE      TYPE C VALUE 'X',
      G_VARIANT   TYPE DISVARIANT,
      GX_VARIANT  TYPE DISVARIANT,
      G_EXIT      TYPE C,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.

DATA IT_MESAGE_TAB TYPE ESP1_MESSAGE_WA_TYPE OCCURS 5 WITH HEADER LINE. "报消息

DATA gt_event_receiver TYPE REF TO lcl_event_receiver .

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.
DATA: GW_ISTABLE TYPE LVC_S_STBL.

  "取项目描述
DATA: t_tline TYPE TABLE OF tline WITH HEADER LINE,
     s_tline type tline.

DATA:tname TYPE thead-tdname.

DATA:BEGIN OF IT_OUT OCCURS 0.
        INCLUDE TYPE ZMM002I.
DATA KDMAT  TYPE VBAP-KDMAT.
DATA SGTXT  TYPE MSEG-SGTXT.
DATA MAKTX  TYPE MAKTX.
DATA ZBOX   TYPE C.
DATA LOCKED TYPE  ICON_D.
DATA ABLAD  TYPE CHAR25.
DATA SGTXT1 TYPE CHAR50.
DATA:   END OF IT_OUT.

 DATA:IS_OUT LIKE IT_OUT.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .

SELECT-OPTIONS: S_DBDH        FOR ZMM002I-DBDH    ,
                S_VBELN       FOR ZMM002I-VBELN   ,
                S_EBELN       FOR ZMM002I-EBELN   ,
                S_ZDY         FOR ZMM002I-ZDY     ,
                S_ZDDT        FOR ZMM002I-ZDDT    ,
                S_POSTER      FOR ZMM002I-POSTER  ,
                S_POSTDT      FOR ZMM002I-POSTDT  .


PARAMETERS:RAD1 RADIOBUTTON GROUP RAD DEFAULT 'X',
           RAD2 RADIOBUTTON GROUP RAD,
           RAD3 RADIOBUTTON GROUP RAD,
           RAD4 RADIOBUTTON GROUP RAD.

SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
  IF SY-TCODE = 'ZMM0041'.
    LOOP AT SCREEN.
      IF SCREEN-NAME  = 'RAD1'
       OR SCREEN-NAME = 'RAD2'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    RAD3 = 'X'.
  ENDIF.
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

**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM DATA_RETRIVEL.
  PERFORM BUILD_FIELDCATALOG.
"  PERFORM FRM_BUILD_EVENT.
  PERFORM DISPLAY_ALV_REPORT.

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVEL .
  FIELD-SYMBOLS <LW_OUT> LIKE LINE OF IT_OUT.
  DATA:LT_MAKT TYPE MAKT OCCURS 0 WITH HEADER LINE .
  DATA:LT_VBAP TYPE VBAP OCCURS 0 WITH HEADER LINE .
  DATA:LT_EKPO TYPE EKPO OCCURS 0 WITH HEADER LINE .

  IF RAD1 = 'X'.

    SELECT
      *
      FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    WHERE DBDH IN S_DBDH
    AND   VBELN IN S_VBELN
    AND   EBELN IN S_EBELN
    AND   ZDY   IN S_ZDY
    AND   ZDDT  IN S_ZDDT
    AND   ZDELFLAG = ''
    AND   ( MBLNR = '' "未生成物料凭证
    OR    REVERSAL = 'X' ) "生成过物料凭证但已冲销
    .
    "增加采购订单删除逻辑
    IF IT_OUT[] IS   NOT INITIAL.
      SELECT EBELN
             EBELP
        INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
        FROM EKPO
        FOR ALL ENTRIES IN IT_OUT
        WHERE EBELN = IT_OUT-EBELN
        AND   EBELP = IT_OUT-EBELP
        AND   LOEKZ = 'X'
        .
      LOOP AT LT_EKPO.
        DELETE IT_OUT WHERE EBELN = LT_EKPO-EBELN
                       AND  EBELP = LT_EKPO-EBELP
                       .
      ENDLOOP.
    ENDIF.
  ELSEIF RAD2 = 'X'.
    "筛选可冲销数据
    SELECT * FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    WHERE DBDH IN S_DBDH
    AND   VBELN IN S_VBELN
    AND   EBELN IN S_EBELN
    AND   ZDY   IN S_ZDY
    AND   ZDDT  IN S_ZDDT
    AND   ZDELFLAG = ''
    AND  MBLNR <> '' AND TZPZH = '' AND REVERSAL = ''  "产生了物料凭证且未调整未冲销
    .
  ELSEIF RAD3 = 'X'.
    SELECT * FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    WHERE DBDH IN S_DBDH
    AND   VBELN IN S_VBELN
    AND   EBELN IN S_EBELN
    AND   ZDY   IN S_ZDY
    AND   ZDDT  IN S_ZDDT   "
    AND   ZDELFLAG = ''
    .
   ELSEIF RAD4 = 'X'.
    SELECT * FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    WHERE DBDH IN S_DBDH
    AND   VBELN IN S_VBELN
    AND   EBELN IN S_EBELN
    AND   ZDY   IN S_ZDY
    AND   ZDDT  IN S_ZDDT
    AND   ZDELFLAG = ''
    AND  MBLNR <> '' AND TZPZH = '' AND REVERSAL = ''   "产生了物料凭证且未调整未冲销
    .
  ENDIF.
  "对用户要处理的数据进行加锁，防止多人同时操作同一个订单数据
  IF RAD3 <> 'X'."用户查看数据不需要加锁
    PERFORM FRM_LOCK TABLES IT_OUT.
  ENDIF.

  IF IT_OUT[] IS NOT INITIAL.
    SELECT VBELN POSNR KDMAT
      FROM VBAP
      INTO CORRESPONDING FIELDS OF TABLE LT_VBAP
      FOR ALL ENTRIES IN IT_OUT
      WHERE VBELN = IT_OUT-VBELN
      AND   POSNR = IT_OUT-POSNR.

    SELECT MATNR
           MAKTX
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FROM MAKT
      FOR ALL ENTRIES IN IT_OUT
      WHERE MATNR = IT_OUT-MATNR
      AND SPRAS = SY-LANGU
      .
    LOOP AT IT_OUT ASSIGNING <LW_OUT>.
      CLEAR LT_VBAP.
      READ TABLE LT_VBAP WITH KEY VBELN = <LW_OUT>-VBELN POSNR = <LW_OUT>-POSNR.
      IF SY-SUBRC = 0.
        <LW_OUT>-KDMAT = LT_VBAP-KDMAT.
      ENDIF.

      CLEAR LT_MAKT.
      READ TABLE LT_MAKT WITH KEY  MATNR = <LW_OUT>-MATNR.
      IF SY-SUBRC = 0.
        <LW_OUT>-MAKTX = LT_MAKT-MAKTX.
      ENDIF.
      PERFORM FRM_READ_TEXT USING <LW_OUT>-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING  <LW_OUT>-SGTXT.

      <LW_OUT>-ABLAD  = <LW_OUT>-VBELN.

*      IF <LW_OUT>-ABLAD IS INITIAL .
*        <LW_OUT>-ABLAD =  <LW_OUT>-EBELN .
*      ENDIF.

      <LW_OUT>-SGTXT1 = <LW_OUT>-SGTXT.
      IF  RAD4 EQ 'X'.
      <LW_OUT>-TZCK  = 'Z999' .     "调整仓库号默认为Z999
      ENDIF.
      IF RAD1 = 'X' AND <LW_OUT>-POSTDT IS INITIAL.
         <LW_OUT>-POSTDT = SY-DATUM .    " 过账日期
        ELSEIF  RAD2 = 'X' AND <LW_OUT>-CXPOST IS INITIAL .
          <LW_OUT>-CXPOST = SY-DATUM.    "冲销过账日期
        ELSEIF  RAD4 = 'X' AND <LW_OUT>-TZPOST IS INITIAL.
         <LW_OUT>-TZPOST = SY-DATUM.    "调整过账日期
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT IT_OUT BY DBDH.
ENDFORM.                    " DATA_RETRIVEL


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
  DATA L_EDIT TYPE C.
  IF RAD1 = 'X' OR RAD2 = 'X' OR RAD4 = 'X' .
    L_EDIT = 'X'.
  ENDIF.

  PERFORM FRM_FILL_CAT USING :
           1  ''  'LOCKED'      TEXT-024 '' ''  , " 是否加锁
           2  ''  'DBDH'        TEXT-002 '' ''  , " 调拨单号
           3  ''  'ZDY'         TEXT-003 '' ''  , " 制单员
           4  ''  'ZDDT'        TEXT-004 'ZMM002I' 'ZDDT'  , " 调拨单日期
           5  ''  'VBELN'       TEXT-005 '' ''  , " 销售订单
           6  ''  'POSNR'       TEXT-006 '' ''  , " 销售订单行项目
           7  ''  'EBELN'       TEXT-007 '' ''  , " 采购订单
           8  ''  'EBELP'       TEXT-008 '' ''  , " 采购订单行项目
           9  ''  'SGTXT'       TEXT-027 '' ''  , " 项目描述
           10 ''  'MATNR'       TEXT-009 '' ''  , " 物料
           11 ''  'MAKTX'       TEXT-010 '' ''  , " 物料描述
           12 ''  'KDMAT'       TEXT-026 '' ''  , " 客户物料
           13 ''  'KWMENG'      TEXT-011 '' ''   , " 订单中数量
           14 ''  'QTYYDB'      TEXT-012 '' ''  , " 已调拨数量
           15 ''  'QTYBDB'      TEXT-013 '' ''   , " 本次调拨数量
           16 ''  'WERKS'       TEXT-014 '' ''  , " 工厂
           17 ''  'STOCTO'      TEXT-015 '' ''   , " 目标库存地点
           18 ''  'SLOCFR'      TEXT-016 '' ''  , " 来源库存地点
           19 ''  'SPCID'       TEXT-017 '' ''  , " 特殊库存标识
           20 L_EDIT  'ABLAD'   TEXT-028  '' ''  , "调拨项目
           21 L_EDIT  'SGTXT1'  TEXT-029 '' ''  , "调拨项目描述
           22 ''  'CHARG'       TEXT-018 '' ''  , " 批次号
           23 ''  'XCHPF'       TEXT-019 '' ''  , " 批次库存标识
           24 ''  'POSTDT'      TEXT-022 'ZMM002I' 'POSTDT'  , " 过账日期
           25 ''  'MBLNR'       TEXT-020 '' ''  , " 过账物料凭证
           26 ''  'CXPOST'      TEXT-033 'ZMM002I' 'CXPOST'  , " 冲销过账日期
           27 ''  'MBLNR2'      TEXT-025 '' ''   , " 冲销物料凭证
           28 ''  'POSTER'      TEXT-021 '' ''  , " 过账员
           29 ''  'REVERSAL'    TEXT-023 '' ''  , " 冲销标示
           30 ''  'TZCK'        TEXT-030 '' ''  ,  "调整仓库
           31 ''  'TZPOST'      TEXT-034 'ZMM002I' 'TZPOST'  , " 调整过账日期
           32 ''  'TZPZH'       TEXT-031 '' ''  ,  "调整凭证号
           33 ''  'TZBS'        TEXT-032 '' ''  .   "调整标识




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
  IF U_FNAME = 'LOCKED'.
    LW_FIELDCAT-ICON = 'X' .
  ENDIF.

  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  LW_FIELDCAT-REF_TABNAME = U_TABLE .
  LW_FIELDCAT-REF_FIELDNAME   = U_FIELD.
  IF  U_FNAME = 'POSTDT' AND RAD1 = 'X'.
    LW_FIELDCAT-EDIT = 'X'.      " 冲销时过账日期改为 “可编辑” MODI BY  IT02  151013
    ELSEIF U_FNAME = 'CXPOST' AND RAD2 = 'X'.
    LW_FIELDCAT-EDIT = 'X'.
    ELSEIF U_FNAME = 'TZPOST' AND RAD4 = 'X'.
    LW_FIELDCAT-EDIT = 'X'.
  ENDIF.
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
  "    IT_EVENTS                = GT_EVENTS[]
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
  DATA BDCDATA_TAB TYPE TABLE OF BDCDATA.
  DATA OPT TYPE CTU_PARAMS.
  DATA L_SUBRC LIKE SY-SUBRC.
  CASE R_UCOMM.
    WHEN '&IC1'.
      CLEAR L_SUBRC.
      READ TABLE IT_OUT INDEX RS_SELFIELD-TABINDEX.
      "检查是否被锁定
      PERFORM CHECK_LOCK CHANGING L_SUBRC.
*      IF sy-subrc = 0.
      IF L_SUBRC NE 0.
*        MESSAGE s016 DISPLAY LIKE 'W'.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'W'.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'MATNR' OR RS_SELFIELD-FIELDNAME = 'MBLNR'.
        READ TABLE IT_OUT INDEX RS_SELFIELD-TABINDEX.
        CHECK SY-SUBRC = 0.
        IF RS_SELFIELD-FIELDNAME = 'MATNR' AND IT_OUT-MATNR <> ''.
          SET PARAMETER ID 'MAT' FIELD IT_OUT-MATNR.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
        ENDIF.
        IF RS_SELFIELD-FIELDNAME = 'MBLNR' AND IT_OUT-MBLNR <> ''.
*            SET PARAMETER ID 'MBN' FIELD it_out-mblnr.
*            SET PARAMETER ID 'MJA' FIELD sy-datum(4).
          BDCDATA_TAB = VALUE #(
            ( PROGRAM  = 'SAPLMIGO' DYNPRO   = '0001' DYNBEGIN = 'X' )
            ( FNAM = 'BDC_OKCODE' FVAL = '=OK_GO' )
            ( FNAM = 'GODYNPRO-ACTION' FVAL = 'A04' )
            ( FNAM = 'GODYNPRO-REFDOC' FVAL = 'R02' )
            ( FNAM = 'BDC_SUBSCR' FVAL = 'SAPLMIGO                                2010SUB_FIRSTLINE_REFDOC' )
            ( FNAM = 'GODYNPRO-MAT_DOC' FVAL = IT_OUT-MBLNR )
            ( FNAM = 'GODYNPRO-DOC_YEAR' FVAL = SY-DATUM(4) )

            ).

          OPT-DISMODE = 'E'.
          OPT-DEFSIZE = 'X'.

          TRY.
              CALL TRANSACTION 'MIGO' WITH AUTHORITY-CHECK
                                      USING BDCDATA_TAB OPTIONS FROM OPT.
            CATCH CX_SY_AUTHORIZATION_ERROR ##NO_HANDLER.
          ENDTRY.

*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        ENDIF.
      ENDIF.
*      ENDIF.
    WHEN '&POST'.
      CLEAR IT_OUT.
*      READ TABLE it_out WITH KEY zbox = 'X' locked = icon_locked.
      CLEAR L_SUBRC.
      LOOP AT IT_OUT WHERE ZBOX = 'X'.
        "检查是否被锁定
        PERFORM CHECK_LOCK CHANGING L_SUBRC.
        "如果检查到被锁定的条目，则退出循环，报错
        IF L_SUBRC NE 0.
          EXIT.
        ENDIF.
      ENDLOOP.
*      IF sy-subrc = 0.
      IF L_SUBRC NE 0.
*        MESSAGE s016 DISPLAY LIKE 'W'.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'W'.
      ELSE.
        PERFORM FRM_POST.
      ENDIF.

    WHEN '&REVERSAL'.
      CLEAR IT_OUT.
      CLEAR L_SUBRC.
*      READ TABLE it_out WITH KEY zbox = 'X' locked = icon_locked.
      LOOP AT IT_OUT WHERE ZBOX = 'X'.
        "检查是否被锁定
        PERFORM CHECK_LOCK CHANGING L_SUBRC.
        "如果检查到被锁定的条目，则退出循环，报错
        IF L_SUBRC NE 0.
          EXIT.
        ENDIF.
      ENDLOOP.
*      IF sy-subrc = 0.
      IF L_SUBRC NE 0.
*        MESSAGE s016 DISPLAY LIKE 'W'.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'W'.
      ELSE.
        PERFORM FRM_REVERSAL.
      ENDIF.

    WHEN '&GZDZ999'.
      CLEAR IT_OUT.
*      READ TABLE it_out WITH KEY zbox = 'X' locked = icon_locked.
      CLEAR L_SUBRC.
      LOOP AT IT_OUT WHERE ZBOX = 'X'.
        "检查是否被锁定
        PERFORM CHECK_LOCK CHANGING L_SUBRC.
        "如果检查到被锁定的条目，则退出循环，报错
        IF L_SUBRC NE 0.
          EXIT.
        ENDIF.
      ENDLOOP.
*      IF sy-subrc = 0.
      IF L_SUBRC NE 0.
*        MESSAGE s016 DISPLAY LIKE 'W'.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'W'.
      ELSE.
        PERFORM FRM_POST.
      ENDIF.

    WHEN '&F03' OR '&F15'OR  '&F12'.
      PERFORM FRM_UNLOCK.
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
  DATA FCODE TYPE TABLE OF SY-UCOMM.


  IF RAD1 = 'X'.
    APPEND '&REVERSAL' TO FCODE.
     APPEND '&GZDZ999' TO FCODE.
  ELSEIF RAD2 = 'X'.
    APPEND '&POST' TO FCODE.
     APPEND '&GZDZ999' TO FCODE.
  ELSEIF RAD3 = 'X'.
    APPEND '&POST' TO FCODE.
    APPEND '&REVERSAL' TO FCODE.
     APPEND '&GZDZ999' TO FCODE.
  ELSEIF RAD4 = 'X'.

    APPEND '&POST' TO FCODE.
    APPEND '&REVERSAL' TO FCODE.
  ENDIF.
  SET PF-STATUS 'ZMM004_STATUS' EXCLUDING FCODE.
ENDFORM.                    "set_pf_status


FORM FRM_POST.
**********************************************************************

  DATA: GMHEAD LIKE BAPI2017_GM_HEAD_01.
  DATA: MAT_DOC LIKE BAPI2017_GM_HEAD_RET-MAT_DOC.
  DATA: GMCODE TYPE TABLE OF BAPI2017_GM_CODE WITH HEADER LINE.
  DATA: MTHEAD TYPE TABLE OF BAPI2017_GM_HEAD_RET WITH HEADER LINE.
  DATA: GOODSMVT_ITEM TYPE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE.
  DATA: RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: L_MESSAGE TYPE STRING.
  DATA:  LT_LOG(100) TYPE C OCCURS 0 WITH HEADER LINE.
  DATA: L_LINE TYPE I.

  DATA: LT_OUT LIKE IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA: LT_OUT_TEMP TYPE ZMM002I OCCURS 0 WITH HEADER LINE.
  DATA: L_SUBRC TYPE SY-SUBRC.
  FIELD-SYMBOLS <LW_OUT> LIKE LINE OF LT_OUT.
  FIELD-SYMBOLS <LW_OUT2> LIKE LINE OF IT_OUT.
  SORT IT_OUT BY DBDH.

  DATA L_TABIX TYPE SY-TABIX.
* GMCODE Table T158G
* 01 - MB01 - Goods Receipts for Purchase Order
* 02 - MB31 - Goods Receipts for Prod Order
* 03 - MB1A - Goods Issue
* 04 - MB1B - Transfer Posting
* 05 - MB1C - Enter Other Goods Receipt
* 06 - MB11

  GMCODE = '04'.

  CLEAR L_SUBRC.
  LOOP AT IT_OUT WHERE ZBOX = 'X'.
    IF IT_OUT-ABLAD IS INITIAL.
      L_SUBRC = 4.
    ENDIF.
    APPEND IT_OUT TO LT_OUT.
  ENDLOOP.

  READ TABLE LT_OUT WITH KEY ZBOX = 'X'.
  IF SY-SUBRC = 0.
*    GMHEAD-PSTNG_DATE = LT_OUT-ZDDT."SY-DATUM."
    "过账凭证抬头
    GMHEAD-DOC_DATE   = LT_OUT-ZDDT."
    IF  RAD1 EQ 'X'.
    GMHEAD-PSTNG_DATE = LT_OUT-POSTDT."SY-DATUM."       “modified by it02 160111
    ELSEIF RAD4 EQ 'X'.
      GMHEAD-PSTNG_DATE = LT_OUT-TZPOST.   "调整过账日期
     ENDIF.
    GMHEAD-PR_UNAME   = SY-UNAME.
  ENDIF.

  IF L_SUBRC <> 4 .
    SORT LT_OUT[] BY DBDH.
    LOOP AT LT_OUT ASSIGNING <LW_OUT>.
      AT NEW DBDH.
        CLEAR:LT_OUT_TEMP[].
        CLEAR:GOODSMVT_ITEM[].
      ENDAT.
      MOVE-CORRESPONDING <LW_OUT> TO LT_OUT_TEMP.
      GOODSMVT_ITEM-MATERIAL         = <LW_OUT>-MATNR.
      GOODSMVT_ITEM-PLANT            = <LW_OUT>-WERKS. "移出工厂
      IF RAD1 EQ 'X'.
      GOODSMVT_ITEM-STGE_LOC         = <LW_OUT>-SLOCFR."移出库位
      GOODSMVT_ITEM-MOVE_TYPE        = 'Z11'.
      ELSEIF RAD4 EQ 'X'.
         GOODSMVT_ITEM-STGE_LOC         = <LW_OUT>-STOCTO."移出库位
         GOODSMVT_ITEM-MOVE_TYPE        = '311'.
      ENDIF.
      GOODSMVT_ITEM-BATCH            = <LW_OUT>-CHARG."移出批次

      GOODSMVT_ITEM-SPEC_STOCK       = <LW_OUT>-SPCID."特殊库存标示
      GOODSMVT_ITEM-ENTRY_QNT        = <LW_OUT>-QTYBDB.
      GOODSMVT_ITEM-ENTRY_UOM        = <LW_OUT>-MEINS.
      GOODSMVT_ITEM-VAL_SALES_ORD    = <LW_OUT>-VGBEL."按单特殊库存订单
      GOODSMVT_ITEM-VAL_S_ORD_ITEM   = <LW_OUT>-VGPOS."按单特殊库存行号
      GOODSMVT_ITEM-MOVE_MAT         = <LW_OUT>-MATNR.
      GOODSMVT_ITEM-MOVE_PLANT       = <LW_OUT>-WERKS. "移入
      IF RAD1 EQ 'X'.
      GOODSMVT_ITEM-MOVE_STLOC       = <LW_OUT>-STOCTO.
      ELSEIF RAD4 EQ 'X'.
        GOODSMVT_ITEM-MOVE_STLOC       = <LW_OUT>-TZCK.
      ENDIF.
      GOODSMVT_ITEM-MOVE_BATCH       = <LW_OUT>-CHARG.

*ADD BY HANDWY 2015-9-28
      CONDENSE <LW_OUT>-ABLAD NO-GAPS.
      TRANSLATE <LW_OUT>-ABLAD TO UPPER CASE.
      GOODSMVT_ITEM-UNLOAD_PT        =  <LW_OUT>-ABLAD. "卸货点（写入销售订单号）
      GOODSMVT_ITEM-ITEM_TEXT        =  <LW_OUT>-SGTXT1. "文本写入描述
*    CONCATENATE <LW_OUT>-VBELN <LW_OUT>-SGTXT  INTO GOODSMVT_ITEM-ITEM_TEXT SEPARATED BY '-'.
*END ADD

      APPEND GOODSMVT_ITEM.
      APPEND LT_OUT_TEMP.

      AT END OF DBDH.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            GOODSMVT_HEADER  = GMHEAD
            GOODSMVT_CODE    = GMCODE
          IMPORTING
            GOODSMVT_HEADRET = MTHEAD
            MATERIALDOCUMENT = MAT_DOC
          TABLES
            GOODSMVT_ITEM    = GOODSMVT_ITEM
            RETURN           = RETURN.
*            GOODSMVT_ITEM_CWM = GT_GOODSMVT_ITEM_CWM.

        READ TABLE RETURN WITH KEY TYPE = 'E'.
        IF SY-SUBRC <> 0.
          LOOP AT LT_OUT_TEMP.
            L_TABIX =  SY-TABIX .
            READ TABLE IT_OUT ASSIGNING <LW_OUT2> WITH KEY DBDH    = LT_OUT_TEMP-DBDH
                                                          WERKS   = LT_OUT_TEMP-WERKS
                                                          VBELN   = LT_OUT_TEMP-VBELN
                                                          EBELN   = LT_OUT_TEMP-EBELN
                                                          POSNR   = LT_OUT_TEMP-POSNR
                                                          EBELP   = LT_OUT_TEMP-EBELP
                                                          MATNR   = LT_OUT_TEMP-MATNR
                                                          SLOCFR  = LT_OUT_TEMP-SLOCFR
                                                          STOCTO  = LT_OUT_TEMP-STOCTO
                                                          CHARG   = LT_OUT_TEMP-CHARG
                                                          VGBEL   = LT_OUT_TEMP-VGBEL
                                                          VGPOS   = LT_OUT_TEMP-VGPOS
                                                          .
            IF SY-SUBRC = 0.
              <LW_OUT2>-POSTER = SY-UNAME.
             " <LW_OUT2>-POSTDT = SY-DATUM.
              IF RAD1 EQ 'X'.
              <LW_OUT2>-MBLNR = MAT_DOC.
*
*              <LW_OUT2>-ZEILE =
              <LW_OUT2>-QTYYDB = <LW_OUT2>-QTYYDB + <LW_OUT2>-QTYBDB.  "过账成功将本次调拨数加入到已调拨数
              <LW_OUT2>-REVERSAL = ''.

* 冲销凭证号清空 BY HANDWY 2015-9-6
              <LW_OUT2>-MBLNR2  = ''.
               <LW_OUT2>-ZEILE1  = ''.

              LT_OUT_TEMP-ZEILE  = L_TABIX.
              LT_OUT_TEMP-POSTER = SY-UNAME.
           "   LT_OUT_TEMP-POSTDT = SY-DATUM.
              LT_OUT_TEMP-MBLNR  = MAT_DOC.
              LT_OUT_TEMP-QTYYDB = LT_OUT_TEMP-QTYYDB + LT_OUT_TEMP-QTYBDB.
              LT_OUT_TEMP-REVERSAL = ''.

* 冲销凭证号清空 BY HANDWY 2015-9-6
              LT_OUT_TEMP-MBLNR2  = ''.
              LT_OUT_TEMP-ZEILE1  = ''.

              ELSEIF RAD4 EQ 'X'.
                <LW_OUT2>-TZPZH = MAT_DOC.
                <LW_OUT2>-TZBS = 'X'.
                <lW_OUT2>-QTYYDB = 0. "已调拨单数量清零
                LT_OUT_TEMP-QTYYDB = 0. "已调拨单数量清零
                LT_OUT_TEMP-ZEILE3  = L_TABIX.
                LT_OUT_TEMP-POSTER = SY-UNAME.
                LT_OUT_TEMP-TZPZH  = MAT_DOC.
                LT_OUT_TEMP-TZBS = 'X'.

               ENDIF.


              DELETE IT_OUT INDEX SY-TABIX.
              MODIFY LT_OUT_TEMP.
            ENDIF.
          ENDLOOP.
          MODIFY ZMM002I FROM TABLE LT_OUT_TEMP.
          IF SY-SUBRC = 0.
            PERFORM FRM_MESSAGE USING '' 'S' 'ZMM01' '012' <LW_OUT>-DBDH '' '' ''.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            COMMIT WORK.
            WAIT UP TO '0.1' SECONDS.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
            ROLLBACK WORK.
          ENDIF.
        ELSE.
          LOOP AT RETURN .
            PERFORM FRM_MESSAGE USING '' RETURN-TYPE RETURN-ID RETURN-NUMBER RETURN-MESSAGE_V1 RETURN-MESSAGE_V2 RETURN-MESSAGE_V3 RETURN-MESSAGE_V4.
          ENDLOOP.
          PERFORM FRM_MESSAGE USING '' 'E' 'ZMM01' '013' <LW_OUT>-DBDH '' '' ''.
        ENDIF.
      ENDAT.
    ENDLOOP.
    IF SY-SUBRC <> 0.
      MESSAGE S007 DISPLAY LIKE 'W'.
    ELSE.
      PERFORM FRM_MESSAGE USING 'X' '' '' '' '' '' '' ''.
      PERFORM FRM_REFRESH_ALV.
    ENDIF.
  ELSE.
    MESSAGE '请输入调拨项目' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

"冲销
FORM FRM_REVERSAL.
  DATA:BEGIN OF LT_MBLNR OCCURS 0,
         MBLNR TYPE MBLNR,
         GJAHR TYPE MJAHR,"过账日期
       END OF LT_MBLNR.
  DATA: RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: LT_ZMM002I TYPE ZMM002I OCCURS 0 WITH HEADER LINE.
  DATA: LW_HEAD_RET TYPE BAPI2017_GM_HEAD_RET.
  DATA: GT_GOODSMVT_MATDOCITEM TYPE TABLE OF BAPI2017_GM_ITEM_04.
  DATA: GS_GOODSMVT_MATDOCITEM TYPE BAPI2017_GM_ITEM_04.

**********************************************************************
  LOOP AT IT_OUT WHERE ZBOX = 'X'.
    LT_MBLNR-MBLNR = IT_OUT-MBLNR.
    LT_MBLNR-GJAHR = IT_OUT-POSTDT+0(4).  "ADD it02 2016022
    APPEND: LT_MBLNR.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM LT_MBLNR.
  DATA L_FLAG TYPE C.
  DATA L_ANS.
  CLEAR L_FLAG.
  CLEAR L_ANS.

  LOOP AT LT_MBLNR.
    LOOP AT IT_OUT WHERE MBLNR = LT_MBLNR-MBLNR
    AND ZBOX = 'X'.
      MOVE-CORRESPONDING IT_OUT TO LT_ZMM002I.
      IF IT_OUT-ZEILE = '0000'.
        L_FLAG = 'X'.
        APPEND LT_ZMM002I.
        CONTINUE.
      ELSE.
        LT_ZMM002I-ZEILE = IT_OUT-ZEILE * 2 - 1.
      ENDIF.

      APPEND LT_ZMM002I.

      GS_GOODSMVT_MATDOCITEM-MATDOC_ITEM = IT_OUT-ZEILE * 2 - 1.
      APPEND GS_GOODSMVT_MATDOCITEM TO GT_GOODSMVT_MATDOCITEM.
      CLEAR GS_GOODSMVT_MATDOCITEM.
    ENDLOOP.

    IF L_FLAG  = 'X'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = '确定冲销'
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = '该调拨单需要全部冲销,请确认选中全部行项目，是否继续执行！！'
          TEXT_BUTTON_1         = '是'(B01)
*         ICON_BUTTON_1         = ' '
          TEXT_BUTTON_2         = '否'(B02)
*         ICON_BUTTON_2         = ' '
*         DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
*         USERDEFINED_F1_HELP   = ' '
*         START_COLUMN          = 25
*         START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = L_ANS
*   TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      CHECK L_ANS EQ '1'.
    ENDIF.


*新增行项目冲销 BY HANDWY 2015-10-22
    DATA:LT_GZGJAHR TYPE GJAHR.
    CLEAR:LT_GZGJAHR .
*    SELECT SINGLE MJAHR INTO LT_GZGJAHR FROM MKPF WHERE MBLNR = LT_MBLNR-MBLNR ."add it02 160111
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        MATERIALDOCUMENT    = LT_MBLNR-MBLNR
      "  MATDOCUMENTYEAR     = SY-DATUM(4)        "(1)
       " MATDOCUMENTYEAR     = LT_ZMM002I-POSTDT+0(4)  "(2) 过账凭证年度 由（2） 替换（1） MODIFIED BY IT02 160109
        MATDOCUMENTYEAR     = LT_MBLNR-GJAHR          "MODIFY IT02 20160222
        GOODSMVT_PSTNG_DATE = LT_ZMM002I-CXPOST
*       GOODSMVT_PR_UNAME   =
      IMPORTING
        GOODSMVT_HEADRET    = LW_HEAD_RET
      TABLES
        RETURN              = RETURN
        GOODSMVT_MATDOCITEM = GT_GOODSMVT_MATDOCITEM.

    READ TABLE RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      "冲销失败
      DELETE LT_ZMM002I WHERE MBLNR = LT_MBLNR-MBLNR.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      LOOP AT RETURN .
        PERFORM FRM_MESSAGE USING '' RETURN-TYPE RETURN-ID RETURN-NUMBER RETURN-MESSAGE_V1 RETURN-MESSAGE_V2 RETURN-MESSAGE_V3 RETURN-MESSAGE_V4.
      ENDLOOP.
      PERFORM FRM_MESSAGE USING '' 'E' 'ZMM01' '011' LT_MBLNR-MBLNR '' '' ''.
      DELETE  LT_MBLNR.
    ELSE.
      LOOP AT LT_ZMM002I WHERE MBLNR = LT_MBLNR-MBLNR.
        LT_ZMM002I-MBLNR2 = LW_HEAD_RET-MAT_DOC.
        LT_ZMM002I-REVERSAL = 'X'.
        LT_ZMM002I-QTYYDB = 0.
        LT_ZMM002I-ZEILE1 = LT_ZMM002I-ZEILE.
        MODIFY LT_ZMM002I.
      ENDLOOP.
      MODIFY ZMM002I FROM TABLE LT_ZMM002I.

      "冲销成功
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      COMMIT WORK.
      PERFORM FRM_MESSAGE USING '' 'S' 'ZMM01' '010' LT_MBLNR-MBLNR '' '' ''.
      WAIT UP TO '0.1' SECONDS.

    ENDIF.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    MESSAGE S007 DISPLAY LIKE 'W'.
  ELSE.
*    "----------修改数据库对应条目数据
*    SELECT * FROM zmm002i
*      INTO TABLE lt_zmm002i
*      FOR ALL ENTRIES IN lt_mblnr
*      WHERE mblnr = lt_mblnr-mblnr.

*    LOOP AT lt_zmm002i.
*      lt_zmm002i-mblnr = ''.
*      lt_zmm002i-reversal = 'X'.
*      lt_zmm002i-qtyydb = 0.
*      MODIFY lt_zmm002i.
*    ENDLOOP.

*    MODIFY ZMM002I FROM TABLE LT_ZMM002I.
*    COMMIT WORK.
    "--------------------------------------
    "------删除alv对应数据
    LOOP AT LT_MBLNR.
      DELETE IT_OUT WHERE MBLNR = LT_MBLNR-MBLNR
                    AND   ZBOX  = 'X'.
    ENDLOOP.

    PERFORM FRM_MESSAGE USING 'X' '' '' '' '' '' '' ''.
    PERFORM FRM_REFRESH_ALV.
  ENDIF.
ENDFORM.


FORM FRM_REFRESH_ALV.
  DATA: O_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA:L_IS_STABLE TYPE LVC_S_STBL.
  L_IS_STABLE-ROW = 'X'.
  L_IS_STABLE-COL = 'X'.
  IF O_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = O_GRID.
  ENDIF.
  CALL METHOD O_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = L_IS_STABLE
*     i_soft_refresh =
*    EXCEPTIONS
*     finished  = 1
*     others    = 2
    .
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM FRM_MESSAGE USING U_FLAG "是否报消息：X为报消息 空为添加消息
                       U_TYPE
                       U_MSID
                       U_MSGNO
                       U_MS1
                       U_MS2
                       U_MS3
                       U_MS4.
*010  &冲销成功！
*011  &冲销失败！
  IF U_FLAG = ''.

    IT_MESAGE_TAB-MSGID  =  U_MSID.
    IT_MESAGE_TAB-MSGTY  =  U_TYPE.
    IT_MESAGE_TAB-MSGNO  =  U_MSGNO.
    IT_MESAGE_TAB-MSGV1  =  U_MS1.
    IT_MESAGE_TAB-MSGV2  =  U_MS2.
    IT_MESAGE_TAB-MSGV3  =  U_MS3.
    IT_MESAGE_TAB-MSGV3  =  U_MS4.
    APPEND IT_MESAGE_TAB.
  ENDIF.


  IF IT_MESAGE_TAB[] IS NOT INITIAL AND U_FLAG = 'X'.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        I_MESSAGE_TAB = IT_MESAGE_TAB.
    CLEAR IT_MESAGE_TAB[].
  ENDIF.
ENDFORM.



FORM FRM_LOCK TABLES T_TABLE STRUCTURE IT_OUT.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  FIELD-SYMBOLS <LW_OUT> LIKE LINE OF T_TABLE.
  LOOP AT T_TABLE ASSIGNING <LW_OUT>.
    CALL FUNCTION 'ENQUEUE_EZMM002I'
      EXPORTING
*       MODE_ZMM002I   = 'E'
*       MANDT          = SY-MANDT
*       DBDH           =
        WERKS          = <LW_OUT>-WERKS
        VBELN          = <LW_OUT>-VBELN
        EBELN          = <LW_OUT>-EBELN
        POSNR          = <LW_OUT>-POSNR
        EBELP          = <LW_OUT>-EBELP
        MATNR          = <LW_OUT>-MATNR
*       SLOCFR         =
*       STOCTO         =
*       CHARG          =
*       VGBEL          =
*       VGPOS          =
*       ZDBNUM         =
*       X_DBDH         = ' '
*       X_WERKS        = ' '
*       X_VBELN        = ' '
*       X_EBELN        = ' '
*       X_POSNR        = ' '
*       X_EBELP        = ' '
*       X_MATNR        = ' '
*       X_SLOCFR       = ' '
*       X_STOCTO       = ' '
*       X_CHARG        = ' '
*       X_VGBEL        = ' '
*       X_VGPOS        = ' '
*       X_ZDBNUM       = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      <LW_OUT>-LOCKED = ICON_LOCKED.
    ELSE.
      <LW_OUT>-LOCKED = ICON_UNLOCKED.
*   Implement suitable error handling here
    ENDIF.
  ENDLOOP.


ENDFORM.


FORM FRM_UNLOCK.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_LOCK  CHANGING L_SUBRC.
  "判断是否被锁定
  CALL FUNCTION 'ENQUEUE_EZMM002I'
    EXPORTING
*     MODE_ZMM002I   = 'E'
*     MANDT          = SY-MANDT
*     DBDH           =
      WERKS          = IT_OUT-WERKS
      VBELN          = IT_OUT-VBELN
      EBELN          = IT_OUT-EBELN
      POSNR          = IT_OUT-POSNR
      EBELP          = IT_OUT-EBELP
      MATNR          = IT_OUT-MATNR
*     SLOCFR         =
*     STOCTO         =
*     CHARG          =
*     VGBEL          =
*     VGPOS          =
*     ZDBNUM         =
*     X_DBDH         = ' '
*     X_WERKS        = ' '
*     X_VBELN        = ' '
*     X_EBELN        = ' '
*     X_POSNR        = ' '
*     X_EBELP        = ' '
*     X_MATNR        = ' '
*     X_SLOCFR       = ' '
*     X_STOCTO       = ' '
*     X_CHARG        = ' '
*     X_VGBEL        = ' '
*     X_VGPOS        = ' '
*     X_ZDBNUM       = ' '
*     _SCOPE         = '2'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  L_SUBRC = SY-SUBRC.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_GZDZ99
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_POST_GZDZ99 .

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

FORM FRM_DATA_CHANGED USING   P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件
  CALL METHOD  GR_ALVGRID->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>MC_EVT_MODIFIED
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gt_event_receiver.
  SET HANDLER   gt_event_receiver->handle_modify FOR GR_ALVGRID.
ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_modify.
*    PERFORM refresh.
   DATA:LS_MOD_CELL TYPE LVC_S_MODI ,  "应用的修改的单元格
         stbl TYPE lvc_s_stbl.
*
*    LOOP AT gt_itab INTO wa_itab.
*      wa_itab-cc = wa_itab-bb * 2 .
*      MODIFY gt_itab FROM wa_itab.
*    ENDLOOP.
    SORT ER_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID.
    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
      AT NEW ROW_ID.
        READ TABLE IT_OUT INTO IS_OUT  INDEX LS_MOD_CELL-ROW_ID.

      ENDAT.

      CASE LS_MOD_CELL-FIELDNAME.
         WHEN 'ABLAD'."调拨项目
           "获取指定单元格改动后内容
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
               EXPORTING
                  I_ROW_ID = LS_MOD_CELL-ROW_ID
                  I_FIELDNAME = 'ABLAD'
               IMPORTING
                   E_VALUE  = IS_OUT-ABLAD.
          "获取项目描述
           CLEAR t_tline[].
           CLEAR:IS_OUT-SGTXT1.
           CONDENSE IS_OUT-ABLAD NO-GAPS.
           TRANSLATE IS_OUT-ABLAD TO UPPER CASE.
           tname = IS_OUT-ABLAD.
               CALL FUNCTION 'READ_TEXT'
                EXPORTING
     "     *       CLIENT                  = SY-MANDT
                id                      = 'Z001'
                language                = sy-langu
                name                    = tname
                object                  = 'VBBK'
          "*       ARCHIVE_HANDLE          = 0
          "*       LOCAL_CAT               = ' '
          "*   IMPORTING
          "*       HEADER                  =
          "*       OLD_LINE_COUNTER        =
                TABLES
                     lines                   = t_tline[]
                EXCEPTIONS
                     id                      = 1
                     language                = 2
                     name                    = 3
                     not_found               = 4
                     object                  = 5
                     reference_check         = 6
                     wrong_access_to_archive = 7
                     OTHERS                  = 8.

             IF t_tline[] IS NOT INITIAL.
                LOOP AT t_tline into s_tline.
                    CONCATENATE is_out-SGTXT1 s_tline-tdline INTO is_out-SGTXT1.
                    CLEAR s_tline.
                ENDLOOP.
             ENDIF.

          ENDCASE.
      "刷新数据到ALV
          MODIFY IT_OUT FROM IS_OUT INDEX LS_MOD_CELL-ROW_ID.
          CLEAR IS_OUT.

    ENDLOOP.
  "  *   稳定刷新
    stbl-row = 'X'." 基于行的稳定刷新
    stbl-col = 'X'." 基于列稳定刷新
    CALL METHOD GR_ALVGRID->refresh_table_display
      EXPORTING
        is_stable = stbl
      EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ENDMETHOD.                    "HANDLE_MODIFY
ENDCLASS.
