*&---------------------------------------------------------------------*
*& Report  ZQM003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zqm003.
TABLES : qals,qave.
TYPE-POOLS : slis.

**INTERNAL TABLE DECLARTION
DATA :
  gr_alv     TYPE REF TO cl_salv_table,
  gr_columns TYPE REF TO cl_salv_columns_table.

DATA: it_fieldcat TYPE  slis_t_fieldcat_alv WITH HEADER LINE,

      g_save      TYPE c VALUE 'X',
      g_variant   TYPE disvariant,
      gx_variant  TYPE disvariant,
      g_exit      TYPE c,
      gt_events   TYPE slis_t_event,
      gw_events   TYPE slis_alv_event.


DATA:BEGIN OF it_out OCCURS 0,
       index_num  TYPE i                ,    "序号
       vdatum     TYPE qave-vdatum      ,    "日期
       vdatum_y   TYPE numc04           ,    "年份
       vdatum_m   TYPE numc2           ,     "月份
       vdatum_d   TYPE numc2           ,     "日份
       vdatum_w   TYPE numc2           ,     "周次
       werks      TYPE qals-werk        ,    "工厂
       ls_kdauf   TYPE qals-ls_kdauf    ,    "销售项目
       ls_kdname  TYPE char40           ,    "项目名称
       prueflos   TYPE qals-prueflos    ,    "检验批号
       matkl      TYPE mara-matkl       ,    "物料组
       lifnr      TYPE qals-lifnr       ,    "供应商
       lifname1   TYPE lfa1-name1       ,    "供应商名称
       matnr      TYPE qals-matnr       ,    "物料编号
       maktx      TYPE makt-maktx       ,    "物料描述
       losmenge   TYPE qals-losmenge    ,    "来料数量
       gesstichpr TYPE qals-gesstichpr  ,    "抽检数
       position   TYPE qamr-position    ,    "不合格数
       posirate   TYPE char07           ,    "不合格率
       kurztext1  TYPE qpct-kurztext    ,    "不合格现象
       pruefbemkt TYPE qamr-pruefbemkt  ,    "不合格简述
       "       fetxt      TYPE qmfel-fetxt      ,    "不合格简述     "2015.3.14修改
       vcode      TYPE qave-vcode       ,    "最终判定代码
       vcodetxt   TYPE char07           ,    "判定
       kurztext2  TYPE qpct-kurztext    ,    "最终判定描述
       maschine   TYPE qamr-maschine    ,    "检验员
       num        TYPE i                ,    "计数
       remark     TYPE qamr-pruefbemkt  ,    "备注
*       LS_KDPOS     TYPE qals-ls_kdpos ,      "
*       werk         TYPE qals-werk     ,      "
*       prtflag   TYPE c,              "是否打印
     END OF it_out.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002 .

SELECT-OPTIONS: s_werk        FOR qave-vwerks    ,
                s_datum       FOR SY-DATUM . "  qals-zeile  更改为 日期时间段筛选日期 IT02 150818   .
*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
  .
  gx_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    g_variant = gx_variant-variant.
  ENDIF.

at SELECTION-SCREEN.
  PERFORM frm_auth_check.
**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM data_retrivel.
  PERFORM build_fieldcatalog.
  PERFORM display_alv_report.



FORM frm_auth_check.
 data lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.
SELECT werks
  FROM t001w
  INTO CORRESPONDING FIELDS OF TABLE lt_t001w
  WHERE werks in s_werk.
  LOOP AT lt_t001w WHERE werks IN s_werk.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*            ID 'ACTVT' FIELD '__________'
             ID 'WERKS' FIELD lt_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH lt_t001w-werks.
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
FORM data_retrivel .
  DATA:lt_qave  TYPE qave OCCURS 0 WITH HEADER LINE,
       lt_qals  TYPE qals OCCURS 0 WITH HEADER LINE,
       lt_qamr  TYPE qamr OCCURS 0 WITH HEADER LINE,
       lt_qmfel TYPE qmfel OCCURS 0 WITH HEADER LINE,
       lt_qpct1 TYPE qpct OCCURS 0 WITH HEADER LINE,
       lt_qpct2 TYPE qpct OCCURS 0 WITH HEADER LINE,
       lt_lfa1  TYPE lfa1 OCCURS 0 WITH HEADER LINE,
       lt_mara  TYPE mara OCCURS 0 WITH HEADER LINE,
       lt_makt  TYPE makt OCCURS 0 WITH HEADER LINE,
       lt_ekpo  TYPE ekpo OCCURS 0 WITH HEADER LINE,
       lt_jest  TYPE jest OCCURS 0 WITH HEADER LINE
       .
  DATA l_numc TYPE p LENGTH 6 DECIMALS 2. "计算不合格率 临时用
  DATA:l_vdatum TYPE sy-datum.            "临时存放月第一天

  SELECT prueflos
         kzart
         zaehler
         vwerks
         vkatart
         vcodegrp
         vcode
         vdatum
   FROM qave
   INTO CORRESPONDING FIELDS OF TABLE lt_qave
   WHERE
*    vwerks IN s_werk
        vdatum IN s_datum
*    AND prueflos = '090000000107' "测试用
    .
  IF lt_qave[] IS NOT INITIAL.

    SELECT
          katalogart
          codegruppe
          code
          sprache
          version
          gueltigab
          kurztext
      FROM qpct
      INTO CORRESPONDING FIELDS OF TABLE lt_qpct1
      FOR ALL ENTRIES IN lt_qave
      WHERE katalogart = lt_qave-vkatart
        AND codegruppe = lt_qave-vcodegrp
        AND code       = lt_qave-vcode
        AND sprache    = sy-langu
      .

    SELECT
           prueflos
           objnr
           werk
           ebeln
           ebelp
           KONT_KDAUF as ls_kdauf
           lifnr
           matnr
           losmenge
           gesstichpr
     FROM qals
     INTO CORRESPONDING FIELDS OF TABLE lt_qals
     FOR ALL ENTRIES IN lt_qave
     WHERE prueflos = lt_qave-prueflos
      AND  werk     IN s_werk
      .
    IF lt_qals[] IS NOT INITIAL.
      SELECT  objnr
              stat
              inact
     FROM jest
     INTO CORRESPONDING FIELDS OF TABLE lt_jest
      FOR ALL ENTRIES IN lt_qals
      WHERE objnr = lt_qals-objnr
      AND stat IN ( 'I0224' )
      AND inact = ''
        .

    SELECT ebeln
           ebelp
           banfn
           bnfpo
      FROM ekpo
      INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
      FOR ALL ENTRIES IN lt_qals
      WHERE ebeln = lt_qals-ebeln
      AND   ebelp = lt_qals-ebelp
      .

      SELECT  prueflos
        vorglfnr
        merknr
        position
        maschine
        pruefbemkt
    FROM qamr
    INTO CORRESPONDING FIELDS OF TABLE lt_qamr
    FOR ALL ENTRIES IN lt_qave
    WHERE prueflos = lt_qave-prueflos
    AND   vorglfnr = '1'
    AND   merknr = '10'
    .
      SELECT qmnum
             fenum
             prueflos
             fekat
             fegrp
             fecod
        FROM qmfel
        INTO CORRESPONDING FIELDS OF TABLE lt_qmfel
        FOR ALL ENTRIES IN lt_qave
        WHERE prueflos = lt_qave-prueflos
        .

      IF lt_qmfel[] IS NOT INITIAL.
        SELECT
             katalogart
             codegruppe
             code
             sprache
             version
             gueltigab
             kurztext
         FROM qpct
         INTO CORRESPONDING FIELDS OF TABLE lt_qpct2
         FOR ALL ENTRIES IN lt_qmfel
         WHERE katalogart = lt_qmfel-fekat
           AND codegruppe = lt_qmfel-fegrp
           AND code       = lt_qmfel-fecod
           AND sprache    = sy-langu
           .
      ENDIF.


      SELECT
            lifnr
            name1
        FROM lfa1
        INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
        FOR ALL ENTRIES IN lt_qals
        WHERE lifnr = lt_qals-lifnr
        .

      SELECT
          matnr
          matkl
      FROM mara
      INTO CORRESPONDING FIELDS OF TABLE lt_mara
      FOR ALL ENTRIES IN lt_qals
      WHERE matnr = lt_qals-matnr
      .

      SELECT
          matnr
          maktx
      FROM makt
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
      FOR ALL ENTRIES IN lt_qals
      WHERE matnr = lt_qals-matnr
      AND  spras = sy-langu
      .
    ENDIF.


  ENDIF.


  LOOP AT  lt_qave.
    it_out-index_num = sy-tabix.
    it_out-num      = 1.
    it_out-vdatum   = lt_qave-vdatum.
    it_out-vdatum_y = lt_qave-vdatum(4).
    it_out-vdatum_m = lt_qave-vdatum+4(2).
    it_out-vdatum_d = lt_qave-vdatum+6(2).
    CONCATENATE lt_qave-vdatum(6) '01' INTO l_vdatum.

    PERFORM frm_week_month USING l_vdatum
                                 lt_qave-vdatum
                           CHANGING it_out-vdatum_w.

    it_out-vcode = lt_qave-vcode.
    IF lt_qave-vcode = 'A0' OR lt_qave-vcode = 'A1'  .
      it_out-vcodetxt = '合格'.
    ELSE.
      it_out-vcodetxt = '不合格'.
    ENDIF.

    it_out-prueflos = lt_qave-prueflos.

    CLEAR lt_qpct1.
    LOOP AT lt_qpct1 WHERE katalogart = lt_qave-vkatart
                       AND codegruppe = lt_qave-vcodegrp
                       AND code       = lt_qave-vcode .
      CONCATENATE it_out-kurztext2   lt_qpct1-kurztext INTO it_out-kurztext2 SEPARATED BY space.
    ENDLOOP.


    CLEAR lt_qals.
    READ TABLE lt_qals WITH KEY prueflos = lt_qave-prueflos.
    IF sy-subrc = 0.
      READ TABLE lt_jest with KEY objnr = lt_qals-objnr.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      it_out-werks    = lt_qals-werk.
      CLEAR lt_qamr.
      READ TABLE lt_qamr WITH KEY prueflos = lt_qave-prueflos.
      IF sy-subrc = 0.
        it_out-position   = lt_qamr-position.
        it_out-maschine   = lt_qamr-maschine.
        it_out-pruefbemkt = lt_qamr-pruefbemkt.
      ENDIF.

      LOOP AT lt_qmfel WHERE prueflos = lt_qave-prueflos.
*        it_out-fetxt = lt_qmfel-fetxt.
        CLEAR lt_qpct2.
        READ TABLE lt_qpct2 with key katalogart = lt_qmfel-fekat
                                     codegruppe = lt_qmfel-fegrp
                                     code       = lt_qmfel-fecod
                                     .
        IF sy-subrc = 0.
          CONCATENATE it_out-kurztext1   lt_qpct2-kurztext INTO it_out-kurztext1 SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      it_out-ls_kdauf   = lt_qals-ls_kdauf.
      IF it_out-ls_kdauf = ''.
       CLEAR lt_ekpo.
       READ TABLE lt_ekpo WITH KEY ebeln = lt_qals-ebeln ebelp = lt_qals-ebelp.
       IF sy-subrc = 0.
        CONCATENATE lt_ekpo-banfn lt_ekpo-bnfpo INTO it_out-ls_kdname.
         PERFORM frm_read_text USING it_out-ls_kdname sy-langu 'B03' 'EBAN'
                              CHANGING it_out-ls_kdname.
       ENDIF.

      ELSE.
        PERFORM frm_read_text USING lt_qals-ls_kdauf sy-langu 'Z001' 'VBBK'
                              CHANGING it_out-ls_kdname.
      ENDIF.
      IF it_out-ls_kdname = ''.
        it_out-ls_kdname = '备库'.
      ENDIF.

      it_out-lifnr      = lt_qals-lifnr.
      CLEAR lt_lfa1.
      READ TABLE lt_lfa1 WITH KEY  lifnr =  lt_qals-lifnr.
      IF sy-subrc = 0.
        it_out-lifname1  = lt_lfa1-name1 .
      ENDIF.

      it_out-matnr      = lt_qals-matnr.
      CLEAR lt_mara.
      READ TABLE lt_mara WITH KEY  matnr = lt_qals-matnr.
      IF sy-subrc = 0.
        it_out-matkl = lt_mara-matkl.
      ENDIF.

      CLEAR lt_makt.
      READ TABLE lt_makt WITH KEY  matnr =  lt_qals-matnr .
      IF sy-subrc = 0.
        it_out-maktx = lt_makt-maktx .
      ENDIF.
      it_out-losmenge   = lt_qals-losmenge.
      it_out-gesstichpr = lt_qals-gesstichpr.
      IF it_out-gesstichpr <> 0.
        l_numc = it_out-position * 100 / it_out-gesstichpr .
        it_out-posirate = l_numc.
        CONCATENATE it_out-posirate '%' INTO it_out-posirate.
      ENDIF.
      APPEND it_out.
    ENDIF.
    CLEAR it_out.
  ENDLOOP.

ENDFORM.                    " DATA_RETRIVEL

"计算某月第几周
FORM frm_week_month USING  p_fdate TYPE sy-datum
                            p_date TYPE sy-datum
                          CHANGING p_num  .
  DATA:
    input_week    LIKE scal-week,
    firstday_week LIKE scal-week.

  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date = p_fdate
    IMPORTING
      week = firstday_week.

  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date = p_date
    IMPORTING
      week = input_week.

  p_num = input_week+4(2) - firstday_week+4(2) + 1.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  CLEAR it_fieldcat[].
  PERFORM frm_fill_cat USING :

           1  ''  'INDEX_NUM'             text-001   , "序号
           2  ''  'VDATUM   '             text-002   , "日期
           3  ''  'VDATUM_Y  '             text-003   , "年份
           4  ''  'VDATUM_M  '             text-004   , "月份
           5  ''  'VDATUM_D  '             text-005   , "日份
           6  ''  'VDATUM_W  '             text-006   , "周次
           7  ''  'WERKS     '             text-007   , "工厂
           8  ''  'LS_KDAUF  '             text-008   , "销售项目
           9  ''  'LS_KDNAME '             text-009   , "项目名称
           10 ''  'PRUEFLOS  '             text-010   , "检验批号
           11 ''  'MATKL     '             text-011   , "物料组
           12 ''  'LIFNR     '             text-012   , "供应商
           13 ''  'LIFNAME1  '             text-013   , "供应商名称
           14 ''  'MATNR     '               text-014   , "物料编号
           15 ''  'MAKTX     '               text-015   , "物料描述
           16 ''  'LOSMENGE  '               text-016   , "来料数量
           17 ''  'GESSTICHPR'              text-017   , "抽检数
           18 ''  'POSITION  '               text-018   , "不合格数
           19 ''  'POSIRATE  '               text-019   , "不合格率
           20 ''  'KURZTEXT1 '                text-020   , "不合格现象
           21 ''  'PRUEFBEMKT'                text-021   , "不合格简述  备注改为不合格简述
           22 ''  'VCODE     '                 text-022   , "最终判定代码
           23 ''  'VCODETXT  '             text-023   , "判定
           24 ''  'KURZTEXT2 '             text-024   , "最终判定描述
           25 ''  'MASCHINE  '             text-025   , "检验员
           26 ''  'NUM       '             text-026   , "计数
           27 ''  'REMARK'                 text-027   . "备注         2014.3.16 改
  .

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
FORM frm_fill_cat USING u_pos u_edit u_fname u_name.
  DATA:lw_fieldcat LIKE LINE OF it_fieldcat.
  lw_fieldcat-col_pos     = u_pos.
  lw_fieldcat-edit        = u_edit.
  lw_fieldcat-fieldname   = u_fname.
  lw_fieldcat-seltext_l   = u_name.
  APPEND lw_fieldcat TO it_fieldcat.
ENDFORM.                    "frm_fill_cat

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report .

  DATA:
    l_layout        TYPE  slis_layout_alv,
    l_grid_settings TYPE  lvc_s_glay.

* l_layout-CWIDTH_OPT = 'X'.
*  l_layout-box_fieldname = 'ZBOX'.
  l_layout-colwidth_optimize = 'X'.
  l_grid_settings-edt_cll_cb ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      it_fieldcat              = it_fieldcat[]
      i_save                   = 'X'
      i_grid_settings          = l_grid_settings
      is_layout                = l_layout
      is_variant               = g_variant
    TABLES
      t_outtab                 = it_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_REPORT" DISPLAY_ALV_REPORT

*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM top-of-page.
*ALV Header declarations
  DATA: t_header      TYPE slis_t_listheader,
        wa_header     TYPE slis_listheader,
        t_line        LIKE wa_header-info,
        ld_lines      TYPE i,
        ld_linesc(10) TYPE c.
* Title
  wa_header-typ  = 'H'.
  wa_header-info =  sy-title."'进料检验记录表打印'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.
* Date
  wa_header-typ  = 'S'.
  wa_header-key = 'Date: '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command  USING r_ucomm LIKE sy-ucomm
                                   rs_selfield TYPE slis_selfield.
  CASE sy-ucomm.
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
FORM frm_save_data.

ENDFORM.                    "frm_save_data

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZQM003_STATUS'.
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
FORM frm_read_text USING t_tdname t_tdspras t_tdid t_tdobject CHANGING t_text.
  DATA:lt_tline TYPE tline OCCURS 0 WITH HEADER LINE.
*  DATA:stxl LIKE stxl OCCURS 0 WITH HEADER LINE."抬头备注
  DATA l_stxl TYPE stxl.
  l_stxl-tdid     = t_tdid     .
  l_stxl-tdspras  = t_tdspras  .
  l_stxl-tdname   = t_tdname   .
  l_stxl-tdobject = t_tdobject .

*  SELECT SINGLE * FROM STXL INTO STXL
*    WHERE TDNAME = T_TDNAME AND TDID = T_TDID AND TDSPRAS = T_TDSPRAS AND TDOBJECT = T_TDOBJECT.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = l_stxl-tdid    "读取文本的id
      language                = l_stxl-tdspras "读取文本的语言
      name                    = l_stxl-tdname    "读取文本的名字
      object                  = l_stxl-tdobject
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

*  DATA: itemp LIKE thead-tdname."itemp为变量无值
  CLEAR t_text.
  LOOP AT lt_tline .
    CONCATENATE t_text lt_tline-tdline INTO t_text SEPARATED BY space.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext
