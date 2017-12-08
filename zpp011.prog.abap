*----------------------------------------------------------------------------
*模块	PP
*
*请求类型           PROG:ZPP011
*内容描述           生产信息记录表
*版本       V1.0
*姓名       HANDLJ
*日期       09.03.2015 14:31:23
*-----------------------------------------------------------------------------
REPORT zpp011.

TABLES : aufk,afko,afpo.
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

TYPES:BEGIN OF ty_out,
        fevor  TYPE afko-fevor,   "生产管理员
        aufnr  TYPE afko-aufnr,   "订单号
        auart  TYPE aufk-auart,   "订单类型
        werks  TYPE aufk-werks,   "工厂
        matnr  TYPE afpo-matnr,   "抬头物料号
        ktext  TYPE aufk-ktext,   "描述
        vornr  TYPE afvc-vornr,   "工序代码
        arbpl  TYPE crhd-arbpl,   "工作中心
        stext  TYPE p1000-stext,  "工作中心描述
        steus  TYPE afvc-steus,   "工序控制码
        ltxa1  TYPE afvc-ltxa1,   "工序描述
        vsttxt TYPE co_sttxt,     "工序状态
        meins  TYPE mara-meins,   "单位
        meins2 TYPE mara-meins,  "单位
        aufpl  TYPE afko-aufpl,   "
        objnr  TYPE aufk-objnr,   "
        dispo  TYPE afko-dispo,  " MRP控制者
        zbox   TYPE c,
      END OF ty_out.

DATA:it_out TYPE ty_out OCCURS 0 WITH HEADER LINE.
DATA:it_prt TYPE TABLE OF ty_out.

*PERFORM frm_read_text USING wa_prt-ls_kdauf sy-langu '0001' 'VBBK'
*             CHANGING wa_prt-ls_kdname.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS :   p_werks TYPE aufk-werks OBLIGATORY.
SELECT-OPTIONS:s_aufnr FOR afko-aufnr,
               s_matnr FOR afko-stlbez,
               s_fevor FOR afko-fevor,
               s_dispo FOR afko-dispo,
               s_gstrp FOR afko-gstrp,
               s_gltrp FOR afko-gltrp.

*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
**GETTING DEFAULT VARIANT

INITIALIZATION.

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
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '__________'
         ID 'WERKS' FIELD  p_werks.
  IF sy-subrc <> 0.
    MESSAGE e603(fco) WITH p_werks.
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

FORM data_retrivel .

  DATA lt_out_temp LIKE it_out OCCURS 0 WITH HEADER LINE.
  DATA lt_afvc TYPE afvc OCCURS 0 WITH HEADER LINE.
  DATA lt_plpo TYPE plpo OCCURS 0 WITH HEADER LINE.
  DATA lt_crhd TYPE crhd OCCURS 0 WITH HEADER LINE.
  DATA lt_crtx TYPE crtx OCCURS 0 WITH HEADER LINE.
  DATA lt_makt TYPE makt OCCURS 0 WITH HEADER LINE.
  DATA lt_jest TYPE jest OCCURS 0 WITH HEADER LINE.

  DATA len TYPE i.
  SELECT afko~aufnr
         aufk~auart
         aufk~werks
         aufk~objnr
         afko~fevor
         afko~aufpl
         afko~dispo
         afko~gmein AS meins2
         afpo~matnr
         aufk~ktext
         mara~meins
    FROM afko JOIN aufk
    ON   afko~aufnr = aufk~aufnr
    JOIN afpo
    ON   afko~aufnr = afpo~aufnr
    LEFT JOIN mara
    ON  afko~stlbez = mara~matnr
    JOIN jest
    ON aufk~objnr = jest~objnr
    INTO CORRESPONDING FIELDS OF TABLE lt_out_temp
    WHERE afko~aufnr  IN s_aufnr
    AND   aufk~werks  = p_werks
    AND   afko~stlbez IN s_matnr
    AND   afko~fevor  IN s_fevor
    AND   afko~dispo  IN s_dispo
    AND   afko~gstrp  IN s_gstrp
    AND   afko~gltrp  IN s_gltrp
    AND   ( jest~stat = 'I0002' AND jest~inact = '' )
    .
  IF lt_out_temp[] IS NOT INITIAL.
    SELECT objnr
            stat
            inact
   FROM jest
   INTO CORRESPONDING FIELDS OF TABLE lt_jest
    FOR ALL ENTRIES IN lt_out_temp
    WHERE objnr = lt_out_temp-objnr
    AND stat IN ( 'I0013', 'I0043','I0045','I0076' )
    AND inact = ''
      .


    SELECT matnr
           maktx
      FROM makt
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
      FOR ALL ENTRIES IN lt_out_temp
      WHERE matnr = lt_out_temp-matnr
      AND   spras = sy-langu.

    SELECT aufpl
           aplzl
           plnty
           plnnr
           plnkn
           zaehl
           objnr
           vornr
           steus
           ltxa1
      FROM afvc
      INTO CORRESPONDING FIELDS OF TABLE lt_afvc
      FOR ALL ENTRIES IN lt_out_temp
      WHERE aufpl = lt_out_temp-aufpl.
    IF lt_afvc[] IS NOT INITIAL.
      SELECT  plnty
              plnnr
              plnkn
              zaehl
              arbid
        FROM plpo
        INTO CORRESPONDING FIELDS OF TABLE lt_plpo
        FOR ALL ENTRIES IN lt_afvc
        WHERE plnty = lt_afvc-plnty
        AND   plnnr = lt_afvc-plnnr
        AND   plnkn = lt_afvc-plnkn
        AND   zaehl = lt_afvc-zaehl
        .
      IF lt_plpo[] IS NOT INITIAL .
        SELECT objty
               objid
               arbpl
          FROM crhd
          INTO CORRESPONDING FIELDS OF TABLE lt_crhd
          FOR ALL ENTRIES IN lt_plpo
          WHERE objty = 'A'
          AND   objid = lt_plpo-arbid
          .

        SELECT objty
               objid
               ktext
          FROM crtx
          INTO CORRESPONDING FIELDS OF TABLE lt_crtx
          FOR ALL ENTRIES IN lt_plpo
          WHERE objty = 'A'
          AND   objid = lt_plpo-arbid
          AND   spras = sy-langu
          .
      ENDIF.
    ENDIF.

    LOOP AT  lt_afvc.
      CLEAR: lt_out_temp,it_out.
      READ TABLE lt_out_temp WITH KEY aufpl = lt_afvc-aufpl.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING lt_out_temp TO it_out.
      ENDIF.

      CLEAR lt_jest.
      READ TABLE lt_jest WITH KEY objnr = it_out-objnr.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR lt_makt.
      READ TABLE lt_makt WITH KEY matnr = it_out-matnr.
      IF sy-subrc = 0.
        it_out-ktext = lt_makt-maktx.
      ENDIF.

      CLEAR lt_plpo.
      READ TABLE lt_plpo WITH KEY plnty = lt_afvc-plnty
                                  plnnr = lt_afvc-plnnr
                                  plnkn = lt_afvc-plnkn
                                  zaehl = lt_afvc-zaehl
                                  .
      IF sy-subrc = 0.
        CLEAR lt_crhd.
        READ TABLE lt_crhd WITH KEY objid = lt_plpo-arbid.
        IF sy-subrc = 0.
          it_out-arbpl = lt_crhd-arbpl.
        ENDIF.

        CLEAR lt_crtx.
        READ TABLE lt_crtx WITH KEY objid = lt_plpo-arbid.
        IF sy-subrc = 0.
          it_out-stext = lt_crtx-ktext.
        ENDIF.
      ENDIF.
      PERFORM frm_read_state USING lt_afvc-objnr CHANGING it_out-vsttxt.

      IF it_out-meins = ''.
        it_out-meins = it_out-meins2.
      ENDIF.
      CLEAR len.
      len = strlen( it_out-ktext ).
      IF len <= 39.
        it_out-ktext+39(1) = '#'.
        REPLACE SECTION OFFSET 39 LENGTH 1 OF: it_out-ktext WITH '　'.
      ENDIF.
      it_out-vornr = lt_afvc-vornr.
      it_out-steus = lt_afvc-steus.
      it_out-ltxa1 = lt_afvc-ltxa1.
      IF it_out-steus <> 'PP02'.
        APPEND it_out.
      ENDIF.

    ENDLOOP.
  ENDIF.

  SORT it_out BY fevor aufnr vornr.
ENDFORM.                    " DATA_RETRIVEL

FORM frm_read_state USING l_objnr CHANGING l_line TYPE bsvx-sttxt.
  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
*     CLIENT                  = SY-MANDT
*     FLG_USER_STAT           = ' '
      objnr = l_objnr
*     ONLY_ACTIVE             = 'X'
      spras = sy-langu
*     BYPASS_BUFFER           = ' '
    IMPORTING
*     ANW_STAT_EXISTING       =
*     E_STSMA                 =
      line  = l_line
*     USER_LINE               =
*     STONR =
* EXCEPTIONS
*     OBJECT_NOT_FOUND        = 1
*     OTHERS                  = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*
*
*FORM browser_pai USING i_object_key
*                       VALUE(i_program)
*                       VALUE(i_screen).
*
*
*
*  DATA: lref_browser TYPE REF TO cl_co_product_browser_prodord.
*  DATA:con_prodbrowser_class_key(10) TYPE c VALUE 'PRBRW_FAUF'.
** performance: check if browser is active in general
*
*
*  CALL METHOD cl_co_product_browser_prodord=>browser_check_active
*    EXCEPTIONS
*      browser_not_active = 1
*      OTHERS             = 2.
*  CHECK sy-subrc IS INITIAL.
*
** get current instance
*  CALL METHOD cl_co_product_browser_prodord=>get_instance
*    EXPORTING
*      i_class_key         = con_prodbrowser_class_key
*      i_object_key        = i_object_key
*    CHANGING
*      cr_browser_object   = lref_browser
*    EXCEPTIONS
*      no_instance_created = 1
*      OTHERS              = 2.
*  CHECK sy-subrc IS INITIAL.
** call pai of browser
*  CALL METHOD lref_browser->browser_pai
*    EXPORTING
*      i_program = i_program
*      i_screen  = i_screen.
*
*ENDFORM.                    " BROWSER_PAI


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
       "  16  '' '' 'PRTFLAG'     text-016   , "是否打印
           1  '' 'AUFNR ' '订单号'      , "订单号
           2  '' 'AUART ' '订单类型'      , "订单类型
           3  '' 'WERKS ' '工厂   '      , "工厂
           4  '' 'DISPO ' 'MRP控制者   '      , "MRP控制者
           5  '' 'FEVOR ' '生产管理员'      , "生产管理员
           6  '' 'MATNR'  '抬头物料号'      , "抬头物料号
           7  '' 'KTEXT ' '描述'      , "描述
           8  '' 'VORNR ' '工序代码'      , "工序代码
           9  '' 'ARBPL ' '工作中心'      , "工作中心
           10 '' 'STEXT ' '工作中心描述'      , "工作中心描述
           11 '' 'STEUS ' '工序控制码'      , "工序控制码
           12 '' 'LTXA1 ' '工序描述'      , "工序描述
           13 '' 'VSTTXT' '工序状态'        "工序状态
*           14 '' 'MEINS ' '单位'        "单位
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
  lw_fieldcat-no_zero   = 'X'.
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
  l_layout-box_fieldname = 'ZBOX'.
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
  DATA lw_out LIKE LINE OF it_out.

  CASE r_ucomm.
    WHEN '&IC1'.
*      BREAK handlj.
*      IF rs_selfield-fieldname = 'VSTTXT'.
*        READ TABLE it_out INTO lw_out INDEX rs_selfield-tabindex.
*        IF sy-subrc = 0.
**          PERFORM browser_pai USING lw_out-aufnr
**                                  'SAPLCOVG'
**                                  sy-dynnr.
*        ENDIF.
*
*      ENDIF.

    WHEN '&ENTER'.

    WHEN '&PRNT'.
      PERFORM frm_print_data.
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
  SET PF-STATUS 'ZPP011_STATUS'.
ENDFORM.                    "set_pf_status


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 1,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFPP011'.
  DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
  DATA:lw_select LIKE LINE OF lt_select.
  lt_select[] = it_out[].

  CLEAR:it_prt[].
  LOOP AT lt_select INTO lw_select WHERE zbox = 'X'.
    APPEND lw_select TO it_prt.
  ENDLOOP.

  IF it_prt[] IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK it_prt[] IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "smartforms的名字
    IMPORTING
      fm_name            = g_name                "对应的smartforms的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  SORT it_prt BY fevor aufnr vornr.

*  DESCRIBE TABLE it_prt LINES ntotalline.
*  ncurrline = ntotalline MOD npageline.
*  IF  ncurrline > 0.
*    emptycount = npageline - ncurrline.
*    DO emptycount TIMES.
*      APPEND INITIAL LINE TO it_prt.
*    ENDDO.
*  ENDIF.

  CALL FUNCTION g_name
    EXPORTING
      control_parameters = control
*     npage_line         = npageline
*     w_head             = lw_prt
*         TABLES
*     t_item             = lt_prt[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

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

  LOOP AT lt_tline .
    CONCATENATE t_text lt_tline-tdline INTO t_text SEPARATED BY space.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext
