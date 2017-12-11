*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZSD003
*内容描述                   装箱清单维护打印平台
*版本       V1.0
*姓名       HANDLj
*日期       20.01.2015 14:47:20
"modified by it02 20160805 增加品能公司送货单打印 、采购订单查询条件
*-----------------------------------------------------------------------------
REPORT zsd003.

TABLES : likp,lips,ekko.
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
       vgbel       TYPE lips-vgbel    , "项目编号
       vbeln       TYPE likp-vbeln    , "交货单编号
       ebeln       TYPE ekko-ebeln    , "采购订单号
       wadat       TYPE likp-wadat    , "交货日期
       lfart       TYPE likp-lfart    , "交货单类型
       vtext1      TYPE tvlkt-vtext   , "交货单类型描述
       vstel       TYPE likp-vstel    , "装运点
       vtext2      TYPE tvstt-vtext   , "装运点描述
       vkorg       TYPE likp-vkorg    , "销售组织
       vtext3      TYPE tvkot-vtext   , "销售组织描述
       posnr       TYPE lips-posnr    , "行项目编号
       zltext(255) TYPE c             , "项目描述         文本类型=‘Z001'   ,
       zshr        TYPE ztsd003-zshr  , "收货人
       zdh         TYPE ztsd003-zdh   , "电话
       zdz         TYPE ztsd003-zdz   , "地址
       zysfs       TYPE ztsd003-zysfs , "运输方式
       zzjs        TYPE ztsd003-zzjs  , "总件数
       zxs         TYPE ztsd003-zxs   , "箱数         "appended by it02 20160802
       zfhr        TYPE ztsd003-zfhr  , "发货人
       zjh         TYPE ztsd003-zjh   , "件号
       matnr       TYPE lips-matnr    , "物料编码
       arktx       TYPE lips-arktx    , "货物名称
       lfimg       TYPE lips-lfimg    , "数量
       meins       TYPE lips-meins    , "单位
       zbz         TYPE ztsd003-zbz   , "备注
       kdmat       TYPE lips-kdmat    , "类别
       zdy         TYPE ztsd003-zdy   , "是否打印
       zbox,
     END OF it_out.

TYPES:BEGIN OF ty_print,
        vbeln       TYPE likp-vbeln    , "交货单编号
        xh          TYPE i             , "序号
        vgbel       TYPE lips-vgbel    , "项目编号
        zltext(255) TYPE c             , "项目描述         文本类型=‘Z001'   ,
        zdz         TYPE ztsd003-zdz   , "地址
        zshr        TYPE ztsd003-zshr  , "收货人
        zdh         TYPE ztsd003-zdh   , "电话
        wadat       TYPE likp-wadat    , "交货日期
        matnr       TYPE lips-matnr    , "成品编码
        arktx       TYPE lips-arktx    , "货物名称
        lfimg       TYPE lips-lfimg    , "数量
        zxs         TYPE ztsd003-zxs   , "箱数
        zysfs       TYPE ztsd003-zysfs , "运输方式
        zbz         TYPE ztsd003-zbz   , "备注
      END OF ty_print .

DATA:gt_print TYPE TABLE OF ty_print,
     gs_print TYPE ty_print.

DATA:gt_print_2 TYPE TABLE OF ty_print,
     gs_print_2 TYPE ty_print.


DATA:g_xh  TYPE i .     "全局序号


DATA:gt_print_pr TYPE TABLE OF ty_print,
     gs_print_pr TYPE ty_print.

DATA:it_prt LIKE TABLE OF it_out WITH HEADER LINE.
DATA:it_prt2 LIKE TABLE OF it_out WITH HEADER LINE.
DATA:it_vbkd LIKE TABLE OF vbkd WITH  HEADER LINE.
DATA: lw_ztsd003 TYPE ztsd003,
      lt_ztsd003 LIKE TABLE OF lw_ztsd003,
      lw_likp    TYPE likp,
      lt_likp    LIKE TABLE OF lw_likp,

      lw_lips    TYPE lips,
      lt_lips    LIKE TABLE OF lw_lips WITH HEADER LINE,

      lw_ekko    TYPE ekko,
      lt_ekko    LIKE TABLE OF lw_ekko,

      lw_ekkn    TYPE ekkn,
      lt_ekkn    LIKE TABLE OF lw_ekkn,

      lw_tvlkt   TYPE tvlkt,
      lt_tvlkt   LIKE TABLE OF lw_tvlkt,
      lw_tvstt   TYPE tvstt,
      lt_tvstt   LIKE TABLE OF lw_tvstt,
      lw_tvkot   TYPE tvkot,
      lt_tvkot   LIKE TABLE OF lw_tvkot.

DATA: BEGIN OF lt_vbap OCCURS 0,
        vbeln TYPE vbeln_vl,
        posnr TYPE posnr_vl,
        kdmat TYPE matnr_ku,
      END OF lt_vbap.

DATA: BEGIN OF lt_lips2 OCCURS 0,
        "PSTYV TYPE PSTYV ,"150730 IT02 add
        vbeln TYPE vbeln_vl,
        posnr TYPE posnr_vl,
        vgbel TYPE vgbel,
        vgpos TYPE ebelp,
      END OF lt_lips2.

DATA:g_index TYPE i .

DATA: BEGIN OF lw_vbak,
        vbeln TYPE vbeln_va,
      END OF lw_vbak,
      lt_vbak LIKE TABLE OF lw_vbak.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-025 .
SELECT-OPTIONS: s_vstel  FOR likp-vstel,
                s_vkorg  FOR likp-vkorg OBLIGATORY,
                s_vbeln  FOR likp-vbeln MATCHCODE OBJECT vmvl,
                s_wadat  FOR likp-wadat,
                s_lfart  FOR likp-lfart,
                s_vgbel  FOR lips-vgbel MATCHCODE OBJECT vmva,
                s_ebeln  FOR ekko-ebeln MATCHCODE OBJECT mekk_c.
PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
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

AT SELECTION-SCREEN.

  PERFORM frm_auth_check.

**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM data_retrivel.
  PERFORM build_fieldcatalog.
  PERFORM display_alv_report.


FORM frm_auth_check.

  DATA:lt_tvko TYPE tvko OCCURS 0 WITH HEADER LINE.
  SELECT vkorg FROM tvko
  INTO CORRESPONDING FIELDS OF TABLE lt_tvko
  WHERE vkorg IN s_vkorg
    .
  LOOP AT lt_tvko WHERE vkorg IN s_vkorg.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
             ID 'VKORG' FIELD lt_tvko-vkorg
             .
    IF sy-subrc <> 0.
      MESSAGE e430(velo) WITH lt_tvko-vkorg.
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


  RANGES l_pr_flag FOR ztsd003-zdy.

  "取交货单头
  SELECT  vbeln
          wadat
          lfart
          vstel
          vkorg
    FROM  likp
    INTO CORRESPONDING FIELDS OF TABLE lt_likp
    WHERE vstel IN s_vstel
    AND   vkorg IN s_vkorg
    AND   vbeln IN s_vbeln
    AND   wadat IN s_wadat
    AND   lfart IN s_lfart
    AND   lfart <> 'ZLDR'
    AND   lfart <> 'RL'
    .
  IF lt_likp[] IS NOT INITIAL.
    SELECT * FROM tvlkt
      INTO TABLE lt_tvlkt
      FOR ALL ENTRIES IN lt_likp
      WHERE lfart = lt_likp-lfart
      AND spras = sy-langu
      .
    SELECT * FROM tvstt
      INTO TABLE lt_tvstt
      FOR ALL ENTRIES IN lt_likp
      WHERE vstel = lt_likp-vstel
      AND spras = sy-langu
      .
    SELECT * FROM tvkot
      INTO TABLE lt_tvkot
      FOR ALL ENTRIES IN lt_likp
      WHERE vkorg = lt_likp-vkorg
      AND spras = sy-langu
      .
    SELECT vbeln
           posnr
           pstyv
           vgbel
           vgpos
           uecha
           matnr
           arktx
           lfimg
           meins
           kdmat
      FROM lips
      INTO CORRESPONDING FIELDS OF TABLE lt_lips
      FOR ALL ENTRIES IN lt_likp
      WHERE vbeln = lt_likp-vbeln
*      AND   vgbel IN s_vgbel
      .
    IF lt_lips[] IS  NOT INITIAL.
      "add it_vbkd  IT02 150730
      SELECT vbeln  posnr bstkd
        INTO CORRESPONDING FIELDS OF TABLE it_vbkd
        FROM vbkd
        FOR ALL ENTRIES IN lt_lips
        WHERE vbeln = lt_lips-vgbel.
      SORT it_vbkd BY vbeln posnr.
      LOOP AT lt_lips INTO lw_lips.
        MOVE-CORRESPONDING lw_lips TO lt_lips2.
        APPEND lt_lips2.
      ENDLOOP.

      SELECT * FROM ztsd003
        INTO TABLE lt_ztsd003
        FOR ALL ENTRIES IN lt_lips
        WHERE vbeln = lt_lips-vbeln
        AND   posnr = lt_lips-posnr
*      AND   zdy in l_pr_flag
        .

      SELECT vbeln FROM vbak
        INTO TABLE lt_vbak
        FOR ALL ENTRIES IN lt_lips
        WHERE vbeln = lt_lips-vgbel
        .

      SELECT vbeln
             vbelp
             ebeln
             ebelp
        FROM ekkn
        INTO CORRESPONDING FIELDS OF TABLE lt_ekkn
        FOR ALL ENTRIES IN lt_lips2
        WHERE ebeln = lt_lips2-vgbel
        AND   ebelp = lt_lips2-vgpos
        .
      IF lt_ekkn[] IS NOT INITIAL.
        SELECT vbeln
               posnr
               kdmat
          FROM vbap
          INTO CORRESPONDING FIELDS OF TABLE lt_vbap
          FOR ALL ENTRIES IN lt_ekkn
          WHERE vbeln = lt_ekkn-vbeln
          AND   posnr = lt_ekkn-vbelp
          .
      ENDIF.

    ENDIF.

    SELECT * INTO TABLE  lt_ekko
      FROM ekko
      WHERE ebeln  IN s_ebeln .
    SORT lt_ekko BY ebeln .
  ENDIF.
  LOOP AT lt_lips INTO lw_lips WHERE  uecha = ''.
    g_index = sy-tabix .   "当前表索引行
    CLEAR it_out.
    CLEAR lw_likp.

    READ TABLE lt_likp INTO lw_likp WITH KEY vbeln = lw_lips-vbeln.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING lw_likp TO it_out.
      CLEAR lw_tvlkt.
      READ TABLE lt_tvlkt INTO lw_tvlkt WITH KEY lfart = it_out-lfart.
      IF sy-subrc = 0.
        it_out-vtext1 = lw_tvlkt-vtext.
      ENDIF.

      CLEAR lw_tvstt.
      READ TABLE lt_tvstt INTO lw_tvstt WITH KEY vstel = it_out-vstel.
      IF sy-subrc = 0.
        it_out-vtext2 = lw_tvstt-vtext.
      ENDIF.

      CLEAR lw_tvkot.
      READ TABLE lt_tvkot INTO lw_tvkot WITH KEY vkorg = it_out-vkorg.
      IF sy-subrc = 0.
        it_out-vtext3 = lw_tvkot-vtext.
      ENDIF.
    ENDIF.

*   it_out-VBELN  =  lw_lips-VBELN.
    it_out-posnr  =  lw_lips-posnr.
    it_out-matnr  =  lw_lips-matnr.
    it_out-arktx  =  lw_lips-arktx.
    it_out-lfimg  =  lw_lips-lfimg.
    it_out-meins  =  lw_lips-meins.
    it_out-kdmat  =  lw_lips-kdmat.


    CLEAR lw_vbak.
    READ TABLE lt_vbak INTO lw_vbak WITH KEY vbeln = lw_lips-vgbel.
    IF sy-subrc <> 0.
      CLEAR lw_ekkn.

      READ TABLE lt_ekko INTO lw_ekko WITH KEY ebeln = lw_lips-vgbel BINARY SEARCH .
      IF sy-subrc EQ 0 .
        it_out-ebeln = lw_ekko-ebeln .
      ENDIF.

      READ TABLE lt_ekkn INTO lw_ekkn WITH KEY ebeln = lw_lips-vgbel ebelp = lw_lips-vgpos.
      IF sy-subrc = 0.
        it_out-vgbel  =  lw_ekkn-vbeln.
        IF it_out-kdmat = ''.
          CLEAR lt_vbap.
          READ TABLE lt_vbap WITH KEY vbeln = lw_ekkn-vbeln posnr = lw_ekkn-vbelp.
          IF sy-subrc = 0.
            it_out-kdmat = lt_vbap-kdmat.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.


      it_out-vgbel  =  lw_lips-vgbel.

    ENDIF.

    "判断参照的采购订单号是否在选择屏幕的采购订单号范围内
    IF it_out-ebeln NOT IN s_ebeln .
      CONTINUE.
    ENDIF.

    IF it_out-vgbel NOT IN s_vgbel.
      CONTINUE.
    ENDIF.
    "批次行数量计算
    LOOP AT lt_lips WHERE vbeln = lw_lips-vbeln AND uecha = lw_lips-posnr.
      it_out-lfimg = it_out-lfimg + lt_lips-lfimg.
    ENDLOOP.

    "销售组织为'2110‘ 备注默认为：lips-kdmat .
    IF it_out-vkorg EQ '2110' OR it_out-vkorg EQ '2100' OR it_out-vkorg EQ '2120'.
      it_out-zbz = lw_lips-kdmat .
    ENDIF.

    CLEAR lw_ztsd003.
    READ TABLE lt_ztsd003 INTO lw_ztsd003 WITH KEY vbeln = lw_lips-vbeln
                                                    posnr = lw_lips-posnr.
    IF sy-subrc = 0.
      it_out-zdy    = lw_ztsd003-zdy  .
      it_out-zshr   = lw_ztsd003-zshr .
      it_out-zdh    = lw_ztsd003-zdh  .
      it_out-zdz    = lw_ztsd003-zdz  .
      it_out-zysfs  = lw_ztsd003-zysfs.
      it_out-zzjs   = lw_ztsd003-zzjs .
      it_out-zfhr   = lw_ztsd003-zfhr .
      it_out-zjh    = lw_ztsd003-zjh  .
      it_out-zbz    = lw_ztsd003-zbz  .
      it_out-zxs    = lw_ztsd003-zxs  .    "箱数
    ENDIF.
    CLEAR it_out-zltext.
    IF p_sel1 = 'X' AND it_out-zdy = 'X'.

      PERFORM frm_read_text USING it_out-vgbel sy-langu 'Z001' 'VBBK' CHANGING it_out-zltext.
      APPEND it_out.
    ENDIF.
    IF p_sel2 = 'X' AND it_out-zdy = ''.
      PERFORM frm_read_text USING it_out-vgbel sy-langu 'Z001' 'VBBK' CHANGING it_out-zltext.
      APPEND it_out.
    ENDIF.
  ENDLOOP.
  DELETE it_out WHERE vgbel NOT IN s_vgbel.
  SORT it_out BY vbeln posnr.
ENDFORM.                    " DATA_RETRIVEL

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
                 1  ''  'ZDY'      text-026   '8', "'打印标示'   ,
                 2  ''  'VGBEL'    text-001   '10', "'项目编号'   ,
                 3  ''  'ZLTEXT'   text-002   '40', "'项目描述'   ,
                 4  ''  'VBELN'    text-003   '10', "'交货单编号'
                 5  ''  'EBELN'    text-025   '10', "'采购订单编号'
                 6  ''  'WADAT'    text-004   '10', "'交货日期'   ,
                 7  ''  'LFART'    text-005   '10', "'交货单类型'
                 8  ''  'VTEXT1'   text-006   '40', "'交货单类型描述'
                 9  ''  'VSTEL'    text-007   '10', "'装运点'   ,
                 10  ''  'VTEXT2'   text-008   '20', "'装运点描述'
                 11 ''  'VKORG'    text-009   '8', "'销售组织'   ,
                 12 ''  'VTEXT3'   text-010   '20', "'销售组织描述'
                 13 'X' 'ZSHR'     text-011   '10', "'收货人'   ,
                 14 'X' 'ZDH'      text-012   '20', "'电话'   ,
                 15 'X' 'ZDZ'      text-013   '40', "'地址'   ,
                 16 'X' 'ZYSFS'    text-014   '10', "'运输方式'   ,
                 17 'X' 'ZZJS'     text-015   '10', "'总件数'   ,
                 17 'X' 'ZXS'      text-027   '10', "'箱数'   ,
                 18 'X' 'ZFHR'     text-016   '10', "'发货人'   ,
                 19 ''  'POSNR'    text-017   '10', "'行项目编号'
                 20 'X' 'ZJH'      text-018   '10', "'件号'   ,
                 21 ''  'MATNR'    text-019   '10', "'物料编码'   ,
                 22 ''  'ARKTX'    text-020   '20', "'货物名称'   ,
                 23 ''  'LFIMG'    text-021   '8', "'数量'   ,
                 24 ''  'MEINS'    text-022   '4', "'单位'   ,
                 25 'X' 'ZBZ'      text-023   '20', "'备注'   ,
                 26 ''  'KDMAT'    text-024   '40'. "'类别'   .
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
FORM frm_fill_cat USING u_pos u_edit u_fname u_name u_lenth.
  DATA:lw_fieldcat LIKE LINE OF it_fieldcat.
  lw_fieldcat-col_pos     = u_pos.
  lw_fieldcat-edit        = u_edit.
  lw_fieldcat-fieldname   = u_fname.
  lw_fieldcat-seltext_l   = u_name.
  lw_fieldcat-outputlen   = u_lenth.
  APPEND lw_fieldcat TO it_fieldcat.
ENDFORM.                    "frm_fill_cat




*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report .
  DATA:
    l_layout        TYPE  slis_layout_alv,
    l_grid_settings TYPE lvc_s_glay.

  l_layout-box_fieldname = 'ZBOX'.
  l_grid_settings-edt_cll_cb ='X'.
  PERFORM frm_create_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      it_fieldcat              = it_fieldcat[]
      i_save                   = 'X'
      i_grid_settings          = l_grid_settings
      it_events                = gt_events
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
  wa_header-info =  sy-title."'装箱单维护打印平台'.
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
      PERFORM frm_save_data.
    WHEN '&PRNT'.
      IF s_vkorg-low EQ '2110' OR  s_vkorg-low EQ '2120' OR  s_vkorg-low EQ '2100'.
        PERFORM frm_print_data_pn.  "品能打印模板
      ELSE.
        PERFORM frm_print_data.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_save_data.
  DATA l_out LIKE LINE OF it_out.
  DATA lt_ztsd003 TYPE ztsd003 OCCURS 0 WITH HEADER LINE.
  LOOP AT  it_out INTO l_out.
    CLEAR lt_ztsd003.
    MOVE-CORRESPONDING l_out TO lt_ztsd003.
    APPEND lt_ztsd003.
  ENDLOOP.
  MODIFY ztsd003 FROM TABLE lt_ztsd003.
  IF sy-subrc = 0.
    MESSAGE s002(z001).
  ENDIF.
ENDFORM.                    "frm_save_data
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSD003_STATUS'.
ENDFORM.                    "set_pf_status


"自动按交货单第一行录入数据处理其他行项目录入信息
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_data_enter USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol..
  DATA: l_grid TYPE REF TO cl_gui_alv_grid,
        stbl   TYPE lvc_s_stbl.

  DATA l_out LIKE LINE OF it_out.
  DATA lt_out LIKE TABLE OF it_out WITH HEADER LINE.

  FIELD-SYMBOLS <l_chang> TYPE any.
  ASSIGN er_data_changed->mp_mod_rows->* TO <l_chang>.
  lt_out[] = <l_chang>.

  FIELD-SYMBOLS <l_out> LIKE LINE OF it_out.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.
  "call METHOD l_grid->CHECK_CHANGED_DATA.

  SORT it_out BY vbeln posnr.
  LOOP AT it_out ASSIGNING <l_out>.
    AT NEW vbeln.
      l_out = <l_out>.
      CLEAR lt_out.
      READ TABLE lt_out WITH KEY vbeln = <l_out>-vbeln posnr = <l_out>-posnr.
      IF sy-subrc = 0.
        l_out-zshr  = lt_out-zshr .
        l_out-zdh   = lt_out-zdh  .
        l_out-zdz   = lt_out-zdz  .
        l_out-zysfs = lt_out-zysfs.
        l_out-zzjs  = lt_out-zzjs .
        l_out-zfhr  = lt_out-zfhr .
        l_out-zxs   = lt_out-zxs .
      ENDIF.
    ENDAT.
    <l_out>-zshr   = l_out-zshr .
    <l_out>-zdh    = l_out-zdh  .
    <l_out>-zdz    = l_out-zdz  .
    <l_out>-zysfs  = l_out-zysfs.
    <l_out>-zzjs   = l_out-zzjs .
    <l_out>-zfhr   = l_out-zfhr .
    <l_out>-zxs    = l_out-zxs  .
  ENDLOOP.
  stbl-col = 'X'.
  stbl-row = 'X'.
  CALL METHOD l_grid->refresh_table_display
    EXPORTING
      is_stable = stbl.

ENDFORM.                    "frm_data_enter


*&---------------------------------------------------------------------**&
*Form  frm_create_events
*&---------------------------------------------------------------------**
FORM frm_create_events.

  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.
  APPEND gw_events TO gt_events.
ENDFORM.                    "frm_create_events


*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM frm_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  PERFORM frm_data_enter USING er_data_changed..
ENDFORM.     "frm_data_changed

*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 8,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFSD003'.
  DATA:lt_ztsd003 TYPE ztsd003 OCCURS 0 WITH HEADER LINE.
  DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
  DATA:lw_select LIKE LINE OF lt_select.
*  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
  DATA:lw_prt LIKE LINE OF it_prt.

  lt_select[] = it_out[].

*   READ TABLE lt_select WITH KEY checkbox = 'X'.
*   IF sy-subrc <> 4.
*     DELETE lt_select WHERE checkbox <> 'X'.  "删除ALV中未选择的行
*   ENDIF.
*   CHECK sy-subrc = 0.
  CLEAR:lw_prt,it_prt[].
  DATA:bztmp TYPE char100.
  LOOP AT lt_select INTO lw_select WHERE zbox = 'X'.
    CLEAR  bztmp.
    MOVE-CORRESPONDING lw_select TO lw_prt.
    "150730 add begin  备注

    READ TABLE lt_lips WITH KEY vbeln = lw_prt-vbeln posnr = lw_prt-posnr .
    IF sy-subrc = 0.
      IF lt_lips-pstyv = 'Z11' OR lt_lips-pstyv = 'Z12' OR  lt_lips-pstyv = 'Z13' OR  lt_lips-pstyv = 'Z14'
         OR  lt_lips-pstyv = 'Z15' OR  lt_lips-pstyv = 'Z16' OR  lt_lips-pstyv = 'Z17' OR  lt_lips-pstyv = 'Z18'.
        READ TABLE it_vbkd WITH KEY vbeln = lt_lips-vgbel posnr = lt_lips-vgpos .
        IF sy-subrc = 0 . .
          bztmp = it_vbkd-bstkd .
        ELSE.
          READ TABLE it_vbkd WITH KEY vbeln = lt_lips-vgbel  posnr = '0000'.
          IF sy-subrc = 0.
            bztmp = it_vbkd-bstkd .
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    IF lw_prt-zbz NE '' .
      IF bztmp NE '' .
        CONCATENATE  lw_prt-zbz '_'  bztmp INTO  lw_prt-zbz .
      ENDIF.
    ELSE.
      lw_prt-zbz = bztmp .
    ENDIF.
    "150730 add end
    APPEND lw_prt TO it_prt.
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

  SORT it_prt[] BY vgbel vbeln posnr.

  LOOP AT it_prt.

    AT NEW vgbel.
      CLEAR : it_prt,
              ncurrline,
              emptycount,
              lw_prt,
              it_prt2,
              it_prt2[].
      npageline = 2.

    ENDAT.
    MOVE it_prt TO lw_prt.
    APPEND lw_prt TO it_prt2.
    AT END OF vgbel.
      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
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
    ENDAT.
  ENDLOOP.

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
    LOOP AT it_prt2 INTO lw_prt.
      lw_prt-zdy = 'X'.
      CLEAR lt_ztsd003.
      MOVE-CORRESPONDING lw_prt TO lt_ztsd003.
      APPEND lt_ztsd003.
    ENDLOOP.
    MODIFY ztsd003 FROM TABLE lt_ztsd003.
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
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA_PN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data_pn .
  "打印
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 8,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname .
  DATA:lt_ztsd003 TYPE ztsd003 OCCURS 0 WITH HEADER LINE.
  DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
  DATA:lw_select LIKE LINE OF lt_select.

*  L_FORMNAME = 'ZSFSD003_PN'.
  l_formname = 'ZSFSD003_PN_A4'.
  CLEAR:gs_print,gt_print[].

  lt_select[] = it_out[].
  DELETE lt_select WHERE zbox NE 'X'.

  LOOP AT lt_select .
    CLEAR:gs_print .
    MOVE-CORRESPONDING lt_select TO gs_print .
    APPEND gs_print TO gt_print .
  ENDLOOP.

  SORT gt_print BY vbeln .


  IF gt_print IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK gt_print IS NOT INITIAL.

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

  SORT it_prt[] BY vgbel vbeln posnr.

  LOOP AT gt_print INTO gs_print .
    g_xh = g_xh + 1.
    AT NEW vbeln.
      CLEAR :
              ncurrline,
              emptycount,
              gs_print_pr,
              gt_print_pr[].
      "  npageline = 2.

    ENDAT.
    CLEAR:gs_print_pr.
    MOVE gs_print TO gs_print_pr.
    gs_print_pr-xh = g_xh .
    APPEND gs_print_pr TO gt_print_pr.

    AT END OF vbeln .
      IF g_xh MOD 7 EQ 0 .

      ENDIF.
      IF g_xh MOD 7 > 0 AND g_xh MOD 7 < 6 .
        DO 7 - g_xh TIMES .
          g_xh = g_xh + 1 .
          CLEAR:gs_print_pr.
          gs_print_pr-xh  = g_xh  .
          APPEND gs_print_pr TO gt_print_pr .
        ENDDO .
      ENDIF.

      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
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
      CLEAR:g_xh .
    ENDAT.
  ENDLOOP.

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
    LOOP AT lt_select INTO lw_select.
      CLEAR lt_ztsd003.

      MOVE-CORRESPONDING lw_select TO lt_ztsd003.
      lt_ztsd003-zdy = 'X'.
      APPEND lt_ztsd003.
    ENDLOOP.
    MODIFY ztsd003 FROM TABLE lt_ztsd003.
  ENDIF.
ENDFORM.
