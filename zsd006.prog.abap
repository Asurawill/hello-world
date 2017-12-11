*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZSD006
*内容描述           退货交货单打印
*版本       V1.0
*姓名       HANDLJ
*日期       08.03.2015 14:26:23
*-----------------------------------------------------------------------------
REPORT zsd006.

TABLES : likp,lips.
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

        vbeln         TYPE  lips-vbeln       ,    "交货单编号
        werks         TYPE  lips-werks      ,     "工厂
        lgort         TYPE  lips-lgort       ,    "库存地点
        vgbel         TYPE  lips-vgbel       ,    "销售订单编号（项目编号）
        vgstring(255) TYPE  c               ,    "项目名称
        posnr         TYPE  lips-posnr       ,    "行项目
        matnr         TYPE  lips-matnr       ,    "物料编号
        arktx         TYPE  lips-arktx       ,    "物料描述
        lfimg         TYPE  lips-lfimg       ,    "数量
        vrkme         TYPE  lips-vrkme       ,    "单位
        charg         TYPE  lips-charg       ,    "批次
        lgobe         TYPE  t001l-lgobe      ,    "库存地点名称
        vkorg         TYPE  likp-vkorg      ,     "销售组织
        kottext       TYPE  tvkot-vtext      ,    "销售组织名称
        vstel         TYPE  likp-vstel      ,     "装运点
        stttext       TYPE  tvstt-vtext      ,    "装运点名称

        werkname1     TYPE  t001w-name1      ,    "工厂名称
        kunnr         TYPE  likp-kunnr      ,     "客户编号
        name1         TYPE  kna1-name1      ,     "客户名称
        wadat_ist     TYPE  likp-wadat_ist  ,     "入库日期
        zbox          TYPE  c                ,         "选中
*     LGORTto      type  LIPS-LGORT      ,    "库存地点
*     LGOBEto      type T001L-LGOBE     ,     "库存地点名称
      END OF ty_out.

DATA:it_out TYPE ty_out OCCURS 0 WITH HEADER LINE.
DATA:it_prt TYPE TABLE OF ty_out.
DATA:wa_prt TYPE ty_out.

*    PERFORM frm_read_text USING wa_prt-ls_kdauf sy-langu '0001' 'VBBK'
*                 CHANGING wa_prt-ls_kdname.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002 .
SELECT-OPTIONS:s_vstel FOR likp-vstel OBLIGATORY,
               s_vkorg FOR likp-vkorg,
               s_vbeln FOR likp-vbeln,
               s_kunnr FOR likp-kunnr,
               s_vgbel FOR lips-vgbel.

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
data:lt_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
  SELECT VKORG FROM tvko
    INTO CORRESPONDING FIELDS OF TABLE lt_tvko
    WHERE vkorg in s_vkorg
    .
 LOOP AT lt_tvko WHERE vkorg in s_vkorg.
   AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
            ID 'VKORG' FIELD lt_tvko-vkorg
            .
   IF sy-subrc <> 0.
      MESSAGE e430(VELO) WITH lt_tvko-vkorg.
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
  SELECT likp~vbeln
         likp~vkorg
         tvkot~vtext AS  kottext
         likp~wadat_ist
         likp~kunnr
         kna1~name1
         likp~vstel
         tvstt~vtext AS stttext
         lips~vgbel
         lips~posnr
         lips~matnr
         lips~arktx
         lips~lfimg
         lips~vrkme
         lips~werks
         t001w~name1 as werkname1
         lips~lgort
         t001l~lgobe
         lips~charg
    FROM lips JOIN likp
    ON lips~vbeln = likp~vbeln
    JOIN vbuk
    ON lips~vbeln = vbuk~vbeln
    LEFT JOIN KNA1
    on likp~kunnr = kna1~kunnr
    LEFT JOIN t001w
    ON  lips~werks = t001w~werks
    LEFT JOIN t001l
    ON  lips~werks = t001l~werks
    AND lips~lgort = t001l~lgort
    LEFT JOIN tvkot
    ON likp~vkorg = tvkot~vkorg
    AND tvkot~spras = sy-langu
    LEFT JOIN tvstt
    ON likp~vstel = tvstt~vstel
    AND tvstt~spras = sy-langu
    INTO CORRESPONDING FIELDS OF TABLE it_out
    WHERE  likp~vbeln IN s_vbeln
    AND   likp~vkorg IN s_vkorg
    AND   likp~vstel IN s_vstel
    AND   likp~kunnr IN s_kunnr
    AND   ( likp~lfart = 'ZLDR' or likp~lfart = 'RL' )
    AND   lips~vgbel IN s_vgbel
*    AND   vbuk~wbstk = 'C'                            "DELETE  BY  HANDWY  2015-7-27.
    .
  IF it_out[] IS NOT INITIAL.
    LOOP AT it_out.
     PERFORM frm_read_text USING it_out-vgbel sy-langu 'Z001' 'VBBK'
                  CHANGING it_out-vgstring.
      modify it_out.
    ENDLOOP.
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
FORM build_fieldcatalog .
  CLEAR it_fieldcat[].

  PERFORM frm_fill_cat USING :
       "  16  '' '' 'PRTFLAG'     text-016   , "是否打印
           1  '' 'VBELN    ' '交货单编号'          , "交货单编号
           2  '' 'VGBEL    ' '销售订单编号'          , "销售订单编号(项目编号)
           3  '' 'VGSTRING ' '项目名称'          , "项目名称
           4  '' 'POSNR    ' '行项目'           , "行项目
           5  '' 'MATNR    ' '物料编号'          , "物料编号
           6  '' 'ARKTX    ' '物料描述'          , "物料描述
           7  '' 'LFIMG    ' '数量'             , "数量
           8  '' 'VRKME    ' '单位'             , "单位
           9  '' 'CHARG    ' '批次'             , "批次
           10 '' 'LGORT    ' '库存地点'          , "库存地点
           11 '' 'LGOBE    ' '库存地点名称'          , "库存地点名称
           12 '' 'VKORG    ' '销售组织'          , "销售组织
           13 '' 'KOTTEXT  ' '销售组织名称'          , "销售组织名称
           14 '' 'VSTEL    ' '装运点'          , "装运点
           15 '' 'STTTEXT  ' '装运点名称'          , "装运点名称
           16 '' 'WERKS    ' '工厂'          , "工厂
           17 '' 'WERKNAME1' '工厂名称'          , "工厂名称
           18 '' 'KUNNR    ' '客户编号'          , "客户编号
           19 '' 'NAME1    ' '客户名称'          , "客户名称
           20 '' 'WADAT_IST' '入库日期'          . "入库日期


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
  CASE sy-ucomm.
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
  SET PF-STATUS 'ZSD006_STATUS'.
ENDFORM.                    "set_pf_status


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 6,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFSD006'.
  DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
  DATA:lw_select LIKE LINE OF lt_select.
  DATA:lt_prt LIKE TABLE OF it_out WITH HEADER LINE.
  FIELD-SYMBOLS <lw_prt> LIKE LINE OF it_out.
  lt_select[] = it_out[].

  CLEAR:lt_prt[].
  LOOP AT lt_select INTO lw_select WHERE zbox = 'X'.
    MOVE-CORRESPONDING lw_select TO lt_prt.
    APPEND lt_prt.
  ENDLOOP.

  IF lt_prt[] IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK lt_prt[] IS NOT INITIAL.

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

  SORT lt_prt BY vbeln
                 werks
                 lgort.

  LOOP AT lt_prt ASSIGNING <lw_prt>.
    AT NEW lgort.
      CLEAR wa_prt.
      CLEAR it_prt[].
      wa_prt = <lw_prt>.
    ENDAT.
    APPEND <lw_prt> TO it_prt[].

    AT END OF lgort.

      DESCRIBE TABLE it_prt LINES ntotalline.
      ncurrline = ntotalline MOD npageline.
      IF  ncurrline > 0.
        emptycount = npageline - ncurrline.
        DO emptycount TIMES.
          APPEND INITIAL LINE TO it_prt.
        ENDDO.
      ENDIF.

      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
          npage_line         = npageline
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
