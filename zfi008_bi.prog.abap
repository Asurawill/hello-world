*&---------------------------------------------------------------------*
*& 程序名称:ZFI008_BI
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :批量导入表ZFI008
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

report zfi008_bi.


************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
*TABLES: .

************************************************************************
* Type Declaration
************************************************************************


************************************************************************
* Internal Table
************************************************************************
data: it_data type table of zfi008.

************************************************************************
* WorkArea
************************************************************************
data: wa_data type zfi008.


************************************************************************
*      DEFINITION
************************************************************************
define init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext = &2.
  gw_lvc-outputlen = &3.
*  if &4 = 'x'.
*    gw_lvc-no_zero = 'x'.
*  endif.
  gw_lvc-icon = &4.
*  gw_lvc-checkbox = &5.
*  gw_lvc-edit = &6.
  gw_lvc-fix_column =  &5.
  gw_lvc-ref_table = &6.
  gw_lvc-ref_field = &7.
  gw_lvc-datatype = &8.
*  gw_lvc-intlen = &9.
  append gw_lvc to gt_lvc.
  clear gw_lvc.
end-of-definition.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
type-pools: slis.

data: gt_lvc           type lvc_t_fcat,
      gt_sort          type lvc_t_sort,
      gw_layout        type lvc_s_layo,   "alv的格式
      gw_variant       type disvariant,
      gw_grid_settings type lvc_s_glay,
      gw_lvc           type lvc_s_fcat,
      gw_sort          type lvc_s_sort,
      gw_grid_setting  type lvc_s_glay,
      g_repid          like sy-repid,  "SY-REPID 指 当前的主程序
      gt_events        type slis_t_event with header line, "保存AVL事件
      gw_events        like line of gt_events.
data: gt_exclude type slis_t_extab,
      gs_exclude type slis_extab.

data: gr_alvgrid type ref to cl_gui_alv_grid.

data: gt_rows type lvc_t_row,
      gt_roid type lvc_t_roid,
      wa_rows type lvc_s_row,
      wa_roid type lvc_s_roid.
data: gs_variant type disvariant.

data: gw_istable type lvc_s_stbl.

************************************************************************
* Global Variant
************************************************************************
*field-symbols: <dyn_table> type standard table,        " 内表结构
*               <dyn_wa>,                               " 表头
*               <dyn_field>.
*
*data: dy_table type ref to data,
*      dy_line  type ref to data.


************************************************************************
* Constant
************************************************************************


************************************************************************
* Selection Screen
************************************************************************
selection-screen begin of block blk1 with frame title text-001.
parameter: p_file type string modif id up memory id zfi008.
selection-screen end of block blk1.


************************************************************************
* Initialization
************************************************************************
initialization.


************************************************************************
* At selection screen
************************************************************************
at selection-screen output.

at selection-screen on value-request for p_file.
  perform frm_get_path changing p_file.


at selection-screen.


************************************************************************
* Event top of page
************************************************************************
top-of-page.

************************************************************************
* Event Start of Selection
************************************************************************
start-of-selection.
  perform frm_chech_filename.             "检查文件名
  perform frm_upload.
  perform frm_display.

************************************************************************
* Event End-of selection
************************************************************************
end-of-selection.

************************************************************************
* Event  End-of page
************************************************************************
end-of-page.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*
form frm_display .
  perform init_layout.              "设置输出格式
  perform init_sort.                "设置排序、合计
  perform init_variant.             "设置变式控制
  perform frm_init_lvc.             " 初始化内表结构/ALV显示结构
  perform frm_exclude.
  perform frm_build_event.
  perform frm_output tables gt_lvc              "输出
                            gt_sort
                            it_data
                     using 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
endform.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       初始化layout参数
*----------------------------------------------------------------------*
form init_layout .
  gw_layout-zebra = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
*  gw_layout-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
endform.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       排序
*----------------------------------------------------------------------*
form init_sort .
*    clear gw_sort.
*    gw_sort-fieldname = 'WERKS'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'FEVOR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'DISPO'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
**    clear gw_sort.
**    gw_sort-fieldname = 'KUNNR'.  "排序字段
**    gw_sort-spos = 1.
**    gw_sort-up = 'X'.             "升序
***  it_sort-subtot = 'X'.         "小计依据
**    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'MATNR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
endform.                    " INIT_SORT
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       初始化变量
*----------------------------------------------------------------------*
form init_variant.
  clear: gw_variant.
  gw_variant-report = sy-repid.
  gw_variant-handle = '0001'.

  clear gw_grid_settings.
  gw_grid_settings-edt_cll_cb = 'X'.

endform.                    " INIT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_exclude .
  refresh gt_exclude.
  clear gs_exclude.
endform.                    " FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
form frm_init_lvc.

*  init_fieldcat 'BOX' text-001 '2' '' 'X' 'X' 'X' '' ''.
*  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
*  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
*  init_fieldcat 'VBELN' '' '' '' '' 'VBAP' 'VBELN' '' .

*call function 'DDIF_FIELDINFO_GET'
*  exporting
*    tabname              =
**   FIELDNAME            = ' '
**   LANGU                = SY-LANGU
**   LFIELDNAME           = ' '
**   ALL_TYPES            = ' '
**   GROUP_NAMES          = ' '
**   UCLEN                =
**   DO_NOT_WRITE         = ' '
** IMPORTING
**   X030L_WA             =
**   DDOBJTYPE            =
**   DFIES_WA             =
**   LINES_DESCR          =
** TABLES
**   DFIES_TAB            =
**   FIXED_VALUES         =
** EXCEPTIONS
**   NOT_FOUND            = 1
**   INTERNAL_ERROR       = 2
**   OTHERS               = 3
*          .
*if sy-subrc <> 0.
** Implement suitable error handling here
*endif.


endform.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
form frm_output tables pt_lvc type lvc_t_fcat
                       pt_sort type lvc_t_sort
                       pt_data
                using pu_status
                      pu_ucomm
                      pw_layout type lvc_s_layo
                      pw_variant type disvariant
                      pw_grid_settings type lvc_s_glay.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = pu_status
      i_callback_user_command  = pu_ucomm
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
      i_structure_name         = 'ZFI008'
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      i_grid_settings          = pw_grid_settings
      is_layout_lvc            = pw_layout
      it_fieldcat_lvc          = pt_lvc[]
      it_excluding             = gt_exclude
*     IT_SPECIAL_GROUPS_LVC    =
      it_sort_lvc              = pt_sort[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = pw_variant
      it_events                = gt_events[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    tables
      t_outtab                 = pt_data
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
form alv_pf_status using rt_extab type slis_t_extab.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  set pf-status 'STANDARD_FULLSCREEN' excluding rt_extab.
endform.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
form alv_user_command using r_ucomm like sy-ucomm
                            rs_selfield type slis_selfield.
  data:l_index type sy-tabix.

  case r_ucomm.
    when '&IC1'."双击
*      read table it_data into wa_data index rs_selfield-tabindex.
*      if sy-subrc = 0.
*        set parameter id 'XXXX' field wa_data-matnr.   "选择屏字段ID
*        call transaction 'XXXX' and skip first screen."填T-code
*      endif.
*    when 'PRINT'."打印
*      perform frm_print_select.
*    when '&ZALL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = 'X'.
*        modify it_data from wa_data index l_index.
*      endloop.
*    when '&ZSAL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = ''.
*        modify it_data from wa_data index l_index.
*      endloop.
    when 'IMPORT'.
      perform frm_import_data.
    when others.
  endcase.

  rs_selfield-refresh = 'X'.
endform.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_SELECT
*&---------------------------------------------------------------------*
*       打印
*----------------------------------------------------------------------*
form frm_print_select .
*  data:lt_prt type table of ty_data,
*       lv_fm_name type rs38l_fnam,        "form名
*       lwa_fp_params type sfpoutputparams,"打印参数
*       lv_result type sfpjoboutput."定义处理结果
*  refresh lt_prt.
*  loop at it_data into wa_data where box = 'X'.
*    append wa_data to lt_prt.
*  endloop.
*
*  if lt_prt[] is not initial.
*    if g_print_type = 'ADOBE'.   "当用ADOBE模式时
**      取得函数的名字
*      call function 'FP_FUNCTION_MODULE_NAME'
*        exporting
*          i_name     = 'XXXX'      "Adobe form的名字
*        importing
*          e_funcname = lv_fm_name.
*
** 这个是RFC的名字
*      lwa_fp_params-connection = 'ADS'.
*
** 打开一个打印job
*      call function 'FP_JOB_OPEN'
*        changing
*          ie_outputparams = lwa_fp_params
*        exceptions
*          cancel          = 1
*          usage_error     = 2
*          system_error    = 3
*          internal_error  = 4
*          others          = 5.
*      if sy-subrc <> 0.
*        message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      endif.
*
**调用真正的form接口函数
*      call function lv_fm_name.
*
** 关闭打印job
*      call function 'FP_JOB_CLOSE'
*        importing
*          e_result       = lv_result
*        exceptions
*          usage_error    = 1
*          system_error   = 2
*          internal_error = 3
*          others         = 4.
*      if sy-subrc <> 0.
*        message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      endif.
*
*    elseif g_print_type = 'SMART'. "当用smartforms模式时
*
*      call function 'SSF_FUNCTION_MODULE_NAME'
*        exporting
*          formname           = 'XXXX'         "smartforms的名字
*          variant            = ' '
*          direct_call        = ' '
*        importing
*          fm_name            = lv_fm_name                "对应的smartforms的函数
*        exceptions
*          no_form            = 1
*          no_function_module = 2
*          others             = 3.
*
*      wait up to 1 seconds.
*
*      call function lv_fm_name
*        exceptions
*          formatting_error = 1
*          internal_error   = 2
*          send_error       = 3
*          user_canceled    = 4
*          others           = 5.
*
*    endif.
*  else.
*  endif.
endform.                    " FRM_PRINT_SELECT
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       ALV事件
*----------------------------------------------------------------------*
form frm_build_event .
  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type = 0
    importing
      et_events   = gt_events[].

*  read table gt_events into gw_events with key name = slis_ev_top_of_page.
*  if sy-subrc = 0.
*    gw_events-form = 'ALV_TOP_OF_PAGE'.
*    modify gt_events from gw_events index sy-tabix.
*  endif.
endform.                    " FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*&      FORM  ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       ALV标题设置
*----------------------------------------------------------------------*
form alv_top_of_page .
  data: lt_listcomm type slis_t_listheader,   "保存ALV表标题
        ls_listcomm like line of lt_listcomm.

  clear   ls_listcomm.
  refresh lt_listcomm.

  ls_listcomm-typ = 'S'.
  ls_listcomm-key = ''.
  ls_listcomm-info = 'XXXX'.
  append ls_listcomm to lt_listcomm.
  clear   ls_listcomm.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = lt_listcomm[]
*     i_logo             = 'ENJOYSAP_LOGO'"LOGO设置
    .
endform. " GET_ALV_HEAD
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_FILENAME  text
*----------------------------------------------------------------------*
form frm_get_path  changing p_filename.
  " Get path for local file

  data: lt_file_table type filetable,
        lw_file_table type file_table,
        l_rc          type i.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = 'File Path'
*     default_extension       =
*     default_filename        =
*     file_filter             =
*     with_encoding           =
*     initial_directory       = 'E:\'
*     multiselection          =
    changing
      file_table              = lt_file_table
      rc                      = l_rc
*     user_action             =
*     file_encoding           =
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.
  if sy-subrc = 0.
    read table lt_file_table into lw_file_table index 1.
    if sy-subrc = 0.
      p_filename = lw_file_table-filename.
    endif.
  else.
*   Implement suitable error handling here
  endif.
endform.                    " FRM_GET_PATH
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_upload .

  data oref type ref to cx_root.
  data lv_error type string.
  " Get file data
*  call method cl_gui_frontend_services=>gui_upload
*    exporting
*      filename                = p_upfn
*      filetype                = 'ASC'
*      has_field_separator     = 'X'
**     header_length           = 0
**     read_by_line            = 'X'
**     dat_mode                = SPACE
*      codepage                = '8400'
**     ignore_cerr             = ABAP_TRUE
**     replacement             = '#'
**     virus_scan_profile      =
**    importing
**     filelength              = g_filelength
**     header                  =
*    changing
*      data_tab                = it_file
**     isscanperformed         = SPACE
*    exceptions
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      not_supported_by_gui    = 17
*      error_no_gui            = 18
*      others                  = 19.
*  if sy-subrc <> 0.
**   Implement suitable error handling here
*    message 'Read File Error!' type 'E'.
*  endif.

  data l_filename type rlgrap-filename.
  l_filename = p_file.

  call function 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = l_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 256
      i_end_row               = 65000
    tables
      intern                  = it_data
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.
  if sy-subrc <> 0.
    message '文件导入错误！' type 'I' display like 'E'.
    stop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECH_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_chech_filename .
  if p_file is initial.
*    MESSAGE i010."主数据文件，路径和文件名，不能为空！
    message '主数据文件，路径和文件名，不能为空！' type 'E'.
    stop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_IMPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_import_data .
  modify zfi008 from table it_data.
  if sy-subrc = 0.
    commit work and wait.
    message 'Update success!' type 'S'.
  endif.
endform.
