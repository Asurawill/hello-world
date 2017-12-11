*&---------------------------------------------------------------------*
*& 程序名称:ZSD004
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :合同料单批导
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

report zsd004.


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
types:begin of ty_file,
        vbeln(20),
        posnr(6),
        matnr(18),
        arktx(40),
        kwmeng(19),
        vrkme(3),
        edatu(10),
        uepos(6),   " 物料单结构中的上层项目
        pstyv(4),
        ltext(200),
      end of ty_file.

types:begin of ty_data,
        vbeln      type vbap-vbeln,
        posnr      type vbap-posnr,
        matnr      type vbap-matnr,
        arktx      type vbap-arktx,
        kwmeng     type vbap-kwmeng,
        vrkme      type vbap-vrkme,
        edatu      type vbep-edatu,
        uepos      type vbap-uepos,
        pstyv      type vbap-pstyv,
        ltext(200),

*        maktx      type makt-maktx,
        ptktx      type vbap-arktx,
        status(20),
        msg(400),
      end of ty_data.


************************************************************************
* Internal Table
************************************************************************
data: it_file type table of ty_file,
      it_data type table of ty_data.

data: it_makt type table of makt,
      it_vbap type table of vbap,
      it_vbak type table of vbak,
      it_vbep type table of vbep.

************************************************************************
* WorkArea
************************************************************************
data: wa_file type ty_file,
      wa_data type ty_data.

data: wa_makt type makt,
      wa_vbap type vbap,
      wa_vbak type vbak,
      wa_vbep type vbep.

field-symbols <fs_data> type ty_data.

************************************************************************
*      BAPI Variant
************************************************************************
data: g_salesdocument type bapivbeln-vbeln.

data: it_order_item_in    type table of bapisditm,
      it_order_item_inx   type table of bapisditmx,
      it_schedule_lines   type table of bapischdl,
      it_schedule_linesx  type table of bapischdlx,
      it_conditions_in    type table of bapicond,
      it_conditions_inx   type table of bapicondx,
      it_order_cfgs_ref   type table of bapicucfg,
      it_order_cfgs_inst  type table of bapicuins,
      it_order_cfgs_value type table of bapicuval,
      it_order_text       type table of bapisdtext,
      it_return           type table of bapiret2.

data: wa_order_header_in  type bapisdh1,
      wa_order_header_inx type bapisdh1x,
      wa_order_item_in    type bapisditm,
      wa_order_item_inx   type bapisditmx,
      wa_schedule_lines   type bapischdl,
      wa_schedule_linesx  type bapischdlx,
      wa_conditions_in    type bapicond,
      wa_conditions_inx   type bapicondx,
      wa_order_cfgs_ref   type bapicucfg,
      wa_order_cfgs_inst  type bapicuins,
      wa_order_cfgs_value type bapicuval,
      wa_order_text       type bapisdtext,
      wa_return           type bapiret2.

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
  gw_lvc-qfieldname =  &5.
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
parameter: p_up type c radiobutton group g1 default 'X' user-command ucomm,
           p_down type c radiobutton group g1.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-002.
parameter: p_upfn type string modif id up memory id zsd004.
selection-screen end of block blk2.

selection-screen begin of block blk3 with frame title text-003.
parameter: p_dofn type string modif id do.
selection-screen end of block blk3.


************************************************************************
* Initialization
************************************************************************
initialization.


************************************************************************
* At selection screen
************************************************************************
at selection-screen output.
  loop at screen.
    if screen-group1 = 'UP'.
      if p_up = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    elseif screen-group1 = 'DO'.
      if p_down = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    endif.
  endloop.

at selection-screen on value-request for p_upfn.
  perform frm_get_path changing p_upfn.

at selection-screen on value-request for p_dofn.
  perform frm_get_do_file changing p_dofn.

at selection-screen.


************************************************************************
* Event top of page
************************************************************************
top-of-page.

************************************************************************
* Event Start of Selection
************************************************************************
start-of-selection.
  if p_up = 'X'.
    perform frm_chech_filename.             "检查文件名
    perform frm_upload.
    perform frm_get_data.
    perform frm_display.
  else.
    perform frm_download.
  endif.

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
  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
  init_fieldcat 'VBELN' '' '' '' '' 'VBAP' 'VBELN' '' .
  init_fieldcat 'POSNR' '' '' '' '' 'VBAP' 'POSNR' '' .
  init_fieldcat 'MATNR' '' '' '' '' 'VBAP' 'MATNR' '' .
  init_fieldcat 'ARKTX' '' '' '' '' 'MAKT' 'MAKTX' '' .
  init_fieldcat 'KWMENG' '' '' '' 'VRKME' 'VBAP' 'KWMENG' '' .
  init_fieldcat 'VRKME' '' '' '' '' 'VBAP' 'VRKME' '' .
  init_fieldcat 'EDATU' '' '' '' '' 'VBEP' 'EDATU' '' .
  init_fieldcat 'UEPOS' '' '' '' '' 'VBAP' 'UEPOS' '' .
  init_fieldcat 'PTKTX' '' '' '' '' 'VBAP' 'ARKTX' '' .
  init_fieldcat 'PSTYV' '' '' '' '' 'VBAP' 'PSTYV' '' .
  init_fieldcat 'LTEXT' '行项目文本' '' '' '' '' '' '' .


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
*     I_STRUCTURE_NAME         = ''
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
  delete rt_extab where fcode = '&ALL'.
  delete rt_extab where fcode = '&SAL'.
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
  l_filename = p_upfn.

  call function 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = l_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 256
      i_end_row               = 65000
    tables
      intern                  = it_file
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.
  if sy-subrc <> 0.
    message '文件导入错误！' type 'I' display like 'E'.
    stop.
  endif.

  try .

      move-corresponding it_file to it_data.

    catch cx_sy_conversion_no_number into oref.
      clear lv_error.
      lv_error = oref->get_text( ) .
      message lv_error  type 'E' .

  endtry.

endform.                    " FRM_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  frm_call_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_call_bapi .
  call function 'BAPI_SALESORDER_CHANGE'
    exporting
      salesdocument         = g_salesdocument
      order_header_inx      = wa_order_header_inx
      int_number_assignment = 'X'
    tables
      return                = it_return
      order_item_in         = it_order_item_in
      order_item_inx        = it_order_item_inx
      schedule_lines        = it_schedule_lines
      schedule_linesx       = it_schedule_linesx
      order_text            = it_order_text
*     conditions_in         = it_conditions_in
*     conditions_inx        = it_conditions_inx
    .

  read table it_return transporting no fields with key type = 'E'.
  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

endform.                    " frm_call_bapi
*&---------------------------------------------------------------------*
*&      Form  frm_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_write .
  loop at it_data into wa_data.
    write: / 'SO:', wa_data-vbeln, 'Result:', wa_data-msg.
  endloop.
endform.                    " frm_write
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_download .
  data: l_objdata     like wwwdatatab,
        l_mime        like w3mime,
        l_destination like rlgrap-filename,
        l_objnam      type string,
        l_rc          like sy-subrc,
        l_errtxt      type string.

  data: l_filename type string,
        l_result,
        l_subrc    type sy-subrc.

  data: l_objid type wwwdatatab-objid .


  l_objid = 'ZSD004'.  "上传的模版名称

  "查找文件是否存在。
  select single relid objid
    from wwwdata
    into corresponding fields of l_objdata
    where srtf2    = 0
    and   relid    = 'MI'
    and   objid    = l_objid.

  "判断模版不存在则报错
  if sy-subrc ne 0 or l_objdata-objid eq space.
    concatenate '模板文件：' l_objid '不存在，请用TCODE：SMW0进行加载'
    into l_errtxt.
    message e000(su) with l_errtxt.
  endif.

  l_filename = p_dofn.

  "判断本地地址是否已经存在此文件。
  call method cl_gui_frontend_services=>file_exist
    exporting
      file                 = l_filename
    receiving
      result               = l_result
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      others               = 5.
  if l_result eq 'X'.  "如果存在则删除原始文件，重新覆盖
    call method cl_gui_frontend_services=>file_delete
      exporting
        filename             = l_filename
      changing
        rc                   = l_subrc
      exceptions
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        others               = 9.
    if l_subrc <> 0. "如果删除失败，则报错。
      concatenate '同名EXCEL文件已打开' '请关闭该EXCEL后重试。'
      into l_errtxt.
      message e000(su) with l_errtxt.
    endif.
  endif.

  l_destination   = p_dofn.

  "下载模版。
  call function 'DOWNLOAD_WEB_OBJECT'
    exporting
      key         = l_objdata
      destination = l_destination
    importing
      rc          = l_rc.
  if l_rc ne 0.
    concatenate '模板文件' '下载失败' into l_errtxt.
    message e000(su) with l_errtxt.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DO_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_DOFN  text
*----------------------------------------------------------------------*
form frm_get_do_file  changing p_fullpath type string.

  data: l_init_path  type string,
        l_init_fname type string,
        l_path       type string,
        l_filename   type string,
        l_fullpath   type string.

* 初始名称(输出的文件名称)
  l_init_fname = 'ZSD004_Template.xlsx'.

* 获取桌面路径
  call method cl_gui_frontend_services=>get_desktop_directory
    changing
      desktop_directory    = l_init_path
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.
  if sy-subrc <> 0.
    exit.
  endif.

* 用户选择名称、路径
  call method cl_gui_frontend_services=>file_save_dialog
    exporting
*     window_title         = '指定保存文件名'
*     default_extension    = 'DOC'
      default_file_name    = l_init_fname
*     FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
*     FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_WORD
      initial_directory    = l_init_path
      prompt_on_overwrite  = 'X'
    changing
      filename             = l_filename
      path                 = l_path
      fullpath             = l_fullpath
*     USER_ACTION          =
*     FILE_ENCODING        =
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.
  if sy-subrc = 0.
    p_fullpath = l_fullpath.
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
  if p_upfn is initial.
*    MESSAGE i010."主数据文件，路径和文件名，不能为空！
    message '主数据文件，路径和文件名，不能为空！' type 'E'.
    stop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  sort it_data by vbeln posnr.

  loop at it_data assigning <fs_data>.
    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = <fs_data>-matnr
      importing
        output       = <fs_data>-matnr
      exceptions
        length_error = 1
        others       = 2.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_data>-vbeln
      importing
        output = <fs_data>-vbeln.
  endloop.

  select *
    into table it_makt
    from makt
    for all entries in it_data
    where matnr = it_data-matnr
      and spras = '1'.
  sort it_makt by matnr.

  select *
    into table it_vbap
    from vbap
    for all entries in it_data
    where vbeln = it_data-vbeln
      and posnr = it_data-uepos.
  sort it_vbap by vbeln posnr.

  select *
  into table it_vbak
  from vbak
  for all entries in it_data
  where vbeln = it_data-vbeln.
  sort it_vbak by vbeln.

  select *
    into table it_vbep
    from vbep
    for all entries in it_data
    where vbeln = it_data-vbeln.
  sort it_vbep by vbeln posnr etenr.

  loop at it_data assigning <fs_data>.
    if <fs_data>-matnr is not initial.
      read table it_makt into wa_makt with key matnr = <fs_data>-matnr binary search.
      if sy-subrc = 0.
*      <fs_data>-maktx = wa_makt-maktx.  " 取物料描述
        <fs_data>-arktx = wa_makt-maktx.  " 取物料描述
      endif.
    endif.

    read table it_vbap into wa_vbap with key vbeln = <fs_data>-vbeln
                                             posnr = <fs_data>-uepos
                                             binary search.
    if sy-subrc = 0.
      <fs_data>-ptktx = wa_vbap-arktx.  " 取屏体描述
    endif.

    if <fs_data>-matnr is initial and <fs_data>-arktx is initial.
      if <fs_data>-matnr is initial and <fs_data>-arktx is initial.
        <fs_data>-status = icon_red_light.
        <fs_data>-msg = '物料号或描述不能都为空！'.
      endif.
    elseif <fs_data>-vbeln is initial or
      <fs_data>-kwmeng is initial or
      <fs_data>-uepos is initial.

      <fs_data>-status = icon_red_light.
      if <fs_data>-vbeln is initial.
        <fs_data>-msg = '销售订单号必输！'.
      endif.
      if <fs_data>-kwmeng is initial.
        <fs_data>-msg = '数量必输！'.
      endif.
      if <fs_data>-uepos is initial.
        <fs_data>-msg = '上层项目必输！'.
      endif.
    else.
      <fs_data>-status = icon_yellow_light.
      <fs_data>-msg = '未导入'.
    endif.
  endloop.
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
  data: l_msg   type bapi_msg,
        l_index type i.

  data: l_total    type i,
        l_cur      type i,
        l_totc     type n length 8,
        l_curc     type n length 8,
        l_perc     type i,
        l_progress type c length 40.

  data: l_dataerr     type c,
        l_errmsg(400).

  data: l_updateflag type updkz_d.

  data: l_posnr type vbap-posnr.

  l_total = lines( it_data ).

  clear wa_order_header_inx.
  wa_order_header_inx-updateflag = 'U'.   " 设置抬头更新标识

  loop at it_data into wa_data.
    at new vbeln.
      l_index = sy-tabix.

      clear: g_salesdocument.

      refresh: it_return,
               it_order_item_in, it_order_item_inx,
               it_schedule_lines, it_schedule_linesx,
               it_order_text,
               it_conditions_in, it_conditions_inx,
               it_order_cfgs_ref, it_order_cfgs_inst, it_order_cfgs_value.

      g_salesdocument = wa_data-vbeln.

      clear l_dataerr.

      read table it_vbak into wa_vbak with key vbeln = wa_data-vbeln binary search.
      if  sy-subrc ne 0.
        l_dataerr = 'X'.

        l_errmsg = 'SO号不存在!'.
      endif.

*      clear l_posnr.
      l_posnr = '100010'.
    endat.

    l_cur = l_cur + 1.
    l_curc = l_cur.
    l_perc = l_cur * 100 / l_total.
    concatenate '正在处理：' l_curc '/' l_totc into l_progress.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = l_perc
        text       = l_progress.

*    read table it_vbap transporting no fields with key vbeln = wa_data-vbeln
*                                                       posnr = wa_data-posnr
*                                                       binary search.
*    if sy-subrc = 0.
*      l_updateflag = 'U'.
*    else.
    l_updateflag = 'I'.
*    endif.

    l_posnr = l_posnr + 10.
    wa_data-posnr = l_posnr.

    " 行项目数据
    clear: wa_order_item_in, wa_order_item_inx.

    wa_order_item_in-itm_number = wa_data-posnr.
    wa_order_item_inx-itm_number = wa_data-posnr.

    wa_order_item_in-hg_lv_item = wa_data-uepos.
    wa_order_item_inx-hg_lv_item = 'X'.

*    wa_order_item_inx-updateflag = 'I'.
    wa_order_item_inx-updateflag = l_updateflag.

    wa_order_item_in-item_categ = wa_data-pstyv.
    if wa_order_item_in-item_categ is not initial.
      wa_order_item_inx-item_categ = 'X'.
    endif.

    if wa_data-matnr is not initial.
      wa_order_item_in-material = wa_data-matnr.
      wa_order_item_inx-material = 'X'.
    else.
      wa_order_item_in-short_text = wa_data-arktx.
      wa_order_item_inx-short_text = 'X'.
    endif.

    if wa_data-vrkme is not initial.
      wa_order_item_in-sales_unit = wa_data-vrkme.
      wa_order_item_inx-sales_unit = 'X'.
    endif.

    wa_order_item_in-cust_mat35 = wa_data-ptktx.
    wa_order_item_inx-cust_mat35 = 'X'.

    append wa_order_item_in to it_order_item_in.
    append wa_order_item_inx to it_order_item_inx.

    " 计划行
    clear: wa_schedule_lines, wa_schedule_linesx.

    if wa_data-edatu ca '.-/'.
      l_dataerr = 'X'.

      l_errmsg = '日期格式错误!'.
    endif.

    wa_schedule_lines-itm_number = wa_data-posnr.
    wa_schedule_lines-req_date = wa_data-edatu.
    wa_schedule_lines-req_qty = wa_data-kwmeng.

    wa_schedule_linesx-itm_number = wa_data-posnr.
*    wa_schedule_linesx-updateflag = 'I'.
    wa_schedule_linesx-updateflag = l_updateflag.
    if wa_schedule_lines-req_date ne ''.
      wa_schedule_linesx-req_date = 'X'.
    endif.
    wa_schedule_linesx-req_qty = 'X'.

    " 取计划行号
    wa_schedule_lines-sched_line = '0001'.
    wa_schedule_linesx-sched_line = '0001'.

    append wa_schedule_lines to it_schedule_lines.
    append wa_schedule_linesx to it_schedule_linesx.

    clear wa_order_text.
    if wa_data-ltext is not initial.
      wa_order_text-doc_number = wa_data-vbeln.
      wa_order_text-itm_number = wa_data-posnr.
      wa_order_text-text_id = '0006'.
      wa_order_text-langu = '1'.
      wa_order_text-text_line = wa_data-ltext(132).
      append wa_order_text to it_order_text.

      if strlen( wa_data-ltext ) > 132.
        wa_order_text-text_line = wa_data-ltext+132.
        append wa_order_text to it_order_text.
      endif.
    endif.

    at end of vbeln.
      if l_dataerr = 'X'.
        wa_data-status = icon_red_light.
        wa_data-msg = l_errmsg.
      else.
        perform frm_call_bapi.

        clear: wa_data-status, wa_data-msg.
        " 输出日志
        read table it_return transporting no fields with key type = 'E'.
        if sy-subrc ne 0.
*      write: 'Success!' color col_positive .
          wa_data-msg = '导入成功!'.
          wa_data-status = icon_green_light.
        else.
          loop at it_return into wa_return where type = 'E'.
            message id wa_return-id
                    type wa_return-type
                    number wa_return-number
                    into l_msg
                    with wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4.
*        write: l_msg color col_negative.
            concatenate wa_data-msg '.' l_msg into wa_data-msg.
          endloop.
          wa_data-status = icon_red_light.
        endif.
      endif.

*      modify it_data from wa_data index l_index transporting status msg.
      loop at it_data assigning <fs_data> where vbeln = wa_data-vbeln.
        <fs_data>-status = wa_data-status.
        <fs_data>-msg = wa_data-msg.
      endloop.
    endat.
  endloop.
endform.
