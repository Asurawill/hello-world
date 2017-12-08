*&---------------------------------------------------------------------*
*&  Include           ZFI008_FRM
*&---------------------------------------------------------------------*
** 修改日期   开发人员  请求号                    描述
" 20170307   IT02     ED1K905285&ED1K906525      FI :ZFI008 项目开票科目替代新增若科目
"是1122020101，则替代为1122020201
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_data_init .
  g_edit_mod = gc_editable.

  wa_head-bldat = sy-datum.
  wa_head-blart = 'RV'.

  SELECT *
    INTO TABLE it_skat
    FROM skat
    WHERE spras = sy-langu
      AND ktopl = '1000'.
  SORT it_skat BY saknr.

*  append wa_item to it_item.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_data_clear .
  REFRESH: it_item.
  CLEAR: wa_head, wa_item.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       初始化layout参数
*----------------------------------------------------------------------*
FORM init_oo_layout .
  gw_layout-zebra = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
  gw_layout-edit_mode = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       排序
*----------------------------------------------------------------------*
FORM init_sort .
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
ENDFORM.                    " INIT_SORT
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       初始化变量
*----------------------------------------------------------------------*
FORM init_variant USING p_handle TYPE slis_handl.
  CLEAR: gw_variant.
  gw_variant-report = sy-repid.
  gw_variant-handle = p_handle.
ENDFORM.                    " INIT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
FORM frm_init_lvc.

  init_fieldcat 'BUZEI' '' '' '' '' 'BSEG' 'BUZEI' 3.
  init_fieldcat 'BSCHL' '' 'X' '' '' 'BSEG' 'BSCHL' 4.
  init_fieldcat 'HKONT' text-f01 'X' '' '' '' '' 10.
  init_fieldcat 'HKTXT' text-f09 '' '' '' 'SKAT' 'TXT20' 20.
  init_fieldcat 'KUNNR' '' 'X' '' '' 'BSEG' 'KUNNR' 10.
  init_fieldcat 'KNAME' text-f10 '' '' '' 'KNA1' 'NAME1' 20.
  init_fieldcat 'WRBTR' '' 'X' 'WAERS' '' 'BSEG' 'WRBTR' 12.
  init_fieldcat 'WAERS' '' '' '' '' 'BKPF' 'WAERS' 5.
  init_fieldcat 'ZTERM' '' 'X' '' '' 'BSEG' 'ZTERM' 4 .
  init_fieldcat 'ZFBDT' '' 'X' '' '' 'BSEG' 'ZFBDT' 10.
  init_fieldcat 'VBELN' '' '' '' '' 'VBAP' 'VBELN' 10.
  init_fieldcat 'POSNR' '' 'X' '' '' 'VBAP' 'POSNR' 6.
  init_fieldcat 'MWSKZ' text-f02 'X' '' '' '' '' 4.
  init_fieldcat 'SGTXT' '' 'X' '' '' 'BSEG' 'SGTXT' 20.


ENDFORM.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
FORM frm_oo_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_exclude TYPE ui_functions
                       pt_data
                USING pw_layout TYPE lvc_s_layo
                      pw_variant TYPE disvariant.

  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
      is_variant                    = pw_variant
      i_save                        = 'A'
*     i_default                     = 'X'
      is_layout                     = pw_layout
*     is_print                      =
*     it_special_groups             =
      it_toolbar_excluding          = pt_exclude[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      it_outtab                     = pt_data[]
      it_fieldcatalog               = pt_lvc[]
      it_sort                       = pt_sort[]
*     it_filter                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_display .
  PERFORM init_oo_layout.              "设置输出格式
  PERFORM init_sort.                "设置排序、合计
  PERFORM init_variant USING '0001'.             "设置变式控制
  PERFORM frm_init_lvc.             " 初始化内表结构/ALV显示结构
  PERFORM exclude_tb_functions TABLES gt_oo_exclude.
  PERFORM frm_oo_build_event.
  PERFORM frm_oo_output TABLES gt_lvc              "输出
                               gt_sort
                               gt_oo_exclude
                               it_item
                        USING gw_layout
                              gw_variant.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_refresh_alv .
  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  CALL METHOD gr_alvgrid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     i_soft_refresh =
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_container .
  CREATE OBJECT gr_container
    EXPORTING
      container_name = 'CONTAINER'
*     lifetime       = cl_gui_custom_container=>lifetime_dynpro
    .

  CREATE OBJECT gr_alvgrid
    EXPORTING
      i_appl_events = 'X'
      i_parent      = gr_container.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_oo_build_event .

  DATA lr_event_handler TYPE REF TO lcl_event_handler.
  CREATE OBJECT lr_event_handler.
  SET HANDLER : lr_event_handler->handle_toolbar              FOR gr_alvgrid,
                lr_event_handler->handle_before_user_command  FOR gr_alvgrid,
                lr_event_handler->handle_user_command         FOR gr_alvgrid,
                lr_event_handler->handle_after_user_command   FOR gr_alvgrid,
                lr_event_handler->handle_onf4                 FOR gr_alvgrid,
                lr_event_handler->handle_data_changed         FOR gr_alvgrid,
                lr_event_handler->handle_data_changed_finished FOR gr_alvgrid,
                lr_event_handler->handle_double_click         FOR gr_alvgrid,
                lr_event_handler->handle_button_click         FOR gr_alvgrid.

  DATA lt_f4 TYPE lvc_t_f4.
  DATA ls_f4 TYPE lvc_s_f4.
  ls_f4-fieldname = 'ZTERM'.      "F4对应的栏位
  ls_f4-register = 'X'.
  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.
  ls_f4-fieldname = 'HKONT'.      "F4对应的栏位
  ls_f4-register = 'X'.
*  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.
  ls_f4-fieldname = 'MWSKZ'.      "F4对应的栏位
  ls_f4-register = 'X'.
  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  CALL METHOD gr_alvgrid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  CALL METHOD gr_alvgrid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  handle_toolbar
*&---------------------------------------------------------------------*
*       自定义按钮
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM handle_toolbar  USING    p_e_object TYPE REF TO cl_alv_event_toolbar_set
                                                     p_e_interactive.
*10	&LOCAL&APPEND
*11	&LOCAL&INSERT_ROW
*12	&LOCAL&DELETE_ROW
*13	&LOCAL©_ROW

  DATA: lw_toolbar LIKE LINE OF p_e_object->mt_toolbar.
  READ TABLE p_e_object->mt_toolbar INTO lw_toolbar WITH KEY function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  IF sy-subrc = 0.
*加入新增行按钮
    CLEAR lw_toolbar.
    lw_toolbar-function    = 'APPEND_ROW '.
*    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    lw_toolbar-quickinfo   = '新增行'.
    lw_toolbar-icon        = icon_create.
    lw_toolbar-disabled    = space.
    INSERT lw_toolbar INTO p_e_object->mt_toolbar INDEX sy-tabix.
**加入插入行按钮
*    CLEAR ls_toolbar.
*    ls_toolbar-function    = 'INSERT_ROW'.
**    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*    ls_toolbar-quickinfo   = '插入行'.
*    ls_toolbar-icon        = icon_insert_row.
*    ls_toolbar-disabled    = space.
*    INSERT ls_toolbar INTO p_e_object->mt_toolbar INDEX 11.
**加入复制按钮
*    CLEAR ls_toolbar.
*    ls_toolbar-function    = 'COPY_ROW'.
**    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*    ls_toolbar-quickinfo   = '复制行'.
*    ls_toolbar-icon        = icon_copy_object.
*    ls_toolbar-disabled    = space.
*    INSERT ls_toolbar INTO p_e_object->mt_toolbar INDEX 13.
  ENDIF.

ENDFORM.                    " handle_toolbar
*&---------------------------------------------------------------------*
*&      Form  handle_before_user_command
*&---------------------------------------------------------------------*
*       覆盖标准功能
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_before_user_command  USING    p_e_ucomm TYPE syucomm.

ENDFORM.                    " handle_before_user_command
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_user_command  USING    p_ucomm.
  save_ok = p_ucomm.
  CLEAR p_ucomm.

  CASE save_ok.
    WHEN 'EDIT'.

    WHEN 'APPEND_ROW'.
      PERFORM frm_append_row.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  handle_after_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*      -->P_E_SAVED  text
*      -->P_E_NOT_PROCESSED  text
*----------------------------------------------------------------------*
FORM handle_after_user_command  USING    p_e_ucomm
                                          p_e_saved
                                          p_e_not_processed.

ENDFORM.                    " handle_after_user_command
*&---------------------------------------------------------------------*
*&      Form  handle_double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING    p_e_row
                                                                   p_e_column
                                                                   p_es_row_no.

ENDFORM.                    " handle_double_click
*&---------------------------------------------------------------------*
*&      Form  handle_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING    p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                                                     p_e_onf4
                                                                     p_e_onf4_before
                                                                     p_e_onf4_after
                                                                     p_e_ucomm TYPE sy-ucomm.
  " 输入检查：1. 记账码限制
  DATA: lt_mod_data TYPE lvc_t_modi,
        wa_mod_data TYPE lvc_s_modi.

  DATA: l_type TYPE c,
        l_msg  TYPE c LENGTH 100.

  lt_mod_data = p_er_data_changed->mt_mod_cells.

  LOOP AT lt_mod_data INTO wa_mod_data.
    IF wa_mod_data-fieldname = 'BSCHL' AND wa_mod_data-value IS NOT INITIAL.
      CLEAR: l_type, l_msg.
*    if wa_mod_data-value is initial.
*      read table it_item assigning <fs_item> index wa_mod_data-row_id.
*      if sy-subrc = 0.
*
*        perform frm_cell_style using 'KUNNR'
*                               'X'
*                               changing <fs_item>-cellstyle.
*      endif.
*    else.
      IF wa_head-bstyp IS INITIAL.
        l_type = 'E'.
        l_msg = '必须先填业务类型'.
      ELSEIF wa_head-bstyp = '1'.
        IF wa_mod_data-value NE '01' AND wa_mod_data-value NE '50'.
          l_type = 'E'.
          l_msg = '业务类型1时记账码只能为''01''或''50'''.
        ENDIF.
      ELSEIF wa_head-bstyp = '2'.
        IF wa_mod_data-value NE '01' AND wa_mod_data-value NE '11' AND wa_mod_data-value NE '40' AND wa_mod_data-value NE '50'.
          l_type = 'E'.
          l_msg = '业务类型2时记账码只能为''01''、''11''、''40''或''50'''.
        ENDIF.
      ENDIF.
      IF l_type = 'E'.
        CALL METHOD p_er_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '00'
            i_msgty     = 'E'
            i_msgno     = '001'
            i_msgv1     = l_msg
            i_fieldname = wa_mod_data-fieldname
*           i_row_id    = wa_mod_data-row_id
          .
      ENDIF.
*    elseif wa_mod_data-fieldname = 'WRBTR' and wa_mod_data-value is not initial.
*      if wa_head-bstyp = '1'.
*        read table it_item into wa_item index wa_mod_data-row_id.
*        if sy-subrc = 0.  " 记账码为50，科目为收入科目时，在过账时对金额进行控制。
**        if wa_item-bschl = '50' and wa_item-hkont >= 6001010101 and wa_item-hkont <= 6001019899 and  wa_item-posnr is not initial.
*          if wa_item-bschl = '50' and wa_item-hkont between 6001010101 and 6001019899.
*            if wa_item-posnr is not initial.
*              data: l_netwr     type vbap-netwr,
*                    l_netwr_new type vbap-netwr.
**          read table it_vbap into wa_vbap with key posnr = wa_item-posnr binary search.
**          if sy-subrc = 0.
**            l_netwr = wa_vbap-netwr.
*              l_netwr = wa_item-netwr.
*              " 扣减已开票金额
*              loop at it_zfi008 into wa_zfi008 where posnr = wa_item-posnr and sfncx = ''.
*                l_netwr = l_netwr - wa_zfi008-wrbtr.
*              endloop.
**          endif.
*              data: l_dcpfm type usr01-dcpfm,
*                    l_comma type c.
*              select single dcpfm
*                into l_dcpfm
*                from usr01
*                where bname = sy-uname.
*              if l_dcpfm = 'X'.
*                l_comma = ','.
*              elseif l_dcpfm = 'Y'.
*                l_comma = ' '.
*              else.
*                l_comma = '.'.
*              endif.
*              replace all occurrences of l_comma in wa_mod_data-value with ''.
*
*              l_netwr_new = wa_mod_data-value.
*              if l_netwr_new > l_netwr..
*                call method p_er_data_changed->add_protocol_entry
*                  exporting
*                    i_msgid     = '00'
*                    i_msgty     = 'E'
*                    i_msgno     = '001'
*                    i_msgv1     = '过账金额不能大于销售订单行项目的净值减已开票金额'
*                    i_fieldname = wa_mod_data-fieldname
**                   i_row_id    = wa_mod_data-row_id
*                  .
*                call method p_er_data_changed->modify_cell
*                  exporting
*                    i_row_id    = wa_mod_data-row_id
*                    i_fieldname = wa_mod_data-fieldname
*                    i_value     = ''.
*              endif.
*            else.
*              call method p_er_data_changed->add_protocol_entry
*                exporting
*                  i_msgid     = '00'
*                  i_msgty     = 'E'
*                  i_msgno     = '001'
*                  i_msgv1     = '请先输入销售订单行项目'
*                  i_fieldname = wa_mod_data-fieldname
**                 i_row_id    = wa_mod_data-row_id
*                .
*              call method p_er_data_changed->modify_cell
*                exporting
*                  i_row_id    = wa_mod_data-row_id
*                  i_fieldname = wa_mod_data-fieldname
*                  i_value     = ''.
*            endif.
*          endif.
*        endif.
*      endif.
**    else.
**      call method p_er_data_changed->modify_cell
**        exporting
**          i_row_id    = wa_mod_data-row_id
**          i_fieldname = wa_mod_data-fieldname
**          i_value     = wa_mod_data-value.
**    elseif wa_mod_data-fieldname = 'HKONT' and wa_mod_data-value is not initial.
**      call method p_er_data_changed->modify_cell
**        exporting
**          i_row_id    = wa_mod_data-row_id
**          i_fieldname = wa_mod_data-fieldname
**          i_value     = wa_mod_data-value.
    ELSE.
*      call method p_er_data_changed->refresh_protocol.
*      clear p_e_ucomm.
      IF p_er_data_changed->mt_protocol IS NOT INITIAL.
        CALL METHOD p_er_data_changed->display_protocol
*        exporting
*          i_container        =
*          i_display_toolbar  =
*          i_optimize_columns =
          .
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " handle_data_changed
*&---------------------------------------------------------------------*
*&      Form  handle_data_changed_finished
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM handle_data_changed_finished  USING    p_e_modified
                                            p_et_good_cells TYPE  lvc_t_modi.
  DATA: lw_cell TYPE lvc_s_modi.

  " 输入记账码时，更改相应字段的可输入属性
  READ TABLE p_et_good_cells INTO lw_cell WITH KEY fieldname = 'BSCHL'.
  IF sy-subrc = 0.
    READ TABLE it_item ASSIGNING <fs_item> INDEX lw_cell-row_id.
    IF sy-subrc = 0.
      IF lw_cell-value = '01' OR lw_cell-value = '11'.
        <fs_item>-vbeln = wa_head-vbeln.
        <fs_item>-kunnr = wa_vbak-kunnr.

        SELECT SINGLE name1
          INTO <fs_item>-kname
          FROM kna1
          WHERE kunnr = <fs_item>-kunnr.

        <fs_item>-zterm = wa_vbkd-zterm.
        <fs_item>-zfbdt = sy-datum.
        <fs_item>-posnr = '100'.

        PERFORM frm_cell_style USING 'KUNNR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZTERM'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZFBDT'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'MWSKZ'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'POSNR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
      ELSEIF lw_cell-value = '40' OR lw_cell-value = '50'.
        <fs_item>-mwskz = 'X1'.

        PERFORM frm_cell_style USING 'KUNNR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZTERM'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZFBDT'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'MWSKZ'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        IF <fs_item>-hkont BETWEEN 6001010101 AND 6001019899.
          <fs_item>-vbeln = wa_head-vbeln.
          <fs_item>-posnr = '100'.

          PERFORM frm_cell_style USING 'POSNR'
                                 'X'
                                 CHANGING <fs_item>-cellstyle.
        ELSE.
          PERFORM frm_cell_style USING 'POSNR'
                                 ''
                                 CHANGING <fs_item>-cellstyle.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  " 输入科目，控制销售订单行是否可输入
  READ TABLE p_et_good_cells INTO lw_cell WITH KEY fieldname = 'HKONT'.
  IF sy-subrc = 0.
    READ TABLE it_item ASSIGNING <fs_item> INDEX lw_cell-row_id.
    IF sy-subrc = 0.
      IF lw_cell-value BETWEEN 6001010101 AND 6001019899.
        IF <fs_item>-bschl = '40' OR <fs_item>-bschl = '50'.
          <fs_item>-vbeln = wa_head-vbeln.
          <fs_item>-posnr = '100'.
          PERFORM frm_cell_style USING 'POSNR'
                                 'X'
                                 CHANGING <fs_item>-cellstyle.
        ENDIF.
      ENDIF.

      READ TABLE it_skat INTO wa_skat WITH KEY saknr = <fs_item>-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_item>-hktxt = wa_skat-txt20.
      ENDIF.
    ENDIF.
  ENDIF.


  " 输入客户号，带出描述
  READ TABLE p_et_good_cells INTO lw_cell WITH KEY fieldname = 'KUNNR'.
  IF sy-subrc = 0.
    READ TABLE it_item ASSIGNING <fs_item> INDEX lw_cell-row_id.
    IF sy-subrc = 0.
      SELECT SINGLE name1
        INTO <fs_item>-kname
        FROM kna1
        WHERE kunnr = <fs_item>-kunnr.
    ENDIF.
  ENDIF.

  " 输入销售订单行号时
  READ TABLE p_et_good_cells INTO lw_cell WITH KEY fieldname = 'POSNR'.
  IF sy-subrc = 0.
    READ TABLE it_item ASSIGNING <fs_item> INDEX lw_cell-row_id.
    IF sy-subrc = 0.
      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = <fs_item>-vbeln
                                               posnr = <fs_item>-posnr
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_item>-netwr = wa_vbap-netwr.
        <fs_item>-matnr = wa_vbap-matnr.
      ENDIF.
    ENDIF.
  ENDIF.

*  "
*  read table p_et_good_cells into lw_cell with key fieldname = 'WRBTR'.
*  if sy-subrc = 0.
*    read table it_item assigning <fs_item> index lw_cell-row_id.
*    if sy-subrc = 0.
*
*    endif.
*  endif.

*自动计算税额
  DATA l_lines      TYPE i.
  DATA l_wrbtr      TYPE bseg-wrbtr.
  DATA l_wrbtr_1    TYPE bseg-wrbtr.

  CLEAR l_lines.
  CLEAR l_wrbtr.
  CLEAR l_wrbtr_1.

  DESCRIBE TABLE it_item LINES l_lines.

  SORT it_item BY buzei.
  LOOP AT it_item INTO wa_item.
*当第一行的时候，记录借放金额
    IF wa_item-buzei = '001'.
      l_wrbtr = wa_item-wrbtr.
    ENDIF.

*减去贷方金额（排除最后一行）
    IF wa_item-buzei <> '001' AND sy-tabix <> l_lines.
      l_wrbtr_1 = wa_item-wrbtr + l_wrbtr_1.
    ENDIF.

*计算最后一行的金额
    IF sy-tabix = l_lines.
      wa_item-wrbtr = l_wrbtr - l_wrbtr_1.
      IF wa_item-wrbtr < 0.
        wa_item-wrbtr = 0.
      ENDIF.

      MODIFY it_item FROM wa_item.
      CLEAR wa_item.
    ENDIF.

  ENDLOOP.

  IF p_e_modified = 'X'.
    g_changed = abap_true.
  ENDIF.

  PERFORM frm_refresh_alv.
ENDFORM.                    " handle_data_changed_finished

*&---------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_COL_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_button_click  USING    p_es_col_id
                                   p_es_row_no.

ENDFORM.                    " HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       弃掉标准按钮（增加、插入）
*----------------------------------------------------------------------*
*      -->P_GT_EXCLUDE  text
*      -->P_ELSE  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions TABLES pt_exclude TYPE ui_functions.
  DATA: ls_exclude TYPE ui_func.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_average .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_sum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_subtot .
*  APPEND ls_exclude TO pt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_find .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_filter .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print_prev .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_export .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_graph .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_view .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_detail .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_help .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_info .
*  APPEND ls_exclude TO pt_exclude.
*   ls_exclude = cl_gui_alv_grid=>MC_MB_VARIANT.
*  APPEND ls_exclude TO pt_exclude.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  frm_cell_style
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_CELLSTYLE  text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM frm_cell_style  USING    p_fieldname
                              p_editable
                     CHANGING pt_cellstyle TYPE lvc_t_styl.
  DATA: lw_cellstyle TYPE lvc_s_styl.

  READ TABLE pt_cellstyle INTO lw_cellstyle WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    MODIFY TABLE pt_cellstyle FROM lw_cellstyle.
  ELSE.
    lw_cellstyle-fieldname = p_fieldname.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    INSERT lw_cellstyle INTO TABLE pt_cellstyle.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_APPEND_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_append_row .
  DATA: l_index TYPE i.
  DATA: l_row_id TYPE lvc_s_row,
        l_col_id TYPE lvc_s_col,
        l_row_no TYPE lvc_s_roid.

  DATA: l_buzei TYPE bseg-buzei.
  l_buzei = 1.
  DO.
    READ TABLE it_item TRANSPORTING NO FIELDS WITH KEY buzei = l_buzei.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    l_buzei = l_buzei + 1.
  ENDDO.

  CLEAR wa_item.
  wa_item-buzei = l_buzei.
  wa_item-waers = wa_head-waers.
*  wa_item-vbeln = wa_head-vbeln.

*  wa_item-posnr = '100'.
*  wa_item-mwskz = 'X1'.

  PERFORM frm_cell_style USING 'KUNNR'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZTERM'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZFBDT'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'MWSKZ'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'POSNR'
                         ''
                         CHANGING wa_item-cellstyle.
  APPEND wa_item TO it_item.

  PERFORM frm_refresh_alv.

  " 设置光标在过账码
  l_index = lines( it_item ).

  l_row_id = l_index.
  l_col_id = 'BSCHL'.
  l_row_no-row_id = l_index.

  CALL METHOD gr_alvgrid->set_current_cell_via_id
    EXPORTING
      is_row_id    = l_row_id
      is_column_id = l_col_id
      is_row_no    = l_row_no.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM handle_onf4  USING    p_fieldname TYPE lvc_fname
                          ps_row_no TYPE lvc_s_roid
                          pr_event_data TYPE REF TO cl_alv_event_data
                          pt_bad_cells TYPE lvc_t_modi
                          p_display.
  FIELD-SYMBOLS <fs_mod_cells> TYPE lvc_t_modi.
  DATA: lw_mod_cell TYPE lvc_s_modi.

  CASE p_fieldname.
    WHEN 'ZTERM'.
      READ TABLE it_item INTO wa_item INDEX ps_row_no-row_id.
      IF sy-subrc = 0.
        PERFORM sub_help_zterm CHANGING wa_item-zterm.
        IF wa_item-zterm IS NOT INITIAL.
          MODIFY it_item FROM wa_item INDEX ps_row_no-row_id.
*          perform frm_refresh_alv.
        ENDIF.
      ENDIF.
    WHEN 'HKONT'.
      READ TABLE it_item INTO wa_item INDEX ps_row_no-row_id.
      IF sy-subrc = 0.
        PERFORM sub_help_hkont CHANGING wa_item-hkont.
        IF wa_item-hkont IS NOT INITIAL.
          MODIFY it_item FROM wa_item INDEX ps_row_no-row_id.

          " Trigger data changed event
          ASSIGN pr_event_data->m_data->* TO <fs_mod_cells>.
          lw_mod_cell-row_id = ps_row_no-row_id.
          lw_mod_cell-sub_row_id = ps_row_no-sub_row_id.
          lw_mod_cell-fieldname = 'HKONT'.
          lw_mod_cell-value = wa_item-hkont.
          APPEND lw_mod_cell TO <fs_mod_cells>.

*          perform frm_refresh_alv.
        ENDIF.
      ENDIF.
    WHEN 'MWSKZ'.
      READ TABLE it_item INTO wa_item INDEX ps_row_no-row_id.
      IF sy-subrc = 0.
        PERFORM sub_help_mwskz CHANGING wa_item-mwskz.
        IF wa_item-mwskz IS NOT INITIAL.
          MODIFY it_item FROM wa_item INDEX ps_row_no-row_id.
*          perform frm_refresh_alv.
        ENDIF.
      ENDIF.

  ENDCASE.

**  Inform ALV Grid that event 'onf4' has been processed
  pr_event_data->m_event_handled = 'X'.           "告知F4动作结束
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_ZTERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_ITEM_ZTERM  text
*----------------------------------------------------------------------*
FORM sub_help_zterm  CHANGING p_zterm.
  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
*     I_KOART       = ' '
      i_zterm       = p_zterm
*     I_XSHOW       = ' '
*     I_ZTYPE       = ' '
*     I_NO_POPUP    = ' '
    IMPORTING
      e_zterm       = p_zterm
*     ET_ZTERM      =
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_ITEM_HKONT  text
*----------------------------------------------------------------------*
FORM sub_help_hkont  CHANGING p_hkont.
  SET PARAMETER ID 'BUK' FIELD wa_head-bukrs.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      matchcode_object          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      select_value              = p_hkont
    EXCEPTIONS
      invalid_dictionary_field  = 1
      invalid_matchdcode_object = 2
      no_selection              = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
FORM sub_help_mwskz  CHANGING p_mwskz.
  CALL FUNCTION 'FI_F4_MWSKZ'
    EXPORTING
      i_kalsm = g_kalsm
*     I_STBUK = BSEG-STBUK                                "N1175497
      i_stbuk = wa_head-bukrs                                   "N1175497
*     i_xshow = f4hlp-fieldinp
    IMPORTING
      e_mwskz = p_mwskz.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_ACCDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_post_accdoc .
  DATA l_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      text_question         = '确定要保存并过账吗？'
      text_button_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      text_button_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      answer                = l_ans
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK l_ans EQ '1'.

  DATA l_subrc TYPE sy-subrc.
  PERFORM frm_check_doc CHANGING l_subrc.

  CHECK l_subrc EQ 0.

  PERFORM frm_auth_check USING '10' wa_head-bukrs.
  IF sy-subrc NE 0.
    MESSAGE s010(zfico01) WITH wa_head-bukrs DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM frm_pre_srys ."收入科目映射   "add it02 20170713
  PERFORM frm_bapi_data_prep USING ''.
  PERFORM frm_call_bapi CHANGING wa_head-belnr wa_head-gjahr.

  IF wa_head-belnr IS NOT INITIAL.  " 创建成功
    " 创建对应的自动凭证
    IF wa_head-bstyp = '1'.
      PERFORM frm_bapi_data_prep USING 'X'.
      PERFORM frm_call_bapi CHANGING wa_head-belnr2 wa_head-gjahr2.
    ENDIF.

    PERFORM frm_save_zfi008.

    g_changed = abap_false.

    " 切换为只读模式
    g_edit_mod = gc_readonly.

    MESSAGE text-m01 TYPE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_doc  CHANGING p_subrc.
  " 凭证日期
  IF wa_head-bldat IS INITIAL.
    MESSAGE text-m02 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF wa_head-budat IS INITIAL.
    MESSAGE text-m03 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 业务类型
  IF wa_head-bstyp IS INITIAL.
    MESSAGE text-m04 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 凭证类型
  IF wa_head-blart IS INITIAL.
    MESSAGE text-m05 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF wa_head-bukrs IS INITIAL.
    MESSAGE text-m06 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 货币
  IF wa_head-waers IS INITIAL.
    MESSAGE text-m07 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 销售订单
  IF wa_head-vbeln IS INITIAL.
    MESSAGE text-m08 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  "ADD BY HANDWY 2015-4-23
  "检查是否已经过账
  IF   wa_head-belnr  IS NOT INITIAL
  AND  wa_head-gjahr  IS NOT INITIAL.
    MESSAGE text-m22 TYPE 'W'  DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 行项目
  IF it_item IS INITIAL.
    MESSAGE text-m09 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  DATA: l_wrbtr TYPE bseg-wrbtr.

  LOOP AT it_item ASSIGNING <fs_item>.
    " 记账码
    IF wa_head-bstyp = '1' AND <fs_item>-bschl NE '01' AND <fs_item>-bschl NE '50'.
      MESSAGE text-m10 TYPE 'W' DISPLAY LIKE 'E'.
      p_subrc = 8.
      EXIT.
    ELSEIF wa_head-bstyp = '2' AND <fs_item>-bschl NE '01' AND <fs_item>-bschl NE '50' AND <fs_item>-bschl NE '11' AND <fs_item>-bschl NE '40'.
      MESSAGE text-m11 TYPE 'W' DISPLAY LIKE 'E'.
      p_subrc = 8.
      EXIT.
    ENDIF.

    " 科目
    IF <fs_item>-hkont IS INITIAL.
      MESSAGE text-m12 TYPE 'W' DISPLAY LIKE 'E'.
      p_subrc = 8.
      EXIT.
    ENDIF.

    IF <fs_item>-bschl = '01' OR <fs_item>-bschl = '11'.
      " 客户
      IF <fs_item>-kunnr IS INITIAL.
        MESSAGE text-m13 TYPE 'W' DISPLAY LIKE 'E'.
        p_subrc = 8.
        EXIT.
      ENDIF.
      " 销售订单行
      IF <fs_item>-posnr IS INITIAL.
        MESSAGE text-m14 TYPE 'W' DISPLAY LIKE 'E'.
        p_subrc = 8.
        EXIT.
      ENDIF.
      " 付款条件
      IF <fs_item>-zterm IS INITIAL.
        MESSAGE text-m15 TYPE 'W' DISPLAY LIKE 'E'.
        p_subrc = 8.
        EXIT.
      ENDIF.
      " 基准日期
      IF <fs_item>-zfbdt IS INITIAL.
        MESSAGE text-m16 TYPE 'W' DISPLAY LIKE 'E'.
        p_subrc = 8.
        EXIT.
      ENDIF.
    ENDIF.

    IF <fs_item>-bschl = '50'.
      " 税码
      IF <fs_item>-mwskz IS INITIAL.
        MESSAGE text-m17 TYPE 'W' DISPLAY LIKE 'E'.
        p_subrc = 8.
        EXIT.
      ENDIF.
    ENDIF.

    IF <fs_item>-bschl = '01' OR <fs_item>-bschl = '40'.
      l_wrbtr = l_wrbtr + <fs_item>-wrbtr.
    ELSEIF <fs_item>-bschl = '11' OR <fs_item>-bschl = '50'.
      l_wrbtr = l_wrbtr - <fs_item>-wrbtr.
    ENDIF.
  ENDLOOP.

  IF p_subrc NE 0.
    EXIT.
  ENDIF.

  IF l_wrbtr NE 0.
    MESSAGE text-m18 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_data_prep USING l_auto.
  " 清空BAPI变量
  PERFORM frm_bapi_data_clear.

  DATA: l_katyp TYPE cskb-katyp.

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  wa_documentheader-doc_date     =  wa_head-bldat.
*过账日期
  wa_documentheader-pstng_date   =  wa_head-budat.
*凭证类型
  wa_documentheader-doc_type     =  wa_head-blart.
*公司代码
  wa_documentheader-comp_code    =  wa_head-bukrs.
*凭证抬头文本
  wa_documentheader-header_txt   =  wa_head-bktxt.
*创建人员
  wa_documentheader-username     =  sy-uname.
*参考凭证号 - 业务类型
  wa_documentheader-ref_doc_no   =  wa_head-bstyp.



**********************************************************************
* 凭证行
**********************************************************************
  LOOP AT it_item INTO wa_item.
    IF l_auto = 'X'.  " 创建自动凭证，映射记账码
      IF wa_item-bschl = '01'.
        wa_item-bschl = '11'.
      ELSEIF wa_item-bschl = '50'.
        wa_item-bschl = '40'.
      ENDIF.

      " 科目映射
*      IF wa_item-hkont BETWEEN 6001010101 AND 6001019899.     " 收入科目  "it02 屏蔽 20170713
*        wa_item-hkont = 6001019901.
*      ELSE
      IF wa_item-hkont BETWEEN 2221030501 AND 2221030502
        OR wa_item-hkont BETWEEN 2221030504 AND 2221030599. " 销项税
        wa_item-hkont = 2221030503.
      ELSEIF wa_item-hkont BETWEEN 2203010101 AND 2203010399. " 预收帐款科目
        wa_item-hkont = 2203010401.
      ELSEIF wa_item-hkont BETWEEN 1122010101 AND 1122010299. " 应收账款科目
        wa_item-hkont = 1122010301.
      ELSEIF wa_item-hkont EQ 1122020101.
        wa_item-hkont = 1122020201.
      ELSE.
        READ TABLE gt_srys INTO gs_srys WITH KEY hkont = wa_item-hkont
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_item-hkont = gs_srys-hkont_tz.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_item-bschl = '01' OR wa_item-bschl = '11'.
      " 客户
      CLEAR wa_accountreceivable.
*       行项目号
      wa_accountreceivable-itemno_acc = wa_item-buzei.
*       科目
      wa_accountreceivable-gl_account = wa_item-hkont.
*       项目文本
      wa_accountreceivable-item_text = wa_item-sgtxt.
*       客户编码
      wa_accountreceivable-customer = wa_item-kunnr.
*       付款条件
      wa_accountreceivable-pmnttrms = wa_item-zterm.
*       付款基准日期
      wa_accountreceivable-bline_date = wa_item-zfbdt.
*       分配号 - 销售订单号/行号 需更改为所有行项目“销售订单号”写到 “分配”字段150602 IT)2
*      IF WA_ITEM-VBELN IS NOT INITIAL.
* "       CONCATENATE WA_ITEM-VBELN '/' WA_ITEM-POSNR INTO WA_ACCOUNTRECEIVABLE-ALLOC_NMBR. "LEYARDIT02 150505
*        WA_ACCOUNTRECEIVABLE-ALLOC_NMBR = WA_ITEM-VBELN .
*      ENDIF.
      wa_accountreceivable-alloc_nmbr = wa_head-vbeln.      "150602

      APPEND wa_accountreceivable TO it_accountreceivable.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc = wa_item-buzei.
*       货币
      wa_currencyamount-currency = wa_item-waers.
*       金额
      wa_currencyamount-amt_doccur = wa_item-wrbtr.
      IF wa_item-bschl = '11' OR wa_item-bschl = '50'.
        wa_currencyamount-amt_doccur = - wa_currencyamount-amt_doccur.
      ENDIF.

      APPEND wa_currencyamount TO it_currencyamount.

      " 记账码 & 反记账
      CLEAR wa_extension2.
      CLEAR wa_zaccdocuext.
      wa_zaccdocuext-posnr = wa_item-buzei."行项目
      wa_zaccdocuext-bschl = wa_item-bschl.
      IF wa_item-bschl = '11' OR wa_item-bschl = '40'.
        wa_zaccdocuext-xnegp = 'X'.
      ENDIF.
      wa_extension2-structure  = 'ZACCDOCUEXT'.
      wa_extension2-valuepart1 = wa_zaccdocuext.
      APPEND wa_extension2 TO it_extension2.
    ELSEIF wa_item-bschl = '40' OR wa_item-bschl = '50'.
      " 总账
      CLEAR wa_accountgl.
*       行项目号
      wa_accountgl-itemno_acc = wa_item-buzei.
*       科目
      wa_accountgl-gl_account = wa_item-hkont.
*       项目文本
      wa_accountgl-item_text = wa_item-sgtxt.
*       税码
      wa_accountgl-tax_code = wa_item-mwskz.

*销售订单，销售订单行项目 ADD BY HANDWY 2015-04-15
      wa_accountgl-sales_ord = wa_item-vbeln.

      wa_accountgl-s_ord_item = wa_item-posnr.

*       分配号 - 销售订单号/行号 更改为 所有生成凭证的行项目的“分配”字段都需写入开票抬头的“销售订单号” 150602
*      IF WA_ITEM-VBELN IS NOT INITIAL.
*    "    CONCATENATE WA_ITEM-VBELN '/' WA_ITEM-POSNR INTO WA_ACCOUNTGL-ALLOC_NMBR.
*        WA_ACCOUNTGL-ALLOC_NMBR = WA_ITEM-VBELN .
*      ENDIF.
      wa_accountgl-alloc_nmbr = wa_head-vbeln.              "150602

      APPEND wa_accountgl TO it_accountgl.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc = wa_item-buzei.
*       货币
      wa_currencyamount-currency = wa_item-waers.
*       金额
      wa_currencyamount-amt_doccur = wa_item-wrbtr.
      IF wa_item-bschl = '11' OR wa_item-bschl = '50'.
        wa_currencyamount-amt_doccur = - wa_currencyamount-amt_doccur.
      ENDIF.

      APPEND wa_currencyamount TO it_currencyamount.

      " 记账码 & 反记账
      CLEAR wa_extension2.
      CLEAR wa_zaccdocuext.
      wa_zaccdocuext-posnr = wa_item-buzei."行项目
      wa_zaccdocuext-bschl = wa_item-bschl.
      IF wa_item-bschl = '11' OR wa_item-bschl = '40'.
        wa_zaccdocuext-xnegp = 'X'.
      ENDIF.
      wa_extension2-structure  = 'ZACCDOCUEXT'.
      wa_extension2-valuepart1 = wa_zaccdocuext.
      APPEND wa_extension2 TO it_extension2.
    ENDIF.

    CLEAR l_katyp.
    SELECT SINGLE katyp
      INTO l_katyp
      FROM cskb
      WHERE kstar = wa_item-hkont.
    IF l_katyp = '11'.
      " 获利能力段
      CLEAR wa_criteria.
      wa_criteria-itemno_acc = wa_item-buzei.
      wa_criteria-fieldname = 'KAUFN'.  " 销售订单
      wa_criteria-character = wa_item-vbeln.
      APPEND wa_criteria TO it_criteria.
      wa_criteria-fieldname = 'KDPOS'.  " 销售订单项目
      wa_criteria-character = wa_item-posnr.
      APPEND wa_criteria TO it_criteria.
      wa_criteria-fieldname = 'KNDNR'.  " 客户
      wa_criteria-character = wa_vbak-kunnr.
      APPEND wa_criteria TO it_criteria.
      wa_criteria-fieldname = 'ARTNR'.  " 生产
      wa_criteria-character = wa_item-matnr.
      APPEND wa_criteria TO it_criteria.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_data_clear .
  REFRESH: it_accountgl, it_accountreceivable, it_currencyamount, it_criteria, it_valuefield, it_extension2, it_return.
  CLEAR: wa_documentheader, wa_obj.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_call_bapi CHANGING p_belnr TYPE belnr_d
                            p_gjahr TYPE gjahr.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = wa_documentheader
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
      obj_type          = wa_obj-obj_type
      obj_key           = wa_obj-obj_key
      obj_sys           = wa_obj-obj_sys
    TABLES
      accountgl         = it_accountgl
      accountreceivable = it_accountreceivable
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      currencyamount    = it_currencyamount
      criteria          = it_criteria
      valuefield        = it_valuefield
*     EXTENSION1        =
      return            = it_return
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      extension2        = it_extension2
*     REALESTATE        =
*     ACCOUNTWT         =
    .


  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM frm_message_display.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    " 回写生成的会计凭证号与会计凭证年度
    p_belnr = wa_obj-obj_key(10).
    p_gjahr = wa_obj-obj_key+14(4).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ALV_EDIT  text
*----------------------------------------------------------------------*
FORM frm_change_edit_mode  USING    p_alv_edit TYPE int4.
  CALL METHOD gr_alvgrid->set_ready_for_input
    EXPORTING
      i_ready_for_input = p_alv_edit.

  "切换模式后刷新ALV
  PERFORM frm_refresh_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_display .
  PERFORM frm_message_initial.

  LOOP AT it_return INTO wa_return.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.
*
*    write: / g_msg.

    "append all error warning messages to below function module
    PERFORM store_messages USING wa_return-id
                                 wa_return-type
                                 wa_return-message_v1
                                 wa_return-message_v2
                                 wa_return-message_v3
                                 wa_return-message_v4
                                 wa_return-number.
  ENDLOOP.

  PERFORM frm_message_show.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_initial .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      log_not_active       = 1
      wrong_identification = 2
      OTHERS               = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_show .
  "at lsat call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA l_exit_command TYPE bal_s_excm.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      show_linno         = space
    IMPORTING
      e_exit_command     = l_exit_command
    EXCEPTIONS
      inconsistent_range = 1
      no_messages        = 2
      OTHERS             = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETURN_ID  text
*      -->P_WA_RETURN_TYPE  text
*      -->P_WA_RETURN_MESSAGE_V1  text
*      -->P_WA_RETURN_MESSAGE_V2  text
*      -->P_WA_RETURN_MESSAGE_V3  text
*      -->P_WA_RETURN_MESSAGE_V4  text
*      -->P_WA_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM store_messages USING p_msgid
                          p_msgty
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                          p_txtnr.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      arbgb                  = p_msgid
      msgty                  = p_msgty
      msgv1                  = p_msgv1
      msgv2                  = p_msgv2
      msgv3                  = p_msgv3
      msgv4                  = p_msgv4
      txtnr                  = p_txtnr
    EXCEPTIONS
      message_type_not_valid = 1
      not_active             = 2
      OTHERS                 = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_KPJL_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_kpjl_report .
  PERFORM frm_kpjl_get_data.

  PERFORM init_layout2.              "设置输出格式
  PERFORM init_sort.                "设置排序、合计
  PERFORM init_variant USING '0002'.             "设置变式控制
  PERFORM frm_init_lvc_kpjl.        " 初始化内表结构/ALV显示结构
  PERFORM frm_exclude.
  PERFORM frm_build_event.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            it_kpjl
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_QKCX_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_qkcx_report .
  PERFORM frm_qkcx_get_data.

  PERFORM init_layout.              "设置输出格式
  PERFORM init_sort.                "设置排序、合计
  PERFORM init_variant USING '0003'.             "设置变式控制
  PERFORM frm_init_lvc_qkcx.        " 初始化内表结构/ALV显示结构
  PERFORM frm_exclude.
  PERFORM frm_build_event.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            it_qkcx
                     USING ''
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_KPJL_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_kpjl_get_data .
  DATA:gtvbak LIKE TABLE OF vbak WITH HEADER LINE.
  DATA:gtkna1 LIKE TABLE OF kna1 WITH HEADER LINE.
  SELECT *
    INTO TABLE it_zfi008
    FROM zfi008
    WHERE bukrs = p_bukrs
      AND vbeln IN s_vbeln
      AND posnr IN s_posnr
      AND budat IN s_budat.

  CHECK it_zfi008 IS NOT INITIAL.

  SELECT *
    INTO TABLE it_bkpf
    FROM bkpf
    FOR ALL ENTRIES IN it_zfi008
    WHERE bukrs = it_zfi008-bukrs
      AND belnr = it_zfi008-belnr
      AND gjahr = it_zfi008-gjahr.
  SORT it_bkpf BY bukrs belnr gjahr.
  "IT02 ADD 150702 BEGIN
  SELECT  vbeln kunnr  INTO CORRESPONDING FIELDS OF TABLE gtvbak
    FROM  vbak
    FOR ALL ENTRIES IN it_zfi008
    WHERE vbeln = it_zfi008-vbeln.
  IF gtvbak[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gtkna1
      FROM kna1
      FOR ALL ENTRIES IN gtvbak
      WHERE kunnr = gtvbak-kunnr.
  ENDIF.
  "IT02 ADD END 150702
  LOOP AT it_zfi008 INTO wa_zfi008.
    CLEAR wa_kpjl.
    MOVE-CORRESPONDING wa_zfi008 TO wa_kpjl.
    "读取客户编码 、客户名称 IT02 150702
    READ TABLE gtvbak WITH KEY vbeln = wa_kpjl-vbeln.
    IF sy-subrc = 0.
      wa_kpjl-kunnr = gtvbak-kunnr.
    ENDIF.
    READ TABLE gtkna1 WITH KEY kunnr =  wa_kpjl-kunnr .
    IF sy-subrc = 0.
      wa_kpjl-name1 = gtkna1-name1.
    ENDIF.                                                  "150702 END
    " 取抬头文本
    READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_kpjl-bukrs
                                             belnr = wa_kpjl-belnr
                                             gjahr = wa_kpjl-gjahr
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_kpjl-bktxt = wa_bkpf-bktxt.
    ENDIF.

    " 取项目名称 - 销售订单抬头文本
    g_objname = wa_kpjl-vbeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = '1'
        name                    = g_objname
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = it_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
      READ TABLE it_lines INTO wa_lines INDEX 1.
      IF sy-subrc = 0.
        wa_kpjl-pjnam = wa_lines-tdline.
      ENDIF.
    ENDIF.
    IF wa_kpjl-bschl = '40'.
      wa_kpjl-wrbtr = - wa_kpjl-wrbtr .
    ENDIF.
    APPEND wa_kpjl TO it_kpjl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       初始化layout参数
*----------------------------------------------------------------------*
FORM init_layout .
  gw_layout-zebra = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
*  gw_layout-edit_mode = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.                    " INIT_LAYOUT

FORM init_layout2.
  gw_layout-zebra = 'X'.
  gw_layout-box_fname = 'BOX'.
  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
*  gw_layout-edit_mode = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC_KPJL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc_kpjl .

  init_fieldcat2 'BUKRS' '' '' '' '' 'BSEG' 'BUKRS' ''.
  init_fieldcat2 'BUDAT' '' '' '' '' 'BKPF' 'BUDAT' ''.
  init_fieldcat2 'VBELN' '' '' '' '' 'VBAK' 'VBELN' ''.
  init_fieldcat2 'PJNAM' text-f03 '' '' '' '' '' ''.
  init_fieldcat2 'BELNR' '' '' '' '' 'BSEG' 'BELNR' ''.
  init_fieldcat2 'GJAHR' '' '' '' '' 'BSEG' 'GJAHR' ''.
  init_fieldcat2 'WRBTR' '' '' 'WAERS' '' 'BSEG' 'WRBTR' ''.
  init_fieldcat2 'WAERS' '' '' '' '' 'BKPF' 'WAERS' ''.
  init_fieldcat2 'BKTXT' '' '' '' '' 'BKPF' 'BKTXT' ''.
  init_fieldcat2 'REFBE' text-f04 '' '' '' 'BSEG' 'BELNR' ''.
  init_fieldcat2 'REFGJ' text-f05 '' '' '' 'BSEG' 'GJAHR' ''.
  init_fieldcat2 'SFNCX' '' '' '' '' 'ZFI008' 'SFNCX' 'X'.
  init_fieldcat2 'IS_ADJUST' '是否调整' '' '' '' '' '' ''."it02 150623 add
  init_fieldcat2 'KUNNR' '客户编码' '' '' '' '' '' ''.
  init_fieldcat2 'NAME1' '客户名称' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_exclude .
  REFRESH gt_exclude.
  CLEAR gs_exclude.
ENDFORM.                    " FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       ALV事件
*----------------------------------------------------------------------*
FORM frm_build_event .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events[].

*  read table gt_events into gw_events with key name = slis_ev_top_of_page.
*  if sy-subrc = 0.
*    gw_events-form = 'ALV_TOP_OF_PAGE'.
*    modify gt_events from gw_events index sy-tabix.
*  endif.
ENDFORM.                    " FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
FORM frm_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_data
                USING pu_status
                      pu_ucomm
                      pw_layout TYPE lvc_s_layo
                      pw_variant TYPE disvariant
                      pw_grid_settings TYPE lvc_s_glay.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
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
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  DATA:l_index TYPE sy-tabix.

  CASE r_ucomm.
    WHEN '&IC1'."双击
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
    WHEN 'REVERSAL'.
      PERFORM frm_acc_reversal.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_input_check .
  IF p_bukrs IS INITIAL.
    MESSAGE '公司代码必输！' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_QKCX_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_qkcx_get_data .
*  select *
*    into corresponding fields of table it_vbap
*    from vbap as p
*    join vbak as k
*      on p~vbeln = k~vbeln
*    where bukrs_vf = p_bukrs
*      and k~vbeln in s_vbeln
*      and posnr in s_posnr.
  DATA: gt_vbel2 LIKE TABLE OF bseg WITH HEADER LINE.

  SELECT p~vbeln
         p~posnr
         k~bukrs_vf
         p~pstyv
         p~netwr
         p~waerk
    INTO TABLE it_vbakap
    FROM vbap AS p
    JOIN vbak AS k
      ON p~vbeln = k~vbeln
    WHERE bukrs_vf = p_bukrs
      AND k~vbeln IN s_vbeln
      AND posnr IN s_posnr.

  CHECK it_vbakap IS NOT INITIAL.

  SELECT *
    INTO TABLE it_vbuk
    FROM vbuk
    FOR ALL ENTRIES IN it_vbakap
    WHERE vbeln = it_vbakap-vbeln.
  SORT it_vbuk BY vbeln.

*  SELECT *
*    INTO TABLE IT_ZFI008
*    FROM ZFI008
*    FOR ALL ENTRIES IN IT_VBAKAP
*    WHERE VBELN = IT_VBAKAP-VBELN
*      AND POSNR = IT_VBAKAP-POSNR
*      AND SFNCX = ''.
  SELECT *
   INTO TABLE it_zfi008
   FROM zfi008
   FOR ALL ENTRIES IN it_vbakap
   WHERE vbeln = it_vbakap-vbeln
     AND posnr = it_vbakap-posnr
     AND sfncx = ''
     AND is_adjust = ''.   "IT02 ADD where 是否为空  150623
  SORT it_zfi008 BY vbeln posnr.

  LOOP AT it_vbakap INTO wa_vbakap WHERE pstyv = 'Z01' OR pstyv = 'Z02'   " 仅输出所有开票行项目
    OR  pstyv = 'Z21' OR pstyv = 'Z22'  OR  pstyv = 'Z31' OR pstyv = 'Z32' OR  pstyv = 'Z41' OR pstyv = 'Z42' .  "IT02 增加 行项目类型 租赁 、服务
    CLEAR wa_qkcx.

    wa_qkcx-bukrs = wa_vbakap-bukrs_vf.
    wa_qkcx-vbeln = wa_vbakap-vbeln.
    "  WA_QKCX-POSNR = WA_VBAKAP-POSNR.
    wa_qkcx-netwr = wa_vbakap-netwr.
    wa_qkcx-waerk = wa_vbakap-waerk.

    " 已开票金额
    LOOP AT it_zfi008 INTO wa_zfi008 WHERE vbeln = wa_qkcx-vbeln
                                        AND posnr = wa_vbakap-posnr.
      " AND POSNR = WA_QKCX-POSNR.
      IF wa_zfi008-bschl = '40'.
        wa_zfi008-wrbtr = - wa_zfi008-wrbtr. "add IT02 150623 若记账码为40 ，就负数累加
      ENDIF.
      wa_qkcx-wrbtr = wa_qkcx-wrbtr + wa_zfi008-wrbtr.
    ENDLOOP.

    " 可开票金额
    wa_qkcx-srdkp = wa_qkcx-netwr - wa_qkcx-wrbtr.

    " 是否确认收入
    READ TABLE it_vbuk INTO wa_vbuk WITH KEY vbeln = wa_qkcx-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_vbuk-fksak = 'C'.
        wa_qkcx-fksak = 'X'.
      ENDIF.
    ENDIF.

    " APPEND WA_QKCX TO IT_QKCX. "150521
    COLLECT wa_qkcx INTO it_qkcx.
  ENDLOOP.

  "IT02 150521 BEGIN
  CLEAR wa_qkcx.
  IF it_qkcx[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vbel2
      FROM bseg
      FOR ALL ENTRIES IN it_qkcx    "根据公司代码、销售订单、会计科目 读取bseg表数据 IT02 150623
      WHERE bukrs = it_qkcx-bukrs AND vbel2 = it_qkcx-vbeln AND hkont = '6001019901' .
    SORT gt_vbel2 BY bukrs vbel2 hkont .
  ENDIF.
  LOOP AT it_qkcx INTO wa_qkcx.
    LOOP AT gt_vbel2 WHERE bukrs = wa_qkcx-bukrs AND vbel2 = wa_qkcx-vbeln .
      IF gt_vbel2-shkzg = 'S'.
        gt_vbel2-pswbt = - gt_vbel2-pswbt.  "根据公司代码、销售订单汇总 收入-调整字段的数据 it02 150623
      ENDIF.
      wa_qkcx-pswbt = wa_qkcx-pswbt + gt_vbel2-pswbt.
    ENDLOOP.
    PERFORM frm_get_pjnam USING wa_qkcx-vbeln
                       CHANGING wa_qkcx-pjnam.
    MODIFY it_qkcx FROM wa_qkcx.
  ENDLOOP.
  "IT02 150521 END
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC_QKCX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc_qkcx .

  init_fieldcat2 'BUKRS' '' '' '' '' 'VBAK' 'BUKRS_VF' ''.
  init_fieldcat2 'VBELN' '' '' '' '' 'VBAP' 'VBELN' ''.
  init_fieldcat2 'POSNR' '' '' '' '' 'VBAP' 'POSNR' ''.
  init_fieldcat2 'NETWR' text-f06 '' 'WAERK' '' 'VBAP' 'NETWR' ''.
  init_fieldcat2 'WRBTR' text-f07 '' 'WAERK' '' 'BSEG' 'WRBTR' ''.
  init_fieldcat2 'SRDKP' text-f08 '' 'WAERK' '' 'BSEG' 'WRBTR' ''.
  init_fieldcat2 'WAERK' '' '' '' '' 'VBAP' 'WAERK' ''.
  init_fieldcat2 'FKSAK' '' '' '' '' 'VBUK' 'FKSAK' 'X'.
  init_fieldcat2 'PJNAM' '项目名称' '' '' '' '' '' ''.          "150521
  init_fieldcat2 'PSWBT' '收入-调整' '' 'WAERK' '' 'BSEG' 'PSWBT' ''. . "IT02 ADD 150623 新增收入-调整字段信息
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_changed  CHANGING p_subrc TYPE sy-subrc.
  p_subrc = 0.
  IF g_changed = abap_true.
    DATA l_ans.
    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
*       DEFAULTOPTION       = 'Y'
        textline1 = text-m20
        textline2 = text-m21
        titel     = text-m19
*       START_COLUMN        = 25
*       START_ROW = 6
      IMPORTING
        answer    = l_ans.
    IF l_ans = 'N'.
      p_subrc = 2.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_ZFI008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save_zfi008 .
  " 保存到自建表记录两张凭证的对应关系等
  REFRESH it_zfi008.
  CLEAR wa_zfi008.
  wa_zfi008-bukrs = wa_head-bukrs.
  wa_zfi008-budat = wa_head-budat.
  wa_zfi008-belnr = wa_head-belnr.
  wa_zfi008-gjahr = wa_head-gjahr.
  wa_zfi008-refbe = wa_head-belnr2.
  wa_zfi008-refgj = wa_head-gjahr2.
  wa_zfi008-sfncx = ''.
  LOOP AT it_item INTO wa_item WHERE hkont BETWEEN 6001010101 AND 6001019901 .   " 6001019899.
    wa_zfi008-buzei = wa_item-buzei.
    wa_zfi008-vbeln = wa_item-vbeln.
    wa_zfi008-posnr = wa_item-posnr.
    wa_zfi008-wrbtr = wa_item-wrbtr.
    wa_zfi008-waers = wa_item-waers.
    wa_zfi008-bschl = wa_item-bschl."IT02 ADD 150623 新增保存记账码
*    IF wa_item-hkont = '6001019901'. ”it02 20170713 注释
*      wa_zfi008-is_adjust = 'X'.   "IT02 ADD 科目为6001019901主营业务收入-非关联方-调整,科目调整设为X ,否则为空
*    ELSE.
*      wa_zfi008-is_adjust = ''.
*    ENDIF.
    "add it02 20170713 begin
    READ TABLE gt_srys INTO gs_srys WITH KEY hkont = wa_item-hkont
                                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_zfi008-is_adjust = 'X'.
    ELSE.
      wa_zfi008-is_adjust = ''.
    ENDIF.
    "add it02 20170713 end
    APPEND wa_zfi008 TO it_zfi008.
  ENDLOOP.

  IF it_zfi008 IS NOT INITIAL.
    MODIFY zfi008 FROM TABLE it_zfi008.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_acc_reversal .
  READ TABLE it_kpjl TRANSPORTING NO FIELDS WITH KEY box = 'X'.
  IF sy-subrc NE 0.
    MESSAGE '请先选择需要冲销的凭证！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA: l_count TYPE i.
  CLEAR l_count.
  LOOP AT it_kpjl INTO wa_kpjl WHERE box = 'X'.
    l_count = l_count + 1.
  ENDLOOP.
  IF l_count > 1.
    MESSAGE '请仅选择一条凭证进行冲销！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " 取冲销原因和过账日期
  CLEAR wa_reversal.
  PERFORM frm_get_rev_reason.

  IF wa_reversal-reason_rev IS INITIAL.
    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM frm_message_initial.
  CLEAR g_suc.

  READ TABLE it_kpjl INTO wa_kpjl WITH KEY box = 'X'.
  IF sy-subrc = 0.
    " 冲销凭证1
    SELECT SINGLE *
      INTO wa_bkpf
      FROM bkpf
      WHERE bukrs = wa_kpjl-bukrs
        AND belnr = wa_kpjl-belnr
        AND gjahr = wa_kpjl-gjahr.

    PERFORM frm_rev_prep.

    PERFORM frm_reversal_bapi.

    " 冲销凭证2
    SELECT SINGLE *
      INTO wa_bkpf
      FROM bkpf
      WHERE bukrs = wa_kpjl-bukrs
        AND belnr = wa_kpjl-refbe
        AND gjahr = wa_kpjl-refgj.

    PERFORM frm_rev_prep.

    PERFORM frm_reversal_bapi.
  ENDIF.

  IF g_suc = 'X'.
*    read table it_kpjl into wa_kpjl with key box = 'X'.
*    if sy-subrc = 0.
*      wa_kpjl-sfncx = 'X'.
*      modify it_kpjl from wa_kpjl index sy-tabix transporting sfncx.
*    endif.

    DATA: l_bukrs TYPE zfi008-bukrs,
          l_belnr TYPE zfi008-belnr,
          l_gjahr TYPE zfi008-gjahr.

    l_bukrs = wa_kpjl-bukrs.
    l_belnr = wa_kpjl-belnr.
    l_gjahr = wa_kpjl-gjahr.

    LOOP AT it_kpjl INTO wa_kpjl WHERE bukrs = l_bukrs
                                   AND belnr = l_belnr
                                   AND gjahr = l_gjahr.
      wa_kpjl-sfncx = 'X'.
      MODIFY it_kpjl FROM wa_kpjl TRANSPORTING sfncx.
    ENDLOOP.

    MODIFY zfi008 FROM wa_kpjl.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  PERFORM frm_message_show.

*  perform frm_refresh_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_REV_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_rev_reason .
  CALL SCREEN 9002 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REV_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_rev_prep .
  wa_reversal-obj_type  = wa_bkpf-awtyp.
  wa_reversal-obj_key   = wa_bkpf-awkey.
  wa_reversal-obj_key_r = wa_bkpf-awkey.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSAL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_reversal_bapi .
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      reversal = wa_reversal
      bus_act  = 'RFBU'
    IMPORTING
      obj_type = wa_obj-obj_type
      obj_key  = wa_obj-obj_key
      obj_sys  = wa_obj-obj_sys
    TABLES
      return   = it_return.

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    g_suc = 'X'.
  ENDIF.

  LOOP AT it_return INTO wa_return.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.

*    write: / g_msg.
    PERFORM store_messages USING wa_return-id
                                 wa_return-type
                                 wa_return-message_v1
                                 wa_return-message_v2
                                 wa_return-message_v3
                                 wa_return-message_v4
                                 wa_return-number.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check USING VALUE(p_actvt)
                          VALUE(p_bukrs).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD p_actvt
                                      ID 'BUKRS' FIELD p_bukrs.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_PJNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_HEAD_VBELN  text
*      <--P_WA_HEAD_PJNAM  text
*----------------------------------------------------------------------*
FORM frm_get_pjnam  USING    p_vbeln
                    CHANGING p_pjnam.

  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  REFRESH: it_lines.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = '1'
      name                    = g_objname
      object                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  "  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  "   MESSAGE '项目名称读取错误!' TYPE 'W'.
  "  ENDIF.

  READ TABLE it_lines INTO wa_lines INDEX 1.
  IF sy-subrc = 0.
    p_pjnam = wa_lines-tdline.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ITEM_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_item_init .
  CHECK it_item IS INITIAL.
  DATA:gt_vbak LIKE TABLE OF vbak WITH HEADER LINE.
  DATA: l_row_id TYPE lvc_s_row,
        l_col_id TYPE lvc_s_col,
        l_row_no TYPE lvc_s_roid.
  DATA:t_posnr LIKE vbap-posnr.
  DATA:xnh  TYPE i . "虚拟行判断
  CLEAR:gt_vbak,gt_vbak[].
  "150610 读取销售订单的 类型、销售组织 begin
  TRANSLATE wa_head-vbeln  TO UPPER CASE .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vbak
    FROM vbak
    WHERE vbeln = wa_head-vbeln .
  SORT it_vbap BY vbeln  posnr . "按销售订单、行项目排序  150521
  "READ TABLE IT_VBAP INTO WA_VBAP INDEX 1.
  READ TABLE  gt_vbak INDEX 1.
  IF sy-subrc EQ 0 .
    IF gt_vbak-auart = 'ZPO' OR gt_vbak-auart = 'ZF1' OR gt_vbak-auart = 'ZWV' OR gt_vbak-auart = 'ZZG'.
      LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_head-vbeln.
        CLEAR:xnh.
        xnh = wa_vbap-posnr MOD 100 .
        IF xnh EQ 0 .
          t_posnr = wa_vbap-posnr.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSEIF gt_vbak-auart = 'ZSO'.
      LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_head-vbeln.
        CLEAR:xnh.
        xnh = wa_vbap-posnr MOD 10 .
        IF xnh EQ 0 .
          t_posnr = wa_vbap-posnr.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


                                                            "150610 end
  " 应收
  CLEAR wa_item.
  wa_item-buzei = '1'.
  wa_item-bschl = '01'.

  "客户改为：RE收票方
  READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_head-vbeln BINARY SEARCH .
  IF sy-subrc = 0 .
    wa_item-kunnr = wa_vbpa-kunnr.
  ENDIF.

  "WA_ITEM-KUNNR = WA_VBAK-KUNNR.

*ADD BY HANDWY  根据客户自动带统驭科目
*调整科目确定逻辑 20171109
  DATA: lv_vbund TYPE kna1-vbund,
        lv_tptpy TYPE zfi008_1-tptpy. "是否关联方'X'为关联方，" "为非关联方
  CLEAR: lv_vbund,wa_item-hkont,lv_tptpy.
  SELECT SINGLE vbund
    INTO lv_vbund
    FROM kna1
    WHERE kunnr = wa_item-kunnr.
  IF lv_vbund IS NOT INITIAL.
    lv_tptpy = 'X'.
  ENDIF.
  IF wa_head-bstyp = '1' OR wa_head-bstyp = '2' OR wa_head-bstyp = '3'.
    "直接从客户主数据取数
    SELECT SINGLE akont  FROM knb1
      INTO  wa_item-hkont
      WHERE kunnr = wa_item-kunnr
      AND   bukrs = wa_head-bukrs.
  ELSEIF wa_head-bstyp = '4'.
    IF lv_tptpy IS INITIAL.
      "非关联方
      wa_item-hkont = '1122010301'."应收账款-非关联方-调整
    ELSE.
      "关联方
      wa_item-hkont = '1122020201'."应收账款-关联方-调整
    ENDIF.
  ENDIF.

*  IF wa_head-bstyp = '4'.
*    wa_item-hkont = '1122010301' . "新增业务类型科目替代为为1122010301应收账款-非关联方-调整
*  ENDIF.
*调整科目确定逻辑 20171109
  READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_item-hkont BINARY SEARCH.
  IF sy-subrc = 0.
    wa_item-hktxt = wa_skat-txt20.
  ENDIF.

  SELECT SINGLE name1
    INTO wa_item-kname
    FROM kna1
    WHERE kunnr = wa_item-kunnr.
  wa_item-waers = wa_head-waers.
  wa_item-zterm = wa_vbkd-zterm.
  wa_item-zfbdt = sy-datum.
  wa_item-vbeln = wa_head-vbeln.
  " WA_ITEM-POSNR = '100'.  IT02 150521 注释
  wa_item-posnr = t_posnr.
  " ENDIF.
*add by handwy 2015-4-15
  READ TABLE it_vbap INTO wa_vbap
  WITH KEY vbeln = wa_item-vbeln
           posnr = wa_item-posnr.
  IF sy-subrc = 0 .
    wa_item-matnr = wa_vbap-matnr.

  ENDIF.
  PERFORM frm_cell_style USING 'KUNNR'
                         'X'
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZTERM'
                         'X'
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZFBDT'
                         'X'
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'MWSKZ'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'POSNR'
                         'X'
                         CHANGING wa_item-cellstyle.
  APPEND wa_item TO it_item.

  " 收入
  CLEAR wa_item.
  wa_item-buzei = '2'.
  wa_item-bschl = '50'.
  "IT02 ADD 选择业务类型1、2 收入科目变更 ,其他不变  150623 begin
  "先根据配置表读取科目
  SELECT SINGLE saknr
    INTO wa_item-hkont
    FROM zfi008_1
    WHERE bukrs = wa_head-bukrs
    AND   vkorg = gt_vbak-vkorg
    AND   auart = gt_vbak-auart
    AND   bstyp = wa_head-bstyp
    AND   tptpy = lv_tptpy.
  IF sy-subrc NE 0.
    wa_item-hkont = '6001010101'. "期初设置主营业务收入-非关联方-硬件收入
    IF wa_head-bstyp = '1' OR  wa_head-bstyp = '2' .
      READ TABLE gt_vbak WITH KEY auart = 'ZSO'.
      IF sy-subrc = 0.
        wa_item-hkont = '6001010501' ."6001010501：主营业务收入-非关联方-广告收入
      ENDIF.
      READ TABLE gt_vbak WITH KEY auart = 'ZWV'.
      IF sy-subrc = 0.
        wa_item-hkont = '6001010601' ."6001010601：主营业务收入-非关联方-租赁收入
      ENDIF.
    ENDIF.
    "IT02 ADD 选择业务类型1、2 收入科目变更 ,其他不变  150623 end
    "IT02 150610 若销售订单类型为“ZPO‘ 销售组织为'1100' 收入为""6001010901
    READ TABLE gt_vbak WITH KEY vkorg = '1110' auart = 'ZPO'.
    IF sy-subrc = 0 .
      wa_item-hkont = '6001010901' ."主营业务收入-非关联方-出口收入
*  ELSE.
*   WA_ITEM-HKONT = '6001010101'. "主营业务收入-非关联方-硬件收入
    ENDIF.
    "APPEND  销售订单类型为 ZPO，销售组织1510，收入科目默认值为6001010901. IT02 151022 begin
    READ TABLE gt_vbak WITH KEY vkorg = '1510' auart = 'ZPO'.
    IF sy-subrc = 0 .
      wa_item-hkont = '6001010901' ."主营业务收入-非关联方-出口收入
    ENDIF.
                                                            "END 151022
    IF wa_head-bstyp = '4'.
      wa_item-hkont = '6001019901'.
    ENDIF.
  ENDIF.

  "IT02 150610 end
  " WA_ITEM-HKONT = '6001010101'. "主营业务收入-非关联方-硬件收入
  READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_item-hkont BINARY SEARCH.
  IF sy-subrc = 0.
    wa_item-hktxt = wa_skat-txt20.
  ENDIF.
  wa_item-waers = wa_head-waers.
  wa_item-vbeln = wa_head-vbeln.
  " WA_ITEM-POSNR = '100'. "150521
  wa_item-posnr = t_posnr.

*ADD BY HANDWY 2015-04-15
  IF wa_head-bstyp = '3'.
    wa_item-mwskz = 'X0'.
  ELSE.
    wa_item-mwskz = 'X1'.
  ENDIF.

*add by handwy 2015-4-15
  READ TABLE it_vbap INTO wa_vbap
  WITH KEY vbeln = wa_item-vbeln
           posnr = wa_item-posnr.
  wa_item-matnr = wa_vbap-matnr.

  PERFORM frm_cell_style USING 'KUNNR'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZTERM'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'ZFBDT'
                         ''
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'MWSKZ'
                         'X'
                         CHANGING wa_item-cellstyle.
  PERFORM frm_cell_style USING 'POSNR'
                         'X'
                         CHANGING wa_item-cellstyle.
  APPEND wa_item TO it_item.

*ADD BY HANDWY 2015-4-15
  IF wa_head-bstyp <> '3'.
    " 税
    CLEAR wa_item.
    wa_item-buzei = '3'.
    wa_item-bschl = '50'.
    wa_item-hkont = '2221030501'.
    IF wa_head-bstyp EQ '4'.
      wa_item-hkont = '2221030503'. "新增业务类型4 科目更改为2221030503应交税费-应交增值税-销项税额-调整
    ENDIF.
    READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_item-hkont BINARY SEARCH.
    IF sy-subrc = 0.
      wa_item-hktxt = wa_skat-txt20.
    ENDIF.
    wa_item-waers = wa_head-waers.
    wa_item-mwskz = 'X1'.

    PERFORM frm_cell_style USING 'KUNNR'
                           ''
                           CHANGING wa_item-cellstyle.
    PERFORM frm_cell_style USING 'ZTERM'
                           ''
                           CHANGING wa_item-cellstyle.
    PERFORM frm_cell_style USING 'ZFBDT'
                           ''
                           CHANGING wa_item-cellstyle.
    PERFORM frm_cell_style USING 'MWSKZ'
                           'X'
                           CHANGING wa_item-cellstyle.
    PERFORM frm_cell_style USING 'POSNR'
                           ''
                           CHANGING wa_item-cellstyle.
    APPEND wa_item TO it_item.
  ENDIF.

  PERFORM frm_refresh_alv.

  l_row_id = 1.
  l_col_id = 'WRBTR'.
  l_row_no-row_id = 1.

  CALL METHOD gr_alvgrid->set_current_cell_via_id
    EXPORTING
      is_row_id    = l_row_id
      is_column_id = l_col_id
      is_row_no    = l_row_no.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_SRYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_pre_srys .
  "添加收入科目映射关系
  CLEAR:gs_srys.
  gs_srys-hkont = '6001010101'.                     "主营业务收入-非关联方-硬件收入
  gs_srys-hkont_tz = '6001019901'.                  "主营业务收入-非关联方-硬件收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010201'.                     "主营业务收入-非关联方-软件收入
  gs_srys-hkont_tz = '6001010202'.                  "主营业务收入-非关联方-软件收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010301'.                      "主营业务收入-非关联方-工程收入
  gs_srys-hkont_tz = '6001010302'.                   "主营业务收入-非关联方-工程收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010401'.                      "主营业务收入-非关联方-维修收入
  gs_srys-hkont_tz = '6001010402'."                  "主营业务收入-非关联方-维修收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010501'.                      "主营业务收入-非关联方-广告收入
  gs_srys-hkont_tz = '6001010502'.                   "主营业务收入-非关联方-广告收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010601'.                      "主营业务收入-非关联方-租赁收入
  gs_srys-hkont_tz = '6001010602'.                   "主营业务收入-非关联方-租赁收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010701'.                      "主营业务收入-非关联方-技术服务收入
  gs_srys-hkont_tz = '6001010702'.                   " 主营业务收入-非关联方-技术服务收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010801'.                     "主营业务收入-非关联方-加工材料收入
  gs_srys-hkont_tz = '6001010802'.                  " 主营业务收入-非关联方-加工材料收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001010901'.                      "主营业务收入-非关联方-出口收入
  gs_srys-hkont_tz = '6001010902'.                   " 营业务收入-非关联方-出口收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001011001'.                     "主营业务收入-非关联方-设计收入
  gs_srys-hkont_tz = '6001011002'.                  "主营业务收入-非关联方-设计收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001012001'.                      "主营业务收入-非关联方-产品收入
  gs_srys-hkont_tz = '6001012002'.                   "主营业务收入-非关联方-产品收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001019901'.                      "主营业务收入-非关联方-调整
  gs_srys-hkont_tz = '6001019901'.                   "主营业务收入-非关联方-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020101'.                       "主营业务收入-关联方-硬件收入
  gs_srys-hkont_tz = '6001029901'.                    "主营业务收入-关联方-硬件收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020201'.                      "主营业务收入-关联方-软件收入
  gs_srys-hkont_tz = '6001020202'.                   "主营业务收入-关联方-软件收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020301'.                       "主营业务收入-关联方-工程收入
  gs_srys-hkont_tz = '6001020302'.                    "主营业务收入-关联方-工程收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020401'.                       "主营业务收入-关联方-维修收入
  gs_srys-hkont_tz = '6001020402'.                    "主营业务收入-关联方-维修收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020501'.                       "主营业务收入-关联方-广告收入
  gs_srys-hkont_tz = '6001020502'.                     "主营业务收入-关联方-广告收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020601'.                       "主营业务收入-关联方-租赁收入
  gs_srys-hkont_tz = '6001020602'.                    "主营业务收入-关联方-租赁收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020701'.                      "主营业务收入-关联方-技术服务收入
  gs_srys-hkont_tz = '6001020702'.                   "主营业务收入-关联方-技术服务收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020801'.                      "主营业务收入-关联方-加工材料收入
  gs_srys-hkont_tz = '6001020802'.                   "主营业务收入-关联方-加工材料收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001020901'.                      "主营业务收入-关联方-出口收入
  gs_srys-hkont_tz = '6001020902'.                   " 主营业务收入-关联方-出口收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001021001'.                      "主营业务收入-关联方-产品收入
  gs_srys-hkont_tz = '6001022002'.                   "主营业务收入-关联方-产品收入-调整
  APPEND gs_srys TO gt_srys.
  gs_srys-hkont = '6001029901'.                      "主营业务收入-关联方-调整
  gs_srys-hkont_tz = '6001029901'.                   "主营业务收入-关联方-调整
  APPEND gs_srys TO gt_srys.
  SORT gt_srys BY hkont.
ENDFORM.
