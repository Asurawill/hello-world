*&---------------------------------------------------------------------*
*&  包含                ZCO004_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
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
*    CLEAR LW_TOOLBAR.
*    LW_TOOLBAR-FUNCTION    = 'APPEND_ROW '.
**    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*    LW_TOOLBAR-QUICKINFO   = '新增行'.
*    LW_TOOLBAR-ICON        = ICON_CREATE.
*    LW_TOOLBAR-DISABLED    = SPACE.
*    INSERT LW_TOOLBAR INTO P_E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ABOVE_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_above_alv_display .
  DATA: lr_event_handler TYPE REF TO lcl_event_handler,
        ls_stable        TYPE lvc_s_stbl.

* ALV容器为空则创建，不为空则刷新
  IF g_above_dock_container IS INITIAL.
*  创建 容器
    CREATE OBJECT g_above_dock_container
      EXPORTING
        repid                       = sy-repid
        dynnr                       = '9001'    "alv所在屏幕
        "dock_at_top:从顶部开始计算占据屏幕的比例；dock_at_bottom:从底部开始计算占据屏幕的比例
        "dock_at_left:从左部开始计算占据屏幕的比例；dock_at_right:从右部开始计算占据屏幕的比例；
        side                        = cl_gui_docking_container=>dock_at_top
        ratio                       = 50       "占屏幕的比例
*       extension                   = 260
*       caption                     = '' "标题
        lifetime                    = cntl_lifetime_dynpro   "容器实例生命周期
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


*  创建容器实例
    CREATE OBJECT g_above_grid
      EXPORTING
        i_parent          = g_above_dock_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   下面是OOALV常用的几个参数
    wa_above_layout-zebra = 'X'.     "X:行项目带颜色标识
    wa_above_layout-no_rowins = 'X'. "X:不允许删除选定行,通过复制粘贴维护可编辑行限定在一定范围内
    wa_above_layout-sel_mode = 'A'.  "A:显示alv的选择行按
    wa_above_layout-box_fname = 'ZBOX'.  "A:显示alv的选择行按
*    wa_above_layout-no_toolbar = 'X'."X:不显示系统标准工具条，该参数默认为空即显示工具条
    wa_above_variant-report = sy-repid.

*  删除系统标准工具条按钮，结合no_toolbar使用
    PERFORM frm_above_alv_exclude.

*  设置输出显示字段
    PERFORM frm_above_fieldcat.

***  设置下拉列表
*   PERFORM FRM_SET_DROP_DOWN_LIST.

**设置事件
    CREATE OBJECT lr_event_handler.
    SET HANDLER lr_event_handler->handle_double_click FOR g_above_grid. "双击事件
*    SET HANDLER LR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR G_ABOVE_GRID."单击事件
    SET HANDLER lr_event_handler->handle_user_command FOR g_above_grid. "按钮事件
    SET HANDLER lr_event_handler->handle_data_changed FOR g_above_grid. "数据改动事件
    SET HANDLER lr_event_handler->handle_toolbar      FOR g_above_grid. "工具条事件

* 注册为 F4 处理的字段表
*    PERFORM FRM_ALV_F4.
    SET HANDLER lr_event_handler->handle_onf4 FOR g_above_grid.        "搜索帮助事件

    CALL METHOD cl_gui_cfw=>flush. "刷新

    CALL METHOD g_above_grid->register_edit_event  "注册回车事件
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_above_grid->register_edit_event  "注册编辑事件
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.


    CALL METHOD g_above_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = 'X'
        is_variant                    = wa_above_variant
        i_save                        = 'A'
        i_default                     = 'X'
        is_layout                     = wa_above_layout
        it_toolbar_excluding          = it_above_exclude
      CHANGING
        it_outtab                     = gt_data_above      "输出数据的内表
        it_fieldcatalog               = it_above_fieldcat  "
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.
  ELSE.
    PERFORM frm_refresh_above_alv.
  ENDIF.
ENDFORM. " FRM_ABOVE_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FRM_BELOW_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_below_alv_display .

  DATA: lr_event_handler TYPE REF TO lcl_event_handler,
        ls_stable        TYPE lvc_s_stbl.

* ALV容器为空则创建，不为空则刷新
  IF g_below_dock_container IS INITIAL.
*  创建 容器
    CREATE OBJECT g_below_dock_container
      EXPORTING
        repid                       = sy-repid
        dynnr                       = '9001'    "alv所在屏幕
        "dock_at_top:从顶部开始计算占据屏幕的比例；dock_at_bottom:从底部开始计算占据屏幕的比例
        "dock_at_left:从左部开始计算占据屏幕的比例；dock_at_right:从右部开始计算占据屏幕的比例；
        side                        = cl_gui_docking_container=>dock_at_bottom
        ratio                       = 50       "占屏幕的比例
*       extension                   = 260
*       caption                     = '' "标题
        lifetime                    = cntl_lifetime_dynpro   "容器实例生命周期
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*  创建容器实例
    CREATE OBJECT g_below_grid
      EXPORTING
        i_parent          = g_below_dock_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   下面是OOALV常用的几个参数
    wa_below_layout-zebra = 'X'.     "X:行项目带颜色标识
*    WA_BELOW_LAYOUT-NO_ROWINS = 'X'. "X:不允许删除选定行,通过复制粘贴维护可编辑行限定在一定范围内
    wa_below_layout-sel_mode = 'A'.  "A:显示alv的选择行按
*    wa_below_layout-no_toolbar = 'X'."X:不显示系统标准工具条，该参数默认为空即显示工具条
    wa_below_variant-report = sy-repid.

*  删除系统标准工具条按钮，结合no_toolbar使用
    PERFORM frm_below_alv_exclude.

*  设置输出显示字段
    PERFORM frm_below_fieldcat1.

**  设置下拉列表
*    PERFORM FRM_SET_DROP_DOWN_LIST.

**设置事件
    CREATE OBJECT lr_event_handler.
*    SET HANDLER LR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK FOR G_BELOW_GRID. "双击事件
*    SET HANDLER LR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR G_BELOW_GRID."单击事件
    SET HANDLER lr_event_handler->handle_user_command FOR g_below_grid. "按钮事件
    SET HANDLER lr_event_handler->handle_data_changed FOR g_below_grid. "数据改动事件
*    SET HANDLER LR_EVENT_HANDLER->HANDLE_TOOLBAR      FOR G_BELOW_GRID. "工具条事件

* 注册为 F4 处理的字段表
*    PERFORM FRM_ALV_F4_1.
    SET HANDLER lr_event_handler->handle_onf4 FOR g_below_grid.        "搜索帮助事件

    CALL METHOD cl_gui_cfw=>flush. "刷新

    CALL METHOD g_below_grid->register_edit_event  "注册回车事件
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_below_grid->register_edit_event  "注册编辑事件
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_below_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = 'X'
        is_variant                    = wa_below_variant
        i_save                        = 'A'
        i_default                     = 'X'
        is_layout                     = wa_below_layout
        it_toolbar_excluding          = it_below_exclude
      CHANGING
        it_outtab                     = gt_data_below      "输出数据的内表
        it_fieldcatalog               = it_below_fieldcat  "
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    PERFORM frm_refresh_below_alv.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ABOVE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_refresh_above_alv .
  DATA ls_stable TYPE lvc_s_stbl.

  DATA:
    ls_celltab TYPE lvc_s_styl,
    lt_celltab TYPE lvc_t_styl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CALL METHOD g_above_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " FRM_REFRESH_ABOVE_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_BELOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_refresh_below_alv .
  DATA:
    ls_celltab TYPE lvc_s_styl , "
    lt_celltab TYPE lvc_t_styl.

  DATA ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CALL METHOD g_below_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. " FRM_REFRESH_BELOW_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_above_alv_exclude .
  DATA: ls_above_exclude TYPE ui_func.
  REFRESH it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_above_exclude TO it_above_exclude.
*
  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_mb_view.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_above_exclude TO it_above_exclude.

  CLEAR ls_above_exclude.
  ls_above_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_above_exclude TO it_above_exclude.

ENDFORM. " FRM_ALV_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_BELOW_ALV_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_below_alv_exclude .
  DATA: ls_below_exclude TYPE ui_func.
  REFRESH it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_below_exclude TO it_below_exclude.

*  CLEAR LS_BELOW_EXCLUDE.
*  LS_BELOW_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_BELOW_EXCLUDE TO IT_BELOW_EXCLUDE.
*
*  CLEAR LS_BELOW_EXCLUDE.
*  LS_BELOW_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_BELOW_EXCLUDE TO IT_BELOW_EXCLUDE.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_below_exclude TO it_below_exclude.
*
  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_mb_view.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_below_exclude TO it_below_exclude.

  CLEAR ls_below_exclude.
  ls_below_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_below_exclude TO it_below_exclude.
ENDFORM. " FRM_BELOW_ALV_EXCLUDE
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
*&      Form  FRM_ABOVE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_above_fieldcat .
  init_fieldcat 'BUKRS'   '公司代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'GJAHR'   '会计年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'   '记账期间'          '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'   '物料'             '' '' '' '' '' 'MARA' 'MATNR'.
  init_fieldcat 'MAKTX'   '物料描述'         40 '' '' '' '' '' ''.
  init_fieldcat 'WERKS'   '工厂'            '' '' '' '' '' '' ''.
  init_fieldcat 'STPRS'   '标准价'           '' '' '' '' '' '' ''.
  init_fieldcat 'VERPR'   '实际价'           '' '' '' '' '' '' ''.
  init_fieldcat 'DMBTR'   '销售发货差异'         15 '' '' '' '' '' ''.
  init_fieldcat 'GJAHR_1'   '过账年度'           '' '' '' '' '' '' ''.
  init_fieldcat 'BELNR_1'   '过账凭证'           '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BELOW_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_below_fieldcat1 .
  init_fieldcat1 'WERKS'   '工厂'         '' '' '' '' '' '' ''.
  init_fieldcat1 'MATNR'   '物料'         20 '' '' '' '' 'MARA' 'MATNR'.
  init_fieldcat1 'VBEL2'   '销售订单号'         '' '' '' '' '' '' ''.
  init_fieldcat1 'POSN2'   '销售订单行项目'         15 '' '' '' '' '' ''.
  init_fieldcat1 'MENGE'   '数量'         '' '' '' '' '' '' ''.
  init_fieldcat1 'MEINS'   '单位'         '' '' '' '' '' '' ''.
  init_fieldcat1 'SFAD'    '是否按单'         '' '' '' '' '' '' ''.
  init_fieldcat1 'CYFT'    '差异分摊'         '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  SELECT * FROM bkpf
  INTO CORRESPONDING FIELDS OF TABLE gt_bkpf_above
  WHERE bukrs = p_bukrs
  AND   gjahr = p_gjahr
  AND   monat = p_monat
  AND   blart = 'ML'
  AND   stblg = ''.

  CHECK gt_bkpf_above IS NOT INITIAL.

  SELECT * FROM bseg
  INTO CORRESPONDING FIELDS OF TABLE gt_bseg_above
  FOR ALL ENTRIES IN gt_bkpf_above
  WHERE gjahr = gt_bkpf_above-gjahr
  AND   bukrs = gt_bkpf_above-bukrs
  AND   belnr = gt_bkpf_above-belnr
  AND   hkont BETWEEN '6404000000' AND '6404999999'
  AND   vbel2 = ''."150504MY IT03 增加筛选条件VBEL2 = ''

  CHECK gt_bseg_above IS NOT INITIAL.

  SELECT * FROM zco004
  INTO CORRESPONDING FIELDS OF TABLE gt_zco004
  FOR ALL ENTRIES IN gt_bseg_above
  WHERE bukrs = gt_bseg_above-bukrs
  AND   gjahr = gt_bseg_above-gjahr
  AND   belnr = gt_bseg_above-belnr.

  SELECT * FROM makt
  INTO CORRESPONDING FIELDS OF TABLE gt_makt_above
  FOR ALL ENTRIES IN gt_bseg_above
  WHERE matnr = gt_bseg_above-matnr AND spras = sy-langu.

*BKLAS
  SELECT
    mbew~matnr
    mbew~bwkey AS werks
    mbew~bklas
    mbewh~verpr
    mbewh~stprs
    FROM mbew LEFT JOIN mbewh
    ON  mbew~matnr = mbewh~matnr
    AND mbew~bwkey = mbewh~bwkey
    AND   mbewh~lfgja = p_gjahr
    AND   mbewh~lfmon = p_monat
    INTO CORRESPONDING FIELDS OF TABLE gt_mbew_above
    FOR ALL ENTRIES IN gt_bseg_above
    WHERE mbew~matnr = gt_bseg_above-matnr
    AND   mbew~bwkey = gt_bseg_above-werks
    .

*第一天
  CONCATENATE p_gjahr p_monat '01' INTO l_first_day.
  l_frstd = l_first_day.

*最后一天
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = l_frstd
    IMPORTING
      last_day_of_month = l_lastd
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .

  gt_bseg_above_1 = gt_bseg_above.

*删除相同物料的行
  SORT gt_bseg_above_1 BY matnr.
  DELETE gt_bseg_above_1 WHERE matnr IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM gt_bseg_above_1 COMPARING matnr werks.

  LOOP AT gt_bseg_above_1 INTO gs_bseg_above_1.

*排除自己过账的ML凭证
    READ TABLE gt_zco004 INTO gs_zco004
    WITH KEY bukrs = gs_bseg_above_1-bukrs
             gjahr = gs_bseg_above_1-gjahr
             belnr = gs_bseg_above_1-belnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

*写入过账凭证
    READ TABLE gt_zco004 INTO gs_zco004
    WITH KEY bukrs = gs_bseg_above_1-bukrs
             matnr = gs_bseg_above_1-matnr.
    IF sy-subrc = 0 .
      gs_data_above-belnr_1 = gs_zco004-belnr.
      gs_data_above-gjahr_1 = gs_zco004-gjahr.
    ENDIF.

    gs_data_above-bukrs = p_bukrs.
    gs_data_above-gjahr = p_gjahr.
    gs_data_above-monat = p_monat.
    gs_data_above-matnr = gs_bseg_above_1-matnr.
    gs_data_above-werks = gs_bseg_above_1-werks.

*查询物料描述
    READ TABLE gt_makt_above INTO gs_makt_above
    WITH KEY matnr = gs_bseg_above_1-matnr.
    IF sy-subrc = 0.
      gs_data_above-maktx = gs_makt_above-maktx.
    ENDIF.

*查询物料价格
    READ TABLE gt_mbew_above INTO gs_mbew_above
    WITH KEY matnr = gs_bseg_above_1-matnr.
    IF sy-subrc = 0.
      gs_data_above-stprs    = gs_mbew_above-stprs. "S价
      gs_data_above-verpr    = gs_mbew_above-verpr. "V价
      gs_data_above-bklas    = gs_mbew_above-bklas. "评估类
    ENDIF.

*根据物料汇总差异金额
    LOOP AT gt_bseg_above INTO gs_bseg_above
     WHERE matnr = gs_bseg_above_1-matnr.

      IF gs_bseg_above-shkzg = 'H'.
        gs_bseg_above-dmbtr = gs_bseg_above-dmbtr * -1.
      ENDIF.

      gs_data_above-dmbtr = gs_data_above-dmbtr + gs_bseg_above-dmbtr.
    ENDLOOP.

    APPEND gs_data_above TO gt_data_above.
    CLEAR gs_data_above.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data_1.

  SELECT * FROM bkpf
  INTO CORRESPONDING FIELDS OF TABLE gt_bkpf_below
  WHERE bukrs = p_bukrs
  AND   gjahr = p_gjahr
  AND   monat = p_monat
  AND   blart = 'WL'
.

  CHECK gt_bkpf_below IS NOT INITIAL.

*取出交货单行项目号
  SELECT * FROM bseg
  INTO CORRESPONDING FIELDS OF TABLE gt_bseg_below_3
  FOR ALL ENTRIES IN gt_bkpf_below
  WHERE gjahr = gt_bkpf_below-gjahr
  AND   bukrs = gt_bkpf_below-bukrs
  AND   belnr = gt_bkpf_below-belnr.

  SELECT * FROM bseg
  INTO CORRESPONDING FIELDS OF TABLE gt_bseg_below
  FOR ALL ENTRIES IN gt_bkpf_below
  WHERE gjahr = gt_bkpf_below-gjahr
  AND   bukrs = gt_bkpf_below-bukrs
  AND   belnr = gt_bkpf_below-belnr
  AND   hkont BETWEEN '6404000000' AND '6404999999'.

*查询是否按单
  SELECT * FROM ckmlhd
  INTO CORRESPONDING FIELDS OF TABLE gt_ckmlhd
  FOR ALL ENTRIES IN gt_bseg_below
  WHERE bwkey = gt_bseg_below-werks
  AND   matnr = gt_bseg_below-matnr
  AND   vbeln = gt_bseg_below-vbel2.

  SORT gt_ckmlhd BY bwkey matnr vbeln posnr.

*查询差异
  SELECT * FROM mlcd
  INTO CORRESPONDING FIELDS OF TABLE gt_mlcd
  FOR ALL ENTRIES IN gt_bseg_below
  WHERE bdatj = gt_bseg_below-gjahr
  AND   poper = p_monat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data_1.

  FIELD-SYMBOLS <lw_data_below> TYPE ty_data_below.
  DATA l_matnr        TYPE matnr.
  DATA l_werks        TYPE werks_d.
  DATA l_cyft_all     TYPE p DECIMALS 2.
  DATA l_cy           TYPE p DECIMALS 2.
  DATA l_menge        TYPE bseg-menge.

***begain 按单物料***
  LOOP AT gt_bseg_below INTO gs_bseg_below.

*交货单行项目 (查询对应行项目前一个序号)
    READ TABLE gt_bseg_below_3 INTO gs_bseg_below_3
    WITH KEY gjahr = gs_bseg_below-gjahr
             belnr = gs_bseg_below-belnr
             buzei = ( gs_bseg_below-buzei - 1 ).
    IF sy-subrc = 0.
*      GS_DATA_BELOW-POSN3 = GS_BSEG_1-POSN2.
    ELSE.
      CLEAR gs_bseg_below_3.
    ENDIF.

*是否按单
    READ TABLE gt_ckmlhd INTO gs_ckmlhd
    WITH  KEY bwkey = gs_bseg_below-werks
              matnr = gs_bseg_below-matnr
              vbeln = gs_bseg_below-vbel2
              posnr = gs_bseg_below_3-posn2.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING gs_bseg_below TO   gs_bseg_below_1 .
      APPEND gs_bseg_below_1 TO gt_bseg_below_1.
      CLEAR gs_bseg_below_1.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.
***end***

***begain按库物料***
  LOOP AT gt_bseg_below INTO gs_bseg_below.
*交货单行项目 (查询对应行项目前一个序号)
    READ TABLE gt_bseg_below_3 INTO gs_bseg_below_3
    WITH KEY gjahr = gs_bseg_below-gjahr
             belnr = gs_bseg_below-belnr
             buzei = ( gs_bseg_below-buzei - 1 ).
    IF sy-subrc = 0.
*      GS_DATA_BELOW-POSN3 = GS_BSEG_1-POSN2.
    ELSE.
      CLEAR gs_bseg_below_3.
    ENDIF.

*是否按单
    READ TABLE gt_ckmlhd INTO gs_ckmlhd
    WITH  KEY bwkey = gs_bseg_below-werks
              matnr = gs_bseg_below-matnr
              vbeln = gs_bseg_below-vbel2
              posnr = gs_bseg_below_3-posn2.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      MOVE-CORRESPONDING gs_bseg_below TO   gs_bseg_below_2 .
      APPEND gs_bseg_below_2 TO gt_bseg_below_2.
      CLEAR gs_bseg_below_2.
    ENDIF.
  ENDLOOP.
***end按库物料***

  SORT gt_bseg_below_1 BY bukrs belnr gjahr buzei.

***begain处理按单物料差异
  LOOP AT gt_bseg_below_1 INTO gs_bseg_below_1.
    gs_data_below-werks = gs_bseg_below_1-werks.
    gs_data_below-matnr = gs_bseg_below_1-matnr.
    gs_data_below-vbel2 = gs_bseg_below_1-vbel2.
    gs_data_below-posn2 = gs_bseg_below_1-posn2.
    gs_data_below-meins = gs_bseg_below_1-meins.
    gs_data_below-menge = gs_bseg_below_1-menge.

*交货单行项目 (查询对应行项目前一个序号)
    READ TABLE gt_bseg_below_3 INTO gs_bseg_below_3
    WITH KEY gjahr = gs_bseg_below_1-gjahr
             belnr = gs_bseg_below_1-belnr
             buzei = ( gs_bseg_below_1-buzei - 1 ).
    IF sy-subrc = 0.
      gs_data_below-posn3 = gs_bseg_below_3-posn2.
    ELSE.
      CLEAR gs_bseg_below_3.
    ENDIF.

*取出成本估算号
    READ TABLE gt_ckmlhd INTO gs_ckmlhd
    WITH KEY bwkey = gs_bseg_below_1-werks
             matnr = gs_bseg_below_1-matnr
             vbeln = gs_bseg_below_1-vbel2
             posnr = gs_bseg_below_3-posn2.

*ADD BY HANDWY 2105-7-21 按单就汇总数量
    CLEAR l_menge.

    LOOP AT gt_bseg_below_1 INTO gs_bseg_below_1
     WHERE matnr = gs_data_below-matnr
     AND   vbel2 = gs_data_below-vbel2.

*判断交货单号是否相同
      READ TABLE gt_bseg_below_3 INTO gs_bseg_below_3
      WITH KEY gjahr = gs_bseg_below_1-gjahr
               belnr = gs_bseg_below_1-belnr
               buzei = ( gs_bseg_below_1-buzei - 1 ).
      IF gs_bseg_below_3-posn2 = gs_data_below-posn3.
        IF gs_bseg_below_1-shkzg = 'H'.
          gs_bseg_below_1-menge = gs_bseg_below_1-menge * -1.
        ENDIF.

        l_menge = l_menge + gs_bseg_below_1-menge.
      ENDIF.
    ENDLOOP.
*ENDADD

    gs_data_below-menge = l_menge.

    READ TABLE gt_mlcd INTO gs_mlcd
    WITH KEY bdatj = p_gjahr
             poper = p_monat
             categ = 'VN'
             ptyp  = 'V+'
             curtp = '10'
             lbkum = l_menge
             kalnr = gs_ckmlhd-kalnr.
    IF sy-subrc = 0.
      gs_data_below-cyft = gs_mlcd-estprd + gs_mlcd-mstprd.
    ENDIF.

    gs_data_below-sfad = 'X'.
    APPEND gs_data_below TO gt_data_below.
    CLEAR gs_data_below.
    CLEAR gs_data_above.
    CLEAR gs_bseg_below_1.
    CLEAR gs_ckmlhd.

  ENDLOOP.
***end***

*删除重复项目
  SORT gt_data_below BY werks matnr vbel2 posn2 posn3.
  DELETE ADJACENT DUPLICATES FROM gt_data_below COMPARING werks matnr vbel2 posn2 posn3.

  SORT gt_bseg_below_2 BY bukrs belnr gjahr buzei matnr menge.

***begain处理按库物料差异***
  LOOP AT gt_bseg_below_2 INTO gs_bseg_below_2.

    gs_data_below-werks = gs_bseg_below_2-werks.
    gs_data_below-matnr = gs_bseg_below_2-matnr.
    gs_data_below-vbel2 = gs_bseg_below_2-vbel2.
    gs_data_below-posn2 = gs_bseg_below_2-posn2.
    gs_data_below-meins = gs_bseg_below_2-meins.

*根据物料汇总差异数量
    LOOP AT gt_bseg_below_2 INTO gs_bseg_below
     WHERE matnr = gs_bseg_below_2-matnr.

      IF gs_bseg_below-shkzg = 'H'.
        gs_bseg_below-menge = gs_bseg_below-menge * -1.
      ENDIF.

      gs_data_below-menge_all = gs_data_below-menge_all + gs_bseg_below-menge.
    ENDLOOP.

*按销售订单和销售订单行汇总数量
    LOOP AT gt_bseg_below_2 INTO gs_bseg_below
    WHERE matnr = gs_bseg_below_2-matnr
    AND   vbel2 = gs_bseg_below_2-vbel2
    AND   posn2 = gs_bseg_below_2-posn2.

      IF gs_bseg_below-shkzg = 'H'.
        gs_bseg_below-menge = gs_bseg_below-menge * -1.
      ENDIF.

      gs_data_below-menge = gs_data_below-menge + gs_bseg_below-menge.
    ENDLOOP.
    CLEAR gs_data_above.
    READ TABLE gt_data_above INTO gs_data_above
    WITH KEY matnr = gs_bseg_below_2-matnr
             werks = gs_bseg_below_2-werks.
    IF sy-subrc = 0.
      "20171012处理按料分摊差异没有考虑已按单分摊出去的差异问题
      "按照物料分摊差异时需要去除按单分摊的差异，再进行剩余项分摊
      CLEAR: gs_data_below_d.
      LOOP AT gt_data_below INTO gs_data_below_d
                             WHERE matnr = gs_bseg_below_2-matnr
                             AND   werks = gs_bseg_below_2-werks
                             AND   sfad  = 'X'.
        gs_data_above-dmbtr = gs_data_above-dmbtr - gs_data_below_d-cyft.
      ENDLOOP.
    ENDIF.


*差异分摊
    IF gs_data_below-menge_all <> 0.
      gs_data_below-cyft     = gs_data_above-dmbtr / gs_data_below-menge_all * gs_data_below-menge.
    ELSE.
      gs_data_below-cyft = 0.
    ENDIF.

    IF l_matnr IS INITIAL AND l_werks IS INITIAL.
      l_matnr = gs_data_below-matnr.
      l_werks = gs_data_below-werks.
    ENDIF.

    IF l_matnr  <> gs_data_below-matnr
    AND l_werks <> gs_data_below-werks.
      gs_data_below-cyft_all = l_cyft_all.
      l_cyft_all = 0.
    ELSE.
      l_cyft_all = l_cyft_all + gs_data_below-cyft.
    ENDIF.

    APPEND gs_data_below TO gt_data_below.
    CLEAR gs_data_below.
    CLEAR gs_data_above.
    CLEAR gs_bseg_below_2.
  ENDLOOP.
***end***
*删除重复项目
  SORT gt_data_below BY werks matnr vbel2 posn2 posn3.
  DELETE ADJACENT DUPLICATES FROM gt_data_below COMPARING werks matnr vbel2 posn2 posn3.

  DATA l_cyft_all2  TYPE p DECIMALS 2.
*差异分摊到金额最大的一行 hanlj 2015.5.1 修改
  SORT gt_data_below BY werks matnr cyft sfad ASCENDING .
  LOOP AT gt_data_below ASSIGNING <lw_data_below>.
    AT NEW matnr.
      l_cyft_all2 = 0.
    ENDAT.
    l_cyft_all2 = l_cyft_all2 + <lw_data_below>-cyft .
    AT END OF matnr.
      CLEAR gs_data_above.
      READ TABLE gt_data_above INTO gs_data_above
      WITH KEY matnr = <lw_data_below>-matnr
               werks = <lw_data_below>-werks.
      <lw_data_below>-cyft = <lw_data_below>-cyft + gs_data_above-dmbtr - l_cyft_all2.
    ENDAT.
  ENDLOOP.

*删除差异是0的行项目
  DELETE gt_data_below WHERE cyft = 0.
*  SORT gt_data_below BY WERKS MATNR VBEL2.


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
  DATA l_subrc TYPE sy-subrc.

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
  PERFORM frm_message_initial.

*付款记账数据准备
  LOOP AT gt_data_above INTO gs_data_above.
    "检查过了帐的就跳过
    PERFORM frm_check_doc CHANGING l_subrc.
    IF l_subrc <> 0.
      CONTINUE.
    ENDIF.
    PERFORM frm_bapi_data_prep CHANGING gs_data_above.

*调用记账BAPI
    PERFORM frm_call_bapi CHANGING l_subrc
                                   gs_data_above.
*    IF l_subrc <> 0.
*      EXIT.
*    ENDIF.
  ENDLOOP.
  PERFORM frm_message_show.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_doc  CHANGING l_subrc TYPE sy-subrc.
  IF gs_data_above-belnr_1 IS NOT INITIAL
  AND gs_data_above-gjahr_1 IS NOT INITIAL.

    MESSAGE '选择年度和期间已经进行过账,请勿重复操作' TYPE 'S' DISPLAY LIKE 'E'.
    l_subrc = 4.
  ELSE.
    l_subrc = 0.
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
FORM frm_bapi_data_prep CHANGING gs_data_above TYPE ty_data_above.
*清空BAPI变量
  PERFORM frm_bapi_data_clear.

  DATA: l_katyp       TYPE cskb-katyp.
  DATA: l_itemno      TYPE i .
  DATA  l_waers       TYPE bseg-werks.
**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  wa_documentheader-doc_date     =  l_lastd.
*过账日期
  wa_documentheader-pstng_date   =  l_lastd.
*凭证类型
  wa_documentheader-doc_type     =  'ML'.
*公司代码
  wa_documentheader-comp_code    =  gs_data_above-bukrs.
*凭证抬头文本
  wa_documentheader-header_txt   =  '物料分类账差异追踪'.
*创建人员
  wa_documentheader-username     =  sy-uname.

**********************************************************************
* 凭证行
**********************************************************************
  CLEAR l_itemno.
  CLEAR l_waers.

  l_itemno = 01.

*ADD 根据公司代码取出货币  CHANGE BY HANDWY 2015-11-5
*查询公司代码货币
  SELECT SINGLE waers FROM t001
   INTO l_waers
   WHERE bukrs = wa_documentheader-comp_code .
*ENDADD

  IF gs_data_above-dmbtr > 0.
* 记账码 & 付款原因
    CLEAR wa_extension2.
    CLEAR wa_zaccdocuext.
    wa_zaccdocuext-posnr = l_itemno."行项目
    wa_zaccdocuext-bschl = '50'."记账码
*    WA_ZACCDOCUEXT-RSTGR = GS_ITEM-RSTGR."付款原因

  ELSE.
* 记账码 & 付款原因
    CLEAR wa_extension2.
    CLEAR wa_zaccdocuext.
    wa_zaccdocuext-posnr = l_itemno."行项目
    wa_zaccdocuext-bschl = '40'."记账码
*    WA_ZACCDOCUEXT-RSTGR = GS_ITEM-RSTGR."付款原因
  ENDIF.

*科目表(根据评估类确定科目)
  CLEAR wa_accountgl.
  wa_accountgl-itemno_acc = l_itemno.
  wa_accountgl-plant  = gs_data_above-werks.
  wa_accountgl-material = gs_data_above-matnr.
  CASE gs_data_above-bklas.
    WHEN 'Z010'.
      wa_accountgl-gl_account = '6404010101'.
    WHEN 'Z020'.
      wa_accountgl-gl_account = '6404020101'.
    WHEN 'Z030'.
      wa_accountgl-gl_account = '6404030101'.
    WHEN 'Z051'.
      wa_accountgl-gl_account = '6404050101'.
  ENDCASE.

  APPEND wa_accountgl TO it_accountgl.

*金额
  CLEAR wa_currencyamount.
  wa_currencyamount-itemno_acc = l_itemno."行项目

  IF wa_zaccdocuext-bschl = '50'.
    IF gs_data_above-dmbtr > 0.
      wa_currencyamount-amt_doccur = gs_data_above-dmbtr  * -1."金额
    ELSE.
      wa_currencyamount-amt_doccur = gs_data_above-dmbtr."金额
    ENDIF.
  ELSE.
    IF gs_data_above-dmbtr < 0.
      wa_currencyamount-amt_doccur = gs_data_above-dmbtr  * -1."金额
    ELSE.
      wa_currencyamount-amt_doccur = gs_data_above-dmbtr."金额
    ENDIF.
  ENDIF.

  wa_currencyamount-currency   = l_waers.

  APPEND wa_currencyamount TO it_currencyamount.

  wa_extension2-structure  = 'ZACCDOCUEXT'.
  wa_extension2-valuepart1 = wa_zaccdocuext.
  APPEND wa_extension2 TO it_extension2.


*销售订单行项目
  LOOP AT gt_data_below INTO gs_data_below
  WHERE matnr = gs_data_above-matnr.

    l_itemno = l_itemno + 1.

    IF gs_data_below-cyft > 0.
* 记账码 & 付款原因
      CLEAR wa_extension2.
      CLEAR wa_zaccdocuext.
      wa_zaccdocuext-posnr = l_itemno."行项目
      wa_zaccdocuext-bschl = '40'."记账码
*    WA_ZACCDOCUEXT-RSTGR = GS_ITEM-RSTGR."付款原因
    ELSE.
* 记账码 & 付款原因
      CLEAR wa_extension2.
      CLEAR wa_zaccdocuext.
      wa_zaccdocuext-posnr = l_itemno."行项目
      wa_zaccdocuext-bschl = '50'."记账码
*    WA_ZACCDOCUEXT-RSTGR = GS_ITEM-RSTGR."付款原因
    ENDIF.

*科目表(根据评估类确定科目)
    CLEAR wa_accountgl.
    wa_accountgl-itemno_acc = l_itemno.
    CASE gs_data_above-bklas.
      WHEN 'Z010'.
        wa_accountgl-gl_account = '6404010101'.
      WHEN 'Z020'.
        wa_accountgl-gl_account = '6404020101'.
      WHEN 'Z030'.
        wa_accountgl-gl_account = '6404030101'.
      WHEN 'Z051'.
        wa_accountgl-gl_account = '6404050101'.
    ENDCASE.

*数量（取绝对值）
    wa_accountgl-quantity   = abs( gs_data_below-menge ).
    wa_accountgl-base_uom   = gs_data_below-meins.

*分配字段 填销售订单号
    wa_accountgl-alloc_nmbr = gs_data_below-vbel2.

*文本
    CONCATENATE  gs_data_below-matnr '/物料分类帐差异追踪' INTO wa_accountgl-item_text.
*    WA_ACCOUNTGL-ITEM_TEXT  = '物料号/物料分类帐差异追踪'.

*销售订单号和行项目号
    wa_accountgl-sales_ord  = gs_data_below-vbel2.
    wa_accountgl-s_ord_item = gs_data_below-posn2.

*工厂及物料号
    wa_accountgl-plant      = gs_data_below-werks.
    wa_accountgl-material   = gs_data_below-matnr.
    APPEND wa_accountgl TO it_accountgl.

*金额
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc = l_itemno."行项目

    IF wa_zaccdocuext-bschl = '50'.
      IF gs_data_below-cyft > 0.
        wa_currencyamount-amt_doccur = gs_data_below-cyft  * -1."金额
      ELSE.
        wa_currencyamount-amt_doccur = gs_data_below-cyft."金额
      ENDIF.

    ELSE.
      IF gs_data_below-cyft < 0.
        wa_currencyamount-amt_doccur = gs_data_below-cyft  * -1."金额
      ELSE.
        wa_currencyamount-amt_doccur = gs_data_below-cyft."金额
      ENDIF.
    ENDIF.

    wa_currencyamount-currency   = l_waers.

    APPEND wa_currencyamount TO it_currencyamount.

    wa_extension2-structure  = 'ZACCDOCUEXT'.
    wa_extension2-valuepart1 = wa_zaccdocuext.
    APPEND wa_extension2 TO it_extension2.
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
  REFRESH: it_accountgl,  it_currencyamount, it_criteria, it_valuefield, it_extension2, it_return.
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
FORM frm_call_bapi CHANGING l_subrc TYPE sy-subrc
                            gs_data_above TYPE ty_data_above.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = wa_documentheader
*     CUSTOMERCPD    =
*     CONTRACTHEADER =
    IMPORTING
      obj_type       = wa_obj-obj_type
      obj_key        = wa_obj-obj_key
      obj_sys        = wa_obj-obj_sys
    TABLES
      accountgl      = it_accountgl
*     ACCOUNTRECEIVABLE = IT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE = IT_ACCOUNTPAYABLE
*     ACCOUNTTAX     =
      currencyamount = it_currencyamount
      criteria       = it_criteria
      valuefield     = it_valuefield
*     EXTENSION1     =
      return         = it_return
*     PAYMENTCARD    =
*     CONTRACTITEM   =
      extension2     = it_extension2
*     REALESTATE     =
*     ACCOUNTWT      =
    .

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    l_subrc = 4.
    PERFORM frm_message_display.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

* 回写生成的会计凭证号与会计凭证年度
    LOOP AT gt_bseg_above INTO gs_bseg_above
    WHERE matnr = gs_data_above-matnr
    AND   werks = gs_data_above-werks.

      gs_data_above-belnr_1 =  wa_obj-obj_key(10).
      gs_data_above-gjahr_1 =  wa_obj-obj_key+14(4).

      MODIFY gt_data_above FROM gs_data_above.
      CLEAR gs_bseg_above.
    ENDLOOP.
*
*更新自建表状态
    REFRESH gt_zco004.
    CLEAR gs_zco004.

    gs_zco004-bukrs = p_bukrs.
    gs_zco004-gjahr = wa_obj-obj_key+14(4).
    gs_zco004-monat = p_monat.
    gs_zco004-matnr = gs_data_above-matnr.
    gs_zco004-belnr = wa_obj-obj_key(10).
    APPEND gs_zco004 TO gt_zco004.
    MODIFY zco004 FROM TABLE gt_zco004.

  ENDIF.
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
*&      Form  FRM_REVERSAL_ACCDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_reversal_accdoc .

ENDFORM.
