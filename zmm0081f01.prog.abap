*&---------------------------------------------------------------------*
*&  包含                ZMM0081F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*"通用user_commond
*&---------------------------------------------------------------------*
*&      Form  frm_user_commond
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM frm_user_commond.
  CASE sy-ucomm.
    WHEN '&CANCLE'.
      LEAVE TO SCREEN 0.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      IF g_dock_1 IS NOT INITIAL OR g_dock_2 IS NOT INITIAL.
        IF g_dock_1 IS NOT INITIAL.
          CALL METHOD g_dock_1->free
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2.
          IF sy-subrc <> 0.
            MESSAGE a000.
          ENDIF.
        ENDIF.

        IF g_dock_2 IS NOT INITIAL.
          CALL METHOD g_dock_2->free
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2.
          IF sy-subrc <> 0.
            MESSAGE a000.
          ENDIF.
        ENDIF.
        CALL METHOD cl_gui_cfw=>flush
          EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2.
        IF sy-subrc <> 0.
          MESSAGE a000.
        ENDIF.
        CLEAR g_dock_1.
        CLEAR g_alv_1.
        CLEAR g_dock_2.
        CLEAR g_alv_2.
      ENDIF.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDFORM. "frm_user_commond



*&---------------------------------------------------------------------*
*&      Form  FRM_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM frm_init .




ENDFORM. " FRM_INIT

*FORM frm_auth_check.
*DATA:lt_tvko TYPE tvko OCCURS 0 WITH HEADER LINE.
*DATA:lt_T024E TYPE T024E OCCURS 0 WITH HEADER LINE.
* CASE mytab-activetab.
*    WHEN  'PUSH1'.
*  SELECT vkorg FROM TVKO INTO CORRESPONDING FIELDS OF TABLE lt_tvko WHERE vkorg in s_vkorg.
*  LOOP AT lt_tvko WHERE vkorg IN s_vkorg.
*    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
*             ID 'VKORG' FIELD lt_tvko-vkorg .
*    IF sy-subrc <> 0.
*      MESSAGE e430(velo) WITH lt_tvko-vkorg.
*    ENDIF.
*  ENDLOOP.
*    WHEN 'PUSH2' .
*  SELECT ekorg FROM T024E INTO CORRESPONDING FIELDS OF TABLE lt_t024e WHERE ekorg in s_vkorg.
*  LOOP AT lt_T024E WHERE EKORG IN s_EKORG.
*    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
*             ID 'VKORG' FIELD lt_T024E-EKORG .
*    IF sy-subrc <> 0.
*      MESSAGE e691(MW) WITH lt_T024E-EKORG.
*    ENDIF.
*  ENDLOOP.
* ENDCASE.

*ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  frm_create_container
*&---------------------------------------------------------------------*
*       创建ALV容器,并将类添加到容器
*----------------------------------------------------------------------*

FORM frm_create_container USING p_ratio p_side
      CHANGING pcl_container TYPE REF TO cl_gui_docking_container
                        pcl_alv TYPE REF TO cl_gui_alv_grid.
  DATA l_side TYPE i.

  CASE p_side.
    WHEN 'DOCK_AT_LEFT'.
      l_side = cl_gui_docking_container=>dock_at_left.
    WHEN 'DOCK_AT_RIGHT'.
      l_side = cl_gui_docking_container=>dock_at_right.
    WHEN 'DOCK_AT_TOP'.
      l_side = cl_gui_docking_container=>dock_at_top.
    WHEN 'DOCK_AT_BOTTOM'.
      l_side = cl_gui_docking_container=>dock_at_bottom.
    WHEN OTHERS.
  ENDCASE.

  IF pcl_container IS INITIAL.
    CREATE OBJECT pcl_container
      EXPORTING
        repid = sy-repid
        dynnr = sy-dynnr
        ratio = p_ratio
        side  = l_side.

    CREATE OBJECT pcl_alv
      EXPORTING
        i_parent = pcl_container.
  ENDIF.

ENDFORM. " frm_create_container



*&---------------------------------------------------------------------*
*&      Form  frm_create_object
*&---------------------------------------------------------------------*
*       Creating Docking Container and grid
*----------------------------------------------------------------------*

FORM frm_create_alv9000.
  PERFORM frm_create_container USING 60 'DOCK_AT_TOP' CHANGING g_dock_1 g_alv_1.
  PERFORM frm_create_container USING 40 'DOCK_AT_TOP' CHANGING g_dock_2 g_alv_2.

  g_init9000 = 'X'.
ENDFORM. " frm_create_object



*&---------------------------------------------------------------------*
*&      Form  frm_upload_event1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM frm_upload_event1.
  DATA:
     l_event_receiver TYPE REF TO gcl_event_handler1.
  CREATE OBJECT l_event_receiver."创造事件对象

*************************************注册时间对象

  SET HANDLER l_event_receiver->handle_user_command FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_toolbar FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_alv_drag FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_alv_drop FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_dropcomplete FOR g_alv_1.

*  SET HANDLER g_event_receiver->handle_onf4 FOR g_alv_1.
*  set handler g_event_receiver->handle_menu_button for gg_alv_9000 .
  SET HANDLER l_event_receiver->handle_double_click FOR g_alv_1 .
*  set handler g_event_receiver->handle_hotspot_click for gg_alv_9000 .
** SET HANDLER g_event_receiver->handle_button_click FOR gg_alv_9000 .

  SET HANDLER l_event_receiver->handle_before_user_command FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_user_command FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_data_changed FOR g_alv_1.
  SET HANDLER l_event_receiver->handle_data_changed_finished FOR g_alv_1.
  IF sy-batch IS INITIAL.
    CALL METHOD g_alv_1->register_edit_event "注册事件
      EXPORTING
        "i_event_id = cl_gui_alv_grid=>mc_evt_enter. "回车时触发
        i_event_id = cl_gui_alv_grid=>mc_evt_modified. "单元格更改触发

*      call method o_grid->set_toolbar_interactive.
*      call method cl_gui_control=>set_focus
*        exporting
*          control = o_grid.

  ENDIF.
ENDFORM. "frm_upload_event1



*&---------------------------------------------------------------------*
*&      Form  frm_upload_event2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM frm_upload_event2.
  DATA:
     l_event_receiver TYPE REF TO gcl_event_handler2.
  CREATE OBJECT l_event_receiver."创造事件对象

*************************************注册时间对象
  SET HANDLER l_event_receiver->handle_user_command FOR g_alv_2.
  SET HANDLER l_event_receiver->handle_toolbar FOR g_alv_2.

  SET HANDLER l_event_receiver->handle_alv_drag FOR g_alv_2.
  SET HANDLER l_event_receiver->handle_alv_drop FOR g_alv_2.
  SET HANDLER l_event_receiver->handle_dropcomplete FOR g_alv_2.

*  SET HANDLER g_event_receiver->handle_onf4 FOR g_alv_2.
*  set handler g_event_receiver->handle_menu_button for gg_alv_9000 .
*  set handler g_event_receiver->handle_double_click for gg_alv_9000 .
*  set handler g_event_receiver->handle_hotspot_click for gg_alv_9000 .
** SET HANDLER g_event_receiver->handle_button_click FOR gg_alv_9000 .
*  set handler l_event_receiver->handle_before_user_command for g_alv_2.
*  SET HANDLER l_event_receiver->handle_user_command FOR g_alv_2.
*  SET HANDLER l_event_receiver->handle_data_changed FOR g_alv_2.
*  SET HANDLER l_event_receiver->handle_data_changed_finished FOR g_alv_2.

  IF sy-batch IS INITIAL.
    CALL METHOD g_alv_2->register_edit_event "注册事件
      EXPORTING
        "i_event_id = cl_gui_alv_grid=>mc_evt_enter. "回车时触发
        i_event_id = cl_gui_alv_grid=>mc_evt_modified. "单元格更改触发


*      call method o_grid->set_toolbar_interactive.
*      call method cl_gui_control=>set_focus
*        exporting
*          control = o_grid.

  ENDIF.
ENDFORM. "frm_upload_event1

*&---------------------------------------------------------------------*
*&      Form  BUILD_AND_ASSIGN_HANDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM build_and_assign_handle.

  DATA: effect     TYPE i,
        handle_alv TYPE i.


* §2. Define a behaviour for drag and drop on alv objects
*     and get its handle.

* define a drag & Drop behaviour for the whole grid

  CREATE OBJECT g_behaviour_alv.
  effect = cl_dragdrop=>move .
  CALL METHOD g_behaviour_alv->add
    EXPORTING
      flavor     = 'Line'                                   "#EC NOTEXT
      dragsrc    = 'X'
      droptarget = 'X'
      effect     = effect.

  CALL METHOD g_behaviour_alv->get_handle
    IMPORTING
      handle = handle_alv.

* provide handle to alv control using the layout-structure
* In this example all rows obtain the same drag and drop behaviour:
  gw_layout_1-s_dragdrop-row_ddid = handle_alv.
  gw_layout_2-s_dragdrop-row_ddid = handle_alv.

ENDFORM. " build_nodes_and_handles


*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM frm_display_alv9000 .
  DATA l_title TYPE lvc_title.
  PERFORM frm_fill_fieldcat.
  l_title = '双击条目进行调拨单数量分配。'.
  PERFORM frm_pre_layout USING l_title space space space '' CHANGING gw_layout_1.
  l_title = '拖拽至此处查看及修改对应调拨单'.
  PERFORM frm_pre_layout USING l_title space space space 'FIELD_STYLE' CHANGING gw_layout_2.
  PERFORM frm_exclude_1.
  PERFORM frm_exclude_2.
  PERFORM frm_variant_1.
  PERFORM frm_variant_2.
  PERFORM build_and_assign_handle.
  PERFORM frm_upload_event1.
  PERFORM frm_upload_event2.
  PERFORM frm_set_tab_display TABLES it_data1[]
                                     gt_fieldcat_1[]
                             USING   g_alv_1
                                     gw_variant_1
                                     gw_layout_1
                                     gw_exclude_1.

  PERFORM frm_set_tab_display TABLES  it_data2[]
                                      gt_fieldcat_2[]
                               USING  g_alv_2
                                      gw_variant_2
                                      gw_layout_2
                                      gw_exclude_2.

ENDFORM. " FRM_DISPLAY_ALV9000



*&---------------------------------------------------------------------*
*&      Form  FRM_SET_TAB_DISPLAY
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*

FORM frm_set_tab_display TABLES
                                ft_data TYPE STANDARD TABLE
                                ft_fieldcat
                          USING pcl_alv TYPE REF TO cl_gui_alv_grid
                                fs_variant STRUCTURE disvariant
                                fs_layout TYPE lvc_s_layo
                                fs_exclude TYPE ui_functions
                               .

  CALL METHOD pcl_alv->set_table_for_first_display
    EXPORTING
      i_structure_name              = 'IT_DATA1'
      is_variant                    = fs_variant
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = fs_layout
      it_toolbar_excluding          = fs_exclude
    CHANGING
      it_outtab                     = ft_data[]
      it_fieldcatalog               = ft_fieldcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " FRM_SET_TAB_DISPLAY


*----------------------------------------------------------------------*
*  设置ALV输出表单布局格式、属性。
*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_LAYOUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TITLE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      <--P_GS_LAYOUT_1  text
*----------------------------------------------------------------------*

FORM frm_pre_layout USING p_title p_tool p_no_head p_coltab p_style
                      CHANGING ps_layout TYPE lvc_s_layo.

  ps_layout-zebra = 'X'.
  ps_layout-no_toolbar = p_tool.
  ps_layout-sel_mode = 'A'.
  ps_layout-smalltitle = 'X'.
  ps_layout-grid_title = p_title.
  ps_layout-no_headers = p_no_head.
  " ps_layout-NO_TOOLBAR = 'X'.
  ps_layout-ctab_fname = p_coltab.
  ps_layout-stylefname = p_style.

  ps_layout-cwidth_opt = 'X'.


ENDFORM. " FRM_PRE_LAYOUT1


*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       排除按钮alv1按钮
*----------------------------------------------------------------------*

FORM frm_exclude_1.
  DATA ls_exclude TYPE ui_func.

*  CLEAR gw_exclude[].
*排除打印按钮

  ls_exclude = cl_gui_alv_grid=>mc_fc_print .
  APPEND ls_exclude TO gw_exclude_1.

*排除添加、删除、插入、复制按钮
*  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh .
*  APPEND ls_exclude TO gw_exclude_1.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row .
  APPEND ls_exclude TO gw_exclude_1.

*  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row .
*  APPEND ls_exclude TO gw_exclude_1.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row .
  APPEND ls_exclude TO gw_exclude_1.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo .
  APPEND ls_exclude TO gw_exclude_1.

*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort.
*  APPEND ls_exclude TO gw_exclude_1.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
*  APPEND ls_exclude TO gw_exclude_1.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
*  APPEND ls_exclude TO gw_exclude_1.
ENDFORM. " FRM_EXCLUDE



*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       排除按钮alv2按钮
*----------------------------------------------------------------------*

FORM frm_exclude_2.
  DATA ls_exclude TYPE ui_func.
*  CLEAR gw_exclude[].
*排除打印按钮
  ls_exclude = cl_gui_alv_grid=>mc_fc_print .
  APPEND ls_exclude TO gw_exclude_2.
*排除添加、删除、插入、复制按钮
*  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh .
*  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row .
  APPEND ls_exclude TO gw_exclude_2.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo .
  APPEND ls_exclude TO gw_exclude_2.
ENDFORM. " FRM_EXCLUDE


*&---------------------------------------------------------------------*
*&      Form  FRM_VARIANT_1
*&---------------------------------------------------------------------*
*       初始化变式
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

FORM frm_variant_1.

*  存入表LTDXT
  gw_variant_1-report  = sy-repid."ABAP 程序名称
  gw_variant_1-handle  = '0001'."从相同程序重复调用管理标识
  gw_variant_1-log_group  = ''."逻辑组名
  gw_variant_1-username  = sy-uname."特定用户存储的用户名称
  gw_variant_1-variant  = ''."格式
  gw_variant_1-text  = ''."布局的描述
  gw_variant_1-dependvars  = '' ."相关变式条目向量
ENDFORM. " FRM_VARIANT



*&---------------------------------------------------------------------*
*&      Form  FRM_VARIANT_2
*&---------------------------------------------------------------------*
*       初始化变式
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

FORM frm_variant_2.

*  存入表LTDXT
  gw_variant_2-report  = sy-repid."ABAP 程序名称
  gw_variant_2-handle  = '0001'."从相同程序重复调用管理标识
  gw_variant_2-log_group  = ''."逻辑组名
  gw_variant_2-username  = sy-uname."特定用户存储的用户名称
  gw_variant_2-variant  = ''."格式
  gw_variant_2-text  = ''."布局的描述
  gw_variant_2-dependvars  = '' ."相关变式条目向量
ENDFORM. " FRM_VARIANT

*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM frm_fill_fieldcat.
  DATA l_col_pos TYPE i VALUE 1.
  CLEAR gt_fieldcat_1[].
  CLEAR gt_fieldcat_2[].
  PERFORM frm_fill_fcat
             TABLES gt_fieldcat_1[]
             USING:
               l_col_pos '' 'LOCKED'  '锁定标示'  '' '',
               l_col_pos '' 'ZBOX'  '调拨单建立标示'  '' '',
               l_col_pos '' 'EDITFLAG'  '操作行'  '' '',
               l_col_pos '' 'VBELN'  '销售订单号'  '' '',
               l_col_pos '' 'POSNR'  '行号'  '' '',
               l_col_pos '' 'EBELN'  '采购单号'  '' '',
               l_col_pos '' 'EBELP'  '行号'  '' '',
               l_col_pos '' 'SGTXT'  '项目描述'  '' '',
               l_col_pos 'MARA' 'MATNR'  '物料号'  '' '',
               l_col_pos '' 'MAKTX'  '物料描述'  '' '',
               l_col_pos '' 'KDMAT'  '客户物料编号'  '' '',
               l_col_pos '' 'XCHPF'  '批次标示'  '' '',
               l_col_pos '' 'SPCID'  '特殊库存标示'  '' '',
               l_col_pos '' 'KWMENG' '订单数量'  '' '',
               l_col_pos '' 'QTYYDB'  '已调拨数量'  '' '',
               l_col_pos '' 'KDBNUM'  '可调拨数量'  '' '',
               l_col_pos '' 'MEINS'  '基本单位'  '' '',
               l_col_pos '' 'WERKS'  '工厂'  '' ''.

  l_col_pos = 1.
  PERFORM frm_fill_fcat
             TABLES gt_fieldcat_2[]
             USING:
               l_col_pos '' 'DBDH'  '调拨单号'  '' '',
               l_col_pos '' 'VBELN'  '销售订单号'  '' '',
               l_col_pos '' 'POSNR'  '行号'  '' '',
               l_col_pos '' 'EBELN'  '采购单号'  '' '',
               l_col_pos '' 'EBELP'  '行号'  '' '',
               l_col_pos 'MARA' 'MATNR'  '物料号'  '' '',
*               l_col_pos '' 'MAKTX'  '物料描述'  '' '',
               l_col_pos '' 'QTYBDB'  '本次调拨数量'  '' '',
               l_col_pos '' 'MEINS'  '基本单位'  '' '',
               l_col_pos '' 'WERKS'  '工厂'  '' '',
               l_col_pos '' 'SLOCFR'  '发出库位'  '' '',
               l_col_pos '' 'STOCTO'  '目标库位'  '' '',
               l_col_pos '' 'XCHPF'  '批次标示'  '' '',
               l_col_pos '' 'CHARG'  '批次'  '' '',
               l_col_pos '' 'SPCID'  '特殊库存标示'  '' '',
               l_col_pos '' 'VGBEL'  '按单订单'  '' '',
               l_col_pos '' 'VGPOS'  '行号'  '' '',
               l_col_pos '' 'ZDY'  '制单员'  '' '',
               l_col_pos '' 'ZDDT'  '制单日期'  '' '',
               l_col_pos '' 'MBLNR'  '物料凭证号'  '' '',
               l_col_pos '' 'MBLNR2'  '冲销物料凭证号'  '' '',
               l_col_pos '' 'REVERSAL'  '冲销标示'  '' '',
               l_col_pos '' 'POSTER'  '过账员'  '' '',
               l_col_pos '' 'POSTDT'  '过账日期'  '' ''.

ENDFORM. " FRM_FILL_FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0261   text
*      -->P_0262   text
*      -->P_0263   text
*      -->P_0264   text
*      -->P_0265   text
*----------------------------------------------------------------------*

FORM frm_fill_fcat TABLES ft_fildcat TYPE lvc_t_fcat
                          USING   p_col_pos
                             VALUE(p_ref_table)
                             VALUE(p_fieldname)
                             VALUE(p_coltext)
                             VALUE(p_no_zero)
                             VALUE(p_edit) .

  DATA lw_fieldcat_1 TYPE LINE OF lvc_t_fcat.

  lw_fieldcat_1-col_pos = p_col_pos.
  lw_fieldcat_1-fieldname = p_fieldname.
  lw_fieldcat_1-coltext = p_coltext.
  lw_fieldcat_1-no_zero = p_no_zero.
  lw_fieldcat_1-edit = p_edit.
  lw_fieldcat_1-ref_table = p_ref_table.
  lw_fieldcat_1-ref_field = p_fieldname.
  lw_fieldcat_1-no_out = ''.
  lw_fieldcat_1-decimals = 3.
*  lw_fieldcat_1-outputlen = '11'.
  IF lw_fieldcat_1-fieldname = 'LOCKED'.
    lw_fieldcat_1-icon = 'X'.
  ENDIF.
  APPEND lw_fieldcat_1 TO ft_fildcat.
  p_col_pos = p_col_pos + 1.
ENDFORM. " FRM_FILL_FCAT



*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM frm_get_data .
  DATA: ls_stylerow TYPE lvc_s_styl,
        lt_style    TYPE lvc_t_styl.
  DATA: lt_marc TYPE marc OCCURS 0 WITH HEADER LINE.
  DATA: lt_ekkn TYPE ekkn OCCURS 0 WITH HEADER LINE.
  DATA: lt_mvke TYPE mvke OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbap TYPE vbap OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS <lw_data1> LIKE LINE OF it_data1.

  CASE mytab-activetab.
    WHEN  'PUSH1'.
      SELECT  vbak~vbeln
              vbap~posnr
              vbap~matnr
              makt~maktx
              vbap~werks
              vbap~kdmat
              vbap~kwmeng
              vbap~meins
              vbap~bedae
        FROM vbap
        JOIN vbak
        ON vbap~vbeln = vbak~vbeln
        JOIN mara
        ON vbap~matnr = mara~matnr
        LEFT JOIN makt
        ON vbap~matnr = makt~matnr
        AND makt~spras = sy-langu

        INTO CORRESPONDING FIELDS OF TABLE it_data1
        WHERE vbak~vbeln = p_vbeln
*        AND   vbak~auart IN s_auart
*        AND   vbak~audat IN s_audat
*        AND   vbak~vkorg IN s_vkorg
        AND   mara~mtart <> 'ZXNJ'   "不取虚拟件
        AND   vbak~vkorg <> '1000'   "不取北京销售订单
        .
      IF it_data1[] IS NOT INITIAL.
        SELECT matnr werks xchpf FROM marc
          INTO CORRESPONDING FIELDS OF TABLE lt_marc
          FOR ALL ENTRIES IN it_data1
          WHERE matnr = it_data1-matnr
          AND   werks = it_data1-werks
          .
        "特殊库存逻辑取值修改
*        SELECT matnr
*               vkorg
*               mtpos
*          FROM mvke
*          INTO CORRESPONDING FIELDS OF TABLE lt_mvke
*          FOR ALL ENTRIES IN it_data1
*          WHERE matnr = it_data1-matnr
*          AND  vkorg = it_data1-werks
*          AND ( mtpos = 'Z003' or mtpos = 'Z005' )
*          .
        SELECT *
*               VBELN
*               EBELN
*               POSNR
*               EBELP
*               MATNR
*               WERKS
          FROM zmm002i
          INTO CORRESPONDING FIELDS OF TABLE it_zmm002i
          FOR ALL ENTRIES IN it_data1
          WHERE vbeln = it_data1-vbeln
          AND   ebeln = it_data1-ebeln
          AND   posnr = it_data1-posnr
          AND   ebelp = it_data1-ebelp
          AND   matnr = it_data1-matnr
          AND   werks = it_data1-werks
          AND   zdelflag = ''.
        LOOP AT it_data1 ASSIGNING <lw_data1>.
          CLEAR it_zmm002i.
          READ TABLE it_zmm002i WITH KEY vbeln = <lw_data1>-vbeln
                                         ebeln = <lw_data1>-ebeln
                                         posnr = <lw_data1>-posnr
                                         ebelp = <lw_data1>-ebelp
                                         matnr = <lw_data1>-matnr
                                         werks = <lw_data1>-werks
                                         .
          IF sy-subrc = 0.
            <lw_data1>-zbox = 'X'.
          ENDIF.
          LOOP AT it_zmm002i WHERE vbeln = <lw_data1>-vbeln
                               AND ebeln = <lw_data1>-ebeln
                               AND posnr = <lw_data1>-posnr
                               AND ebelp = <lw_data1>-ebelp
                               AND matnr = <lw_data1>-matnr
                               AND werks = <lw_data1>-werks
                               AND mblnr <> ''
                               AND reversal = ''
                               AND tzbs = ''  .  "ADD IT02 20160222.
            <lw_data1>-qtyydb = <lw_data1>-qtyydb + it_zmm002i-qtybdb.
          ENDLOOP.
          CLEAR lt_marc.
          READ TABLE lt_marc WITH KEY matnr = <lw_data1>-matnr
                                      werks = <lw_data1>-werks
                                      .
          IF sy-subrc = 0.
            <lw_data1>-xchpf = lt_marc-xchpf.
          ENDIF.

          <lw_data1>-kdbnum = <lw_data1>-kwmeng - <lw_data1>-qtyydb.


          "特殊库存取值逻辑修改
*          CLEAR lt_mvke.
*          READ TABLE lt_mvke WITH KEY matnr = <lw_data1>-matnr
*                                      vkorg = <lw_data1>-werks
*                                      .
*          IF sy-subrc = 0.
*            <lw_data1>-spcid = 'E'.
*          ENDIF.
          IF <lw_data1>-bedae = 'KEV'. "如果是按销售订单调拨，根据VBAP-BEDAE判断，如果为KEV，则为E；如果为KSV，则为空；
            <lw_data1>-spcid = 'E'.
          ELSE.
            <lw_data1>-spcid = ''.
          ENDIF.
          PERFORM frm_read_text USING <lw_data1>-vbeln sy-langu 'Z001' 'VBBK' CHANGING  <lw_data1>-sgtxt.
        ENDLOOP.
      ENDIF.
      IF rad1 = 'X'.
        DELETE it_data1 WHERE kdbnum = 0.
      ENDIF.
      IF rad2 = 'X'.
        DELETE it_data1 WHERE kdbnum > 0.
      ENDIF.


    WHEN  'PUSH2'.
      SELECT  ekko~ebeln
              ekpo~ebelp
              ekpo~matnr
              makt~maktx
              ekkn~vbeln
              ekkn~vbelp AS posnr
*              ekpo~werks
              ekpv~vstel AS werks
              ekpo~menge AS kwmeng
              ekpo~meins
              ekpo~umsok AS spcid "按采购订单调拨  特殊库存标示
        FROM ekpo
        JOIN ekko
        ON ekpo~ebeln = ekko~ebeln
        JOIN mara
        ON ekpo~matnr = mara~matnr
        JOIN ekpv
        ON ekpo~ebeln = ekpv~ebeln
        AND ekpo~ebelp = ekpv~ebelp
        LEFT JOIN ekkn
        ON  ekpo~ebeln = ekkn~ebeln
        AND ekpo~ebelp = ekkn~ebelp
        LEFT JOIN makt
        ON ekpo~matnr = makt~matnr
        AND makt~spras = sy-langu
        INTO CORRESPONDING FIELDS OF TABLE it_data1
        WHERE ekko~ebeln = p_ebeln
*        AND   ekko~bsart IN s_bsart
*        AND   ekko~bedat IN s_bedat
*        AND   ekko~ekorg IN s_ekorg
*        AND   ekko~bsart = 'Z06'
    "    AND   ekpo~loekz = ''
        AND   mara~mtart <> 'ZXNJ'
        .
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ekpo
        FROM ekpo
        WHERE ebeln =  p_ebeln AND loekz = 'L' .
      IF it_data1[] IS NOT INITIAL.
        SELECT vbeln posnr kdmat
          FROM vbap
          INTO CORRESPONDING FIELDS OF TABLE lt_vbap
          FOR ALL ENTRIES IN it_data1
          WHERE vbeln = it_data1-vbeln
          AND   posnr = it_data1-posnr
          .

        SELECT matnr werks xchpf FROM marc
          INTO CORRESPONDING FIELDS OF TABLE lt_marc
          FOR ALL ENTRIES IN it_data1
          WHERE matnr = it_data1-matnr
          AND   werks = it_data1-werks .

*        SELECT ebeln
*               ebelp
*               vbeln
*               vbelp
*          FROM ekkn
*          INTO CORRESPONDING FIELDS OF TABLE lt_ekkn
*          FOR ALL ENTRIES IN it_data1
*          WHERE ebeln = it_data1-ebeln
*          AND   ebelp = it_data1-ebelp .
*
*          CLEAR lt_ekkn.
*          READ TABLE lt_ekkn WITH KEY ebeln = <lw_data1>-ebeln
*                                      ebelp = <lw_data1>-ebelp
*                                      .
*          IF sy-subrc = 0.
*            <lw_data1>-vbeln = lt_ekkn-vbeln.
*            <lw_data1>-posnr = lt_ekkn-vbelp.
*          ENDIF.

        "按采购订单调拨 特殊库存标示逻辑更改
*        SELECT matnr
*               vkorg
*               mtpos
*          FROM mvke
*          INTO CORRESPONDING FIELDS OF TABLE lt_mvke
*          FOR ALL ENTRIES IN it_data1
*          WHERE matnr = it_data1-matnr
*          AND  vkorg = it_data1-werks
*          AND  ( mtpos = 'Z003' or mtpos = 'Z005' )
*          .
        SELECT *
*               VBELN
*               EBELN
*               POSNR
*               EBELP
*               MATNR
*               WERKS
          FROM zmm002i
          INTO CORRESPONDING FIELDS OF TABLE it_zmm002i
          FOR ALL ENTRIES IN it_data1
          WHERE vbeln = it_data1-vbeln
          AND   ebeln = it_data1-ebeln
          AND   posnr = it_data1-posnr
          AND   ebelp = it_data1-ebelp
          AND   matnr = it_data1-matnr
          AND   werks = it_data1-werks
          AND   zdelflag = ''.

        LOOP AT it_data1 ASSIGNING <lw_data1>.
          READ TABLE it_ekpo WITH KEY ebeln = <lw_data1>-ebeln  ebelp = <lw_data1>-ebelp .
          IF sy-subrc = 0.
            READ TABLE it_zmm002i WITH KEY vbeln = <lw_data1>-vbeln ebeln = <lw_data1>-ebeln  ebelp = <lw_data1>-ebelp  .
            IF sy-subrc <> 0 .
              DELETE it_data1 WHERE  vbeln = <lw_data1>-vbeln AND ebeln = <lw_data1>-ebeln AND  ebelp = <lw_data1>-ebelp .
              CONTINUE .
            ENDIF.
          ENDIF.


          CLEAR lt_vbap.
          READ TABLE lt_vbap WITH KEY vbeln = <lw_data1>-vbeln
                                      posnr = <lw_data1>-posnr
                                         .
          IF sy-subrc = 0.
            <lw_data1>-kdmat = lt_vbap-kdmat.
          ENDIF.

          CLEAR it_zmm002i.
          READ TABLE it_zmm002i WITH KEY vbeln = <lw_data1>-vbeln
                                         ebeln = <lw_data1>-ebeln
                                         posnr = <lw_data1>-posnr
                                         ebelp = <lw_data1>-ebelp
                                         matnr = <lw_data1>-matnr
                                         werks = <lw_data1>-werks
                                         .
          IF sy-subrc = 0.
            <lw_data1>-zbox = 'X'.
          ENDIF.
          LOOP AT it_zmm002i WHERE vbeln = <lw_data1>-vbeln
                               AND ebeln = <lw_data1>-ebeln
                               AND posnr = <lw_data1>-posnr
                               AND ebelp = <lw_data1>-ebelp
                               AND matnr = <lw_data1>-matnr
                               AND werks = <lw_data1>-werks
                               AND mblnr <> ''
                               AND reversal = ''
                               AND tzbs = ''  "ADD IT02 20160222
                               .
            <lw_data1>-qtyydb = <lw_data1>-qtyydb + it_zmm002i-qtybdb.
          ENDLOOP.
          CLEAR lt_marc.
          READ TABLE lt_marc WITH KEY matnr = <lw_data1>-matnr
                                      werks = <lw_data1>-werks
                                      .
          IF sy-subrc = 0.
            <lw_data1>-xchpf = lt_marc-xchpf.
          ENDIF.

          <lw_data1>-kdbnum = <lw_data1>-kwmeng - <lw_data1>-qtyydb.
          "特殊库存标示更改：按公司间采购订单调拨，直接取EKPO-UMSOK的值
*          CLEAR lt_mvke.
*          READ TABLE lt_mvke WITH KEY matnr = <lw_data1>-matnr
*                                      vkorg = <lw_data1>-werks
*                                      .
*          IF sy-subrc = 0.
*            <lw_data1>-spcid = 'E'.
*          ENDIF.
          PERFORM frm_read_text USING <lw_data1>-vbeln sy-langu 'Z001' 'VBBK' CHANGING  <lw_data1>-sgtxt.
        ENDLOOP.
      ENDIF.
      IF rad3 = 'X'.
        DELETE it_data1 WHERE kdbnum = 0.
      ENDIF.
      IF rad4 = 'X'.
        DELETE it_data1 WHERE kdbnum > 0.
      ENDIF.


    WHEN OTHERS.
  ENDCASE.
  PERFORM frm_lock TABLES it_data1.


ENDFORM. " FRM_GET_DATA



*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       刷新ALV
*----------------------------------------------------------------------*

FORM frm_refresh_alv CHANGING pcl_alv TYPE REF TO cl_gui_alv_grid.

*刷新稳定性

  DATA: lw_stbl TYPE lvc_s_stbl.
  lw_stbl-row = 'X'.
  lw_stbl-col = 'X'.
  CALL METHOD pcl_alv->refresh_table_display
    EXPORTING
      is_stable = lw_stbl
*     I_SOFT_REFRESH =
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " FRM_REFRESH_ALV



*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*       获取改变数据
*----------------------------------------------------------------------*

FORM frm_check_changed_data USING pcl_alv TYPE REF TO cl_gui_alv_grid..
  DATA:
    l_valid.
  CALL METHOD pcl_alv->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDFORM. "FRM_CHECK_CHANGED_DATA




*&---------------------------------------------------------------------*
*&      Form  FRM_GET_SELECTED_DATA_1
*&---------------------------------------------------------------------*
*       获取选择的数据 并检查
*----------------------------------------------------------------------*
*      -->P_GCL_ALV_1  text
*----------------------------------------------------------------------*

FORM frm_get_selected_data1.
  CLEAR:it_data_s[].

  DATA:
    ls_row_no    TYPE lvc_s_roid,
    lt_row_index TYPE lvc_t_row,
    lt_row_no    TYPE lvc_t_roid.

*  获取选择的行

  CALL METHOD g_alv_1->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_index
      et_row_no     = lt_row_no.

  LOOP AT lt_row_no INTO ls_row_no.

    READ TABLE it_data1 INDEX ls_row_no-row_id."读取选择的行
    IF sy-subrc = 0.
      APPEND it_data1 TO  it_data_s.
    ENDIF.
  ENDLOOP.

  IF it_data_s[] IS INITIAL.
    MESSAGE s007 DISPLAY LIKE 'W'.
  ENDIF.
  CHECK it_data_s IS NOT INITIAL.
ENDFORM. " FRM_GET_SELECTED_DATA_1


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_SELECTED_DATA_1
*&---------------------------------------------------------------------*
*       获取选择的数据 并检查
*----------------------------------------------------------------------*
*      -->P_GCL_ALV_1  text
*----------------------------------------------------------------------*

FORM frm_get_selected_data2.
  CLEAR:it_data_s2[].
  DATA:
    ls_row_no    TYPE lvc_s_roid,
    lt_row_index TYPE lvc_t_row,
    lt_row_no    TYPE lvc_t_roid.

*  获取选择的行
  CALL METHOD g_alv_2->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_index
      et_row_no     = lt_row_no.

  LOOP AT lt_row_no INTO ls_row_no.
    READ TABLE it_data2 INDEX ls_row_no-row_id."读取选择的行
    IF sy-subrc = 0.
      APPEND it_data2 TO  it_data_s2.
    ENDIF.
  ENDLOOP.

  IF it_data_s2[] IS INITIAL.
    MESSAGE s007 DISPLAY LIKE 'W'.
  ENDIF.
  CHECK it_data_s2 IS NOT INITIAL.
ENDFORM. " FRM_GET_SELECTED_DATA_1


*&---------------------------------------------------------------------*
*&      Form  frm_delrow_data1
*&---------------------------------------------------------------------*
*       删除alv1选中数据
*----------------------------------------------------------------------*

FORM frm_delrow_data1.
  PERFORM frm_get_selected_data1.
  LOOP AT it_data_s.


  ENDLOOP.
  PERFORM frm_refresh_alv USING g_alv_1.
ENDFORM. "frm_delrow_data1



*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATE
*&---------------------------------------------------------------------*
*       保存交期
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM frm_save_date .


  PERFORM frm_refresh_alv USING g_alv_1.
  PERFORM frm_refresh_alv USING g_alv_2.
ENDFORM. " FRM_SAVE_DATE





*&---------------------------------------------------------------------*
*&      Form  frm_add_zero
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_IN       text
*----------------------------------------------------------------------*

FORM frm_add_zero CHANGING f_in.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = f_in
    IMPORTING
      output = f_in.
ENDFORM. "frm_add_zero


*&--------------------------------------------------------------------*
*&      Form  FRM_HINTMSGTXT
*&--------------------------------------------------------------------*
*       显示状态栏信息
*---------------------------------------------------------------------*

FORM frm_hintmsgtxt USING p_count p_index .

  DATA:l_c(6),p_msg(40).
  l_c = p_index.
  p_msg = p_count.
  DATA l_per TYPE p DECIMALS 2.
  l_per = p_index / p_count * 100.
  CONDENSE:l_c,p_msg.
  CONCATENATE '总共：' p_msg '条，当前处理：' l_c '条' INTO p_msg.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_per
      text       = p_msg
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM. "FRM_HINTMSGTXT



*&---------------------------------------------------------------------*
*&      Form  frm_confirm
*&---------------------------------------------------------------------*
*       确认对话框
*----------------------------------------------------------------------*

FORM frm_confirm USING p_title p_text CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = p_title
      text_question = p_text
    IMPORTING
      answer        = p_answer.                             "1、 2、 A

ENDFORM. " frm_confirm

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

    WHEN 'SASCEND'   OR
         'SDESCEND'.                  "sort column
      PERFORM fcode_sort_tc USING p_tc_name
                                  p_table_name
                                  l_ok.
      CLEAR l_ok.
    WHEN 'POSI'.
      CLEAR GV_CANCEL.
      CALL SCREEN 9002 STARTING AT 50 8.
      CHECK GV_CANCEL NE 'X'.
      PERFORM fcode_posi_tc USING p_tc_name
                                  p_table_name
                                  l_ok.
      CLEAR l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*
  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline
*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*
  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline
*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_IT_DATA3
*&---------------------------------------------------------------------*
*       保存临时建立的调拨单数据
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save_it_data3 .
  FIELD-SYMBOLS <lw_data1> LIKE LINE OF it_data1.
  DATA lt_zmm002i TYPE zmm002i OCCURS 0 WITH HEADER LINE.
  DATA l_dbdh TYPE zdbdh.
  IF it_data3[] IS NOT INITIAL.
    SELECT SINGLE MAX( dbdh ) FROM zmm002i
                              INTO l_dbdh.
    IF l_dbdh = 0.
      l_dbdh = g_dbdh_begin.
    ENDIF.
    l_dbdh = l_dbdh + 1.
    LOOP AT it_data3.
      READ TABLE it_data1 ASSIGNING <lw_data1> WITH KEY vbeln = it_data3-vbeln
                                                        ebeln = it_data3-ebeln
                                                        posnr = it_data3-posnr
                                                        ebelp = it_data3-ebelp
                                                        matnr = it_data3-matnr
                                                        werks = it_data3-werks
                                                        .
      IF sy-subrc = 0.
        <lw_data1>-zbox = 'X'.  "改行已经建立了单据，打上标记
        <lw_data1>-editflag = ''."临时数据已经存储，临时编辑标记取消
      ENDIF.
      MOVE-CORRESPONDING it_data3 TO lt_zmm002i.
      lt_zmm002i-dbdh = l_dbdh.
      APPEND lt_zmm002i.
    ENDLOOP.
    APPEND LINES OF lt_zmm002i TO it_zmm002i.
    INSERT  zmm002i FROM TABLE lt_zmm002i[].
    IF sy-subrc = 0.
      MESSAGE s006 WITH l_dbdh.
    ENDIF.
    CLEAR: it_data3[],it_data3.
  ELSE.
    MESSAGE s005 DISPLAY LIKE 'W'.
  ENDIF.
  PERFORM frm_refresh_alv USING g_alv_1.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 8,  "打印单据默认行数
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i ,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.

  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFMM0082'.
* DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
* DATA:lw_select LIKE LINE OF lt_select.
  DATA:lt_prt TYPE zmm002i OCCURS 0 WITH HEADER LINE.
  DATA:lw_prt LIKE LINE OF lt_prt.
  FIELD-SYMBOLS <lw_prt> LIKE LINE OF lt_prt.

  IF it_data_s2[] IS INITIAL.
    MESSAGE s007(zmm01) DISPLAY LIKE 'W'.
  ENDIF.
  CHECK it_data_s2[] IS NOT INITIAL.

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

  SORT it_data_s2[] BY dbdh werks vbeln matnr.

  LOOP AT it_data_s2[] ASSIGNING <lw_prt>.
    AT NEW vbeln.
      CLEAR : lt_prt[],
              ncurrline,
              emptycount.
*      npageline = 8.
    ENDAT.

    APPEND <lw_prt> TO lt_prt.

    AT END OF vbeln.
      DESCRIBE TABLE lt_prt LINES ntotalline.
      ncurrline = ntotalline MOD npageline.
      IF  ncurrline > 0.
        emptycount = npageline - ncurrline.
        DO emptycount TIMES.
          APPEND INITIAL LINE TO lt_prt.
        ENDDO.
      ENDIF.

      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
          w_head             = <lw_prt>
          npage_line         = npageline
        TABLES
          t_item             = lt_prt[]
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


FORM frm_lock TABLES t_table STRUCTURE it_data1.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  FIELD-SYMBOLS <lw_out> LIKE LINE OF t_table.
  LOOP AT t_table ASSIGNING <lw_out>.
    CALL FUNCTION 'ENQUEUE_EZMM002I'
      EXPORTING
*       MODE_ZMM002I   = 'E'
*       MANDT          = SY-MANDT
*       DBDH           =
        werks          = <lw_out>-werks
        vbeln          = <lw_out>-vbeln
        ebeln          = <lw_out>-ebeln
        posnr          = <lw_out>-posnr
        ebelp          = <lw_out>-ebelp
        matnr          = <lw_out>-matnr
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
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      <lw_out>-locked = icon_locked.
    ELSE.
      <lw_out>-locked = icon_unlocked.
*   Implement suitable error handling here
    ENDIF.
  ENDLOOP.


ENDFORM.


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
  CLEAR t_text.
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

FORM frm_unlock.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FCODE_SORT_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TC_NAME  text
*      -->P_P_OK  text
*----------------------------------------------------------------------*
FORM fcode_sort_tc  USING    p_tc_name
                             p_table_name
                             p_okcode .
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.
  DATA: cl_descr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <tc_cols>    TYPE scxtab_column.
  FIELD-SYMBOLS <sel_fiel>   TYPE fieldname.
  FIELD-SYMBOLS:<fs_comp> TYPE abap_compdescr.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*
  ASSIGN (p_tc_name) TO <tc>.
  READ TABLE <tc>-cols ASSIGNING <tc_cols> WITH KEY selected = 'X'.
  CHECK <tc_cols> IS ASSIGNED.
*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline
  CHECK <table> IS ASSIGNED.
  cl_descr ?= cl_abap_typedescr=>describe_by_data( it_sel_stock ).
  READ TABLE cl_descr->components ASSIGNING <fs_comp> INDEX <tc_cols>-index.
  CHECK <fs_comp> IS ASSIGNED.
  IF p_okcode = 'SASCEND'.
    SORT <table> BY (<fs_comp>-name).
  ELSEIF p_okcode = 'SDESCEND'.
    SORT <table> BY (<fs_comp>-name) DESCENDING.
  ENDIF.
*&SPWIZARD: SOT table with sel field                                   *


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FCODE_POSI_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TC_NAME  text
*      -->P_P_TABLE_NAME  text
*      -->P_L_OK  text
*----------------------------------------------------------------------*
FORM fcode_posi_tc  USING    p_tc_name
                             p_table_name
                             p_okcode .
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.
  DATA: cl_descr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
*  FIELD-SYMBOLS <tc_cols>    TYPE scxtab_column.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*
  ASSIGN (p_tc_name) TO <tc>.
*  READ TABLE <tc>-cols ASSIGNING <tc_cols> WITH KEY selected = 'X'.
*  CHECK <tc_cols> IS ASSIGNED.
*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline
  CHECK <table> IS ASSIGNED.
  IF gv_charg IS INITIAL AND gv_lgort IS NOT INITIAL.
    READ TABLE it_sel_stock WITH KEY lgort = gv_lgort.

  ELSEIF gv_charg IS NOT INITIAL AND gv_lgort IS INITIAL.
    READ TABLE it_sel_stock WITH KEY charg = gv_charg.
  ELSEIF gv_charg IS NOT INITIAL AND gv_lgort IS NOT INITIAL.
    READ TABLE it_sel_stock WITH KEY lgort = gv_lgort
                                     charg = gv_charg.
  ENDIF.
  IF sy-subrc = 0.
    <tc>-top_line = sy-tabix.
  ENDIF.
*&SPWIZARD: SOT table with sel field
ENDFORM.
