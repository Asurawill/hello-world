*&---------------------------------------------------------------------*
*&  Include           ZFI008_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.
    methods:
*    工具栏自定义按钮
      handle_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,
*    监控按下按钮前
      handle_before_user_command for event before_user_command of cl_gui_alv_grid
        importing e_ucomm,
*    监控按钮
      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm,
*    监控按下按钮后
      handle_after_user_command for event after_user_command of cl_gui_alv_grid
        importing e_ucomm e_saved e_not_processed,
*     双击事件
      handle_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column es_row_no,
*    数据改变
      handle_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
*    数据改变后
      handle_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified  et_good_cells,
      handle_button_click for event button_click of cl_gui_alv_grid
        importing es_col_id es_row_no,
      handle_onf4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname es_row_no er_event_data et_bad_cells e_display.
endclass.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler implementation.
*  handle toolbar
  method handle_toolbar.
    perform handle_toolbar using e_object e_interactive.
  endmethod.                    "handle_toolbar
*  handler after user command
  method handle_before_user_command.
    perform handle_before_user_command using e_ucomm.
  endmethod.                    "handle_before_user_command
*  handler user command
  method handle_user_command.
    perform handle_user_command using e_ucomm.
  endmethod.                    "handle_user_command
*  handler after user command
  method handle_after_user_command.
    perform handle_after_user_command using e_ucomm e_saved e_not_processed.
  endmethod.                    "handle_after_user_command
*  handler double click
  method handle_double_click.
    perform handle_double_click using e_row e_column es_row_no.
  endmethod.                    "handle_double_click
*  handler data changed
  method handle_data_changed.
    perform handle_data_changed using er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
  endmethod.                    "handle_data_changed
*  handler data changed finished
  method handle_data_changed_finished.
    perform handle_data_changed_finished using e_modified et_good_cells.
  endmethod.                    "handle_data_changed_finished
*  HANDLE BUTTON CLICK
  method handle_button_click.
    perform handle_button_click using es_col_id es_row_no.
  endmethod.                    "handle_data_changed_finished
  method handle_onf4.
    perform handle_onf4 using e_fieldname es_row_no er_event_data et_bad_cells e_display.
  endmethod.                                                "on_f4
endclass.                    "lcl_event_handler IMPLEMENTATION
