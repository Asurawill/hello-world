*&---------------------------------------------------------------------*
*&  包含                ZFI006_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS:
*    工具栏自定义按钮
      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,
*    监控按下按钮前
      HANDLE_BEFORE_USER_COMMAND FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,
*    监控按钮
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,
*    监控按下按钮后
      HANDLE_AFTER_USER_COMMAND FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM E_SAVED E_NOT_PROCESSED,
*     双击事件
      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN ES_ROW_NO,
*    数据改变
      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,
*    数据改变后
      HANDLE_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED  ET_GOOD_CELLS,
      HANDLE_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_COL_ID ES_ROW_NO,
      HANDLE_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*  handle toolbar
  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar
*  handler after user command
  METHOD HANDLE_BEFORE_USER_COMMAND.
*    PERFORM HANDLE_BEFORE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "handle_before_user_command
*  handler user command
  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command
*  handler after user command
  METHOD HANDLE_AFTER_USER_COMMAND.
*    PERFORM HANDLE_AFTER_USER_COMMAND USING E_UCOMM E_SAVED E_NOT_PROCESSED.
  ENDMETHOD.                    "handle_after_user_command
*  handler double click
  METHOD HANDLE_DOUBLE_CLICK.
*    PERFORM HANDLE_DOUBLE_CLICK USING E_ROW E_COLUMN ES_ROW_NO.
  ENDMETHOD.                    "handle_double_click
*  handler data changed
  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
  ENDMETHOD.                    "handle_data_changed
*  handler data changed finished
  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM HANDLE_DATA_CHANGED_FINISHED USING E_MODIFIED ET_GOOD_CELLS.
  ENDMETHOD.                    "handle_data_changed_finished
*  HANDLE BUTTON CLICK
  METHOD HANDLE_BUTTON_CLICK.
*    PERFORM HANDLE_BUTTON_CLICK USING ES_COL_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_data_changed_finished
  METHOD HANDLE_ONF4.
   PERFORM HANDLE_ONF4 USING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.
  ENDMETHOD.                                                "on_f4
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
