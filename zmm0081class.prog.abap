*&---------------------------------------------------------------------*
*&  包含                ZMM0081CLASS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS GCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
* 处理ALV事件类声明
*----------------------------------------------------------------------*

CLASS LCL_DRAGDROPOBJ DEFINITION.
  PUBLIC SECTION.
    DATA: IT_DATA  LIKE TABLE OF IT_DATA1,
          WA_DATA  LIKE LINE OF IT_DATA1,
          INDEX    TYPE I, "Index of Line to be moved or copied.
          DROP_ALV TYPE C . "1  = avl1  2 = avl2

ENDCLASS. "LCL_DRAGDROPOBJ DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_event_handler1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS GCL_EVENT_HANDLER1 DEFINITION.
  PUBLIC SECTION.
    METHODS:

*    拖拽排序

      HANDLE_ALV_DRAG
      FOR EVENT ONDRAG
                    OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ,
      HANDLE_ALV_DROP
      FOR EVENT ONDROP
                    OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ,

      HANDLE_DROPCOMPLETE
      FOR EVENT ONDROPCOMPLETE  OF
            CL_GUI_ALV_GRID
        IMPORTING
            E_ROW
            E_DRAGDROPOBJ ,

*    工具栏自定义按钮

      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

*    监控按钮

      HANDLE_BEFORE_USER_COMMAND FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

*    监控按钮

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

*     单击

      HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO,

*    双击

      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN  ES_ROW_NO,

*    数据改变

      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,

*    数据改变 后

      HANDLE_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS,

      HANDLE_ONF4  FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.

ENDCLASS. "GCL_EVENT_HANDLER_GDKB DEFINITION


*----------------------------------------------------------------------*
*       CLASS gcl_event_handler2 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS GCL_EVENT_HANDLER2 DEFINITION.
  PUBLIC SECTION.
    METHODS:

*    拖拽排序

      HANDLE_ALV_DRAG
      FOR EVENT ONDRAG
                    OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ,

      HANDLE_ALV_DROP
      FOR EVENT ONDROP
                    OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ,

      HANDLE_DROPCOMPLETE
      FOR EVENT ONDROPCOMPLETE  OF
            CL_GUI_ALV_GRID
        IMPORTING
            E_ROW
            E_DRAGDROPOBJ ,



*    工具栏自定义按钮

      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

*    监控按钮

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

*     单击

      HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO,

*    双击

      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN  ES_ROW_NO,

*    数据改变

      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,

*    数据改变 后

      HANDLE_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS,

      HANDLE_ONF4  FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.

ENDCLASS. "GCL_EVENT_HANDLER_GDKB DEFINITION


*----------------------------------------------------------------------*
*       CLASS GCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
* 处理ALV事件类实现
*----------------------------------------------------------------------*

CLASS GCL_EVENT_HANDLER1 IMPLEMENTATION.
  METHOD HANDLE_ALV_DRAG.
    PERFORM HANDLE_ALV_DRAG1 USING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ.
  ENDMETHOD.                    "HANDLE_ALV_DRAG

  METHOD HANDLE_ALV_DROP.
    PERFORM HANDLE_ALV_DROP1 USING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ.
  ENDMETHOD.                    "HANDLE_ALV_DRAG

  METHOD HANDLE_DROPCOMPLETE .
    PERFORM HANDLE_DROPCOMPLETE_FROM_ALV1 USING E_ROW E_DRAGDROPOBJ.
  ENDMETHOD .                    "HANDLE_DROPCOMPLETE

*  handle toolbar

  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR1 USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

*  handler user command

  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND1 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command


*  handler befor user command

  METHOD HANDLE_BEFORE_USER_COMMAND.
    PERFORM HANDLE_BEFORE_USER_COMMAND1 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command


*  handle hotspot click

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK1 USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click

*  handler double click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDE_DOUBLE_CLICK1 USING E_ROW E_COLUMN  ES_ROW_NO.
  ENDMETHOD.                    "handle_double_click

*  handler data changed

  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED1 USING ER_DATA_CHANGED E_ONF4
E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
  ENDMETHOD.                    "handle_data_changed

*handle_data_changed_finished

  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM HANDLE_DATA_CHANGED_FINISHED1 USING E_MODIFIED ET_GOOD_CELLS.
  ENDMETHOD.                    "handle_data_changed_finished

  METHOD   HANDLE_ONF4.
    PERFORM HANDLE_ONF41 USING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.
  ENDMETHOD.                    "handle_onf4
ENDCLASS. "GCL_EVENT_HANDLER IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS gcl_event_handler2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS GCL_EVENT_HANDLER2 IMPLEMENTATION.
  METHOD HANDLE_ALV_DRAG.
    PERFORM HANDLE_ALV_DRAG2 USING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ.
  ENDMETHOD.                    "HANDLE_ALV_DRAG

  METHOD HANDLE_ALV_DROP.
    PERFORM HANDLE_ALV_DROP2 USING E_ROW E_COLUMN ES_ROW_NO E_DRAGDROPOBJ.
  ENDMETHOD.                    "HANDLE_ALV_DRAG

  METHOD HANDLE_DROPCOMPLETE .
    PERFORM HANDLE_DROPCOMPLETE_FROM_ALV2 USING E_ROW E_DRAGDROPOBJ.
  ENDMETHOD .                    "HANDLE_DROPCOMPLETE

*  handle toolbar

  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR2 USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

*  handler user command

  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND2 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command

*  handle hotspot click

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK2 USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click

*  handler double click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDE_DOUBLE_CLICK2 USING E_ROW E_COLUMN  ES_ROW_NO.
  ENDMETHOD.                    "handle_double_click

*  handler data changed

  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED2 USING ER_DATA_CHANGED E_ONF4
E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
  ENDMETHOD.                    "handle_data_changed

*handle_data_changed_finished

  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM HANDLE_DATA_CHANGED_FINISHED2 USING E_MODIFIED ET_GOOD_CELLS.
  ENDMETHOD.                    "handle_data_changed_finished

  METHOD   HANDLE_ONF4.
    PERFORM HANDLE_ONF42 USING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.
  ENDMETHOD.                    "handle_onf4
ENDCLASS. "GCL_EVENT_HANDLER IMPLEMENTATION



*&---------------------------------------------------------------------*
*&      Form  HANDLE_ALV_DRAG1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM HANDLE_ALV_DRAG1 USING E_ROW TYPE LVC_S_ROW
                             E_COLUMN TYPE LVC_S_COL
                             ES_ROW_NO TYPE LVC_S_ROID
                             E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                             .
  DATA: DATAOBJ TYPE REF TO LCL_DRAGDROPOBJ,
        LINE    LIKE LINE OF IT_DATA1.

  PERFORM FRM_GET_SELECTED_DATA1.

* create and fill dataobject for events ONDROP and ONDROPCOMPLETE

  CREATE OBJECT DATAOBJ.

* store the dragged line, too.

  READ TABLE IT_DATA1 INTO DATAOBJ->WA_DATA INDEX E_ROW-INDEX.
  DELETE IT_DATA_S WHERE LOCKED = ICON_LOCKED.
  APPEND LINES OF IT_DATA_S[] TO DATAOBJ->IT_DATA[].
  E_DRAGDROPOBJ->OBJECT = DATAOBJ.
ENDFORM. "HANDLE_ALV_DRAG1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_ALV_drop1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_ROW          text
*      -->E_COLUMN       text
*      -->ES_ROW_NO      text
*      -->E_DRAGDROPOBJ  text
*----------------------------------------------------------------------*

FORM HANDLE_ALV_DROP1 USING E_ROW TYPE LVC_S_ROW
                             E_COLUMN TYPE LVC_S_COL
                             ES_ROW_NO TYPE LVC_S_ROID
                             E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                             .
*  DATA : dataobj TYPE REF TO lcl_dragdropobj .
*  dataobj  ?= e_dragdropobj->object .
*  dataobj->index = e_row-index.
*  dataobj->drop_alv = '1' .
*  e_dragdropobj->object = dataobj .
ENDFORM. "HANDLE_ALV_drop1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DROPCOMPLETE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM HANDLE_DROPCOMPLETE_FROM_ALV1 USING E_ROW TYPE LVC_S_ROW
                                   E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                                    .
  DATA: DATAOBJ TYPE REF TO LCL_DRAGDROPOBJ.
  DATA: LS_EDIT TYPE LVC_S_STYL,
        LT_EDIT TYPE LVC_T_STYL.
  FIELD-SYMBOLS <LW_DATA> LIKE DATAOBJ->WA_DATA.
  DATA:L_LINE TYPE I.
  DATA:LT_ZMM002I TYPE ZMM002I OCCURS 0 WITH HEADER LINE.
  CATCH SYSTEM-EXCEPTIONS MOVE_CAST_ERROR = 1.
    DATAOBJ ?= E_DRAGDROPOBJ->OBJECT.
    CLEAR: IT_DATA2[].
    IF DATAOBJ->DROP_ALV = '1'.
      IF E_DRAGDROPOBJ->EFFECT EQ CL_DRAGDROP=>MOVE.
        DELETE IT_DATA1 INDEX E_ROW-INDEX.
        INSERT DATAOBJ->WA_DATA INTO IT_DATA1 INDEX DATAOBJ->INDEX.
      ENDIF.
    ELSE.
      L_LINE = DATAOBJ->INDEX.
      IF E_DRAGDROPOBJ->EFFECT EQ CL_DRAGDROP=>MOVE.
        "保证数据实时性,从数据库中取数
        SELECT * FROM ZMM002I INTO TABLE LT_ZMM002I
          FOR ALL ENTRIES IN DATAOBJ->IT_DATA
                WHERE  VBELN     = DATAOBJ->IT_DATA-VBELN
                  AND  EBELN     = DATAOBJ->IT_DATA-EBELN
                  AND  POSNR     = DATAOBJ->IT_DATA-POSNR
                  AND  EBELP     = DATAOBJ->IT_DATA-EBELP
                  AND  MATNR     = DATAOBJ->IT_DATA-MATNR
                  AND  ZDELFLAG  = ''.

*       append LINES OF dataobj->it_data[] to it_data2[].
        LOOP AT DATAOBJ->IT_DATA ASSIGNING <LW_DATA>.
          "it_zmm002i换成lt_ZMM002i
          LOOP AT LT_ZMM002I WHERE VBELN     = <LW_DATA>-VBELN
                               AND  EBELN    = <LW_DATA>-EBELN
                               AND  POSNR    = <LW_DATA>-POSNR
                               AND  EBELP    = <LW_DATA>-EBELP
                               AND  MATNR    = <LW_DATA>-MATNR
                               AND  ZDELFLAG = ''
                               .
            MOVE-CORRESPONDING LT_ZMM002I TO IT_DATA2.
            APPEND IT_DATA2.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.

    SORT IT_DATA2[] BY DBDH.
    IF IT_DATA2[] IS INITIAL.
      APPEND INITIAL LINE TO IT_DATA2[].
    ENDIF.
* 9. Check which operation the user has conducted (copy or move).
    CLEAR DATAOBJ->IT_DATA[].
    PERFORM FRM_REFRESH_ALV USING G_ALV_1.
    PERFORM FRM_REFRESH_ALV USING G_ALV_2.
  ENDCATCH.

  IF SY-SUBRC <> 0.
* If anything went wrong this is the clean way of aborting the
* drag and drop operation:
    CALL METHOD E_DRAGDROPOBJ->ABORT.
  ENDIF.
ENDFORM. "HANDLE_DROPCOMPLETE2



*&---------------------------------------------------------------------*
*&      Form  handle_alv_drag2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_ROW          text
*      -->E_COLUMN       text
*      -->ES_ROW_NO      text
*      -->E_DRAGDROPOBJ  text
*----------------------------------------------------------------------*

FORM HANDLE_ALV_DRAG2 USING E_ROW TYPE LVC_S_ROW
                             E_COLUMN TYPE LVC_S_COL
                             ES_ROW_NO TYPE LVC_S_ROID
                             E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                             .
*  DATA: dataobj TYPE REF TO lcl_dragdropobj.
*
*  CREATE OBJECT dataobj.
*
** store the dragged line, too.
*
*  READ TABLE it_data1 INTO dataobj->wa_data INDEX e_row-index.
*  PERFORM frm_get_selected_data2.
*
** store the dragged line, too.
*
*  APPEND LINES OF it_data_s2[] TO dataobj->it_data[].
*  e_dragdropobj->object = dataobj.
ENDFORM. "HANDLE_ALV_DRAG1


*&---------------------------------------------------------------------*
*&      Form  handle_alv_drop2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_ROW          text
*      -->E_COLUMN       text
*      -->ES_ROW_NO      text
*      -->E_DRAGDROPOBJ  text
*----------------------------------------------------------------------*

FORM HANDLE_ALV_DROP2 USING E_ROW TYPE LVC_S_ROW
                             E_COLUMN TYPE LVC_S_COL
                             ES_ROW_NO TYPE LVC_S_ROID
                             E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                             .
  DATA : DATAOBJ TYPE REF TO LCL_DRAGDROPOBJ .
  DATAOBJ  ?= E_DRAGDROPOBJ->OBJECT .

* remember the row index to copy or move a line

  DATAOBJ->INDEX = E_ROW-INDEX.
  DATAOBJ->DROP_ALV = '2' .
  E_DRAGDROPOBJ->OBJECT = DATAOBJ .
ENDFORM. "HANDLE_ALV_drop1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DROPCOMPLETE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM HANDLE_DROPCOMPLETE_FROM_ALV2 USING E_ROW TYPE LVC_S_ROW
                                   E_DRAGDROPOBJ TYPE REF TO CL_DRAGDROPOBJECT
                                    .
  DATA L_LINES TYPE I.
  DATA: DATAOBJ TYPE REF TO LCL_DRAGDROPOBJ.
  FIELD-SYMBOLS <LW_DATA> LIKE DATAOBJ->WA_DATA.

  CATCH SYSTEM-EXCEPTIONS MOVE_CAST_ERROR = 1.

    DATAOBJ ?= E_DRAGDROPOBJ->OBJECT.
    DESCRIBE TABLE DATAOBJ->IT_DATA[] LINES L_LINES.
    IF L_LINES > 0.
      L_LINES =  DATAOBJ->INDEX.
      IF E_DRAGDROPOBJ->EFFECT EQ CL_DRAGDROP=>MOVE AND  DATAOBJ->DROP_ALV = '1'.

*      insert dataobj->wa_data INTO it_data1 index dataobj->index.
*      DELETE it_data2 INDEX e_row-index.


        LOOP AT DATAOBJ->IT_DATA ASSIGNING <LW_DATA>.
          INSERT <LW_DATA> INTO IT_DATA1[] INDEX L_LINES.
*          DELETE it_data2 WHERE vbeln = <lw_data>-vbeln AND posnr = <lw_data>-posnr.
          L_LINES = L_LINES + 1.
        ENDLOOP.

      ENDIF.

* 9. Check which operation the user has conducted (copy or move).


      PERFORM FRM_REFRESH_ALV USING G_ALV_1.
      PERFORM FRM_REFRESH_ALV USING G_ALV_2.
    ENDIF.
  ENDCATCH.
  IF SY-SUBRC = 0.

* If anything went wrong this is the clean way of aborting the
* drag and drop operation:

    CALL METHOD E_DRAGDROPOBJ->ABORT.
  ENDIF.
ENDFORM. "HANDLE_DROPCOMPLETE2



*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       自定义按钮-ALV1增加调拨单数据建单保存按钮
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM HANDLE_TOOLBAR1 USING
      P_E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
      P_E_INTERACTIVE.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 4.
  MOVE '&SAVE1' TO LS_TOOLBAR-FUNCTION.
  MOVE '保存' TO LS_TOOLBAR-QUICKINFO.

  MOVE ICON_SYSTEM_SAVE TO LS_TOOLBAR-ICON.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

ENDFORM. " handle_toolbar_gdkb



*&---------------------------------------------------------------------*
*&      Form  handle_before_user_command1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*

FORM HANDLE_BEFORE_USER_COMMAND1 USING P_E_UCOMM.
  CASE P_E_UCOMM.
    WHEN ''.

    WHEN OTHERS.

  ENDCASE.
ENDFORM. " HANDLE_USER_COMMAND1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND_GDKB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*

FORM HANDLE_USER_COMMAND1 USING P_E_UCOMM.
  CASE P_E_UCOMM.
    WHEN '&SAVE1'.
      "保存建单数据
      PERFORM FRM_SAVE_IT_DATA3.
    WHEN OTHERS.

  ENDCASE.
ENDFORM. " HANDLE_USER_COMMAND1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW_ID  TEXT
*      -->P_COLUMN_ID  TEXT
*      -->PS_ROW_NO  TEXT
*----------------------------------------------------------------------*

FORM HANDLE_HOTSPOT_CLICK1 USING PS_ROW TYPE LVC_S_ROW
                                  PS_COLUMN TYPE  LVC_S_COL
                                  PS_ROW_NO TYPE  LVC_S_ROID.

ENDFORM. " HANDLE_HOTSPOT_CLICK_1


*&---------------------------------------------------------------------*
*&      Form  HANDE_DOUBLE_CLICK_GDKB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*

FORM HANDE_DOUBLE_CLICK1 USING PS_ROW TYPE LVC_S_ROW
                                  PS_COLUMN TYPE  LVC_S_COL
                                  PS_ROW_NO TYPE  LVC_S_ROID.
  DATA LT_MSKA TYPE MSKA OCCURS 0 WITH HEADER LINE.
  DATA:LT_DATA3 LIKE TABLE OF IT_DATA3 WITH HEADER LINE.
  CLEAR IT_DATA1.
  READ TABLE IT_DATA1 INDEX PS_ROW_NO-ROW_ID.
  IF SY-SUBRC = 0.
    CLEAR:G_DBD_NUM, G_DBD_MARGIN.
    "判断是否被锁定
    CALL FUNCTION 'ENQUEUE_EZMM002I'
      EXPORTING
*       MODE_ZMM002I   = 'E'
*       MANDT          = SY-MANDT
*       DBDH           =
        WERKS          = IT_DATA1-WERKS
        VBELN          = IT_DATA1-VBELN
        EBELN          = IT_DATA1-EBELN
        POSNR          = IT_DATA1-POSNR
        EBELP          = IT_DATA1-EBELP
        MATNR          = IT_DATA1-MATNR
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
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
*    IF it_data1-locked = icon_locked. "对于已被加锁数据不能操作
    IF SY-SUBRC NE 0. "对于已被加锁数据不能操作
      "获取锁定的消息
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'W'.
*      MESSAGE s016 DISPLAY LIKE 'W'.
*    ELSEIF it_data1-locked = icon_unlocked.
    ELSE.
      "取订单已建立调拨单调拨数量
      SELECT SUM( QTYBDB )
        INTO G_DBD_NUM
        FROM ZMM002I
        WHERE VBELN    = IT_DATA1-VBELN
        AND   EBELN    = IT_DATA1-EBELN
        AND   POSNR    = IT_DATA1-POSNR
        AND   EBELP    = IT_DATA1-EBELP
        AND   MATNR    = IT_DATA1-MATNR
        AND   ZDELFLAG = ''.

      G_DBD_MARGIN = IT_DATA1-KWMENG - G_DBD_NUM. "订单数量-已建立调拨单数量 = 可建立调拨单数量
*      spcid E 按单
*      xchpf X 批次库存
      IF IT_DATA1-SPCID = 'E'.
        IF IT_DATA1-XCHPF = 'X'.
          SELECT  MSKA~MATNR
                  MSKA~WERKS
                  MSKA~LGORT
                  T001L~LGOBE
                  MSKA~CHARG
                  MSKA~VBELN
                  MSKA~POSNR
                  MSKA~KALAB
            FROM MSKA
            LEFT JOIN T001L
            ON MSKA~WERKS = T001L~WERKS
            AND MSKA~LGORT = T001L~LGORT
            INTO CORRESPONDING FIELDS OF TABLE IT_SEL_STOCK
            WHERE MSKA~MATNR = IT_DATA1-MATNR
            AND   MSKA~WERKS = IT_DATA1-WERKS
            AND   MSKA~VBELN = IT_DATA1-VBELN
            AND   MSKA~POSNR = IT_DATA1-POSNR
            AND   MSKA~KALAB > 0
            AND   MSKA~CHARG <> ''.
        ELSE.
          SELECT MSKA~MATNR
                 MSKA~WERKS
                 MSKA~LGORT
                 T001L~LGOBE
                 MSKA~CHARG
                 MSKA~VBELN
                 MSKA~POSNR
                 MSKA~KALAB
           FROM MSKA
            LEFT JOIN T001L
            ON MSKA~WERKS = T001L~WERKS
            AND MSKA~LGORT = T001L~LGORT
           INTO CORRESPONDING FIELDS OF TABLE IT_SEL_STOCK
           WHERE MSKA~MATNR = IT_DATA1-MATNR
            AND  MSKA~WERKS = IT_DATA1-WERKS
            AND  MSKA~VBELN = IT_DATA1-VBELN
            AND  MSKA~POSNR = IT_DATA1-POSNR
            AND  MSKA~KALAB > 0
            AND  MSKA~CHARG = ''.
        ENDIF.
      ELSE.
        IF IT_DATA1-XCHPF = 'X'.
          SELECT MCHB~MATNR
                 MCHB~WERKS
                 MCHB~LGORT
                 T001L~LGOBE
                 MCHB~CHARG
                 MCHB~CLABS AS KALAB
            FROM MCHB
            LEFT JOIN T001L
            ON  MCHB~WERKS = T001L~WERKS
            AND MCHB~LGORT = T001L~LGORT
            INTO CORRESPONDING FIELDS OF TABLE IT_SEL_STOCK
            WHERE MCHB~MATNR = IT_DATA1-MATNR
            AND   MCHB~WERKS = IT_DATA1-WERKS
            AND   MCHB~CLABS > 0.
        ELSE.
          SELECT MARD~MATNR
                 MARD~WERKS
                 MARD~LGORT
                 T001L~LGOBE
                 MARD~LABST AS KALAB
            FROM MARD
            LEFT JOIN T001L
            ON MARD~WERKS = T001L~WERKS
            AND MARD~LGORT = T001L~LGORT
            INTO CORRESPONDING FIELDS OF TABLE IT_SEL_STOCK
            WHERE MARD~MATNR = IT_DATA1-MATNR
            AND   MARD~WERKS = IT_DATA1-WERKS
            AND   MARD~LABST > 0.
        ENDIF.
      ENDIF.
      IF IT_SEL_STOCK[] IS NOT INITIAL.
        LOOP AT IT_DATA3 WHERE VBELN  = IT_DATA1-VBELN
                        AND  EBELN = IT_DATA1-EBELN
                        AND  POSNR = IT_DATA1-POSNR
                        AND  EBELP = IT_DATA1-EBELP
                        AND  MATNR = IT_DATA1-MATNR
                        .
          CLEAR IT_SEL_STOCK.
          READ TABLE IT_SEL_STOCK WITH KEY MATNR = IT_DATA3-MATNR
                                           WERKS  = IT_DATA3-WERKS
                                           LGORT  = IT_DATA3-SLOCFR
                                           CHARG  = IT_DATA3-CHARG
                                           VBELN  = IT_DATA3-VGBEL
                                           POSNR  = IT_DATA3-VGPOS .
          IF SY-SUBRC = 0.
            IT_SEL_STOCK-LGORTO = IT_DATA3-STOCTO.
            IT_SEL_STOCK-LABST = IT_DATA3-QTYBDB.
            MODIFY IT_SEL_STOCK FROM IT_SEL_STOCK   INDEX SY-TABIX.
          ENDIF.
        ENDLOOP.

        CLEAR:
        TAB_STOCK-FIXED_COLS,
        TAB_STOCK-LINES,
        TAB_STOCK-TOP_LINE,
        TAB_STOCK-CURRENT_LINE.

        CALL SCREEN 9001 STARTING AT 10 1 ENDING AT 120 20.
        IT_DATA1-EDITFLAG = 'X'.
        MODIFY IT_DATA1 INDEX PS_ROW_NO-ROW_ID.
        PERFORM FRM_REFRESH_ALV USING G_ALV_1.
      ELSE.
        MESSAGE S009.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM. " HANDE_DOUBLE_CLICK_ITEM


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*

FORM HANDLE_DATA_CHANGED1 USING
      P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
      P_E_ONF4
      P_E_ONF4_BEFORE
      P_E_ONF4_AFTER
      P_E_UCOMM TYPE SY-UCOMM.

  DATA LT_DATA LIKE TABLE OF IT_DATA1 WITH HEADER LINE.
  DATA LT_MODI  TYPE LVC_T_MODI WITH HEADER LINE.
  FIELD-SYMBOLS <LT_DATA> TYPE DATA .
  FIELD-SYMBOLS <IT_DATA1> LIKE LINE OF IT_DATA1 .
  LT_MODI[] = P_ER_DATA_CHANGED->MT_MOD_CELLS[].
  READ TABLE LT_MODI INDEX 1.

  PERFORM FRM_REFRESH_ALV USING G_ALV_1.


ENDFORM. " HANDLE_DATA_CHANGED_1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*

FORM HANDLE_DATA_CHANGED_FINISHED1 USING P_E_MODIFIED
                                              P_CELLS.

  FIELD-SYMBOLS <LW_DATA> LIKE LINE OF IT_DATA1.
  LOOP AT IT_DATA1 ASSIGNING <LW_DATA>.
*    <lw_data>-zleve = sy-tabix.
  ENDLOOP.
*  SORT it_data1 BY zleve.


ENDFORM. " HANDLE_DATA_CHANGED_FINISHED_1



*&---------------------------------------------------------------------*
*&      Form  handle_onf4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_FIELDNAME    text
*      -->ES_ROW_NO      text
*      -->ER_EVENT_DATA  text
*----------------------------------------------------------------------*

FORM HANDLE_ONF41 USING E_FIELDNAME
                           ES_ROW_NO TYPE LVC_S_ROID
                           ER_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA.



ENDFORM. " HANDLE_ONF4



*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       自定义按钮
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM HANDLE_TOOLBAR2 USING
      P_E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
      P_E_INTERACTIVE.


  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 0.
  MOVE '&DEL2' TO LS_TOOLBAR-FUNCTION.
  MOVE '删行' TO LS_TOOLBAR-QUICKINFO.

  MOVE ICON_DELETE_ROW TO LS_TOOLBAR-ICON.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.


  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 0.
  MOVE '&PRT2' TO LS_TOOLBAR-FUNCTION.
  MOVE '打印' TO LS_TOOLBAR-QUICKINFO.
  MOVE ICON_PRINT TO LS_TOOLBAR-ICON.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.


ENDFORM. " handle_toolbar_gdkb


*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND_GDKB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*

FORM HANDLE_USER_COMMAND2 USING P_E_UCOMM.
  FIELD-SYMBOLS <LW_DATA2> LIKE LINE OF IT_DATA2.
  FIELD-SYMBOLS <LW_ZMM002I> TYPE ZMM002I.
  DATA:LT_ZMM002I TYPE ZMM002I OCCURS 0 WITH HEADER LINE.
  CASE P_E_UCOMM.
    WHEN '&DEL2'.
      PERFORM FRM_GET_SELECTED_DATA2.
*      LOOP AT it_data_s2 WHERE mblnr = '' OR reversal = 'X'.
      LOOP AT IT_DATA_S2 WHERE  REVERSAL = 'X'
                          OR ( REVERSAL = '' AND MBLNR = '' )
                          OR TZBS = 'X' . "ADD it02 20160222 打调整标识也可删除
        READ TABLE IT_DATA2 ASSIGNING <LW_DATA2> WITH KEY
                                VBELN  = IT_DATA_S2-VBELN
                                EBELN  = IT_DATA_S2-EBELN
                                POSNR  = IT_DATA_S2-POSNR
                                EBELP  = IT_DATA_S2-EBELP
                                MATNR  = IT_DATA_S2-MATNR
                                WERKS  = IT_DATA_S2-WERKS
                                SLOCFR = IT_DATA_S2-SLOCFR
                                STOCTO = IT_DATA_S2-STOCTO
                                CHARG  = IT_DATA_S2-CHARG
                                VGBEL  = IT_DATA_S2-VGBEL
                                VGPOS  = IT_DATA_S2-VGPOS
                                DBDH   = IT_DATA_S2-DBDH
                                .
        IF SY-SUBRC = 0 .
          <LW_DATA2>-ZDELFLAG = 'X'.
        ENDIF.
        READ TABLE IT_ZMM002I ASSIGNING <LW_ZMM002I> WITH KEY
                                VBELN  = IT_DATA_S2-VBELN
                                EBELN  = IT_DATA_S2-EBELN
                                POSNR  = IT_DATA_S2-POSNR
                                EBELP  = IT_DATA_S2-EBELP
                                MATNR  = IT_DATA_S2-MATNR
                                WERKS  = IT_DATA_S2-WERKS
                                SLOCFR = IT_DATA_S2-SLOCFR
                                STOCTO = IT_DATA_S2-STOCTO
                                CHARG  = IT_DATA_S2-CHARG
                                VGBEL  = IT_DATA_S2-VGBEL
                                VGPOS  = IT_DATA_S2-VGPOS
                                DBDH   = IT_DATA_S2-DBDH
                                .
        IF SY-SUBRC = 0 .
          <LW_ZMM002I>-ZDELFLAG = 'X'.
          APPEND <LW_ZMM002I> TO LT_ZMM002I[].
        ENDIF.
      ENDLOOP.
      MODIFY ZMM002I FROM TABLE LT_ZMM002I[].
      IF SY-SUBRC = 0.
        DELETE IT_DATA2 WHERE ZDELFLAG = 'X'.
        DELETE IT_ZMM002I WHERE ZDELFLAG = 'X'.
      ENDIF.
*     APPEND LINES OF it_data_s2[] TO dataobj->it_data[].
      PERFORM FRM_REFRESH_ALV USING G_ALV_2.
    WHEN '&PRT2'.
      PERFORM FRM_GET_SELECTED_DATA2.
      PERFORM FRM_PRINT_DATA.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_USER_COMMAND_GDKB


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW_ID  TEXT
*      -->P_COLUMN_ID  TEXT
*      -->PS_ROW_NO  TEXT
*----------------------------------------------------------------------*

FORM HANDLE_HOTSPOT_CLICK2 USING PS_ROW TYPE LVC_S_ROW
                                  PS_COLUMN TYPE  LVC_S_COL
                                  PS_ROW_NO TYPE  LVC_S_ROID.

ENDFORM. " HANDLE_HOTSPOT_CLICK_1


*&---------------------------------------------------------------------*
*&      Form  HANDE_DOUBLE_CLICK_GDKB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*

FORM HANDE_DOUBLE_CLICK2 USING PS_ROW TYPE LVC_S_ROW
                                  PS_COLUMN TYPE  LVC_S_COL
                                  PS_ROW_NO TYPE  LVC_S_ROID.



ENDFORM. " HANDE_DOUBLE_CLICK_ITEM


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*

FORM HANDLE_DATA_CHANGED2 USING
      P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
      P_E_ONF4
      P_E_ONF4_BEFORE
      P_E_ONF4_AFTER
      P_E_UCOMM TYPE SY-UCOMM.
  DATA LT_DATA LIKE IT_DATA1 OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS <LW_DATA> LIKE LINE OF IT_DATA1.
  FIELD-SYMBOLS <LT_DATA> TYPE DATA .
  ASSIGN P_ER_DATA_CHANGED->MP_MOD_ROWS->* TO <LT_DATA>.
  LT_DATA[] = <LT_DATA>.
  LOOP AT LT_DATA.

*    READ TABLE it_data ASSIGNING <lw_data> WITH KEY matnr = lt_data-matnr
*                                                    werks = lt_data-werks
*                                                    mtart = lt_data-mtart
*                                                    matkl = lt_data-matkl
*                                                    .



  ENDLOOP.
  PERFORM FRM_REFRESH_ALV USING G_ALV_2.
ENDFORM. " HANDLE_DATA_CHANGED_1


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*

FORM HANDLE_DATA_CHANGED_FINISHED2 USING P_E_MODIFIED
                                              P_CELLS.

ENDFORM. " HANDLE_DATA_CHANGED_FINISHED_1



*&---------------------------------------------------------------------*
*&      Form  handle_onf4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_FIELDNAME    text
*      -->ES_ROW_NO      text
*      -->ER_EVENT_DATA  text
*----------------------------------------------------------------------*

FORM HANDLE_ONF42 USING E_FIELDNAME
                           ES_ROW_NO TYPE LVC_S_ROID
                           ER_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA.



ENDFORM. " HANDLE_ONF4
