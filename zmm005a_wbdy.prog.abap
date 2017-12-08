


*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2017/05/16                                            *
*& Created By   : leyard-it02-weiyun                                        *
*& Description  :外币订单打印                                        *


"变量定义


TYPES:BEGIN OF ty_hd,
        ebeln         TYPE ekko-ebeln, "采购订单
        aedat         TYPE ekko-aedat, "日期
        name1         TYPE lfa1-name1, "供应商名称
        lifnr         TYPE lfa1-lifnr,  "供应商编码
        stras         TYPE lfa1-stras,  "地址
        "lxr   type string,       "联系人
        telf1         TYPE lfa1-telf1,  "电话
        telfx         TYPE lfa1-telfx,  "传真
        butxt         TYPE t001-butxt,  "公司中文名称
        butxt_en      TYPE string,   "英文名称
        street        TYPE adrc-street,  "中文地址
        street_en     TYPE string,   "英文地址
        tel_number    TYPE v_024-tel_number, "电话
        telfx_gs      TYPE v_024-telfx, "传真
        menge_hj      TYPE ekpo-menge, "数量-总计
        kwert_hj      TYPE konv-kwert,  "金额-总计
        htlx          TYPE string,  "合同类型
        zbz           TYPE string,  "备注
        zlbz          TYPE string,  "质量保证
        zqtyd         TYPE string,  "其他约定
        zmydd         TYPE string,   "贸易地点
        zycd(50)      TYPE c ,       "原产地
        zycd_en(50)   TYPE c,
        zmdd(50)      TYPE c,         "目的地
        zmdd_en(50)   TYPE c,
        zzzsyd(50)    TYPE c,       "最终使用地
        zzzsyd_en(50) TYPE c,
        zmytj(50)     TYPE c,        "贸易条件
        zmytj_en(50)  TYPE c,
        zjhsj         TYPE string,  "交货时间
        zjhsj_en(50)  TYPE c,
        zpp(50)       TYPE c,  "品牌
        zpp_en(50)    TYPE c,
        waers         TYPE ekko-waers,  "货币码
      END OF ty_hd.

TYPES:BEGIN OF ty_mx,
        ebeln TYPE ekpo-ebeln,  "采购订单
        ebelp TYPE ekpo-ebelp,  "采购明细
        matnr TYPE ekpo-matnr,  "物料号
        txz01 TYPE ekpo-txz01, "物料名称
        menge TYPE ekpo-menge,  "数量
        meins TYPE ekpo-meins,   "单位
        kbetr TYPE konv-kbetr,  "单价
        peinh type ekpo-peinh,  "价格单位
        kwert TYPE konv-kwert, "金额
        waers TYPE konv-waers,  "货币码
      END OF ty_mx.

DATA:pt_hd TYPE TABLE OF ty_hd,
     ps_hd TYPE ty_hd.

DATA:pt_mx TYPE TABLE OF ty_mx,
     ps_mx TYPE ty_mx.

DATA:gt_hd TYPE TABLE OF ty_hd,
     gs_hd TYPE ty_hd.

DATA:gt_mx TYPE TABLE OF ty_mx,
     gs_mx TYPE ty_mx.

DATA: g_menge_hj TYPE ekpo-menge, "数量-总计
      g_kwert_hj TYPE konv-kwert.  "金额-总计


TYPES:BEGIN OF ty_cgdd,
        ebeln TYPE ekko-ebeln,
      END OF ty_cgdd.

TYPES:BEGIN OF ty_lifnr,
        lifnr TYPE ekko-lifnr,
      END OF ty_lifnr.

TYPES:BEGIN OF ty_gsxx,
        bukrs     TYPE t001-bukrs, "公司代码
        butxt     TYPE t001-butxt, "公司名称
        butxt_en  TYPE string, "公司名称_英文
        street_en TYPE string,   "英文地址
      END OF ty_gsxx.

TYPES:BEGIN OF ty_dzh,
        adrnr TYPE ekpo-adrnr,
      END OF ty_dzh.

TYPES:BEGIN OF ty_cgz,
        ekgrp TYPE ekko-ekgrp,
      END OF ty_cgz.

*DATA:gt_zwbcgdd TYPE TABLE OF zwbcgdd,
*     gs_zwbcgdd TYPE zwbcgdd.             "外币订单打印采购信息

DATA:gt_ekgrp TYPE TABLE OF ty_cgz,
     gs_ekgrp TYPE ty_cgz.

DATA:gt_cgz TYPE TABLE OF ty_cgz,
     gs_cgz TYPE ty_cgz.

DATA:gt_dzh TYPE  TABLE OF ty_dzh,
     gs_dzh TYPE ty_dzh.

DATA:gt_gsxx TYPE TABLE OF ty_gsxx,
     gs_gsxx TYPE ty_gsxx.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.

DATA:gt_lifnr TYPE TABLE OF ty_lifnr,
     gs_lifnr TYPE ty_lifnr.

DATA:gt_cgdd TYPE TABLE OF ty_cgdd,
     gs_cgdd TYPE ty_cgdd.

DATA:gt_ekko TYPE TABLE OF ekko,
     gs_ekko TYPE ekko.

DATA:gt_t024 TYPE TABLE OF t024,
     gs_t024 TYPE t024.
*DATA:gt_ekpo type table of ekpo,
*     gs_ekpo type ekpo.
DATA:gt_adrc TYPE TABLE OF adrc,
     gs_adrc TYPE adrc.

DATA:gt_konv TYPE TABLE OF konv,
     gs_konv TYPE konv.

DATA:
  r1(1)         TYPE c,    "采购合同
  r2(1)         TYPE c,    " 委外加工合同
  zycd(30)      TYPE c ,       "原产地
  zycd_en(30)   TYPE c,
  zmdd(30)      TYPE c,         "目的地
  zmdd_en(30)   TYPE c,
  zzzsyd(30)    TYPE c,       "最终使用地
  zzzsyd_en(30) TYPE c,
  zmytj(30)     TYPE c,        "贸易条件
  zmytj_en(30)  TYPE c,
  zjhsj         TYPE d,  "交货时间
  zjhsj_en(30)  TYPE c,
  zpp(30)       TYPE c,  "品牌
  zpp_en(50)    TYPE c.

DATA:g_tree_index TYPE i .

DATA:g_tree_ini TYPE i .


"下拉列表变量定义
DATA:lv_name  TYPE vrm_id,
     lt_list  TYPE vrm_values,
     lw_value LIKE LINE OF lt_list.

*   ----->TREE

CLASS lcl_application DEFINITION DEFERRED.
CLASS cl_gui_cfw DEFINITION LOAD.
TYPES: node_table_type LIKE STANDARD TABLE OF mtreesnode
WITH DEFAULT KEY.
DATA:node_table TYPE node_table_type .
* CAUTION: MTREESNODE IS THE NAME OF THE NODE STRUCTURE WHICH MUST
* BE DEFINED BY THE PROGRAMMER. DO NOT USE MTREESNODE!
DATA: g_application      TYPE REF TO lcl_application,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_tree             TYPE REF TO cl_gui_simple_tree.
DATA: g_event(30),
      g_node_key TYPE tv_nodekey.


***---------------------------------------------------------------
* 定制控制 编辑长文本对象定义
DATA:container TYPE REF TO cl_gui_custom_container,
     editor    TYPE REF TO cl_gui_textedit,
     wa_thead  LIKE thead .  "用于存放save_text表头数据

DATA: w_zbz(100)        TYPE c OCCURS 0,  "备注
      w_zbz_line(100)   TYPE c,
      w_zlbz(100)       TYPE c OCCURS 0,  "质量保证
      w_zlbz_line(100)  TYPE c,
      w_qtyd(10)        TYPE c OCCURS 0,  "其他约定
      w_qtyd_line(100)  TYPE c,
      w_zmydd(100)      TYPE c OCCURS 0,    "贸易地点
      w_zmydd_line(100) TYPE c.

DATA:init.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB01'
CONSTANTS:    BEGIN OF c_tab01,
                tab1 LIKE sy-ucomm VALUE 'TAB01_FC1',
                tab2 LIKE sy-ucomm VALUE 'TAB01_FC2',
              END OF c_tab01.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB01'
CONTROLS:  tab01 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab01,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZMM005A',
             pressed_tab LIKE sy-ucomm VALUE c_tab01-tab1,
           END OF g_tab01.

INCLUDE zinclude_cwb .  "存放处理长文本的代码

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB01'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab01_active_tab_set OUTPUT.
  tab01-activetab = g_tab01-pressed_tab.
  CASE g_tab01-pressed_tab.
    WHEN c_tab01-tab1.
      g_tab01-subscreen = '9002'.
*      IF gt_zwbcgdd  IS NOT INITIAL.
*        PERFORM init_9002.
*      ENDIF.
    WHEN c_tab01-tab2.
      g_tab01-subscreen = '9003'.
      PERFORM  get_pro_item .  "执行树目录的创建
      PERFORM  set_txt.        "设置文本编辑器

    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TAB01'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab01_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab01-tab1.
      g_tab01-pressed_tab = c_tab01-tab1.
    WHEN c_tab01-tab2.
      g_tab01-pressed_tab = c_tab01-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SETZYCD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE setzycd INPUT.
  CLEAR:lt_list,lw_value.
  lv_name = 'ZYCD'.
  CLEAR:lw_value.
  lw_value-key = '台湾'.
  lw_value-text =  '台湾'.
  APPEND lw_value TO lt_list.

  lw_value-key = '香港'.
  lw_value-text =  '香港'.
  APPEND lw_value TO lt_list.

  lw_value-key = '日本'.
  lw_value-text =  '日本'.
  APPEND lw_value TO lt_list.

  lw_value-key = '美国'.
  lw_value-text =  '美国'.
  APPEND lw_value TO lt_list.

  lw_value-key = '韩国'.
  lw_value-text =  '韩国'.
  APPEND lw_value TO lt_list.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.



ENDMODULE.



*&---------------------------------------------------------------------*
*&      Module  SETZMDD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE setzmdd INPUT.
  CLEAR:lt_list,lw_value.
  lv_name = 'ZMDD'.
  CLEAR:lw_value.
  lw_value-key = '北京'.
  lw_value-text =  '北京'.
  APPEND lw_value TO lt_list.

  lw_value-key = '香港'.
  lw_value-text =  '香港'.
  APPEND lw_value TO lt_list.

  lw_value-key = '台湾'.
  lw_value-text =  '台湾'.
  APPEND lw_value TO lt_list.

  lw_value-key = '深圳'.
  lw_value-text =  '深圳'.
  APPEND lw_value TO lt_list.



  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SETZZZSYD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE setzzzsyd INPUT.
  CLEAR:lt_list,lw_value.
  lv_name = 'ZZZSYD'.
  CLEAR:lw_value.
  lw_value-key = '北京'.
  lw_value-text =  '北京'.
  APPEND lw_value TO lt_list.

  lw_value-key = '深圳'.
  lw_value-text =  '深圳'.
  APPEND lw_value TO lt_list.



  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SETZMYTJ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE setzmytj INPUT.
  CLEAR:lt_list,lw_value.
  lv_name = 'ZMYTJ'.
  CLEAR:lw_value.
  lw_value-key = 'DDU'.
  lw_value-text =  'DDU'.
  APPEND lw_value TO lt_list.

  lw_value-key = 'CIF'.
  lw_value-text =  'CIF'.
  APPEND lw_value TO lt_list.

  lw_value-key = 'FOB'.
  lw_value-text =  'FOB'.
  APPEND lw_value TO lt_list.

  lw_value-key = 'CFR'.
  lw_value-text =  'CFR'.
  APPEND lw_value TO lt_list.

  lw_value-key = 'DDP'.
  lw_value-text =  'DDP'.
  APPEND lw_value TO lt_list.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SETZPP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE setzpp INPUT.
  CLEAR:lt_list,lw_value.
  lv_name = 'ZPP'.
  CLEAR:lw_value.
  lw_value-key = '亿光'.
  lw_value-text =  '亿光'.
  APPEND lw_value TO lt_list.

  lw_value-key = '三安'.
  lw_value-text =  '三安'.
  APPEND lw_value TO lt_list.

  lw_value-key = '晶元'.
  lw_value-text =  '晶元'.
  APPEND lw_value TO lt_list.

  lw_value-key = '宏齐'.
  lw_value-text =  '宏齐'.
  APPEND lw_value TO lt_list.



  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  "根据中文选择项默认英文值
  CASE sy-ucomm .
    WHEN 'YCD' .
      PERFORM get_zycd. "查询原产地值。
    WHEN 'MDD'.
      PERFORM get_zmdd. "查询目的的。
    WHEN 'ZZSYD'.
      PERFORM get_zzsyd. "查询最终使用地.

  ENDCASE.

  "若editor 赋予对象后，读取文本框值到各长文本字段
  "0&1:代表备注 2：代表质量保证 3：代表其他约定
  IF  editor IS NOT INITIAL.

    CASE g_tree_index .
      WHEN '0'.
        "读取备注
        CALL METHOD editor->get_text_as_r3table
          IMPORTING
            table = w_zbz.

      WHEN '1'.
        "读取备注
*       wa_thead-tdid = 'ZWBDD_BZ'.      "文本对应ID
*     PERFORM savetext USING editor wa_thead.    wa_thead-tdid = 'ZWBDD_BZ'.      "文本对应ID

        CALL METHOD editor->get_text_as_r3table
          IMPORTING
            table = w_zbz.
      WHEN '2'  .
*       wa_thead-tdid = 'ZWBDD_ZLBZ'.      "文本对应ID
*       PERFORM savetext USING editor wa_thead.
        "读取质量保证
        CALL METHOD editor->get_text_as_r3table
          IMPORTING
            table = w_zlbz.
      WHEN '3'.
        "读取其他约定
*       wa_thead-tdid = 'ZWBDD_QTYD'.      "文本对应ID
*       PERFORM savetext USING editor wa_thead.
        CALL METHOD editor->get_text_as_r3table
          IMPORTING
            table = w_qtyd.
      WHEN '4'.
        "贸易地点
        CALL METHOD editor->get_text_as_r3table
          IMPORTING
            table = w_zmydd.
    ENDCASE.
  ENDIF.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GET_ZYCD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_ZYCD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_zycd .
  CLEAR:zycd_en .
  CASE zycd .
    WHEN '台湾'.
      zycd_en = 'Taiwan'.
    WHEN '香港'.
      zycd_en = 'Hongkong'.
    WHEN '日本'.
      zycd_en = 'Japan'.
    WHEN '美国'.
      zycd_en = 'USA'.
    WHEN '韩国'.
      zycd_en = 'Korea'.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ZMDD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_zmdd .
  CLEAR:zmdd_en .
  CASE zmdd .
    WHEN '北京'.
      zmdd_en = 'Beijing'.
    WHEN '香港'.
      zmdd_en = 'Hongkong'.
    WHEN '台湾'.
      zmdd_en = 'Taiwan'.
    WHEN '深圳'.
      zmdd_en = 'Shenzhen'.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ZZSYD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_zzsyd .
  CLEAR:zzzsyd_en .
  CASE zzzsyd .
    WHEN '北京'.
      zzzsyd_en = 'Beijing'.
    WHEN '深圳'.
      zzzsyd_en = 'Shenzhen'.
  ENDCASE.
ENDFORM.

CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
      FOR EVENT node_double_click
                    OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "LCL_APPLICATION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_node_double_click.
    FIELD-SYMBOLS : <node> TYPE mtreesnode.

    " THIS METHOD HANDLES THE NODE DOUBLE CLICK EVENT OF THE TREE
    " CONTROL INSTANCE
    "初始化读取文本对象
    CLEAR:wa_thead.
    wa_thead-tdobject = 'Z001'.  "文本对象
    " wa_head-tdname =  "采购订单
    wa_thead-tdspras = sy-langu .
    READ TABLE node_table ASSIGNING <node>
    WITH KEY node_key =  node_key .

    IF sy-subrc EQ 0 .
      "读取长文本
      CASE node_key .
        WHEN '          1' .

*          IF w_zbz IS INITIAL.
*            PERFORM  readtext USING editor wa_thead.
*          ELSE.
*          wa_thead-tdid = 'ZWBDD_BZ'.      "文本对应ID
*          PERFORM  readtext USING editor wa_thead.
*
*          "将内表数据赋值给文本编辑器
          CALL METHOD editor->set_text_as_r3table
            EXPORTING
              table = w_zbz.

          g_tree_index = 1.

        WHEN '          2'.
          "质量保证'
*             if w_zlbz is initial.
*                PERFORM  readtext USING editor wa_thead.
*               else.

*          wa_thead-tdid = 'ZWBDD_ZLBZ'.      "文本对应ID
*          PERFORM  readtext USING editor wa_thead.
*

          CALL METHOD editor->set_text_as_r3table
            EXPORTING
              table = w_zlbz.


          "  endif.

          g_tree_index = 2 .

        WHEN '          3'.
          "其他约定
*           if w_qtyd is initial.
*               PERFORM  readtext USING editor wa_thead.
*              else.

*          wa_thead-tdid = 'ZWBDD_QTYD'.      "文本对应ID
*          PERFORM  readtext USING editor wa_thead.
**
          CALL METHOD editor->set_text_as_r3table
            EXPORTING
              table = w_qtyd.

          g_tree_index = 3.
        WHEN '          4'.
          "其他约定
*           if w_qtyd is initial.
*               PERFORM  readtext USING editor wa_thead.
*              else.

*          wa_thead-tdid = 'ZWBDD_QTYD'.      "文本对应ID
*          PERFORM  readtext USING editor wa_thead.
**
          CALL METHOD editor->set_text_as_r3table
            EXPORTING
              table = w_zmydd.

          g_tree_index = 4.
          "   endif.
      ENDCASE.

    ENDIF.

    IF wa_thead IS NOT INITIAL.

    ENDIF.

*    IF NODE_KEY  CS 'ZXZX'.                  "點擊父節點不做處理
*      MESSAGE <NODE>-TEXT TYPE 'I'.
*    ENDIF.


*    READ TABLE IT_TREE INTO IT_TREE1 WITH KEY USERNO = <NODE>-TEXT.
*    IF SY-SUBRC = 0.
*      USERNO =  IT_TREE1-USERNO.
*      "USERNAME =  IT_TREE1-NAME.
*      PASSWORD =  IT_TREE1-PASS.
*      BRANCH = IT_TREE1-BRANCH.
*      UNIT =  IT_TREE1-UNIT.
*      MAIL =  IT_TREE1-MAIL.
*      TDATE =  IT_TREE1-TDATE.
*      SEX =  IT_TREE1-SEX.
*
*      IF IT_TREE1-STATE  = '在职'.
*        NOQUIT = 'X'.
*        QUIT = ''.
*      ELSE.
*        NOQUIT = ''.
*        QUIT = 'X'.
*      ENDIF.
*    ELSE.
*      MESSAGE NODE_KEY TYPE 'I'.
*    ENDIF.
  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK


ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree .
  DATA:  events TYPE cntl_simple_events,
         event  TYPE cntl_simple_event.
* CREATE A CONTAINER FOR THE TREE CONTROL
  CREATE OBJECT g_custom_container
    EXPORTING " THE CONTAINER IS LINKED TO THE CUSTOM CONTROL WITH THE
      " NAME 'TREE_CONTAINER' ON THE DYNPRO
      container_name              = 'P_TREE'          "填充到画面上的容器里
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* CREATE A TREE CONTROL
  CREATE OBJECT g_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_simple_tree=>node_sel_mode_single   "单选模式
    EXCEPTIONS
      lifetime_error              = 1 " SINGLE NODE SELECTIONIS USED
      cntl_system_error           = 2
      create_error                = 3
      failed                      = 4
      illegal_node_selection_mode = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* DEFINE THE EVENTS WHICH WILL BE PASSED TO THE BACKEND
  " NODE DOUBLE CLICK
  event-eventid = cl_gui_simple_tree=>eventid_node_double_click.  "定义双击事件
  event-appl_event = 'X'. " PROCESS PAI IF EVENT OCCURS
  APPEND event TO events.

  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* ASSIGN EVENT HANDLERS IN THE APPLICATION CLASS TO EACH DESIRED EVENT
  CREATE OBJECT g_application.
  SET HANDLER g_application->handle_node_double_click FOR g_tree.   "实现双击


  PERFORM build_node_table USING node_table.    "填充树的节点

  CALL METHOD g_tree->add_nodes
    EXPORTING
      table_structure_name           = 'MTREESNODE'
      node_table                     = node_table
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*設置TREE CONTROL中的節點默認為展開狀態
  CALL METHOD g_tree->expand_node
    EXPORTING
      node_key            = g_node_key
*     LEVEL_COUNT         =
*     EXPAND_SUBTREE      =
    EXCEPTIONS
      failed              = 1
      illegal_level_count = 2
      cntl_system_error   = 3
      node_not_found      = 4
      cannot_expand_leaf  = 5
      OTHERS              = 6.
  IF sy-subrc <> 0."...
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PRO_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pro_item .
  IF g_tree IS INITIAL.
    PERFORM create_and_init_tree. "初始化树节点
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*----------------------------------------------------------------------*
FORM build_node_table  USING   node_table TYPE node_table_type.
  DATA:node TYPE mtreesnode.
  DATA: l_index LIKE sy-tabix.

**設置父節點屬性
  node-node_key = 'ZXZX'.
  g_node_key = node-node_key.
  CLEAR node-relatkey.      " SPECIAL CASE: A ROOT NODE HAS NO PARENT
  CLEAR node-relatship.
  node-hidden = ''.         " THE NODE IS VISIBLE,
  node-disabled = ''.       " SELECTABLE,
  node-isfolder = 'X'.      " A FOLDER.
*  NODE-N_IMAGE = 'X'.
*  NODE-EXP_IMAGE = 'X'.
  node-style = cl_gui_simple_tree=>style_default.
*  NODE-NO_BRANCH = 'X'.
*  NODE-EXPANDER = 'X'.
*  NODE-DRAGDROPID
  node-text = '采购文本'.
  APPEND node TO node_table.
**設置子節點屬性

  CLEAR: node-n_image.
  node-node_key = 1  .
  node-relatkey = 'ZXZX'.
  node-relatship = cl_gui_simple_tree=>relat_last_child.
  node-hidden = ''.        " THE NODE IS VISIBLE,
  node-disabled = ''.
  node-isfolder = ' '. " 'X' 表示可折叠；'':表示不可折叠
  node-text = '备注'.
  APPEND node TO node_table.
  CLEAR: node-n_image.
  node-node_key = 2  .
  node-relatkey = 'ZXZX'.
  node-relatship = cl_gui_simple_tree=>relat_last_child.
  node-hidden = ''.        " THE NODE IS VISIBLE,
  node-disabled = ''.
  node-isfolder = ' '. " 'X' 表示可折叠；'':表示不可折叠
  node-text = '质量保证'.
  APPEND node TO node_table.
  CLEAR: node-n_image.
  node-node_key = 3  .
  node-relatkey = 'ZXZX'.
  node-relatship = cl_gui_simple_tree=>relat_last_child.
  node-hidden = ''.        " THE NODE IS VISIBLE,
  node-disabled = ''.
  node-isfolder = ' '. " 'X' 表示可折叠；'':表示不可折叠
  node-text = '其他约定'.
  APPEND node TO node_table.
  CLEAR: node-n_image.
  node-node_key = 4  .
  node-relatkey = 'ZXZX'.
  node-relatship = cl_gui_simple_tree=>relat_last_child.
  node-hidden = ''.        " THE NODE IS VISIBLE,
  node-disabled = ''.
  node-isfolder = ' '. " 'X' 表示可折叠；'':表示不可折叠
  node-text = '贸易地点'.
  APPEND node TO node_table.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_txt .
  "初始化容器对象
  PERFORM create_container_object USING container 'P_TXT'.
  "初始化文本编辑器
  PERFORM create_editor_object USING editor container .

*
*  READ TABLE gt_cgdd INTO gs_cgdd INDEX 1.
*  IF sy-subrc EQ 0 .
*    CLEAR:wa_thead .
*    wa_thead-tdobject = 'Z001'.  "文本对象
*
*    wa_thead-tdname  =  gs_cgdd-ebeln . "文本对象名称 ：以采购订单号存储
*    wa_thead-tdspras = sy-langu.
*
*  ENDIF.
  "设置内表数据复制到文本框。 g_tree_index为0&1:：备注 2：质量保证 3:其他约定
  CASE g_tree_index .
    WHEN '0'.
      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = w_zbz.
    WHEN '1' .
*      wa_thead-tdid = 'ZWBDD_BZ'.      "文本对应ID
*      PERFORM  readtext USING editor wa_thead.
      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = w_zbz.
    WHEN '2' .
*      wa_thead-tdid = 'ZWBDD_ZLBZ'.      "文本对应ID
*      PERFORM  readtext USING editor wa_thead.
      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = w_zlbz.

    WHEN  '3' .
*      wa_thead-tdid = 'ZWBDD_QTYD'.      "文本对应ID
*      PERFORM  readtext USING editor wa_thead.
      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = w_qtyd.
    WHEN  '4' .
*      wa_thead-tdid = 'ZWBDD_QTYD'.      "文本对应ID
*      PERFORM  readtext USING editor wa_thead.
      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = w_zmydd.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CONTAINER  text
*      -->P_1061   text
*----------------------------------------------------------------------*
FORM create_container_object USING p_container TYPE REF TO cl_gui_custom_container p_container_name.
  IF p_container IS INITIAL.
    CREATE OBJECT p_container   "初始化容器对象
      EXPORTING
        container_name              = p_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
  ENDIF.
ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  CREATE_EDITOR_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EDITOR  text
*      -->P_CONTAINER  text
*----------------------------------------------------------------------*
FORM create_editor_object USING p_editor TYPE REF TO cl_gui_textedit
                                p_container TYPE REF TO cl_gui_custom_container .

  CHECK p_editor IS INITIAL .
  CREATE OBJECT p_editor "初始文本编辑器对象
    EXPORTING
      parent                     = p_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 41  "c_line_length
      wordwrap_to_linebreak_mode = cl_gui_textedit=>false
      max_number_chars           = 100000
    EXCEPTIONS
      OTHERS                     = 1.

ENDFORM . "create_editor_object_cu
*&---------------------------------------------------------------------*
*&      Form  PRT_WB_DD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prt_wb_dd USING i_cgmx TYPE ANY TABLE.

  REFRESH:gt_ekko.
  IF gt_cgdd IS NOT INITIAL.
    "查询采购订单信息
    SELECT * INTO TABLE gt_ekko
      FROM ekko
      FOR ALL ENTRIES IN gt_cgdd
      WHERE ebeln = gt_cgdd-ebeln.
    SORT gt_ekko BY ebeln .

    "查询采购明细
    SELECT * INTO TABLE gt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN gt_cgmx
      WHERE ebeln = gt_cgmx-ebeln
      AND ebelp = gt_cgmx-ebelp.
    SORT gt_ekpo BY ebeln ebelp.

    "供应商信息
    MOVE-CORRESPONDING gt_ekko TO gt_lifnr.
    SORT gt_lifnr BY lifnr .
    DELETE ADJACENT DUPLICATES FROM gt_lifnr COMPARING lifnr.

    "查询供应商信息
    IF gt_lifnr IS NOT INITIAL .
      SELECT * INTO TABLE gt_lfa1
        FROM lfa1
        FOR ALL ENTRIES IN gt_lifnr
        WHERE lifnr = gt_lifnr-lifnr.
      SORT gt_lfa1 BY lifnr .

    ENDIF.

    "公司信息
    MOVE-CORRESPONDING gt_ekko TO gt_gsxx.
    SORT gt_gsxx BY bukrs.
    DELETE ADJACENT DUPLICATES FROM gt_gsxx COMPARING bukrs.

    SELECT * INTO TABLE gt_t001
      FROM t001
      FOR ALL ENTRIES IN gt_ekko
      WHERE bukrs = gt_ekko-bukrs.
    SORT gt_t001 BY bukrs .

    "采购组信息
    MOVE-CORRESPONDING gt_ekko TO gt_ekgrp.
    SORT  gt_ekgrp BY ekgrp.
    DELETE ADJACENT DUPLICATES FROM gt_ekgrp COMPARING ekgrp .

    IF  gt_ekgrp IS NOT INITIAL.
      "查询采购组描述
      SELECT * INTO TABLE gt_t024
      FROM t024
      FOR ALL ENTRIES IN gt_ekgrp
     WHERE ekgrp = gt_ekgrp-ekgrp .
      SORT gt_t024 BY ekgrp .
    ENDIF.


    "公司信息的英文名称、英文地址赋值
    LOOP AT gt_gsxx INTO gs_gsxx.
      READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_gsxx-bukrs
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_gsxx-butxt = gs_t001-butxt.
      ENDIF.
      CASE gs_gsxx-bukrs.
        WHEN '1000'.
          gs_gsxx-butxt_en = 'Leyard Optoelectronic Co., Ltd.'.
          gs_gsxx-street_en  = 'NO.9zhenghongqi west street,North of summerPalace,Haidian District Beijing,100091'.
        WHEN' 1100'.
          gs_gsxx-butxt_en = 'Shenzhen Leyard Opto-Electronic Co Ltd.'.
          gs_gsxx-street_en = 'Block4, Jia An Da Industrial Park, Dalang,Baoan District, Shenzhen, China'.
        WHEN '1300'.
          gs_gsxx-butxt_en = 'Leyard (Hongkong) Co.,Limited'.
          gs_gsxx-street_en =  'No.11 DingYe Street,Five serial Industrial Development Zone,DaXing Area,BeiJing,P.R.C.100091'.
        WHEN '1500'.
          gs_gsxx-butxt_en = 'LEYARD TV TECHNOLOGY   CO.,LTD.'.
          gs_gsxx-street_en = 'No.11 DingYe Street,Five serial Industrial Development Zone,DaXing Area,BeiJing,P.R.C.100091'.
      ENDCASE.
      MODIFY gt_gsxx FROM gs_gsxx.
    ENDLOOP.

    "地址号
    MOVE-CORRESPONDING gt_ekpo TO gt_dzh.
    SORT gt_dzh BY adrnr .
    DELETE gt_dzh WHERE adrnr IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM gt_dzh COMPARING adrnr .

    "查询地址信息
    IF gt_dzh IS NOT INITIAL.
      SELECT * INTO TABLE gt_adrc
        FROM adrc
        FOR ALL ENTRIES IN gt_dzh
        WHERE addrnumber = gt_dzh-adrnr.
      SORT gt_adrc BY addrnumber .
    ENDIF.

    "查询条件信息
    SELECT * INTO TABLE gt_konv
      FROM konv
      FOR ALL ENTRIES IN gt_ekko
      WHERE knumv = gt_ekko-knumv
      AND  (  kschl = 'PB00' OR kschl = 'PBXX'
      ).

    SORT gt_konv BY knumv kposn.
  ENDIF.

  "采购抬头
  REFRESH:gt_hd.
  LOOP AT gt_ekko INTO gs_ekko.
    CLEAR:gs_hd .
    "采购订单
    gs_hd-ebeln = gs_ekko-ebeln.
    "日期
    gs_hd-aedat = gs_ekko-aedat.
    "供应商名称
    gs_hd-lifnr = gs_ekko-lifnr.
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_hd-lifnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_hd-name1 = gs_lfa1-name1.
      "地址
      gs_hd-stras = gs_lfa1-stras.
      "电话
      gs_hd-telf1 = gs_lfa1-telf1.
      "传真
      gs_hd-telfx = gs_lfa1-telfx.

    ENDIF.
    "公司名称
    READ TABLE gt_gsxx INTO gs_gsxx WITH KEY bukrs = gs_ekko-bukrs
                                    BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_hd-butxt = gs_gsxx-butxt.
      gs_hd-butxt_en = gs_gsxx-butxt_en.
      gs_hd-street_en = gs_gsxx-street_en.
    ENDIF.

    "查询公司地址信息
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_ekko-ebeln
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = gs_ekpo-adrnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd-street = gs_adrc-street.
      ENDIF.
    ENDIF.
    "电话——公司代码
    READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_ekko-ekgrp
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_hd-tel_number = gs_t024-tel_number.
      gs_hd-telfx_gs = gs_t024-telfx.

    ENDIF .
    "订单类型
    IF r1 EQ 'X'.
      gs_hd-htlx = '采购合同'.
    ELSE.
      gs_hd-htlx = '委外加工合同'.
    ENDIF.
    "原产地
    gs_hd-zycd = zycd.
    "原产地-英文
    gs_hd-zycd_en = zycd_en.
    "目的地
    gs_hd-zmdd = zmdd.
    "目的地-英文
    gs_hd-zmdd_en = zmdd_en.
    "最终使用地
    gs_hd-zzzsyd = zzzsyd.
    "最终使用地-英文
    gs_hd-zzzsyd_en = zzzsyd_en.
    "贸易条件
    gs_hd-zmytj = zmytj.
    "贸易条件-英文
    gs_hd-zmytj_en = zmytj_en.
    "交货时间:****年**月**日
    CONCATENATE zjhsj+0(4) '年' INTO gs_hd-zjhsj .
    CONCATENATE gs_hd-zjhsj zjhsj+4(2) '月' INTO  gs_hd-zjhsj.
    CONCATENATE  gs_hd-zjhsj zjhsj+6(2) '日' INTO gs_hd-zjhsj.

*    gs_hd-zjhsj = .

    "交货时间-英文
    gs_hd-zjhsj_en = zjhsj_en.
    "品牌
    gs_hd-zpp    = zpp.
    gs_hd-zpp_en = zpp_en.

    "
    "备注信息
    IF w_zbz IS NOT INITIAL.
      LOOP AT w_zbz INTO w_zbz_line .
        CONDENSE w_zbz_line NO-GAPS.
        IF gs_hd-zbz IS INITIAL.
          gs_hd-zbz = w_zbz_line.
        ELSE.
          CONCATENATE gs_hd-zbz w_zbz_line INTO gs_hd-zbz .
        ENDIF.
      ENDLOOP.
    ENDIF.

    " 质量保证
    IF w_zlbz IS NOT INITIAL.
      LOOP AT w_zlbz INTO w_zlbz_line.
        CONDENSE w_zlbz_line NO-GAPS.
        IF gs_hd-zlbz IS INITIAL.
          gs_hd-zlbz = w_zlbz_line.
        ELSE.
          CONCATENATE gs_hd-zlbz w_zlbz_line INTO gs_hd-zlbz.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "其他约定
    IF w_qtyd IS NOT INITIAL.
      LOOP AT w_qtyd INTO w_qtyd_line.
        CONDENSE w_qtyd_line NO-GAPS.
        IF gs_hd-zqtyd IS INITIAL.
          gs_hd-zqtyd =  w_qtyd_line.
        ELSE.
          CONCATENATE gs_hd-zqtyd w_qtyd_line INTO gs_hd-zqtyd.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "贸易地点
    IF w_zmydd IS NOT INITIAL.
      LOOP AT w_zmydd INTO w_zmydd_line.
        CONDENSE w_zmydd_line NO-GAPS.
        IF gs_hd-zmydd IS INITIAL.
          gs_hd-zmydd =  w_zmydd_line .
        ELSE.
          CONCATENATE gs_hd-zmydd w_zmydd_line INTO gs_hd-zmydd.
        ENDIF.

      ENDLOOP.
    ENDIF.

    "货币码
    gs_hd-waers = gs_ekko-waers.
    APPEND gs_hd TO gt_hd.
  ENDLOOP.
  "采购明细
  REFRESH:gt_mx.
  LOOP AT gt_ekpo INTO gs_ekpo.
    CLEAR:gs_mx.
    "采购订单
    gs_mx-ebeln = gs_ekpo-ebeln.
    "采购行项目
    gs_mx-ebelp = gs_ekpo-ebelp.
    "物料号
    gs_mx-matnr = gs_ekpo-matnr.
    "物料描述
    gs_mx-txz01 = gs_ekpo-txz01.
    "数量
    gs_mx-menge = gs_ekpo-menge.
    "单价
    READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = gs_ekpo-ebeln
                                            BINARY SEARCH.
    IF sy-subrc EQ 0 .
      READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_ekko-knumv
                                               kposn = gs_ekpo-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_mx-kbetr = gs_konv-kbetr.
        gs_mx-kwert = gs_konv-kwert.
      ENDIF.

    ENDIF.
    "价格单位
    gs_mx-peinh = gs_ekpo-peinh.
    "订单单位
    gs_mx-meins = gs_ekpo-meins.

    "货币码
    READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = gs_mx-ebeln
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_mx-waers = gs_ekko-waers.
    ENDIF.

    APPEND gs_mx TO gt_mx.

  ENDLOOP.

  SORT gt_mx BY ebeln ebelp.

  DATA:lv_fmname TYPE rs38l_fnam,
       lw_output TYPE ssfcompop.


  control-no_open = 'X'.
  control-no_close = 'X'.
  lw_output-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
      output_options     = lw_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*     ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSFMM005_WBDD'
    IMPORTING
      fm_name            = lv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  REFRESH:pt_mx,pt_hd.
  "根据采购分组打印采购抬头、采购明细
  LOOP AT gt_mx INTO gs_mx.
    AT NEW ebeln.
      CLEAR: g_menge_hj  ,
             g_kwert_hj  .
      REFRESH:pt_mx.
    ENDAT.
    CLEAR:ps_mx.
    g_menge_hj = gs_mx-menge +  g_menge_hj . "以采购为组 数量统计
    g_kwert_hj  = gs_mx-kwert + g_kwert_hj . "以采购为组 金额统计
    MOVE-CORRESPONDING gs_mx TO ps_mx.
    APPEND ps_mx TO pt_mx.
    AT END OF ebeln .
      "表头信息
      REFRESH:pt_hd.
      READ TABLE gt_hd INTO gs_hd WITH KEY ebeln = gs_mx-ebeln
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_hd TO ps_hd.
        ps_hd-menge_hj = g_menge_hj .
        ps_hd-kwert_hj = g_kwert_hj.
        APPEND ps_hd TO pt_hd.
      ENDIF.
      SORT pt_mx BY ebeln ebelp .
      CALL FUNCTION lv_fmname
        EXPORTING
*         ARCHIVE_INDEX      =
*         ARCHIVE_INDEX_TAB  =
*         ARCHIVE_PARAMETERS =
          control_parameters = control
*         MAIL_APPL_OBJ      =
*         MAIL_RECIPIENT     =
*         MAIL_SENDER        =
          output_options     = lw_output
*         USER_SETTINGS      = 'X'
*    IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
*         JOB_OUTPUT_INFO    =
*         JOB_OUTPUT_OPTIONS =
*        TABLES
**          it_hd              = pt_hd
**          it_mx              = pt_mx
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
*        CLEAR:gs_zwbcgdd.
*        gs_zwbcgdd-zebeln = ps_hd-ebeln."采购订单
*        IF r1 EQ 'X'.
*          gs_zwbcgdd-zhtlx = 1. "合同类型
*        ELSE.
*          gs_zwbcgdd-zhtlx = 2 .
*        ENDIF.
*        gs_zwbcgdd-zycd = ps_hd-zycd. "原产地
*        gs_zwbcgdd-zycd_en = ps_hd-zycd_en.
*        gs_zwbcgdd-zmdd = ps_hd-zmdd.
*        gs_zwbcgdd-zmdd_en = ps_hd-zmdd_en.
*        gs_zwbcgdd-zzzsyd  = ps_hd-zzzsyd.
*        gs_zwbcgdd-zzzsyd_en = ps_hd-zzzsyd_en.
*        gs_zwbcgdd-zmytj = ps_hd-zmytj.
*        gs_zwbcgdd-zjhsj = ps_hd-zjhsj.
*        gs_zwbcgdd-zjhsj_en = ps_hd-zjhsj_en.
*        gs_zwbcgdd-zpp = ps_hd-zpp.
*        gs_zwbcgdd-zpp_en = ps_hd-zpp_en.
*        gs_zwbcgdd-zname = sy-uname.
*        gs_zwbcgdd-zdate = sy-datum.
*        gs_zwbcgdd-zuzeit = sy-uzeit .
*        APPEND gs_zwbcgdd TO gt_zwbcgdd.

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
*     error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.



  ENDIF.
*  "保存采购外币抬头信息
*  IF gt_zwbcgdd IS NOT INITIAL.
*    MODIFY zwbcgdd FROM TABLE gt_zwbcgdd .
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  INIT_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_9001 OUTPUT.
  "筛选已选择的采购订单、采购明细
  REFRESH :gt_cgmx,gt_cgdd.
  LOOP AT gt_alv WHERE checkbox EQ 'X'.
    CLEAR:gs_cgmx.
    gs_cgmx-ebeln = gt_alv-ebeln.
    gs_cgmx-ebelp = gt_alv-ebelp.
    APPEND gs_cgmx TO gt_cgmx.
  ENDLOOP.
  SORT gt_cgmx BY ebeln ebelp.

  "查询采购订单
  MOVE-CORRESPONDING gt_cgmx TO gt_cgdd.
  SORT gt_cgdd BY ebeln.
  DELETE ADJACENT DUPLICATES FROM  gt_cgdd COMPARING  ebeln .
*  REFRESH:gt_zwbcgdd.

*  IF gt_cgdd  IS NOT INITIAL.
*    IF r1 EQ 'x'.
*      SELECT * INTO TABLE gt_zwbcgdd
*        FROM zwbcgdd
*        FOR ALL ENTRIES IN gt_cgdd
*        WHERE zebeln = gt_cgdd-ebeln
*        AND   zhtlx = '1'.
*      SORT gt_zwbcgdd BY zebeln .
*
*    ELSE.
*      SELECT * INTO TABLE gt_zwbcgdd
*        FROM zwbcgdd
*        FOR ALL ENTRIES IN gt_cgdd
*        WHERE zebeln = gt_cgdd-ebeln
*        AND   zhtlx = '2'.
*      SORT gt_zwbcgdd BY zebeln .
*    ENDIF.
*
*  ENDIF.
  "
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM init_9002 .
*  LOOP AT gt_zwbcgdd INTO gs_zwbcgdd.
*    zycd = gs_zwbcgdd-zycd.
*    zycd_en = gs_zwbcgdd-zycd_en.
*    zmdd = gs_zwbcgdd-zmdd.
*    zmdd_en = gs_zwbcgdd-zmdd_en.
*    zzzsyd = gs_zwbcgdd-zzzsyd .
*    zzzsyd_en = gs_zwbcgdd-zzzsyd_en.
*    zmytj = gs_zwbcgdd-zmytj.
*    zjhsj = gs_zwbcgdd-zjhsj.
*    zjhsj_en = gs_zwbcgdd-zjhsj_en.
*    zpp = gs_zwbcgdd-zpp.
*  ENDLOOP.
*ENDFORM.
