*&---------------------------------------------------------------------*
*&  包括                ZPP005_TOP
*&---------------------------------------------------------------------*
TABLES:AFKO,AFPO,TJ02T.

TYPES:BEGIN OF TY_ALV1,
        BOX        TYPE C,
        CHECK      TYPE C,          "检查结果
        AUFNR      TYPE AFKO-AUFNR, " 生产订单
        MATNR_HEAD TYPE AFPO-MATNR, " 物料
        MAKTX_HEAD TYPE MAKT-MAKTX, " 物料描述
        DWERK      TYPE AFPO-DWERK, " 工厂
        DISPO      TYPE AFKO-DISPO, " MRP控制者
        FEVOR      TYPE AFKO-FEVOR, " 生产管理员
        PSMNG      TYPE AFPO-PSMNG, " 订单数量
        WEMNG      TYPE AFPO-WEMNG, " 入库数量
        IGMNG      TYPE AFKO-IGMNG, " 报工数量
        OBJNR      TYPE AUFK-OBJNR, " 对象号
        KTEXT      TYPE AUFK-KTEXT, " " 生产订单描述
        TXT        TYPE T024F-TXT,  " 生产管理员描述
        DAUAT      TYPE AFPO-DAUAT, " 订单类型
        DSNAM      TYPE T024D-DSNAM, "MRP控制者描述
        KDAUF      TYPE AFPO-KDAUF,  "销售订单号
        KDPOS      TYPE AFPO-KDPOS,  "销售订单行号
        FTRMI      TYPE AFKO-FTRMI,  "下达日期
      END OF TY_ALV1.

TYPES:BEGIN OF TY_ALV2,
        CHECK      TYPE C, " 组件检查结果
        AUFNR      TYPE AFKO-AUFNR, " 生产订单
        DWERK      TYPE AFPO-DWERK, " 工厂
        MATNR_HEAD TYPE AFPO-MATNR, " 物料
        MAKTX_HEAD TYPE MAKT-MAKTX, " 物料描述
        PSMNG      TYPE AFPO-PSMNG, " 订单数量
        WEMNG      TYPE AFPO-WEMNG, " 入库数量
        MATNR_ITEM TYPE RESB-MATNR, " 组件
        MAKTX_ITEM TYPE MAKT-MAKTX, " 组件描述
        BDMNG      TYPE RESB-BDMNG, " 需求数量
        ENMNG      TYPE RESB-ENMNG, " 计划消耗
        ERFMG      TYPE RESB-ENMNG, " 计划外消耗
        JHWMNG     TYPE RESB-BDMNG, " 总计消耗
        MEINS      TYPE RESB-MEINS, " 单位
        RKBL       TYPE RESB-BDMNG, " 入库比率需求
        KTEXT      TYPE AUFK-KTEXT, " " 生产订单描述
        ZFLQL      TYPE RESB-BDMNG, " 发料欠料 = 需求数量 - 计划内发料
        LLYLSL     TYPE RESB-BDMNG, "理论用料数量
        ZZXS       TYPE RESB-BDMNG, " 在线数 = 总计消耗 - 入库数量
        XLOEK      TYPE RESB-XLOEK, "删除标记
      END OF TY_ALV2.

DATA:GT_ALV1 TYPE STANDARD TABLE OF TY_ALV1 WITH HEADER LINE.
DATA:GT_ALV2 TYPE STANDARD TABLE OF TY_ALV2 WITH HEADER LINE.
DATA:GT_ALV3 TYPE STANDARD TABLE OF TY_ALV2 WITH HEADER LINE.

*ALV 变量定义
DATA:WCL_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER, " 自适应窗口
     GCL_ALV       TYPE REF TO CL_GUI_ALV_GRID, " ALV类
     GS_LAYOUT     TYPE        LVC_S_LAYO, " ALV样式
     GS_EXCLUDE    TYPE        UI_FUNCTIONS, " ALV排除按钮
     GT_FIELDCAT   TYPE        LVC_T_FCAT, " ALV字段目录
     GT_SORT       TYPE        LVC_T_SORT. " ALV排序

DATA:WCL_CONTAINER2 TYPE REF TO CL_GUI_DOCKING_CONTAINER, " 自适应窗口
     GCL_ALV2       TYPE REF TO CL_GUI_ALV_GRID, " ALV类
     GS_LAYOUT2     TYPE        LVC_S_LAYO, " ALV样式
*     GS_EXCLUDE2 TYPE UI_FUNCTIONS, " ALV排除按钮
     GT_FIELDCAT2   TYPE        LVC_T_FCAT, " ALV字段目录
     GT_SORT2       TYPE        LVC_T_SORT. " ALV排序

DATA:GT_BDCDATA TYPE TABLE OF BDCDATA,
     GW_BDCDATA TYPE          BDCDATA.

DATA:GT_RETURN TYPE BAPIRET2 OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
*       CLASS GCL_EVENT_HANDLER_create DEFINITION
*----------------------------------------------------------------------*
* 处理ALV事件类声明 create
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
*双击事件
      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING
            E_COLUMN
            ES_ROW_NO,
*ALV工具栏增加新按钮
      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING
            E_OBJECT
            E_INTERACTIVE,
*实现用户命令
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING
            E_UCOMM.
ENDCLASS.                    "gcl_event_handler_cu DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD.                    "handle_double_click

  METHOD HANDLE_TOOLBAR.
    PERFORM FRM_HANDLE_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "HANDLE_TOOLBAR


  METHOD HANDLE_USER_COMMAND.
    PERFORM FRM_HANDLE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
S_DWERK FOR   AFPO-DWERK OBLIGATORY NO INTERVALS NO-EXTENSION  MEMORY ID ZPP008, " 工厂
S_AUFNR FOR   AFKO-AUFNR, " 生产订单
S_MATNR FOR   AFPO-MATNR, " 物料
S_DISPO FOR   AFKO-DISPO, " MRP控制者
S_FEVOR FOR   AFKO-FEVOR, " 生产管理员
S_DAUAT FOR   AFPO-DAUAT, " 订单类型
S_ISTAT FOR   TJ02T-ISTAT,"订单状态
S_FTRMI FOR   AFKO-FTRMI. "下达日期
SELECTION-SCREEN END OF BLOCK B1.
