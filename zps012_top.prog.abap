*&---------------------------------------------------------------------*
*&  包含                ZPS012_TOP
*&---------------------------------------------------------------------*
TABLES: PROJ .
TYPE-POOLS: SLIS,ICON .

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPES:BEGIN OF TY_ALV,
        SEL       TYPE C,
        NUM       TYPE I,
        POSID     TYPE PRPS-POSID, "项目定义或WBS元素外码
        POST1     TYPE PROJ-POST1, "项目定义名称
        MATNR     TYPE MARA-MATNR, "物料号
        MAKTX     TYPE MAKT-MAKTX, "物料描述
        MENGE     TYPE EBKN-MENGE, "预算数量
        MENGE2    TYPE EBKN-MENGE, "已申请数量
        MENGE3    TYPE EBKN-MENGE, "本次申购数
        BUDAT     TYPE SY-DATUM,  "需求日期
        MENGE4    TYPE EBKN-MENGE, "超预算数
        USRNAM    TYPE STRING,    "申请人
        EKGRP     TYPE T024-EKGRP,    "采购组
        TYPPS     TYPE KIS1-TYPPS , "项目类型
        TXT       TYPE STRING,
        BEDNR     TYPE EKPO-BEDNR , " 需求跟踪号
* 返回消息
        TYPE      TYPE C,
        MSG       TYPE STRING,
        LIGHT(10) TYPE C , "灯
      END OF TY_ALV .
DATA:GT_ALV TYPE TABLE OF TY_ALV,
     GS_ALV TYPE TY_ALV.
FIELD-SYMBOLS <FS_ALV> TYPE TY_ALV .

DATA:GS_LAYOUT        TYPE LVC_S_LAYO ,      "alv的格式
     GS_GRID_SETTINGS TYPE LVC_S_GLAY,
     GS_LVC           TYPE LVC_S_FCAT,
     GT_LVC           TYPE LVC_T_FCAT,
     GT_SORT          TYPE LVC_T_SORT,
     GS_VARIANT       TYPE DISVARIANT,
     GT_EVENTS        TYPE SLIS_T_EVENT  , "保存AVL事件
     GS_EVENT         LIKE LINE OF GT_EVENTS.
DATA:
  GR_GRID TYPE REF TO CL_GUI_ALV_GRID,
  GS_STBL TYPE LVC_S_STBL.

DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  GS_LVC-fieldname = &1.
  GS_LVC-coltext   = &2.
  GS_LVC-scrtext_l = &2.
  GS_LVC-scrtext_m = &2.
  GS_LVC-scrtext_s = &2.
  GS_LVC-reptext   = &2.
  GS_LVC-outputlen = &3.
  IF &4 = 'X'.
    GS_LVC-key = 'X'.
  ENDIF.
  GS_LVC-checkbox = &5.
  GS_LVC-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GS_LVC-hotspot   = &7.
  GS_LVC-ref_field = &9.
  GS_LVC-ref_table = &8.

*IF gw_lvc-fieldname = 'PROJK'.
*   gw_lvc-NO_ZERO = 'X'.
*ENDIF.
  APPEND GS_LVC TO gt_lvc.
  CLEAR GS_LVC.
END-OF-DEFINITION.

************************************************************************
*      CLASS DEFINITION
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION .
  PUBLIC SECTION .
    METHODS:
      HANDLE_MODIFY FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS .
*    METHODS:
*      ZUSER_COMMAND FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
*        IMPORTING E_UCOMM
*                    E_SAVED
*                    E_NOT_PROCESSED .
ENDCLASS .

************************************************************************
*      CLASS IMPLEMENTATION
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION .
  METHOD HANDLE_MODIFY .
*    BREAK-POINT .
    IF SY-UCOMM IS INITIAL OR SY-UCOMM EQ '' .

      LOOP AT GT_ALV ASSIGNING <FS_ALV>  .
        <FS_ALV>-MENGE4 = <FS_ALV>-MENGE - <FS_ALV>-MENGE2 - <FS_ALV>-MENGE3 .
        IF <FS_ALV>-MENGE4 >= 0 .
          CLEAR <FS_ALV>-MENGE4 .
        ELSEIF <FS_ALV>-MENGE4 < 0 .
          <FS_ALV>-MENGE4 = ABS( <FS_ALV>-MENGE4 ).
        ENDIF.
      ENDLOOP.

      GS_STBL-ROW = 'X'." 基于行的稳定刷新
      GS_STBL-COL = 'X'." 基于列稳定刷新
      CALL METHOD GR_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = GS_STBL.

    ENDIF.
*  Inform ALV Grid that event 'onf4' has been processed
*    ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
  ENDMETHOD .
*
*  METHOD ZUSER_COMMAND .
*    BREAK-POINT .
*    LOOP AT GT_ALV ASSIGNING <FS_ALV>  .
*      <FS_ALV>-MENGE4 = <FS_ALV>-MENGE - <FS_ALV>-MENGE2 - <FS_ALV>-MENGE3 .
*      IF <FS_ALV>-MENGE4 >= 0 .
*        CLEAR <FS_ALV>-MENGE4 .
*      ELSEIF <FS_ALV>-MENGE4 < 0 .
*        <FS_ALV>-MENGE4 = ABS( <FS_ALV>-MENGE4 ).
*      ENDIF.
*    ENDLOOP.
*
*    GS_STBL-ROW = 'X'." 基于行的稳定刷新
*    GS_STBL-COL = 'X'." 基于列稳定刷新
*    CALL METHOD GR_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = GS_STBL.
**  Inform ALV Grid that event 'onf4' has been processed
**    ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
*  ENDMETHOD .
ENDCLASS .
