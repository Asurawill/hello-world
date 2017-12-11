class ZCL_IM_COOIS definition
  public
  final
  create public .

public section.

  interfaces IF_EX_WORKORDER_INFOSYSTEM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COOIS IMPLEMENTATION.


  method IF_EX_WORKORDER_INFOSYSTEM~AT_OUTPUT_SCREEN.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~AT_OUTPUT_SCREEN_LAY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~AT_SELECTION_SCREEN.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~DETAIL_LIST_LAY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~ORDER_TABLES_MODIFY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~OVERVIEW_MODIFY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~OVERVIEW_TREE_LAY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~PLANNED_ORDER_TABLES_MODIFY.
  endmethod.


  method IF_EX_WORKORDER_INFOSYSTEM~TABLES_MODIFY_LAY.
    "设置计划订单“创建者”字段PLAF-PUSER
    DATA : ST_HEADER TYPE IOHEADER.
    LOOP AT CT_IOHEADER INTO ST_HEADER.
      SELECT SINGLE PUSER FROM PLAF INTO ST_HEADER-PUSER WHERE PLNUM EQ ST_HEADER-AUFNR.
      MODIFY CT_IOHEADER FROM ST_HEADER.
    ENDLOOP.
  endmethod.
ENDCLASS.
