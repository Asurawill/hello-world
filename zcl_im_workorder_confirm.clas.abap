class ZCL_IM_WORKORDER_CONFIRM definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_CONFIRM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_WORKORDER_CONFIRM IMPLEMENTATION.


  method IF_EX_WORKORDER_CONFIRM~AT_CANCEL_CHECK.
  endmethod.


  method IF_EX_WORKORDER_CONFIRM~AT_SAVE.
  endmethod.


  method IF_EX_WORKORDER_CONFIRM~BEFORE_UPDATE.

  endmethod.


  method IF_EX_WORKORDER_CONFIRM~INDIVIDUAL_CAPACITY.
  endmethod.


  method IF_EX_WORKORDER_CONFIRM~IN_UPDATE.
  endmethod.
ENDCLASS.
