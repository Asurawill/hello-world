class ZCL_IM_I_SHP_DELIVER_PROC definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_I_SHP_DELIVER_PROC IMPLEMENTATION.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION.

  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION.

  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION.

  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE.
  endmethod.
ENDCLASS.
