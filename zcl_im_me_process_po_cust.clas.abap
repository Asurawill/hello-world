class ZCL_IM_ME_PROCESS_PO_CUST definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_PO_CUST IMPLEMENTATION.


  method IF_EX_ME_PROCESS_PO_CUST~CHECK.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~OPEN.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~POST.

  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.
    " ADD IT02 20160331
    "采购订单行项目点“删除按钮"增强CHECK 对应行项目是否已做过且未删除ZMM002I的调拨单

    IF ( SY-TCODE = 'ME22N' OR SY-TCODE = 'ME23N' )  AND SY-UCOMM = 'YES'.
       DATA:OBJ_HEADER TYPE REF TO IF_PURCHASE_ORDER_MM,
            RE_HEADER TYPE MEPOHEADER,
            RE_ITEM  TYPE MEPOITEM.

       DATA:ERMSG TYPE STRING,
           DBDH_ITEM  TYPE ZMM002I-DBDH.

       CLEAR:obj_header,re_header,re_item,dbdh_item,ermsg.

*       obj_header = im_item->get_header( ).
*       re_header = obj_header->get_data( ).
       re_item = im_item->get_data( ).
       SELECT SINGLE DBDH INTO DBDH_ITEM  FROM ZMM002I WHERE EBELN = RE_ITEM-EBELN
                                                         AND EBELP = RE_ITEM-EBELP
                                                         AND ZDELFLAG = ''.
         IF SY-SUBRC EQ 0.
*               *        clear:re_item-loekz .
*               *     call method im_item->set_data
*               *        exporting
*                *         im_data = re_item.
           CONCATENATE '采购订单' RE_ITEM-EBELN '行号' RE_ITEM-EBELP '已经存在调拨订单号:' DBDH_ITEM  ',请联系相关人员'  INTO ERMSG .
           MESSAGE ERMSG  TYPE 'E'.
        ENDIF.

    ENDIF.

  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.

  endmethod.
ENDCLASS.
