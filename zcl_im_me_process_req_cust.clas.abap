class ZCL_IM_ME_PROCESS_REQ_CUST definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PROCESS_REQ_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_REQ_CUST IMPLEMENTATION.


  method IF_EX_ME_PROCESS_REQ_CUST~CHECK.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~OPEN.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~POST.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_HEADER.
  endmethod.


  METHOD IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ITEM.

    IF SY-TCODE = 'ME51N'.

      DATA LS_DATA  TYPE  MEREQ_ITEM.

      CLEAR LS_DATA.

      LS_DATA = IM_ITEM->GET_DATA( ).

      IF LS_DATA-AFNAM IS INITIAL.
        LS_DATA-AFNAM = SY-UNAME.
      ENDIF.

      IM_ITEM->SET_DATA( LS_DATA ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
