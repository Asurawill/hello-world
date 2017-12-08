class ZCL_IM__FAGL_ITEMS_CH_DATA definition
  public
  final
  create public .

public section.

  interfaces IF_EX_FAGL_ITEMS_CH_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM__FAGL_ITEMS_CH_DATA IMPLEMENTATION.


  METHOD if_ex_fagl_items_ch_data~change_items.

    DATA: wa_items TYPE faglposx.
    LOOP AT ct_items INTO wa_items.
      CALL FUNCTION 'GET_GKONT'
        EXPORTING
          belnr           = wa_items-belnr
          bukrs           = wa_items-bukrs
          buzei           = wa_items-buzei
          gjahr           = wa_items-gjahr
          gknkz           = '3'
        IMPORTING
          gkont           = wa_items-gkont
          koart           = wa_items-gkart
        EXCEPTIONS
          belnr_not_found = 1
          buzei_not_found = 2
          gknkz_not_found = 3
          OTHERS          = 4.
      IF sy-subrc = 0.
        MODIFY ct_items FROM wa_items.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
