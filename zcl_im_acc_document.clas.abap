class ZCL_IM_ACC_DOCUMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ACC_DOCUMENT IMPLEMENTATION.


  method if_ex_acc_document~change.
    data: wa_extension   type bapiparex,
          ext_value(960) type c,
          wa_accit       type accit,
          l_ref          type ref to data.

    field-symbols: <l_struc> type any,
                   <l_field> type any.

    sort c_extension2 by structure.

    loop at c_extension2 into wa_extension.
      at new structure.
        create data l_ref type (wa_extension-structure).
        assign l_ref->* to <l_struc>.
      endat.
      concatenate wa_extension-valuepart1 wa_extension-valuepart2
                  wa_extension-valuepart3 wa_extension-valuepart4
             into ext_value.
      move ext_value to <l_struc>.
      assign component 'POSNR' of structure <l_struc> to <l_field>.
      if sy-subrc = 0.
        read table c_accit into wa_accit with key posnr = <l_field>.
        if sy-subrc = 0.
          move-corresponding <l_struc> to wa_accit.
          modify c_accit from wa_accit index sy-tabix.
        endif.
      endif.
    endloop.
  endmethod.


  method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
  endmethod.
ENDCLASS.
