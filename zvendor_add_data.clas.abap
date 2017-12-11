class ZVENDOR_ADD_DATA definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZVENDOR_ADD_DATA IMPLEMENTATION.


  METHOD IF_EX_VENDOR_ADD_DATA~CHECK_ADD_ON_ACTIVE.
    " If the screen group is ur custom screen group you have to activate it manually
    IF I_SCREEN_GROUP = 'ZC'. " check the import parameter for the screen group name.
      E_ADD_ON_ACTIVE = 'X'. " Activate it by setting the return parameter
    ENDIF.
  ENDMETHOD.


  method IF_EX_VENDOR_ADD_DATA~CHECK_ALL_DATA.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_PORG_ALTERNATIVE.
  endmethod.
ENDCLASS.
