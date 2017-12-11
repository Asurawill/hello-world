class ZVENDOR_ADD_DATA_CS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS ZVENDOR_ADD_DATA_CS IMPLEMENTATION.


  METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_DATA.

    DATA LV_ZSFZH  TYPE ZSFZH.
    CALL FUNCTION 'Z_GET_FIRM_TYPE'
      EXPORTING
        IV_ZSFZH = LV_ZSFZH. " sent here

    S_LFA1-ZSFZH = LV_ZSFZH.
  ENDMETHOD.


  METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_TAXI_SCREEN.
    BREAK HANDABAP.
    IF FLT_VAL = 'ZC'  "Check the import parameter screen group if it is ur custom screen group name
    AND I_TAXI_FCODE = 'ZFIRM'. "And import parameter FCODE for Tab : for custom Tab Function code
      " If these two are true then
      E_SCREEN   = '9000'.   "set the changing parameter sub-screen to custom sub-screen number u created in FG ZMM_ZC
      E_PROGRAM  = 'SAPLZMM_ZC'.  "Function Group (Main program name of the function group ZMM_ZC)
      E_HEADERSCREEN_LAYOUT = ' '."Filter if u want to make it. as of now its global.
    ENDIF.
  ENDMETHOD.


  METHOD IF_EX_VENDOR_ADD_DATA_CS~SET_DATA.

    CALL FUNCTION 'Z_SET_FIRM_TYPE'
      EXPORTING
        IV_ZZFIRM_TYPE = I_LFA1-ZSFZH. " sent here

  ENDMETHOD.
ENDCLASS.
