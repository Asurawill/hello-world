class ZCL_IM_VENDOR_ADD_DATA_CS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_VENDOR_ADD_DATA_CS IMPLEMENTATION.


  METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_DATA.
    DATA LV_LFA1 TYPE LFA1 .

    CALL FUNCTION 'Z_GET_FIRM_TYPE'
      IMPORTING
        IV_LFA1 = LV_LFA1.

MOVE-CORRESPONDING LV_LFA1 TO S_LFA1.
*    S_LFA1-ZSFZH = LV_ZSFZH.
  ENDMETHOD.


  method IF_EX_VENDOR_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
  endmethod.


  METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_TAXI_SCREEN.
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
        IV_LFA1 = I_LFA1. " sent here
  ENDMETHOD.


  method IF_EX_VENDOR_ADD_DATA_CS~SET_FCODE.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
  endmethod.
ENDCLASS.
