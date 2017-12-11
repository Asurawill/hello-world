FUNCTION Z_DIGITAL_LOWER_TO_UPPER.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(DIGITALIN) TYPE  C
*"  EXPORTING
*"     REFERENCE(DIGITALOUT) TYPE  C
*"----------------------------------------------------------------------
IF DIGITALIN = '0' .
   DIGITALOUT = '零'.
ELSEIF DIGITALIN = '1'.
     DIGITALOUT = '壹'.
ELSEIF DIGITALIN = '2'.
      DIGITALOUT = '贰'.
ELSEIF DIGITALIN = '3'.
    DIGITALOUT = '叁'.
 ELSEIF DIGITALIN = '4'.
    DIGITALOUT = '肆'.
   elseIF DIGITALIN = '5'.
    DIGITALOUT = '伍'.
    elseIF DIGITALIN = '6'.
    DIGITALOUT = '陆'.
    elseIF DIGITALIN = '7'.
    DIGITALOUT = '柒'.
    elseIF DIGITALIN = '8'.
    DIGITALOUT = '捌'.
    elseIF DIGITALIN = '9'.
    DIGITALOUT = '玖'.
    ELSE.
      DIGITALOUT = 'X'.

      ENDIF.




ENDFUNCTION.
