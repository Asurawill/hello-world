*----------------------------------------------------------------------*
***INCLUDE ZSET_GET_COUNTRY_NAME.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_COUNTRY_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_COUNTRY_NAME INPUT.
  DATA ZSD0301 TYPE char30.
  IF  VBAK-COUNTRY IS NOT INITIAL.
    SELECT SINGLE  landx50
          FROM t005t
          INTO ZSD0301
          WHERE land1 = VBAK-COUNTRY.
  ENDIF.
ENDMODULE.
