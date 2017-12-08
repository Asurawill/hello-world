*----------------------------------------------------------------------*
***INCLUDE ZSET_GET_REGION_NAME.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_REGION_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_REGION_NAME INPUT.
  DATA ZSD0302 TYPE char30.
  IF  vbak-COUNTRY IS NOT INITIAL
  AND vbak-REGION IS NOT INITIAL.
    SELECT SINGLE  bezei
                  FROM t005u
                  INTO ZSD0302
      WHERE land1 = vbak-COUNTRY
           AND bland = vbak-REGION
           and SPRAS = sy-langu.
  ENDIF.
ENDMODULE.
