*----------------------------------------------------------------------*
***INCLUDE ZSET_ZSET_ADD_HEAD_02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZSET_ADD_HEAD_02  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSET_ADD_HEAD_02 OUTPUT.
*DATA ZSD0303 TYPE char30.
*DATA ZSD0301 TYPE char30.
*DATA ZSD0302 TYPE char30.
IF VBAK-ZSD0303  IS NOT INITIAL.
SELECT SINGLE ZSD00202
     FROM ZSD002
     INTO ZSD0303
     WHERE ZSD00201 = VBAK-ZSD0303
             and SPRAS = sy-langu..
ENDIF.


  IF  VBAK-COUNTRY IS NOT INITIAL.
    SELECT SINGLE  landx50
          FROM t005t
          INTO ZSD0301
          WHERE land1 = VBAK-COUNTRY
          and SPRAS = sy-langu.
  ENDIF.

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
