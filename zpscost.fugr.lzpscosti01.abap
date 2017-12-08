*----------------------------------------------------------------------*
***INCLUDE LZPSCOSTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_PERCENT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_percent INPUT.
  IF ( zpscost-zcsxm2 IS NOT INITIAL
    AND zpscost-zcsxm2 > 100 )
    OR ( zpscost-zxfc2 IS NOT INITIAL
    AND zpscost-zxfc2 > 100 )
    OR ( zpscost-zgf2 IS NOT INITIAL
    AND zpscost-zgf2 > 100 )
    OR ( zpscost-zsj2 IS NOT INITIAL
    AND zpscost-zsj2 > 100 )
    OR ( zpscost-zzlj2 IS NOT INITIAL
    AND zpscost-zzlj2 > 100 )
    OR ( zpscost-zqtfy2 IS NOT INITIAL
    AND zpscost-zqtfy2 > 100 )
    .
    MESSAGE e000(oo) WITH '比例不能大于100'.
  ENDIF.
ENDMODULE.
