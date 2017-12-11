*&---------------------------------------------------------------------*
*&  包含                LZFGZMM008_2O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CUSTOMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_customer OUTPUT.
  IF status = 'EULG'.

    LOOP AT SCREEN .
      IF
         screen-name    = 'ZMM002_2-MATNR  '
         OR screen-name = 'ZMM002_2-KWMENG '
         OR screen-name = 'ZMM002_2-WERKS  '
         OR screen-name = 'ZMM002_2-UMLGO  '
         OR screen-name = 'ZMM002_2-VBELNTO'
         OR screen-name = 'ZMM002_2-POSNRTO'
         OR screen-name = 'ZMM002_2-LGORT  '
         OR screen-name = 'ZMM002_2-VBELNFR'
         OR screen-name = 'ZMM002_2-POSNRFR'
         OR screen-name = 'ZMM002_2-CHARG  '
         OR screen-name = 'ZMM002_2-ZRUSER'
         OR screen-name = 'ZMM002_2-REMARK ' .
        IF extract+28(12) <> sy-uname.
          screen-input = '0'.
        ELSEIF extract+28(12) = sy-uname.
          screen-input = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'ZMM002_2-ZRUSER' .
      IF extract+121(4) = '3009' OR extract+141(4) = '3009' .
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'ZMM002_2-CHARG'.
      IF extract+138(1) = 'X'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF
        screen-name = 'ZMM002_2-VBELNTO' OR
        screen-name = 'ZMM002_2-POSNRTO'.
      IF extract+52(3) = '231'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-name = 'ZMM002_2-VBELNFR' OR  screen-name = 'ZMM002_2-POSNRFR' .
      IF extract+97(1) = 'E'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.
