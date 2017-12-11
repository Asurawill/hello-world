*----------------------------------------------------------------------*
***INCLUDE LZFGZMM008_1O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_PF_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_pf_status OUTPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CUSTOMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_customer OUTPUT.
  IF status = 'EULG'.
    IF extract+28(12) <> sy-uname.
      LOOP AT SCREEN .
        IF
              screen-name = 'ZMM002_1-MATNR      '
           OR screen-name = 'ZMM002_1-KWMENG     '
           OR screen-name = 'ZMM002_1-WERKS      '
           OR screen-name = 'ZMM002_1-BWART      '
           OR screen-name = 'ZMM002_1-LGORT     '
           OR screen-name = 'ZMM002_1-VGBEL      '
           OR screen-name = 'ZMM002_1-VGPOS      '
*           OR screen-name = 'ZMM002_1-XCHPF      '
           OR screen-name = 'ZMM002_1-CHARG_D    '
           OR screen-name = 'ZMM002_1-VBELN      '
           OR screen-name = 'ZMM002_1-VBDESCRIB  '
           OR screen-name = 'ZMM002_1-ZJWUNAME   '
           OR screen-name = 'ZMM002_1-ZSYUNAME   '
           OR screen-name = 'ZMM002_1-ZRETURNDATE'
           OR screen-name = 'ZMM002_1-REMARK     ' .
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSEIF extract+28(12) = sy-uname.
      LOOP AT SCREEN .
        IF
              screen-name = 'ZMM002_1-MATNR      '
           OR screen-name = 'ZMM002_1-KWMENG     '
           OR screen-name = 'ZMM002_1-WERKS      '
           OR screen-name = 'ZMM002_1-BWART      '
           OR screen-name = 'ZMM002_1-LGORT     '
           OR screen-name = 'ZMM002_1-VGBEL      '
           OR screen-name = 'ZMM002_1-VGPOS      '
*           OR screen-name = 'ZMM002_1-XCHPF      '
           OR screen-name = 'ZMM002_1-CHARG_D    '
           OR screen-name = 'ZMM002_1-VBELN      '
           OR screen-name = 'ZMM002_1-VBDESCRIB  '
           OR screen-name = 'ZMM002_1-ZJWUNAME   '
           OR screen-name = 'ZMM002_1-ZSYUNAME   '
           OR screen-name = 'ZMM002_1-ZRETURNDATE'
           OR screen-name = 'ZMM002_1-REMARK     ' .
          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMODULE.
