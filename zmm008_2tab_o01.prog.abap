
*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_contr_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_zmm002_2 LINES tab_contr-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_CONTR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tab_contr_get_lines OUTPUT.
  g_tab_contr_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CUSTOMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_customer OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'WA_ZMM002_2-ZRUSER' .
      IF zmm002_2-lgort = '3009' OR wa_zmm002_2-umlgo = '3009' .
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'WA_ZMM002_2-CHARG'.
      IF wa_zmm002_2-xchpf = 'X'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF
        screen-name = 'WA_ZMM002_2-VBELNTO' OR
        screen-name = 'WA_ZMM002_2-POSNRTO'.
      IF wa_zmm002_2-bwart = '231'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-name = 'WA_ZMM002_2-VBELNFR' OR
        screen-name = 'WA_ZMM002_2-POSNRFR' .
      IF wa_zmm002_2-spcid = 'E'.
        screen-required = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.
