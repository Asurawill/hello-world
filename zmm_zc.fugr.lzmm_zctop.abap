FUNCTION-POOL ZMM_ZC.                       "MESSAGE-ID ..

DATA G_LFA1 TYPE LFA1.

* INCLUDE LZMM_ZCD...                        " Local class definition
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
*  IMPORT act TO g_aktyp FROM MEMORY ID 'ZZACT'. “ check the exported activity type * and change accordingly the screen *                                             elements.
*    LOOP AT SCREEN.
*        IF g_aktyp = 'A'. " Display
*            screen-input = '0'.
*        ENDIF.
*        MODIFY SCREEN.
*   ENDLOOP.


  LOOP AT SCREEN.
    IF SY-TCODE EQ 'XK03'." Display
      SCREEN-INPUT = '0'.
    ELSEIF SY-TCODE EQ 'XK01' OR SY-TCODE EQ 'XK02'.
      SCREEN-INPUT = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " STATUS_1100  OUTPUT
