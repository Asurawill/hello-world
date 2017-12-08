* 4.6B
* MBB038421 240699 Stack-Verwaltung
* 4.5A
* MBA087033 280498 Auflösung KndAuftStl
* 3.0A
* YHG136566 290695 BACK from view
* YHG126159 230595 Umleitung auf Sichtdynpro
* 3.0
* YHG130758 270295 Umstellung Sichtpopups
* YHG101328 041094 CSBOMEX skip first screen
* 2.2
* YHG079407 060594 Anzeigen Zuordnungen
* YHG077712 070494 Popups fuer KlassenVerwTransaktion aktiviert
* YHG077295 050494 MatVerwendung ueber Klassen und Folgeaktionen
* YHG000420 220394 Ueberarbeitung der Einstiegsdynpros zur StlAnz
***********************************************************************
*        M C 2 9 L I F 1           PROGRAMM    S A P M C 2 9 L        *
*                                                                     *
*---------------------------------------------------------------------*
*        P A I - Module            F..                                *
***********************************************************************
*---------------------------------------------------------------------*
*        FCODE_PRUEFEN                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
module fcode_pruefen.
  check crs_feld is initial.
  check ok-code ne 'VLST'.                                  "HGA246532

*del IF OK-CODE EQ 'CSB1'.                                    "YHG000420
*del    PERFORM SAVE_FLAGS.                                   "YHG000420
*del    CALL SCREEN 101 STARTING AT 20 2.                     "YHG000420
*del ENDIF.                                                   "YHG000420

*d IF     SY-DYNNR = 100.                                     "YHG000420
*d    WHILE (    OK-CODE EQ 'CSB1'                            "YHG000420
*d            OR OK-CODE EQ 'CSB2'                            "YHG000420
*d            OR OK-CODE EQ 'CSB3' ).                         "YHG000420
*d
*d       CASE OK-CODE.                                        "YHG000420
*d          WHEN 'CSB1'.                                      "YHG000420
*d             CALL SCREEN 101 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 3.              "YHG000420
*d
*d          WHEN 'CSB2'.                                      "YHG000420
*d             CALL SCREEN 102 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 4.              "YHG079407
*d
*d          WHEN 'CSB3'.                                      "YHG000420
*d             CALL SCREEN 103 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 10.             "YHG000420
*d       ENDCASE.                                             "YHG000420
*d    ENDWHILE.                                               "YHG000420
*d ENDIF.                                                     "YHG000420

  if     sy-dynnr = 100.                                    "YHG130758
    if ok-code = 'CSN1'.                                    "YHG130758
      csn1_dynnr = sy-dynnr.                                "YHG130758

      set screen 105.                                       "YHG130758
      leave screen.                                         "YHG130758
    endif.                                                  "YHG130758
  endif.                                                    "YHG130758

  if    sy-dynnr = 110.                                     "MBA087033
    if ok-code = 'CSN1'.                                    "MBA087033
      csn1_dynnr = sy-dynnr.                                "MBA087033
      set screen 105.                                       "MBA087033
      leave screen.                                         "MBA087033
    endif.                                                  "MBA087033
  endif.                                                    "MBA087033

  if    sy-dynnr = 120.                                      "MBAxx
    if ok-code = 'CSN1'.                                    "MBAxx
      csn1_dynnr = sy-dynnr.                               "MBAxx
      set screen 105.                                      "MBAxx
      leave screen.                                        "MBAxx
    endif.                                                 "MBAxx
  endif.                                                     "MBAxx

  if     sy-dynnr = 200                                     "YHG130758
     or  sy-dynnr = 210                                     "YHG130758
     or  sy-dynnr = 310.                                    "YHG130758

*del  IF OK-CODE = 'CSN1'.                          "YHG130758"YHG126159
    if     ok-code = 'CSN1'                                 "YHG126159
       or  ok-code = 'CSN2'.                                "YHG126159
      csn1_dynnr = sy-dynnr.                                "YHG130758

      set screen 205.                                       "YHG130758
      leave screen.                                         "YHG130758
    endif.                                                  "YHG130758
  endif.                                                    "YHG130758

*del IF     SY-DYNNR = 200.                        "YHG000420 "YHG077712
*d IF     SY-DYNNR = 200                                      "YHG077712
*del  OR  SY-DYNNR = 210.                           "YHG077712"YHG081810
*d    OR  SY-DYNNR = 210                                      "YHG081810
*d    OR  SY-DYNNR = 310.                                     "YHG081810
*d
*d    WHILE (    OK-CODE EQ 'CSB1'                            "YHG000420
*d            OR OK-CODE EQ 'CSB2'                            "YHG000420
*d            OR OK-CODE EQ 'CSB3' ).                         "YHG000420
*d
*d       CASE OK-CODE.                                        "YHG000420
*d          WHEN 'CSB1'.                                      "YHG000420
*d             CALL SCREEN 201 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 3.              "YHG000420
*d          WHEN 'CSB2'.                                      "YHG000420
*d             CALL SCREEN 202 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 3.              "YHG000420
*d          WHEN 'CSB3'.                                      "YHG000420
*d             CALL SCREEN 203 STARTING AT 35 2               "YHG000420
*d                             ENDING   AT 91 3.              "YHG000420
*d       ENDCASE.                                             "YHG000420
*d    ENDWHILE.                                               "YHG000420
*d ENDIF.                                                     "YHG000420

  if     sy-dynnr = 105                                     "YHG130758
     or  sy-dynnr = 205.                                    "YHG130758

    case ok-code.                                           "YHG130758
      when 'ABBR'.                                          "YHG130758
        perform restore_rc29l.                              "YHG130758
*del        CLEAR: SAV_RC29L.                       "YHG130758"YHG136566
        clear: sav_rc29l,                                   "YHG136566
               ok-code.                                     "YHG136566
        set screen csn1_dynnr.                              "YHG130758
        leave screen.                                       "YHG130758
      when 'BACK'.                                          "YHG130758
*del        CLEAR: SAV_RC29L.                       "YHG130758"YHG136566
        clear: sav_rc29l,                                   "YHG136566
               ok-code.                                     "YHG136566
        set screen csn1_dynnr.                              "YHG130758
        leave screen.                                       "YHG130758
      when others.                                          "YHG130758
        clear: sav_rc29l.                                   "YHG130758
    endcase.                                                "YHG130758
  endif.                                                    "YHG130758

  case ok-code.
    when 'CSGO'.
      if r_matnr[] is initial and r_matnr-low is not initial.
        if r_matnr-high is not initial.
          r_matnr-option = 'BT'.
        else.
          r_matnr-option = 'EQ'.
        endif.
        r_matnr-sign = 'I'.
        append r_matnr.
      endif.
      if r_matnr[] is initial.
        message '物料必输！' type 'S' display like 'E'.
        exit.
      endif.
      perform altinput_prf.                                 "YHG000420
      perform fcode_reaktion.

    when 'CSEN'.
      leave to transaction '    '.

    when 'CSDW'.                                            "YHG077295
      call screen 901 starting at 17 1.                     "YHG077295
      clear ok-code.                                        "YHG077295

    when 'MMAT'.
      data: lw_tab_and_field type rstabfield.
      lw_tab_and_field-tablename = 'MARA'.
      lw_tab_and_field-fieldname = 'MATNR'.
      if r_matnr[] is initial and r_matnr-low is not initial.
        if r_matnr-high is not initial.
          r_matnr-option = 'BT'.
        else.
          r_matnr-option = 'EQ'.
        endif.
        r_matnr-sign = 'I'.
        append r_matnr.
      endif.
      call function 'COMPLEX_SELECTIONS_DIALOG'
        exporting
*         TITLE             = '物料'
          text              = '物料'
*         SIGNED            = 'X'
*         LOWER_CASE        = ' '
*         NO_INTERVAL_CHECK = ' '
*         JUST_DISPLAY      = ' '
*         JUST_INCL         = ' '
*         EXCLUDED_OPTIONS  =
*         DESCRIPTION       =
*         HELP_FIELD        =
*         SEARCH_HELP       =
          tab_and_field     = lw_tab_and_field
        tables
          range             = r_matnr[]
        exceptions
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          others            = 5.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.
      read table r_matnr index 1.

    when others.
      clear ok-code.
  endcase.
endmodule.
