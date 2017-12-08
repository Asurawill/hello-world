* 4.6B
* MBB038421 240699 Stack-Verwaltung
* 4.5A
* MBA087033 280498 Auflösung KndStl
* 3.0E
* HGD072824 170596 Ruecksprung bei call transaction
* 3.0D
* HGA072824 150596 Werksberechtigungspruefung
* 3.0A
* YHG126765 240595 Selektion Sicht einschraenken auf Baugruppe
* 2.2
* YHGK081810 110594 Dokumentverwendung
* YHGK077515 060494 Klassenverwendung
* YHGK077295 050494 MatVerwendung ueber Klassen und Folgeaktionen
* YHGK000381 240394 erweiterte Materialverwendung / Klassen
* YHGK000420 220394 Ueberarbeitung der Einstiegsdynpros zur StlAnz
* 2.1
* YHGK049857 071093 K049857 verbessert (hier entfernt)
* YHGK049857 061093 GPA/SPA Vorschlaege
***********************************************************************
*        M C 2 9 L I E 1           PROGRAMM    S A P M C 2 9 L        *
*                                                                     *
*---------------------------------------------------------------------*
*        P A I - Module            E..                                *
***********************************************************************
*---------------------------------------------------------------------*
*        EINGABEN_PRUEFEN                                             *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
MODULE EINGABEN_PRUEFEN.
   IF     OK-CODE EQ 'CSEN'
      OR  OK-CODE EQ 'BACK' AND SY-DYNNR = 100
      OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = 100
      OR  OK-CODE EQ 'BACK' AND SY-DYNNR = '0110'             "MBA087033
      OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = '0110'             "MBA087033
      OR  OK-CODE EQ 'BACK' AND SY-DYNNR = '0120'             "MBAxx
      OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = '0120'             "MBAxx
      OR  OK-CODE EQ 'BACK' AND SY-DYNNR = 200
*del  OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = 200 .             "YHGK077515
      OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = 200               "YHGK077515
      OR  OK-CODE EQ 'BACK' AND SY-DYNNR = 210               "YHGK077515
      OR  OK-CODE EQ 'ABBR' AND SY-DYNNR = 210 .             "YHGK077515

      SET SCREEN 0 .
      LEAVE SCREEN.
   ENDIF.

   IMPORT CSBOMEX FROM MEMORY ID 'CSNN_BOMEX'.                "HGD072824
*  rufende ACLAS sichern
   CAL_ACLAS = CSBOMEX-ACLAS.                                 "HGD072824

   IF FSAS_FLAG IS INITIAL.                                   "YHG101328
*d    IMPORT CSBOMEX FROM MEMORY ID 'CSNN_BOMEX'.   "YHG101328"HGD072824
      IF NOT CSBOMEX-SKFSF IS INITIAL.                        "YHG101328
         OK-CODE = 'CSGO'.                                    "YHG101328
         CLEAR: CSBOMEX.                                      "YHG101328
*        bis auf die rufende ACLAS ...
         CSBOMEX-ACLAS = CAL_ACLAS.                           "HGD072824
*        ... CSBOMEX im Memory loeschen
         EXPORT CSBOMEX TO MEMORY ID 'CSNN_BOMEX'.            "YHG101328
*        trotzdem aber Skip first screen merken
         FSAS_FLAG = 'x'.                                     "YHG101328
      ENDIF.                                                  "YHG101328
   ENDIF.                                                     "YHG101328

*  Werk Berechtigungspruefung                                 "HGA072824
   IF RC29L-WERKS IS INITIAL.
      AUTHORITY-CHECK OBJECT 'C_STUE_WRK'        "HGA072824"#EC CI_SUBRC
*                                                             "ci 042902
         ID 'ACTVT' FIELD DSP_ACTVT                           "HGA072824
         ID 'CSWRK' DUMMY.                                    "HGA072824
   ELSE.
      AUTHORITY-CHECK OBJECT 'C_STUE_WRK'        "HGA072824"#EC CI_SUBRC
*                                                             "ci 042902
         ID 'ACTVT' FIELD DSP_ACTVT                           "HGA072824
         ID 'CSWRK' FIELD RC29L-WERKS.                        "HGA072824
   ENDIF.

   IF SY-SUBRC <> 0.                                          "HGA072824
      OK-CODE = 'ERR'.                                        "HGA072824
      CRS_FELD = 'WERKS'.                                     "HGA072824
      MESSAGE S523 WITH 'E:' RC29L-WERKS.                     "HGA072824
      LEAVE SCREEN.                                           "HGA072824
   ENDIF.                                                     "HGA072824

   PERFORM FIELD_CONVERT USING RC29L-STLAL.
   PERFORM ANWENDUNG_PRF.
   PERFORM GUELTIGKEIT_PRF.
*del PERFORM ALTINPUT_PRF.                        "YHGK050884"YHGK000420
*del PERFORM MENGENINPUT_PRF.                                "YHGK000420
   PERFORM AUF_SELEKTION_PRF.                                 "MBA087033
   PERFORM MAT_SELEKTION_PRF
      USING RC29L-MATNR 'MATNR'.                              "YHG126765
   PERFORM KLA_SELEKTION_PRF.                                "YHGK077515
   PERFORM DOK_SELEKTION_PRF.                                "YHGK081810
*   PERFORM STL_SELEKTION_PRF.
   PERFORM SELMODUS_PRF.                                     "YHGK000381
ENDMODULE.


*---------------------------------------------------------------------*
*        END_OF_POPUP                                                 *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
MODULE END_OF_POPUP.
   IF     OK-CODE EQ 'BACK'
      OR  OK-CODE EQ 'CSB1'                                  "YHGK000420
      OR  OK-CODE EQ 'CSB2'                                  "YHGK000420
      OR  OK-CODE EQ 'CSB3'                                  "YHGK000420
      OR  OK-CODE EQ 'CSGO'.

      CLEAR: SAV_RC29L.                                      "YHGK000420
      SET SCREEN 0 .
      LEAVE SCREEN.
   ENDIF.

   IF     OK-CODE EQ 'CSUB'.                                 "YHGK077295
      CLEAR: SAV_TCSPR.                                      "YHGK077295

      PERFORM KEEP_TCSPR.                                    "YHGK077295

      SET SCREEN 0 .                                         "YHGK077295
      LEAVE SCREEN.                                          "YHGK077295
   ENDIF.                                                    "YHGK077295

   IF     OK-CODE EQ 'ABBR'.
      CASE SY-DYNNR.                                         "YHGK077295
         WHEN '0901'.                                        "YHGK077295
            PERFORM RESTORE_TCSPR.                           "YHGK077295

         WHEN OTHERS.                                        "YHGK077295
            PERFORM RESTORE_RC29L.                           "YHGK000420
      ENDCASE.                                               "YHGK077295

      SET SCREEN 0 .
      LEAVE SCREEN.
   ENDIF.
ENDMODULE.


*---------------------------------------------------------------------*
*        EXIT                                                         *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
MODULE EXIT.
   IF OK-CODE = 'CSDW'.                                      "YHGK077295
      CALL SCREEN 901 STARTING AT 17 1.                      "YHGK077295
      LEAVE SCREEN.                                          "YHGK077295
   ENDIF.                                                    "YHGK077295

   IF     OK-CODE = 'ANMS'
*del  OR  OK-CODE = 'ANMT'.                                   "YHG081810
      OR  OK-CODE = 'ANMT'                                    "YHG081810
      OR  OK-CODE = 'ANCL'                                    "YHG081810
      OR  OK-CODE = 'ANDC'.                                   "YHG081810

      LEAVE TO TRANSACTION SY-TCODE.
   ELSE.
      SET SCREEN 0.
      LEAVE SCREEN.
   ENDIF.
ENDMODULE.


module emeng_in_01.                                         "note 397323
   g_emeng_in_flg = 'x'.                                    "note 397323
endmodule.                                                  "note 397323


module emeng_in_02.                                         "note 397323
   g_emeng_in_flg = ' '.                                    "note 397323
endmodule.                                                  "note 397323
