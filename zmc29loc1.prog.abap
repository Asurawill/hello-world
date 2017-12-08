* 4.0C
* MBA087033 030398 Auflösung KndStl
* 3.0A
* YHG126765 240595 Selektion Sicht einschraenken auf Baugruppe
* YHG126159 230595 Umleitung auf Sichtdynpro
* 3.0
* YHG140225 050495 Nachtrag zu 130758 - Statusnamen
* YHG130758 060295 Umstellung 'Sicht' / mehrst. Verwendung
* 2.2
* YHG077515 050494 Klassenverwendung
* YHG077295 050494 MatVerwendung ueber Klassen und Folgeaktionen
* YHG000420 220394 Ueberarbeitung der Einstiegsdynpros zur StlAnz
* YHG000136 020394 Revision Level
* 2.1
* YHG036694 060793 Anzeige und Ueberschriften
***********************************************************************
*        M C 2 9 L O C 1           PROGRAMM    S A P M C 2 9 L        *
*                                                                     *
*---------------------------------------------------------------------*
*        P B O - Module            C..                                *
***********************************************************************
*---------------------------------------------------------------------*
*        CUA_SETZEN                                                   *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
MODULE CUA_SETZEN OUTPUT.
   CHECK OK-CODE NE 'ERR '.

   CASE SY-DYNNR.
      WHEN '0100'.
         CASE SY-TCODE.
            WHEN 'ZCS11'.
*del           SET PF-STATUS 'SA11'.                          "YHG140225
               SET PF-STATUS 'S111'.                          "YHG140225
               SET TITLEBAR 'A01' WITH
*del               'Anzeigen Baukasten' 'mehrstufig'.         "YHG036694
                   TEXT-011.                                  "YHG036694

            WHEN 'ZCS12'.
*del           SET PF-STATUS 'SA12'.                          "YHG140225
               SET PF-STATUS 'S121'.                          "YHG140225
               SET TITLEBAR 'A01' WITH
*del               'Anzeigen Struktur' 'mehrstufig'.          "YHG036694
                   TEXT-012.                                  "YHG036694

            WHEN 'ZCS13'.
               SET PF-STATUS 'S131'.
               SET TITLEBAR 'A01' WITH
*del               'Anzeigen Mengenübersicht' ''.             "YHG036694
                   TEXT-013.                                  "YHG036694
         ENDCASE.

*     Sichtpopups eliminiert                                  "YHG140225
*d    WHEN '0101'.
*d       SET PF-STATUS 'SP01'.
*d       SET TITLEBAR 'P01'.

*d    WHEN '0102'.                                            "YHG000420
*d       SET PF-STATUS 'SP02'.                                "YHG000420
*d       SET TITLEBAR 'P02'.                                  "YHG000420

*d    WHEN '0103'.                                            "YHG000420
*d       SET PF-STATUS 'SP03'.                                "YHG000420
*d       SET TITLEBAR 'P03'.                                  "YHG000420

      WHEN '0105'.                                            "YHG140225
         CASE SY-TCODE.                                       "YHG140225
            WHEN 'CS11'.                                      "YHG140225
               SET PF-STATUS 'S1N2'.                          "YHG140225
               SET TITLEBAR 'A10' WITH                        "YHG140225
                   TEXT-011.                                  "YHG140225

            WHEN 'CS12'.                                      "YHG140225
               SET PF-STATUS 'S1N2'.                          "YHG140225
               SET TITLEBAR 'A10' WITH                        "YHG140225
                   TEXT-012.                                  "YHG140225

            WHEN 'CS13'.                                      "YHG140225
               SET PF-STATUS 'S1N2'.                          "YHG140225
               SET TITLEBAR 'A10' WITH                        "YHG140225
                   TEXT-013.                                  "YHG140225

            WHEN 'CSK1'.                                      "MBA087033
               SET PF-STATUS 'S1K2'.                          "MBA087033
*              $: Sicht                                       "MBA087033
               SET TITLEBAR 'A10' WITH                        "MBA087033
*                  Stücklistenauflösung: Baukasten mehrstufig "MBA087033
                   TEXT-011.                                  "MBA087033

            WHEN 'CSK2'.                                      "MBA087033
               SET PF-STATUS 'S1K2'.                          "MBA087033
*              $: Sicht                                       "MBA087033
               SET TITLEBAR 'A10' WITH                        "MBA087033
*                  Stücklistenauflösung: Struktur mehrstufig  "MBA087033
                   TEXT-012.                                  "MBA087033

            WHEN 'CSK3'.                                      "MBA087033
               SET PF-STATUS 'S1K2'.                          "MBA087033
*              $: Sicht                                       "MBA087033
               SET TITLEBAR 'A10' WITH                        "MBA087033
*                  Stücklistenauflösung: Mengenübersicht      "MBA087033
                   TEXT-013.                                  "MBA087033

         ENDCASE.                                             "YHG140225

      WHEN '0110'.                                            "MBA087033
         CASE SY-TCODE.                                       "MBA087033
            WHEN 'CSK1'.                                      "MBA087033
               SET PF-STATUS 'SK11'.                          "MBA087033
*              $: Einstieg                                    "MBA087033
               SET TITLEBAR 'A01' WITH                        "MBA087033
*                       Stücklistenauflösung: Baukasten mehrstufig
                        TEXT-011.                             "MBA087033
            WHEN 'CSK2'.                                      "MBA087033
               SET PF-STATUS 'SK21'.                          "MBA087033
*              $: Einstieg                                    "MBA087033
               SET TITLEBAR 'A01' WITH                        "MBA087033
*                       Stücklistenauflösung: Struktur mehrstufig
                        TEXT-012.                             "MBA087033
            WHEN 'CSK3'.                                      "MBA087033
               SET PF-STATUS 'SK31'.                          "MBA087033
*              $: Einstieg                                    "MBA087033
               SET TITLEBAR 'A01' WITH                        "MBA087033
*                       Stücklistenauflösung: Mengenübersicht "MBA087033
                        TEXT-013.                             "MBA087033
         ENDCASE.                                             "MBA087033

      WHEN '0120'.                                            "MBAxx
         CASE SY-TCODE.                                       "MBAxx
            WHEN 'CSP1'.                                      "MBAxx
               SET PF-STATUS 'SP11'.                          "MBAxx
*              $: Einstieg                                    "MBAxx
               SET TITLEBAR 'A01' WITH                        "MBAxx
*                       Stücklistenauflösung: Baukasten mehrstufig
                        TEXT-011.                             "MBAxx
            WHEN 'CSP2'.                                      "MBAxx
               SET PF-STATUS 'SP21'.                          "MBAxx
*              $: Einstieg                                    "MBAxx
               SET TITLEBAR 'A01' WITH                        "MBAxx
*                       Stücklistenauflösung: Struktur mehrstufig
                        TEXT-012.                             "MBAxx
            WHEN 'CSP3'.                                      "MBAxx
               SET PF-STATUS 'SP31'.                          "MBAxx
*              $: Einstieg                                    "MBAxx
               SET TITLEBAR 'A01' WITH                        "MBAxx
*                       Stücklistenauflösung: Mengenübersicht "MBAxx
                        TEXT-013.                             "MBAxx
         ENDCASE.                                             "MBAxx
      WHEN '0200'.
*del     SET PF-STATUS 'SA15'.                                "YHG140225
         IF VDAS_FLAG IS INITIAL.                             "YHG126159
            SET PF-STATUS 'S153'.                             "YHG126159
         ELSE.                                                "YHG126159
            SET PF-STATUS 'S151'.                             "YHG140225
         ENDIF.                                               "YHG126159

         SET TITLEBAR 'A01' WITH
*del         'Anzeigen Materialverwendung' 'einstufig'.       "YHG036694
             TEXT-015.                                        "YHG036694

*del  WHEN '0201'.                                  "YHG000420"YHG130758
*del     SET PF-STATUS 'SP01'.                      "YHG000420"YHG130758
*del     SET TITLEBAR 'P01'.                        "YHG000420"YHG130758

*del  WHEN '0202'.                                  "YHG000420"YHG130758
*del     SET PF-STATUS 'SP02'.                      "YHG000420"YHG130758
*del     SET TITLEBAR 'P02'.                        "YHG000420"YHG130758

*del  WHEN '0203'.                                  "YHG000420"YHG130758
*del     SET PF-STATUS 'SP03'.                      "YHG000420"YHG130758
*del     SET TITLEBAR 'P03'.                        "YHG000420"YHG130758

      WHEN '0205'.                                            "YHG130758
         VDAS_FLAG = 'x'.                                     "YHG126159

         CASE SY-TCODE.
            WHEN 'CS15'.
               SET PF-STATUS 'S152'.                          "YHG130758
               SET TITLEBAR 'A10' WITH                        "YHG130758
                   TEXT-015.                                  "YHG130758

            WHEN 'CSD5'.                                      "YHG140225
               SET PF-STATUS 'SD52'.                          "YHG140225
               SET TITLEBAR 'A10' WITH                        "YHG140225
                   TEXT-315.                                  "YHG140225

            WHEN 'CSC5'.                                      "YHG140225
               SET PF-STATUS 'SC52'.                          "YHG140225
               SET TITLEBAR 'A10' WITH                        "YHG140225
                   TEXT-215.                                  "YHG140225
         ENDCASE.                                             "YHG140225

      WHEN '0210'.                                            "YHG077515
*del     SET PF-STATUS 'SC15'.                      "YHG077515
         IF VDAS_FLAG IS INITIAL.                             "YHG126159
            SET PF-STATUS 'SC53'.                             "YHG126159
         ELSE.                                                "YHG126159
            SET PF-STATUS 'SC51'.                             "YHG140225
         ENDIF.                                               "YHG126159

         SET TITLEBAR 'A01' WITH                              "YHG077515
             TEXT-215.                                        "YHG077515

      WHEN '0310'.                                            "YHG081810
*del     SET PF-STATUS 'SD15'.                      "YHG081810"YHG140225
         IF VDAS_FLAG IS INITIAL.                             "YHG126159
            SET PF-STATUS 'SD53'.                             "YHG126159
         ELSE.                                                "YHG126159
            SET PF-STATUS 'SD51'.                             "YHG140225
         ENDIF.                                               "YHG126159

         SET TITLEBAR 'A01' WITH                              "YHG081810
             TEXT-315.                                        "YHG081810

      WHEN '0901'.                                            "YHG077295
         SET PF-STATUS 'SP90'.                                "YHG077295
         SET TITLEBAR 'P90'.                                  "YHG077295
   ENDCASE.
ENDMODULE.


*---------------------------------------------------------------------*
*        CURSOR_SETZEN                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
MODULE CURSOR_SETZEN OUTPUT.
   CHECK NOT CRS_FELD IS INITIAL.
   CLEAR: OK-CODE.

   CASE CRS_FELD.
      WHEN 'STLAN'.
         SET CURSOR FIELD 'RC29L-STLAN'.

      WHEN 'CAPID'.
         SET CURSOR FIELD 'RC29L-CAPID'.

      WHEN 'MATNR'.
         SET CURSOR FIELD 'RC29L-MATNR'.

      WHEN 'STLAL'.
         SET CURSOR FIELD 'RC29L-STLAL'.

      WHEN 'WERKS'.
         SET CURSOR FIELD 'RC29L-WERKS'.

      WHEN 'DATUV'.
         SET CURSOR FIELD 'RC29L-DATUV'.

      WHEN 'AENNR'.                                           "YHG000136
         SET CURSOR FIELD 'RC29L-AENNR'.                      "YHG000136

      WHEN 'REVLV'.                                           "YHG000136
         SET CURSOR FIELD 'RC29L-REVLV'.                      "YHG000136

      WHEN 'TECHV'.
         SET CURSOR FIELD 'RC29L-TECHV'.

      WHEN 'EMENG'.
         SET CURSOR FIELD 'RC29L-EMENG'.

      WHEN 'DIRKT'.
         SET CURSOR FIELD 'RC29L-DIRKT'.

      WHEN 'BAGRP'.                                           "YHG126765
         SET CURSOR FIELD 'RC29L-BAGRP'.                      "YHG126765

      WHEN 'VBELN'.                                           "MBA087033
         SET CURSOR FIELD 'RC29L-VBELN'.                      "MBA087033

      WHEN 'VBPOS'.                                           "MBA087033
         SET CURSOR FIELD 'RC29L-VBPOS'.                      "MBA087033

   ENDCASE.

   CLEAR:
      CRS_FELD.
ENDMODULE.
