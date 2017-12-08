* 4.0C
* MBA089075 120398 Auflösung PrjStl
* HGA084505 060398 neue Gueltigkeit
* MBA087033 030398 Auflösung KndStl
* 3.0E
* HGD072824 170596 Ruecksprung bei call transaction
* 3.0C
* YHG046403 260196 Anpassung an Basisaenderung bzgl. SUBMIT
* 3.0
* YHG140341 060494 Festlegung Vorschlagswerte Standardprofile
* YHG134257 030295 Schalter  Baukasten ein-/mehrstufig
* YHG130758 280295 Umstellung Sichtpopups
* 2.2
* YHG081810 110594 Dokumentverwendung
* YHG079407 060594 Zuordnungen anzeigen
* YHG077515 050494 Klassenverwendung
* YHG000381 240394 erweiterte Materialverwendung / Klassen
* 2.1
* YHG048259 160993 Alternativenbestimmung an Disponeuerungen angepasst
* YHG035807 220693 Feldueberlauf bei div. Berechnungen
* YHG035283 170693 EQUI-Anbindung; Parm. IH-BauGrp
***********************************************************************
*        M C 2 9 L F F 1           PROGRAMM    S A P M C 2 9 L        *
*                                                                     *
*---------------------------------------------------------------------*
*        F O R M - Routinen        F..                                *
***********************************************************************
*---------------------------------------------------------------------*
*        FCODE_REAKTION                                               *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form fcode_reaktion.
  clear: csbomex.
  csbomex-submf = subm_flag.
  csbomex-aclas = cal_aclas.                                "HGD072824
  export csbomex to memory id 'CSNN_BOMEX'.

  case sy-tcode.
    when 'ZCS11'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG140241
          rc29l-dsplp = 'SAPCSLBLMP01      '.               "YHG140241
        endif.                                              "YHG140241

        if rc29l-prntp is initial.                          "YHG140241
          rc29l-prntp = 'SAPCSLBLMP01      '.               "YHG140241
        endif.                                              "YHG140241
      endif.                                                "HGA246532

      submit rcs11001
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
         with pm_techs incl anr_techs                       "HGA084505
         with pm_aennr incl rc29l-aennr                     "HGA084505
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
         with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp                     "YHG140241
         with pm_prprf incl rc29l-prntp                     "YHG140241
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco                     "YHG079407
         with pm_altvo incl rc29l-altvo
         with pm_mehrs incl rc29l-mehrs                     "YHG134257
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz                     "YHG035283
         with pm_stpst incl rc29l-maxst                     "HGD246532
         and return.

    when 'ZCS12'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG140241
          rc29l-dsplp = 'SAPCSMLVMP01      '.               "YHG140241
        endif.                                              "YHG140241

        if rc29l-prntp is initial.                          "YHG140241
          rc29l-prntp = 'SAPCSMLVMP01      '.               "YHG140241
        endif.                                              "YHG140241
      endif.                                                "HGA246532

*         SUBMIT RCS12001
*            WITH PM_MTNRV INCL RC29L-MATNR
*            WITH PM_WERKS INCL RC29L-WERKS
*            WITH PM_STLAL INCL RC29L-STLAL
**           WITH PM_STLAN INCL RC29L-STLAN
*            WITH PM_CAPID INCL RC29L-CAPID
*            WITH PM_DATUV INCL RC29L-DATUV
*            WITH PM_TECHS INCL ANR_TECHS                      "HGA084505
*            WITH PM_AENNR INCL RC29L-AENNR                    "HGA084505
**del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
*            WITH PM_UPSAZ INCL RC29L-UPSAZ
**           solange Parameters nicht mit Nachkommastellen definiert
**           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
**           werden.
**del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
*            WITH PM_EMENG INCL RC29L-EMENG                    "YHG046403
**del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
*            WITH PM_AUSCH INCL RC29L-AUSSS
*            with pm_alvsa incl rc29l-valst                    "HGA246532
*            WITH PM_DSPRF INCL RC29L-DSPLP                    "YHG140241
*            WITH PM_PRPRF INCL RC29L-PRNTP                    "YHG140241
*            WITH PM_LTEXT INCL RC29L-LTEXT
*            WITH PM_DSPCO INCL RC29L-DSPCO                    "YHG079407
*            WITH PM_ALTVO INCL RC29L-ALTVO
*            WITH PM_BREMS INCL RC29L-BREMS
*            WITH PM_BAGRP INCL RC29L-BAGRP
*            WITH PM_POSTP INCL RC29L-POSTP
*            WITH PM_ERSKZ INCL RC29L-ERSKZ
*            WITH PM_ERSSL INCL RC29L-ERSSL
*            WITH PM_BEIKZ INCL RC29L-BEIKZ
*            WITH PM_BESSL INCL RC29L-BESSL
*            WITH PM_SANKO INCL RC29L-SANKO
*            WITH PM_SANFE INCL RC29L-SANFE
*            WITH PM_SANKA INCL RC29L-SANKA
*            WITH PM_SANIN INCL RC29L-SANIN
*            WITH PM_SANVS INCL RC29L-SANVS
*            WITH PM_RVREL INCL RC29L-RVREL
*            WITH PM_SCHGT INCL RC29L-SCHGT
*            WITH PM_STKKZ INCL RC29L-STKKZ                    "YHG035283
*            WITH PM_STPST INCL RC29L-MAXST                    "HGD246532
*            AND RETURN.

      submit zrcs12001
         with s_matnr in r_matnr
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
         with pm_techs incl anr_techs                       "HGA084505
         with pm_aennr incl rc29l-aennr                     "HGA084505
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
         with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp                     "YHG140241
         with pm_prprf incl rc29l-prntp                     "YHG140241
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco                     "YHG079407
         with pm_altvo incl rc29l-altvo
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz                     "YHG035283
         with pm_stpst incl rc29l-maxst                     "HGD246532
         and return.

    when 'ZCS13'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG109837
          rc29l-dsplp = 'SAPCSSMXMP01      '.               "YHG109837
        endif.                                              "YHG109837

        if rc29l-prntp is initial.                          "YHG109837
          rc29l-prntp = 'SAPCSSMXMP01      '.               "YHG109837
        endif.                                              "YHG109837
      endif.                                                "HGA246532

      submit rcs13001
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
         with pm_techs incl anr_techs                       "HGA084505
         with pm_aennr incl rc29l-aennr                     "HGA084505
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp                     "YHG140241
         with pm_prprf incl rc29l-prntp                     "YHG140241
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco                     "YHG079407
         with pm_altvo incl rc29l-altvo
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz                     "YHG035283
         with pm_zstnr incl rc29l-zbgrp
         with pm_stpst incl rc29l-maxst                     "HGD246532
         with pm_sortk incl rc29l-srtkz
         with pm_idnrk incl rc29l-idnrk
         and return.

    when 'CS15'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG109837
          rc29l-dsplp = 'SAPCSWSDMP01      '.               "YHG109837
        endif.                                              "YHG109837

        if rc29l-prntp is initial.                          "YHG109837
          rc29l-prntp = 'SAPCSWSDMP01      '.               "YHG109837
        endif.                                              "YHG109837
      endif.                                                "HGA246532

      if not rc29l-mehrs is initial.                        "YHG130758
        clear:                                              "YHG130758
           rc29l-equtp,                                     "YHG130758
           rc29l-kndtp,                                     "MB075252
           rc29l-prjtp,                                     "MBA089075
           rc29l-stdtp,                                     "YHG130758
           rc29l-tpltp.                                     "YHG130758

        rc29l-mattp = 'X'.                                  "YHG130758
      endif.                                                "YHG130758

      if     rc29l-equtp is initial                         "YHG130758
         and rc29l-kndtp is initial                         "MB075252
         and rc29l-mattp is initial                         "YHG130758
         and rc29l-prjtp is initial                         "MBA089075
         and rc29l-stdtp is initial                         "YHG130758
         and rc29l-tpltp is initial.                        "YHG130758

        rc29l-mattp = 'X'.                                  "YHG130758
      endif.                                                "YHG130758

      submit rcs15001
         with pm_idnrk incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlan incl rc29l-stlan
         with pm_datuv incl rc29l-datuv
         with pm_datub incl rc29l-datub
         with pm_dirkt incl rc29l-dirkt                     "YHG000381
         with pm_uebkl incl rc29l-uebkl                     "YHG000381
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ
         with pm_mehrs incl rc29l-mehrs                     "YHG134257
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HRMNG statt PM_RMENG uebergeben
*           werden.
*del        WITH PM_RMENG INCL RC29L-RMENG                    "YHG035807
         with pm_rmeng incl rc29l-rmeng                     "YHG046403
*del        WITH PM_HRMNG INCL RC29L-RMENG          "YHG035807"YHG046403
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp                     "YHG109837
         with pm_prprf incl rc29l-prntp                     "YHG109837
         with pm_ltext incl rc29l-ltext
         with pm_postp incl rc29l-postp
         with pm_equtp incl rc29l-equtp                     "YHG110068
         with pm_kndtp incl rc29l-kndtp                     "MB075252
         with pm_mattp incl rc29l-mattp                     "YHG110068
         with pm_prjtp incl rc29l-prjtp                     "MBA089075
         with pm_stdtp incl rc29l-stdtp                     "YHG110068
         with pm_tpltp incl rc29l-tpltp                     "YHG110068
         and return.

    when 'CSC5'.                                            "YHG077515
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG140241
          rc29l-dsplp = 'SAPCSWSDCP01      '.               "YHG140241
        endif.                                              "YHG140241

        if rc29l-prntp is initial.                          "YHG140241
          rc29l-prntp = 'SAPCSWSDCP01      '.               "YHG140241
        endif.                                              "YHG140241
      endif.                                                "HGA246532

      submit rcs15021                                       "YHG077515
         with pm_class incl rc29l-class                     "YHG077515
         with pm_klart incl rc29l-klart                     "YHG077515
         with pm_werks incl rc29l-werks                     "YHG077515
         with pm_stlan incl rc29l-stlan                     "YHG077515
         with pm_datuv incl rc29l-datuv                     "YHG077515
         with pm_datub incl rc29l-datub                     "YHG077515
         with pm_dirkt incl rc29l-dirkt                     "YHG077515
         with pm_uebkl incl rc29l-uebkl                     "YHG077515
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ         "YHG077515"YHG140241
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG077515"YHG046403
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HRMNG statt PM_RMENG uebergeben
*           werden.
         with pm_rmeng incl rc29l-rmeng                     "YHG046403
*del        WITH PM_HRMNG INCL RC29L-RMENG          "YHG077515"YHG046403
         with pm_dsprf incl rc29l-dsplp                     "YHG140241
         with pm_prprf incl rc29l-prntp                     "YHG140241
         with pm_ltext incl rc29l-ltext                     "YHG077515
         with pm_postp incl rc29l-postp                     "YHG077515
         with pm_alvsa incl rc29l-valst                     "HGA246532
*d          with pm_mattp incl rc29l-mattp               "MB  "HGA...
*d          with pm_kndtp incl rc29l-kndtp               "MB  "HGA...
         and return.                                        "YHG077515

    when 'CSD5'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "YHG129654
          rc29l-dsplp = 'SAPCSWSDDP01      '.               "YHG129654
        endif.                                              "YHG129654

        if rc29l-prntp is initial.                          "YHG129654
          rc29l-prntp = 'SAPCSWSDDP01      '.               "YHG129654
        endif.                                              "YHG129654
      endif.                                                "HGA246532

      if     rc29l-doctp is initial                         "YHG129654
         and rc29l-equtp is initial                         "YHG129654
         and rc29l-mattp is initial                         "YHG129654
         and rc29l-stdtp is initial                         "YHG129654
         and rc29l-kndtp is initial                         "HGA246532
         and rc29l-prjtp is initial                         "HGA246532
         and rc29l-tpltp is initial.                        "YHG129654

        rc29l-doctp = 'X'.                                  "YHG129654
      endif.                                                "YHG129654

      submit rcs15011
         with pm_doknr incl rc29l-doknr
         with pm_dokar incl rc29l-dokar
         with pm_doktl incl rc29l-doktl
         with pm_dokvr incl rc29l-dokvr
         with pm_werks incl rc29l-werks
         with pm_stlan incl rc29l-stlan
         with pm_datuv incl rc29l-datuv
         with pm_datub incl rc29l-datub
         with pm_dirkt incl rc29l-dirkt
         with pm_uebkl incl rc29l-uebkl
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG129654
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
         with pm_emeng incl rc29l-emeng                     "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG                    "YHG046403
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HRMNG statt PM_RMENG uebergeben
*           werden.
         with pm_rmeng incl rc29l-rmeng                     "YHG046403
*del        WITH PM_HRMNG INCL RC29L-RMENG                    "YHG046403
         with pm_dsprf incl rc29l-dsplp                     "YHG140241
         with pm_prprf incl rc29l-prntp                     "YHG140241
         with pm_ltext incl rc29l-ltext
         with pm_postp incl rc29l-postp
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_doctp incl rc29l-doctp                     "YHG129654
         with pm_equtp incl rc29l-equtp                     "YHG129654
         with pm_mattp incl rc29l-mattp                     "YHG129654
         with pm_stdtp incl rc29l-stdtp                     "YHG129654
         with pm_tpltp incl rc29l-tpltp                     "YHG129654
         with pm_kndtp incl rc29l-kndtp                     "HGA246532
         with pm_prjtp incl rc29l-prjtp                     "HGA246532
         and return.

    when 'CSK1'.                                            "MBA087033
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "MBA087033
          rc29l-dsplp = 'SAPCSLBLMP01      '.               "MBA087033
        endif.                                              "MBA087033

        if rc29l-prntp is initial.                          "MBA087033
          rc29l-prntp = 'SAPCSLBLMP01      '.               "MBA087033
        endif.                                              "MBA087033
      endif.                                                "HGA246532

      submit rcs11001                                       "MBA087033
        with pm_mtnrv incl rc29l-matnr
        with pm_werks incl rc29l-werks
        with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
        with pm_capid incl rc29l-capid
        with pm_datuv incl rc29l-datuv
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
        with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
        with pm_emeng incl rc29l-emeng                      "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
        with pm_ausch incl rc29l-ausss
        with pm_alvsa incl rc29l-valst                      "HGA246532
        with pm_dsprf incl rc29l-dsplp                      "YHG140241
        with pm_prprf incl rc29l-prntp                      "YHG140241
        with pm_ltext incl rc29l-ltext
        with pm_dspco incl rc29l-dspco                      "YHG079407
        with pm_altvo incl rc29l-altvo
        with pm_mehrs incl rc29l-mehrs                      "YHG134257
        with pm_brems incl rc29l-brems
        with pm_bagrp incl rc29l-bagrp
        with pm_postp incl rc29l-postp
        with pm_erskz incl rc29l-erskz
        with pm_erssl incl rc29l-erssl
        with pm_beikz incl rc29l-beikz
        with pm_bessl incl rc29l-bessl
        with pm_sanko incl rc29l-sanko
        with pm_sanfe incl rc29l-sanfe
        with pm_sanka incl rc29l-sanka
        with pm_sanin incl rc29l-sanin
        with pm_sanvs incl rc29l-sanvs
        with pm_rvrel incl rc29l-rvrel
        with pm_schgt incl rc29l-schgt
        with pm_stkkz incl rc29l-stkkz                      "YHG035283
        with pm_stpst incl rc29l-maxst                      "HGD246532
        with pm_vbeln incl rc29l-vbeln                      "MBA087033
        with pm_vbpos incl rc29l-vbpos                      "MBA087033
        and return.

    when 'CSK2'.                                            "MBA087033
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "MBA087033
          rc29l-dsplp = 'SAPCSMLVMP01      '.               "MBA087033
        endif.                                              "MBA087033

        if rc29l-prntp is initial.                          "MBA087033
          rc29l-prntp = 'SAPCSMLVMP01      '.               "MBA087033
        endif.                                              "MBA087033
      endif.                                                "HGA246532

      submit rcs12001                                       "MBA087033
        with pm_mtnrv incl rc29l-matnr
        with pm_werks incl rc29l-werks
        with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
        with pm_capid incl rc29l-capid
        with pm_datuv incl rc29l-datuv
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
        with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
        with pm_emeng incl rc29l-emeng                      "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
        with pm_ausch incl rc29l-ausss
        with pm_alvsa incl rc29l-valst                      "HGA246532
        with pm_dsprf incl rc29l-dsplp                      "YHG140241
        with pm_prprf incl rc29l-prntp                      "YHG140241
        with pm_ltext incl rc29l-ltext
        with pm_dspco incl rc29l-dspco                      "YHG079407
        with pm_altvo incl rc29l-altvo
        with pm_brems incl rc29l-brems
        with pm_bagrp incl rc29l-bagrp
        with pm_postp incl rc29l-postp
        with pm_erskz incl rc29l-erskz
        with pm_erssl incl rc29l-erssl
        with pm_beikz incl rc29l-beikz
        with pm_bessl incl rc29l-bessl
        with pm_sanko incl rc29l-sanko
        with pm_sanfe incl rc29l-sanfe
        with pm_sanka incl rc29l-sanka
        with pm_sanin incl rc29l-sanin
        with pm_sanvs incl rc29l-sanvs
        with pm_rvrel incl rc29l-rvrel
        with pm_schgt incl rc29l-schgt
        with pm_stkkz incl rc29l-stkkz                      "YHG035283
        with pm_stpst incl rc29l-maxst                      "HGD246532
        with pm_vbeln incl rc29l-vbeln                      "MBA087033
        with pm_vbpos incl rc29l-vbpos                      "MBA087033
        and return.

    when 'CSK3'.                                            "MBA087033
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.                          "MBA087033
          rc29l-dsplp = 'SAPCSSMXMP01      '.               "MBA087033
        endif.                                              "MBA087033

        if rc29l-prntp is initial.                          "MBA087033
          rc29l-prntp = 'SAPCSSMXMP01      '.               "MBA087033
        endif.                                              "MBA087033
      endif.                                                "HGA246532

      submit rcs13001                                       "MBA087033
        with pm_mtnrv incl rc29l-matnr
        with pm_werks incl rc29l-werks
        with pm_stlal incl rc29l-stlal
*           WITH PM_STLAN INCL RC29L-STLAN
        with pm_capid incl rc29l-capid
        with pm_datuv incl rc29l-datuv
*del        WITH PM_GBRAZ INCL RC29L-GBRAZ                    "YHG140241
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
*del        WITH PM_EMENG INCL RC29L-EMENG                    "YHG035807
        with pm_emeng incl rc29l-emeng                      "YHG046403
*del        WITH PM_HEMNG INCL RC29L-EMENG          "YHG035807"YHG046403
        with pm_ausch incl rc29l-ausss
        with pm_alvsa incl rc29l-valst                      "HGA246532
        with pm_dsprf incl rc29l-dsplp                      "YHG140241
        with pm_prprf incl rc29l-prntp                      "YHG140241
        with pm_ltext incl rc29l-ltext
        with pm_dspco incl rc29l-dspco                      "YHG079407
        with pm_altvo incl rc29l-altvo
        with pm_brems incl rc29l-brems
        with pm_bagrp incl rc29l-bagrp
        with pm_postp incl rc29l-postp
        with pm_erskz incl rc29l-erskz
        with pm_erssl incl rc29l-erssl
        with pm_beikz incl rc29l-beikz
        with pm_bessl incl rc29l-bessl
        with pm_sanko incl rc29l-sanko
        with pm_sanfe incl rc29l-sanfe
        with pm_sanka incl rc29l-sanka
        with pm_sanin incl rc29l-sanin
        with pm_sanvs incl rc29l-sanvs
        with pm_rvrel incl rc29l-rvrel
        with pm_schgt incl rc29l-schgt
        with pm_stkkz incl rc29l-stkkz                      "YHG035283
        with pm_zstnr incl rc29l-zbgrp
        with pm_stpst incl rc29l-maxst                      "HGD246532
        with pm_sortk incl rc29l-srtkz
        with pm_idnrk incl rc29l-idnrk
        with pm_vbeln incl rc29l-vbeln                      "MBA087033
        with pm_vbpos incl rc29l-vbpos                      "MBA087033
        and return.

    when 'CSP1'.                                            "MBxx
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.
          rc29l-dsplp = 'SAPCSLBLMP01      '.
        endif.

        if rc29l-prntp is initial.
          rc29l-prntp = 'SAPCSLBLMP01      '.
        endif.
      endif.                                                "HGA246532

      submit rcs11001
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
         with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
         with pm_emeng incl rc29l-emeng
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp
         with pm_prprf incl rc29l-prntp
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco
         with pm_altvo incl rc29l-altvo
         with pm_mehrs incl rc29l-mehrs
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz
         with pm_stpst incl rc29l-maxst                     "HGD246532
         with pm_pspnr incl rc29l-pspnr
         and return.

    when 'CSP2'.
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.
          rc29l-dsplp = 'SAPCSMLVMP01      '.
        endif.

        if rc29l-prntp is initial.
          rc29l-prntp = 'SAPCSMLVMP01      '.
        endif.
      endif.                                                "HGA246532

      submit rcs12001
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
         with pm_upsaz incl rc29l-upsaz
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
         with pm_emeng incl rc29l-emeng
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp
         with pm_prprf incl rc29l-prntp
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco
         with pm_altvo incl rc29l-altvo
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz
         with pm_stpst incl rc29l-maxst                     "HGD246532
         with pm_pspnr incl rc29l-pspnr
         and return.

    when 'CSP3'.                                            "MBxx
      if not rc29l-valst is initial.                        "HGA246532
        if rc29l-dsplp is initial.
          rc29l-dsplp = 'SAPCSSMXMP01      '.
        endif.

        if rc29l-prntp is initial.
          rc29l-prntp = 'SAPCSSMXMP01      '.
        endif.
      endif.                                                "HGA246532

      submit rcs13001
         with pm_mtnrv incl rc29l-matnr
         with pm_werks incl rc29l-werks
         with pm_stlal incl rc29l-stlal
         with pm_capid incl rc29l-capid
         with pm_datuv incl rc29l-datuv
*           solange Parameters nicht mit Nachkommastellen definiert
*           werden koennen, muss PM_HEMNG statt PM_EMENG uebergeben
*           werden.
         with pm_emeng incl rc29l-emeng
         with pm_ausch incl rc29l-ausss
         with pm_alvsa incl rc29l-valst                     "HGA246532
         with pm_dsprf incl rc29l-dsplp
         with pm_prprf incl rc29l-prntp
         with pm_ltext incl rc29l-ltext
         with pm_dspco incl rc29l-dspco
         with pm_altvo incl rc29l-altvo
         with pm_brems incl rc29l-brems
         with pm_bagrp incl rc29l-bagrp
         with pm_postp incl rc29l-postp
         with pm_erskz incl rc29l-erskz
         with pm_erssl incl rc29l-erssl
         with pm_beikz incl rc29l-beikz
         with pm_bessl incl rc29l-bessl
         with pm_sanko incl rc29l-sanko
         with pm_sanfe incl rc29l-sanfe
         with pm_sanka incl rc29l-sanka
         with pm_sanin incl rc29l-sanin
         with pm_sanvs incl rc29l-sanvs
         with pm_rvrel incl rc29l-rvrel
         with pm_schgt incl rc29l-schgt
         with pm_stkkz incl rc29l-stkkz
         with pm_zstnr incl rc29l-zbgrp
         with pm_stpst incl rc29l-maxst                     "HGD246532
         with pm_sortk incl rc29l-srtkz
         with pm_stpst incl rc29l-maxst                     "HGD246532
         with pm_idnrk incl rc29l-idnrk
         with pm_pspnr incl rc29l-pspnr
         and return.
  endcase.

  import csbomex from memory id 'CSNN_BOMEX'.

  if not csbomex-retcd is initial.                          "YHG080653
    perform set_msgvars.                                    "YHG080653
  endif.                                                    "YHG080653

  if csbomex-aclas eq 'CC  '.                               "HGD072824
    leave.                                                  "HGD072824
  endif.                                                    "HGD072824

  case csbomex-retcd.
    when 0.
      clear: ok-code.
      if not fsas_flag is initial.                          "YHG101328
        leave.                                              "YHG101328
      endif.                                                "YHG101328

    when 4.
      ok-code = 'ERR '.
      crs_feld = 'MATNR'.
    when 8.
      ok-code = 'ERR '.
      crs_feld = 'WERKS'.
    when 12.
      ok-code = 'ERR '.
      crs_feld = 'MATNR'.
    when 16.
      ok-code = 'ERR '.
      crs_feld = 'DATUV'.
    when 20.
      ok-code = 'ERR '.
      crs_feld = 'TECHV'.
    when 24.
      ok-code = 'ERR '.
      crs_feld = 'MATNR'.
    when 28.
      ok-code = 'ERR '.
      crs_feld = 'MATNR'.
    when 32.
      ok-code = 'ERR '.

      if     rc29l-stlal is initial                         "YHG048259
         and marc-altsl ne '1'.                             "YHG048259
        crs_feld = 'EMENG'.                                 "YHG048259
      else.                                                 "YHG048259
        crs_feld = 'STLAL'.
      endif.                                                "YHG048259

    when 36.
      ok-code = 'ERR '.
      crs_feld = 'MATNR'.
  endcase.
endform.
