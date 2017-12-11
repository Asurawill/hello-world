*eject
* 4.7
* HGA095729 230102 keine Grafik unter webgui
* 4.6C
* HGB020150 260100 Reserve beim Druck (Var.List)
* HGA020150 240100 UComm  anderes Material (ALV)
* HGD246532 171199 Stoppstufe f. Aufl.
* HGA246532 081199 ALV_S
* 4.6B
* HGA046836 270799 Aufl. ProjektStl
* 4.6A
* HGA154419 231298 ECN - FB Anpassung
* 4.5B
* HGC072185 180199 Anzeige KndSelDaten
* HGD059252 161198 Conversion-Error entschaerft
* HGC059252 121198 Nachtrag A033687
* HGA059252 121198 ParmEff-Popup abh. vom Stl.-TopMat (auch bei KndStl)
* 4.0C/4.5A
* HGA033687 140798 KndStl-Aufloesen - Kein Ausweichen auf MatStl mehr
* HGA029649 090798 Langtextausgabe (KndStl)
* HGA084505 060398 neue Gueltigkeit
* HGB084505 030398 KND-Stueckliste
* 4.0A
* HGA025100 230797 Profilvorbelegung / VList Performance
* 3.1G
* HGA127128 270197 Langtextkennz. / Druck SelKriterien
* 3.0F
* HGA108310 251096 EinsatzwahrscheinlichkeitsHandling
* HGA105944 161096 MatNr-Aufbereitung bei ZwBauGrp-Info
* HGG099459 180996 TCSPR-Default
* HGB099459 010896 alter Materialobjekttyp
* 3.0E
* HGA080825 030796 Vervollst. HG0D51356; x-stell. MatNum
* HGC072824 150596 CATT; Druck im Batch ausschalten
* HGA072824 150596 Werksberechtigungspruefung
* 3.0D
* HGB070841 260496 keine Memoryverwaltung fuer CA
* HGB057558 210396 Langtext Includes auflösen
* 3.0C
* YHG046403 260196 Anpassung an Basisaenderung bzgl. SUBMIT
* YHG032486 041295 Performance/Konvertierungen, Coding
* 3.0B
* YHG020009 231095 Performance
*                  CSIO Memory / Puffer Aktivierung / APPEND
* 3.0A
* YHG137563 270695 Druck; falsche Positionierung im Listenkopf
* YHG132854 130695 Keine Grafik bei nicht zus.haengender StlStruktur
* YHG127645 120695 Erweiterung und Korr. zu var. Listen
* YHG123656 160595 Verhalten bei CALLs korrigiert
* YHG121750 100595 Nichtlager ohne Materialstamm
* 3.0
* YHG140031 040495 SKIP bei var. Liste im Druck korrigiert
* YHG139715 030495 Selectoptions-Anzeige
* YHG139336 300395 Ende der Liste
* YHG139320 300395 Kennz Wissen zugeordnet
* YHG138467 270395 Oberfläche / Anpassung Statusname
* YHG137424 230395 Defaultprofil Absicherung
* YHG134257 020395 Korr. CAD-Versorgung
* YHG133914 020395 Umstellung auf var. Liste und neue FB-Generation
* YHG101965 041094 Schalter Mengenrunden
* YHG100916 150994 Korr. Vgl. mit expliziter Laenge
* 2.2                                                            < 1995
* Coding bereinigt: 000109 / 000301 / 077295 / 078090 / 079407
*                   079319 / 087863 / 88020
* 2.1D                                                           < 1995
* Coding bereinigt: 66003 / 68719 / 72697
* 2.1C                                                           < 1994
* Coding bereinigt: 44318
* 2.1B                                                           < 1994
* Coding bereinigt: 35283 / 35785 / 35807 / 36365 / 36694 / 41147
*                   42334 / 44860 / 49857 / 52824 / 53243 / 62728
***********************************************************************
*del REPORT RCS12001 MESSAGE-ID 29 LINE-SIZE 81               "YHG139715
report rcs12001 message-id 29 line-size 132                 "YHG139715
                no standard page heading.

***********************************************************************
*        D A T E N  -  Definitionen                                   *
***********************************************************************
*---------------------------------------------------------------------*
*        ATAB-Tabellen                                                *
*---------------------------------------------------------------------*
tables:
  makt,
  t418,                                                     "YHG121750
  tcs03,
  tcspr.


*---------------------------------------------------------------------*
*        interne Tabellen                                             *
*---------------------------------------------------------------------*
data: begin of selpool.
        include structure cstmat.
data: end of selpool.

data: begin of stb occurs 1000.
        include structure stpox.
data: end of stb.

*     Materialkatalog
data: begin of matcat occurs 50.                            "YHG133914
        include structure cscmat.                           "YHG133914
data: end of matcat.                                        "YHG133914

data: begin of sav_stb.
        include structure stpox.
data: end of sav_stb.

data: begin of txt occurs 25.
        include structure tline.
data: end of txt.

data: begin of cl_clstab occurs 0,
        class like klah-class,
        klart like klah-klart,
        chked like csdata-xfeld,
        noobj like csdata-xfeld,
        dsply like csdata-xfeld,
      end of cl_clstab.

data: begin of cl_objtab occurs 0.
        include structure ckssk.
data: end of cl_objtab.

data: begin of cl_objmemo occurs 0,
        class like klah-class,
        klart like klah-klart.
        include structure ckssk.
data: end of cl_objmemo.

*     WAs fuer AnzBlockAusgabe (var. Liste)
data: begin of watab occurs 0.                              "YHG133914
        include structure cltable.                          "YHG133914
data: end of watab.                                         "YHG133914

*     Sicherungstabelle der AnzBlockWAs
data: begin of sav_watab occurs 0.                          "YHG133914
        include structure cltable.                          "YHG133914
data: end of sav_watab.                                     "YHG133914

*     Uebergabestruktur Typ STPOX (fuer STB)
data: begin of stb_orig.                                    "YHG133914
        include structure stpox.                            "YHG133914
data: end of stb_orig.                                      "YHG133914

*     Uebergabestruktur Typ STPOL_ADD
data: begin of stb_add.                                     "YHG133914
        include structure stpol_add.                        "YHG133914
data: end of stb_add.                                       "YHG133914

data : begin of vbap_wa occurs 0.                           "HGB084505
        include structure vbap.                             "HGB084505
data : end of vbap_wa.                                      "HGB084505

data: matpos_flg like csdata-xfeld,                         "HGB084505
      mpos_page  like sy-pagno,                             "HGB084505
      mpos_line  like sy-linno.                             "HGB084505

data: begin of wa_kdstb occurs 0.                           "HGA033687
        include structure kdstb.                            "HGA033687
data: end of wa_kdstb.                                      "HGA033687

data lv_config_status type abap_bool.

data: it_stb    like stb[],
      it_matcat like matcat[].
*---------------------------------------------------------------------*
*        interne Feldleisten                                          *
*---------------------------------------------------------------------*
*del DATA:                                                    "YHG133914
*del    BEGIN OF INFO_LINIE,                                  "YHG133914
*del       MATNR LIKE MARA-MATNR,                             "YHG133914
*del       ABSTD(1)   TYPE C,                                 "YHG133914
*del       STRICH(18) TYPE C,                                 "YHG133914
*del    END OF INFO_LINIE.                                    "YHG133914

*---------------------------------------------------------------------*
*        interne Felder                                               *
*---------------------------------------------------------------------*
data:
  cal_aclas     like tappl-applclass,
  hlf_stlnr     like stko-stlnr,
  hlp_stlal     like stko-stlal,
  clo_matnr     like mara-matnr,
  anz_stptx(3)  type c,
  anz_stufe(11) type c,
  anz_datuv     like stko-datuv,
  anz_datub     like stko-datuv,
  anz_stlal     like stko-stlal,
  abw_stlal     like stko-stlal,
  anz_upmng     like stpu-upmng,
  txt_key       like thead-tdname,
  txt_kyln(3)   type n,
  col_switch(1) type c,          "intensified on/off
  tmp_hrechf    type f,          "temp. HilfsRechenfeld
  ups_faktor    type f,
  ret_code      like sy-subrc.

data: anr_datuv like aenr-datuv,                            "HGA084505
      anr_techs like tecs-techs.                            "HGA084505

* ---------------------------------
data: lins_out     like sy-linno,
      lins_out_sav like sy-linno,
      lst_lin_on_p like sy-linno,
      lins_to_skip like sy-linno,
      lins_per_pag like sy-pagno.
* ---------------------------------
*     Ueberlaufschalter (f. Zahlen)
data: ueberl_flg(1) type c,
*del  EDTLIN(79) TYPE C,                                      "YHG133914
*del  SAV_EDTLIN(79) TYPE C,                                  "YHG133914
      first_flg(1)  type c.

* ---------------------------------
data: page_mat like sy-pagno,
      page_dok like sy-pagno,
      page_kla like sy-pagno,
      page_noo like sy-pagno,
      page_exc like sy-pagno.
* ---------------------------------
*     --> kann nach Bereinigung der anderen Reports geloescht werden
*d DATA: REAL_LINE LIKE SY-LINNO.                             "HGA046836
*d DATA: STB-MATNR LIKE MARA-MATNR.                 "YHG133681"HGA046836


* ---------------------------------
*     EndOfBlock-Kennzeichen
data: eoblc      like csdata-xfeld,                         "YHG133914
*     Zeilenbreite aus dem aktuellen Profil
      itf_prfsz  like klah-lbrei,                   "YHG133914"YHG32486
*del  SAV_PRFSZ  LIKE KLAH-LBREI,                   "YHG133914"YHG32486
      sav_prfsz  type i,                                    "YHG32486
*     Zeilenbreite des akt. Profils plus Rand ( + 2 )
*del  SIZ_LINPF  LIKE KLAH-LBREI,  "Lnsz plus frame "YHG133914"YHG32486
      siz_linpf  type i,                                    "YHG32486
*     Kennzeichen 'akt. Zeile ist leer'
      lnmpt_flg  like csdata-xfeld,                         "YHG133914
*     das aktuell gueltige Profil
      act_profil like klah-class,                           "YHG133914
*     BlockzeilenZaehler
      blclns_cnt like sy-linno,                             "YHG133914
*     Anzahl Zeilen Listenkopf
      nbr_hdrlns like sy-linno.                             "YHG133914

data: dstst_flg like csdata-xfeld.                          "YHG132854

data: mem_mngmt(1) type c.                                  "HGB070841

data: mnr_lng     type i.                                   "HGA080825


*---------------------------------------------------------------------*
*        Konstanten                                                   *
*---------------------------------------------------------------------*
data:
  bom_txobj  like thead-tdobject value 'BOM',
*d bom_txid   like thead-tdid value 'MPO',                    "HGA029649
  bom_txid   like thead-tdid,                               "HGA029649
  mbm_txid   like thead-tdid value 'MPO',                   "HGA029649
  kbm_txid   like thead-tdid value 'KPO',                   "HGA029649
*  maximal anzeigbare Menge
  max_num(7) type p decimals 3 value '9999999999.999',
  min_num(7) type p decimals 3 value '9999999999.999-',
  cnull(18)  type c          value '000000000000000000',
  strich(50) type c          value
     '--------------------------------------------------',
  min_grg    like sy-datum   value '19000101',
  max_grg    like sy-datum   value '99991231'.

data: ueberl_kz(1) type c value '*'.
data: b_flag(1) type c value 'X'.

data:
  otyp_mat(1)  type c value '1',
  ootyp_mat(1) type c value 'M',                            "HGB099459
*  Objekttyp 'kein Objekt'
  otyp_noo(1)  type c value '2',                            "YHG133914
  otyp_doc(1)  type c value '3',
  otyp_kla(1)  type c value '4',
*  Objekttyp 'Intramaterial'
  otyp_ntm(1)  type c value '5'.                            "YHG133914

data: all_pstat(9) type c value 'KVEDLAPSB'.


* ---------------------------------
*     langes leeres Feld
data: ecfld(250) type c.                                    "YHG133914


*---------------------------------------------------------------------*
*        spezielle Export- Importbereiche                             *
*---------------------------------------------------------------------*
data: begin of stpub occurs 0.
        include structure stpub.
data: end of stpub.

data: begin of csbomex.
        include structure csbomex.
data: end of csbomex.


*---------------------------------------------------------------------*
*        Selektionsparameter                                          *
*---------------------------------------------------------------------*
select-options:
  s_matnr for makt-matnr memory id mat no intervals.
parameters:
*d pm_mtnrv like mara-matnr memory id mat obligator           "HGC059252
  pm_mtnrv     like mara-matnr memory id mat no-display,    "HGC059252
  pm_werks     like marc-werks memory id wrk,
  pm_stlal     like stko-stlal,
  pm_stlan     like stzu-stlan,
  pm_capid     like tc04-capid,
  pm_datuv     like stko-datuv default sy-datum obligatory,
*  Parameterbelegung P-Effektivity
  pm_techs     like tecs-techs,                             "HGA084505
*  Aenderungsnummer
  pm_aennr     like aenr-aennr,                             "HGA084505
*  Einsatzmenge
  pm_emeng     like stko-bmeng,
*  zugehoeriges SUBMIT-Hilfsfeld:
  pm_hemng(13) type n no-display,
  pm_alvsa     like rc29l-valst,                            "HGA246532
*  Profil zur Bildschirmanzeige
*d pm_dsprf      like klah-class default 'SAPCSMLVM "YHG133681"HGA025100
  pm_dsprf     like klah-class,                             "HGA025100
*  Profil beim Druck
*d pm_prprf      like klah-class default 'SAPCSMLVM "YHG133681"HGA025100
  pm_prprf     like klah-class,                             "HGA025100
  pm_ausch(1)  type c,
  pm_ltext(1)  type c,
*  (Dok.)Revisionsstand ermitteln
  pm_drldt(1)  type c,                                    "note 438371
  pm_dspco(1)  type c,
*  Stoppstufe
  pm_stpst     like rc29l-maxst,                            "HGD246532
  pm_altvo(1)  type c,
  pm_upsaz(1)  type c,
*d PM_GBRAZ(1)   TYPE C,                                      "HGA046836
  pm_brems(1)  type c,
  pm_erskz(1)  type c,
  pm_erssl(1)  type c,
  pm_beikz(1)  type c,
  pm_bessl(1)  type c,
  pm_bagrp     like mara-matnr,
  pm_postp(1)  type c,
  pm_ehndl(1)  type c default '1',                          "HGA108310
  pm_sanko     like stpo-sanko,
  pm_sanfe     like stpo-sanfe,
  pm_sanka     like stpo-sanka,
  pm_sanin     like stpo-sanin,
  pm_sanvs     like stpo-sanvs,
  pm_rvrel     like stpo-rvrel,
  pm_schgt     like stpo-schgt,
  pm_stkkz     like stpo-stkkz.

parameters:
*  ProjektNr.
  pm_pspnr like prst-pspnr,                                 "HGA046836

  pm_vbeln like kdst-vbeln,                                 "HGB084505
  pm_vbpos like kdst-vbpos.                                 "HGB084505

*  reporteigene Konstanten
data:
  list_id    like klah-class value 'SAPCSMLVM         ',    "YHG137424
*  Profil zur Bildschirmanzeige
  dflt_dsprf like klah-class value 'SAPCSMLVMP01      ',    "YHG137424
*  Profil beim Druck
  dflt_prprf like klah-class value 'SAPCSMLVMP01      '.    "YHG137424

data:                                                       "HGA046836
*  obsolete
   pm_gbraz type c.                                         "HGA046836

types:                                                      "HGA095729
  begin of y_menu_fc_type,                                  "HGA095729
    fcode like rsmpe-func,                                  "HGA095729
  end of y_menu_fc_type.                                    "HGA095729

types:                                                      "HGA095729
  y_menu_fc_tb_type                                         "HGA095729
    type standard table of y_menu_fc_type                   "HGA095729
         with non-unique default key initial size 10.       "HGA095729

data:                                                       "HGA095729
  menu_fc_tb    type y_menu_fc_tb_type,                     "HGA095729

  wa_menu_fc_tb type y_menu_fc_type.                        "HGA095729

* .. Declaration for Menu Enhancement........................
data: lp_badi    type ref to badi_rcs12001_fcode_enh,
      lv_error   type abap_bool,
      ls_message type symsg,
      lt_sel_tab type table of rsparams.

*---------------------------------------------------------------------*
*.. ALV_S  beg .............................................. "HGA246532

type-pools: slis.
*..................................
data:
  report_name    like sy-repid,
  alvlo_stb      type slis_layout_alv,
  alvvr          like disvariant,
  alvvr_sav      type c,
  exit_by_caller type c,
  exit_by_user   type slis_exit_by_user.

data:
   wa_stb_fields_tb type slis_fieldcat_alv.

data:
*  ALV Events complete
  alv_evnt_tb_cmpl type slis_t_event,
*  ALV Events pf exit only
  alv_evnt_tb_pfxt type slis_t_event,
*  ALV Top of page table
  alv_top_tb       type slis_t_listheader,
*  field display properties  stb tab
  stb_fields_tb    type slis_t_fieldcat_alv.

data:
  alvvr_sav_all    type c value 'A',
  alvvr_sav_no_usr type c value 'X'.

data: begin of alv_stb occurs 0.
        include structure stpox_alv.
data:   info(3) type c,
        end of alv_stb.

data: begin of ftab occurs 200.
        include structure dfies.
data: end   of ftab.

data:
*  ALV-Variante
  pm_alvvr like ltdx-variant,
*  alv variant user specific
  pm_alvvu type c.

*.. ALV_S  end .......................................................*
*---------------------------------------------------------------------*

*eject
***********************************************************************
*        M A I N  -  Routinen                                         *
***********************************************************************
*eject
initialization.
  set titlebar 'E01' with text-001 text-002.
  import csbomex from memory id 'CSNN_BOMEX'.

  cal_aclas = csbomex-aclas.

*  CATT-Info besorgen
  perform import_catt_flag.                                 "HGC072824

  perform tcs03_lesen.
  perform tcspr_lesen.
  perform set_schalter.

  if not csbomex-submf = 'x'.
    perform sel_grenzen_01.

*del  IF SY-SUBRC = 0.                                        "YHG133914
    perform sel_grenzen_02.
*del  ENDIF.                                                  "YHG133914

    if not pm_alvsa is initial.                             "HGA246532
      perform get_profs.                                    "HGA025100
    endif.                                                  "HGA246532
  endif.


*eject
* ---------------------------------
at selection-screen.
*  ?Report per SUBMIT gestartet
*  nein, ... per SE38 oder sonstwie
  if csbomex-submf is initial.
    perform chk_plant_auth.                                 "HGA072824

    if sy-subrc <> 0.                                       "HGA072824
      if sy-batch is initial.                               "HGA072824
        message e523 with 'E:' pm_werks.                    "HGA072824
      else.                                                 "HGA072824
        leave.                                              "HGA072824
      endif.                                                "HGA072824
    endif.                                                  "HGA072824

*     ?weder Stuecklistenverwendung noch StlAnwendung angegeben
*     ja, weder - noch
    if     pm_stlan is initial
       and pm_capid is initial.

*        ?Batchverarbeitung aktiv
*        nein
      if     sy-batch is initial.
*           Msg.: wenigstens eine Verwendg od. eine Anwendg angeben
        message e560 with ''.
*        ja, Report laeuft im Moment im Batch
      else.
*           Report abbrechen - Selektion macht keinen Sinn
        leave.
      endif.
    endif.

*     ?weder Einsatzmenge noch Alternative angegeben
*     ja, weder - noch
    if     pm_emeng is initial
       and pm_stlal is initial.

*        ?Batchverarbeitung aktiv
*        nein
      if     sy-batch is initial.
*           Msg.: wenigstens eine Verwendg od. eine Anwendg angeben
        message w561 with ''.
      endif.
    endif.

*     ?weder Material noch KndDaten
*     ja, weder - noch
*    if     pm_mtnrv is initial                              "HGC059252
*       and (    pm_pspnr is initial )                       "HGA046836
*       and (    pm_vbeln is initial                         "HGC059252
*             or pm_vbpos is initial ).                      "HGC059252
**        ?Batchverarbeitung aktiv
**        nein
*      if     sy-batch is initial.                           "HGC059252
**           Msg.: wenigstens ein Mat od. KNDDaten angeben
*        message e562 with ''.                               "HGC059252
**        ja, Report laeuft im Moment im Batch
*      else.                                                 "HGC059252
**           Report abbrechen - Selektion macht keinen Sinn
*        leave.                                              "HGC059252
*      endif.                                                "HGC059252
*    endif.                                                  "HGC059252

    clear:                                                  "HGA084505
       anr_techs,                                           "HGA084505
       anr_datuv.                                           "HGA084505

    if not pm_aennr is initial.                             "HGA084505
      call function 'CC_CHANGE_NUMBER_READ'                 "HGA084505
        exporting                                        "HGA084505
          eaennr = pm_aennr                           "HGA084505
        importing                                        "HGA084505
*d                 atechs = anr_techs               "HGA084505"HGA154419
          adatuv = anr_datuv.                         "HGA084505

      if sy-subrc = 0.                                      "HGA084505
        if not anr_datuv is initial.                        "HGA084505
          if pm_datuv ne anr_datuv.                         "HGA084505
            pm_datuv = anr_datuv.                           "HGA084505
            message w042 with pm_datuv.                     "HGA084505
          endif.                                            "HGA084505
        endif.                                              "HGA084505
        if not anr_techs is initial.                        "HGA084505
          if pm_techs ne anr_techs.                         "HGA084505
            pm_techs = anr_techs.                           "HGA084505
          endif.                                            "HGA084505
        endif.                                              "HGA084505
      else.                                                 "HGA084505
        message w521 with 'E:' pm_aennr.                    "HGA084505
      endif.                                                "HGA084505
    endif.                                                  "HGA084505
  endif.


*eject
* ---------------------------------
start-of-selection.
  loop at s_matnr.
    pm_mtnrv = s_matnr-low.

    if csbomex-submf is initial.
      set parameter id 'CSV' field pm_stlan.
      set parameter id 'CSA' field pm_capid.
    endif.

    if pm_alvsa is initial.                                 "HGA246532
      report_name = sy-repid.                               "HGA246532

      alvlo_stb-detail_popup = 'X'.                         "HGA246532
      alvlo_stb-zebra        = 'X'.                         "HGA246532

      perform alv_evnt_tb_prep                              "HGA246532
        using                                               "HGA246532
          'A'                                               "HGA246532
          alv_evnt_tb_cmpl.                                 "HGA246532

      alvvr-report = report_name.                           "HGA246532
      if not pm_alvvr is initial.                           "HGA246532
        alvvr-variant = pm_alvvr.                           "HGA246532

        if not pm_alvvu is initial.                         "HGA246532
          alvvr-username = sy-uname.                        "HGA246532
        endif.                                              "HGA246532
      endif.                                                "HGA246532

      alvvr_sav = alvvr_sav_all.                            "HGA246532
    else.                                                   "HGA246532
      if    pm_dsprf is initial                             "HGA246532
         or pm_prprf is initial.                            "HGA246532

        perform get_profs.                                  "HGA246532
      endif.                                                "HGA246532

      class: cl_gui_frontend_services definition load.      "HGA095729

      if not cl_gui_frontend_services=>www_active is initial. "HGA095729
        wa_menu_fc_tb-fcode = 'CSGR'.                       "HGA095729
        append wa_menu_fc_tb to menu_fc_tb.                 "HGA095729

        set pf-status 'S121' excluding menu_fc_tb.          "HGA095729
      else.                                                 "HGA095729
*del    SET PF-STATUS 'SA12'.                                 "YHG139715
        set pf-status 'S121'.                               "YHG139715
      endif.                                                "HGA095729

      set titlebar 'A01'.
    endif.                                                  "HGA246532

*d IF NOT CSBOMEX-SUBMF = 'x'.                                "YHG046403
*del  PM_EMENG = PM_EMENG * 1000 .                            "YHG046403
*d ELSE.                                                      "YHG046403
*d    TMP_HRECHF = PM_HEMNG.                                  "YHG046403
*d    TMP_HRECHF = TMP_HRECHF / 1000.                         "YHG046403
*d    PM_EMENG   = TMP_HRECHF.                                "YHG046403
*d ENDIF.                                                     "YHG046403

    perform field_convert using pm_stlal.
    perform sel_grenzen_03.

    if cal_aclas eq 'CA  '.                                 "HGB070841
      mem_mngmt = '0'.                                      "HGB070841
    else.                                                   "HGB070841
      mem_mngmt = '1'.                                      "HGB070841
    endif.                                                  "HGB070841

*d DESCRIBE FIELD makt-matnr LENGTH mnr_lng.               "HGA080825"uc
    describe field makt-matnr                                         "uc
      length mnr_lng                                                  "uc
      in character mode.                                              "uc

*  die eigentliche Aufloesung
*d IF     PM_VBELN IS INITIAL                       "HGB084505"HGA046836
*d    OR  PM_VBPOS IS INITIAL.                      "HGB084505"HGA046836
    if     ( pm_vbeln is initial                            "HGA046836
             or  pm_vbpos is initial )                      "HGA046836
       and ( pm_pspnr is initial ).                         "HGA046836

      perform ecm_proc_init                                 "HGA084505
*d    using pm_techs.                               "HGA084505"HGA059252
         using pm_mtnrv                                     "HGA059252
               pm_techs.                                    "HGA059252

*  CS_BOM_EXPLOSION_MAT ersetzt                               "YHG133914
      call function 'CS_BOM_EXPL_MAT_V2'
        exporting
          altvo                 = pm_altvo
          aufsw                 = ' '
          auskz                 = pm_ausch
          bagrp                 = pm_bagrp
          beikz                 = pm_beikz
          bessl                 = pm_bessl
          brems                 = pm_brems
          capid                 = pm_capid
          datuv                 = pm_datuv
          drldt                 = pm_drldt                                   "note 438371
          ehndl                 = pm_ehndl                    "HGA108310
          emeng                 = pm_emeng
          erskz                 = pm_erskz
          erssl                 = pm_erssl
          mbwls                 = ' '
          mtnrv                 = pm_mtnrv
          mehrs                 = 'X'
*                                "YHG020009"HGB070841
          mmory                 = mem_mngmt                   "HGB070841
          postp                 = pm_postp
          sanko                 = pm_sanko
          sanfr                 = pm_sanfe
          sanka                 = pm_sanka
          sanin                 = pm_sanin
          sanvs                 = pm_sanvs
          rndkz                 = tcspr-amekz                 "YHG100738
          rvrel                 = pm_rvrel
          schgt                 = pm_schgt
          stkkz                 = pm_stkkz
          stlal                 = pm_stlal
          stlan                 = pm_stlan
          stpst                 = pm_stpst                    "HGD246532
          werks                 = pm_werks
          aumgb                 = 'X'                                        "NOTE_887365
        importing
          topmat                = selpool
          dstst                 = dstst_flg                   "YHG132854
        tables
          stb                   = stb
          matcat                = matcat
        exceptions
          material_not_found    = 4
          no_plant_data         = 8
          no_bom_found          = 12
          no_suitable_bom_found = 16
          alt_not_found         = 24
*                                                           "HGD059252
          missing_authorization = 28                          "HGD059252
          conversion_error      = 36.                         "HGD059252
*d ELSE.                                            "HGB084505"HGA046836
    elseif pm_pspnr is initial.                             "HGA046836
      call function 'SD_VBAP_SELECT'                          "HGB084505
        exporting                                            "HGB084505
          i_document_number = pm_vbeln                      "HGB084505
          i_item_number     = pm_vbpos                      "HGB084505
        importing                                            "HGB084505
          e_vbap            = vbap_wa                       "HGB084505
        exceptions                                           "HGB084505
          item_not_found    = 1                             "HGB084505
          others            = 2.                            "HGB084505

*d    wa_kdstb-matnr = vbap_wa-matnr.               "HGA033687"HGC059252
*d    wa_kdstb-werks = vbap_wa-werks.               "HGA033687"HGC059252
*d    wa_kdstb-vbeln = pm_vbeln.                    "HGA033687"HGC059252
*d    wa_kdstb-vbpos = pm_vbpos.                    "HGA033687"HGC059252

*d    call function 'GET_KDST'                      "HGA033687"HGC059252
*d         exporting                                "HGA033687"HGC059252
*d              all             = 'X'               "HGA033687"HGC059252
*d              no_buffer       = ' '               "HGA033687"HGC059252
*d              set             = 'X'               "HGA033687"HGC059252
*d         tables                                   "HGA033687"HGC059252
*d              wa              = wa_kdstb          "HGA033687"HGC059252
*d         exceptions                               "HGA033687"HGC059252
*d*             CALL_INVALID    = 1                           "HGC059252
*d*             END_OF_TABLE    = 2                           "HGC059252
*d*             GET_WITHOUT_SET = 3                           "HGC059252
*d*             KEY_INCOMPLETE  = 4                           "HGC059252
*d*             KEY_INVALID     = 5                           "HGC059252
*d              no_record_found = 32                "HGA033687"HGC059252
*d*             OTHERS          = 7                           "HGC059252
*d              .                                   "HGA033687"HGC059252

      if sy-subrc = 0.                                      "HGA033687
*     -->
        if vbap_wa-matnr ne pm_mtnrv.                       "HGB084505
          matpos_flg = 'x'.                                 "HGB084505
        endif.                                              "HGB084505

        if not vbap_wa-techs is initial.                    "HGA084505
          perform ecm_proc_init                             "HGA084505
*d          using vbap_wa-techs.                    "HGA084505"HGA059252
             using vbap_wa-matnr                            "HGA059252
                   vbap_wa-techs.                           "HGA059252
        else.                                               "HGA084505
*....... I: pm_techs kann auch init sein
          perform ecm_proc_init                             "HGA084505
*d          using pm_techs.                         "HGA084505"HGA059252
             using vbap_wa-matnr                            "HGA059252
                   pm_techs.                                "HGA059252
        endif.                                              "HGA084505

*     perform werks_best                             "note 353640 384597
*       using                                        "note 353640 384597
*         vbap_wa-matnr                              "note 353640 384597
*       changing                                     "note 353640 384597
*         vbap_wa-werks.                             "note 353640 384597

        if matpos_flg is initial  or                          "note 847517
           pm_mtnrv is initial.                               "note 847517

          perform compute_plant(saplcuko)                       "note 384597
                  using    vbap_wa-matnr                        "note 384597
                           vbap_wa-werks                        "note 384597
                  changing vbap_wa-werks.                       "note 384597

          call function 'CS_BOM_EXPL_KND_V1'                  "HGB084505
            exporting                                        "HGB084505
              altvo                 = pm_altvo                              "HGB084505
              aufsw                 = ' '                                   "HGB084505
              auskz                 = pm_ausch                              "HGB084505
              bagrp                 = pm_bagrp                              "HGB084505
              beikz                 = pm_beikz                              "HGB084505
              bessl                 = pm_bessl                              "HGB084505
              brems                 = pm_brems                              "HGB084505
              capid                 = pm_capid                              "HGB084505
              cuobj                 = vbap_wa-cuobj                         "HGB084505
              datuv                 = pm_datuv                              "HGB084505
              drldt                 = pm_drldt                                "note 438371
              ehndl                 = pm_ehndl                              "HGB084505
              emeng                 = pm_emeng                              "HGB084505
              erskz                 = pm_erskz                              "HGB084505
              erssl                 = pm_erssl                              "HGB084505
              mbwls                 = ' '                                   "HGB084505
              mtnrv                 = vbap_wa-matnr                         "HGB084505
              mehrs                 = 'X'                                   "HGB084505
              mmory                 = mem_mngmt                             "HGB084505
              postp                 = pm_postp                              "HGB084505
              sanko                 = pm_sanko                              "HGB084505
              sanfr                 = pm_sanfe                              "HGB084505
              sanka                 = pm_sanka                              "HGB084505
              sanin                 = pm_sanin                              "HGB084505
              sanvs                 = pm_sanvs                              "HGB084505
              rndkz                 = tcspr-amekz                           "HGB084505
              rvrel                 = pm_rvrel                              "HGB084505
              schgt                 = pm_schgt                              "HGB084505
              stkkz                 = pm_stkkz                              "HGB084505
              stlal                 = pm_stlal                              "HGB084505
              stlan                 = pm_stlan                              "HGB084505
              stpst                 = pm_stpst                              "HGD246532
              werks                 = vbap_wa-werks                         "HGB084505
              vbeln                 = pm_vbeln                              "HGB084505
              vbpos                 = pm_vbpos                              "HGB084505
            importing                                        "HGB084505
              topmat                = selpool                              "HGB084505
              dstst                 = dstst_flg                            "HGB084505
            tables                                           "HGB084505
              stb                   = stb                                     "HGB084505
              matcat                = matcat                               "HGB084505
            exceptions                                       "HGB084505
              material_not_found    = 4                     "HGB084505
              no_plant_data         = 8                     "HGB084505
*d          no_bom_found          = 12              "HGB084505"HGC059252
              no_bom_found          = 32                    "HGB084505
              no_suitable_bom_found = 16                    "HGB084505
              alt_not_found         = 24                    "HGB084505
*d          missing_authorization = 28.             "HGB084505"HGD059252
              missing_authorization = 28                    "HGD059252
              conversion_error      = 36.                   "HGD059252
*     <--
* START OF NOTE 847517
        else.

          perform compute_plant(saplcuko)                 "note 1226225
            using    pm_mtnrv                             "note 1226225
                     vbap_wa-werks                        "note 1226225
            changing vbap_wa-werks.                       "note 1226225

          call function 'CS_BOM_EXPL_KND_V1'
            exporting
              altvo                 = pm_altvo
              aufsw                 = ' '
              auskz                 = pm_ausch
              bagrp                 = pm_bagrp
              beikz                 = pm_beikz
              bessl                 = pm_bessl
              brems                 = pm_brems
              capid                 = pm_capid
              cuobj                 = vbap_wa-cuobj
              datuv                 = pm_datuv
              drldt                 = pm_drldt
              ehndl                 = pm_ehndl
              emeng                 = pm_emeng
              erskz                 = pm_erskz
              erssl                 = pm_erssl
              mbwls                 = ' '
              mtnrv                 = pm_mtnrv
              mehrs                 = 'X'
              mmory                 = mem_mngmt
              postp                 = pm_postp
              sanko                 = pm_sanko
              sanfr                 = pm_sanfe
              sanka                 = pm_sanka
              sanin                 = pm_sanin
              sanvs                 = pm_sanvs
              rndkz                 = tcspr-amekz
              rvrel                 = pm_rvrel
              schgt                 = pm_schgt
              stkkz                 = pm_stkkz
              stlal                 = pm_stlal
              stlan                 = pm_stlan
              stpst                 = pm_stpst
              werks                 = vbap_wa-werks
              vbeln                 = pm_vbeln
              vbpos                 = pm_vbpos
            importing
              topmat                = selpool
              dstst                 = dstst_flg
            tables
              stb                   = stb
              matcat                = matcat
            exceptions
              material_not_found    = 4
              no_plant_data         = 8
              no_bom_found          = 32
              no_suitable_bom_found = 16
              alt_not_found         = 24
              missing_authorization = 28
              conversion_error      = 36.

        endif.

*END OF NOTE 847517
      else.                                                 "HGC059252
        sy-subrc = 32.                                      "HGC059252
      endif.                                                "HGA033687
    elseif (     pm_vbeln is initial                        "HGA046836
             and pm_vbpos is initial ) .                    "HGA046836

      perform ecm_proc_init                                 "note 485260
         using pm_mtnrv                                     "note 485260
               pm_techs.                                    "note 485260

      call function 'CS_BOM_EXPL_PSP_V1'                      "HGA046836
        exporting                                            "HGA046836
          altvo                 = pm_altvo                                  "HGA046836
          aufsw                 = ' '                                       "HGA046836
          auskz                 = pm_ausch                                  "HGA046836
          bagrp                 = pm_bagrp                                  "HGA046836
          beikz                 = pm_beikz                                  "HGA046836
          bessl                 = pm_bessl                                  "HGA046836
          brems                 = pm_brems                                  "HGA046836
          capid                 = pm_capid                                  "HGA046836
          datuv                 = pm_datuv                                  "HGA046836
          drldt                 = pm_drldt                                "note 438371
          ehndl                 = pm_ehndl                                  "HGA046836
          emeng                 = pm_emeng                                  "HGA046836
          erskz                 = pm_erskz                                  "HGA046836
          erssl                 = pm_erssl                                  "HGA046836
          mbwls                 = ' '                                       "HGA046836
          mehrs                 = 'X'                                       "HGA046836
          mmory                 = mem_mngmt                                 "HGA046836
          mtnrv                 = pm_mtnrv                                  "HGA046836
          postp                 = pm_postp                                  "HGA046836
          pspnr                 = pm_pspnr                                  "HGA046836
          rndkz                 = tcspr-amekz                               "HGA046836
          rvrel                 = pm_rvrel                                  "HGA046836
          sanko                 = pm_sanko                                  "HGA046836
          sanfr                 = pm_sanfe                                  "HGA046836
          sanin                 = pm_sanin                                  "HGA046836
          sanka                 = pm_sanka                                  "HGA046836
          sanvs                 = pm_sanvs                                  "HGA046836
          schgt                 = pm_schgt                                  "HGA046836
          stkkz                 = pm_stkkz                                  "HGA046836
          stlal                 = pm_stlal                                  "HGA046836
          stlan                 = pm_stlan                                  "HGA046836
          stpst                 = pm_stpst                                  "HGD246532
          werks                 = pm_werks                                  "HGA046836
        importing                                            "HGA046836
          topmat                = selpool                   "HGA046836
          dstst                 = dstst_flg                 "HGA046836
        tables                                               "HGA046836
          stb                   = stb                       "HGA046836
          matcat                = matcat                    "HGA046836
        exceptions                                           "HGA046836
          material_not_found    = 4                         "HGA046836
          no_plant_data         = 8                         "HGA046836
          no_suitable_bom_found = 16                        "HGA046836
          alt_not_found         = 24                        "HGA046836
          missing_authorization = 28                        "HGA046836
          conversion_error      = 36                        "HGA046836
          no_bom_found          = 40                        "HGA046836
        .                                                 "HGA046836

      if sy-subrc <> 0.                                     "HGA046836
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.                                                "HGA046836
    endif.                                                  "HGB084505


*  ggf Grafikbutton ausschalten
    if not dstst_flg is initial.                            "YHG132854
      set pf-status sy-pfkey excluding 'CSGR'.              "YHG132854
    endif.                                                  "YHG132854

    if lines( s_matnr ) gt 1 and sy-subrc is not initial.
      continue.
    endif.

*  SubRc auswerten
    perform subrc_explr.

*del   PERFORM ANZEIGEFELDER_SETZEN.                          "YHG133914
    perform cs12_vorbereiten.

    if cal_aclas eq 'CA  '.
      export selpool to memory id 'CSNN_CSTMAT'.
      export stb     to memory id 'CSNN_STB'.
      export matcat  to memory id 'CSNN_MATCAT'.            "YHG134257

      clear: cal_aclas.
      leave.
    endif.

    if not pm_alvsa is initial.                             "HGA246532
      perform cs12.
      perform clr_hide_area.
      clear: hlf_stlnr.

      if not mpos_line is initial.                          "HGB084505
        scroll list to page mpos_page line mpos_line.       "HGB084505
      endif.                                                "HGB084505
    else.                                                   "HGA246532
      perform alv_top_tb_prep using alv_top_tb.             "HGA246532

      append lines of stb to it_stb.
      append lines of matcat to it_matcat.
    endif.                                                  "HGA246532


  endloop.

  stb[] = it_stb.
  matcat[] = it_matcat.

  loop at stb.
    stb-index = sy-tabix.
    modify stb.

    perform alv_stb_prep.
  endloop.

  perform cs12_alv.                                         "HGA246532

*eject
* ---------------------------------
top-of-page.
  perform top_01_79.


*eject
* ---------------------------------
top-of-page during line-selection.
  perform top_01_79.

  if not mpos_line is initial.                              "HGB084505
    scroll list to page mpos_page line mpos_line.           "HGB084505
  endif.                                                    "HGB084505


*eject
* ---------------------------------
end-of-selection.
**del   PERFORM ANZEIGEFELDER_SETZEN.                          "YHG133914
*  perform cs12_vorbereiten.
*
*  if cal_aclas eq 'CA  '.
*    export selpool to memory id 'CSNN_CSTMAT'.
*    export stb     to memory id 'CSNN_STB'.
*    export matcat  to memory id 'CSNN_MATCAT'.              "YHG134257
*
*    clear: cal_aclas.
*    leave.
*  endif.
*
*  if not pm_alvsa is initial.                               "HGA246532
*    perform cs12.
*    perform clr_hide_area.
*    clear: hlf_stlnr.
*
*    if not mpos_line is initial.                            "HGB084505
*      scroll list to page mpos_page line mpos_line.         "HGB084505
*    endif.                                                  "HGB084505
*  else.                                                     "HGA246532
*    perform alv_top_tb_prep using alv_top_tb.               "HGA246532
*    perform cs12_alv.                                       "HGA246532
*  endif.                                                    "HGA246532


*eject
* ---------------------------------
  include rcsnn001.


*eject
***********************************************************************
*        F O R M  -  Routinen                                         *
***********************************************************************
*eject
*---------------------------------------------------------------------*
*        ANZEIGEFELDER_SETZEN                                         *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeigefelder_setzen.
  clear:
     anz_stptx.

  if not selpool-altst is initial.
    anz_stptx = '101'.
    anz_stlal = selpool-stlal.
  endif.
  if not selpool-varst is initial.
    anz_stptx = '102'.
  endif.
  if not selpool-kbaus is initial.
    anz_stptx = '103'.
  endif.

  anz_datuv = selpool-datuv.
  anz_datub = selpool-datub.
endform.                    "anzeigefelder_setzen


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_01_79                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeige_01_79.
* ---------------------------------
* Routine im wesentlichen nach ANZEIGE_01_79_DRUCK uebernommen
* ---------------------------------
  if stb-mngko >= max_num.
    ueberl_flg = ueberl_kz.
  else.
    if stb-mngko <= min_num.
      ueberl_flg = ueberl_kz.
    else.
      clear: ueberl_flg.
    endif.
  endif.

  perform stufe_aufbereiten.
  perform anzeige_02_79.

*del IF SY-UCOMM NE 'CSPR'.                                   "YHG133914
  perform anzeige_01_79_liste.
*del ELSE.                                                    "YHG133914
*del    PERFORM ANZEIGE_01_79_DRUCK.                          "YHG133914
*del ENDIF.                                                   "YHG133914

*  ?Klassenposition
*   und Zuordnungen komplett od. teilweise anzeigen bzw. drucken
*  ja
  if     stb-objty eq otyp_kla
     and (     not pm_dspco is initial
*d          OR  sy-ucomm EQ 'CSCA'                          "note 351902
*d          OR  sy-ucomm EQ 'CSPR' ).                       "note 351902
           or  sv_ucomm eq 'CSCA'                          "note 351902
           or  sv_ucomm eq 'CSPR' ).                       "note 351902

*     Teilkey der Klassenpositionentabelle fuellen
    cl_clstab-class = stb-class.
    cl_clstab-klart = stb-klart.

*     Bearbeitungsstand zur aktuellen Klassenposition versorg
    read table cl_clstab
       with key cl_clstab(21)
       binary search.

*     ?Zuordnungen anzeigen
*     ja
    if not cl_clstab-dsply is initial.
*        ?Klassenposition bereits bearbeitet
*        nein
      if cl_clstab-chked is initial.
*           Klassenposition bearbeiten
        perform chk_cl_clstab_entry.
      endif.

*        ?gibt es ueberhaupt Zuordnungen zum Anzeigen
*        ja
      if cl_clstab-noobj is initial.
*           na also, dann los
        perform anzeige_04_79.
      endif.
    endif.
  endif.
endform.                    "anzeige_01_79


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_01_79_DRUCK                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  entfaellt mit var. Liste
*del FORM ANZEIGE_01_79_DRUCK.
*del ENDFORM.


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_01_79_LISTE                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  komplett ueberarbeitet                                     "YHG133914
form anzeige_01_79_liste.
  data: lkl_blockname like klah-class.
* ---------------------------------

*  Stufe - ohne Vorzeichen - in Uebergabestruktur uebernehmen
  write anz_stufe to stb_add-dglvl no-sign.

*  Feld fuer ObjAnzeige in Uebergabestruktur initialisieren
  clear: stb_add-dobjt.
  clear: lkl_blockname.

*  ?die Komponente ist ...
  case stb_orig-objty.
*     ... ein Material
    when otyp_mat.
*        Blockname festlegen
      lkl_blockname = 'ITEM_M            '.

*     ... ein Material (ein altes)
    when 'M'.
*        Blockname festlegen
      lkl_blockname = 'ITEM_M            '.

*     ... objektlos; z.B. eine Textposition
    when otyp_noo.
*        Blockname festlegen
      lkl_blockname = 'ITEM_NOO          '.

*     ... ein Dokument
    when otyp_doc.
*        Blockname festlegen
      lkl_blockname = 'ITEM_D            '.

*     ... eine Klasse
    when otyp_kla.
*        Blockname festlegen
      lkl_blockname = 'ITEM_C            '.

*     ... ein Intramaterial
    when otyp_ntm.
*        Blockname festlegen
      lkl_blockname = 'ITEM_NTM          '.

*     ... ein UFO
    when others.
*        Blockname festlegen
      clear: lkl_blockname.
  endcase.

*  Ueberlaufschalter in Uebergabestruktur uebernehmen
  stb_add-ovfls = ueberl_flg.

*  ?Komponente ist selber Stueckliste
*  ja
  if not stb_orig-xtlnr is initial.
*     Kennzeichen in Uebergabestruktur setzen
    stb_add-bomfl = b_flag.
  endif.

*  ?Komponente ist Wissensbaustein zugeordnet
*  ja
*d IF NOT STB_ORIG-KNOBJ IS INITIAL.                "YHG139320"YHG127645
  if     not stb_orig-knobj is initial                      "YHG127645
     or  not stb_orig-class is initial                      "YHG127645
     or  not stb_orig-kzclb is initial.                     "YHG127645

*     Kennzeichen in Uebergabestruktur setzen
    stb_add-knofl = 'X'.                                    "YHG139320
  endif.                                                    "YHG139320

*  ?es gibt einen Positionstext
*  ja
  if not stb_orig-potx1 is initial.
*     ?der Positionstext stimmt mit dem Kurztext ueberein
*     ja
    if     stb_orig-potx1 eq stb_orig-ojtxp.
*        dann kann man den Positionstext loeschen
      clear: stb_orig-potx1.
    endif.
  endif.

*d IF NOT pm_ltext IS INITIAL.                    "HGA132240"note 306308
*d    IF NOT stb-ltxsp IS INITIAL.                "HGA132240"note 306308
  if     not pm_ltext is initial                           "note 306308
     and not stb-ltxsp is initial.                         "note 306308

    perform ltext_holen.                                    "HGA132240

    if sy-subrc = 0.                                        "HGA132240
      read table txt index 1.                               "HGA132240
      if stb-potx1 = txt-tdline.                            "HGA132240
        if stb_orig-potx1 is initial.                "note 375902
          if stb_orig-ojtxp = txt-tdline.           "note 375902
            clear: stb_orig-ojtxp.                 "note 375902
          endif.                                    "note 375902
        endif.                                       "note 375902

        clear: stb_orig-potx1.                              "HGA132240

        read table txt index 2.                             "HGA132240
        if stb-potx2 = txt-tdline.                          "HGA132240
          clear: stb_orig-potx2.                            "HGA132240
        endif.                                              "HGA132240
      endif.                                                "HGA132240

      perform txt_conv_chk                            "note 306308
        tables                                        "note 306308
          txt                                         "note 306308
        using                                         "note 306308
          ' '                                         "note 306308
          ' '.                                        "note 306308

      call function 'FORMAT_TEXTLINES'                      "HGA132240
        exporting                                          "HGA132240
*d                   FORMATWIDTH = 41                         "HGA132240
          formatwidth = 40                       "note 667950
          linewidth   = 40                       "note 875961
        tables                                             "HGA132240
          lines       = txt.                           "HGA132240

      if     stb_orig-potx1 is initial                      "HGA132240
         and stb_orig-potx2 is initial.                     "HGA132240

        read table txt index 1.                             "HGA132240
        stb_orig-potx1 = txt-tdline.                        "HGA132240
        stb-potx1      = txt-tdline.                        "HGA132240

        read table txt index 2.                             "HGA132240
        stb_orig-potx2 = txt-tdline.                        "HGA132240
        stb-potx2      = txt-tdline.                        "HGA132240
      endif.                                                "HGA132240
    else.                                                   "HGA029649
      txt-tdline = text-097.                                "HGA029649
      append txt.                                           "HGA029649
    endif.                                                  "HGA132240
*d    ENDIF.                                      "HGA132240"note 306308
  else.                                                    "note 306308
    perform txt_conv_chk                                  "note 306308
      tables                                              "note 306308
        txt                                               "note 306308
      using                                               "note 306308
        stb_orig-potx1                                    "note 306308
        stb_orig-potx2.                                   "note 306308
  endif.                                                    "HGA132240

*  Positionszeile
*  je nach Farbschalter
  if col_switch is initial.
*     hellen Hintergrund an-
    format color col_normal intensified on.
    col_switch = 'x'.
  else.
*     ... oder ausschalten
    format color col_normal intensified off.
    clear: col_switch.
  endif.

*  WATAB initialisieren und komplett leeren
  clear: watab. refresh: watab.
*  Uebergabestruktur (Typ STPOX) ...
*d watab-tname = 'STPOX'. watab-table = stb_orig .                   "uc
  watab-tname = 'STPOX'.                                            "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stb_orig    to <x_stpox_wa>     casting.                   "uc
  <x_watab-table> = <x_stpox_wa> .                                  "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear: watab.
*  Uebergabestruktur (Typ CSCMAT) ...
*d watab-tname = 'CSCMAT'. watab-table = matcat.                     "uc
  watab-tname = 'CSCMAT'.                                           "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign matcat      to <x_cscmat_wa>    casting.                   "uc
  <x_watab-table> = <x_cscmat_wa> .                                 "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.
*  Uebergabestruktur (Typ STPOL_ADD) ...
*d watab-tname = 'STPOL_ADD'. watab-table = stb_add .                "uc
  watab-tname = 'STPOL_ADD'.                                        "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stb_add     to <x_stpol_add_wa> casting.                   "uc
  <x_watab-table> = <x_stpol_add_wa> .                              "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.

*  Daten zur Position ausgeben
  perform write_block
     using  lkl_blockname
*            ausgegebene Zeilen nicht zaehlen
            ' '
*            Hide ausfuehren
            'x'.                                            "YHG123656

*  ?soll Langtext ausgegeben werdem
*  ja
  if not pm_ltext is initial.
*     ?gibt es Langtext
*     ja
    if not stb-ltxsp is initial.
*        Langtext einlesen
*d       perform ltext_holen.                                 "HGA132240
      read table txt index 1.                               "HGA132240

*        ?Langtext konnte eingelesen werden
*        ja
      if sy-subrc = 0.
*           pro Textzeile ...
        loop at txt.
*              ?wenn Zeile 1 ...
          if sy-tabix = 1.
*                 gleich der ersten PosTextzeile ist, - nicht ausgeben
            check stb-potx1 <> txt-tdline.
          endif.

*              ?wenn Zeile 1 ...
          if sy-tabix = 2.
*                 gleich der zweiten PosTextzeile ist, - nicht ausgeben
            check stb-potx2 <> txt-tdline.
          endif.

*              Uebergabestruktur initialisieren
          clear: stb_add.
*              Textzeile in Uebergabestruktur uebernehmen
          stb_add-tline = txt-tdline.

*              WATAB initialisieren und komplett leeren
          clear watab. refresh watab.
*              Uebergabestruktur (Typ STPOL_ADD) ...
*d             watab-tname = 'STPOL_ADD'. watab-table = stb_add .    "uc
          watab-tname = 'STPOL_ADD'.                            "uc
          assign watab-table to <x_watab-table>  casting.       "uc
          assign stb_add     to <x_stpol_add_wa> casting.       "uc
          <x_watab-table> = <x_stpol_add_wa> .                  "uc
*              ... sichern
          append watab.

*              WATAB initialisieren und komplett leeren
          clear watab.
*              Langtextzeile ausgeben
          perform write_block
             using 'LTEXT_LIN         '
*                       ausgegebene Zeilen nicht zaehlen
                   ' '
*                       Hide ausfuehren
                   'x'.                                     "YHG123656
        endloop.
      endif.
    endif.
  endif.
endform.                    "anzeige_01_79_liste


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_02_79                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeige_02_79.
* ---------------------------------
* Routine im wesentlichen nach ANZEIGE_02_79_DRUCK uebernommen
* ---------------------------------
*del IF SY-UCOMM NE 'CSPR'.                                   "YHG133914
  perform anzeige_02_79_liste.
*del ELSE.                                                    "YHG133914
*del    PERFORM ANZEIGE_02_79_DRUCK.                          "YHG133914
*del ENDIF.                                                   "YHG133914
endform.                    "anzeige_02_79


*---------------------------------------------------------------------*
*        ANZEIGE_02_79_DRUCK                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  entfaellt mit var. Liste
*del FORM ANZEIGE_02_79_DRUCK.
*del ENDFORM.


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_02_79_LISTE                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  komplett ueberarbeitet                                     "YHG133914
form anzeige_02_79_liste.
  if stb-stlnr ne hlf_stlnr.
    if     pm_beikz ne space
       or  pm_bessl ne space
       or  pm_erskz ne space
       or  pm_erssl ne space
       or  pm_bagrp ne space
       or  pm_postp ne space.

      if first_flg is initial.
        first_flg = 'n'.
      else.
        uline at /1(siz_linpf).
      endif.

      write cnull to stb_add-dposn.
*d       WRITE MATCAT-MATNR TO STB_ADD-MATNR.                 "HGA105944
      stb_add-matnr = matcat-matnr.                         "HGA105944

      clear: col_switch.
      format color col_group intensified off.

*        WATAB initialisieren und komplett leeren
      clear: watab. refresh: watab.
*        Uebergabestruktur (Typ STPOL_ADD) ...
*d       watab-tname = 'STPOL_ADD'. watab-table = stb_add ."HGA025100"uc
      watab-tname = 'STPOL_ADD'.                                  "uc
      assign watab-table to <x_watab-table>  casting.             "uc
      assign stb_add     to <x_stpol_add_wa> casting.             "uc
      <x_watab-table> = <x_stpol_add_wa> .                        "uc
*        ... sichern
      append watab.

*        WATAB initialisieren und komplett leeren
      clear: watab.                                         "HGA025100
*        Uebergabestruktur (Typ CSCMAT) ...
*d       watab-tname = 'CSCMAT'. watab-table = matcat.     "HGA025100"uc
      watab-tname = 'CSCMAT'.                                     "uc
      assign watab-table to <x_watab-table>  casting.             "uc
      assign matcat     to <x_cscmat_wa>     casting.             "uc
      <x_watab-table> = <x_cscmat_wa> .                           "uc
*        ... sichern
      append watab.                                         "HGA025100

*        WATAB initialisieren
      clear watab.
*        Kopfdaten zum SemiFinishedProduct ausgeben
      perform  write_block
         using 'SFP_INFO          '
*                 ausgegebene Zeilen nicht zaehlen
               ' '
*                 Hide nicht ausfuehren
               ' '.                                         "YHG123656

      hlf_stlnr = stb-stlnr.
    endif.
  endif.
endform.                    "anzeige_02_79_liste


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_03_79                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeige_03_79.
* ---------------------------------
* Routine im wesentlichen nach ANZEIGE_03_79_DRUCK uebernommen
* ---------------------------------
*del IF SY-UCOMM NE 'CSPR'.                                   "YHG133914
  perform anzeige_03_79_liste.
*del ELSE.                                                    "YHG133914
*del    PERFORM ANZEIGE_03_79_DRUCK.                          "YHG133914
*del ENDIF.                                                   "YHG133914
endform.                    "anzeige_03_79


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_03_79_DRUCK                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  entfaellt mit var. Liste
*del FORM ANZEIGE_03_79_DRUCK.
*del ENDFORM.


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_03_79_LISTE                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  komplett ueberarbeitet                                     "YHG133914
form anzeige_03_79_liste.
*  Uebergabestruktur (Typ STPOL_ADD) initialisieren
  clear: stb_add.
*  Kennzeichen 'Unterpositionen existieren od. auch nicht' uebernehmen
  stb_add-dupmg = anz_upmng.

*  WATAB initialisieren und komplett leeren
  clear: watab. refresh: watab.
*  Uebergabestruktur (Typ STPUB) ...
*d watab-tname = 'STPUB'. watab-table = stpub .                      "uc
  watab-tname = 'STPUB'.                                            "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stpub       to <x_stpub_wa>     casting.                   "uc
  <x_watab-table> = <x_stpub_wa> .                                  "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.
*  Uebergabestruktur (Typ STPOX) ...
*d watab-tname = 'STPOX'. watab-table = stb_orig .                   "uc
  watab-tname = 'STPOX'.                                            "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stb_orig    to <x_stpox_wa>     casting.                   "uc
  <x_watab-table> = <x_stpox_wa> .                                  "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear: watab.
*  Uebergabestruktur (Typ CSCMAT) ...
*d watab-tname = 'CSCMAT'. watab-table = matcat.                     "uc
  watab-tname = 'CSCMAT'.                                           "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign matcat     to <x_cscmat_wa>     casting.                   "uc
  <x_watab-table> = <x_cscmat_wa> .                                 "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.
*  Uebergabestruktur (Typ STPOL_ADD) ...
*d watab-tname = 'STPOL_ADD'. watab-table = stb_add .                "uc
  watab-tname = 'STPOL_ADD'.                                        "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stb_add     to <x_stpol_add_wa> casting.                   "uc
  <x_watab-table> = <x_stpol_add_wa> .                              "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.
*  Daten zur Unterposition ausgeben
  perform write_block
     using 'SUB_ITEM          '
*           ausgegebene Zeilen nicht zaehlen
           ' '
*           Hide ausfuehren
           'x'.                                             "YHG123656
endform.                    "anzeige_03_79_liste


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_04_79                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeige_04_79.
*del IF SY-UCOMM NE 'CSPR'.                                   "YHG133914
  perform anzeige_04_79_liste.
*del ELSE.                                                    "YHG133914
*del    PERFORM ANZEIGE_04_79_DRUCK.                          "YHG133914
*del ENDIF.                                                   "YHG133914
endform.                    "anzeige_04_79


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_04_79_DRUCK                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  entfaellt mit var. Liste
*del FORM ANZEIGE_04_79_DRUCK.
*del ENDFORM.


*eject
*---------------------------------------------------------------------*
*        ANZEIGE_04_79_LISTE                                          *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form anzeige_04_79_liste.
* ---------------------------------
  data: sav_cl_objmemo like cl_objmemo.
* ---------------------------------
*  Teilkey der KlassenObjekteMemotabelle fuellen
  sav_cl_objmemo(18)   = stb-class.
  sav_cl_objmemo+18(3) = stb-klart.

*  Merktabelle lesen
  read table cl_objmemo
     with key sav_cl_objmemo(21)
     binary search.

*  ?passende Eintraege (Zuordnungen) gefunden
*  ja
  if sy-subrc = 0.
    clear: stb_add.                                         "YHG133914
*     Alle passenden Zuordnungen ...
    loop at cl_objmemo from sy-tabix.
*        ?Eintrag passt noch
*        nein
*del     IF CL_OBJMEMO(21) NE SAV_CL_OBJMEMO.                 "YHG100916
      if cl_objmemo(21) ne sav_cl_objmemo(21).              "YHG100916
*           Ausgabe beenden
        exit.
      endif.

*        MatNr fuer HIDE festlegen
*d       CLO_MATNR = CL_OBJMEMO-OBJECT(18).                   "HGA080825
      clo_matnr = cl_objmemo-object(mnr_lng).               "HGA080825

*        gib Zuordnungen aus
*-- ab hier neu -------------------------------------------------------*
*        Klassenobjekt (Mat) in Uebergabestruktur uebernehmen
*d     WRITE CL_OBJMEMO-OBJECT(18) TO STB_ADD-CLOBJ."YHG133914"HGA080825
      write cl_objmemo-object to stb_add-clobj(mnr_lng).    "HGA080825
*        Kurztext zum Objekt in Uebergabestruktur uebernehmen
      write cl_objmemo-text(40) to stb_add-clotx.           "YHG133914

*        WATAB initialisieren und komplett leeren
      clear: watab. refresh: watab.                         "YHG133914
*        Uebergabestruktur (Typ STPOL_ADD) ...
*d       watab-tname = 'STPOL_ADD'. watab-table = stb_add ."YHG133914"uc
      watab-tname = 'STPOL_ADD'.                                  "uc
      assign watab-table to <x_watab-table>  casting.             "uc
      assign stb_add     to <x_stpol_add_wa> casting.             "uc
      <x_watab-table> = <x_stpol_add_wa> .                        "uc
*        ... sichern
      append watab.                                         "YHG133914

*        WATAB initialisieren
      clear watab.                                          "YHG133914
*        Daten zum Objekt der Klasse ausgeben
      perform write_block                                   "YHG133914
         using 'CLASS_OBJ         '                         "YHG133914
*                 ausgegebene Zeilen nicht zaehlen
               ' '                                          "YHG133914
*                 Hide nicht ausfuehren
               ' '.                                         "YHG123656
    endloop.
  endif.
endform.                    "anzeige_04_79_liste




*eject
*---------------------------------------------------------------------*
*        CS12                                                         *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form cs12.
*d IF NOT SY-BATCH IS INITIAL.                                "HGC072824
  if     not sy-batch is initial                            "HGC072824
     and cattaktiv is initial.                              "HGC072824

    perform print_mode_batch.
  endif.

*  ?Druck eingeschaltet
*  ja
*d IF sy-ucomm EQ 'CSPR'.                         "YHG133914"note 351902
  if sv_ucomm eq 'CSPR'.                                   "note 351902
*     Druckprofil aktivieren
    act_profil = pm_prprf.                                  "YHG133914

    perform prof_get_cmpl                                   "HGB020150
       using itf_prfsz.                                     "HGB020150

    perform prrsv_tb_cre.                                   "HGB020150
*  nein, Bildschirmausgabe
  else.                                                     "YHG133914
*     Bildschirmausgabeprofil aktivieren
    act_profil = pm_dsprf.                                  "YHG133914
  endif.                                                    "YHG133914

*  im Profil definierte Zeilenbreite besorgen
  call function 'CLFC_PROFILE_SIZE'                         "YHG133914
    exporting                                            "YHG133914
*del         LISTID           = 'SAPCSMLVM         '"YHG133914"YHG137424
      listid                = list_id                 "YHG137424
      profile               = act_profil              "YHG133914
    importing                                            "YHG133914
*del         SIZE                  = SAV_PRFSZ      "YHG133914"YHG32486
      size                  = itf_prfsz               "YHG32486
    exceptions                                           "YHG133914
      listid_not_found      = 01                      "YHG133914
      no_valid_listid       = 02                      "YHG133914
      no_valid_profile      = 03                      "YHG133914
      profile_not_found     = 04                      "YHG133914
      profile_not_in_listid = 05.                     "YHG133914

  sav_prfsz = itf_prfsz.                                    "YHG32486

  if sy-subrc <> 0.                                         "YHG137424
*     ?Druck eingeschaltet
*     ja
*d    IF sy-ucomm EQ 'CSPR'.                      "YHG137424"note 351902
    if sv_ucomm eq 'CSPR'.                                "note 351902
*        Druckprofil aktivieren
      act_profil = dflt_prprf.                              "YHG137424
*     nein, Bildschirmausgabe
    else.                                                   "YHG137424
*        Bildschirmausgabeprofil aktivieren
      act_profil = dflt_dsprf.                              "YHG137424
    endif.                                                  "YHG137424

    call function 'CLFC_PROFILE_SIZE'                       "YHG137424
      exporting                                          "YHG137424
        listid  = list_id                             "YHG137424
        profile = act_profil                          "YHG137424
      importing                                          "YHG137424
*del            SIZE    = SAV_PRFSZ.                "YHG137424"YHG032486
        size    = itf_prfsz.                          "YHG032486

    sav_prfsz = itf_prfsz.                                  "YHG032486
  endif.                                                    "YHG137424

*  Zeilenbreiten wg. Rahmen um 2 erhoehen
  siz_linpf = sav_prfsz + 2.                                "YHG133914

*d IF sy-ucomm EQ 'CSPR'.                         "YHG139715"note 351902
  if sv_ucomm eq 'CSPR'.                                   "note 351902
    perform prep_druck.                                     "YHG139715
  endif.                                                    "YHG139715

  perform create_dsp_sel.                                   "YHG139715

*d IF sy-ucomm EQ 'CSPR'.                         "YHG139715"note 351902
  if sv_ucomm eq 'CSPR'.                                   "note 351902
    perform selkrit_druck.                                  "YHG139715
  endif.                                                    "YHG139715

  loop at stb.
*     Uebergabestrukturen initialisieren
    clear: stb_orig,                                        "YHG133914
           stb_add.                                         "YHG133914

    if stb-ttidx <> matcat-index.
      read table matcat index stb-ttidx.
    endif.

*     STB-Eintrag in Uebergabestruktur uebernehmen
    stb_orig = stb.                                         "YHG133914

    perform anzeige_01_79.

    if not pm_upsaz is initial.
      perform upos_bearbeitung.
    endif.
  endloop.

*  ?gab es ueberhaupt einen STB-Eintrag
*  nein
  if sy-subrc <> 0.
*     traurige Nachricht ausgeben
    message s513 with 'E: '.
*  ja, Positionen gefunden und ausgegeben
  else.
*     ?wird gerade gedruckt
*     nein
*d    IF sy-ucomm NE 'CSPR'.                                "note 351902
    if sv_ucomm ne 'CSPR'.                                "note 351902
*        Liste mit Strichlinie abschliessen
*del     ULINE (81).                                "YHG036694"YHG133914
      uline at /1(siz_linpf).                               "YHG133914
*del     PERFORM END_PAGE.                          "YHG036694"YHG133914
    else.                                                   "YHG139336
      skip.                                                 "YHG139336
*        Ende der Liste
      format color col_background.                          "YHG139336
      write: /       text-098 intensified.                  "YHG139336
    endif.
  endif.
endform.                                                    "cs12


*eject
*---------------------------------------------------------------------*
*        CS12_VORBEREITEN                                             *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form cs12_vorbereiten.
  clear: cl_clstab,
         cl_objmemo.

  refresh: cl_clstab,
           cl_objmemo.

  loop at stb.
    perform obj_ident.                                      "YHG121750

    perform cl_clstab_maint.
  endloop.

  sort stb by index ascending.

*  ?Fuer Klassenpositionen alle Zuordnungen ausgeben
*  ja
  if not pm_dspco is initial.
*     alle Zuordnungen aller Klassenpositionen besorgen
    perform get_cla_allocs_all.
  endif.
endform.                    "cs12_vorbereiten


*eject
*---------------------------------------------------------------------*
*        LISTE_DRUCKEN                                                *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form liste_drucken.
  perform cs12.
endform.                    "liste_drucken


*eject
*---------------------------------------------------------------------*
*        LTEXT_HOLEN                                                  *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form ltext_holen.
  data: begin of txhdr.                                     "HGB057558
          include structure thead.                          "HGB057558
  data: end of txhdr.                                       "HGB057558
*----------------------------------

  clear: txt_key.
  write sy-mandt  to txt_key using no edit mask.
  txt_kyln = strlen( txt_key ).
  write stb-stlty to txt_key+txt_kyln.
  txt_kyln = strlen( txt_key ).
  write stb-stlnr to txt_key+txt_kyln using no edit mask.
  txt_kyln = strlen( txt_key ).
  write stb-stlkn to txt_key+txt_kyln.
  txt_kyln = strlen( txt_key ).
  write stb-stpoz to txt_key+txt_kyln.

*d bom_txid = mbm_txid.                           "HGA029649"note 411978
*d IF stb-stlty EQ typ_knd.                       "HGA029649"note 411978
*d   bom_txid = kbm_txid.                         "HGA029649"note 411978
*d ENDIF.                                         "HGA029649"note 411978

  bom_txid(1) = stb-stlty.                                 "note 411978
  bom_txid+1  = 'PO'.                                      "note 411978

  call function 'READ_TEXT'
    exporting
      id        = bom_txid
      language  = stb-ltxsp
      name      = txt_key
      object    = bom_txobj
    importing                                               "HGB057558
      header    = txhdr                                     "HGB057558
    tables
*                                                           "HGA029649
      lines     = txt                                       "HGA029649
    exceptions                                              "HGA029649
      not_found = 1.                                        "HGA029649

  if sy-subrc = 0.                                          "HGA029649
    perform create_txincl_cmd.                              "HGB057558

    call function 'TEXT_INCLUDE_REPLACE'                    "HGB057558
      exporting                                            "HGB057558
        header = txhdr                                 "HGB057558
      tables                                               "HGB057558
        lines  = txt.                                  "HGB057558
  endif.                                                    "HGA029649
endform.                    "ltext_holen


*eject
*---------------------------------------------------------------------*
*        MENGN_01                                                     *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form mengn_01.
  if stb-menge <> stb-mngko.
    ups_faktor = stb-mngko / stb-menge .
  else.
    ups_faktor = 1 .
  endif.

  if stb-roanz <> 0.
    ups_faktor = ups_faktor * ( stb-menge / stb-roanz ) .
  endif.
endform.                                                    "mengn_01


*eject
*---------------------------------------------------------------------*
*        MENGN_02                                                     *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form mengn_02.
  if ups_faktor <> 1 .
    anz_upmng = stpub-upmng * ups_faktor.
  else.
    anz_upmng = stpub-upmng.
  endif.
endform.                                                    "mengn_02


*eject
*---------------------------------------------------------------------*
*        NEU_ANZEIGE                                                  *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form neu_anzeige.
*  Liste anzeigen
  perform cs12.
*  Hidebereich loeschen
  perform clr_hide_area.
endform.                    "neu_anzeige


*eject
*---------------------------------------------------------------------*
*        OBJ_IDENT                                                    *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form obj_ident.                                             "YHG121750
*  weder Mat noch Doc
  check: stb-objty ne otyp_mat,
         stb-objty ne ootyp_mat,                            "HGB099459
         stb-objty ne otyp_doc,
         stb-objty ne otyp_ntm.

*  ?T418-WA schon ok
*  nein
  if stb-postp ne t418-postp.
*     T418 einlesen
    perform t418_lesen using stb-postp.
  endif.

*  ?MatNr-Eingabe bei diesem PosTyp moeglich
*   und keine Textposition
*  trifft zu
  if     t418-matin ne '-'
     and t418-txpos is initial.

*     aktuelles Objekt ist Material NLAG
    stb-objty = '1'.
*     PosKurztext in ObjKurztext uebernehmen.
    stb-ojtxp = stb-potx1.
    modify stb.
  else.
    if stb-objty is initial.
      stb-objty = '2'.
      modify stb.
    endif.
  endif.
endform.                    "obj_ident


*eject
*---------------------------------------------------------------------*
*        SEL_GRENZEN_01                                               *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form sel_grenzen_01.
  if tcs03-lowdt is initial.
    pm_datuv = syst-datum.
  else.
    pm_datuv = min_grg.
  endif.
endform.                    "sel_grenzen_01


*eject
*---------------------------------------------------------------------*
*        SEL_GRENZEN_02                                               *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form sel_grenzen_02.
*  ?Report wurde von woanders per Submit gestartet
*  nein
*del IF CSBOMEX-SUBMF IS INITIAL.                             "YHG133914
*  Stuecklistenverwendung aus der ParameterArea vorschlagen
  get parameter id 'CSV' field pm_stlan.
*  Stuecklistenanwendung aus der ParameterArea vorschlagen
  get parameter id 'CSA' field pm_capid.
*del ENDIF.                                                   "YHG133914
endform.                    "sel_grenzen_02


*eject
*---------------------------------------------------------------------*
*        SET_MARGIN                                                   *
*---------------------------------------------------------------------*
*        in Include RCSNN001 verlagert
*---------------------------------------------------------------------*


*eject
*---------------------------------------------------------------------*
*        SET_SCHALTER                                                 *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form set_schalter.
  if not tcspr-upsaz is initial.
    pm_upsaz = 'X'.
  endif.

  if not tcspr-ltext is initial.                            "HGA127128
    pm_ltext = 'X'.                                         "HGA127128
  endif.                                                    "HGA127128

  if not tcspr-dspco is initial.                            "HGA246532
    pm_dspco = 'X'.                                         "HGA246532
  endif.                                                    "HGA246532

  if not tcspr-valst is initial.                            "HGA246532
    pm_alvsa = 'X'.                                         "HGA246532
  endif.                                                    "HGA246532
endform.                    "set_schalter


*eject
*---------------------------------------------------------------------*
*        STPU_LESEN                                                   *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form stpu_lesen.
  call function 'GET_STPU'
    exporting
      set             = 'X'
      all             = 'X'
    tables
      wa              = stpub
    exceptions
      no_record_found = 4
      key_incomplete  = 16
      call_invalid    = 24.
endform.                    "stpu_lesen


*eject
*---------------------------------------------------------------------*
*        STUFE_AUFBEREITEN                                            *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form stufe_aufbereiten.
  anz_stufe = stb-stufe.
  translate anz_stufe using ' .'.
  anz_stufe+10(1) = ' '.

  if stb-stufe < 9.
    stb-stufe = 9 - stb-stufe.
    shift anz_stufe by stb-stufe places.
    stb-stufe = 9 - stb-stufe.
  endif.
endform.                    "stufe_aufbereiten


*eject
*---------------------------------------------------------------------*
*        T418_LESEN                                                   *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <-- T418-WA                                                  *
*                                                                     *
*---------------------------------------------------------------------*
form t418_lesen using lkl_postp.                            "YHG121750
*  T418-WA initialisieren
  clear:
     t418.

*  Key angeben
  t418-postp = lkl_postp.
*  PosTypDefinition lesen
  read table t418.
endform.                                                    "t418_lesen


*eject
*---------------------------------------------------------------------*
*        TCS03_LESEN                                                  *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form tcs03_lesen.
  tcs03-agb29 = '29'.
  read table tcs03.
endform.                    "tcs03_lesen


*eject
*---------------------------------------------------------------------*
*        TCSPR_LESEN                                                  *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form tcspr_lesen.
  tcspr-csusr = sy-uname.
  read table tcspr.

  if sy-subrc <> 0.                                         "HGG099459
    tcspr-csusr = 'DUMMY'.                                  "HGG099459
    read table tcspr.                                       "HGG099459
  endif.                                                    "HGG099459
endform.                    "tcspr_lesen


*eject
*---------------------------------------------------------------------*
*        TOP_01_79                                                    *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form top_01_79.
*d CHECK sy-ucomm NE 'CSSL'.                      "YHG139715"note 351902
  check sv_ucomm ne 'CSSL'.                                "note 351902

*del CLEAR: REAL_LINE.                                        "YHG133914

*del IF SY-UCOMM NE 'CSPR'.                                   "YHG133914
  perform top_01_79_liste.
*del ELSE.                                                    "YHG133914
*del    PERFORM TOP_01_79_DRUCK.                              "YHG133914
*del ENDIF.                                                   "YHG133914
endform.                                                    "top_01_79


*eject
*---------------------------------------------------------------------*
*        TOP_01_79_DRUCK                                              *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  entfaellt mit var. Liste
*del FORM TOP_01_79_DRUCK.
*del ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TOP_01_79_LISTE                                              *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
*  komplett ueberarbeitet
form top_01_79_liste.
*  Zeilenzaehler initialisieren
  clear: blclns_cnt.
*  SAV-WATAB-Entry initialisieren
  clear: sav_watab.
*  SAV-WATAB leeren
  refresh: sav_watab.

*  die aktuellen WATab-Eintraege (var. Liste)
*del LOOP AT WATAB.                                           "YHG020009
*     ... nach SAV_WATAB ...                                  "YHG020009
*del  SAV_WATAB = WATAB.                                      "YHG020009
*     ... sichern                                             "YHG020009
*del  APPEND SAV_WATAB.                                       "YHG020009
*del ENDLOOP.                                                 "YHG020009
  sav_watab[] = watab[].                                    "YHG020009

*  ?EinstiegsStl ist MehrfachStl
*  ja
  if not selpool-altst is initial.
*     fuehrende Nullen der AlternativenNr entfernen
    if selpool-stlal(1) = '0'.
      selpool-stlal(1) = ' '.
    endif.
*  nein, keine MehrfachStl
  else.
*     AlternativenNr loeschen
    clear: selpool-stlal.
  endif.

*  Ausgabeformat festlegen
  format color col_background intensified off.

*  ?Druck
*  ja
*d IF sy-ucomm EQ 'CSPR'.                                   "note 351902
  if sv_ucomm eq 'CSPR'.                                   "note 351902
*     Strich auf Zeile 2 mit Strichlinie aus AnzBlock ueberschreiben
*del  SKIP TO LINE 2.                                         "YHG139336
*del  SKIP TO LINE 1.                               "YHG139336"YHG137563
    skip to line 2.                                         "YHG137563
  endif.

*  WATAB initialisieren und komplett leeren
  clear: watab. refresh: watab.
*  Uebergabestruktur (Typ CSTMAT) ...
*d watab-tname = 'CSTMAT'. watab-table = selpool .                   "uc
  watab-tname = 'CSTMAT'.                                           "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign selpool    to <x_cstmat_wa>     casting.                   "uc
  <x_watab-table> = <x_cstmat_wa> .                                 "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear: watab,                                             "YHG127645
         stb_add.                                           "YHG127645

  stb_add-sldtv = pm_datuv.                                 "YHG127645
  stb_add-pspnr = pm_pspnr.                                 "HGA046836
  stb_add-vbeln = pm_vbeln.                                 "HGC072185
  stb_add-vbpos = pm_vbpos.                                 "HGC072185
*  Uebergabestruktur (Typ STPOL_ADD). - wg TOKNZ
*d watab-tname = 'STPOL_ADD'. watab-table = stb_add .      "YHG127645"uc
  watab-tname = 'STPOL_ADD'.                                        "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign stb_add     to <x_stpol_add_wa> casting.                   "uc
  <x_watab-table> = <x_stpol_add_wa> .                              "uc
*  ... sichern
  append watab.                                             "YHG127645

*  WATAB initialisieren
  clear watab.
*  Listenkopf ausgeben
  perform write_block
     using 'LISTHDR           '
*           ausgegebene Zeilen zaehlen
           'x'
*           Hide nicht ausfuehren
           ' '.                                             "YHG123656

*  WATAB initialisieren und komplett leeren
  clear: watab. refresh: watab.
*  Uebergabestruktur (Typ STPOX) ...
  watab-tname = 'STPOX'.
*  ... sichern
  append watab.

*  WATAB initialisieren und komplett leeren
  clear: watab.
*  Uebergabestruktur (Typ CSCMAT) ...
*d watab-tname = 'CSCMAT'. watab-table = matcat.                     "uc
  watab-tname = 'CSCMAT'.                                           "uc
  assign watab-table to <x_watab-table>  casting.                   "uc
  assign matcat     to <x_cscmat_wa>     casting.                   "uc
  <x_watab-table> = <x_cscmat_wa> .                                 "uc
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.
*  Uebergabestruktur (Typ STPOL_ADD) ...
  watab-tname = 'STPOL_ADD'.
*  ... sichern
  append watab.

*  WATAB initialisieren
  clear watab.

*  Ausgabe festlegen
  format color col_heading intensified on.
*  Listenueberschrift ausgeben
  perform write_block
     using 'LISTHDNG          '
*           ausgegebene Zeilen zaehlen
           'x'
*           Hide nicht ausfuehren
           ' '.                                             "YHG123656

*  WATab-Entry (var. Liste) initialisieren
  clear: watab.
*  WATab (var. Liste) leeren
  refresh: watab.

*  gesicherte Saetze aus SAV_WATAB
*del LOOP AT SAV_WATAB.                                       "YHG020009
*     wieder nach WATAB ...                                   "YHG020009
*del  WATAB = SAV_WATAB.                                      "YHG020009
*     ... uebernehmen                                         "YHG020009
*del  APPEND WATAB.                                           "YHG020009
*del ENDLOOP.                                                 "YHG020009
  watab[] = sav_watab[].                                    "YHG020009

*  Anzahl Zeilen Listenkopf sichern
  nbr_hdrlns = blclns_cnt.
*  Reset Zeilenzaehler
  clear: blclns_cnt.
endform.                    "top_01_79_liste


*eject
*---------------------------------------------------------------------*
*        UPOS_BEARBEITUNG                                             *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form upos_bearbeitung.
  check not stb-upskz is initial.

  clear: stpub.
  stpub-stlty = stb-stlty.
  stpub-stlnr = stb-stlnr.
  stpub-stlkn = stb-stlkn.
  stpub-stpoz = stb-stpoz.
  perform stpu_lesen.

  if sy-subrc = 0 .
    perform mengn_01.

    loop at stpub.
      perform mengn_02.
      perform anzeige_03_79.
    endloop.
  endif.
endform.                    "upos_bearbeitung


*eject
*---------------------------------------------------------------------*
*        WRITE_BLOCK                                                  *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form write_block                                            "YHG133914
     using lcl_blcnm
           lcl_lncnt
           lcl_hidef.                                       "YHG123656

  data: first_swt(1) type c.
* ---------------------------------

*d IF sy-ucomm EQ 'CSPR'.                         "HGB020150"note 351902
  if sv_ucomm eq 'CSPR'.                                   "note 351902
    read table prrsv_tb                                     "HGB020150
      with key lcl_blcnm.                                   "HGB020150

    reserve prrsv_tb-maxln lines.                           "HGB020150
  endif.                                                    "HGB020150

*  Kennzeichen 'erste Zeile ausgeben' setzen
  first_swt = 'x'.

*  bis zum St. Nimmerleinstag
  do.
*     Zeile ausgeben
    perform write_line
*del     USING 'SAPCSMLVM         '                           "YHG137424
       using list_id                                        "YHG137424
             act_profil
             lcl_blcnm
             first_swt
             ' '.

*     ?Ist die auszugebende Zeile leer (und sitzt BLANK-LINES OFF!!)
*     nein
    if lnmpt_flg is initial.
*        ?sollen die ausgegebenen Zeilen (weiter-) gezaehlt werden
*        ja
      if not lcl_lncnt is initial.
*           Zeilenzaehler um eins erhoehen
        blclns_cnt = blclns_cnt + 1 .
      endif.

*        ?wird gerade gedruckt
*        nein
*d       IF sy-ucomm NE 'CSPR'.                             "note 351902
      if sv_ucomm ne 'CSPR'.                             "note 351902
*           gib die Rahmenstriche aus
        write 1 sy-vline.
        write at siz_linpf sy-vline.

*           ... und - wenn gewuenscht -
        if not lcl_hidef is initial.                        "YHG123656
*              ... versorge den HIDE-Bereich
          perform hide_routine_01.
        endif.                                              "YHG123656

        if     lcl_blcnm eq 'ITEM_M            '            "HGB084505
           and     mpos_line  is initial                    "HGB084505
           and not first_swt  is initial                    "HGB084505
           and not matpos_flg is initial                    "HGB084505
           and matcat-matnr   eq pm_mtnrv.                  "HGB084505

          mpos_page = syst-pagno.                           "HGB084505
          mpos_line = syst-linno - nbr_hdrlns - 2.          "HGB084505

          if mpos_line < 1.                                 "HGB084505
            mpos_line = 1.                                  "HGB084505
          endif.                                            "HGB084505
        endif.                                              "HGB084505
      else.                                                 "YHG140031
        if sy-colno = 1.                                    "YHG140031
          skip.                                             "YHG140031
        endif.                                              "YHG140031
      endif.
    endif.

*     ?sitzt EndOfBlock-Kennzeichen
*     ja
    if not eoblc is initial.
*        ... dann Schleife beenden
      exit.
    endif.

*     Kennzeichen 'erste Zeile ausgeben' zuruecknehmen
    clear: first_swt.
  enddo.
endform.                    "write_block


*eject
*---------------------------------------------------------------------*
*        WRITE_LINE                                                   *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
form write_line                                             "YHG133914
   using lcl_lstid
         lcl_profl
         lcl_blcnm
         lcl_first
         lcl_nline.

  data: val_shift like sy-cucol.


* ---------------------------------
*  ?wenn nicht gedruckt wird, ...
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
  if sv_ucomm ne 'CSPR'.                                   "note 351902
*     ... Blockausgabe um eine Stelle nach rechts verschieben
    val_shift = 1 .
*  sonst ...
  else.
*     ... nicht
    clear: val_shift.
  endif.

*  neue Zeile
  new-line.

*  ?wird gerade gedruckt
*  nein
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
  if sv_ucomm ne 'CSPR'.                                   "note 351902
*     Leerzeile in Profillaenge + 2 ausgeben
    write at 2(sav_prfsz) ecfld.
**  ja, es wird gedruckt
*   ELSE.
**     Hintergrundfarben etc. ausschalten
*      FORMAT RESET.
  endif.

*  Zeile endgueltig ausgeben
  call function 'CS_VLIST_BLOCK_PROCESSING'
    exporting
      blcnm        = lcl_blcnm
      lstid        = lcl_lstid
      profl        = lcl_profl
      first        = lcl_first
      rshift       = val_shift
      newline      = lcl_nline
    importing
      eoblc        = eoblc
      lnmpt        = lnmpt_flg
    tables
      watab        = watab
    exceptions
      call_invalid = 4.
endform.                    "write_line


*. Here begins ALV section ............................       "HGA246532
form alv_dsp_sel_dsp.
*...................................

  data:
    sel_fields_tb    type slis_t_fieldcat_alv,
    wa_sel_fields_tb type slis_fieldcat_alv.

  data:
    alvlo_sel type slis_layout_alv.
*....................................

  perform alv_dsp_sel_prep.

  perform alv_evnt_tb_prep
    using
      'B'
      alv_evnt_tb_pfxt.

  wa_sel_fields_tb-fieldname = 'TEXT'.
  wa_sel_fields_tb-outputlen = 30.
  wa_sel_fields_tb-col_pos   = 1.
  append wa_sel_fields_tb to sel_fields_tb.

  wa_sel_fields_tb-fieldname = 'WERT'.
  wa_sel_fields_tb-outputlen = 32.
  wa_sel_fields_tb-col_pos   = 2.
  append wa_sel_fields_tb to sel_fields_tb.

  write text-050 to alvlo_sel-window_titlebar.

  alvvr-handle = '2'.                                         "note 877609
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = report_name
      is_layout               = alvlo_sel
      i_save                  = ' '
      is_variant              = alvvr                              "note 877609
      it_fieldcat             = sel_fields_tb
      it_events               = alv_evnt_tb_pfxt
      i_screen_start_column   = 7
      i_screen_start_line     = 8
      i_screen_end_column     = 71
      i_screen_end_line       = 16
    importing
      e_exit_caused_by_caller = exit_by_caller
      es_exit_caused_by_user  = exit_by_user
    tables
      t_outtab                = dsp_sel
    exceptions
      program_error           = 1
      others                  = 2.

  if sy-subrc = 0.
    if exit_by_caller = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    else.
*     User left list via F3, F12 or F15
      if exit_by_user-back = 'X'.       "F3
*       <do_something>.
      else.
        if exit_by_user-exit = 'X'.     "F15
*         <do_something>.
        else.
          if exit_by_user-cancel = 'X'. "F12
*           <do_something>.
          else.
*           should not occur!
*           <do_Abnormal_End>.
          endif.
        endif.
      endif.
    endif.
  else.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  endif.
endform. "alv_dsp_sel_dsp


*&---------------------------------------------------------------------*
*&      Form  alv_dsp_sel_prep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form alv_dsp_sel_prep.
*...................................

  check dsp_sel[] is initial.

  read textpool sy-repid into txt_sel.

  call function 'RS_REFRESH_FROM_SELECTOPTIONS'
    exporting
      curr_report     = report_name
    tables
      selection_table = inp_sel
    exceptions
      not_found       = 01
      no_report       = 02.

  loop at inp_sel
    where selname ne 'PM_HEMNG'
      and selname ne 'PM_DSPCO'
      and selname ne 'PM_DSPRF'
      and selname ne 'PM_LTEXT'
      and selname ne 'PM_PRPRF'
      and selname ne 'PM_UPSAZ'.

    loop at txt_sel
*d    WHERE id+1 EQ inp_sel-selname.                                 "uc
      where key eq inp_sel-selname.                                  "uc

*d    ASSIGN (txt_sel-id+1) TO <pm_name>.                            "uc
      assign (txt_sel-key) to <pm_name>.                             "uc
      if not <pm_name> is initial.
*d      dsp_sel-text = txt_sel-text+8.                               "uc
        dsp_sel-text = txt_sel-entry.                                "uc

        dsp_sel-wert = inp_sel-low.

        if inp_sel-selname eq 'PM_DATUV'.
          clear:
            dsp_sel-wert.
          write pm_datuv to dsp_sel-wert.
        endif.

        if    inp_sel-selname eq 'PM_EMENG'
          and not pm_emeng is initial.

          clear:
            dsp_sel-wert.
          write pm_emeng to dsp_sel-wert decimals 3.

          while dsp_sel-wert(1) eq space.
            shift dsp_sel-wert left.
          endwhile.
        endif.

        append dsp_sel.
      endif.
    endloop.
  endloop.

  sort dsp_sel by text.

  check not ecm_sl[] is initial.

  refresh:
    dsp_sel2.

  clear:
    dsp_sel.
  dsp_sel-text = '_'.
  append dsp_sel.

  dsp_sel-text = text-020.
  append dsp_sel.

  loop at ecm_sl into wa_ecm_sl.
    check not wa_ecm_sl-value is initial.
    check not wa_ecm_sl-fieldname eq 'DATE_LO'.

    dsp_sel2-text = wa_ecm_sl-text.

    dsp_sel2-wert = wa_ecm_sl-value.
    append dsp_sel2.
  endloop.

  append lines of dsp_sel2 to dsp_sel.
endform. "alv_dsp_sel_prep


*&---------------------------------------------------------------------*
*&      Form  alv_evnt_tb_prep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EVENT_SPEC text
*      -->EVENT_TB   text
*----------------------------------------------------------------------*
form alv_evnt_tb_prep
  using
    event_spec type c
    event_tb type slis_t_event.
*..................................

  data:
    wa_event_tb type slis_alv_event.
*..................................

  check event_tb[] is initial.

  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type = 0
    importing
      et_events   = event_tb.

  case event_spec.
*   complete
    when 'A'.
*      read table event_tb
*        with key name = slis_ev_top_of_page
*        into wa_event_tb.
*
*      if sy-subrc = 0.
*        wa_event_tb-form = 'ALV_TOP_OF_PAGE'.
*        append wa_event_tb to event_tb.
*      endif.


      read table event_tb
        with key name = slis_ev_user_command
        into wa_event_tb.

      if sy-subrc = 0.
        wa_event_tb-form = 'ALV_USER_COMMAND'.
        append wa_event_tb to event_tb.
      endif.


      read table event_tb
        with key name = slis_ev_pf_status_set
        into wa_event_tb.

      if sy-subrc = 0.
        wa_event_tb-form = 'ALV_PF_STATUS_SET_MAIN'.
        append wa_event_tb to event_tb.
      endif.

*   PF EXIT only
    when 'B'.
      read table event_tb
        with key name = slis_ev_pf_status_set
        into wa_event_tb.

      if sy-subrc = 0.
        wa_event_tb-form = 'ALV_PF_STATUS_SET_EXIT'.
        append wa_event_tb to event_tb.
      endif.
  endcase.
endform. "alv_evnt_tb_prep


*&---------------------------------------------------------------------*
*&      Form  alv_pf_status_set_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
form alv_pf_status_set_exit
  using
    rt_extab type slis_t_extab.

  set pf-status 'SNN1'
    excluding rt_extab.
endform. "alv_pf_status_set_exit


*&---------------------------------------------------------------------*
*&      Form  alv_pf_status_set_main
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
form alv_pf_status_set_main
  using
    rt_extab type slis_t_extab.
*.........................................

  data:
    wa_rt_extab like rsmpe-func.
*.........................................
* ex sort
  wa_rt_extab = '&OUP'.
  append wa_rt_extab to rt_extab.
  wa_rt_extab = '&ODN'.
  append wa_rt_extab to rt_extab.

*** Enhancement for controlling the enhanced pushbutton
  try.
      get badi lp_badi.
    catch cx_badi_not_implemented .
  endtry.
  try.
      call badi lp_badi->control_menu_enhancement
        changing
          ct_extab = rt_extab.
    catch cx_badi_initial_reference.
  endtry.
****

  set pf-status 'S121_ALV'
    excluding rt_extab.
endform. "alv_pf_status_set_main


*&---------------------------------------------------------------------*
*&      Form  alv_stb_prep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form alv_stb_prep.
*..................................
  clear:
    alv_stb,
    stb_orig,
    stb_add.

  stb_orig = stb.

  if stb-mngko >= max_num.
    stb_add-ovfls = ueberl_kz.
  else.
    if stb-mngko <= min_num.
      stb_add-ovfls = ueberl_kz.
    else.
      clear: stb_add-ovfls.
    endif.
  endif.

  if not stb-xtlnr is initial.
    stb_add-bomfl = b_flag.
  endif.

  if    not stb-knobj is initial
     or not stb-class is initial
     or not stb-kzclb is initial.

    stb_add-knofl = 'X'.
  endif.

  clear:
    stb_add-dobjt,
    stb_add-objic.

  case stb-objty.
    when otyp_mat.
      write: stb_orig-idnrk to ecfld.
*     Begin of Note_1553704
*     stb_add-objic = '@A6@'.
      if not stb_orig-hdnfo is initial.
        call function 'ICON_CREATE'
          exporting
            name                  = icon_bom
            info                  = text-074
          importing
            result                = stb_add-objic
          exceptions
            icon_not_found        = 1
            outputfield_too_short = 2
            others                = 3.
      else.

        call function 'ICON_CREATE'
          exporting
            name                  = icon_material
            info                  = text-070
          importing
            result                = stb_add-objic
          exceptions
            icon_not_found        = 1
            outputfield_too_short = 2
            others                = 3.
      endif.
*     End of Note_1553704

    when 'M'.
      write: stb_orig-idnrk to ecfld.
*     Begin of Note_1553704
*     stb_add-objic = '@A6@'.
      if not stb_orig-hdnfo is initial.
        call function 'ICON_CREATE'
          exporting
            name                  = icon_bom
            info                  = text-074
          importing
            result                = stb_add-objic
          exceptions
            icon_not_found        = 1
            outputfield_too_short = 2
            others                = 3.
      else.

        call function 'ICON_CREATE'
          exporting
            name                  = icon_material
            info                  = text-070
          importing
            result                = stb_add-objic
          exceptions
            icon_not_found        = 1
            outputfield_too_short = 2
            others                = 3.
      endif.
*     End of Note_1553704

    when otyp_noo.
      write: stb_orig-potx1 to ecfld.
*     Begin of Note_1553704
*     stb_add-objic = '@0Q@'.
      call function 'ICON_CREATE'
        exporting
          name                  = icon_change_text
          info                  = text-073
        importing
          result                = stb_add-objic
        exceptions
          icon_not_found        = 1
          outputfield_too_short = 2
          others                = 3.
*     End of Note_1553704

    when otyp_doc.
      write stb_orig-doknr to ecfld.                        "note 489354

      if ecfld cp '*# '. endif.                             "note 489354
      sy-fdpos = sy-fdpos + 1.                              "note 489354

      concatenate
*d      stb_orig-doknr                                      "note 489354
        stb_orig-dokar
        stb_orig-doktl
        stb_orig-dokvr
*d      INTO ecfld                                          "note 489354
        into ecfld+sy-fdpos                                 "note 489354
        separated by space.

*     Begin of Note_1553704
*     stb_add-objic = '@AR@'.
      call function 'ICON_CREATE'
        exporting
          name                  = icon_document
          info                  = text-071
        importing
          result                = stb_add-objic
        exceptions
          icon_not_found        = 1
          outputfield_too_short = 2
          others                = 3.
*     End of Note_1553704

    when otyp_kla.
      concatenate
        stb_orig-class
        stb_orig-klart
        into ecfld
        separated by space.

*     Begin of Note_1553704
*     stb_add-objic = '@7C@'.
      call function 'ICON_CREATE'
        exporting
          name                  = icon_oo_class
          info                  = text-072
        importing
          result                = stb_add-objic
        exceptions
          icon_not_found        = 1
          outputfield_too_short = 2
          others                = 3.
*     End of Note_1553704

    when otyp_ntm.
      write: stb_orig-intrm to ecfld.

    when others.
  endcase.

*d CONDENSE ecfld.                                          "note 515408
  stb_add-dobjt = ecfld(40).
  clear: ecfld.

  write stb_orig-stufe to stb_add-dstuf no-sign.
  perform stufe_aufbereiten.
  write anz_stufe to stb_add-dglvl no-sign.

*d  MOVE-CORRESPONDING stb_orig TO alv_stb.                 "note 331962
  move-corresponding stb_add to alv_stb.
  move-corresponding stb_orig to alv_stb.                   "note 331962

* Phantom item adjusted for ALV filtering                   "Note 1327742
  if not alv_stb-dumps is initial.                          "Note 1327742
    translate alv_stb-dumps to upper case. "#EC TRANSLANG  "Note 1327742
  endif.                                                    "Note 1327742

  append alv_stb.
endform. "alv_stb_prep


*&---------------------------------------------------------------------*
*&      Form  alv_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form alv_top_of_page.
*.....................................

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = alv_top_tb.
endform. "alv_top_of_page


*&---------------------------------------------------------------------*
*&      Form  alv_top_tb_prep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TOP_TB     text
*----------------------------------------------------------------------*
form alv_top_tb_prep
  using
    top_tb type slis_t_listheader.
*......................................

  data:
    wa_top_tb type slis_listheader.

  data:
    lkl_matnr like mara-matnr.                                "note 363714
*ENHANCEMENT-POINT RCS12001_L2 SPOTS ES_RCS12001 STATIC .

*......................................

* CLEAR wa_top_tb.
* wa_top_tb-typ  = 'H'.
* CONCATENATE
*   text-001
*   text-002
*   INTO wa_top_tb-info
*   SEPARATED BY space(1).
* APPEND wa_top_tb TO top_tb.

  clear wa_top_tb.

  if    not pm_vbeln is initial
    and pm_pspnr is initial.

    wa_top_tb-typ  = 'S'.
    ecfld    = text-025.
    ecfld+13 = '/'.
    ecfld+14 = text-026.
    wa_top_tb-key  = ecfld(19).
    condense wa_top_tb-key.
    clear: ecfld.

    concatenate
      pm_vbeln
      pm_vbpos
      into wa_top_tb-info
      separated by ' / '.

    append wa_top_tb to top_tb.
  endif.

  if    not pm_pspnr is initial
    and pm_vbeln is initial.

    wa_top_tb-typ  = 'S'.
    ecfld = text-028.
    ecfld+13 = '/'.
    ecfld+14 = text-029.
    wa_top_tb-key  = ecfld(19).
    condense wa_top_tb-key no-gaps.
    clear: ecfld.

    write pm_pspnr to wa_top_tb-info.
    wa_top_tb-info+25 = '/'.
*ENHANCEMENT-SECTION RCS12001_L3 SPOTS ES_RCS12001 .
    wa_top_tb-info+27 = pm_mtnrv.
*END-ENHANCEMENT-SECTION.
    condense wa_top_tb-info.
    append wa_top_tb to top_tb.
  endif.

  if wa_top_tb is initial.
    wa_top_tb-typ  = 'S'.
    ecfld = text-010.
    wa_top_tb-key  = ecfld(11).
    clear: ecfld.
*ENHANCEMENT-SECTION     RCS12001_L1 SPOTS ES_RCS12001.
    write pm_mtnrv to lkl_matnr.                            "note 363714
*d  wa_top_tb-info = pm_mtnrv.                              "note 363714
    wa_top_tb-info = lkl_matnr.                             "note 363714
*END-ENHANCEMENT-SECTION.
    append wa_top_tb to top_tb.
  endif.

  clear wa_top_tb.
  wa_top_tb-typ  = 'S'.
  ecfld    = text-005.
  ecfld+5  = '/'.
  ecfld+6  = text-006.
  ecfld+11 = '/'.
  ecfld+12 = text-014.
  wa_top_tb-key  = ecfld(17).
  condense wa_top_tb-key no-gaps.
  clear: ecfld.

  concatenate
    selpool-werks
    selpool-stlan
    selpool-stlal
    into wa_top_tb-info
    separated by ' / '.
  append wa_top_tb to top_tb.

  clear wa_top_tb.
  wa_top_tb-typ  = 'S'.
  wa_top_tb-key = text-017.
  condense wa_top_tb-key.
  wa_top_tb-info = selpool-maktx.
  append wa_top_tb to top_tb.

  clear wa_top_tb.
  wa_top_tb-typ  = 'S'.
  ecfld = text-013.
  ecfld+13 = ' ('.
* ecfld+15 = selpool-bmein.                                 "NOTE_975832
  write selpool-bmein to ecfld+15(3).                       "NOTE_975832
  ecfld+18 = ')'.
  wa_top_tb-key  = ecfld(19).
  clear: ecfld.

*d wa_top_tb-info = selpool-bmeng.                          "note 317957
  write selpool-bmeng to wa_top_tb-info .                   "note 317957
  condense wa_top_tb-info.
  append wa_top_tb to top_tb.

* ^_NOTE_811372

  clear wa_top_tb.
  wa_top_tb-typ = 'S'.
  ecfld = text-009.
  ecfld+11 = ' ('.
* ecfld+13 = selpool-emgme.                                 "NOTE_975832
  write selpool-emgme to ecfld+13(3).                       "NOTE_975832
  ecfld+16 = ')'.
  wa_top_tb-key  = ecfld(17).
  clear: ecfld.
  write selpool-emeng to wa_top_tb-info unit selpool-bmein.
  condense wa_top_tb-info.
  append wa_top_tb to top_tb.

*v_NOTE_811372

  clear wa_top_tb.
  wa_top_tb-typ  = 'S'.
* so it looks better
  append wa_top_tb to top_tb.
endform. "alv_top_tb_prep


*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*      -->I_SELFIELD text
*----------------------------------------------------------------------*
form alv_user_command
  using i_ucomm like sy-ucomm
    i_selfield type slis_selfield.
*.......................................

  case i_ucomm.
    when 'ANMS'.                                            "HGA020150
      leave to transaction sy-tcode.                        "HGA020150

    when 'CSAP' or '&IC1'.
      sort stb by index.

      read table alv_stb index i_selfield-tabindex.
      if not alv_stb-index is initial.
        read table stb index alv_stb-index.
        read table matcat index stb-ttidx.
      else.
        clear:
           stb,
           matcat.
      endif.

      perform position_anzeigen.

    when 'CSAO'.
      sort stb by index.

      read table alv_stb index i_selfield-tabindex.
      if not alv_stb-index is initial.
        read table stb index alv_stb-index.
      else.
        clear:
           stb.
      endif.

      perform objekt_anzeigen.

    when 'CSWU'.
      sort stb by index.

      read table alv_stb index i_selfield-tabindex.
      if not alv_stb-index is initial.
        read table stb index alv_stb-index.
      else.
        clear:
           stb.
      endif.

      perform verwendung_anzeigen.

    when 'CSSL'.
      perform alv_dsp_sel_dsp.

    when '+ENH1'.

*** getting the input entered on screen
      call function 'RS_REFRESH_FROM_SELECTOPTIONS'
        exporting
          curr_report     = report_name
        tables
          selection_table = lt_sel_tab
        exceptions
          not_found       = 1
          no_report       = 2.

*** Enhancement for exporting the screen input
      try.
          get badi lp_badi.
        catch cx_badi_not_implemented .
      endtry.

      try.
          call badi lp_badi->get_dynamic_bom_data
            exporting
              it_sel_tab = lt_sel_tab
            importing
              es_message = ls_message.
          if ls_message is not initial.
            message id ls_message-msgid type ls_message-msgty number ls_message-msgno
             with ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
          endif.
        catch cx_badi_initial_reference.
      endtry.

  endcase.
endform. "alv_user_command


*&---------------------------------------------------------------------*
*&      Form  cs12_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form cs12_alv.
*...................................
*
*  loop at stb.
*    stb-index = sy-tabix.
*    modify stb.
*
*    perform alv_stb_prep.
*  endloop.

  perform stb_fields_tb_prep.

  alvlo_stb-info_fieldname = 'INFO'.

  alvvr-handle = '1'.                                         "note 877609
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = report_name
      i_structure_name        = 'STPOX_ALV'
      is_layout               = alvlo_stb
      i_save                  = alvvr_sav_all
      is_variant              = alvvr
      it_events               = alv_evnt_tb_cmpl
      it_fieldcat             = stb_fields_tb
    importing
      e_exit_caused_by_caller = exit_by_caller
      es_exit_caused_by_user  = exit_by_user
    tables
      t_outtab                = alv_stb
    exceptions
      program_error           = 1
      others                  = 2.

  if sy-subrc = 0.
    if exit_by_caller = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    else.
*     User left list via F3, F12 or F15
      if exit_by_user-back = 'X'.       "F3
*       <do_something>.
      else.
        if exit_by_user-exit = 'X'.     "F15
*         <do_something>.
        else.
          if exit_by_user-cancel = 'X'. "F12
*           <do_something>.
          else.
*           should not occur!
*           <do_Abnormal_End>.
          endif.
        endif.
      endif.
    endif.
  else.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  endif.

  if sy-subrc <> 0.
    message s513 with 'E: '.
    exit.
  endif.
endform.                                                    "cs13_alv


*&---------------------------------------------------------------------*
*&      Form  stb_fields_tb_prep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form stb_fields_tb_prep.
*.....................................

*dCALL FUNCTION 'GET_FIELDTAB'                                "uc 070302
*d  EXPORTING                                                 "uc 070302
*d    langu    = sy-langu                                     "uc 070302
*d    tabname  = 'STPOX_ALV'                                  "uc 070302
*d    withtext = ' '                                          "uc 070302
*d    only     = 'T'                                          "uc 070302
*d  TABLES                                                    "uc 070302
*d    fieldtab = ftab                                         "uc 070302
*d  EXCEPTIONS                                                "uc 070302
*d    OTHERS   = 1.                                           "uc 070302

  call function 'DDIF_FIELDINFO_GET'                        "uc 070302
    exporting                                               "uc 070302
      langu     = sy-langu                       "uc 070302
      tabname   = 'STPOX_ALV'                    "uc 070302
*     UCLEN     = '01'                             "uc 070302
    tables                                                  "uc 070302
      dfies_tab = ftab                           "uc 070302
    exceptions                                              "uc 070302
      others    = 1.                             "uc 070302

  loop at ftab.
    clear: wa_stb_fields_tb.

    case ftab-fieldname.
      when 'DGLVL'.
        wa_stb_fields_tb-fieldname = 'DGLVL'.
        wa_stb_fields_tb-col_pos   =  1.
        wa_stb_fields_tb-fix_column = 'X' .
        wa_stb_fields_tb-outputlen = 11.
        wa_stb_fields_tb-just      = 'L' .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'POSNR'.
        wa_stb_fields_tb-fieldname = 'POSNR'.
        wa_stb_fields_tb-col_pos   =  2.
        wa_stb_fields_tb-fix_column =  'X' .
        wa_stb_fields_tb-outputlen = 4 .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'OBJIC'.
        wa_stb_fields_tb-fieldname = 'OBJIC'.
        wa_stb_fields_tb-col_pos   =  3.
        wa_stb_fields_tb-fix_column =  'X' .
        wa_stb_fields_tb-outputlen = 3 .
        wa_stb_fields_tb-icon       =  'X' .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'DOBJT'.
        wa_stb_fields_tb-fieldname = 'DOBJT'.
        wa_stb_fields_tb-col_pos   =  4.
        wa_stb_fields_tb-fix_column =  'X' .
        wa_stb_fields_tb-outputlen = 23 .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'OJTXP'.
        wa_stb_fields_tb-fieldname = 'OJTXP'.
        wa_stb_fields_tb-col_pos   =  5.
        wa_stb_fields_tb-outputlen = 19.
        append wa_stb_fields_tb to stb_fields_tb.

      when 'OVFLS'.
        wa_stb_fields_tb-fieldname = 'OVFLS'.
        wa_stb_fields_tb-col_pos   = 6.
        wa_stb_fields_tb-outputlen = 3 .
        wa_stb_fields_tb-just      = 'R' .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'MNGKO'.
        wa_stb_fields_tb-fieldname = 'MNGKO'.
        wa_stb_fields_tb-col_pos   = 7.
        wa_stb_fields_tb-outputlen = 18.
        wa_stb_fields_tb-no_sum    = 'X'.
        wa_stb_fields_tb-no_zero   = 'X'.
        append wa_stb_fields_tb to stb_fields_tb.

      when 'MEINS'.
        wa_stb_fields_tb-fieldname = 'MEINS'.
        wa_stb_fields_tb-col_pos   = 8.
        wa_stb_fields_tb-outputlen = 3 .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'POSTP'.
        wa_stb_fields_tb-fieldname = 'POSTP'.
        wa_stb_fields_tb-col_pos   = 9.
        wa_stb_fields_tb-outputlen = 3 .
        wa_stb_fields_tb-just      = 'C' .
        append wa_stb_fields_tb to stb_fields_tb.

      when 'AUSNM'.
        wa_stb_fields_tb-fieldname = 'AUSNM'.
        wa_stb_fields_tb-col_pos   = 10.
        wa_stb_fields_tb-outputlen = 5 .
        append wa_stb_fields_tb to stb_fields_tb.

      when others.
        wa_stb_fields_tb-fieldname = ftab-fieldname.
        wa_stb_fields_tb-no_out    = 'X'.
        wa_stb_fields_tb-no_sum    = 'X'.
        append wa_stb_fields_tb to stb_fields_tb.

    endcase.
  endloop.
endform. "stb_fields_tb_prep
*. Here ends ALV section ..............................       "HGA246532
*eject
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
