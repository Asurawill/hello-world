*eject
* 4.7
* HGA087938 090102 mehrzeilige Literale (div.)
* 4.6B
* MBB038421 240699 Stack-Verwaltung
* MBA038421 240699 Emeng ausblenden in CSKx
* 4.5A
* MBA030424 090798 Alternative auf 'Sicht' hinzufügen
* HGA084505 030398 neue Gueltigkeit
* MBA087033 020398 Auflösung KundenauftragsStl
* 4.0A
* HGA025100 230797 Profilvorbelegung / VList Performance
* 3.0E
* HGD072824 170596 Ruecksprung bei call transaction
* 3.0D
* HGA072824 150596 Werksberechtigungspruefung
* 3.0A
* YHG147318 020895 CSIO-Memoryverwaltung f. MC29L-Transaktionen
* YHG126159 230595 Umleitung auf Sichtdynpro
* 3.0
* YHG130758 270295 Umstellung Sichtpopups
* YHG118802 080295 werkslos
* YHG101328 041094 CSBOMEX skip first screen
* 2.2
* YHG087082 070694 Anz. Revisionsstand TCC08-abh.
* YHG077295 050494 MatVerwendung ueber Klassen und Folgeaktionen
* YHG000420 220394 Ueberarbeitung der Einstiegsdynpros zur StlAnz
* YHG000136 020394 Revision Level
* 2.1
* YHG048259 160993 Alternativenbestimmung an Disponeuerungen angepasst
* YHG037359 160793 Massnahmen zur Performanceverbesserung
***********************************************************************
PROGRAM SAPMC29L MESSAGE-ID 29.
***********************************************************************
*        M C 2 9 L T O P           PROGRAMM    S A P M C 2 9 L        *
***********************************************************************
*        D A T E N  -  Definitionen                                   *
***********************************************************************
*---------------------------------------------------------------------*
*        DB-Tabellen und Strukturen                                   *
*---------------------------------------------------------------------*
TABLES:
   AENR,                                                      "YHG000136
   MARC,                                                      "YHG048259
   MTCOM,
   MC29M,
   MC29S,
   MTCOR,
   USR02,                                                     "HGA025100
   RC29L.


*---------------------------------------------------------------------*
*        ATAB-Tabellen                                                *
*---------------------------------------------------------------------*
TABLES:
   T001W,                                                     "YHG118802
   TC04,
   TCS41,
   TCC08,                                                     "YHG087082
   TCS03,
   TCSVL,                                                     "HGA025100
   TCSPR.


*---------------------------------------------------------------------*
*        interne Tabellen                                             *
*---------------------------------------------------------------------*
DATA:
   BEGIN OF MASTB OCCURS 0.
      INCLUDE STRUCTURE MASTB.
DATA:
   END OF MASTB.

DATA: BEGIN OF STZUB OCCURS 0.
         INCLUDE STRUCTURE STZUB.
DATA: END OF STZUB.

DATA: BEGIN OF TCS41B OCCURS 0.
         INCLUDE STRUCTURE TCS41.
DATA: END OF TCS41B.

DATA:
   BEGIN OF DUMMYTAB OCCURS 0,
      DUMMY1,
   END OF DUMMYTAB.

DATA : BEGIN OF DYNPTAB OCCURS 2.                             "YHG118802
         INCLUDE STRUCTURE DYNPREAD.                          "YHG118802
DATA : END OF DYNPTAB.                                        "YHG118802


*---------------------------------------------------------------------*
*        spezielle Export- Importbereiche                             *
*---------------------------------------------------------------------*
DATA: BEGIN OF CSBOMEX.
         INCLUDE STRUCTURE CSBOMEX.
DATA: END OF CSBOMEX.

DATA: BEGIN OF SAV_POSFLAGS,
         SANKO LIKE RC29L-SANKO,
         SANFE LIKE RC29L-SANFE,
         SANKA LIKE RC29L-SANKA,
         SANIN LIKE RC29L-SANIN,
         SANVS LIKE RC29L-SANVS,
         RVREL LIKE RC29L-RVREL,
         ERSSL LIKE RC29L-ERSSL,
         ERSKZ LIKE RC29L-ERSKZ,
         BESSL LIKE RC29L-BESSL,
         BEIKZ LIKE RC29L-BEIKZ,
         SCHGT LIKE RC29L-SCHGT,
      END OF SAV_POSFLAGS.

DATA: BEGIN OF SAV_RC29L.                                     "YHG000420
         INCLUDE STRUCTURE RC29L.                             "YHG000420
DATA: END OF SAV_RC29L.                                       "YHG000420

DATA: BEGIN OF SAV_TCSPR.                                     "YHG077295
         INCLUDE STRUCTURE TCSPR.                             "YHG077295
DATA: END OF SAV_TCSPR.                                       "YHG077295

DATA: MEM_OPTN LIKE CSDATA-XFELD.                             "YHG147318

*---------------------------------------------------------------------*
*        interne Felder                                               *
*---------------------------------------------------------------------*
DATA:
   CSN1_DYNNR LIKE SYST-DYNNR,                                "YHG125422
   OK-CODE(4)      TYPE C,
   RET_CODE        LIKE SY-SUBRC,
*  modif_wert(3)   type n,
*del MODIF_KOPIE(32) TYPE C,                                  "YHG037359
*del MODIF_KOPIE(33) TYPE C,                       "YHG037359 "YHG000136
*del MODIF_KOPIE(37) TYPE C,                       "YHG000136 "YHG130758
*  modif_kopie(49) TYPE c,                          "YHG130758 MBA030423
   MODIF_KOPIE(55) TYPE C,                                    "MBA030423
*  first screen already skipped
   FSAS_FLAG(1)    TYPE C,                                    "YHG101328
*  calling aclas
   CAL_ACLAS LIKE TAPPL-APPLCLASS,                            "HGD072824
*  View  dynpro already seen
   VDAS_FLAG(1)    TYPE C,                                    "YHG126159
   CRS_FELD(5)     TYPE C.

DATA:
   SUBM_FLAG(1)   TYPE C VALUE 'x',
   DSP_ACTVT(2)   TYPE C VALUE '03',                          "HGA072824

*  Pflegebewertung existiert
   ANR_TECHS LIKE TECS-TECHS,                                 "HGA084505

*  Übergabestrucktur PDM-Stack
   l_object_keyfields like object_keyfields,                  "MBB038421

   g_lst_vbeln like rc29l-vbeln,                            "note 397323
   g_lst_vbpos like rc29l-vbpos,                            "note 397323

*d g_emeng_in_flg type c,                                   "note 397323
   g_emeng_in_flg type c.                         "note 397323"HGA087938



*---------------------------------------------------------------------*
*        Konstanten                                                   *
*---------------------------------------------------------------------*
*del                                                          "YHG037359
*  MODIF_CS11(32) TYPE C VALUE '......-......--........-........',
*  MODIF_CS12(32) TYPE C VALUE '......-......--........-........',
*  MODIF_CS13(32) TYPE C VALUE '......-.................-.......',
*  MODIF_CS15(32) TYPE C VALUE '......-.................-.......'.

*del neue Laenge, erste Stelle Blank                          "YHG037359
*  MODIF_CS11(33) TYPE C VALUE ' ......-......--........-........',
*  MODIF_CS12(33) TYPE C VALUE ' ......-......--........-........',
*  MODIF_CS13(33) TYPE C VALUE ' ......-.................-.......',
*  MODIF_CS15(33) TYPE C VALUE ' ......-.................-.......'.

*neue Laenge                                                  "YHG000136
*                               0....+....1....+....2....+....3....+.
*  MODIF_CS11(35) TYPE C VALUE ' ......-......--........-............',
*  MODIF_CS12(35) TYPE C VALUE ' ......-......--........-............',
*  MODIF_CS13(35) TYPE C VALUE ' ......-.................-...........',
*  MODIF_CS15(35) TYPE C VALUE ' ..................--....-...........'.

*neue Laenge                                                  "YHG130758
*                               0....+....1....+....2....+....3....+....
*  modif_cs11(49) TYPE c VALUE ' ......-......--........-...............
*....+...
*......--.',                                                  "MBA030423


*  modif_cs12(49) TYPE c VALUE ' ......-......--........-...............
*...-..--.',                                                  "MBA030423
*  modif_cs13(49) type c value ' ......-.................-..............
*...-..--.',                                                  "MBA030423
*  modif_cs15(49) TYPE c VALUE ' ..................--....-..............
*......--.',                                                  "MBA030423
*  modif_csd5(49) type c value ' ..................--....-..............
*...-..--.',                                                  "MBA030423
*  modif_csc5(49) TYPE c VALUE ' ..................--....-..............
*...-..--.'.                                                  "MBA087033
*  modif_csc5(49) type c value ' ..................--....-..............
*...-..--.',                                        "MBA087033 MBA030423
*  modif_csk1(49) type c value ' ......-......--........-...............
*......--.',                                        "MBA087033 MBA030423
*  modif_csk2(49) type c value ' ......-......--........-...............
*...-..--.',                                        "MBA087033 MBA030423
*  modif_csk3(49) type c value ' ......-.................-..............
*...-..--.'.                                        "MBA087033 MBA030423


*---------------------------------------------------"Begin ---"HGA087938
* Wieder neue Länge (55)
*                               0....+....1....+....2....+....3....+....
*....+....5....+
*  MODIF_CS11(55) TYPE C VALUE ' ......-......--........-...............
*......--.-',                                                 "MBA030423
*  MODIF_CS12(55) TYPE C VALUE ' ......-......--........-...............
*...-..--.-',                                                  MBA030423
*  MODIF_CS13(55) TYPE C VALUE ' ......-.................-..............
*...-..--.-',                                                 "MBA030423
*  MODIF_CS15(55) TYPE C VALUE ' ..................--....-..............
*......----',                                     "MBA030423"note 415214
*  MODIF_CSD5(55) TYPE C VALUE ' ..................--....-..............
*...-..----',                                     "MBA030423"note 415214
*  MODIF_CSC5(55) TYPE C VALUE ' ..................--....-..............
*...-..----',                                     "MBA030423"note 415214
*  MODIF_CS15(55) TYPE C VALUE ' ..................--....-..............
*......--.-.',                                              "note 415214
*  MODIF_CSD5(55) TYPE C VALUE ' ..................--....-..............
*...-..--.-.',                                              "note 415214
*  MODIF_CSC5(55) TYPE C VALUE ' ..................--....-.............-
*...-..--.--',                                              "note 415214

constants:
   MODIF_CSP1(55) TYPE C VALUE
' .............--........-.....................--..',  "MBxx
   MODIF_CSP2(55) TYPE C VALUE
' .............--........-..................-..--..',
   MODIF_CSP3(55) TYPE C VALUE
' ........................-.................-..--..'.


data:
  begin of MODIF_CS11,
    part1(40) TYPE C VALUE ' ......-......--........-...............',
    part2(15) TYPE C VALUE '......--.-',
  end of MODIF_CS11.

data:
  begin of MODIF_CS12,
    part1(40) TYPE C VALUE ' ......-......--........-...............',
    part2(15) TYPE C VALUE '...-..--.-',
  end of MODIF_CS12.

data:
  begin of MODIF_CS13,
    part1(40) TYPE C VALUE ' ......-.................-..............',
    part2(15) TYPE C VALUE '...-..--.-',
  end of MODIF_CS13.

data:
  begin of MODIF_CS15,
    part1(40) TYPE C VALUE ' ..................--....-..............',
    part2(15) TYPE C VALUE '......--.-.',
  end of MODIF_CS15.

data:
  begin of MODIF_CSD5,
    part1(40) TYPE C VALUE ' ..................--....-..............',
    part2(15) TYPE C VALUE '...-..--.-.',
  end of MODIF_CSD5.

data:
  begin of MODIF_CSC5,
    part1(40) TYPE C VALUE ' ..................--....-.............-',
    part2(15) TYPE C VALUE '...-..--.--',
  end of MODIF_CSC5.

data:
  begin of MODIF_CSK1,
    part1(40) TYPE C VALUE                                 "note 517995
    ' ......-......--........-...............',            "note 517995
    part2(15) TYPE C VALUE '......--..',
  end of MODIF_CSK1.

data:
  begin of MODIF_CSK2,
    part1(40) TYPE C VALUE                                 "note 517995
    ' ......-......--........-...............',            "note 517995
    part2(15) TYPE C VALUE '...-..--..',
  end of MODIF_CSK2.

data:
  begin of MODIF_CSK3,
    part1(40) TYPE C VALUE                                 "note 517995
    ' ......-.................-..............',            "note 517995
    part2(15) TYPE C VALUE '...-..--..',
  end of MODIF_CSK3.
*---------------------------------------------------"End   ---"HGA087938



FIELD-SYMBOLS:
   <MODIF_MARK>.

INCLUDE: CSINCL02.

SELECTION-SCREEN BEGIN OF SCREEN 2100 AS SUBSCREEN.
SELECT-OPTIONS: S_MATNR FOR MARC-MATNR MEMORY ID MAT.
SELECTION-SCREEN END OF SCREEN 2100.

DATA: R_MATNR TYPE RANGE OF MARC-MATNR WITH HEADER LINE.
