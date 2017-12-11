*&---------------------------------------------------------------------*
*&  包含                ZXM06U36
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_TRTYP)
*"             VALUE(I_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI
*"             VALUE(I_BSTYP) LIKE  EKKO-BSTYP
*"             VALUE(I_NO_SCREEN)
*"             VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"             VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"             VALUE(I_KEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_AEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_REKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_EKKO_OLD) LIKE  EKKO STRUCTURE  EKKO OPTIONAL
*"             VALUE(I_VORGA) LIKE  T160-VORGA
*"       TABLES
*"              TEKPO STRUCTURE  BEKPO OPTIONAL
*"              TEKET STRUCTURE  BEKET OPTIONAL
*"              TEKKN STRUCTURE  EKKNU OPTIONAL
*"              TKOMV STRUCTURE  KOMV OPTIONAL
*"----------------------------------------------------------------------


EKKO_CI = I_CI_EKKO.

G_TRTYP_1 = I_TRTYP .
