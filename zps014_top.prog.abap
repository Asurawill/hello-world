*&---------------------------------------------------------------------*
*&  包含                ZPS014_TOP
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES: PRPS,T001W,MARA .

************************************************************************
* Type Declaration
************************************************************************
TYPES: BEGIN OF TY_ALV ,
         SEL       TYPE C,
         NUM       TYPE I,
         PSPID     TYPE PROJ-PSPID,
         POSID     TYPE PRPS-POSID,
         POST1     TYPE PRPS-POST1,
         WERKS     TYPE PRPS-WERKS,
         MATNR     TYPE MARA-MATNR,
         MAKTX     TYPE MAKT-MAKTX,
         BESKZ     TYPE MARC-BESKZ,
         MATKL     TYPE MARA-MATKL,
         WGBEZ60   TYPE T023T-WGBEZ60,
         MEINS     TYPE MARA-MEINS,
         GPREIS_HT TYPE CKIS-GPREIS,
         MENGE_HT  TYPE CKIS-MENGE,
         GPREIS_YS TYPE CKIS-GPREIS,
         MENGE_YS  TYPE CKIS-MENGE,
         BDMNG     TYPE RESB-BDMNG,
         MENGE_PR  TYPE EBAN-MENGE,
         EBELN     TYPE EKKO-EBELN,
         EBELP     TYPE EKPO-EBELP,
         TXZ01     TYPE EKPO-TXZ01,
         NETPR     TYPE EKPO-NETPR,
         MENGE_PO  TYPE EKPO-MENGE,
         MENGE_RK  TYPE EKBE-MENGE,
         MENGE_FH  TYPE MSEG-MENGE,
         " 颜色
         CLR(4)    TYPE C,
         LBKUM     TYPE QBEW-lbkum,
       END OF TY_ALV .

************************************************************************
* Internal Table & WorkArea
************************************************************************
DATA: GT_ALV TYPE TABLE OF TY_ALV,
      GS_ALV TYPE TY_ALV.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA: GS_LAYOUT TYPE LVC_S_LAYO,
      GS_LVC    TYPE LVC_S_FCAT,
      GT_LVC    TYPE LVC_T_FCAT.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  GS_LVC-fieldname = &1.
  GS_LVC-coltext   = &2.
  GS_LVC-scrtext_l = &2.
  GS_LVC-scrtext_m = &2.
  GS_LVC-scrtext_s = &2.
  GS_LVC-reptext   = &2.
  GS_LVC-outputlen = &3.
  IF &4 = 'X'.
    GS_LVC-key = 'X'.
  ENDIF.
  GS_LVC-checkbox = &5.
  GS_LVC-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GS_LVC-hotspot   = &7.
  GS_LVC-ref_field = &9.
  GS_LVC-ref_table = &8.

*IF gw_lvc-fieldname = 'PROJK'.
*   gw_lvc-NO_ZERO = 'X'.
*ENDIF.
  APPEND GS_LVC TO gt_lvc.
  CLEAR GS_LVC.
END-OF-DEFINITION.
