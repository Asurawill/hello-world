*&---------------------------------------------------------------------*
*&  包含                ZFI045_TOP
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES: T001,BKPF .

************************************************************************
* Type Declaration
************************************************************************
TYPES: BEGIN OF TY_ALV ,
         SEL       TYPE C,
         NUM       TYPE I,
         BUKRS     TYPE MSEG-BUKRS,
         GJAHR     TYPE BKPF-GJAHR,
         MONAT     TYPE BKPF-MONAT,
         BUDAT     TYPE SY-DATUM,
         MATNR     TYPE MARA-MATNR,
         ANLN1     TYPE MSEG-ANLN1,
         ANLN2     TYPE MSEG-ANLN2,
         MENGE_FH  TYPE MSEG-MENGE,
         MEINS     TYPE MARA-MEINS,
         MENGE_COL TYPE MSEG-MENGE,
         DMBTR_COL TYPE BSEG-DMBTR,
         DMBTR_DW  TYPE BSEG-DMBTR,
         DMBTR_FT  TYPE BSEG-DMBTR,
         WC        TYPE C,
         DMBTR_WC  TYPE BSEG-DMBTR,
         DMBTR_GZ  TYPE BSEG-DMBTR,
         BELNR     TYPE BSEG-BELNR,
         " 消息
         TYPE      TYPE C,
         MSG       TYPE STRING,
       END OF TY_ALV .

************************************************************************
* Internal Table & WorkArea
************************************************************************
DATA: GT_ALV TYPE TABLE OF TY_ALV,
      GS_ALV TYPE TY_ALV.

DATA: BEGIN OF GS_HEAD ,
        NUM   TYPE I,
        BLDAT TYPE BKPF-BLDAT,
        BUDAT TYPE BKPF-BUDAT,
        BLART TYPE BKPF-BLART,
        BUKRS TYPE BKPF-BUKRS,
        BKTXT TYPE BKPF-BKTXT,
        MONAT TYPE BKPF-MONAT,
        WAERS TYPE BKPF-WAERS,
      END OF GS_HEAD .
DATA GT_HEAD LIKE TABLE OF GS_HEAD .
DATA: BEGIN OF GS_ITEM ,
        NUM   TYPE I,
        BUZEI TYPE BSEG-BUZEI,
        BSCHL TYPE BSEG-BSCHL,
        HKONT TYPE BSEG-HKONT,
        MATNR TYPE BSEG-MATNR,
        MEINS TYPE BSEG-MEINS ,
        MENGE TYPE BSEG-MENGE,
        WRBTR TYPE BSEG-WRBTR,
        ANLN1 TYPE BSEG-ANLN1,
        ANLN2 TYPE BSEG-ANLN2,
        BZDAT TYPE BSEG-BZDAT,
        BEWAR TYPE BSEG-BEWAR,
        KOART TYPE BSEG-KOART ,
      END OF GS_ITEM .
DATA GT_ITEM LIKE TABLE OF GS_ITEM .
FIELD-SYMBOLS <FS_ITEM> LIKE GS_ITEM .

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA:GS_LAYOUT TYPE LVC_S_LAYO,
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
************************************************************************
* BAPI
************************************************************************
* 过账BAPI
DATA: GS_DOCUMENTHEADER    TYPE BAPIACHE09,
      GT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,
      GS_ACCOUNTGL         TYPE BAPIACGL09,
      GT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,
      GS_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      GT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,
      GS_CURRENCYAMOUNT    TYPE BAPIACCR09,
      GT_CRITERIA          TYPE TABLE OF BAPIACKEC9,
      GS_CRITERIA          TYPE BAPIACKEC9,
      GT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      GS_VALUEFIELD        TYPE BAPIACKEV9,
      GT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      GS_EXTENSION2        TYPE BAPIPAREX,
      GT_RETURN            TYPE TABLE OF BAPIRET2,
      GS_RETURN            TYPE BAPIRET2.

DATA: GS_OBJ TYPE BAPIACHE09.

DATA: GS_ZACCDOCUEXT TYPE ZACCDOCUEXT.
