*&---------------------------------------------------------------------*
*&  包含                ZFI042_TOP
*&---------------------------------------------------------------------*

TABLES: T001,T001W,BKPF,MARA,ANLA,MVKE .

* ALV输出字段
TYPES: BEGIN OF TY_OUTPUT ,
         SEL     TYPE C,
         NUM     TYPE I,
         BUKRS   TYPE T001-BUKRS,
         WERKS   TYPE T001W-WERKS,
         MATNR   TYPE MARA-MATNR,
         MAKTX   TYPE MAKT-MAKTX,
         LABST   TYPE MARD-LABST,
         ANLN1   TYPE ANLA-ANLN1,
         TXT50   TYPE ANLA-TXT50,
         MENGE   TYPE ANLA-MENGE,
         CHAYI   TYPE MARD-LABST,
         CLR(4) TYPE C,
       END OF TY_OUTPUT .
DATA: GT_ALV TYPE TABLE OF TY_OUTPUT,
      GS_ALV TYPE TY_OUTPUT.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA:GS_LAYOUT TYPE LVC_S_LAYO ,      "alv的格式
*     GS_GRID_SETTINGS TYPE LVC_S_GLAY,
     GS_LVC    TYPE LVC_S_FCAT,
     GT_LVC    TYPE LVC_T_FCAT.
*     GT_SORT          TYPE LVC_T_SORT,
*     GS_VARIANT       TYPE DISVARIANT,
*     GT_EVENTS        TYPE SLIS_T_EVENT  , "保存AVL事件
*     GS_EVENT         LIKE LINE OF GT_EVENTS.

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
