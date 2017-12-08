*&---------------------------------------------------------------------*
*&  包含                ZFI042_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS p_bukrs TYPE t001-bukrs OBLIGATORY .
SELECT-OPTIONS s_werks FOR t001w-werks .
*SELECT-OPTIONS S_GJAHR FOR BKPF-GJAHR .
*PARAMETERS P_MONAT TYPE BKPF-MONAT .
SELECT-OPTIONS s_matnr FOR mara-matnr .
SELECT-OPTIONS s_mvgr1 FOR mvke-mvgr1 DEFAULT '347' .
SELECT-OPTIONS s_anln1 FOR anla-anln1 .
PARAMETERS: p_gjahr TYPE bkpf-gjahr OBLIGATORY.
PARAMETERS: p_monat TYPE bkpf-monat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1 .
