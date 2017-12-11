*&---------------------------------------------------------------------*
*&  包含                ZFI043_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .
PARAMETERS P_BUKRS TYPE T001-BUKRS OBLIGATORY .
SELECT-OPTIONS S_PSPID FOR PROJ-PSPID   .
SELECT-OPTIONS S_BLDAT FOR BKPF-BLDAT	 .
SELECTION-SCREEN END OF BLOCK B1 .
