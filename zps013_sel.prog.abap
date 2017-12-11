*&---------------------------------------------------------------------*
*&  包含                ZPS013_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .
PARAMETERS P_BUKRS TYPE T001-BUKRS OBLIGATORY .
SELECT-OPTIONS S_POSID FOR PRPS-POSID .
SELECTION-SCREEN END OF BLOCK B1 .
