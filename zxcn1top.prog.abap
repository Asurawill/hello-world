*&---------------------------------------------------------------------*
*&  包含                ZXCN1TOP
*&---------------------------------------------------------------------*
DATA FLAG  TYPE C.

DATA FLAG1 TYPE C.

DATA LS_PROJ TYPE PROJ.

DATA L_ZPMLOGINFO TYPE ZPMLOGINFO.

DATA:GINCLUDE_PROJ TYPE PROJ. "IT02 150819

DATA:GIN_ZKHBMNAME TYPE NAME1_GP.

*&--代码添加 BY HANDYBY 05.06.2017 16:47:09  BEGIN
  TABLES cnci_proj .
  DATA g_display TYPE c .
*&--代码添加 BY HANDYBY 05.06.2017 16:47:09  END

TABLES: CNCI_PRPS.
