*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.01.30 at 15:23:15
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI_V12_002.....................................*
DATA:  BEGIN OF STATUS_ZFI_V12_002                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_V12_002                   .
CONTROLS: TCTRL_ZFI_V12_002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_V12_002                   .
TABLES: ZFI_V12_002                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
