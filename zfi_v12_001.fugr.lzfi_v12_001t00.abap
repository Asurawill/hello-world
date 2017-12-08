*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.01.30 at 11:42:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI_V12_001.....................................*
DATA:  BEGIN OF STATUS_ZFI_V12_001                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_V12_001                   .
CONTROLS: TCTRL_ZFI_V12_001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_V12_001                   .
TABLES: ZFI_V12_001                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
