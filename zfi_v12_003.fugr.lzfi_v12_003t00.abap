*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.03.16 at 12:52:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI_V12_003.....................................*
DATA:  BEGIN OF STATUS_ZFI_V12_003                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_V12_003                   .
CONTROLS: TCTRL_ZFI_V12_003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_V12_003                   .
TABLES: ZFI_V12_003                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
