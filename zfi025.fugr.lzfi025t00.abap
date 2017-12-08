*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.04.21 at 14:25:17
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI025..........................................*
DATA:  BEGIN OF STATUS_ZFI025                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI025                        .
CONTROLS: TCTRL_ZFI025
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI025                        .
TABLES: ZFI025                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
