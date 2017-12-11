*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.07.09 at 11:18:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI006..........................................*
DATA:  BEGIN OF STATUS_ZFI006                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI006                        .
CONTROLS: TCTRL_ZFI006
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI006                        .
TABLES: ZFI006                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
