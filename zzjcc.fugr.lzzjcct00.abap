*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.07.24 at 10:37:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZJCC...........................................*
DATA:  BEGIN OF STATUS_ZZJCC                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZJCC                         .
CONTROLS: TCTRL_ZZJCC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZJCC                         .
TABLES: ZZJCC                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
