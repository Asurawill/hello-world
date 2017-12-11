*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.08.05 at 17:33:54
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZJFT...........................................*
DATA:  BEGIN OF STATUS_ZZJFT                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZJFT                         .
CONTROLS: TCTRL_ZZJFT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZJFT                         .
TABLES: ZZJFT                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
