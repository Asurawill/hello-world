*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.07.28 at 10:58:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZLYW...........................................*
DATA:  BEGIN OF STATUS_ZZLYW                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZLYW                         .
CONTROLS: TCTRL_ZZLYW
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZLYW                         .
TABLES: ZZLYW                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
