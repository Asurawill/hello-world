*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.10.14 at 18:17:42
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCJ20N_CHECK....................................*
DATA:  BEGIN OF STATUS_ZCJ20N_CHECK                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCJ20N_CHECK                  .
CONTROLS: TCTRL_ZCJ20N_CHECK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCJ20N_CHECK                  .
TABLES: ZCJ20N_CHECK                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
