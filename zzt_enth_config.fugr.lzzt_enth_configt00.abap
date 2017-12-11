*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.08.18 at 10:29:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZT_ENTH_CONFIG.................................*
DATA:  BEGIN OF STATUS_ZZT_ENTH_CONFIG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZT_ENTH_CONFIG               .
CONTROLS: TCTRL_ZZT_ENTH_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZT_ENTH_CONFIG               .
TABLES: ZZT_ENTH_CONFIG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
