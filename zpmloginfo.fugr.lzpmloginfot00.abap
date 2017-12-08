*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.08.2015 at 18:05:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPMLOGINFO......................................*
DATA:  BEGIN OF STATUS_ZPMLOGINFO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMLOGINFO                    .
CONTROLS: TCTRL_ZPMLOGINFO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMLOGINFO                    .
TABLES: ZPMLOGINFO                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
