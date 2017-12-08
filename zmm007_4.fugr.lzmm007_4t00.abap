*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.08.16 at 19:19:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM007_4........................................*
DATA:  BEGIN OF STATUS_ZMM007_4                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM007_4                      .
CONTROLS: TCTRL_ZMM007_4
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM007_4                      .
TABLES: ZMM007_4                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
