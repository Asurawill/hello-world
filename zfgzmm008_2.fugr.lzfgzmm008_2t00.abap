*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02.03.2015 at 14:38:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM002_2........................................*
DATA:  BEGIN OF STATUS_ZMM002_2                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM002_2                      .
CONTROLS: TCTRL_ZMM002_2
            TYPE TABLEVIEW USING SCREEN '1011'.
*.........table declarations:.................................*
TABLES: *ZMM002_2                      .
TABLES: ZMM002_2                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
