*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.10.31 at 19:03:41
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFIFKTJ.........................................*
DATA:  BEGIN OF STATUS_ZFIFKTJ                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIFKTJ                       .
CONTROLS: TCTRL_ZFIFKTJ
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIFKTJ                       .
TABLES: ZFIFKTJ                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
