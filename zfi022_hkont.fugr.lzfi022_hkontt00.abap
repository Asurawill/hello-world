*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.05.07 at 00:36:32
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI022_HKONT....................................*
DATA:  BEGIN OF STATUS_ZFI022_HKONT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI022_HKONT                  .
CONTROLS: TCTRL_ZFI022_HKONT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI022_HKONT                  .
TABLES: ZFI022_HKONT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
