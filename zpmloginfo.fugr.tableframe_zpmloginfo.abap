*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPMLOGINFO
*   generation date: 20.08.2015 at 18:05:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPMLOGINFO         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
