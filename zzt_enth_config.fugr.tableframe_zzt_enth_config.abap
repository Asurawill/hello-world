*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZZT_ENTH_CONFIG
*   generation date: 2017.08.18 at 10:29:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZZT_ENTH_CONFIG    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
