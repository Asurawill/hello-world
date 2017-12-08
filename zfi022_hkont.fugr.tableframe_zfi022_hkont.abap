*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI022_HKONT
*   generation date: 2015.05.07 at 00:36:32
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI022_HKONT       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
