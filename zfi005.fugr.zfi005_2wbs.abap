FUNCTION ZFI005_2WBS.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(BELNS_001) LIKE  BDCDATA-FVAL DEFAULT '100000009'
*"     VALUE(BUKRS_002) LIKE  BDCDATA-FVAL DEFAULT '1000'
*"     VALUE(GJAHS_003) LIKE  BDCDATA-FVAL DEFAULT '2015'
*"     VALUE(STGRD_004) LIKE  BDCDATA-FVAL DEFAULT '03'
*"     VALUE(CXBUDAT_005) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(CXMONAT_006) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMF05A' '0105'.
perform bdc_field       using 'BDC_CURSOR'
                              'UF05A-STGRD'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'RF05A-BELNS'
                              BELNS_001.
perform bdc_field       using 'BKPF-BUKRS'
                              BUKRS_002.
perform bdc_field       using 'RF05A-GJAHS'
                              GJAHS_003.
perform bdc_field       using 'UF05A-STGRD'
                              STGRD_004.
perform bdc_field       using 'BSIS-BUDAT'
                              CXBUDAT_005.
perform bdc_field       using 'BSIS-MONAT'
                              CXMONAT_006.
perform bdc_transaction tables messtab
using                         'FB08'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
