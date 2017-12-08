FUNCTION ZFI005_3.
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
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL DEFAULT '2015.01.22'
*"     VALUE(BLART_002) LIKE  BDCDATA-FVAL DEFAULT 'SA'
*"     VALUE(BUKRS_003) LIKE  BDCDATA-FVAL DEFAULT '1000'
*"     VALUE(BUDAT_004) LIKE  BDCDATA-FVAL DEFAULT '2015.01.22'
*"     VALUE(MONAT_005) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(WAERS_006) LIKE  BDCDATA-FVAL DEFAULT 'CNY'
*"     VALUE(XBLNR_007) LIKE  BDCDATA-FVAL DEFAULT '参照文本123'
*"     VALUE(NEWBS_009) LIKE  BDCDATA-FVAL DEFAULT '09'
*"     VALUE(NEWKO_010) LIKE  BDCDATA-FVAL DEFAULT '100000'
*"     VALUE(NEWUM_011) LIKE  BDCDATA-FVAL DEFAULT 'W'
*"     VALUE(WRBTR_012) LIKE  BDCDATA-FVAL DEFAULT '123'
*"     VALUE(ZUONR_013) LIKE  BDCDATA-FVAL DEFAULT '分配1'
*"     VALUE(SGTXT_014) LIKE  BDCDATA-FVAL DEFAULT '文本1'
*"     VALUE(ZFBDT_015) LIKE  BDCDATA-FVAL DEFAULT '2015.02.14'
*"     VALUE(WNAME_016) LIKE  BDCDATA-FVAL DEFAULT '利亚德光电'
*"     VALUE(WORT1_017) LIKE  BDCDATA-FVAL DEFAULT '北京'
*"     VALUE(WBZOG_018) LIKE  BDCDATA-FVAL DEFAULT '杭州奥体'
*"     VALUE(WDATE) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(WLZBP) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(WBANK_019) LIKE  BDCDATA-FVAL DEFAULT '支付地点333'
*"     VALUE(NEWBS_020) LIKE  BDCDATA-FVAL DEFAULT '11'
*"     VALUE(NEWKO_021) LIKE  BDCDATA-FVAL DEFAULT '100000'
*"     VALUE(HKONT_022) LIKE  BDCDATA-FVAL DEFAULT '2203010101'
*"     VALUE(WRBTR_023) LIKE  BDCDATA-FVAL DEFAULT '123'
*"     VALUE(ZFBDT_024) LIKE  BDCDATA-FVAL DEFAULT '2015.01.22'
*"     VALUE(ZUONR_025) LIKE  BDCDATA-FVAL DEFAULT '分配2'
*"     VALUE(SGTXT_026) LIKE  BDCDATA-FVAL DEFAULT '文本2'
*"     VALUE(NEWUM_027) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(BKTXT) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMF05A' '0100'.
perform bdc_field       using 'BDC_CURSOR'
                              'BKPF-XBLNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BKPF-BLDAT'
                              BLDAT_001.
perform bdc_field       using 'BKPF-BLART'
                              BLART_002.
perform bdc_field       using 'BKPF-BUKRS'
                              BUKRS_003.
perform bdc_field       using 'BKPF-BUDAT'
                              BUDAT_004.
perform bdc_field       using 'BKPF-MONAT'
                              MONAT_005.
perform bdc_field       using 'BKPF-WAERS'
                              WAERS_006.
perform bdc_field       using 'BKPF-XBLNR'
                              XBLNR_007.
perform bdc_field       using 'BKPF-BKTXT'
                              BKTXT.
perform bdc_field       using 'RF05A-NEWBS'
                              NEWBS_009.
perform bdc_field       using 'RF05A-NEWKO'
                              NEWKO_010.
perform bdc_field       using 'RF05A-NEWUM'
                              NEWUM_011.
perform bdc_dynpro      using 'SAPMF05A' '0320'.
perform bdc_field       using 'BDC_CURSOR'
                              'RF05A-NEWKO'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BSEG-WRBTR'
                              WRBTR_012.
perform bdc_field       using 'BSED-WBANK'
                              ZUONR_013.
perform bdc_field       using 'BSEG-SGTXT'
                              SGTXT_014.
perform bdc_field       using 'BSEG-ZFBDT'
                              ZFBDT_015.
perform bdc_field       using 'BSED-WNAME'
                              WNAME_016.
perform bdc_field       using 'BSED-WORT1'
                              WORT1_017.
perform bdc_field       using 'BSED-WBZOG'
                              WBZOG_018.
perform bdc_field       using 'BSED-WDATE'
                              WDATE. "出票日期 W
perform bdc_field       using 'BSED-WLZBP'
                              WLZBP. "出票银行 W
perform bdc_field       using 'BSED-WBANK'
                              WBANK_019.
perform bdc_field       using 'RF05A-NEWBS'
                              NEWBS_020.
perform bdc_field       using 'RF05A-NEWKO'
                              NEWKO_021.
perform bdc_field       using 'RF05A-NEWUM'
                              NEWUM_027.
IF NEWBS_020 EQ '11'.
perform bdc_dynpro      using 'SAPMF05A' '0301'.
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-SGTXT'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
*perform bdc_field       using 'BSEG-HKONT'
*                              HKONT_022.
perform bdc_field       using 'BSEG-WRBTR'
                              WRBTR_023.
perform bdc_field       using 'BSEG-ZFBDT'
                              ZFBDT_024.
perform bdc_field       using 'BSEG-ZUONR'
                              ZUONR_025.
*perform bdc_field       using 'COBL-KDAUF'
*                              ZUONR_025.
perform bdc_field       using 'BSEG-SGTXT'
                              SGTXT_026.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
perform bdc_dynpro      using 'SAPMF05A' '0301'.
ELSE.
perform bdc_dynpro      using 'SAPMF05A' '0304'.
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-SGTXT'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
*perform bdc_field       using 'BSEG-HKONT'
*                              HKONT_022.
perform bdc_field       using 'BSEG-WRBTR'
                              WRBTR_023.
perform bdc_field       using 'BSEG-ZFBDT'
                              ZFBDT_024.
perform bdc_field       using 'BSEG-ZUONR'
                              ZUONR_025.
*perform bdc_field       using 'COBL-KDAUF'
*                              ZUONR_025.
perform bdc_field       using 'BSEG-SGTXT'
                              SGTXT_026.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
perform bdc_dynpro      using 'SAPMF05A' '0304'.
ENDIF.
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-WRBTR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_transaction tables messtab
using                         'F-02'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
