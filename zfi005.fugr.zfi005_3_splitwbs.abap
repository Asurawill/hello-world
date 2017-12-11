FUNCTION ZFI005_3_SPLITWBS.
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
*"     VALUE(HKONT_AKONT) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(WRBTR_012) LIKE  BDCDATA-FVAL DEFAULT '123'
*"     VALUE(WBANK) LIKE  BDCDATA-FVAL OPTIONAL
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
DATA: BEGIN OF T_ITEM_SPLIT3 OCCURS 1,
        NEWBS_020 TYPE BDCDATA-FVAL,
        NEWKO_021 TYPE BDCDATA-FVAL,
        NEWUM_027 TYPE BDCDATA-FVAL,
        WRBTR_023 TYPE BDCDATA-FVAL,
        ZFBDT_024 TYPE BDCDATA-FVAL,
      "  PS_POSID2 TYPE BDCDATA-FVAL,"IT02 ADD WBS 150818
        ZUONR_025 TYPE BDCDATA-FVAL,
        SGTXT_026 TYPE BDCDATA-FVAL,
      END OF T_ITEM_SPLIT3.

CLEAR T_ITEM_SPLIT3[].
IMPORT T_ITEM_SPLIT3[] FROM MEMORY ID 'ZFI005_3_ITEMWBS'.

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
perform bdc_field       using 'BSEG-ZUONR'
                              ZUONR_013.
*perform bdc_field       using 'BSED-WBANK'
*                               'WRANK'. "汇票号
*perform bdc_field       using 'BSED-WBANK'
*                              ZUONR_013.
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
"以下是凭证第2~n行内容->
LOOP AT T_ITEM_SPLIT3.
perform bdc_field       using 'RF05A-NEWBS'
                              T_ITEM_SPLIT3-NEWBS_020.
perform bdc_field       using 'RF05A-NEWKO'
                              T_ITEM_SPLIT3-NEWKO_021.
perform bdc_field       using 'RF05A-NEWUM'
                              T_ITEM_SPLIT3-NEWUM_027.
*  IF T_ITEM_SPLIT3-PS_POSID2 IS NOT INITIAL .
*   perform bdc_dynpro      using 'SAPLKACB' '0002'."Line 1 subscreen <-
*
*   perform bdc_field       using 'BDC_CURSOR'
*                              'COBL-PS_POSID'.
*
*   perform bdc_field       using 'COBL-PS_POSID'
*                               T_ITEM_SPLIT3-PS_POSID2."  WBS 元素号 IT02 150818
*   perform bdc_field       using 'BDC_OKCODE'
*                              '=ENTE'.
*   perform bdc_dynpro      using 'BDC_SUBSCR'
*                              'SAPLKACB'.
*
*  ENDIF.
IF T_ITEM_SPLIT3-NEWBS_020 EQ '11'.
perform bdc_dynpro      using 'SAPMF05A' '0301'.
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-SGTXT'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
*perform bdc_field       using 'BSEG-HKONT'
*                              HKONT_022.
IF HKONT_AKONT NE ''.
  perform bdc_field       using 'BSEG-HKONT'   "统驭科目  ADDED BY IT02 151110
                              HKONT_AKONT.

ENDIF.
perform bdc_field       using 'BSEG-WRBTR'
                              T_ITEM_SPLIT3-WRBTR_023.
perform bdc_field       using 'BSEG-ZFBDT'
                              T_ITEM_SPLIT3-ZFBDT_024.
perform bdc_field       using 'BSEG-ZUONR'
                              T_ITEM_SPLIT3-ZUONR_025.
*perform bdc_field       using 'COBL-KDAUF'
*                              ZUONR_025.
perform bdc_field       using 'BSEG-SGTXT'
                              T_ITEM_SPLIT3-SGTXT_026.
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
                              T_ITEM_SPLIT3-WRBTR_023.
perform bdc_field       using 'BSEG-ZFBDT'
                              T_ITEM_SPLIT3-ZFBDT_024.
perform bdc_field       using 'BSEG-ZUONR'
                              T_ITEM_SPLIT3-ZUONR_025.
*perform bdc_field       using 'COBL-KDAUF'
*                              ZUONR_025.
perform bdc_field       using 'BSEG-SGTXT'
                              T_ITEM_SPLIT3-SGTXT_026.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
perform bdc_dynpro      using 'SAPMF05A' '0304'.
ENDIF.
ENDLOOP.
"<-以上是凭证第2~n行内容
"以下是凭证输入完毕，点击保存的内容->
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-WRBTR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
"<-以上是凭证输入完毕，点击保存的内容
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

FREE MEMORY ID 'ZFI005_3_ITEM'.

ENDFUNCTION.
