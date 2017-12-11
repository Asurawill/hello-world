FUNCTION ZFI005_1_SPLIT.
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
*"     VALUE(NEWUM_008) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(NEWBS_009) LIKE  BDCDATA-FVAL DEFAULT '40'
*"     VALUE(NEWKO_010) LIKE  BDCDATA-FVAL DEFAULT '1002010101'
*"     VALUE(WRBTR_011) LIKE  BDCDATA-FVAL DEFAULT '123'
*"     VALUE(ZUONR_012) LIKE  BDCDATA-FVAL DEFAULT '分配1'
*"     VALUE(SGTXT_013) LIKE  BDCDATA-FVAL DEFAULT '文本1'
*"     VALUE(NEWBS_014) LIKE  BDCDATA-FVAL DEFAULT '11'
*"     VALUE(NEWKO_015) LIKE  BDCDATA-FVAL DEFAULT '100000'
*"     VALUE(FMORE_016) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(RSTGR_018) LIKE  BDCDATA-FVAL DEFAULT 'A01'
*"     VALUE(HKONT_021) LIKE  BDCDATA-FVAL DEFAULT '1122010101'
*"     VALUE(WRBTR_022) LIKE  BDCDATA-FVAL DEFAULT '123'
*"     VALUE(ZFBDT_023) LIKE  BDCDATA-FVAL DEFAULT '2015.01.22'
*"     VALUE(ZUONR_024) LIKE  BDCDATA-FVAL DEFAULT '分配2'
*"     VALUE(SGTXT_025) LIKE  BDCDATA-FVAL DEFAULT '文本2'
*"     VALUE(BKTXT) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
DATA: BEGIN OF T_ITEM_SPLIT1 OCCURS 1,
        NEWUM_008 TYPE BDCDATA-FVAL,
        NEWBS_014 TYPE BDCDATA-FVAL,
        NEWKO_015 TYPE BDCDATA-FVAL,
*        FMORE_016 TYPE BDCDATA-FVAL,
*        RSTGR_018 TYPE BDCDATA-FVAL,
        WRBTR_022 TYPE BDCDATA-FVAL,
        ZFBDT_023 TYPE BDCDATA-FVAL,
        ZUONR_024 TYPE BDCDATA-FVAL,
        SGTXT_025 TYPE BDCDATA-FVAL,
      END OF T_ITEM_SPLIT1.
CLEAR T_ITEM_SPLIT1[].
IMPORT T_ITEM_SPLIT1[] FROM MEMORY ID 'ZFI005_1_ITEM'.

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMF05A' '0100'."HEAD <-
*perform bdc_field       using 'BDC_CURSOR'
*                              'RF05A-NEWKO'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
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
perform bdc_dynpro      using 'SAPMF05A' '0300'."Line 1 <-
perform bdc_field       using 'BDC_CURSOR'
                              'RF05A-NEWUM'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BSEG-WRBTR'
                              WRBTR_011.
perform bdc_field       using 'BSEG-ZUONR'
                              ZUONR_012."项目号，写入“分配”
*perform bdc_field       using 'COBL-KDAUF'
*                              ZUONR_012."项目号，写入“销售订单”
perform bdc_field       using 'BSEG-SGTXT'
                              SGTXT_013.
perform bdc_field       using 'DKACB-FMORE'
                              FMORE_016.
perform bdc_dynpro      using 'SAPLKACB' '0002'."Line 1 subscreen <-
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_dynpro      using 'SAPMF05A' '0330'."Line 1 enter <-
perform bdc_field       using 'BDC_CURSOR'
                              'BSEG-RSTGR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BSEG-RSTGR'
                              RSTGR_018.
"以下是凭证第2~n行内容->
LOOP AT T_ITEM_SPLIT1.
perform bdc_field       using 'RF05A-NEWUM'
                              T_ITEM_SPLIT1-NEWUM_008.
perform bdc_field       using 'RF05A-NEWBS'
                              T_ITEM_SPLIT1-NEWBS_014.
perform bdc_field       using 'RF05A-NEWKO'
                              T_ITEM_SPLIT1-NEWKO_015.
"如果填写的记账代码是11，则调用301的屏幕
IF T_ITEM_SPLIT1-NEWBS_014 EQ '11'.
  perform bdc_dynpro      using 'SAPMF05A' '0301'."
  perform bdc_field       using 'BSEG-WRBTR'
                                T_ITEM_SPLIT1-WRBTR_022.
  IF T_ITEM_SPLIT1-ZFBDT_023 IS NOT INITIAL AND T_ITEM_SPLIT1-ZFBDT_023 NE '00000000'.
    perform bdc_field       using 'BSEG-ZFBDT'
                                  T_ITEM_SPLIT1-ZFBDT_023.
  ENDIF.
  perform bdc_field       using 'BSEG-ZUONR' "项目号，写入“分配”
                                T_ITEM_SPLIT1-ZUONR_024.
*  perform bdc_field       using 'COBL-KDAUF' "项目号，写入“销售订单”
*                                ZUONR_024.
  perform bdc_field       using 'BSEG-SGTXT'
                                T_ITEM_SPLIT1-SGTXT_025.
ELSE.
  perform bdc_dynpro      using 'SAPMF05A' '0304'."
  perform bdc_field       using 'BSEG-WRBTR'
                                T_ITEM_SPLIT1-WRBTR_022.
  IF T_ITEM_SPLIT1-ZFBDT_023 IS NOT INITIAL AND T_ITEM_SPLIT1-ZFBDT_023 NE '00000000'.
    perform bdc_field       using 'BSEG-ZFBDT'
                                  T_ITEM_SPLIT1-ZFBDT_023.
  ENDIF.
  perform bdc_field       using 'BSEG-ZUONR'"项目号，写入“分配”
                                T_ITEM_SPLIT1-ZUONR_024.
*  perform bdc_field       using 'COBL-KDAUF'"项目号，写入“销售订单”
*                                ZUONR_024.
  perform bdc_field       using 'BSEG-SGTXT'
                                T_ITEM_SPLIT1-SGTXT_025.
ENDIF.
ENDLOOP.
"<-以上是凭证第2~n行内容

"以下是凭证输入完毕，点击保存的内容->
"如果填写的记账代码是11，则调用301的屏幕
IF T_ITEM_SPLIT1-NEWBS_014 EQ '11'.
  perform bdc_dynpro      using 'SAPMF05A' '0301'."
ELSE.
  perform bdc_dynpro      using 'SAPMF05A' '0304'."
ENDIF.
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

FREE MEMORY ID 'ZFI005_1_ITEM'.

ENDFUNCTION.
*INCLUDE BDCRECXY .
