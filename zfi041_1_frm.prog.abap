*&---------------------------------------------------------------------*
*&  包含                ZFI041_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DEAL_DATA_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DEAL_DATA_ALV1 .

  " 物料描述
  DATA: LT_MAKT TYPE TABLE OF MAKT,
        LS_MAKT TYPE MAKT.
  " 库存地点描述
  DATA: BEGIN OF LS_T001L ,
          WERKS TYPE T001L-WERKS,
          LGORT TYPE T001L-LGORT,
          LGOBE TYPE T001L-LGOBE,
        END OF LS_T001L .
  DATA LT_T001L LIKE TABLE OF LS_T001L .
  " 供应商描述
  DATA: BEGIN OF LS_LFA1 ,
          LIFNR TYPE LFA1-LIFNR,
          NAME1 TYPE LFA1-NAME1,
        END OF LS_LFA1.
  DATA LT_LFA1 LIKE TABLE OF LS_LFA1 .
  " 物料单位
  DATA: BEGIN OF LS_MARA ,
          MATNR TYPE MARA-MATNR,
          MEINS TYPE MARA-MEINS,
        END OF LS_MARA .
  DATA LT_MARA LIKE TABLE OF LS_MARA .
  " 资产折旧费
  DATA: BEGIN OF LS_ANLP ,
          BUKRS TYPE ANLP-BUKRS,
          GJAHR TYPE ANLP-GJAHR,
          PERAF TYPE ANLP-PERAF,
          ANLN1 TYPE ANLP-ANLN1,
          ANLN2 TYPE ANLP-ANLN2,
          NAFAZ TYPE ANLP-NAFAZ,
        END OF LS_ANLP .
  DATA LT_ANLP LIKE TABLE OF LS_ANLP .

  " XXX
  DATA L_NUM TYPE I VALUE IS INITIAL . " ALV1 行数
  DATA L_LEN TYPE I VALUE IS INITIAL .  " 取物料后12位作为固定资产

* 将当前所有类型的库存的物料放进ALV
*  LOOP AT GT_MARD_NOW INTO GS_MARD_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MARD_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MARD_NOW-MATNR .
*    GS_ALV1-LGORT = GS_MARD_NOW-LGORT .
**    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MARD_NOW-WERKS BINARY SEARCH .
**    IF SY-SUBRC = 0 .
**      GS_ALV-ZYWLX = GS_ZZLYW-ZYWLX .
**    ENDIF.
*    GS_ALV1-OS_QTY = GS_MARD_NOW-LABST + GS_MARD_NOW-UMLME + GS_MARD_NOW-INSME + GS_MARD_NOW-EINME +
*                     GS_MARD_NOW-SPEME + GS_MARD_NOW-RETME .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MARD_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MARD_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MARD_NOW .
*  ENDLOOP.

*  LOOP AT GT_MARDH_NOW INTO GS_MARDH_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MARDH_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MARDH_NOW-MATNR .
*    GS_ALV1-LGORT = GS_MARDH_NOW-LGORT .
**    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MARDH_NOW-WERKS BINARY SEARCH .
**    IF SY-SUBRC = 0 .
**      GS_ALV-ZYWLX = GS_ZZLYW-ZYWLX .
**    ENDIF.
*    GS_ALV1-OS_QTY = GS_MARDH_NOW-LABST + GS_MARDH_NOW-UMLME + GS_MARDH_NOW-INSME + GS_MARDH_NOW-EINME +
*                     GS_MARDH_NOW-SPEME + GS_MARDH_NOW-RETME .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MARDH_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MARDH_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MARDH_NOW .
*  ENDLOOP.

*  LOOP AT GT_MSKA_NOW INTO GS_MSKA_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MSKA_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MSKA_NOW-MATNR .
*    GS_ALV1-LGORT = GS_MSKA_NOW-LGORT .
*    GS_ALV1-VBELN = GS_MSKA_NOW-VBELN .
*    GS_ALV1-POSNR = GS_MSKA_NOW-POSNR .
*    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSKA_NOW-WERKS
*                                               SOBKZ = GS_MSKA_NOW-SOBKZ BINARY SEARCH .
*    IF SY-SUBRC = 0 .
*      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
*    ENDIF.
*    GS_ALV1-OS_QTY = GS_MSKA_NOW-KALAB + GS_MSKA_NOW-KAINS + GS_MSKA_NOW-KASPE + GS_MSKA_NOW-KAEIN .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MSKA_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MSKA_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MSKA_NOW .
*  ENDLOOP.

*  LOOP AT GT_MSKAH_NOW INTO GS_MSKAH_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MSKAH_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MSKAH_NOW-MATNR .
*    GS_ALV1-LGORT = GS_MSKAH_NOW-LGORT .
*    GS_ALV1-VBELN = GS_MSKAH_NOW-VBELN .
*    GS_ALV1-POSNR = GS_MSKAH_NOW-POSNR .
*    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSKAH_NOW-WERKS
*                                               SOBKZ = GS_MSKAH_NOW-SOBKZ BINARY SEARCH .
*    IF SY-SUBRC = 0 .
*      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
*    ENDIF.
*    GS_ALV1-OS_QTY = GS_MSKAH_NOW-KALAB + GS_MSKAH_NOW-KAINS + GS_MSKAH_NOW-KASPE + GS_MSKAH_NOW-KAEIN .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MSKAH_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MSKAH_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MSKAH_NOW .
*  ENDLOOP.

*  LOOP AT GT_MSLB_NOW INTO GS_MSLB_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MSLB_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MSLB_NOW-MATNR .
*    GS_ALV1-LIFNR = GS_MSLB_NOW-LIFNR .
*    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSLB_NOW-WERKS
*                                               SOBKZ = GS_MSLB_NOW-SOBKZ BINARY SEARCH .
*    IF SY-SUBRC = 0 .
*      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
*    ENDIF.
*    GS_ALV1-OS_QTY = GS_MSLB_NOW-LBLAB + GS_MSLB_NOW-LBINS + GS_MSLB_NOW-LBUML .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MSLB_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MSLB_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MSLB_NOW .
*  ENDLOOP.

*  LOOP AT GT_MSLBH_NOW INTO GS_MSLBH_NOW .
*    L_NUM = L_NUM + 1 .
*    GS_ALV1-NUM = L_NUM .
*    GS_ALV1-WERKS = GS_MSLBH_NOW-WERKS .
*    GS_ALV1-MATNR = GS_MSLBH_NOW-MATNR .
*    GS_ALV1-LIFNR = GS_MSLBH_NOW-LIFNR .
*    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSLBH_NOW-WERKS
*                                               SOBKZ = GS_MSLBH_NOW-SOBKZ BINARY SEARCH .
*    IF SY-SUBRC = 0 .
*      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
*    ENDIF.
*    GS_ALV1-OS_QTY = GS_MSLBH_NOW-LBLAB + GS_MSLBH_NOW-LBINS + GS_MSLBH_NOW-LBUML .
*    " 将物料号后12位作为固定资产编号
*    L_LEN = STRLEN( GS_MSLBH_NOW-MATNR ) .
*    L_LEN = L_LEN - 12 .
*    GS_ALV1-ANLN1 = GS_MSLBH_NOW-MATNR+L_LEN(12) .
*
*    APPEND GS_ALV1 TO GT_ALV1 .
*    CLEAR GS_ALV1 .
*
*    CLEAR GS_ZZLYW .
*    CLEAR L_LEN .
*    CLEAR GS_MSLBH_NOW .
*  ENDLOOP.

*  " 暂存MARD/MSKA/MSLB 保存到ALV1的数据
*  DATA: LT_ALV1 LIKE GT_ALV1,
*        LS_ALV1 LIKE LINE OF LT_ALV1.
*  DATA: LT_ALV2 LIKE GT_ALV1,
*        LS_ALV2 LIKE LINE OF LT_ALV2.
*  DATA: LT_ALV3 LIKE GT_ALV1,
*        LS_ALV3 LIKE LINE OF LT_ALV3.

* 将期初所有类型的库存的物料放进ALV
  LOOP AT GT_MARD_BEFORE INTO GS_MARD_BEFORE .
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MARD_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MARD_BEFORE-MATNR .
    GS_ALV1-LGORT = GS_MARD_BEFORE-LGORT .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MARD_BEFORE-WERKS
                                               LGORT = GS_MARD_BEFORE-LGORT
                                               SOBKZ = ''
                                               BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
      GS_ALV1-VBELN = GS_ZZLYW-VBELN .
      GS_ALV1-POSNR = GS_ZZLYW-POSNR .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MARD_BEFORE-LABST + GS_MARD_BEFORE-UMLME + GS_MARD_BEFORE-INSME + GS_MARD_BEFORE-EINME +
                     GS_MARD_BEFORE-SPEME + GS_MARD_BEFORE-RETME .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MARD_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MARD_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MARD_BEFORE .
  ENDLOOP.
*  MOVE-CORRESPONDING GT_ALV1 TO LT_ALV1 .
*  SORT LT_ALV1 BY MATNR WERKS LGORT .
  LOOP AT GT_MARDH_BEFORE INTO GS_MARDH_BEFORE .
*    " 先根据物料、工厂、库存地点字段判断记录是否已经在MARD中出现，如果出现不再加入ALV
*    READ TABLE LT_ALV1 WITH KEY MATNR = GS_MARDH_BEFORE-MATNR
*                                WERKS = GS_MARDH_BEFORE-WERKS
*                                LGORT = GS_MARDH_BEFORE-LGORT BINARY SEARCH TRANSPORTING NO FIELDS .
*    IF SY-SUBRC = 0 .
*      CONTINUE .
*    ENDIF.
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MARDH_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MARDH_BEFORE-MATNR .
    GS_ALV1-LGORT = GS_MARDH_BEFORE-LGORT .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MARDH_BEFORE-WERKS
                                               LGORT = GS_MARDH_BEFORE-LGORT
                                               SOBKZ = ''
                                               BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
      GS_ALV1-VBELN = GS_ZZLYW-VBELN .
      GS_ALV1-POSNR = GS_ZZLYW-POSNR .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MARDH_BEFORE-LABST + GS_MARDH_BEFORE-UMLME + GS_MARDH_BEFORE-INSME + GS_MARDH_BEFORE-EINME +
                     GS_MARDH_BEFORE-SPEME + GS_MARDH_BEFORE-RETME .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MARDH_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MARDH_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MARDH_BEFORE .
  ENDLOOP.

  LOOP AT GT_MSKA_BEFORE INTO GS_MSKA_BEFORE .
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MSKA_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MSKA_BEFORE-MATNR .
    GS_ALV1-LGORT = GS_MSKA_BEFORE-LGORT .
    GS_ALV1-VBELN = GS_MSKA_BEFORE-VBELN .
    GS_ALV1-POSNR = GS_MSKA_BEFORE-POSNR .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSKA_BEFORE-WERKS
                                               LGORT = GS_MSKA_BEFORE-LGORT
                                               SOBKZ = GS_MSKA_BEFORE-SOBKZ BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MSKA_BEFORE-KALAB + GS_MSKA_BEFORE-KAINS + GS_MSKA_BEFORE-KASPE + GS_MSKA_BEFORE-KAEIN .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MSKA_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MSKA_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MSKA_BEFORE .
  ENDLOOP.
*  MOVE-CORRESPONDING GT_ALV2 TO LT_ALV2 .
*  SORT LT_ALV2 BY MATNR WERKS LGORT VBELN POSNR  .
  LOOP AT GT_MSKAH_BEFORE INTO GS_MSKAH_BEFORE .
*    " 先根据物料、工厂、库存地点字段判断记录是否已经在MARD中出现，如果出现不再加入ALV
*    READ TABLE LT_ALV2 WITH KEY MATNR = GS_MSKAH_BEFORE-MATNR
*                                WERKS = GS_MSKAH_BEFORE-WERKS
*                                LGORT = GS_MSKAH_BEFORE-LGORT
*                                VBELN = GS_MSKAH_BEFORE-VBELN
*                                POSNR = GS_MSKAH_BEFORE-POSNR BINARY SEARCH TRANSPORTING NO FIELDS .
*    IF SY-SUBRC = 0 .
*      CONTINUE .
*    ENDIF.
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MSKAH_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MSKAH_BEFORE-MATNR .
    GS_ALV1-LGORT = GS_MSKAH_BEFORE-LGORT .
    GS_ALV1-VBELN = GS_MSKAH_BEFORE-VBELN .
    GS_ALV1-POSNR = GS_MSKAH_BEFORE-POSNR .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSKAH_BEFORE-WERKS
                                               LGORT = GS_MSKAH_BEFORE-LGORT
                                               SOBKZ = GS_MSKAH_BEFORE-SOBKZ BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MSKAH_BEFORE-KALAB + GS_MSKAH_BEFORE-KAINS + GS_MSKAH_BEFORE-KASPE + GS_MSKAH_BEFORE-KAEIN .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MSKAH_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MSKAH_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MSKAH_BEFORE .
  ENDLOOP.

  LOOP AT GT_MSLB_BEFORE INTO GS_MSLB_BEFORE .
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MSLB_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MSLB_BEFORE-MATNR .
    GS_ALV1-LIFNR = GS_MSLB_BEFORE-LIFNR .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSLB_BEFORE-WERKS
                                               LGORT = ''
                                               SOBKZ = GS_MSLB_BEFORE-SOBKZ BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
      GS_ALV1-VBELN = GS_ZZLYW-VBELN .
      GS_ALV1-POSNR = GS_ZZLYW-POSNR .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MSLB_BEFORE-LBLAB + GS_MSLB_BEFORE-LBINS + GS_MSLB_BEFORE-LBUML .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MSLB_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MSLB_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MSLB_BEFORE .
  ENDLOOP.
*  MOVE-CORRESPONDING GT_ALV3 TO LT_ALV3 .
*  SORT LT_ALV3 BY MATNR WERKS LIFNR .
  LOOP AT GT_MSLBH_BEFORE INTO GS_MSLBH_BEFORE .
*    " 先根据物料、工厂、库存地点字段判断记录是否已经在MARD中出现，如果出现不再加入ALV
*    READ TABLE LT_ALV3 WITH KEY MATNR = GS_MSLBH_BEFORE-MATNR
*                                WERKS = GS_MSLBH_BEFORE-WERKS
*                                LIFNR = GS_MSLBH_BEFORE-LIFNR BINARY SEARCH TRANSPORTING NO FIELDS .
*    IF SY-SUBRC = 0 .
*      CONTINUE .
*    ENDIF.
    L_NUM = L_NUM + 1 .
    GS_ALV1-NUM = L_NUM .
    GS_ALV1-WERKS = GS_MSLBH_BEFORE-WERKS .
    GS_ALV1-MATNR = GS_MSLBH_BEFORE-MATNR .
    GS_ALV1-LIFNR = GS_MSLBH_BEFORE-LIFNR .
    READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = GS_MSLBH_BEFORE-WERKS
                                               LGORT = ''
                                               SOBKZ = GS_MSLBH_BEFORE-SOBKZ BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV1-ZYWLX = GS_ZZLYW-ZYWLX .
      GS_ALV1-VBELN = GS_ZZLYW-VBELN .
      GS_ALV1-POSNR = GS_ZZLYW-POSNR .
    ENDIF.
    GS_ALV1-OS_QTY = GS_MSLBH_BEFORE-LBLAB + GS_MSLBH_BEFORE-LBINS + GS_MSLBH_BEFORE-LBUML .
    " 将物料号后12位作为固定资产编号
    L_LEN = STRLEN( GS_MSLBH_BEFORE-MATNR ) .
    L_LEN = L_LEN - 12 .
    GS_ALV1-ANLN1 = GS_MSLBH_BEFORE-MATNR+L_LEN(12) .

    APPEND GS_ALV1 TO GT_ALV1 .
    CLEAR GS_ALV1 .

    CLEAR GS_ZZLYW .
    CLEAR L_LEN .
    CLEAR GS_MSLBH_BEFORE .
  ENDLOOP.

  IF GT_ALV1 IS NOT INITIAL .

* 根据ALV里的数据将剩下的没赋值的字段的值从表里取出
    SELECT *
      INTO TABLE LT_MAKT
      FROM MAKT
       FOR ALL ENTRIES IN GT_ALV1
     WHERE MATNR = GT_ALV1-MATNR
       AND SPRAS = SY-LANGU .
    SORT LT_MAKT BY MATNR .

    SELECT WERKS
           LGORT
           LGOBE
      INTO CORRESPONDING FIELDS OF TABLE LT_T001L
      FROM T001L
       FOR ALL ENTRIES IN GT_ALV1
     WHERE WERKS = GT_ALV1-WERKS
       AND LGORT = GT_ALV1-LGORT .
    SORT LT_T001L BY WERKS LGORT .

    SELECT LIFNR
           NAME1
      INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
      FROM LFA1
       FOR ALL ENTRIES IN GT_ALV1
     WHERE LIFNR = GT_ALV1-LIFNR .
    SORT LT_LFA1 BY LIFNR .

    SELECT MATNR
           MEINS
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FROM MARA
       FOR ALL ENTRIES IN GT_ALV1
     WHERE MATNR = GT_ALV1-MATNR .
    SORT LT_MARA BY MATNR .

    SELECT BUKRS
           GJAHR
           PERAF
           ANLN1
           ANLN2
           NAFAZ
      INTO CORRESPONDING FIELDS OF TABLE LT_ANLP
      FROM ANLP
       FOR ALL ENTRIES IN GT_ALV1
     WHERE BUKRS = P_BUKRS
       AND GJAHR = P_LFGJA
       AND PERAF = P_LFMON
       AND ANLN1 = GT_ALV1-ANLN1 .
    SORT LT_ANLP BY ANLN1 .

  ELSE .
    IF G_BUT1 = 'X'.
      MESSAGE '没有数据' TYPE 'E' .
    ENDIF.
  ENDIF.

* 根据ALV里的数据给剩下的没赋值的字段赋值
  SORT GT_ALV1 BY MATNR WERKS LGORT .
  LOOP AT GT_ALV1 ASSIGNING <FS_ALV1> .
    " 给物料描述和物料单位赋值
    IF <FS_ALV1>-MATNR IS NOT INITIAL .
      READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0 .
        <FS_ALV1>-MAKTX = LS_MAKT-MAKTX .
      ENDIF.
      READ TABLE LT_MARA INTO LS_MARA WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0 .
        <FS_ALV1>-MEINS = LS_MARA-MEINS .
      ENDIF.
    ENDIF.
    " 给库存地点描述赋值
    IF <FS_ALV1>-LGORT IS NOT INITIAL .
      READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = <FS_ALV1>-WERKS
                                                 LGORT = <FS_ALV1>-LGORT BINARY SEARCH .
      IF SY-SUBRC = 0 .
        <FS_ALV1>-LGOBE = LS_T001L-LGOBE .
      ENDIF.
    ENDIF.
    " 给供应商描述赋值
    IF <FS_ALV1>-LIFNR IS NOT INITIAL .
      READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = <FS_ALV1>-LIFNR BINARY SEARCH .
      IF SY-SUBRC = 0 .
        <FS_ALV1>-NAME1 = LS_LFA1-NAME1 .
      ENDIF.
    ENDIF.

    " 赋值当前期初总数量(包括主库存、特殊库存E，特殊库存O )
    READ TABLE GT_MARD_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MARD_BEFORE INTO GS_MARD_BEFORE FROM SY-TABIX .
        IF GS_MARD_BEFORE-MATNR = <FS_ALV1>-MATNR .
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MARD_BEFORE-LABST + GS_MARD_BEFORE-UMLME + GS_MARD_BEFORE-INSME +
                                              GS_MARD_BEFORE-EINME + GS_MARD_BEFORE-SPEME + GS_MARD_BEFORE-RETME .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MARD_BEFORE .
      ENDLOOP.
    ENDIF.
    READ TABLE GT_MARDH_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MARDH_BEFORE INTO GS_MARDH_BEFORE FROM SY-TABIX .
        IF GS_MARDH_BEFORE-MATNR = <FS_ALV1>-MATNR .
*          READ TABLE LT_ALV1 WITH KEY MATNR = GS_MARDH_BEFORE-MATNR
*                                      WERKS = GS_MARDH_BEFORE-WERKS
*                                      LGORT = GS_MARDH_BEFORE-LGORT BINARY SEARCH TRANSPORTING NO FIELDS .
*          IF SY-SUBRC = 0 .
*            CONTINUE .
*          ENDIF.
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MARDH_BEFORE-LABST + GS_MARDH_BEFORE-UMLME + GS_MARDH_BEFORE-INSME +
                                              GS_MARDH_BEFORE-EINME + GS_MARDH_BEFORE-SPEME + GS_MARDH_BEFORE-RETME .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MARDH_BEFORE .
      ENDLOOP.
    ENDIF.

    READ TABLE GT_MSKA_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MSKA_BEFORE INTO GS_MSKA_BEFORE FROM SY-TABIX .
        IF GS_MSKA_BEFORE-MATNR = <FS_ALV1>-MATNR .
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MSKA_BEFORE-KALAB + GS_MSKA_BEFORE-KAINS + GS_MSKA_BEFORE-KASPE +
                                              GS_MSKA_BEFORE-KAEIN .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MSKA_BEFORE .
      ENDLOOP.
    ENDIF.
    READ TABLE GT_MSKAH_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MSKAH_BEFORE INTO GS_MSKAH_BEFORE FROM SY-TABIX .
        IF GS_MSKAH_BEFORE-MATNR = <FS_ALV1>-MATNR .
*          READ TABLE LT_ALV2 WITH KEY MATNR = GS_MSKAH_BEFORE-MATNR
*                                      WERKS = GS_MSKAH_BEFORE-WERKS
*                                      LGORT = GS_MSKAH_BEFORE-LGORT
*                                      VBELN = GS_MSKAH_BEFORE-VBELN
*                                      POSNR = GS_MSKAH_BEFORE-POSNR BINARY SEARCH TRANSPORTING NO FIELDS.
*          IF SY-SUBRC = 0 .
*            CONTINUE .
*          ENDIF.
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MSKAH_BEFORE-KALAB + GS_MSKAH_BEFORE-KAINS + GS_MSKAH_BEFORE-KASPE +
                                              GS_MSKAH_BEFORE-KAEIN .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MSKAH_BEFORE .
      ENDLOOP.
    ENDIF.

    READ TABLE GT_MSLB_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MSLB_BEFORE INTO GS_MSLB_BEFORE FROM SY-TABIX .
        IF GS_MSLB_BEFORE-MATNR = <FS_ALV1>-MATNR .
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MSLB_BEFORE-LBLAB + GS_MSLB_BEFORE-LBINS + GS_MSLB_BEFORE-LBUML .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MSLB_BEFORE .
      ENDLOOP.
    ENDIF.
    READ TABLE GT_MSLBH_BEFORE WITH KEY MATNR = <FS_ALV1>-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_MSLBH_BEFORE INTO GS_MSLBH_BEFORE FROM SY-TABIX .
        IF GS_MSLBH_BEFORE-MATNR = <FS_ALV1>-MATNR .
*          READ TABLE LT_ALV3 WITH KEY MATNR = GS_MSLBH_BEFORE-MATNR
*                                      WERKS = GS_MSLBH_BEFORE-WERKS
*                                      LIFNR = GS_MSLBH_BEFORE-LIFNR BINARY SEARCH TRANSPORTING NO FIELDS .
*          IF SY-SUBRC = 0 .
*            CONTINUE .
*          ENDIF.
          <FS_ALV1>-MENGE = <FS_ALV1>-MENGE + GS_MSLBH_BEFORE-LBLAB + GS_MSLBH_BEFORE-LBINS + GS_MSLBH_BEFORE-LBUML .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR GS_MSLBH_BEFORE .
      ENDLOOP.
    ENDIF.

    " 计算当前折旧总费用
    READ TABLE LT_ANLP WITH KEY ANLN1 = <FS_ALV1>-ANLN1 BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT LT_ANLP INTO LS_ANLP FROM SY-TABIX .
        IF LS_ANLP-ANLN1 = <FS_ALV1>-ANLN1 .
          <FS_ALV1>-NAFAZ = <FS_ALV1>-NAFAZ + LS_ANLP-NAFAZ .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR LS_ANLP .
      ENDLOOP.
    ENDIF.

    " 计算分摊比例
    IF <FS_ALV1>-MENGE NE 0 .
      <FS_ALV1>-FTBL = <FS_ALV1>-OS_QTY / <FS_ALV1>-MENGE .
    ELSE .
    ENDIF.
    " 计算分摊折旧成本
    <FS_ALV1>-FTZJCB = <FS_ALV1>-FTBL * <FS_ALV1>-NAFAZ .

    CLEAR LS_MAKT .
    CLEAR LS_MARA .
    CLEAR LS_T001L .
    CLEAR LS_LFA1 .
  ENDLOOP.

* 删除期初占用数量=0的行
  DELETE GT_ALV1 WHERE OS_QTY = 0.

* 分摊折旧成本尾差放到金额最大的行项目上去
  DATA: BEGIN OF LS_ALV1 ,
          ANLN1  TYPE ANLA-ANLN1,
          SEL    TYPE C,
          NUM    TYPE I,
          WERKS  TYPE T001W-WERKS,
          MATNR  TYPE MARA-MATNR,
          MAKTX  TYPE MAKT-MAKTX,
          LGORT  TYPE T001L-LGORT,
          LGOBE  TYPE T001L-LGOBE,
          VBELN  TYPE MSKA-VBELN,
          POSNR  TYPE MSKA-POSNR,
          LIFNR  TYPE LFA1-LIFNR,
          NAME1  TYPE LFA1-NAME1,
          ZYWLX  TYPE ZZLYW-ZYWLX,
          OS_QTY TYPE MARD-LABST,
          MENGE  TYPE MARD-LABST,
          MEINS  TYPE MARA-MEINS,
          NAFAZ  TYPE ANLP-NAFAZ,
          FTBL   TYPE ANLP-NAFAZ,
          FTZJCB TYPE ANLP-NAFAZ,
        END OF LS_ALV1 .
  DATA LT_ALV1 LIKE TABLE OF LS_ALV1 .
  DATA: LT_ALV2 LIKE TABLE OF LS_ALV1,
        LS_ALV2 LIKE LINE OF LT_ALV2.
  DATA LS_ALV1_1 LIKE LS_ALV1 .
  DATA LS_ALV1_2 LIKE LS_ALV1 .
  DATA L_ZJCB TYPE ANLP-NAFAZ .

  MOVE-CORRESPONDING GT_ALV1 TO LT_ALV1 .
  SORT LT_ALV1 BY ANLN1 ASCENDING FTZJCB DESCENDING .

  " 将成本价先换算成绝对值
  MOVE-CORRESPONDING LT_ALV1 TO LT_ALV2 .
  LOOP AT LT_ALV2 INTO LS_ALV2 .
    LS_ALV2-FTZJCB = ABS( LS_ALV2-FTZJCB ) .
    MODIFY LT_ALV2 FROM LS_ALV2 .
    CLEAR LS_ALV2 .
  ENDLOOP.
  SORT LT_ALV2 BY ANLN1 ASCENDING FTZJCB DESCENDING .
  DELETE ADJACENT DUPLICATES FROM LT_ALV2 COMPARING ANLN1 .

  LOOP AT LT_ALV1 INTO LS_ALV1 .
    MOVE-CORRESPONDING LS_ALV1 TO LS_ALV1_1 .
    L_ZJCB = L_ZJCB + LS_ALV1_1-FTZJCB .
    " 取出成本绝对值最大的一行
    READ TABLE LT_ALV2 WITH KEY ANLN1 = LS_ALV1-ANLN1 BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LS_ALV1_1 TO LS_ALV1_2 .
    ENDIF.
    AT END OF ANLN1 .
      LS_ALV1_2-FTZJCB = LS_ALV1_2-NAFAZ - ( L_ZJCB - LS_ALV1_2-FTZJCB ) .
      MOVE-CORRESPONDING LS_ALV1_2 TO GS_ALV1 .
      MODIFY GT_ALV1 FROM GS_ALV1 TRANSPORTING FTZJCB WHERE NUM = GS_ALV1-NUM .
      CLEAR L_ZJCB .
      CLEAR GS_ALV1 .
      CLEAR LS_ALV1_2 .
    ENDAT .
    CLEAR LS_ALV1_1 .
    CLEAR LS_ALV1 .
  ENDLOOP.

  REFRESH: LT_ALV1,LT_ALV2.
  CLEAR: LS_ALV1,LS_ALV2,LS_ALV1_1,LS_ALV1_2.
  CLEAR L_ZJCB .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INITIAL_STATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INITIAL_STATE .
  PERFORM FRM_CHECK_VAR_ALV .  "选择界面参数校验
  PERFORM FRM_CLEAR_ALV_VAR . " 清空ALV 参数
  PERFORM FRM_GET_BASE .  "获取基础数据(工厂、库存、租赁自建表)
  PERFORM FRM_GET_DEAL_DATA_ALV1 .  "数据读取和整理
  PERFORM FRM_SHOW_ALV .  " ALV 展示
  PERFORM FRM_SAVE_DATA . " 将ALV 的值保存起来，后面过账用
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_VAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_VAR_ALV .
  IF P_BUKRS IS INITIAL .
    MESSAGE '公司代码必填' TYPE 'E' .
  ENDIF.
  IF P_WERKS IS INITIAL .
    MESSAGE '工厂必填' TYPE 'E' .
  ENDIF.
  IF P_LFGJA IS INITIAL .
    MESSAGE '会计年度必填' TYPE 'E' .
  ENDIF.
  IF P_LFMON IS INITIAL .
    MESSAGE '会计期间必填' TYPE 'E' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .
  PERFORM INIT_LAYOUT.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.            "字段列定义
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
*  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT  .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GS_LAYOUT-ZEBRA        = 'X'.
  GS_LAYOUT-CWIDTH_OPT   = 'X'.
  GS_LAYOUT-BOX_FNAME = 'SEL'.
*  GS_LAYOUT-INFO_FNAME = 'CLR'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .

  IF SAVE_OK = 'BUT1'.

    INIT_FIELDCAT 'NUM'        '序号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' 'MARA' 'MATNR'.
    INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LGORT'        '库存地点'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LGOBE'        '库存地点描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'VBELN'        '销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POSNR'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LIFNR'        '供应商'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAME1'        '供应商描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYWLX'        '业务区分'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'OS_QTY'        '期初占用数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'        '期初总数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'        '单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ANLN1'        '固定资产'         '' '' '' '' '' 'ANLA' 'ANLN1'.
    INIT_FIELDCAT 'NAFAZ'        '当月折旧总费用'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FTBL'        '分摊比例'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FTZJCB'        '分摊折旧成本额'         '' '' '' '' '' '' ''.

  ELSEIF SAVE_OK = 'BUT2'.

    INIT_FIELDCAT 'NUM'        '序号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MBLNR'        '物料凭证'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BWART'        '移动类型'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUDAT'        '过账日期'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' 'MARA' 'MATNR'.
    INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LGORT'        '从_库存地点'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LGOBE1'        '从_库存地点描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SOBKZ'        '从_特殊库存标识'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MAT_KDAUF'        '从_销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MAT_KDPOS'        '从_行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LIFNR1'        '从_供应商'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAME1'        '从_供应商描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYWLX1'        '从_业务区分'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'UMLGO'        '到_库存地点'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LGOBE2'        '到_库存地点描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'UMSOK'        '到_特殊库存标识'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'KDAUF'        '到_销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'KDPOS'        '到_行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LIFNR2'        '到_供应商'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAME2'        '到_供应商描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYWLX2'        '到_业务区分'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'        '占用数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'QCKC'        '期初总数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'        '单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ANLN1'        '固定资产'         '' '' '' '' '' 'ANLA' 'ANLN1'.
    INIT_FIELDCAT 'NAFAZ'        '当月折旧总费用'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYTS'        '占用天数'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'DYTS'        '当月天数'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FTBL'        '分摊比例'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FTZJCB'        '分摊折旧成本'         '' '' '' '' '' '' ''.

  ELSEIF SAVE_OK = 'EXCU3' .

    INIT_FIELDCAT 'NUM'        '序号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUKRS'        '公司代码'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'GJAHR'        '会计年度'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MONAT'        '会计期间'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUDAT'        '过账日期'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYWLX'        '业务类型'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'        '物料编号'         '' '' '' '' '' 'MARA' 'MATNR'.
    INIT_FIELDCAT 'LGORT'        '库存地点'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'VBELN'        '销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POSNR'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LIFNR'        '供应商'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAME1'        '供应商描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'        '期初占用数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE_SUM'        '期初总数量'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'        '单位'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAFAZ'        '当月折旧总费用'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZYTS'        '占用天数'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'DYTS'        '当月天数'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZFTBL'        '分摊比例'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAFAP_DIV'        '分摊折旧成本'         '' '' '' '' '' '' ''.

  ELSEIF SAVE_OK = 'EXCU4' .

    INIT_FIELDCAT 'NUM'        '序号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUKRS'        '公司代码'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'        '物料编号'         '' '' '' '' '' 'MARA' 'MATNR'.
    INIT_FIELDCAT 'VBELN'        '销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POSNR'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAFAP_DIV'        '分摊折旧成本'         '' '' '' '' '' '' ''.

  ELSEIF SAVE_OK = 'EXCU'.

    INIT_FIELDCAT 'NUM'        '序号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUKRS'        '公司代码'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'GJAHR'        '会计年度'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MONAT'        '会计期间'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'        '物料编号'         '' '' '' '' '' 'MARA' 'MATNR'.
    INIT_FIELDCAT 'VBELN'        '销售订单'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
*    INIT_FIELDCAT 'POSNR'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NAFAZ'        '分摊折旧成本'         '' '' '' '' '' '' ''.
*    INIT_FIELDCAT 'FLAG'        '凭证行类型'         '' '' '' '' '' '' ''.
*    INIT_FIELDCAT 'TEXT'        '凭证行类型描述'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BELNR'        '会计凭证'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUZEI'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TYPE'        '消息类型'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MSG'        '具体消息'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BELNR_REV'        '冲销凭证号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUZEI_REV'        '行号'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TYPE_REV'        '冲销的消息类型'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MSG_REV'        '冲销的具体消息'         '' '' '' '' '' '' ''.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .

  IF SAVE_OK = 'BUT1' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER =
*       I_BUFFER_ACTIVE    =
        I_CALLBACK_PROGRAM = SY-REPID
*       I_CALLBACK_PF_STATUS_SET = PU_STATUS
*       I_CALLBACK_USER_COMMAND  = PU_UCOMM
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   = ''
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    = PW_GRID_SETTINGS
        IS_LAYOUT_LVC      = GS_LAYOUT
        IT_FIELDCAT_LVC    = GT_LVC
*       IT_EXCLUDING       = GT_EXCLUDE
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC        = PT_SORT[]
*       IT_FILTER_LVC      =
*       IT_HYPERLINK       =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        I_SAVE             = 'A'
*       IS_VARIANT         = PW_VARIANT
*       IT_EVENTS          = GT_EVENTS
*       IT_EVENT_EXIT      =
*       IS_PRINT_LVC       =
*       IS_REPREP_ID_LVC   =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
*       IT_ALV_GRAPHICS    =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB           = GT_ALV1
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF SAVE_OK = 'BUT2' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER =
*       I_BUFFER_ACTIVE    =
        I_CALLBACK_PROGRAM = SY-REPID
*       I_CALLBACK_PF_STATUS_SET = PU_STATUS
*       I_CALLBACK_USER_COMMAND  = PU_UCOMM
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   = ''
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    = PW_GRID_SETTINGS
        IS_LAYOUT_LVC      = GS_LAYOUT
        IT_FIELDCAT_LVC    = GT_LVC
*       IT_EXCLUDING       = GT_EXCLUDE
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC        = PT_SORT[]
*       IT_FILTER_LVC      =
*       IT_HYPERLINK       =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        I_SAVE             = 'A'
*       IS_VARIANT         = PW_VARIANT
*       IT_EVENTS          = GT_EVENTS
*       IT_EVENT_EXIT      =
*       IS_PRINT_LVC       =
*       IS_REPREP_ID_LVC   =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
*       IT_ALV_GRAPHICS    =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB           = GT_ALV2
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF SAVE_OK = 'EXCU3' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER =
*       I_BUFFER_ACTIVE    =
        I_CALLBACK_PROGRAM = SY-REPID
*       I_CALLBACK_PF_STATUS_SET = PU_STATUS
*       I_CALLBACK_USER_COMMAND  = PU_UCOMM
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   = ''
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    = PW_GRID_SETTINGS
        IS_LAYOUT_LVC      = GS_LAYOUT
        IT_FIELDCAT_LVC    = GT_LVC
*       IT_EXCLUDING       = GT_EXCLUDE
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC        = PT_SORT[]
*       IT_FILTER_LVC      =
*       IT_HYPERLINK       =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        I_SAVE             = 'A'
*       IS_VARIANT         = PW_VARIANT
*       IT_EVENTS          = GT_EVENTS
*       IT_EVENT_EXIT      =
*       IS_PRINT_LVC       =
*       IS_REPREP_ID_LVC   =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
*       IT_ALV_GRAPHICS    =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB           = GT_ALV3
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF SAVE_OK = 'EXCU4' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER =
*       I_BUFFER_ACTIVE    =
        I_CALLBACK_PROGRAM = SY-REPID
*       I_CALLBACK_PF_STATUS_SET = PU_STATUS
*       I_CALLBACK_USER_COMMAND  = PU_UCOMM
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   = ''
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    = PW_GRID_SETTINGS
        IS_LAYOUT_LVC      = GS_LAYOUT
        IT_FIELDCAT_LVC    = GT_LVC
*       IT_EXCLUDING       = GT_EXCLUDE
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC        = PT_SORT[]
*       IT_FILTER_LVC      =
*       IT_HYPERLINK       =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        I_SAVE             = 'A'
*       IS_VARIANT         = PW_VARIANT
*       IT_EVENTS          = GT_EVENTS
*       IT_EVENT_EXIT      =
*       IS_PRINT_LVC       =
*       IS_REPREP_ID_LVC   =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
*       IT_ALV_GRAPHICS    =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB           = GT_ALV4
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF SAVE_OK = 'EXCU' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK        = ' '
*       I_BYPASSING_BUFFER       =
*       I_BUFFER_ACTIVE          =
        I_CALLBACK_PROGRAM       = SY-REPID
        I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'
        I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME         = ''
*       I_BACKGROUND_ID          = ' '
*       I_GRID_TITLE             =
*       I_GRID_SETTINGS          = PW_GRID_SETTINGS
        IS_LAYOUT_LVC            = GS_LAYOUT
        IT_FIELDCAT_LVC          = GT_LVC
*       IT_EXCLUDING             = GT_EXCLUDE
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC              = PT_SORT[]
*       IT_FILTER_LVC            =
*       IT_HYPERLINK             =
*       IS_SEL_HIDE              =
*       I_DEFAULT                = 'X'
        I_SAVE                   = 'A'
*       IS_VARIANT               = PW_VARIANT
*       IT_EVENTS                = GT_EVENTS
*       IT_EVENT_EXIT            =
*       IS_PRINT_LVC             =
*       IS_REPREP_ID_LVC         =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE        = 0
*       I_HTML_HEIGHT_TOP        =
*       I_HTML_HEIGHT_END        =
*       IT_ALV_GRAPHICS          =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB                 = GT_ALV5_1
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CLEAR_ALV_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CLEAR_ALV_VAR .
  REFRESH: GT_ALV1,GT_ALV2,GT_ALV5,GT_LVC .
  CLEAR: GS_LAYOUT .
ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SAVE_DATA .
  MOVE-CORRESPONDING GT_ALV1 TO GT_ALV_POST1 .
  MOVE-CORRESPONDING GT_ALV2 TO GT_ALV_POST2 .
  MOVE-CORRESPONDING GT_ALV2 TO GT_ALV_POST3 .
  REFRESH: GT_ALV1,GT_ALV2,GT_ALV5_1 .
  SORT GT_ALV_POST1 BY WERKS MATNR VBELN .
  SORT GT_ALV_POST2 BY WERKS MATNR MAT_KDAUF .
  SORT GT_ALV_POST3 BY WERKS MATNR KDAUF .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FMR_STORAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FMR_STORAGE .
  PERFORM FRM_CHECK_VAR_ALV .  "选择界面参数校验
  PERFORM FRM_CLEAR_ALV_VAR . " 清空ALV 参数
  PERFORM FRM_GET_BASE .  "获取基础数据(工厂、库存、租赁自建表)
  PERFORM FRM_GET_DEAL_DATA_ALV2 .  "数据读取和整理
  PERFORM FRM_SHOW_ALV .  " ALV 展示
  PERFORM FRM_SAVE_DATA . " 将GT_ALV1的值保存起来，后面过账用
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DEAL_DATA_ALV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DEAL_DATA_ALV2 .

  " 业务数据
  DATA: BEGIN OF LS_MKSG ,
          MBLNR     TYPE MSEG-MBLNR,
          BWART     TYPE MSEG-BWART,
          BUDAT     TYPE MKPF-BUDAT,
          WERKS     TYPE MSEG-WERKS,
          MATNR     TYPE MSEG-MATNR,
          LGORT     TYPE MSEG-LGORT,
          SOBKZ     TYPE MSEG-SOBKZ,
          MAT_KDAUF TYPE MSEG-MAT_KDAUF,
          MAT_KDPOS TYPE MSEG-MAT_KDPOS,
          LIFNR     TYPE MSEG-LIFNR,
          UMLGO     TYPE MSEG-UMLGO,
          UMSOK     TYPE MSEG-UMSOK,
          KDAUF     TYPE MSEG-KDAUF,
          KDPOS     TYPE MSEG-KDPOS,
          MENGE     TYPE MSEG-MENGE,
          MEINS     TYPE MSEG-MEINS,
          SHKZG     TYPE MSEG-SHKZG,
        END OF LS_MKSG .
  DATA LT_MKSG LIKE TABLE OF LS_MKSG .

  " 物料描述
  DATA: LT_MAKT TYPE TABLE OF MAKT,
        LS_MAKT TYPE MAKT.
  " 库存地点描述
  DATA: BEGIN OF LS_T001L ,
          WERKS TYPE T001L-WERKS,
          LGORT TYPE T001L-LGORT,
          LGOBE TYPE T001L-LGOBE,
        END OF LS_T001L .
  DATA LT_T001L LIKE TABLE OF LS_T001L .
  " 供应商描述
  DATA: BEGIN OF LS_LFA1 ,
          LIFNR TYPE LFA1-LIFNR,
          NAME1 TYPE LFA1-NAME1,
        END OF LS_LFA1.
  DATA LT_LFA1 LIKE TABLE OF LS_LFA1 .
  " 资产折旧费
  DATA: BEGIN OF LS_ANLP ,
          BUKRS TYPE ANLP-BUKRS,
          GJAHR TYPE ANLP-GJAHR,
          PERAF TYPE ANLP-PERAF,
          ANLN1 TYPE ANLP-ANLN1,
          ANLN2 TYPE ANLP-ANLN2,
          NAFAZ TYPE ANLP-NAFAZ,
        END OF LS_ANLP .
  DATA LT_ANLP LIKE TABLE OF LS_ANLP .

  " 物料凭证过账日期范围
  DATA: L_BUDAT_BEGIN(8) TYPE C,
        L_BUDAT_END(8)   TYPE C.
  CONCATENATE P_LFGJA P_LFMON '01' INTO L_BUDAT_BEGIN .
  CONCATENATE P_LFGJA P_LFMON '31' INTO L_BUDAT_END .

  " XXX
  DATA: L_NUM TYPE I VALUE IS INITIAL,  " ALV2 行数
        L_LEN TYPE I VALUE IS INITIAL.  " 取物料后12位作为固定资产
  " DATE_GET_MONTH_LASTDAY 的参数
  DATA: L_I_DATE TYPE SY-DATUM,
        L_E_DATE TYPE SY-DATUM.
  " FIMA_DAYS_AND_MONTHS_AND_YEARS 的参数
  DATA: L_DATE_FROM TYPE VTBBEWE-DBERVON,
        L_DATE_TO   TYPE VTBBEWE-DBERBIS,
        L_E_DAYS    TYPE VTBBEWE-ATAGE.

  IF GT_T001W IS NOT INITIAL .

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_MKSG
      FROM MKPF AS A
     INNER JOIN MSEG AS B
        ON A~MBLNR = B~MBLNR
       AND A~MJAHR = B~MJAHR
       FOR ALL ENTRIES IN GT_T001W
     WHERE B~WERKS = GT_T001W-WERKS
       AND B~BWART IN ('411','412','413','414','311','312','Z07','Z08','Z09','Z10','Z42')
       AND B~PARENT_ID = ''
       AND A~BUDAT BETWEEN L_BUDAT_BEGIN AND L_BUDAT_END .
    IF LT_MKSG IS NOT INITIAL .

      LOOP AT LT_MKSG INTO LS_MKSG .
        L_NUM = L_NUM + 1 .
        GS_ALV2-NUM = L_NUM .
        GS_ALV2-MBLNR = LS_MKSG-MBLNR .
        GS_ALV2-BWART = LS_MKSG-BWART .
        GS_ALV2-BUDAT = LS_MKSG-BUDAT .
        GS_ALV2-WERKS = LS_MKSG-WERKS .
        GS_ALV2-MATNR = LS_MKSG-MATNR .
        IF LS_MKSG-SHKZG = 'H'.
          " 从
          GS_ALV2-LGORT = LS_MKSG-LGORT .
          GS_ALV2-SOBKZ = LS_MKSG-SOBKZ .
          GS_ALV2-MAT_KDAUF = LS_MKSG-MAT_KDAUF .
          GS_ALV2-MAT_KDPOS = LS_MKSG-MAT_KDPOS .
          GS_ALV2-LIFNR1 = LS_MKSG-LIFNR .
          READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = LS_MKSG-WERKS
                                                     LGORT = LS_MKSG-LGORT
                                                     SOBKZ = LS_MKSG-SOBKZ BINARY SEARCH .
          IF SY-SUBRC = 0 .
            GS_ALV2-ZYWLX1 = GS_ZZLYW-ZYWLX .
            IF LS_MKSG-MAT_KDAUF IS INITIAL .
              GS_ALV2-MAT_KDAUF = GS_ZZLYW-VBELN .
              GS_ALV2-MAT_KDPOS = GS_ZZLYW-POSNR .
            ENDIF.
            CLEAR GS_ZZLYW .
          ENDIF.
          " 到
          GS_ALV2-UMLGO = LS_MKSG-UMLGO .
          GS_ALV2-UMSOK = LS_MKSG-UMSOK .
          GS_ALV2-KDAUF = LS_MKSG-KDAUF .
          GS_ALV2-KDPOS = LS_MKSG-KDPOS .
          GS_ALV2-LIFNR2 = LS_MKSG-LIFNR .
          READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = LS_MKSG-WERKS
                                                     LGORT = LS_MKSG-UMLGO
                                                     SOBKZ = LS_MKSG-UMSOK BINARY SEARCH .
          IF SY-SUBRC = 0 .
            GS_ALV2-ZYWLX2 = GS_ZZLYW-ZYWLX .
            IF LS_MKSG-KDAUF IS INITIAL .
              GS_ALV2-KDAUF = GS_ZZLYW-VBELN .
              GS_ALV2-KDPOS = GS_ZZLYW-POSNR .
            ENDIF.
            CLEAR GS_ZZLYW .
          ENDIF.

        ELSEIF LS_MKSG-SHKZG = 'S'.
          " 从
          GS_ALV2-LGORT = LS_MKSG-UMLGO .
          GS_ALV2-SOBKZ = LS_MKSG-UMSOK .
          GS_ALV2-MAT_KDAUF = LS_MKSG-KDAUF .
          GS_ALV2-MAT_KDPOS = LS_MKSG-KDPOS .
          GS_ALV2-LIFNR1 = LS_MKSG-LIFNR .
          READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = LS_MKSG-WERKS
                                                     LGORT = LS_MKSG-UMLGO
                                                     SOBKZ = LS_MKSG-UMSOK BINARY SEARCH .
          IF SY-SUBRC = 0 .
            GS_ALV2-ZYWLX1 = GS_ZZLYW-ZYWLX .
            IF LS_MKSG-KDAUF IS INITIAL .
              GS_ALV2-MAT_KDAUF = GS_ZZLYW-VBELN .
              GS_ALV2-MAT_KDPOS = GS_ZZLYW-POSNR .
            ENDIF.
            CLEAR GS_ZZLYW .
          ENDIF.
          " 到
          GS_ALV2-UMLGO = LS_MKSG-LGORT .
          GS_ALV2-UMSOK = LS_MKSG-SOBKZ .
          GS_ALV2-KDAUF = LS_MKSG-MAT_KDAUF .
          GS_ALV2-KDPOS = LS_MKSG-MAT_KDPOS .
          GS_ALV2-LIFNR2 = LS_MKSG-LIFNR .
          READ TABLE GT_ZZLYW INTO GS_ZZLYW WITH KEY WERKS = LS_MKSG-WERKS
                                                     LGORT = LS_MKSG-LGORT
                                                     SOBKZ = LS_MKSG-SOBKZ BINARY SEARCH .
          IF SY-SUBRC = 0 .
            GS_ALV2-ZYWLX2 = GS_ZZLYW-ZYWLX .
            IF LS_MKSG-MAT_KDAUF IS INITIAL .
              GS_ALV2-KDAUF = GS_ZZLYW-VBELN .
              GS_ALV2-KDPOS = GS_ZZLYW-POSNR .
            ENDIF.
            CLEAR GS_ZZLYW .
          ENDIF.

        ENDIF.

        GS_ALV2-MENGE = LS_MKSG-MENGE . " 占用数量

        " 赋值当前期初总数量(包括主库存、特殊库存E，特殊库存O )
        READ TABLE GT_MARD_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MARD_BEFORE INTO GS_MARD_BEFORE FROM SY-TABIX .
            IF GS_MARD_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MARD_BEFORE-LABST + GS_MARD_BEFORE-UMLME + GS_MARD_BEFORE-INSME +
                                            GS_MARD_BEFORE-EINME + GS_MARD_BEFORE-SPEME + GS_MARD_BEFORE-RETME .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MARD_BEFORE .
          ENDLOOP.
        ENDIF.
        READ TABLE GT_MARDH_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MARDH_BEFORE INTO GS_MARDH_BEFORE FROM SY-TABIX .
            IF GS_MARDH_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MARDH_BEFORE-LABST + GS_MARDH_BEFORE-UMLME + GS_MARDH_BEFORE-INSME +
                                            GS_MARDH_BEFORE-EINME + GS_MARDH_BEFORE-SPEME + GS_MARDH_BEFORE-RETME .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MARDH_BEFORE .
          ENDLOOP.
        ENDIF.

        READ TABLE GT_MSKA_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MSKA_BEFORE INTO GS_MSKA_BEFORE FROM SY-TABIX .
            IF GS_MSKA_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MSKA_BEFORE-KALAB + GS_MSKA_BEFORE-KAINS + GS_MSKA_BEFORE-KASPE +
                                            GS_MSKA_BEFORE-KAEIN .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MSKA_BEFORE .
          ENDLOOP.
        ENDIF.
        READ TABLE GT_MSKAH_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MSKAH_BEFORE INTO GS_MSKAH_BEFORE FROM SY-TABIX .
            IF GS_MSKAH_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MSKAH_BEFORE-KALAB + GS_MSKAH_BEFORE-KAINS + GS_MSKAH_BEFORE-KASPE +
                                            GS_MSKAH_BEFORE-KAEIN .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MSKAH_BEFORE .
          ENDLOOP.
        ENDIF.

        READ TABLE GT_MSLB_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MSLB_BEFORE INTO GS_MSLB_BEFORE FROM SY-TABIX .
            IF GS_MSLB_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MSLB_BEFORE-LBLAB + GS_MSLB_BEFORE-LBINS + GS_MSLB_BEFORE-LBUML .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MSLB_BEFORE .
          ENDLOOP.
        ENDIF.
        READ TABLE GT_MSLBH_BEFORE WITH KEY MATNR = LS_MKSG-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_MSLBH_BEFORE INTO GS_MSLBH_BEFORE FROM SY-TABIX .
            IF GS_MSLBH_BEFORE-MATNR = LS_MKSG-MATNR .
              GS_ALV2-QCKC = GS_ALV2-QCKC + GS_MSLBH_BEFORE-LBLAB + GS_MSLBH_BEFORE-LBINS + GS_MSLBH_BEFORE-LBUML .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR GS_MSLBH_BEFORE .
          ENDLOOP.
        ENDIF.

        " 单位
        GS_ALV2-MEINS = LS_MKSG-MEINS .
        " 将物料号后12位作为固定资产编号
        L_LEN = STRLEN( LS_MKSG-MATNR ) .
        L_LEN = L_LEN - 12 .
        GS_ALV2-ANLN1 = LS_MKSG-MATNR+L_LEN(12) .

        " 取当月最后一天
        L_I_DATE = LS_MKSG-BUDAT .
        CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
          EXPORTING
            I_DATE = L_I_DATE
          IMPORTING
            E_DATE = L_E_DATE.
        L_DATE_FROM = LS_MKSG-BUDAT.
        L_DATE_TO = L_E_DATE .
        CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          EXPORTING
            I_DATE_FROM = L_DATE_FROM
            I_DATE_TO   = L_DATE_TO
          IMPORTING
            E_DAYS      = L_E_DAYS.
        GS_ALV2-ZYTS = L_E_DAYS .
        GS_ALV2-DYTS = L_E_DATE+6(2) .

        APPEND GS_ALV2 TO GT_ALV2 .
        CLEAR GS_ALV2 .

        CLEAR L_LEN .
        CLEAR: L_I_DATE,L_E_DATE .
        CLEAR: L_DATE_FROM,L_DATE_TO,L_E_DAYS.
        CLEAR LS_MKSG .
      ENDLOOP.

* 根据ALV里的数据将剩下的没赋值的字段的值从表里取出
      SELECT *
        INTO TABLE LT_MAKT
        FROM MAKT
         FOR ALL ENTRIES IN GT_ALV2
       WHERE MATNR = GT_ALV2-MATNR
         AND SPRAS = SY-LANGU .
      SORT LT_MAKT BY MATNR .

      SELECT WERKS
             LGORT
             LGOBE
        INTO CORRESPONDING FIELDS OF TABLE LT_T001L
        FROM T001L
         FOR ALL ENTRIES IN GT_T001W
       WHERE WERKS = GT_T001W-WERKS .
      SORT LT_T001L BY WERKS LGORT .

      SELECT LIFNR
             NAME1
        INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
        FROM LFA1
         FOR ALL ENTRIES IN LT_MKSG
       WHERE LIFNR = LT_MKSG-LIFNR .
      SORT LT_LFA1 BY LIFNR .

*      SELECT MATNR
*             MEINS
*        INTO CORRESPONDING FIELDS OF TABLE LT_MARA
*        FROM MARA
*         FOR ALL ENTRIES IN GT_ALV2
*       WHERE MATNR = GT_ALV2-MATNR .
*      SORT LT_MARA BY MATNR .

      SELECT BUKRS
             GJAHR
             PERAF
             ANLN1
             ANLN2
             NAFAZ
        INTO CORRESPONDING FIELDS OF TABLE LT_ANLP
        FROM ANLP
         FOR ALL ENTRIES IN GT_ALV2
       WHERE BUKRS = P_BUKRS
         AND GJAHR = P_LFGJA
         AND PERAF = P_LFMON
         AND ANLN1 = GT_ALV2-ANLN1 .
      SORT LT_ANLP BY ANLN1 .

* 根据ALV里的数据给剩下的没赋值的字段赋值
      LOOP AT GT_ALV2 ASSIGNING <FS_ALV2> .
        " 给物料描述赋值
        IF <FS_ALV2>-MATNR IS NOT INITIAL .
          READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = <FS_ALV2>-MATNR BINARY SEARCH .
          IF SY-SUBRC = 0 .
            <FS_ALV2>-MAKTX = LS_MAKT-MAKTX .
            CLEAR LS_MAKT .
          ENDIF.
*          READ TABLE LT_MARA INTO LS_MARA WITH KEY MATNR = <FS_ALV2>-MATNR BINARY SEARCH .
*          IF SY-SUBRC = 0 .
*            <FS_ALV2>-MEINS = LS_MARA-MEINS .
*            CLEAR LS_MAKT .
*          ENDIF.
        ENDIF.
        " 给库存地点描述赋值
        IF <FS_ALV2>-LGORT IS NOT INITIAL .
          READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = <FS_ALV2>-WERKS
                                                     LGORT = <FS_ALV2>-LGORT BINARY SEARCH .
          IF SY-SUBRC = 0 .
            <FS_ALV2>-LGOBE1 = LS_T001L-LGOBE .
            CLEAR LS_T001L .
          ENDIF.
        ENDIF.
        IF <FS_ALV2>-UMLGO IS NOT INITIAL .
          READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = <FS_ALV2>-WERKS
                                                     LGORT = <FS_ALV2>-UMLGO BINARY SEARCH .
          IF SY-SUBRC = 0 .
            <FS_ALV2>-LGOBE2 = LS_T001L-LGOBE .
            CLEAR LS_T001L .
          ENDIF.
        ENDIF.
        " 给供应商描述赋值
        IF <FS_ALV2>-LIFNR1 IS NOT INITIAL .
          READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = <FS_ALV2>-LIFNR1 BINARY SEARCH .
          IF SY-SUBRC = 0 .
            <FS_ALV2>-NAME1 = LS_LFA1-NAME1 .
            CLEAR LS_LFA1 .
          ENDIF.
        ENDIF.
        IF <FS_ALV2>-LIFNR2 IS NOT INITIAL .
          READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = <FS_ALV2>-LIFNR2 BINARY SEARCH .
          IF SY-SUBRC = 0 .
            <FS_ALV2>-NAME2 = LS_LFA1-NAME1 .
            CLEAR LS_LFA1 .
          ENDIF.
        ENDIF.
        " 计算当前折旧总费用
        READ TABLE LT_ANLP WITH KEY ANLN1 = <FS_ALV2>-ANLN1 BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT LT_ANLP INTO LS_ANLP FROM SY-TABIX .
            IF LS_ANLP-ANLN1 = <FS_ALV2>-ANLN1 .
              <FS_ALV2>-NAFAZ = <FS_ALV2>-NAFAZ + LS_ANLP-NAFAZ .
            ELSE .
              EXIT .
            ENDIF.
            CLEAR LS_ANLP .
          ENDLOOP.
        ENDIF.
        " 计算分摊比例
        IF <FS_ALV2>-QCKC NE 0 .
          <FS_ALV2>-FTBL = ( <FS_ALV2>-MENGE / <FS_ALV2>-QCKC ) * ( <FS_ALV2>-ZYTS / <FS_ALV2>-DYTS ) .
        ELSE .
        ENDIF.
        " 计算分摊折旧成本
        <FS_ALV2>-FTZJCB = <FS_ALV2>-FTBL * <FS_ALV2>-NAFAZ .

      ENDLOOP.

    ELSE .
      IF G_BUT2 = 'X'.
        MESSAGE '没数据' TYPE 'E'   .
      ENDIF.
    ENDIF.

  ELSE .
    MESSAGE '该公司代码下没有该工厂' TYPE 'E'.
  ENDIF.

* 800系统去掉 4900365831 4900368895  2张物料凭证，数据错误
  IF SY-MANDT = '800'.
    DELETE GT_ALV2 WHERE MBLNR = '4900365831' OR MBLNR = '4900368895' .
  ENDIF.

* 删除期初占用数量=0的行
  DELETE GT_ALV2 WHERE MENGE = 0 .

* 分摊折旧成本最后一行倒算
*  DATA: BEGIN OF LS_ALV2 ,
*          ANLN1     TYPE ANLP-ANLN1,
*          SEL       TYPE C,
*          NUM       TYPE I,
*          MBLNR     TYPE MSEG-MBLNR,
*          BWART     TYPE MSEG-BWART,
*          BUDAT     TYPE MKPF-BUDAT,
*          WERKS     TYPE MSEG-WERKS,
*          MATNR     TYPE MSEG-MATNR,
*          MAKTX     TYPE MAKT-MAKTX,
*          LGORT     TYPE MSEG-LGORT,
*          LGOBE1    TYPE T001L-LGOBE,
*          SOBKZ     TYPE MSEG-SOBKZ,
*          MAT_KDAUF TYPE MSEG-MAT_KDAUF,
*          MAT_KDPOS TYPE MSEG-MAT_KDPOS,
*          LIFNR1    TYPE MSEG-LIFNR,
*          NAME1     TYPE LFA1-NAME1,
*          ZYWLX1    TYPE ZZLYW-ZYWLX,
*          UMLGO     TYPE MSEG-UMLGO,
*          LGOBE2    TYPE T001L-LGOBE,
*          UMSOK     TYPE MSEG-UMSOK,
*          KDAUF     TYPE MSEG-KDAUF,
*          KDPOS     TYPE MSEG-KDPOS,
*          LIFNR2    TYPE MSEG-LIFNR,
*          NAME2     TYPE LFA1-NAME1,
*          ZYWLX2    TYPE ZZLYW-ZYWLX,
*          MENGE     TYPE MSEG-MENGE,
*          QCKC      TYPE MARD-LABST,
*          MEINS     TYPE MSEG-MEINS,
*          NAFAZ     TYPE ANLP-NAFAZ,
*          ZYTS      TYPE VTBBEWE-ATAGE,
*          DYTS      TYPE VTBBEWE-ATAGE,
*          FTBL      TYPE MSEG-MENGE,
*          FTZJCB    TYPE ANLP-NAFAZ,
*        END OF LS_ALV2 .
*  DATA LT_ALV2 LIKE TABLE OF LS_ALV2 .
*  DATA LS_ALV2_1 LIKE LS_ALV2 .
*  DATA  L_ZJCB  TYPE ANLP-NAFAZ .

*  MOVE-CORRESPONDING GT_ALV2 TO LT_ALV2 .
*  SORT LT_ALV2 BY ANLN1 ASCENDING .
*  LOOP AT LT_ALV2 INTO LS_ALV2 .
*    MOVE-CORRESPONDING LS_ALV2 TO LS_ALV2_1 .
*    L_ZJCB = L_ZJCB + LS_ALV2_1-FTZJCB .
*    AT END OF ANLN1 .
*      LS_ALV2_1-FTZJCB = LS_ALV2_1-NAFAZ - L_ZJCB .
*      MOVE-CORRESPONDING LS_ALV2_1 TO GS_ALV2 .
*      MODIFY GT_ALV2 FROM GS_ALV2 INDEX GS_ALV2-NUM .
*      CLEAR L_ZJCB .
*      CLEAR GS_ALV2 .
*    ENDAT .
*    CLEAR LS_ALV2_1 .
*    CLEAR LS_ALV2 .
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_STORAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_STORAGE  .

  DATA L_LFMON TYPE LFMON .   " 期间

  L_LFMON = P_LFMON - 1 .
  " 当前主库存( 包括未发生货物移动的(GT_MARD_NOW)和发生了货物移动的(GT_MARDH_NOW) ) 这里是为了计算占用数量
*  SELECT MATNR
*         WERKS
*         LGORT
*         LABST
*         UMLME
*         INSME
*         EINME
*         SPEME
*         RETME
*    INTO CORRESPONDING FIELDS OF TABLE GT_MARD_NOW
*    FROM MARD
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .
*  SELECT MATNR
*         WERKS
*         LGORT
*         LABST
*         UMLME
*         INSME
*         EINME
*         SPEME
*         RETME
*    INTO CORRESPONDING FIELDS OF TABLE GT_MARDH_NOW
*    FROM MARDH
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .

* 只取347规格型号的物料
  DATA: BEGIN OF LS_MVKE ,
          MATNR TYPE MVKE-MATNR,
          VKORG TYPE MVKE-VKORG,
          VTWEG TYPE MVKE-VTWEG,
        END OF LS_MVKE .
  DATA LT_MVKE LIKE TABLE OF LS_MVKE .
  SELECT MATNR
         VKORG
         VTWEG
    INTO CORRESPONDING FIELDS OF TABLE LT_MVKE
    FROM MVKE
   WHERE MVGR1 EQ '347' .
  SORT LT_MVKE BY MATNR .

  " 期初库存
  SELECT MATNR
         WERKS
         LGORT
         LFGJA
         LFMON
         LABST
         UMLME
         INSME
         EINME
         SPEME
         RETME
    INTO CORRESPONDING FIELDS OF TABLE GT_MARDH_BEFORE
    FROM MARDH
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND LFGJA = P_LFGJA AND LFMON GE L_LFMON .
  SORT GT_MARDH_BEFORE BY MATNR WERKS LGORT LFGJA ASCENDING LFMON ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_MARDH_BEFORE COMPARING MATNR WERKS LGORT .
  SELECT MATNR
         WERKS
         LGORT
         LFGJA
         LFMON
         LABST
         UMLME
         INSME
         EINME
         SPEME
         RETME
    INTO CORRESPONDING FIELDS OF TABLE GT_MARD_BEFORE
    FROM MARD
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND (
         ( LFGJA = P_LFGJA AND LFMON LE L_LFMON ) OR
         ( LFGJA < P_LFGJA )
         ) .
  SORT GT_MARD_BEFORE BY MATNR WERKS LGORT LFGJA DESCENDING LFMON DESCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_MARD_BEFORE COMPARING MATNR WERKS LGORT .

  " 排除已经存在于MARDH中的记录
  LOOP AT GT_MARD_BEFORE INTO GS_MARD_BEFORE .
    READ TABLE GT_MARDH_BEFORE WITH KEY MATNR = GS_MARD_BEFORE-MATNR
                                        WERKS = GS_MARD_BEFORE-WERKS
                                        LGORT = GS_MARD_BEFORE-LGORT BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      DELETE TABLE GT_MARD_BEFORE FROM GS_MARD_BEFORE .
    ENDIF.
    CLEAR GS_MARD_BEFORE .
  ENDLOOP.
  " 排除347规格型号以外的物料
  LOOP AT GT_MARD_BEFORE INTO GS_MARD_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MARD_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MARD_BEFORE FROM GS_MARD_BEFORE .
    ENDIF.
    CLEAR GS_MARD_BEFORE .
  ENDLOOP.
  LOOP AT GT_MARDH_BEFORE INTO GS_MARDH_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MARDH_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MARDH_BEFORE FROM GS_MARDH_BEFORE .
    ENDIF.
    CLEAR GS_MARDH_BEFORE .
  ENDLOOP.

  " 当前特殊库存E ( 包括未发生货物移动的(GT_MSKA_NOW)和发生了货物移动的(GT_MSKAH_NOW) ) 这里是为了计算占用数量
*  SELECT MATNR
*         WERKS
*         LGORT
*         SOBKZ
*         VBELN
*         POSNR
*         KALAB
*         KAINS
*         KASPE
*         KAEIN
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSKA_NOW
*    FROM MSKA
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .
*  SELECT MATNR
*         WERKS
*         LGORT
*         SOBKZ
*         VBELN
*         POSNR
*         KALAB
*         KAINS
*         KASPE
*         KAEIN
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSKAH_NOW
*    FROM MSKAH
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .

  " 期初库存
  SELECT MATNR
         WERKS
         LGORT
         VBELN
         POSNR
         SOBKZ
         LFGJA
         LFMON
         KALAB
         KAINS
         KASPE
         KAEIN
    INTO CORRESPONDING FIELDS OF TABLE GT_MSKAH_BEFORE
    FROM MSKAH
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND LFGJA = P_LFGJA AND LFMON GE L_LFMON .
  SORT GT_MSKAH_BEFORE BY MATNR WERKS LGORT VBELN POSNR LFGJA ASCENDING LFMON ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_MSKAH_BEFORE COMPARING MATNR WERKS LGORT VBELN POSNR LFGJA .
  SELECT MATNR
         WERKS
         LGORT
         VBELN
         POSNR
         SOBKZ
         LFGJA
         LFMON
         KALAB
         KAINS
         KASPE
         KAEIN
    INTO CORRESPONDING FIELDS OF TABLE GT_MSKA_BEFORE
    FROM MSKA
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND (
         ( LFGJA = P_LFGJA AND LFMON LE L_LFMON ) OR
         ( LFGJA < P_LFGJA )
          ) .
  SORT GT_MSKA_BEFORE BY MATNR WERKS LGORT VBELN POSNR LFGJA DESCENDING LFMON DESCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_MSKA_BEFORE COMPARING MATNR WERKS LGORT VBELN POSNR.

  " 排除已经存在于MSKAH中的记录
  LOOP AT GT_MSKA_BEFORE INTO GS_MSKA_BEFORE .
    READ TABLE GT_MSKAH_BEFORE WITH KEY MATNR = GS_MSKA_BEFORE-MATNR
                                        WERKS = GS_MSKA_BEFORE-WERKS
                                        LGORT = GS_MSKA_BEFORE-LGORT
                                        VBELN = GS_MSKA_BEFORE-VBELN
                                        POSNR = GS_MSKA_BEFORE-POSNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      DELETE TABLE GT_MSKA_BEFORE FROM GS_MSKA_BEFORE .
    ENDIF.
    CLEAR GS_MSKA_BEFORE .
  ENDLOOP.
  " 排除347规格型号以外的物料
  LOOP AT GT_MSKA_BEFORE INTO GS_MSKA_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MSKA_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MSKA_BEFORE FROM GS_MSKA_BEFORE .
    ENDIF.
    CLEAR GS_MSKA_BEFORE .
  ENDLOOP.
  LOOP AT GT_MSKAH_BEFORE INTO GS_MSKAH_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MSKAH_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MSKAH_BEFORE FROM GS_MSKAH_BEFORE .
    ENDIF.
    CLEAR GS_MSKAH_BEFORE .
  ENDLOOP.

  " 当前特殊库存O ( 包括未发生货物移动的(GT_MSLB_NOW)和发生了货物移动的(GT_MSLB_NOW) ) 这里是为了计算占用数量
*  SELECT MATNR
*         WERKS
*         SOBKZ
*         LIFNR
*         LBLAB
*         LBINS
*         LBUML
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSLB_NOW
*    FROM MSLB
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .
*  SELECT MATNR
*         WERKS
*         SOBKZ
*         LIFNR
*         LBLAB
*         LBINS
*         LBUML
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSLBH_NOW
*    FROM MSLBH
*     FOR ALL ENTRIES IN GT_T001W
*   WHERE WERKS = GT_T001W-WERKS
*     AND LFGJA = P_LFGJA
*     AND LFMON = P_LFMON .

  " 期初库存
  SELECT MATNR
         WERKS
         LIFNR
         SOBKZ
         LFGJA
         LFMON
         LBLAB
         LBINS
         LBUML
    INTO CORRESPONDING FIELDS OF TABLE GT_MSLBH_BEFORE
    FROM MSLBH
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND LFGJA = P_LFGJA AND LFMON GE L_LFMON .
  SORT GT_MSLBH_BEFORE BY MATNR WERKS LIFNR LFGJA ASCENDING LFMON ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_MSLBH_BEFORE COMPARING MATNR WERKS LIFNR .
  SELECT MATNR
         WERKS
         LIFNR
         SOBKZ
         LFGJA
         LFMON
         LBLAB
         LBINS
         LBUML
    INTO CORRESPONDING FIELDS OF TABLE GT_MSLB_BEFORE
    FROM MSLB
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS
     AND (
         ( LFGJA = P_LFGJA AND LFMON LE L_LFMON ) OR
         ( LFGJA < P_LFGJA )
          ) .
  SORT GT_MSLB_BEFORE BY MATNR WERKS LIFNR LFGJA DESCENDING LFMON DESCENDING .

  " 排除已经存在于MSLBH中的记录
  LOOP AT GT_MSLB_BEFORE INTO GS_MSLB_BEFORE .
    READ TABLE GT_MSLBH_BEFORE WITH KEY MATNR = GS_MSLB_BEFORE-MATNR
                                        WERKS = GS_MSLB_BEFORE-WERKS
                                        LIFNR = GS_MSLB_BEFORE-LIFNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      DELETE TABLE GT_MSLB_BEFORE FROM GS_MSLB_BEFORE .
    ENDIF.
    CLEAR GS_MSLB_BEFORE .
  ENDLOOP.
  " 排除347规格型号以外的物料
  LOOP AT GT_MSLB_BEFORE INTO GS_MSLB_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MSLB_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MSLB_BEFORE FROM GS_MSLB_BEFORE .
    ENDIF.
    CLEAR GS_MSLB_BEFORE .
  ENDLOOP.
  LOOP AT GT_MSLBH_BEFORE INTO GS_MSLBH_BEFORE .
    READ TABLE LT_MVKE WITH KEY MATNR = GS_MSLBH_BEFORE-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC NE 0 .
      DELETE TABLE GT_MSLBH_BEFORE FROM GS_MSLBH_BEFORE .
    ENDIF.
    CLEAR GS_MSLBH_BEFORE .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_WERKS .
  " 公司代码
  SELECT BWKEY
         BUKRS
    INTO CORRESPONDING FIELDS OF TABLE GT_T001K
    FROM T001K
   WHERE BUKRS = P_BUKRS .
  IF GT_T001K IS NOT INITIAL .
    " 工厂
    SELECT WERKS
           BWKEY
      INTO CORRESPONDING FIELDS OF TABLE GT_T001W
      FROM T001W
       FOR ALL ENTRIES IN GT_T001K
     WHERE BWKEY = GT_T001K-BWKEY
       AND WERKS = P_WERKS .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ZZLYW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_ZZLYW .
  SELECT *
    INTO TABLE GT_ZZLYW
    FROM ZZLYW
     FOR ALL ENTRIES IN GT_T001W
   WHERE WERKS = GT_T001W-WERKS .
  SORT GT_ZZLYW BY WERKS LGORT SOBKZ .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV_BEFORE_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV_BEFORE_POST .
  PERFORM FRM_CHECK_VAR_ALV .  "选择界面参数校验
  PERFORM FRM_CLEAR_ALV_VAR . " 清空ALV 参数
  PERFORM FRM_GET_BASE .  "获取基础数据(工厂、库存、租赁自建表)
  PERFORM FRM_GET_DEAL_DATA_ALV1 .  "期初状态报表数据读取和整理
  PERFORM FRM_GET_DEAL_DATA_ALV2 .  "日常转储报表数据读取和整理
  PERFORM FRM_SAVE_DATA . " 将ALV 的值保存起来，后面过账用
  PERFORM FRM_SHOW_POST_DATA .  " 展示将要过账的数据
  PERFORM FRM_SHOW_ALV .  " ALV 展示
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_BASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_BASE .

  " 获取工厂
  PERFORM FRM_GET_WERKS .

  IF GT_T001W IS NOT INITIAL .
    " 获取库存
    PERFORM FRM_GET_STORAGE  .
    " 获取租赁业务类型自建表
    PERFORM FRM_GET_ZZLYW .
  ELSE .
    MESSAGE '该公司代码下没有该工厂' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_POST_DATA .

  DATA L_NUM TYPE I VALUE IS INITIAL .
  DATA LS_THEAD TYPE THEAD .
  DATA LT_TLINE TYPE TABLE OF TLINE WITH HEADER LINE .

  " 汇总
  DATA: BEGIN OF LS_CLT ,
          BUKRS    TYPE ZZJFT-BUKRS,
          WERKS    TYPE ZZJFT-WERKS,
          MATNR    TYPE ZZJFT-MATNR,
          VBELN    TYPE ZZJFT-VBELN,
          POSNR(6) TYPE C,
          FTZJCB   TYPE ZZJFT-NAFAP_DIV,
          FLAG(1)  TYPE C , " 标记金额是从哪来的，1：期初  2：转储（从） 3：转储（到）
        END OF LS_CLT .
  DATA LT_CLT LIKE TABLE OF LS_CLT .

  LOOP AT GT_ALV_POST1 INTO GS_ALV_POST1 WHERE FTZJCB IS NOT INITIAL
                                           AND FTZJCB NE 0 .
    LS_CLT-BUKRS = P_BUKRS .
    LS_CLT-WERKS = P_WERKS .
    LS_CLT-MATNR = GS_ALV_POST1-MATNR .
    LS_CLT-VBELN = GS_ALV_POST1-VBELN .
    LS_CLT-POSNR = GS_ALV_POST1-POSNR .
    LS_CLT-FTZJCB = GS_ALV_POST1-FTZJCB .
    LS_CLT-FLAG = '1' .
    COLLECT LS_CLT INTO LT_CLT .
    CLEAR LS_CLT .
    CLEAR GS_ALV_POST1 .
  ENDLOOP.
  LOOP AT GT_ALV_POST2 INTO GS_ALV_POST2 WHERE FTZJCB IS NOT INITIAL
                                           AND FTZJCB NE 0 .
    LS_CLT-BUKRS = P_BUKRS .
    LS_CLT-WERKS = P_WERKS .
    LS_CLT-MATNR = GS_ALV_POST2-MATNR .
*    IF GS_ALV_POST2-MAT_KDAUF IS NOT INITIAL .
    LS_CLT-VBELN = GS_ALV_POST2-MAT_KDAUF .
    LS_CLT-POSNR = GS_ALV_POST2-MAT_KDPOS .
    LS_CLT-FTZJCB = - GS_ALV_POST2-FTZJCB .
    LS_CLT-FLAG = '2' .
    COLLECT LS_CLT INTO LT_CLT .
    CLEAR: LS_CLT-VBELN ,
           LS_CLT-POSNR ,
           LS_CLT-FTZJCB .
*    ELSEIF GS_ALV_POST2-KDAUF IS NOT INITIAL .
    LS_CLT-VBELN = GS_ALV_POST2-KDAUF .
    LS_CLT-POSNR = GS_ALV_POST2-KDPOS .
    LS_CLT-FTZJCB = GS_ALV_POST2-FTZJCB .
    LS_CLT-FLAG = '3'.
*    ENDIF.
    COLLECT LS_CLT INTO LT_CLT .
    CLEAR LS_CLT .
    CLEAR GS_ALV_POST2 .
  ENDLOOP.
  SORT LT_CLT BY VBELN MATNR .

  LOOP AT LT_CLT INTO LS_CLT .
    L_NUM = L_NUM + 1 .
    GS_ALV5-NUM = L_NUM .
    GS_ALV5-BUKRS = P_BUKRS .
    GS_ALV5-WERKS = P_WERKS .
    GS_ALV5-GJAHR = P_LFGJA .
    GS_ALV5-MONAT = P_LFMON .
    GS_ALV5-MATNR = LS_CLT-MATNR .
    GS_ALV5-VBELN = LS_CLT-VBELN .
    GS_ALV5-POSNR = LS_CLT-POSNR .
    GS_ALV5-NAFAP_DIV = LS_CLT-FTZJCB .
    GS_ALV5-FLAG = LS_CLT-FLAG .
    IF GS_ALV5-FLAG = '1'.
      GS_ALV5-TEXT = '期初'.
    ELSEIF GS_ALV5-FLAG = '2'.
      GS_ALV5-TEXT = '转储从方'.
    ELSEIF GS_ALV5-FLAG = '3'.
      GS_ALV5-TEXT = '转储到方'.
    ENDIF.
    APPEND GS_ALV5 TO GT_ALV5 .
    CLEAR GS_ALV5 .
    CLEAR LS_CLT .
  ENDLOOP.

  SORT GT_ALV5 BY WERKS MATNR VBELN .
  CLEAR L_NUM .

* 检查折旧分摊表是否已经有数据(是否已生成会计凭证)
  DATA: LT_ZZJFT TYPE TABLE OF ZZJFT,
        LS_ZZJFT TYPE ZZJFT.
  IF GT_ALV5[] IS NOT INITIAL .
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_ZZJFT
      FROM ZZJFT
       FOR ALL ENTRIES IN GT_ALV5
     WHERE BUKRS = GT_ALV5-BUKRS
       AND WERKS = GT_ALV5-WERKS
       AND MATNR = GT_ALV5-MATNR
       AND VBELN = GT_ALV5-VBELN
       AND GJAHR = P_LFGJA
       AND MONAT = P_LFMON .
    SORT LT_ZZJFT BY BUKRS WERKS MATNR VBELN .
  ENDIF.

* 根据工厂、物料、订单号合并ALV数据，前台只显示一行
  LOOP AT GT_ALV5 INTO GS_ALV5 .
    GS_ALV5_1-BUKRS = GS_ALV5-BUKRS .
    GS_ALV5_1-WERKS = GS_ALV5-WERKS .
    GS_ALV5_1-GJAHR = GS_ALV5-GJAHR .
    GS_ALV5_1-MONAT = GS_ALV5-MONAT .
    GS_ALV5_1-MATNR = GS_ALV5-MATNR .
    GS_ALV5_1-VBELN = GS_ALV5-VBELN .
    GS_ALV5_1-NAFAZ = GS_ALV5-NAFAP_DIV .
    COLLECT GS_ALV5_1 INTO GT_ALV5_1 .
    CLEAR GS_ALV5_1 .
    READ TABLE LT_ZZJFT INTO LS_ZZJFT WITH KEY BUKRS = GS_ALV5-BUKRS
                                               WERKS = GS_ALV5-WERKS
                                               MATNR = GS_ALV5-MATNR
                                               VBELN = GS_ALV5-VBELN BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV5-BELNR = LS_ZZJFT-BELNR .
      GS_ALV5-BELNR_REV = LS_ZZJFT-BELNR_REV .
      CLEAR LS_ZZJFT .
    ENDIF.
    MODIFY GT_ALV5 FROM GS_ALV5 .
    CLEAR GS_ALV5.
  ENDLOOP.
  LOOP AT GT_ALV5_1 INTO GS_ALV5_1 .
    L_NUM = L_NUM + 1 .
    GS_ALV5_1-NUM = L_NUM .
    READ TABLE LT_ZZJFT INTO LS_ZZJFT WITH KEY BUKRS = GS_ALV5_1-BUKRS
                                               WERKS = GS_ALV5_1-WERKS
                                               MATNR = GS_ALV5_1-MATNR
                                               VBELN = GS_ALV5_1-VBELN BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV5_1-BELNR = LS_ZZJFT-BELNR .
      GS_ALV5_1-BELNR_REV = LS_ZZJFT-BELNR_REV .
      CLEAR LS_ZZJFT .
    ENDIF.

    LS_THEAD-TDID = 'Z001'.
    LS_THEAD-TDSPRAS = SY-LANGU .
    LS_THEAD-TDNAME = GS_ALV5_1-VBELN .
    LS_THEAD-TDOBJECT = 'VBBK'.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = LS_THEAD-TDID
        LANGUAGE                = LS_THEAD-TDSPRAS
        NAME                    = LS_THEAD-TDNAME
        OBJECT                  = LS_THEAD-TDOBJECT
      TABLES
        LINES                   = LT_TLINE[]
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    LOOP AT LT_TLINE.
      CONCATENATE GS_ALV5_1-POST1 LT_TLINE-TDLINE ';' INTO GS_ALV5_1-POST1 .
    ENDLOOP.

    MODIFY GT_ALV5_1 FROM GS_ALV5_1 .
    CLEAR GS_ALV5_1 .

    REFRESH LT_TLINE[] .
    CLEAR LT_TLINE .
    CLEAR LS_THEAD .
  ENDLOOP.

* 删除分摊折旧成本=0的行
  DELETE GT_ALV5 WHERE NAFAP_DIV = 0.
  DELETE GT_ALV5_1 WHERE NAFAZ = 0.
  SORT GT_ALV5_1 BY WERKS  MATNR  VBELN  .

  REFRESH LT_ZZJFT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_SCREEN .
  IF P3 = 'X' .
    CALL SCREEN 9003 .
  ELSEIF P4 = 'X'.
    CALL SCREEN 9004 .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_DETAIL .
  PERFORM FRM_CHECK_VAR_ALV2 .  " 校验必输字段
  PERFORM FRM_CLEAR_ALV_VAR2 . " 清空ALV 参数
  PERFORM FRM_GET_AND_DEAL_DATA . " 取数并处理后放进ALV
  PERFORM FRM_SHOW_ALV .  " ALV 展示
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_COLLECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_COLLECT .
  PERFORM FRM_CHECK_VAR_ALV3 .  " 校验必输字段
  PERFORM FRM_CLEAR_ALV_VAR2 . " 清空ALV 参数
  PERFORM FRM_GET_AND_DEAL_DATA2 . " 取数并处理后放进ALV
  PERFORM FRM_SHOW_ALV .  " ALV 展示
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_VAR_ALV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_VAR_ALV2 .
  IF P_BUKRS2 IS INITIAL .
    MESSAGE '公司代码必填' TYPE 'E' .
  ENDIF.
  IF P_WERKS2 IS INITIAL .
    MESSAGE '工厂必填' TYPE 'E' .
  ENDIF.
  IF P_GJAHR IS INITIAL .
    MESSAGE '会计年度必填' TYPE 'E' .
  ENDIF.
  IF P_MONAT IS INITIAL .
    MESSAGE '会计期间必填' TYPE 'E' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_DEAL_DATA .

  DATA: LT_ZZJFT TYPE TABLE OF ZZJFT,
        LS_ZZJFT TYPE ZZJFT.
  DATA: BEGIN OF LS_LFA1 ,
          LIFNR TYPE LFA1-LIFNR,
          NAME1 TYPE LFA1-NAME1,
        END OF LS_LFA1 .
  DATA LT_LFA1 LIKE TABLE OF LS_LFA1 .

  DATA L_NUM TYPE I VALUE IS INITIAL .
  DATA LS_THEAD TYPE THEAD .
  DATA: LT_TLINE TYPE TABLE OF TLINE WITH HEADER LINE .

  SELECT *
    INTO TABLE LT_ZZJFT
    FROM ZZJFT
   WHERE BUKRS = P_BUKRS2
     AND WERKS = P_WERKS2
     AND GJAHR = P_GJAHR
     AND MONAT = P_MONAT
     AND MATNR IN S_MATNR
     AND LGORT IN S_LGORT
     AND VBELN IN S_VBELN
     AND LIFNR IN S_LIFNR .
  IF LT_ZZJFT IS NOT INITIAL .

    SELECT LIFNR
           NAME1
      INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
      FROM LFA1
       FOR ALL ENTRIES IN LT_ZZJFT
     WHERE LIFNR = LT_ZZJFT-LIFNR .
    SORT LT_LFA1 BY LIFNR .

    LOOP AT LT_ZZJFT INTO LS_ZZJFT .
      L_NUM = L_NUM + 1 .
      GS_ALV3-NUM = L_NUM .
      GS_ALV3-BUKRS = P_BUKRS2 .
      GS_ALV3-WERKS = P_WERKS2 .
      GS_ALV3-GJAHR = P_GJAHR .
      GS_ALV3-MONAT = P_MONAT .
      GS_ALV3-BUDAT = LS_ZZJFT-BUDAT .
      GS_ALV3-ZYWLX = LS_ZZJFT-ZYWLX .
      GS_ALV3-MATNR = LS_ZZJFT-MATNR .
      GS_ALV3-LGORT = LS_ZZJFT-LGORT .
      GS_ALV3-VBELN = LS_ZZJFT-VBELN .
      GS_ALV3-POSNR = LS_ZZJFT-POSNR .

      LS_THEAD-TDID = 'Z001'.
      LS_THEAD-TDSPRAS = SY-LANGU .
      LS_THEAD-TDNAME = LS_ZZJFT-VBELN .
      LS_THEAD-TDOBJECT = 'VBBK'.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = LS_THEAD-TDID
          LANGUAGE                = LS_THEAD-TDSPRAS
          NAME                    = LS_THEAD-TDNAME
          OBJECT                  = LS_THEAD-TDOBJECT
        TABLES
          LINES                   = LT_TLINE[]
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      LOOP AT LT_TLINE.
        CONCATENATE GS_ALV3-POST1 LT_TLINE-TDLINE ';' INTO GS_ALV3-POST1 .
      ENDLOOP.

      GS_ALV3-LIFNR = LS_ZZJFT-LIFNR .

      READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = LS_ZZJFT-LIFNR BINARY SEARCH .
      IF SY-SUBRC = 0 .
        GS_ALV3-LIFNR = LS_LFA1-LIFNR .
      ENDIF.

      GS_ALV3-MENGE = LS_ZZJFT-MENGE .
      GS_ALV3-MENGE_SUM = LS_ZZJFT-MENGE_SUM .
      GS_ALV3-MEINS = LS_ZZJFT-MEINS .
      GS_ALV3-NAFAZ = LS_ZZJFT-NAFAZ .
      GS_ALV3-ZYTS = LS_ZZJFT-ZYTS .
      GS_ALV3-DYTS = LS_ZZJFT-DYTS .
      GS_ALV3-ZFTBL = LS_ZZJFT-ZFTBL .
      GS_ALV3-NAFAP_DIV = LS_ZZJFT-NAFAP_DIV .

      APPEND GS_ALV3 TO GT_ALV3 .
      CLEAR GS_ALV3 .

      REFRESH LT_TLINE[] .
      CLEAR LT_TLINE .
      CLEAR LS_LFA1 .
      CLEAR LS_THEAD .
      CLEAR LS_ZZJFT .
    ENDLOOP.
  ELSE .
    MESSAGE '没数据' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CLEAR_ALV_VAR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CLEAR_ALV_VAR2 .
  REFRESH: GT_LVC,GT_ALV3,GT_ALV4.
  CLEAR: GS_LAYOUT .
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  FRM_SHOW_ALV2
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM FRM_SHOW_ALV2 .
*  PERFORM INIT_LAYOUT.             "设置输出格式
**  PERFORM INIT_SORT.               "设置排序、合计
**  PERFORM INIT_VARIANT.            "设置变式控制
*  PERFORM FRM_INIT_LVC.            "字段列定义
**  PERFORM FRM_EXCLUDE.
**  PERFORM FRM_BUILD_EVENT.
**  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
*  PERFORM FRM_OUTPUT  .
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_VAR_ALV3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_VAR_ALV3 .
  IF P_BUKRS3 IS INITIAL .
    MESSAGE '公司代码必填' TYPE 'E' .
  ENDIF.
  IF P_WERKS3 IS INITIAL .
    MESSAGE '工厂必填' TYPE 'E' .
  ENDIF.
  IF P_GJAHR2 IS INITIAL .
    MESSAGE '会计年度必填' TYPE 'E' .
  ENDIF.
  IF P_MONAT2 IS INITIAL .
    MESSAGE '会计期间必填' TYPE 'E' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_DEAL_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_DEAL_DATA2 .

  " 租赁业务折旧分摊数据 自建表
  DATA: LT_ZZJFT TYPE TABLE OF ZZJFT,
        LS_ZZJFT TYPE ZZJFT.
  " 汇总自建表
  DATA: BEGIN OF LS_ZZJFT_COLLECT ,
          BUKRS     TYPE ZZJFT-BUKRS,
          WERKS     TYPE ZZJFT-WERKS,
          MATNR     TYPE ZZJFT-MATNR,
          VBELN     TYPE ZZJFT-VBELN,
          POSNR(6)  TYPE C,
          NAFAP_DIV TYPE ZZJFT-NAFAP_DIV,
        END OF LS_ZZJFT_COLLECT .
  DATA LT_ZZJFT_COLLECT LIKE TABLE OF LS_ZZJFT_COLLECT .

  DATA LS_THEAD TYPE THEAD .
  DATA: LT_TLINE TYPE TABLE OF TLINE WITH HEADER LINE .

  DATA L_NUM TYPE I VALUE IS INITIAL .

  " 取自建表数据
  SELECT *
    INTO TABLE LT_ZZJFT
    FROM ZZJFT
   WHERE BUKRS = P_BUKRS3
     AND WERKS = P_WERKS3
     AND GJAHR = P_GJAHR2
     AND MONAT = P_MONAT2
     AND MATNR IN S_MATNR2
     AND VBELN IN S_VBELN2 .

  IF LT_ZZJFT IS NOT INITIAL .

    " 汇总分摊折旧成本
    LOOP AT LT_ZZJFT INTO LS_ZZJFT .
      MOVE-CORRESPONDING LS_ZZJFT TO LS_ZZJFT_COLLECT .
      LS_ZZJFT_COLLECT-POSNR = LS_ZZJFT-POSNR .
      COLLECT LS_ZZJFT_COLLECT INTO LT_ZZJFT_COLLECT .
      CLEAR LS_ZZJFT .
      CLEAR LS_ZZJFT_COLLECT .
    ENDLOOP .

    " 将数据放进ALV
    LOOP AT LT_ZZJFT_COLLECT INTO LS_ZZJFT_COLLECT .
      L_NUM = L_NUM + 1 .
      GS_ALV4-NUM = L_NUM .
      GS_ALV4-BUKRS = P_BUKRS3 .
      GS_ALV4-WERKS = P_WERKS3 .
      GS_ALV4-MATNR = LS_ZZJFT_COLLECT-MATNR .
      GS_ALV4-VBELN = LS_ZZJFT_COLLECT-VBELN .
      GS_ALV4-POSNR = LS_ZZJFT_COLLECT-POSNR .
      GS_ALV4-NAFAP_DIV = LS_ZZJFT_COLLECT-NAFAP_DIV .

      LS_THEAD-TDID = 'Z001'.
      LS_THEAD-TDSPRAS = SY-LANGU .
      LS_THEAD-TDNAME = LS_ZZJFT_COLLECT-VBELN .
      LS_THEAD-TDOBJECT = 'VBBK'.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = LS_THEAD-TDID
          LANGUAGE                = LS_THEAD-TDSPRAS
          NAME                    = LS_THEAD-TDNAME
          OBJECT                  = LS_THEAD-TDOBJECT
        TABLES
          LINES                   = LT_TLINE[]
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      LOOP AT LT_TLINE.
        CONCATENATE GS_ALV4-POST1 LT_TLINE-TDLINE ';' INTO GS_ALV4-POST1 .
      ENDLOOP.

      APPEND GS_ALV4 TO GT_ALV4 .
      CLEAR GS_ALV4 .

      REFRESH LT_TLINE[] .
      CLEAR LT_TLINE .
      CLEAR LS_THEAD .
      CLEAR LS_ZZJFT_COLLECT .
    ENDLOOP.
  ELSE .
    MESSAGE '没数据' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  SET PF-STATUS 'POST_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA:L_INDEX TYPE SY-TABIX.

  CASE R_UCOMM.
    WHEN 'BACK' OR 'CANCEL' .
      LEAVE TO SCREEN 0  .
*    WHEN '&IC1'."双击
*      read table it_data into wa_data index rs_selfield-tabindex.
*      if sy-subrc = 0.
*        set parameter id 'XXXX' field wa_data-matnr.   "选择屏字段ID
*        call transaction 'XXXX' and skip first screen."填T-code
*      endif.
*    when 'PRINT'."打印
*      perform frm_print_select.
*    when '&ZALL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = 'X'.
*        modify it_data from wa_data index l_index.
*      endloop.
*    when '&ZSAL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = ''.
*        modify it_data from wa_data index l_index.
*      endloop.
    WHEN 'POST' .
      PERFORM FRM_DOCUMENT_POST .
    WHEN 'REVERSAL'.
      PERFORM FRM_ACC_REVERSAL.
    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_DOCUMENT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DOCUMENT_POST .
  DATA L_SUBRC TYPE SY-SUBRC.
  DATA L_NUM TYPE I VALUE IS INITIAL .

  PERFORM FRM_FILLING_DATA .  " 将数据装进凭证抬头和行

* 检查折旧分摊表是否已经有数据(是否已生成会计凭证)
  DATA: LT_ZZJFT TYPE TABLE OF ZZJFT,
        LS_ZZJFT TYPE ZZJFT.
  IF GT_HEAD[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_ZZJFT
      FROM ZZJFT
       FOR ALL ENTRIES IN GT_HEAD
     WHERE BUKRS = GT_HEAD-BUKRS
       AND WERKS = GT_HEAD-WERKS
       AND MATNR = GT_HEAD-MATNR
       AND VBELN = GT_HEAD-VBELN
       AND GJAHR = P_LFGJA
       AND MONAT = P_LFMON .
    IF LT_ZZJFT IS NOT INITIAL .
      SORT LT_ZZJFT BY BUKRS WERKS GJAHR MONAT ZXH DESCENDING .
      READ TABLE LT_ZZJFT INTO LS_ZZJFT INDEX 1 .
      L_NUM = LS_ZZJFT-ZXH .
    ELSE .
      L_NUM = 1 .
    ENDIF.
    SORT LT_ZZJFT BY BUKRS WERKS MATNR VBELN .
  ENDIF.


  LOOP AT GT_HEAD INTO GS_HEAD .
    " 检查改条数据是否已过账，如已过账则将凭证编号附上
    READ TABLE LT_ZZJFT INTO LS_ZZJFT WITH KEY BUKRS = GS_HEAD-BUKRS
                                               WERKS = GS_HEAD-WERKS
                                               MATNR = GS_HEAD-MATNR
                                               VBELN = GS_HEAD-VBELN BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_HEAD-BELNR = LS_ZZJFT-BELNR .
      CLEAR LS_ZZJFT .
    ENDIF.
    PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

    CHECK L_SUBRC EQ 0.

    PERFORM FRM_BAPI_DATA_PREP .
    PERFORM FRM_CALL_BAPI CHANGING GS_HEAD-BELNR GS_HEAD-BUZEI GS_HEAD-MSG .

    IF GS_HEAD-BELNR IS NOT INITIAL.  " 创建成功
      PERFORM FRM_SAVE_ZZJFT USING L_NUM .
      READ TABLE GT_ALV5_1 WITH KEY WERKS = GS_HEAD-WERKS
                                    MATNR = GS_HEAD-MATNR
                                    VBELN = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_ALV5_1 INTO GS_ALV5_1 FROM SY-TABIX .
          IF GS_ALV5_1-WERKS = GS_HEAD-WERKS AND
              GS_ALV5_1-MATNR = GS_HEAD-MATNR AND
              GS_ALV5_1-VBELN = GS_HEAD-VBELN .
            GS_ALV5_1-TYPE = 'S' .
            GS_ALV5_1-MSG = '创建成功' .
            GS_ALV5_1-BELNR = GS_HEAD-BELNR .
            MODIFY GT_ALV5_1 FROM GS_ALV5_1 INDEX SY-TABIX  .
            CLEAR GS_ALV5_1 .
          ELSE .
            EXIT .
          ENDIF.
          CLEAR GS_ALV5_1 .
        ENDLOOP.
      ENDIF.

    ELSE .
      READ TABLE GT_ALV5_1 WITH KEY WERKS = GS_HEAD-WERKS
                                    MATNR = GS_HEAD-MATNR
                                    VBELN = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_ALV5_1 INTO GS_ALV5_1 FROM SY-TABIX .
          IF GS_ALV5_1-WERKS = GS_HEAD-WERKS AND
              GS_ALV5_1-MATNR = GS_HEAD-MATNR AND
              GS_ALV5_1-VBELN = GS_HEAD-VBELN .
            GS_ALV5_1-TYPE = 'E' .
            CONCATENATE '创建失败：' GS_HEAD-MSG INTO GS_ALV5_1-MSG .
            MODIFY GT_ALV5_1 FROM GS_ALV5_1 INDEX SY-TABIX  .
            CLEAR GS_ALV5_1 .
          ELSE .
            EXIT .
          ENDIF.
          CLEAR GS_ALV5_1 .
        ENDLOOP.
      ENDIF.
    ENDIF.
    CLEAR GS_HEAD .

  ENDLOOP.

  CLEAR L_NUM .
  REFRESH LT_ZZJFT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.
  " 凭证日期
  IF GS_HEAD-BLDAT IS INITIAL.
    MESSAGE TEXT-M02 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF GS_HEAD-BUDAT IS INITIAL.
    MESSAGE TEXT-M03 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

*  " 业务类型
*  IF GS_HEAD-ZYWLX IS INITIAL.
*    MESSAGE TEXT-M04 TYPE 'W' DISPLAY LIKE 'E'.
*    P_SUBRC = 8.
*    EXIT.
*  ENDIF.

  " 凭证类型
  IF GS_HEAD-BLART IS INITIAL.
    MESSAGE TEXT-M05 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF GS_HEAD-BUKRS IS INITIAL.
    MESSAGE TEXT-M06 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 货币
  IF GS_HEAD-WAERS IS INITIAL.
    MESSAGE TEXT-M07 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 销售订单
  IF GS_HEAD-MONAT IS INITIAL.
    MESSAGE TEXT-M08 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  "检查是否已经过账
  IF   GS_HEAD-BELNR  IS NOT INITIAL .
*  AND  GS_HEAD-GJAHR  IS NOT INITIAL.
    MESSAGE TEXT-M22 TYPE 'W'  DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 行项目
  IF GT_ITEM IS INITIAL.
    MESSAGE TEXT-M09 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  DATA: L_OPREIS TYPE CKIS-OPREIS.

  READ TABLE GT_ITEM WITH KEY WERKS = GS_HEAD-WERKS
                              MATNR = GS_HEAD-MATNR
                              VBEL2 = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC EQ 0 .

    LOOP AT GT_ITEM ASSIGNING <FS_ITEM> FROM SY-TABIX .

      IF <FS_ITEM>-WERKS = GS_HEAD-WERKS AND
          <FS_ITEM>-MATNR = GS_HEAD-MATNR AND
          <FS_ITEM>-VBEL2 = GS_HEAD-VBELN .

        " 记账码
        IF <FS_ITEM>-BSCHL IS INITIAL.
          MESSAGE TEXT-M11 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 科目
        IF <FS_ITEM>-HKONT IS INITIAL.
          MESSAGE TEXT-M12 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 物料号
        IF <FS_ITEM>-MATNR IS INITIAL.
          MESSAGE TEXT-M16 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 金额
        IF <FS_ITEM>-WRBTR IS INITIAL.
          MESSAGE TEXT-M17 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        IF GS_HEAD-NAFAZ LT 0 .

          IF <FS_ITEM>-BSCHL = '40'  .
            " 销售订单号
            IF <FS_ITEM>-VBEL2 IS INITIAL.
              MESSAGE TEXT-M14 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
            " 订单行号
            IF <FS_ITEM>-POSN2 IS INITIAL.
              MESSAGE TEXT-M15 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
          ELSEIF <FS_ITEM>-BSCHL = '50'  .
            " 成本中心
            IF <FS_ITEM>-KOSTL IS INITIAL.
              MESSAGE TEXT-M13 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
          ENDIF.

          IF <FS_ITEM>-BSCHL = '40'  .
            L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
          ELSEIF <FS_ITEM>-BSCHL = '50'  .
            IF L_OPREIS LT 0 .
              L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
            ELSEIF L_OPREIS GT 0 .
              L_OPREIS = L_OPREIS - <FS_ITEM>-WRBTR.
            ENDIF.
          ENDIF.

        ELSEIF GS_HEAD-NAFAZ GT 0 .

          IF <FS_ITEM>-BSCHL = '50'  .
            " 销售订单号
            IF <FS_ITEM>-VBEL2 IS INITIAL.
              MESSAGE TEXT-M14 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
            " 订单行号
            IF <FS_ITEM>-POSN2 IS INITIAL.
              MESSAGE TEXT-M15 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
          ELSEIF <FS_ITEM>-BSCHL = '40'  .
            " 成本中心
            IF <FS_ITEM>-KOSTL IS INITIAL.
              MESSAGE TEXT-M13 TYPE 'W' DISPLAY LIKE 'E'.
              P_SUBRC = 8.
              EXIT.
            ENDIF.
          ENDIF.

          IF <FS_ITEM>-BSCHL = '50'  .
            L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
          ELSEIF <FS_ITEM>-BSCHL = '40'  .
            IF L_OPREIS LT 0 .
              L_OPREIS = L_OPREIS - <FS_ITEM>-WRBTR.
            ELSEIF L_OPREIS GT 0 .
              L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
            ENDIF.
          ENDIF.

        ENDIF.

*        IF <FS_ITEM>-BSCHL = '40'  .
*          L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
*        ELSEIF <FS_ITEM>-BSCHL = '50'  .
*          IF L_OPREIS LT 0 .
*            L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
*          ELSEIF L_OPREIS GT 0 .
*            L_OPREIS = L_OPREIS - <FS_ITEM>-WRBTR.
*          ENDIF.
*        ENDIF.

      ELSE .
        EXIT .
      ENDIF.
    ENDLOOP.

  ENDIF.

  CLEAR L_OPREIS .

  IF P_SUBRC NE 0.
    EXIT.
  ENDIF.

  IF L_OPREIS NE 0.
    MESSAGE TEXT-M18 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FILLING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILLING_DATA .

  PERFORM FRM_CLEAR_HEADITEM .

  " 按订单号汇总分摊折旧成本
  DATA:BEGIN OF LS_HEAD ,
*         BUKRS     TYPE T001-BUKRS,
         WERKS     TYPE T001W-WERKS,
         MATNR     TYPE MARA-MATNR,
         VBELN     TYPE VBAP-VBELN,
         BELNR     TYPE BSEG-BELNR,
         NAFAP_DIV TYPE ZZJFT-NAFAP_DIV,
       END OF LS_HEAD .
  DATA LT_HEAD LIKE TABLE OF LS_HEAD .

  " DATE_GET_MONTH_LASTDAY 函数的参数
  DATA: L_I_DATE TYPE SY-DATUM,
        L_E_DATE TYPE SY-DATUM.

  DATA L_WAERS TYPE T001-WAERS .  " 货币
  DATA L_KOSTL TYPE KOSTL . " 成本中心
  DATA L_WRBTR TYPE BSEG-WRBTR .
  "凭证头转文本用
  DATA: L_LFGJA(4)  TYPE C,
        L_LFMON(2)  TYPE C,
        L_NAFAP(16) TYPE C.
  DATA L_NUM TYPE I VALUE IS INITIAL .

  SELECT SINGLE WAERS
    INTO L_WAERS
    FROM T001
   WHERE BUKRS = P_BUKRS .

  SELECT SINGLE KOSTL
    INTO L_KOSTL
    FROM ZZJCC
   WHERE BUKRS = P_BUKRS .

  " 按工厂、物料、订单号的维度汇总分摊折旧成本
  LOOP AT GT_ALV5 INTO GS_ALV5 .
    MOVE-CORRESPONDING GS_ALV5 TO LS_HEAD .
    COLLECT LS_HEAD INTO LT_HEAD .
    CLEAR GS_ALV5 .
    CLEAR LS_HEAD.
  ENDLOOP.

  SORT GT_ALV5 BY WERKS MATNR VBELN .
  SORT LT_HEAD BY WERKS MATNR VBELN .
  DELETE ADJACENT DUPLICATES FROM LT_HEAD COMPARING WERKS MATNR VBELN .

* 按工厂、物料、订单号、行号的维度汇总凭证行
  DATA: BEGIN OF LS_ITEM ,
          " 汇总字段
          WERKS    TYPE T001W-WERKS,
          MATNR    TYPE BSEG-MATNR,
          VBEL2    TYPE BSEG-VBEL2,
          POSN2(6) TYPE C,
          WRBTR    TYPE BSEG-WRBTR,
          " 凭证字段
          BUZEI    TYPE BSEG-BUZEI,
          BSCHL    TYPE BSEG-BSCHL,
          HKONT    TYPE BSEG-HKONT,
          KOSTL    TYPE BSEG-KOSTL,
          SGTXT    TYPE BSEG-SGTXT,
        END OF LS_ITEM .
  DATA LT_ITEM LIKE TABLE OF LS_ITEM .
  LOOP AT GT_ALV5 INTO GS_ALV5 .
    LS_ITEM-WERKS = GS_ALV5-WERKS .
    LS_ITEM-MATNR = GS_ALV5-MATNR .
    LS_ITEM-VBEL2 = GS_ALV5-VBELN .
    LS_ITEM-POSN2 = GS_ALV5-POSNR .
    LS_ITEM-WRBTR = GS_ALV5-NAFAP_DIV .
    COLLECT LS_ITEM INTO LT_ITEM .
    CLEAR LS_ITEM .
    CLEAR GS_ALV5 .
  ENDLOOP.
  SORT LT_ITEM BY WERKS MATNR VBEL2 .

  " 将数据放进凭证头行表
  LOOP AT LT_HEAD INTO LS_HEAD WHERE VBELN IS NOT INITIAL .

    " 头
    GS_HEAD-WERKS = LS_HEAD-WERKS .
    GS_HEAD-MATNR = LS_HEAD-MATNR .
    GS_HEAD-VBELN = LS_HEAD-VBELN .
    GS_HEAD-BELNR = LS_HEAD-BELNR .
    GS_HEAD-BLART = 'SA' .  " 凭证类型
    CONCATENATE P_LFGJA P_LFMON '10' INTO L_I_DATE .
    CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = L_I_DATE
      IMPORTING
        E_DATE = L_E_DATE.

    GS_HEAD-BLDAT = L_E_DATE .
    GS_HEAD-BUDAT = L_E_DATE .
    GS_HEAD-BUKRS = P_BUKRS .
    GS_HEAD-WAERS = L_WAERS .
    GS_HEAD-MONAT = P_LFMON .
    GS_HEAD-NAFAZ = LS_HEAD-NAFAP_DIV .
    L_LFGJA = P_LFGJA .
    L_LFMON = P_LFMON .
    L_NAFAP = LS_HEAD-NAFAP_DIV .
    CONCATENATE L_LFGJA L_LFMON L_NAFAP INTO GS_HEAD-BKTXT .

    APPEND GS_HEAD TO GT_HEAD .
    CLEAR GS_HEAD .
    CLEAR: L_LFGJA,L_LFMON,L_NAFAP .

    " 按订单号、行号的维度汇总凭证行
    IF LS_HEAD-NAFAP_DIV LT 0 .
      " 分摊折旧成本为负数，则放到40，成本中心放到50
      READ TABLE LT_ITEM WITH KEY WERKS = LS_HEAD-WERKS
                                  MATNR = LS_HEAD-MATNR
                                  VBEL2 = LS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_ITEM INTO LS_ITEM FROM SY-TABIX .
          IF LS_ITEM-WERKS = LS_HEAD-WERKS AND
              LS_ITEM-MATNR = LS_HEAD-MATNR AND
              LS_ITEM-VBEL2 = LS_HEAD-VBELN .

            L_NUM = L_NUM + 1 .
            GS_ITEM-BUZEI = L_NUM .
            GS_ITEM-BSCHL = '40' .
            GS_ITEM-HKONT = '8002000021' .
            GS_ITEM-WERKS = LS_ITEM-WERKS .
            GS_ITEM-VBEL2 = LS_ITEM-VBEL2 .
            GS_ITEM-POSN2 = LS_ITEM-POSN2 .
            GS_ITEM-MATNR = LS_ITEM-MATNR .
            GS_ITEM-WRBTR = LS_ITEM-WRBTR .
            L_WRBTR = L_WRBTR + LS_ITEM-WRBTR .
            " 行项目文本
            L_LFGJA = P_LFGJA .
            L_LFMON = P_LFMON .
            L_NAFAP = LS_ITEM-WRBTR .
            CONCATENATE L_LFGJA L_LFMON L_NAFAP INTO GS_ITEM-SGTXT .

            APPEND GS_ITEM TO GT_ITEM .
            CLEAR GS_ITEM .

            CLEAR: L_LFGJA,L_LFMON,L_NAFAP .
          ELSE.
            EXIT .
          ENDIF.
          CLEAR LS_ITEM .
        ENDLOOP.

        L_NUM = L_NUM + 1 .
        GS_ITEM-BUZEI = L_NUM .
        GS_ITEM-BSCHL = '50' .
        GS_ITEM-HKONT = '8002000021' .
        GS_ITEM-WERKS = P_WERKS .
        GS_ITEM-VBEL2 = LS_HEAD-VBELN .
*          GS_ITEM-POSN2 = GS_ALV5-POSNR .
        GS_ITEM-KOSTL = L_KOSTL .
        GS_ITEM-MATNR = LS_HEAD-MATNR .
        GS_ITEM-WRBTR = - L_WRBTR .
        " 上项目文本
        L_LFGJA = P_LFGJA .
        L_LFMON = P_LFMON .
        L_NAFAP = GS_ITEM-WRBTR .
        CONCATENATE L_LFGJA L_LFMON L_NAFAP INTO GS_ITEM-SGTXT .

        APPEND GS_ITEM TO GT_ITEM .
        CLEAR GS_ITEM .

        CLEAR: L_LFGJA,L_LFMON,L_NAFAP,L_WRBTR .
      ENDIF.

    ELSEIF LS_HEAD-NAFAP_DIV GT 0 . " 分摊折旧成本为正数，则放到50，成本中心放到40
      READ TABLE LT_ITEM WITH KEY WERKS = LS_HEAD-WERKS
                                  MATNR = LS_HEAD-MATNR
                                  VBEL2 = LS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_ITEM INTO LS_ITEM FROM SY-TABIX .
          IF LS_ITEM-WERKS = LS_HEAD-WERKS AND
              LS_ITEM-MATNR = LS_HEAD-MATNR AND
              LS_ITEM-VBEL2 = LS_HEAD-VBELN .

            L_NUM = L_NUM + 1 .
            GS_ITEM-BUZEI = L_NUM .
            GS_ITEM-BSCHL = '50' .
            GS_ITEM-HKONT = '8002000021' .
            GS_ITEM-WERKS = LS_ITEM-WERKS .
            GS_ITEM-VBEL2 = LS_ITEM-VBEL2 .
            GS_ITEM-POSN2 = LS_ITEM-POSN2 .
            GS_ITEM-MATNR = LS_ITEM-MATNR .
            GS_ITEM-WRBTR = LS_ITEM-WRBTR .
            L_WRBTR = L_WRBTR + LS_ITEM-WRBTR .
            " 行项目文本
            L_LFGJA = P_LFGJA .
            L_LFMON = P_LFMON .
            L_NAFAP = LS_ITEM-WRBTR .
            CONCATENATE L_LFGJA L_LFMON L_NAFAP INTO GS_ITEM-SGTXT .

            APPEND GS_ITEM TO GT_ITEM .
            CLEAR GS_ITEM .

            CLEAR: L_LFGJA,L_LFMON,L_NAFAP .
          ELSE.
            EXIT .
          ENDIF.
          CLEAR LS_ITEM .
        ENDLOOP.

        L_NUM = L_NUM + 1 .
        GS_ITEM-BUZEI = L_NUM .
        GS_ITEM-BSCHL = '40' .
        GS_ITEM-HKONT = '8002000021' .
        GS_ITEM-WERKS = P_WERKS .
        GS_ITEM-VBEL2 = LS_HEAD-VBELN .
*          GS_ITEM-POSN2 = GS_ALV5-POSNR .
        GS_ITEM-KOSTL = L_KOSTL .
        GS_ITEM-MATNR = LS_HEAD-MATNR .
        GS_ITEM-WRBTR = - L_WRBTR .
        " 上项目文本
        L_LFGJA = P_LFGJA .
        L_LFMON = P_LFMON .
        L_NAFAP = GS_ITEM-WRBTR .
        CONCATENATE L_LFGJA L_LFMON L_NAFAP INTO GS_ITEM-SGTXT .

        APPEND GS_ITEM TO GT_ITEM .
        CLEAR GS_ITEM .

        CLEAR: L_LFGJA,L_LFMON,L_NAFAP,L_WRBTR .
      ENDIF.
    ENDIF.

    CLEAR L_NUM .
    CLEAR LS_HEAD .
  ENDLOOP.

  SORT GT_ITEM BY WERKS MATNR VBEL2 .
  CLEAR: L_WAERS,L_KOSTL .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4698   text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_PREP  .

  " 清空BAPI变量
  PERFORM FRM_BAPI_DATA_CLEAR.

  DATA: L_KATYP TYPE CSKB-KATYP.

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  GS_DOCUMENTHEADER-DOC_DATE     =  GS_HEAD-BLDAT.
*过账日期
  GS_DOCUMENTHEADER-PSTNG_DATE   =  GS_HEAD-BUDAT.
*凭证类型
  GS_DOCUMENTHEADER-DOC_TYPE     =  GS_HEAD-BLART.
*公司代码
  GS_DOCUMENTHEADER-COMP_CODE    =  GS_HEAD-BUKRS.
*凭证抬头文本
  GS_DOCUMENTHEADER-HEADER_TXT   =  GS_HEAD-BKTXT.
*创建人员
  GS_DOCUMENTHEADER-USERNAME     =  SY-UNAME.
  "过账期间
  GS_DOCUMENTHEADER-FIS_PERIOD     =  GS_HEAD-MONAT .
**参考凭证号 - 业务类型
*  GS_DOCUMENTHEADER-REF_DOC_NO   =  GS_HEAD-BSTYP.


**********************************************************************
* 凭证行
**********************************************************************
  READ TABLE GT_ITEM WITH KEY WERKS = GS_HEAD-WERKS
                              MATNR = GS_HEAD-MATNR
                              VBEL2 = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .

    LOOP AT GT_ITEM INTO GS_ITEM FROM SY-TABIX .
      IF GS_ITEM-WERKS = GS_HEAD-WERKS AND
          GS_ITEM-MATNR = GS_HEAD-MATNR AND
          GS_ITEM-VBEL2 = GS_HEAD-VBELN .

        " 总账
        CLEAR GS_ACCOUNTGL.
*       行项目号
        GS_ACCOUNTGL-ITEMNO_ACC = GS_ITEM-BUZEI.
*       科目
        GS_ACCOUNTGL-GL_ACCOUNT = GS_ITEM-HKONT.
*       项目文本
        GS_ACCOUNTGL-ITEM_TEXT = GS_ITEM-SGTXT.
*       物料
        GS_ACCOUNTGL-MATERIAL = GS_ITEM-MATNR.
*       税码
*      GS_ACCOUNTGL-TAX_CODE = GS_ITEM-MWSKZ.

        IF GS_HEAD-NAFAZ LT 0 .
          IF GS_ITEM-BSCHL = '40'  .
            GS_ACCOUNTGL-SALES_ORD = GS_ITEM-VBEL2.
            GS_ACCOUNTGL-S_ORD_ITEM = GS_ITEM-POSN2.
          ELSEIF GS_ITEM-BSCHL = '50'  .
            GS_ACCOUNTGL-COSTCENTER = GS_ITEM-KOSTL.
          ENDIF.
        ELSEIF GS_HEAD-NAFAZ GT 0 .
          IF GS_ITEM-BSCHL = '50'  .
            GS_ACCOUNTGL-SALES_ORD = GS_ITEM-VBEL2.
            GS_ACCOUNTGL-S_ORD_ITEM = GS_ITEM-POSN2.
          ELSEIF GS_ITEM-BSCHL = '40'  .
            GS_ACCOUNTGL-COSTCENTER = GS_ITEM-KOSTL.
          ENDIF.
        ENDIF.

*      GS_ACCOUNTGL-ALLOC_NMBR = GS_HEAD-VBELN.

        APPEND GS_ACCOUNTGL TO GT_ACCOUNTGL.

        CLEAR GS_CURRENCYAMOUNT.
        GS_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
        GS_CURRENCYAMOUNT-CURRENCY = GS_HEAD-WAERS.
*       金额
        IF GS_HEAD-NAFAZ LT 0 .
          IF GS_ITEM-BSCHL = '40'.
            IF GS_ITEM-WRBTR LE 0 .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_ITEM-WRBTR.
            ELSE .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
            ENDIF.
          ELSEIF GS_ITEM-BSCHL = '50'.
            IF GS_ITEM-WRBTR LE 0 .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
            ELSE .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_ITEM-WRBTR.
            ENDIF.
          ENDIF.
        ELSEIF GS_HEAD-NAFAZ GT 0 .
          IF GS_ITEM-BSCHL = '50'.
            IF GS_ITEM-WRBTR LE 0 .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
            ELSE .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_ITEM-WRBTR.
            ENDIF.
          ELSEIF GS_ITEM-BSCHL = '40'.
            IF GS_ITEM-WRBTR LE 0 .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_ITEM-WRBTR.
            ELSE .
              GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

        " 记账码 & 反记账
        CLEAR GS_EXTENSION2.
        CLEAR GS_ZACCDOCUEXT.
        GS_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
        GS_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL.
*      IF WA_ITEM-BSCHL = '11' OR WA_ITEM-BSCHL = '40'.
*        WA_ZACCDOCUEXT-XNEGP = 'X'.
*      ENDIF.
        GS_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
        GS_EXTENSION2-VALUEPART1 = GS_ZACCDOCUEXT.
        APPEND GS_EXTENSION2 TO GT_EXTENSION2.

        CLEAR GS_ITEM .


      ELSE .
        EXIT .
      ENDIF.
      CLEAR GS_ITEM .
    ENDLOOP.
  ENDIF .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_CLEAR .
  REFRESH: GT_ACCOUNTGL, GT_ACCOUNTRECEIVABLE, GT_CURRENCYAMOUNT, GT_CRITERIA, GT_VALUEFIELD, GT_EXTENSION2, GT_RETURN.
  CLEAR: GS_DOCUMENTHEADER, GS_OBJ.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_BELNR  text
*      <--P_GJAHR  text
*----------------------------------------------------------------------*
FORM FRM_CALL_BAPI  CHANGING P_BELNR
                             P_GJAHR
                              P_MSG .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = GS_DOCUMENTHEADER
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
      OBJ_TYPE          = GS_OBJ-OBJ_TYPE
      OBJ_KEY           = GS_OBJ-OBJ_KEY
      OBJ_SYS           = GS_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL         = GT_ACCOUNTGL
      ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
      CRITERIA          = GT_CRITERIA
      VALUEFIELD        = GT_VALUEFIELD
*     EXTENSION1        =
      RETURN            = GT_RETURN
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      EXTENSION2        = GT_EXTENSION2
*     REALESTATE        =
*     ACCOUNTWT         =
    .


  READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    P_MSG = GS_RETURN-MESSAGE .
*    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    " 回写生成的会计凭证号与会计凭证年度
    P_BELNR = GS_OBJ-OBJ_KEY(10).
    P_GJAHR = GS_OBJ-OBJ_KEY+14(4).
  ENDIF.

  LOOP AT GT_RETURN INTO GS_RETURN WHERE TYPE = 'E' OR TYPE = 'A' .
    CONCATENATE P_MSG GS_RETURN-MESSAGE INTO P_MSG .
    CLEAR GS_RETURN .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_ZZJFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SAVE_ZZJFT USING P_NUM .

  DATA: L_I_DATE TYPE SY-DATUM,
        L_E_DATE TYPE SY-DATUM.

  READ TABLE GT_ALV_POST1 WITH KEY WERKS = GS_HEAD-WERKS
                                   MATNR = GS_HEAD-MATNR
                                   VBELN = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_ALV_POST1 INTO GS_ALV_POST1 FROM SY-TABIX .
      IF GS_ALV_POST1-WERKS = GS_HEAD-WERKS AND
          GS_ALV_POST1-MATNR = GS_HEAD-MATNR AND
          GS_ALV_POST1-VBELN = GS_HEAD-VBELN .

        READ TABLE GT_ZZJFT_H WITH KEY BUKRS = P_BUKRS
                                       WERKS = P_WERKS
                                       GJAHR = P_LFGJA
                                       MONAT = P_LFMON
                                       MATNR = GS_ALV_POST1-MATNR
                                       LGORT = GS_ALV_POST1-LGORT
                                       VBELN = GS_ALV_POST1-VBELN
                                       POSNR = GS_ALV_POST1-POSNR
                                       LIFNR = GS_ALV_POST1-LIFNR TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0.
          CONTINUE .
        ENDIF.
        P_NUM = P_NUM + 1 .
        GS_ZZJFT-BUKRS = P_BUKRS .
        GS_ZZJFT-WERKS = P_WERKS .
        GS_ZZJFT-GJAHR = P_LFGJA .
        GS_ZZJFT-MONAT = P_LFMON .
        GS_ZZJFT-ZXH = P_NUM .

        G_LFGJA = P_LFGJA .
        G_LFMON = P_LFMON - 1 .
        CONCATENATE G_LFGJA G_LFMON '10' INTO L_I_DATE .
        CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
          EXPORTING
            I_DATE = L_I_DATE
          IMPORTING
            E_DATE = L_E_DATE.

        GS_ZZJFT-BUDAT = L_E_DATE .
        GS_ZZJFT-ZYWLX = GS_ALV_POST1-ZYWLX .
        GS_ZZJFT-MATNR = GS_ALV_POST1-MATNR .
        GS_ZZJFT-LGORT = GS_ALV_POST1-LGORT .
        GS_ZZJFT-VBELN = GS_ALV_POST1-VBELN .
        GS_ZZJFT-POSNR = GS_ALV_POST1-POSNR .
        GS_ZZJFT-LIFNR = GS_ALV_POST1-LIFNR .
        GS_ZZJFT-MENGE = GS_ALV_POST1-OS_QTY .
        GS_ZZJFT-MENGE_SUM = GS_ALV_POST1-MENGE .
        GS_ZZJFT-MEINS = GS_ALV_POST1-MEINS .
        GS_ZZJFT-ANLN1 = GS_ALV_POST1-ANLN1 .
        GS_ZZJFT-NAFAZ = GS_ALV_POST1-NAFAZ .
*    LS_ZZJFT-ZYTS = GS_ALV_POST1-ZYTS .
*    LS_ZZJFT-DYTS = GS_ALV_POST1-LGORT .
        GS_ZZJFT-ZFTBL = GS_ALV_POST1-FTBL .
        GS_ZZJFT-NAFAP_DIV = GS_ALV_POST1-FTZJCB .
        GS_ZZJFT-BELNR = GS_HEAD-BELNR .

        APPEND GS_ZZJFT TO GT_ZZJFT .
        CLEAR GS_ZZJFT .
        CLEAR GS_ALV_POST1 .
      ELSE .
        EXIT .
      ENDIF.
      CLEAR GS_ALV_POST1 .
    ENDLOOP.
  ENDIF.

  READ TABLE GT_ALV_POST2 WITH KEY WERKS = GS_HEAD-WERKS
                                   MATNR = GS_HEAD-MATNR
                                   MAT_KDAUF = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_ALV_POST2 INTO GS_ALV_POST2 FROM SY-TABIX .
      IF GS_ALV_POST2-WERKS = GS_HEAD-WERKS AND
          GS_ALV_POST2-MATNR = GS_HEAD-MATNR AND
          GS_ALV_POST2-MAT_KDAUF = GS_HEAD-VBELN .

* 从
        READ TABLE GT_ZZJFT_H WITH KEY BUKRS = P_BUKRS
                                       WERKS = P_WERKS
                                       GJAHR = P_LFGJA
                                       MONAT = P_LFMON
                                       MATNR = GS_ALV_POST2-MATNR
                                       LGORT = GS_ALV_POST2-LGORT
                                       VBELN = GS_ALV_POST2-MAT_KDAUF
                                       POSNR = GS_ALV_POST2-MAT_KDPOS
                                       LIFNR = GS_ALV_POST2-LIFNR1 TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0.
          CONTINUE .
        ENDIF.
        P_NUM = P_NUM + 1 .
        GS_ZZJFT-BUKRS = P_BUKRS .
        GS_ZZJFT-WERKS = P_WERKS .
        GS_ZZJFT-GJAHR = P_LFGJA .
        GS_ZZJFT-MONAT = P_LFMON .
        GS_ZZJFT-ZXH = P_NUM .

*        G_LFGJA = P_LFGJA .
*        G_LFMON = P_LFMON - 1 .
*        CONCATENATE G_LFGJA G_LFMON '10' INTO L_I_DATE .
*        CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
*          EXPORTING
*            I_DATE = L_I_DATE
*          IMPORTING
*            E_DATE = L_E_DATE.

        GS_ZZJFT-BUDAT = GS_ALV_POST2-BUDAT .
        GS_ZZJFT-ZYWLX = GS_ALV_POST2-ZYWLX1 .
        GS_ZZJFT-MATNR = GS_ALV_POST2-MATNR .
        GS_ZZJFT-LGORT = GS_ALV_POST2-LGORT .
        GS_ZZJFT-VBELN = GS_ALV_POST2-MAT_KDAUF .
        GS_ZZJFT-POSNR = GS_ALV_POST2-MAT_KDPOS .
        GS_ZZJFT-LIFNR = GS_ALV_POST2-LIFNR1 .
        GS_ZZJFT-MENGE = GS_ALV_POST2-MENGE .
        GS_ZZJFT-MENGE_SUM = GS_ALV_POST2-QCKC .
        GS_ZZJFT-MEINS = GS_ALV_POST2-MEINS .
        GS_ZZJFT-ANLN1 = GS_ALV_POST2-ANLN1 .
        GS_ZZJFT-NAFAZ = GS_ALV_POST2-NAFAZ .
        GS_ZZJFT-ZYTS = GS_ALV_POST2-ZYTS .
        GS_ZZJFT-DYTS = GS_ALV_POST2-DYTS .
        GS_ZZJFT-ZFTBL = GS_ALV_POST2-FTBL .
        GS_ZZJFT-NAFAP_DIV = - GS_ALV_POST2-FTZJCB .
        GS_ZZJFT-BELNR = GS_HEAD-BELNR .

        APPEND GS_ZZJFT TO GT_ZZJFT .
        CLEAR GS_ZZJFT .

        CLEAR GS_ALV_POST2 .
      ELSE .
        EXIT .
      ENDIF.
      CLEAR GS_ALV_POST2 .
    ENDLOOP.
  ENDIF.

  READ TABLE GT_ALV_POST3 WITH KEY WERKS = GS_HEAD-WERKS
                                   MATNR = GS_HEAD-MATNR
                                   KDAUF = GS_HEAD-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_ALV_POST3 INTO GS_ALV_POST3 FROM SY-TABIX .
      IF GS_ALV_POST3-WERKS = GS_HEAD-WERKS AND
          GS_ALV_POST3-MATNR = GS_HEAD-MATNR AND
          GS_ALV_POST3-KDAUF = GS_HEAD-VBELN .

* 到
        READ TABLE GT_ZZJFT_H WITH KEY BUKRS = P_BUKRS
                                       WERKS = P_WERKS
                                       GJAHR = P_LFGJA
                                       MONAT = P_LFMON
                                       MATNR = GS_ALV_POST3-MATNR
                                       LGORT = GS_ALV_POST3-UMLGO
                                       VBELN = GS_ALV_POST3-KDAUF
                                       POSNR = GS_ALV_POST3-KDPOS
                                       LIFNR = GS_ALV_POST3-LIFNR2 TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0.
          CONTINUE .
        ENDIF.
        P_NUM = P_NUM + 1 .
        GS_ZZJFT-BUKRS = P_BUKRS .
        GS_ZZJFT-WERKS = P_WERKS .
        GS_ZZJFT-GJAHR = P_LFGJA .
        GS_ZZJFT-MONAT = P_LFMON .
        GS_ZZJFT-ZXH = P_NUM .

*        G_LFGJA = P_LFGJA .
*        G_LFMON = P_LFMON - 1 .
*        CONCATENATE G_LFGJA G_LFMON '10' INTO L_I_DATE .
*        CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
*          EXPORTING
*            I_DATE = L_I_DATE
*          IMPORTING
*            E_DATE = L_E_DATE.

        GS_ZZJFT-BUDAT = GS_ALV_POST3-BUDAT .
        GS_ZZJFT-ZYWLX = GS_ALV_POST3-ZYWLX2 .
        GS_ZZJFT-MATNR = GS_ALV_POST3-MATNR .
        GS_ZZJFT-LGORT = GS_ALV_POST3-UMLGO .
        GS_ZZJFT-VBELN = GS_ALV_POST3-KDAUF .
        GS_ZZJFT-POSNR = GS_ALV_POST3-KDPOS .
        GS_ZZJFT-LIFNR = GS_ALV_POST3-LIFNR2 .
        GS_ZZJFT-MENGE = GS_ALV_POST3-MENGE .
        GS_ZZJFT-MENGE_SUM = GS_ALV_POST3-QCKC .
        GS_ZZJFT-MEINS = GS_ALV_POST3-MEINS .
        GS_ZZJFT-ANLN1 = GS_ALV_POST3-ANLN1 .
        GS_ZZJFT-NAFAZ = GS_ALV_POST3-NAFAZ .
        GS_ZZJFT-ZYTS = GS_ALV_POST3-ZYTS .
        GS_ZZJFT-DYTS = GS_ALV_POST3-DYTS .
        GS_ZZJFT-ZFTBL = GS_ALV_POST3-FTBL .
        GS_ZZJFT-NAFAP_DIV = GS_ALV_POST3-FTZJCB .
        GS_ZZJFT-BELNR = GS_HEAD-BELNR .

        APPEND GS_ZZJFT TO GT_ZZJFT .
        CLEAR GS_ZZJFT .

        CLEAR GS_ALV_POST3 .
      ELSE .
        CLEAR GS_ALV_POST3 .
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.

  INSERT ZZJFT FROM TABLE GT_ZZJFT.
  IF SY-SUBRC = 0 .
    COMMIT WORK AND WAIT .
  ELSE .
    ROLLBACK WORK .
  ENDIF.
  MOVE-CORRESPONDING GT_ZZJFT TO GT_ZZJFT_H .

  CLEAR: L_I_DATE ,L_E_DATE .
  REFRESH GT_ZZJFT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ACC_REVERSAL .

*  DATA: LT_ALV LIKE GT_ALV5_1 ,
*        LS_ALV LIKE LINE OF LT_ALV.
*  DATA: LS_BKPF TYPE BKPF .

  DATA L_TABIX TYPE SY-TABIX .

  " 取冲销原因和过账日期
*  CLEAR GS_REVERSAL.
*  PERFORM FRM_GET_REV_REASON.
*  GS_REVERSAL-REASON_REV = '03' .
*  GS_REVERSAL-PSTNG_DATE = '' .
*  IF GS_REVERSAL-REASON_REV IS INITIAL.
*    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

*  PERFORM FRM_MESSAGE_INITIAL.
*  CLEAR G_SUC.
*  MOVE-CORRESPONDING GT_ALV5_1 TO LT_ALV .
*  SORT LT_ALV BY VBELN .
*  DELETE ADJACENT DUPLICATES FROM LT_ALV COMPARING VBELN .
  DATA L_FLAG TYPE C .
  DATA L_FLAG_REV TYPE C .
  LOOP AT GT_ALV5_1 INTO GS_ALV5_1 WHERE BELNR IS NOT INITIAL .
    L_FLAG = 'X' .
    IF GS_ALV5_1-BELNR_REV IS NOT INITIAL .
      L_FLAG_REV = 'X' .
    ENDIF.
    CLEAR GS_ALV5_1 .
  ENDLOOP.
  IF L_FLAG IS INITIAL .
    MESSAGE '数据全都还没过账，无法冲销' TYPE 'E' .
    LEAVE LIST-PROCESSING .
    RETURN .
  ELSE .

    IF L_FLAG_REV = 'X'.
      MESSAGE '不允许重复冲销' TYPE 'E' .
      LEAVE LIST-PROCESSING .
      RETURN .
    ENDIF.
    DATA: BEGIN OF LS_BKPF ,
            BUKRS TYPE BKPF-BUKRS,
            BELNR TYPE BKPF-BELNR,
            GJAHR TYPE BKPF-GJAHR,
            BUDAT TYPE BKPF-BUDAT,
            AWTYP TYPE BKPF-AWTYP,
            AWKEY TYPE BKPF-AWKEY,
          END OF LS_BKPF .
    DATA LT_BKPF LIKE TABLE OF LS_BKPF .
    SELECT BUKRS
           BELNR
           GJAHR
           BUDAT
           AWTYP
           AWKEY
      INTO CORRESPONDING FIELDS OF TABLE LT_BKPF
      FROM BKPF
       FOR ALL ENTRIES IN GT_ALV5_1
     WHERE BUKRS = GT_ALV5_1-BUKRS
       AND BELNR = GT_ALV5_1-BELNR .
    SORT LT_BKPF BY BUKRS BELNR GJAHR .

    LOOP AT GT_ALV5_1 INTO GS_ALV5_1 WHERE BELNR IS NOT INITIAL .
      L_TABIX = SY-TABIX .
*    SELECT SINGLE *
*      INTO LS_BKPF
*      FROM BKPF
*      WHERE BUKRS = P_BUKRS
*        AND BELNR = LS_ALV-BELNR
*        AND GJAHR = P_LFGJA.
      READ TABLE LT_BKPF INTO LS_BKPF WITH KEY BUKRS = GS_ALV5_1-BUKRS
                                               BELNR = GS_ALV5_1-BELNR
                                               GJAHR = GS_ALV5_1-GJAHR BINARY SEARCH .
      EXPORT LS_BKPF TO MEMORY ID 'LS_BKPF' .
      PERFORM FRM_BAPI_CLEAR .
      PERFORM FRM_REV_PREP .
      PERFORM FRM_REVERSAL_BAPI CHANGING GS_ALV5_1 .

      MODIFY GT_ALV5_1 FROM GS_ALV5_1 INDEX L_TABIX .

      IF GS_ALV5_1-BELNR_REV IS NOT INITIAL .
        DELETE FROM ZZJFT WHERE BUKRS = GS_ALV5_1-BUKRS
                            AND WERKS = GS_ALV5_1-WERKS
                            AND GJAHR = GS_ALV5_1-GJAHR
                            AND MONAT = GS_ALV5_1-MONAT
                            AND MATNR = GS_ALV5_1-MATNR
                            AND VBELN = GS_ALV5_1-VBELN .
        IF SY-SUBRC = 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

      CLEAR L_TABIX .
      CLEAR LS_BKPF .
      CLEAR GS_ALV5_1 .
    ENDLOOP.

  ENDIF.
*  LOOP AT LT_ALV INTO LS_ALV .
*    READ TABLE GT_ALV5 WITH KEY VBELN = LS_ALV-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
*    IF SY-SUBRC = 0 .
*      LOOP AT GT_ALV5 INTO GS_ALV5 FROM SY-TABIX .
*        IF GS_ALV5-VBELN = LS_ALV-VBELN .
*          GS_ALV5-TYPE_REV = LS_ALV-TYPE_REV .
*          GS_ALV5-MSG_REV = LS_ALV-MSG_REV .
*          GS_ALV5-BELNR_REV = LS_ALV-BELNR_REV .
*          MODIFY GT_ALV5 FROM GS_ALV5 INDEX SY-TABIX .
*        ELSE .
*          EXIT .
*        ENDIF.
*        CLEAR GS_ALV5 .
*      ENDLOOP.
*    ENDIF.
*    CLEAR LS_ALV.
*  ENDLOOP.

*  PERFORM FRM_MESSAGE_SHOW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_REV_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_REV_REASON .
*  CALL SCREEN 9005 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REV_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REV_PREP .
  DATA: BEGIN OF LS_BKPF ,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          BUDAT TYPE BKPF-BUDAT,
          AWTYP TYPE BKPF-AWTYP,
          AWKEY TYPE BKPF-AWKEY,
        END OF LS_BKPF .

  IMPORT LS_BKPF FROM MEMORY ID 'LS_BKPF' .
  GS_REVERSAL-OBJ_TYPE  = LS_BKPF-AWTYP.
  GS_REVERSAL-OBJ_KEY   = LS_BKPF-AWKEY.
  GS_REVERSAL-OBJ_KEY_R = LS_BKPF-AWKEY.
  GS_REVERSAL-REASON_REV = '03'.
  GS_REVERSAL-PSTNG_DATE = LS_BKPF-BUDAT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSAL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REVERSAL_BAPI CHANGING PS_ALV LIKE GS_ALV5_1 .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      REVERSAL = GS_REVERSAL
      BUS_ACT  = 'RFBU'
    IMPORTING
      OBJ_TYPE = GS_OBJ-OBJ_TYPE
      OBJ_KEY  = GS_OBJ-OBJ_KEY
      OBJ_SYS  = GS_OBJ-OBJ_SYS
    TABLES
      RETURN   = GT_RETURN.

  CLEAR: PS_ALV-TYPE_REV,PS_ALV-MSG_REV .
  READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PS_ALV-TYPE_REV = 'E' .
*    PS_ALV-MSG_REV = '冲销失败'.
*    CONCATENATE '冲销失败：' GS_RETURN-MESSAGE INTO PS_ALV-MSG_REV .
    CLEAR GS_RETURN .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    PS_ALV-TYPE_REV = 'S' .
    PS_ALV-MSG_REV = '冲销成功'.
    " 回写生成的会计凭证号与会计凭证年度
    PS_ALV-BELNR_REV = GS_OBJ-OBJ_KEY(10).
*    G_SUC = 'X'.
  ENDIF.

  DATA L_MSG TYPE STRING .
  IF PS_ALV-TYPE_REV = 'E'.
    LOOP AT GT_RETURN INTO GS_RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
      CONCATENATE L_MSG GS_RETURN-MESSAGE INTO L_MSG .
      CLEAR GS_RETURN .
    ENDLOOP.
    CONCATENATE '冲销失败:' L_MSG INTO L_MSG .
    PS_ALV-MSG_REV = L_MSG .
  ENDIF.
*  LOOP AT IT_RETURN INTO WA_RETURN.
**    call function 'MESSAGE_TEXT_BUILD'
**      exporting
**        msgid               = wa_return-id
**        msgnr               = wa_return-number
**        msgv1               = wa_return-message_v1
**        msgv2               = wa_return-message_v2
**        msgv3               = wa_return-message_v3
**        msgv4               = wa_return-message_v4
**      importing
**        message_text_output = g_msg.
*
**    write: / g_msg.
*    PERFORM STORE_MESSAGES USING WA_RETURN-ID
*                                 WA_RETURN-TYPE
*                                 WA_RETURN-MESSAGE_V1
*                                 WA_RETURN-MESSAGE_V2
*                                 WA_RETURN-MESSAGE_V3
*                                 WA_RETURN-MESSAGE_V4
*                                 WA_RETURN-NUMBER.
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_CLEAR .
  REFRESH GT_RETURN .
  CLEAR GS_OBJ .
  CLEAR GS_REVERSAL .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CLEAR_HEADITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CLEAR_HEADITEM .
  REFRESH: GT_HEAD,GT_ITEM .
ENDFORM.
