*&---------------------------------------------------------------------*
*&  包含                ZFI042_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  DATA: BEGIN OF ls_t001k,
          bwkey TYPE t001k-bwkey,
          bukrs TYPE t001k-bukrs,
        END OF ls_t001k .
  DATA lt_t001k LIKE TABLE OF ls_t001k .
  DATA: BEGIN OF ls_t001w ,
          werks TYPE t001w-werks,
          bwkey TYPE t001w-bwkey,
        END OF ls_t001w .
  DATA lt_t001w LIKE TABLE OF ls_t001w .
  DATA: BEGIN OF ls_mce,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
          vkorg TYPE mvke-vkorg,
          vtweg TYPE mvke-vtweg,
          mvgr1 TYPE mvke-mvgr1,
        END OF ls_mce .
  DATA lt_mce LIKE TABLE OF ls_mce .
  DATA: lt_mce_unique LIKE lt_mce,
        ls_mce_unique LIKE LINE OF lt_mce_unique.
*  DATA: BEGIN OF LS_MVKE ,
*          MATNR TYPE MVKE-MATNR,
*          VKORG TYPE MVKE-VKORG,
*          VTWEG TYPE MVKE-VTWEG,
*          MVGR1 TYPE MVKE-MVGR1,
*        END OF LS_MVKE .
*  DATA LT_MVKE LIKE TABLE OF LS_MVKE .
  DATA: BEGIN OF ls_makt,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF ls_makt .
  DATA lt_makt LIKE TABLE OF ls_makt .
* 普通库存
  DATA: BEGIN OF ls_mard,
          matnr TYPE mard-matnr,
          werks TYPE mard-werks,
          lgort TYPE mard-lgort,
          labst TYPE mard-labst,
          umlme TYPE mard-umlme,
          insme TYPE mard-insme,
          einme TYPE mard-einme,
          speme TYPE mard-speme,
          retme TYPE mard-retme,
        END OF ls_mard .
  DATA lt_mard LIKE TABLE OF ls_mard .
  DATA: BEGIN OF ls_mard_col ,
          matnr TYPE mard-matnr,
          werks TYPE mard-werks,
          labst TYPE mard-labst,
          umlme TYPE mard-umlme,
          insme TYPE mard-insme,
          einme TYPE mard-einme,
          speme TYPE mard-speme,
          retme TYPE mard-retme,
        END OF ls_mard_col .
  DATA lt_mard_col LIKE TABLE OF ls_mard_col .
*  DATA: BEGIN OF LS_MARDH,
*          MATNR TYPE MARDH-MATNR,
*          WERKS TYPE MARDH-WERKS,
*          LGORT TYPE MARDH-LGORT,
*          LFGJA TYPE MARDH-LFGJA,
*          LFMON TYPE MARDH-LFMON,
*          LABST TYPE MARDH-LABST,
*          UMLME TYPE MARDH-UMLME,
*          INSME TYPE MARDH-INSME,
*          EINME TYPE MARDH-EINME,
*          SPEME TYPE MARDH-SPEME,
*          RETME TYPE MARDH-RETME,
*        END OF LS_MARDH .
*  DATA LT_MARDH LIKE TABLE OF LS_MARDH .
*  DATA: BEGIN OF LS_MARDH_COL,
*          MATNR TYPE MARDH-MATNR,
*          WERKS TYPE MARDH-WERKS,
*          LABST TYPE MARDH-LABST,
*          UMLME TYPE MARDH-UMLME,
*          INSME TYPE MARDH-INSME,
*          EINME TYPE MARDH-EINME,
*          SPEME TYPE MARDH-SPEME,
*          RETME TYPE MARDH-RETME,
*        END OF LS_MARDH_COL .
*  DATA LT_MARDH_COL LIKE TABLE OF LS_MARDH_COL .
* 供应商外包库存
  DATA: BEGIN OF ls_mslb,
          matnr TYPE mslb-matnr,
          werks TYPE mslb-werks,
          charg TYPE mslb-charg,
          sobkz TYPE mslb-sobkz,
          lifnr TYPE mslb-lifnr,
          lblab TYPE mslb-lblab,
          lbins TYPE mslb-lbins,
          lbuml TYPE mslb-lbuml,
        END OF ls_mslb .
  DATA lt_mslb LIKE TABLE OF ls_mslb .
  DATA: BEGIN OF ls_mslb_col,
          matnr TYPE mslb-matnr,
          werks TYPE mslb-werks,
          lblab TYPE mslb-lblab,
          lbins TYPE mslb-lbins,
          lbuml TYPE mslb-lbuml,
        END OF ls_mslb_col .
  DATA lt_mslb_col LIKE TABLE OF ls_mslb_col .
*  DATA: BEGIN OF LS_MSLBH,
*          MATNR TYPE MSLBH-MATNR,
*          WERKS TYPE MSLBH-WERKS,
*          CHARG TYPE MSLBH-CHARG,
*          SOBKZ TYPE MSLBH-SOBKZ,
*          LIFNR TYPE MSLBH-LIFNR,
*          LFGJA TYPE MSLBH-LFGJA,
*          LFMON TYPE MSLBH-LFMON,
*          LBLAB TYPE MSLBH-LBLAB,
*          LBINS TYPE MSLBH-LBINS,
*          LBUML TYPE MSLBH-LBUML,
*        END OF LS_MSLBH .
*  DATA LT_MSLBH LIKE TABLE OF LS_MSLBH .
*  DATA: BEGIN OF LS_MSLBH_COL,
*          MATNR TYPE MSLBH-MATNR,
*          WERKS TYPE MSLBH-WERKS,
*          LBLAB TYPE MSLBH-LBLAB,
*          LBINS TYPE MSLBH-LBINS,
*          LBUML TYPE MSLBH-LBUML,
*        END OF LS_MSLBH_COL .
*  DATA LT_MSLBH_COL LIKE TABLE OF LS_MSLBH_COL .
* 销售订单库存
  DATA: BEGIN OF ls_mska,
          matnr TYPE mska-matnr,
          werks TYPE mska-werks,
          lgort TYPE mska-lgort,
          charg TYPE mska-charg,
          sobkz TYPE mska-sobkz,
          vbeln TYPE mska-vbeln,
          posnr TYPE mska-posnr,
          kalab TYPE mska-kalab,
          kains TYPE mska-kains,
          kaspe TYPE mska-kaspe,
          kaein TYPE mska-kaein,
        END OF ls_mska .
  DATA lt_mska LIKE TABLE OF ls_mska .
  DATA: BEGIN OF ls_mska_col,
          matnr TYPE mska-matnr,
          werks TYPE mska-werks,
          kalab TYPE mska-kalab,
          kains TYPE mska-kains,
          kaspe TYPE mska-kaspe,
          kaein TYPE mska-kaein,
        END OF ls_mska_col .
  DATA lt_mska_col LIKE TABLE OF ls_mska_col .
*  DATA: LT_MSKAH TYPE TABLE OF MSKAH,
*        LS_MSKAH TYPE MSKAH.
*  DATA: BEGIN OF LS_MSKAH_COL,
*          MATNR TYPE MSKAH-MATNR,
*          WERKS TYPE MSKAH-WERKS,
*          KALAB TYPE MSKAH-KALAB,
*          KAINS TYPE MSKAH-KAINS,
*          KASPE TYPE MSKAH-KASPE,
*          KAEIN TYPE MSKAH-KAEIN,
*        END OF LS_MSKAH_COL .
*  DATA LT_MSKAH_COL LIKE TABLE OF LS_MSKAH_COL .
* 固定资产
  DATA: BEGIN OF ls_anla,
          bukrs TYPE anla-bukrs,
          anln1 TYPE anla-anln1,
          menge TYPE anla-menge,
          txt50 TYPE anla-txt50,
        END OF ls_anla .
  DATA lt_anla LIKE TABLE OF ls_anla .
  DATA: BEGIN OF ls_anla_col,
          matnr TYPE marc-matnr,
          anln1 TYPE anla-anln1,
          menge TYPE anla-menge,
        END OF ls_anla_col .
  DATA lt_anla_col LIKE TABLE OF ls_anla_col .
  DATA l_len TYPE i VALUE IS INITIAL .
  DATA l_index TYPE sy-tabix .

  SELECT bwkey
         bukrs
    INTO CORRESPONDING FIELDS OF TABLE lt_t001k
    FROM t001k
   WHERE bukrs = p_bukrs .
  IF lt_t001k IS NOT INITIAL .
    SELECT werks
           bwkey
      INTO CORRESPONDING FIELDS OF TABLE lt_t001w
      FROM t001w
       FOR ALL ENTRIES IN lt_t001k
     WHERE bwkey = lt_t001k-bwkey
       AND werks IN s_werks .
    IF lt_t001w IS NOT INITIAL .
      SELECT c~matnr
             c~werks
             e~vkorg
             e~vtweg
             e~mvgr1
        INTO CORRESPONDING FIELDS OF TABLE lt_mce
        FROM marc AS c
       INNER JOIN mvke AS e
          ON c~matnr = e~matnr
         FOR ALL ENTRIES IN lt_t001w
       WHERE c~werks = lt_t001w-werks
         AND c~matnr IN s_matnr
         AND e~mvgr1 IN s_mvgr1 .
      IF lt_mce IS NOT INITIAL .
        SELECT matnr
               maktx
          INTO CORRESPONDING FIELDS OF TABLE lt_makt
          FROM makt
           FOR ALL ENTRIES IN lt_mce
         WHERE matnr = lt_mce-matnr
           AND spras = sy-langu .
        "库存信息从MSEG表取数，程玉婷 夏俊
        "获取期间的最后一天
        DATA: lv_date_last LIKE sy-datum.
*        DATA: lv_gjahr TYPE bkpf-gjahr.
*        DATA: lv_monat TYPE N LENGTH 2.
        RANGES: lr_werks FOR mseg-werks.
        lr_werks = 'IEQ'.
        LOOP AT lt_t001w INTO ls_t001w.
          lr_werks-low = ls_t001w-werks.
          APPEND lr_werks.
        ENDLOOP.
*        IF p_monat = 1.
*          lv_gjahr = p_gjahr - 1.
*          lv_date_last = lv_gjahr && '12' && '31'.
*        ELSE.
*          lv_monat = p_monat - 1.
          lv_date_last = p_gjahr && p_monat && '01'.
          CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = lv_date_last
            IMPORTING
              last_day_of_month = lv_date_last
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
*        ENDIF.
        DATA: BEGIN OF lt_mseg OCCURS 0,
                mblnr TYPE mblnr,
                mjahr TYPE mjahr,
                zeile TYPE mblpo,
                matnr TYPE matnr,
                werks TYPE werks_d,
                menge TYPE menge_d,
                shkzg TYPE shkzg,
              END OF lt_mseg.
        DATA: lt_mseg_sum LIKE TABLE OF lt_mseg WITH HEADER LINE.
        SELECT
          mseg~mblnr
          mseg~mjahr
          zeile
          matnr
          werks
          menge
          shkzg
          INTO CORRESPONDING FIELDS OF TABLE lt_mseg
          FROM mkpf
          INNER JOIN mseg
          ON mkpf~mblnr = mseg~mblnr
          AND mkpf~mjahr = mseg~mjahr
          FOR ALL ENTRIES IN lt_mce
          WHERE mkpf~budat <= lv_date_last
          AND   mseg~werks IN s_werks
          AND   mseg~werks IN lr_werks
          AND   mseg~matnr = lt_mce-matnr
          AND   mseg~bwart NOT IN ('103','104','124','125').
        LOOP AT lt_mseg .
          CLEAR: lt_mseg-mblnr,lt_mseg-mjahr,lt_mseg-zeile.
          IF lt_mseg-shkzg = 'H'.
            lt_mseg-menge = - lt_mseg-menge.
          ENDIF.
          CLEAR lt_mseg-shkzg.
          COLLECT lt_mseg INTO lt_mseg_sum.
        ENDLOOP.
        SORT lt_mseg_sum BY matnr werks.

***** 开始计算库存
****        " 普通库存(当前)
****        SELECT MATNR
****               WERKS
****               LGORT
****               LABST
****               UMLME
****               INSME
****               EINME
****               SPEME
****               RETME
****          INTO CORRESPONDING FIELDS OF TABLE LT_MARD
****          FROM MARD
****           FOR ALL ENTRIES IN  LT_MCE
****         WHERE MATNR = LT_MCE-MATNR
****           AND WERKS = LT_MCE-WERKS .
****        LOOP AT LT_MARD INTO LS_MARD.
****          MOVE-CORRESPONDING LS_MARD TO LS_MARD_COL .
****          COLLECT LS_MARD_COL INTO LT_MARD_COL .
****          CLEAR LS_MARD .
****          CLEAR LS_MARD_COL .
****        ENDLOOP.
****        " 普通库存(历史)
*****        SELECT MATNR
*****               WERKS
*****               LGORT
*****               LFGJA
*****               LFMON
*****               LABST
*****               UMLME
*****               INSME
*****               EINME
*****               SPEME
*****               RETME
*****          INTO CORRESPONDING FIELDS OF TABLE LT_MARDH
*****          FROM MARDH
*****           FOR ALL ENTRIES IN LT_MARC
*****         WHERE MATNR = LT_MARC-MATNR
*****           AND WERKS = LT_MARC-WERKS .
******           AND LFGJA IN S_GJAHR
******           AND LFMON = P_MONAT .
*****        LOOP AT LT_MARDH INTO LS_MARDH.
*****          MOVE-CORRESPONDING LS_MARDH TO LS_MARDH_COL .
*****          COLLECT LS_MARDH_COL INTO LT_MARDH_COL .
*****          CLEAR LS_MARDH .
*****          CLEAR LS_MARDH_COL .
*****        ENDLOOP.
****        " 供应商外包库存(当前)
****        SELECT MATNR
****               WERKS
****               CHARG
****               SOBKZ
****               LIFNR
****               LBLAB
****               LBINS
****               LBUML
****          INTO CORRESPONDING FIELDS OF TABLE LT_MSLB
****          FROM MSLB
****           FOR ALL ENTRIES IN LT_MCE
****         WHERE MATNR = LT_MCE-MATNR
****           AND WERKS = LT_MCE-WERKS .
****        LOOP AT LT_MSLB INTO LS_MSLB.
****          MOVE-CORRESPONDING LS_MSLB TO LS_MSLB_COL .
****          COLLECT LS_MSLB_COL INTO LT_MSLB_COL .
****          CLEAR LS_MSLB .
****          CLEAR LS_MSLB_COL .
****        ENDLOOP.
****        " 供应商外包库存(历史)
*****        SELECT MATNR
*****               WERKS
*****               CHARG
*****               SOBKZ
*****               LIFNR
*****               LFGJA
*****               LFMON
*****               LBLAB
*****               LBINS
*****               LBUML
*****          INTO CORRESPONDING FIELDS OF TABLE LT_MSLBH
*****          FROM MSLBH
*****           FOR ALL ENTRIES IN LT_MARC
*****         WHERE MATNR = LT_MARC-MATNR
*****           AND WERKS = LT_MARC-WERKS .
******           AND LFGJA IN S_GJAHR
******           AND LFMON = P_MONAT .
*****        LOOP AT LT_MSLBH INTO LS_MSLBH .
*****          MOVE-CORRESPONDING LS_MSLBH TO LS_MSLBH_COL .
*****          COLLECT LS_MSLBH_COL INTO LT_MSLBH_COL .
*****          CLEAR LS_MSLBH .
*****          CLEAR LS_MSLBH_COL .
*****        ENDLOOP.
****        "销售订单库存（当前）
****        SELECT MATNR
****               WERKS
****               LGORT
****               CHARG
****               SOBKZ
****               VBELN
****               POSNR
****               KALAB
****               KAINS
****               KASPE
****               KAEIN
****          INTO CORRESPONDING FIELDS OF TABLE LT_MSKA
****          FROM MSKA
****           FOR ALL ENTRIES IN LT_MCE
****         WHERE MATNR = LT_MCE-MATNR
****           AND WERKS = LT_MCE-WERKS .
****        LOOP AT LT_MSKA INTO LS_MSKA .
****          MOVE-CORRESPONDING LS_MSKA TO LS_MSKA_COL .
****          COLLECT LS_MSKA_COL INTO LT_MSKA_COL .
****          CLEAR LS_MSKA .
****          CLEAR LS_MSKA_COL .
****        ENDLOOP.
****        " 销售订单库存（历史）
*****        SELECT *
*****          INTO TABLE LT_MSKAH
*****          FROM MSKAH
*****           FOR ALL ENTRIES IN LT_MARC
*****         WHERE MATNR = LT_MARC-MATNR
*****           AND WERKS = LT_MARC-WERKS .
*****        LOOP AT LT_MSKAH INTO LS_MSKAH .
*****          MOVE-CORRESPONDING LS_MSKAH TO LS_MSKAH_COL .
*****          COLLECT LS_MSKAH_COL INTO LT_MSKAH_COL .
*****          CLEAR LS_MSKAH .
*****          CLEAR LS_MSKAH_COL .
*****        ENDLOOP.
        "库存信息从MSEG表取数，程玉婷 夏俊
        " 开始取固定资产相关信息
        SELECT bukrs
               anln1
               menge
               txt50
          INTO CORRESPONDING FIELDS OF TABLE lt_anla
          FROM anla
         WHERE bukrs = p_bukrs .
        SORT lt_anla BY anln1 .
        " 固定资产的值只要取一次就行了，所以物料号要去重
        MOVE-CORRESPONDING lt_mce TO lt_mce_unique .
        SORT lt_mce_unique BY matnr .
        DELETE ADJACENT DUPLICATES FROM lt_mce_unique COMPARING matnr .
        LOOP AT lt_mce_unique INTO ls_mce_unique .
          l_len = strlen( ls_mce_unique-matnr ) .
          IF l_len <= 12. "长度小于等于12不用截取
            READ TABLE lt_anla WITH KEY anln1 = ls_mce_unique-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
            IF sy-subrc = 0 .
              LOOP AT lt_anla INTO ls_anla FROM sy-tabix .
                IF ls_anla-anln1 = ls_mce_unique-matnr .
                  MOVE-CORRESPONDING ls_anla TO ls_anla_col .
                  ls_anla_col-matnr = ls_mce_unique-matnr .
                  COLLECT ls_anla_col INTO lt_anla_col .
                  CLEAR ls_anla .
                  CLEAR ls_anla_col .
                ELSE .
                  EXIT .
                ENDIF.
              ENDLOOP.
            ENDIF.
          ELSE. "长度大于12就要截取
            l_len = l_len - 12 .
            READ TABLE lt_anla WITH KEY anln1 = ls_mce_unique-matnr+l_len(12) BINARY SEARCH TRANSPORTING NO FIELDS .
            IF sy-subrc = 0 .
              LOOP AT lt_anla INTO ls_anla FROM sy-tabix .
                IF ls_anla-anln1 = ls_mce_unique-matnr+l_len(12) .
                  MOVE-CORRESPONDING ls_anla TO ls_anla_col .
                  ls_anla_col-matnr = ls_mce_unique-matnr .
                  COLLECT ls_anla_col INTO lt_anla_col .
                  CLEAR ls_anla .
                  CLEAR ls_anla_col .
                ELSE .
                  EXIT .
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF .
    ENDIF.
  ENDIF.

* 将数据装进ALV
  DATA l_num TYPE i VALUE IS INITIAL .
  LOOP AT lt_mce_unique INTO ls_mce_unique .
    l_num = l_num + 1 .
    gs_alv-num = l_num .
    gs_alv-bukrs = p_bukrs .
    gs_alv-werks = ls_mce_unique-werks .
    gs_alv-matnr = ls_mce_unique-matnr .
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_mce_unique-matnr .
    IF sy-subrc = 0 .
      gs_alv-maktx = ls_makt-maktx .
      CLEAR ls_makt .
    ENDIF.
    "库存信息从MSEG表取数，程玉婷 夏俊
******** 取普通库存
*******    READ TABLE lt_mard_col INTO ls_mard_col WITH KEY matnr = ls_mce_unique-matnr
*******                                                     werks = ls_mce_unique-werks .
*******    IF sy-subrc = 0 .
*******      gs_alv-labst = ls_mard_col-labst + ls_mard_col-umlme + ls_mard_col-insme + ls_mard_col-einme +
*******                     ls_mard_col-speme + ls_mard_col-retme .
*******      CLEAR ls_mard_col .
*******    ENDIF.
********    READ TABLE LT_MARDH_COL INTO LS_MARDH_COL WITH KEY MATNR = LS_MARC-MATNR
********                                                       WERKS = LS_MARC-WERKS .
********    IF SY-SUBRC = 0   .
********      GS_ALV-LABST = GS_ALV-LABST + LS_MARDH_COL-LABST + LS_MARDH_COL-UMLME + LS_MARDH_COL-INSME + LS_MARDH_COL-EINME +
********                                    LS_MARDH_COL-SPEME + LS_MARDH_COL-RETME .
********      CLEAR LS_MARDH_COL .
********    ENDIF.
******** 取供应商库存
*******    READ TABLE lt_mslb_col INTO ls_mslb_col WITH KEY matnr = ls_mce_unique-matnr
*******                                                     werks = ls_mce_unique-werks .
*******    IF sy-subrc = 0 .
*******      gs_alv-labst = gs_alv-labst + ls_mslb_col-lblab + ls_mslb_col-lbins + ls_mslb_col-lbuml .
*******      CLEAR ls_mslb_col .
*******    ENDIF.
********    READ TABLE LT_MSLBH_COL INTO LS_MSLBH_COL WITH KEY MATNR = LS_MARC-MATNR
********                                                       WERKS = LS_MARC-WERKS .
********    IF SY-SUBRC = 0 .
********      GS_ALV-LABST = GS_ALV-LABST + LS_MSLBH_COL-LBLAB + LS_MSLBH_COL-LBINS + LS_MSLBH_COL-LBUML .
********      CLEAR LS_MSLBH_COL .
********    ENDIF.
******** 取销售订单库存
*******    READ TABLE lt_mska_col INTO ls_mska_col WITH KEY matnr = ls_mce_unique-matnr
*******                                                     werks = ls_mce_unique-werks .
*******    IF sy-subrc = 0 .
*******      gs_alv-labst = gs_alv-labst + ls_mska_col-kalab + ls_mska_col-kains + ls_mska_col-kaspe + ls_mska_col-kaein .
*******      CLEAR ls_mska_col .
*******    ENDIF.
********    READ TABLE LT_MSKAH_COL INTO LS_MSKAH_COL WITH KEY MATNR = LS_MARC-MATNR
********                                                       WERKS = LS_MARC-WERKS .
********    IF SY-SUBRC = 0 .
********      GS_ALV-LABST = GS_ALV-LABST + LS_MSKAH_COL-KALAB + LS_MSKAH_COL-KAINS + LS_MSKAH_COL-KASPE + LS_MSKAH_COL-KAEIN .
********      CLEAR LS_MSKAH_COL .
********    ENDIF.
    CLEAR lt_mseg_sum.
    READ TABLE lt_mseg_sum WITH KEY matnr = ls_mce_unique-matnr
                                    werks = ls_mce_unique-werks .
    IF sy-subrc = 0.
      gs_alv-labst = lt_mseg_sum-menge.
    ENDIF.
    "库存信息从MSEG表取数，程玉婷 夏俊

* 计算资产数
    READ TABLE lt_anla_col INTO ls_anla_col WITH KEY matnr = ls_mce_unique-matnr .
    IF sy-subrc = 0 .
      gs_alv-anln1 = ls_anla_col-anln1 .
      gs_alv-menge = ls_anla_col-menge .
*      CLEAR LS_ANLA_COL .
    ENDIF.
    READ TABLE lt_anla INTO ls_anla WITH KEY anln1 = ls_anla_col-anln1 .
    IF sy-subrc = 0 .
      gs_alv-txt50 = ls_anla-txt50 .
      CLEAR ls_anla .
    ENDIF.
    gs_alv-chayi = abs( gs_alv-labst - gs_alv-menge ) .
    APPEND gs_alv TO gt_alv .
    CLEAR gs_alv .
    CLEAR ls_anla_col .

  ENDLOOP.

* 设置颜色行
  LOOP AT gt_alv INTO gs_alv .
    IF gs_alv-chayi <> 0 .
      gs_alv-clr = 'C610'.
      MODIFY gt_alv FROM gs_alv .
    ENDIF.
    CLEAR gs_alv .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_alv .
  PERFORM init_layout.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM frm_init_lvc.            "字段列定义
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
*  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM frm_output  .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_layout .
  gs_layout-zebra        = 'X'.
  gs_layout-cwidth_opt   = 'X'.
  gs_layout-box_fname = 'SEL'.
  gs_layout-info_fname = 'CLR'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'NUM'        '序号'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'BUKRS'        '公司代码'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'WERKS'        '工厂'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'LABST'        '物料数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'ANLN1'        '固定资产主编号'         '' '' '' '' '' '' ''.
  init_fieldcat 'TXT50'        '固定资产中文描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE'        '固定资产数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'CHAYI'        '物料与资产差异数'         '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_ALV  text
*      -->P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM frm_output .
*
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET = PU_STATUS
*     I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   = ''
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    = PW_GRID_SETTINGS
      is_layout_lvc      = gs_layout
      it_fieldcat_lvc    = gt_lvc
*     IT_EXCLUDING       = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        = PT_SORT[]
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         = PW_VARIANT
*     IT_EVENTS          = GT_EVENTS
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
