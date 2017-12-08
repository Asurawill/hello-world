FUNCTION zmes_sap_fixedassets_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_CJ_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CJ_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_XG_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_XG_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_ASSETS_IN STRUCTURE  ZMES_ASSETS
*"      T_MES_ASSETS_OUT STRUCTURE  ZMES_ASSETS_SEL
*"----------------------------------------------------------------------
  CLEAR:t_mes_assets_out,t_mes_assets_out[].
  DATA:gt_anla TYPE TABLE OF anla,
       gs_anla TYPE anla.

  DATA:gt_anlz TYPE TABLE OF anlz,
       gs_anlz TYPE anlz.

  DATA:gt_ankt TYPE TABLE OF ankt,
       gs_ankt TYPE ankt.

  DATA:gt_anlh TYPE TABLE OF anlh,
       gs_anlh TYPE anlh.

  DATA:gt_cskt TYPE TABLE OF cskt,
       gs_cskt TYPE cskt.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  IF t_mes_assets_in[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_anla
      FROM anla
      FOR ALL ENTRIES IN t_mes_assets_in
      WHERE bukrs = p_bukrs
      AND anln1 = t_mes_assets_in-anln1
      AND anlkl IN ('Z120','Z130','Z150')
      AND erdat BETWEEN p_cj_l AND p_cj_h
      AND aedat BETWEEN p_xg_l AND p_xg_h
      AND spras = p_spras.
    SORT gt_anla BY anln1 .

  ELSE.
    SELECT * INTO TABLE gt_anla
     FROM anla
     WHERE bukrs = p_bukrs
     AND anlkl IN ('Z120','Z130','Z150')
     AND erdat BETWEEN p_cj_l AND p_cj_h
     AND aedat BETWEEN p_xg_l AND p_xg_h
     AND spras = p_spras.

    SORT gt_anla BY anln1 .


  ENDIF.

  IF gt_anla[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_anlz
      FROM anlz
      FOR ALL ENTRIES IN gt_anla
      WHERE  bukrs = p_bukrs
      AND anln1 = gt_anla-anln1.
    SORT gt_anlz BY bukrs ASCENDING anln1 ASCENDING bdatu DESCENDING .
    DELETE ADJACENT DUPLICATES FROM gt_anlz COMPARING bukrs anln1 .

    "读取主资产号
    SELECT * INTO TABLE gt_anlh
      FROM anlh
      FOR ALL ENTRIES IN gt_anla
      WHERE  bukrs = p_bukrs
      AND anln1 = gt_anla-anln1.

    SORT gt_anlh BY anln1.

    "读取资产类别数据
    SELECT * INTO TABLE gt_ankt
      FROM ankt
      WHERE spras = p_spras .
    SORT gt_ankt BY anlkl .

    "查询成本中心
    SELECT * INTO TABLE gt_cskt
      FROM cskt
      WHERE spras = p_spras
      AND  kokrs = '1000'.
    SORT gt_cskt BY kostl .

    LOOP AT gt_anla INTO gs_anla.
      CLEAR:t_mes_assets_out.
      "公司代码
      " t_mes_assets_out-bukrs = gs_anla-bukrs.
      "主资产号
      t_mes_assets_out-anln1 = gs_anla-anln1.
      "资产分类
      t_mes_assets_out-anlkl = gs_anla-anlkl.
      "主资产描述
      t_mes_assets_out-txt50 = gs_anla-txt50.
      "成本中心
      READ TABLE  gt_anlz  INTO gs_anlz WITH KEY bukrs = gs_anla-bukrs
                                                anln1 = gs_anla-anln1 BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_assets_out-kostl = gs_anlz-kostl .
        READ TABLE  gt_cskt INTO gs_cskt WITH KEY kostl = gs_anlz-kostl BINARY SEARCH.
        IF sy-subrc EQ 0 .
          t_mes_assets_out-ktext = gs_cskt-ktext.

        ENDIF.
      ENDIF.
      "主资产号
      READ TABLE gt_anlh INTO gs_anlh WITH KEY bukrs = gs_anla-bukrs
                                               anln1 = gs_anla-anln1 BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_assets_out-anlhtxt = gs_anlh-anlhtxt.

      ENDIF.
      "资产分类描述
      READ TABLE gt_ankt INTO gs_ankt WITH KEY anlkl = t_mes_assets_out-anlkl BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_assets_out-txk50 = gs_ankt-txk50.
      ENDIF.
      APPEND t_mes_assets_out .
    ENDLOOP.
    SORT t_mes_assets_out BY  anln1.
  ENDIF.
  IF t_mes_assets_out[] IS NOT INITIAL.
    r_msg = '数据读取成功,请查看T_MES_ASSETS_OUT表'.
    DESCRIBE TABLE t_mes_assets_out[]  LINES t_lines .
    gs_zzacknow-zzprofg = 'S'.
    MOVE t_lines TO gs_zzacknow-zzcomen.
    CONDENSE gs_zzacknow-zzcomen NO-GAPS.
    CONCATENATE p_dyxtm '系统已成功读取' gs_zzacknow-zzcomen '条记录' INTO gs_zzacknow-zzcomen .
  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'ASSETS'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_FIXEDASSETS_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.


ENDFUNCTION.
