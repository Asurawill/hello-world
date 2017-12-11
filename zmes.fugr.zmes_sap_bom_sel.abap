FUNCTION zmes_sap_bom_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D
*"     VALUE(P_CJ_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CJ_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_BOM_OUT STRUCTURE  ZMES_BOM
*"      T_MATNR_IN STRUCTURE  ZMES_MATNR_SEL
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_bom_header ,
          matnr TYPE matnr,
          werks TYPE werks,
          stlan TYPE stlan,
          stlnr TYPE stnum,
          stlal TYPE stlal,
          stlty TYPE stlty,
          stkoz TYPE cim_count,
          andat TYPE andat,
          annam TYPE annam,
          datuv TYPE datuv,
          bmein TYPE basme,
          bmeng TYPE basmn,
          stlst TYPE stlst,
          lkenz TYPE lkenz,
        END OF ty_bom_header .

  TYPES:BEGIN OF ty_bom_item,
          stlty TYPE stlty,
          stlnr TYPE  stnum,
          stlal TYPE stlal,
          stlkn TYPE stlkn,
          stasz TYPE  cim_count,
          stpoz TYPE  cim_count,
          datuv TYPE datuv,
          lkenz TYPE lkenz,
          idnrk TYPE idnrk,
          postp TYPE postp,
          posnr TYPE sposn,
          meins TYPE kmpme,
          menge TYPE kmpmg,
        END OF ty_bom_item.


  DATA:gt_header TYPE TABLE OF ty_bom_header,
       gs_header TYPE ty_bom_header.

  DATA:gt_item TYPE TABLE OF ty_bom_item,
       gs_item TYPE ty_bom_item.

  DATA:gt_makt TYPE TABLE OF makt,
       gs_makt TYPE makt.

  DATA:gt_t416t TYPE TABLE OF t416t,
       gs_t416t TYPE t416t.

  DATA:gt_t418t TYPE TABLE OF t418t,
       gs_t418t TYPE t418t.

  DATA:gt_t415t TYPE TABLE OF t415t,
       gs_t415t TYPE t415t.

  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  DATA:t_lines TYPE i.

  IF t_matnr_in[] IS NOT INITIAL.
    "读取查询某个工厂、创建BOM期间的未删除 的 BOM清单抬头
    SELECT a~matnr a~werks a~stlan a~stlnr a~stlal a~andat a~annam
           b~stlty b~stkoz b~datuv b~bmein b~bmeng b~stlst b~lkenz
      INTO CORRESPONDING FIELDS OF TABLE gt_header
      FROM mast AS a
      INNER JOIN stko AS b
      ON a~stlnr = b~stlnr
      AND a~stlal = b~stlal
      FOR ALL ENTRIES IN t_matnr_in
      WHERE a~werks EQ p_werks
      AND  a~matnr EQ t_matnr_in-matnr
      AND   a~andat BETWEEN p_cj_l AND p_cj_h
      AND   b~lkenz NE 'X'.
    SORT  gt_header  BY  stlan  stlnr stlal .



  ELSE.
    "读取查询某个工厂、创建BOM期间的未删除 的 BOM清单抬头
    SELECT a~matnr a~werks a~stlan a~stlnr a~stlal a~andat a~annam
           b~stlty b~stkoz b~datuv b~bmein b~bmeng b~stlst b~lkenz
      INTO CORRESPONDING FIELDS OF TABLE gt_header
      FROM mast AS a
      INNER JOIN stko AS b
      ON a~stlnr = b~stlnr
      AND a~stlal = b~stlal
      WHERE a~werks EQ p_werks
      AND   a~andat BETWEEN p_cj_l AND p_cj_h
      AND   b~lkenz NE 'X'.

    SORT  gt_header  BY  stlan  stlnr stlal .

  ENDIF.

  IF gt_header[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_makt
  FROM makt
  WHERE spras = p_spras .
    SORT gt_makt BY matnr .

    "查询项目类别文本
    SELECT * INTO TABLE gt_t418t
      FROM t418t
      WHERE spras = p_spras .
    SORT gt_t418t BY postp.

    "查询BOM使用文本
    SELECT * INTO TABLE gt_t416t
      FROM t416t
      WHERE spras = p_spras.
    SORT gt_t416t BY stlan .

    "查询BOM状态文本
    SELECT * INTO TABLE gt_t415t
      FROM t415t
      WHERE spras = p_spras.
    SORT gt_t415t BY stlst .

    SELECT a~stlty a~stlnr a~stlal a~stlkn a~stasz a~lkenz
           b~stpoz b~idnrk b~postp b~posnr b~meins b~menge b~datuv b~idnrk
       INTO CORRESPONDING FIELDS OF TABLE gt_item
       FROM stas AS a
       INNER JOIN  stpo AS b
       ON a~stlty = b~stlty
        AND a~stlnr = b~stlnr
        AND a~stlkn = b~stlkn
        FOR ALL ENTRIES IN gt_header
        WHERE  a~stlnr = gt_header-stlnr
        AND   a~stlal = gt_header-stlal
        AND   a~lkenz NE 'X'.

    SORT gt_item BY stlty stlnr stlal  stlkn .

  ENDIF.
  IF gt_item IS NOT INITIAL.
    LOOP AT gt_item INTO gs_item .
      CLEAR:t_matnr_in .
      MOVE-CORRESPONDING gs_item TO t_mes_bom_out .
      "查询项目类别文本
      READ TABLE gt_t418t INTO gs_t418t WITH KEY postp = gs_item-postp BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_bom_out-ptext = gs_t418t-ptext .
      ENDIF.
      "查询组件名称
      READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_item-idnrk BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_bom_out-idnrk_txt = gs_makt-maktx.
      ENDIF.
      READ TABLE gt_header INTO gs_header WITH KEY stlnr = gs_item-stlnr stlal = gs_item-stlal.
      IF sy-subrc EQ 0 .
        t_mes_bom_out-werks = gs_header-werks.
        t_mes_bom_out-matnr = gs_header-matnr.
        "查询主件名称
        READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_header-matnr BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_mes_bom_out-matnr_txt = gs_makt-maktx.
        ENDIF.
        t_mes_bom_out-stlan = gs_header-stlan.  "BOM用途
        READ TABLE gt_t416t INTO gs_t416t WITH KEY stlan = gs_header-stlan BINARY SEARCH.
        IF sy-subrc EQ 0 .
          t_mes_bom_out-antxt = gs_t416t-antxt.
        ENDIF.
        t_mes_bom_out-stlst = gs_header-stlst.  "BOM状态
        READ TABLE gt_t415t INTO gs_t415t WITH KEY stlst = gs_header-stlst BINARY SEARCH.  "BOM状态
        IF sy-subrc EQ 0 .
          t_mes_bom_out-sttxt = gs_t415t-sttxt.
        ENDIF.
        t_mes_bom_out-bmein = gs_header-bmein. "单位
        t_mes_bom_out-bmeng = gs_header-bmeng.  "数量
        t_mes_bom_out-andat = gs_header-andat.
      ENDIF.


      APPEND t_mes_bom_out.
    ENDLOOP.
    SORT t_mes_bom_out BY werks matnr stlan stlal posnr .
    r_msg = '数据据读取成功,请查看T_MES_BOM_OUT表'.
    gs_zzacknow-zzprofg = 'S'.
    DESCRIBE TABLE t_mes_bom_out[]  LINES t_lines .
    MOVE t_lines TO gs_zzacknow-zzcomen.
    CONDENSE gs_zzacknow-zzcomen NO-GAPS.
    CONCATENATE p_dyxtm '系统已成功读取' gs_zzacknow-zzcomen '条记录' INTO gs_zzacknow-zzcomen .
  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.
  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'BOM'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_werks.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_BOM_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.

ENDFUNCTION.
