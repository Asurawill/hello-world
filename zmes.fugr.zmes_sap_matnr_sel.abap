FUNCTION zmes_sap_matnr_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D
*"     VALUE(P_CJ_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CJ_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_XG_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_XG_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_MATNR_IN STRUCTURE  ZMES_MATNR_SEL
*"      T_MES_MATNR_OUT STRUCTURE  ZMES_MATNR OPTIONAL
*"----------------------------------------------------------------------
  REFRESH :t_mes_matnr_out[] .
  DATA:gt_makt TYPE TABLE OF makt,
       gs_makt TYPE makt.

  DATA:gt_t134t TYPE TABLE OF t134t,
       gs_t134t TYPE t134t.

  DATA:gt_t023t TYPE TABLE OF t023t,
       gs_t023t TYPE t023t.

  DATA:gt_char TYPE TABLE OF bapi1003_alloc_values_char,
       gs_char TYPE bapi1003_alloc_values_char,
       gt_num  TYPE TABLE OF  bapi1003_alloc_values_num,
       gs_num  TYPE bapi1003_alloc_values_num,
       gt_curr TYPE TABLE OF   bapi1003_alloc_values_curr,
       gs_curr TYPE  bapi1003_alloc_values_curr,
       gt_ret  TYPE TABLE OF bapiret2,
       gs_ret  TYPE bapiret2.

  DATA:l_object LIKE bapi1003_key-object.

  DATA:t_lines TYPE i.

  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.


  REFRESH: gt_makt ,gt_t023t,gt_t134t .
  CLEAR:gs_makt,gs_t134t,gs_t023t.
  DATA:P_LEN TYPE i.
  FIELD-SYMBOLS <fs_data> TYPE zmes_matnr .

  IF  t_mes_matnr_in[] IS NOT INITIAL.
    loop at t_mes_matnr_in .
      p_len = STRLEN( t_mes_matnr_in-matnr ).
       if p_len < 14.
           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input         = t_mes_matnr_in-matnr
           IMPORTING
             OUTPUT        = t_mes_matnr_in-matnr
                     .
           modify t_mes_matnr_in.
       endif.

     endloop.
    IF  p_cj_l IS NOT INITIAL OR   p_cj_h IS  INITIAL.
      SELECT a~matnr a~werks b~meins b~mtart
          b~matkl  b~ersda b~laeda b~lvorm
     INTO CORRESPONDING FIELDS OF  TABLE  t_mes_matnr_out
     FROM  marc  AS a
     INNER JOIN mara AS b
     ON a~matnr = b~matnr
     FOR ALL ENTRIES IN t_mes_matnr_in
     WHERE
     a~matnr EQ t_mes_matnr_in-matnr
     AND a~werks EQ p_werks
     AND a~lvorm NE 'X'
     AND  b~mtart IN ('ZFRT','ZHLB','ZKGL','ZROH','ZWJJ')
     AND  b~ersda BETWEEN p_cj_l AND p_cj_h .
      "  AND  b~laeda BETWEEN p_xg_l AND p_xg_h .

      SORT t_mes_matnr_out BY matnr .
    ELSE.
      SELECT a~matnr a~werks b~meins b~mtart
            b~matkl  b~ersda b~laeda b~lvorm
       INTO CORRESPONDING FIELDS OF  TABLE  t_mes_matnr_out
       FROM  marc  AS a
       INNER JOIN mara AS b
       ON a~matnr = b~matnr
       FOR ALL ENTRIES IN t_mes_matnr_in
       WHERE
       a~matnr EQ t_mes_matnr_in-matnr
       AND a~werks EQ p_werks
       AND  b~mtart IN ('ZFRT','ZHLB','ZKGL','ZROH','ZWJJ')
     "  AND  b~ersda BETWEEN p_cj_l AND p_cj_h .
      AND  b~laeda BETWEEN p_xg_l AND p_xg_h .

      SORT t_mes_matnr_out BY matnr .
    ENDIF.


  ELSE.
    IF  p_cj_l IS NOT INITIAL OR   p_cj_h IS  INITIAL.
      SELECT a~matnr a~werks b~meins b~mtart
             b~matkl  b~ersda b~laeda b~lvorm
       INTO CORRESPONDING FIELDS OF  TABLE  t_mes_matnr_out
       FROM  marc  AS a
       INNER JOIN mara AS b
       ON a~matnr = b~matnr
       WHERE
        a~werks EQ p_werks
       AND a~lvorm NE 'X'
       AND  b~mtart IN ('ZFRT','ZHLB','ZKGL','ZROH','ZWJJ')
       AND  b~ersda BETWEEN p_cj_l AND p_cj_h .
      "AND  b~laeda BETWEEN p_xg_l AND p_xg_h .

      SORT t_mes_matnr_out BY matnr .
    ELSE.
      SELECT a~matnr a~werks b~meins b~mtart
             b~matkl  b~ersda b~laeda b~lvorm
      INTO CORRESPONDING FIELDS OF  TABLE  t_mes_matnr_out
      FROM  marc  AS a
      INNER JOIN mara AS b
      ON a~matnr = b~matnr
      WHERE
      a~werks EQ p_werks
      AND  b~mtart IN ('ZFRT','ZHLB','ZKGL','ZROH','ZWJJ')
" AND  b~ersda BETWEEN p_cj_l AND p_cj_h .
      AND  b~laeda BETWEEN p_xg_l AND p_xg_h .

      SORT t_mes_matnr_out BY matnr .
    ENDIF.

  ENDIF.

  IF t_mes_matnr_out[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN t_mes_matnr_out
      WHERE    matnr = t_mes_matnr_out-matnr
       AND ( spras eq p_spras or spras eq 'E' ).
    SORT  gt_makt BY matnr spras.
    SELECT * INTO TABLE gt_t134t
      FROM t134t
      WHERE spras = p_spras.
    SORT gt_t134t BY mtart .
    SELECT * INTO TABLE gt_t023t
      FROM  t023t
      WHERE spras = '1'.
    SORT gt_t023t BY matkl .
    LOOP AT t_mes_matnr_out ASSIGNING <fs_data> .
      "查询物料描述
      READ TABLE gt_makt INTO gs_makt WITH KEY matnr = <fs_data>-matnr spras = p_spras BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-maktx = gs_makt-maktx .

      ENDIF.
        READ TABLE gt_makt INTO gs_makt WITH KEY matnr = <fs_data>-matnr spras = 'E' BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-maktx_en = gs_makt-maktx .

      ENDIF.
      "查询英文物料描述

      "查询物料类型

      READ TABLE gt_t134t INTO gs_t134t WITH KEY mtart = <fs_data>-mtart BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-mtbez = gs_t134t-mtbez.

      ENDIF.

      "查询物料组

      READ TABLE gt_t023t INTO gs_t023t WITH KEY matkl = <fs_data>-matkl BINARY SEARCH .
      IF sy-subrc EQ 0 .

        <fs_data>-wgbez = gs_t023t-wgbez.

      ENDIF.

      "查询特征值
      l_object = <fs_data>-matnr .
      REFRESH:gt_char.
      CLEAR:gs_char.

      "获取特性
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey       = l_object
          objecttable     = 'MARA'
          classnum        = 'LYD001'
          classtype       = '001'
        TABLES
          allocvaluesnum  = gt_num
          allocvalueschar = gt_char
          allocvaluescurr = gt_curr
          return          = gt_ret.
      LOOP AT gt_char INTO gs_char .
        CASE gs_char-charact .
          WHEN 'ROHS'.
            <fs_data>-rohs = gs_char-value_char.
          WHEN 'GGXH'.
            <fs_data>-ggxh = gs_char-value_char.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  "  r_msg = '数据据读取成功,请查看T_MES_MATNR_OUT表'.
    CLEAR:gs_zzacknow.
    DESCRIBE TABLE t_mes_matnr_out[]  LINES t_lines .

    r_msg = '数据据读取成功,请查看T_MES_MATNR_OUT表'.
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
  gs_zzacknow-zzintty  = 'MATNR'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_werks.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_MATNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.

ENDFUNCTION.
