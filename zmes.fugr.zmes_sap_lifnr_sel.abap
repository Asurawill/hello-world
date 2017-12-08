FUNCTION zmes_sap_lifnr_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_CJ_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CJ_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_LIFNR_IN STRUCTURE  ZMES_LIFNR_SEL
*"      T_MES_LIFNR_OUT STRUCTURE  ZMES_LIFNR OPTIONAL
*"----------------------------------------------------------------------
  DATA:gt_t077y TYPE TABLE OF t077y,
       gs_t077y TYPE t077y.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  FIELD-SYMBOLS:<fs_data> TYPE zmes_lifnr .
  CLEAR:t_mes_lifnr_out ,t_mes_lifnr_out[].
  IF t_mes_lifnr_in IS NOT INITIAL.
    SELECT a~lifnr a~bukrs  a~erdat a~loevm
       b~name1 b~name2 b~name3 b~ktokk
    INTO CORRESPONDING FIELDS OF TABLE t_mes_lifnr_out
    FROM lfb1 AS a
    INNER JOIN  lfa1 AS b
    ON a~lifnr =  b~lifnr
    FOR ALL ENTRIES IN t_mes_lifnr_in
    WHERE a~lifnr EQ t_mes_lifnr_in-lifnr
     AND a~bukrs = p_bukrs
     AND a~loevm NE 'X'
     AND a~erdat BETWEEN p_cj_l AND p_cj_h
     AND b~ktokk IN ('Z001','Z002','Z003')
 .
    SORT t_mes_lifnr_out BY lifnr .

  ELSE.
    SELECT a~lifnr a~bukrs  a~erdat a~loevm
       b~name1 b~name2 b~name3 b~ktokk
    INTO CORRESPONDING FIELDS OF TABLE t_mes_lifnr_out
    FROM lfb1 AS a
    INNER JOIN  lfa1 AS b
    ON a~lifnr =  b~lifnr
    WHERE a~bukrs = p_bukrs
      AND a~loevm NE 'X'
     AND a~erdat BETWEEN p_cj_l AND p_cj_h
     AND b~ktokk IN ('Z001','Z002','Z003').
    SORT t_mes_lifnr_out BY lifnr .

  ENDIF.

  IF t_mes_lifnr_out[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_t077y
      FROM t077y
      WHERE spras = p_spras .
    SORT gt_t077y BY ktokk .
    LOOP AT  t_mes_lifnr_out ASSIGNING <fs_data>.
      "查询物料组
      READ TABLE gt_t077y INTO gs_t077y WITH KEY ktokk = <fs_data>-ktokk .
      IF sy-subrc EQ 0 .
        <fs_data>-txt30 = gs_t077y-txt30.
      ENDIF.

    ENDLOOP.

    r_msg = '数据读取成功,请查看T_MES_LIFNR_OUT表'.

    gs_zzacknow-zzprofg = 'S'.
    DESCRIBE TABLE t_mes_lifnr_out[]  LINES t_lines .
    MOVE t_lines TO gs_zzacknow-zzcomen.
    CONDENSE gs_zzacknow-zzcomen NO-GAPS.
    CONCATENATE p_dyxtm '系统已成功读取' gs_zzacknow-zzcomen '条记录' INTO gs_zzacknow-zzcomen .
  ELSE.

    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.
  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'LIFNR'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_LIFNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.








ENDFUNCTION.
