FUNCTION zmes_sap_kunnr_sel.
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
*"      T_MES_KUNNR_IN STRUCTURE  ZMES_KUNNR_SEL
*"      T_MES_KUNNR_OUT STRUCTURE  ZMES_KUNNR
*"----------------------------------------------------------------------
  DATA:gt_t077x TYPE TABLE OF t077x,
       gs_t077x TYPE t077x.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  FIELD-SYMBOLS:<fs_data> TYPE zmes_kunnr .
  CLEAR:t_mes_kunnr_out,t_mes_kunnr_out[].
  IF t_mes_kunnr_in[] IS NOT INITIAL.
    SELECT a~kunnr a~bukrs a~erdat a~loevm
   b~name1 b~name2  b~ktokd
   INTO CORRESPONDING FIELDS OF TABLE t_mes_kunnr_out
   FROM knb1 AS a
   INNER JOIN kna1 AS b
   ON a~kunnr = b~kunnr
   FOR ALL ENTRIES IN t_mes_kunnr_in
WHERE a~kunnr = t_mes_kunnr_in-kunnr
   AND  a~bukrs = p_bukrs
   AND a~loevm NE 'X'
   AND a~erdat BETWEEN p_cj_l AND p_cj_h
   AND b~ktokd IN ('Z001','Z002','Z005').
    SORT t_mes_kunnr_out BY kunnr .

  ELSE.
    SELECT a~kunnr a~bukrs a~erdat a~loevm
      b~name1 b~name2  b~ktokd
 INTO CORRESPONDING FIELDS OF TABLE t_mes_kunnr_out
 FROM knb1 AS a
 INNER JOIN kna1 AS b
 ON a~kunnr = b~kunnr
 WHERE a~bukrs = p_bukrs
 AND a~loevm NE 'X'
 AND a~erdat BETWEEN p_cj_l AND p_cj_h
      AND b~ktokd IN ('Z001','Z002','Z005').

    SORT t_mes_kunnr_out BY kunnr .

  ENDIF..


  IF t_mes_kunnr_out[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_t077x
      FROM t077x
      WHERE spras = p_spras .
    SORT gt_t077x BY ktokd.

    LOOP AT t_mes_kunnr_out ASSIGNING <fs_data>.
      "查询账户组
      READ TABLE gt_t077x INTO gs_t077x WITH KEY ktokd = <fs_data>-ktokd BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-txt30 = gs_t077x-txt30 .
      ENDIF.

    ENDLOOP.

    r_msg = '数据读取成功,请查看T_MES_KUNNR_OUT表'.

    DESCRIBE TABLE t_mes_kunnr_out[]  LINES t_lines .
  ELSE.
    r_msg = '据读取成功,请查看T_MES_KUNNR_OUT表'.
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'KUNNR'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_KUNNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.
ENDFUNCTION.
