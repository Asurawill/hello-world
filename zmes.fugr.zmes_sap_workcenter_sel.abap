FUNCTION zmes_sap_workcenter_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D
*"     VALUE(P_GG_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_GG_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_WORKCENTER_IN STRUCTURE  ZMES_WORKCENTER_SEL
*"      T_WORKCENTER_OUT STRUCTURE  ZMES_WORKCENTER
*"----------------------------------------------------------------------
  CLEAR:t_workcenter_out,t_workcenter_out[].
  TYPES:BEGIN OF ty_out,
          objty      TYPE cr_objty,
          objid      TYPE cr_objid,
          arbpl      TYPE arbpl,
          aedat_text TYPE aedtm,
          ktext      TYPE cr_ktext,
        END OF ty_out .
  DATA:gt_data TYPE TABLE OF ty_out,
       gs_data TYPE ty_out.
  CLEAR:gt_data[], gt_data,gs_data.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  IF  t_workcenter_in[] IS NOT INITIAL.
    SELECT a~objty a~objid a~arbpl
           b~aedat_text b~ktext
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM crhd AS a
      INNER JOIN crtx AS b
      ON a~objty = b~objty
      AND a~objid = b~objid
      FOR ALL ENTRIES IN  t_workcenter_in
      WHERE a~werks = p_werks
      AND a~objty = 'A'
      AND a~arbpl = t_workcenter_in-arbpl
      AND b~spras = p_spras
      AND b~aedat_text BETWEEN p_gg_l AND p_gg_h.
    SORT gt_data BY arbpl .

  ELSE.
    SELECT a~objty a~objid a~arbpl
              b~aedat_text b~ktext
         INTO CORRESPONDING FIELDS OF TABLE gt_data
         FROM crhd AS a
         INNER JOIN crtx AS b
         ON a~objty = b~objty
         AND a~objid = b~objid
         WHERE a~werks = p_werks
         AND a~objty = 'A'
         AND b~spras = p_spras
         AND b~aedat_text BETWEEN p_gg_l AND p_gg_h.
    SORT gt_data BY arbpl .


  ENDIF.
  IF gt_data[] IS NOT INITIAL.
    LOOP AT gt_data INTO gs_data .
      MOVE-CORRESPONDING gs_data TO t_workcenter_out .
      APPEND t_workcenter_out  .
    ENDLOOP.
    SORT t_workcenter_out BY arbpl .
    r_msg = '数据读取成功,请查看T_WORKCENTER_OUT表'.
    gs_zzacknow-zzprofg = 'S'.
    DESCRIBE TABLE t_workcenter_out[]  LINES t_lines .
    MOVE t_lines TO gs_zzacknow-zzcomen.
    CONDENSE gs_zzacknow-zzcomen NO-GAPS.
    CONCATENATE p_dyxtm '系统已成功读取' gs_zzacknow-zzcomen '条记录' INTO gs_zzacknow-zzcomen .
  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'WORKCENTER'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_werks.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_WORKCENTER_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.


ENDFUNCTION.
