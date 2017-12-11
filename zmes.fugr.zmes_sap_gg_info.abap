FUNCTION zmes_sap_gg_info.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_OBJECTCLAS) TYPE  CDOBJECTCL
*"     VALUE(P_GG_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_GG_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_GG_INFO_IN STRUCTURE  ZMES_GG_OBJECTID
*"      T_MES_GG_INFO_OUT STRUCTURE  ZMES_GG_INFO
*"----------------------------------------------------------------------
  DATA:gt_cdhdr TYPE TABLE OF cdhdr,
       gs_cdhdr TYPE cdhdr.

  DATA:gt_cdpos TYPE TABLE OF cdpos,
       gs_cdpos TYPE cdpos.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  CONDENSE p_objectclas NO-GAPS.

  TRANSLATE p_objectclas TO UPPER CASE .
  CLEAR:t_mes_gg_info_out,t_mes_gg_info_out[].
  IF p_objectclas EQ 'KRED'.  "KRED 代表供应商更改名称
    CLEAR:gt_cdhdr[],gt_cdhdr .
    IF t_mes_gg_info_in IS NOT INITIAL.
      SELECT * INTO TABLE gt_cdhdr
  FROM cdhdr
  FOR ALL ENTRIES IN t_mes_gg_info_in
  WHERE objectid = t_mes_gg_info_in-objectid
  AND objectclas =  'KRED'
  AND   udate >= p_gg_l
  AND   udate <= p_gg_h
  AND   change_ind = 'U'
   .
      SORT gt_cdhdr BY objectclas objectid changenr udate.

    ELSE.
      SELECT * INTO TABLE gt_cdhdr
  FROM cdhdr
  WHERE objectclas =  'KRED'
  AND   udate >= p_gg_l
  AND   udate <= p_gg_h
  AND   change_ind = 'U'
   .
      SORT gt_cdhdr BY objectclas objectid changenr udate.

    ENDIF.



    IF gt_cdhdr IS NOT INITIAL.
      CLEAR:gt_cdpos[] ,gt_cdpos .
      SELECT * INTO TABLE gt_cdpos
        FROM cdpos
        FOR ALL ENTRIES IN gt_cdhdr
        WHERE objectclas = gt_cdhdr-objectclas
        AND objectid = gt_cdhdr-objectid
        AND changenr = gt_cdhdr-changenr
        AND tabname IN ('LFA1','LFB1')
        AND fname IN ('NAME1','NAME2','NAME3','NAME4','LOEVM')
        AND chngind EQ 'U'.

      SORT  gt_cdpos BY objectclas ASCENDING objectid ASCENDING fname ASCENDING   changenr DESCENDING  .
      DELETE ADJACENT DUPLICATES FROM gt_cdpos COMPARING objectclas objectid  fname .

    ENDIF.
  ELSEIF p_objectclas EQ 'DEBI'.  "代表客户更改名称
    CLEAR:gt_cdhdr[],gt_cdhdr .
    IF t_mes_gg_info_in IS NOT INITIAL.
      SELECT * INTO TABLE gt_cdhdr
  FROM cdhdr
  FOR ALL ENTRIES IN  t_mes_gg_info_in
  WHERE objectid = t_mes_gg_info_in-objectid
  AND objectclas =  'DEBI'
  AND   udate >= p_gg_l
  AND   udate <= p_gg_h
  AND   change_ind = 'U'
   .
      SORT gt_cdhdr BY objectclas objectid changenr udate.

    ELSE.
      SELECT * INTO TABLE gt_cdhdr
FROM cdhdr
WHERE objectclas =  'DEBI'
AND   udate >= p_gg_l
AND   udate <= p_gg_h
AND   change_ind = 'U'.

      SORT gt_cdhdr BY objectclas objectid changenr udate.

    ENDIF.

    IF gt_cdhdr IS NOT INITIAL.
      CLEAR:gt_cdpos[] ,gt_cdpos .
      SELECT * INTO TABLE gt_cdpos
        FROM cdpos
        FOR ALL ENTRIES IN gt_cdhdr
        WHERE objectclas = gt_cdhdr-objectclas
        AND objectid = gt_cdhdr-objectid
        AND changenr = gt_cdhdr-changenr
        AND tabname IN ('KNA1','KNB1')
        AND fname IN ('NAME1','NAME2','LOEVM')
        AND chngind EQ 'U'.

      SORT  gt_cdpos BY objectclas ASCENDING objectid ASCENDING fname ASCENDING   changenr DESCENDING  .
      DELETE ADJACENT DUPLICATES FROM gt_cdpos COMPARING objectclas objectid  fname .


    ENDIF.
  ENDIF.
  IF gt_cdpos IS NOT INITIAL.
    LOOP AT gt_cdpos INTO gs_cdpos .
      CLEAR:t_mes_gg_info_out .
      t_mes_gg_info_out-objectid = gs_cdpos-objectid .
      t_mes_gg_info_out-fname = gs_cdpos-fname.
      t_mes_gg_info_out-value_new = gs_cdpos-value_new.
      READ TABLE gt_cdhdr INTO gs_cdhdr WITH KEY objectclas = gs_cdpos-objectclas objectid = gs_cdpos-objectid
                      changenr = gs_cdpos-changenr  BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_gg_info_out-udate = gs_cdhdr-udate  .   "更改日期
      ENDIF.

      APPEND t_mes_gg_info_out .


    ENDLOOP.
    SORT  t_mes_gg_info_out BY objectid fname .
  ENDIF.
  IF  t_mes_gg_info_out[] IS NOT INITIAL.

    r_msg = '数据读取成功,请查看T_MES_GG_INFO_OUT表'.
    DESCRIBE TABLE t_mes_gg_info_out[]  LINES t_lines .
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
  gs_zzacknow-zzintty  = 'GG'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_GG_INFO' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.

ENDFUNCTION.
