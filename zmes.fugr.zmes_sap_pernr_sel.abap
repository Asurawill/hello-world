FUNCTION zmes_sap_pernr_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_GJ) TYPE  DATS DEFAULT SY-DATUM
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_PERNR_OUT STRUCTURE  ZMES_PERNR_SEL
*"----------------------------------------------------------------------
  TYPES:BEGIN  OF ty_pernr,
          pernr TYPE persno,
          begda TYPE begda,
          endda TYPE endda,
          bukrs TYPE bukrs,
          ename TYPE ename,
          orgeh TYPE orgeh,
          plans TYPE plans,

        END OF ty_pernr.

  DATA:gt_pernr TYPE TABLE OF ty_pernr,
       gs_pernr TYPE ty_pernr.

  DATA:gt_pa0001 TYPE TABLE OF pa0001,
       gs_pa0001 TYPE pa0001.

  DATA:gt_pa0000 TYPE TABLE OF pa0000,
       gs_pa0000 TYPE pa0000.

  DATA:gt_pa0002 TYPE TABLE OF pa0002,
       gs_pa0002 TYPE pa0002.

  DATA:gt_pa9105 TYPE TABLE OF pa9105,
       gs_pa9105 TYPE pa9105.

  DATA:gt_t528t TYPE TABLE OF t528t,
       gs_t528t TYPE t528t.

  DATA:t_lines TYPE i.


  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  CLEAR:t_mes_pernr_out[] ,t_mes_pernr_out ,
        gs_pernr,gs_pa0001,gs_pa0000,
        gs_pa0002, gs_pa9105 , gs_t528t .

  REFRESH:gt_pernr,gt_pa0001 ,gt_pa0000,gt_pa0002 ,gt_pa9105  .
*  DATA:gt_hrp1001 TYPE TABLE OF hrp1001,
*       gs_hrp1001 TYPE hrp1001.
*
*  DATA:gt_hrp1000 TYPE TABLE OF hrp1000,
*       gs_hrp1000 TYPE hrp1000.

  DATA:p_short  TYPE short_d.
  DATA:p_long  TYPE  stext .
  DATA:objid TYPE hrp1000-objid.

  DATA:  gt_stru_tab TYPE TABLE OF qcat_stru,
         gs_stru_tab TYPE qcat_stru.

  "根据公司代码取出关键日期有效数据
  SELECT a~pernr a~begda a~endda a~bukrs a~orgeh a~plans a~ename

     INTO CORRESPONDING FIELDS OF TABLE  gt_pernr
    FROM  pa0001  AS a
    INNER JOIN pa0000  AS b
    ON  a~pernr = b~pernr
    WHERE a~bukrs EQ p_bukrs
    AND   a~begda  <= p_gj
    AND a~endda >= p_gj
     AND   b~begda  <= p_gj
    AND b~endda >= p_gj
    AND b~massn NE 'Z6'.

  SORT  gt_pernr BY pernr .

  "取出0002信息类型人员数据
  IF gt_pernr IS NOT INITIAL .
    SELECT * INTO TABLE gt_pa0002
  FROM pa0002
  FOR ALL ENTRIES IN gt_pernr
  WHERE pernr = gt_pernr-pernr
   AND  begda  <= p_gj
  AND endda >= p_gj  .

    SORT gt_pa0002 BY pernr .

    "取出9105信息类型人员数据
    SELECT * INTO TABLE gt_pa9105
       FROM pa9105
       FOR ALL ENTRIES IN gt_pernr
      WHERE pernr = gt_pernr-pernr .

    SORT gt_pa9105 BY pernr .

    SELECT * INTO TABLE gt_t528t FROM t528t
    WHERE sprsl = p_spras AND otype = 'S' AND  endda = '99991231'.

    SORT gt_t528t BY plans .

    LOOP AT  gt_pernr INTO gs_pernr.

      CLEAR:t_mes_pernr_out.
      MOVE-CORRESPONDING gs_pernr TO t_mes_pernr_out.

      objid = gs_pernr-orgeh .
      "取本组织信息
      CALL FUNCTION 'HR_HCP_READ_OBJECT_TEXT'
        EXPORTING
          im_plvar = '01'
          im_otype = 'O'
*         IM_VIEW_OBJID       =
*         IM_VIEW_KOKRS       =
          im_objid = objid
      "   IM_ISTAT = ' '
          im_begda = p_gj
          im_endda = p_gj
        IMPORTING
          short    = p_short
          long     = p_long.
      IF p_short NE ''.
        CASE p_short  .

          WHEN 'B0'.
            t_mes_pernr_out-gstext = p_long.
          WHEN 'C1' .
            t_mes_pernr_out-c10rgen  =  p_long.
          WHEN 'C2'.
            t_mes_pernr_out-c20rgen  =  p_long.
          WHEN 'C3'.
            t_mes_pernr_out-c30rgen  =  p_long.
          WHEN 'C4'.
            t_mes_pernr_out-c40rgen  =  p_long.
        ENDCASE.

      ENDIF.


      " GS_STRU_TAB 存储当前组织及以上所有组织编码
      CALL FUNCTION 'RHPH_STRUCTURE_READ'
        EXPORTING
          plvar             = '01'
          otype             = 'O'
          objid             = objid
          wegid             = 'A002'
          begda             = gs_pernr-begda
          endda             = gs_pernr-endda
          pup_info          = 'X'
          with_stext        = 'X'
   "      TDEPTH            =
        TABLES
          stru_tab          = gt_stru_tab
        EXCEPTIONS
          catalogue_problem = 1
          root_not_found    = 2
          wegid_not_found   = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.

* IMPLEMENT SUITABLE ERROR HANDLING HERE
      ELSE.
        LOOP AT gt_stru_tab INTO gs_stru_tab.
          IF gs_stru_tab-short = 'C4' .
            t_mes_pernr_out-c40rgen  =  gs_stru_tab-stext.
          ENDIF.
          IF gs_stru_tab-short = 'C3'.
            t_mes_pernr_out-c30rgen  =  gs_stru_tab-stext.
          ENDIF.
          IF gs_stru_tab-short = 'C2'.
            t_mes_pernr_out-c20rgen  =  gs_stru_tab-stext.
          ENDIF.
          IF  gs_stru_tab-short = 'C1'.
            t_mes_pernr_out-c10rgen  =  gs_stru_tab-stext.
          ENDIF.
          IF  gs_stru_tab-short = 'B0'.
            t_mes_pernr_out-gstext  =  gs_stru_tab-stext.
          ENDIF.

        ENDLOOP.

      ENDIF.
      "职位
      READ TABLE gt_t528t INTO gs_t528t WITH KEY
                 plans = gs_pernr-plans  BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_pernr_out-plans = gs_t528t-plstx.
      ENDIF.


      READ TABLE gt_pa0002 INTO gs_pa0002 WITH KEY
                 pernr = gs_pernr-pernr BINARY SEARCH .
      IF sy-subrc EQ 0 .
        "性别
        IF gs_pa0002-gesch = '1'.
          gs_pa0002-gesch = '男'.
        ELSE.
          gs_pa0002-gesch = '女'.
        ENDIF.
      ENDIF.
      "曾用名
      t_mes_pernr_out-rufnm = gs_pa0002-rufnm ."曾用名
      "公司邮箱

      READ TABLE gt_pa9105  INTO gs_pa9105 WITH KEY pernr = gs_pernr-pernr BINARY SEARCH .
      IF sy-subrc EQ 0 .
        t_mes_pernr_out-yjdz = gs_pa9105-zzgsyx."公司邮箱
        t_mes_pernr_out-lxdh = gs_pa9105-zzgrsjhy. "手机号码一
      ENDIF.
      APPEND t_mes_pernr_out .
    ENDLOOP.
    SORT t_mes_pernr_out  BY gstext c10rgen c20rgen c30rgen c40rgen .
  ENDIF.

  CLEAR:gs_zzacknow.
  IF t_mes_pernr_out[] IS NOT INITIAL.
    DESCRIBE TABLE t_mes_pernr_out[]  LINES t_lines .

    r_msg = '数据据读取成功,请查看T_MES_PERNR_OUT表'.
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
  gs_zzacknow-zzintty  = 'PERNR'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_PERNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.



ENDFUNCTION.
