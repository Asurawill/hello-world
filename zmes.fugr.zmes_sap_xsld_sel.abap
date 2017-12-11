FUNCTION zmes_sap_xsld_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  VKBUK
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"     VALUE(P_VBELN) TYPE  VBELN_VA OPTIONAL
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_XSLD_XX STRUCTURE  ZXSXX
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_wlz,
          matkl TYPE vbap-matkl, "物料组
          wgbez TYPE t023t-wgbez, "物料组描述
        END OF ty_wlz.

  TYPES:BEGIN OF ty_vbeln ,
          vbeln TYPE vbak-vbeln,
          xmmc  TYPE c LENGTH 100,
        END OF ty_vbeln .


  DATA:gt_wlz TYPE TABLE OF  ty_wlz,
       gs_wlz TYPE ty_wlz.


  DATA:gt_vbak TYPE TABLE OF vbak,
       gs_vbak TYPE vbak.

  DATA:gt_vbap TYPE TABLE OF vbap,
       gs_vbap TYPE vbap.

  DATA:gt_tvko TYPE TABLE OF tvko,
       gs_tvko TYPE tvko.

  RANGES:r_vkorg FOR vbak-vkorg .

  DATA:gt_t023t TYPE TABLE OF t023t,
       gs_t023t TYPE t023t.

  DATA:gt_tvagt TYPE TABLE OF tvagt,
       gs_tvagt TYPE tvagt.

  DATA: g_objname TYPE thead-tdname.

  DATA: it_lines TYPE TABLE OF tline,
        wa_lines TYPE tline.

  DATA:gs_xsld_xx TYPE zxsxx .

  DATA:gt_vbeln TYPE TABLE OF ty_vbeln,
       gs_vbeln TYPE ty_vbeln.

  "*记录日志数据
  DATA:len1 TYPE string.
  DATA:t_lines    TYPE i.
  DATA: gt_zzacknow TYPE TABLE OF zzacknow,
        gs_zzacknow TYPE zzacknow.

  REFRESH:gt_zzacknow.
  CLEAR:gs_zzacknow.

  "查询输入销售公司代码下的销售组织
  REFRESH:gt_tvko.
  SELECT * INTO TABLE gt_tvko
    FROM tvko
    WHERE  bukrs EQ p_bukrs .
  SORT gt_tvko BY vkorg .

  "查询拒绝原因文本
  REFRESH:gt_tvagt.
  SELECT * INTO TABLE gt_tvagt
    FROM tvagt
    WHERE spras = '1'.
  SORT gt_tvagt BY abgru.

  REFRESH:r_vkorg .
  REFRESH:r_vkorg.
  LOOP AT gt_tvko INTO gs_tvko.
    CLEAR:r_vkorg.
    r_vkorg-sign = 'I'.
    r_vkorg-option = 'EQ'.
    r_vkorg-low = gs_tvko-vkorg.
    APPEND r_vkorg .
  ENDLOOP.

  REFRESH:gt_vbak.
  IF p_vbeln NE ''.
    REFRESH:gt_vbak.
    SELECT * INTO  TABLE gt_vbak
   FROM vbak
   WHERE vkorg IN r_vkorg
   AND vbeln = p_vbeln .

  ELSE.
    REFRESH:gt_vbak.
    SELECT * INTO  TABLE gt_vbak
  FROM vbak
  WHERE vkorg IN r_vkorg
  .

  ENDIF.


  IF gt_vbak IS NOT INITIAL.
    REFRESH:gt_vbap.
    SELECT * INTO TABLE gt_vbap
      FROM vbap
      FOR ALL ENTRIES IN gt_vbak
      WHERE vbeln = gt_vbak-vbeln.
    SORT gt_vbap BY vbeln posnr.

    REFRESH:gt_wlz.
    MOVE-CORRESPONDING gt_vbap TO gt_wlz.
    SORT gt_wlz BY matkl.
    DELETE gt_wlz WHERE matkl IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM gt_wlz COMPARING matkl .

    REFRESH:gt_t023t.
    SELECT * INTO TABLE gt_t023t
      FROM t023t
      FOR ALL ENTRIES IN gt_wlz
      WHERE matkl =  gt_wlz-matkl
      AND spras = '1' .
    SORT gt_t023t BY matkl.

    REFRESH:gt_vbeln.
    MOVE-CORRESPONDING gt_vbap TO gt_vbeln .
    LOOP AT gt_vbeln INTO gs_vbeln .
      CLEAR:g_objname .
      REFRESH:it_lines .
      g_objname = gs_vbeln-vbeln .
      REFRESH:it_lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'Z001'
          language                = '1'
          name                    = g_objname
          object                  = 'VBBK'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
* IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = it_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        LOOP AT it_lines INTO wa_lines .
          IF gs_vbeln-xmmc EQ ''.
            gs_vbeln-xmmc = wa_lines-tdline .
          ELSE.
            CONCATENATE wa_lines-tdline gs_vbeln-xmmc INTO gs_vbeln-xmmc .
          ENDIF.
        ENDLOOP.

      ENDIF.

      MODIFY gt_vbeln FROM gs_vbeln .
    ENDLOOP.


  ENDIF.
  REFRESH:t_xsld_xx..
  IF gt_vbap IS NOT INITIAL.
    LOOP AT gt_vbap INTO gs_vbap .
      CLEAR:gs_xsld_xx.
      gs_xsld_xx-vbeln = gs_vbap-vbeln. "销售订单
      gs_xsld_xx-posnr = gs_vbap-posnr. "销售凭证项目
      gs_xsld_xx-matnr = gs_vbap-matnr. "物料号
      gs_xsld_xx-arktx = gs_vbap-arktx. "短文本
      gs_xsld_xx-matkl = gs_vbap-matkl.   "物料组
      gs_xsld_xx-kwmeng = gs_vbap-kwmeng. "以销售单位表示的累计数量
      gs_xsld_xx-vrkme = gs_vbap-vrkme.  "销售单位
      gs_xsld_xx-werks = gs_vbap-werks.   "工厂

      READ TABLE gt_t023t INTO gs_t023t WITH KEY matkl = gs_vbap-matkl
                                     BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_xsld_xx-wgbez = gs_t023t-wgbez.
      ENDIF.
      READ TABLE gt_vbeln INTO gs_vbeln WITH KEY vbeln  = gs_xsld_xx-vbeln
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_xsld_xx-xmmc = gs_vbeln-xmmc .
      ENDIF.

      "拒绝原因
      gs_xsld_xx-abgru = gs_vbap-abgru.

      IF gs_xsld_xx-abgru IS NOT INITIAL.
        READ TABLE gt_tvagt INTO gs_tvagt WITH KEY abgru = gs_xsld_xx-abgru  .
        IF sy-subrc EQ 0 .

          gs_xsld_xx-bezei = gs_tvagt-bezei .
        ENDIF.
      ENDIF.
      APPEND gs_xsld_xx TO t_xsld_xx.

    ENDLOOP.
    r_msg = '数据据读取成功,请查看T_GSJCG表'.
    CLEAR:gs_zzacknow.
    DESCRIBE TABLE t_xsld_xx[]  LINES t_lines .
    gs_zzacknow-zzprofg = 'S'.
    MOVE t_lines TO len1.
    CONCATENATE p_dyxtm '系统已成功读取销售订单信息' len1 '条主记录' INTO gs_zzacknow-zzcomen .



  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取销售订单信息失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'XSLD'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_bukrs.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_XSLD_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.

ENDFUNCTION.
