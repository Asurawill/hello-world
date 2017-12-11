FUNCTION zmes_sap_jh_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_VSTEL) TYPE  VSTEL
*"     VALUE(P_CJ_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CJ_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_JHDH STRUCTURE  ZJHDH
*"      T_JH STRUCTURE  ZMES_JHXX
*"      T_JH_MX STRUCTURE  ZMES_JHXX_MX
*"----------------------------------------------------------------------
  DATA:gt_likp TYPE TABLE OF likp,
       gs_likp TYPE likp.

  DATA:gt_lips TYPE TABLE OF lips,
       gs_lips TYPE lips.

  DATA:gt_vbak TYPE TABLE OF vbak,
       gs_vbak TYPE vbak.

  DATA:gt_ekkn TYPE TABLE OF ekkn,
       gs_ekkn TYPE ekkn.

  TYPES:BEGIN OF ty_vbeln ,
          vbeln TYPE vbak-vbeln,
          xmmc  TYPE c LENGTH 100,
        END OF ty_vbeln .

  TYPES:BEGIN OF ty_matnr,
          matnr    TYPE matnr,
          maktx_en TYPE maktx,
        END OF ty_matnr .

  DATA:gt_vbeln   TYPE TABLE OF ty_vbeln,
       gt_vbeln_1 TYPE TABLE OF ty_vbeln,
       gt_vbeln_2 TYPE TABLE OF ty_vbeln,
       gs_vbeln   TYPE ty_vbeln.

  DATA: g_objname TYPE thead-tdname.

  DATA: it_lines TYPE TABLE OF tline,
        wa_lines TYPE tline.

  DATA:gt_matnr TYPE TABLE OF ty_matnr,
       gs_matnr TYPE ty_matnr.

  DATA:gt_makt TYPE TABLE OF makt,
       gs_makt TYPE makt.

  DATA:t_lines    TYPE i,
       t_lines_mx TYPE i.


  DATA:len1 TYPE string,
       len2 TYPE string.

  REFRESH:t_jh,t_jh_mx.

  "*记录日志数据
  DATA: gt_zzacknow TYPE TABLE OF zzacknow,
        gs_zzacknow TYPE zzacknow.

  "查询交货抬头信息
  IF t_jhdh IS NOT INITIAL.

    SELECT * INTO TABLE gt_likp
      FROM likp
      FOR ALL ENTRIES IN t_jhdh
      WHERE vbeln = t_jhdh-vbeln
      AND vstel = p_vstel
      AND erdat BETWEEN p_cj_l AND p_cj_h .
    SORT gt_likp BY vbeln .
  ELSE.
    SELECT * INTO TABLE gt_likp
    FROM likp
   WHERE  vstel = p_vstel
    AND erdat BETWEEN p_cj_l AND p_cj_h .
    SORT gt_likp BY vbeln .
  ENDIF .

  IF gt_likp IS NOT INITIAL .
    SELECT * INTO TABLE gt_lips
      FROM lips
      FOR ALL ENTRIES IN gt_likp
      WHERE vbeln = gt_likp-vbeln .

    SORT gt_lips BY vbeln posnr .

    SELECT * INTO TABLE gt_vbak
        FROM vbak
        FOR ALL ENTRIES IN gt_lips
        WHERE vbeln = gt_lips-vgbel .

    SORT gt_vbak BY vbeln .

    MOVE-CORRESPONDING gt_vbak TO gt_vbeln_1 .

    APPEND LINES OF gt_vbeln_1 TO gt_vbeln .

    SELECT * INTO TABLE gt_ekkn
      FROM ekkn
      FOR ALL ENTRIES IN gt_lips
      WHERE ebeln = gt_lips-vgbel
      .
    SORT gt_ekkn BY ebeln ebelp .

    MOVE-CORRESPONDING gt_ekkn TO gt_vbeln_2 .

    SORT gt_vbeln_2 BY vbeln .

    APPEND LINES OF gt_vbeln_2 TO gt_vbeln .

    SORT gt_vbeln BY vbeln .

    DELETE ADJACENT DUPLICATES FROM  gt_vbeln COMPARING vbeln .

    MOVE-CORRESPONDING gt_lips TO gt_matnr .

    DELETE gt_matnr WHERE matnr IS INITIAL .
    SORT gt_matnr BY matnr .
    DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

    SELECT * INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN gt_matnr
      WHERE matnr = gt_matnr-matnr
      AND spras = 'E'.

    SORT gt_makt BY matnr .


    LOOP AT gt_vbeln INTO gs_vbeln .
      CLEAR:g_objname .
      REFRESH:it_lines .
      g_objname = gs_vbeln-vbeln .
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

    LOOP AT gt_vbak INTO gs_vbak .
      "
      CLEAR:t_jh.
      "交货单号
      t_jh-vbeln = gs_vbak-vbeln .
      t_jh-erdat = gs_vbak-erdat . "记录创建日期
      APPEND t_jh .
    ENDLOOP.

    LOOP AT gt_lips INTO gs_lips .

      CLEAR:t_jh_mx .
      "交货单号
      t_jh_mx-vbeln = gs_lips-vbeln .
      "交货项目
      t_jh_mx-posnr = gs_lips-posnr .
      "物料号
      t_jh_mx-matnr = gs_lips-matnr .
      "物料描述
      t_jh_mx-arktx = gs_lips-arktx .
      "数量
      t_jh_mx-lfimg = gs_lips-lfimg .
      "单位
      t_jh_mx-meins = gs_lips-meins .
      "工厂
      t_jh_mx-werks = gs_lips-werks.
      "项目编号
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_lips-vgbel
                                               BINARY SEARCH .
      IF sy-subrc EQ 0.
        t_jh_mx-xmbh = gs_vbak-vbeln .
      ELSE.
        READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_lips-vgbel
                                                 ebelp = gs_lips-vgpos+1(5)
                                                 BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_jh_mx-xmbh = gs_ekkn-vbeln .

        ENDIF.

      ENDIF.
      IF t_jh_mx-xmbh IS NOT INITIAL.
        READ TABLE gt_vbeln INTO gs_vbeln WITH KEY vbeln  = t_jh_mx-xmbh
                                           BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_jh_mx-xmmc = gs_vbeln-xmmc .
        ENDIF.

      ENDIF.

      "读取项目的英文描述

      IF t_jh_mx-matnr IS NOT INITIAL .
        READ TABLE gt_makt INTO gs_makt WITH KEY matnr = t_jh_mx-matnr
                                           BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_jh_mx-arktx_en = gs_makt-maktx .

        ENDIF.

      ENDIF.

      APPEND t_jh_mx .


    ENDLOOP.

    r_msg = '数据据读取成功,请查看T_JH、T_JH_MX表'.
    CLEAR:gs_zzacknow.
    DESCRIBE TABLE t_jh[]  LINES t_lines .
    DESCRIBE TABLE t_jh_mx[]  LINES t_lines_mx .
    gs_zzacknow-zzprofg = 'S'.
    MOVE t_lines TO len1.
    MOVE t_lines_mx TO len2 .

    CONCATENATE p_dyxtm '系统已成功读取' len1 '条主记录' INTO gs_zzacknow-zzcomen .
    CONCATENATE  gs_zzacknow-zzcomen  '&' len2 '条子记录' INTO gs_zzacknow-zzcomen .


  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'JH'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_vstel.    "装运点
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_MATNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.

ENDFUNCTION.
