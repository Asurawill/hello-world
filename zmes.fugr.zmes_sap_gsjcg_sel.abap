FUNCTION zmes_sap_gsjcg_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_GYGC) TYPE  LIFNR
*"     VALUE(P_EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(P_VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_GSJCG STRUCTURE  ZGSJCG
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

  DATA:gt_ekko TYPE TABLE OF ekko,
       gs_ekko TYPE ekko.

  DATA:gt_ekpo TYPE TABLE OF ekpo,
       gs_ekpo TYPE ekpo.

  DATA:gt_ekkn TYPE TABLE OF ekkn,
       gs_ekkn TYPE ekkn.

  DATA:gt_vbap TYPE TABLE OF vbap,
       gs_vbap TYPE vbap.

  DATA:gt_t023t TYPE TABLE OF t023t,
       gs_t023t TYPE t023t.

  DATA:gt_vbeln TYPE TABLE OF ty_vbeln,
       gs_vbeln TYPE ty_vbeln.

  DATA: g_objname TYPE thead-tdname.

  DATA: it_lines TYPE TABLE OF tline,
        wa_lines TYPE tline.

  DATA:gs_gsjcg TYPE zgsjcg.

  DATA:g_tabix TYPE sy-tabix.


  "*记录日志数据
  DATA:len1 TYPE string.
  DATA:t_lines    TYPE i.
  DATA: gt_zzacknow TYPE TABLE OF zzacknow,
        gs_zzacknow TYPE zzacknow.

  REFRESH:gt_zzacknow.
  CLEAR:gs_zzacknow.

  REFRESH:t_gsjcg.
  "查询供应商为输入参数的采购订单信息
  IF p_ebeln  EQ ''.
    REFRESH:gt_ekko.
    SELECT * INTO TABLE gt_ekko
     FROM ekko
     WHERE lifnr EQ p_gygc.

    SORT gt_ekko BY ebeln .

  ELSE.
    REFRESH:gt_ekko.
    SELECT * INTO TABLE gt_ekko
   FROM ekko
   WHERE lifnr EQ p_gygc
      AND ebeln EQ p_ebeln.

    SORT gt_ekko BY ebeln .

  ENDIF.

  "查询采购订单账户分配信息
  IF gt_ekko IS NOT INITIAL.
    REFRESH:gt_ekpo.
    SELECT * INTO TABLE gt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN gt_ekko
      WHERE ebeln = gt_ekko-ebeln
      AND loekz NE 'L'.
    SORT gt_ekpo BY ebeln ebelp .

    IF p_vbeln EQ ''.
      REFRESH:gt_ekkn.
      SELECT * INTO TABLE  gt_ekkn
   FROM ekkn
   FOR ALL ENTRIES IN gt_ekko
   WHERE ebeln = gt_ekko-ebeln
      AND loekz NE 'X'.
      SORT gt_ekkn BY ebeln ebelp .

    ELSE.
      REFRESH:gt_ekkn.
      SELECT * INTO TABLE  gt_ekkn
   FROM ekkn
   FOR ALL ENTRIES IN gt_ekko
   WHERE ebeln = gt_ekko-ebeln
   AND   vbeln EQ p_vbeln.
      .
      SORT gt_ekkn BY ebeln ebelp .
      LOOP AT gt_ekpo INTO gs_ekpo.
        g_tabix = sy-tabix.
        READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_ekpo-ebeln
                                                  ebelp = gs_ekpo-ebelp
                                                  BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE gt_ekpo INDEX g_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gt_ekpo IS NOT INITIAL.
      "查询物料组信息
      REFRESH:gt_wlz.
      MOVE-CORRESPONDING gt_ekpo TO gt_wlz.
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

    ENDIF.

    IF  gt_ekkn IS NOT INITIAL.
      "查询销售信息
      REFRESH:gt_vbeln.
      MOVE-CORRESPONDING gt_ekkn TO gt_vbeln.
      SORT gt_vbeln BY vbeln.
      DELETE gt_vbeln WHERE vbeln IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM gt_vbeln COMPARING vbeln .
      LOOP AT gt_vbeln INTO gs_vbeln .
        CLEAR:g_objname .
        REFRESH:it_lines .
        g_objname = gs_vbeln-vbeln .
        REFRESH:it_lines.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
*           CLIENT                  = SY-MANDT
            id                      = 'Z001'
            language                = '1'
            name                    = g_objname
            object                  = 'VBBK'
*           ARCHIVE_HANDLE          = 0
*           LOCAL_CAT               = ' '
* IMPORTING
*           HEADER                  =
*           OLD_LINE_COUNTER        =
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


    LOOP AT gt_ekpo INTO gs_ekpo .
      CLEAR:gs_gsjcg.
      "采购订单
      gs_gsjcg-ebeln = gs_ekpo-ebeln.
      "采购订单行项目
      gs_gsjcg-ebelp = gs_ekpo-ebelp.
      "采购数量
      gs_gsjcg-menge = gs_ekpo-menge.
      "采购单位
      gs_gsjcg-meins = gs_ekpo-meins.
      "物料号
      gs_gsjcg-matnr = gs_ekpo-matnr.
      "采购文本
      gs_gsjcg-txz01 = gs_ekpo-txz01.
      READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_gsjcg-ebeln
                                               ebelp = gs_gsjcg-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        "销售订单
        gs_gsjcg-vbeln = gs_ekkn-vbeln.
        gs_gsjcg-posnr = gs_ekkn-vbelp.
        READ TABLE gt_vbeln INTO gs_vbeln WITH KEY vbeln  = gs_gsjcg-vbeln
                                          BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_gsjcg-xmmc = gs_vbeln-xmmc .
        ENDIF.
      ENDIF.
      "物料组
      gs_gsjcg-matkl = gs_ekpo-matkl.
      READ TABLE gt_t023t INTO gs_t023t WITH KEY matkl = gs_ekpo-matkl
                                        BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_gsjcg-wgbez = gs_t023t-wgbez.
      ENDIF.
      APPEND  gs_gsjcg TO t_gsjcg.

    ENDLOOP.
    SORT t_gsjcg BY ebeln ebelp .
    r_msg = '数据据读取成功,请查看T_GSJCG表'.
    CLEAR:gs_zzacknow.
    DESCRIBE TABLE t_gsjcg[]  LINES t_lines .
    gs_zzacknow-zzprofg = 'S'.
    MOVE t_lines TO len1.
    CONCATENATE p_dyxtm '系统已成功读取公司间采购信息' len1 '条主记录' INTO gs_zzacknow-zzcomen .
  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取公司间采购信息失败！'  INTO gs_zzacknow-zzcomen .

  ENDIF.

  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'GSJCG'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_gygc+6(4).    "供应工厂
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_GSJCG_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.



ENDFUNCTION.
