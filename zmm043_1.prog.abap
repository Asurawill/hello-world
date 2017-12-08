REPORT zmm043_1.

"Created by :IT02
"Request:    项目物料管理报表
"Modify by:
"Modify date:
"
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2017-06-16  IT02&WEIYUN    ED1K905743     新增项目物料管理报表

TABLES:mseg,proj,ekkn,prps,precp2,ckis,ekpo,qbew.


TYPES:BEGIN OF ty_data,
        bukrs   LIKE t001-bukrs  , "公司代码
        pspid   LIKE proj-pspid  , "项目定义
        post1   LIKE proj-post1 , "项目描述
        matnr   LIKE mara-matnr,  "物料号
        maktx   LIKE makt-maktx,  "物料描述
        meins   LIKE mara-meins,  "基本单位
        yssl    LIKE ckis-menge,  "预算数量
        cgsl    LIKE ekpo-menge,   "采购数量
        kcsl    LIKE qbew-lbkum,   "库存数量
        cksl    LIKE mseg-menge,   "出库数量
        sjcgsl  LIKE ekpo-menge, "实际采购数量
        sjkcsl  LIKE qbew-lbkum,  "实际库存数量
        sjcksl  LIKE mseg-menge,  "实际出库数量
        sjxmdy  LIKE proj-pspid,   "实际项目定义
        gpreis  LIKE ckis-gpreis,  "预算价
        netpr   LIKE ekpo-netpr,    "采购价
        verpr   LIKE qbew-verpr,    "移动平均价
        waers   LIKE t001-waers,     "货币
        bz      TYPE string,      "备注
        celltab TYPE   lvc_t_styl,     "控制单元格属性
        zsel    TYPE c,          "选择项

      END OF ty_data.

TYPES:BEGIN OF ty_sel,
        bukrs  LIKE t001-bukrs  , "公司代码
        pspid  LIKE proj-pspid  , "项目定义
        matnr  LIKE mara-matnr,  "物料号
        meins  LIKE mara-meins,  "基本单位
        sjcgsl LIKE ekpo-menge, "实际采购数量
        sjkcsl LIKE qbew-lbkum,  "实际库存数量
        sjcksl LIKE mseg-menge,  "实际出库数量
        sjxmdy LIKE proj-pspid,   "实际项目定义
        bz     TYPE string,      "备注
        zsel   TYPE c,          "选择项

      END OF ty_sel.

TYPES:BEGIN OF ty_wlhz,
        pspid LIKE proj-pspid,   "项目定义
        matnr LIKE mseg-matnr,  "物料号
        menge LIKE mseg-menge,  "数量
      END OF ty_wlhz.

TYPES:BEGIN OF ty_xmwl,
        pspid LIKE proj-pspid,  "项目定义
        matnr LIKE mseg-matnr,  "项目物料
      END OF ty_xmwl.

TYPES:BEGIN OF ty_matnr,
        matnr TYPE mara-matnr,
      END OF ty_matnr.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_mara TYPE TABLE OF mara,
     gs_mara TYPE mara.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.

DATA:gs_xmwl   TYPE  ty_xmwl,
     gt_xmwl   TYPE TABLE OF ty_xmwl,
     gt_xmwl_1 TYPE TABLE OF ty_xmwl,
     gt_xmwl_2 TYPE TABLE OF ty_xmwl,
     gt_xmwl_3 TYPE TABLE OF ty_xmwl,
     gt_xmwl_4 TYPE TABLE OF ty_xmwl,
     gt_xmwl_5 TYPE TABLE OF ty_xmwl.

DATA:gs_wlhz   TYPE ty_wlhz,
     gt_wlhz_1 TYPE TABLE OF ty_wlhz,
     gt_wlhz_2 TYPE TABLE OF ty_wlhz,
     gt_wlhz_3 TYPE TABLE OF ty_wlhz,
     gt_wlhz_4 TYPE TABLE OF ty_wlhz,
     gt_wlhz_5 TYPE TABLE OF ty_wlhz.


DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_proj TYPE TABLE OF proj,
     gs_proj TYPE proj.

DATA:gt_prps TYPE TABLE OF prps,
     gs_prps TYPE prps.

DATA:gt_ckis TYPE TABLE OF ckis,
     gs_ckis TYPE ckis.

DATA:gt_precp2 TYPE TABLE OF precp2,
     gs_precp2 TYPE precp2.

DATA:gt_ekkn TYPE TABLE OF ekkn,
     gs_ekkn TYPE ekkn.

DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

DATA:gt_qbew TYPE TABLE OF qbew,
     gs_qbew TYPE qbew.

DATA:gt_mseg TYPE TABLE OF mseg,
     gs_mseg TYPE mseg.

DATA:gt_zmm043 TYPE TABLE OF zmm043,
     gs_zmm043 TYPE zmm043.


DATA:gt_change TYPE TABLE OF zmm043_change,
     gs_change TYPE zmm043_change.


DATA:gt_sel TYPE TABLE OF ty_sel,
     gs_sel TYPE ty_sel.

DATA:gt_del TYPE TABLE OF ty_sel,
     gs_del TYPE ty_sel.

DATA: gr_alvgrid TYPE REF TO cl_gui_alv_grid.


DATA ls_stable  TYPE lvc_s_stbl.
DATA ls_celltab TYPE lvc_s_styl.
DATA lt_celltab TYPE lvc_t_styl.


DATA stbl       TYPE lvc_s_stbl.




*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gw_layout        TYPE lvc_s_layo,                    "alv的格式
      gw_variant       TYPE disvariant,
      gw_grid_settings TYPE lvc_s_glay,
      gw_lvc           TYPE lvc_s_fcat,
      gw_sort          TYPE lvc_s_sort,
      gw_grid_setting  TYPE lvc_s_glay,
      g_repid          LIKE sy-repid,                      "SY-REPID 指 当前的主程序
      gt_events        TYPE slis_t_event WITH HEADER LINE, "保存AVL事件
      gw_events        LIKE LINE OF gt_events.
DATA: gt_exclude TYPE slis_t_extab,
      gs_exclude TYPE slis_extab.


DATA: gt_rows TYPE lvc_t_row,
      gt_roid TYPE lvc_t_roid,
      wa_rows TYPE lvc_s_row,
      wa_roid TYPE lvc_s_roid.
DATA: gs_variant TYPE disvariant.
DATA: gw_istable TYPE lvc_s_stbl.
DATA:e_mseg TYPE string.

FIELD-SYMBOLS:<fs_data> TYPE ty_data .


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS:p_bukrs TYPE t001-bukrs OBLIGATORY .  "公司代码

SELECT-OPTIONS: s_pspid FOR proj-pspid.  "项目定义
SELECTION-SCREEN END OF BLOCK blk1.


*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
*  IF sy-subrc NE 0.
*    MESSAGE e603(fco) WITH p_werks DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0057   text
*----------------------------------------------------------------------*
FORM frm_auth_check  USING    VALUE(p_0057).
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'WERKS' FIELD p_bukrs.
  IF sy-subrc <> 0.
    CONCATENATE '无权显示公司代码：' p_bukrs '的报表' INTO e_mseg.
    MESSAGE e_mseg TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  SELECT * INTO TABLE gt_proj
    FROM proj
    WHERE vbukr = p_bukrs
    AND pspid IN s_pspid.

  SORT gt_proj BY pspnr .

  IF gt_proj IS NOT INITIAL.
    SELECT * INTO TABLE gt_prps
      FROM prps
      FOR ALL ENTRIES IN gt_proj
      WHERE psphi = gt_proj-pspnr.

    SORT gt_prps BY pspnr .

    SELECT * INTO TABLE gt_precp2
      FROM precp2
      FOR ALL ENTRIES IN gt_prps
      WHERE subnr = gt_prps-objnr
      AND  versn = '000'
     .

    SORT gt_precp2 BY kalnr.

    IF gt_precp2 IS NOT INITIAL.
      SELECT * INTO TABLE gt_ckis
        FROM ckis
        FOR ALL ENTRIES IN gt_precp2
        WHERE kalnr = gt_precp2-kalnr
        AND  typps = 'M'
        .
      SORT gt_ckis BY kalnr matnr ..

    ENDIF.

    IF gt_prps IS NOT INITIAL.
      SELECT * INTO TABLE gt_ekkn
        FROM ekkn
        FOR ALL ENTRIES IN gt_prps
        WHERE ps_psp_pnr = gt_prps-pspnr
        .
      SORT gt_ekkn BY ebeln ebelp .

      IF gt_ekkn IS NOT INITIAL.
        SELECT * INTO TABLE gt_ekpo
          FROM ekpo
          FOR ALL ENTRIES IN gt_ekkn
          WHERE ebeln = gt_ekkn-ebeln
          AND ebelp = gt_ekkn-ebelp
          AND loekz = ''
          AND matnr NE ''.
        SORT  gt_ekpo BY ebeln ebelp .

      ENDIF.

      SELECT * INTO TABLE gt_qbew
        FROM qbew
        FOR ALL ENTRIES IN gt_prps
        WHERE pspnr = gt_prps-pspnr
        .
      SORT gt_qbew BY pspnr matnr .

      SELECT * INTO TABLE gt_mseg
        FROM mseg
        FOR ALL ENTRIES IN gt_prps
        WHERE mat_pspnr = gt_prps-pspnr
        AND bwart IN ('281','282')
         .
      SORT gt_mseg BY mat_pspnr matnr.


    ENDIF.

    "查询自建表的
    SELECT * INTO TABLE gt_zmm043
      FROM zmm043
      WHERE bukrs EQ p_bukrs
      AND   pspid IN s_pspid.

    SORT gt_zmm043 BY pspid matnr .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .
  DATA:g_pspid LIKE proj-pspid.

  " "统计汇总项目号、物料号的预算数量
  CLEAR:gs_wlhz.
  LOOP AT gt_precp2 INTO gs_precp2.
    AT NEW subnr.
      CLEAR:g_pspid.
      "取项目号
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_precp2-subnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-psphi
                                         BINARY SEARCH.
        IF sy-subrc EQ 0 .
          g_pspid = gs_proj-pspid.
        ENDIF.
      ENDIF.
    ENDAT .

    CLEAR:gs_wlhz.
    gs_wlhz-pspid = g_pspid. "项目号赋值
    LOOP AT gt_ckis INTO gs_ckis WHERE kalnr = gs_precp2-kalnr .
      CLEAR:gs_wlhz-matnr,gs_wlhz-menge.
      gs_wlhz-matnr = gs_ckis-matnr.
      gs_wlhz-menge = gs_ckis-menge.
      COLLECT gs_wlhz INTO gt_wlhz_1.
    ENDLOOP.

  ENDLOOP.
  SORT gt_wlhz_1 BY pspid matnr .

  "项目物料
  MOVE-CORRESPONDING gt_wlhz_1 TO gt_xmwl_1.
  SORT gt_xmwl_1 BY pspid matnr.

  "按项目、物料汇总采购订单数量
  CLEAR:gs_wlhz.
  LOOP AT gt_ekkn INTO gs_ekkn.
    AT NEW ps_psp_pnr.
      CLEAR:g_pspid.
      "取项目号
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_ekkn-ps_psp_pnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-psphi
                                         BINARY SEARCH.
        IF sy-subrc EQ 0 .
          g_pspid = gs_proj-pspid.
        ENDIF.
      ENDIF.
    ENDAT.

    CLEAR:gs_wlhz.
    gs_wlhz-pspid = g_pspid. "项目号赋值
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_ekkn-ebeln
                                             ebelp = gs_ekkn-ebelp
                                     BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR:gs_wlhz-matnr,gs_wlhz-menge.
      gs_wlhz-matnr = gs_ekpo-matnr.
      IF gs_ekpo-retpo EQ 'X'.
        gs_wlhz-menge = gs_ekpo-menge * -1.
      ELSE.
        gs_wlhz-menge = gs_ekpo-menge .
      ENDIF.

      COLLECT gs_wlhz INTO gt_wlhz_2.

    ENDIF.

  ENDLOOP.
  SORT gt_wlhz_2 BY pspid matnr .
  "项目物料
  MOVE-CORRESPONDING gt_wlhz_2 TO gt_xmwl_2.
  SORT gt_xmwl_2 BY pspid matnr.

  "按项目、物料汇总库存数量
  CLEAR:gs_wlhz.
  LOOP AT gt_qbew INTO gs_qbew.
    AT NEW pspnr.
      CLEAR:g_pspid.
      "取项目号
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_qbew-pspnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-psphi
                                         BINARY SEARCH.
        IF sy-subrc EQ 0 .
          g_pspid = gs_proj-pspid.
        ENDIF.
      ENDIF.

    ENDAT.
    CLEAR:gs_wlhz.
    gs_wlhz-pspid = g_pspid.
    gs_wlhz-matnr = gs_qbew-matnr.
    gs_wlhz-menge = gs_qbew-lbkum.

    COLLECT gs_wlhz INTO gt_wlhz_3.


  ENDLOOP.

  SORT gt_wlhz_3 BY pspid matnr .
  "项目物料
  MOVE-CORRESPONDING gt_wlhz_3 TO gt_xmwl_3.
  SORT gt_xmwl_3 BY pspid matnr.

  "按项目、物料汇总出库数量
  CLEAR:gs_wlhz.
  LOOP AT gt_mseg INTO gs_mseg.
    AT NEW mat_pspnr.
      CLEAR:g_pspid.
      "取项目号
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_mseg-mat_pspnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-psphi
                                         BINARY SEARCH.
        IF sy-subrc EQ 0 .
          g_pspid = gs_proj-pspid.
        ENDIF.
      ENDIF.

    ENDAT.
    CLEAR:gs_wlhz.
    gs_wlhz-pspid = g_pspid.
    gs_wlhz-matnr = gs_mseg-matnr.
    IF gs_mseg-shkzg = 'S'.
      gs_wlhz-menge = gs_mseg-menge * -1.
    ELSE.
      gs_wlhz-menge = gs_mseg-menge.
    ENDIF.

    COLLECT gs_wlhz INTO gt_wlhz_4.


  ENDLOOP.
  SORT gt_wlhz_4 BY pspid matnr .
  "项目物料
  MOVE-CORRESPONDING gt_wlhz_4 TO gt_xmwl_4.
  SORT gt_xmwl_4 BY pspid matnr.


  "统计台账的项目物料
  MOVE-CORRESPONDING gt_zmm043 TO gt_xmwl_5.
  SORT gt_xmwl_5 BY pspid matnr.


  "统计项目、物料的总分类
  IF gt_xmwl_1 IS NOT INITIAL.
    APPEND LINES OF gt_xmwl_1 TO gt_xmwl.
  ENDIF.
  IF gt_xmwl_2 IS NOT INITIAL.
    APPEND LINES OF gt_xmwl_2 TO gt_xmwl.
  ENDIF.
  IF gt_xmwl_3 IS NOT INITIAL.
    APPEND LINES OF gt_xmwl_3 TO gt_xmwl.
  ENDIF.
  IF gt_xmwl_4 IS NOT INITIAL.
    APPEND LINES OF gt_xmwl_4 TO gt_xmwl.
  ENDIF.
  IF gt_xmwl_5 IS NOT INITIAL.
    APPEND LINES OF gt_xmwl_5 TO gt_xmwl.
  ENDIF.

  SORT gt_xmwl BY pspid matnr.
  DELETE ADJACENT DUPLICATES FROM gt_xmwl COMPARING pspid matnr.

  MOVE-CORRESPONDING gt_xmwl TO gt_matnr.
  SORT gt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr.

  SELECT * INTO TABLE gt_mara
    FROM mara
    FOR ALL ENTRIES IN gt_matnr
    WHERE matnr = gt_matnr-matnr.

  SORT gt_mara BY matnr .


  SELECT * INTO TABLE gt_makt
    FROM makt
   FOR ALL ENTRIES IN gt_matnr
    WHERE matnr = gt_matnr-matnr.

  SORT gt_makt BY matnr.


  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs = p_bukrs
    .
  SORT gt_t001 BY bukrs.

  LOOP AT gt_xmwl INTO gs_xmwl.
    CLEAR:gs_data.
    gs_data-pspid = gs_xmwl-pspid. "项目定义
    READ TABLE gt_proj INTO gs_proj WITH KEY  pspid = gs_xmwl-pspid
                                  .
    IF sy-subrc EQ 0.
      gs_data-bukrs = gs_proj-vbukr.

      gs_data-post1 = gs_proj-post1. "项目名称

    ENDIF.
    gs_data-matnr = gs_xmwl-matnr. "物料号
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-maktx = gs_makt-maktx.  "物料描述

    ENDIF.
    READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_data-matnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.

      gs_data-meins = gs_mara-meins.
    ENDIF.

    "预算数量
    READ TABLE gt_wlhz_1 INTO gs_wlhz WITH KEY pspid = gs_data-pspid
                                               matnr = gs_data-matnr
                                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-yssl = gs_wlhz-menge.
    ENDIF.

    "采购数量
    READ TABLE gt_wlhz_2 INTO gs_wlhz WITH KEY pspid = gs_data-pspid
                                           matnr = gs_data-matnr
                                           BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-cgsl = gs_wlhz-menge.

    ENDIF.

    "库存数量
    READ TABLE gt_wlhz_3 INTO gs_wlhz WITH KEY pspid = gs_data-pspid
                                              matnr = gs_data-matnr
                                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-kcsl = gs_wlhz-menge.

    ENDIF.

    "出库数量
    READ TABLE gt_wlhz_4 INTO gs_wlhz WITH KEY pspid = gs_data-pspid
                                              matnr = gs_data-matnr
                                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-cksl = gs_wlhz-menge.
    ENDIF.

    "预算价

    READ TABLE gt_ckis INTO gs_ckis WITH KEY matnr = gs_data-matnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-gpreis = gs_ckis-gpreis .

    ENDIF.
    "采购价

    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY matnr = gs_data-matnr
                                    .
    IF sy-subrc EQ 0.
      gs_data-netpr = gs_ekpo-netpr.

    ENDIF.

    "移动平均价
    READ TABLE gt_qbew INTO gs_qbew WITH KEY matnr = gs_data-matnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-verpr = gs_qbew-verpr.
    ENDIF.
    "货币
    READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_data-bukrs
                                    BINARY SEARCH..
    IF sy-subrc EQ 0.
      gs_data-waers = gs_t001-waers.
    ENDIF.

    READ TABLE gt_zmm043 INTO gs_zmm043 WITH KEY pspid = gs_data-pspid
                                                 matnr = gs_data-matnr
                                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-sjcgsl = gs_zmm043-sjcgsl.
      gs_data-sjkcsl = gs_zmm043-sjkcsl.
      gs_data-sjcksl = gs_zmm043-sjcksl.
      gs_data-sjxmdy = gs_zmm043-sjxmdy.
      gs_data-bz = gs_zmm043-bz.
    ENDIF.

    APPEND gs_data TO gt_data.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_show .
  PERFORM init_layout.             "设置输出格式
  PERFORM init_sort.               "设置排序、合计
  PERFORM init_variant.            "设置变式控制
  PERFORM frm_init_lvc.
  PERFORM frm_exclude.
  PERFORM frm_build_event.
  gw_grid_settings-edt_cll_cb = 'X'.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            gt_data
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_layout .
  gw_layout-zebra         = 'X'.
  gw_layout-cwidth_opt    = 'X'.
  gw_layout-box_fname     = 'ZSEL'.
  gw_layout-stylefname   = 'CELLTAB'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_sort .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_variant .

ENDFORM.

DEFINE init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
ENDIF.
IF gw_lvc-fieldname = 'BZ'.
    gw_lvc-outputlen = 100.
endif.
  if   gw_lvc-fieldname eq 'YSSL'
     OR  gw_lvc-fieldname eq 'CGSL'
     OR  gw_lvc-fieldname eq 'KCSL'
     OR  gw_lvc-fieldname eq 'CKSL'
     oR gw_lvc-fieldname eq 'SJKCSL'
     OR gw_lvc-fieldname eq  'SJCKSL'
    OR gw_lvc-fieldname eq  'SJCGSL'.


      gw_lvc-qfieldname = 'MEINS'.
  endif.

  if   gw_lvc-fieldname eq 'GPREIS'
   OR  gw_lvc-fieldname eq  'NETPR'
   OR  gw_lvc-fieldname eq 'VERPR'
    .
    gw_lvc-cfieldname = 'WAERS'.
  endif.

  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'BUKRS'        '公司代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'PSPID'        '项目定义'         '' '' '' 'X' '' 'PROJ' 'PSPID'.
  init_fieldcat 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' '' '' 'X' '' 'MARA' 'MATNR'.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'        '基本单位'         '' '' '' '' '' '' ''.
  init_fieldcat 'YSSL'        '预算数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'CGSL'        '采购数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'KCSL'        '库存数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'CKSL'        '出库数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJCGSL'        '实际采购数量'         '' '' '' 'X' '' 'EKPO' 'MENGE'.
  init_fieldcat 'SJKCSL'        '实际库存数量'         '' '' '' 'X' '' 'QBEW' 'LBKUM'.
  init_fieldcat 'SJCKSL'        '实际出库数量'         '' '' '' 'X' '' 'MSEG' 'MENGE'.
  init_fieldcat 'SJXMDY'        '实际项目定义'         '' '' '' 'X' '' 'PROJ' 'PSPID'.
  init_fieldcat 'GPREIS'        '预算价'         '' '' '' '' '' '' ''.
  init_fieldcat 'NETPR'        '采购价'         '' '' '' '' '' '' ''.
  init_fieldcat 'VERPR'        '移动平均价'         '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'        '货币'         '' '' '' '' '' '' ''.
  init_fieldcat 'BZ'        '备注'         '' '' '' 'X' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_exclude .
  REFRESH gt_exclude.
  CLEAR gs_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_event .
  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.
  APPEND gw_events TO gt_events.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0114   text
*      -->P_0115   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM frm_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_data
                USING pu_status
                      pu_ucomm
                      pw_layout TYPE lvc_s_layo
                      pw_variant TYPE disvariant
                      pw_grid_settings TYPE lvc_s_glay.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = pu_status
      i_callback_user_command  = pu_ucomm
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      i_grid_settings          = pw_grid_settings
      is_layout_lvc            = pw_layout
      it_fieldcat_lvc          = pt_lvc[]
      it_excluding             = gt_exclude
*     IT_SPECIAL_GROUPS_LVC    =
      it_sort_lvc              = pt_sort[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = pw_variant
      it_events                = gt_events[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
      READ TABLE gt_data INTO gs_data INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'PSPID'
        AND gs_data-pspid IS NOT INITIAL.
        SET PARAMETER ID 'PSP' FIELD gs_data-pspid.
        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&ADD'.
      CLEAR:gs_data.
      gs_data-bukrs = p_bukrs.
*      ls_celltab-fieldname = 'PSPID' .
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.                "mc_style_disabled.
*
*      INSERT ls_celltab INTO TABLE lt_celltab.
*
*      ls_celltab-fieldname = 'MATNR' .
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.                "mc_style_disabled.
*
*      INSERT ls_celltab INTO TABLE lt_celltab.
*      INSERT LINES OF lt_celltab INTO TABLE gs_data-celltab.
      APPEND gs_data TO gt_data.
    WHEN '&DELE'.
      DATA:p_tabix TYPE sy-tabix.
      MOVE-CORRESPONDING gt_data TO gt_del.
      DELETE gt_del WHERE zsel NE 'X'.
      IF gt_del IS INITIAL.
        MESSAGE '请选择要删除的行记录' TYPE 'S'  DISPLAY LIKE 'E'.
      ENDIF.
      SORT gt_del BY pspid matnr.
      "判断删除的行是否已保存
      IF gt_del IS NOT INITIAL.
        SELECT * INTO TABLE gt_zmm043
       FROM zmm043
       FOR ALL ENTRIES IN gt_del
       WHERE pspid = gt_del-pspid
          AND matnr = gt_del-matnr.
        SORT gt_zmm043 BY pspid matnr .
        LOOP AT gt_del INTO gs_del.
          p_tabix = sy-tabix.
          READ TABLE gt_zmm043 INTO gs_zmm043 WITH KEY pspid = gs_del-pspid
                                                       matnr = gs_del-matnr
                                                       .
          IF sy-subrc NE 0.
            DELETE gt_del INDEX p_tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.

      DELETE gt_data WHERE zsel = 'X'.
    WHEN '&DATA_SAVE'.
      PERFORM save_data.
  ENDCASE.

  "*自动定位光标
  stbl-col = 'X'.
  stbl-row = 'X'.
  CALL METHOD g_ref_grid->refresh_table_display
    EXPORTING
      is_stable = stbl.



ENDFORM.                    "ALV_USER_COMMAND

FORM frm_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  PERFORM frm_data_enter USING er_data_changed..
ENDFORM.     "frm_data_changed


*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_ENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM frm_data_enter USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: g_ref_grid TYPE REF TO cl_gui_alv_grid,
        stbl       TYPE lvc_s_stbl.
  DATA lt_out TYPE  TABLE OF ty_data.
  DATA ls_out TYPE ty_data.

  DATA:ls_makt TYPE makt .

  DATA:ls_mara TYPE mara.

  DATA:tmp_post1 TYPE proj-post1.
  DATA:tmp_maktx TYPE makt-maktx.
  DATA:tmp_meins TYPE mara-meins.

  DATA: wa_mod_cell TYPE lvc_s_modi.

  FIELD-SYMBOLS:<l_matnr> TYPE any.

  CLEAR:ls_makt,
        ls_mara.

  REFRESH lt_out.

  "MODIFIED By:IT02 20161008 由原只针对第一行更改物料的变动扩为：现目前快速复制多个物料的更改
  "针对物料号更改
  LOOP AT er_data_changed->mt_mod_cells INTO wa_mod_cell WHERE fieldname = 'MATNR'.

    SELECT SINGLE maktx INTO tmp_maktx FROM makt
     WHERE matnr = wa_mod_cell-value
   AND spras = '1'.

    CLEAR:ls_mara .
    SELECT SINGLE meins FROM mara
      INTO tmp_meins
      WHERE matnr = wa_mod_cell-value.


    READ TABLE  gt_data INTO gs_data INDEX wa_mod_cell-row_id.
    IF sy-subrc = 0.
      CLEAR:gs_data-maktx,gs_data-meins.
      gs_data-maktx = tmp_maktx.
      gs_data-meins = tmp_meins.
      MODIFY gt_data FROM gs_data INDEX wa_mod_cell-row_id.
      CLEAR gs_data.
    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = g_ref_grid.

    CALL METHOD g_ref_grid->refresh_table_display.

  ENDLOOP .

  "项目号
  LOOP AT er_data_changed->mt_mod_cells INTO wa_mod_cell WHERE fieldname = 'PSPID'.
    SELECT SINGLE post1 INTO tmp_post1 FROM proj WHERE pspid = wa_mod_cell-value .

    READ TABLE  gt_data INTO gs_data  INDEX wa_mod_cell-row_id.
    IF sy-subrc = 0.
      CLEAR:gs_data-post1.
      gs_data-post1 = tmp_post1.
      MODIFY gt_data FROM gs_data INDEX wa_mod_cell-row_id.
      CLEAR gs_data.
    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = g_ref_grid.

    CALL METHOD g_ref_grid->refresh_table_display.
  ENDLOOP.
ENDFORM.                    "frm_data_enter
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .
  DATA:l_s TYPE c .
  CLEAR:l_s.
  REFRESH:gt_sel,gt_zmm043.
  DATA:gt_bg TYPE TABLE OF zmm043_change,
       gs_bg TYPE zmm043_change.

  DATA:gt_save TYPE TABLE OF zmm043,
       gs_save TYPE zmm043.

  MOVE-CORRESPONDING gt_data TO gt_sel.
  DELETE gt_sel WHERE zsel NE 'X'.
  SORT gt_sel BY bukrs pspid matnr.

  IF gt_sel IS INITIAL AND gt_del IS INITIAL.
    MESSAGE '无选择要保存的记录或保存已删除的记录' TYPE 'S'  DISPLAY LIKE 'E'.
  ENDIF.
  IF gt_sel IS NOT INITIAL .
    SELECT * INTO TABLE gt_zmm043
      FROM zmm043
      FOR ALL ENTRIES IN gt_sel
      WHERE bukrs = gt_sel-bukrs
      AND  pspid = gt_sel-pspid
      AND  matnr = gt_sel-matnr.
    SORT gt_zmm043 BY bukrs pspid matnr.

    "对比更改值
    LOOP AT gt_sel INTO gs_sel .
      READ TABLE gt_zmm043 INTO gs_zmm043 WITH KEY bukrs = gs_sel-bukrs
                                                   pspid = gs_sel-pspid
                                                   matnr = gs_sel-matnr
                                                   BINARY SEARCH.
      IF sy-subrc EQ 0.

        IF gs_sel-sjcgsl NE gs_zmm043-sjcgsl
            OR
           gs_sel-sjcksl NE gs_zmm043-sjcksl
          OR
          gs_sel-sjkcsl NE gs_zmm043-sjkcsl
          OR
          gs_sel-sjxmdy NE gs_zmm043-sjxmdy .

          CLEAR:gs_bg.
          gs_bg-bukrs = gs_sel-bukrs.               "公司代码
          gs_bg-pspid = gs_sel-pspid.               "项目定义
          gs_bg-matnr = gs_sel-matnr.               "物料号
          gs_bg-meins = gs_sel-meins.               "单位
          gs_bg-whrq = sy-datum.                    "维护日期
          gs_bg-whsj = sy-uzeit .                   "维护时间
          gs_bg-whzh = sy-uname.                    "维护账号
          "实际采购数量
          IF gs_sel-sjcgsl NE gs_zmm043-sjcgsl .
            gs_bg-sjcgsl = gs_zmm043-sjcgsl .        "旧值
            gs_bg-sjcgsl_1 = gs_sel-sjcgsl.          "新值
          ENDIF.
          "实际出库数量
          IF  gs_sel-sjcksl NE gs_zmm043-sjcksl .
            gs_bg-sjcksl = gs_zmm043-sjcksl .          "旧值
            gs_bg-sjcksl_1 = gs_sel-sjcksl .           "新值

          ENDIF.
          "实际库存数量
          IF gs_sel-sjkcsl NE gs_zmm043-sjkcsl.
            gs_bg-sjkcsl = gs_zmm043-sjkcsl .          "旧值
            gs_bg-sjkcsl_1 = gs_sel-sjkcsl.            "新值

          ENDIF.

          "实际项目定义
          IF  gs_sel-sjxmdy NE gs_zmm043-sjxmdy .
            gs_bg-sjxmdy = gs_zmm043-sjxmdy.            "旧值
            gs_bg-sjxmdy_1 = gs_sel-sjxmdy .            "新值
          ENDIF.

          APPEND gs_bg TO gt_bg.

        ENDIF.
      ELSE.
        CLEAR:gs_bg.
        gs_bg-bukrs = gs_sel-bukrs.         "公司代码
        gs_bg-pspid = gs_sel-pspid.         "项目定义
        gs_bg-matnr = gs_sel-matnr.         "物料号
        gs_bg-meins = gs_sel-meins.         "单位
        gs_bg-sjcgsl_1 = gs_sel-sjcgsl.     "采购订单数量新值
        gs_bg-sjcksl_1 = gs_sel-sjcksl .    "出库数量新值
        gs_bg-sjkcsl_1 = gs_sel-sjkcsl.     "库存数量新值
        gs_bg-sjxmdy_1 = gs_sel-sjxmdy .    "新值
        gs_bg-whrq = sy-datum.              "维护日期
        gs_bg-whsj = sy-uzeit .             "维护时间
        gs_bg-whzh = sy-uname.              "维护账号
        APPEND gs_bg TO gt_bg.

      ENDIF.

    ENDLOOP.
    IF gt_bg IS NOT INITIAL.
      "更新项目管理台账日志信息表
      MODIFY  zmm043_change FROM TABLE gt_bg.
    ENDIF.

    "更新项目管理物料表
    MOVE-CORRESPONDING  gt_sel TO gt_save.
    SORT gt_save BY pspid matnr.
    IF gt_save IS NOT INITIAL.
      MODIFY zmm043 FROM TABLE gt_save.
      l_s = 'X'.

    ENDIF.

  ENDIF.

  "保存删除记录到项目物料管理台账及变更表
  IF gt_del IS NOT INITIAL.
    REFRESH:gt_bg,gt_save.
    CLEAR:gs_bg.

    REFRESH:gt_zmm043.
    SELECT * INTO TABLE gt_zmm043
      FROM zmm043
      FOR ALL ENTRIES IN gt_del
      WHERE pspid = gt_del-pspid
      AND matnr = gt_del-matnr.
    SORT gt_zmm043 BY pspid matnr .

    MOVE-CORRESPONDING gt_zmm043 TO gt_save.
    DELETE zmm043 FROM TABLE gt_save.
    IF sy-subrc EQ 0.
      l_s = 'X'.
    ENDIF.


    "以删除项目、物料取出台账最新的信息作为旧值存储到变更日志表
    LOOP AT gt_zmm043 INTO gs_zmm043.
      CLEAR:gs_bg.
      gs_bg-pspid = gs_zmm043-pspid.
      gs_bg-matnr = gs_zmm043-matnr.
      gs_bg-bukrs = gs_zmm043-bukrs.
      gs_bg-meins = gs_zmm043-meins.
      gs_bg-sjcgsl = gs_zmm043-sjcgsl.
      gs_bg-sjcksl = gs_zmm043-sjcksl.
      gs_bg-sjkcsl = gs_zmm043-sjkcsl .
      gs_bg-whrq = sy-datum.              "维护日期
      gs_bg-whsj = sy-uzeit .             "维护时间
      gs_bg-whzh = sy-uname.              "维护账号
      gs_bg-zdel = 'X'.                   "删除标记
      APPEND gs_bg TO gt_bg.
    ENDLOOP.
    SORT gt_bg BY pspid matnr.
    IF gt_bg IS NOT INITIAL.
      "更新项目管理台账日志信息表
      MODIFY  zmm043_change FROM TABLE gt_bg.

    ENDIF.

  ENDIF.

  IF l_s EQ 'X' .
    MESSAGE '数据保存成功' TYPE 'I'.
  ENDIF.

ENDFORM.
