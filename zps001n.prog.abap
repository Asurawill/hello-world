REPORT zps001n.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/8
*& Request       :
*& Descriptions  : 项目产值信息表
*&
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2016-05-06  IT02&WEIYUN                   优化运行速度、新增替代折算
*2017-04-01  IT02&WEIYUN    ED1K905328     APPEND:自定义费用的产值信息
*&
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES:proj,mseg.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        pspid     TYPE proj-pspid,      "项目编号
        post1     TYPE proj-post1,      "项目名称
        mat_pspnr TYPE mseg-mat_pspnr, "WBS
        sjdm      TYPE zmm024-sjdm,     "设计代码
        matnr     TYPE mseg-matnr,      "物料号
        maktx     TYPE makt-maktx,      "物料描述
        gpreis    TYPE ckis-gpreis,     "物料综合单价
        fpreis    TYPE ckis-fpreis,     "人工预算单价
        kbetr     TYPE konv-kbetr,      "人工单价
        xmwzck    TYPE mseg-menge,      "物料凭证数量
        cz        TYPE ckis-gpreis,     "产值
        lwydjdk   TYPE ckis-gpreis,     "劳务月度进度款
        lwydjdk1  TYPE ckis-gpreis,     "劳务月度进度款(预算)
        zhtje     TYPE proj-zhtje,      "合同金额
        cktd      TYPE c         ,      "出库替代
        zcsxm     TYPE zpscost-zcsxm,    "措施项目
        zxfc      TYPE zpscost-zxfc,     "小辅材
        zgf       TYPE zpscost-zgf,      "规费
        zsj       TYPE zpscost-zsj,     "税金
        zzlj      TYPE zpscost-zzlj,    "暂列金
        zqtfy     TYPE zpscost-zqtfy,   "其他费用
      END OF ty_data.

TYPES:BEGIN OF ty_mseg,
        werks     TYPE mseg-werks,               "工厂
        matnr     TYPE mseg-matnr,               "物料凭证号
        mat_pspnr TYPE mseg-mat_pspnr,          "WBS元素
      END OF ty_mseg.

TYPES:BEGIN OF ty_td,
        "  PSPNR TYPE PS_INTNR,
        pspid TYPE ps_pspid,    "项目号
        posid TYPE ps_posid,    "WBS号
        werks TYPE werks_d,
        matnr TYPE matnr,
        menge TYPE menge_d,

      END OF ty_td.

DATA:BEGIN OF gs_cg,
       ebeln      TYPE ekkn-ebeln,
       ebelp      TYPE ekkn-ebelp,
       ps_psp_pnr TYPE ekkn-ps_psp_pnr,
       matnr      TYPE ekpo-matnr,
       netpr      TYPE ekpo-netpr,
     END OF gs_cg .

DATA:gt_cg LIKE TABLE OF gs_cg WITH HEADER LINE .


************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA gt_data    TYPE TABLE OF ty_data.
DATA gs_data    TYPE ty_data.
DATA gs_data_1  TYPE ty_data.

DATA gt_proj  TYPE TABLE OF proj.
DATA gs_proj  TYPE proj.

DATA gt_prps  TYPE TABLE OF prps.
DATA gs_prps  TYPE prps.

DATA gt_mseg  TYPE TABLE OF mseg.
DATA gs_mseg  TYPE mseg.

DATA gt_mseg1 TYPE TABLE OF ty_mseg.
DATA gs_mseg1 TYPE ty_mseg.

DATA gt_makt  TYPE TABLE OF makt.
DATA gs_makt  TYPE makt.

DATA gt_ekkn  TYPE TABLE OF ekkn.
DATA gs_ekkn  TYPE ekkn.

DATA gt_ekpo  TYPE TABLE OF ekpo.
DATA gs_ekpo  TYPE ekpo.

DATA gt_ekko  TYPE TABLE OF ekko.
DATA gs_ekko  TYPE ekko.

DATA gt_konv  TYPE TABLE OF konv.
DATA gs_konv  TYPE konv.

DATA gt_zpsprod TYPE TABLE OF zpsprod.
DATA gs_zpsprod TYPE zpsprod.

DATA gt_zmm024     TYPE TABLE OF zmm024.
DATA gs_zmm024     TYPE zmm024.

DATA:gt_ck TYPE TABLE  OF ty_td,
     gs_ck TYPE ty_td.

DATA:gt_ztdtz_ck TYPE TABLE OF ztdtz_ck,
     gs_ztdtz_ck TYPE ztdtz_ck.

DATA:gt_ztdtz_ck_z TYPE TABLE OF ztdtz_ck,
     gs_ztdtz_ck_z TYPE ztdtz_ck.

DATA:gt_zpscost TYPE TABLE OF zpscost,
     gs_zpscost TYPE zpscost.

DATA e_wbs_ecp        TYPE tty_proj_element_ck_items_rdex.
DATA lt_e_wbs_ecp     TYPE TABLE OF proj_element_ck_items_rdexp.
DATA ls_e_wbs_ecp     TYPE proj_element_ck_items_rdexp.
DATA lt_cost_lines_100    TYPE TABLE OF kis1.
DATA ls_cost_lines_100   TYPE kis1.
DATA lt_cost_lines_000    TYPE TABLE OF kis1.
DATA ls_cost_lines_000    TYPE kis1.
************************************************************************
*      DEFINITION
************************************************************************
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

  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.

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

DATA: gr_alvgrid TYPE REF TO cl_gui_alv_grid.

DATA: gt_rows TYPE lvc_t_row,
      gt_roid TYPE lvc_t_roid,
      wa_rows TYPE lvc_s_row,
      wa_roid TYPE lvc_s_roid.
DATA: gs_variant TYPE disvariant.
DATA: gw_istable TYPE lvc_s_stbl.

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECT-OPTIONS:s_pspid FOR proj-pspid,      "项目定义
               s_vernr FOR proj-vernr,      "项目经理
               s_budat FOR mseg-budat_mkpf. "过账日期
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.
*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**权限检查检查公司代码
*  " PERFORM FRM_AUTH_CHECK USING '03'.
*  PERFORM FRM_AUTH_CHECK.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM frm_get_data.
  PERFORM frm_deal_data.
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .

  SELECT * FROM proj
   INTO CORRESPONDING FIELDS OF TABLE gt_proj
   WHERE pspid IN s_pspid
   AND   vernr IN s_vernr.
  SORT gt_proj BY pspnr .

  IF gt_proj IS  NOT INITIAL.
    SELECT * FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_prps
    FOR ALL ENTRIES IN gt_proj
    WHERE psphi = gt_proj-pspnr.

    SORT gt_prps BY pspnr.

    SELECT * FROM zmm024
     INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
     FOR ALL ENTRIES IN gt_proj
     WHERE posid = gt_proj-pspid.

*查询期初数据
    SELECT * FROM zpsprod
    INTO CORRESPONDING FIELDS OF TABLE gt_zpsprod
    FOR ALL ENTRIES IN gt_proj
    WHERE pspid = gt_proj-pspid.

    IF gt_prps[] IS NOT INITIAL.
      SELECT ekkn~ebeln ekkn~ebelp
             ekkn~ps_psp_pnr  ekpo~matnr ekpo~netpr
          FROM  ekkn
          INNER JOIN ekpo
          ON ekkn~ebeln = ekpo~ebeln
          AND  ekkn~ebelp = ekpo~ebelp
          INTO CORRESPONDING FIELDS OF TABLE gt_cg
          FOR ALL ENTRIES IN gt_prps
          WHERE ekkn~ps_psp_pnr  = gt_prps-pspnr
          AND  ekkn~loekz NE 'L'
          AND  ekpo~knttp = 'Y'
          AND  ekpo~loekz NE 'L'
          AND  ekpo~matnr NE ''.
      SORT gt_cg  BY ps_psp_pnr ASCENDING matnr ASCENDING ebeln DESCENDING ebelp ASCENDING .
      DELETE ADJACENT  DUPLICATES FROM gt_cg COMPARING ps_psp_pnr matnr  .

    ENDIF.
*    SELECT * FROM EKKN
*    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
*     FOR ALL ENTRIES IN GT_PRPS
*     WHERE PS_PSP_PNR  = GT_PRPS-PSPNR
*     AND  LOEKZ <> 'L'.
*
*    IF GT_EKKN IS NOT INITIAL.
*      SELECT * FROM EKPO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
*      FOR ALL ENTRIES IN GT_EKKN
*      WHERE EBELN = GT_EKKN-EBELN
*      AND   EBELP = GT_EKKN-EBELP
*      AND   LOEKZ <> 'L'
*      AND   KNTTP = 'Y'.
*   ENDIF.
*
*   IF GT_EKPO IS NOT INITIAL.
*      SELECT * FROM EKKO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
*      FOR ALL ENTRIES IN GT_EKPO
*      WHERE EBELN = GT_EKPO-EBELN.
*   ENDIF.
*
*   IF GT_EKKO IS NOT INITIAL.
*
*      SELECT * FROM KONV
*      INTO CORRESPONDING FIELDS OF TABLE GT_KONV
*      FOR ALL ENTRIES IN GT_EKKO
*      WHERE KNUMV = GT_EKKO-KNUMV.
*
*    ENDIF.


    SELECT * FROM mseg
    INTO CORRESPONDING FIELDS OF TABLE gt_mseg
    FOR ALL ENTRIES IN gt_prps
    WHERE mat_pspnr = gt_prps-pspnr
    AND   budat_mkpf IN s_budat
     AND   bwart IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .

*    MOVE-CORRESPONDING GT_MSEG TO GT_MSEG1.
*
*    SORT GT_MSEG1 BY WERKS MATNR MAT_PSPNR.
*    DELETE ADJACENT DUPLICATES FROM GT_MSEG1 COMPARING WERKS MATNR MAT_PSPNR.
*
*    SORT  GT_MSEG BY MAT_PSPNR MJAHR MBLNR ZEILE  .

    IF gt_mseg IS NOT INITIAL.
      SELECT * FROM ztdtz_ck
     INTO TABLE gt_ztdtz_ck
     FOR ALL ENTRIES IN gt_mseg
     WHERE mjahr = gt_mseg-mjahr
     AND   mblnr = gt_mseg-mblnr
     AND   zeile = gt_mseg-zeile
     AND   td_grpflag NE '' .

      SORT gt_ztdtz_ck BY pspid posid td_matnr td_grpflag  .
      gt_ztdtz_ck_z = gt_ztdtz_ck .
      SORT gt_ztdtz_ck_z BY pspid posid  mjahr mblnr zeile td_matnr td_grpflag  .
      "    DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_CK COMPARING PSPID POSID  MJAHR MBLNR ZEILE TD_MATNR TD_GRPFLAG.
      DELETE ADJACENT DUPLICATES FROM gt_ztdtz_ck COMPARING pspid posid   td_matnr td_grpflag.

      SELECT * FROM ztdtz_ck
     APPENDING TABLE gt_ztdtz_ck
     FOR ALL ENTRIES IN gt_mseg
     WHERE mjahr = gt_mseg-mjahr
     AND   mblnr = gt_mseg-mblnr
     AND   zeile = gt_mseg-zeile
     AND   td_grpflag EQ '' .

      SORT gt_ztdtz_ck BY mjahr mblnr zeile .


    ENDIF.


  ENDIF.

  "add ZPSCOST ：预算成本测算维护产值项 by it02 20161122
  SELECT * INTO TABLE gt_zpscost
      FROM zpscost
      WHERE  zpspid IN s_pspid
      .

  SORT gt_zpscost BY zpspid .

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

  DATA l_xmwzck         TYPE mseg-menge.
  "统计出库数量申请数量
  DATA:pspid TYPE ps_pspid .
  DATA:posid TYPE ps_posid .
  DATA:pspnr TYPE prps-pspnr,
       post1 TYPE proj-post1,
       zhtje TYPE proj-zhtje.

  LOOP AT gt_mseg INTO gs_mseg.
    CLEAR:gs_ck .
    AT NEW mat_pspnr .
      READ TABLE gt_prps INTO gs_prps
        WITH KEY pspnr = gs_mseg-mat_pspnr
          BINARY SEARCH .
      IF sy-subrc EQ 0 .
        posid = gs_prps-posid.
        READ TABLE gt_proj INTO gs_proj
            WITH KEY pspnr = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid .
        ENDIF.
      ENDIF.
    ENDAT.
    gs_ck-pspid = pspid.
    gs_ck-posid = posid.
    IF gs_mseg-shkzg EQ 'S'.
      gs_mseg-menge = gs_mseg-menge * -1 .
    ENDIF.
    READ TABLE gt_ztdtz_ck INTO gs_ztdtz_ck
    WITH KEY  mjahr = gs_mseg-mjahr
              mblnr = gs_mseg-mblnr
              zeile = gs_mseg-zeile
              matnr = gs_mseg-matnr
              BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_ck-matnr = gs_ztdtz_ck-td_matnr.
      gs_ck-menge = gs_ztdtz_ck-td_menge .
      IF gs_ztdtz_ck-shkzg EQ 'S'.
        gs_ck-menge = gs_ztdtz_ck-td_menge  * -1 .
      ENDIF.
      COLLECT gs_ck INTO gt_ck .
      IF gs_ztdtz_ck-td_grpflag NE ''.
        LOOP AT gt_ztdtz_ck_z INTO gs_ztdtz_ck_z
               WHERE pspid = gs_ztdtz_ck-pspid
               AND   posid = gs_ztdtz_ck-posid
*                        AND   MJAHR = GS_ZTDTZ_CK-MJAHR
*                        AND   MBLNR = GS_ZTDTZ_CK-MBLNR
               AND   td_matnr = gs_ztdtz_ck-td_matnr
               AND   td_grpflag = gs_ztdtz_ck-td_grpflag .

          DELETE gt_mseg WHERE mblnr = gs_ztdtz_ck_z-mblnr
                            AND mjahr = gs_ztdtz_ck_z-mjahr
                            AND zeile = gs_ztdtz_ck_z-zeile.


        ENDLOOP.
      ENDIF.
    ELSE.
      gs_ck-matnr = gs_mseg-matnr.
      gs_ck-menge = gs_mseg-menge .
      COLLECT gs_ck INTO gt_ck .
      CONTINUE .
    ENDIF.
  ENDLOOP.

  SORT gt_ck BY pspid posid matnr .

  SELECT * FROM makt
 INTO CORRESPONDING FIELDS OF TABLE gt_makt
 FOR ALL ENTRIES IN gt_ck
 WHERE matnr = gt_ck-matnr.

  SORT gt_makt BY matnr .

  LOOP AT  gt_ck INTO  gs_ck .
    AT  NEW  pspid.
      CLEAR:pspid,posid,zhtje.
      READ TABLE gt_proj INTO gs_proj
     WITH KEY pspid = gs_ck-pspid.
      IF sy-subrc EQ 0 .
        pspid  = gs_proj-pspid.
        post1  = gs_proj-post1.
        zhtje  = gs_proj-zhtje.
      ENDIF.
      CLEAR:e_wbs_ecp,
            ls_e_wbs_ecp,
            ls_cost_lines_100.

      REFRESH:lt_e_wbs_ecp,
              lt_cost_lines_100.

      CALL FUNCTION 'CNECP_READ'
        EXPORTING
          i_proj_def    = pspid
          i_version     = '100'
        IMPORTING
          e_wbs_ecp     = e_wbs_ecp
        EXCEPTIONS
          error_message = 1.

      lt_e_wbs_ecp = e_wbs_ecp.

      IF  lt_e_wbs_ecp IS NOT INITIAL.
        READ TABLE lt_e_wbs_ecp INTO ls_e_wbs_ecp
        INDEX 1.
        IF sy-subrc = 0.
          lt_cost_lines_100 = ls_e_wbs_ecp-cost_lines.
          DELETE lt_cost_lines_100 WHERE  typps NE 'M' .

        ENDIF.
      ENDIF.
      CLEAR:e_wbs_ecp,
              ls_e_wbs_ecp,
              ls_cost_lines_000.

      REFRESH:lt_e_wbs_ecp,
               lt_cost_lines_000.

      CALL FUNCTION 'CNECP_READ'
        EXPORTING
          i_proj_def    = pspid
          i_version     = '000'
        IMPORTING
          e_wbs_ecp     = e_wbs_ecp
        EXCEPTIONS
          error_message = 1.

      lt_e_wbs_ecp = e_wbs_ecp.
      IF  lt_e_wbs_ecp IS NOT INITIAL.
        READ TABLE lt_e_wbs_ecp INTO ls_e_wbs_ecp
        INDEX 1.
        IF sy-subrc = 0.
          lt_cost_lines_000 = ls_e_wbs_ecp-cost_lines.

          DELETE lt_cost_lines_000  WHERE typps NE 'M'.
        ENDIF.
      ENDIF.
    ENDAT.
    "项目号
    CLEAR:gs_data.
    gs_data-pspid = pspid.
    "项目描述
    gs_data-post1 = post1.
    "合同总金额
    gs_data-zhtje = zhtje.
    "物料号
    gs_data-matnr = gs_ck-matnr.
    " *物料描述
    READ TABLE gt_makt INTO gs_makt
    WITH KEY matnr = gs_data-matnr
             spras = sy-langu.
    IF sy-subrc = 0.
      gs_data-maktx   = gs_makt-maktx.
    ENDIF.
    " *设计代码
    READ TABLE gt_zmm024 INTO gs_zmm024
    WITH KEY posid = gs_data-pspid
             matnr = gs_data-matnr.
    IF sy-subrc = 0.
      gs_data-sjdm = gs_zmm024-sjdm.
    ENDIF.
    "*物料综合单价
    PERFORM wlzhdj_js  USING  gs_data-pspid gs_data-matnr CHANGING  gs_data-gpreis .
    " **人工预算单价

    PERFORM wlzhdj_js1  USING  gs_data-pspid gs_data-matnr CHANGING  gs_data-fpreis .

    "项目物资出库量
    gs_data-xmwzck = gs_ck-menge .


    "  *人工单价
    READ TABLE gt_prps INTO gs_prps
     WITH KEY posid = gs_ck-posid .
    IF sy-subrc EQ  0 .
      READ TABLE gt_cg WITH KEY  ps_psp_pnr = gs_prps-pspnr
                         matnr = gs_data-matnr
                         BINARY SEARCH .
      IF sy-subrc EQ 0 .

        gs_data-kbetr = gt_cg-netpr.
      ENDIF.
    ENDIF.
*产值
    gs_data-cz = gs_data-xmwzck * gs_data-gpreis.

*劳务月度进度款(预算)
    gs_data-lwydjdk = gs_data-xmwzck * gs_data-fpreis.

*劳务月度进度款
    gs_data-lwydjdk1 = gs_data-xmwzck * gs_data-kbetr.

    "出库替代
    READ TABLE gt_ztdtz_ck INTO gs_ztdtz_ck WITH KEY pspid = gs_data-pspid td_matnr = gs_data-matnr .
    IF sy-subrc EQ 0 .
      gs_data-cktd = '是'.
    ELSE.
      gs_data-cktd = '否'.
    ENDIF.

    APPEND gs_data TO gt_data.
    CLEAR gs_data.

  ENDLOOP.

  SORT gt_data BY pspid  matnr.

  LOOP AT gt_zpscost INTO gs_zpscost.
    CLEAR:gs_data.
    gs_data-pspid = gs_zpscost-zpspid.

    READ TABLE gt_proj INTO gs_proj WITH KEY pspid = gs_data-pspid.
    IF sy-subrc EQ 0.
      gs_data-post1 = gs_proj-post1.
    ENDIF.

    gs_data-maktx = '产值'.
    gs_data-zcsxm = gs_zpscost-zcsxm * gs_zpscost-zcsxm2 / 100 .
    gs_data-zxfc = gs_zpscost-zxfc   * gs_zpscost-zxfc2 / 100.
    gs_data-zgf = gs_zpscost-zgf   * gs_zpscost-zgf2 / 100.
    gs_data-zsj = gs_zpscost-zsj   * gs_zpscost-zsj2 / 100.
    gs_data-zzlj = gs_zpscost-zzlj  * gs_zpscost-zzlj2 / 100.
    gs_data-zqtfy = gs_zpscost-zqtfy  * gs_zpscost-zqtfy2 / 100 .
    gs_data-cz   = gs_data-zcsxm + gs_data-zxfc + gs_data-zgf
                  + gs_data-zsj + gs_data-zzlj + gs_data-zqtfy.
    APPEND gs_data TO gt_data.
  ENDLOOP.

  SORT gt_data BY pspid matnr .



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
*  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'PSPID'        '项目编号'         '' '' '' '' 'X' '' ''.
  init_fieldcat 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
  init_fieldcat 'MAT_PSPNR'   'WBS'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'GPREIS'       '物料综合单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'FPREIS'       '人工预算单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'KBETR'        '人工单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'XMWZCK'       '项目物资出库量'         '' '' '' '' '' '' ''.
  init_fieldcat 'CZ'           '产值'         '' '' '' '' '' '' ''.
  init_fieldcat 'LWYDJDK'      '劳务月度进度款（预算）'         '' '' '' '' '' '' ''.
  init_fieldcat 'LWYDJDK1'     '劳务月度进度款'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZHTJE'       '合同金额'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'CKTD'         '出库替代'         '' '' '' '' '' '' '' .
  init_fieldcat 'ZCSXM'     '措施项目'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZXFC'     '小辅材'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZGF'     '规费'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZSJ'     '税金'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZZLJ'     '暂列金'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZQTFY'     '其他费用'         '' '' '' '' '' '' ''.
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
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING rt_extab.
ENDFORM.                    "

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

  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND
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
*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DATA_PSPID  text
*      <--P_GS_DATA_GPREIS  text
*----------------------------------------------------------------------*
FORM wlzhdj_js  USING    p_pspid  LIKE proj-pspid
                         p_matnr  LIKE mseg-matnr
                CHANGING p_gpreis LIKE ckis-gpreis.
*  CLEAR:E_WBS_ECP,
*            LS_E_WBS_ECP,
*            LS_COST_LINES_100,
*            P_GPREIS.
*
*  REFRESH:LT_E_WBS_ECP,
*          LT_COST_LINES.
*
*  CALL FUNCTION 'CNECP_READ'
*    EXPORTING
*      I_PROJ_DEF    = P_PSPID
*      I_VERSION     = '100'
*    IMPORTING
*      E_WBS_ECP     = E_WBS_ECP
*    EXCEPTIONS
*      ERROR_MESSAGE = 1.
*
*  LT_E_WBS_ECP = E_WBS_ECP.
*
*  IF  LT_E_WBS_ECP IS NOT INITIAL.
*    READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*    INDEX 1.
*    IF SY-SUBRC = 0.
*      LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

  READ TABLE   lt_cost_lines_100 INTO ls_cost_lines_100
  WITH KEY matnr = p_matnr.
  IF sy-subrc = 0.
    p_gpreis = ls_cost_lines_100-gpreis.
  ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DATA_PSPID  text
*      <--P_GS_DATA_GPREIS  text
*----------------------------------------------------------------------*
FORM wlzhdj_js1  USING    p_pspid  LIKE proj-pspid
                         p_matnr  LIKE mseg-matnr
                CHANGING p_fpreis LIKE ckis-fpreis.
*  CLEAR:E_WBS_ECP,
*            LS_E_WBS_ECP,
*            LS_COST_LINES,
*            P_FPREIS.
*
*  REFRESH:LT_E_WBS_ECP,
*          LT_COST_LINES.
*
*  CALL FUNCTION 'CNECP_READ'
*    EXPORTING
*      I_PROJ_DEF    = P_PSPID
*      I_VERSION     = '000'
*    IMPORTING
*      E_WBS_ECP     = E_WBS_ECP
*    EXCEPTIONS
*      ERROR_MESSAGE = 1.
*
*  LT_E_WBS_ECP = E_WBS_ECP.
*
*  IF  LT_E_WBS_ECP IS NOT INITIAL.
*    READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*    INDEX 1.
*    IF SY-SUBRC = 0.
*      LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

  READ TABLE   lt_cost_lines_000 INTO ls_cost_lines_000
  WITH KEY  "TYPPS = 'M'
            matnr = p_matnr.
  IF sy-subrc = 0.
    p_fpreis = ls_cost_lines_000-fpreis.
  ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.
