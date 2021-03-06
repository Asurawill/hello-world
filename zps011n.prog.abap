REPORT zps011n.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : IT02 （hand)
*& Create date   : 2015/09/8
*& Request       :
*& Descriptions  : 项目产值明细信息表
*&
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2016-05-06  IT02&WEIYUN                   优化运行速度、新增替代折算
*2017-04-01  IT02&WEIYUN    ED1K905330     APPEND:自定义费用的产值信息
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

TABLES:proj,prps,mara,makt,zmm024,mseg.
DATA:BEGIN OF gs_data,
       pspid      TYPE proj-pspid,      "项目编号
       post1      TYPE proj-post1,      "项目名称
       ps_psp_pnr TYPE mseg-ps_psp_pnr, "WBS
       budat_mkpf TYPE mseg-budat_mkpf, "过账日期
       bwart      TYPE mseg-bwart,     "移动类型
       wempf      TYPE mseg-wempf,      "劳务方
       lifnr      TYPE mseg-lifnr,      "劳务供应商编号
       lifnr_name TYPE lfa1-name1 ,     "劳务供应商名称
       sjdm       TYPE zmm024-sjdm,     "设计代码
       matnr      TYPE mseg-matnr,      "物料号
       maktx      TYPE makt-maktx,      "物料描述
       gpreis     TYPE ckis-gpreis,     "物料综合单价
       " FPREIS     TYPE CKIS-FPREIS,     "人工预算单价
       kbetr      TYPE konv-kbetr,      "人工单价
       ckl        TYPE mseg-menge,      "出库量
       cz         TYPE ckis-gpreis,     "产值
       lwjdk      TYPE ckis-gpreis,     "劳务进度款
       menge      TYPE menge_d,          "出库量
       " LWYDJDK    TYPE CKIS-GPREIS,     "劳务月度进度款
       " LWYDJDK1   TYPE CKIS-GPREIS,     "劳务月度进度款(预算)
       zhtje      TYPE proj-zhtje,      "合同金额
       td_matnr   TYPE matnr,            "被替代物料
       td_maktx   TYPE maktx,            "被替代物料描述
       td_meins   TYPE meins,              "被替代单位
       td_grpflag TYPE ztdtz_pr-td_grpflag, "替代组标识
       td_menge   TYPE menge_d,          "折算数量
       yjwl       TYPE matnr,           "应计物料
       yjwlms     TYPE maktx,            "应计物料描述
       yjsl       TYPE menge_d,          "应计数量
       zcsxm      TYPE zpscost-zcsxm,    "措施项目
       zxfc       TYPE zpscost-zxfc,     "小辅材
       zgf        TYPE zpscost-zgf,      "规费
       zsj        TYPE zpscost-zsj,     "税金
       zzlj       TYPE zpscost-zzlj,    "暂列金
       zqtfy      TYPE zpscost-zqtfy,   "其他费用
       sel(1),
     END OF gs_data.
DATA:BEGIN OF gs_mseg,
       mat_pspnr  TYPE ps_psp_pnr ,  "WBS号
       mjahr      TYPE mseg-mjahr,
       mblnr      TYPE mseg-mblnr,
       zeile      TYPE mseg-zeile,
       matnr      TYPE mseg-matnr,
       menge      TYPE menge_d,
       werks      TYPE werks,
       lifnr      TYPE lifnr,
       budat_mkpf TYPE mseg-budat_mkpf,           "过账日期
       bwart      TYPE mseg-bwart,
       shkzg      TYPE shkzg,
       wempf      TYPE mseg-wempf,
     END OF gs_mseg.

DATA:BEGIN OF gs_cg,

       ps_psp_pnr TYPE ekkn-ps_psp_pnr,
       matnr      TYPE ekpo-matnr,
       ebeln      TYPE ekkn-ebeln,
       ebelp      TYPE ekkn-ebelp,
       netpr      TYPE ekpo-netpr,
     END OF gs_cg .

DATA:gt_cg LIKE TABLE OF gs_cg WITH HEADER LINE .
DATA:gt_mseg1_sum  LIKE TABLE OF gs_mseg WITH HEADER LINE.
DATA:gt_mseg_fl LIKE TABLE OF mseg WITH HEADER LINE.
DATA: gt_data LIKE TABLE OF gs_data  WITH HEADER LINE.

DATA: gt_proj LIKE TABLE OF proj WITH HEADER LINE.

DATA: gt_prps LIKE TABLE OF prps WITH HEADER LINE.

DATA:gt_mseg LIKE TABLE OF gs_mseg WITH HEADER LINE .

DATA: gt_makt  LIKE  TABLE OF makt WITH HEADER  LINE.

DATA: gt_ekkn LIKE TABLE OF ekkn WITH HEADER LINE.

DATA: gt_ekpo  LIKE TABLE OF ekpo WITH HEADER LINE.

DATA: gt_konv LIKE TABLE OF konv  WITH HEADER LINE.

DATA: gt_zpsprod LIKE TABLE OF zpsprod WITH HEADER LINE.

DATA: gt_zmm024 LIKE TABLE OF zmm024 WITH HEADER LINE.

DATA: gt_ekko LIKE TABLE OF ekko WITH HEADER LINE.

DATA: gt_t156t LIKE TABLE OF t156t WITH HEADER LINE.

DATA:gt_lfa1 LIKE TABLE OF lfa1 WITH HEADER LINE.

DATA:gt_ztdtz_ck TYPE TABLE OF ztdtz_ck,
     gs_ztdtz_ck TYPE ztdtz_ck.

DATA:gt_ztdtz_ck_z TYPE TABLE OF ztdtz_ck,
     gs_ztdtz_ck_z TYPE ztdtz_ck.

DATA:gt_zpscost TYPE TABLE OF zpscost,
     gs_zpscost TYPE zpscost.

RANGES:r_bwart FOR mseg-bwart.
DATA e_wbs_ecp        TYPE tty_proj_element_ck_items_rdex.
DATA lt_e_wbs_ecp     TYPE TABLE OF proj_element_ck_items_rdexp.
DATA ls_e_wbs_ecp     TYPE proj_element_ck_items_rdexp.
DATA lt_cost_lines    TYPE TABLE OF kis1.
DATA ls_cost_lines    TYPE kis1.
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
               s_budat FOR mseg-budat_mkpf, "过账日期
               s_lifnr FOR mseg-lifnr.       "供应商编码
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*CLEAR :R_BWART.
*"('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .
*R_BWART-SIGN = 'I'.
*R_BWART-OPTION = 'EQ'.
*R_BWART-LOW = '281'.
*APPEND R_BWART.
*R_BWART-LOW = '282'.
*APPEND R_BWART.
*R_BWART-LOW = '221'.
*APPEND R_BWART.
*R_BWART-LOW = '222'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z19'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z20'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z21'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z22'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z23'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z24'.
*APPEND R_BWART.

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
  SORT  gt_proj BY pspnr pspid .
  CHECK gt_proj[] IS NOT INITIAL.

  SELECT * FROM prps
     INTO CORRESPONDING FIELDS OF TABLE gt_prps
     FOR ALL ENTRIES IN gt_proj
     WHERE psphi = gt_proj-pspnr.
  SORT gt_prps BY pspnr posid.

  SELECT * FROM zmm024
       INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
       FOR ALL ENTRIES IN gt_proj
       WHERE posid = gt_proj-pspid.
  SORT gt_zmm024 BY matnr posid.

  SELECT * FROM zpsprod
   INTO CORRESPONDING FIELDS OF TABLE gt_zpsprod
   FOR ALL ENTRIES IN gt_proj
   WHERE pspid = gt_proj-pspid.
  SORT gt_zpsprod BY pspid.
  CHECK gt_prps[] IS NOT INITIAL.
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

*    SELECT * FROM EKKN
*    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
*     FOR ALL ENTRIES IN GT_PRPS
*     WHERE PS_PSP_PNR  = GT_PRPS-PSPNR
*     AND  LOEKZ <> 'L'.
*SORT GT_EKKN BY PS_PSP_PNR .
*CHECK GT_EKKN[] IS NOT INITIAL.



*SELECT * FROM EKPO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
*      FOR ALL ENTRIES IN GT_EKKN
*      WHERE EBELN = GT_EKKN-EBELN
*      AND   EBELP = GT_EKKN-EBELP
*      AND   LOEKZ <> 'L'
*      AND   KNTTP = 'Y'.
*SORT GT_EKPO BY EBELN EBELP.
*      SELECT * FROM EKKO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
*      FOR ALL ENTRIES IN GT_EKPO
*      WHERE EBELN = GT_EKPO-EBELN.
*SORT GT_EKKO BY EBELN .
*      SELECT * FROM KONV
*      INTO CORRESPONDING FIELDS OF TABLE GT_KONV
*      FOR ALL ENTRIES IN GT_EKKO
*      WHERE KNUMV = GT_EKKO-KNUMV
*        AND STUNR = '001'.
*SORT GT_KONV BY KNUMV KPOSN.
  IF gt_prps[] IS NOT INITIAL.
    SELECT mjahr mblnr zeile matnr
        menge werks lifnr budat_mkpf
        bwart shkzg  wempf mat_pspnr
   INTO CORRESPONDING FIELDS OF TABLE gt_mseg
   FROM mseg
   FOR ALL ENTRIES IN gt_prps
   WHERE mat_pspnr = gt_prps-pspnr
   AND budat_mkpf IN s_budat
  AND   bwart IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' )
   AND lifnr IN  s_lifnr .

    SORT gt_mseg  BY mat_pspnr budat_mkpf matnr.

  ENDIF.

  CHECK gt_mseg[] IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN gt_mseg
    WHERE lifnr = gt_mseg-lifnr .
  SORT gt_lfa1 BY  lifnr .

  SELECT * FROM makt
    INTO CORRESPONDING FIELDS OF TABLE gt_makt
    WHERE  spras = '1'.
  SORT gt_makt BY matnr .

  SELECT * INTO TABLE gt_ztdtz_ck
      FROM ztdtz_ck
      FOR ALL ENTRIES IN gt_mseg
      WHERE mjahr = gt_mseg-mjahr
       AND  mblnr = gt_mseg-mblnr
       AND  zeile = gt_mseg-zeile
       AND td_grpflag NE ''.
  SORT gt_ztdtz_ck BY pspid posid td_matnr td_grpflag.
  gt_ztdtz_ck_z =  gt_ztdtz_ck .
  SORT gt_ztdtz_ck_z BY pspid posid mjahr  mblnr .
  " DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_CK COMPARING PSPID POSID MJAHR MBLNR .
  DELETE ADJACENT DUPLICATES FROM gt_ztdtz_ck COMPARING pspid posid td_matnr td_grpflag  .

  SELECT * APPENDING TABLE gt_ztdtz_ck
    FROM ztdtz_ck
    FOR ALL ENTRIES IN gt_mseg
    WHERE mjahr = gt_mseg-mjahr
    AND   mblnr = gt_mseg-mblnr
    AND   zeile = gt_mseg-zeile
    AND   td_grpflag EQ ''.
  SORT gt_ztdtz_ck BY mjahr mblnr zeile .

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
  DATA:pspid TYPE proj-pspid,
       post1 TYPE proj-post1,
       zhtje TYPE proj-zhtje,
       pspnr TYPE prps-pspnr.

  LOOP AT gt_mseg.
    AT NEW mat_pspnr.
      CLEAR:pspid,post1,zhtje,pspnr.
      READ TABLE gt_prps WITH KEY pspnr = gt_mseg-mat_pspnr BINARY SEARCH .
      IF sy-subrc EQ 0 .
        pspnr = gt_prps-pspnr.
        READ TABLE  gt_proj WITH KEY pspnr = gt_prps-psphi.
        IF sy-subrc EQ 0.
          pspid = gt_proj-pspid.
          post1 = gt_proj-post1. "项目名称
          zhtje = gt_proj-zhtje.  "合同金额
        ENDIF.
      ENDIF.

      CLEAR:e_wbs_ecp,
            ls_e_wbs_ecp,
            ls_cost_lines.
      .

      REFRESH:lt_e_wbs_ecp,
              lt_cost_lines.

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
          lt_cost_lines = ls_e_wbs_ecp-cost_lines.
          DELETE lt_cost_lines WHERE  typps NE 'M' .
        ENDIF.
      ENDIF.
    ENDAT.
    CLEAR:gs_data.
    gs_data-pspid = pspid. "项目号
    gs_data-post1 = post1. "项目名称
    gs_data-zhtje = zhtje.  "合同金额

    gs_data-ps_psp_pnr = pspnr.
    "物料号
    gs_data-matnr = gt_mseg-matnr.
    "移动类型
    gs_data-bwart = gt_mseg-bwart .

    "物料描述
    READ TABLE gt_makt WITH KEY matnr = gs_data-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      gs_data-maktx = gt_makt-maktx.
    ENDIF.
    "过账日期
    gs_data-budat_mkpf = gt_mseg-budat_mkpf .
    "凭证行数量
    IF  gt_mseg-shkzg = 'S'.
      gs_data-menge = gt_mseg-menge * -1 .
    ELSE.
      gs_data-menge = gt_mseg-menge .
    ENDIF.
    "应计物料、应计物料描述、应计数量
    gs_data-yjwl = gs_data-matnr.
    gs_data-yjwlms = gs_data-maktx.
    gs_data-yjsl = gs_data-menge .

    "劳务方
    gs_data-wempf = gt_mseg-wempf.
    "劳务供应商编号
    gs_data-lifnr = gt_mseg-lifnr .
    READ TABLE gt_lfa1 WITH KEY  lifnr = gt_mseg-lifnr .
    IF sy-subrc = 0.
      gs_data-lifnr_name = gt_lfa1-name1.
    ENDIF.

    "替代物料
    READ TABLE gt_ztdtz_ck  INTO gs_ztdtz_ck
      WITH KEY mjahr = gt_mseg-mjahr
               mblnr = gt_mseg-mblnr
               zeile = gt_mseg-zeile
               matnr = gs_data-matnr
            BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-td_matnr = gs_ztdtz_ck-td_matnr .
      gs_data-td_menge = gs_ztdtz_ck-td_menge.
      gs_data-td_grpflag = gs_ztdtz_ck-td_grpflag.
      IF gs_ztdtz_ck-shkzg EQ 'S'.
        gs_data-td_menge = gs_data-td_menge * -1 .
      ENDIF.
      gs_data-td_meins = gs_ztdtz_ck-meins.
      READ TABLE gt_makt
       WITH KEY matnr = gs_data-td_matnr
           BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-td_maktx = gt_makt-maktx .
      ENDIF.
      "应计物料、应计物料描述 、应计数量
      gs_data-yjwl   =  gs_data-td_matnr.
      gs_data-yjwlms =  gs_data-td_maktx .
      gs_data-yjsl   =  gs_data-td_menge .

    ELSE.
      READ TABLE gt_ztdtz_ck_z INTO gs_ztdtz_ck_z
          WITH KEY mjahr = gt_mseg-mjahr
                   mblnr = gt_mseg-mblnr
                   zeile = gt_mseg-zeile
                   BINARY SEARCH .
      IF sy-subrc EQ 0  .
        "物料综合单价
        "   PERFORM WLZHDJ_JS  USING  GS_DATA-PSPID GS_ZTDTZ_CK_Z-TD_MATNR CHANGING  GS_DATA-GPREIS .
        READ TABLE gt_ztdtz_ck INTO gs_ztdtz_ck
             WITH KEY mjahr = gt_mseg-mjahr
                      mblnr = gt_mseg-mblnr
                      zeile = gt_mseg-zeile
                      BINARY SEARCH.
        IF sy-subrc NE 0.
          gs_data-td_matnr = gs_ztdtz_ck_z-td_matnr.
          gs_data-td_menge = gs_ztdtz_ck_z-td_menge.
          gs_data-td_meins = gs_ztdtz_ck_z-meins.
          gs_data-td_grpflag = gs_ztdtz_ck_z-td_grpflag.
          READ TABLE gt_makt
             WITH KEY matnr = gs_data-td_matnr
             BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_data-td_maktx = gt_makt-maktx .
          ENDIF.
          "应计物料
          gs_data-yjwl   =  gs_data-td_matnr.
          gs_data-yjwlms =  gs_data-td_maktx .
          gs_data-yjsl   =  0 .
        ENDIF.
      ENDIF.
    ENDIF.

    "出库量  : = 应计物料的应计数量
    gs_data-ckl = gs_data-yjsl .
*人工单价
*  LOOP AT  GT_EKKN
*    WHERE  PS_PSP_PNR = GS_DATA-PS_PSP_PNR.
*      READ TABLE GT_EKPO
*      WITH KEY EBELN = GT_EKKN-EBELN
*               EBELP = GT_EKKN-EBELP
*               MATNR = GS_DATA-YJWL
*               KNTTP = 'Y'.
*      IF SY-SUBRC = 0.
*        READ TABLE GT_EKKO
*        WITH KEY EBELN = GT_EKPO-EBELN.
*        IF SY-SUBRC = 0.
*          READ TABLE GT_KONV
*          WITH KEY KNUMV = GT_EKKO-KNUMV
*                   KPOSN = GT_EKPO-EBELP
*                   STUNR = 1.
*          IF SY-SUBRC = 0.
*            GS_DATA-KBETR = GT_KONV-KBETR.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
    " *人工单价
    READ TABLE gt_cg WITH KEY  ps_psp_pnr = gs_data-ps_psp_pnr
                               matnr = gs_data-yjwl
                               BINARY SEARCH .
    IF sy-subrc EQ 0 .

      gs_data-kbetr = gt_cg-netpr.
    ENDIF.
    "设计代码
    READ TABLE gt_zmm024 WITH KEY  matnr = gs_data-yjwl posid = gs_data-pspid BINARY SEARCH .
    IF sy-subrc = 0.
      gs_data-sjdm = gt_zmm024-sjdm.
    ENDIF.
    "物料综合单价
    PERFORM wlzhdj_js  USING  gs_data-pspid gs_data-yjwl CHANGING  gs_data-gpreis .

    "产值
    gs_data-cz = gs_data-ckl * gs_data-gpreis.
    " 劳务进度款
    gs_data-lwjdk = gs_data-ckl * gs_data-kbetr.
    APPEND gs_data TO gt_data.
    CLEAR gs_data.
  ENDLOOP.

  LOOP AT gt_zpsprod  .
    CLEAR gs_data.
    gs_data-pspid = gt_zpsprod-pspid.

    SELECT SINGLE post1
    INTO gs_data-post1 FROM proj
    WHERE pspid = gs_data-pspid.

    gs_data-maktx = '期初项目产值、劳务进度款'.
    gs_data-cz    = gt_zpsprod-zcz.
    gs_data-lwjdk = gt_zpsprod-zlwk.

    APPEND gs_data TO gt_data.
    CLEAR:gs_data,
         gt_data.
  ENDLOOP.

  LOOP AT gt_zpscost INTO gs_zpscost.
    CLEAR:gs_data.
    gs_data-pspid = gs_zpscost-zpspid.

    READ TABLE gt_proj WITH KEY pspid = gs_data-pspid.
    IF sy-subrc EQ 0.
      gs_data-post1 = gt_proj-post1.
    ENDIF.

    gs_data-maktx = '产值'.
    gs_data-zcsxm = gs_zpscost-zcsxm * gs_zpscost-zcsxm2 / 100 .
    gs_data-zxfc = gs_zpscost-zxfc * gs_zpscost-zxfc2 / 100.
    gs_data-zgf = gs_zpscost-zgf  * gs_zpscost-zgf2 / 100.
    gs_data-zsj = gs_zpscost-zsj * gs_zpscost-zsj2 / 100.
    gs_data-zzlj = gs_zpscost-zzlj * gs_zpscost-zzlj2 / 100.
    gs_data-zqtfy = gs_zpscost-zqtfy * gs_zpscost-zqtfy2 / 100 .
    gs_data-cz   = gs_data-zcsxm + gs_data-zxfc + gs_data-zgf
                  + gs_data-zsj + gs_data-zzlj + gs_data-zqtfy.
    APPEND gs_data TO gt_data.
  ENDLOOP.
  SORT gt_data BY pspid budat_mkpf matnr.

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
  gw_layout-box_fname     = 'SEL'.
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
  " INIT_FIELDCAT 'PS_PSP_PNR'   'WBS'         '' '' '' '' '' '' ''.
  init_fieldcat 'BUDAT_MKPF'   '过账日期'         '' '' '' '' '' '' ''.
  init_fieldcat 'BWART'   '移动类型'         '' '' '' '' '' '' ''.
  init_fieldcat 'WEMPF'   '劳务方'         '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR'   '劳务供应商编号'         '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR_NAME'   '劳务供应商名称'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat  'MENGE'       '物料凭证数量'     '' '' '' '' '' '' ''.
  init_fieldcat 'TD_MATNR'     '替代物料号'         '' '' '' '' '' '' ''.
  init_fieldcat 'TD_MAKTX'     '替代物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'TD_GRPFLAG'     '替代标识'         '' '' '' '' '' '' ''.
  init_fieldcat 'TD_MEINS'     '替代单位'         '' '' '' '' '' '' ''.
  init_fieldcat 'TD_MENGE'     '折算数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'GPREIS'       '物料综合单价'         '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'FPREIS'       '人工预算单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'YJWL'       '应计物料'         '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'YJWLMS'     '应计物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'YJSL'       '应计数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'KBETR'     '人工单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'CKL'       '出库量'         '' '' '' '' '' '' ''.
  init_fieldcat 'CZ'        '产值'         '' '' '' '' '' '' ''.
  init_fieldcat 'LWJDK'     '劳务进度款'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZHTJE'     '合同金额'         '' '' '' '' '' '' ''.
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
*      -->P_GT_DATA  text
*      -->P_1090   text
*      -->P_1091   text
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
*
*  CASE R_UCOMM.
*** 双击
**    WHEN '&IC1'.
**      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
**      CHECK SY-SUBRC = 0.
**      IF RS_SELFIELD-FIELDNAME = 'PSPID'
**        AND GS_DATA-PSPID IS NOT INITIAL.
**        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
**        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
**      ENDIF.
*
*  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND
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
*CLEAR:E_WBS_ECP,
*          LS_E_WBS_ECP,
*          LS_COST_LINES,
*          P_GPREIS.
*
*    REFRESH:LT_E_WBS_ECP,
*            LT_COST_LINES.
*
*    CALL FUNCTION 'CNECP_READ'
*      EXPORTING
*        I_PROJ_DEF    = P_PSPID
*        I_VERSION     = '100'
*      IMPORTING
*        E_WBS_ECP     = E_WBS_ECP
*      EXCEPTIONS
*        ERROR_MESSAGE = 1.
*
*    LT_E_WBS_ECP = E_WBS_ECP.
*
*        IF  LT_E_WBS_ECP IS NOT INITIAL.
*      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*      INDEX 1.
*      IF SY-SUBRC = 0.
*        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

  READ TABLE   lt_cost_lines INTO ls_cost_lines
  WITH KEY  matnr = p_matnr.
  IF sy-subrc = 0.
    p_gpreis = ls_cost_lines-gpreis.
  ENDIF.
*      ENDIF.
*    ENDIF.
ENDFORM.
