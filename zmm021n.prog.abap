REPORT zmm021n.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/9
*& Request       :
*& Descriptions  : 预算与累计申购数据对比报表
*&
*& Modify by     :IT02_魏云
*& Modify date   :20160506
*& Request       :
*& Descriptions  :新增替代折算
*&
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES:proj,prps,mara,zmm024.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        pspid     TYPE proj-pspid,    "项目定义
        post1     TYPE proj-post1,    "项目订单描述
        werks     TYPE proj-werks,    "工厂
        matnr     TYPE mara-matnr,    "物料号
        maktx     TYPE makt-maktx,    "物料描述
        matkl     TYPE mara-matkl,    "物料组
        wgbez     TYPE t023t-wgbez,   "物料组描述
        meins     TYPE mara-meins,    "计量单位
        posid1    TYPE prps-posid,    "WBS元素
        menge     TYPE ekpo-menge,    "预算数
        gpreis    TYPE ckis-gpreis,     "物料综合单价
        fpreis    TYPE ckis-fpreis,     "人工预算单价
        kbetr     TYPE konv-kbetr,      "人工单价
        wqrljsgsl TYPE ekpo-menge,    "未确认累计申购数量
        yqrljsgsl TYPE ekpo-menge,    "已去人累计申购数量
        syksqsl   TYPE ekpo-menge,    "剩余可申购数量
        ljcgxdsl  TYPE ekpo-menge,    "累计采购下达数量
        cgwxdsl   TYPE ekpo-menge,    "采购未下达数量
        qtdb      TYPE mseg-menge,     "其他调拨数量
        sjdm      TYPE zmm024-sjdm,   "设计代码
        prtd      TYPE c ,             "采购申请替代
        potd      TYPE c,              "采购订单替代
        dbtd      TYPE c,              "采购订单替代
*        WBS_TEXT  TYPE STRING,        "WBS传送文本字段
      END OF ty_data.

TYPES:BEGIN OF ty_ecp,
        pspnr  TYPE ps_intnr,
        werks  TYPE werks_d,
        matnr  TYPE matnr,
        meeht  TYPE meins,
        menge  TYPE menge_pos,
        fpreis TYPE ckis-fpreis,     "人工预算单价
      END OF ty_ecp.

TYPES:BEGIN OF ty_ecp_100,
        pspnr  TYPE ps_intnr,
        werks  TYPE werks_d,
        matnr  TYPE matnr,
        gpreis TYPE ckis-gpreis,     "物料综合单价
      END OF ty_ecp_100.

TYPES:BEGIN OF ty_td,
        "  PSPNR TYPE PS_INTNR,
        pspid TYPE ps_pspid,    "项目号
        posid TYPE ps_posid,    "WBS号
        werks TYPE werks_d,
        matnr TYPE matnr,
        menge TYPE menge_d,

      END OF ty_td.

TYPES :BEGIN OF ty_mseg,
         mjahr  TYPE mjahr,              "物料凭证年度
         mblnr  TYPE mblnr,              "物料凭证号
         zeile  TYPE mblpo,              "物料凭证行项目
         matnr  TYPE matnr,              "物料号
         menge  TYPE menge_d,            "数量
         meins  TYPE meins,              "单位
         werks  TYPE werks_d,            "工厂
         bwart  TYPE  bwart,             "移动类型
         shkzg  TYPE shkzg,              "借贷标识
         pspnr  TYPE ps_posnr,           "来源项目号
         pspnr1 TYPE ps_posnr,           "目标项目号
       END OF ty_mseg .

TYPES:BEGIN OF ty_cg,
        ebeln      TYPE ekkn-ebeln,
        ebelp      TYPE ekkn-ebelp,
        ps_psp_pnr TYPE ekkn-ps_psp_pnr,
        matnr      TYPE ekpo-matnr,
        netpr      TYPE ekpo-netpr,
      END OF ty_cg .

DATA:gt_cg TYPE TABLE OF ty_cg,
     gs_cg TYPE ty_cg.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA gt_data TYPE TABLE OF ty_data.
DATA gs_data TYPE ty_data.

DATA gt_proj  TYPE TABLE OF proj.
DATA gs_proj  TYPE proj.

DATA gt_prps  TYPE TABLE OF prps.
DATA gs_prps  TYPE prps.

DATA gt_makt  TYPE TABLE OF makt.
DATA gs_makt  TYPE makt.

DATA gt_t023t TYPE TABLE OF t023t.
DATA gs_t023t TYPE t023t.

DATA gt_ebkn  TYPE TABLE OF ebkn.
DATA gs_ebkn  TYPE  ebkn.

DATA gt_eban  TYPE TABLE OF eban.
DATA gs_eban  TYPE eban.

DATA gt_ekkn  TYPE TABLE OF ekkn.
DATA gs_ekkn  TYPE ekkn.

DATA gt_ekpo  TYPE TABLE OF ekpo.
DATA gs_ekpo  TYPE ekpo.

DATA gt_ecp   TYPE TABLE OF ty_ecp.
DATA gs_ecp   TYPE ty_ecp.

DATA gt_ecp_100   TYPE TABLE OF ty_ecp_100.
DATA gs_ecp_100   TYPE ty_ecp_100.


DATA gt_zmm009 TYPE TABLE OF zmm009.
DATA gs_zmm009 TYPE zmm009.

DATA gt_zmm024 TYPE TABLE OF zmm024.
DATA gs_zmm024 TYPE zmm024.

DATA:gt_ztdtz_pr TYPE TABLE OF ztdtz_pr,
     gs_ztdtz_pr TYPE ztdtz_pr.

DATA:gt_ztdtz_pr_z TYPE TABLE OF ztdtz_pr,
     gs_ztdtz_pr_z TYPE ztdtz_pr.

DATA:gt_zs_pr TYPE TABLE OF ty_td,
     gs_zs_pr TYPE ty_td.

DATA:gt_ys_pr TYPE TABLE OF ty_td,
     gs_ys_pr TYPE ty_td.

DATA:gt_po TYPE TABLE OF ty_td,
     gs_po TYPE ty_td.

DATA:gt_db TYPE TABLE OF ty_td,
     gs_db TYPE ty_td.

DATA:gt_ztdtz_po TYPE TABLE OF ztdtz_po,
     gs_ztdtz_po TYPE ztdtz_po.

DATA:gt_ztdtz_po_z TYPE TABLE OF ztdtz_po,
     gs_ztdtz_po_z TYPE ztdtz_po.

DATA:gt_ztdtz_db TYPE TABLE OF ztdtz_db,
     gs_ztdtz_db TYPE ztdtz_db.

DATA:gt_ztdtz_db_z TYPE TABLE OF ztdtz_db,
     gs_ztdtz_db_z TYPE ztdtz_db.


DATA:gt_mseg TYPE TABLE OF  ty_mseg,
     gs_mseg TYPE ty_mseg.


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
SELECT-OPTIONS:s_pspid FOR proj-pspid,    "项目定义
               s_werks FOR proj-werks,    "工厂
               s_matnr FOR mara-matnr,    "预算物料
               s_matkl FOR mara-matkl,    "物料组
               s_sjdm  FOR zmm024-sjdm.   "设计代码
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
   WHERE pspid IN s_pspid.

  SELECT * FROM zmm024
   INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
   FOR ALL ENTRIES IN gt_proj
   WHERE posid = gt_proj-pspid.

  IF gt_proj IS  NOT INITIAL.
    SELECT * FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_prps
    FOR ALL ENTRIES IN gt_proj
    WHERE psphi = gt_proj-pspnr.

    CHECK gt_prps IS NOT INITIAL.

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

    SELECT * FROM ebkn
    INTO CORRESPONDING FIELDS OF TABLE gt_ebkn
    FOR ALL ENTRIES IN gt_prps
    WHERE ps_psp_pnr = gt_prps-pspnr
    AND   loekz <> 'X'.

    SORT gt_ebkn BY ps_psp_pnr .
*
*    CHECK GT_EBKN IS NOT INITIAL.
    IF gt_ebkn IS NOT INITIAL .
      SELECT * FROM eban
      INTO CORRESPONDING FIELDS OF TABLE gt_eban
      FOR ALL ENTRIES IN gt_ebkn
      WHERE banfn = gt_ebkn-banfn
      AND   bnfpo = gt_ebkn-bnfpo
      AND   loekz <> 'X'
      AND   frgkz IN ('X','2').
      SORT gt_eban BY banfn  bnfpo  .

      SELECT * FROM ztdtz_pr
        INTO TABLE gt_ztdtz_pr
       FOR ALL ENTRIES IN gt_ebkn
        WHERE banfn = gt_ebkn-banfn
        AND   bnfpo = gt_ebkn-bnfpo
        AND   td_grpflag NE '' .
      SORT gt_ztdtz_pr BY pspid posid  td_matnr td_grpflag .

      gt_ztdtz_pr_z = gt_ztdtz_pr .
      SORT gt_ztdtz_pr BY pspid posid banfn bnfpo td_matnr td_grpflag .

      "  DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_PR COMPARING PSPID POSID BANFN  TD_MATNR TD_GRPFLAG.

      DELETE ADJACENT DUPLICATES FROM gt_ztdtz_pr COMPARING pspid posid td_matnr td_grpflag.

      SELECT * FROM ztdtz_pr
       APPENDING TABLE gt_ztdtz_pr
        FOR ALL ENTRIES IN gt_ebkn
        WHERE banfn = gt_ebkn-banfn
        AND   bnfpo = gt_ebkn-bnfpo
        AND   td_grpflag EQ '' .
      SORT gt_ztdtz_pr BY banfn bnfpo .
    ENDIF.

    SELECT * FROM ekkn
    INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
    FOR ALL ENTRIES IN gt_prps
    WHERE ps_psp_pnr = gt_prps-pspnr
    AND   loekz <> 'L'.

    SORT gt_ekkn BY ps_psp_pnr  .

*    CHECK GT_EKKN IS NOT INITIAL.

    IF gt_ekkn IS NOT INITIAL .
      SELECT * FROM ekpo
      INTO CORRESPONDING FIELDS OF TABLE gt_ekpo
      FOR ALL ENTRIES IN gt_ekkn
      WHERE ebeln = gt_ekkn-ebeln
      AND   ebelp = gt_ekkn-ebelp
      AND   ( knttp = 'Q'
      OR knttp = '' )
      AND   loekz <> 'L'.
      SORT  gt_ekpo BY ebeln ebelp .

      SELECT * FROM ztdtz_po
        INTO TABLE gt_ztdtz_po
         FOR ALL ENTRIES IN gt_ekpo
         WHERE ebeln = gt_ekpo-ebeln
         AND   ebelp = gt_ekpo-ebelp
         AND   td_grpflag NE '' .

      SORT gt_ztdtz_po BY pspid posid  td_matnr td_grpflag  .
      gt_ztdtz_po_z = gt_ztdtz_po .
      SORT gt_ztdtz_po_z BY pspid posid ebeln ebelp td_matnr td_grpflag  .
      "      DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_PO COMPARING PSPID POSID EBELN TD_MATNR TD_GRPFLAG.

      DELETE ADJACENT DUPLICATES FROM gt_ztdtz_po COMPARING pspid posid td_matnr td_grpflag.

      SELECT * FROM ztdtz_po
     APPENDING TABLE gt_ztdtz_po
    FOR ALL ENTRIES IN gt_ekpo
    WHERE ebeln = gt_ekpo-ebeln
     AND   ebelp = gt_ekpo-ebelp
     AND   td_grpflag EQ '' .

      SORT gt_ztdtz_po BY ebeln ebelp .

*      SELECT * FROM ZTDTZ_DB
*        INTO TABLE GT_ZTDTZ_DB
*        FOR
*         .
    ENDIF.

  ENDIF.

  SELECT * FROM zmm009
    INTO CORRESPONDING FIELDS OF TABLE gt_zmm009.

  SELECT mjahr mblnr zeile
         matnr menge meins
         werks bwart shkzg
         ps_psp_pnr AS pspnr
         mat_pspnr AS pspnr1
    INTO CORRESPONDING FIELDS OF TABLE gt_mseg
FROM mseg
FOR ALL  ENTRIES IN gt_prps
WHERE mat_pspnr EQ gt_prps-pspnr
AND bwart  IN ('315','316') .

  SORT gt_mseg BY pspnr1  mblnr mjahr zeile .

  SELECT mjahr mblnr zeile
        matnr menge meins
        werks bwart shkzg
        mat_pspnr AS pspnr
        ps_psp_pnr AS pspnr1
   APPENDING  CORRESPONDING FIELDS OF TABLE gt_mseg
FROM mseg
FOR ALL  ENTRIES IN gt_prps
WHERE ps_psp_pnr EQ gt_prps-pspnr
AND bwart  IN ('415')
AND shkzg EQ 'H' .

  SORT gt_mseg BY pspnr1  mblnr mjahr zeile .

  SELECT mjahr mblnr zeile
            matnr menge meins
            werks bwart shkzg
            mat_pspnr AS pspnr1
            ps_psp_pnr AS pspnr
       APPENDING  CORRESPONDING FIELDS OF TABLE gt_mseg
   FROM mseg
   FOR ALL  ENTRIES IN gt_prps
   WHERE ps_psp_pnr EQ gt_prps-pspnr
   AND bwart  IN ('416')
   AND shkzg EQ 'S' .

  SORT gt_mseg BY pspnr1  mblnr mjahr zeile .

  SELECT * INTO TABLE gt_ztdtz_db
    FROM ztdtz_db
    FOR ALL ENTRIES IN  gt_proj
    WHERE  pspid1 EQ gt_proj-pspid
    AND   td_grpflag NE ''.

  SORT gt_ztdtz_db BY pspid posid pspid1 posid1  td_matnr td_grpflag  .
  gt_ztdtz_db_z = gt_ztdtz_db.
  SORT gt_ztdtz_db_z BY pspid posid pspid1 posid1 mjahr mblnr zeile td_matnr td_grpflag  .
  "DELETE ADJACENT DUPLICATES FROM GT_ZTDTZ_DB COMPARING PSPID POSID PSPID1 POSID1 MJAHR MBLNR TD_MATNR TD_GRPFLAG.
  DELETE ADJACENT DUPLICATES FROM gt_ztdtz_db COMPARING pspid posid pspid1 posid1  td_matnr td_grpflag.

  SELECT * APPENDING TABLE gt_ztdtz_db
   FROM ztdtz_db
   FOR ALL ENTRIES IN  gt_proj
   WHERE  pspid1 EQ gt_proj-pspid
   AND   td_grpflag EQ ''.

  SORT gt_ztdtz_db BY mjahr mblnr zeile   .


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

  DATA e_wbs_ecp        TYPE tty_proj_element_ck_items_rdex.
  DATA lt_e_wbs_ecp     TYPE TABLE OF proj_element_ck_items_rdexp.
  DATA ls_e_wbs_ecp     TYPE proj_element_ck_items_rdexp.
  DATA lt_cost_lines    TYPE TABLE OF kis1.
  DATA ls_cost_lines    TYPE kis1.
  DATA l_pspid          TYPE proj-pspid.
  DATA l_post1          TYPE proj-post1.
  DATA l_longtext1 TYPE thead-tdname.
  "统计采购申请数量
  DATA:pspid TYPE ps_pspid .
  DATA:posid TYPE ps_posid .
  DATA:ps_psp_pnr  TYPE ps_psp_pnr.


  LOOP AT gt_ebkn INTO gs_ebkn.
    AT NEW ps_psp_pnr.
      ps_psp_pnr = gs_ebkn-ps_psp_pnr.
      CLEAR:pspid,posid .
      READ TABLE gt_prps INTO gs_prps
         WITH KEY pspnr = gs_ebkn-ps_psp_pnr. .
      IF sy-subrc EQ 0 .
        posid = gs_prps-posid.
        READ TABLE gt_proj INTO gs_proj
          WITH KEY pspnr = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid .
        ENDIF.
      ENDIF.
    ENDAT.

    READ TABLE  gt_eban  INTO gs_eban
         WITH KEY banfn = gs_ebkn-banfn
                  bnfpo = gs_ebkn-bnfpo
                  BINARY SEARCH .
    IF sy-subrc EQ 0 .
      IF gs_eban-frgkz = 'X'.
        CLEAR:gs_zs_pr .
        gs_zs_pr-pspid = pspid.
        gs_zs_pr-posid = posid.

        READ TABLE gt_ztdtz_pr INTO  gs_ztdtz_pr
         WITH KEY banfn = gs_eban-banfn
                  bnfpo = gs_eban-bnfpo
                  matnr = gs_eban-matnr
                  BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_zs_pr-matnr = gs_ztdtz_pr-td_matnr .
          gs_zs_pr-menge = gs_ztdtz_pr-td_menge.
          COLLECT gs_zs_pr INTO gt_zs_pr .

          IF gs_ztdtz_pr-td_grpflag NE ''.
            LOOP AT gt_ztdtz_pr_z INTO gs_ztdtz_pr_z
                     WHERE pspid = gs_ztdtz_pr-pspid
                     AND   posid = gs_ztdtz_pr-posid
                   "  AND   BANFN  = GS_ZTDTZ_PR-BANFN
                     AND   td_matnr = gs_ztdtz_pr-td_matnr
                     AND   td_grpflag = gs_ztdtz_pr-td_grpflag .

              DELETE gt_ebkn WHERE banfn = gs_ztdtz_pr_z-banfn
                               AND bnfpo = gs_ztdtz_pr_z-bnfpo .
              DELETE gt_eban WHERE banfn = gs_ztdtz_pr_z-banfn
                               AND bnfpo = gs_ztdtz_pr_z-bnfpo .

            ENDLOOP.
          ENDIF.

        ELSE.
          gs_zs_pr-matnr = gs_eban-matnr .
          gs_zs_pr-menge = gs_eban-menge.
          COLLECT gs_zs_pr INTO gt_zs_pr .
          CONTINUE .

        ENDIF.
      ELSE.
        CLEAR:gs_ys_pr .
        gs_ys_pr-pspid = pspid.
        gs_ys_pr-posid = posid.
        READ TABLE gt_ztdtz_pr INTO  gs_ztdtz_pr
         WITH KEY banfn = gs_eban-banfn
                  bnfpo = gs_eban-bnfpo
                  matnr = gs_eban-matnr
                  BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_ys_pr-matnr = gs_ztdtz_pr-td_matnr .
          gs_ys_pr-menge = gs_ztdtz_pr-td_menge.
          COLLECT gs_ys_pr INTO gt_ys_pr .
          IF gs_ztdtz_pr-td_grpflag NE ''.
            LOOP AT gt_ztdtz_pr_z INTO gs_ztdtz_pr_z
                     WHERE pspid = gs_ztdtz_pr-pspid
                     AND   posid = gs_ztdtz_pr-posid
                 "    AND   BANFN  = GS_ZTDTZ_PR-BANFN
                     AND   td_matnr = gs_ztdtz_pr-td_matnr
                     AND   td_grpflag = gs_ztdtz_pr-td_grpflag .

              DELETE gt_ebkn WHERE banfn = gs_ztdtz_pr_z-banfn
                               AND bnfpo = gs_ztdtz_pr_z-bnfpo .
              DELETE gt_eban WHERE banfn = gs_ztdtz_pr_z-banfn
                               AND bnfpo = gs_ztdtz_pr_z-bnfpo .

            ENDLOOP.
          ENDIF.
          CONTINUE .

        ELSE.
          gs_ys_pr-matnr = gs_eban-matnr .
          gs_ys_pr-menge = gs_eban-menge.
          COLLECT gs_ys_pr INTO gt_ys_pr .
          CONTINUE .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT  gt_zs_pr BY pspid posid matnr .

  SORT gt_ys_pr BY pspid posid matnr .

  LOOP AT gt_ekkn INTO gs_ekkn .
    AT NEW ps_psp_pnr..
      CLEAR:pspid,posid .
      READ TABLE gt_prps INTO gs_prps
         WITH KEY pspnr = gs_ekkn-ps_psp_pnr. .
      IF sy-subrc EQ 0 .
        posid = gs_prps-posid.
        READ TABLE gt_proj INTO gs_proj
          WITH KEY pspnr = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid .
        ENDIF.
      ENDIF.
    ENDAT.

    READ TABLE gt_ekpo INTO gs_ekpo
        WITH KEY ebeln = gs_ekkn-ebeln
                 ebelp = gs_ekkn-ebelp
                 BINARY SEARCH.
    IF sy-subrc EQ 0 .

      READ TABLE gt_ztdtz_po INTO  gs_ztdtz_po
          WITH KEY ebeln = gs_ekpo-ebeln
                   ebelp = gs_ekpo-ebelp
                    matnr = gs_ekpo-matnr
                    BINARY SEARCH .

      IF sy-subrc EQ 0 .
        CLEAR  gs_po .
        gs_po-pspid = pspid.
        gs_po-posid = posid.
        gs_po-matnr = gs_ztdtz_po-td_matnr.
        gs_po-menge = gs_ztdtz_po-td_menge.
        COLLECT gs_po INTO gt_po.
        IF gs_ztdtz_po-td_grpflag NE ''.
          LOOP AT  gt_ztdtz_po_z INTO gs_ztdtz_po_z
                    WHERE pspid = gs_ztdtz_po-pspid
                       AND posid = gs_ztdtz_po-posid
                 "      AND   EBELN  = GS_ZTDTZ_PO-EBELN
                       AND   td_matnr = gs_ztdtz_po-td_matnr
                       AND   td_grpflag = gs_ztdtz_po-td_grpflag  .

            DELETE  gt_ekkn WHERE ebeln = gs_ztdtz_po_z-ebeln
                            AND       ebelp = gs_ztdtz_po_z-ebelp.

            DELETE gt_ekpo WHERE ebeln = gs_ztdtz_po_z-ebeln
                           AND   ebelp = gs_ztdtz_po_z-ebelp
                           AND   matnr = gs_ztdtz_po_z-matnr.
          ENDLOOP.



        ENDIF.
        CONTINUE.
      ELSE.
        CLEAR  gs_po .
        gs_po-pspid = pspid.
        gs_po-posid = posid.
        gs_po-matnr = gs_ekpo-matnr .
        gs_po-menge = gs_ekpo-menge.
        COLLECT gs_po INTO gt_po.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT gt_po BY pspid posid matnr .



  "统计从其他项目调拨数量
  LOOP  AT gt_mseg INTO gs_mseg .
    CLEAR:gs_db.
    AT NEW pspnr1.
      CLEAR:pspid,posid .
      READ TABLE gt_prps INTO gs_prps
         WITH KEY pspnr = gs_mseg-pspnr1. .
      IF sy-subrc EQ 0 .
        posid = gs_prps-posid.
        READ TABLE gt_proj INTO gs_proj
          WITH KEY pspnr = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid .
        ENDIF.
      ENDIF.

    ENDAT.
    gs_db-pspid = pspid.
    gs_db-posid = posid.
    IF gs_mseg-bwart EQ '316' OR gs_mseg-bwart EQ '416'.
      gs_mseg-menge = gs_mseg-menge * -1 .
    ENDIF.
    READ TABLE gt_ztdtz_db INTO gs_ztdtz_db
    WITH KEY  mjahr = gs_mseg-mjahr
              mblnr = gs_mseg-mblnr
              zeile = gs_mseg-zeile
              matnr = gs_mseg-matnr
              BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_db-matnr = gs_ztdtz_db-td_matnr.
      gs_db-menge = gs_ztdtz_db-td_menge.
      IF gs_ztdtz_db-bwart EQ '316' OR gs_ztdtz_db-bwart EQ '416'.

        gs_db-menge = gs_ztdtz_db-td_menge * -1.

      ENDIF.
      COLLECT gs_db INTO gt_db .

      IF gs_ztdtz_db-td_grpflag NE ''.
        LOOP AT gt_ztdtz_db_z INTO gs_ztdtz_db_z
                WHERE pspid = gs_ztdtz_db-pspid
                   AND posid = gs_ztdtz_db-posid
                   AND pspid1 = gs_ztdtz_db-pspid1
                   AND posid1 = gs_ztdtz_db-posid1
*                             AND MBLNR  = GS_ZTDTZ_DB-MBLNR
*                             AND MJAHR = GS_ZTDTZ_DB-MJAHR
                   AND td_matnr = gs_ztdtz_db-td_matnr
                   AND td_grpflag = gs_ztdtz_db-td_grpflag.

          DELETE gt_mseg WHERE mblnr = gs_ztdtz_db_z-mblnr
                          AND  mjahr = gs_ztdtz_db_z-mjahr
                          AND  zeile = gs_ztdtz_db_z-zeile.



        ENDLOOP.

      ENDIF.
      CONTINUE.
    ELSE.
      gs_db-matnr = gs_mseg-matnr.
      gs_db-menge = gs_mseg-menge .
      COLLECT gs_db INTO gt_db .
      CONTINUE.
    ENDIF.
  ENDLOOP.
  SORT gt_db BY pspid posid matnr .

  LOOP AT gt_proj INTO gs_proj.
    CLEAR:l_pspid,
          l_post1.

    l_pspid   = gs_proj-pspid.

    l_post1  = gs_proj-post1.

*工厂
    CLEAR:e_wbs_ecp,
       ls_e_wbs_ecp,
       ls_cost_lines.

    REFRESH:lt_e_wbs_ecp,
            lt_cost_lines.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        i_proj_def    = l_pspid
        i_version     = '000'
      IMPORTING
        e_wbs_ecp     = e_wbs_ecp
      EXCEPTIONS
        error_message = 1.
*    IF SY-SUBRC <> 0.
*      RAISE NOT_FOUND.
*    ENDIF.

    lt_e_wbs_ecp = e_wbs_ecp.
    IF  lt_e_wbs_ecp IS NOT INITIAL.
      READ TABLE lt_e_wbs_ecp INTO ls_e_wbs_ecp
      INDEX 1.
      IF sy-subrc = 0.
        lt_cost_lines = ls_e_wbs_ecp-cost_lines.

*物料号，工厂，计量单位
        REFRESH gt_ecp.

*选择条件筛选
        DELETE lt_cost_lines WHERE matnr NOT IN s_matnr.
        DELETE lt_cost_lines WHERE matkl NOT IN s_matkl.

        LOOP AT   lt_cost_lines INTO ls_cost_lines
        WHERE    typps = 'M'.
          MOVE-CORRESPONDING ls_cost_lines TO gs_ecp.
          COLLECT gs_ecp INTO gt_ecp.
          CLEAR gs_ecp.
        ENDLOOP.

        CLEAR:e_wbs_ecp,
            ls_e_wbs_ecp,
            ls_cost_lines.

        REFRESH:lt_e_wbs_ecp,
                lt_cost_lines.

        CALL FUNCTION 'CNECP_READ'
          EXPORTING
            i_proj_def    = l_pspid
            i_version     = '100'
          IMPORTING
            e_wbs_ecp     = e_wbs_ecp
          EXCEPTIONS
            error_message = 1.
*    IF SY-SUBRC <> 0.
*      RAISE NOT_FOUND.
*    ENDIF.

        lt_e_wbs_ecp = e_wbs_ecp.
        IF  lt_e_wbs_ecp IS NOT INITIAL.
          READ TABLE lt_e_wbs_ecp INTO ls_e_wbs_ecp
          INDEX 1.
          IF sy-subrc = 0.
            lt_cost_lines = ls_e_wbs_ecp-cost_lines.

*物料号，工厂，计量单位
            REFRESH gt_ecp_100.

*选择条件筛选
            DELETE lt_cost_lines WHERE matnr NOT IN s_matnr.
            DELETE lt_cost_lines WHERE matkl NOT IN s_matkl.
            DELETE lt_cost_lines WHERE typps NE 'M'.


            LOOP AT   lt_cost_lines INTO ls_cost_lines.
              MOVE-CORRESPONDING ls_cost_lines TO gs_ecp_100.
              COLLECT gs_ecp_100 INTO gt_ecp_100.
              CLEAR gs_ecp_100.
            ENDLOOP.
            sort gt_ecp_100 by pspnr werks matnr .
          ENDIF.
        ENDIF.
        LOOP AT  gt_ecp INTO gs_ecp.

*项目定义
          gs_data-pspid = l_pspid.

*项目描述
          gs_data-post1 = l_post1.

          gs_data-werks = gs_ecp-werks.
          gs_data-matnr = gs_ecp-matnr.

*设计代码
          READ TABLE gt_zmm024 INTO gs_zmm024
          WITH KEY matnr = gs_data-matnr
                   posid = gs_proj-pspid.
          IF sy-subrc = 0.
            gs_data-sjdm = gs_zmm024-sjdm.
          ENDIF.

          gs_data-meins = gs_ecp-meeht.
          gs_data-menge = gs_ecp-menge.

*人工预算单价
          gs_data-fpreis = gs_ecp-fpreis.
          "物料综合单价
          READ TABLE gt_ecp_100 INTO gs_ecp_100 WITH KEY
                                                pspnr = gs_ecp-pspnr
                                                werks = gs_ecp-werks
                                                matnr = gs_ecp-matnr
                                                BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_data-gpreis = gs_ecp_100-gpreis .
            "                           gpreis .
          ENDIF.
          "人工单价
          READ TABLE gt_cg INTO gs_cg WITH KEY
                               ps_psp_pnr = gs_prps-pspnr
                               matnr = gs_data-matnr
                               BINARY SEARCH .
          IF sy-subrc EQ 0 .

            gs_data-kbetr = gs_cg-netpr.
          ENDIF.
*物料描述
*          READ TABLE GT_MAKT INTO GS_MAKT
*          WITH KEY MATNR = GS_DATA-MATNR
*                   SPRAS = SY-LANGU.
*          IF SY-SUBRC = 0.
*            GS_DATA-MAKTX = GS_MAKT-MAKTX.
*          ENDIF.
          SELECT SINGLE maktx FROM makt
           INTO  gs_data-maktx
           WHERE matnr = gs_data-matnr
           AND   spras = sy-langu.


*物料组
          SELECT SINGLE matkl FROM mara
          INTO gs_data-matkl
          WHERE matnr = gs_data-matnr.

*物料组描述
*          IF GS_DATA-MATKL IS NOT INITIAL.
*            READ TABLE GT_T023T INTO GS_T023T
*            WITH KEY MATKL = GS_DATA-MATKL
*                     SPRAS = SY-LANGU.
*            IF SY-SUBRC = 0.
*              GS_DATA-WGBEZ = GS_T023T-WGBEZ.
*            ENDIF.
*          ENDIF.
          SELECT SINGLE matkl FROM t023t
          INTO gs_data-wgbez
          WHERE matkl = gs_data-matkl
          AND   spras = sy-langu.

*未确认累计申购数量

          LOOP AT gt_zs_pr INTO gs_zs_pr
                 WHERE pspid = gs_proj-pspid
                 AND   matnr = gs_data-matnr .

            gs_data-wqrljsgsl = gs_data-wqrljsgsl + gs_zs_pr-menge.
          ENDLOOP.

*已确认累计申购数量
          LOOP AT gt_ys_pr INTO gs_ys_pr
                WHERE pspid = gs_proj-pspid
                AND   matnr = gs_data-matnr .

            gs_data-yqrljsgsl = gs_data-yqrljsgsl + gs_ys_pr-menge.
          ENDLOOP.
*  累计采购已下单数量


          LOOP AT gt_po INTO   gs_po
                 WHERE pspid = gs_proj-pspid
                 AND   matnr = gs_data-matnr .
            gs_data-ljcgxdsl = gs_data-ljcgxdsl + gs_po-menge.
          ENDLOOP.

          "其他项目调拨数量
          LOOP AT gt_db INTO gs_db
                   WHERE pspid = gs_proj-pspid
                   AND   matnr = gs_data-matnr.
            gs_data-qtdb = gs_data-qtdb + gs_db-menge .
          ENDLOOP.
**WBS元素
*          LOOP AT GT_PRPS INTO GS_PRPS
*          WHERE PSPHI = GS_PROJ-PSPNR.
*
**未确认累计申购数量
*            LOOP AT GT_EBKN INTO GS_EBKN
*             WHERE PS_PSP_PNR = GS_PRPS-PSPNR.
*              READ TABLE GT_EBAN INTO GS_EBAN
*              WITH KEY   BANFN = GS_EBKN-BANFN
*                         BNFPO = GS_EBKN-BNFPO
*                         MATNR = GS_DATA-MATNR
*                         FRGKZ = 'X'.
*              IF SY-SUBRC = 0.
*                GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_EBAN-MENGE.
*              ENDIF.
*            ENDLOOP.
*
**已确认累计申购数量
*            LOOP AT GT_EBKN INTO GS_EBKN
*             WHERE PS_PSP_PNR = GS_PRPS-PSPNR.
*
*              READ TABLE GT_EBAN INTO GS_EBAN
*              WITH KEY   BANFN = GS_EBKN-BANFN
*                         BNFPO = GS_EBKN-BNFPO
*                         MATNR = GS_DATA-MATNR
*                         FRGKZ = '2'.
*              IF SY-SUBRC = 0.
*                GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_EBAN-MENGE.
*              ENDIF.
*            ENDLOOP.
*
**累计采购已下单数量
*            LOOP AT GT_EKKN INTO GS_EKKN
*              WHERE PS_PSP_PNR = GS_PRPS-PSPNR.
*              READ TABLE GT_EKPO INTO GS_EKPO
*              WITH KEY   EBELN = GS_EKKN-EBELN
*                         EBELP = GS_EKKN-EBELP
*                         MATNR = GS_DATA-MATNR..
*              IF SY-SUBRC = 0.
*                GS_DATA-LJCGXDSL = GS_DATA-LJCGXDSL + GS_EKPO-MENGE.
*              ENDIF.
*            ENDLOOP.
*          ENDLOOP.

**期初数量(未确认累计申购数量)
*          LOOP AT GT_ZMM009 INTO GS_ZMM009
*          WHERE PSPHI = GS_DATA-PSPID
*          AND   MATNR = GS_DATA-MATNR.
*            GS_DATA-WQRLJSGSL = GS_DATA-WQRLJSGSL + GS_ZMM009-WQRLJSGS.
*          ENDLOOP.

**期初数量(已确认累计申购数量)
*          LOOP AT GT_ZMM009 INTO GS_ZMM009
*          WHERE PSPHI = GS_DATA-PSPID
*          AND   MATNR = GS_DATA-MATNR.
*            GS_DATA-LJCGXDSL = GS_DATA-LJCGXDSL + GS_ZMM009-LJCGXDS.
*          ENDLOOP.


**期初数量(已确认累计申购数量)
*          LOOP AT GT_ZMM009 INTO GS_ZMM009
*          WHERE PSPHI = GS_DATA-PSPID
*          AND   MATNR = GS_DATA-MATNR.
*            GS_DATA-YQRLJSGSL = GS_DATA-YQRLJSGSL + GS_ZMM009-YQRLJSGS.
*          ENDLOOP.

*剩余申购数量 （D= A - B - C - G）
          gs_data-syksqsl = gs_data-menge - gs_data-yqrljsgsl - gs_data-wqrljsgsl - gs_data-qtdb .

*采购未下单数量
          gs_data-cgwxdsl = gs_data-yqrljsgsl - gs_data-ljcgxdsl.

          "采购申请替代
          READ TABLE gt_ztdtz_pr INTO gs_ztdtz_pr
                WITH KEY pspid = gs_data-pspid
                         td_matnr = gs_data-matnr.
          IF sy-subrc EQ 0 .
            gs_data-prtd = '是'.
          ELSE.
            gs_data-prtd = '否'.
          ENDIF.
          "采购订单替代
          READ TABLE gt_ztdtz_po INTO gs_ztdtz_po
                WITH KEY  pspid = gs_data-pspid
                          td_matnr = gs_data-matnr.
          IF sy-subrc EQ 0 .
            gs_data-potd = '是'.
          ELSE.
            gs_data-potd = '否'.
          ENDIF.
          "调拨替代
          READ TABLE gt_ztdtz_db INTO gs_ztdtz_db
               WITH KEY pspid1 = gs_data-pspid
                        td_matnr = gs_data-matnr.
          IF sy-subrc EQ 0.
            gs_data-dbtd = '是'.
          ELSE.
            gs_data-dbtd = '否'.
          ENDIF.

          APPEND gs_data TO gt_data.
          CLEAR gs_data.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDLOOP.


*设计代码
  IF s_sjdm IS NOT INITIAL .
    DELETE gt_data WHERE sjdm NOT IN s_sjdm.
  ENDIF.

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
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
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
  init_fieldcat 'PSPID'        '项目定义'         '' '' '' '' 'X' '' ''.
  init_fieldcat 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATKL'        '物料组'         '' '' '' '' '' '' ''.
  init_fieldcat 'WGBEZ'        '物料组描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'        '计量单位'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE'        '预算数(A)'         '' '' '' '' '' '' ''.
  init_fieldcat 'WQRLJSGSL'    '材料单在审数量(B)'         '' '' '' '' '' '' ''.
  init_fieldcat 'YQRLJSGSL'    '材料单已审数量(C)'         '' '' '' '' '' '' ''.
  init_fieldcat 'SYKSQSL'      '剩余申购数量(D=A-B-C-G)'         '' '' '' '' '' '' ''.
  init_fieldcat 'LJCGXDSL'     '采购已下单数量(E)'         '' '' '' '' '' '' ''.
  init_fieldcat 'CGWXDSL'      '材料单已审采购未下单数量(F=C-E)'         '' '' '' '' '' '' ''.
  init_fieldcat 'QTDB'         '从其他项目调拨数量(G)'         '' '' '' '' '' '' ''.
  init_fieldcat 'GPREIS'       '物料综合单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'FPREIS'       '人工预算单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'KBETR'        '人工单价'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'PRTD'         '采购申请替代'         '' '' '' '' '' '' ''.
  init_fieldcat 'POTD'         '采购订单替代'         '' '' '' '' '' '' ''.
  init_fieldcat 'DBTD'         '调拨替代'         '' '' '' '' '' '' ''.

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
