REPORT ztdtz_pr.
"调拔台账维护
" IT02
" 2016-04-18

TABLES:mseg,mkpf,makt,proj,prps,zmm024 .

TYPES:BEGIN OF ty_data ,
        pspnr       TYPE ps_posnr,     "源PSPNR号
        " PSPID TYPE PS_PSPID,    "来源项目号
        mjahr       TYPE mjahr,              "物料凭证年度
        mblnr       TYPE mblnr,      "物料凭证号
        zeile       TYPE mblpo,      "物料凭证行项目
        bldat       TYPE bldat,      "创建日期
        budat       TYPE budat,       "过账日期
        werks       TYPE werks_d,     "工厂
        pspid       TYPE ps_pspid,    "来源项目号
        post        TYPE ps_post1,     "来源项目名称
        posid       TYPE ps_posid,    "来源WBS
        "PSPNR TYPE PS_POSNR,     "源PSPNR号
        pspid1      TYPE ps_pspid,    "目标项目号
        post1       TYPE ps_post1,     "目标项目名称
        posid1      TYPE ps_posid,    "目标WBS
        pspnr1      TYPE ps_posnr,   "目标PSPNR号
        matnr       TYPE matnr,       "物料号
        maktx       TYPE maktx,       "物料描述
        menge       TYPE menge_d,     "数量
        meins       TYPE meins,       "单位
        " LGORT TYPE LGORT_D,       "库存地点
        sjdm        TYPE zmm024-sjdm,        "源设计代码
        sjdm1       TYPE zmm024-sjdm,        "目标设计代码
        td_matnr    TYPE matnr,              "被替代物料
        td_maktx    TYPE maktx,            "被替代物料描述
        td_meins    TYPE meins,            "被替代物料单位
        td_grpflag  TYPE   c LENGTH 25,               "替代组表识
        td_menge    TYPE    eban-menge ,                "折算数
        bwart       TYPE  bwart  ,  "移动类型
        shkzg       TYPE shkzg,   "借贷标识
        uname       TYPE sy-tcode,            "维护账户
        udate       TYPE dats,                "维护日期
        utime       TYPE tims,                "维护时间
        sel(1),
        td_matnr1   TYPE matnr,              "被替代物料
        td_maktx1   TYPE maktx,            "被替代物料描述
        td_meins1   TYPE meins,            "被替代物料单位
        td_grpflag1 TYPE   c LENGTH 25,               "替代组表识
        td_menge1   TYPE    eban-menge ,                "折算数
      END OF ty_data  .


TYPES:BEGIN OF ty_proj_sel,
        mblnr  TYPE mblnr,      "物料凭证号
        mjahr  TYPE mjahr,              "物料凭证年度
        pspid  TYPE ps_pspid,    "来源项目号
        pspid1 TYPE ps_pspid,    "目标项目号


      END OF ty_proj_sel.

TYPES:BEGIN OF ty_proj_pr,
        pspid TYPE ps_pspid,
        banfn TYPE banfn,            "采购申请编号




      END OF ty_proj_pr .

"声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data et_bad_cells e_display,
      handle_modify  FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified  et_good_cells ." ER_DATA_CHANGED."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM . "data_changed_finished


ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

DATA:gs_data TYPE ty_data,
     gt_data TYPE TABLE OF ty_data.

DATA:gs_data_sel TYPE ty_data,
     gt_data_sel TYPE TABLE OF ty_data.

DATA:gt_data_del TYPE TABLE OF ty_data,
     gs_data_del TYPE ty_data.

DATA:gs_proj_f TYPE proj,
     gt_proj_f TYPE TABLE OF proj.

DATA:gs_proj_t TYPE proj,
     gt_proj_t TYPE TABLE OF proj.

DATA:gs_proj TYPE proj,
     gt_proj TYPE TABLE OF proj.

DATA:gs_prps_f TYPE prps,
     gt_prps_f TYPE TABLE OF prps.

DATA: gs_prps_t TYPE prps,
      gt_prps_t TYPE TABLE OF prps.


DATA:gt_t024 TYPE TABLE OF t024,
     gs_t024 TYPE t024.

DATA:gt_zmm024 TYPE TABLE OF zmm024,
     gs_zmm024 TYPE zmm024.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.
DATA: g_objname TYPE thead-tdname.

DATA:gt_ztdtz_db TYPE TABLE OF ztdtz_db,
     gs_ztdtz_db TYPE ztdtz_db.


RANGES:r_pspnr_f FOR prps-pspnr,
       r_pspnr_t FOR prps-pspnr.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_proj_sel TYPE TABLE OF   ty_proj_sel,
     gs_proj_sel TYPE ty_proj_sel.

DATA:gt_db TYPE TABLE OF ztdtz_db,
     gs_db TYPE ztdtz_db.

DATA g_answer     TYPE string. "控制弹出框
FIELD-SYMBOLS: <fs_data> TYPE ty_data.

DEFINE init_fieldcat.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  IF &4 = 'X'.
    GW_LVC-KEY = 'X'.
  ENDIF.
*   IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
   IF &1 = 'MENGE' .
   GW_LVC-QFIELDNAME = 'MEINS'.
  ENDIF.

  IF &1 = 'TD_MENGE' .
    GW_LVC-QFIELDNAME = 'TD_MEINS'.
    GW_LVC-DECIMALS = 3.
 ENDIF.
  IF &1 = 'TD_MENGE1' .
    GW_LVC-QFIELDNAME = 'TD_MEINS1'.
    GW_LVC-DECIMALS = 3.
 ENDIF.



  IF &1 = 'TD_MATNR' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.
   IF &1 = 'TD_MATNR1' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.

  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gw_layout        TYPE lvc_s_layo,                    "ALV的格式
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

DATA gt_event_receiver TYPE REF TO lcl_event_receiver .

************************************************************************
* CONSTANT
************************************************************************

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETER:
p_bukrs  TYPE bkpf-bukrs OBLIGATORY DEFAULT '1800'.                 "公司代码
"P_MJAHR  TYPE MKPF-MJAHR OBLIGATORY DEFAULT SY-DATUM+0(4).           "物料凭证年度
*P_PSPID_F TYPE  PROJ-PSPID  OBLIGATORY,   "源项目号
*P_PSPID_T TYPE  PROJ-PSPID .         "目标项目号

SELECT-OPTIONS: " S_PSPID  FOR PROJ-PSPID ,   "过账日期
                s_xmh_f FOR proj-pspid," OBLIGATORY,  "源项目号
                s_xmh_t FOR proj-pspid ,    "目标项目号
                s_mjahr  FOR mkpf-mjahr ,  "物料凭证年度
                s_matnr  FOR mseg-matnr,    "物料号
                s_mblnr  FOR mseg-mblnr,   "物料凭证号
                s_budat  FOR mkpf-budat.   "过账日期


PARAMETERS:  g_wh TYPE char1 RADIOBUTTON GROUP g1,
             g_xs TYPE char1 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
  s_mjahr-low = sy-datum+0(4).

  APPEND s_mjahr .
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
*  PERFORM FRM_AUTH_CHECK USING '03'.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
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

  " IF S_XMH_F IS NOT INITIAL.
  IF  g_wh EQ 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_proj_f
       FROM proj
      WHERE pspid IN s_xmh_f
            AND werks EQ p_bukrs.
    SORT gt_proj_f BY pspnr.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_prps_f
   FROM prps
   FOR ALL ENTRIES IN gt_proj_f
   WHERE psphi = gt_proj_f-pspnr
      AND werks EQ p_bukrs.
    SORT gt_prps_f BY pspnr.


    LOOP AT gt_prps_f INTO gs_prps_f .
      r_pspnr_f-sign = 'I'.
      r_pspnr_f-option = 'EQ'.
      r_pspnr_f-low = gs_prps_f-pspnr.
      APPEND r_pspnr_f.
    ENDLOOP.

    " ENDIF.

    "IF S_XMH_T IS NOT INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_proj_t
     FROM proj
    WHERE pspid IN s_xmh_t
          AND werks EQ p_bukrs.
    SORT gt_proj_t BY pspnr.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_prps_t
     FROM prps
     FOR ALL ENTRIES IN gt_proj_t
     WHERE psphi = gt_proj_t-pspnr
        AND werks EQ p_bukrs.
    SORT gt_prps_t BY pspnr.



    LOOP AT gt_prps_t INTO gs_prps_t.
      r_pspnr_t-sign = 'I'.
      r_pspnr_t-option = 'EQ'.
      r_pspnr_t-low = gs_prps_t-pspnr.
      APPEND r_pspnr_t.
    ENDLOOP.



    "ENDIF.


    SELECT mkpf~mjahr mkpf~mblnr mkpf~bldat mkpf~budat
           mseg~zeile mseg~matnr mseg~menge mseg~meins
           mseg~shkzg mseg~werks mseg~bwart
           mseg~ps_psp_pnr AS pspnr
           mseg~mat_pspnr AS pspnr1
          INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM mkpf
      INNER JOIN mseg
      ON mkpf~mjahr = mseg~mjahr
      AND mkpf~mblnr = mseg~mblnr
     WHERE mkpf~mblnr IN s_mblnr
      AND mkpf~mjahr IN s_mjahr
      AND mseg~werks EQ p_bukrs
      AND mseg~ps_psp_pnr IN r_pspnr_f
      AND mseg~mat_pspnr IN r_pspnr_t
      AND mseg~bwart  IN ('315','316') .

    SORT gt_data BY pspnr  mblnr mjahr zeile .

    SELECT mkpf~mjahr mkpf~mblnr mkpf~bldat mkpf~budat
          mseg~zeile mseg~matnr mseg~menge mseg~meins
          mseg~shkzg mseg~werks mseg~bwart
          mseg~mat_pspnr AS pspnr
          mseg~ps_psp_pnr AS pspnr1
         APPENDING CORRESPONDING FIELDS OF TABLE gt_data
     FROM mkpf
     INNER JOIN mseg
     ON mkpf~mjahr = mseg~mjahr
     AND mkpf~mblnr = mseg~mblnr
    WHERE  mkpf~mblnr IN s_mblnr
      AND mkpf~mjahr IN s_mjahr
     AND mseg~werks EQ p_bukrs
     AND mseg~mat_pspnr IN r_pspnr_f
     AND mseg~ps_psp_pnr IN r_pspnr_t
     AND mseg~bwart IN ('415')
     AND mseg~shkzg EQ 'H' .

    SELECT mkpf~mjahr mkpf~mblnr mkpf~bldat mkpf~budat
         mseg~zeile mseg~matnr mseg~menge mseg~meins
         mseg~shkzg mseg~werks mseg~bwart
         mseg~mat_pspnr AS pspnr1
         mseg~ps_psp_pnr AS pspnr
        APPENDING CORRESPONDING FIELDS OF TABLE gt_data
    FROM mkpf
    INNER JOIN mseg
    ON mkpf~mjahr = mseg~mjahr
    AND mkpf~mblnr = mseg~mblnr
   WHERE  mkpf~mblnr IN s_mblnr
     AND mkpf~mjahr IN s_mjahr
    AND mseg~werks EQ p_bukrs
    AND mseg~mat_pspnr IN r_pspnr_t
    AND mseg~ps_psp_pnr IN r_pspnr_f
    AND mseg~bwart IN ('416')
    AND mseg~shkzg EQ 'S' .

    SORT gt_data BY pspnr  mblnr mjahr zeile .

    CHECK gt_data IS NOT INITIAL.

    SELECT * INTO TABLE gt_makt
      FROM makt
      WHERE matnr IN s_matnr
          AND spras = '1'.

    SORT gt_makt BY matnr .

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
     FROM zmm024
     WHERE posid  IN s_xmh_f
     OR   posid IN s_xmh_t .

    SORT gt_zmm024 BY matnr posid.

    SELECT * INTO TABLE gt_db
      FROM ztdtz_db
      WHERE mblnr IN s_mblnr
       AND    mjahr IN s_mjahr
      AND    pspid IN s_xmh_f
      AND    pspid1 IN s_xmh_t
      AND    werks EQ  p_bukrs
      AND    matnr IN s_matnr.

    SORT  gt_db BY mblnr mjahr  zeile .

  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM  ztdtz_db
     WHERE  mblnr IN s_mblnr
     AND    mjahr IN s_mjahr
     AND    pspid IN s_xmh_f
     AND    pspid1 IN s_xmh_t
     AND    werks EQ  p_bukrs
     AND    matnr IN s_matnr.
    SORT gt_data BY  mblnr mjahr  zeile .

    SELECT * INTO TABLE gt_proj
      FROM proj
      WHERE pspid IN s_xmh_f
      OR pspid IN s_xmh_t .
    SORT gt_proj BY pspid .

    SELECT * INTO TABLE gt_makt
      FROM makt
      WHERE spras = '1'
       AND   matnr IN s_matnr.

    SORT gt_makt BY matnr .






  ENDIF.

*   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T024
*     FROM T024
*    .
*   SORT GT_T024 BY EKGRP.
*


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
  DATA: pspid TYPE ps_pspid,
        post  TYPE ps_post1,
        posid TYPE ps_posid.
  IF  g_wh EQ 'X'.
    LOOP AT gt_data ASSIGNING <fs_data> .

      AT NEW pspnr .
        CLEAR:pspid,post,posid.
        "源项目号
        READ TABLE gt_prps_f INTO gs_prps_f
               WITH KEY pspnr = <fs_data>-pspnr
                       BINARY SEARCH.
        IF sy-subrc EQ 0 .
          "WBS号
          posid = gs_prps_f-posid.
          READ TABLE gt_proj_f INTO  gs_proj_f
            WITH KEY pspnr = gs_prps_f-psphi
             BINARY SEARCH .
          IF sy-subrc EQ 0 .
            pspid = gs_proj_f-pspid.
            post = gs_proj_f-post1.
          ENDIF.
        ENDIF.


      ENDAT .
      "源项目号
      <fs_data>-pspid = pspid.
      <fs_data>-posid = posid.
      <fs_data>-post =  post .

      "目标项目号
      READ TABLE gt_prps_t INTO gs_prps_t
                WITH KEY pspnr = <fs_data>-pspnr1
                        BINARY SEARCH.
      IF sy-subrc EQ 0 .
        "WBS号
        <fs_data>-posid1 = gs_prps_t-posid.
        READ TABLE gt_proj_t INTO  gs_proj_t
          WITH KEY pspnr = gs_prps_t-psphi
           BINARY SEARCH .
        IF sy-subrc EQ 0 .
          <fs_data>-pspid1 = gs_proj_t-pspid.
          <fs_data>-post1 = gs_proj_t-post1.
        ENDIF.
      ENDIF.
      "物料描述
      READ TABLE gt_makt INTO gs_makt
                       WITH KEY matnr = <fs_data>-matnr BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-maktx = gs_makt-maktx.
      ENDIF.

      "设计代码
      READ TABLE gt_zmm024 INTO gs_zmm024
                     WITH KEY matnr = <fs_data>-matnr
                              posid = <fs_data>-pspid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-sjdm = gs_zmm024-sjdm.
      ENDIF.

      READ TABLE gt_zmm024  INTO gs_zmm024
                            WITH KEY matnr = <fs_data>-matnr
                              posid = <fs_data>-pspid1 BINARY SEARCH .
      IF sy-subrc EQ 0 .

        <fs_data>-sjdm1 = gs_zmm024-sjdm.

      ENDIF.

      READ TABLE gt_db INTO gs_db WITH KEY
               mblnr = <fs_data>-mblnr
               mjahr = <fs_data>-mjahr
               zeile = <fs_data>-zeile
               matnr = <fs_data>-matnr
               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-td_matnr = gs_db-td_matnr . "替代物料
        <fs_data>-td_meins = gs_db-td_meins.  "替代单位
        <fs_data>-td_grpflag = gs_db-td_grpflag. "替代组标识
        <fs_data>-td_menge = gs_db-td_menge.    "替代数量
        "被替代物料描述
        READ TABLE gt_makt INTO gs_makt
             WITH KEY matnr = <fs_data>-td_matnr
                BINARY SEARCH .
        IF sy-subrc EQ 0 .
          <fs_data>-td_maktx = gs_makt-maktx .
        ENDIF.
        <fs_data>-td_matnr1 = gs_db-td_matnr1 . "替代物料
        <fs_data>-td_meins1 = gs_db-td_meins1.  "替代单位
        <fs_data>-td_grpflag1 = gs_db-td_grpflag1. "替代组标识
        <fs_data>-td_menge1 = gs_db-td_menge1.    "替代数量
        "被替代物料描述
        READ TABLE gt_makt INTO gs_makt
             WITH KEY matnr = <fs_data>-td_matnr1
                BINARY SEARCH .
        IF sy-subrc EQ 0 .
          <fs_data>-td_maktx1 = gs_makt-maktx .
        ENDIF.

      ENDIF.


    ENDLOOP.
  ELSE.
    LOOP AT gt_data ASSIGNING <fs_data> .
      "源项目描述
      READ TABLE gt_proj INTO  gs_proj
                  WITH KEY pspid = <fs_data>-pspid
                   BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-post = gs_proj-post1.
      ENDIF.
      "目标项目描述
      READ TABLE gt_proj INTO gs_proj
                WITH KEY pspid = <fs_data>-pspid1
                         BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-post1 = gs_proj-post1.

      ENDIF.

      "物料描述
      READ TABLE gt_makt INTO gs_makt
               WITH KEY matnr = <fs_data>-matnr
                BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-maktx = gs_makt-maktx .

      ENDIF.
      "被替代物料描述
      READ TABLE gt_makt INTO gs_makt
           WITH KEY matnr = <fs_data>-td_matnr
              BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-td_maktx = gs_makt-maktx .
      ENDIF.
*      "被替代物料描述
*      READ TABLE gt_makt INTO gs_makt
*           WITH KEY matnr = <fs_data>-td_matnr
*              BINARY SEARCH .
*      IF sy-subrc EQ 0 .
*        <fs_data>-td_maktx = gs_makt-maktx .
*      ENDIF.
      "被替代物料描述-源项目
      READ TABLE gt_makt INTO gs_makt
           WITH KEY matnr = <fs_data>-td_matnr1
              BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-td_maktx1 = gs_makt-maktx .
      ENDIF.

    ENDLOOP.




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
  PERFORM frm_exclude.
  gw_grid_settings-edt_cll_cb = 'X'.
  PERFORM frm_build_event.
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
  gw_layout-zebra        = 'X'.
  gw_layout-cwidth_opt   = 'X'.
  gw_layout-box_fname = 'SEL'.
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
  IF  g_wh EQ 'X' .
    init_fieldcat 'MJAHR'        '物料凭证年度'         '' '' '' '' '' '' ''.
    init_fieldcat 'MBLNR'        '物料凭证号'         '' '' '' '' '' '' ''.
    init_fieldcat 'ZEILE'         '物料凭证行项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'BLDAT'         '创建日期'         '' '' '' '' '' '' ''.
    init_fieldcat 'BUDAT'         '过账日期'         '' '' '' '' '' '' ''.
    init_fieldcat 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    init_fieldcat 'PSPID'        '来源项目号'         '' '' '' '' '' '' ''.
    init_fieldcat 'POST'          '来源项目名称'         '' '' '' '' '' '' ''.
    init_fieldcat 'POSID'         '来源WBS'         '' '' '' '' '' '' ''.
    init_fieldcat 'PSPID1'        '目标项目号'         '' '' '' '' '' '' ''.
    init_fieldcat 'POST1'         '目标项目名称'         '' '' '' '' '' '' ''.
    init_fieldcat 'POSID1'        '目标WBS'         '' '' '' '' '' '' ''.
    init_fieldcat 'MATNR'         '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
    init_fieldcat 'MAKTX'         '物料描述'         '' '' '' '' '' '' ''.
    init_fieldcat 'MENGE'         '数量'         '' '' '' '' '' '' ''.
    init_fieldcat 'MEINS'          '单位'         '' '' '' '' '' '' ''.
    init_fieldcat 'BWART'         '移动类型'         '' '' '' '' '' '' ''.
    init_fieldcat 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
    init_fieldcat 'SJDM'          '源设计代码'         '' '' '' '' '' '' ''.
    init_fieldcat 'SJDM1'          '目标设计代码'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MATNR'       '被替代物料'         '' '' '' 'X' '' 'EBAN' 'MATNR'.
    init_fieldcat 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_GRPFLAG'     '替代组标识'         '25' '' '' 'X' '' '' ''.
    init_fieldcat 'TD_MENGE'       '折算数'         '' '' '' 'X' '' 'MSEG' 'MENGE'.
    init_fieldcat 'TD_MATNR1'       '被替代物料-来源项目'         '' '' '' 'X' '' 'EBAN' 'MATNR'.
    init_fieldcat 'TD_MAKTX1'       '被替代物料描述-来源项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MEINS1'       '被替代物料单位-来源项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_GRPFLAG1'     '替代组标识-来源项目'         '25' '' '' 'X' '' '' ''.
    init_fieldcat 'TD_MENGE1'       '折算数-来源项目'         '' '' '' 'X' '' 'MSEG' 'MENGE'.
  ELSE.
    init_fieldcat 'MJAHR'        '物料凭证年度'         '' '' '' '' '' '' ''.
    init_fieldcat 'MBLNR'        '物料凭证号'         '' '' '' '' '' '' ''.
    init_fieldcat 'ZEILE'         '物料凭证行项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'WERKS'        '工厂'         '' '' '' '' '' '' ''.
    init_fieldcat 'PSPID'        '来源项目号'         '' '' '' '' '' '' ''.
    init_fieldcat 'POST'          '来源项目名称'         '' '' '' '' '' '' ''.
    init_fieldcat 'POSID'         '来源WBS'         '' '' '' '' '' '' ''.
    init_fieldcat 'PSPID1'        '目标项目号'         '' '' '' '' '' '' ''.
    init_fieldcat 'POST1'         '目标项目名称'         '' '' '' '' '' '' ''.
    init_fieldcat 'POSID1'        '目标WBS'         '' '' '' '' '' '' ''.
    init_fieldcat 'MATNR'         '物料号'         '' '' '' '' '' '' ''.
    init_fieldcat 'MAKTX'         '物料描述'         '' '' '' '' '' '' ''.
    init_fieldcat 'MENGE'         '数量'         '' '' '' '' '' '' ''.
    init_fieldcat 'MEINS'          '单位'         '' '' '' '' '' '' ''.
    init_fieldcat 'BWART'         '移动类型'         '' '' '' '' '' '' ''.
    init_fieldcat 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MATNR'       '被替代物料'         '' '' '' '' '' 'EBAN' 'MATNR'.
    init_fieldcat 'TD_MAKTX'       '被替代物料描述'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MEINS'       '被替代物料单位'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_GRPFLAG'     '替代组标识'         '25' '' '' '' '' '' ''.
    init_fieldcat 'TD_MENGE'       '折算数'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MATNR1'       '被替代物料-来源项目'         '' '' '' '' '' 'EBAN' 'MATNR'.
    init_fieldcat 'TD_MAKTX1'       '被替代物料描述-来源项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_MEINS1'       '被替代物料单位-来源项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'TD_GRPFLAG1'     '替代组标识-来源项目'         '25' '' '' '' '' '' ''.
    init_fieldcat 'TD_MENGE1'       '折算数-来源项目'         '' '' '' '' '' '' ''.
    init_fieldcat 'UNAME'       '维护账户'         '' '' '' '' '' '' ''.
    init_fieldcat 'UDATE'       '维护日期'         '' '' '' '' '' '' ''.
    init_fieldcat 'UTIME'       '维护时间'         '' '' '' '' '' '' ''.



  ENDIF.
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
*  GW_EVENTS-NAME =  'CALLER_EXIT' .
*  GW_EVENTS-FORM =  'FRM_BUTTON'.   "f4事件
*  APPEND GW_EVENTS TO GT_EVENTS.
  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
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
*      -->P_0360   text
*      -->P_0361   text
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
*     I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*      IF RS_SELFIELD-FIELDNAME = 'VBELN'
*        AND GS_DATA-VBELN IS NOT INITIAL.
*        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*      ENDIF.
      "保存维护数据
    WHEN '&DATA_SAVE'.

      IF g_wh EQ 'X'.
        DATA l_subrc TYPE c."检查输入项

        " *提示对话框
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            text_question  = '是否执行保存操作'
          IMPORTING
            answer         = g_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF g_answer <> '1'.
          EXIT.
        ENDIF.

        READ TABLE gt_data INTO gs_data WITH KEY sel = 'X'.
        IF sy-subrc EQ 0 .

          LOOP AT gt_data INTO gs_data WHERE sel = 'X' .
            LOOP AT gt_data INTO gs_data WHERE  mblnr =  gs_data-mblnr
                               AND  mjahr = gs_data-mjahr
                               AND  pspid = gs_data-pspid
                               AND  pspid1 = gs_data-pspid1
                               AND   ( td_matnr NE '' OR td_matnr1 NE '' ).

              gs_data-sel = 'X'.


              MODIFY gt_data FROM  gs_data .
            ENDLOOP.
          ENDLOOP.


        ELSE.

          MESSAGE '请选择要保存的相应物料凭证替代记录保存'  TYPE 'S'   DISPLAY LIKE 'E'.

        ENDIF.

        "检查必输项

        PERFORM check_input CHANGING l_subrc .

        IF l_subrc = '4'.
          EXIT.
        ENDIF.

        IF gt_proj_sel IS NOT INITIAL.
          SORT gt_proj_sel BY mblnr mjahr  pspid pspid1.
          DELETE ADJACENT DUPLICATES FROM gt_proj_sel COMPARING mblnr mjahr  pspid pspid1 .
          "先删除已选择物理凭证对应的记录台账信息
          LOOP AT gt_proj_sel INTO gs_proj_sel.

            "根据选择物料凭证、源项目号、目标项目号维度区分 删除已维护台账信息表
            DELETE FROM ztdtz_db WHERE mblnr = gs_proj_sel-mblnr
                                 AND mjahr = gs_proj_sel-mjahr
                                 AND pspid = gs_proj_sel-pspid
                                 AND pspid1 = gs_proj_sel-pspid1.

          ENDLOOP.
        ENDIF.


        "保存选择数据到 替代调拨台账维护表
        CHECK gs_data_sel IS NOT INITIAL.

        CHECK l_subrc EQ 0.
        LOOP AT gt_data_sel INTO gs_data_sel .

          CLEAR gs_ztdtz_db .
          MOVE-CORRESPONDING gs_data_sel TO gs_ztdtz_db.

          "维护账号
          gs_ztdtz_db-uname = sy-uname.
          "维护日期
          gs_ztdtz_db-udate = sy-datum.

          "维护时间
          gs_ztdtz_db-utime = sy-uzeit.

          APPEND gs_ztdtz_db TO gt_ztdtz_db.
        ENDLOOP.


        SORT gt_ztdtz_db BY mblnr mjahr zeile .
        CHECK gt_ztdtz_db IS NOT INITIAL.
        "保存数据到 ZTDTZ_PR.
        MODIFY ztdtz_db FROM TABLE gt_ztdtz_db .

        "先删除已选择物理凭证对应的记录台账信息
        LOOP AT gt_proj_sel INTO gs_proj_sel.

          "根据选择物料凭证、源项目号、目标项目号维度区分 删除且替代组且替代组标识且折算数量这三项已维护台账信息表
          DELETE FROM ztdtz_db WHERE mblnr = gs_proj_sel-mblnr
                               AND mjahr = gs_proj_sel-mjahr
                               AND pspid = gs_proj_sel-pspid
                               AND pspid1 = gs_proj_sel-pspid1
                               AND td_matnr EQ ''
                               AND td_grpflag EQ ''
                               AND td_menge EQ 0
                               AND td_matnr1 EQ ''
                               AND td_grpflag1 EQ ''
                               AND td_menge1 EQ 0 .

        ENDLOOP.

        IF sy-subrc EQ 0 .

          MESSAGE '数据保存成功' TYPE 'S'.
        ELSE.

          MESSAGE '数据更新失败' TYPE 'S'.
        ENDIF.

      ENDIF.
      CALL METHOD g_ref_grid->refresh_table_display.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXWBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_BANFN  text
*      -->P_<FS_DATA>_BNFPO  text
*      <--P_<FS_DATA>_WBSCSWB  text
*----------------------------------------------------------------------*
FORM selxwbs  USING    p_banfn
                       p_bnfpo
              CHANGING p_wbscswb.
  CONCATENATE p_banfn p_bnfpo INTO g_objname .

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'B01'
      language                = '1'
      name                    = g_objname
      object                  = 'EBAN'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
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
      CONDENSE wa_lines-tdline NO-GAPS.
      IF p_wbscswb IS INITIAL.
        p_wbscswb = wa_lines-tdline.
      ELSE.
        CONCATENATE p_wbscswb wa_lines-tdline INTO p_wbscswb.
      ENDIF.

    ENDLOOP.

  ENDIF.
ENDFORM.

FORM frm_button  USING e_grid TYPE slis_data_caller_exit.
  DATA lt_f4 TYPE lvc_t_f4.
  DATA ls_f4 TYPE lvc_s_f4.

  ls_f4-fieldname = 'TD_MATNR'.      "F4对应的栏位
  ls_f4-register = 'X'.
  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.



  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_alvgrid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_onf4   FOR gr_alvgrid.

  CALL METHOD gr_alvgrid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].



ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_modify.
    PERFORM handle_data_changed_finished USING e_modified et_good_cells.
**    PERFORM refresh.
*   DATA:LS_MOD_CELL TYPE LVC_S_MODI ,  "应用的修改的单元格
*         stbl TYPE lvc_s_stbl.
**
**    LOOP AT gt_itab INTO wa_itab.
**      wa_itab-cc = wa_itab-bb * 2 .
**      MODIFY gt_itab FROM wa_itab.
**    ENDLOOP.
*    SORT ER_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID.
*    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
*      AT NEW ROW_ID.
*        READ TABLE GT_DATA INTO GS_DATA INDEX LS_MOD_CELL-ROW_ID.
*
*      ENDAT.
*
*      CASE LS_MOD_CELL-FIELDNAME.
*         WHEN 'TD_MATNR'." "被替代物料
*           "获取指定单元格改动后内容
*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*               EXPORTING
*                  I_ROW_ID = LS_MOD_CELL-ROW_ID
*                  I_FIELDNAME = 'TD_MATNR'
*               IMPORTING
*                   E_VALUE  = GS_DATA-TD_MATNR.
*          "查询被替代物料描述
*           SELECT SINGLE MAKTX INTO GS_DATA-TD_MAKTX
*             FROM MAKT
*             WHERE MATNR = GS_DATA-TD_MATNR.
*          "查询被替代物料单位
*           SELECT SINGLE MEINS INTO GS_DATA-TD_MEINS
*            FROM MARA
*            WHERE MATNR = GS_DATA-TD_MATNR .
*
*          ENDCASE.
*      "刷新数据到ALV
*          MODIFY GT_DATA FROM GS_DATA INDEX LS_MOD_CELL-ROW_ID.
*          CLEAR GS_DATA.
*
*    ENDLOOP.
*  "  *   稳定刷新
*    stbl-row = 'X'." 基于行的稳定刷新
*    stbl-col = 'X'." 基于列稳定刷新
*    CALL METHOD GR_ALVGRID->refresh_table_display
*      EXPORTING
*        is_stable = stbl
*      EXCEPTIONS
*      FINISHED  = 1
*      OTHERS    = 2.
*  IF SY-SUBRC <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_onf4.
    FIELD-SYMBOLS <fs_mod_cells> TYPE lvc_t_modi.
    DATA: lw_mod_cell TYPE lvc_s_modi.

    CASE e_fieldname.
      WHEN 'TD_MATNR'.
        READ TABLE gt_data INTO gs_data INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          PERFORM sub_help_matnr CHANGING gs_data-td_matnr.
          IF gs_data-td_matnr IS NOT INITIAL.
            MODIFY gt_data FROM gs_data INDEX es_row_no-row_id.
            ASSIGN er_event_data->m_data->* TO <fs_mod_cells>.
            lw_mod_cell-row_id = es_row_no-row_id.
            lw_mod_cell-sub_row_id = es_row_no-sub_row_id.
            lw_mod_cell-fieldname = 'TD_MATNR'.
            lw_mod_cell-value = gs_data-td_matnr.
            APPEND lw_mod_cell TO <fs_mod_cells>.

          ENDIF.
        ENDIF.

    ENDCASE.

    "**  Inform ALV Grid that event 'onf4' has been processed
    er_event_data->m_event_handled = 'X'.           "告知F4动作结束

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_DATA_BTDWL  text
*----------------------------------------------------------------------*
FORM sub_help_matnr  CHANGING p_td_matnr.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      matchcode_object          = 'MAT1_S_MPN'
*     TABNAME                   = ' '
    IMPORTING
      select_value              = p_td_matnr
    EXCEPTIONS
      invalid_dictionary_field  = 1
      invalid_matchdcode_object = 2
      no_selection              = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM frm_data_changed USING   p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_alvgrid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件



  CALL METHOD gr_alvgrid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.



  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_modify FOR gr_alvgrid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC．  text
*      <--P_CALL  text
*      <--P_METHOD  text
*      <--P_G_REF_GRID_>REFRESH_TABLE_DISP  text
*----------------------------------------------------------------------*
FORM check_input  CHANGING l_subrc TYPE c.
  REFRESH gt_data_sel .
  " REFRESH GT_PROJ_PR.
  LOOP AT gt_data INTO gs_data
      WHERE sel = 'X'.
*   IF GS_DATA-TD_MATNR EQ '' AND GS_DATA-TD_MENGE EQ '' AND  GS_DATA-TD_GRPFLAG EQ ''.
*      CONTINUE.
*   ENDIF.
*    IF  GS_DATA-TD_MATNR EQ ''.
*      MESSAGE '被替代物料不为空'  TYPE 'S' DISPLAY LIKE 'E'.
*         L_SUBRC = 4.
*    ENDIF.

    IF  gs_data-td_matnr NE '' AND gs_data-td_menge EQ 0 .
      MESSAGE '被替代物料不为空，对应记录的折算数不能为0'  TYPE 'S' DISPLAY LIKE 'E'.
      l_subrc = 4.
      EXIT.
    ENDIF.
    IF  gs_data-td_matnr1 NE '' AND gs_data-td_menge1 EQ 0 .
      MESSAGE '被替代物料不为空，对应记录的折算数不能为0'  TYPE 'S' DISPLAY LIKE 'E'.
      l_subrc = 4.
      EXIT.
    ENDIF.
    CLEAR:gs_data_sel .
    MOVE-CORRESPONDING gs_data TO gs_data_sel.
    APPEND gs_data_sel TO gt_data_sel .
    MOVE-CORRESPONDING gs_data TO gs_proj_sel.
    APPEND gs_proj_sel TO gt_proj_sel.
*     APPEND GS_PROJ_PR TO GT_PROJ_PR.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM handle_data_changed_finished  USING    p_e_modified
                                            p_et_good_cells TYPE  lvc_t_modi.
  DATA:lw_cell TYPE lvc_s_modi.
  DATA: ls_stable TYPE lvc_s_stbl.
  "替代物料描述
  READ TABLE   p_et_good_cells INTO lw_cell  WITH KEY fieldname = 'TD_MATNR '.
  IF sy-subrc EQ 0 .
    READ TABLE gt_data ASSIGNING <fs_data> INDEX  lw_cell-row_id.
    IF sy-subrc EQ 0 .
      CLEAR:<fs_data>-td_maktx,<fs_data>-td_meins.
      SELECT SINGLE maktx INTO <fs_data>-td_maktx
            FROM makt
            WHERE matnr = <fs_data>-td_matnr.
      "查询被替代物料单位
      SELECT SINGLE meins INTO <fs_data>-td_meins
       FROM mara
       WHERE matnr = <fs_data>-td_matnr .
      CLEAR:<fs_data>-td_maktx1,<fs_data>-td_meins1.
      SELECT SINGLE maktx INTO <fs_data>-td_maktx1
            FROM makt
            WHERE matnr = <fs_data>-td_matnr1.
      "查询被替代物料单位1
      SELECT SINGLE meins INTO <fs_data>-td_meins1
       FROM mara
       WHERE matnr = <fs_data>-td_matnr1 .

    ENDIF.

  ENDIF.
  "替代物料描述1
  READ TABLE   p_et_good_cells INTO lw_cell  WITH KEY fieldname = 'TD_MATNR1'.
  IF sy-subrc EQ 0 .
    READ TABLE gt_data ASSIGNING <fs_data> INDEX  lw_cell-row_id.
    IF sy-subrc EQ 0 .
      CLEAR:<fs_data>-td_maktx1,<fs_data>-td_meins1.
      SELECT SINGLE maktx INTO <fs_data>-td_maktx1
            FROM makt
            WHERE matnr = <fs_data>-td_matnr1.
      "查询被替代物料单位1
      SELECT SINGLE meins INTO <fs_data>-td_meins1
       FROM mara
       WHERE matnr = <fs_data>-td_matnr1 .

    ENDIF.

  ENDIF.

  "  *   稳定刷新
  ls_stable-row = 'X'." 基于行的稳定刷新
  ls_stable-col = 'X'." 基于列稳定刷新
  CALL METHOD gr_alvgrid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
