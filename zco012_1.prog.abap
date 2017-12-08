REPORT zco012_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20161028
*& Request       :
*& Descriptions  : 公司间采购成本收入计算
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:mkpf,mseg ,makt .
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        bukrs  TYPE bukrs,                                   "公司代码
        mjahr  TYPE mjahr,                                   "会计年度
        monat  TYPE monat,                                   "会计期间
        ndqj   TYPE n LENGTH 6,                              "年度期间
        ebeln  TYPE ebeln,                                   "采购订单
        ebelp  TYPE ebelp,                                   "采购订单行
        kdauf  TYPE kdauf,                                   "销售订单
        kdpos  TYPE kdpos,                                   "销售订单行
        xmms   TYPE string,                                  "项目描述
        matnr  TYPE matnr,                                   "物料编码
        maktx  TYPE maktx,                                   "物料描述
        bqjhsl TYPE menge_d,                                 "本期交货数量
        bqjhcb TYPE dmbtr,                                   "本期交货成本
        bqcbcy TYPE dmbtr,                                   "本期成本差异
        bqsjcb TYPE dmbtr,                                   "本期实际成本
        bqkpsl TYPE menge_d,                                 "本期开票数量
        zyywsr TYPE dmbtr,                                   "主营业务收入
        qqwkps TYPE menge_d,                                 "前期未开票数
        qqgccb TYPE dmbtr,                                   "前期工程成本
        bqwkps TYPE menge_d,                                 "本期未开票数
        bqzycb TYPE dmbtr,                                   "本期主营业务成本
        bqgccb TYPE dmbtr,                                    "本期工程成本
        mfh    TYPE c,                                       "免费行
        sgqrcb TYPE c,                                       "手动确认成本
        kalnr  TYPE ck_kalnr,                                "成本估算号
        bxxh   TYPE ckmlmv005-kalnr,                         "备选消耗
        dqzcy  TYPE dmbtr,                                   "当期总差异
        dqzfh  TYPE menge_d,                                 "当前总发货
        dwcy   TYPE p LENGTH 16 DECIMALS 6,  "menge_d,       "单位差异保留6位小数
        ftcy   TYPE p LENGTH 13 DECIMALS 2,                  "分摊差异
        wccd   TYPE c,                                       "尾差承担
        wc     TYPE p LENGTH 13 DECIMALS 2,                  "尾差
        meins  TYPE meins,                                   "单位
        waers  TYPE waers,                                   "本文币
        cggc   TYPE werks_d,                                 "采购工厂
        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_item,
        mblnr     TYPE mblnr,
        mjahr     TYPE mjahr,
        budat     TYPE budat,
        werks     TYPE werks,
        zeile     TYPE mblpo, "物料凭证行项目
        bwart     TYPE bwart,  "移动类型
        ebeln     TYPE ebeln,  "采购订单
        ebelp     TYPE ebelp,  "采购订单行
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE mseg-mat_kdpos,   "销售订单行项目
        matnr     TYPE mseg-matnr, "物料编码
        menge     TYPE mseg-menge,
        dmbtr     TYPE mseg-dmbtr,
        shkzg     TYPE shkzg,
        meins     TYPE meins,
        wempf     TYPE mseg-wempf,
      END OF ty_item .

TYPES:BEGIN OF ty_kp,
        buks  TYPE bukrs,  "公司代码
        belnr TYPE belnr_d, "会计凭证编号
        gjahr TYPE gjahr,   "年度
        buzei TYPE buzei,   "会计凭证中的行项目数
        shkzg TYPE shkzg,   "借方/贷方标识
        dmbtr TYPE dmbtr,  "
        menge TYPE menge_d,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
      END OF ty_kp .

TYPES:BEGIN OF ty_kp_hj,
        ebeln TYPE ebeln,
        ebelp TYPE c LENGTH 5,
        dmbtr TYPE dmbtr,
        menge TYPE menge_d,
      END OF ty_kp_hj .

TYPES:BEGIN OF ty_matnr ,
        matnr TYPE matnr,
        werks TYPE werks_d,
        maktx TYPE maktx,
      END OF ty_matnr .

TYPES:BEGIN OF ty_xm,
        kdauf TYPE kdauf,
        xmms  TYPE string,
      END OF ty_xm .


TYPES:BEGIN OF ty_cg,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
      END OF ty_cg.

DATA:gt_matnr   TYPE TABLE OF ty_matnr,
     gt_matnr_2 TYPE TABLE OF ty_matnr,
     gs_matnr   TYPE ty_matnr.

DATA:gt_xm   TYPE TABLE OF ty_xm,
     gt_xm_2 TYPE TABLE OF ty_xm,
     gs_xm   TYPE ty_xm.

DATA: gt_cg_1 TYPE TABLE OF ty_cg,
      gt_cg_2 TYPE TABLE OF ty_cg,
      gt_cg   TYPE TABLE OF ty_cg,
      gs_cg   TYPE  ty_cg.


DATA:gt_kp_hj TYPE TABLE OF ty_kp_hj,
     gs_kp_hj TYPE ty_kp_hj.

DATA:gt_kp TYPE TABLE OF ty_kp,
     gs_kp TYPE ty_kp.

DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.

DATA:gt_ekkn TYPE TABLE OF ekkn,
     gs_ekkn TYPE ekkn.

"*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.


DATA: g_objname TYPE thead-tdname.

TYPES:BEGIN OF ty_xs_matnr,
        matnr     TYPE matnr,
        werks     TYPE werks_d, "工厂
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE mseg-mat_kdpos,   "销售订单行项目
      END OF ty_xs_matnr .

*TYPES:BEGIN OF ty_matnr,
*        matnr TYPE matnr, "物料
*        werks TYPE werks_d, "工厂
*      END OF ty_matnr .
*
*DATA:gt_matnr TYPE TABLE OF ty_matnr,
*     gs_matnr TYPE ty_matnr.

TYPES:BEGIN OF ty_item_hj,
        werks     TYPE werks,
        ebeln     TYPE ebeln,  "采购订单
        ebelp     TYPE c LENGTH 5,  "采购订单行
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE c LENGTH 6,   "销售订单行项目
        matnr     TYPE mseg-matnr, "物料编码
        cggc      TYPE werks_d,  "采购工厂
        meins     TYPE meins,    "单位
        bqjhsl    TYPE menge_d,  "本期交货数量
        bqjhcb    TYPE dmbtr,   "本期交货成本
      END OF ty_item_hj .

DATA:gt_xs_matnr TYPE TABLE OF ty_xs_matnr,
     gs_xs_matnr TYPE ty_xs_matnr.

DATA:gt_item TYPE TABLE OF ty_item,
     gs_item TYPE ty_item.

DATA:gt_item_hj TYPE TABLE OF  ty_item_hj,
     gs_item_hj TYPE ty_item_hj.

DATA:gt_ckmlhd TYPE TABLE OF ckmlhd,
     gs_ckmlhd TYPE ckmlhd.

DATA:gt_ckmlhd_xs TYPE TABLE OF ckmlhd,
     gs_ckmlhd_xs TYPE ckmlhd.

DATA:gt_mlcd TYPE TABLE OF mlcd,
     gs_mlcd TYPE mlcd.

DATA:gt_ckmlmv005 TYPE TABLE OF ckmlmv005,
     gs_ckmlmv005 TYPE ckmlmv005.

DATA:gt_ckmlmv005_xs TYPE TABLE OF ckmlmv005,
     gs_ckmlmv005_xs TYPE ckmlmv005.

DATA:gt_ckmlmv_aux_kalnr TYPE TABLE OF ckmlmv_aux_kalnr,
     gs_ckmlmv_aux_kalnr TYPE ckmlmv_aux_kalnr.

TYPES:BEGIN OF ty_matnr_jb,
        matnr TYPE mara-matnr,  "物料号
        werks TYPE marc-werks,   "工厂
        maktx TYPE makt-maktx,    "物料描述

      END OF ty_matnr_jb .

TYPES:BEGIN OF ty_kalnr,
        kalnr TYPE ck_kalnr,  "成本估算号
        menge TYPE menge_d,  "数量
      END OF ty_kalnr .

TYPES:BEGIN OF ty_ljcy,
        kalnr TYPE ck_kalnr,  "成本估算号
        ftcy  TYPE p LENGTH 16 DECIMALS 2,    "分摊差异
      END OF ty_ljcy .

DATA:gt_ljcy    TYPE TABLE OF ty_ljcy,
     gs_ljcy    TYPE ty_ljcy,
     gt_ljcy_hj TYPE TABLE OF ty_ljcy,
     gs_ljcy_hj TYPE ty_ljcy.


DATA:gt_kalnr TYPE TABLE OF ty_kalnr,
     gs_kalnr TYPE ty_kalnr.

DATA:gt_matnr_jb TYPE TABLE OF ty_matnr_jb,
     gs_matnr_jb TYPE ty_matnr_jb.

DATA:gt_data   TYPE TABLE OF ty_data,
     gt_data_2 TYPE TABLE OF ty_data,
     gs_data   TYPE  ty_data.

DATA:gt_zco012_1 TYPE TABLE OF zco012_1,
     gs_zco012_1 TYPE zco012_1.

DATA:l_tabix TYPE i.




FIELD-SYMBOLS: <fs_data> TYPE ty_data .



DATA:p_date_l TYPE  sy-datum,
     p_date_h TYPE sy-datum.



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

  if gw_lvc-fieldname eq 'BQJHSL' " OR gw_lvc-fieldname eq 'BCRK'
     OR gw_lvc-fieldname eq 'BQKPSL' OR gw_lvc-fieldname eq 'QQWKPS'
     OR gw_lvc-fieldname eq 'BQWKPS' OR gw_lvc-fieldname eq 'DQZFH'.
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'MEINS'.
  endif.

  if GW_LVC-FIELDNAME EQ 'BQJHCB' OR GW_LVC-FIELDNAME EQ 'BQCBCY' OR GW_LVC-FIELDNAME EQ 'BQSJCB'
    OR GW_LVC-FIELDNAME EQ 'ZYYWSR' OR GW_LVC-FIELDNAME EQ 'QQGCCB' OR  GW_LVC-FIELDNAME EQ 'BQZYCB'
    OR GW_LVC-FIELDNAME EQ 'BQGCCB' .
    gw_lvc-CFIELDNAME = 'WAERS'.
  endif.






*  IF gw_lvc-fieldname = 'LYTS'
*  OR gw_lvc-fieldname = 'BCLLS'.
*     gw_lvc-NO_ZERO = 'X'.
*  ENDIF.

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

DATA g_edit TYPE c VALUE 'X'. "控制不可编辑

DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
PARAMETERS:p_g1 TYPE char1 RADIOBUTTON GROUP g1,
           p_g2 TYPE char1 RADIOBUTTON GROUP g1.

PARAMETERS:p_bukrs TYPE bukrs,
           p_gjahr TYPE gjahr,
           p_monat TYPE monat.

SELECTION-SCREEN END OF BLOCK b1.

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
 PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check USING VALUE(P_ACTVT)..
 AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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

  "查询期间的最小日期
  IF p_g1 EQ 'X'.
    CONCATENATE p_gjahr p_monat '01' INTO p_date_l .
    "查询期间的最大日期
    CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = p_date_l
      IMPORTING
        e_date = p_date_h.




    SELECT a~mblnr a~mjahr a~budat
           b~zeile b~bwart b~ebeln b~ebelp b~mat_kdauf b~mat_kdpos
           b~matnr b~menge b~dmbtr b~shkzg b~meins b~wempf b~werks
          INTO CORRESPONDING FIELDS OF TABLE gt_item
          FROM mkpf AS a
          INNER JOIN mseg AS b
          ON a~mblnr = b~mblnr
          AND a~mjahr = b~mjahr
          WHERE a~mjahr EQ p_gjahr
          AND b~werks EQ p_bukrs
          AND a~budat BETWEEN p_date_l AND p_date_h
          AND b~bwart IN ('643','644','673','674') .
    SORT gt_item BY ebeln ebelp mat_kdauf mat_kdpos matnr .

    CHECK gt_item IS NOT INITIAL .

    "gt_matnr 存储不重复的物料号
    MOVE-CORRESPONDING gt_item TO gt_matnr .
    SORT gt_matnr BY matnr werks .
    DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr werks .

    "gt_xs_matnr:存储不重复的物料号、工厂、销售订单、销售订单行号
    MOVE-CORRESPONDING gt_item TO gt_xs_matnr .
    SORT gt_xs_matnr BY matnr werks  mat_kdauf mat_kdpos .
    DELETE ADJACENT DUPLICATES FROM gt_xs_matnr COMPARING matnr werks  mat_kdauf mat_kdpos .


    IF gt_xs_matnr IS NOT INITIAL.
      SELECT * INTO TABLE gt_ckmlmv_aux_kalnr
        FROM ckmlmv_aux_kalnr
        FOR ALL ENTRIES IN gt_xs_matnr
        WHERE matnr = gt_xs_matnr-matnr
         AND  bwkey = gt_xs_matnr-werks
         AND  vbeln = gt_xs_matnr-mat_kdauf
         AND  posnr = gt_xs_matnr-mat_kdpos .
      SORT  gt_ckmlmv_aux_kalnr BY matnr bwkey vbeln posnr .
    ENDIF.

    "查询成本估算号
    IF gt_xs_matnr IS NOT INITIAL.

      SELECT * INTO TABLE gt_ckmlhd
       FROM ckmlhd
       FOR ALL ENTRIES IN gt_xs_matnr
       WHERE bwkey = gt_xs_matnr-werks
       AND   matnr = gt_xs_matnr-matnr
       AND   vbeln = gt_xs_matnr-mat_kdauf
       AND   posnr = gt_xs_matnr-mat_kdpos .

      SORT gt_ckmlhd BY bwkey matnr vbeln posnr .
    ENDIF.

    "gt_ckmlhd_xs :存储销售订单不为空的记录
    gt_ckmlhd_xs = gt_ckmlhd .
    DELETE gt_ckmlhd_xs WHERE vbeln EQ ''.
    SORT gt_ckmlhd_xs BY kalnr .

    IF gt_ckmlhd IS NOT INITIAL .
      SELECT * INTO TABLE gt_mlcd
       FROM mlcd
       FOR ALL ENTRIES IN gt_ckmlhd
       WHERE kalnr = gt_ckmlhd-kalnr
        AND  bdatj = p_gjahr
        AND  poper = p_monat
        AND  categ = 'VN'
        AND  ptyp = 'V+'
        AND  curtp = '10' .
      "   AND  bvalt = '25'.
      SORT gt_mlcd BY kalnr  bvalt .
    ENDIF.

    IF gt_matnr IS NOT INITIAL.
      SELECT * INTO TABLE gt_ckmlmv005
      FROM ckmlmv005
      FOR ALL  ENTRIES IN gt_matnr
      WHERE matnr = gt_matnr-matnr
      AND werks = gt_matnr-werks
      AND saknr_nd = '6401020101'
      AND kalnr_mat EQ '000000000000' .

      SORT gt_ckmlmv005 BY matnr werks  .
    ENDIF.

    IF gt_ckmlmv_aux_kalnr IS NOT INITIAL.
      SELECT * INTO TABLE gt_ckmlmv005_xs
      FROM ckmlmv005
      FOR ALL ENTRIES IN gt_ckmlmv_aux_kalnr
      WHERE matnr = gt_ckmlmv_aux_kalnr-matnr
       AND  werks = gt_ckmlmv_aux_kalnr-bwkey
       AND  saknr_nd = '6401020101'
       AND kalnr_mat = gt_ckmlmv_aux_kalnr-kalnr .
      SORT gt_ckmlmv005_xs BY matnr werks kalnr_mat .
*      SELECT * INTO TABLE gt_ckmlmv005_xs
*      FROM ckmlmv005
*      FOR ALL ENTRIES IN gt_ckmlhd_xs
*      WHERE matnr = gt_ckmlhd_xs-matnr
*      AND werks = gt_ckmlhd_xs-bwkey
*      AND saknr_nd = '6401020101'
*      AND kalnr_mat = gt_ckmlhd_xs-kalnr .
*   "   AND kalnr_mat NE '000000000000'.
*      SORT gt_ckmlmv005_xs BY matnr werks saknr_nd .

    ENDIF.

    SELECT * INTO TABLE gt_t001
      FROM t001
      WHERE bukrs = p_bukrs .
  ELSE.
    SELECT * INTO TABLE gt_zco012_1
      FROM zco012_1
      WHERE bukrs = p_bukrs
      AND mjahr = p_gjahr
      AND monat = p_monat .

    SORT gt_zco012_1  BY bukrs mjahr monat ebeln ebelp .
    MOVE-CORRESPONDING gt_zco012_1 TO gt_data .
    SORT gt_data BY bukrs mjahr monat ebeln ebelp  .
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
  DATA:p1_data   TYPE datum.
  DATA: p1_gjahr TYPE gjahr,
        p1_monat TYPE monat.
  p1_data = p_date_l - 1 .
  p1_gjahr = p1_data+0(4).
  p1_monat = p1_data+4(2).
  IF p_g1 EQ 'X'.
    LOOP AT gt_item  INTO gs_item .
      CLEAR:gs_item_hj .
      gs_item_hj-werks = gs_item-werks .
      gs_item_hj-ebeln = gs_item-ebeln.
      gs_item_hj-ebelp = gs_item-ebelp.
      gs_item_hj-mat_kdauf = gs_item-mat_kdauf.
      gs_item_hj-mat_kdpos = gs_item-mat_kdpos.
      gs_item_hj-matnr = gs_item-matnr.
      gs_item_hj-cggc = gs_item-wempf+6(4).
      gs_item_hj-meins = gs_item-meins.
      gs_item_hj-bqjhsl = gs_item-menge.  "本期交货数量
      gs_item_hj-bqjhcb = gs_item-dmbtr .
      IF gs_item-shkzg = 'S'.
        gs_item_hj-bqjhsl = gs_item_hj-bqjhsl  * -1 .
        gs_item_hj-bqjhcb = gs_item_hj-bqjhcb * -1.
      ENDIF.
      COLLECT gs_item_hj INTO gt_item_hj .
    ENDLOOP .
    SORT gt_item_hj BY ebeln ebelp mat_kdauf mat_kdpos matnr .
    DELETE gt_item_hj WHERE bqjhsl  EQ 0 AND bqjhcb EQ 0 .

    "统计成本估算号最大的数量
    LOOP AT  gt_item_hj  INTO  gs_item_hj  .
      CLEAR:gs_kalnr .
      READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY bwkey  = gs_item_hj-werks
                                                   matnr = gs_item_hj-matnr
                                                   vbeln = gs_item_hj-mat_kdauf
                                                   posnr = gs_item_hj-mat_kdpos
                                                   BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_kalnr-kalnr = gs_ckmlhd-kalnr .
        gs_kalnr-menge = gs_item_hj-bqjhsl.
        IF gs_kalnr-menge < 0 .
          gs_kalnr-menge = gs_kalnr-menge * -1 .
        ENDIF.
        APPEND gs_kalnr TO gt_kalnr .
      ENDIF.
    ENDLOOP.

    SORT gt_kalnr BY kalnr ASCENDING menge DESCENDING .

    DELETE ADJACENT DUPLICATES FROM gt_kalnr COMPARING kalnr.

    "gt_cg :存储采购订单及采购订单行记录
    IF gt_item_hj IS NOT INITIAL .
      MOVE-CORRESPONDING gt_item_hj TO gt_cg_1 .
      APPEND LINES OF gt_cg_1 TO gt_cg .
    ENDIF.

    "查询上期间已保存的公司间成本收入表
    SELECT * INTO TABLE gt_zco012_1
      FROM zco012_1
      WHERE bukrs = p_bukrs
      AND mjahr = p1_gjahr
      AND monat = p1_monat
      AND bqwkps NE  0.

    SORT gt_zco012_1  BY bukrs mjahr monat ebeln ebelp .

    IF gt_zco012_1 IS NOT INITIAL .
      MOVE-CORRESPONDING gt_zco012_1 TO gt_cg_2.
      APPEND LINES OF gt_cg_2 TO gt_cg .
    ENDIF.
    SORT gt_cg BY ebeln ebelp .
    DELETE ADJACENT DUPLICATES FROM gt_cg COMPARING ebeln ebelp .


    "查询开票数量、主营业务收入
    IF gt_item_hj IS NOT INITIAL .
      SELECT a~belnr a~gjahr a~monat
             b~buzei b~shkzg b~menge b~dmbtr b~ebeln b~ebelp
       INTO CORRESPONDING FIELDS OF TABLE gt_kp
        FROM bkpf AS a
        INNER JOIN bseg AS b
        ON a~belnr = b~belnr AND a~gjahr = b~gjahr
        FOR ALL ENTRIES IN gt_cg
        WHERE a~bukrs EQ p_bukrs
        AND a~gjahr EQ p_gjahr
        AND a~monat EQ p_monat
        AND b~hkont BETWEEN '6001020000' AND '6001029999'
        AND b~ebeln = gt_cg-ebeln
        AND b~ebelp = gt_cg-ebelp
        AND b~ebeln NE ''..

      SORT gt_kp BY ebeln ebelp belnr gjahr buzei .

      SELECT * INTO TABLE gt_ekpo
        FROM ekpo
        FOR ALL ENTRIES IN gt_item_hj
        WHERE ebeln = gt_item_hj-ebeln
 "       AND ebelp = gt_item_hj-ebelp
        AND repos EQ ''.

      SORT gt_ekpo BY ebeln ebelp .

    ENDIF.


    LOOP AT gt_kp INTO gs_kp.
      CLEAR:gs_kp_hj .
      gs_kp_hj-ebeln = gs_kp-ebeln .
      gs_kp_hj-ebelp = gs_kp-ebelp .
      gs_kp_hj-menge = gs_kp-menge.
      gs_kp_hj-dmbtr = gs_kp-dmbtr .
      IF gs_kp-shkzg EQ 'S'.
        gs_kp_hj-dmbtr = gs_kp_hj-dmbtr * -1 .
        gs_kp_hj-menge = gs_kp_hj-menge * -1 .
      ENDIF.
      COLLECT gs_kp_hj INTO gt_kp_hj .
    ENDLOOP.

    SORT gt_kp_hj BY ebeln ebelp .

    DATA:p_menge TYPE menge_d .
    LOOP AT gt_cg INTO gs_cg .
      CLEAR:gs_data .
      "公司代码
      gs_data-bukrs = p_bukrs.
      gs_data-mjahr = p_gjahr.

      "会计期间
      gs_data-monat = p_monat .
      CONCATENATE gs_data-mjahr gs_data-monat INTO gs_data-ndqj .
      "采购订单
      gs_data-ebeln = gs_cg-ebeln.
      "采购订单行
      gs_data-ebelp = gs_cg-ebelp .
      "先判断在查询期间是否有交货数量的明细
      READ TABLE gt_item_hj INTO gs_item_hj WITH KEY  ebeln = gs_data-ebeln
                                                      ebelp = gs_data-ebelp
                                                      BINARY SEARCH .
      IF sy-subrc EQ 0.
        gs_data-kdauf = gs_item_hj-mat_kdauf . "销售订单
        gs_data-kdpos = gs_item_hj-mat_kdpos .  "销售订单行
        gs_data-matnr = gs_item_hj-matnr .  "物料号
        gs_data-bqjhsl = gs_item_hj-bqjhsl ."本期交货数量
        gs_data-bqjhcb = gs_item_hj-bqjhcb . "本期交货成本
        gs_data-meins = gs_item_hj-meins .  "单位
        gs_data-cggc = gs_item_hj-cggc .  "采购工厂
        READ TABLE gt_kp_hj INTO gs_kp_hj WITH KEY ebeln = gs_data-ebeln
                                                ebelp = gs_data-ebelp
                                                BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_data-bqkpsl = gs_kp_hj-menge .  "本期开票数量
          gs_data-zyywsr = gs_kp_hj-dmbtr.     "主营业务收入
        ENDIF.

        "前期未开票数、前期工程成本
        READ TABLE gt_zco012_1 INTO gs_zco012_1 WITH KEY bukrs = gs_data-bukrs
                                                    mjahr = p1_gjahr
                                                    monat = p1_monat
                                                    ebeln = gs_data-ebeln
                                                    ebelp = gs_data-ebelp
                                                    BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_data-qqwkps = gs_zco012_1-bqwkps .
          gs_data-qqgccb = gs_zco012_1-bqgccb  .
        ENDIF.
        "判断免费行
        READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_data-ebeln
                                                 ebelp = gs_data-ebelp
                                                 BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-mfh = 'X'.


        ELSE.
          " "本期未开票数
          gs_data-bqwkps =  gs_data-bqjhsl + gs_data-qqwkps  - gs_data-bqkpsl .

        ENDIF.
        "成本估算号
        READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY bwkey  = gs_data-bukrs
                                            matnr = gs_data-matnr
                                            vbeln = gs_data-kdauf
                                            posnr = gs_data-kdpos
                                            BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-kalnr = gs_ckmlhd-kalnr .
        ENDIF.


        "备选消耗
        IF gs_data-kdauf IS INITIAL .
          READ TABLE gt_ckmlmv005 INTO gs_ckmlmv005 WITH KEY matnr = gs_data-matnr
                                                             werks = gs_data-bukrs
                                                             BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_data-bxxh  = gs_ckmlmv005-kalnr.
          ENDIF.

        ELSE.
          READ TABLE gt_ckmlmv_aux_kalnr INTO gs_ckmlmv_aux_kalnr
                                         WITH KEY matnr = gs_data-matnr
                                                  bwkey = gs_data-bukrs
                                                  vbeln = gs_data-kdauf
                                                  posnr = gs_data-kdpos
                                                  BINARY SEARCH .
          IF sy-subrc EQ 0 .
            READ TABLE gt_ckmlmv005_xs INTO gs_ckmlmv005_xs WITH KEY matnr = gs_data-matnr
                                                         werks = gs_data-bukrs
                                                         kalnr_mat = gs_ckmlmv_aux_kalnr-kalnr
                                                         BINARY SEARCH .
            IF sy-subrc EQ 0 .
              gs_data-bxxh  = gs_ckmlmv005_xs-kalnr.
            ENDIF.

          ENDIF.


        ENDIF.
        "当期总差异
        READ TABLE gt_mlcd INTO gs_mlcd WITH KEY kalnr = gs_data-kalnr
                                                 bvalt = gs_data-bxxh
                                                BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-dqzcy = gs_mlcd-estprd + gs_mlcd-estkdm + gs_mlcd-mstprd + gs_mlcd-mstkdm .
          "当期总发货
          gs_data-dqzfh = gs_mlcd-lbkum .
          "单位差异
          IF gs_data-dqzfh  NE 0 .
            gs_data-dwcy = gs_data-dqzcy / gs_data-dqzfh .
            "分摊差异
            gs_data-ftcy  = gs_data-bqjhsl * gs_data-dwcy .
          ENDIF.
        ENDIF.
        "尾差承担
        p_menge = abs( gs_data-bqjhsl ).
        READ TABLE gt_kalnr INTO gs_kalnr WITH KEY kalnr = gs_data-kalnr
                                                   menge = p_menge
                                                   BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-wccd = 'X'.
          "尾差
          l_tabix = sy-tabix.

          "   gs_data-wc = gs_data-dqzcy  - gs_data-ftcy .

          DELETE gt_kalnr INDEX l_tabix.
        ENDIF.

        "货币码
        READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = p_bukrs BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-waers = gs_t001-waers .
        ENDIF.

      ELSE.
        READ TABLE gt_zco012_1 INTO gs_zco012_1 WITH KEY ebeln = gs_data-ebeln
                                                     ebelp = gs_data-ebelp
                                                     BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-kdauf = gs_zco012_1-kdauf . "销售订单
          gs_data-kdpos = gs_zco012_1-kdpos .  "销售订单行
          gs_data-matnr = gs_zco012_1-matnr.  "物料编码
          gs_data-mfh   = gs_zco012_1-mfh .   "免费行
          gs_data-kalnr = gs_zco012_1-kalnr .  "成本估算号
          gs_data-bxxh = gs_zco012_1-bxxh .   "备选消耗
          gs_data-meins = gs_zco012_1-meins .  "单位
          gs_data-waers = gs_zco012_1-waers . "货币码
          gs_data-qqwkps = gs_zco012_1-bqwkps ."前期未开票数、
          gs_data-qqgccb = gs_zco012_1-bqgccb  ."前期工程成本
          gs_data-cggc   = gs_zco012_1-cggc  .  "采购工厂
          READ TABLE gt_kp_hj INTO gs_kp_hj WITH KEY ebeln = gs_data-ebeln
                                              ebelp = gs_data-ebelp
                                              BINARY SEARCH.
          IF sy-subrc EQ 0 .
            gs_data-bqkpsl = gs_kp_hj-menge .  "本期开票数量
            gs_data-zyywsr = gs_kp_hj-dmbtr.     "主营业务收入
          ENDIF.

        ENDIF.


      ENDIF.


      APPEND gs_data TO gt_data .
    ENDLOOP.
  ELSE.
    MOVE-CORRESPONDING gt_zco012_1 TO gt_data .
    SORT gt_data BY bukrs mjahr monat ebeln ebelp .

  ENDIF.
  "读取物料描述 、项目描述
  MOVE-CORRESPONDING gt_data TO gt_matnr .
  SORT gt_matnr BY matnr .
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

  SELECT a~matnr b~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr_2
    FROM marc AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    FOR ALL ENTRIES IN gt_matnr
    WHERE a~werks EQ p_bukrs
     AND b~spras = '1'
     AND a~matnr = gt_matnr-matnr .
  SORT gt_matnr_2 BY matnr .

  gt_data_2 = gt_data .
  DELETE gt_data_2 WHERE kdauf IS NOT INITIAL .

  IF gt_data_2 IS NOT INITIAL .
    SELECT * INTO TABLE gt_ekkn
      FROM ekkn
      FOR ALL ENTRIES IN gt_data_2
      WHERE ebeln = gt_data_2-ebeln .
     "    AND ebelp = gt_data_2-ebelp .
    SORT gt_ekkn BY ebeln ." ebelp .
  ENDIF .



  MOVE-CORRESPONDING gt_data TO gt_xm .
  SORT gt_xm BY kdauf .
  "追加参照销售订单转采购订单的项目号
  LOOP  AT  gt_data_2 INTO gs_data.
    READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data-ebeln
                                         "    ebelp = gs_data-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_xm-kdauf  = gs_ekkn-vbeln .
      APPEND gs_xm TO gt_xm.
    ENDIF.
  ENDLOOP.
  DELETE gt_xm WHERE kdauf IS INITIAL .
  SORT gt_xm BY kdauf .
  DELETE ADJACENT DUPLICATES FROM gt_xm COMPARING kdauf .

  LOOP AT gt_xm INTO gs_xm .
    PERFORM selxmmc USING gs_xm-kdauf sy-langu CHANGING gs_xm-xmms.

    MODIFY gt_xm FROM gs_xm.
  ENDLOOP.

  MOVE-CORRESPONDING gt_data TO gt_ljcy.
  DELETE gt_ljcy WHERE ftcy EQ 0 .
  SORT gt_ljcy BY kalnr .

  LOOP AT gt_ljcy INTO gs_ljcy .
    CLEAR :gs_ljcy_hj.
    gs_ljcy_hj-kalnr = gs_ljcy-kalnr.
    gs_ljcy_hj-ftcy = gs_ljcy-ftcy.
    COLLECT gs_ljcy_hj INTO gt_ljcy_hj .
  ENDLOOP.
  SORT gt_ljcy_hj BY kalnr .

  LOOP AT gt_data INTO gs_data .
    READ TABLE gt_matnr_2 INTO gs_matnr WITH KEY matnr = gs_data-matnr
                                          BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-maktx = gs_matnr-maktx .
    ENDIF.
    IF gs_data-kdauf IS NOT INITIAL .
      READ TABLE gt_xm INTO gs_xm WITH KEY kdauf = gs_data-kdauf
                              BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-xmms = gs_xm-xmms .
      ENDIF.
    ELSE.
      READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data-ebeln
                                            "   ebelp = gs_data-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-kdauf = gs_ekkn-vbeln.
       " gs_data-kdpos = gs_ekkn-vbelp .
        READ TABLE gt_xm INTO gs_xm WITH KEY kdauf = gs_data-kdauf
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          gs_data-xmms = gs_xm-xmms .
        ENDIF.
      ENDIF.
    ENDIF.

    "计算尾差
    IF gs_data-wccd EQ 'X'.
      gs_data-wc = gs_data-dqzcy .
      READ TABLE gt_ljcy_hj  INTO gs_ljcy_hj WITH KEY kalnr = gs_data-kalnr
                                             BINARY SEARCH .
      IF sy-subrc EQ 0.
        gs_data-wc = gs_data-dqzcy  - gs_ljcy_hj-ftcy .
      ENDIF.
    ENDIF.


    "本期成本差异 : 分摊差异 + 尾差
    gs_data-bqcbcy =  gs_data-ftcy +  gs_data-wc .
    "本期实际成本 = 本期交货成本 + 本期成本差异
    gs_data-bqsjcb = gs_data-bqjhcb + gs_data-bqcbcy .
    "本期未开票数
    IF gs_data-mfh NE 'X'.
      gs_data-bqwkps =  gs_data-bqjhsl + gs_data-qqwkps - gs_data-bqkpsl .
    ENDIF.
    "本期主营业务成本

    IF gs_data-mfh EQ 'X' OR gs_data-bqwkps EQ 0.
      gs_data-bqzycb = gs_data-bqsjcb + gs_data-qqgccb .

    ELSE.
      IF gs_data-bqjhsl  + gs_data-qqwkps NE 0 .
        gs_data-bqzycb =  ( gs_data-bqsjcb + gs_data-qqgccb ) / ( gs_data-bqjhsl  + gs_data-qqwkps ) *  gs_data-bqkpsl .
      ENDIF.
    ENDIF.

    "本期工程成本
    gs_data-bqgccb = gs_data-bqsjcb +  gs_data-qqgccb - gs_data-bqzycb .


    MODIFY gt_data FROM gs_data .
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
  init_fieldcat 'BUKRS'             '公司代码'               '' '' '' '' '' '' ''.
  init_fieldcat 'MJAHR'             '会计年度'               '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'             '会计期间'               '' '' '' '' '' '' ''.
  init_fieldcat 'EBELN'             '采购订单'               '' '' '' '' '' '' ''.
  init_fieldcat 'EBELP'             '采购订单行'             '' '' '' '' '' '' ''.
  init_fieldcat 'KDAUF'             '销售订单'               '' '' '' '' '' '' ''.
  init_fieldcat 'KDPOS'             '销售订单行'             '' '' '' '' '' '' ''.
  init_fieldcat 'XMMS'              '项目描述'               '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'             '物料编码'               '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'             '物料描述'               '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHSL'            '本期交货数量'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHCB'            '本期交货成本'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQCBCY'            '本期成本差异'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQSJCB'            '本期实际成本'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQKPSL'            '本期开票数量'           '' '' '' '' '' '' ''.
  init_fieldcat 'ZYYWSR'            '主营业务收入'           '' '' '' '' '' '' ''.
  init_fieldcat 'QQWKPS'            '前期未开票数'           '' '' '' '' '' '' ''.
  init_fieldcat 'QQGCCB'            '前期工程成本'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQWKPS'            '本期未开票数'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQZYCB'            '本期主营业务成本'       '' '' '' '' '' '' ''.
  init_fieldcat 'BQGCCB'            '本期工程成本'           '' '' '' '' '' '' ''.
  init_fieldcat 'MFH'               '免费行'                '' '' '' '' '' '' ''.
  init_fieldcat 'SGQRCB'            '手动确认成本'           '' '' 'X' 'X' '' '' ''.
  init_fieldcat 'KALNR'             '成本估算号'             '' '' '' '' '' '' ''.
  init_fieldcat 'BXXH'              '备选消耗'              '' '' '' '' '' '' ''.
  init_fieldcat 'DQZCY'             '当期总差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'DQZFH'             '当期总发货'            '' '' '' '' '' '' ''.
  init_fieldcat 'DWCY'              '单位差异'              '' '' '' '' '' '' ''.
  init_fieldcat 'FTCY'              '分摊差异'              '' '' '' '' '' '' ''.
  init_fieldcat 'WCCD'              '尾差承担'              '' '' '' '' '' '' ''.
  init_fieldcat 'WC'                '尾差'                  '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'             '单位'                  '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'             '本位币'                '' '' '' '' '' '' ''.
  init_fieldcat 'CGGC'              '采购工厂'              '' '' '' '' '' '' ''.


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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0351   text
*      -->P_0352   text
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
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.

FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表
  DATA l_subrc TYPE sy-subrc.
  CLEAR l_subrc.

  "CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
*    WHEN '&IC1'.
*      链接到生产订单确认事务：CO14
*     READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*       IF SY-SUBRC EQ 0 .
*           IF RS_SELFIELD-FIELDNAME = 'AUFNR'.
*              SET PARAMETER ID 'ANR' FIELD  GS_DATA-AUFNR .
*              CALL TRANSACTION 'CO14' AND SKIP  FIRST  SCREEN .
*            ENDIF.
*
*        ENDIF.
*
**打印
*    WHEN '&PRNT'.
*
*      PERFORM frm_print_data.


    WHEN '&SAVE'.
      PERFORM frm_save_data .


  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND             "ALV_PF_STATUS\
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save_data .
  DATA:gt_save TYPE TABLE OF zco012_1,
       gs_save TYPE zco012_1.
  LOOP AT gt_data INTO gs_data WHERE sgqrcb EQ 'X'.
    gs_data-bqwkps = 0  .  "本期开票数量
    gs_data-bqgccb = 0 .   "本期工程成本
    gs_data-bqzycb = gs_data-bqsjcb + gs_data-qqgccb .
    MODIFY gt_data FROM gs_data.
  ENDLOOP.
  REFRESH:gt_save .
  MOVE-CORRESPONDING gt_data TO gt_save .
  LOOP AT gt_save INTO gs_save ..
    gs_save-uname = sy-uname.
    gs_save-udate = sy-datum.
    gs_save-utime = sy-uzeit.
    MODIFY gt_save FROM gs_save.
  ENDLOOP.
  SORT gt_save BY bukrs mjahr monat ebeln ebelp .
  "保存前先删除 按公司代码 、年度 、期间的数据
  DELETE  FROM  zco012_1 WHERE bukrs = p_bukrs
                             AND   mjahr = p_gjahr
                               AND    monat = p_monat .
  "保存数据
  MODIFY zco012_1 FROM TABLE gt_save .

  MESSAGE '数据保存成功!' TYPE 'I'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_XM_KDAUF  text
*      -->P_SY_LANGU  text
*      <--P_GS_XM_XMMC  text
*----------------------------------------------------------------------*
FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = p_yy
      name                    = g_objname
      object                  = 'VBBK'
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
    READ TABLE it_lines INTO wa_lines INDEX 1.
    IF sy-subrc = 0.
      p_xmmc = wa_lines-tdline.
    ENDIF.
  ENDIF.
ENDFORM.
