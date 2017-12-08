REPORT zco012_3.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20161028
*& Request       :
*& Descriptions  : 产品订单成本收入计算
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
        bukrs  TYPE bukrs,                 "公司代码
        mjahr  TYPE mjahr,                 "会计年度
        monat  TYPE monat,                 "会计期间
        ndqj   TYPE n LENGTH 6,            "年度期间
        kdauf  TYPE kdauf,                 "销售订单
        kdpos  TYPE kdpos,                 "销售订单行
        sobkz  TYPE mseg-sobkz,             "特殊库存
        xmms   TYPE string,                "项目描述
        matnr  TYPE matnr,                 "物料编码
        maktx  TYPE maktx,                 "物料描述
        bqjhsl TYPE menge_d,               "本期交货数量
        bqjhcb TYPE dmbtr,                 "本期交货成本
        meins  TYPE meins,                 "单位
        waers  TYPE waers,                 "本文币
        kalnr  TYPE ck_kalnr,              "成本估算号
        bxxh   TYPE ckmlmv005-kalnr,       "备选消耗
        dqzcy  TYPE dmbtr,                 "当期总差异
        dqzfh  TYPE menge_d,               "当期总发货（数量）
        dwcy   TYPE p LENGTH 16 DECIMALS 6,               "保留6位小数单位差异
        ftcy   TYPE dmbtr,               "分摊差异
        wccd   TYPE c,                     "尾差承担
        wc     TYPE dmbtr,               "尾差
        bqcbcy TYPE dmbtr,                 "本期成本差异
        zyywsr TYPE dmbtr,                 "主营业务收入
        cgcy   TYPE dmbtr,                 "采购差异
        zyywcb TYPE dmbtr,                 "主营业务成本
        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_item,
        mblnr     TYPE mblnr,
        mjahr     TYPE mjahr,
        budat     TYPE budat,
        werks     TYPE werks,
        zeile     TYPE mblpo, "物料凭证行项目
        bwart     TYPE bwart,  "移动类型
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE mseg-mat_kdpos,   "销售订单行项目
        matnr     TYPE mseg-matnr, "物料编码
        menge     TYPE mseg-menge,
        meins     TYPE meins,
        dmbtr     TYPE mseg-dmbtr,
        waers     TYPE mseg-waers, "货币码
        shkzg     TYPE shkzg,
        wempf     TYPE mseg-wempf,
        sobkz     TYPE mseg-sobkz, "特殊库存
      END OF ty_item .

TYPES:BEGIN OF ty_item_1,
        mblnr     TYPE mblnr,
        mjahr     TYPE mjahr,
        budat     TYPE budat,
        werks     TYPE werks,
        zeile     TYPE mblpo, "物料凭证行项目
        bwart     TYPE bwart,  "移动类型
*        ebeln     TYPE ebeln,  "采购订单
*        ebelp     TYPE ebelp,  "采购订单行
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE mseg-mat_kdpos,   "销售订单行项目
        vbeln_im  TYPE mseg-vbeln_im,  "交货单
        vbelp_im  TYPE mseg-vbelp_im,  "交货行项目
        matnr     TYPE mseg-matnr, "物料编码
        menge     TYPE mseg-menge,
        dmbtr     TYPE mseg-dmbtr,
        shkzg     TYPE shkzg,
        meins     TYPE meins,
        waers     TYPE mseg-waers,  "货币码
        wempf     TYPE mseg-wempf,

      END OF ty_item_1 .

TYPES:BEGIN OF ty_kp,
        buks  TYPE bukrs,  "公司代码
        belnr TYPE belnr_d, "会计凭证编号
        gjahr TYPE gjahr,   "年度
        buzei TYPE buzei,   "会计凭证中的行项目数
        shkzg TYPE shkzg,   "借方/贷方标识
        dmbtr TYPE dmbtr,  "
        menge TYPE menge_d,
        vbel2 TYPE bseg-vbel2,
        posn2 TYPE bseg-posn2,
      END OF ty_kp .


TYPES:BEGIN OF ty_fimx,
        bukrs TYPE bseg-bukrs,  "公司代码
        belnr TYPE bseg-belnr,   " 会计凭证编号
        gjahr TYPE bseg-gjahr,  "会计凭证年度
        buzei TYPE bseg-buzei,  "会计凭证明细
        shkzg TYPE bseg-shkzg,   "借贷方标识
        dmbtr TYPE bseg-dmbtr,  "本币金额
        hkont TYPE bseg-hkont,  "科目
        vbel2 TYPE bseg-vbel2,   "销售凭证
        posn2 TYPE bseg-posn2,   "销售凭证项目
      END OF ty_fimx.

TYPES:BEGIN OF ty_cgcy,
        vbel2 TYPE bseg-vbel2,   "销售凭证
        posn2 TYPE c LENGTH 6,   "销售凭证项目
        dmbtr TYPE bseg-dmbtr,  "本币金额
      END OF ty_cgcy .

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,
        werks TYPE werks_d,
        maktx TYPE maktx,
      END OF ty_matnr .

TYPES:BEGIN OF ty_xm,
        kdauf TYPE kdauf,
        xmms  TYPE string,
      END OF ty_xm .

DATA:gt_cgcy TYPE TABLE OF ty_cgcy,
     gs_cgcy TYPE ty_cgcy.

DATA:gt_matnr   TYPE TABLE OF ty_matnr,
     gt_matnr_1 TYPE TABLE OF ty_matnr,
     gt_matnr_2 TYPE TABLE OF ty_matnr,
     gs_matnr   TYPE ty_matnr.

DATA:gt_xm TYPE TABLE OF ty_xm,
     gs_xm TYPE ty_xm.




DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.


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

TYPES:BEGIN OF ty_xs,
        mat_kdauf TYPE kdauf,   "销售订单
        mat_kdpos TYPE kdpos,    "销售订单行
      END OF ty_xs.

DATA:gt_fimx TYPE TABLE OF ty_fimx,
     gs_fimx TYPE ty_fimx.


DATA:gt_xs   TYPE TABLE OF ty_xs,
     gt_xs_1 TYPE TABLE OF ty_xs,
     gs_xs   TYPE ty_xs.
*TYPES:BEGIN OF ty_matnr,
*        matnr TYPE matnr, "物料
*        werks TYPE werks_d, "工厂
*      END OF ty_matnr .
*
*DATA:gt_matnr TYPE TABLE OF ty_matnr,
*     gs_matnr TYPE ty_matnr.

TYPES:BEGIN OF ty_item_hj,
        werks     TYPE werks,
        mat_kdauf TYPE mseg-mat_kdauf,  "销售订单
        mat_kdpos TYPE c LENGTH 6,      "销售订单行项目
        matnr     TYPE mseg-matnr,      "物料编码
        sobkz     TYPE mseg-sobkz,      "特殊库存
        meins     TYPE meins,           "单位
        bqjhsl    TYPE menge_d,         "本期交货数量
        waers     TYPE waers,           "货币码
        bqjhcb    TYPE dmbtr,           "本期交货成本
      END OF ty_item_hj .




TYPES:BEGIN OF ty_jhdh,
        vbeln_im TYPE mseg-vbeln_im,  "交货单
        vbelp_im TYPE mseg-vbelp_im,  "交货行项目
      END OF ty_jhdh.

DATA:gt_jhdh TYPE TABLE OF ty_jhdh,
     gs_jhdh TYPE ty_jhdh.

DATA:gt_xs_matnr   TYPE TABLE OF ty_xs_matnr,
     gt_xs_matnr_1 TYPE TABLE OF ty_xs_matnr,
     gs_xs_matnr   TYPE ty_xs_matnr.

DATA:gt_item TYPE TABLE OF ty_item,
     gs_item TYPE ty_item.

DATA:gt_item_1 TYPE TABLE OF ty_item_1,
     gs_item_1 TYPE ty_item_1.

DATA:gt_item_hj TYPE TABLE OF  ty_item_hj,
     gs_item_hj TYPE ty_item_hj.

DATA:gt_item_hj_e TYPE TABLE OF  ty_item_hj,
     gs_item_hj_e TYPE ty_item_hj.

DATA:gt_item_hj_1 TYPE TABLE OF  ty_item_hj,
     gs_item_hj_1 TYPE ty_item_hj.

DATA:gt_ckmlhd   TYPE TABLE OF ckmlhd,
     gt_ckmlhd_1 TYPE TABLE OF ckmlhd,
     gs_ckmlhd   TYPE ckmlhd.



DATA:gt_mlcd TYPE TABLE OF mlcd,
     gs_mlcd TYPE mlcd.

DATA:gt_ckmlmv005   TYPE TABLE OF ckmlmv005,
     gt_ckmlmv005_1 TYPE TABLE OF ckmlmv005,
     gs_ckmlmv005   TYPE ckmlmv005,
     gs_ckmlmv005_1 TYPE ckmlmv005.

DATA:gt_ckmlmv_aux_kalnr TYPE TABLE OF ckmlmv_aux_kalnr,
     gs_ckmlmv_aux_kalnr TYPE ckmlmv_aux_kalnr.

DATA:gt_lips TYPE TABLE OF lips,
     gs_lips TYPE lips.

TYPES:BEGIN OF ty_matnr_jb,
        matnr TYPE mara-matnr,  "物料号
        werks TYPE marc-werks,   "工厂
        maktx TYPE makt-maktx,    "物料描述

      END OF ty_matnr_jb .

TYPES:BEGIN OF ty_kalnr,
        kalnr TYPE ck_kalnr,  "成本估算号
        menge TYPE menge_d,  "数量
      END OF ty_kalnr .

"定义主营业务收入结构
TYPES:BEGIN OF ty_zyywsr,
*        bukrs TYPE ce11000-bukrs,  "公司代码
*        gjahr TYPE ce11000-gjahr,  "年度
*        perde TYPE ce11000-perde,   "期间
        kaufn TYPE ce11000-kaufn,    "销售订单
        kdpos TYPE ce11000-kdpos,    "销售订单行
        vv120 TYPE ce11000-vv120,   "销售收入
      END OF ty_zyywsr .

DATA:gt_zyywsr TYPE TABLE OF ty_zyywsr,  "主营业务收入
     gs_zyywsr TYPE ty_zyywsr .          "主营业务收入

DATA:gt_kalnr TYPE TABLE OF ty_kalnr,
     gs_kalnr TYPE ty_kalnr.

DATA:gt_matnr_jb TYPE TABLE OF ty_matnr_jb,
     gs_matnr_jb TYPE ty_matnr_jb.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE  ty_data.

DATA:gt_zco012_3 TYPE TABLE OF zco012_3,
     gs_zco012_3 TYPE zco012_3.

DATA:gt_ce11000 TYPE TABLE OF ce11000,
     gs_ce11000 TYPE ce11000.

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
     OR gw_lvc-fieldname eq 'DQZFH' .
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'MEINS'.
  endif.

  if GW_LVC-FIELDNAME EQ 'BQJHCB' OR GW_LVC-FIELDNAME EQ 'DQZCY' OR GW_LVC-FIELDNAME EQ 'BQCBCY'
    OR GW_LVC-FIELDNAME EQ 'ZYYWSR' OR GW_LVC-FIELDNAME EQ 'CGCY' OR  GW_LVC-FIELDNAME EQ 'ZYYWCB'
    OR gw_lvc-fieldname eq 'FTCY' OR gw_lvc-fieldname eq 'WC' .
    gw_lvc-CFIELDNAME = 'WAERS'.
  endif.



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
  " *权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
  IF sy-subrc NE 0.
    MESSAGE i011(zfico01) WITH p_bukrs DISPLAY LIKE 'E'.
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
FORM frm_auth_check USING VALUE(p_actvt).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD p_actvt
                                      ID 'BUKRS' FIELD p_bukrs.
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
    "查询特殊库存数据
    SELECT a~mblnr  a~mjahr a~budat
           b~zeile  b~bwart b~mat_kdauf b~mat_kdpos
           b~matnr  b~menge b~dmbtr     b~shkzg
           b~meins  b~werks b~sobkz     b~waers
          INTO CORRESPONDING FIELDS OF TABLE gt_item
          FROM mkpf AS a
          INNER JOIN mseg AS b
          ON a~mblnr = b~mblnr
          AND a~mjahr = b~mjahr
          WHERE a~mjahr EQ p_gjahr
          AND b~werks EQ p_bukrs
          AND a~budat BETWEEN p_date_l AND p_date_h
          AND b~bwart IN ('Z77','Z78','Z79','Z80')
          AND sobkz EQ 'E'.
    SORT gt_item BY  mat_kdauf mat_kdpos matnr .

    IF gt_item IS NOT INITIAL.
      "统计过账销售订单
      MOVE-CORRESPONDING gt_item TO gt_xs .
      SORT gt_xs BY mat_kdauf mat_kdpos .
      DELETE gt_xs WHERE mat_kdauf EQ '' .
      DELETE ADJACENT DUPLICATES FROM gt_xs COMPARING mat_kdauf mat_kdpos .

      "统计过账物料号
      MOVE-CORRESPONDING gt_item TO gt_matnr .
      SORT gt_matnr BY matnr werks .
      DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr werks .

      "统计过账物料、工厂、销售订单、销售行项目
      MOVE-CORRESPONDING gt_item TO gt_xs_matnr.
      SORT gt_xs_matnr BY matnr werks  mat_kdauf mat_kdpos .
      DELETE ADJACENT DUPLICATES FROM gt_xs_matnr COMPARING matnr werks  mat_kdauf mat_kdpos .

      ""查询评估对象的成本评估编号
      IF gt_xs_matnr IS NOT INITIAL.
        SELECT * INTO TABLE gt_ckmlmv_aux_kalnr
          FROM ckmlmv_aux_kalnr
          FOR ALL ENTRIES IN gt_xs_matnr
          WHERE matnr = gt_xs_matnr-matnr
          AND   bwkey = gt_xs_matnr-werks
          AND   vbeln = gt_xs_matnr-mat_kdauf
          AND   posnr = gt_xs_matnr-mat_kdpos .
        SORT gt_ckmlmv_aux_kalnr BY matnr bwkey  vbeln posnr .

        "查询消费替代品
        IF gt_ckmlmv_aux_kalnr  IS NOT INITIAL.
          SELECT * INTO TABLE gt_ckmlmv005
            FROM ckmlmv005
            FOR ALL ENTRIES IN gt_ckmlmv_aux_kalnr
            WHERE matnr = gt_ckmlmv_aux_kalnr-matnr
            AND   werks = gt_ckmlmv_aux_kalnr-bwkey
            AND   saknr_nd = '6401010101'
            AND   kalnr_mat = gt_ckmlmv_aux_kalnr-kalnr .

          SORT  gt_ckmlmv005 BY matnr werks kalnr_mat .

        ENDIF.
      ENDIF.
    ENDIF.


    "查询按库
    SELECT a~mblnr a~mjahr  a~budat
           b~zeile b~bwart  b~vbeln_im b~vbelp_im
           b~matnr b~menge  b~dmbtr    b~shkzg b~meins
           b~werks b~waers
      INTO CORRESPONDING FIELDS OF TABLE gt_item_1
      FROM mkpf AS a
      INNER JOIN mseg AS b
      ON a~mblnr = b~mblnr
      AND a~mjahr = b~mjahr
      WHERE a~mjahr EQ p_gjahr
      AND b~werks EQ p_bukrs
      AND a~budat BETWEEN p_date_l AND p_date_h
      AND b~bwart IN ('Z77','Z78','Z79','Z80')
      AND sobkz EQ ''.

    SORT gt_item_1 BY  vbeln_im vbelp_im matnr .

    IF gt_item_1 IS NOT INITIAL.
      "查询按库的物料、工厂、销售订单、销售行项目 （按库存查询明细的销售信息是空）
      MOVE-CORRESPONDING gt_item_1 TO gt_xs_matnr_1.
      SORT gt_xs_matnr_1 BY matnr werks  mat_kdauf mat_kdpos .
      DELETE ADJACENT DUPLICATES FROM gt_xs_matnr_1 COMPARING matnr werks  mat_kdauf mat_kdpos .

      "查询统计过账物料、工厂
      MOVE-CORRESPONDING gt_item_1 TO gt_matnr_1 .
      SORT gt_matnr_1 BY matnr werks .
      DELETE ADJACENT DUPLICATES FROM gt_matnr_1 COMPARING matnr werks .

      "查询消费替代品
      IF gt_matnr_1 IS NOT INITIAL .
        SELECT * INTO TABLE gt_ckmlmv005_1
        FROM ckmlmv005
        FOR ALL  ENTRIES IN gt_matnr_1
        WHERE matnr = gt_matnr_1-matnr
        AND werks = gt_matnr_1-werks
        AND saknr_nd = '6401020101'
        AND kalnr_mat EQ '' .

        SORT gt_ckmlmv005_1 BY matnr werks  .
      ENDIF.

      "查询统计过账交货单、交货行项目
      MOVE-CORRESPONDING gt_item_1 TO gt_jhdh .
      SORT gt_jhdh BY vbeln_im vbelp_im .


      IF gt_jhdh IS NOT INITIAL.
        "查询生成交货明细的参考依据
        SELECT * INTO TABLE gt_lips
          FROM lips
          FOR ALL ENTRIES IN gt_jhdh
          WHERE vbeln = gt_jhdh-vbeln_im
          AND   posnr = gt_jhdh-vbelp_im.
        SORT gt_lips BY vbeln posnr .
        "根据过账的交货单号查找 销售订单号
        LOOP AT gt_item_1 INTO gs_item_1.
          READ TABLE gt_lips INTO gs_lips WITH KEY vbeln = gs_item_1-vbeln_im
                                                   posnr = gs_item_1-vbelp_im
                                                   BINARY SEARCH.
          IF sy-subrc EQ 0 .
            gs_item_1-mat_kdauf = gs_lips-vgbel.
            gs_item_1-mat_kdpos = gs_lips-vgpos.
          ENDIF.
          MODIFY gt_item_1 FROM gs_item_1.
        ENDLOOP.
      ENDIF.

      "统计按库查询匹对的销售订单、销售行项目
      MOVE-CORRESPONDING gt_item_1 TO gt_xs_1 .
      SORT gt_xs_1  BY mat_kdauf mat_kdpos .
      DELETE gt_xs_1  WHERE mat_kdauf EQ '' .
      DELETE ADJACENT DUPLICATES FROM gt_xs_1  COMPARING mat_kdauf mat_kdpos .

      "追加按库的销售订单到 特殊库存按单的销售订单汇总
      APPEND LINES OF gt_xs_1 TO gt_xs .
      SORT gt_xs  BY mat_kdauf mat_kdpos .
      DELETE gt_xs  WHERE mat_kdauf EQ '' .
      DELETE ADJACENT DUPLICATES FROM gt_xs COMPARING mat_kdauf mat_kdpos .

      "追加：gt_xs_matnr_1 到 gt_xs_matnr
      APPEND LINES OF gt_xs_matnr_1 TO gt_xs_matnr.
      SORT gt_xs_matnr BY matnr werks  mat_kdauf mat_kdpos .
      DELETE ADJACENT DUPLICATES FROM gt_xs_matnr COMPARING matnr werks  mat_kdauf mat_kdpos .

    ENDIF.


    "根据汇总销售消息查找 查询主营业务收入明细
    IF gt_xs IS NOT INITIAL.
      SELECT a~bukrs a~belnr a~gjahr b~buzei
             b~shkzg b~dmbtr b~hkont b~vbel2
             b~posn2
         INTO CORRESPONDING FIELDS OF TABLE gt_fimx
         FROM  bkpf AS a
         INNER JOIN bseg AS b
         ON a~bukrs = b~bukrs
          AND a~belnr = b~belnr
        AND a~gjahr = b~gjahr
        FOR ALL ENTRIES IN gt_xs
        WHERE a~bukrs = p_bukrs
        AND   a~gjahr = p_gjahr
        AND   a~monat = p_monat
        AND   a~blart = 'SA'
        AND   b~vbel2 = gt_xs-mat_kdauf
        AND   b~posn2 = gt_xs-mat_kdpos
        AND   b~hkont BETWEEN '6401000000' AND '6401999999' .
      SORT gt_fimx BY vbel2 posn2 .
    ENDIF.


    "根据按单 、库统计物料、工厂、销售订单、销售行项信息查询估算号
    IF gt_xs_matnr IS NOT INITIAL .
      SELECT * INTO TABLE gt_ckmlhd
     FROM ckmlhd
     FOR ALL ENTRIES IN gt_xs_matnr
     WHERE bwkey = gt_xs_matnr-werks
     AND   matnr = gt_xs_matnr-matnr
     AND   vbeln = gt_xs_matnr-mat_kdauf
     AND   posnr = gt_xs_matnr-mat_kdpos .
      SORT gt_ckmlhd BY  matnr bwkey vbeln posnr .
    ENDIF.

    "查询物料分类账汇总信息
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
      "AND  bvalt = '15'.
      SORT gt_mlcd BY kalnr bvalt.
    ENDIF.

    IF gt_xs IS NOT INITIAL.
      SELECT * INTO TABLE gt_ce11000
        FROM ce11000
        FOR ALL ENTRIES IN gt_xs
        WHERE bukrs = p_bukrs
        AND gjahr = p_gjahr
        AND perde = p_monat
        AND kaufn = gt_xs-mat_kdauf
        AND kdpos = gt_xs-mat_kdpos
        AND paledger = '02'
        AND vrgar = 'F'.

      SORT gt_ce11000 BY  kaufn kdpos .

    ENDIF.

    "查询公司代码信息

    SELECT * INTO TABLE gt_t001
      FROM t001
      WHERE bukrs = p_bukrs .
  ELSE.
    SELECT * INTO TABLE gt_zco012_3
      FROM zco012_3
      WHERE bukrs = p_bukrs
      AND mjahr = p_gjahr
      AND monat = p_monat .

    SORT gt_zco012_3  BY bukrs mjahr monat kdauf kdpos .
    MOVE-CORRESPONDING gt_zco012_3 TO gt_data .
    SORT gt_data BY bukrs mjahr monat kdauf kdpos  .
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

  IF p_g1 EQ 'X'.
    "汇总：有特殊库存过账明细数据
    LOOP AT gt_item  INTO gs_item .
      CLEAR:gs_item_hj .
      gs_item_hj-werks = gs_item-werks .               "工厂
      gs_item_hj-mat_kdauf = gs_item-mat_kdauf.        "销售订单
      gs_item_hj-mat_kdpos = gs_item-mat_kdpos.        "销售行项目
      gs_item_hj-matnr = gs_item-matnr.                "物料号
      gs_item_hj-sobkz = gs_item-sobkz.                "特殊库存
      gs_item_hj-meins = gs_item-meins.                "单位
      gs_item_hj-bqjhsl = gs_item-menge.               "本期交货数量
      gs_item_hj-waers = gs_item-waers.                "货币码
      gs_item_hj-bqjhcb = gs_item-dmbtr .              "本期交货成本
      IF gs_item-shkzg = 'S'.
        gs_item_hj-bqjhsl = gs_item_hj-bqjhsl  * -1 .
        gs_item_hj-bqjhcb = gs_item_hj-bqjhcb * -1.
      ENDIF.
      COLLECT gs_item_hj INTO gt_item_hj .
    ENDLOOP .
    SORT gt_item_hj BY   werks mat_kdauf mat_kdpos matnr .
    DELETE gt_item_hj WHERE bqjhsl  EQ 0 AND bqjhcb EQ 0 .



    "汇总按库查询过账明细数据
    LOOP AT gt_item_1  INTO gs_item_1 .
      CLEAR:gs_item_hj_1 .
      gs_item_hj_1-werks = gs_item_1-werks .            "工厂
      gs_item_hj_1-mat_kdauf = gs_item_1-mat_kdauf.     "销售订单
      gs_item_hj_1-mat_kdpos = gs_item_1-mat_kdpos.      "销售行项目
      gs_item_hj_1-matnr = gs_item_1-matnr.              "物料号
      gs_item_hj_1-meins = gs_item_1-meins.              "单位
      gs_item_hj_1-bqjhsl = gs_item_1-menge.              "本期交货数量
      gs_item_hj_1-waers = gs_item_1-waers.                   "货币码
      gs_item_hj_1-bqjhcb = gs_item_1-dmbtr .             "本期交货成本
      IF gs_item_1-shkzg = 'S'.
        gs_item_hj_1-bqjhsl = gs_item_hj_1-bqjhsl  * -1 .
        gs_item_hj_1-bqjhcb = gs_item_hj_1-bqjhcb * -1.
      ENDIF.
      COLLECT gs_item_hj_1 INTO gt_item_hj_1 .
    ENDLOOP .
    SORT gt_item_hj_1 BY  werks mat_kdauf mat_kdpos matnr .
    DELETE gt_item_hj_1 WHERE bqjhsl  EQ 0 AND bqjhcb EQ 0 .

    "追加按库汇总数据到到按单数据
    APPEND LINES OF gt_item_hj_1 TO gt_item_hj .
    SORT  gt_item_hj BY werks mat_kdauf mat_kdpos matnr .

    "统计采购差异
    LOOP AT gt_fimx INTO gs_fimx .
      CLEAR:gs_cgcy .
      IF gs_fimx-shkzg EQ 'H'.
        gs_fimx-dmbtr = gs_fimx-dmbtr * -1.
      ENDIF.
      gs_cgcy-vbel2 = gs_fimx-vbel2.   "销售订单
      gs_cgcy-posn2 = gs_fimx-posn2.   "销售行项目
      gs_cgcy-dmbtr = gs_fimx-dmbtr.
      COLLECT gs_cgcy INTO gt_cgcy .
    ENDLOOP.

    SORT gt_cgcy BY vbel2 posn2 .

    "统计成本估算号最大的数量
    LOOP AT  gt_item_hj  INTO  gs_item_hj  .
      CLEAR:gs_kalnr .
      "若统计数据不是特殊库存，清掉销售订单信息
      IF gs_item_hj-sobkz EQ ''.
        CLEAR:gs_item_hj-mat_kdauf ,gs_item_hj-mat_kdpos.
      ENDIF.
      "查询成本估算号
      READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY
                                             matnr = gs_item_hj-matnr
                                             bwkey  = gs_item_hj-werks
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

    "统计主营业务收入
    LOOP AT gt_ce11000 INTO gs_ce11000.
      CLEAR:gs_zyywsr.
*      gs_zyywsr-bukrs = gs_ce11000-bukrs ."公司代码
*      gs_zyywsr-gjahr = gs_ce11000-gjahr. "年度
*      gs_zyywsr-perde = gs_ce11000-perde. "期间
      gs_zyywsr-kaufn = gs_ce11000-kaufn.  "销售订单
      gs_zyywsr-kdpos = gs_ce11000-kdpos.  "销售订单行
      gs_zyywsr-vv120 = gs_ce11000-vv120.   "销售收入
      COLLECT gs_zyywsr INTO gt_zyywsr.
    ENDLOOP.
    SORT gt_zyywsr BY kaufn kdpos ."bukrs gjahr perde

    DATA:p_menge TYPE menge_d .

    LOOP AT gt_item_hj INTO gs_item_hj.
      CLEAR:gs_data .

      gs_data-bukrs = p_bukrs. "公司代码
      gs_data-mjahr = p_gjahr. "会计年度
      gs_data-monat = p_monat . "会计期间
      CONCATENATE gs_data-mjahr gs_data-monat INTO gs_data-ndqj . "年度期间

      gs_data-kdauf = gs_item_hj-mat_kdauf .   "销售订单
      gs_data-kdpos = gs_item_hj-mat_kdpos .   "销售订单行
      gs_data-matnr = gs_item_hj-matnr .       "物料号
      gs_data-meins = gs_item_hj-meins .       "单位
      gs_data-bqjhsl = gs_item_hj-bqjhsl      ."本期交货数量
      gs_data-waers = gs_item_hj-waers.        "货币码
      gs_data-bqjhcb = gs_item_hj-bqjhcb .     "本期交货成本
      gs_data-sobkz  = gs_item_hj-sobkz .      "特殊库存

      IF gs_data-sobkz EQ ''.
        CLEAR:gs_item_hj-mat_kdauf,gs_item_hj-mat_kdpos.
      ENDIF.

      "成本估算号
      READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY
                                          matnr = gs_item_hj-matnr
                                          bwkey  = gs_item_hj-werks
                                          vbeln = gs_item_hj-mat_kdauf
                                          posnr = gs_item_hj-mat_kdpos
                                          BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kalnr = gs_ckmlhd-kalnr .
      ENDIF.

      "备选消耗
      IF gs_data-sobkz EQ ''..
        READ TABLE gt_ckmlmv005_1 INTO gs_ckmlmv005_1 WITH KEY matnr = gs_data-matnr
                                                               werks = gs_data-bukrs
                                                               BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-bxxh  = gs_ckmlmv005_1-kalnr.
        ENDIF.

      ELSE.
        READ TABLE gt_ckmlmv_aux_kalnr INTO gs_ckmlmv_aux_kalnr WITH KEY matnr = gs_data-matnr
                                                                         bwkey = p_bukrs
                                                                         vbeln = gs_data-kdauf
                                                                         posnr = gs_data-kdpos
                                                                 BINARY SEARCH .
        IF sy-subrc EQ 0 .

          READ TABLE gt_ckmlmv005 INTO gs_ckmlmv005 WITH KEY matnr = gs_data-matnr
                                                    werks = gs_data-bukrs
                                                    kalnr_mat = gs_ckmlmv_aux_kalnr-kalnr
                                                    BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_data-bxxh  = gs_ckmlmv005-kalnr.
          ENDIF.

        ENDIF.
      ENDIF.

      "当期总差异
      READ TABLE gt_mlcd INTO gs_mlcd WITH KEY kalnr = gs_data-kalnr
                                               bvalt = gs_data-bxxh
                                              BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-dqzcy = gs_mlcd-estprd + gs_mlcd-estkdm + gs_mlcd-mstprd + gs_mlcd-mstkdm  .
        "当期总发货(数量)
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
        gs_data-wc = gs_data-dqzcy  - gs_data-ftcy .
      ENDIF.


      "本期成本差异 : 分摊差异 + 尾差
      gs_data-bqcbcy =  gs_data-ftcy +  gs_data-wc .

      "主营业务收入
      READ TABLE gt_zyywsr INTO gs_zyywsr WITH KEY "bukrs = gs_data-bukrs
                                                "  gjahr = gs_data-mjahr
                                                 "  perde = gs_data-monat
                                                   kaufn = gs_data-kdauf
                                                   kdpos = gs_data-kdpos
                                                   BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-zyywsr = gs_zyywsr-vv120 .

      ENDIF.

      "采购差异
      READ TABLE gt_cgcy INTO gs_cgcy WITH KEY vbel2 = gs_data-kdauf
                                               posn2 = gs_data-kdpos
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-cgcy = gs_cgcy-dmbtr.
      ENDIF.
      "主营业务成本
      gs_data-zyywcb = gs_data-bqjhcb + gs_data-bqcbcy + gs_data-cgcy .

      APPEND gs_data TO gt_data .
    ENDLOOP.

  ELSE.
    MOVE-CORRESPONDING gt_zco012_3 TO gt_data .
    SORT gt_data BY bukrs mjahr monat kdauf kdpos .

  ENDIF.
  "读取物料描述 、项目描述
  MOVE-CORRESPONDING gt_data TO gt_matnr .
  SORT gt_matnr BY matnr .
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

  IF gt_matnr IS NOT INITIAL.
    SELECT a~matnr b~maktx
 INTO CORRESPONDING FIELDS OF TABLE gt_matnr_2
 FROM marc AS a
 INNER JOIN makt AS b
 ON a~matnr = b~matnr
 FOR ALL ENTRIES IN gt_matnr
 WHERE a~werks EQ p_bukrs
  AND b~spras = '1'
  AND a~matnr = gt_matnr-matnr .
    SORT gt_matnr_2 BY matnr werks .
  ENDIF.


  MOVE-CORRESPONDING gt_data TO gt_xm .
  SORT gt_xm BY kdauf .
  DELETE gt_xm WHERE kdauf IS INITIAL .
  DELETE ADJACENT DUPLICATES FROM gt_xm COMPARING kdauf .

  "读取项目描述
  LOOP AT gt_xm INTO gs_xm .
    PERFORM selxmmc USING gs_xm-kdauf sy-langu CHANGING gs_xm-xmms.

    MODIFY gt_xm FROM gs_xm.
  ENDLOOP.

  LOOP AT gt_data INTO gs_data .
    READ TABLE gt_matnr_2 INTO gs_matnr WITH KEY matnr = gs_data-matnr
                                          BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-maktx = gs_matnr-maktx .
    ENDIF.
    READ TABLE gt_xm INTO gs_xm WITH KEY kdauf = gs_data-kdauf
                                BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-xmms = gs_xm-xmms .
    ENDIF.
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
  init_fieldcat 'BUKRS'            '公司代码'            '' '' '' '' '' '' ''.
  init_fieldcat 'MJAHR'            '会计年度'            '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'            '会计期间'            '' '' '' '' '' '' ''.
  init_fieldcat 'KDAUF'            '销售订单'            '' '' '' '' '' 'VBAK' 'VBELN'.
  init_fieldcat 'KDPOS'            '销售订单行'            '' '' '' '' '' '' ''.
  init_fieldcat 'XMMS'            '项目描述'            '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'            '物料编码'            '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'            '物料描述'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHSL'            '本期交货数量'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHCB'            '本期交货成本'            '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'            '单位'            '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'            '本位币'            '' '' '' '' '' '' ''.
  init_fieldcat 'KALNR'            '成本估算号'            '' '' '' '' '' '' ''.
  init_fieldcat 'BXXH'            '备选消耗'            '' '' '' '' '' '' ''.
  init_fieldcat 'DQZCY'            '当期总差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'DQZFH'            '当期总发货'            '' '' '' '' '' '' ''.
  init_fieldcat 'DWCY'            '单位差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'FTCY'            '分摊差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'WCCD'            '尾差承担'            '' '' '' '' '' '' ''.
  init_fieldcat 'WC'            '尾差'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQCBCY'            '本期成本差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'ZYYWSR'            '主营业务收入'            '' '' '' '' '' '' ''.
  init_fieldcat 'CGCY'            '采购差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'ZYYWCB'            '主营业务成本'            '' '' '' '' '' '' ''.

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
  DATA:gt_save TYPE TABLE OF zco012_3,
       gs_save TYPE zco012_3.

  REFRESH:gt_save .
  MOVE-CORRESPONDING gt_data TO gt_save .
  LOOP AT gt_save INTO gs_save ..
    gs_save-uname = sy-uname.
    gs_save-udate = sy-datum.
    gs_save-utime = sy-uzeit.
    MODIFY gt_save FROM gs_save.
  ENDLOOP.
  SORT gt_save BY bukrs mjahr monat kdauf  kdpos .
  "保存前先删除 按公司代码 、年度 、期间的数据
  DELETE  FROM  zco012_3 WHERE bukrs = p_bukrs
                             AND   mjahr = p_gjahr
                               AND    monat = p_monat .
  "保存数据
  MODIFY zco012_3 FROM TABLE gt_save .

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
