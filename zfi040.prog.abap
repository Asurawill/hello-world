REPORT zfi040.
"DES  项目明细账-PS
"DATE 20160621
"ADDED BY IT02
TABLES:bseg,bkpf,proj,prps,skat .

TYPES:BEGIN OF ty_item,
        bukrs TYPE bukrs,  "公司代码
        pspid TYPE ps_pspid, "项目定义
        xmmc  TYPE string,   "项目名称
        hkont TYPE hkont,     "科目
        kmms  TYPE string,    "科目描述
        belnr TYPE belnr_d,    "会计凭证
        buzei TYPE buzei,     "凭证行号
        dmbtr TYPE dmbtr,    "本币码
        gjahr TYPE gjahr,   "会计年度
        bktxt TYPE bktxt,     "摘要
        jfje  TYPE dmbtr,     "借方金额
        dfje  TYPE dmbtr,     "贷方金额
        fx    TYPE string,    "方向
        waers TYPE waers,     "货币码
        ye    TYPE dmbtr,     "余额
        matnr TYPE matnr,     "物料号
        maktx TYPE maktx,     "物料描述
        shkzg TYPE shkzg,      "借贷标识
        xnegp TYPE xnegp,     "反记账
        xref3 TYPE xref3,     "参考码3
        blart TYPE blart,     "凭证类型
        budat TYPE budat,     "过账日期
        kunnr TYPE kunnr,      "客户
        khms  TYPE string,     "客户描述
        lifnr TYPE lifnr,     "供应商
        gysms TYPE string,   "供应商描述
        projk TYPE ps_psp_pnr, "WBS
        zuonr TYPE dzuonr,    "分配
        sel,

      END OF ty_item .

TYPES:BEGIN OF ty_qc,
        pspid TYPE ps_pspid, "项目定义
        hkont TYPE hkont,    "科目
        jfje  TYPE dmbtr,     "借方金额
        dfje  TYPE dmbtr,     "贷方金额
        fx    TYPE string,    "方向
        ye    TYPE dmbtr,     "余额
      END OF ty_qc .


DATA:gs_item TYPE ty_item,
     gt_item TYPE TABLE OF ty_item.



DATA:gs_item_2 TYPE ty_item,
     gt_item_2 TYPE TABLE OF ty_item.

DATA:gs_item_qc TYPE ty_item,
     gt_item_qc TYPE TABLE OF ty_item.

DATA:gs_item_qc_2 TYPE ty_item,
     gt_item_qc_2 TYPE TABLE OF ty_item.

DATA:gt_data   TYPE TABLE OF ty_item,
     gs_data   TYPE ty_item,
     gs_data_1 TYPE ty_item.


DATA:gt_data_hz TYPE TABLE OF ty_item,
     gs_data_hz TYPE ty_item.

DATA:gt_data_sum TYPE TABLE OF ty_item,
     gs_data_sum TYPE ty_item.

DATA:gs_qc TYPE ty_qc,
     gt_qc TYPE TABLE OF ty_qc.

DATA:gt_skat TYPE TABLE OF skat,
     gs_skat TYPE skat.

DATA:gt_t001 TYPE  TABLE OF t001,
     gs_t001 TYPE t001.

DATA:gt_proj TYPE TABLE OF proj,
     gs_proj TYPE proj.

DATA:gt_prps TYPE TABLE OF prps,
     gs_prps TYPE prps.

DATA:gt_lfa1 TYPE TABLE OF lfa1,
     gs_lfa1 TYPE lfa1.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_kna1 TYPE TABLE OF kna1,
     gs_kna1 TYPE kna1.

RANGES:r_hkont FOR bseg-hkont.


FIELD-SYMBOLS:" <FS_DATA>  LIKE GS_DATA,
  <fs_qc>   LIKE gs_qc,
  <fs_item> LIKE gs_item.


DEFINE init_fieldcat.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  GW_LVC-KEY = &4.
*  IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
  IF &1 = 'JFJE' OR &1 = 'DFJE' OR &1 = 'YE' ." OR  &1 = 'YZCB'.
  GW_LVC-CFIELDNAME = 'WAERS'.
 ENDIF.
* IF &1 = 'YZCB'.
*   GW_LVC-NO_SIGN = ''.
*   GW_LVC-DECIMALS = 2.
* ENDIF.
* IF &1 = 'JZKM' OR &1 = 'DFKM' .
*   gw_lvc-f4availabl = 'X'.
* ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_TABLE = &8.
  GW_LVC-REF_FIELD = &9.
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

DATA:g_waers TYPE waers .  "货币码



************************************************************************
* GLOBAL VARIANT
************************************************************************


************************************************************************
* CONSTANT
************************************************************************

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETER:
p_bukrs  TYPE bkpf-bukrs OBLIGATORY.                      "公司代码
SELECT-OPTIONS: s_budat  FOR bkpf-budat OBLIGATORY,    "过账日期
                s_pspid  FOR proj-pspid  OBLIGATORY,
                s_hkont  FOR bseg-hkont .

SELECTION-SCREEN END OF BLOCK blk1.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1001000000'.
  r_hkont-high = '1001999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1002000000'.
  r_hkont-high = '1002999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1012000000'.
  r_hkont-high = '1012999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1121000000'.
  r_hkont-high = '1123999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1241000000'.
  r_hkont-high = '1241999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1401000000'.
  r_hkont-high = '1401999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1408000000'.
  r_hkont-high = '1408999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '2201000000'.
  r_hkont-high = '2202999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1121000000'.
  r_hkont-high = '1123999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1241000000'.
  r_hkont-high = '1241999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1401000000'.
  r_hkont-high = '1401999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '1408000000'.
  r_hkont-high = '1408999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '2201000000'.
  r_hkont-high = '2203999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '2221030500'.
  r_hkont-high = '2221030599'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '2241000000'.
  r_hkont-high = '2241999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '5401000000'.
  r_hkont-high = '5401999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '6001000000'.
  r_hkont-high = '6001999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '6401000000'.
  r_hkont-high = '6401999999'.
  APPEND r_hkont.
  r_hkont-sign = 'I'.
  r_hkont-option = 'BT'.
  r_hkont-low = '8000000000'.
  r_hkont-high = '8000999999'.
  APPEND r_hkont.
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

  "权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
  IF sy-subrc NE 0.
    MESSAGE i011(zfico01) WITH p_bukrs DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

  "*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0401   text
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

  "取出项目项目信息
  SELECT * INTO TABLE gt_proj
    FROM proj
    WHERE pspid IN s_pspid
                    AND   vbukr EQ p_bukrs  .
  SORT gt_proj BY pspnr .


  CHECK gt_proj IS NOT INITIAL .

  "取WBS信息
  SELECT * INTO TABLE gt_prps
    FROM prps
    WHERE posid IN s_pspid.
  .
  SORT gt_prps BY pspnr.

  "取出查询期间过账信息
  SELECT a~bukrs  a~gjahr a~belnr  a~budat  a~bktxt a~blart
    b~buzei b~hkont b~shkzg b~dmbtr  b~matnr b~xref3 b~xnegp
     b~kunnr b~lifnr      b~projk
    INTO CORRESPONDING FIELDS OF TABLE gt_item_2
    FROM bkpf AS a
    INNER JOIN  bseg AS b
    ON a~bukrs = b~bukrs
    AND a~gjahr = b~gjahr
    AND a~belnr = b~belnr
     WHERE a~bukrs EQ p_bukrs
   AND a~budat IN s_budat
   AND a~blart NOT IN ('SC','AB')
   AND b~hkont IN s_hkont
   AND b~hkont IN r_hkont
   AND b~projk IN ( SELECT pspnr  FROM  prps
                    WHERE  posid IN s_pspid
                AND pbukr  EQ p_bukrs ) .

  SORT gt_item_2 BY projk  hkont .

  "取出查询期间过账信息
  SELECT a~bukrs  a~gjahr a~belnr  a~budat  a~bktxt a~blart
    b~buzei b~hkont b~shkzg b~dmbtr  b~matnr b~xref3 b~xnegp
      b~kunnr b~lifnr  b~zuonr AS pspid
    APPENDING CORRESPONDING FIELDS OF TABLE gt_item
    FROM bkpf AS a
    INNER JOIN  bseg AS b
    ON a~bukrs = b~bukrs
    AND a~gjahr = b~gjahr
    AND a~belnr = b~belnr
     WHERE a~bukrs EQ p_bukrs
   AND a~budat IN s_budat
   AND a~blart EQ 'SC'
   AND b~hkont IN s_hkont
     AND b~hkont IN r_hkont
   AND b~zuonr IN ( SELECT pspid  FROM  proj
                    WHERE  pspid IN s_pspid
                AND vbukr  EQ p_bukrs ) .

  SORT gt_item BY pspid hkont budat.

  "查询小于最小期间的过账明细
  SELECT a~bukrs  a~gjahr a~belnr
     b~buzei b~hkont b~shkzg b~dmbtr b~xnegp
      b~projk
     INTO CORRESPONDING FIELDS OF TABLE gt_item_qc_2
     FROM bkpf AS a
     INNER JOIN  bseg AS b
     ON a~bukrs = b~bukrs
     AND a~gjahr = b~gjahr
     AND a~belnr = b~belnr
      WHERE a~bukrs EQ p_bukrs
    AND a~budat < s_budat-low
    AND a~blart NOT IN ('SC','AB')
    AND b~hkont IN s_hkont
    AND b~hkont IN r_hkont
    AND b~projk IN ( SELECT pspnr  FROM  prps
                     WHERE  posid IN s_pspid
                 AND pbukr  EQ p_bukrs ) .
  SORT gt_item_qc_2 BY projk hkont .

  "查询小于最小期间的过账明细
  SELECT a~bukrs  a~gjahr a~belnr   a~blart
    b~buzei b~hkont b~shkzg b~dmbtr  b~xnegp
     b~zuonr AS pspid
    APPENDING CORRESPONDING FIELDS OF TABLE gt_item_qc
    FROM bkpf AS a
    INNER JOIN  bseg AS b
    ON a~bukrs = b~bukrs
    AND a~gjahr = b~gjahr
    AND a~belnr = b~belnr
     WHERE a~bukrs EQ p_bukrs
   AND a~budat < s_budat-low
   AND a~blart EQ 'SC'
   AND b~hkont IN s_hkont
     AND b~hkont IN r_hkont
   AND b~zuonr IN ( SELECT pspid  FROM  proj
                    WHERE  pspid IN s_pspid
                AND vbukr  EQ p_bukrs ) .

  SORT gt_item_qc BY pspid hkont budat.

  "查询供应商主数据信息
  SELECT * INTO TABLE gt_lfa1
    FROM lfa1
    .
  SORT gt_lfa1 BY lifnr .

  "查询物料主数据信息
  SELECT * INTO TABLE gt_makt
    FROM makt
    WHERE spras = '1'.

  SORT gt_makt BY matnr .

  "查询客户主数据信息
  SELECT * INTO TABLE gt_kna1
    FROM kna1
    .

  SORT gt_kna1 BY kunnr.

  "查询总账科目主数据信息
  SELECT * INTO TABLE gt_skat
    FROM skat
    WHERE spras = '1'
    AND ktopl = '1000'.
  SORT gt_skat BY saknr .

  "查询公司的本币货币码
  SELECT SINGLE waers INTO g_waers FROM t001 WHERE  bukrs = p_bukrs .

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
  "按项目号 、科目维度 汇总期初
  LOOP AT gt_item_qc INTO gs_item_qc .
    "项目
    CLEAR:gs_qc.
    gs_qc-pspid = gs_item_qc-pspid.
    gs_qc-hkont = gs_item_qc-hkont .
    IF gs_item_qc-shkzg EQ 'S' .
      IF gs_item_qc-xnegp EQ ''.
        gs_qc-jfje = gs_item_qc-dmbtr.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ELSE.
        gs_qc-dfje = gs_item_qc-dmbtr * -1.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ENDIF.
    ELSE.
      IF gs_item_qc-xnegp EQ ''.
        gs_qc-dfje = gs_item_qc-dmbtr.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ELSE.
        gs_qc-jfje = gs_item_qc-dmbtr * -1.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ENDIF.
    ENDIF.

  ENDLOOP.
  SORT gt_qc BY pspid hkont.
  DATA:posid TYPE ps_posid,
       pspid TYPE ps_pspid.
  LOOP AT gt_item_qc_2 INTO gs_item_qc_2.
    CLEAR :gs_qc .
    AT NEW projk.
      CLEAR:pspid.
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_item_qc_2-projk.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr  = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid.

        ENDIF.
      ENDIF.
    ENDAT.
    gs_item_qc_2-pspid = pspid.
    MODIFY gt_item_qc_2 FROM gs_item_qc_2.
    gs_qc-pspid = pspid.
    gs_qc-hkont = gs_item_qc_2-hkont.
    IF gs_item_qc_2-shkzg EQ 'S' .
      IF gs_item_qc_2-xnegp EQ ''.
        gs_qc-jfje = gs_item_qc_2-dmbtr.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ELSE.
        gs_qc-dfje = gs_item_qc_2-dmbtr * -1.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ENDIF.
    ELSE.
      IF gs_item_qc-xnegp EQ ''.
        gs_qc-dfje = gs_item_qc_2-dmbtr.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ELSE.
        gs_qc-jfje = gs_item_qc_2-dmbtr * -1.
        COLLECT gs_qc INTO gt_qc.
        CONTINUE.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "先赋值项目订单号
  LOOP AT gt_item_2 INTO gs_item_2 .
    AT NEW projk.
      CLEAR:pspid.
      READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_item_2-projk.
      IF sy-subrc EQ 0 .
        READ TABLE gt_proj INTO gs_proj WITH KEY pspnr  = gs_prps-psphi .
        IF sy-subrc EQ 0 .
          pspid = gs_proj-pspid.

        ENDIF.
      ENDIF.
    ENDAT.
    gs_item_2-pspid = pspid.
    MODIFY gt_item_2 FROM gs_item_2.
  ENDLOOP.

  APPEND LINES OF gt_item_2 TO gt_item .

  "先按过账明细按项目号 、科目 排序
  SORT gt_item BY pspid hkont.
  DATA:xh TYPE i VALUE 0 .
  CLEAR:xh.
  DATA:jfxj TYPE dmbtr, "借方小计
       dfxj TYPE dmbtr. "贷方小计

  DATA:xmmc TYPE string.

  "过账明细处理：物料名称、借贷金额、供应商明细、客户明细 到 GT_DATA
  LOOP AT gt_item ASSIGNING <fs_item>.
    " xh = xh + 1.
    CLEAR:gs_data.
    MOVE <fs_item> TO gs_data.

    "物料名称
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr BINARY SEARCH .
    IF sy-subrc EQ 0 .

      gs_data-maktx = gs_makt-maktx.
    ENDIF.

    "客户名称
    READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = gs_data-kunnr BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-khms = gs_kna1-name1.
    ENDIF.


    "供应商名称
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data-lifnr BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-gysms = gs_lfa1-name1.

    ENDIF.

    "物料名称
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-maktx = gs_makt-maktx .
    ENDIF.

    "借贷方金额
    IF gs_data-shkzg EQ 'S'.
      IF gs_data-xnegp EQ ''.
        gs_data-jfje = gs_data-dmbtr .
        gs_data-fx = '借'.
*        jfxj = jfxj + gs_data-jfje ."借方小计
      ELSE.
        gs_data-dfje = gs_data-dmbtr  * -1 .
        gs_data-fx = '贷'.
*        dfxj = dfxj + gs_data-dfje . "贷方小计
      ENDIF.


    ELSE.

      IF gs_data-xnegp EQ ''.
        gs_data-dfje = gs_data-dmbtr .
        gs_data-fx = '贷'.
*        dfxj = dfxj + gs_data-dfje . "贷方小计
      ELSE.
        gs_data-jfje = gs_data-dmbtr  * -1 .
        gs_data-fx = '借'.
*        jfxj = jfxj + gs_data-jfje ."借方小计
      ENDIF.
    ENDIF.

    "余额
    gs_data-ye = gs_data-jfje - gs_data-dfje.
    gs_data-waers = g_waers.    "货币码
    APPEND gs_data TO gt_data.



  ENDLOOP.

  "再添加期初合计 到 gt_data.
  SORT gt_qc BY pspid ASCENDING hkont DESCENDING.
  DATA:qc_tabix TYPE i,
       l_tabix  TYPE i.

  LOOP AT gt_qc INTO gs_qc .
    qc_tabix = sy-tabix .
    CLEAR:gs_data.
    gs_data-bukrs = p_bukrs. "公司代码
    gs_data-pspid = gs_qc-pspid. "项目
    gs_data-hkont = gs_qc-hkont.  "科目
    gs_data-jfje = gs_qc-jfje.  "借方金额
    gs_data-dfje = gs_qc-dfje.  "贷方金额
    gs_data-ye =  gs_data-jfje - gs_data-dfje  .
    IF gs_data-ye > 0 .
      gs_data-fx =  '借'.

    ELSEIF  gs_data-ye < 0 .
      gs_data-fx = '贷'.
    ELSE.
      gs_data-fx = '平'.
    ENDIF.

    gs_data-waers = g_waers.    "货币码
    gs_data-bktxt = '期初余额'.     "摘要
    "汇总期初余额明细插入到GT_DATA 明细
    "1:先按项目、科目查找到GT_DATA明细就插入对应的前行
    "2:1不满足就按项目查找 就插入到对应的前行
    "3:1、2不满足就直接直接插入到GT_DATA_HZ
    READ TABLE gt_data INTO gs_data_1 WITH KEY pspid = gs_data-pspid hkont = gs_data-hkont .
    IF sy-subrc EQ 0 .
      l_tabix = sy-tabix.
      INSERT gs_data INTO gt_data INDEX l_tabix .
      DELETE gt_qc INDEX qc_tabix .
      CONTINUE.
    ELSE.
      READ TABLE gt_data  INTO gs_data_1 WITH KEY pspid = gs_data-pspid  .
      IF sy-subrc EQ 0 .
        l_tabix = sy-tabix.
        INSERT gs_data INTO gt_data INDEX l_tabix .
        DELETE gt_qc INDEX qc_tabix.
        CONTINUE.
      ELSE.
        APPEND gs_data TO gt_data_hz .  "先添加期初合计到 gt_data_hj
        CONTINUE.
      ENDIF.

    ENDIF.
  ENDLOOP.

  "追加GT_DATA行明细到 gt_data_hz.
  IF gt_data IS NOT INITIAL.
    APPEND LINES OF   gt_data TO gt_data_hz .
  ENDIF.


  "把期初、过账明细 按项目、科目维度汇总小计
  LOOP AT  gt_data_hz INTO gs_data_hz .
    CLEAR:gs_data_sum.
    MOVE-CORRESPONDING gs_data_hz TO gs_data_sum.
    AT NEW pspid.
      CLEAR:xmmc.
      READ TABLE gt_proj INTO gs_proj WITH KEY pspid = gs_data_hz-pspid .
      IF sy-subrc EQ 0 .
        xmmc = gs_proj-post1.
      ENDIF.
    ENDAT .
    "项目名称
    gs_data_sum-xmmc = xmmc .
    "科目名称
    READ TABLE gt_skat INTO gs_skat WITH KEY saknr = gs_data_sum-hkont BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data_sum-kmms = gs_skat-txt50 .
    ENDIF.

    APPEND  gs_data_sum TO gt_data_sum.

    dfxj = dfxj + gs_data_sum-dfje . "贷方小计
    jfxj = jfxj + gs_data_sum-jfje ."借方小计

    AT END OF  hkont .
      CLEAR:gs_data_sum .
      "新增一行按项目、科目汇总的小计一行
      gs_data_sum-bukrs = gs_data_hz-bukrs. "公司代码
      gs_data_sum-pspid = gs_data_hz-pspid. "项目号
      gs_data_sum-xmmc = xmmc.   "项目名称
      gs_data_sum-hkont = gs_data_hz-hkont.  "科目
      "科目名称
      READ TABLE gt_skat INTO gs_skat WITH KEY saknr = gs_data_sum-hkont BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data_sum-kmms = gs_skat-txt50 .
      ENDIF.
      gs_data_sum-bktxt = '小计'.          "摘要
      gs_data_sum-waers = g_waers. "货币码
      gs_data_sum-jfje = jfxj.  "借方合计
      gs_data_sum-dfje = dfxj.  "贷方合计
      gs_data_sum-ye =  gs_data_sum-jfje -  gs_data_sum-dfje .  "余额
      IF gs_data_sum-ye > 0 .
        gs_data_sum-fx = '借'.
      ELSEIF gs_data_sum-ye < 0 .
        gs_data_sum-fx = '贷'.
      ELSE.
        gs_data_sum-fx = '平'.
      ENDIF.
      APPEND gs_data_sum TO gt_data_sum.

      "清除借方合计、贷方合计
      CLEAR:jfxj,dfxj.
    ENDAT .



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
  gw_grid_settings-edt_cll_cb = 'X'.
  PERFORM frm_build_event.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            gt_data_sum
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
FORM init_variant .

ENDFORM.
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  init_fieldcat 'PSPID'          '项目定义'             '' '' '' '' '' '' ''.
  init_fieldcat 'XMMC'          '项目名称'             '' '' '' '' '' '' ''.
  init_fieldcat 'HKONT'          '科目'             '' '' '' '' '' '' ''.
  init_fieldcat 'KMMS'          '科目描述'             '' '' '' '' '' '' ''.
  init_fieldcat 'BELNR'          '会计凭证'             '' '' '' '' '' '' ''.
  init_fieldcat 'BUZEI'          '凭证行号'             '' '' '' '' '' '' ''.
  init_fieldcat 'GJAHR'          '会计年度'             '' '' '' '' '' '' ''.
  init_fieldcat 'BKTXT'          '摘要'             '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'          '货币码'             '' '' '' '' '' '' ''.
  init_fieldcat 'JFJE'          '借方金额'             '' '' '' '' '' '' ''.
  init_fieldcat 'DFJE'          '贷方金额'             '' '' '' '' '' '' ''.
  init_fieldcat 'FX'          '方向'             '' '' '' '' '' '' ''.
  init_fieldcat 'YE'          '余额'             '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'          '物料号'             '' '' '' '' '' '' ''.
  init_fieldcat 'MAKTX'          '物料描述'             '' '' '' '' '' '' ''.
  init_fieldcat 'XREF3'          '参考码3'             '' '' '' '' '' '' ''.
  init_fieldcat 'BLART'          '凭证类型'             '' '' '' '' '' '' ''.
  init_fieldcat 'BUDAT'          '过账日期'             '' '' '' '' '' '' ''.
  init_fieldcat 'KUNNR'          '客户'             '' '' '' '' '' '' ''.
  init_fieldcat 'KHMS'          '客户描述'             '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR'          '供应商'             '' '' '' '' '' '' ''.
  init_fieldcat 'GYSMS'          '供应商描述'             '' '' '' '' '' '' ''.
  init_fieldcat 'PROJK'          'WBS'             '' '' '' '' '' '' ''.
  init_fieldcat 'ZUONR'          '分配'             '' '' '' '' '' '' ''.


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
*      -->P_0468   text
*      -->P_0469   text
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
      t_outtab                 = gt_data_sum
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT

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

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.
