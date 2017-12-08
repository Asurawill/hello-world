REPORT zco011.


*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160726
*& Request       :
*& Descriptions  : 产品实际成本构成
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
************************************************************************
* TABLES
************************************************************************

TABLES:ckmlhd,ckmlprkeph,ckmlcr.

TYPES:BEGIN OF ty_data,
        bwkey      TYPE bwkey,                        "公司代码
        bdatj      TYPE bdatj,                        "会计年度
        poper      TYPE poper,                        "会计期间
        kalnr      TYPE ck_kalnr,                     "成本估算号
        matnr      TYPE matnr,                        "物料号
        maktx      TYPE maktx,                        "物料描述
        mtart      TYPE mtart,                        "物料类型
        vbeln      TYPE vbeln,                        "销售订单
        posnr      TYPE posnr,                        "销售订单行号
        xmmc       TYPE string,                       "项目名称
        waers      TYPE waers,                        "本币码
        pvprs      TYPE ck_pvprs_1,                   "实际价
        peinh      TYPE peinh,                        "价格单位
        kst001_v   TYPE mlccs_d_kstel,                "直接材料-V
        kst003_v   TYPE mlccs_d_kstel,                "直接人工-V
        kst005_v   TYPE mlccs_d_kstel,                "直接机器-V
        kst007_v   TYPE mlccs_d_kstel,                "能源-电-V
        kst009_v   TYPE mlccs_d_kstel,                "其他制费-V
        kst011_v   TYPE mlccs_d_kstel,                "外协加工-V
        kst001_v_% TYPE mlccs_d_kstel,                "直接材料%-V
        kst003_v_% TYPE mlccs_d_kstel,                "直接人工%-V
        kst005_v_% TYPE mlccs_d_kstel,                "直接机器%-V
        kst007_v_% TYPE mlccs_d_kstel,                "能源-电%-V
        kst009_v_% TYPE mlccs_d_kstel,                "其他制费%-V
        kst011_v_% TYPE mlccs_d_kstel,                "外协加工%-V
        stprs      TYPE stprs,                        "标准价
        kst001_s   TYPE mlccs_d_kstel,                "直接材料-S
        kst003_s   TYPE mlccs_d_kstel,                "直接人工-S
        kst005_s   TYPE mlccs_d_kstel,                "直接机器-S
        kst007_s   TYPE mlccs_d_kstel,                "能源-电-S
        kst009_s   TYPE mlccs_d_kstel,               "其他制费-S
        kst011_s   TYPE mlccs_d_kstel,               "外协加工-S
        kst001_s_% TYPE mlccs_d_kstel,               "直接材料%-S
        kst003_s_% TYPE mlccs_d_kstel,               "直接人工%-S
        kst005_s_% TYPE mlccs_d_kstel,               "直接机器%-S
        kst007_s_% TYPE mlccs_d_kstel,               "能源-电%-S
        kst009_s_% TYPE mlccs_d_kstel,               "其他制费%-S
        kst011_s_% TYPE mlccs_d_kstel,               "外协加工%-S
        sel,
      END OF ty_data.

TYPES:BEGIN OF ty_ckmlhd,
        kalnr TYPE ck_kalnr,
        bwkey TYPE bwkey,
        matnr TYPE matnr,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        xmmc  TYPE string,
      END OF ty_ckmlhd.

TYPES:BEGIN OF ty_vbeln,
        vbeln TYPE vbeln,
        xmmc  TYPE string,

      END OF ty_vbeln .


TYPES:BEGIN OF ty_ckmlprkeph_v,
        kalnr    TYPE ck_kalnr,
        bdatj    TYPE bdatj,
        poper    TYPE poper,
        kst001_v TYPE mlccs_d_kstel,  "直接材料-V
        kst003_v TYPE mlccs_d_kstel,   "直接人工-V
        kst005_v TYPE mlccs_d_kstel,   "直接机器-V
        kst007_v TYPE mlccs_d_kstel,   "能源-电-V
        kst009_v TYPE mlccs_d_kstel,   "其他制费-V
        kst011_v TYPE mlccs_d_kstel,   "外协加工-V
        waers    TYPE waers,           "货币码
      END OF ty_ckmlprkeph_v.

TYPES:BEGIN OF ty_ckmlprkeph_s,
        kalnr    TYPE ck_kalnr,
        bdatj    TYPE bdatj,
        poper    TYPE poper,
        kst001_s TYPE mlccs_d_kstel,  "直接材料-V
        kst003_s TYPE mlccs_d_kstel,   "直接人工-V
        kst005_s TYPE mlccs_d_kstel,   "直接机器-V
        kst007_s TYPE mlccs_d_kstel,   "能源-电-V
        kst009_s TYPE mlccs_d_kstel,   "其他制费-V
        kst011_s TYPE mlccs_d_kstel,   "外协加工-V
        waers    TYPE waers,           "货币码
      END OF ty_ckmlprkeph_s.

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,
        maktx TYPE maktx,
        mtart TYPE mtart,
      END OF ty_matnr.

TYPES:BEGIN OF ty_ckmlcr,
        kalnr TYPE ck_kalnr,
        bdatj TYPE bdatj,
        poper TYPE poper,
        pvprs TYPE ck_pvprs_1,
        peinh TYPE ck_peinh_1,
        stprs TYPE ck_stprs_1,
      END OF ty_ckmlcr .

DATA:gs_data TYPE ty_data,
     gt_data TYPE TABLE OF ty_data.

DATA:gt_ckmlhd TYPE TABLE OF ty_ckmlhd,
     gs_ckmlhd TYPE ty_ckmlhd.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_ckmlprkeph_v TYPE TABLE OF ty_ckmlprkeph_v,
     gs_ckmlprkeph_v TYPE ty_ckmlprkeph_v.

DATA:gt_ckmlprkeph_s TYPE TABLE OF ty_ckmlprkeph_s,
     gs_ckmlprkeph_s TYPE ty_ckmlprkeph_s.

DATA:gt_ckmlcr TYPE TABLE OF ty_ckmlcr,
     gs_ckmlcr TYPE ty_ckmlcr.

DATA:gt_t001 LIKE TABLE OF t001,
     gs_t001 LIKE t001.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.


DATA:gt_vbeln TYPE TABLE OF ty_vbeln,
     gs_vbeln TYPE ty_vbeln.

DATA: g_objname TYPE thead-tdname.

DATA:g_tabix TYPE sy-tabix .

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
*   IF &1 = 'MENGE' .
*   GW_LVC-QFIELDNAME = 'MEINS'.
*  ENDIF.

  "GW_LVC-CHECKBOX = &5.
  GW_LVC-CFIELDNAME = &5.
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

SELECT-OPTIONS: s_bwkey  FOR ckmlhd-bwkey OBLIGATORY.   "公司代码
PARAMETERS:     p_bdatj  LIKE ckmlprkeph-bdatj.   "会计年度
SELECT-OPTIONS: s_poper  FOR ckmlprkeph-poper,   "会计期间
                s_matnr  FOR ckmlhd-matnr,       "物料号
                s_vbeln  FOR ckmlhd-vbeln.       "销售订单
SELECTION-SCREEN END OF BLOCK blk1.


*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
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
  PERFORM frm_auth_check .
*  IF sy-subrc NE 0.
*    MESSAGE i011(zfico01) WITH p_bukrs DISPLAY LIKE 'E'.
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
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0419   text
*----------------------------------------------------------------------*
*FORM frm_auth_check USING VALUE(p_actvt).
*  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD p_actvt
*                                      ID 'BUKRS' FIELD p_bukrs.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  "取物料账的成本估算号
  SELECT kalnr  bwkey matnr vbeln posnr
    INTO CORRESPONDING FIELDS OF TABLE gt_ckmlhd
    FROM ckmlhd
    WHERE bwkey IN s_bwkey
    AND   matnr IN s_matnr
    AND   vbeln IN s_vbeln.

  SORT gt_ckmlhd BY kalnr .

  "取估算号对应的V价
  SELECT bdatj poper kalnr  waers kst001 AS kst001_v kst003 AS kst003_v
     kst005 AS kst005_v kst007 AS kst007_v kst009  AS kst009_v kst011 AS kst011_v
    INTO CORRESPONDING FIELDS OF TABLE gt_ckmlprkeph_v
    FROM ckmlprkeph
    FOR ALL ENTRIES IN gt_ckmlhd
    WHERE bdatj EQ p_bdatj
    AND   poper IN s_poper
    AND   kalnr = gt_ckmlhd-kalnr
    AND   kkzst EQ ''
    AND   curtp EQ '10'
    AND   prtyp EQ 'V'

  "  AND  bdatj ! poper >= '20160303'
    .
  SORT gt_ckmlprkeph_v BY kalnr bdatj poper  .

  "取估算号对应的S价
  SELECT bdatj poper kalnr waers  kst001 AS kst001_s kst003 AS kst003_s
     kst005 AS kst005_s kst007 AS kst007_s kst009  AS kst009_s kst011 AS kst011_s
    INTO CORRESPONDING FIELDS OF TABLE gt_ckmlprkeph_s
    FROM ckmlprkeph
    FOR ALL ENTRIES IN gt_ckmlhd
    WHERE bdatj EQ p_bdatj
    AND   poper IN s_poper
    AND   kalnr = gt_ckmlhd-kalnr
    AND   kkzst EQ ''
    AND   curtp EQ '10'
    AND   prtyp EQ 'S'.
  SORT gt_ckmlprkeph_s BY kalnr bdatj poper  .

  "取估算号对应的标准价和实际价
  SELECT kalnr bdatj poper  pvprs peinh stprs
    INTO CORRESPONDING FIELDS OF TABLE gt_ckmlcr
    FROM ckmlcr
     FOR ALL ENTRIES IN gt_ckmlhd
    WHERE bdatj EQ p_bdatj
    AND   poper IN s_poper
    AND   kalnr = gt_ckmlhd-kalnr
    AND   curtp EQ '10'
    AND   vprsv EQ 'V'.


  SORT gt_ckmlcr BY kalnr bdatj poper  . .


  "读取物料描述

  SELECT a~matnr b~maktx c~mtart
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
    FROM marc  AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    INNER JOIN mara AS c
    ON a~matnr = c~matnr
    WHERE a~werks IN s_bwkey
    AND a~matnr IN s_matnr
    AND b~spras = '1'.

  SORT gt_matnr BY matnr .



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


  "读取项目名称

  MOVE-CORRESPONDING   gt_ckmlhd TO gt_vbeln .
  DELETE gt_vbeln  WHERE vbeln IS INITIAL .

  SORT gt_vbeln BY vbeln .

  DELETE ADJACENT DUPLICATES FROM gt_vbeln COMPARING vbeln .

  "先读取项目名称
  LOOP AT gt_vbeln INTO gs_vbeln .

    PERFORM selxmmc USING gs_vbeln-vbeln CHANGING gs_vbeln-xmmc .
    MODIFY gt_vbeln FROM gs_vbeln .

  ENDLOOP.

  "读取物料描述
  LOOP AT gt_ckmlprkeph_v INTO gs_ckmlprkeph_v .
    g_tabix = sy-tabix .
    "读取标准价和实际价
    READ TABLE gt_ckmlcr INTO gs_ckmlcr WITH KEY kalnr = gs_ckmlprkeph_v-kalnr
                                                 bdatj = gs_ckmlprkeph_v-bdatj
                                                 poper = gs_ckmlprkeph_v-poper
                                        BINARY SEARCH  .
    IF sy-subrc EQ 0 .
      CLEAR:gs_data.
      "先读取V价的明细
      MOVE-CORRESPONDING gs_ckmlprkeph_v TO gs_data.
      gs_data-pvprs = gs_ckmlcr-pvprs.
      gs_data-peinh = gs_ckmlcr-peinh.
      gs_data-stprs = gs_ckmlcr-stprs.
    ELSE.
      DELETE gt_ckmlprkeph_v  INDEX g_tabix .
      CONTINUE.
    ENDIF.


    "读取S价的明细
    READ TABLE gt_ckmlprkeph_s INTO gs_ckmlprkeph_s WITH KEY kalnr = gs_ckmlprkeph_v-kalnr
                                                             bdatj = gs_ckmlprkeph_v-bdatj
                                                            poper = gs_ckmlprkeph_v-poper
                                                    BINARY SEARCH  .
    IF sy-subrc EQ 0 .

      MOVE-CORRESPONDING gs_ckmlprkeph_s TO gs_data.
    ENDIF.


    "读取标准价和实际价
    READ TABLE gt_ckmlcr INTO gs_ckmlcr WITH KEY kalnr = gs_ckmlprkeph_v-kalnr
                                                 bdatj = gs_ckmlprkeph_v-bdatj
                                                 poper = gs_ckmlprkeph_v-poper
                                        BINARY SEARCH  .
    IF sy-subrc EQ 0 .
      gs_data-pvprs = gs_ckmlcr-pvprs.
      gs_data-peinh = gs_ckmlcr-peinh.
      gs_data-stprs = gs_ckmlcr-stprs.

    ENDIF.

    IF  gs_data-pvprs NE 0 .
      gs_data-kst001_v_%  = gs_data-kst001_v / gs_data-pvprs * 100 .
      gs_data-kst003_v_%  = gs_data-kst003_v / gs_data-pvprs * 100 .
      gs_data-kst005_v_%  = gs_data-kst005_v / gs_data-pvprs * 100 .
      gs_data-kst007_v_%  = gs_data-kst007_v / gs_data-pvprs * 100 .
      gs_data-kst009_v_%  = gs_data-kst009_v / gs_data-pvprs * 100 .
      gs_data-kst011_v_%  = gs_data-kst011_v / gs_data-pvprs * 100 .

    ENDIF.

    IF gs_data-stprs  NE 0 .
      gs_data-kst001_s_%  = gs_data-kst001_s / gs_data-stprs * 100 .
      gs_data-kst003_s_%  = gs_data-kst003_s / gs_data-stprs  * 100 .
      gs_data-kst005_s_%  = gs_data-kst005_s / gs_data-stprs  * 100 .
      gs_data-kst007_s_%  = gs_data-kst007_s / gs_data-stprs  * 100 .
      gs_data-kst009_s_%  = gs_data-kst009_s / gs_data-stprs  * 100 .
      gs_data-kst011_s_%  = gs_data-kst011_s / gs_data-stprs  * 100 .

    ENDIF.

    "读取公司代码 、会计年度、会计期间、物料号 、物料描述、 物料类型 、项目名称
    READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY kalnr = gs_data-kalnr
                                        BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-bwkey = gs_ckmlhd-bwkey .
      gs_data-matnr = gs_ckmlhd-matnr.
      READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_data-matnr
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-maktx = gs_matnr-maktx.
        gs_data-mtart = gs_matnr-mtart.
      ENDIF.
      IF gs_ckmlhd-vbeln IS NOT INITIAL.
        gs_data-vbeln = gs_ckmlhd-vbeln .
        gs_data-posnr = gs_ckmlhd-posnr .
        READ TABLE gt_vbeln INTO gs_vbeln WITH KEY vbeln = gs_data-vbeln
                                BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_data-xmmc = gs_vbeln-xmmc.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND gs_data TO gt_data .




  ENDLOOP.
  SORT gt_data BY matnr kalnr bdatj poper bwkey.
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
  init_fieldcat 'BWKEY'          '公司代码'          '' '' '' '' '' 'CKMLHD' 'BWKEY'.
  init_fieldcat 'BDATJ'          '会计年度'          '' '' '' '' '' 'CKMLPRKEPH' 'BDATJ'.
  init_fieldcat 'POPER'          '会计期间'          '' '' '' '' '' 'CKMLPRKEPH' 'POPER'.
  init_fieldcat 'KALNR'          '成本估算号'        '' '' '' '' '' 'CKMLPRKEPH' 'KALNR'.
  init_fieldcat 'MATNR'          '物料号'            '' '' '' '' '' 'CKMLHD' 'MATNR'.
  init_fieldcat 'MAKTX'          '物料描述'          '' '' '' '' '' 'MAKT' 'MAKTX'.
  init_fieldcat 'MTART'          '物料类型'          '' '' '' '' '' 'MARA' 'MTART'.
  init_fieldcat 'VBELN'          '销售订单'          '' '' '' '' '' 'VBAK' 'VBELN'.
  init_fieldcat 'POSNR'          '销售订单行号'      '' '' '' '' '' 'VBAK' 'POSNR'.
  init_fieldcat 'XMMC'           '项目名称'          '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'          '本币码'            '' '' '' '' '' 'T001' 'WAERS'.
  init_fieldcat 'PVPRS'          '实际价'            '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'PEINH'          '价格单位'          '' '' '' '' '' '' ''.
  init_fieldcat 'KST001_V'       '直接材料-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST003_V '      '直接人工-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST005_V'       '直接机器-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST007_V'       '能源-电-V'         '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST009_V'       '其他制费-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST011_V'       '外协加工-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST001_V_%'     '直接材料%-V'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST003_V_%'     '直接人工%-V'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST005_V_%'     '直接机器%-V'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST007_V_%'     '能源-电%-V'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST009_V_%'     '其他制费%-V'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST011_V_%'     '外协加工%-V'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'STPRS'          '标准价'            '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST001_S'       '直接材料-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST003_S'       '直接人工-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST005_S'       '直接机器-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST007_S'       '能源-电-S'         '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST009_S'       '其他制费-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST011_S'       '外协加工-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST001_S_%'     '直接材料%-S'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST003_S_%'     '直接人工%-S'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST005_S_%'     '直接机器%-S'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST007_S_%'     '能源-电%-S'        '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST009_S_%'     '其他制费%-S'       '' '' 'WAERS' '' '' '' ''.
  init_fieldcat 'KST011_S_%'     '外协加工%-S'       '' '' 'WAERS' '' '' '' ''.

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
*      -->P_0486   text
*      -->P_0487   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM frm_output  TABLES  pt_lvc TYPE lvc_t_fcat
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
      t_outtab                 = pt_data
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

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND\

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CKMLHD_VBELN  text
*      <--P_G_XMMC  text
*----------------------------------------------------------------------*
FORM selxmmc  USING    p_vbeln TYPE vbeln
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = '1'
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
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0650   text
*----------------------------------------------------------------------*
FORM frm_auth_check  .
  SELECT * INTO TABLE  gt_t001
     FROM t001
    WHERE bukrs IN s_bwkey .

  SORT gt_t001 BY bukrs .

  LOOP AT gt_t001 INTO  gs_t001.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD '03'
                                   ID 'BUKRS' FIELD gs_t001-bukrs.
    IF sy-subrc NE 0 .
      MESSAGE i011(zfico01) WITH  gs_t001-bukrs DISPLAY LIKE 'E'.
      STOP.
      EXIT .
    ENDIF.

  ENDLOOP.


ENDFORM.
