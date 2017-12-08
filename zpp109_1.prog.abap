REPORT zpp109_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170113
*& Request       :  ED1K905165
*& Descriptions  : 工艺路线报表导出
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
TABLES:mapl,plas,plko,plpo.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        matnr TYPE mapl-matnr,  "物料
        werks TYPE mapl-werks,  "工厂
        maktx TYPE makt-maktx,  "物料描述
        plnty TYPE mapl-plnty, "任务清单类型
        plnnr TYPE mapl-plnnr,  "组
        plnal TYPE mapl-plnal,  "组计数器
        plnkn TYPE plas-plnkn,  "任务清单节点
        andat TYPE mapl-andat,    "创建日期
        annam TYPE mapl-annam,    "创建人
        aedat TYPE mapl-aedat,   "更改日期
        aenam TYPE mapl-aenam,   "更改人
        verwe TYPE plko-verwe,  "用途
        statu TYPE plko-statu,  "状态
        vornr TYPE plpo-vornr,  "序号
        arbid TYPE plpo-arbid,   "对象标识
        ltxa1 TYPE plpo-ltxa1,  "工序短文本
        bmsch TYPE plpo-bmsch,  "基本数量
        meinh TYPE plpo-meinh,  "数量单位
        vgw01 TYPE plpo-vgw01,  "直接人工
        vgw02 TYPE plpo-vgw02,  "直接机器
        vgw03 TYPE plpo-vgw03, "能源—电
        vgw04 TYPE plpo-vgw04, "其他制费
        vge01 TYPE plpo-vge01, "作业单位
      END OF ty_data.

TYPES:BEGIN OF ty_matnr,
        matnr TYPE mapl-matnr, "物料号
      END OF ty_matnr.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

"内表序列号
DATA:g_tabix TYPE sy-tabix.

DATA:gt_plas TYPE TABLE OF plas,
     gs_plas TYPE plas.

DATA:gt_mapl TYPE TABLE OF mapl,
     gs_mapl TYPE mapl.


DATA:gt_plko TYPE TABLE OF plko,
     gs_plko TYPE plko.

DATA:gt_plpo TYPE TABLE OF plpo,
     gs_plpo TYPE plpo.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

"XML格式变量定义
DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

DATA:l_check TYPE i.

DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.


************************************************************************
* Global Variant
************************************************************************

DATA:g_plnty TYPE mapl-plnty,
     g_plnnr TYPE mapl-plnnr,
     g_plnal TYPE mapl-plnal,
     g_andat TYPE mapl-andat,
     g_annam TYPE mapl-annam,
     g_aedat TYPE mapl-aedat,
     g_aenam TYPE mapl-aenam,
     g_verwe TYPE plko-verwe,
     g_statu TYPE plko-statu,
     g_matnr TYPE mapl-matnr,
     g_werks TYPE mapl-werks,
     g_maktx TYPE makt-maktx.



************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS:
                s_werks FOR  mapl-werks ,     "工厂
                s_matnr FOR  mapl-matnr.      "物料

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  "*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_auth_check.
  CHECK l_check = 0.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_zh_xml.   "转换XML格式
  PERFORM frm_cc_sapfwq."生成SAP服务器文件

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  SELECT * INTO TABLE gt_t001w
    FROM t001w
    WHERE werks IN s_werks.
  SORT gt_t001w BY werks .
  IF gt_t001w IS NOT INITIAL.

  ELSE.

  ENDIF.
  LOOP AT gt_t001w INTO gs_t001w.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
       ID 'WERKS' FIELD  gs_t001w-werks.
    IF sy-subrc <> 0.
      l_check  = 1.
      MESSAGE e603(fco) WITH gs_t001w-werks.
    ELSE.
      l_check = 0.
    ENDIF.

  ENDLOOP.

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
  SELECT * INTO TABLE gt_mapl
    FROM mapl
    WHERE werks IN s_werks
    AND   matnr IN s_matnr
    AND   loekz ne 'X'.

  SORT gt_mapl BY plnty plnnr plnal .

  CHECK gt_mapl IS NOT INITIAL.

  MOVE-CORRESPONDING gt_mapl TO gt_matnr.
  SORT gt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

  SELECT * INTO TABLE gt_makt
    FROM makt
    FOR ALL ENTRIES IN gt_matnr
    WHERE matnr = gt_matnr-matnr
    AND spras = '1'.
  SORT gt_makt BY matnr .

  SELECT * INTO TABLE gt_plas
    FROM plas
    FOR ALL ENTRIES IN  gt_mapl
    WHERE plnty EQ gt_mapl-plnty
    AND   plnnr EQ gt_mapl-plnnr
    AND   plnal EQ gt_mapl-plnal
    AND   loekz NE 'X'.
  SORT gt_plas BY plnty plnnr plnal plnkn .


  SELECT * INTO TABLE gt_plko
    FROM plko
    FOR ALL ENTRIES IN gt_mapl
    WHERE plnty = gt_mapl-plnty
    AND   plnnr = gt_mapl-plnnr
    AND   plnal = gt_mapl-plnal.

  SORT gt_plko BY plnty plnnr plnal .

  IF gt_plas IS NOT INITIAL.
    SELECT * INTO TABLE gt_plpo
      FROM plpo
      FOR ALL ENTRIES IN gt_plas
      WHERE plnty = gt_plas-plnty
      AND   plnnr = gt_plas-plnnr
      AND   plnkn = gt_plas-plnkn .
    SORT gt_plpo BY plnty plnnr plnkn.
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




  LOOP AT gt_plas INTO gs_plas.
    g_tabix = sy-tabix.
    AT NEW plnal.
      CLEAR:g_plnty,g_plnnr,g_plnal,
            g_andat,g_annam,g_aedat,
            g_aenam,g_verwe,g_statu.
      READ TABLE gt_plas INTO gs_plas INDEX g_tabix.
      IF sy-subrc EQ 0 ..
        g_plnty = gs_plas-plnty.
        g_plnnr = gs_plas-plnnr.
        g_plnal = gs_plas-plnal.
        READ TABLE gt_mapl INTO gs_mapl WITH KEY plnty = g_plnty
                                                 plnnr = g_plnnr
                                                 plnal = g_plnal
                                                 .
        IF sy-subrc EQ 0 .

          g_matnr = gs_mapl-matnr.
          READ TABLE gt_makt INTO gs_makt WITH KEY matnr = g_matnr
                                          BINARY SEARCH.
          IF sy-subrc EQ 0.
            g_maktx = gs_makt-maktx.
          ENDIF.

          g_werks = gs_mapl-werks .
          g_andat = gs_mapl-andat.
          g_annam = gs_mapl-annam.
          g_aedat = gs_mapl-aedat.
          g_aenam = gs_mapl-aenam.

        ENDIF.
        READ TABLE gt_plko INTO gs_plko WITH KEY plnty = g_plnty
                                                plnnr = g_plnnr
                                                plnal = g_plnal
                                                BINARY SEARCH.
        IF sy-subrc EQ 0.
          g_verwe = gs_plko-verwe.   "用途
          g_statu = gs_plko-statu .  "状态
        ENDIF.
      ENDIF.

    ENDAT .
    CLEAR:gs_data.
    gs_data-matnr = g_matnr. "物料
    gs_data-maktx = g_maktx ."物料描述
    gs_data-werks = g_werks. "工厂
    gs_data-plnty = g_plnty.   "任务清单类型
    gs_data-plnnr = g_plnnr.   "组
    gs_data-plnal = g_plnal.    "组计数器
    gs_data-plnkn = gs_plas-plnkn . "任务清单节点
    gs_data-andat = g_andat . "创建日期
    gs_data-annam = g_annam . "创建人
    gs_data-aedat = g_aedat ."更改日期
    gs_data-aenam = g_aenam .  "更改人》
    gs_data-verwe = g_verwe . "用途
    gs_data-statu = g_statu . "状态
    READ TABLE gt_plpo INTO gs_plpo WITH KEY plnty = g_plnty
                                             plnnr = g_plnnr
                                             plnkn = gs_plas-plnkn
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_data-vornr = gs_plpo-vornr. "序号
      gs_data-arbid = gs_plpo-arbid. "对象标识
      gs_data-ltxa1 = gs_plpo-ltxa1. "工序短文本
      gs_data-bmsch = gs_plpo-bmsch. "基本数量
      gs_data-meinh = gs_plpo-meinh ."数量单位
      gs_data-vgw01 = gs_plpo-vgw01. "直接人工
      gs_data-vgw02 = gs_plpo-vgw02. "直接机器
      gs_data-vgw03 = gs_plpo-vgw03.  "能源-电
      gs_data-vgw04 = gs_plpo-vgw04. "其他制费
      gs_data-vge01 = gs_plpo-vge01. "作业单位

    ENDIF.

    APPEND gs_data TO gt_data .

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ZH_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_zh_xml .

    CALL TRANSFORMATION zpp109_1_zh
  SOURCE table = gt_data
  RESULT XML xmlstr.
    REPLACE FIRST OCCURRENCE OF 'encoding="utf-16"' IN xmlstr WITH 'encoding="gbk"'.
    APPEND xmlstr TO xml_table.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CC_SAPFWQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_cc_sapfwq .

    dname = 'ZPP109工艺路线报表导出.XLS'.
    CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
    DELETE DATASET dname .
    OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
    IF sy-subrc NE 0. EXIT. ENDIF.
    LOOP AT xml_table INTO wa_xml.
      TRANSFER wa_xml TO dname.
    ENDLOOP.
    CLOSE DATASET dname.

ENDFORM.
