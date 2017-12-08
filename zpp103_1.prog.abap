REPORT zpp103_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170113
*& Request       : ED1K905163
*& Descriptions  : BOM单层查看
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
TABLES:mast,stpo,stko,stzu,stas.
************************************************************************
* Type Declaration
************************************************************************
"导出数据类型定义
TYPES:BEGIN OF ty_data,
        stlnr TYPE mast-stlnr,   "物料单
        werks TYPE mast-werks,   "工厂
        matnr TYPE mast-matnr,   "母件
        maktx TYPE makt-maktx,   "母件描述
        stlan TYPE mast-stlan,   "BOM用途
        stlal TYPE mast-stlal,   "可选BOM
        andat TYPE mast-andat,   "创建日期
        annam TYPE mast-annam,   "创建人
        stlkn TYPE stpo-stlkn,    "BOM项目节点
        idnrk TYPE stpo-idnrk,     "BOM组件
        zjms  TYPE makt-maktx,    "组件描述
        posnr TYPE stpo-posnr,    "BOM项目号
        menge TYPE stpo-menge,    "组件数量
        meins TYPE stpo-meins,    "计量单位
        potx1 TYPE stpo-potx1,    "BOM项目文本(行1)
        potx2 TYPE stpo-potx2,     "BOM项目文本(行2)
        stlst TYPE stko-stlst,     "BOM状态
        sttxt TYPE t415t-sttxt,    "BOM状态描述
        bmeng TYPE stko-bmeng,    "基本数量
        bmein TYPE stko-bmein,     "BOM基本单位
        ztext TYPE stzu-ztext,    "抬头文本
      END OF ty_data.

"母件物料号
TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,  "物料号
      END OF ty_matnr.

"组件号
TYPES:BEGIN OF ty_idnrk,
        idnrk TYPE idnrk,  "BOM组件
      END OF ty_idnrk .

"组件号定义
DATA:gt_idnrk TYPE TABLE OF ty_idnrk,
     gs_idnrk TYPE ty_idnrk.

"母件号定义
DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

"母件描述定义
DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

"组件描述定义
DATA:gt_zj TYPE TABLE OF makt,
     gs_zj TYPE makt.

"导出数据定义
DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

"BOM链接物料号定义
DATA:gt_mast TYPE TABLE OF mast,
     gs_mast TYPE mast.

"BOM表头定义
DATA:gt_stko TYPE TABLE OF stko,
     gs_stko TYPE stko.

"BOM项目定义
DATA:gt_stpo LIKE TABLE OF stpo,
     gs_stpo LIKE stpo.

"永久BOM数据定义
DATA:gt_stzu TYPE TABLE OF stzu,
     gs_stzu TYPE stzu.

"定义工厂数据
DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

"内表序列号
DATA:g_tabix TYPE sy-tabix.

"BOMs-项选择定义
DATA:gt_stas TYPE TABLE OF stas,
     gs_stas TYPE stas.

"BOM状态描述
DATA:gt_t415t TYPE TABLE OF t415t,
     gs_t415t TYPE t415t.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

"XML格式变量定义
DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

************************************************************************
* Global Variant
************************************************************************
DATA:l_check TYPE i.



************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS:
                s_werks FOR  mast-werks  OBLIGATORY,     "工厂
                s_matnr FOR  mast-matnr,       "物料
                s_andat FOR  mast-andat,     "创建日期
                s_stlst FOR  stko-stlst.    "BOM状态

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
  "BOM链接物料查询
  SELECT * INTO TABLE gt_mast
    FROM mast
    WHERE werks IN s_werks
    AND   matnr IN s_matnr
    AND   andat IN s_andat.
  SORT gt_mast BY matnr werks stlnr.
  CHECK gt_mast IS NOT INITIAL.
  "母件物料号分类
  MOVE-CORRESPONDING gt_mast TO gt_matnr.
  DELETE gt_matnr WHERE matnr IS INITIAL.
  SORT gt_matnr BY matnr .
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .
  IF gt_matnr IS NOT INITIAL.
    "母件物料描述
    SELECT * INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN gt_matnr
      WHERE matnr = gt_matnr-matnr.
    SORT gt_makt BY matnr .
  ENDIF.

  "BOM表头查询
  SELECT * INTO TABLE gt_stko
    FROM stko
    FOR ALL ENTRIES IN gt_mast
    WHERE stlnr = gt_mast-stlnr
    AND   stlst IN s_stlst .

  SORT gt_stko BY stlnr .
  LOOP AT gt_mast INTO gs_mast.
    g_tabix = sy-tabix .
    "判断是否在查询屏幕BOM状态
    READ TABLE gt_stko INTO gs_stko WITH KEY stlnr = gs_mast-stlnr
                                     BINARY SEARCH.
    IF sy-subrc NE 0 .
      DELETE gt_mast INDEX g_tabix.
    ENDIF.
  ENDLOOP.

  "查询组件明细
  SELECT * INTO TABLE gt_stpo
    FROM stpo
    FOR ALL ENTRIES IN gt_mast
    WHERE stlnr EQ gt_mast-stlnr .

  SORT gt_stpo BY stlty stlnr stlkn.

  "组件号分类
  MOVE-CORRESPONDING gt_stpo TO gt_idnrk.
  DELETE gt_idnrk WHERE idnrk IS INITIAL.
  SORT gt_idnrk BY idnrk .
  DELETE ADJACENT DUPLICATES FROM gt_idnrk COMPARING idnrk .

  "组件物料描述
  IF gt_idnrk IS NOT INITIAL.
    SELECT * INTO TABLE gt_zj
      FROM makt
      FOR ALL ENTRIES IN gt_idnrk
      WHERE matnr = gt_idnrk-idnrk.
    SORT gt_zj BY matnr .
  ENDIF.

  "永久BOM数据查询
  IF gt_stko IS NOT INITIAL.
    SELECT * INTO TABLE gt_stzu
      FROM stzu
      FOR ALL ENTRIES IN gt_stko
      WHERE stlnr = gt_stko-stlnr.

    SORT gt_stzu BY stlnr .
  ENDIF.

  "BOMs-项选择 查询
  SELECT * INTO TABLE gt_stas
    FROM stas
    FOR ALL ENTRIES IN gt_stpo
    WHERE stlnr = gt_stpo-stlnr
    AND   stlkn = gt_stpo-stlkn
    AND   lkenz = 'X'.

  SORT gt_stas BY stlnr stlkn .


  "取BOM状态描述
  SELECT * INTO TABLE gt_t415t
    FROM t415t
    WHERE spras = '1'.

  SORT gt_t415t BY stlst .

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


  LOOP AT gt_stpo INTO gs_stpo.
    g_tabix = sy-tabix .
    READ TABLE gt_stas INTO gs_stas WITH KEY stlnr = gs_stpo-stlnr
                                             stlkn = gs_stpo-stlkn
                                             BINARY SEARCH.
    IF sy-subrc EQ  0.
      DELETE gt_stpo INDEX g_tabix .
      CONTINUE .
    ENDIF.

  ENDLOOP.

  LOOP AT gt_mast INTO gs_mast .
    CLEAR:gs_data.
    "工厂
    gs_data-werks = gs_mast-werks.
    "物料单
    gs_data-stlnr = gs_mast-stlnr.
    "母件

    gs_data-matnr = gs_mast-matnr.

    "母件描述
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-maktx = gs_makt-maktx .
    ENDIF.

    "抬头文本
    READ TABLE gt_stzu INTO gs_stzu WITH KEY stlnr = gs_data-stlnr
                                    BINARY SEARCH .
    IF sy-subrc EQ  0.
      gs_data-ztext = gs_stzu-ztext.
    ENDIF.



    "BOM用途
    gs_data-stlan = gs_mast-stlan.
    "可选BOM
    gs_data-stlal = gs_mast-stlal.
    "创建日期
    gs_data-andat = gs_mast-andat.
    "创建人
    gs_data-annam = gs_mast-annam .

    READ TABLE gt_stko INTO gs_stko WITH KEY stlnr = gs_data-stlnr
                                             BINARY SEARCH .
    IF sy-subrc EQ 0  .
      "BOM状态
      gs_data-stlst = gs_stko-stlst.
      "BOM状态描述
      READ TABLE gt_t415t INTO gs_t415t WITH KEY stlst = gs_data-stlst
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-sttxt = gs_t415t-sttxt.
      ENDIF.
      "基本数量
      gs_data-bmeng = gs_stko-bmeng.
      "BOM基本单位
      gs_data-bmein = gs_stko-bmein.
    ENDIF.


    "BOM明细
    LOOP AT gt_stpo INTO gs_stpo WHERE stlnr = gs_data-stlnr .
      CLEAR:gs_data-stlkn ,gs_data-idnrk,gs_data-zjms,gs_data-posnr ,gs_data-menge,gs_data-meins,
            gs_data-potx1, gs_data-potx2.
      "BOM项目节点
      gs_data-stlkn = gs_stpo-stlkn.
      "BOM组件
      gs_data-idnrk = gs_stpo-idnrk .
      READ TABLE gt_zj INTO gs_zj WITH KEY matnr =  gs_data-idnrk
                                   BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-zjms = gs_zj-maktx.
      ENDIF.
      "BOM项目号
      gs_data-posnr = gs_stpo-posnr.
      "组件数量
      gs_data-menge = gs_stpo-menge.
      "计量单位
      gs_data-meins = gs_stpo-meins.
      "BOM项目文本（行1）
      gs_data-potx1 = gs_stpo-potx1.
      "BOM项目文本（行2）
      gs_data-potx2 = gs_stpo-potx2.
      APPEND gs_data TO gt_data.
    ENDLOOP.
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

  CALL TRANSFORMATION zpp103_1_zh
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

  dname = 'ZPP103BOM单层导出列表.XLS'.
  CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
  DELETE DATASET dname .
  OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0. EXIT. ENDIF.
  LOOP AT xml_table INTO wa_xml.
    TRANSFER wa_xml TO dname.
  ENDLOOP.
  CLOSE DATASET dname.

ENDFORM.
