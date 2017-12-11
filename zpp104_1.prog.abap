REPORT zpp104_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170110
*& Request       : ED1K905179
*& Descriptions  : 报工日报表导出
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
TABLES:afru,afvc,afvv,afpo.
************************************************************************
* Type Declaration
************************************************************************
"导出数据类型定义
TYPES:BEGIN OF ty_data,
        rueck  TYPE afru-rueck,           "确认
        rmzhl  TYPE afru-rmzhl,           "确认计数器
        aufpl  TYPE afru-aufpl,           "订单工艺路线
        aplzl  TYPE afru-aplzl,           "工艺路线计数器
        ersda  TYPE afru-ersda,           "录入日期
        ernam  TYPE afru-ernam,           "录入人
        budat  TYPE afru-budat,           "过账日期
        werks  TYPE afru-werks,           "工厂
        ltxa1  TYPE afru-ltxa1,           "备注
        ism01  TYPE afru-ism01,           "人工工时
        ism02  TYPE afru-ism02,           "机器工时
        ism03  TYPE afru-ism03,           "电工时
        ism04  TYPE afru-ism04,           "制造工时
        ism05  TYPE afru-ism05,           "人数
        ism06  TYPE afru-ism06,           "加班工时
        ile01  TYPE afru-ile01,           "作业单位
        lmnga  TYPE afru-lmnga,           "产量
        gmein  TYPE afru-gmein,           "物料单位
        aufnr  TYPE afru-aufnr,           "生产订单
        stzhl  TYPE afru-stzhl,           "冲销标识
        kdauf  TYPE afpo-kdauf,           "项目号
        xmms   TYPE string,               "项目描述
        psmng  TYPE afpo-psmng,           "订单数量
        wemng  TYPE afpo-wemng,           "收货数量
        meins  TYPE afpo-meins,           "数量单位
        matnr  TYPE afpo-matnr,           "物料编码
        maktx  TYPE makt-maktx,           "物料描述
        dauat  TYPE afpo-dauat,           "订单类型
        ltrmi  TYPE afpo-ltrmi,           "实际完成日期
        gyms   TYPE afvc-ltxa1,           "工艺描述
        bzrg   TYPE afvv-vgw01,           "标准人工
        bzjq   TYPE afvv-vgw01,           "标准机器
        bzd    TYPE afvv-vgw01,           "标准电
        bzzf   TYPE afvv-vgw01,           "标准制费
        rgcy   TYPE afvv-vgw01,           "人工差异
        jqcy   TYPE afvv-vgw01,           "机器差异
        rgcybl TYPE afvv-vgw01,           "人工差异比例%
        jqcybl TYPE afvv-vgw01,           "机器差异比例%
      END OF ty_data.


TYPES:BEGIN OF ty_aufnr,
        aufnr TYPE aufnr,
      END OF ty_aufnr .

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,
      END OF ty_matnr.

TYPES:BEGIN OF ty_kdauf,
        kdauf TYPE afpo-kdauf,
        xmms  TYPE string,
      END OF ty_kdauf.

DATA:gt_kdauf   TYPE TABLE OF ty_kdauf,
     gt_kdauf_1 TYPE TABLE OF ty_kdauf,
     gt_kdauf_2 TYPE TABLE OF ty_kdauf,
     gs_kdauf   TYPE ty_kdauf.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_aufnr   TYPE TABLE OF ty_aufnr,
     gt_aufnr_1 TYPE TABLE OF ty_aufnr,
     gs_aufnr   TYPE ty_aufnr.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

DATA:gt_afru TYPE TABLE OF afru,
     gs_afru TYPE afru.


DATA:gt_afvc TYPE TABLE OF afvc,
     gs_afvc TYPE afvc.


DATA:gt_afvv TYPE TABLE OF afvv,
     gs_afvv TYPE afvv.

DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

DATA:gt_afpo   TYPE TABLE OF afpo,
     gt_afpo_1 TYPE TABLE OF afpo,
     gt_afpo_2 TYPE TABLE OF afpo,
     gs_afpo   TYPE afpo.

DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.

"XML格式变量定义
DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

************************************************************************
* Global Variant
************************************************************************


DATA:l_tabix TYPE sy-tabix .

DATA:l_check TYPE i.

************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS:
                s_werks FOR  afru-werks OBLIGATORY,     "工厂
                s_aufnr FOR  afru-aufnr,      "生产订单
                s_matnr FOR  afpo-matnr,     "物料
                s_budat FOR  afru-budat.     "过账日期

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
  l_check = 1.
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
  SELECT * INTO TABLE gt_afru
    FROM afru
    WHERE werks IN s_werks
    AND   budat IN s_budat
    AND   aufnr IN s_aufnr .
  SORT gt_afru BY rueck rmzhl aufpl aplzl .

  MOVE-CORRESPONDING gt_afru TO gt_aufnr.

  SORT gt_aufnr BY aufnr .

  DELETE ADJACENT DUPLICATES FROM gt_aufnr COMPARING aufnr .

  IF gt_aufnr IS NOT INITIAL.

    SELECT * INTO TABLE gt_afpo
      FROM afpo
      FOR ALL ENTRIES IN gt_aufnr
      WHERE aufnr = gt_aufnr-aufnr
      AND   matnr IN s_matnr .

    SORT gt_afpo BY aufnr .

    gt_afpo_1 = gt_afpo.
    gt_afpo_2 = gt_afpo.

    DELETE gt_afpo_1 WHERE kdauf IS INITIAL.

    MOVE-CORRESPONDING gt_afpo_1 TO gt_kdauf_1.
    SORT gt_kdauf_1 BY kdauf.
    DELETE ADJACENT DUPLICATES FROM  gt_kdauf_1 COMPARING kdauf.



    DELETE gt_afpo_2 WHERE kdauf IS NOT INITIAL.

    MOVE-CORRESPONDING gt_afpo_2 TO gt_aufnr_1.

    SORT gt_aufnr_1 BY aufnr.
    DELETE ADJACENT DUPLICATES FROM gt_aufnr_1 COMPARING aufnr .

    LOOP AT gt_aufnr_1 INTO gs_aufnr.
      CLEAR:gs_kdauf.
      gs_kdauf-kdauf = gs_aufnr+0(7).
      APPEND gs_kdauf TO gt_kdauf_2.
    ENDLOOP.

    SORT gt_kdauf_2 BY kdauf .
    DELETE ADJACENT DUPLICATES FROM gt_kdauf_2 COMPARING kdauf .

    IF  gt_kdauf_1 IS NOT INITIAL.
      gt_kdauf = gt_kdauf_1.

    ENDIF.

    IF  gt_kdauf_2 IS NOT INITIAL.
      APPEND LINES OF gt_kdauf_2 TO gt_kdauf.

    ENDIF.
    SORT gt_kdauf BY kdauf .
    DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf .


    MOVE-CORRESPONDING gt_afpo TO gt_matnr.

    DELETE gt_matnr WHERE matnr EQ ''.

    SORT gt_matnr BY matnr.

    DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

    SELECT * INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN gt_matnr
      WHERE matnr = gt_matnr-matnr
      AND  spras = '1'.

    SORT gt_makt BY matnr.

  ENDIF.

  CHECK gt_afru IS NOT INITIAL.
  LOOP AT gt_afru INTO gs_afru .
    l_tabix = sy-tabix .
    READ TABLE gt_afpo INTO gs_afpo WITH KEY aufnr = gs_afru-aufnr
                                    BINARY SEARCH .
    IF sy-subrc NE 0 .
      DELETE gt_afru INDEX l_tabix.
    ENDIF.

  ENDLOOP.
  SELECT * INTO TABLE gt_afvc
    FROM afvc
    FOR ALL ENTRIES IN gt_afru
    WHERE aufpl = gt_afru-aufpl
    AND   aplzl = gt_afru-aplzl .

  SORT gt_afvc BY aufpl aplzl .

  SELECT * INTO TABLE gt_afvv
    FROM afvv
    FOR ALL ENTRIES IN gt_afru
    WHERE aufpl = gt_afru-aufpl
    AND   aplzl = gt_afru-aplzl .

  SORT gt_afvv BY aufpl aplzl .

  SELECT * INTO TABLE gt_vbak
    FROM vbak.

  SORT gt_vbak BY vbeln .

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

  "查询项目名称
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  CLEAR t_tline[].
  DATA:tname TYPE thead-tdname.
  LOOP AT  gt_kdauf INTO gs_kdauf.
    tname = gs_kdauf-kdauf..
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = sy-langu
        name                    = tname
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        CONCATENATE gs_kdauf-xmms t_tline-tdline INTO gs_kdauf-xmms.
        CLEAR t_tline.
      ENDLOOP.
    ENDIF.
    MODIFY gt_kdauf FROM gs_kdauf .

  ENDLOOP.
  LOOP AT gt_afru INTO gs_afru .
    CLEAR:gs_data.
    "确认
    gs_data-rueck = gs_afru-rueck.
    "确认计数器
    gs_data-rmzhl = gs_afru-rmzhl.

    "订单工艺路线
    gs_data-aufpl = gs_afru-aufpl .
    ""工艺路线计数器
    gs_data-aplzl = gs_afru-aplzl.
    "录入日期
    gs_data-ersda = gs_afru-ersda.
    "录入人
    gs_data-ernam = gs_afru-ernam.
    "过账日期
    gs_data-budat = gs_afru-budat.
    "工厂
    gs_data-werks = gs_afru-werks.
    "备注
    gs_data-ltxa1 = gs_afru-ltxa1.
    "人工工、机器工时、电工时、制费工时、人数、加班工时、产量
    IF gs_afru-stzhl IS NOT INITIAL.
      gs_data-ism01 = gs_afru-ism01 * -1 .
      gs_data-ism02 = gs_afru-ism02 * -1.
      gs_data-ism03 = gs_afru-ism03 * -1 .
      gs_data-ism04 = gs_afru-ism04 * -1.
      gs_data-ism05 = gs_afru-ism05 * -1.
      gs_data-ism06 = gs_afru-ism06 * -1.
      gs_data-lmnga = gs_afru-lmnga * -1 .

    ELSE.
      gs_data-ism01 = gs_afru-ism01  .
      gs_data-ism02 = gs_afru-ism02 .
      gs_data-ism03 = gs_afru-ism03  .
      gs_data-ism04 = gs_afru-ism04 .
      gs_data-ism05 = gs_afru-ism05 .
      gs_data-ism06 = gs_afru-ism06 .
      gs_data-lmnga = gs_afru-lmnga .
    ENDIF.
    "作业单位
    gs_data-ile01 = gs_afru-ile01.
    "物料单位
    gs_data-gmein = gs_afru-gmein.
    "生产订单
    gs_data-aufnr = gs_afru-aufnr.
    "冲销标识
    gs_data-stzhl = gs_afru-stzhl.
    "项目号
    READ TABLE gt_afpo INTO gs_afpo WITH KEY aufnr = gs_data-aufnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0 .

      IF gs_data-kdauf IS  NOT  INITIAL.
        gs_data-kdauf = gs_afpo-kdauf.
      ELSE.
        READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_afpo-aufnr+0(7)
                                        BINARY SEARCH.
        IF sy-subrc EQ 0 .
          gs_data-kdauf = gs_vbak-vbeln.
        ENDIF.
      ENDIF.
      "项目描述\
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf = gs_data-kdauf
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-xmms = gs_kdauf-xmms.
      ENDIF.
      "订单数量
      gs_data-psmng = gs_afpo-psmng.
      "收货数量
      gs_data-wemng = gs_afpo-wemng.
      "数量单位
      gs_data-meins = gs_afpo-meins.
      "物料编码
      gs_data-matnr = gs_afpo-matnr.
      "物料描述
      READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-maktx = gs_makt-maktx.
      ENDIF.
      "订单类型
      gs_data-dauat = gs_afpo-dauat.
      "实际完成日期
      gs_data-ltrmi = gs_afpo-ltrmi.

    ENDIF.
    "工艺描述
    READ TABLE gt_afvc INTO gs_afvc WITH KEY aufpl = gs_afru-aufpl
                                             aplzl = gs_afru-aplzl
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_data-gyms = gs_afvc-ltxa1.
    ENDIF.
    READ TABLE gt_afvv INTO gs_afvv WITH KEY aufpl = gs_afru-aufpl
                                              aplzl = gs_afru-aplzl
                                              BINARY SEARCH.
    IF sy-subrc EQ 0 .
      "标准人工
      gs_data-bzrg = gs_data-lmnga * gs_afvv-vgw01 / gs_afvv-bmsch .
      "标准机器
      gs_data-bzjq = gs_data-lmnga * gs_afvv-vgw02 / gs_afvv-bmsch .
      "标准电
      gs_data-bzd  = gs_data-lmnga * gs_afvv-vgw03 / gs_afvv-bmsch .
      "标准制费
      gs_data-bzzf  = gs_data-lmnga * gs_afvv-vgw04 / gs_afvv-bmsch .
      "人工差异
      gs_data-rgcy = gs_data-bzrg - gs_data-ism01.
      "机器差异
      gs_data-jqcy = gs_data-bzjq - gs_data-ism02.
      "人工差异比例%
      IF gs_data-bzrg NE 0 .
        gs_data-rgcybl = gs_data-rgcy / gs_data-bzrg * 100 .
      ENDIF.
      "机器差异比例%
      IF gs_data-bzjq NE 0 .
        gs_data-jqcybl = gs_data-jqcy / gs_data-bzjq  * 100 .
      ENDIF.
    ENDIF.
    APPEND gs_data TO gt_data.
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

  CALL TRANSFORMATION zpp104_1_zh
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

  dname = 'ZPP104报工日报表导出.XLS'.
  CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
  DELETE DATASET dname .
  OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0. EXIT. ENDIF.
  LOOP AT xml_table INTO wa_xml.
    TRANSFER wa_xml TO dname.
  ENDLOOP.
  CLOSE DATASET dname.


ENDFORM.
