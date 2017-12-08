REPORT zmm101_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170113
*& Request       :
*& Descriptions  : 采购申请执行报表导出
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :

** 修改日期   开发人员  请求号        描述
" 20170221   IT02     ED1K905261   品能报表导出增加:需求跟踪号、项目描述、采购订单审批、统计交货日期、备注
"20170331    IT02     ED1K905323   增加字段：实际到货日期、预计超期天数、实际超期天数、实际超期天数
"优化未入库数量
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:eban,ekko,ekpo.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        banfn     TYPE eban-banfn,             "采购申请
        bnfpo     TYPE eban-bnfpo,             "采购申请行
        loekz     TYPE eban-loekz,             "采购申请删除标识
        afnam     TYPE eban-afnam,             "申请人
        matnr     TYPE eban-matnr,             "物料号
        txz01     TYPE eban-txz01,             "短文本
        menge     TYPE eban-menge,             "数量
        meins     TYPE eban-meins,             "单位
        werks     TYPE eban-werks,             "工厂
        badat     TYPE eban-badat,             "申请日期
        lfdat     TYPE eban-lfdat,             "请求交货日期
        plifz     TYPE eban-plifz,             "计划交货天数
        ebeln     TYPE ekpo-ebeln,             "采购订单
        ebelp     TYPE ekpo-ebelp,             "采购订单行项目
        bedat     TYPE ekko-bedat,             "采购订单下单日期
        eindt     TYPE eket-eindt,             "回复交货日期
        cg_menge  TYPE ekpo-menge,             "采购订单数量
        cg_meins  TYPE ekpo-meins,             "采购订单单位
        lifnr     TYPE ekko-lifnr,             "供应商
        gysms     TYPE lfa1-name1,             "供应商描述
        ekgrp     TYPE ekko-ekgrp,             "采购组
        eknam     TYPE t024-eknam,             "采购组描述
        yshwrk    TYPE ekpo-menge,             "已收货未入库数量
        rksl      TYPE eket-menge,             "入库数量
        wrksl     TYPE ekpo-menge,             "未入库数量
        elikz     TYPE ekpo-elikz,             "交货已完成标识
        bednr     TYPE eban-bednr,              "需求跟踪号
        xmms      TYPE string,                  "项目描述
        cgddsp    TYPE string,                  "采购订单审批
        slfdt     TYPE eket-slfdt,              "统计交货日期
        bz        TYPE string,                  "备注
        sjdhrq    TYPE ekbe-budat,               "实际到货日期
        yjcqts    TYPE i,                         "预计超期天数
        sjcqts    TYPE i,                         "实际超期天数
        cgcqts    TYPE i,                         "采购超期天数
        netpr     TYPE ekpo-netpr,               "净价-采购
        peinh_cg  TYPE ekpo-peinh,               "价格单位-采购
        netwr     TYPE ekpo-netwr,               "净值
        brtwr     TYPE ekpo-brtwr,               "含税值
        verpr     TYPE mbew-verpr,               "标准价
        peinh_bzj TYPE mbew-peinh,               "标准价格单位
      END OF ty_data.

"采购订单定义
TYPES:BEGIN OF ty_cgdd,
        ebeln TYPE ekko-ebeln,
      END OF ty_cgdd .

"采购订单明细定义
TYPES:BEGIN OF ty_cgmx,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        bz    TYPE string,
      END OF ty_cgmx.

"采购行项目：已收货未入库数量
TYPES:BEGIN OF ty_yshwrk,
        ebeln TYPE eban-ebeln,
        ebelp TYPE eban-ebelp,
        wesbs TYPE ekbe-wesbs,
      END OF ty_yshwrk.

"采购组类型
TYPES:BEGIN OF ty_cgz,
        ekgrp TYPE ekko-ekgrp,
      END OF ty_cgz .

"供应商类型
TYPES:BEGIN OF ty_gys,
        lifnr TYPE ekko-lifnr,
      END OF ty_gys.

"需求跟踪号
TYPES:BEGIN OF ty_bednr,
        bednr TYPE eban-bednr,
        xmms  TYPE string,
      END OF ty_bednr .

"工厂物料
TYPES:BEGIN OF ty_gcwl,
        matnr TYPE mara-matnr, "物料
        werks TYPE werks_d,    "工厂
      END OF ty_gcwl.

"采购组定义
DATA:gt_cgz TYPE TABLE OF ty_cgz,
     gs_cgz TYPE ty_cgz.

"供应商定义
DATA:gt_gys TYPE TABLE OF ty_gys,
     gs_gys TYPE ty_gys.

"已收货未入库数量
DATA:gt_yshwrk TYPE TABLE OF ty_yshwrk,
     gs_yshwrk TYPE ty_yshwrk.

"采购行项目定义
DATA:gt_cgmx TYPE TABLE OF ty_cgmx,
     gs_cgmx TYPE ty_cgmx.

"采购订单定义
DATA:gt_cgdd TYPE TABLE OF ty_cgdd,
     gs_cgdd TYPE ty_cgdd.

"统计明细数据定义
DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

"采购明细定义
DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

"采购抬头定义
DATA:gt_ekko TYPE TABLE OF ekko,
     gs_ekko TYPE ekko.

"计划协议定义
DATA:gt_eket TYPE TABLE OF eket,
     gs_eket TYPE eket.

"采购历史定义
DATA:gt_ekbe TYPE TABLE OF ekbe,
     gs_ekbe TYPE ekbe.

DATA:gt_ekbe_2 TYPE TABLE OF ekbe,
     gs_ekbe_2 TYPE ekbe.

"采购组定义
DATA:gt_t024 TYPE TABLE OF t024,
     gs_t024 TYPE t024.

"供应商主数据定义
DATA:gt_lfa1 TYPE TABLE OF lfa1,
     gs_lfa1 TYPE lfa1.

"项目描述
DATA:gt_bednr TYPE TABLE OF ty_bednr,
     gs_bednr TYPE ty_bednr.

"销售订单
DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.

"工厂信息
DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

"价格
DATA:gt_mbew TYPE TABLE OF mbew,
     gs_mbew TYPE mbew.

"工厂物料
DATA:gt_gcwl TYPE TABLE OF ty_gcwl,
     gs_gcwl TYPE ty_gcwl.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
CLEAR t_tline[].
DATA:tname TYPE thead-tdname.

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
SELECT-OPTIONS:
                s_werks FOR eban-werks ,     "工厂
                s_banfn FOR eban-banfn,       "采购申请
                s_ebeln FOR  ekpo-ebeln,     "采购订单
                s_matnr FOR  eban-matnr,    "物料编码
                s_afnam FOR  eban-afnam,   "申请人
                s_badat FOR  eban-badat,    "申请日期
                s_lfdat FOR  eban-lfdat.   "请求交货日期

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
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_zh_xml.   "转换XML格式
  PERFORM frm_cc_sapfwq."生成SAP服务器文件

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
**FORM frm_auth_check .
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'ACTVT' FIELD '03'
                                       ID 'WERKS' FIELD gs_t001w-werks.
    IF sy-subrc NE 0 .
      MESSAGE e603(fco) WITH gs_t001w-werks DISPLAY LIKE 'E'.
      STOP.
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
  "读取采购申请明细
  SELECT banfn bnfpo loekz afnam matnr txz01 menge meins werks
         lfdat badat plifz ebeln ebelp bednr
     INTO CORRESPONDING FIELDS OF TABLE gt_data
     FROM eban
     WHERE werks IN s_werks
      AND  banfn IN s_banfn
      AND  ebeln IN s_ebeln
      AND  matnr IN s_matnr
      AND  afnam IN s_afnam
      AND  lfdat IN s_lfdat
      AND  badat IN s_badat .

  SORT gt_data BY banfn bnfpo .

  CHECK gt_data  IS NOT INITIAL .

  MOVE-CORRESPONDING gt_data TO gt_bednr.
  SORT gt_bednr BY bednr .
  DELETE gt_bednr WHERE bednr IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM gt_bednr COMPARING bednr .

  "工厂物料
  MOVE-CORRESPONDING gt_data TO gt_gcwl.
  SORT gt_gcwl BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM gt_gcwl COMPARING matnr werks .

  SELECT * INTO TABLE gt_mbew
    FROM mbew
    FOR ALL ENTRIES IN gt_gcwl
    WHERE matnr = gt_gcwl-matnr
    AND   bwkey = gt_gcwl-werks.

  SORT gt_mbew BY matnr bwkey .

  "采购订单分类
  MOVE-CORRESPONDING gt_data TO gt_cgdd.

  SORT gt_cgdd BY ebeln .

  DELETE ADJACENT DUPLICATES FROM gt_cgdd COMPARING ebeln .

  "采购行项目分类
  MOVE-CORRESPONDING gt_data TO gt_cgmx .

  SORT gt_cgmx BY ebeln ebelp .

  DELETE ADJACENT DUPLICATES FROM gt_cgmx COMPARING ebeln ebelp .

  IF gt_cgdd IS NOT INITIAL .
    "读取采购订单抬头主数据
    SELECT * INTO TABLE gt_ekko
      FROM  ekko
      FOR ALL ENTRIES IN gt_cgdd
      WHERE ebeln = gt_cgdd-ebeln .

    SORT gt_ekko BY ebeln .

    "采购组分类
    MOVE-CORRESPONDING gt_ekko TO gt_cgz.
    SORT gt_cgz BY ekgrp .
    DELETE ADJACENT DUPLICATES FROM gt_cgz COMPARING ekgrp .

    "读取采购组主数据
    SELECT * INTO TABLE gt_t024
      FROM t024
      FOR ALL ENTRIES IN gt_cgz
      WHERE ekgrp = gt_cgz-ekgrp.
    SORT gt_t024 BY ekgrp .

    "供应商分类
    MOVE-CORRESPONDING gt_ekko TO gt_gys.
    SORT gt_gys BY lifnr .
    DELETE ADJACENT DUPLICATES FROM gt_gys COMPARING lifnr.

    "读取供应商主数据
    SELECT * INTO TABLE gt_lfa1
      FROM lfa1
      FOR ALL ENTRIES IN gt_gys
      WHERE lifnr = gt_gys-lifnr.

    SORT gt_lfa1 BY lifnr .

  ENDIF.

  IF gt_cgmx IS NOT INITIAL .
    LOOP AT gt_cgmx INTO gs_cgmx.
      REFRESH:t_tline[] .
      CONCATENATE gs_cgmx-ebeln gs_cgmx-ebelp INTO tname .
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = 'F01'
          language                = sy-langu
          name                    = tname
          object                  = 'EKPO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
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
          CONCATENATE gs_cgmx-bz t_tline-tdline INTO gs_cgmx-bz.
          CLEAR t_tline.
        ENDLOOP.
      ENDIF.
      MODIFY gt_cgmx FROM gs_cgmx .

    ENDLOOP.

    "读取采购明细
    SELECT * INTO TABLE gt_ekpo
    FROM ekpo
    FOR ALL ENTRIES IN gt_data
    WHERE  banfn = gt_data-banfn
      AND  bnfpo = gt_data-bnfpo
      AND  loekz NE ''.

    SORT gt_ekpo BY  banfn  bnfpo .

    "读取采购历史
    SELECT * INTO TABLE gt_ekbe
      FROM ekbe
        FOR ALL ENTRIES IN gt_cgmx
    WHERE  ebeln = gt_cgmx-ebeln
      AND  ebelp = gt_cgmx-ebelp
      AND  vgabe = '1'.

    SORT gt_ekbe BY ebeln ebelp .

    gt_ekbe_2 = gt_ekbe.
    SORT gt_ekbe_2 BY ebeln ASCENDING  ebelp ASCENDING  budat ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_ekbe_2 COMPARING ebeln ebelp .

    "计划协议计划行
    SELECT * INTO TABLE gt_eket
      FROM eket
      FOR ALL ENTRIES IN gt_cgmx
     WHERE  ebeln = gt_cgmx-ebeln
       AND  ebelp = gt_cgmx-ebelp .
    SORT gt_eket BY ebeln ebelp .

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

  "查询项目描述
  LOOP AT  gt_bednr INTO gs_bednr.
    tname = gs_bednr-bednr.
    REFRESH:t_tline.
    CONDENSE gs_bednr-bednr .
    TRANSLATE gs_bednr-bednr TO UPPER CASE.
    SELECT SINGLE * INTO gs_vbak FROM vbak
        WHERE vbeln = gs_bednr-bednr .
    IF sy-subrc NE 0 .
      CONTINUE .
    ELSE.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'Z001'
          language                = sy-langu
          name                    = tname
          object                  = 'VBBK'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
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
        CONTINUE.
      ELSE.
        IF t_tline[] IS NOT INITIAL.
          LOOP AT t_tline.
            CONCATENATE gs_bednr-xmms t_tline-tdline INTO gs_bednr-xmms.
            CLEAR t_tline.
          ENDLOOP.
        ENDIF.
        MODIFY gt_bednr FROM gs_bednr .
      ENDIF.
    ENDIF.


  ENDLOOP.
  SORT gt_bednr BY bednr .
  "累计：已收货未入库数量
  LOOP AT gt_ekbe INTO gs_ekbe.
    CLEAR:gs_yshwrk.
    gs_yshwrk-ebeln = gs_ekbe-ebeln.
    gs_yshwrk-ebelp = gs_ekbe-ebelp.
    gs_yshwrk-wesbs = gs_ekbe-wesbs.
    IF gs_ekbe-shkzg = 'H'.
      gs_yshwrk-wesbs = gs_yshwrk-wesbs  * - 1.
    ENDIF.
    COLLECT gs_yshwrk INTO gt_yshwrk .
  ENDLOOP.
  SORT gt_yshwrk BY ebeln ebelp .

  LOOP AT gt_data ASSIGNING <fs_data>.
    "项目描述
    READ TABLE gt_bednr INTO gs_bednr WITH KEY bednr = <fs_data>-bednr
                                       BINARY SEARCH.
    IF sy-subrc EQ 0 .
      <fs_data>-xmms = gs_bednr-xmms.
    ENDIF.

    READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = <fs_data>-ebeln
                                            BINARY SEARCH .
    IF sy-subrc EQ 0 .
      "采购订单下单日期
      <fs_data>-bedat = gs_ekko-bedat.
      "供应商
      <fs_data>-lifnr = gs_ekko-lifnr.
      "供应商描述
      READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr =  <fs_data>-lifnr
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        <fs_data>-gysms = gs_lfa1-name1.
      ENDIF.

      "采购组
      <fs_data>-ekgrp = gs_ekko-ekgrp.
      "采购组描述
      READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = <fs_data>-ekgrp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        <fs_data>-eknam = gs_t024-eknam .
      ENDIF.
      "采购订单审批
      IF gs_ekko-frgke EQ  'U' .
        <fs_data>-cgddsp = '待审批'.
      ELSE.
        <fs_data>-cgddsp = '已审批'.
      ENDIF.

    ENDIF.
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY banfn = <fs_data>-banfn
                                             bnfpo = <fs_data>-bnfpo
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      "采购订单数量
      <fs_data>-cg_menge = gs_ekpo-menge.
      "采购订单单位
      <fs_data>-cg_meins = gs_ekpo-meins.

      "已交货完成标识
      <fs_data>-elikz = gs_ekpo-elikz.
      "净价：
      <fs_data>-netpr = gs_ekpo-netpr.
      "价格单位
      <fs_data>-peinh_cg = gs_ekpo-peinh.
      "净值
      <fs_data>-netwr = gs_ekpo-netwr.
      "含税值
      <fs_data>-brtwr = gs_ekpo-brtwr.
    ENDIF.

    READ TABLE gt_mbew INTO gs_mbew WITH KEY matnr = <fs_data>-matnr
                                             bwkey = <fs_data>-werks
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      "标准价
      <fs_data>-verpr = gs_mbew-verpr.
      "标准价格单位
      <fs_data>-peinh_bzj = gs_mbew-peinh.
    ENDIF.

    READ TABLE gt_eket INTO gs_eket WITH KEY ebeln = <fs_data>-ebeln
                                             ebelp = <fs_data>-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      "回复交货日期
      <fs_data>-eindt = gs_eket-eindt.
      "入库数量
      <fs_data>-rksl = gs_eket-wemng.
      "统计交货日期
      <fs_data>-slfdt = gs_eket-slfdt.
    ENDIF.

    "未入库数量
    IF <fs_data>-elikz  NE 'X'.
      <fs_data>-wrksl = <fs_data>-cg_menge - <fs_data>-rksl .
    ENDIF.



    "已收货未入库数量
    READ TABLE gt_yshwrk INTO gs_yshwrk WITH KEY ebeln = <fs_data>-ebeln
                                                 ebelp = <fs_data>-ebelp
                                                 BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-yshwrk = gs_yshwrk-wesbs.
    ENDIF.
    "备注
    READ TABLE gt_cgmx INTO gs_cgmx WITH KEY ebeln = <fs_data>-ebeln
                                             ebelp = <fs_data>-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      <fs_data>-bz = gs_cgmx-bz.
    ENDIF.

    "实际到货日期
    READ TABLE gt_ekbe_2 INTO gs_ekbe_2 WITH KEY ebeln = <fs_data>-ebeln
                                             ebelp = <fs_data>-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_data>-sjdhrq = gs_ekbe_2-budat.
    ENDIF.
    "预计超期天数
    <fs_data>-yjcqts  = <fs_data>-eindt - <fs_data>-lfdat .
    "实际超期天数
    <fs_data>-sjcqts = <fs_data>-sjdhrq - <fs_data>-lfdat .
    "采购超期天数
    <fs_data>-cgcqts = <fs_data>-sjdhrq - <fs_data>-eindt .

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

  CALL TRANSFORMATION zmm101_1_zh
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
  IF xml_table IS NOT INITIAL.
    dname = 'ZMM101采购申请导出列表.XLS'.
    CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
    DELETE DATASET dname .
    OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
    IF sy-subrc NE 0. EXIT. ENDIF.
    LOOP AT xml_table INTO wa_xml.
      TRANSFER wa_xml TO dname.
    ENDLOOP.
    CLOSE DATASET dname.
  ENDIF.
ENDFORM.
