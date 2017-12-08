REPORT zmm102.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170316
*& Request       :ED1K905301
*& Descriptions  : 物料出入库明细表导出
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
TABLES:mseg,mkpf,ekko,ekpo,eket.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        mblnr      TYPE mseg-mblnr,                     "物料凭证
        mjahr      TYPE mseg-mjahr,                     "物料凭证年度
        zeile      TYPE mseg-zeile,                     "物料凭证行
        werks      TYPE mseg-werks,                     "工厂
        matnr      TYPE mseg-matnr,                     "物料号
        maktx      TYPE makt-maktx,                     "物料描述
        mtart      TYPE mara-mtart,                     "物料类型
        mtbez      TYPE t134t-mtbez,                    "物料类型描述
        menge      TYPE mseg-menge,                     "数量
        meins      TYPE mseg-meins,                     "单位
        budat_mkpf TYPE mseg-budat_mkpf,                "过账日期
        cpudt_mkpf TYPE mseg-cpudt_mkpf,                "操作日期
        bwart      TYPE mseg-bwart,                     "移动类型
        btext      TYPE t156t-btext,                    "移动类型文本
        lgort      TYPE mseg-lgort,                     "库位
        lgobe      TYPE t001l-lgobe,                    "库位描述
        ebeln      TYPE mseg-ebeln,                     "采购订单行
        ebelp      TYPE mseg-ebelp,                     "采购订单行
        bednr      TYPE ekpo-bednr,                     "需求跟踪号
        aufnr      TYPE mseg-aufnr,                     "生产订单
        kdauf      TYPE mseg-kdauf,                     "销售订单号
        shkzg      TYPE mseg-shkzg,                     "借贷标识
        xmms       TYPE string,                         "项目描述
        cgbz       TYPE string,                         "采购备注
        lifnr      TYPE mseg-lifnr,                     "供应商
        lifnr_name TYPE lfa1-name1,                     "供应描述
        usnam_mkpf TYPE mseg-usnam_mkpf,                "用户名
        sgtxt      TYPE mseg-sgtxt,                     "抬头文本
        xblnr_mkpf TYPE mseg-xblnr_mkpf,                "参照
        cg_menge   TYPE ekpo-menge,                     "采购订单数量
        cg_wemng   TYPE eket-wemng,                     "采购已入库数量
        eindt      TYPE eket-eindt,                     "回复交货日期
        slfdt      TYPE eket-slfdt,                     "统计交货日期
        psmng      TYPE afpo-psmng,                     "生产订单数量
        sc_wemng   TYPE afpo-wemng,                     "生产已入库数量
      END OF ty_data.

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,     "物料号
      END OF ty_matnr .


TYPES:BEGIN OF ty_kdauf,
        kdauf TYPE kdauf,
        xmmc  TYPE string,

      END OF ty_kdauf .

TYPES:BEGIN OF ty_cgmx,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        cgbz  TYPE string,  "采购备注
      END OF ty_cgmx.

TYPES:BEGIN OF ty_aufnr,
        aufnr TYPE afko-aufnr,
      END OF ty_aufnr .

TYPES:BEGIN OF ty_lifnr,
        lifnr TYPE lfb1-lifnr,
      END OF ty_lifnr.

"生产订单信息
DATA:gt_aufnr TYPE TABLE OF ty_aufnr,
     gs_aufnr TYPE ty_aufnr.

"采购明细
DATA:gt_cgmx TYPE TABLE OF ty_cgmx,
     gs_cgmx TYPE ty_cgmx.

"输出主数据表
DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE  ty_data.

"销售订单
DATA:gt_kdauf   TYPE TABLE OF ty_kdauf,
     gs_kdauf   TYPE ty_kdauf,
     gs_kdauf_1 TYPE ty_kdauf.

"物料信息
DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

"物料主数据
DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

"采购明细数据
DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

"移动类型描述
DATA:gt_t156t TYPE TABLE OF t156t,
     gs_t156t TYPE t156t.

"生产订单信息
DATA:gt_afpo TYPE TABLE OF afpo,
     gs_afpo TYPE afpo.

"采购账号控制分配信息
DATA:gt_ekkn TYPE TABLE OF ekkn,
     gs_ekkn TYPE ekkn.

"库位描述
DATA:gt_t001l TYPE TABLE OF t001l,
     gs_t001l TYPE t001l.

"销售订单信息
DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.

"供应商信息
DATA:gt_lfa1 TYPE TABLE OF lfa1,
     gs_lfa1 TYPE lfa1.

"计划协议计划行
DATA:gt_eket TYPE TABLE OF eket,
     gs_eket TYPE eket.

"物料主数据信息
DATA:gt_mara TYPE TABLE OF mara,
     gs_mara TYPE mara.

"物料类型
DATA:gt_t134t TYPE TABLE OF t134t,
     gs_t134t TYPE t134t.

DATA:gt_lifnr TYPE TABLE OF ty_lifnr,
     gs_lifnr TYPE ty_lifnr.

DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

DATA:l_subrc TYPE sy-subrc.

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .

SELECT-OPTIONS:
                s_werks FOR mseg-werks  OBLIGATORY,"工厂
                s_matnr FOR mseg-matnr,  "物料
                s_bwart FOR mseg-bwart ,  "移动类型
                s_budat FOR mkpf-budat OBLIGATORY,  "过账日期
                s_lgort FOR mseg-lgort. "库位
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
  LOOP AT gt_t001w INTO gs_t001w .
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
          ID 'ACTVT' FIELD '03'
          ID 'WERKS' FIELD  gs_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gs_t001w-werks.
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
  "根据选择屏幕取出查询范围的过账明细
  SELECT mblnr mjahr zeile werks matnr menge shkzg meins budat_mkpf cpudt_mkpf
         bwart lgort ebeln ebelp aufnr kdauf lifnr usnam_mkpf sgtxt xblnr_mkpf
       FROM mseg
       INTO CORRESPONDING FIELDS OF TABLE gt_data
       WHERE werks IN s_werks
       AND  matnr IN s_matnr
       AND  bwart IN s_bwart
       AND  budat_mkpf IN s_budat
       AND  lgort IN s_lgort .

  CHECK gt_data IS NOT INITIAL .

  "输出物料信息
  MOVE-CORRESPONDING gt_data TO gt_matnr .
  DELETE gt_matnr WHERE matnr IS INITIAL.
  SORT gt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .


  "查询物料主数据信息
  IF gt_matnr IS NOT INITIAL.
    SELECT * INTO TABLE gt_makt
  FROM makt
  FOR ALL ENTRIES IN gt_matnr
  WHERE matnr = gt_matnr-matnr
   AND  spras = '1'.
    SORT gt_makt BY matnr .

    "查询物料基本信息
    SELECT * INTO TABLE gt_mara
     FROM  mara
     FOR ALL ENTRIES IN gt_matnr
     WHERE matnr = gt_matnr-matnr.

    SORT gt_mara BY matnr .

  ENDIF.


  "输出采购明细
  MOVE-CORRESPONDING gt_data TO gt_cgmx.
  DELETE gt_cgmx WHERE ebeln IS INITIAL.
  SORT gt_cgmx BY ebeln ebelp .
  DELETE ADJACENT DUPLICATES FROM gt_cgmx COMPARING ebeln ebelp .

  "查询采购明细
  IF gt_cgmx IS NOT INITIAL.
    SELECT * INTO TABLE gt_ekpo
   FROM ekpo
   FOR ALL ENTRIES IN gt_cgmx
   WHERE ebeln = gt_cgmx-ebeln
     AND ebelp = gt_cgmx-ebelp.
    SORT gt_ekpo BY ebeln ebelp .

    "取出采购凭证的账户设置
    SELECT * INTO TABLE gt_ekkn
       FROM ekkn
       FOR ALL ENTRIES IN gt_cgmx
       WHERE ebeln = gt_cgmx-ebeln
        AND ebelp = gt_cgmx-ebelp
        AND vbeln NE ''.
    SORT gt_ekkn BY ebeln ebelp .

    "取出计划协议计划行
    SELECT * INTO TABLE gt_eket
      FROM eket
      FOR ALL ENTRIES IN gt_cgmx
       WHERE ebeln = gt_cgmx-ebeln
        AND ebelp = gt_cgmx-ebelp .
    SORT gt_eket BY ebeln ebelp .
  ENDIF.


  "输出销售订单信息

  MOVE-CORRESPONDING gt_data TO gt_kdauf .

  DELETE gt_kdauf WHERE kdauf IS INITIAL  .

  SORT gt_kdauf BY kdauf.
  DELETE ADJACENT DUPLICATES FROM gt_kdauf  COMPARING kdauf.
  SORT gt_kdauf BY kdauf .


  "查收移动类型描述
  SELECT * INTO TABLE gt_t156t
   FROM t156t
   WHERE spras = '1'.

  SORT gt_t156t BY bwart .

  "生产订单信息
  MOVE-CORRESPONDING gt_data TO gt_aufnr .
  DELETE gt_aufnr WHERE aufnr IS INITIAL.
  SORT gt_aufnr BY aufnr .
  DELETE ADJACENT DUPLICATES FROM gt_aufnr COMPARING aufnr .


  IF gt_aufnr IS NOT INITIAL.
    "取出订单项
    SELECT * INTO TABLE gt_afpo
     FROM afpo
     FOR ALL ENTRIES IN gt_aufnr
     WHERE aufnr = gt_aufnr-aufnr.

    SORT gt_afpo BY aufnr .

  ENDIF.

  "库位描述
  SELECT * INTO TABLE gt_t001l
    FROM t001l
    WHERE werks IN s_werks .
  SORT gt_t001l BY werks lgort .


  "销售订单信息
  SELECT * INTO TABLE gt_vbak
    FROM vbak .
  SORT gt_vbak BY vbeln .

  "供应商信息
  MOVE-CORRESPONDING gt_data TO gt_lifnr.
  DELETE gt_lifnr WHERE lifnr IS INITIAL.
  SORT gt_lifnr BY lifnr .
  DELETE ADJACENT DUPLICATES FROM gt_lifnr COMPARING lifnr.
  IF gt_lifnr IS NOT INITIAL.
    SELECT * INTO TABLE gt_lfa1
      FROM lfa1
      FOR ALL ENTRIES IN gt_lifnr
      WHERE lifnr = gt_lifnr-lifnr.
    SORT gt_lfa1 BY lifnr .

  ENDIF.

  "物料类型
  SELECT * INTO TABLE gt_t134t
    FROM t134t
    WHERE spras = 1.
  SORT gt_t134t BY mtart .


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

  "销售订单追加
  "追加生产订单信息
  LOOP AT gt_aufnr INTO gs_aufnr.
    READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_aufnr+0(7)
                                          BINARY SEARCH.
    IF sy-subrc EQ 0 .
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_aufnr+0(7) .
      IF sy-subrc NE 0.
        CLEAR:gs_kdauf_1.
        gs_kdauf_1-kdauf = gs_aufnr+0(7) .
        APPEND gs_kdauf_1 TO gt_kdauf .

      ENDIF.
    ENDIF.

  ENDLOOP.

  SORT gt_kdauf BY kdauf .





  "追加采购订单信息
  LOOP AT gt_cgmx INTO gs_cgmx.
    READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_cgmx-ebeln
                                             ebelp = gs_cgmx-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0.
      CLEAR:gs_kdauf_1.
      gs_kdauf_1-kdauf = gs_ekkn-vbeln .
      APPEND gs_kdauf_1 TO gt_kdauf.
    ELSE.
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_cgmx-ebeln
                                               ebelp = gs_cgmx-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        CONDENSE gs_ekpo-bednr .
        READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln =  gs_ekpo-bednr
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_vbak-vbeln..
          IF sy-subrc NE 0.
            CLEAR:gs_kdauf_1.
            gs_kdauf_1-kdauf = gs_vbak-vbeln..
            APPEND gs_kdauf_1 TO gt_kdauf.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_kdauf BY kdauf .
  "销售名称
  LOOP AT gt_kdauf INTO gs_kdauf.

    PERFORM selxmmc USING gs_kdauf-kdauf sy-langu  CHANGING gs_kdauf-xmmc .
    MODIFY gt_kdauf FROM gs_kdauf.
  ENDLOOP.


  "采购备注
  LOOP AT gt_cgmx INTO gs_cgmx .
    PERFORM selcgxx USING gs_cgmx-ebeln gs_cgmx-ebelp CHANGING gs_cgmx-cgbz.
    MODIFY gt_cgmx FROM gs_cgmx.

  ENDLOOP.

  "采购信息


  LOOP AT gt_data INTO gs_data.

    IF gs_data-matnr IS NOT INITIAL.
      "物料描述
      READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-maktx = gs_makt-maktx.
      ENDIF.
      "物料类型
      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_data-matnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-mtart = gs_mara-mtart.
      ENDIF.
      READ TABLE gt_t134t INTO gs_t134t WITH KEY mtart = gs_data-mtart
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-mtbez = gs_t134t-mtbez+3(21).
      ENDIF.
    ELSE.
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_data-ebeln
                                               ebelp = gs_data-ebelp
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-maktx = gs_ekpo-txz01.
      ENDIF.
    ENDIF.


    IF gs_data-shkzg EQ 'H'.
      gs_data-menge = gs_data-menge * -1.
    ENDIF.

    "移动类型描述
    CASE gs_data-bwart.
      WHEN '101'.
        IF gs_data-aufnr NE ''.
          gs_data-btext = '生产入库'.
        ELSE.
          gs_data-btext = '采购入库'.
        ENDIF.
      WHEN '102'.
        IF gs_data-aufnr NE ''.
          gs_data-btext = '生产入库-撤销'.
        ELSE.
          gs_data-btext = '采购入库-撤销'.
        ENDIF.
      WHEN '103'.
        gs_data-btext = '采购入质检'.
      WHEN '104'.
        gs_data-btext = '采购入质检-撤销'.
      WHEN '105'.
        gs_data-btext = '质检入库'.
      WHEN '106'.
        gs_data-btext = '质检入库-撤销'.
      WHEN '122'.
        gs_data-btext = '退货供应商'.
      WHEN '161'.
        gs_data-btext = '退货供应商'.
      WHEN '124'.
        gs_data-btext = '退货供应商'.
      WHEN '123'.
        gs_data-btext = '退货供应商-撤销'.
      WHEN '162'.
        gs_data-btext = '退货供应商-撤销'.
      WHEN '125'.
        gs_data-btext = '退货供应商-撤销'.
      WHEN '643'.
        gs_data-btext = '发货上海'.
      WHEN '644'.
        gs_data-btext = '发货上海-撤销'.
      WHEN 'Z77'.
      WHEN 'Z80'.
        gs_data-btext = '发货国外'.
      WHEN 'Z78'.
        gs_data-btext = '发货国外-撤销'.
      WHEN OTHERS.
        READ TABLE  gt_t156t INTO gs_t156t WITH KEY bwart = gs_data-bwart
                                           BINARY SEARCH.
        IF sy-subrc EQ 0.
          gs_data-btext = gs_t156t-btext .
        ENDIF.

    ENDCASE.

    "库位描述
    IF gs_data-lgort NE ''.
      READ TABLE gt_t001l INTO gs_t001l WITH KEY werks = gs_data-werks
                                                 lgort = gs_data-lgort
                                         BINARY SEARCH .
      IF sy-subrc EQ 0.
        gs_data-lgobe = gs_t001l-lgobe.
      ENDIF.
    ENDIF.

    "读取项目号
    IF gs_data-kdauf IS INITIAL AND gs_data-aufnr NE ''.
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-aufnr+0(7).
      IF sy-subrc EQ 0.
        gs_data-kdauf = gs_vbak-vbeln .
      ENDIF.


    ELSEIF gs_data-kdauf IS INITIAL AND gs_data-ebeln NE ''.
      READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data-ebeln
                                               ebelp = gs_data-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_data-kdauf = gs_ekkn-vbeln .
      ELSE.
        READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_data-ebeln
                                        ebelp = gs_data-ebelp
                                        BINARY SEARCH.
        IF sy-subrc EQ 0.
          gs_data-bednr =  gs_ekpo-bednr.
          IF gs_data-bednr IS NOT INITIAL .
            CONDENSE gs_data-bednr .
            READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-bednr
                                            BINARY SEARCH.
            IF sy-subrc EQ 0.
              gs_data-kdauf = gs_vbak-vbeln .
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
    "读取项目名称
    IF gs_data-kdauf  IS NOT INITIAL.
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf = gs_data-kdauf.
      IF sy-subrc EQ 0.
        gs_data-xmms = gs_kdauf-xmmc.
      ENDIF.

    ENDIF.
    "采购备注
    IF gs_data-ebeln IS NOT INITIAL.
      READ TABLE gt_cgmx INTO gs_cgmx WITH KEY ebeln = gs_data-ebeln
                                               ebelp = gs_data-ebelp
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-cgbz =  gs_cgmx-cgbz.
      ENDIF.
      "供应商
      READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data-lifnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-lifnr_name = gs_lfa1-name1.
      ENDIF.

      "采购订单数量
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_data-ebeln
                                              ebelp = gs_data-ebelp
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_data-cg_menge = gs_ekpo-menge.
      ENDIF.
      "采购已入库数量
      READ TABLE gt_eket INTO gs_eket WITH KEY  ebeln = gs_data-ebeln
                                            ebelp = gs_data-ebelp
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.

        gs_data-cg_wemng = gs_eket-wemng.
        "回复交货日期
        gs_data-eindt = gs_eket-eindt.
        "统计交货日期
        gs_data-slfdt = gs_eket-slfdt.
      ENDIF.

    ENDIF.

    IF gs_data-aufnr IS NOT INITIAL.

      READ TABLE gt_afpo INTO gs_afpo WITH KEY aufnr = gs_data-aufnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        "生产订单数量
        gs_data-psmng = gs_afpo-psmng.
        "生产已入库数量
        gs_data-sc_wemng = gs_afpo-wemng.
      ENDIF.
    ENDIF.

    MODIFY gt_data FROM gs_data.
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
  CALL TRANSFORMATION zmm102_zh
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
  dname = 'ZMM102物料出入库明细表.XLS'.
  CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
  DELETE DATASET dname .
  OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0. EXIT. ENDIF.
  LOOP AT xml_table INTO wa_xml.
    TRANSFER wa_xml TO dname.
  ENDLOOP.
  CLOSE DATASET dname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_KDAUF_KDAUF  text
*      -->P_SY_LANGU  text
*      <--P_GS_KDAUF_XMMC  text
*----------------------------------------------------------------------*
FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CLEAR:p_xmmc.
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
    LOOP AT it_lines INTO wa_lines .
      IF p_xmmc IS INITIAL.
        p_xmmc = wa_lines-tdline.
      ELSE.
        CONCATENATE p_xmmc    wa_lines-tdline INTO p_xmmc.
      ENDIF.
    ENDLOOP.



  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELCGXX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CGMX_EBELN  text
*      -->P_GS_CGMX_EBELP  text
*      <--P_GS_CGMX_CGBZ  text
*----------------------------------------------------------------------*
FORM selcgxx  USING   p_ebeln TYPE ebeln
                      p_ebelp  TYPE ebelp
                     CHANGING p_cgbz TYPE string.

  " 取采购项目 的备注 长文本信息
  CLEAR:p_cgbz.
  CONCATENATE p_ebeln p_ebelp INTO g_objname .

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'F01'
      language                = sy-langu
      name                    = g_objname
      object                  = 'EKPO'
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
      IF p_cgbz IS INITIAL.
        p_cgbz = wa_lines-tdline.
      ELSE.
        CONCATENATE p_cgbz   wa_lines-tdline INTO p_cgbz.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.
