REPORT zqm005_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160815
*& Request       :ED1K905127
*& Descriptions  : 产成品检验报告
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
TABLES:aufk,afpo,afko,mara .
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        aufnr  TYPE afpo-aufnr ,   "订单号
        objnr  TYPE aufk-objnr,    "对象号
        auart  TYPE  aufk-auart,     "订单类型
        matnr  TYPE afpo-matnr,         "抬头物料号
        maktx  TYPE makt-maktx,         "物料描述
        ktext  TYPE aufk-ktext,    "描述
        ddzt   TYPE string,     "订单状态
        matkl  TYPE mara-matkl,      "物料组
        psmng  TYPE afpo-psmng,      "订单数量
        bcrk   TYPE afpo-psmng,      "本次入库数量
        wemng  TYPE afpo-wemng,       "已入库数量
        "   ljrk   TYPE bdmng,       "累计入库数量
        charg  TYPE afpo-charg,   "批次
        amein  TYPE afpo-amein,   "单位
        lgort  TYPE afpo-lgort,   "数量
        kdauf  TYPE afpo-kdauf,   "销售订单号
        xmmc   TYPE string,       "项目名称
        zjtlts TYPE bdmng,       "组件投料套数
        rsnum  TYPE afko-rsnum,   "预留相关需求
        igmng  TYPE afko-igmng,   "报工数量
        gamng  TYPE afko-gamng,    "订单数量
        xs     TYPE i,        "箱数
        bz     TYPE string,       "备注
        zsel,
      END OF ty_data.


TYPES:BEGIN OF ty_matnr,
        werks TYPE werks_d,   "工厂
        matnr TYPE matnr,     "物料号
        maktx TYPE maktx,     "物料描述
        matkl TYPE matkl,     "物料组
      END OF ty_matnr .


TYPES:BEGIN OF ty_print,
        xh    TYPE  i ,           "序号
        aufnr TYPE afpo-aufnr ,   "订单号
        matnr TYPE afpo-matnr,         "抬头物料号
        maktx TYPE makt-maktx,         "物料描述
        ktext TYPE aufk-ktext,    "描述
        psmng TYPE afpo-psmng,      "订单数量
        bcrk  TYPE bdmng,      "本次入库数量
        amein TYPE afpo-amein,   "单位
        lgort TYPE afpo-lgort,   "库存地点
        kdauf TYPE afpo-kdauf,   "销售订单号
        xmmc  TYPE string,       "项目名称
        xs    TYPE i,        "箱数
        bz    TYPE string,       "备注
      END OF ty_print .

TYPES:BEGIN OF ty_resb,
        rsnum TYPE resb-rsnum,  "预留号
        rspos TYPE resb-rspos,
        rsart TYPE resb-rsart,
        xloek TYPE resb-xloek,  "删除标识
        kzear TYPE resb-kzear,  "最后发货
        bdmng TYPE resb-bdmng,  "需求数量
        enmng TYPE resb-enmng,  "提货数量
        ts    TYPE resb-enmng,   "套数
      END OF ty_resb .

TYPES:BEGIN OF ty_kdauf,
        kdauf TYPE kdauf,
        xmmc  TYPE string,

      END OF ty_kdauf .

TYPES:BEGIN OF ty_aufnr,
        aufnr TYPE aufnr,
      END OF ty_aufnr .

DATA:gt_data   TYPE TABLE OF ty_data,
     gt_data_1 TYPE TABLE OF ty_data,
     gs_data   TYPE  ty_data.

DATA:gt_print TYPE TABLE OF ty_print,
     gs_print TYPE ty_print.

DATA:lt_prt TYPE TABLE OF ty_print,
     ls_prt TYPE ty_print.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_aufnr TYPE TABLE OF ty_aufnr,
     gs_aufnr TYPE ty_aufnr.

DATA:gt_jest TYPE TABLE OF jest,
     gs_jest TYPE jest.

DATA:gt_tj02t TYPE TABLE OF tj02t,
     gs_tj02t TYPE tj02t.

DATA:gt_zqm005 TYPE TABLE OF zqm005,
     gs_zqm005 TYPE zqm005.     "打印产成品检验报告表

DATA:gt_resb TYPE TABLE OF ty_resb,
     gs_resb TYPE ty_resb.

DATA:gt_kdauf TYPE TABLE OF ty_kdauf,
     gs_kdauf TYPE ty_kdauf.


FIELD-SYMBOLS: <fs_data> TYPE ty_data .

DATA:g_xh  TYPE i .     "全局序号

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.


*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

*打印参数变量
DATA: control    TYPE ssfctrlop,
      ntotalline TYPE i,
      npageline  TYPE i VALUE 9,
      p_index    LIKE sy-tabix.
DATA: emptycount      TYPE i VALUE 0,  "空行数.
      ncurrline       TYPE i,      "中间变量
      job_output_info TYPE ssfcrescl.
DATA: g_name TYPE rs38l_fnam.
DATA:l_formname TYPE tdsfname  VALUE 'ZSFQM004'.
DATA l_line TYPE i. "统计打印的行进行补行
DATA g_line TYPE i. "设定换页行数
DATA name   TYPE char20. "打印人


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
PARAMETERS    : p_werks TYPE aufk-werks OBLIGATORY.
SELECT-OPTIONS:
                s_aufnr FOR aufk-aufnr ,"OBLIGATORY ,
                s_matnh FOR afpo-matnr,
                s_dispo FOR afko-dispo,
                s_fevor FOR afko-fevor,
                s_gstrp FOR afko-gstrp,
                s_gltrp FOR afko-gltrp.
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
FORM frm_get_data .
  SELECT a~aufnr a~objnr a~auart a~ktext
         b~matnr b~psmng b~wemng b~charg b~amein b~lgort b~kdauf
         c~rsnum c~igmng c~gamng
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM aufk  AS a
    INNER JOIN  afpo AS b
    ON a~aufnr = b~aufnr
    INNER JOIN afko AS c
    ON a~aufnr = c~aufnr
    WHERE a~werks EQ p_werks
       AND a~aufnr IN s_aufnr
       AND a~loekz NE 'X'
       AND c~dispo IN s_dispo
       AND c~fevor IN s_fevor
       AND c~gstrp IN s_gstrp
       AND c~gltrp IN s_gltrp
       AND b~matnr IN s_matnh    .
  SORT gt_data BY aufnr .

  CHECK gt_data IS NOT INITIAL .

  MOVE-CORRESPONDING  gt_data TO gt_kdauf .


  DELETE gt_kdauf  WHERE kdauf IS INITIAL .

  SORT gt_kdauf BY kdauf .

  DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf .

  gt_data_1 = gt_data.

  DELETE gt_data_1  WHERE kdauf IS NOT INITIAL.

  IF gt_data_1 IS NOT INITIAL.

    MOVE-CORRESPONDING gt_data_1  TO gt_aufnr.

    DELETE gt_aufnr WHERE aufnr IS INITIAL.

    SORT gt_aufnr BY aufnr .

    DELETE ADJACENT DUPLICATES FROM gt_aufnr COMPARING aufnr .

    LOOP AT gt_aufnr INTO gs_aufnr.
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_aufnr+0(7)
                                   BINARY SEARCH .
      IF sy-subrc EQ 0 .
        CLEAR:gs_kdauf .
        gs_kdauf-kdauf = gs_aufnr+0(7) .
        APPEND gs_kdauf TO gt_kdauf.
      ENDIF.

    ENDLOOP.

    SORT gt_kdauf BY kdauf .
    DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf.

  ENDIF.


  "查找排除已锁定、技术性完成的对象
  SELECT * INTO TABLE gt_jest
    FROM jest
    FOR ALL ENTRIES IN gt_data
    WHERE objnr  = gt_data-objnr
    AND    stat  NOT IN ('I0043','I0045')
    AND    inact = ''.
  SORT  gt_jest BY objnr .


  "查状态描述

  SELECT * INTO TABLE gt_tj02t
    FROM tj02t
    FOR ALL ENTRIES IN gt_jest
    WHERE istat = gt_jest-stat
    AND spras = sy-langu.
  SORT gt_tj02t BY istat .

  "读取物料描述

  SELECT a~matnr b~maktx c~matkl
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
    FROM marc  AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    INNER JOIN mara AS c
    ON a~matnr = c~matnr
    WHERE a~werks EQ p_werks
     AND b~spras = sy-langu.

  SORT gt_matnr BY matnr .

  SELECT rsnum rspos rsart xloek kzear bdmng enmng INTO TABLE gt_resb
    FROM resb
    FOR ALL ENTRIES  IN gt_data
    WHERE rsnum = gt_data-rsnum .
  DELETE gt_resb WHERE xloek EQ 'X' OR kzear EQ'X' .


  SORT gt_resb BY rsnum .

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
  DATA:t_m TYPE f .

  "先读取项目名称
  LOOP AT gt_kdauf INTO gs_kdauf .

    PERFORM selxmmc USING gs_kdauf-kdauf sy-langu  CHANGING gs_kdauf-xmmc .
    MODIFY gt_kdauf FROM gs_kdauf.

  ENDLOOP.

  LOOP AT gt_resb INTO gs_resb .
    "   * 方法二
    IF gs_resb-bdmng NE 0 .
      READ TABLE gt_data INTO gs_data WITH KEY rsnum = gs_resb-rsnum BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_resb-ts = gs_resb-enmng / gs_resb-bdmng *  gs_data-gamng .
      ENDIF.
    ENDIF.

    MODIFY gt_resb FROM  gs_resb .
  ENDLOOP.
  SORT gt_resb BY rsnum ASCENDING ts ASCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_resb COMPARING rsnum ts .
  LOOP AT gt_data ASSIGNING <fs_data> .
    p_index = sy-tabix .
    "读取状态
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = <fs_data>-objnr BINARY SEARCH .
    IF sy-subrc NE 0 .
      "排除TECO、LKD的订单
      DELETE gt_data INDEX p_index .

    ELSE.
      LOOP AT gt_jest INTO gs_jest WHERE objnr = <fs_data>-objnr .
        IF <fs_data>-ddzt IS INITIAL .
          READ TABLE gt_tj02t INTO gs_tj02t WITH KEY istat = gs_jest-stat BINARY SEARCH .
          IF sy-subrc EQ 0 .
            <fs_data>-ddzt = gs_tj02t-txt04.
          ENDIF.

        ELSE.
          READ TABLE gt_tj02t INTO gs_tj02t WITH KEY istat = gs_jest-stat BINARY SEARCH .
          IF sy-subrc EQ 0 .
            CONCATENATE <fs_data>-ddzt   '_'  gs_tj02t-txt04  INTO <fs_data>-ddzt.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

    "物料号
    IF <fs_data>-matnr IS NOT INITIAL .
      READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = <fs_data>-matnr BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-maktx = gs_matnr-maktx .
        <fs_data>-matkl = gs_matnr-matkl.

      ENDIF.

    ENDIF.

    "累计入库
    "   <fs_data>-ljrk = <fs_data>-wemng.
    "默认本次入库数量
    <fs_data>-bcrk = <fs_data>-psmng - <fs_data>-wemng .

    "项目名称
    IF <fs_data>-kdauf IS INITIAL .

      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = <fs_data>-aufnr+0(7)
                                       BINARY SEARCH.
      IF sy-subrc EQ 0 .

        <fs_data>-kdauf = <fs_data>-aufnr+0(7) .

      ENDIF.
    ENDIF.

    IF <fs_data>-kdauf IS  NOT INITIAL .

      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf = <fs_data>-kdauf
                                         BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-xmmc = gs_kdauf-xmmc .
      ENDIF.

    ENDIF.
    "组件投料套数

    READ TABLE gt_resb INTO gs_resb WITH KEY rsnum = <fs_data>-rsnum BINARY SEARCH .
    IF sy-subrc EQ 0 .
      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0       " 保留多少位小数
          input         = gs_resb-ts
          sign          = '+'     " + 向上取舍 - 向下取舍 （负数也一样）
        IMPORTING
          output        = gs_resb-ts     " 输出返回结果
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.
      IF gs_resb-ts > <fs_data>-gamng .
        <fs_data>-zjtlts = <fs_data>-gamng .
      ELSE.
        <fs_data>-zjtlts  = gs_resb-ts .
      ENDIF.
    ELSE.
      <fs_data>-zjtlts = <fs_data>-gamng .
    ENDIF.

  ENDLOOP.
ENDFORM.



FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  clear:p_xmmc.
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
    LOOP AT it_lines INTO wa_lines .
      IF p_xmmc IS INITIAL.
        p_xmmc = wa_lines-tdline.
      ELSE.
        CONCATENATE wa_lines-tdline p_xmmc INTO p_xmmc.
      ENDIF.
    ENDLOOP.

  ENDIF.
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

  CALL TRANSFORMATION zqm005_1_zh
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

  dname = 'ZQM005产成品检验报告.XLS'.
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
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '__________'
            ID 'WERKS' FIELD  p_werks.
  IF sy-subrc <> 0.
    MESSAGE e603(fco) WITH p_werks.
  ENDIF.
ENDFORM.
