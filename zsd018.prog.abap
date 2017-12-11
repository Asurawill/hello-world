*&---------------------------------------------------------------------*
*&程序名称/Program Name         :   ZSD018 .
*&程序描述/Program Des.         :   深圳金立翔出入库单打印
*&申请人/Applicant              :   HANDSSY .
*&申请日期/Date of App          :   2017.5.12
*&作者/Author                   :   张召 .
*&完成日期/Completion Date      :
*&---------------------------------------------------------------------*
*&摘要：
*&
*&---------------------------------------------------------------------*
*&变更记录：
*&---------------------------------------------------------------------*
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2017.5.12    张召                             初始开发
*&---------------------------------------------------------------------*

REPORT zsd018.
*---------------------------------------------------------------------*
* Type-pool                                                           *
*                                                                     *
*---------------------------------------------------------------------*
TYPE-POOLS: slis .                       " Globale Type

*---------------------------------------------------------------------*
* Declare tables                                                      *
*                                                                     *
*---------------------------------------------------------------------*
TABLES: likp, lips, vbak, ekko .

*---------------------------------------------------------------------*
* Types                                                               *
*                                                                     *
*---------------------------------------------------------------------*
******   创建内表结构    *****
TYPES: BEGIN OF ty_tab_1,

         vgbel  TYPE   lips-vgbel,          " 订单编号1 .
         vbeln2 TYPE   vbak-vbeln,          " 订单编号2 .
         ebeln  TYPE   ekko-ebeln,          " 订单编号3 .
         bstnk  TYPE   vbak-bstnk,          " 合同编号
*         bstKD  TYPE   vbak-bstKD,          " 合同编号
         name1  TYPE   kna1-name1,          " 客户名称
         ztext  TYPE   string,              " 项目名称
         vdatu  TYPE   vbak-vdatu,          " 计划交货时间1 .
         edatu  TYPE   vbep-edatu,          " 计划交货时间2 .
         eindt  TYPE   eket-eindt,          " 计划交货时间3 .
         vbeln  TYPE   likp-vbeln,          " 交货单编号  KEY *
         erdat  TYPE   likp-erdat,          " 交货单创建日期
         lfart  TYPE   likp-lfart,          " 交货单类型
         vtext  TYPE   tvlkt-vtext,         " 交货类型描述
         vstel  TYPE   likp-vstel,          " 装运点
         vtext1 TYPE   tvstt-vtext,         " 装运点描述
         vkorg1 TYPE   likp-vkorg,          " 销售组织1 .
         vkorg2 TYPE   vbak-vkorg,          " 销售组织2 .
         vkorg3 TYPE   ekko-ekorg,          " 采购组织3 .
         vtext2 TYPE   tvkot-vtext,         " 销售组织描述
         posnr1 TYPE   lips-posnr,          " 行项目标号1 .
         posnr2 TYPE   vbap-posnr,          " 行项目标号2 .
         posnr3 TYPE   ekpo-ebelp,          " 行项目标号3 .
         matnr1 TYPE   lips-matnr,          " 物料编码1 .
         matnr2 TYPE   vbap-matnr,          " 物料编码2 .
         matnr3 TYPE   ekpo-matnr,          " 物料编码3 .
         maktx  TYPE   makt-maktx,          " 物料名称
         lfimg  TYPE   lips-lfimg,          " 数量1 .
         kwmeng TYPE   vbap-kwmeng,         " 数量2 .
         menge  TYPE   ekpo-menge,          " 数量3 .
         meins1 TYPE   lips-meins,          " 单位1 .
         meins2 TYPE   vbap-meins,          " 单位2 .
         meins3 TYPE   ekpo-meins,          " 单位3 .
         charg1 TYPE   lips-charg,          " 批次1 .
         charg2 TYPE   vbap-charg,          " 批次2 .
         werks  TYPE   lips-werks,          " 工厂
         name2  TYPE   t001w-name1,         " 工厂名称
         lgort  TYPE   lips-lgort,          " 库位
         lgobe  TYPE   t001l-lgobe,         " 库位描述
         kunnr  TYPE   likp-kunnr,          "客户编码
         kdmat  TYPE   vbap-kdmat,           "备注
         box    TYPE   c,           " box盒子
       END OF ty_tab_1.


TYPES: BEGIN OF ty_lips_likp,
         vgbel  TYPE   lips-vgbel,          " 订单编号1 .
         vbeln  TYPE   likp-vbeln,          " 交货单编号  KEY *
         erdat  TYPE   likp-erdat,          " 交货单创建日期
         lfart  TYPE   likp-lfart,          " 交货单类型
         vstel  TYPE   likp-vstel,          " 装运点
         vkorg1 TYPE   likp-vkorg,          " 销售组织1 .
         kunnr  TYPE   likp-kunnr,
         posnr1 TYPE   lips-posnr,          " 行项目标号1 .
         matnr1 TYPE   lips-matnr,          " 物料编码1 .
         lfimg  TYPE   lips-lfimg,          " 数量1 .
         meins1 TYPE   lips-meins,          " 单位1 .
         charg  TYPE   lips-charg,          " 批次1 .
         werks  TYPE   lips-werks,          " 工厂
         lgort  TYPE   lips-lgort,          " 库存地点
         vgpos  TYPE   lips-vgpos,          "订单项目

       END OF ty_lips_likp.


TYPES: BEGIN OF ty_vbak_kna1 ,
         vbeln1 TYPE   vbak-vbeln,          "* 取值所需关联字段

         bstnk  TYPE   vbak-bstnk,          " 合同编号
         vdatu  TYPE   vbak-vdatu,          " 计划交货时间
*         NAME1  TYPE   KNA1-NAME1,          " 客户名称

         kunnr  TYPE   vbak-kunnr,          "* 取值所需关联字段
         posnr  TYPE   vbap-posnr,          "行项目
         kdmat  TYPE   vbap-kdmat,          "备注
       END OF ty_vbak_kna1.

TYPES: BEGIN OF ty_vbak_kna2 ,
         kunnr TYPE   kna1-kunnr,          "* 取值所需关联字段
         name1 TYPE   kna1-name1,          " 客户名称

       END OF ty_vbak_kna2.


TYPES: BEGIN OF ty_tvlkt,      " SPRAS 语言
         lfart TYPE   tvlkt-lfart,         " 交货单类型 *
         vtext TYPE   tvlkt-vtext,         " 交货类型描述

       END OF ty_tvlkt.


TYPES: BEGIN OF ty_tvstt,      " SPRAS 语言
         vstel  TYPE   tvstt-vstel,         " 装运点 *
         vtext1 TYPE   tvstt-vtext,         " 装运点描述

       END OF ty_tvstt.


TYPES: BEGIN OF ty_tvkot,      " SPRAS 语言
         vkorg  TYPE   tvkot-vkorg,         " 销售组织 *
         vtext2 TYPE   tvkot-vtext,         " 销售组织描述

       END OF ty_tvkot.


TYPES: BEGIN OF ty_makt,      " SPRAS 语言
         matnr1 TYPE   makt-matnr,          " 物料编码 *
         maktx  TYPE   makt-maktx,          " 物料名称

       END OF ty_makt.


TYPES: BEGIN OF ty_t001w,
         werks TYPE   t001w-werks,         " 工厂 *
         name2 TYPE   t001w-name1,         " 工厂名称

       END OF ty_t001w.


TYPES: BEGIN OF ty_t001l,
         werks TYPE   t001l-werks,         " 工厂 *
         lgort TYPE   t001l-lgort,         " 库存地点 *
         lgobe TYPE   t001l-lgobe,         " 库位描述

       END OF ty_t001l.


*****------------  业务二  ----------------*****

TYPES: BEGIN OF ty_vbak_vbap,

         vbeln2 TYPE   vbak-vbeln,          " 订单编号2 . *
*         bstKD  TYPE   vbak-bstKD,          " 合同编号
         bstnk  TYPE  vbak-bstnk,
         vkorg2 TYPE   vbak-vkorg,          " 销售组织2 .
         posnr2 TYPE   vbap-posnr,          " 行项目标号2 .
         matnr2 TYPE   vbap-matnr,          " 物料编码2 . *
         kwmeng TYPE   vbap-kwmeng,         " 数量2 .
         meins2 TYPE   vbap-meins,          " 单位2 .
         charg  TYPE   vbap-charg,          " 批次2 .
         kdmat  TYPE   vbap-kdmat,            "备注
*         ETDAT  TYPE   VBAP-ETDAT,          "计划交货时间
         kunnr  TYPE   vbak-kunnr,          "* 取值所需关联字段
         werks  TYPE  vbap-werks,
       END OF ty_vbak_vbap.

TYPES: BEGIN OF ty_vbep,
         vbeln TYPE   vbep-vbeln,          " 订单编号2 . *
         posnr TYPE   vbep-posnr,          " 行项目 *对应取值 .
         edatu TYPE   vbep-edatu,          " 计划交货时间2 .

       END OF ty_vbep.

TYPES: BEGIN OF ty_lips_likp_2,
         vgbel TYPE   lips-vgbel,          "* 关联字段 VBAK-VBELN=LIPS-VGBEL

         vbeln TYPE   likp-vbeln,          " 交货单编号  KEY *
         erdat TYPE   likp-erdat,          " 交货单创建日期
         lfart TYPE   likp-lfart,          " 交货单类型
         vstel TYPE   likp-vstel,          " 装运点
         kunnr TYPE   likp-kunnr,
         werks TYPE   lips-werks,          " 工厂
         lgort TYPE   lips-lgort,          " 库存地点
         vgpos TYPE   lips-vgpos,          " 行项目 *对应取值
         charg TYPE   lips-charg,

       END OF ty_lips_likp_2.

TYPES: BEGIN OF ty_kna1 ,
         name1 TYPE   kna1-name1,          " 客户名称
         kunnr TYPE   kna1-kunnr,          "* 取值所需关联字段

       END OF ty_kna1.

*****------------  业务三  ----------------*****

TYPES: BEGIN OF ty_ekko_ekpo,
         ebeln  TYPE   ekko-ebeln,          " 订单编号3 .
         vkorg3 TYPE   ekko-ekorg,          " 采购组织3 .
         posnr3 TYPE   ekpo-ebelp,          " 行项目标号3 .
         matnr3 TYPE   ekpo-matnr,          " 物料编码3 .
         menge  TYPE   ekpo-menge,          " 数量3 .
         meins3 TYPE   ekpo-meins,          " 单位3 .

         kunnr  TYPE   ekpo-kunnr,          "* 取值所需关联字段

       END OF ty_ekko_ekpo.

TYPES: BEGIN OF ty_eket,
         ebeln TYPE   eket-ebeln,          " 订单编号3 . *
         ebelp TYPE   eket-ebelp,          " 行项目 *对应取值 .
         eindt TYPE   eket-eindt,          " 计划交货时间3 .

       END OF ty_eket.

TYPES: BEGIN OF ty_vbap,
         vbeln TYPE vbap-vbeln,
         kdmat TYPE vbap-kdmat,
       END OF ty_vbap.
DATA: gt_vbap TYPE TABLE OF ty_vbap,
      gs_vbap LIKE  LINE OF gt_vbap.


*---------------------------------------------------------------------*
* Defining a structured                                               *
*                                                                     *
*---------------------------------------------------------------------*
*****   定义内表    *****
DATA: gt_tab_1     TYPE TABLE OF ty_tab_1,
      gs_tab_1     LIKE LINE OF gt_tab_1,

      gt_tab_sf    TYPE TABLE OF ty_tab_1 ,   " 传入SMARTFORMS数据时所用临时表
      gs_tab_sf    LIKE LINE OF gt_tab_sf,

      gt_tab_2     TYPE TABLE OF ty_tab_1 ,   " 传入SMARTFORMS数据时所用临时表
      gs_tab_2     LIKE LINE OF gt_tab_2,

      gt_tab_3     TYPE TABLE OF ty_tab_1 ,   " 传入SMARTFORMS数据时所用临时表
      gs_tab_3     LIKE LINE OF gt_tab_3,

      gt_lips_likp TYPE TABLE OF ty_lips_likp,
      gs_lips_likp LIKE LINE OF gt_lips_likp,

      gt_vbak_kna1 TYPE TABLE OF ty_vbak_kna1,
      gs_vbak_kna1 LIKE LINE OF gt_vbak_kna1,

      gt_vbak_kna2 TYPE TABLE OF ty_vbak_kna2,
      gs_vbak_kna2 LIKE LINE OF gt_vbak_kna2,

      gt_tvlkt     TYPE TABLE OF ty_tvlkt,
      gs_tvlkt     LIKE LINE OF gt_tvlkt,

      gt_tvstt     TYPE TABLE OF ty_tvstt,
      gs_tvstt     LIKE LINE OF gt_tvstt,

      gt_tvkot     TYPE TABLE OF ty_tvkot,
      gs_tvkot     LIKE LINE OF gt_tvkot,

      gt_makt      TYPE TABLE OF ty_makt,
      gs_makt      LIKE LINE OF gt_makt,

      gt_t001w     TYPE TABLE OF ty_t001w,
      gs_t001w     LIKE LINE OF gt_t001w,

      gt_t001l     TYPE TABLE OF ty_t001l,
      gs_t001l     LIKE LINE OF gt_t001l.

*     业务二所需内表
DATA: gt_lips_likp_2 TYPE TABLE OF ty_lips_likp_2,
      gs_lips_likp_2 LIKE LINE OF gt_lips_likp_2,

      gt_vbep        TYPE TABLE OF ty_vbep,
      gs_vbep        LIKE LINE OF gt_vbep,

      gt_kna1        TYPE TABLE OF ty_kna1,
      gs_kna1        LIKE LINE OF gt_kna1,

      gt_vbak_vbap   TYPE TABLE OF ty_vbak_vbap,
      gs_vbak_vbap   LIKE LINE OF gt_vbak_vbap,
      gt_vbak_vbap_2 TYPE TABLE OF ty_vbak_vbap,
      gs_vbak_vbap_2 LIKE LINE OF gt_vbak_vbap_2.

*     业务三所需内表
DATA: gt_ekko_ekpo TYPE TABLE OF ty_ekko_ekpo,
      gs_ekko_ekpo LIKE LINE OF gt_ekko_ekpo,

      gt_eket      TYPE TABLE OF ty_eket,
      gs_eket      LIKE LINE OF gt_eket.


*     销售订单所需内表
DATA: gt_lips_likp_4 TYPE TABLE OF ty_lips_likp_2,
      gs_lips_likp_4 LIKE LINE OF gt_lips_likp_4,

      gt_vbep_4      TYPE TABLE OF ty_vbep,
      gs_vbep_4      LIKE LINE OF gt_vbep_4,

      gt_kna1_4      TYPE TABLE OF ty_kna1,
      gs_kna1_4      LIKE LINE OF gt_kna1_4,

      gt_vbak_vbap_4 TYPE TABLE OF ty_vbak_vbap,
      gs_vbak_vbap_4 LIKE LINE OF gt_vbak_vbap_4.

*****   项目名称长文本   *****
DATA:gt_text TYPE TABLE OF tline,
     gs_text LIKE LINE OF gt_text.

*****  项目名称的参考类型   *****
DATA l_name TYPE thead-tdrefname.


*   ZSCREEN 中功能码返回值
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE  sy-ucomm.


*****  屏幕中业务选择   *****
DATA: type_1 TYPE c,
      type_2 TYPE c,
      type_3 TYPE c.

*****  业务控制打印参数   *****
DATA gc_type TYPE i VALUE 1.


*定义打印属性
DATA: fm_name            TYPE rs38l_fnam,
      output             TYPE ssfcompop,
      control_parameters TYPE ssfctrlop,
      lw_ssfcrescl       TYPE ssfcrescl,
      option             TYPE ssfcrescl.

*分页页数
DATA gc_num TYPE i VALUE 25.
DATA gc_count TYPE i.

DATA: gt_tab_print LIKE TABLE OF zsd018_item, " 定义传入SMARTFORMS表中的内表
      gs_tab_print LIKE LINE OF gt_tab_print,
      zzs_tab      LIKE zsd018_head.         " 表抬头数据

*---------------------------------------------------------------------*
* ALV                                                                 *
*                                                                     *
*---------------------------------------------------------------------*
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,                "ALV显示字段內表
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      g_repid     LIKE sy-repid VALUE sy-repid,
      lt_sort     TYPE slis_t_sortinfo_alv.

*---------------------------------------------------------------------*
* Macro                                                               *
*                                                                     *
*---------------------------------------------------------------------*
DEFINE catalog  .
  CLEAR GS_FIELDCAT.

  &1 = &1 + 1 .

  GS_FIELDCAT-COL_POS = &1.         " 行数
  GS_FIELDCAT-FIELDNAME = &2.       " 字段的名字
  GS_FIELDCAT-SELTEXT_S = &3.       " 字段的文本描述
  GS_FIELDCAT-KEY = &4.             " 是否为KEY值   X .
  GS_FIELDCAT-NO_ZERO = &5.         " 删除前导“0”  X .
  GS_FIELDCAT-JUST = &6.            " 字段输出居中   C .
  GS_FIELDCAT-EDIT  = &7.           " 可编辑列   X .
  GS_FIELDCAT-REF_TABNAME = &8.       " 参考表
  GS_FIELDCAT-REF_FIELDNAME = &9.       " 参考字段
*  GS_FIELDCAT-ICON = &.            " 是否激活图标  X .
*  GS_FIELDCAT-DO_SUM = &.
  IF GS_FIELDCAT-FIELDNAME = 'menge'
    OR GS_FIELDCAT-FIELDNAME = 'lfimg'
    OR GS_FIELDCAT-FIELDNAME = 'kwmeng'.
   GS_FIELDCAT-datatype      = 'QUAN' .  " 指定数据类型
   GS_FIELDCAT-inttype         = 'C' .       "这个是指定字段的类型为C
  ENDIF.
  APPEND  GS_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.



*---------------------------------------------------------------------*
* Selection-screen                                                    *
*                                                                     *
*---------------------------------------------------------------------*
* 按照选择业务不同，显示不同的选择屏幕                                                  *
*                                                                     *
*---------------------------------------------------------------------*
PARAMETERS: type1 RADIOBUTTON GROUP grp1 USER-COMMAND flag DEFAULT 'X', " 项目，产品销售（包含公司间）
            type2 RADIOBUTTON GROUP grp1 ,                              " 租赁，借料(非关联方)
            type3 RADIOBUTTON GROUP grp1 ,                              " 借料(关联方)
            type4 RADIOBUTTON GROUP grp1 .                              " 销售订单



SELECTION-SCREEN BEGIN OF BLOCK blk_01 WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_vstel FOR likp-vstel MODIF ID bl1,             " 装运点
               s_vkorg FOR likp-vkorg MODIF ID bl1,             " 销售组织
               s_vbeln FOR likp-vbeln MODIF ID bl1,             " 交货单编号
               s_wadat FOR likp-wadat MODIF ID bl1,             " 交货日期
               s_lfart FOR likp-lfart MODIF ID bl1,             " 交货单类型
               s_vgbel FOR lips-vgbel MODIF ID bl1.             " 订单编号
SELECTION-SCREEN END OF BLOCK blk_01.

SELECTION-SCREEN BEGIN OF BLOCK blk_02 WITH FRAME TITLE text-002.
SELECT-OPTIONS:s_vkorg2 FOR vbak-vkorg MODIF ID bl2,            " 销售组织
               s_auart FOR vbak-auart MODIF ID bl2,             " 订单类型
               s_vbeln2 FOR vbak-vbeln MODIF ID bl2,            " 订单编号
               s_bstnk FOR vbak-bstnk MODIF ID bl2.             " 合同编号
SELECTION-SCREEN END OF BLOCK blk_02.

SELECTION-SCREEN BEGIN OF BLOCK blk_03 WITH FRAME TITLE text-003.
SELECT-OPTIONS:s_ekorg FOR ekko-ekorg MODIF ID bl3,             " 采购组织
               s_bsart FOR ekko-bsart MODIF ID bl3,             " 订单类型
               s_ebeln FOR ekko-ebeln MODIF ID bl3.             " 订单编号
SELECTION-SCREEN END OF BLOCK blk_03.

*-----------销售订单--------------
SELECTION-SCREEN BEGIN OF BLOCK blk_04 WITH FRAME TITLE text-004.
SELECT-OPTIONS:s_vkorg4 FOR likp-vkorg MODIF ID bl4,            " 销售组织
               s_auart4 FOR vbak-auart MODIF ID bl4,             " 订单类型
               s_vbeln4 FOR lips-vgbel MODIF ID bl4,            " 订单编号
               s_bstnk4 FOR vbak-bstnk MODIF ID bl4.             " 合同编号
SELECTION-SCREEN END OF BLOCK blk_04.
*---------------------------------------------------------------------*
* Initialization                                                      *
*                                                                     *
*---------------------------------------------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'BL1'.
        IF type1 IS NOT INITIAL .  " 项目，产品销售（包含公司间）
          screen-active = '1'."显示
        ELSE.
          screen-active = '0'."隐藏
        ENDIF.

      WHEN 'BL2'.
        IF  type2 IS NOT INITIAL .  " 租赁，借料(非关联方)
          screen-active = '1'."显示
        ELSE.
          screen-active = '0'."隐藏
        ENDIF.

      WHEN 'BL3'.
        IF  type3 IS NOT INITIAL .  " 借料(关联方)
          screen-active = '1'."显示
        ELSE.
          screen-active = '0'."隐藏
        ENDIF.
*--------------销售订单------------
      WHEN 'BL4'.
        IF  type4 IS NOT INITIAL .  " 租赁，借料(非关联方)
          screen-active = '1'."显示
        ELSE.
          screen-active = '0'."隐藏
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*---------------------------------------------------------------------*
* At selection-screen                                                 *
*                                                                     *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*---------------------------------------------------------------------*
* Start-of-selection                                                  *
*                                                                     *
*---------------------------------------------------------------------*
START-OF-SELECTION.

*&--代码添加 BY HANDYBY 23.08.2017 16:33:24  BEGIN
* 权限对象检查
  PERFORM frm_auth_check .
*&--代码添加 BY HANDYBY 23.08.2017 16:33:24  END

* 项目，产品销售（包含公司间）
  IF type1 = 'X'.
*    IF s_vkorg IS INITIAL .  " 项目，产品销售（包含公司间）
*      MESSAGE '请输入必输字段“销售组织”' TYPE 'S' DISPLAY LIKE 'E'.   " 必输字段判断
*    ELSE.
    PERFORM frm_seldata_1.     " 取表数据函数
    PERFORM frm_write .   " ALV输出打印函数
*    ENDIF.
  ELSEIF type2 = 'X'. "  租赁，借料(非关联方)
*    IF s_vkorg2 IS INITIAL .  " 项目，产品销售（包含公司间）
*      MESSAGE '请输入必输字段“销售组织”' TYPE 'S' DISPLAY LIKE 'E'.   " 必输字段判断
*    ELSE.
    PERFORM frm_seldata_2.     " 取表数据函数
    PERFORM frm_write .   " ALV输出打印函数
*    ENDIF.
  ELSEIF type3 = 'X'.  " 借料(关联方)
*    IF s_ekorg IS INITIAL .  " 项目，产品销售（包含公司间）
*      MESSAGE '请输入必输字段“采购组织”' TYPE 'S' DISPLAY LIKE 'E'.   " 必输字段判断
*    ELSE.
    PERFORM frm_seldata_3.     " 取表数据函数
    PERFORM frm_write .   " ALV输出打印函数
*    ENDIF.
*-----------销售订单-------------
  ELSEIF type4 = 'X'.
*    IF s_vkorg4 IS INITIAL .  " 项目，产品销售（包含公司间）
*      MESSAGE '请输入必输字段“销售组织”' TYPE 'S' DISPLAY LIKE 'E'.   " 必输字段判断
*    ELSE.
    PERFORM frm_seldata_4.     " 取表数据函数
    PERFORM frm_write .   " ALV输出打印函数
*    ENDIF.

  ENDIF.



*---------------------------------------------------------------------*
*   End-of-selection                                                  *
*                                                                     *
*---------------------------------------------------------------------*
END-OF-SELECTION.



*-------------- 取表数据函数---------------*

*&---------------------------------------------------------------------*
*&      FoRM  FRM_SELDATA_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_seldata_1.       " 业务一
*----------------  定义局部变量  --------------------------*
  DATA:lt_lips_likp TYPE TABLE OF ty_lips_likp,
       lv_lips_likp LIKE LINE OF lt_lips_likp.


*****   LIPS 和 LIKP 表链接查找    *****
  SELECT
    lips~vgbel           " 订单编号
    likp~vbeln           " 交货单编号  KEY *
    likp~erdat           " 交货单创建日期
    likp~lfart           " 交货单类型
    likp~vstel           " 装运点
    likp~vkorg           " 销售组织1 .
    likp~kunnr

    lips~posnr           " 行项目标号1 .
    lips~matnr           " 物料编码1 .
    lips~lfimg           " 数量1 .
    lips~meins           " 单位1 .
    lips~charg           " 批次1 .
    lips~werks           " 工厂
    lips~lgort           " 库位
    LIPS~VGPOS           " 订单行项目
    INTO TABLE gt_lips_likp
    FROM likp INNER JOIN lips
    ON  likp~vbeln = lips~vbeln
    WHERE  likp~vstel IN s_vstel        " 装运点
    AND    likp~vkorg IN s_vkorg        " 销售组织
    AND    likp~vbeln IN s_vbeln        " 交货单编号
    AND    likp~wadat IN s_wadat        " 交货日期
    AND    likp~lfart IN s_lfart        " 交货单类型
    AND    lips~vgbel IN s_vgbel .      " 订单编号
  SORT gt_lips_likp BY vgbel. " 排序方便二分法查找


  IF gt_lips_likp IS NOT INITIAL.
    APPEND LINES OF gt_lips_likp TO lt_lips_likp.
    SORT lt_lips_likp BY vgbel matnr1 lfart vstel vkorg1 werks lgort .
    DELETE ADJACENT DUPLICATES FROM lt_lips_likp COMPARING vgbel matnr1 lfart vstel vkorg1 werks lgort .

    SELECT
      vbak~vbeln           "* 取值所需关联字段
      vbak~bstnk           " 合同编号
      vbak~vdatu           " 计划交货时间
*      kna1~name1           " 客户名称
      vbak~kunnr           "* 取值所需关联字段
      vbap~posnr
      vbap~kdmat            "备注
    INTO TABLE gt_vbak_kna1
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
*    FROM vbak INNER JOIN kna1
*      ON kna1~kunnr = vbak~kunnr
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE vbak~vbeln = lt_lips_likp-vgbel.
    SORT gt_vbak_kna1 BY vbeln1 posnr.
    DELETE ADJACENT DUPLICATES FROM gt_vbak_kna1 COMPARING vbeln1 posnr.

    SELECT
      kna1~kunnr
      kna1~name1
      INTO TABLE gt_vbak_kna2
      FROM kna1
      FOR ALL ENTRIES IN lt_lips_likp
      WHERE kna1~kunnr = lt_lips_likp-kunnr.
    SORT gt_vbak_kna2 BY kunnr.

    SELECT
      tvlkt~lfart          " 交货单类型 *
      tvlkt~vtext          " 交货类型描述
    INTO CORRESPONDING FIELDS OF TABLE  gt_tvlkt
    FROM tvlkt
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE tvlkt~lfart = lt_lips_likp-lfart   " 交货单类型 *
      AND spras = '1'.        " 语言
    SORT gt_tvlkt BY lfart.
    DELETE ADJACENT DUPLICATES FROM gt_tvlkt COMPARING lfart .


    SELECT
      tvstt~vstel          " 装运点  *
      tvstt~vtext          " 装运点描述
    INTO TABLE gt_tvstt
    FROM tvstt
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE tvstt~vstel = lt_lips_likp-vstel   " 装运点  *
      AND spras = '1'.       " 语言
    SORT gt_tvstt BY vstel.
    DELETE ADJACENT DUPLICATES FROM gt_tvstt COMPARING vstel .


    SELECT
      tvkot~vkorg          " 销售组织 *
      tvkot~vtext          " 销售组织描述
    INTO TABLE gt_tvkot
    FROM tvkot
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE tvkot~vkorg = lt_lips_likp-vkorg1    " 销售组织 *
      AND spras = '1'.       " 语言
    SORT gt_tvkot BY vkorg.
    DELETE ADJACENT DUPLICATES FROM gt_tvkot COMPARING vkorg .


    SELECT
      makt~matnr           " 物料编码  *
      makt~maktx           " 物料名称
    INTO TABLE gt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE makt~matnr = lt_lips_likp-matnr1    " 物料编码  *
      AND spras = '1'.
    SORT gt_makt BY matnr1.
    DELETE ADJACENT DUPLICATES FROM gt_makt COMPARING matnr1 .


    SELECT
      t001w~werks          " 工厂 *
      t001w~name1          " 工厂名称
    INTO TABLE gt_t001w
    FROM t001w
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE t001w~werks = lt_lips_likp-werks.       " 工厂
    SORT gt_t001w BY werks.
    DELETE ADJACENT DUPLICATES FROM gt_t001w COMPARING werks .


    SELECT
      t001l~werks          " 工厂 *
      t001l~lgort          " 库存地点 *
      t001l~lgobe          " 库位描述
    INTO CORRESPONDING FIELDS OF TABLE gt_t001l
    FROM t001l
    FOR ALL ENTRIES IN lt_lips_likp
    WHERE t001l~werks = lt_lips_likp-werks       " 工厂
      AND t001l~lgort = lt_lips_likp-lgort.      " 库存地点
    SORT gt_t001l BY werks lgort.
    DELETE ADJACENT DUPLICATES FROM gt_t001l COMPARING werks lgort .

  ENDIF.


  LOOP AT gt_lips_likp INTO gs_lips_likp.
    gs_tab_1-vgbel  = gs_lips_likp-vgbel .          " 订单编号
    gs_tab_1-vbeln  = gs_lips_likp-vbeln .          " 交货单编号  KEY *
    gs_tab_1-erdat  = gs_lips_likp-erdat .          " 交货单创建日期
    gs_tab_1-lfart  = gs_lips_likp-lfart .          " 交货单类型
    gs_tab_1-vstel  = gs_lips_likp-vstel .          " 装运点
    gs_tab_1-vkorg1 = gs_lips_likp-vkorg1.          " 销售组织1 .
    gs_tab_1-posnr1 = gs_lips_likp-posnr1.          " 行项目标号1 .
    gs_tab_1-matnr1 = gs_lips_likp-matnr1.          " 物料编码1 .
    gs_tab_1-lfimg  = gs_lips_likp-lfimg .          " 数量1 .
    gs_tab_1-meins1 = gs_lips_likp-meins1.          " 单位1 .
    gs_tab_1-charg1 = gs_lips_likp-charg.          " 批次1 .
    gs_tab_1-werks  = gs_lips_likp-werks .          " 工厂
    gs_tab_1-lgort  = gs_lips_likp-lgort .          " 库存地点
    gs_tab_1-kunnr  = gs_lips_likp-kunnr .          " 客户编码
    READ TABLE gt_vbak_kna1  INTO gs_vbak_kna1 WITH KEY vbeln1 = gs_lips_likp-vgbel    " 订单编号
                                                        posnr  = gs_lips_likp-vgpos
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-bstnk = gs_vbak_kna1-bstnk.          " 合同编号
      gs_tab_1-vdatu = gs_vbak_kna1-vdatu.          " 计划交货时间
      gs_tab_1-kdmat = gs_vbak_kna1-kdmat.          " 备注
*      gs_tab_1-name1 = gs_vbak_kna1-name1.          " 客户名称
    ENDIF.

    READ TABLE gt_vbak_kna2  INTO gs_vbak_kna2 WITH KEY kunnr = gs_lips_likp-kunnr    " 订单编号
                                                           BINARY SEARCH.
    IF sy-subrc = 0.
*      gs_tab_1-bstnk = gs_vbak_kna1-bstnk.          " 合同编号
*      gs_tab_1-vdatu = gs_vbak_kna1-vdatu.          " 计划交货时间
      gs_tab_1-name1 = gs_vbak_kna2-name1.          " 客户名称
    ENDIF.

    READ TABLE gt_tvlkt  INTO gs_tvlkt WITH KEY lfart = gs_lips_likp-lfart    " 交货单类型
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext = gs_tvlkt-vtext.         " 交货类型描述
    ENDIF.
    READ TABLE gt_tvstt  INTO gs_tvstt WITH KEY vstel = gs_lips_likp-vstel    " 装运点
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext1 = gs_tvstt-vtext1.        " 装运点描述
    ENDIF.
    READ TABLE gt_tvkot  INTO gs_tvkot WITH KEY vkorg = gs_lips_likp-vkorg1    " 销售组织1
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext2 = gs_tvkot-vtext2.         " 销售组织描述
    ENDIF.
    READ TABLE gt_makt  INTO gs_makt WITH KEY matnr1 = gs_lips_likp-matnr1    " 物料编码
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-maktx = gs_makt-maktx.           " 物料名称
    ENDIF.
    READ TABLE gt_t001w  INTO gs_t001w WITH KEY werks = gs_lips_likp-werks    " 工厂
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name2 = gs_t001w-name2.          " 工厂名称
    ENDIF.
    READ TABLE gt_t001l  INTO gs_t001l WITH KEY werks = gs_lips_likp-werks    " 工厂
                                                lgort = gs_lips_likp-lgort     " 库存地点
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-lgobe = gs_t001l-lgobe.          " 库位描述
    ENDIF.

*****  采购订单号表头长文本   *****
    l_name  = gs_lips_likp-vgbel.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z001'
        language                = '1'
        name                    = l_name
        object                  = 'VBBK'
      TABLES
        lines                   = gt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text IS NOT INITIAL.
      LOOP AT gt_text INTO gs_text.
*        CONCATENATE LINES OF gt_text INTO gs_tab_1-ztext." SEPARATED BY '&'.
*        gs_tab_1-ztext = gs_text-tdline.
        CONCATENATE gs_tab_1-ztext gs_text-tdline  INTO gs_tab_1-ztext.
      ENDLOOP.
    ENDIF.

    APPEND gs_tab_1 TO gt_tab_1.
*    DELETE gt_tab_1 WHERE bstnk EQ '' OR
    CLEAR:  gs_tab_1,
            gs_vbak_kna1,
            gs_tvlkt,
            gs_tvstt,
            gs_tvkot,
            gs_makt,
            gs_t001w,
            gs_t001l,
            gt_text,
            gs_lips_likp.

  ENDLOOP.


ENDFORM.                    " FRM_SELDATA_1                           *


*&---------------------------------------------------------------------*
*&      Form  FRM_SELDATA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_seldata_2 .       " 业务二
*----------------  定义局部变量  --------------------------
  DATA:lt_lips_likp_2 TYPE TABLE OF ty_lips_likp_2,
       lv_lips_likp_2 LIKE LINE OF lt_lips_likp_2,

       lt_vbak_vbap   TYPE TABLE OF ty_vbak_vbap,
       lv_vbak_vbap   LIKE LINE OF lt_vbak_vbap,

       lt_vbak_vbap_2 TYPE TABLE OF ty_vbak_vbap,
       ls_vbak_vbap_2 LIKE LINE OF lt_vbak_vbap_2.


*****   LIKP 和 LIPS 表链接查找    *****


  SELECT
    vbak~vbeln           " 订单编号2 . *
    vbak~bstnk           " 合同编号
    vbak~vkorg           " 销售组织2 .
    vbap~posnr           " 行项目标号2 .
    vbap~matnr           " 物料编码2 . *
    vbap~kwmeng          " 数量2 .
    vbap~meins           " 单位2 .
    vbap~charg           " 批次2 .
    vbap~kdmat           "备注
    vbak~kunnr           "* 取值所需关联字段
*      VBAP~ETDAT
    vbap~werks
    INTO TABLE gt_vbak_vbap
    FROM vbak INNER JOIN vbap
    ON  vbak~vbeln = vbap~vbeln  " 订单编号2 . *
    WHERE vbak~vkorg IN s_vkorg2       " 销售组织
    AND   vbak~auart IN s_auart        " 订单类型
    AND   vbak~vbeln IN s_vbeln2       " 订单编号
    AND   vbak~bstnk IN s_bstnk       " 合同编号
   .
*    SORT gt_vbak_vbap BY vbeln2 posnr2. " 排序方便二分法查找

  SORT gt_vbak_vbap BY vbeln2 posnr2. " 排序方便二分法查找



  IF gt_vbak_vbap IS NOT INITIAL.
    APPEND LINES OF gt_vbak_vbap TO lt_vbak_vbap_2.
*    SORT lt_vbak_vbap_2 BY  vgpos lfart vstel werks lgort.
*    DELETE ADJACENT DUPLICATES FROM lt_vbak_vbap_2 COMPARING  vgpos lfart vstel werks lgort.

    SELECT
      lips~vgbel           "* 关联字段 VBAK-VBELN=LIPS-VGBEL
      likp~vbeln           " 交货单编号  KEY *
      likp~erdat           " 交货单创建日期
      likp~lfart           " 交货单类型
      likp~vstel           " 装运点
      likp~kunnr
      lips~werks           " 工厂
      lips~lgort           " 库存地点
      lips~vgpos           " 行项目 *对应取值
      INTO CORRESPONDING FIELDS OF TABLE gt_lips_likp_2
      FROM likp INNER JOIN lips
      ON  likp~vbeln = lips~vbeln    " 交货单编号  KEY *
      FOR ALL ENTRIES IN lt_vbak_vbap_2

        WHERE lips~vgbel = lt_vbak_vbap_2-vbeln2   " 订单编号2 . *
        AND   lips~vgpos = lt_vbak_vbap_2-posnr2.  " 订单编号2 . *
*      and lips~vgbel IN s_vbeln2        " 订单编号
*    AND   likp~vkorg IN s_vkorg2.       " 销售组织
    SORT gt_lips_likp_2 BY vgbel vgpos lfart vstel werks lgort. " 排序方便二分法查找


    IF gt_vbak_vbap IS NOT INITIAL.
      APPEND LINES OF gt_vbak_vbap TO lt_vbak_vbap.
      SORT lt_vbak_vbap BY vbeln2 posnr2 vkorg2 matnr2 kunnr .
      DELETE ADJACENT DUPLICATES FROM lt_vbak_vbap COMPARING vbeln2 posnr2 vkorg2 matnr2 kunnr .

      SELECT
        vbep~vbeln           " 订单编号2 . *
        vbep~posnr           " 行项目 .
        vbep~edatu           " 计划交货时间2 .
        INTO CORRESPONDING FIELDS OF TABLE gt_vbep
        FROM  vbep
        FOR ALL ENTRIES IN gt_vbak_vbap
        WHERE vbep~vbeln = gt_vbak_vbap-vbeln2    " 订单编号2 . *
          AND vbep~posnr = gt_vbak_vbap-posnr2.   " 行项目 .
      SORT gt_vbep BY vbeln posnr.
      DELETE ADJACENT DUPLICATES FROM gt_vbep COMPARING vbeln posnr.

      SELECT
        kna1~kunnr           "* 取值所需关联字段
        kna1~name1           " 客户名称
        INTO CORRESPONDING FIELDS OF TABLE gt_kna1
        FROM  kna1
        FOR ALL ENTRIES IN gt_vbak_vbap
        WHERE kna1~kunnr = gt_vbak_vbap-kunnr.
      SORT gt_kna1 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM gt_kna1 COMPARING kunnr.


      SELECT
        tvkot~vkorg          " 销售组织 *
        tvkot~vtext          " 销售组织描述
        INTO TABLE gt_tvkot
        FROM tvkot
        FOR ALL ENTRIES IN gt_vbak_vbap
        WHERE tvkot~vkorg = gt_vbak_vbap-vkorg2    " 销售组织2 *
        AND spras = '1'.       " 语言
      SORT gt_tvkot BY vkorg.
      DELETE ADJACENT DUPLICATES FROM gt_tvkot COMPARING vkorg.


      SELECT
        makt~matnr           " 物料编码  *
        makt~maktx           " 物料名称
        INTO TABLE gt_makt
        FROM makt
        FOR ALL ENTRIES IN gt_vbak_vbap
        WHERE makt~matnr = gt_vbak_vbap-matnr2    " 物料编码2  *
        AND spras = '1'.
      SORT gt_makt BY matnr1.
      DELETE ADJACENT DUPLICATES FROM gt_makt COMPARING matnr1.

      SELECT
     t001w~werks          " 工厂 *
     t001w~name1          " 工厂名称
   INTO TABLE gt_t001w
   FROM t001w
   FOR ALL ENTRIES IN gt_vbak_vbap
   WHERE t001w~werks = gt_vbak_vbap-werks.       " 工厂
      SORT gt_t001w BY werks.
      DELETE ADJACENT DUPLICATES FROM gt_t001w COMPARING werks.
    ENDIF.


    SELECT
      tvlkt~lfart          " 交货单类型 *
      tvlkt~vtext          " 交货类型描述
    INTO CORRESPONDING FIELDS OF TABLE  gt_tvlkt
    FROM tvlkt
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE tvlkt~lfart = lt_lips_likp_2-lfart   " 交货单类型 *
      AND spras = '1'.        " 语言
    SORT gt_tvlkt BY lfart.
    DELETE ADJACENT DUPLICATES FROM gt_tvlkt COMPARING lfart.

    SELECT
      tvstt~vstel          " 装运点  *
      tvstt~vtext          " 装运点描述
    INTO TABLE gt_tvstt
    FROM tvstt
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE tvstt~vstel = lt_lips_likp_2-vstel   " 装运点  *
      AND spras = '1'.       " 语言
    SORT gt_tvstt BY vstel.
    DELETE ADJACENT DUPLICATES FROM gt_tvstt COMPARING vstel.

*    SELECT
*      t001w~werks          " 工厂 *
*      t001w~name1          " 工厂名称
*    INTO TABLE gt_t001w
*    FROM t001w
*    FOR ALL ENTRIES IN lt_lips_likp_2
*    WHERE t001w~werks = lt_lips_likp_2-werks.       " 工厂
*    SORT gt_t001w BY werks.
*    DELETE ADJACENT DUPLICATES FROM gt_t001w COMPARING werks.

    SELECT
      t001l~werks          " 工厂 *
      t001l~lgort          " 库存地点 *
      t001l~lgobe          " 库位描述
    INTO CORRESPONDING FIELDS OF TABLE gt_t001l
    FROM t001l
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE t001l~werks = lt_lips_likp_2-werks       " 工厂
      AND t001l~lgort = lt_lips_likp_2-lgort.      " 库存地点
    SORT gt_t001l BY werks lgort.
    DELETE ADJACENT DUPLICATES FROM gt_t001l COMPARING werks lgort.

  ENDIF.


  LOOP AT gt_vbak_vbap INTO gs_vbak_vbap.
    gs_tab_1-vbeln2 = gs_vbak_vbap-vbeln2.         " 订单编号2 . *
    gs_tab_1-bstnk  = gs_vbak_vbap-bstnk .         " 合同编号
    gs_tab_1-vkorg2 = gs_vbak_vbap-vkorg2.         " 销售组织2 .
    gs_tab_1-posnr2 = gs_vbak_vbap-posnr2.         " 行项目标号2 .
    gs_tab_1-matnr2 = gs_vbak_vbap-matnr2.         " 物料编码2 . *
    gs_tab_1-kwmeng = gs_vbak_vbap-kwmeng.         " 数量2 .
    gs_tab_1-meins2 = gs_vbak_vbap-meins2.         " 单位2 .
    gs_tab_1-charg2 = gs_vbak_vbap-charg.         " 批次2 .
    gs_tab_1-kdmat = gs_vbak_vbap-kdmat.         " 备注 .
    gs_tab_1-werks = gs_vbak_vbap-werks.         " 工厂.


    READ TABLE gt_lips_likp_2  INTO gs_lips_likp_2 WITH KEY vgbel = gs_vbak_vbap-vbeln2    " 订单编号
                                                        vgpos = gs_vbak_vbap-posnr2    " 行项目 *对应取值
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vbeln = gs_lips_likp_2-vbeln.          " 交货单编号  KEY *
      gs_tab_1-erdat = gs_lips_likp_2-erdat.          " 交货单创建日期
      gs_tab_1-lfart = gs_lips_likp_2-lfart.          " 交货单类型
      gs_tab_1-vstel = gs_lips_likp_2-vstel.          " 装运点
      gs_tab_1-werks = gs_lips_likp_2-werks.          " 工厂
      gs_tab_1-lgort = gs_lips_likp_2-lgort.          " 库存地点
      gs_tab_1-kunnr = gs_lips_likp_2-kunnr.
    ENDIF.
    READ TABLE gt_vbep  INTO gs_vbep WITH KEY vbeln = gs_vbak_vbap-vbeln2     " 订单编号2 . *
                                              posnr = gs_vbak_vbap-posnr2     " 行项目 *对应取值
                                                          BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-edatu = gs_vbep-edatu.          " 计划交货时间2 .
    ENDIF.
    READ TABLE gt_kna1  INTO gs_kna1 WITH KEY kunnr = gs_vbak_vbap-kunnr     "* 取值所需关联字段
                                                          BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name1 = gs_kna1-name1.          " 客户名称
    ENDIF.
    READ TABLE gt_tvkot  INTO gs_tvkot WITH KEY vkorg = gs_vbak_vbap-vkorg2    " 销售组织2 .
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext2 = gs_tvkot-vtext2.         " 销售组织描述
    ENDIF.
    READ TABLE gt_makt  INTO gs_makt WITH KEY matnr1 = gs_vbak_vbap-matnr2    " 物料编码2 . *
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-maktx = gs_makt-maktx.           " 物料名称
    ENDIF.
*    ELSE.
*      CLEAR gs_tab_1.
*      CONTINUE.

*    ENDIF.


    READ TABLE gt_tvlkt  INTO gs_tvlkt WITH KEY lfart = gs_lips_likp_2-lfart    " 交货单类型
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext = gs_tvlkt-vtext.         " 交货类型描述
    ENDIF.
    READ TABLE gt_tvstt  INTO gs_tvstt WITH KEY vstel = gs_lips_likp_2-vstel    " 装运点
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext1 = gs_tvstt-vtext1.        " 装运点描述
    ENDIF.

    READ TABLE gt_t001w  INTO gs_t001w WITH KEY werks = gs_vbak_vbap-werks    " 工厂
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name2 = gs_t001w-name2.          " 工厂名称
    ENDIF.
    READ TABLE gt_t001l  INTO gs_t001l WITH KEY werks = gs_lips_likp_2-werks    " 工厂
                                                lgort = gs_lips_likp_2-lgort     " 库存地点
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-lgobe = gs_t001l-lgobe.          " 库位描述
    ENDIF.

*****  采购订单号表头长文本   *****
    l_name  = gs_vbak_vbap-vbeln2.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z001'
        language                = '1'
        name                    = l_name
        object                  = 'VBBK'
      TABLES
        lines                   = gt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text IS NOT INITIAL.
      LOOP AT gt_text INTO gs_text.
*        CONCATENATE LINES OF gt_text INTO gs_tab_1-ztext." SEPARATED BY '&'.
        CONCATENATE gs_tab_1-ztext gs_text-tdline  INTO gs_tab_1-ztext.
*        gs_tab_1-ztext = gs_text-tdline.
      ENDLOOP.
    ENDIF.

    APPEND gs_tab_1 TO gt_tab_1.
    CLEAR:  gs_tab_1,
            gs_vbak_vbap,
            gs_lips_likp_2,
            gs_vbep,
            gs_kna1,
            gs_tvlkt,
            gs_tvstt,
            gs_tvkot,
            gs_makt,
            gs_t001w,
            gs_t001l,
            gt_text.

  ENDLOOP.

***-----  增强选择屏幕筛选   ------*****
  DELETE gt_tab_1 WHERE bstnk  NOT IN s_bstnk.     " 合同编号
*  DELETE gt_tab_1 WHERE VBELN  IS  INITIAL.

ENDFORM.                    " FRM_SELDATA_2                           *



*&---------------------------------------------------------------------*
*&      Form  FRM_SELDATA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_seldata_3 .       " 业务三
*----------------  定义局部变量  --------------------------
  DATA:lt_lips_likp_2 TYPE TABLE OF ty_lips_likp_2,
       lv_lips_likp_2 LIKE LINE OF lt_lips_likp_2,

       lt_ekko_ekpo   TYPE TABLE OF ty_ekko_ekpo,
       lv_ekko_ekpo   LIKE LINE OF lt_ekko_ekpo.


*****   LIKP 和 LIPS 表链接查找    *****
  SELECT
    lips~vgbel           "* 关联字段 VBAK-VBELN=LIPS-VGBEL
    likp~vbeln           " 交货单编号  KEY *
    likp~erdat           " 交货单创建日期
    likp~lfart           " 交货单类型
    likp~vstel           " 装运点
    likp~kunnr
    lips~werks           " 工厂
    lips~lgort           " 库存地点
    lips~vgpos           " 行项目 *对应取值
    lips~charg
    INTO CORRESPONDING FIELDS OF TABLE gt_lips_likp_2
    FROM likp INNER JOIN lips
    ON  likp~vbeln = lips~vbeln    " 交货单编号  KEY *
    WHERE lips~vgbel IN s_ebeln .       " 订单编号
  SORT gt_lips_likp_2 BY vgbel vgpos. " 排序方便二分法查找


  IF gt_lips_likp_2 IS NOT INITIAL.
    APPEND LINES OF gt_lips_likp_2 TO lt_lips_likp_2.
    SORT lt_lips_likp_2 BY vgbel vgpos lfart vstel werks lgort.
    DELETE ADJACENT DUPLICATES FROM lt_lips_likp_2 COMPARING vgbel vgpos lfart vstel werks lgort.

    SELECT
      ekko~ebeln           " 订单编号3 .
      ekko~ekorg           " 采购组织3 .
      ekpo~ebelp AS posnr3 " 行项目标号3 .
      ekpo~matnr           " 物料编码3 .
      ekpo~menge           " 数量3 .
      ekpo~meins           " 单位3 .
      ekpo~kunnr           "* 取值所需关联字段
      INTO TABLE gt_ekko_ekpo
      FROM ekko INNER JOIN ekpo
      ON  ekko~ebeln = ekpo~ebeln  " 订单编号3 . *
      FOR ALL ENTRIES IN lt_lips_likp_2
      WHERE ekko~ebeln = lt_lips_likp_2-vgbel   " 订单编号3 . *
      AND   ekpo~ebelp = lt_lips_likp_2-vgpos+1(5)    " 行项目标号3 .
      AND   ekko~ekorg IN s_ekorg        " 采购组织
      AND   ekko~bsart IN s_bsart        " 订单类型
      AND   ekko~ebeln IN s_ebeln.       " 订单编号
    SORT gt_ekko_ekpo BY ebeln posnr3. " 排序方便二分法查找

    SELECT vbap~vbeln
           vbap~kdmat
          FROM vbap
          INTO CORRESPONDING FIELDS OF TABLE gt_vbap
          FOR ALL ENTRIES IN lt_lips_likp_2
          WHERE vbap~vbeln = lt_lips_likp_2-vgbel.   " 订单编号3 . *
    SORT gt_vbap BY vbeln.

    IF gt_ekko_ekpo IS NOT INITIAL.
      APPEND LINES OF gt_ekko_ekpo TO lt_ekko_ekpo.
      SORT lt_ekko_ekpo BY ebeln posnr3 vkorg3 matnr3 kunnr .
      DELETE ADJACENT DUPLICATES FROM lt_ekko_ekpo COMPARING ebeln posnr3 vkorg3 matnr3 kunnr .

      SELECT
        eket~ebeln           " 订单编号3 . *
        eket~ebelp           " 行项目 .
        eket~eindt           " 计划交货时间3 .
        INTO CORRESPONDING FIELDS OF TABLE gt_eket
        FROM  eket
        FOR ALL ENTRIES IN lt_ekko_ekpo
        WHERE eket~ebeln = lt_ekko_ekpo-ebeln     " 订单编号3 . *
          AND eket~ebelp = lt_ekko_ekpo-posnr3.   " 行项目 .
      SORT gt_eket BY ebeln.
      DELETE ADJACENT DUPLICATES FROM gt_eket COMPARING ebeln.


      SELECT
        kna1~kunnr           "* 取值所需关联字段
        kna1~name1           " 客户名称
        INTO CORRESPONDING FIELDS OF TABLE gt_kna1
        FROM  kna1
        FOR ALL ENTRIES IN lt_ekko_ekpo
        WHERE kna1~kunnr = lt_ekko_ekpo-kunnr.
      SORT gt_kna1 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM gt_kna1 COMPARING kunnr.


      SELECT
        tvkot~vkorg          " 销售组织 *
        tvkot~vtext          " 销售组织描述
        INTO TABLE gt_tvkot
        FROM tvkot
        FOR ALL ENTRIES IN lt_ekko_ekpo
        WHERE tvkot~vkorg = lt_ekko_ekpo-vkorg3    " 采购组织3 *
          AND spras = '1'.       " 语言
      SORT gt_tvkot BY vkorg.
      DELETE ADJACENT DUPLICATES FROM gt_tvkot COMPARING vkorg.


      SELECT
        makt~matnr           " 物料编码  *
        makt~maktx           " 物料名称
      INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_ekko_ekpo
      WHERE makt~matnr = lt_ekko_ekpo-matnr3    " 物料编码2  *
        AND spras = '1'.
      SORT gt_makt BY matnr1.
      DELETE ADJACENT DUPLICATES FROM gt_makt COMPARING matnr1.

    ENDIF.


    SELECT
      tvlkt~lfart          " 交货单类型 *
      tvlkt~vtext          " 交货类型描述
    INTO CORRESPONDING FIELDS OF TABLE  gt_tvlkt
    FROM tvlkt
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE tvlkt~lfart = lt_lips_likp_2-lfart   " 交货单类型 *
      AND spras = '1'.        " 语言
    SORT gt_tvlkt BY lfart.
    DELETE ADJACENT DUPLICATES FROM gt_tvlkt COMPARING lfart.

    SELECT
      tvstt~vstel          " 装运点  *
      tvstt~vtext          " 装运点描述
    INTO TABLE gt_tvstt
    FROM tvstt
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE tvstt~vstel = lt_lips_likp_2-vstel   " 装运点  *
      AND spras = '1'.       " 语言
    SORT gt_tvstt BY vstel.
    DELETE ADJACENT DUPLICATES FROM gt_tvstt COMPARING vstel.

    SELECT
      t001w~werks          " 工厂 *
      t001w~name1          " 工厂名称
    INTO TABLE gt_t001w
    FROM t001w
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE t001w~werks = lt_lips_likp_2-werks.       " 工厂
    SORT gt_t001w BY werks.
    DELETE ADJACENT DUPLICATES FROM gt_t001w COMPARING werks.

    SELECT
      t001l~werks          " 工厂 *
      t001l~lgort          " 库存地点 *
      t001l~lgobe          " 库位描述
    INTO CORRESPONDING FIELDS OF TABLE gt_t001l
    FROM t001l
    FOR ALL ENTRIES IN lt_lips_likp_2
    WHERE t001l~werks = lt_lips_likp_2-werks       " 工厂
      AND t001l~lgort = lt_lips_likp_2-lgort.      " 库存地点
    SORT gt_t001l BY werks lgort.
    DELETE ADJACENT DUPLICATES FROM gt_t001l COMPARING werks lgort.

  ENDIF.


  LOOP AT gt_lips_likp_2 INTO gs_lips_likp_2.
    gs_tab_1-vbeln = gs_lips_likp_2-vbeln.          " 交货单编号  KEY *
    gs_tab_1-erdat = gs_lips_likp_2-erdat.          " 交货单创建日期
    gs_tab_1-lfart = gs_lips_likp_2-lfart.          " 交货单类型
    gs_tab_1-vstel = gs_lips_likp_2-vstel.          " 装运点
    gs_tab_1-werks = gs_lips_likp_2-werks.          " 工厂
    gs_tab_1-lgort = gs_lips_likp_2-lgort.          " 库存地点
    gs_tab_1-kunnr = gs_lips_likp_2-kunnr.
    READ TABLE gt_vbap INTO gs_vbap WITH KEY  vbeln  = gs_lips_likp_2-vgbel BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-kdmat = gs_vbap-kdmat.
    ENDIF.
    READ TABLE gt_ekko_ekpo  INTO gs_ekko_ekpo WITH KEY ebeln  = gs_lips_likp_2-vgbel    " 订单编号3 .
                                                        posnr3 = gs_lips_likp_2-vgpos+1(5)     " 行项目 *对应取值
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-ebeln  = gs_ekko_ekpo-ebeln .         "  订单编号3 .
      gs_tab_1-vkorg3 = gs_ekko_ekpo-vkorg3.         "  采购组织3 .
      gs_tab_1-posnr3 = gs_ekko_ekpo-posnr3.         "  行项目标号3 .
      gs_tab_1-matnr3 = gs_ekko_ekpo-matnr3.         "  物料编码3 .
      gs_tab_1-menge  = gs_ekko_ekpo-menge .         "  数量3 .
      gs_tab_1-meins3 = gs_ekko_ekpo-meins3.         "  单位3 .
      READ TABLE gt_eket  INTO gs_eket WITH KEY ebeln = gs_ekko_ekpo-ebeln     " 订单编号3 . *
                                                ebelp = gs_ekko_ekpo-posnr3     " 行项目 *对应取值
                                                            BINARY SEARCH.
      IF sy-subrc = 0.
        gs_tab_1-eindt = gs_eket-eindt.          " 计划交货时间3 .
      ENDIF.
      READ TABLE gt_kna1  INTO gs_kna1 WITH KEY kunnr = gs_ekko_ekpo-kunnr     "* 取值所需关联字段
                                                            BINARY SEARCH.
      IF sy-subrc = 0.
        gs_tab_1-name1 = gs_kna1-name1.          " 客户名称
      ENDIF.
      READ TABLE gt_tvkot  INTO gs_tvkot WITH KEY vkorg = gs_ekko_ekpo-vkorg3    " 采购组织3 .
                                                              BINARY SEARCH.
      IF sy-subrc = 0.
        gs_tab_1-vtext2 = gs_tvkot-vtext2.         " 销售组织描述
      ENDIF.
      READ TABLE gt_makt  INTO gs_makt WITH KEY matnr1 = gs_ekko_ekpo-matnr3    " 物料编码3 . *
                                                               BINARY SEARCH.
      IF sy-subrc = 0.
        gs_tab_1-maktx = gs_makt-maktx.           " 物料名称
      ENDIF.
    ELSE.
      CLEAR gs_tab_1.
      CONTINUE.

    ENDIF.


    READ TABLE gt_tvlkt  INTO gs_tvlkt WITH KEY lfart = gs_lips_likp_2-lfart    " 交货单类型
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext = gs_tvlkt-vtext.         " 交货类型描述
    ENDIF.
    READ TABLE gt_tvstt  INTO gs_tvstt WITH KEY vstel = gs_lips_likp_2-vstel    " 装运点
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext1 = gs_tvstt-vtext1.        " 装运点描述
    ENDIF.

    READ TABLE gt_t001w  INTO gs_t001w WITH KEY werks = gs_lips_likp_2-werks    " 工厂
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name2 = gs_t001w-name2.          " 工厂名称
    ENDIF.
    READ TABLE gt_t001l  INTO gs_t001l WITH KEY werks = gs_lips_likp_2-werks    " 工厂
                                                lgort = gs_lips_likp_2-lgort.    " 库存地点
    IF sy-subrc = 0.
      gs_tab_1-lgobe = gs_t001l-lgobe.          " 库位描述
    ENDIF.

*****  采购订单号表头长文本   *****
    l_name  = gs_lips_likp_2-vgbel.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z001'
        language                = '1'
        name                    = l_name
        object                  = 'VBBK'
      TABLES
        lines                   = gt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text IS NOT INITIAL.
      LOOP AT gt_text INTO gs_text.
*        CONCATENATE LINES OF gt_text-TDLINE INTO gs_tab_1-ztext." SEPARATED BY '&'.
*        gs_tab_1-ztext = gs_text-tdline.
        CONCATENATE gs_tab_1-ztext gs_text-tdline  INTO gs_tab_1-ztext.
      ENDLOOP.
    ENDIF.


    APPEND gs_tab_1 TO gt_tab_1.
    CLEAR:  gs_tab_1,
            gs_ekko_ekpo,
            gs_lips_likp_2,
            gs_eket,
            gs_kna1,
            gs_tvlkt,
            gs_tvstt,
            gs_tvkot,
            gs_makt,
            gs_t001w,
            gs_t001l,
            gt_text.

  ENDLOOP.

***-----  增强选择屏幕筛选   ------*****
  DELETE gt_tab_1 WHERE vkorg3  NOT IN s_ekorg.     " 采购组织3

ENDFORM.                    " FRM_SELDATA_3


*-------------------销售订单---------------
FORM frm_seldata_4 .       " 销售订单
*----------------  定义局部变量  --------------------------
  DATA:lt_lips_likp_4 TYPE TABLE OF ty_lips_likp_2,
       lv_lips_likp_4 LIKE LINE OF lt_lips_likp_4,

       lt_vbak_vbap_4 TYPE TABLE OF ty_vbak_vbap,
       lv_vbak_vbap_4 LIKE LINE OF lt_vbak_vbap_4.


*****   LIKP 和 LIPS 表链接查找    *****

  SELECT
      vbak~vbeln           " 订单编号2 . *
      vbak~bstnk           " 合同编号
      vbak~vkorg           " 销售组织2 .
      vbap~posnr           " 行项目标号2 .
      vbap~matnr           " 物料编码2 . *
      vbap~kwmeng          " 数量2 .
      vbap~meins           " 单位2 .
      vbap~charg           " 批次2 .
      vbap~kdmat            "备注
      vbak~kunnr           "* 取值所需关联字段
      INTO TABLE gt_vbak_vbap_4
      FROM vbak INNER JOIN vbap
      ON  vbak~vbeln = vbap~vbeln  " 订单编号2 . *
*      FOR ALL ENTRIES IN lt_lips_likp_4
*      WHERE vbak~vbeln = lt_lips_likp_4-vgbel   " 订单编号2 . *
*      AND   vbap~posnr = lt_lips_likp_4-vgpos    " 订单编号2 . *
      WHERE vbak~vkorg IN s_vkorg4       " 销售组织
      AND   vbak~auart IN s_auart4        " 订单类型
      AND   vbak~vbeln IN s_vbeln4       " 订单编号
      AND   vbak~bstnk IN s_bstnk4.       " 合同编号
  SORT gt_vbak_vbap_4 BY vbeln2 posnr2. " 排序方便二分法查找

  IF gt_vbak_vbap_4 IS NOT INITIAL.
    APPEND LINES OF gt_vbak_vbap_4 TO lt_vbak_vbap_4.
    SORT lt_vbak_vbap_4 BY vbeln2 posnr2 vkorg2 matnr2 kunnr .
    DELETE ADJACENT DUPLICATES FROM lt_vbak_vbap_4 COMPARING vbeln2 posnr2 vkorg2 matnr2 kunnr .
*    APPEND LINES OF gt_vbak_vbap_4 TO gs_vbak_vbap_4.
*    SORT lt_lips_likp_4 BY vgbel vgpos lfart vstel werks lgort.
*    DELETE ADJACENT DUPLICATES FROM lt_lips_likp_4 COMPARING vgbel vgpos lfart vstel werks lgort.
    SELECT
      lips~vgbel           "* 关联字段 VBAK-VBELN=LIPS-VGBEL
      likp~vbeln           " 交货单编号  KEY *
      likp~erdat           " 交货单创建日期
      likp~lfart           " 交货单类型
      likp~vstel           " 装运点
      likp~kunnr
      lips~werks           " 工厂
      lips~lgort           " 库存地点
      lips~vgpos           " 行项目 *对应取值
      INTO CORRESPONDING FIELDS OF TABLE gt_lips_likp_4
      FROM likp INNER JOIN lips
      ON  likp~vbeln = lips~vbeln    " 交货单编号  KEY *
      FOR ALL ENTRIES IN  gt_vbak_vbap_4
      WHERE lips~vgbel = gt_vbak_vbap_4-vbeln2   " 订单编号2 . *
        AND  lips~vgpos = gt_vbak_vbap_4-posnr2.    " 订单编号2 . *
*    WHERE liPS~vgbel IN s_vbeln4        " 订单编号
*    AND   likp~vkorg IN s_vkorg4.       " 销售组织
    SORT gt_lips_likp_4 BY vgbel vgpos. " 排序方便二分法查找


*    IF gt_vbak_vbap_4 IS NOT INITIAL.
*      APPEND LINES OF gt_vbak_vbap_4 TO lt_vbak_vbap_4.
*      SORT lt_vbak_vbap_4 BY vbeln2 posnr2 vkorg2 matnr2 kunnr .
*      DELETE ADJACENT DUPLICATES FROM lt_vbak_vbap_4 COMPARING vbeln2 posnr2 vkorg2 matnr2 kunnr .

    SELECT
      vbep~vbeln           " 订单编号2 . *
      vbep~posnr           " 行项目 .
      vbep~edatu           " 计划交货时间2 .
      INTO CORRESPONDING FIELDS OF TABLE gt_vbep_4
      FROM  vbep
      FOR ALL ENTRIES IN gt_vbak_vbap_4
      WHERE vbep~vbeln = gt_vbak_vbap_4-vbeln2    " 订单编号2 . *
        AND vbep~posnr = gt_vbak_vbap_4-posnr2.   " 行项目 .
    SORT gt_vbep_4 BY vbeln posnr.
    DELETE ADJACENT DUPLICATES FROM gt_vbep_4 COMPARING vbeln posnr.

    SELECT
      kna1~kunnr           "* 取值所需关联字段
      kna1~name1           " 客户名称
      INTO CORRESPONDING FIELDS OF TABLE gt_kna1_4
      FROM  kna1
      FOR ALL ENTRIES IN gt_vbak_vbap_4
      WHERE kna1~kunnr = gt_vbak_vbap_4-kunnr.
    SORT gt_kna1_4 BY kunnr.
    DELETE ADJACENT DUPLICATES FROM gt_kna1_4 COMPARING kunnr.


    SELECT
      tvkot~vkorg          " 销售组织 *
      tvkot~vtext          " 销售组织描述
      INTO TABLE gt_tvkot
      FROM tvkot
      FOR ALL ENTRIES IN gt_vbak_vbap_4
      WHERE tvkot~vkorg = gt_vbak_vbap_4-vkorg2    " 销售组织2 *
      AND spras = '1'.       " 语言
    SORT gt_tvkot BY vkorg.
    DELETE ADJACENT DUPLICATES FROM gt_tvkot COMPARING vkorg.


    SELECT
      makt~matnr           " 物料编码  *
      makt~maktx           " 物料名称
      INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN gt_vbak_vbap_4
      WHERE makt~matnr = gt_vbak_vbap_4-matnr2    " 物料编码2  *
      AND spras = '1'.
    SORT gt_makt BY matnr1.
    DELETE ADJACENT DUPLICATES FROM gt_makt COMPARING matnr1.

*    ENDIF.


    SELECT
      tvlkt~lfart          " 交货单类型 *
      tvlkt~vtext          " 交货类型描述
    INTO CORRESPONDING FIELDS OF TABLE  gt_tvlkt
    FROM tvlkt
    FOR ALL ENTRIES IN lt_lips_likp_4
    WHERE tvlkt~lfart = lt_lips_likp_4-lfart   " 交货单类型 *
      AND spras = '1'.        " 语言
    SORT gt_tvlkt BY lfart.
    DELETE ADJACENT DUPLICATES FROM gt_tvlkt COMPARING lfart.

    SELECT
      tvstt~vstel          " 装运点  *
      tvstt~vtext          " 装运点描述
    INTO TABLE gt_tvstt
    FROM tvstt
    FOR ALL ENTRIES IN lt_lips_likp_4
    WHERE tvstt~vstel = lt_lips_likp_4-vstel   " 装运点  *
      AND spras = '1'.       " 语言
    SORT gt_tvstt BY vstel.
    DELETE ADJACENT DUPLICATES FROM gt_tvstt COMPARING vstel.

    SELECT
      t001w~werks          " 工厂 *
      t001w~name1          " 工厂名称
    INTO TABLE gt_t001w
    FROM t001w
    FOR ALL ENTRIES IN lt_lips_likp_4
    WHERE t001w~werks = lt_lips_likp_4-werks.       " 工厂
    SORT gt_t001w BY werks.
    DELETE ADJACENT DUPLICATES FROM gt_t001w COMPARING werks.

    SELECT
      t001l~werks          " 工厂 *
      t001l~lgort          " 库存地点 *
      t001l~lgobe          " 库位描述
    INTO CORRESPONDING FIELDS OF TABLE gt_t001l
    FROM t001l
    FOR ALL ENTRIES IN lt_lips_likp_4
    WHERE t001l~werks = lt_lips_likp_4-werks       " 工厂
      AND t001l~lgort = lt_lips_likp_4-lgort.      " 库存地点
    SORT gt_t001l BY werks lgort.
    DELETE ADJACENT DUPLICATES FROM gt_t001l COMPARING werks lgort.

  ENDIF.

  LOOP AT gt_vbak_vbap_4 INTO gs_vbak_vbap_4.
    gs_tab_1-vbeln2 = gs_vbak_vbap_4-vbeln2.         " 订单编号2 . *
    gs_tab_1-bstnk  = gs_vbak_vbap_4-bstnk .         " 合同编号
    gs_tab_1-vkorg2 = gs_vbak_vbap_4-vkorg2.         " 销售组织2 .
    gs_tab_1-posnr2 = gs_vbak_vbap_4-posnr2.         " 行项目标号2 .
    gs_tab_1-matnr2 = gs_vbak_vbap_4-matnr2.         " 物料编码2 . *
    gs_tab_1-kwmeng = gs_vbak_vbap_4-kwmeng.         " 数量2 .
    gs_tab_1-meins2 = gs_vbak_vbap_4-meins2.         " 单位2 .
    gs_tab_1-charg2 = gs_vbak_vbap_4-charg.         " 批次2 .
    gs_tab_1-kdmat =  gs_vbak_vbap_4-kdmat.         " 备注 .
    READ TABLE gt_lips_likp_4  INTO gs_lips_likp_4 WITH KEY  vgbel = gs_vbak_vbap_4-vbeln2    " 订单编号
                                                          vgpos = gs_vbak_vbap_4-posnr2     " 行项目 *对应取值
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vbeln = gs_lips_likp_4-vbeln.          " 交货单编号  KEY *
      gs_tab_1-erdat = gs_lips_likp_4-erdat.          " 交货单创建日期
      gs_tab_1-lfart = gs_lips_likp_4-lfart.          " 交货单类型
      gs_tab_1-vstel = gs_lips_likp_4-vstel.          " 装运点
      gs_tab_1-werks = gs_lips_likp_4-werks.          " 工厂
      gs_tab_1-lgort = gs_lips_likp_4-lgort.          " 库存地点
      gs_tab_1-kunnr = gs_lips_likp_4-kunnr.
    ENDIF.
    READ TABLE gt_vbep_4  INTO gs_vbep_4 WITH KEY vbeln = gs_vbak_vbap_4-vbeln2     " 订单编号2 . *
                                              posnr = gs_vbak_vbap_4-posnr2     " 行项目 *对应取值
                                                          BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-edatu = gs_vbep_4-edatu.          " 计划交货时间2 .
    ENDIF.
    READ TABLE gt_kna1_4  INTO gs_kna1_4 WITH KEY kunnr = gs_vbak_vbap_4-kunnr     "* 取值所需关联字段
                                                          BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name1 = gs_kna1_4-name1.          " 客户名称
    ENDIF.
    READ TABLE gt_tvkot  INTO gs_tvkot WITH KEY vkorg = gs_vbak_vbap_4-vkorg2    " 销售组织2 .
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext2 = gs_tvkot-vtext2.         " 销售组织描述
    ENDIF.
    READ TABLE gt_makt  INTO gs_makt WITH KEY matnr1 = gs_vbak_vbap_4-matnr2    " 物料编码2 . *
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-maktx = gs_makt-maktx.           " 物料名称
    ENDIF.
*    ELSE.
*      CLEAR gs_tab_1.
*      CONTINUE.
*
*    ENDIF.


    READ TABLE gt_tvlkt  INTO gs_tvlkt WITH KEY lfart = gs_lips_likp_4-lfart    " 交货单类型
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext = gs_tvlkt-vtext.         " 交货类型描述
    ENDIF.
    READ TABLE gt_tvstt  INTO gs_tvstt WITH KEY vstel = gs_lips_likp_4-vstel    " 装运点
                                                            BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-vtext1 = gs_tvstt-vtext1.        " 装运点描述
    ENDIF.

    READ TABLE gt_t001w  INTO gs_t001w WITH KEY werks = gs_lips_likp_4-werks    " 工厂
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-name2 = gs_t001w-name2.          " 工厂名称
    ENDIF.
    READ TABLE gt_t001l  INTO gs_t001l WITH KEY werks = gs_lips_likp_4-werks    " 工厂
                                                lgort = gs_lips_likp_4-lgort     " 库存地点
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      gs_tab_1-lgobe = gs_t001l-lgobe.          " 库位描述
    ENDIF.

*****  采购订单号表头长文本   *****
    l_name  = gs_vbak_vbap_4-vbeln2.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z001'
        language                = '1'
        name                    = l_name
        object                  = 'VBBK'
      TABLES
        lines                   = gt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text IS NOT INITIAL.
      LOOP AT gt_text INTO gs_text.
*        CONCATENATE LINES OF gt_text INTO gs_tab_1-ztext." SEPARATED BY '&'.
        CONCATENATE gs_tab_1-ztext gs_text-tdline  INTO gs_tab_1-ztext.
*        gs_tab_1-ztext = gs_text-tdline.
      ENDLOOP.
    ENDIF.

    APPEND gs_tab_1 TO gt_tab_1.
    CLEAR:  gs_tab_1,
            gs_vbak_vbap_4,
            gs_lips_likp_4,
            gs_vbep_4,
            gs_kna1_4,
            gs_tvlkt,
            gs_tvstt,
            gs_tvkot,
            gs_makt,
            gs_t001w,
            gs_t001l,
            gt_text.

  ENDLOOP.

***-----  增强选择屏幕筛选   ------*****
  DELETE gt_tab_1 WHERE bstnk  NOT IN s_bstnk.     " 合同编号

ENDFORM.                    " FRM_SELDATA_2                           *


*-----------------ALV输出---------------------*

*&---------------------------------------------------------------------*
*&      Form  FRM_WRITE_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_write .

  PERFORM layout_build.       " 输出样式

  IF type1 = 'X'.   " 租赁，借料(非关联方)
    PERFORM fieldcat_build_1.   " 输出格式属性
  ELSEIF type2 = 'X'. "  租赁，借料(非关联方)
    PERFORM fieldcat_build_2.   " 输出格式属性
  ELSEIF type3 = 'X'.  " 借料(关联方)
    PERFORM fieldcat_build_3.   " 输出格式属性
  ELSEIF type4 = 'X'. "  销售订单
    PERFORM fieldcat_build_2.   " 输出格式属性
  ENDIF.

*SORT gt_tab_1 BY vbeln.
*   DELETE ADJACENT DUPLICATES FROM gt_tab_1 COMPARING vbeln. " 删除打印重复订单

  PERFORM alv_print .        " alv打印输出

ENDFORM.                    " FRM_WRITE_1                             *
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout_build .
  gs_layout-box_fieldname = 'BOX'.    "添加BOX选择框
  gs_layout-zebra = 'X'.              "ALV表格按斑马线条纹显示
  gs_layout-colwidth_optimize = 'X'.  "ALV字段宽度最优化
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_build_1 .
  DATA:l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.

  CHECK gt_fieldcat IS  INITIAL.

  catalog:  " 1 行数 "2 字段的名字 "3 字段的文本描述 "4 是否为KEY值   X . "5 删除前导“0”  X .
            " 6 字段输出居中   C . "7 可编辑列   X . "8 参考表  "9 参考字段
    l_colpos 'vgbel '   '订单编号   '      'X'    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'bstnk '   '合同编号   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'KUNNR '   '客户编码   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'NAME1 '   '客户名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'ztext '   '项目名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vdatu '   '计划交货时间   '  ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vbeln '   '交货单编号     '  ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'erdat '   '交货单创建日期  ' ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lfart '   '交货单类型  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext '   '交货类型描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vstel '   '装运点      '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext1'   '装运点描述  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vkorg1'   '销售组织    '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext2'   '销售组织描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'posnr1'   '行项目标号   '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'matnr1'   '物料编码  '       ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'maktx '   '物料名称  '       ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lfimg '   '数量 '            ' '    ' '    'C'  'X '   ' '  ' '  ,
    l_colpos 'meins1'   '单位 '            ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'charg1'   '批次   '          ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'werks '   '工厂   '          ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'name2 '   '工厂名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lgort '   '库位    '         ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'lgobe '   '库位描述  '       ' '    ' '    'C'  ' '   ' '  ' '  .

ENDFORM.                    " FIELDCAT_BUILD_1                        *

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_build_2 .
  DATA:l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.

  CHECK gt_fieldcat IS  INITIAL.

  catalog:  " 1 行数 "2 字段的名字 "3 字段的文本描述 "4 是否为KEY值   X . "5 删除前导“0”  X .
            " 6 字段输出居中   C . "7 可编辑列   X . "8 参考表  "9 参考字段
    l_colpos 'vbeln2'   '订单编号   '      'X'    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'bstnk '   '合同编号   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'KUNNR '   '客户编码   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'name1 '   '客户名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'ztext '   '项目名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'edatu '   '计划交货时间   '  ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vbeln '   '交货单编号     '  ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'erdat '   '交货单创建日期  ' ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lfart '   '交货单类型  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext '   '交货类型描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vstel '   '装运点      '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext1'   '装运点描述  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vkorg2'   '销售组织    '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext2'   '销售组织描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'posnr2'   '行项目标号   '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'matnr2'   '物料编码  '       ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'maktx '   '物料名称  '       ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'kwmeng'   '数量 '            ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'meins2'   '单位 '            ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'charg2'   '批次   '          ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'werks '   '工厂   '          ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'name2 '   '工厂名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lgort '   '库位    '         ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'lgobe '   '库位描述  '       ' '    ' '    'C'  ' '   ' '  ' '  .

ENDFORM.                    " FIELDCAT_BUILD_2                        *


*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_build_3 .
  DATA:l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.

  CHECK gt_fieldcat IS  INITIAL.

  catalog:  " 1 行数 "2 字段的名字 "3 字段的文本描述 "4 是否为KEY值   X . "5 删除前导“0”  X .
            " 6 字段输出居中   C . "7 可编辑列   X . "8 参考表  "9 参考字段
    l_colpos 'ebeln '   '订单编号   '      'X'    ' '    'C'  ' '   ' '  ' '  ,
*    l_colpos 'bstnk '   '合同编号   '      ' '    ' '    'C'  ' '   ' '  ' '  ,  " 无
    l_colpos 'KUNNR '   '客户编码   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'name1 '   '客户名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'ztext '   '项目名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'eindt '   '计划交货时间   '  ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vbeln '   '交货单编号     '  ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'erdat '   '交货单创建日期  ' ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lfart '   '交货单类型  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext '   '交货类型描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vstel '   '装运点      '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext1'   '装运点描述  '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vkorg3'   '销售组织    '     ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'vtext2'   '销售组织描述 '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'posnr3'   '行项目标号   '    ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'matnr3'   '物料编码  '       ' '    'X'    'C'  ' '   ' '  ' '  ,
    l_colpos 'maktx '   '物料名称  '       ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'menge '   '数量 '            ' '    ' '    'C'  'X '   ' '  ' '  ,
    l_colpos 'meins3'   '单位 '            ' '    ' '    'C'  ' '   ' '  ' '  ,
*    l_colpos 'charg1'   '批次   '          ' '    ' '    'C'  'X'   ' '  ' '  ,   " 无
    l_colpos 'werks '   '工厂   '          ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'name2 '   '工厂名称   '      ' '    ' '    'C'  ' '   ' '  ' '  ,
    l_colpos 'lgort '   '库位    '         ' '    ' '    'C'  'X'   ' '  ' '  ,
    l_colpos 'lgobe '   '库位描述  '       ' '    ' '    'C'  ' '   ' '  ' '  .

ENDFORM.                    " FIELDCAT_BUILD_3                        *


*&---------------------------------------------------------------------*
*&      Form  ALV_PRINT_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_print .
  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = g_repid    " 当前程序名
      i_callback_pf_status_set = 'SET_PF_STATUS'  "设置ALV的自定义按钮
      i_callback_user_command  = 'USER_COMMAND'   "自定义按钮响应事件
      i_callback_top_of_page   = 'TOP_OF_PAGE'    " 表头文字
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = gs_layout       " 输出样式
      it_fieldcat              = gt_fieldcat[]   " 字段定义描述表
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     it_sort                  = lt_sort   " 数据小计
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      " I_SAVE有4个可选值： I_SAVE= '' --- 不能保存格式；
    " I_SAVE                   = 'X' --- 保存标准格式；
    " I_SAVE                   = 'U' --- 保存特定用户格式；
    " I_SAVE                   = 'A' --- 保存标准格式和特定用户格式。
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_tab_1      " 数据内表输出名称
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_PRINT_1                           *

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  DATA: BEGIN OF ex_tab OCCURS 0,
          fcode LIKE sy-ucomm,
        END OF ex_tab.
  ex_tab-fcode = '&ILT'.       APPEND ex_tab.
  SET PF-STATUS '9000' EXCLUDING ex_tab.
ENDFORM.                    "F_SET_STATUS                             *


*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

  DATA:lt_header TYPE slis_t_listheader,
       ls_header TYPE slis_listheader.

  IF type1 = 'X'.  " 租赁，借料(非关联方)
    ls_header-typ = 'S'.
    ls_header-info = '项目，产品销售（包含公司间）'.
    APPEND ls_header TO lt_header.
  ELSEIF type2 = 'X'. " 租赁，借料(非关联方)
    ls_header-typ = 'S'.
    ls_header-info = '租赁，借料（非关联方）'.
    APPEND ls_header TO lt_header.
  ELSEIF type3 = 'X'.  " 借料(关联方)
    ls_header-typ = 'S'.
    ls_header-info = '借料（关联方）'.
    APPEND ls_header TO lt_header.
  ELSEIF type4 = 'X'.  " 借料(关联方)
    ls_header-typ = 'S'.
    ls_header-info = '销售订单'.
    APPEND ls_header TO lt_header.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

ENDFORM.                  "TOP_OF_PAGE_1

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
rs_selfield TYPE slis_selfield.
  DATA: gv_grid TYPE REF TO cl_gui_alv_grid.

  DATA: l_num TYPE i.   " 计数BOX选择条数

  "刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = gv_grid.
  CALL METHOD gv_grid->check_changed_data.

  CASE r_ucomm.
    WHEN 'PRINT'.

      LOOP AT gt_tab_1 INTO gs_tab_1 WHERE box = 'X'.
        l_num = l_num + 1.
      ENDLOOP.

      IF  l_num = 0.
        MESSAGE '请选择数据' TYPE 'E'.   " 未选中报错
      ENDIF.
      IF type1 = 'X'.  " 租赁，借料(非关联方)
        CALL SCREEN 9001 STARTING AT 35 3.   " 跳转屏幕   | 变弹窗式
      ELSEIF type2 = 'X'. " 租赁，借料(非关联方)
        CALL SCREEN 9001 STARTING AT 35 3.   " 跳转屏幕   | 变弹窗式
      ELSEIF type3 = 'X'.  " 借料(关联方)
        CALL SCREEN 9001 STARTING AT 35 3.   " 跳转屏幕   | 变弹窗式
      ELSEIF type4 = 'X'.  " 销售订单
        CALL SCREEN 9002 STARTING AT 35 3.   " 跳转屏幕   | 变弹窗式
      ENDIF.


  ENDCASE.
ENDFORM. "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CANCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cancel INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " CANCEL  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'PRINT'.
      gt_tab_2 = gt_tab_1.
      LOOP AT gt_tab_1 INTO gs_tab_1 WHERE box = 'X' .
        gs_tab_sf-vbeln = gs_tab_1-vbeln.          " 交货单编号  KEY *
        gs_tab_sf-ztext = gs_tab_1-ztext.          " 项目名称  SUB
        gs_tab_sf-name1 = gs_tab_1-name1.          " 客户名称

        IF type1 = 'X'.    " 项目，产品销售（包含公司间）
          gs_tab_sf-vgbel = gs_tab_1-vgbel.          " 订单编号1 .
          gs_tab_sf-vdatu = gs_tab_1-vdatu.          " 计划交货时间1 .
          gs_tab_sf-bstnk = gs_tab_1-bstnk.          " 合同编号
        ELSEIF type2 = 'X'. "  租赁，借料(非关联方)
          gs_tab_sf-vgbel = gs_tab_1-vbeln2.          " 订单编号2 .
          gs_tab_sf-vdatu = gs_tab_1-edatu.          " 计划交货时间2 .
          gs_tab_sf-bstnk = gs_tab_1-bstnk.          " 合同编号
        ELSEIF type3 = 'X'.  " 借料(关联方)
          gs_tab_sf-vgbel = gs_tab_1-ebeln.          " 订单编号3 .
          gs_tab_sf-vdatu = gs_tab_1-eindt.          " 计划交货时间3 .
          gs_tab_sf-bstnk = gs_tab_1-bstnk.          " 合同编号   “ 无值

        ENDIF.

        APPEND gs_tab_sf TO gt_tab_sf.
        SORT gt_tab_sf BY vgbel.
*        DELETE ADJACENT DUPLICATES FROM gt_tab_sf COMPARING vbeln. " 删除打印重复订单
*        CLEAR gs_tab_sf.
      ENDLOOP.


      " 业务判断，打印
      IF type_1 = 'X'.   " 出库通知单
        gc_type = 1.
        gc_num = 25.
      ELSEIF type_2 = 'X'.   " 入库通知单
        gc_type = 2.
        gc_num = 15.
      ENDIF.

      PERFORM setsmart.   " 调用打印

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS '9100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_9001  OUTPUT


MODULE user_command_9002 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'PRINT'.
      gt_tab_2 = gt_tab_1.
      LOOP AT gt_tab_1 INTO gs_tab_1 WHERE box = 'X'.
        gs_tab_sf-vbeln = gs_tab_1-vbeln.          " 交货单编号  KEY *
        gs_tab_sf-ztext = gs_tab_1-ztext.          " 项目名称  SUB
        gs_tab_sf-name1 = gs_tab_1-name1.          " 客户名称

        IF type4 = 'X'. "  销售订单
          gs_tab_sf-vgbel = gs_tab_1-vbeln2.          " 订单编号2 .
          gs_tab_sf-vdatu = gs_tab_1-edatu.          " 计划交货时间2 .
          gs_tab_sf-bstnk = gs_tab_1-bstnk.          " 合同编号
        ENDIF.

        APPEND gs_tab_sf TO gt_tab_sf.
        SORT gt_tab_sf BY vgbel.
*        DELETE ADJACENT DUPLICATES FROM gt_tab_sf COMPARING vbeln. " 删除打印重复订单
        CLEAR gs_tab_sf.
      ENDLOOP.


      " 业务判断，打印
      IF type_1 = 'X'.   " 出库通知单
        gc_type = 1.
        gc_num = 25.
      ELSEIF type_2 = 'X'.   " 入库通知单
        gc_type = 2.
        gc_num = 15.
      ELSEIF type_3 = 'X'.   " 销售订单
        gc_type = 3.
        gc_num = 25.
      ENDIF.

      PERFORM setsmart.   " 调用打印

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT

*&---------------------------------------------------------------------*
*&      Form  SETSMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setsmart .      " 出库通知单

  DATA:  name_sf TYPE tdsfname.
  IF type_1 = 'X'.   " 出库通知单
    name_sf = 'ZSD018_SF_2'.
  ELSEIF type_2 = 'X'. " 入库通知单
    name_sf = 'ZSD018_SF_1'.
  ELSE.
    name_sf = 'ZSD018_SF'.
  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = name_sf
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  "打印设置
  control_parameters-no_open   = 'X'.
  control_parameters-no_close  = 'X'.
*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  output-tddest = 'LP01'.
*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
  output-rqposname = ''.
  output-tddataset = ''.
  output-tdsuffix1 = ''.
  output-tdsuffix2 = ''.
  output-tdimmed   = 'X'.
  output-tddelete  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control_parameters
      output_options     = output
*    IMPORTING
*     JOB_OUTPUT_OPTIONS = OPTION
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.


***-------------   往打印内表传值   --------------***

  LOOP AT gt_tab_sf INTO gs_tab_sf .
    gc_count = 0.
    gs_tab_3-vbeln = gs_tab_sf-vbeln.
    gs_tab_3-ztext = gs_tab_sf-ztext.
    gs_tab_3-name1 = gs_tab_sf-name1.
    gs_tab_3-vgbel = gs_tab_sf-vgbel.
    gs_tab_3-bstnk = gs_tab_sf-bstnk.
    gs_tab_3-vdatu = gs_tab_sf-vdatu.
    AT NEW vgbel.
      zzs_tab-vbeln = gs_tab_3-vbeln.          " 交货单编号  KEY *
      zzs_tab-ztext = gs_tab_3-ztext.          " 项目名称  SUB
      zzs_tab-name1 = gs_tab_3-name1.          " 客户名称
      zzs_tab-vgbel = gs_tab_3-vgbel.          " 订单编号 .
      zzs_tab-vdatu = gs_tab_3-vdatu.          " 计划交货时间 .
      zzs_tab-bstnk = gs_tab_3-bstnk. " 合同编号
    ENDAT.
    AT END OF vgbel.
      LOOP AT gt_tab_2 INTO gs_tab_2 WHERE box = 'X'.
        gc_count = gc_count + 1.

        gs_tab_print-maktx = gs_tab_2-maktx .          " 物料名称
        gs_tab_print-lgort = gs_tab_2-lgort .          " 库位
        gs_tab_print-lgobe = gs_tab_2-lgobe .          " 库位描述

        IF type1 = 'X'.    " 项目，产品销售（包含公司间）
          gs_tab_print-matnr = gs_tab_2-matnr1.          " 物料编码1 .
          gs_tab_print-lfimg = gs_tab_2-lfimg .          " 数量1 .
          gs_tab_print-meins = gs_tab_2-meins1.          " 单位1 .
          gs_tab_print-charg = gs_tab_2-charg1.          " 批次1 .
          gs_tab_print-kdmat = gs_tab_2-kdmat.          " 备注 .

*****  采购订单号表头长文本   *****
          l_name  = gs_tab_2-matnr1.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'BEST'
              language                = '1'
              name                    = l_name
              object                  = 'MATERIAL'
            TABLES
              lines                   = gt_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF gt_text IS NOT INITIAL.
            LOOP AT gt_text INTO gs_text.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
              CONCATENATE gs_tab_print-zstan gs_text-tdline  INTO gs_tab_print-zstan.
            ENDLOOP.
          ENDIF.

        ELSEIF type2 = 'X'. "  租赁，借料(非关联方)
          gs_tab_print-matnr = gs_tab_2-matnr2.          " 物料编码2 .
          gs_tab_print-lfimg = gs_tab_2-kwmeng.          " 数量2 .
          gs_tab_print-meins = gs_tab_2-meins2.          " 单位2 .
          gs_tab_print-charg = gs_tab_2-charg2.          " 批次2 .
          gs_tab_print-kdmat = gs_tab_2-kdmat.          " 备注 .

*****  采购订单号表头长文本   *****
          l_name  = gs_tab_2-matnr2.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'BEST'
              language                = '1'
              name                    = l_name
              object                  = 'MATERIAL'
            TABLES
              lines                   = gt_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF gt_text IS NOT INITIAL.
            LOOP AT gt_text INTO gs_text.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
              CONCATENATE gs_tab_print-zstan gs_text-tdline  INTO gs_tab_print-zstan.
            ENDLOOP.
          ENDIF.

        ELSEIF type3 = 'X'.  " 借料(关联方)
          gs_tab_print-matnr = gs_tab_2-matnr3.          " 物料编码3 .
          gs_tab_print-lfimg = gs_tab_2-menge .          " 数量3 .
          gs_tab_print-meins = gs_tab_2-meins3.          " 单位3 .
          gs_tab_print-kdmat = gs_tab_2-kdmat.          " 备注 .
          gs_tab_print-charg = gs_tab_2-charg2.          " 批次2 .
*****  采购订单号表头长文本   *****
          l_name  = gs_tab_2-matnr3.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'BEST'
              language                = '1'
              name                    = l_name
              object                  = 'MATERIAL'
            TABLES
              lines                   = gt_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF gt_text IS NOT INITIAL.
            LOOP AT gt_text INTO gs_text.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
              CONCATENATE gs_tab_print-zstan gs_text-tdline  INTO gs_tab_print-zstan.
            ENDLOOP.
          ENDIF.
        ELSEIF type4 = 'X'.  " 销售订单
          gs_tab_print-matnr = gs_tab_2-matnr2.          " 物料编码2 .
          gs_tab_print-lfimg = gs_tab_2-kwmeng.          " 数量2 .
          gs_tab_print-meins = gs_tab_2-meins2.          " 单位2 .
          gs_tab_print-charg = gs_tab_2-charg2.          " 批次2 .
          gs_tab_print-kdmat = gs_tab_2-kdmat.          " 备注

*****  采购订单号表头长文本   *****
          l_name  = gs_tab_2-matnr2.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'BEST'
              language                = '1'
              name                    = l_name
              object                  = 'MATERIAL'
            TABLES
              lines                   = gt_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF gt_text IS NOT INITIAL.
            LOOP AT gt_text INTO gs_text.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
              CONCATENATE gs_tab_print-zstan gs_text-tdline  INTO gs_tab_print-zstan.
            ENDLOOP.
          ENDIF.
        ENDIF.

        APPEND gs_tab_print TO gt_tab_print.
        CLEAR gs_tab_print.
        CLEAR gs_tab_1.
        CLEAR gt_text.
      ENDLOOP.


**判断空行
*    gc_count = gc_count MOD gc_num.
*    IF gc_count NE 0.
*      gc_count = gc_num - gc_count.   " 空行数
*      CLEAR gs_tab_print.
*      DO gc_count TIMES.
*        APPEND gs_tab_print TO gt_tab_print.
*      ENDDO.
*    ENDIF.

*调用Smartforms的Function Module打印
      CALL FUNCTION fm_name
        EXPORTING
          control_parameters = control_parameters
          output_options     = output
          gs_head            = zzs_tab
          gc_num             = gc_num
          gc_type            = gc_type
        TABLES
          gt_item            = gt_tab_print
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4.
*      IF SY-SUBRC NE 0.
**&--代码添加 BY HANDYBY 11.07.2017 14:16:01  BEGIN
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**&--代码添加 BY HANDYBY 11.07.2017 14:16:01  END
*      ENDIF.
      CLEAR gt_tab_print. " 清空表数据内表
    ENDAT.
  ENDLOOP.

  CLEAR gt_tab_sf. " 打印一次清理BOX选中后传入的临时内表

  "#  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = lw_ssfcrescl
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " SETSMART
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  DATA l_msg TYPE string .

  IF type1 = 'X'.

    LOOP AT s_vkorg.
      AUTHORITY-CHECK OBJECT 'Z_VKORG'
              ID 'ZVKORG' FIELD s_vkorg-low .
      IF sy-subrc NE 0 .
        CONCATENATE '你没权限看销售组织' s_vkorg-low '的数据' INTO l_msg .
        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E' .
        LEAVE LIST-PROCESSING .
      ENDIF.
    ENDLOOP.

  ELSEIF type2 = 'X'.

    LOOP AT s_vkorg2.
      AUTHORITY-CHECK OBJECT 'Z_VKORG'
              ID 'ZVKORG' FIELD s_vkorg2-low .
      IF sy-subrc NE 0 .
        CONCATENATE '你没权限看销售组织' s_vkorg2-low '的数据' INTO l_msg .
        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E' .
        LEAVE LIST-PROCESSING .
      ENDIF.
    ENDLOOP.

  ELSEIF type3 = 'X'.

*    LOOP AT S_VKORG.
*      AUTHORITY-CHECK OBJECT 'Z_VKORG'
*              ID 'ZVKORG' FIELD S_VKORG-LOW .
*      IF SY-SUBRC NE 0 .
*        CONCATENATE '你没权限看销售组织' S_VKORG-LOW '的数据' INTO L_MSG .
*        MESSAGE L_MSG TYPE 'S' DISPLAY LIKE 'E' .
*        LEAVE LIST-PROCESSING .
*      ENDIF.
*    ENDLOOP.

  ELSEIF type4 = 'X'.

    LOOP AT s_vkorg4.
      AUTHORITY-CHECK OBJECT 'Z_VKORG'
              ID 'ZVKORG' FIELD s_vkorg4-low .
      IF sy-subrc NE 0 .
        CONCATENATE '你没权限看销售组织' s_vkorg4-low '的数据' INTO l_msg .
        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E' .
        LEAVE LIST-PROCESSING .
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
