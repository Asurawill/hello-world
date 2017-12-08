*----------------------------------------------------------------------*
*  程序名称         : ZPS002_01
*  创建者           : 吴丽娟
*  创建日期         : 2015-10-28
*----------------------------------------------------------------------*
*  概要说明
* ECP批导
*----------------------------------------------------------------------*
* modify  check 只允许一个项目导入   it02 20160229 add
"原中间删除0和100版本数据现更改为：先删除成功后，再重新创建0版本和100版本数据

REPORT ZPS002_01.

TYPES:icon,slis.

TYPES:BEGIN OF ty_ch_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        hpeinh     TYPE menge_pos, "ckis-peinh,              "合同工程量
        hgpreis    TYPE ckis-gpreis,             "合同单价
        jpeinh     TYPE menge_pos,              "计划工程量
        cgpreis    TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis     TYPE ckis-fpreis,             "人工单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_ch_source.



TYPES:BEGIN OF ty_h_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        hpeinh     TYPE menge_pos,              "合同工程量
        hgpreis    TYPE ckis-gpreis,             "合同单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_h_source.

TYPES:BEGIN OF ty_c_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        jpeinh     TYPE menge_pos, "CKIS-PEINH,    "计划工程量
        cgpreis    TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis     TYPE ckis-fpreis,             "人工单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_c_source.

TYPES:BEGIN OF ty_ch_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        hpeinh       TYPE menge_pos,              "合同工程量
        hgpreis      TYPE ckis-gpreis,             "合同单价
        jpeinh       TYPE menge_pos,              "计划工程量
        cgpreis      TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis       TYPE ckis-fpreis,             "人工单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_ch_output.

TYPES:BEGIN OF ty_h_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        hpeinh       TYPE menge_pos,              "合同工程量
        hgpreis      TYPE ckis-gpreis,             "合同单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_h_output.

TYPES:BEGIN OF ty_c_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        jpeinh       TYPE menge_pos,              "计划工程量
        cgpreis      TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis       TYPE ckis-fpreis,             "人工单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_c_output.
TYPES:BEGIN OF ty_chz_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        hpeinh     TYPE menge_pos,              "合同工程量
        hgpreis    TYPE ckis-gpreis,             "合同单价
        jpeinh     TYPE menge_pos,              "计划工程量
        cgpreis    TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis     TYPE ckis-fpreis,             "人工单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_chz_source.

TYPES:BEGIN OF ty_hz_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        hpeinh     TYPE menge_pos,              "合同工程量
        hgpreis    TYPE ckis-gpreis,             "合同单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_hz_source.

TYPES:BEGIN OF ty_cz_source,
        xh         TYPE int4,                    "序号
        posid      TYPE prps-posid,              "WBS
        xmlb(1)    TYPE c,                       "项目类别
        werks      TYPE t001w-werks,             "工厂
        matnr      TYPE ckis-matnr,              "物料编码
        maktx      TYPE makt-maktx,              "物料描述
        jpeinh     TYPE menge_pos,              "计划工程量
        cgpreis    TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis     TYPE ckis-fpreis,             "人工单价
        cbys       TYPE ckis-kstar,              "成本要素
        cbysmc(40) TYPE c,                       "成本要素名称
      END OF ty_cz_source.

TYPES:BEGIN OF ty_chz_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        hpeinh       TYPE menge_pos,              "合同工程量
        hgpreis      TYPE ckis-gpreis,             "合同单价
        jpeinh       TYPE menge_pos,              "计划工程量
        cgpreis      TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis       TYPE ckis-fpreis,             "人工单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_chz_output.

TYPES:BEGIN OF ty_hz_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        hpeinh       TYPE menge_pos,              "合同工程量
        hgpreis      TYPE ckis-gpreis,             "合同单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_hz_output.

TYPES:BEGIN OF ty_cz_output,
        xh           TYPE int4,                    "序号
        posid        TYPE prps-posid,              "WBS
        xmlb(1)      TYPE c,                       "项目类别
        werks        TYPE t001w-werks,             "工厂
        matnr        TYPE ckis-matnr,              "物料编码
        maktx        TYPE makt-maktx,              "物料描述
        jpeinh       TYPE menge_pos,              "计划工程量
        cgpreis      TYPE p LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        fpreis       TYPE ckis-fpreis,             "人工单价
        cbys         TYPE ckis-kstar,              "成本要素
        cbysmc(40)   TYPE c,                       "成本要素名称
        post1        TYPE proj-post1,
        vernr        TYPE proj-vernr,
        vbukr        TYPE proj-vbukr,
        kalid        TYPE proj-kalid,
        zteht        TYPE proj-zteht,
        type(10)     TYPE c,                       "状态
        message(400) TYPE c,                       "消息
      END OF ty_cz_output.
TYPES:BEGIN OF ty_versn,
        versn TYPE precp1-versn,
      END OF ty_versn.
TYPES:BEGIN OF ty_objid,
        topnr TYPE precp1-topnr,
        versn TYPE precp1-versn,
      END OF ty_objid.
TYPES:BEGIN OF ty_meins,
        meins TYPE mara-meins,
      END OF ty_meins.
TYPES:BEGIN OF ty_psphi,
        psphi TYPE prps-psphi,
      END OF ty_psphi.
TYPES:BEGIN OF ty_objnr,
        objnr TYPE proj-objnr,
      END OF ty_objnr.
TYPES:BEGIN OF ty_kalnr,
        subnr TYPE j_objnr,
        versn TYPE versn,
        kalnr TYPE precp2-kalnr,
        topnr TYPE j_objnr,
      END OF ty_kalnr.
TYPES:BEGIN OF ty_htgcl,
        matnr  TYPE ckis-matnr,              "物料编码
        hpeinh TYPE menge_pos,              "合同工程量
      END OF ty_htgcl.
TYPES:BEGIN OF ty_jhgcl,
        matnr  TYPE ckis-matnr,              "物料编码
        jpeinh TYPE menge_pos,              "计划工程量
      END OF ty_jhgcl.
TYPES:BEGIN OF ty_hb,
        matnr  TYPE ckis-matnr,              "物料编码
        hpeinh TYPE menge_pos,              "合同工程量
        jpeinh TYPE menge_pos,              "计划工程量
      END OF ty_hb.
TYPES:BEGIN OF ty_jg,
        matnr TYPE ckis-matnr,
        htdj  TYPE ckis-gpreis,
        cldj  TYPE p LENGTH 16 DECIMALS 4,
        rgdj  TYPE ckis-fpreis,
      END OF ty_jg.
TYPES:BEGIN OF ty_jghj,
        matnr   TYPE ckis-matnr,
        hgpreis TYPE ckis-gpreis,
        cgpreis TYPE p LENGTH 16 DECIMALS 4,
        fpreis  TYPE ckis-fpreis,
      END OF ty_jghj.
TYPES:BEGIN OF ty_jg1,
        matnr TYPE ckis-matnr,
        cldj  TYPE p LENGTH 16 DECIMALS 4,
        rgdj  TYPE ckis-fpreis,
      END OF ty_jg1.
TYPES:BEGIN OF ty_jghj1,
        matnr   TYPE ckis-matnr,
        cgpreis TYPE p LENGTH 16 DECIMALS 4,
        fpreis  TYPE ckis-fpreis,
      END OF ty_jghj1.
TYPES:BEGIN OF ty_jg2,
        matnr TYPE ckis-matnr,
        htdj  TYPE ckis-gpreis,
        rgdj  TYPE ckis-fpreis,
      END OF ty_jg2.
TYPES:BEGIN OF ty_jghj2,
        matnr   TYPE ckis-matnr,
        hgpreis TYPE ckis-gpreis,
        fpreis  TYPE ckis-fpreis,
      END OF ty_jghj2.
TYPES:BEGIN OF ty_check,
        posid   TYPE prps-posid,              "WBS
        xmlb(1) TYPE c,                       "项目类别
        cbys    TYPE ckis-kstar,              "成本要素
      END OF ty_check.

TYPES:BEGIN OF ty_check_matnr  ,
        matnr TYPE matnr,
        meins TYPE meins,
      END OF ty_check_matnr .

DATA:it_jg TYPE TABLE OF ty_jg,
     wa_jg TYPE ty_jg.
DATA:it_jg1 TYPE TABLE OF ty_jg1,
     wa_jg1 TYPE ty_jg1.
DATA:it_jg2 TYPE TABLE OF ty_jg2,
     wa_jg2 TYPE ty_jg2.

DATA:it_jghj TYPE TABLE OF ty_jghj,
     wa_jghj TYPE ty_jghj.
DATA:it_jghj1 TYPE TABLE OF ty_jghj1,
     wa_jghj1 TYPE ty_jghj1.
DATA:it_jghj2 TYPE TABLE OF ty_jghj2,
     wa_jghj2 TYPE ty_jghj2.

DATA:gt_htgcl TYPE TABLE OF ty_htgcl,
     gs_htgcl TYPE ty_htgcl.

DATA:gt_jhgcl TYPE TABLE OF ty_jhgcl,
     gs_jhgcl TYPE ty_jhgcl.

DATA:gt_hb TYPE TABLE OF ty_hb,
     gs_hb TYPE ty_hb.

DATA:gt_check_matnr TYPE TABLE OF ty_check_matnr,
     gs_check_matnr TYPE ty_check_matnr.

DATA:g(50)  TYPE c,
     w(50)  TYPE c,
     ll(50) TYPE c,
     jj(50) TYPE c.
DATA:message1(200) TYPE c,
     message2(200) TYPE c,
     message3(200) TYPE c.
DATA:  gt_ch_source      TYPE TABLE OF ty_ch_source WITH HEADER LINE.
DATA:gt_h_source       TYPE TABLE OF ty_h_source WITH HEADER LINE.
DATA:gt_c_source       TYPE TABLE OF ty_c_source WITH HEADER LINE.
DATA:gt_ch_output      TYPE TABLE OF ty_ch_output WITH HEADER LINE.
DATA:gt_ch_output1     TYPE TABLE OF ty_ch_output WITH HEADER LINE.
DATA:gt_h_output       TYPE TABLE OF ty_h_output WITH HEADER LINE.
DATA:gt_h_output1      TYPE TABLE OF ty_h_output WITH HEADER LINE.
DATA:gt_c_output       TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:gt_c_output1      TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:gt_chz_source     TYPE TABLE OF ty_ch_source WITH HEADER LINE.
DATA:gt_hz_source      TYPE TABLE OF ty_h_source WITH HEADER LINE.
DATA:gt_cz_source      TYPE TABLE OF ty_c_source WITH HEADER LINE.
DATA:gt_chz_output     TYPE TABLE OF ty_ch_output WITH HEADER LINE.
DATA:gs_chz_output     TYPE ty_ch_output .
DATA:gt_chz_output1    TYPE TABLE OF ty_ch_output WITH HEADER LINE.
DATA:gt_hz_output      TYPE TABLE OF ty_h_output WITH HEADER LINE.
DATA:gs_hz_output      type ty_h_output.
DATA:gt_hz_output1     TYPE TABLE OF ty_h_output WITH HEADER LINE.
DATA:gt_cz_output      TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:gt_cz_output1     TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:gt_return         TYPE TABLE OF bapiret2.                            "声明返回消息的内表和工作区
DATA:gt_return1        TYPE TABLE OF bapiret2.                           "声明返回消息的内表和工作区
DATA:gt_return2        TYPE TABLE OF bapiret2.                           "声明返回消息的内表和工作区
DATA:gt_return3        TYPE TABLE OF bapiret2.                           "声明返回消息的内表和工作区
DATA:gt_return4        TYPE TABLE OF bapiret2.                           "声明返回消息的内表和工作区
DATA:gt_return5        TYPE TABLE OF bapiret2.                           "声明返回消息的内表和工作区
DATA:gs_return         TYPE bapiret2.
DATA:gs_return1        TYPE bapiret2.
DATA:gs_return2        TYPE bapiret2.
DATA:gs_return3        TYPE bapiret2.
DATA:gs_return4        TYPE bapiret2.
DATA:gs_return5        TYPE bapiret2.
DATA:p_objid(20)       TYPE c.
DATA:ls_item           TYPE bapickecp_item.                               "定义BAPI中用到的结构和内表
DATA:ls_item1          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_item2          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_item3          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_item4          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_item5          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_item6          TYPE bapickecp_item.                              "定义BAPI中用到的结构和内表
DATA:ls_wbs_costlines  TYPE proj_element_ck_items.
DATA:ls_wbs_costlines1 TYPE proj_element_ck_items.
DATA:ls_wbs_costlines2 TYPE proj_element_ck_items.
DATA:ls_wbs_costlines3 TYPE proj_element_ck_items.
DATA:ls_wbs_costlines4 TYPE proj_element_ck_items.
DATA:ls_wbs_costlines5 TYPE proj_element_ck_items.
DATA:ls_wbs_costlines6 TYPE proj_element_ck_items.
DATA:lt_wbs_costlines  TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines1 TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines2 TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines3 TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines4 TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines5 TYPE tty_proj_element_ck_items.
DATA:lt_wbs_costlines6 TYPE tty_proj_element_ck_items.
DATA:gt_versn1         TYPE TABLE OF ty_versn WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:gt_versn2         TYPE TABLE OF ty_versn WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:gt_versn3         TYPE TABLE OF ty_versn WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:gt_objid          TYPE TABLE OF ty_objid WITH HEADER LINE.
DATA:gt_objid1         TYPE TABLE OF ty_objid WITH HEADER LINE.
DATA:gt_objid2         TYPE TABLE OF ty_objid WITH HEADER LINE.
DATA:gt_meins          TYPE TABLE OF ty_meins WITH HEADER LINE.
DATA:gt_psphi          TYPE TABLE OF ty_psphi WITH HEADER LINE.
DATA:it_bdcdata TYPE STANDARD TABLE OF bdcdata.
DATA:wa_bdcdata TYPE bdcdata.
DATA:it_message TYPE STANDARD TABLE OF bdcmsgcoll.
DATA:wa_message TYPE bdcmsgcoll.
DATA:it_proj TYPE TABLE OF proj.
DATA:wa_proj TYPE proj.
DATA:it_kalnr TYPE TABLE OF ty_kalnr.
DATA:it_kalnr1 TYPE TABLE OF ty_kalnr.
DATA:wa_kalnr TYPE ty_kalnr.
DATA:wa_kalnr2 TYPE ty_kalnr.
DATA:it_objnr TYPE TABLE OF ty_objnr.
DATA:wa_objnr TYPE ty_objnr.
DATA:it_ckis TYPE TABLE OF ckis.
DATA:it_ckis1 TYPE TABLE OF ckis.
DATA:wa_ckis TYPE ckis.
DATA:wa_ckis1 TYPE ckis.
DATA:js TYPE int4 VALUE '0'.
DATA:l(50) TYPE c.
DATA:j(50) TYPE c.

DATA:sc_len TYPE i.      "成功导入预算行数
DATA:sc_img TYPE string ."成功执行预算消息
DATA:l1 TYPE i,       "0版本导入数量
     l2 TYPE i,       "100版本导入数量
     l3 TYPE i,       "0版本导入最后数量
     l4 TYPE i.       "100版本导入最后数量
"DATA:gt_c_output_check       TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:proj_objnr TYPE j_objnr , "项目对象号
     prps_objnr TYPE j_objnr . "WBS对象号

DATA:msg000(50) TYPE c ,   "000版本消息通知
     msg001(50) TYPE  c,    "000后最新版本消息通知
     msg100(50) TYPE  c,    "100版本消息通知
     msg101(50) TYPE  c,   "100后最新版本消息通知
     ermsg      TYPE string.  "错误消息

DATA:e1_msg TYPE string,
     e2_msg TYPE string.

DATA:gt_c_output_check TYPE TABLE OF ty_check WITH HEADER LINE.
"    gs_c_output_check TYPE TY_CHECK.



DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

DATA:gt_fieldcat       TYPE lvc_t_fcat.                                   "ALV定义
DATA:gs_fieldcat       TYPE lvc_s_fcat.
DATA:gs_layout         TYPE lvc_s_layo.

SELECTION-SCREEN PUSHBUTTON 2(13) but1 USER-COMMAND cmd2 MODIF ID 11.    "模板下载
PARAMETERS:p_upc   TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',               "上传成本预算
           p_uph   TYPE c RADIOBUTTON GROUP g1,                           "上传合同报价
           p_upch  TYPE c RADIOBUTTON GROUP g1,                           "上传成本预算和合同报价
           p_upcz  TYPE c RADIOBUTTON GROUP g1,                           "上传成本预算
           p_uphz  TYPE c RADIOBUTTON GROUP g1,                           "上传合同报价
           p_upchz TYPE c RADIOBUTTON GROUP g1,                           "上传成本预算和合同报价
           p_path  LIKE rlgrap-filename MEMORY ID fil MODIF ID 11.        "上传文件路径

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM frm_get_filename.                                              "选择文件，获取文件名，返回文件名

INITIALIZATION.
  CALL FUNCTION 'ICON_CREATE'                                            "调用模板下载按钮的图片
    EXPORTING
      name   = icon_export
      text   = '模板下载'
    IMPORTING
      result = but1
    EXCEPTIONS
      OTHERS = 0.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CMD2'.                                                         "模板下载
      PERFORM download_tpt USING text-001
                                 p_objid
                           CHANGING p_path.
    WHEN OTHERS.
  ENDCASE.

START-OF-SELECTION.
  IF p_path IS NOT INITIAL.
    PERFORM frm_upload.     "导入文件
    " add it02 20160229-有预算值导入成功消息提示 begin
*    IF SC_LEN >= 1 .
*      READ TABLE gt_c_output_check   index 1.
*      IF SY-SUBRC EQ 0 .
*        CLEAR:SC_IMG.
*        SC_IMG = SC_LEN.
*        CONCATENATE '项目:' gt_c_output_check-posid '已成功导入' SC_IMG '条预算行数' INTO SC_IMG .
*        MESSAGE SC_IMG  TYPE 'I'.
*       ENDIF.
*     ENDIF.
    " add it02 20160229-有预算值导入成功消息提示 end
    PERFORM frm_fieldcat.                                                  "字段名
    PERFORM frm_layout.                                                    "布局
    IF p_upc = 'X'.
      PERFORM frm_alv_report TABLES gt_c_output.                           "成本预算的ALV报表显示
    ELSEIF p_uph = 'X'.
      PERFORM frm_alv_report TABLES gt_h_output.                          "合同报价的ALV报表显示
    ELSEIF p_upch = 'X'.
      PERFORM frm_alv_report TABLES gt_ch_output.                         "成本预算和合同报价的ALV报表显示
    ELSEIF p_upcz = 'X'.
      PERFORM frm_alv_report TABLES gt_cz_output.                           "成本预算的ALV报表显示
    ELSEIF p_uphz = 'X'.
      PERFORM frm_alv_report TABLES gt_hz_output.                          "合同报价的ALV报表显示
    ELSEIF p_upchz = 'X'.
      PERFORM frm_alv_report TABLES gt_chz_output.                         "成本预算和合同报价的ALV报表显示
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_filename .                                                  "获取上传文件的路径
  DATA: lw_rc    TYPE i,
        lw_user  TYPE i,
        lit_file TYPE filetable,
        liw_file TYPE file_table.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = '选择文件'
      file_filter             = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
    CHANGING
      file_table              = lit_file
      rc                      = lw_rc
      user_action             = lw_user
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CHECK lw_rc = 1
      AND lw_user <> 9.
    READ TABLE lit_file INTO liw_file INDEX 1.
    p_path = liw_file-filename.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_001  text
*      -->P_P_OBJID  text
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM download_tpt   USING    p_text
                   VALUE(p_objid)
                   CHANGING pv_file.
  IF p_upc = 'X'.                                                        "下载成本预算批导模板
    p_objid = 'ZUPC'.
  ELSEIF p_uph = 'X'.                                                    "下载合同报价金额批导模板
    p_objid = 'ZUPH'.
  ELSEIF p_upch = 'X'.                                                   "下载成本预算和合同报价批导模板
    p_objid = 'ZUPCH'.
  ELSEIF p_upcz = 'X'.                                                        "下载成本预算批导模板
    p_objid = 'ZUPCZ'.
  ELSEIF p_uphz = 'X'.                                                    "下载合同报价金额批导模板
    p_objid = 'ZUPHZ'.
  ELSEIF p_upchz = 'X'.                                                   "下载成本预算和合同报价批导模板
    p_objid = 'ZUPCHZ'.
  ENDIF.

  DATA: lv_fname TYPE string,
        lv_title TYPE string,
        lv_path  TYPE string VALUE 'D:/',
        lv_fpath TYPE string VALUE 'D:/'.

  DATA: ls_wdatb   LIKE wwwdatatab.
  DATA: lv_subrc   TYPE sy-subrc.
  DATA: gv_msg TYPE string .

  lv_fname = p_text.

  CONCATENATE p_text '下载' INTO lv_title.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = lv_title
      default_extension    = 'XLS'
      default_file_name    = lv_fname
      initial_directory    = 'D:\'
      file_filter          = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
      prompt_on_overwrite  = 'X'
    CHANGING
      filename             = lv_fname
      path                 = lv_path
      fullpath             = lv_fpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    SELECT SINGLE relid
                  objid
      FROM wwwdata
      INTO CORRESPONDING FIELDS OF ls_wdatb
      WHERE srtf2 = 0
      AND relid = 'MI'
      AND objid = p_objid.                        "p_objid就是传入模板的参数
    IF ls_wdatb IS INITIAL.
      MESSAGE '模板文件不存在！' TYPE 'E'.
    ELSE.
      pv_file = lv_fpath.
      IF pv_file IS NOT INITIAL.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            key         = ls_wdatb
            destination = pv_file
          IMPORTING
            rc          = lv_subrc.
        IF lv_subrc NE 0.
          MESSAGE '模板下载失败！' TYPE 'E'.
        ELSE.
          CLEAR gv_msg.
          CONCATENATE '模板下载到本地文件' pv_file INTO gv_msg.
          MESSAGE gv_msg TYPE 'S' .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_upload .
    "取出相应的1800 工厂下的物料新
  SELECT a~matnr b~meins
    INTO CORRESPONDING FIELDS OF TABLE gt_check_matnr
    FROM marc AS a
    INNER JOIN mara AS b
    ON a~matnr = b~matnr
    WHERE a~werks = '1800'.
  SORT gt_check_matnr BY matnr .
  IF p_upc = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_c_source.                           "成本预算的导入
    PERFORM frm_bapi_gt_c_output.                                        "成本预算的BAPI
  ELSEIF p_uph = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_h_source.                          "合同报价的导入
    PERFORM frm_bapi_gt_h_output.                                       "合同报价的BAPI
  ELSEIF p_upch = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_ch_source.                         "成本预算和合同报价的导入
    PERFORM frm_bapi_gt_ch_output.                                      "成本预算和合同报价的BAPI
  ELSEIF p_upcz = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_cz_source.                           "成本预算的导入
    PERFORM frm_bapi_gt_cz_output.                                        "成本预算的BAPI
  ELSEIF p_uphz = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_hz_source.                          "合同报价的导入
    PERFORM frm_bapi_gt_hz_output.                                       "合同报价的BAPI
  ELSEIF p_upchz = 'X'.
    PERFORM frm_xls_to_sap TABLES gt_chz_source.                         "成本预算和合同报价的导入
    PERFORM frm_bapi_gt_chz_output.                                      "成本预算和合同报价的BAPI
  ENDIF.
  "统计最后版本导入数量

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  IF p_upc = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF p_uph = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF p_upch = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF p_upcz = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF p_uphz = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF p_upchz = 'X'.
    PERFORM frm_field_cat USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_layout .
  gs_layout-cwidth_opt = 'X'.
  gs_layout-zebra = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_C_SOURCE  text
*----------------------------------------------------------------------*
FORM frm_alv_report TABLES t_gt_output.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'STANDARD_FULLSCREEN '
*     I_CALLBACK_USER_COMMAND  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = t_gt_output
* EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_C_SOURCE  text
*----------------------------------------------------------------------*
FORM frm_xls_to_sap TABLES p_gs_source.

  DATA:dr_len TYPE i.
  REFRESH gt_c_output_check[].
  REFRESH p_gs_source[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_path
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 256
      i_end_row               = 65000
    TABLES
      intern                  = p_gs_source[]
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN  '1'.
        MESSAGE '存在错误的传入参数' TYPE 'E'.
      WHEN  '2'.
        MESSAGE 'OLE控件错误，请检查EXCEL插件及系统' TYPE 'E'.
      WHEN  '3'.
        MESSAGE '数据上载出错' TYPE 'E'.
      WHEN OTHERS.
    ENDCASE.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH:gt_c_output,gt_h_output,gt_ch_output,
          gt_cz_output,gt_hz_output,gt_chz_output.
  "add it02 begin check 导入模板几个项目
  MOVE-CORRESPONDING p_gs_source[] TO gt_c_output_check[] .
  SORT gt_c_output_check BY posid .
  DELETE ADJACENT DUPLICATES FROM gt_c_output_check  COMPARING posid.
  DESCRIBE TABLE gt_c_output_check LINES dr_len .
  IF dr_len > 1 .
    MESSAGE '只允许导入一个项目的预算值，请检查！' TYPE 'E' .
  ENDIF.
  MOVE-CORRESPONDING p_gs_source[] TO gt_c_output_check[] .
  DELETE gt_c_output_check WHERE xmlb = 'M'.
  SORT gt_c_output_check BY cbys .
  CLEAR :dr_len .
  LOOP AT gt_c_output_check WHERE xmlb = 'V'.
    CLEAR:dr_len .
    LOOP AT gt_c_output_check WHERE cbys = gt_c_output_check-cbys .
      dr_len = dr_len + 1 .

    ENDLOOP.
    IF dr_len > 1.
      MESSAGE 'V类型只允许导入一个成本要素的预算值或模板错误，请检查！' TYPE 'E' .
      EXIT.
    ENDIF.
  ENDLOOP.

  " add it02 20160229 end
  IF p_upc = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_c_output.
      gt_c_output-type = icon_light_out.
      APPEND gt_c_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_c_output
    WHERE pspid = gt_c_output-posid.

    LOOP AT gt_c_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_c_output-post1 = wa_proj-post1.
      gt_c_output-vernr = wa_proj-vernr.
      gt_c_output-vbukr = wa_proj-vbukr.
      gt_c_output-kalid = wa_proj-kalid.
      gt_c_output-zteht = wa_proj-zteht.
      MODIFY gt_c_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_c_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF p_uph = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_h_output.
      gt_h_output-type = icon_light_out.
      APPEND gt_h_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_h_output
    WHERE pspid = gt_h_output-posid.
    LOOP AT gt_h_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_h_output-post1 = wa_proj-post1.
      gt_h_output-vernr = wa_proj-vernr.
      gt_h_output-vbukr = wa_proj-vbukr.
      gt_h_output-kalid = wa_proj-kalid.
      gt_h_output-zteht = wa_proj-zteht.
      MODIFY gt_h_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_h_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF p_upch = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_ch_output.
      gt_ch_output-type = icon_light_out.
      APPEND gt_ch_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_ch_output
    WHERE pspid = gt_ch_output-posid.

    LOOP AT gt_ch_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_ch_output-post1 = wa_proj-post1.
      gt_ch_output-vernr = wa_proj-vernr.
      gt_ch_output-vbukr = wa_proj-vbukr.
      gt_ch_output-kalid = wa_proj-kalid.
      gt_ch_output-zteht = wa_proj-zteht.
      MODIFY gt_ch_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_ch_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF p_upcz = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_cz_output.
      gt_cz_output-type = icon_light_out.
      APPEND gt_cz_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_cz_output
    WHERE pspid = gt_cz_output-posid.

    LOOP AT gt_cz_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_cz_output-post1 = wa_proj-post1.
      gt_cz_output-vernr = wa_proj-vernr.
      gt_cz_output-vbukr = wa_proj-vbukr.
      gt_cz_output-kalid = wa_proj-kalid.
      gt_cz_output-zteht = wa_proj-zteht.
      MODIFY gt_cz_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_cz_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF p_uphz = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_hz_output.
      gt_hz_output-type = icon_light_out.
      APPEND gt_hz_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_hz_output
    WHERE pspid = gt_hz_output-posid.

    LOOP AT gt_hz_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_hz_output-post1 = wa_proj-post1.
      gt_hz_output-vernr = wa_proj-vernr.
      gt_hz_output-vbukr = wa_proj-vbukr.
      gt_hz_output-kalid = wa_proj-kalid.
      gt_hz_output-zteht = wa_proj-zteht.
      MODIFY gt_hz_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_hz_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF p_upchz = 'X'.
    LOOP AT p_gs_source.
      MOVE-CORRESPONDING p_gs_source TO gt_chz_output.
      gt_chz_output-type = icon_light_out.
      APPEND gt_chz_output.
    ENDLOOP.

    SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_proj
    FOR ALL ENTRIES IN gt_chz_output
    WHERE pspid = gt_chz_output-posid.

    LOOP AT gt_chz_output.
      READ TABLE it_proj INTO wa_proj INDEX 1.
      gt_chz_output-post1 = wa_proj-post1.
      gt_chz_output-vernr = wa_proj-vernr.
      gt_chz_output-vbukr = wa_proj-vbukr.
      gt_chz_output-kalid = wa_proj-kalid.
      gt_chz_output-zteht = wa_proj-zteht.
      MODIFY gt_chz_output.
      CLEAR wa_proj.
    ENDLOOP.
    IF gt_chz_output[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0852   text
*      -->P_0853   text
*      -->P_0854   text
*      -->P_0855   text
*----------------------------------------------------------------------*
FORM frm_field_cat  USING  pname
                           ptable
                           pfield
                           ptext.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = pname.
  gs_fieldcat-ref_table = ptable.
  gs_fieldcat-ref_field = pfield.
  gs_fieldcat-coltext = ptext.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_C_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_c_output.                                            "成本预算
  CLEAR :sc_len .
  SELECT psphi                                                        "取出了WBS元素对应的项目定义
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_c_output
    WHERE posid = gt_c_output-posid.

  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn1                        "取一个对象编号下对应的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid
        WHERE topnr = gt_objid-topnr
        AND versn BETWEEN '000' AND '099'.
    ENDIF.

  ELSE.
    MESSAGE 'WBS元素没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH:gt_objid,gt_psphi.

  DATA:p_bb1 TYPE precp1-versn.

  SORT gt_versn1 BY versn.

  IF gt_versn1[] IS NOT INITIAL.
    LOOP AT gt_versn1 WHERE versn >= '000' AND versn <= '099'.
*    if gt_versn1-versn is not initial.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn1-versn = gt_versn1-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn1-versn
          IMPORTING
            output = gt_versn1-versn.

        p_bb1 = gt_versn1-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb1 = '001'.
  ENDIF.

  SORT gt_c_output BY matnr.

  REFRESH it_jg1.
  LOOP AT gt_c_output.
    AT NEW matnr.
      READ TABLE gt_c_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg1-matnr = gt_c_output-matnr.
        wa_jg1-cldj = gt_c_output-cgpreis.
        wa_jg1-rgdj = gt_c_output-fpreis.
        APPEND wa_jg1 TO it_jg1.
      ENDIF.
    ENDAT.
    CLEAR gt_c_output.
  ENDLOOP.

  REFRESH it_jghj1.
  LOOP AT gt_c_output WHERE matnr <> ''.
    CLEAR wa_jghj.
    wa_jghj1-matnr = gt_c_output-matnr.
    wa_jghj1-cgpreis = gt_c_output-cgpreis.
    wa_jghj1-fpreis = gt_c_output-fpreis.
    COLLECT wa_jghj1 INTO it_jghj1.
  ENDLOOP.

  LOOP AT it_jg1 INTO wa_jg1.
    READ TABLE it_jghj1 INTO wa_jghj1 WITH KEY matnr = wa_jg1-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_c_output WHERE matnr = wa_jghj1-matnr.
        g = g + 1.
      ENDLOOP.
      ll = wa_jghj1-cgpreis / g.
      jj = wa_jghj1-fpreis / g.


      IF ll <> wa_jg1-cldj.
        CONCATENATE '物料' wa_jghj1-matnr '的材料单价不一致！' INTO message2.
        MESSAGE message2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF jj <> wa_jg1-rgdj.
        CONCATENATE '物料' wa_jghj1-matnr '的人工单价不一致！' INTO message3.
        MESSAGE message3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg1,w,ll,jj,message2,message3.
  ENDLOOP.

  DELETE gt_c_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_c_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_jhgcl.
  LOOP AT gt_c_output WHERE matnr <> ''.
    CLEAR gs_jhgcl.
    gs_jhgcl-matnr = gt_c_output-matnr.
    gs_jhgcl-jpeinh = gt_c_output-jpeinh.
    COLLECT gs_jhgcl INTO gt_jhgcl.
  ENDLOOP.

  LOOP AT gt_c_output WHERE matnr = ''.
    APPEND gt_c_output TO gt_c_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_c_output COMPARING matnr.

  DELETE gt_c_output WHERE matnr = ''.

  LOOP AT gt_c_output1.
    APPEND gt_c_output1 TO gt_c_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_c_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的将被导入一次，且将合计相同物料的计划工程量！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.
*  ........................add IT02 20160307begin ...................... *
  CLEAR:proj_objnr,prps_objnr.
  READ TABLE gt_c_output INDEX 1.
  IF sy-subrc EQ 0 .
    SELECT SINGLE  objnr
         INTO proj_objnr
         FROM proj
         WHERE pspid = gt_c_output-posid.
    SELECT SINGLE objnr
        INTO prps_objnr
        FROM  prps
        WHERE posid = gt_c_output-posid.
    IF proj_objnr NE '' AND prps_objnr NE ''.
      SELECT subnr versn  kalnr topnr
      FROM precp2
      INTO CORRESPONDING FIELDS OF TABLE it_kalnr
      WHERE topnr = proj_objnr
      AND subnr   = prps_objnr
      AND versn = '000'.
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.
    ENDIF.
    REFRESH: lt_wbs_costlines3,gt_return2,ls_wbs_costlines3-cost_lines.
    CLEAR:ls_wbs_costlines3,gs_return2.
    IF it_ckis IS NOT INITIAL.
      LOOP AT it_ckis INTO wa_ckis.
        CLEAR:ls_item3.
        AT FIRST .
          ls_wbs_costlines3-wbs_name = gt_c_output-posid.
        ENDAT.
        ls_item3-item_number = wa_ckis-posnr.
        ls_item3-flag_delete_item = 'X'.
        APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
        AT LAST .
          APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
        ENDAT.


      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除000版本
        EXPORTING
          i_proj_id       = gt_c_output-posid                           "项目定义
          i_version       = '000'                                         "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines3                            "内表
        IMPORTING
          messages        = gt_return2.

      READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
      IF sy-subrc EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ermsg .
        CONCATENATE '该项目' gt_c_output-posid  '删除0版本失败：' gs_return2-message  INTO ermsg .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ermsg  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  "再重新读取100版本的预算项目信息
  IF it_kalnr IS NOT INITIAL.
    SELECT *
         FROM ckis
         INTO CORRESPONDING FIELDS OF TABLE it_ckis
         FOR ALL ENTRIES IN it_kalnr
         WHERE kalnr = it_kalnr-kalnr.
  ENDIF.
*  ........................add IT02 20160307end ...................... *
  IF it_ckis IS INITIAL.
    CLEAR:l1,l2,l3,l4.   "初始导入版本数量
    LOOP AT gt_c_output.

      IF gt_c_output-posid IS NOT INITIAL.
        ls_wbs_costlines-wbs_name = gt_c_output-posid.                  "WBS
      ELSE.
        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.

      gt_c_output-type = icon_yellow_light.                             "状态

      ls_item-resource-typps = gt_c_output-xmlb.                        "项目类别

      IF gt_c_output-werks IS INITIAL.
        ls_item-resource-werks = 1800.                                  "工厂
      ELSE.
        ls_item-resource-werks = gt_c_output-werks.
      ENDIF.

      IF gt_c_output-xmlb = 'M'.
        IF gt_c_output-matnr IS NOT INITIAL.
          ls_item-resource-matnr = gt_c_output-matnr.                  "物料号
        ELSE.
          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-resource-matnr = gt_c_output-matnr.                    "物料编码
      ENDIF.

      REFRESH gt_meins[].
      CLEAR gt_meins.
      IF gt_c_output-matnr IS NOT INITIAL.
        SELECT meins
          FROM mara
          INTO TABLE gt_meins
          WHERE matnr = gt_c_output-matnr.
      ENDIF.

      READ TABLE gt_meins.
      IF sy-subrc = 0.
        ls_item-quantity-unit_of_measure = gt_meins-meins.                "单位
      ENDIF.
      ls_item-descript       = gt_c_output-maktx.                       "物料描述

      READ TABLE gt_jhgcl INTO gs_jhgcl WITH KEY matnr = gt_c_output-matnr.
      IF sy-subrc = 0.
        gt_c_output-jpeinh = gs_jhgcl-jpeinh.
      ENDIF.

      ls_item-quantity-quantity = gt_c_output-jpeinh.                   "计划工程量
      ls_item-price-total    = gt_c_output-cgpreis.                     "材料单价
      ls_item-price-fixed    = gt_c_output-fpreis.                      "人工单价

      IF gt_c_output-xmlb = 'V'.
        IF gt_c_output-cbys IS NOT INITIAL.
          ls_item-cost_elem = gt_c_output-cbys.                          "成本要素
        ELSE.
          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-cost_elem = gt_c_output-cbys.                             "成本要素
      ENDIF.
      ls_item-currency = 'CNY'.
      APPEND ls_item TO ls_wbs_costlines-cost_lines.
      APPEND ls_wbs_costlines TO lt_wbs_costlines.

      CALL FUNCTION 'CNECP_MAINTAIN'
        EXPORTING
          i_proj_id       = gt_c_output-posid                           "项目定义
          i_version       = p_bb1                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines                            "内表
        IMPORTING
          messages        = gt_return.

      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        l3 = l3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
      "  gt_c_output-type = icon_green_light.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
          EXPORTING
            i_proj_id       = gt_c_output-posid                           "项目定义
            i_version       = '000'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines                            "内表
          IMPORTING
            messages        = gt_return1.
        READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
        IF sy-subrc NE 0.
          l1 = l1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ELSE.
          CONCATENATE gs_return-message gs_return1-message INTO gt_c_output-message.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.


      ELSE.
    "    gt_c_output-type = icon_red_light.
       " gt_c_output-message = gs_return-message.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
      MODIFY gt_c_output TRANSPORTING jpeinh type message.

      CLEAR gt_c_output.

      CLEAR:ls_item,ls_wbs_costlines-cost_lines,ls_wbs_costlines.
      REFRESH lt_wbs_costlines.
      CLEAR:gs_return.
      REFRESH:gt_return.

      " js = js + 1.

    ENDLOOP.
  ELSE.
    MESSAGE '该项目原0版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
  "增加消息提示
  "000版本
  msg000 = l1 .
  CONDENSE msg000 NO-GAPS.
  CONCATENATE '0版本已成功导入:'   msg000 '条数量' INTO msg000 .
  "000后版本
  msg001 = l3 .
  CONDENSE msg001 NO-GAPS.
  CONCATENATE p_bb1   '版本已成功导入' msg001 '条数量' INTO  msg001.
  CALL SCREEN 0101 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_H_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_h_output.                                            "合同报价
  CLEAR :sc_len .
  SELECT psphi
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_h_output
    WHERE posid = gt_h_output-posid.

  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.
    IF gt_objid[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn2                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid
        WHERE topnr = gt_objid-topnr
        AND versn BETWEEN '100' AND '199'.
    ENDIF.
  ELSE.
    MESSAGE 'WBS没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH gt_objid.

  DATA:p_bb2 TYPE precp1-versn.

  SORT gt_versn2 BY versn.

  IF gt_versn2[] IS NOT INITIAL.
    LOOP AT gt_versn2 WHERE versn >= '100' AND versn <= '199'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn2-versn = gt_versn2-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn2-versn
          IMPORTING
            output = gt_versn2-versn.

        p_bb2 = gt_versn2-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb2 = 101.
  ENDIF.

  SORT gt_h_output BY matnr.

  REFRESH it_jg2.
  LOOP AT gt_h_output.
    AT NEW matnr.
      READ TABLE gt_h_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg2-matnr = gt_h_output-matnr.
        wa_jg2-htdj = gt_h_output-hgpreis.
        APPEND wa_jg2 TO it_jg2.
      ENDIF.
    ENDAT.
    CLEAR gt_h_output.
  ENDLOOP.

  REFRESH it_jghj2.
  LOOP AT gt_h_output WHERE matnr <> ''.
    CLEAR wa_jghj2.
    wa_jghj2-matnr = gt_h_output-matnr.
    wa_jghj2-hgpreis = gt_h_output-hgpreis.
    COLLECT wa_jghj2 INTO it_jghj2.
  ENDLOOP.

  LOOP AT it_jg2 INTO wa_jg2.
    READ TABLE it_jghj2 INTO wa_jghj2 WITH KEY matnr = wa_jg2-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_h_output WHERE matnr = wa_jghj2-matnr.
        g = g + 1.
      ENDLOOP.
      w = wa_jghj2-hgpreis / g.


      IF w <> wa_jg2-htdj.
        CONCATENATE '物料' wa_jghj2-matnr '的合同单价不一致！' INTO message1.
        MESSAGE message1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg2,w,message1.
  ENDLOOP.

  DELETE gt_h_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_h_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_htgcl.
  LOOP AT gt_h_output WHERE matnr <> ''.
    CLEAR gs_htgcl.
    gs_htgcl-matnr = gt_h_output-matnr.
    gs_htgcl-hpeinh = gt_h_output-hpeinh.
    COLLECT gs_htgcl INTO gt_htgcl.
  ENDLOOP.

  LOOP AT gt_h_output WHERE matnr = ''.
    APPEND gt_h_output TO gt_h_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_h_output COMPARING matnr.

  DELETE gt_h_output WHERE matnr = ''.

  LOOP AT gt_h_output1.
    APPEND gt_h_output1 TO gt_h_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_h_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的只被导入一次，且相同物料的合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.
*  -------------------------------先删除100版本数据 it02 20160308 begin----------------------
  CLEAR:proj_objnr,prps_objnr.
  READ TABLE gt_h_output INDEX 1.
  IF sy-subrc EQ 0 .
    SELECT SINGLE  objnr
          INTO proj_objnr
          FROM proj
          WHERE pspid = gt_h_output-posid.
    SELECT SINGLE objnr
        INTO prps_objnr
        FROM  prps
        WHERE posid = gt_h_output-posid.
    IF proj_objnr NE '' AND prps_objnr NE ''.
      SELECT subnr versn  kalnr topnr
     FROM precp2
     INTO CORRESPONDING FIELDS OF TABLE it_kalnr
     WHERE topnr = proj_objnr
     AND subnr   = prps_objnr
     AND versn = '100'.
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.
    ENDIF.

  ENDIF.
  REFRESH: lt_wbs_costlines3,gt_return2,ls_wbs_costlines2-cost_lines.
  CLEAR:ls_wbs_costlines3,gs_return2.
  IF it_ckis IS NOT INITIAL.
    LOOP AT it_ckis INTO wa_ckis.
      CLEAR:ls_item3.
      AT FIRST .
        ls_wbs_costlines3-wbs_name = gt_h_output-posid.
      ENDAT.
      ls_item3-item_number = wa_ckis-posnr.
      ls_item3-flag_delete_item = 'X'.
      APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
      AT LAST .
        APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
      ENDAT.
    ENDLOOP.
    CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
      EXPORTING
        i_proj_id       = gt_h_output-posid                           "项目定义
        i_version       = '100'                                         "版本
        i_cost_variant  = 'PS06'                                      "核算变式
        i_wbs_costlines = lt_wbs_costlines3                            "内表
      IMPORTING
        messages        = gt_return2.
    READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
    IF sy-subrc EQ 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR:ermsg .
      CONCATENATE '该项目' gt_h_output-posid  '删除100版本失败：' gs_return2-message  INTO ermsg .
      "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
      MESSAGE ermsg  TYPE 'E'.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDIF.
  "再重新读取000版本的预算项目信息
  IF it_kalnr IS NOT INITIAL.
    SELECT *
         FROM ckis
         INTO CORRESPONDING FIELDS OF TABLE it_ckis
         FOR ALL ENTRIES IN it_kalnr
         WHERE kalnr = it_kalnr-kalnr.
  ENDIF.
  IF it_ckis IS  INITIAL.
    LOOP AT gt_h_output.

      IF gt_h_output-posid IS NOT INITIAL.
        ls_wbs_costlines-wbs_name = gt_h_output-posid.                   "WBS
      ELSE.
        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.

      gt_h_output-type = icon_yellow_light.                             "状态

      ls_item-resource-typps = gt_h_output-xmlb.                        "项目类别

      IF gt_h_output-werks IS INITIAL.
        ls_item-resource-werks = 1800.                                  "工厂
      ELSE.
        ls_item-resource-werks = gt_h_output-werks.
      ENDIF.

      IF gt_h_output-xmlb = 'M'.
        IF gt_h_output-matnr IS NOT INITIAL.
          ls_item-resource-matnr = gt_h_output-matnr.                       "物料号
        ELSE.
          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-resource-matnr = gt_h_output-matnr.                       "物料号
      ENDIF.

      REFRESH gt_meins[].
      CLEAR gt_meins.
      IF gt_h_output-matnr IS NOT INITIAL.
        SELECT meins
              FROM mara
              INTO TABLE gt_meins
              WHERE matnr = gt_h_output-matnr.
      ENDIF.

      READ TABLE gt_meins.
      IF sy-subrc = 0.
        ls_item-quantity-unit_of_measure = gt_meins-meins.                "单位
      ENDIF.
      ls_item-descript       = gt_h_output-maktx.                       "物料描述

      READ TABLE gt_htgcl INTO gs_htgcl WITH TABLE KEY matnr = gt_h_output-matnr.
      IF sy-subrc = 0.
        gt_h_output-hpeinh = gs_htgcl-hpeinh.
      ENDIF.

      ls_item-quantity-quantity = gt_h_output-hpeinh.                  "合同工程量
      ls_item-price-total   = gt_h_output-hgpreis.                     "合同单价

      IF gt_h_output-xmlb = 'V'.
        IF gt_h_output-cbys IS NOT INITIAL.
          ls_item-cost_elem = gt_h_output-cbys.                          "成本要素
        ELSE.
          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-cost_elem = gt_h_output-cbys.                             "成本要素
      ENDIF.
      ls_item-currency = 'CNY'.
      APPEND ls_item TO ls_wbs_costlines-cost_lines.
      APPEND ls_wbs_costlines TO lt_wbs_costlines.

      CALL FUNCTION 'CNECP_MAINTAIN'
        EXPORTING
          i_proj_id       = gt_h_output-posid                           "项目编号
          i_version       = p_bb2                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines                            "内表
        IMPORTING
          messages        = gt_return.

      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        l4 = l4 + 1. "统计导入100后最新版本的数据  IT02 20160307
        gt_h_output-type = icon_green_light.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
          EXPORTING
            i_proj_id       = gt_h_output-posid                           "项目定义
            i_version       = '100'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines                            "内表
          IMPORTING
            messages        = gt_return1.
        READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
        IF sy-subrc NE 0.
          l2 = l2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
        ELSE.
          CONCATENATE gs_return-message gs_return1-message INTO gt_h_output-message.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

      ELSE.
        gt_h_output-type = icon_red_light.
        gt_h_output-message = gs_return-message.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ENDIF.

      MODIFY gt_h_output TRANSPORTING hpeinh type message.

      CLEAR gt_h_output.

      CLEAR:ls_item,ls_wbs_costlines-cost_lines,ls_wbs_costlines.
      REFRESH lt_wbs_costlines.
      CLEAR:gs_return.
      REFRESH:gt_return.

      " js = js + 1.

    ENDLOOP.
  ELSE.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.

  "增加消息提示
  "100版本
  msg100 = l2 .
  CONDENSE msg100 NO-GAPS.
  CONCATENATE '100版本已成功导入:'   msg100 '条数量' INTO msg100 .
  "100后版本
  msg101 = l4 .
  CONDENSE msg101 NO-GAPS.
  CONCATENATE p_bb2  '版本已成功导入'  msg101 '条数量' INTO msg101.
  CALL SCREEN 0101 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CH_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_ch_output.                                            "成本预算和合同报价
  CLEAR :sc_len .
  SELECT psphi
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_ch_output
    WHERE posid = gt_ch_output-posid.
  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid1
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid1[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn1                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid1
        WHERE topnr = gt_objid1-topnr.
    ENDIF.

    SELECT topnr versn                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid2
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid2[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn2                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid2
        WHERE topnr = gt_objid2-topnr.
    ENDIF.
  ENDIF.

  REFRESH:gt_objid1,gt_objid2.

  DATA:p_bb1 TYPE precp1-versn.

  SORT gt_versn1 BY versn.
  SORT gt_versn2 BY versn.

  IF gt_versn1[] IS NOT INITIAL.
    LOOP AT gt_versn1 WHERE versn >= '000' AND versn <= '099'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn1-versn = gt_versn1-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn1-versn
          IMPORTING
            output = gt_versn1-versn.

        p_bb1 = gt_versn1-versn.
      ENDAT.
    ENDLOOP.
  ELSEIF gt_versn1[] IS INITIAL.
    p_bb1 = '001'.
  ENDIF.

  DATA:p_bb2 TYPE precp1-versn.

  IF gt_versn2[] IS NOT INITIAL.
    LOOP AT gt_versn2 WHERE versn >= '100' AND versn <= '199'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn2-versn = gt_versn2-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn2-versn
          IMPORTING
            output = gt_versn2-versn.

        p_bb2 = gt_versn2-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb2 = 101.
  ENDIF.

  SORT gt_ch_output BY matnr.

  REFRESH it_jg.
  LOOP AT gt_ch_output.
    AT NEW matnr.
      READ TABLE gt_ch_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg-matnr = gt_ch_output-matnr.
        wa_jg-htdj = gt_ch_output-hgpreis.
        wa_jg-cldj = gt_ch_output-cgpreis.
        wa_jg-rgdj = gt_ch_output-fpreis.
        APPEND wa_jg TO it_jg.
      ENDIF.
    ENDAT.
    CLEAR gt_ch_output.
  ENDLOOP.

  REFRESH it_jghj.
  LOOP AT gt_ch_output WHERE matnr <> ''.
    CLEAR wa_jghj.
    wa_jghj-matnr = gt_ch_output-matnr.
    wa_jghj-hgpreis = gt_ch_output-hgpreis.
    wa_jghj-cgpreis = gt_ch_output-cgpreis.
    wa_jghj-fpreis = gt_ch_output-fpreis.
    COLLECT wa_jghj INTO it_jghj.
  ENDLOOP.

  LOOP AT it_jg INTO wa_jg.
    READ TABLE it_jghj INTO wa_jghj WITH KEY matnr = wa_jg-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_ch_output WHERE matnr = wa_jghj-matnr.
        g = g + 1.
      ENDLOOP.
      w = wa_jghj-hgpreis / g.
      ll = wa_jghj-cgpreis / g.
      jj = wa_jghj-fpreis / g.


      IF w <> wa_jg-htdj.
        CONCATENATE '物料' wa_jghj-matnr '的合同单价不一致！' INTO message1.
        MESSAGE message1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF ll <> wa_jg-cldj.
        CONCATENATE '物料' wa_jghj-matnr '的材料单价不一致！' INTO message2.
        MESSAGE message2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF jj <> wa_jg-rgdj.
        CONCATENATE '物料' wa_jghj-matnr '的人工单价不一致！' INTO message3.
        MESSAGE message3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg,w,ll,jj,message1,message2,message3.
  ENDLOOP.

  DELETE gt_ch_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_ch_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_hb.
  LOOP AT gt_ch_output WHERE matnr <> ''.
    CLEAR gs_hb.
    gs_hb-matnr = gt_ch_output-matnr.
    gs_hb-hpeinh = gt_ch_output-hpeinh.
    gs_hb-jpeinh = gt_ch_output-jpeinh.
    COLLECT gs_hb INTO gt_hb.
  ENDLOOP.

  LOOP AT gt_ch_output WHERE matnr = ''.
    APPEND gt_ch_output TO gt_ch_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_ch_output COMPARING matnr.

  DELETE gt_ch_output WHERE matnr = ''.

  LOOP AT gt_ch_output1.
    APPEND gt_ch_output1 TO gt_ch_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_ch_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的只被导入一次，且相同物料的计划工程量和合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.

  CLEAR:proj_objnr,prps_objnr.
  READ TABLE gt_ch_output INDEX 1.
  IF sy-subrc EQ 0 .
    SELECT SINGLE  objnr
         INTO proj_objnr
         FROM proj
         WHERE pspid = gt_ch_output-posid.
    SELECT SINGLE objnr
        INTO prps_objnr
        FROM  prps
        WHERE posid = gt_ch_output-posid.
    IF proj_objnr NE '' AND prps_objnr NE ''.
      SELECT subnr versn  kalnr topnr
       FROM precp2
       INTO CORRESPONDING FIELDS OF TABLE it_kalnr
       WHERE topnr = proj_objnr
       AND subnr   = prps_objnr
       AND versn = '100'.
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.
      SELECT subnr versn  kalnr topnr
            FROM precp2
            INTO CORRESPONDING FIELDS OF TABLE it_kalnr1
            WHERE topnr = proj_objnr
            AND subnr   = prps_objnr
            AND versn = '000'.

      " IF it_kalnr IS NOT INITIAL.
      IF it_kalnr1 IS NOT INITIAL.
        SELECT *
          FROM ckis
          INTO CORRESPONDING FIELDS OF TABLE it_ckis1
          FOR ALL ENTRIES IN it_kalnr1
          WHERE kalnr = it_kalnr1-kalnr.

      ENDIF.
    ENDIF.
    REFRESH: lt_wbs_costlines3,gt_return5,lt_wbs_costlines4,gt_return5,ls_wbs_costlines3-cost_lines,ls_wbs_costlines4-cost_lines.
    CLEAR:ls_wbs_costlines3,gs_return5,ls_wbs_costlines4,gs_return5.
    IF it_ckis IS NOT INITIAL.
      LOOP AT it_ckis INTO wa_ckis.
        CLEAR:ls_item3.
        AT FIRST .
          ls_wbs_costlines3-wbs_name = gt_ch_output-posid.
        ENDAT.
        ls_item3-item_number = wa_ckis-posnr.
        ls_item3-flag_delete_item = 'X'.
        APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
        AT LAST .
          APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
        ENDAT.


      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          i_proj_id       = gt_ch_output-posid                           "项目定义
          i_version       = '100'                                         "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines3                            "内表
        IMPORTING
          messages        = gt_return5.

      READ TABLE gt_return5 INTO gs_return5 WITH KEY type = 'E'.
      IF sy-subrc EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ermsg .
        CONCATENATE '该项目' gt_ch_output-posid  '删除100版本失败：' gs_return5-message  INTO ermsg .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ermsg  TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取100版本的预算项目信息
    IF it_kalnr IS NOT INITIAL.
      SELECT *
           FROM ckis
           INTO CORRESPONDING FIELDS OF TABLE it_ckis
           FOR ALL ENTRIES IN it_kalnr
           WHERE kalnr = it_kalnr-kalnr.
    ENDIF.


    IF it_ckis1 IS NOT INITIAL.
      LOOP AT it_ckis1 INTO wa_ckis1.
        CLEAR:ls_item4.
        AT FIRST .
          ls_wbs_costlines4-wbs_name = gt_ch_output-posid.
        ENDAT.
        ls_item4-item_number = wa_ckis1-posnr.
        ls_item4-flag_delete_item = 'X'.
        APPEND ls_item4 TO ls_wbs_costlines4-cost_lines.
        AT LAST .
          APPEND ls_wbs_costlines4 TO lt_wbs_costlines4.
        ENDAT.

      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除0版本
        EXPORTING
          i_proj_id       = gt_ch_output-posid                           "项目定义
          i_version       = '000'                                         "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines4                            "内表
        IMPORTING
          messages        = gt_return4.

      READ TABLE gt_return4 INTO gs_return4 WITH KEY type = 'E'.
      IF sy-subrc EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ermsg .
        CONCATENATE '该项目' gt_ch_output-posid  '删除000版本失败：' gs_return4-message  INTO ermsg .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ermsg  TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取000版本的预算项目信息
    IF it_kalnr1 IS NOT INITIAL.
      SELECT *
           FROM ckis
           INTO CORRESPONDING FIELDS OF TABLE it_ckis1
           FOR ALL ENTRIES IN it_kalnr1
           WHERE kalnr = it_kalnr1-kalnr.
    ENDIF.

  ENDIF.
  IF it_ckis1 IS INITIAL AND it_ckis IS INITIAL.
    CLEAR:l1,l2,l3,l4.   "初始导入版本数量
    LOOP AT gt_ch_output.

      IF gt_ch_output-posid IS NOT INITIAL.
        ls_wbs_costlines-wbs_name = gt_ch_output-posid.                   "WBS
        ls_wbs_costlines1-wbs_name = gt_ch_output-posid.                  "WBS
      ELSE.
        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.

      gt_ch_output-type = icon_yellow_light.                             "状态

      ls_item-resource-typps = gt_ch_output-xmlb.                        "项目类别
      ls_item1-resource-typps = gt_ch_output-xmlb.                       "项目类别

      IF gt_ch_output-werks IS INITIAL.
        ls_item-resource-werks = 1800.                                  "工厂
        ls_item1-resource-werks = 1800.                                  "工厂
      ELSE.
        ls_item-resource-werks = gt_ch_output-werks.
        ls_item1-resource-werks = gt_ch_output-werks.
      ENDIF.

      IF gt_ch_output-xmlb = 'M'.
        IF gt_ch_output-matnr IS NOT INITIAL.
          ls_item-resource-matnr = gt_ch_output-matnr.                       "物料号
          ls_item1-resource-matnr = gt_ch_output-matnr.                       "物料号
        ELSE.
          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-resource-matnr = gt_ch_output-matnr.                       "物料号
        ls_item1-resource-matnr = gt_ch_output-matnr.                       "物料号
      ENDIF.

      REFRESH gt_meins[].
      CLEAR gt_meins.
      IF gt_ch_output-matnr IS NOT INITIAL.
        SELECT meins
              FROM mara
              INTO TABLE gt_meins
              WHERE matnr = gt_ch_output-matnr.
      ENDIF.

      READ TABLE gt_meins.
      IF sy-subrc = 0.
        ls_item-quantity-unit_of_measure = gt_meins-meins.                 "单位
        ls_item1-quantity-unit_of_measure = gt_meins-meins.                "单位
      ENDIF.
      ls_item-descript       = gt_ch_output-maktx.                       "物料描述
      ls_item1-descript       = gt_ch_output-maktx.                       "物料描述

      ls_item1-price-fixed    = gt_ch_output-fpreis.                      "人工单价

      IF gt_ch_output-xmlb = 'V'.
        IF gt_ch_output-cbys IS NOT INITIAL.
          ls_item-cost_elem = gt_ch_output-cbys.                          "成本要素
          ls_item1-cost_elem = gt_ch_output-cbys.                          "成本要素
        ELSE.
          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-cost_elem = gt_ch_output-cbys.                             "成本要素
        ls_item1-cost_elem = gt_ch_output-cbys.                             "成本要素
      ENDIF.

      READ TABLE gt_hb INTO gs_hb WITH KEY matnr = gt_ch_output-matnr.
      IF sy-subrc = 0.
        gt_ch_output-hpeinh = gs_hb-hpeinh.
        gt_ch_output-jpeinh = gs_hb-jpeinh.
      ENDIF.

      ls_item-quantity-quantity = gt_ch_output-hpeinh.                   "合同工程量
      ls_item-price-total = gt_ch_output-hgpreis.                        "合同单价
      ls_item1-quantity-quantity = gt_ch_output-jpeinh.                   "计划工程量
      ls_item1-price-total = gt_ch_output-cgpreis.                     "材料单价
      ls_item-currency = 'CNY'.
      ls_item1-currency = 'CNY'.
      APPEND ls_item TO ls_wbs_costlines-cost_lines.
      APPEND ls_wbs_costlines TO lt_wbs_costlines.

      APPEND ls_item1 TO ls_wbs_costlines1-cost_lines.
      APPEND ls_wbs_costlines1 TO lt_wbs_costlines1.

      "合同
      APPEND ls_wbs_costlines TO lt_wbs_costlines5.

      APPEND ls_wbs_costlines1 TO lt_wbs_costlines6.                    "成本


      CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
        EXPORTING
          i_proj_id       = gt_ch_output-posid                          "项目编号
          i_version       = p_bb2                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines                            "内表
        IMPORTING
          messages        = gt_return.

      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        l4 = l4 + 1. "统计导入100版本后最新版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
          EXPORTING
            i_proj_id       = gt_ch_output-posid                          "项目编号
            i_version       = p_bb1                                       "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines1                           "内表
          IMPORTING
            messages        = gt_return1.

        READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
        IF sy-subrc <> 0 .
          l3 = l3 + 1. "统计导入000版本后最新版本导入数量          ADD IT02 20160307
          gt_ch_output-type = icon_green_light.      "如果两个都更新成功，那么绿灯，而且0版本和100版本也会更新成功，只是消息会报目前两个是否成功

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
            EXPORTING
              i_proj_id       = gt_ch_output-posid                           "项目定义
              i_version       = '100'                                         "版本
              i_cost_variant  = 'PS06'                                      "核算变式
              i_wbs_costlines = lt_wbs_costlines5                            "内表
            IMPORTING
              messages        = gt_return2.

          READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
          IF sy-subrc <> 0 .
            l2 = l2 + 1. "统计导入100版本导入数量          ADD IT02 20160307
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            gt_ch_output-type = icon_red_light. "报出相应的错误，
            CONCATENATE '100版本' gs_return2-message INTO gt_ch_output-message.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

          CALL FUNCTION 'CNECP_MAINTAIN'          "创建0版本
            EXPORTING
              i_proj_id       = gt_ch_output-posid                           "项目定义
              i_version       = '000'                                         "版本
              i_cost_variant  = 'PS06'                                      "核算变式
              i_wbs_costlines = lt_wbs_costlines6                            "内表
            IMPORTING
              messages        = gt_return3.

          READ TABLE gt_return3 INTO gs_return3 WITH KEY type = 'E'.
          IF sy-subrc <> 0 .
            l1 = l1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            gt_ch_output-type = icon_red_light. "报出相应的错误，
            CONCATENATE '000版本' gt_ch_output-message  gs_return3-message  INTO gt_ch_output-message.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.


          CONCATENATE gs_return-message gs_return1-message INTO gt_ch_output-message.

        ELSE.

          gt_ch_output-type = icon_red_light.
          CONCATENATE gs_return-message gs_return1-message INTO gt_ch_output-message.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        gt_ch_output-type = icon_red_light. "报出相应的错误，同时不更新0版本和100版本
        gt_ch_output-message = gs_return-message.

      ENDIF.

      MODIFY gt_ch_output TRANSPORTING hpeinh jpeinh type message.

      CLEAR:ls_item,ls_wbs_costlines-cost_lines,ls_wbs_costlines,gt_ch_output.
      REFRESH:lt_wbs_costlines,lt_wbs_costlines5,lt_wbs_costlines6.
      CLEAR:ls_item1,ls_wbs_costlines1-cost_lines,ls_wbs_costlines1.
      REFRESH lt_wbs_costlines1.
      CLEAR:gs_return,gs_return1.
      REFRESH:gt_return,gt_return1,gt_return2,gt_return3.

    ENDLOOP.
  ELSEIF it_ckis IS NOT INITIAL.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ELSEIF it_ckis1 IS NOT INITIAL.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
  "增加消息提示
  "000版本
  msg000 = l1 .
  CONDENSE msg000 NO-GAPS.
  CONCATENATE '0版本已成功导入:'   msg000 '条数量' INTO msg000 .
  "100版本
  msg100 = l2 .
  CONDENSE msg100 NO-GAPS.
  CONCATENATE '100版本已成功导入:'   msg100 '条数量' INTO msg100 .
  "000后版本
  msg001 = l3 .
  CONDENSE msg001 NO-GAPS.
  CONCATENATE p_bb1   '版本已成功导入' msg001 '条数量' INTO  msg001.
  "100后版本
  msg101 = l4 .
  CONDENSE msg101 NO-GAPS.
  CONCATENATE p_bb2  '版本已成功导入'  msg101 '条数量' INTO msg101.
  CALL SCREEN 0101 STARTING AT 25 10.


  CLEAR:p_bb1,p_bb2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_cz_output .
  CLEAR :sc_len .
  SELECT psphi                                                        "取出了WBS元素对应的项目定义
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_cz_output
    WHERE posid = gt_cz_output-posid.

  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn1                        "取一个对象编号下对应的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid
        WHERE topnr = gt_objid-topnr
        AND versn BETWEEN '000' AND '099'.
    ENDIF.

  ELSE.
    MESSAGE 'WBS元素没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH:gt_objid,gt_psphi.

  DATA:p_bb1 TYPE precp1-versn.

  SORT gt_versn1 BY versn.

  IF gt_versn1[] IS NOT INITIAL.
    LOOP AT gt_versn1 WHERE versn >= '000' AND versn <= '099'.
*    if gt_versn1-versn is not initial.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn1-versn = gt_versn1-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn1-versn
          IMPORTING
            output = gt_versn1-versn.

        p_bb1 = gt_versn1-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb1 = '001'.
  ENDIF.

  SORT gt_cz_output BY matnr.

  REFRESH it_jg1.
  LOOP AT gt_cz_output.
    AT NEW matnr.
      READ TABLE gt_cz_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg1-matnr = gt_cz_output-matnr.
        wa_jg1-cldj = gt_cz_output-cgpreis.
        wa_jg1-rgdj = gt_cz_output-fpreis.
        APPEND wa_jg1 TO it_jg1.
      ENDIF.
    ENDAT.
    CLEAR gt_cz_output.
  ENDLOOP.

  REFRESH it_jghj1.
  LOOP AT gt_cz_output WHERE matnr <> ''.
    CLEAR wa_jghj.
    wa_jghj1-matnr = gt_cz_output-matnr.
    wa_jghj1-cgpreis = gt_cz_output-cgpreis.
    wa_jghj1-fpreis = gt_cz_output-fpreis.
    COLLECT wa_jghj1 INTO it_jghj1.
  ENDLOOP.

  LOOP AT it_jg1 INTO wa_jg1.
    READ TABLE it_jghj1 INTO wa_jghj1 WITH KEY matnr = wa_jg1-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_cz_output WHERE matnr = wa_jghj1-matnr.
        g = g + 1.
      ENDLOOP.
      ll = wa_jghj1-cgpreis / g.
      jj = wa_jghj1-fpreis / g.


      IF ll <> wa_jg1-cldj.
        CONCATENATE '物料' wa_jghj1-matnr '的材料单价不一致！' INTO message2.
        MESSAGE message2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF jj <> wa_jg1-rgdj.
        CONCATENATE '物料' wa_jghj1-matnr '的人工单价不一致！' INTO message3.
        MESSAGE message3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg1,w,ll,jj,message2,message3.
  ENDLOOP.

  DELETE gt_cz_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_cz_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_jhgcl.
  LOOP AT gt_cz_output WHERE matnr <> ''.
    CLEAR gs_jhgcl.
    gs_jhgcl-matnr = gt_cz_output-matnr.
    gs_jhgcl-jpeinh = gt_cz_output-jpeinh.
    COLLECT gs_jhgcl INTO gt_jhgcl.
  ENDLOOP.

  LOOP AT gt_cz_output WHERE matnr = ''.
    APPEND gt_cz_output TO gt_cz_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_cz_output COMPARING matnr.

  DELETE gt_cz_output WHERE matnr = ''.

  LOOP AT gt_cz_output1.
    APPEND gt_cz_output1 TO gt_cz_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_cz_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的将被导入一次，且将合计相同物料的计划工程量！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.
*  ........................add IT02 20160307begin ...................... *
  CLEAR:proj_objnr,prps_objnr.
  READ TABLE gt_cz_output INDEX 1.
  IF sy-subrc EQ 0 .
    SELECT SINGLE  objnr
        INTO proj_objnr
        FROM proj
        WHERE pspid = gt_cz_output-posid.
    SELECT SINGLE objnr
        INTO prps_objnr
        FROM  prps
        WHERE posid = gt_cz_output-posid.
    IF proj_objnr NE '' AND prps_objnr NE ''.
      SELECT subnr versn  kalnr topnr
      FROM precp2
      INTO CORRESPONDING FIELDS OF TABLE it_kalnr
      WHERE topnr = proj_objnr
      AND subnr   = prps_objnr
      AND versn = '000'.
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.
    ENDIF.
    REFRESH: lt_wbs_costlines3,gt_return2,ls_wbs_costlines3-cost_lines.
    CLEAR:ls_wbs_costlines3,gs_return2.
    IF it_ckis IS NOT INITIAL.
      LOOP AT it_ckis INTO wa_ckis.
        CLEAR:ls_item3.
        AT FIRST .
          ls_wbs_costlines3-wbs_name = gt_cz_output-posid.
        ENDAT.
        ls_item3-item_number = wa_ckis-posnr.
        ls_item3-flag_delete_item = 'X'.
        APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
        AT LAST .
          APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
        ENDAT.


      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除000版本
        EXPORTING
          i_proj_id       = gt_cz_output-posid                           "项目定义
          i_version       = '000'                                         "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines3                            "内表
        IMPORTING
          messages        = gt_return2.

      READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
      IF sy-subrc EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ermsg .
        CONCATENATE '该项目' gt_cz_output-posid  '删除0版本失败：' gs_return2-message  INTO ermsg .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ermsg  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  "再重新读取100版本的预算项目信息
  IF it_kalnr IS NOT INITIAL.
    SELECT *
         FROM ckis
         INTO CORRESPONDING FIELDS OF TABLE it_ckis
         FOR ALL ENTRIES IN it_kalnr
         WHERE kalnr = it_kalnr-kalnr.
  ENDIF.
  IF it_ckis IS  INITIAL.
    CLEAR:l1,l2,l3,l4.   "初始导入版本数量
    LOOP AT gt_cz_output.

      IF gt_cz_output-posid IS NOT INITIAL.
        ls_wbs_costlines-wbs_name = gt_cz_output-posid.                  "WBS
      ELSE.
        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.

      gt_cz_output-type = icon_yellow_light.                             "状态

      ls_item-resource-typps = gt_cz_output-xmlb.                        "项目类别

      IF gt_cz_output-werks IS INITIAL.
        ls_item-resource-werks = 1800.                                  "工厂
      ELSE.
        ls_item-resource-werks = gt_cz_output-werks.
      ENDIF.

      IF gt_cz_output-xmlb = 'M'.
        IF gt_cz_output-matnr IS NOT INITIAL.
          ls_item-resource-matnr = gt_cz_output-matnr.                  "物料号
        ELSE.
          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-resource-matnr = gt_cz_output-matnr.                    "物料编码
      ENDIF.

      REFRESH gt_meins[].
      CLEAR gt_meins.
      IF gt_cz_output-matnr IS NOT INITIAL.
        SELECT meins
          FROM mara
          INTO TABLE gt_meins
          WHERE matnr = gt_cz_output-matnr.
      ENDIF.

      READ TABLE gt_meins.
      IF sy-subrc = 0.
        ls_item-quantity-unit_of_measure = gt_meins-meins.                "单位
      ENDIF.
      ls_item-descript       = gt_cz_output-maktx.                       "物料描述

      READ TABLE gt_jhgcl INTO gs_jhgcl WITH KEY matnr = gt_cz_output-matnr.
      IF sy-subrc = 0.
        gt_cz_output-jpeinh = gs_jhgcl-jpeinh.
      ENDIF.

      ls_item-quantity-quantity = gt_cz_output-jpeinh.                   "计划工程量
      ls_item-price-total    = gt_cz_output-cgpreis.                     "材料单价
      ls_item-price-fixed    = gt_cz_output-fpreis.                      "人工单价

      IF gt_cz_output-xmlb = 'V'.
        IF gt_cz_output-cbys IS NOT INITIAL.
          ls_item-cost_elem = gt_cz_output-cbys.                          "成本要素
        ELSE.
          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
          CONTINUE.
        ENDIF.
      ELSE.
        ls_item-cost_elem = gt_cz_output-cbys.                             "成本要素
      ENDIF.
      ls_item-currency = 'CNY'.
      APPEND ls_item TO ls_wbs_costlines-cost_lines.
      APPEND ls_wbs_costlines TO lt_wbs_costlines.

      CALL FUNCTION 'CNECP_MAINTAIN'
        EXPORTING
          i_proj_id       = gt_cz_output-posid                           "项目定义
          i_version       = p_bb1                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines                            "内表
        IMPORTING
          messages        = gt_return.

      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        l3 = l3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
        gt_c_output-type = icon_green_light.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
          EXPORTING
            i_proj_id       = gt_cz_output-posid                           "项目定义
            i_version       = '000'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines                            "内表
          IMPORTING
            messages        = gt_return1.
        READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
        IF sy-subrc NE 0.
          l1 = l1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ELSE.
          CONCATENATE gs_return-message gs_return1-message INTO gt_cz_output-message.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.


      ELSE.
        gt_cz_output-type = icon_red_light.
        gt_cz_output-message = gs_return-message.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      MODIFY gt_cz_output TRANSPORTING jpeinh type message.

      CLEAR gt_cz_output.

      CLEAR:ls_item,ls_wbs_costlines-cost_lines,ls_wbs_costlines.
      REFRESH lt_wbs_costlines.
      CLEAR:gs_return.
      REFRESH:gt_return.

      "  js = js + 1.
    ENDLOOP.
  ELSE.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
    "增加消息提示
    "000版本
    msg000 = l1 .
    CONDENSE msg000 NO-GAPS.
    CONCATENATE '0版本已成功导入:'   msg000 '条数量' INTO msg000 .
    "000后版本
    msg001 = l3 .
    CONDENSE msg001 NO-GAPS.
    CONCATENATE p_bb1   '版本已成功导入' msg001 '条数量' INTO  msg001.
    CALL SCREEN 0101 STARTING AT 25 10.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_HZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_hz_output .
  CLEAR :sc_len .
  SELECT psphi
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_hz_output
    WHERE posid = gt_hz_output-posid.

  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.
    IF gt_objid[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn2                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid
        WHERE topnr = gt_objid-topnr
        AND versn BETWEEN '100' AND '199'.
    ENDIF.
  ELSE.
    MESSAGE 'WBS没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH gt_objid.

  DATA:p_bb2 TYPE precp1-versn.

  SORT gt_versn2 BY versn.

  IF gt_versn2[] IS NOT INITIAL.
    LOOP AT gt_versn2 WHERE versn >= '100' AND versn <= '199'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn2-versn = gt_versn2-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn2-versn
          IMPORTING
            output = gt_versn2-versn.

        p_bb2 = gt_versn2-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb2 = 101.
  ENDIF.

  SORT gt_hz_output BY matnr.

  REFRESH it_jg2.
  LOOP AT gt_hz_output.
    AT NEW matnr.
      READ TABLE gt_hz_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg2-matnr = gt_hz_output-matnr.
        wa_jg2-htdj = gt_hz_output-hgpreis.
        APPEND wa_jg2 TO it_jg2.
      ENDIF.
    ENDAT.
    CLEAR gt_hz_output.
  ENDLOOP.

  REFRESH it_jghj2.
  LOOP AT gt_hz_output WHERE matnr <> ''.
    CLEAR wa_jghj2.
    wa_jghj2-matnr = gt_hz_output-matnr.
    wa_jghj2-hgpreis = gt_hz_output-hgpreis.
    COLLECT wa_jghj2 INTO it_jghj2.
  ENDLOOP.

  LOOP AT it_jg2 INTO wa_jg2.
    READ TABLE it_jghj2 INTO wa_jghj2 WITH KEY matnr = wa_jg2-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_hz_output WHERE matnr = wa_jghj2-matnr.
        g = g + 1.
      ENDLOOP.
      w = wa_jghj2-hgpreis / g.


      IF w <> wa_jg2-htdj.
        CONCATENATE '物料' wa_jghj2-matnr '的合同单价不一致！' INTO message1.
        MESSAGE message1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg2,w,message1.
  ENDLOOP.

  DELETE gt_hz_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_hz_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_htgcl.
  LOOP AT gt_hz_output WHERE matnr <> ''.
    CLEAR gs_htgcl.
    gs_htgcl-matnr = gt_hz_output-matnr.
    gs_htgcl-hpeinh = gt_hz_output-hpeinh.
    COLLECT gs_htgcl INTO gt_htgcl.
  ENDLOOP.

  LOOP AT gt_hz_output WHERE matnr = ''.
    APPEND gt_hz_output TO gt_hz_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_hz_output COMPARING matnr.

  DELETE gt_hz_output WHERE matnr = ''.

  LOOP AT gt_hz_output1.
    APPEND gt_hz_output1 TO gt_hz_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_hz_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的只被导入一次，且相同物料的合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.

* --------------------先删除000增值税版本数据 it02 20160308 begin---------------
  CLEAR:proj_objnr,prps_objnr.
  READ TABLE gt_hz_output INDEX 1.
  IF sy-subrc EQ 0 .
    SELECT SINGLE  objnr
          INTO proj_objnr
          FROM proj
          WHERE pspid = gt_hz_output-posid.
    SELECT SINGLE objnr
        INTO prps_objnr
        FROM  prps
        WHERE posid = gt_hz_output-posid.
    IF proj_objnr NE '' AND prps_objnr NE ''.
      SELECT subnr versn  kalnr topnr
     FROM precp2
     INTO CORRESPONDING FIELDS OF TABLE it_kalnr
     WHERE topnr = proj_objnr
     AND subnr   = prps_objnr
     AND versn = '100'.
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.
    ENDIF.

  ENDIF.


  REFRESH: lt_wbs_costlines3,gt_return2,ls_wbs_costlines3-cost_lines.
  CLEAR:ls_wbs_costlines3,gs_return2.
  IF it_ckis IS NOT INITIAL.
    LOOP AT it_ckis INTO wa_ckis.
      CLEAR:ls_item3.
      AT FIRST .
        ls_wbs_costlines3-wbs_name = gt_hz_output-posid.
      ENDAT.
      ls_item3-item_number = wa_ckis-posnr.
      ls_item3-flag_delete_item = 'X'.
      APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
      AT LAST .
        APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
      ENDAT.
    ENDLOOP.
    CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
      EXPORTING
        i_proj_id       = gt_hz_output-posid                           "项目定义
        i_version       = '100'                                         "版本
        i_cost_variant  = 'PS06'                                      "核算变式
        i_wbs_costlines = lt_wbs_costlines3                            "内表
      IMPORTING
        messages        = gt_return2.
    READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
    IF sy-subrc EQ 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR:ermsg .
      CONCATENATE '该项目' gt_hz_output-posid  '删除100版本失败：' gs_return2-message  INTO ermsg .
      "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
      MESSAGE ermsg  TYPE 'E'.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDIF.
  " * --------------------先删除000增值税版本数据 it02 20160308  end---------------

  "再重新读取100版本的预算项目信息
  IF it_kalnr IS NOT INITIAL.
    SELECT *
         FROM ckis
         INTO CORRESPONDING FIELDS OF TABLE it_ckis
         FOR ALL ENTRIES IN it_kalnr
         WHERE kalnr = it_kalnr-kalnr.
  ENDIF.
  IF it_ckis IS  INITIAL.

   LOOP AT gt_hz_output.

    IF gt_hz_output-posid IS NOT INITIAL.
      ls_wbs_costlines-wbs_name = gt_hz_output-posid.                   "WBS
    ELSE.
      MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
      CONTINUE.
    ENDIF.

    gt_hz_output-type = icon_yellow_light.                             "状态

    ls_item-resource-typps = gt_hz_output-xmlb.                        "项目类别

    IF gt_hz_output-werks IS INITIAL.
      ls_item-resource-werks = 1800.                                  "工厂
    ELSE.
      ls_item-resource-werks = gt_hz_output-werks.
    ENDIF.

    IF gt_hz_output-xmlb = 'M'.
      IF gt_hz_output-matnr IS NOT INITIAL.
        ls_item-resource-matnr = gt_hz_output-matnr.                       "物料号
      ELSE.
        MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.
    ELSE.
      ls_item-resource-matnr = gt_hz_output-matnr.                       "物料号
    ENDIF.

    REFRESH gt_meins[].
    CLEAR gt_meins.
    IF gt_hz_output-matnr IS NOT INITIAL.
      SELECT meins
            FROM mara
            INTO TABLE gt_meins
            WHERE matnr = gt_hz_output-matnr.
    ENDIF.

    READ TABLE gt_meins.
    IF sy-subrc = 0.
      ls_item-quantity-unit_of_measure = gt_meins-meins.                "单位
    ENDIF.
    ls_item-descript       = gt_hz_output-maktx.                       "物料描述

    READ TABLE gt_htgcl INTO gs_htgcl WITH TABLE KEY matnr = gt_hz_output-matnr.
    IF sy-subrc = 0.
      gt_hz_output-hpeinh = gs_htgcl-hpeinh.
    ENDIF.

    ls_item-quantity-quantity = gt_hz_output-hpeinh.                  "合同工程量
    ls_item-price-total   = gt_hz_output-hgpreis.                     "合同单价

    IF gt_hz_output-xmlb = 'V'.
      IF gt_hz_output-cbys IS NOT INITIAL.
        ls_item-cost_elem = gt_hz_output-cbys.                          "成本要素
      ELSE.
        MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
        CONTINUE.
      ENDIF.
    ELSE.
      ls_item-cost_elem = gt_hz_output-cbys.                             "成本要素
    ENDIF.
    ls_item-currency = 'CNY'.
    APPEND ls_item TO ls_wbs_costlines-cost_lines.
    APPEND ls_wbs_costlines TO lt_wbs_costlines.

    CALL FUNCTION 'CNECP_MAINTAIN'
      EXPORTING
        i_proj_id       = gt_hz_output-posid                           "项目编号
        i_version       = p_bb2                                       "版本
        i_cost_variant  = 'PS06'                                      "核算变式
        i_wbs_costlines = lt_wbs_costlines                            "内表
      IMPORTING
        messages        = gt_return.

    READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.
        L4 = L4 + 1. "统计导入100后最新版本的数据  IT02 20160307
      gt_h_output-type = icon_green_light.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
            EXPORTING
              i_proj_id       = gt_hz_output-posid                           "项目定义
              i_version       = '100'                                         "版本
              i_cost_variant  = 'PS06'                                      "核算变式
              i_wbs_costlines = lt_wbs_costlines                            "内表
            IMPORTING
              messages        = gt_return1.
      READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
      IF sy-subrc NE 0.
         L2 = L2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
       ELSE.
         CONCATENATE gs_return-message gs_return1-message INTO gt_hz_output-message.
         CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
     ENDIF.

    ELSE.
      gt_hz_output-type = icon_red_light.
      gt_hz_output-message = gs_return-message.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.


    MODIFY gt_hz_output TRANSPORTING hpeinh type message.

    CLEAR gt_hz_output.

    CLEAR:ls_item,ls_wbs_costlines-cost_lines,ls_wbs_costlines.
    REFRESH lt_wbs_costlines.
    CLEAR:gs_return.
    REFRESH:gt_return.

    "js = js + 1.
  ENDLOOP.
  ELSE.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
  "增加消息提示
  "100版本
  msg100 = l2 .
  CONDENSE msg100 NO-GAPS.
  CONCATENATE '100版本已成功导入:'   msg100 '条数量' INTO msg100 .
  "100后版本
  msg101 = l4 .
  CONDENSE msg101 NO-GAPS.
  CONCATENATE p_bb2  '版本已成功导入'  msg101 '条数量' INTO msg101.
  CALL SCREEN 0101 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CHZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_gt_chz_output .
  CLEAR :sc_len .
  SELECT psphi
    FROM prps
    INTO CORRESPONDING FIELDS OF TABLE gt_psphi
    FOR ALL ENTRIES IN gt_chz_output
    WHERE posid = gt_chz_output-posid.
  IF gt_psphi[] IS NOT INITIAL.
    SELECT topnr versn                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid1
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid1[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn1                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid1
        WHERE topnr = gt_objid1-topnr
        AND versn BETWEEN '000' AND '099'.
    ENDIF.

    SELECT topnr versn                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE gt_objid2
      FROM precp1
      JOIN proj ON precp1~topnr = proj~objnr
      FOR ALL ENTRIES IN gt_psphi
      WHERE proj~pspnr = gt_psphi-psphi.

    IF gt_objid2[] IS NOT INITIAL.
      SELECT versn "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE gt_versn2                        "取一个对象编号下对应的当前最大的版本号
        FROM precp1
        FOR ALL ENTRIES IN gt_objid2
        WHERE topnr = gt_objid2-topnr
        AND versn BETWEEN '100' AND '199'.
    ENDIF.

  ENDIF.

  REFRESH:gt_objid1,gt_objid2.

  DATA:p_bb1 TYPE precp1-versn.

  SORT gt_versn1 BY versn.
  SORT gt_versn2 BY versn.

  IF gt_versn1[] IS NOT INITIAL.
    LOOP AT gt_versn1 WHERE versn >= '000' AND versn <= '099'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn1-versn = gt_versn1-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn1-versn
          IMPORTING
            output = gt_versn1-versn.

        p_bb1 = gt_versn1-versn.
      ENDAT.
    ENDLOOP.
  ELSEIF gt_versn1[] IS INITIAL.
    p_bb1 = '001'.
  ENDIF.

  DATA:p_bb2 TYPE precp1-versn.

  IF gt_versn2[] IS NOT INITIAL.
    LOOP AT gt_versn2 WHERE versn >= '100' AND versn <= '199'.
      AT END OF versn.                                                  "取出了最大的版本号
        gt_versn2-versn = gt_versn2-versn + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gt_versn2-versn
          IMPORTING
            output = gt_versn2-versn.

        p_bb2 = gt_versn2-versn.
      ENDAT.
    ENDLOOP.
  ELSE.
    p_bb2 = 101.
  ENDIF.

  SORT gt_chz_output BY matnr.

  REFRESH it_jg.
  LOOP AT gt_chz_output.
    AT NEW matnr.
      READ TABLE gt_chz_output INDEX sy-tabix.
      IF sy-subrc = 0.
        wa_jg-matnr = gt_chz_output-matnr.
        wa_jg-htdj = gt_chz_output-hgpreis.
        wa_jg-cldj = gt_chz_output-cgpreis.
        wa_jg-rgdj = gt_chz_output-fpreis.
        APPEND wa_jg TO it_jg.
      ENDIF.
    ENDAT.
    CLEAR gt_chz_output.
  ENDLOOP.

  REFRESH it_jghj.
  LOOP AT gt_chz_output WHERE matnr <> ''.
    CLEAR wa_jghj.
    wa_jghj-matnr = gt_chz_output-matnr.
    wa_jghj-hgpreis = gt_chz_output-hgpreis.
    wa_jghj-cgpreis = gt_chz_output-cgpreis.
    wa_jghj-fpreis = gt_chz_output-fpreis.
    COLLECT wa_jghj INTO it_jghj.
  ENDLOOP.

  LOOP AT it_jg INTO wa_jg.
    READ TABLE it_jghj INTO wa_jghj WITH KEY matnr = wa_jg-matnr.
    IF sy-subrc = 0.
      CLEAR g.
      LOOP AT gt_chz_output WHERE matnr = wa_jghj-matnr.
        g = g + 1.
      ENDLOOP.
      w = wa_jghj-hgpreis / g.
      ll = wa_jghj-cgpreis / g.
      jj = wa_jghj-fpreis / g.


      IF w <> wa_jg-htdj.
        CONCATENATE '物料' wa_jghj-matnr '的合同单价不一致！' INTO message1.
        MESSAGE message1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF ll <> wa_jg-cldj.
        CONCATENATE '物料' wa_jghj-matnr '的材料单价不一致！' INTO message2.
        MESSAGE message2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF jj <> wa_jg-rgdj.
        CONCATENATE '物料' wa_jghj-matnr '的人工单价不一致！' INTO message3.
        MESSAGE message3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:wa_jg,w,ll,jj,message1,message2,message3.
  ENDLOOP.

  DELETE gt_chz_output WHERE posid = ''.

  CLEAR l.
  LOOP AT gt_chz_output.
    l = l + 1.
  ENDLOOP.

  REFRESH gt_hb.
  LOOP AT gt_chz_output WHERE matnr <> ''.
    CLEAR gs_hb.
    gs_hb-matnr = gt_chz_output-matnr.
    gs_hb-hpeinh = gt_chz_output-hpeinh.
    gs_hb-jpeinh = gt_chz_output-jpeinh.
    COLLECT gs_hb INTO gt_hb.
  ENDLOOP.

  LOOP AT gt_chz_output WHERE matnr = ''.
    APPEND gt_chz_output TO gt_chz_output1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_chz_output COMPARING matnr.

  DELETE gt_chz_output WHERE matnr = ''.

  LOOP AT gt_chz_output1.
    APPEND gt_chz_output1 TO gt_chz_output.
  ENDLOOP.

  CLEAR j.
  LOOP AT gt_chz_output.
    j = j + 1.
  ENDLOOP.

  IF j < l.
    MESSAGE '物料号相同的只被导入一次，且相同物料的计划工程量和合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.

  "先检查一下可控的行错误信息.

  LOOP AT gt_chz_output INTO gs_chz_output.

    IF gs_chz_output-posid IS INITIAL.

      gs_chz_output-type = icon_red_light.                             "状态
      gs_chz_output-message = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF gs_chz_output-xmlb = 'M'.
      IF gs_chz_output-matnr IS  INITIAL. .
        gs_chz_output-type = icon_red_light.                             "状态
        gs_chz_output-message = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
        READ TABLE gt_check_matnr INTO gs_check_matnr WITH KEY
               matnr = gs_chz_output-matnr BINARY SEARCH .
      IF sy-subrc NE 0 .
        gs_chz_output-type = icon_red_light.     "状态
        CONCATENATE '1800工厂' '不存在:'      gs_chz_output-matnr '物料号' INTO    gs_chz_output-message ."消息
      ENDIF.
    ENDIF.





    IF gs_chz_output-xmlb = 'V' AND gs_chz_output-cbys IS  INITIAL..

      gs_chz_output-type = icon_red_light.                             "状态
      gs_chz_output-message = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY gt_chz_output FROM gs_chz_output.
  ENDLOOP.

*-----------------先删除100 、100版本增值税 it02 20160308
  READ TABLE  gt_chz_output WITH KEY type = icon_red_light.
  IF sy-subrc NE 0 .
    CLEAR:proj_objnr,prps_objnr.
    READ TABLE gt_chz_output INDEX 1.
    IF sy-subrc EQ 0 .
      SELECT SINGLE  objnr
           INTO proj_objnr
           FROM proj
           WHERE pspid = gt_chz_output-posid.
      SELECT SINGLE objnr
          INTO prps_objnr
          FROM  prps
          WHERE posid = gt_chz_output-posid.
      IF proj_objnr NE '' AND prps_objnr NE ''.
        SELECT subnr versn  kalnr topnr
         FROM precp2
         INTO CORRESPONDING FIELDS OF TABLE it_kalnr
         WHERE topnr = proj_objnr
         AND subnr   = prps_objnr
         AND versn = '100'.
        IF it_kalnr IS NOT INITIAL.
          SELECT *
               FROM ckis
               INTO CORRESPONDING FIELDS OF TABLE it_ckis
               FOR ALL ENTRIES IN it_kalnr
               WHERE kalnr = it_kalnr-kalnr.
        ENDIF.
        SELECT subnr versn  kalnr topnr
              FROM precp2
              INTO CORRESPONDING FIELDS OF TABLE it_kalnr1
              WHERE topnr = proj_objnr
              AND subnr   = prps_objnr
              AND versn = '000'.

        " IF it_kalnr IS NOT INITIAL.
        IF it_kalnr1 IS NOT INITIAL.
          SELECT *
            FROM ckis
            INTO CORRESPONDING FIELDS OF TABLE it_ckis1
            FOR ALL ENTRIES IN it_kalnr1
            WHERE kalnr = it_kalnr1-kalnr.

        ENDIF.
      ENDIF.
      REFRESH: lt_wbs_costlines3,gt_return5,lt_wbs_costlines4,gt_return5,ls_wbs_costlines3-cost_lines,ls_wbs_costlines4-cost_lines.
      CLEAR:ls_wbs_costlines3,gs_return5,ls_wbs_costlines4,gs_return5.
      IF it_ckis IS NOT INITIAL.
        LOOP AT it_ckis INTO wa_ckis.
          CLEAR:ls_item3.
          AT FIRST .
            ls_wbs_costlines3-wbs_name = gt_chz_output-posid.
          ENDAT.
          ls_item3-item_number = wa_ckis-posnr.
          ls_item3-flag_delete_item = 'X'.
          APPEND ls_item3 TO ls_wbs_costlines3-cost_lines.
          AT LAST .
            APPEND ls_wbs_costlines3 TO lt_wbs_costlines3.
          ENDAT.


        ENDLOOP.
        CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
          EXPORTING
            i_proj_id       = gt_chz_output-posid                           "项目定义
            i_version       = '100'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines3                            "内表
          IMPORTING
            messages        = gt_return5.

        READ TABLE gt_return5 INTO gs_return5 WITH KEY type = 'E'.
        IF sy-subrc EQ 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CLEAR:ermsg .
          CONCATENATE '该项目' gt_chz_output-posid  '删除100版本失败：' gs_return5-message  INTO ermsg .
          MESSAGE ermsg  TYPE 'E'.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          WAIT UP TO 0 SECONDS .
        ENDIF.
      ENDIF.
      "再重新读取100版本的预算项目信息
      IF it_kalnr IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis
             FOR ALL ENTRIES IN it_kalnr
             WHERE kalnr = it_kalnr-kalnr.
      ENDIF.


      IF it_ckis1 IS NOT INITIAL.
        LOOP AT it_ckis1 INTO wa_ckis1.
          CLEAR:ls_item4.
          AT FIRST .
            ls_wbs_costlines4-wbs_name = gt_chz_output-posid.
          ENDAT.
          ls_item4-item_number = wa_ckis1-posnr.
          ls_item4-flag_delete_item = 'X'.
          APPEND ls_item4 TO ls_wbs_costlines4-cost_lines.
          AT LAST .
            APPEND ls_wbs_costlines4 TO lt_wbs_costlines4.
          ENDAT.

        ENDLOOP.

        CALL FUNCTION 'CNECP_MAINTAIN'          "删除0版本
          EXPORTING
            i_proj_id       = gt_chz_output-posid                           "项目定义
            i_version       = '000'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines4                            "内表
          IMPORTING
            messages        = gt_return4.

        READ TABLE gt_return4 INTO gs_return4 WITH KEY type = 'E'.
        IF sy-subrc EQ 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CLEAR:ermsg .
          CONCATENATE '该项目' gt_chz_output-posid  '删除0版本失败：' gs_return4-message  INTO ermsg .
          MESSAGE ermsg  TYPE 'E'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          WAIT UP TO 0 SECONDS .
        ENDIF.
      ENDIF.
      "再重新读取000版本的预算项目信息
      IF it_kalnr1 IS NOT INITIAL.
        SELECT *
             FROM ckis
             INTO CORRESPONDING FIELDS OF TABLE it_ckis1
             FOR ALL ENTRIES IN it_kalnr1
             WHERE kalnr = it_kalnr1-kalnr.
      ENDIF.

    ENDIF.
    ELSE.
        MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.
  " *-----------------先删除100 、100版本增值税 it02 20160308 end-------------------

  IF it_ckis1 IS INITIAL AND it_ckis IS INITIAL.
    CLEAR:l1,l2,l3,l4.   "初始导入版本数量



    DATA:l_index TYPE sy-tabix.
    LOOP AT gt_chz_output.
      CLEAR:ls_item,ls_item1.
      l_index = sy-tabix.


      AT FIRST .
        READ TABLE gt_chz_output INDEX 1 .
        IF sy-subrc EQ 0 .
          ls_wbs_costlines-wbs_name = gt_chz_output-posid.                   "WBS
          ls_wbs_costlines1-wbs_name = gt_chz_output-posid.                  "WBS
        ENDIF.

      ENDAT.

      "序号
      ls_item-item_number = l_index .                                      "序号
      ls_item1-item_number = l_index .                                      "序号

      ls_item-resource-matnr = gt_chz_output-matnr.                       "物料号
      ls_item1-resource-matnr = gt_chz_output-matnr.                       "物料号


      IF  ls_item1-resource-matnr IS NOT INITIAL.
        READ TABLE gt_check_matnr INTO gs_check_matnr WITH KEY
                  matnr = gt_chz_output-matnr BINARY SEARCH .
        IF sy-subrc EQ 0 .
          ls_item-quantity-unit_of_measure = gs_check_matnr-meins.                 "单位
          ls_item1-quantity-unit_of_measure = gs_check_matnr-meins.                "单位

        ENDIF.

      ENDIF.

      IF gt_chz_output-cbys IS NOT INITIAL.
        ls_item-cost_elem = gt_chz_output-cbys.                          "成本要素
        ls_item1-cost_elem = gt_chz_output-cbys.                          "成本要素

      ENDIF.

      " gt_chz_output-type = icon_yellow_light.                             "状态

      ls_item-resource-typps = gt_chz_output-xmlb.                        "项目类别
      ls_item1-resource-typps = gt_chz_output-xmlb.                       "项目类别

      ls_item-resource-werks = gt_chz_output-werks.                     "工厂
      ls_item1-resource-werks = gt_chz_output-werks.                    "工厂



      ls_item-descript       = gt_chz_output-maktx.                       "物料描述
      ls_item1-descript       = gt_chz_output-maktx.                       "物料描述

      ls_item1-price-fixed    = gt_chz_output-fpreis.                      "人工单价

      READ TABLE gt_hb INTO gs_hb WITH KEY matnr = gt_chz_output-matnr.
      IF sy-subrc = 0.
        gt_chz_output-hpeinh = gs_hb-hpeinh.
        gt_chz_output-jpeinh = gs_hb-jpeinh.
      ENDIF.

      ls_item-quantity-quantity = gt_chz_output-hpeinh.                   "合同工程量
      ls_item-price-total = gt_chz_output-hgpreis.                        "合同单价
      ls_item1-quantity-quantity = gt_chz_output-jpeinh.                   "计划工程量
      ls_item1-price-total = gt_chz_output-cgpreis.                        "材料单价
      ls_item-currency = 'CNY'.
      ls_item1-currency = 'CNY'.
      APPEND ls_item TO ls_wbs_costlines-cost_lines.
      APPEND ls_item1 TO ls_wbs_costlines1-cost_lines.
      AT LAST .
        APPEND ls_wbs_costlines TO lt_wbs_costlines.
        APPEND ls_wbs_costlines1 TO lt_wbs_costlines1.

        APPEND ls_wbs_costlines TO lt_wbs_costlines5.

        APPEND ls_wbs_costlines1 TO lt_wbs_costlines6.

      ENDAT.

    ENDLOOP.
    READ TABLE  gt_chz_output WITH KEY type = icon_red_light.
    IF sy-subrc NE 0 .
      "创建 1000版本往下的数据
      CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
        EXPORTING
          i_proj_id       = gt_chz_output-posid                          "项目编号
          i_version       = p_bb2                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines                            "内表
        IMPORTING
          messages        = gt_return.
      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        " DESCRIBE TABLE ls_wbs_costlines-cost_lines LINES l4  .

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
          EXPORTING
            i_proj_id       = gt_chz_output-posid                           "项目定义
            i_version       = '100'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines5                            "内表
          IMPORTING
            messages        = gt_return2.
        READ TABLE gt_return2 INTO gs_return2 WITH KEY type = 'E'.
        IF sy-subrc <> 0.
          l2 = l4 .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          MESSAGE '上传合同报价（增值税）成功！'  TYPE 'I'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        LOOP AT gt_return INTO gs_return  WHERE  type = 'E'.
          IF e1_msg IS INITIAL .
            e1_msg =    '上传合同报价（增值税）错误:' .
          ELSE.
            CONCATENATE  e1_msg gs_return-message INTO e1_msg .
          ENDIF.
        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        MESSAGE e1_msg TYPE 'I'.    "消息提示
      ENDIF.

      "创建000版本以下数据
      CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
        EXPORTING
          i_proj_id       = gt_chz_output-posid                          "项目编号
          i_version       = p_bb1                                       "版本
          i_cost_variant  = 'PS06'                                      "核算变式
          i_wbs_costlines = lt_wbs_costlines1                           "内表
        IMPORTING
          messages        = gt_return1.

      READ TABLE gt_return1 INTO gs_return1 WITH KEY type = 'E'.
      IF sy-subrc <> 0 .
        " DESCRIBE TABLE ls_wbs_costlines-cost_lines LINES l3  .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        "  创建0版本
        CALL FUNCTION 'CNECP_MAINTAIN'
          EXPORTING
            i_proj_id       = gt_chz_output-posid                           "项目定义
            i_version       = '000'                                         "版本
            i_cost_variant  = 'PS06'                                      "核算变式
            i_wbs_costlines = lt_wbs_costlines6                            "内表
          IMPORTING
            messages        = gt_return3.

        READ TABLE gt_return3 INTO gs_return3 WITH KEY type = 'E'.
        IF sy-subrc <> 0 .
          l2 = l3.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          MESSAGE '上传成本预算（增值税）成功！'   TYPE 'I'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT gt_return1 INTO gs_return1  WHERE  type = 'E'.
          IF e1_msg IS INITIAL .
            e2_msg =    '上传成本预算（增值税）错误:' .
          ELSE.
            CONCATENATE  e1_msg gs_return1-message INTO e2_msg .
          ENDIF.
        ENDLOOP.

        MESSAGE e2_msg TYPE 'I'.    "消息提示
      ENDIF.
    ELSE.
      MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
    ENDIF.

  ELSEIF it_ckis IS NOT INITIAL.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ELSEIF it_ckis1 IS NOT INITIAL.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*  "增加消息提示
*  "000版本
*  msg000 = l1 .
*  CONDENSE msg000 NO-GAPS.
*  CONCATENATE '0版本已成功导入:'   msg000 '条数量' INTO msg000 .
*  "100版本
*  msg100 = l2 .
*  CONDENSE msg100 NO-GAPS.
*  CONCATENATE '100版本已成功导入:'   msg100 '条数量' INTO msg100 .
*  "000后版本
*  msg001 = l3 .
*  CONDENSE msg001 NO-GAPS.
*  CONCATENATE p_bb1   '版本已成功导入' msg001 '条数量' INTO  msg001.
*  "100后版本
*  msg101 = l4 .
*  CONDENSE msg101 NO-GAPS.
*  CONCATENATE p_bb2  '版本已成功导入'  msg101 '条数量' INTO msg101.
*  CALL SCREEN 0101 STARTING AT 25 10.
*  CLEAR:p_bb1,p_bb2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'STA0101'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.


FORM STANDARD_FULLSCREEN USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
