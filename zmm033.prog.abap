REPORT zmm033.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160824
*& Request       :
*& Descriptions  : 采购订单打印
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
** 修改日期   开发人员  请求号        描述
" 20170228   IT02     ED1K905271  格式3-外协订单：打印条款内容变更
" 20170310   IT02     ED1K905295  外协3：标准价格为：EKPO上物料的标准价 - 子件物料的标准价
************************************************************************
* Tables
************************************************************************
TABLES:ekko,ekpo,lfa1.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        ebeln  TYPE ekko-ebeln , "采购订单
        bukrs  TYPE ekko-bukrs,   "公司代码
        bsart  TYPE ekko-bsart,  "采购类型
        waers  TYPE ekko-waers,   "订单货币
        zterm  TYPE ekko-zterm,   "付款条件
        "       zterm_text TYPE T052-TEXT1,  "付款条件
        ebelp  TYPE ekpo-ebelp,  "采购行项目
        matnr  TYPE ekpo-matnr, "物料号
        txz01  TYPE ekpo-txz01, "描述
        menge  TYPE ekpo-menge, "数量
        meins  TYPE ekpo-meins,   "单位
        netpr  TYPE ekpo-netpr,
        werks  TYPE ekpo-werks,   "工厂
        pstyp  TYPE ekpo-pstyp,   "项目类别
        peinh  TYPE ekpo-peinh,   "价格单位
        prdat  TYPE ekpo-prdat,  "价格确定日期
        lifnr  TYPE ekko-lifnr,  "供应商
        name1  TYPE lfa1-name1,  "供应商名称
        banfn  TYPE ekpo-banfn,   "采购申请编号
        bnfpo  TYPE ekpo-bnfpo,   "采购申请的项目编号
        ekorg  TYPE ekko-ekorg,  "采购组织
        ekgrp  TYPE ekko-ekgrp,  "采购组
        aedat  TYPE ekko-aedat,   "采购订单日期
        knumv  TYPE konv-knumv,   "单据条件
        kbetr  TYPE konv-kbetr,   "合同单价
        htjj   TYPE ekpo-netwr,   "合同净价
        bzjg   TYPE mbew-stprs,   "标准价格
        bzjgdw TYPE mbew-peinh,    "标准价格单位
        zjcgjj TYPE ekpo-netwr,    "最近采购净价
        jjbl   TYPE ekpo-netwr,    "降价比例
        jjbl_% TYPE string,
        bhhtje TYPE tslvt12,    "本行合同金额
        xmmc   TYPE string,        "项目名称
        ppv    TYPE mbew-stprs,    "PPV
        gsmng  TYPE gsmng,        "计划数量
        sqsl   TYPE ekpo-menge,   "申请数量
        cgcsg  TYPE eban-menge,   "采购超申购数量
        cjhsl  TYPE ekpo-menge,   "超计划数量
        cjhje  TYPE  tslvt12,  "超计划金额
        cjhl   TYPE  konv-kbetr,  "超级率
        cjhl_1 TYPE  string,
        zsel ,
      END OF ty_data .

TYPES:BEGIN OF ty_wx_hz,
        ebeln TYPE ekpo-ebeln,  "采购订单
        ebelp TYPE ekpo-ebelp,  "采购明细
        zjbzj TYPE konv-kbetr, "子件物料标准价格
      END OF ty_wx_hz.


TYPES:BEGIN OF ty_header_1,
        ebeln     TYPE ekko-ebeln,
        bsart     TYPE ekko-bsart,  "订单类型
        zterm     TYPE lfm1-zterm,   "付款条件
        zterm_txt TYPE t052u-text1, "付款条件文本
        aedat     TYPE ekko-aedat,   "订单日期
        xfdw      TYPE string,       "需方单位
        xfdh      TYPE string,       "需方电话
        xfcz      TYPE string,       "需方传真
        xfdz      TYPE ad_smtpadr,   "需方地址
        xfywy     TYPE c LENGTH 60, "需方业务员
        waers     TYPE ekko-waers,   "货币码
        lifnr     TYPE ekko-lifnr,  "供应商号
        name1     TYPE lfa1-name1,   "供应商全称
        stras     TYPE lfa1-stras,  "供方地址
        telf1     TYPE lfa1-telf1,  "供方电话
        telfx     TYPE lfa1-telfx,   "供方传真
        ysfs      TYPE c LENGTH 50,  "运输方式
        thfs      TYPE c LENGTH 50,  "提货方式
        hhzlyq    TYPE c LENGTH 50,  "货物质量要求
        zbq       TYPE c LENGTH 20,  "质保期
        "   zjhjdx TYPE C LENGTH 200, "总价合计大写
      END OF ty_header_1 .

TYPES:BEGIN OF ty_item_1,
        ebeln     TYPE ekko-ebeln,
        ebelp     TYPE ekpo-ebelp,  "采购行项目
        matnr     TYPE ekpo-matnr,   "物料编号
        txz01     TYPE ekpo-txz01,  "描述
        meins     TYPE ekpo-meins,   "单位
        menge     TYPE ekpo-menge,  "数量
        waers     TYPE waers,       "货币码
        kbetr     TYPE konv-kbetr,  "含税单价
        peinh     TYPE ekpo-peinh,   "价格单位
        kwert     TYPE konv-kwert,  "价税合计
        kbetr_tax TYPE i ,    "税率
        eindt     TYPE eket-eindt ,        "交货日期
*        sqsl      TYPE ekpo-menge,   "申请数量
*        shsl      TYPE ekpo-menge,   "损耗数量
        "zjhj  TYPE konv-kwert,  "总价合计
        " slhj  TYPE EKPO-MENGE,   "

      END OF ty_item_1.

TYPES:BEGIN OF ty_header_2,
        ebeln  TYPE ekko-ebeln,
        xfdw   TYPE string,   "需方单位
*        xfdh   TYPE string,   "需方电话
*        xfcz   TYPE string,   "需方传真
        "   xfdz   TYPE ad_smtpadr,   "需方地址
        lifnr  TYPE ekko-lifnr,  "供应商号
        name1  TYPE lfa1-name1,   "卖方名
        stras  TYPE lfa1-stras,  "地址
        telf1  TYPE lfa1-telf1,  "电话
        telfx  TYPE lfa1-telfx,   "传真
        zjhj   TYPE konv-kwert,  "总价合计
        waers  TYPE ekko-waers,   "货币码
        zjhjdx TYPE c LENGTH 200, "总价合计大写
      END OF ty_header_2 .

TYPES:BEGIN OF ty_item_2,
        ebeln TYPE ekko-ebeln,
        ebelp TYPE ekpo-ebelp,  "采购行项目
        txz01 TYPE ekpo-txz01,  "描述
        anln1 TYPE ekkn-anln1,  "资产编号
        kbetr TYPE konv-kbetr,  "单价
        kwert TYPE konv-kwert,  "总价
        waers TYPE konv-waers,  "货币码
        menge TYPE ekpo-menge,  "数量
        meins TYPE ekpo-meins,   "单位
      END OF ty_item_2.

TYPES:BEGIN OF ty_header_3,
        ebeln TYPE ekko-ebeln,
        bsart TYPE ekko-bsart,  "订单类型
        aedat TYPE ekko-aedat,   "订单日期
        xfdw  TYPE string,   "需方单位
        xfdh  TYPE string,   "需方电话
        xfcz  TYPE string,   "需方传真
        xfywy TYPE c LENGTH 60, "需方业务员
        waers TYPE ekko-waers,   "货币码
        lifnr TYPE ekko-lifnr,  "供应商号
        name1 TYPE lfa1-name1,   "供应商全称
        stras TYPE lfa1-stras,  "供方地址
        telf1 TYPE lfa1-telf1,  "供方电话
        telfx TYPE lfa1-telfx,   "供方传真s
        "   zjhjdx TYPE C LENGTH 200, "总价合计大写
      END OF ty_header_3 .

TYPES:BEGIN OF ty_item_3,
        ebeln     TYPE ekko-ebeln,
        ebelp     TYPE ekpo-ebelp,  "采购行项目
        matnr     TYPE ekpo-matnr,   "物料编号
        txz01     TYPE ekpo-txz01,  "描述
        meins     TYPE ekpo-meins,   "单位
        menge     TYPE ekpo-menge,  "数量
        waers     TYPE waers,       "货币码
        kbetr     TYPE konv-kbetr,  "含税单价
        peinh     TYPE ekpo-peinh,   "价格单位
        kwert     TYPE konv-kwert,  "价税合计
        kbetr_tax TYPE i ,    "税率
        eindt     TYPE  eket-eindt ,        "交货日期
        sqsl      TYPE ekpo-menge,   "申请数量
        shsl      TYPE ekpo-menge,   "损耗数量


      END OF ty_item_3.

TYPES:BEGIN OF ty_header_4,
        ebeln  TYPE ekko-ebeln,
        xfdw   TYPE string,   "需方单位
*        xfdh   TYPE string,   "需方电话
*        xfcz   TYPE string,   "需方传真
*        xfdz   TYPE ad_smtpadr,   "需方地址
        lifnr  TYPE ekko-lifnr,  "供应商号
        name1  TYPE lfa1-name1,   "卖方名
        stras  TYPE lfa1-stras,  "地址
        telf1  TYPE lfa1-telf1,  "电话
        telfx  TYPE lfa1-telfx,   "传真
        zjhj   TYPE konv-kwert,  "总价合计
        waers  TYPE ekko-waers,  "货币码
        zjhjdx TYPE c LENGTH 200, "总价合计大写
      END OF ty_header_4 .

TYPES:BEGIN OF ty_item_4,
        ebeln TYPE ekko-ebeln,
        ebelp TYPE ekpo-ebelp,  "采购行项目
        txz01 TYPE ekpo-txz01,  "描述
        anln1 TYPE ekkn-anln1,  "资产编号
        kbetr TYPE konv-kbetr,  "单价
        kwert TYPE konv-kwert,  "总价
        waers TYPE konv-waers,  "货币码
        menge TYPE ekpo-menge,  "数量
        meins TYPE ekpo-meins,   "单位
      END OF ty_item_4.

TYPES:BEGIN OF ty_lfa1,

        lifnr TYPE lfb1-lifnr,  "供应商
        bukrs TYPE lfb1-bukrs,  "公司代码
        name1 TYPE lfa1-name1,  "卖方
        stras TYPE lfa1-stras,  "地址
        telf1 TYPE lfa1-telf1,    "电话
        telfx TYPE lfa1-telfx,   "传真
      END OF ty_lfa1 .

TYPES:BEGIN OF ty_konv,
        knumv TYPE konv-knumv,
        kposn TYPE konv-kposn,
        stunr TYPE konv-stunr,
        zaehk TYPE konv-zaehk,
        kschl TYPE konv-kschl,
        kbetr TYPE konv-kbetr,
        kwert TYPE konv-kwert,  "总价
        waers TYPE konv-waers,  "货币码
      END OF ty_konv .

TYPES:BEGIN OF ty_ekkn,
        ebeln TYPE ekkn-ebeln,
        ebelp TYPE ekkn-ebelp,
        zekkn TYPE ekkn-zekkn,
        anln1 TYPE ekkn-anln1,
      END OF ty_ekkn .

DATA:gs_lfm1 TYPE lfm1,
     gt_lfm1 TYPE TABLE OF lfm1.

DATA:gt_t052u TYPE TABLE OF t052u,
     gs_t052u TYPE t052u.

DATA:gt_konv TYPE TABLE OF ty_konv,
     gs_konv TYPE ty_konv.

DATA:gt_konv_z010 TYPE TABLE OF ty_konv,
     gs_konv_z010 TYPE ty_konv.

DATA:gt_lfa1 TYPE TABLE OF ty_lfa1,
     gs_lfa1 TYPE ty_lfa1.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_data_wx TYPE TABLE OF ty_data,
     gs_data_wx TYPE ty_data.

DATA:gt_data_n TYPE TABLE OF ty_data,
     gs_data_n TYPE ty_data.


DATA:gt_data_01 TYPE TABLE OF ty_data,
     gs_data_01 TYPE ty_data.

DATA:gt_data_02 TYPE TABLE OF ty_data,
     gs_data_02 TYPE ty_data.

DATA:gt_data_03 TYPE TABLE OF ty_data,
     gs_data_03 TYPE ty_data.

DATA:gt_data_04 TYPE TABLE OF ty_data,
     gs_data_04 TYPE ty_data.

"打印格式1的模板 数据 定义 begin

DATA:gt_header_1 TYPE TABLE OF ty_header_1,
     gs_header_1 TYPE ty_header_1.

DATA:hd_prt_1 TYPE TABLE OF ty_header_1,
     hd_prs_1 TYPE ty_header_1.

DATA:gt_item_1 TYPE TABLE OF ty_item_1,
     gs_item_1 TYPE ty_item_1.

DATA:mx_prt_1 TYPE TABLE OF ty_item_1,
     mx_prs_1 TYPE ty_item_1.

"打印格式1的模板 数据 定义 end

"打印格式2的模板 数据 定义 begin

DATA:gt_header_2 TYPE TABLE OF ty_header_2,
     gs_header_2 TYPE ty_header_2.

DATA:hd_prt_2 TYPE TABLE OF ty_header_2,
     hd_prs_2 TYPE ty_header_2.

DATA:gt_item_2 TYPE TABLE OF ty_item_2,
     gs_item_2 TYPE ty_item_2.

DATA:mx_prt_2 TYPE TABLE OF ty_item_2,
     mx_prs_2 TYPE ty_item_2.

"打印格式2的模板 数据 定义 end

"打印格式3的模板 数据 定义 begin

DATA:gt_header_3 TYPE TABLE OF ty_header_3,
     gs_header_3 TYPE ty_header_3.

DATA:hd_prt_3 TYPE TABLE OF ty_header_3,
     hd_prs_3 TYPE ty_header_3.

DATA:gt_item_3 TYPE TABLE OF ty_item_3,
     gs_item_3 TYPE ty_item_3.

DATA:mx_prt_3 TYPE TABLE OF ty_item_3,
     mx_prs_3 TYPE ty_item_3.

"打印格式4的模板 数据 定义 end


"打印格式4的模板 数据 定义 begin

DATA:gt_header_4 TYPE TABLE OF ty_header_4,
     gs_header_4 TYPE ty_header_4.

DATA:hd_prt_4 TYPE TABLE OF ty_header_4,
     hd_prs_4 TYPE ty_header_4.

DATA:gt_item_4 TYPE TABLE OF ty_item_4,
     gs_item_4 TYPE ty_item_4.

DATA:mx_prt_4 TYPE TABLE OF ty_item_4,
     mx_prs_4 TYPE ty_item_4.

"打印格式4的模板 数据 定义 end

DATA:gt_eket TYPE  TABLE OF eket WITH HEADER LINE,
     gs_eket TYPE eket.


DATA:gt_eban TYPE TABLE OF eban WITH HEADER LINE,
     gs_eban TYPE eban.

DATA:gt_t024 TYPE TABLE OF t024 WITH HEADER LINE,
     gs_t024 TYPE t024.

DATA:gt_t001 TYPE TABLE OF t001 WITH HEADER LINE,
     gs_t001 TYPE t001.

DATA:gt_zmd14 TYPE TABLE OF zmd14_zhrz WITH HEADER LINE,
     gs_zmd14 TYPE zmd14_zhrz.

DATA:gt_wx_hz TYPE TABLE OF ty_wx_hz,
     gs_wx_hz TYPE ty_wx_hz.

*DATA:gt_lfm1 TYPE TABLE OF lfm1 WITH HEADER LINE,
*     gs_lfm1 TYPE lfm1.
*
*
*DATA:gt_t052u TYPE TABLE OF t052u WITH HEADER LINE,
*     gs_t052u TYPE t052u.

"打印变量
DATA: control    TYPE ssfctrlop,
      ntotalline TYPE i,
      npageline  TYPE i VALUE 6,
      p_index    LIKE sy-tabix.
DATA: emptycount      TYPE i VALUE 0,  "空行数.
      ncurrline       TYPE i,      "中间变量
      job_output_info TYPE ssfcrescl.

DATA:g_tabix  TYPE sy-tabix .

DATA:g_hj TYPE konv-kwert.   "合计值

DATA: g_name TYPE rs38l_fnam.
DATA:l_formname TYPE tdsfname ."VALUE 'ZSFMM033_4'.

DATA:gt_ekkn TYPE TABLE OF ty_ekkn,
     gs_ekkn TYPE ty_ekkn.

DATA:gt_mbew TYPE TABLE OF mbew,
     gs_mbew TYPE mbew.

DATA:g_zjhj TYPE konv-kwert,  "总价合计
     g_slhj TYPE ekpo-menge.  "

DATA:gt_resb TYPE TABLE OF resb,
     gs_resb TYPE resb.

FIELD-SYMBOLS:  <fs_data> TYPE ty_data .

DATA l_tdname TYPE thead-tdname.

DATA x_style1 TYPE c VALUE 'X'.
DATA x_style2 TYPE c.
DATA x_style3 TYPE c.
DATA x_style4 TYPE c.
DATA g_style TYPE char10.
DATA ok_code TYPE char20.

***---------------------------------------------------------------
* 定制控制 编辑长文本对象定义
DATA:container TYPE REF TO cl_gui_custom_container,
     editor    TYPE REF TO cl_gui_textedit.
DATA: init,
      w_ysfs          TYPE c LENGTH 50,  "运输方式
      w_thfs          TYPE c LENGTH 50,  "提货方式
      w_hhzlyq(50)    TYPE c OCCURS 0,  "货物质量要求
      hhzlyq_line(50) TYPE c,
      w_zbq           TYPE c LENGTH 20. "质保期


************************************************************************
*      DEFINITION
************************************************************************
DEFINE init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

  if gw_lvc-fieldname eq 'MENGE'.
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'MEINS'.
  endif.


  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gw_layout        TYPE lvc_s_layo,                    "alv的格式
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

DATA g_edit TYPE c VALUE 'X'. "控制不可编辑

DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.

DATA:l_check TYPE c .



" ************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln."采购订单
SELECT-OPTIONS: s_bsart FOR ekko-bsart."采购订单类型
SELECT-OPTIONS: s_aedat FOR ekko-aedat."采购订单日期
"SELECT-OPTIONS: s_bukrs FOR ekko-bukrs."公司代码
"SELECT-OPTIONS: s_ekorg FOR ekko-ekorg."采购组织
SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp."采购组
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
*  PERFORM frm_auth_check.
*  CHECK L_CHECK IS INITIAL .
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

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
*  DATA:e_msg TYPE string .
*  REFRESH:gt_t001 .
*  CLEAR:L_CHECK .
*  SELECT * INTO TABLE gt_t001
*    FROM t001
*    WHERE bukrs IN s_bukrs .
*  LOOP AT gt_t001 INTO gs_t001 .
*    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
*        ID 'ACTVT' FIELD '03'
*        ID 'BUK' FIELD gs_t001-bukrs.
*    IF sy-subrc <> 0.
*      L_CHECK = 4 .
*      CLEAR:e_msg .
*      CONCATENATE '无权查询公司代码：'  gs_t001-bukrs  '采购订单信息'  INTO e_msg .
*      MESSAGE e_msg   TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDLOOP.
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
  SELECT a~ebeln  a~bukrs a~bsart a~ekorg a~ekgrp a~aedat a~knumv a~lifnr a~waers a~zterm
         b~ebelp b~matnr b~txz01 b~menge b~meins b~peinh b~banfn b~bnfpo b~netpr b~werks b~prdat b~pstyp
         INTO CORRESPONDING FIELDS OF TABLE gt_data
         FROM ekko AS a
         INNER JOIN ekpo AS b
         ON a~ebeln = b~ebeln
         WHERE a~ebeln IN s_ebeln
         AND  a~bsart IN s_bsart
         AND  a~aedat IN s_aedat
         AND a~bukrs IN ('2110')
        " AND  a~bukrs IN s_bukrs
        " AND  a~ekorg IN s_ekorg
        AND a~ekorg IN ('2110')
         AND  a~ekgrp IN s_ekgrp
         AND b~loekz NE 'L'.
  SORT gt_data BY ebeln ebelp ..

  "外协采购明细
  gt_data_wx = gt_data.

  DELETE gt_data_wx WHERE pstyp NE '3'.

  SORT gt_data_wx BY ebeln ebelp ...


  MOVE-CORRESPONDING gt_data TO gt_data_01 .

  CHECK gt_data IS NOT INITIAL.
  SELECT a~lifnr a~bukrs
         b~name1 b~stras
         b~telf1 b~telfx
    INTO CORRESPONDING  FIELDS OF TABLE gt_lfa1
    FROM lfb1 AS a
    INNER JOIN lfa1 AS b
    ON a~lifnr = b~lifnr
    WHERE a~bukrs   IN ('2110') .                " IN s_bukrs .

  SORT gt_lfa1 BY lifnr bukrs .

  SELECT knumv kposn stunr zaehk kschl  kbetr kwert waers
     INTO CORRESPONDING FIELDS OF TABLE gt_konv
     FROM konv
     FOR ALL ENTRIES IN gt_data
     WHERE knumv = gt_data-knumv
     AND kschl IN ('PBXX','PB00')
"     AND   kposn = gt_data_01-ebelp
     AND   kinak EQ space  ."激活状态

  SORT gt_konv BY knumv kposn .

  "查询采购申请数量
  SELECT * INTO TABLE gt_eban
    FROM eban
    FOR ALL ENTRIES IN gt_data_01
    WHERE banfn = gt_data_01-banfn
    AND   bnfpo = gt_data_01-bnfpo.

  SORT gt_eban BY banfn bnfpo .

  "查询存储ZMD14_ZHRZ :MD14： 计划订单转采购申请数量
  SELECT * INTO TABLE gt_zmd14
    FROM  zmd14_zhrz
    FOR  ALL ENTRIES IN gt_eban
    WHERE banfn = gt_eban-banfn
    AND   bnfpo = gt_eban-bnfpo .
  SORT gt_zmd14 BY banfn bnfpo .

  SELECT * INTO TABLE  gt_mbew
    FROM mbew
    WHERE bwkey IN ('2110').

  SORT gt_mbew BY matnr bwkey.

  IF  gt_data_wx  IS NOT INITIAL.
    SELECT * INTO TABLE gt_resb
 FROM resb
 FOR ALL ENTRIES IN gt_data_wx
 WHERE ebeln = gt_data_wx-ebeln
 AND   ebelp = gt_data_wx-ebelp .

    SORT gt_resb BY ebeln ebelp .
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

  DATA:gs_ekpo LIKE ekpo,
       gt_ekpo LIKE TABLE OF ekpo.

  LOOP AT gt_resb INTO gs_resb.
    CLEAR:gs_wx_hz.
    gs_wx_hz-ebeln = gs_resb-ebeln.
    gs_wx_hz-ebelp = gs_resb-ebelp.
    READ TABLE gt_data_wx INTO gs_data_wx WITH KEY ebeln =  gs_wx_hz-ebeln
                                             ebelp =  gs_wx_hz-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0 .
      READ TABLE gt_mbew INTO gs_mbew WITH KEY matnr = gs_resb-matnr
                                               bwkey = gs_resb-werks
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        "标准价MBEW-STPRS* EKPO-PEINH/MBEW-PEINH* RESB-BDMNG /EKPO-MENGE
        gs_wx_hz-zjbzj = gs_mbew-stprs * gs_data_wx-peinh / gs_mbew-peinh * gs_resb-bdmng / gs_data_wx-menge.

      ENDIF.

    ENDIF.
    COLLECT  gs_wx_hz INTO gt_wx_hz.
  ENDLOOP.
  SORT gt_wx_hz BY ebeln ebelp .

  LOOP AT gt_data ASSIGNING <fs_data>.

    IF <fs_data>-matnr IS NOT INITIAL .
      REFRESH:gt_ekpo .
      SELECT  * INTO TABLE gt_ekpo
        FROM ekpo
        WHERE werks = <fs_data>-werks
         AND  matnr = <fs_data>-matnr
         AND  loekz NE 'L'
         AND  prdat < <fs_data>-prdat
        .
      SORT gt_ekpo BY matnr ASCENDING werks ASCENDING prdat DESCENDING .

      DELETE ADJACENT DUPLICATES FROM gt_ekpo COMPARING matnr werks .
    ELSE.
      REFRESH:gt_ekpo .
      SELECT  * INTO TABLE gt_ekpo
        FROM ekpo
        WHERE werks = <fs_data>-werks
         AND  txz01 = <fs_data>-txz01
         AND  loekz NE 'L'
         AND  prdat < <fs_data>-prdat
        .
      SORT gt_ekpo BY txz01 ASCENDING werks ASCENDING prdat DESCENDING .

      DELETE ADJACENT DUPLICATES FROM gt_ekpo COMPARING txz01 werks .
    ENDIF.


    "供应商名称
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY
                    lifnr = <fs_data>-lifnr bukrs = <fs_data>-bukrs
                    BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-name1 = gs_lfa1-name1 .

    ENDIF.
    "申请数量
    READ TABLE gt_eban INTO gs_eban WITH KEY banfn = <fs_data>-banfn
                                              bnfpo = <fs_data>-bnfpo
                                              BINARY SEARCH .
    IF sy-subrc EQ 0.
      <fs_data>-sqsl = gs_eban-menge .

    ENDIF.

    "计划数量
    READ TABLE gt_zmd14 INTO gs_zmd14 WITH KEY banfn = <fs_data>-banfn
                                              bnfpo = <fs_data>-bnfpo
                                              BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-gsmng = gs_zmd14-gsmng .

    ENDIF.

    "合同单价含税
    READ TABLE gt_konv INTO gs_konv WITH KEY knumv = <fs_data>-knumv
                                             kposn+1(5) = <fs_data>-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-kbetr = gs_konv-kbetr .

    ENDIF.

    "合同净价
    <fs_data>-htjj = <fs_data>-netpr.


    "标准价格

    IF <fs_data>-matnr IS NOT INITIAL.



      READ TABLE gt_mbew INTO gs_mbew WITH KEY matnr = <fs_data>-matnr
                                               bwkey = <fs_data>-bukrs
                                   BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-bzjg = gs_mbew-stprs * <fs_data>-peinh / gs_mbew-peinh ..
        "     <fs_data>-bzjgdw = gs_mbew-peinh .
      ENDIF.

      "标准价格：EKPO上物料的标准价 - 子件物料的标准价
      IF <fs_data>-pstyp EQ '3'.
        READ TABLE gt_wx_hz INTO gs_wx_hz WITH KEY  ebeln = <fs_data>-ebeln
                                                    ebelp = <fs_data>-ebelp
                                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_data>-bzjg = <fs_data>-bzjg - gs_wx_hz-zjbzj .
        ENDIF.
      ENDIF.


      "最近采购净价
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY matnr = <fs_data>-matnr
                                               werks = <fs_data>-werks
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-zjcgjj = gs_ekpo-netpr / gs_ekpo-peinh * <fs_data>-peinh .
      ELSE.
        <fs_data>-zjcgjj = <fs_data>-netpr / <fs_data>-peinh * <fs_data>-peinh .
      ENDIF.

    ELSE.
      "最近采购净价
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY txz01 = <fs_data>-txz01
                                               werks = <fs_data>-werks
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        <fs_data>-zjcgjj = gs_ekpo-netpr / gs_ekpo-peinh * <fs_data>-peinh .
      ELSE.
        <fs_data>-zjcgjj = <fs_data>-netpr / <fs_data>-peinh * <fs_data>-peinh .
      ENDIF.


    ENDIF.


    IF <fs_data>-htjj NE 0 .
      <fs_data>-jjbl = ( <fs_data>-htjj - <fs_data>-zjcgjj )  /  <fs_data>-htjj  * 100 .
      <fs_data>-jjbl_% = <fs_data>-jjbl .
      CONCATENATE <fs_data>-jjbl_% '%' INTO <fs_data>-jjbl_%.

    ENDIF.

    "本行合同金额
    <fs_data>-bhhtje = <fs_data>-kbetr / <fs_data>-peinh * <fs_data>-menge .
    CONCATENATE <fs_data>-ebeln <fs_data>-ebelp INTO l_tdname.
    "项目名称
    PERFORM read_text USING 'F01' l_tdname 'EKPO' '0' CHANGING <fs_data>-xmmc."备注
    "PPV
    <fs_data>-ppv = <fs_data>-htjj  -  <fs_data>-bzjg .
    "采购超申购数量
    <fs_data>-cgcsg = <fs_data>-menge - <fs_data>-sqsl .
    "超计划数量
    <fs_data>-cjhsl = <fs_data>-sqsl  - <fs_data>-gsmng .
    IF <fs_data>-cjhsl < 0 .
      <fs_data>-cjhsl = 0 .
    ENDIF.
    "超计划金额
    <fs_data>-cjhje =  <fs_data>-htjj *  <fs_data>-cjhsl .

    "超计划率
    IF <fs_data>-gsmng NE 0 .
      <fs_data>-cjhl = ( <fs_data>-cjhsl /  <fs_data>-gsmng ) * 100 .

    ENDIF.

    <fs_data>-cjhl_1 = <fs_data>-cjhl .
    CONCATENATE <fs_data>-cjhl_1 '%' INTO <fs_data>-cjhl_1 .
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
  PERFORM frm_build_event.
  gw_grid_settings-edt_cll_cb = 'X'.
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
  gw_layout-zebra         = 'X'.
  gw_layout-cwidth_opt    = 'X'.
  gw_layout-box_fname     = 'ZSEL'.
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
*      -->P_0328   text
*      -->P_0329   text
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
*     I_CALLBACK_TOP_OF_PAGE   = ' '
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
"     it_events                = gt_events[]
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

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS\

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表
  DATA l_subrc TYPE sy-subrc.
  CLEAR l_subrc.

  "CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.


*打印
    WHEN '&PRNT'.

      CALL SCREEN 9000 STARTING AT 11 3 ENDING AT 50 15.

      PERFORM frm_print_data.





  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data .
  CASE 'X'.
    WHEN x_style1.
      PERFORM   frm_print_data_1 .
    WHEN x_style2.
      PERFORM   frm_print_data_2 .
    WHEN x_style3.
      PERFORM   frm_print_data_3 .
    WHEN x_style4.
      PERFORM   frm_print_data_4 .
  ENDCASE.
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
  init_fieldcat 'BSART'            '采购订单类型'        '' '' '' '' '' 'EKKO' 'BSART'.
  init_fieldcat 'EBELN'            '采购订单'        '' '' '' '' '' 'EKPO' 'EBELN'.
  init_fieldcat 'ZTERM'            '付款条件'        '' '' '' '' '' 'EKKO' 'ZTERM'.
  init_fieldcat 'EBELP'            '项目'        '' '' '' '' '' 'EKPO' 'EBELP'.
  init_fieldcat 'MATNR'            '物料编码'        '' '' '' '' '' 'EKPO' 'MATNR'.
  init_fieldcat 'TXZ01'            '物料描述'        '' '' '' '' '' 'EKPO' 'TXZ01'.
  init_fieldcat 'MENGE'            '数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'            '单位'        '' '' '' '' '' 'EKPO' 'MEINS'.
  init_fieldcat 'LIFNR'            '供应商'        '' '' '' '' '' 'EKKO' 'LIFNR'.
  init_fieldcat 'NAME1'            '供应商名称'        '' '' '' '' '' 'LFA1' 'NAME1'.
  init_fieldcat 'EKORG'             '采购组织'        '' '' '' '' '' 'EKKO' 'EKORG'.
  init_fieldcat 'EKGRP'             '采购组'        '' '' '' '' '' 'EKKO' 'EKGRP'.
  init_fieldcat 'KBETR'             '合同单价'       '' '' '' '' '' '' ''.
  init_fieldcat 'HTJJ'              '合同净价'        '' '' '' '' '' '' ''.
  init_fieldcat 'PEINH'             '合同价格单位'        '' '' '' '' '' '' ''.
  init_fieldcat 'BZJG'             '标准价格'        '' '' '' '' '' '' ''.
  " init_fieldcat 'BZJGDW'            '标准价格单位'        '' '' '' '' '' '' ''.
  init_fieldcat 'ZJCGJJ'            '最近采购净价'        '' '' '' '' '' '' ''.
  init_fieldcat 'JJBL_%'             '降价比例'        '' '' '' '' '' '' ''.
  init_fieldcat 'BHHTJE'             '本行合同金额'        '' '' '' '' '' '' ''.
  init_fieldcat 'XMMC'             '项目名称'        '' '' '' '' '' '' ''.
  init_fieldcat 'PPV'              'PPV'        '' '' '' '' '' '' ''.
  init_fieldcat 'GSMNG'             '计划数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'SQSL'              '申请数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'CGCSG'             '采购超申购数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'CJHSL'             '超计划数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'CJHJE'             '超计划金额'        '' '' '' '' '' '' ''.
  init_fieldcat 'CJHL_1'             '超计划率'        '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZPOPUP'.
  IF init IS INITIAL .
    init = 'X'.
    CREATE OBJECT container
      EXPORTING
        container_name = 'P1'.

    CREATE OBJECT editor
      EXPORTING
        parent                     = container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = 50
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.


  ENDIF.
  CALL METHOD editor->set_text_as_r3table
    EXPORTING
      table = w_hhzlyq.

*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  IF ok_code EQ 'CANCEL'.
    CLEAR: x_style1,x_style2,x_style3,x_style4.
  ENDIF.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data_1 .
  l_formname = 'ZSFMM033_1'.  "打印模板名称

  CLEAR:g_slhj,g_zjhj .

  MOVE-CORRESPONDING gt_data TO gt_data_01 .

  DELETE gt_data_01 WHERE zsel NE 'X'.

  IF  gt_data_01 IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.
  SORT gt_data_01 BY ebeln ebelp .

  CHECK gt_data_01 IS NOT INITIAL .

  REFRESH:gt_konv_z010 .
  SELECT knumv kposn stunr zaehk kschl  kbetr kwert waers
       INTO CORRESPONDING FIELDS OF TABLE gt_konv_z010
       FROM konv
       FOR ALL ENTRIES IN gt_data_01
       WHERE knumv = gt_data_01-knumv
       AND kschl EQ 'Z010'
 "     AND   kposn = gt_data_01-ebelp
       AND   kinak EQ space  ."激活状态

  SORT gt_konv_z010 BY knumv kposn .


  "获取交货计划
  REFRESH :gt_eket .
  SELECT * INTO TABLE gt_eket
    FROM eket
    FOR ALL ENTRIES IN gt_data_01
    WHERE ebeln = gt_data_01-ebeln
    AND   ebelp = gt_data_01-ebelp
    AND   etenr EQ '0001'.
  SORT gt_eket BY ebeln ebelp .



  REFRESH:gt_t001 .
  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs IN ('2110' ).  "s_bukrs .

  SORT gt_t001 BY bukrs .

  REFRESH:gt_t024.
  SELECT * INTO TABLE gt_t024
    FROM t024
    .
  SORT gt_t024 BY ekgrp .

*  REFRESH:gt_lfm1 .
*
*  SELECT * INTO TABLE gt_lfm1
*    FROM lfm1
*     .
*
*  SORT gt_lfm1 BY lifnr ekorg .
*
  REFRESH:gt_t052u .

  SELECT * INTO TABLE gt_t052u
    FROM t052u
    WHERE spras = sy-langu ..

  SORT  gt_t052u BY zterm .

  REFRESH:gt_header_1 , gt_item_1 .
  LOOP AT gt_data_01 INTO gs_data_01 .
    g_tabix  = sy-tabix .
    CLEAR:gs_item_1 .
    MOVE-CORRESPONDING gs_data_01 TO gs_item_1.
    AT NEW ebeln .
      CLEAR:gs_header_1 .
      gs_header_1-ebeln = gs_data_01-ebeln .
      READ TABLE gt_data_01 INTO gs_data_01 INDEX g_tabix.
      IF sy-subrc EQ 0 .
        "订单类型
        gs_header_1-bsart = gs_data_01-bsart .
        "订单日期
        gs_header_1-aedat = gs_data_01-aedat .
        "需方信息
        READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_data_01-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_1-xfdw = gs_t001-butxt .
        ENDIF.

        READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_data_01-ekgrp
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_1-xfdh = gs_t024-tel_number .
          gs_header_1-xfcz = gs_t024-telfx .
          gs_header_1-xfywy = gs_t024-eknam .
          gs_header_1-xfdz  = gs_t024-smtp_addr .
        ENDIF.
        gs_header_1-waers = gs_data_01-waers .  "货币码
        "供应商信息
        gs_header_1-lifnr =  gs_data_01-lifnr .
        gs_header_1-name1 = gs_data_01-name1 .
        READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data_01-lifnr
                                        bukrs = gs_data_01-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_1-stras = gs_lfa1-stras.   "地址
          gs_header_1-telf1 = gs_lfa1-telf1.   "电话
          gs_header_1-telfx = gs_lfa1-telfx.   "传真

        ENDIF.

        "付款条件

*    "    READ TABLE gt_lfm1 INTO gs_lfm1 WITH KEY lifnr = gs_data_01-lifnr
*                                                 ekorg = gs_data_01-ekorg
*                                                 BINARY SEARCH .
        "    IF sy-subrc EQ 0.
        "   gs_header_1-zterm  = gs_lfm1-zterm.
        gs_header_1-zterm = gs_data_01-zterm .
        READ TABLE gt_t052u INTO gs_t052u WITH KEY zterm = gs_header_1-zterm
                                                   BINARY SEARCH .
        IF sy-subrc EQ 0.
          gs_header_1-zterm_txt = gs_t052u-text1 .
        ENDIF.
        "   ENDIF.

      ENDIF.
      "运输方式
      CASE w_ysfs.
        WHEN '1'.
          gs_header_1-ysfs = '汽运' . "
        WHEN '2'.
          gs_header_1-ysfs = '空运' .
        WHEN '3'.
          gs_header_1-ysfs = '快递' .

      ENDCASE.
      "提货方式
      CASE w_thfs.

        WHEN '1'.
          gs_header_1-thfs = '送货上门' .
        WHEN '2'.
          gs_header_1-thfs = '自提' .

      ENDCASE.
      "质保期
      gs_header_1-zbq = w_zbq.

      "货物质量要求
      REFRESH w_hhzlyq .
      CALL METHOD editor->get_text_as_r3table
        IMPORTING
          table = w_hhzlyq.
      LOOP AT w_hhzlyq INTO hhzlyq_line .
        CONDENSE hhzlyq_line NO-GAPS.
        IF gs_header_1-hhzlyq IS INITIAL.
          gs_header_1-hhzlyq = hhzlyq_line .
        ELSE.
          CONCATENATE gs_header_1-hhzlyq  hhzlyq_line INTO gs_header_1-hhzlyq .
        ENDIF.
      ENDLOOP.
      APPEND gs_header_1 TO gt_header_1 .
    ENDAT .
    READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_data_01-knumv
                                             kposn+1(5) = gs_data_01-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_1-kbetr = gs_konv-kbetr.
      gs_item_1-kwert = gs_konv-kwert .
      gs_item_1-waers = gs_konv-waers .

    ENDIF.

    "税率
    READ TABLE gt_konv_z010 INTO gs_konv_z010 WITH KEY knumv = gs_data_01-knumv
                                            kposn+1(5) = gs_data_01-ebelp
                                            BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_1-kbetr_tax = gs_konv_z010-kbetr / 10 .

    ENDIF.

    "交货日期
    READ TABLE gt_eket INTO gs_eket WITH KEY ebeln = gs_data_01-ebeln
                                             ebelp = gs_data_01-ebelp
                                     BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_1-eindt = gs_eket-eindt.
    ENDIF.

*    "申请数量
*    READ TABLE gt_eban INTO gs_eban WITH KEY banfn = gs_data_01-banfn
*                                             bnfpo = gs_data_01-bnfpo
*                                             BINARY SEARCH .
*    IF sy-subrc EQ 0.
*      gs_item_1-sqsl = gs_eban-menge .
*
*    ENDIF.
*    "损耗数量
*
*    gs_item_1-shsl = gs_item_1-menge - gs_item_1-sqsl .

    APPEND gs_item_1 TO gt_item_1 .

  ENDLOOP.

  SORT gt_header_1 BY ebeln .

  SORT gt_item_1 BY ebeln ebelp .


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "SMARTFORMS的名字
    IMPORTING
      fm_name            = g_name                "对应的SMARTFORMS的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.





  LOOP AT gt_header_1 INTO gs_header_1 .

    CLEAR:hd_prs_1,g_slhj,g_zjhj.
    MOVE-CORRESPONDING gs_header_1 TO hd_prs_1.

    REFRESH:hd_prt_1,mx_prt_1.
    LOOP AT gt_item_1 INTO gs_item_1 WHERE ebeln = gs_header_1-ebeln .
      CLEAR:mx_prs_1 .
      MOVE-CORRESPONDING gs_item_1 TO mx_prs_1 .
      APPEND mx_prs_1 TO mx_prt_1 .
      g_slhj = g_slhj + gs_item_1-menge .  "数量合计
      g_zjhj = g_zjhj + gs_item_1-kwert.   "总价合计
    ENDLOOP.
*    PERFORM conv_amount USING  hd_prs_1-zjhj
*                  CHANGING  hd_prs_1-zjhjdx.
    APPEND hd_prs_1 TO hd_prt_1 .

    SORT mx_prt_1 BY ebeln ebelp .

    CALL FUNCTION g_name
      EXPORTING
        control_parameters = control
 "      NPAGE_LINE         = NPAGELINE
*       W_HEAD             = LW_PRT
        g_slhj             = g_slhj
        g_zjhj             = g_zjhj
      TABLES
        t_item             = mx_prt_1
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.


    ENDIF.

  ENDLOOP.



  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data_2 .

  l_formname = 'ZSFMM033_2'.  "打印模板名称

  MOVE-CORRESPONDING gt_data TO gt_data_02 .

  DELETE gt_data_02 WHERE zsel NE 'X'.

  IF  gt_data_02 IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.
  SORT gt_data_02 BY ebeln ebelp .

  CHECK gt_data_02 IS NOT INITIAL .





  SELECT ebeln ebelp zekkn  anln1
    INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
    FROM ekkn
    FOR ALL ENTRIES IN gt_data_02
    WHERE ebeln = gt_data_02-ebeln
    AND ebelp = gt_data_02-ebelp .

  SORT gt_ekkn BY ebeln ebelp .

  REFRESH:gt_t001 .
  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs IN  ('2110')."s_bukrs .

  SORT gt_t001 BY bukrs .

*  REFRESH:gt_t024.
*  SELECT * INTO TABLE gt_t024
*    FROM t024
*    .
*  SORT gt_t024 BY ekgrp .

  REFRESH:gt_header_2 , gt_item_2 .
  LOOP AT gt_data_02 INTO gs_data_02 .
    g_tabix  = sy-tabix .
    CLEAR:gs_item_2 .
    MOVE-CORRESPONDING gs_data_02 TO gs_item_2.
    AT NEW ebeln .
      CLEAR:gs_header_2 .
      gs_header_2-ebeln = gs_data_02-ebeln .



      READ TABLE gt_data_02 INTO gs_data_02 INDEX g_tabix.
      IF sy-subrc EQ 0 .
        gs_header_2-waers = gs_data_02-waers .  "货币码

        "需方信息
        READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_data_02-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_2-xfdw = gs_t001-butxt .
        ENDIF.

*        READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_data_02-ekgrp
*                                        BINARY SEARCH .
*        IF sy-subrc EQ 0 .
*          gs_header_2-xfdh = gs_t024-tel_number .
*          gs_header_2-xfcz = gs_t024-telfx .
*          gs_header_2-xfdz = gs_t024-smtp_addr .
*          "    gs_header_2-xfywy = gs_t024-eknam .
*        ENDIF.

        "供应商信息
        gs_header_2-lifnr =  gs_data_02-lifnr .
        gs_header_2-name1 = gs_data_02-name1 .
        READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data_02-lifnr
                                        bukrs = gs_data_02-bukrs
                      BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_2-stras = gs_lfa1-stras.   "地址
          gs_header_2-telf1 = gs_lfa1-telf1.   "电话
          gs_header_2-telfx = gs_lfa1-telfx.   "传真

        ENDIF.
      ENDIF.

      APPEND gs_header_2 TO gt_header_2 .
    ENDAT .
    READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_data_02-knumv
                                             kposn+1(5) = gs_data_02-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_2-kbetr = gs_konv-kbetr.
      gs_item_2-kwert = gs_konv-kwert .
      gs_item_2-waers = gs_konv-waers .

    ENDIF.

    READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data_02-ebeln
                                             ebelp = gs_data_02-ebelp
                                     BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_2-anln1 = gs_ekkn-anln1.
    ENDIF.

    APPEND gs_item_2 TO gt_item_2 .

  ENDLOOP.

  SORT gt_header_2 BY ebeln .

  SORT gt_item_2 BY ebeln ebelp .


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "SMARTFORMS的名字
    IMPORTING
      fm_name            = g_name                "对应的SMARTFORMS的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.





  LOOP AT gt_header_2 INTO gs_header_2 .
    CLEAR:hd_prs_2.
    MOVE-CORRESPONDING gs_header_2 TO hd_prs_2.

    REFRESH:hd_prt_2,mx_prt_2.
    LOOP AT gt_item_2 INTO gs_item_2 WHERE ebeln = gs_header_2-ebeln .
      CLEAR:mx_prs_2 .
      MOVE-CORRESPONDING gs_item_2 TO mx_prs_2 .
      APPEND mx_prs_2 TO mx_prt_2 .
      hd_prs_2-zjhj = hd_prs_2-zjhj + gs_item_2-kwert .
    ENDLOOP.
    PERFORM conv_amount USING  hd_prs_2-zjhj
                  CHANGING  hd_prs_2-zjhjdx.
    APPEND hd_prs_2 TO hd_prt_2 .

    SORT mx_prt_2 BY ebeln ebelp .

    CALL FUNCTION g_name
      EXPORTING
        control_parameters = control
 "      NPAGE_LINE         = NPAGELINE
*       W_HEAD             = LW_PRT
     "  G_HJ               = G_HJ
      TABLES
        t_item             = mx_prt_2
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.


    ENDIF.

  ENDLOOP.



  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data_3 .
  l_formname = 'ZSFMM033_3'.  "打印模板名称

  CLEAR:g_slhj,g_zjhj .

  MOVE-CORRESPONDING gt_data TO gt_data_03 .

  DELETE gt_data_03 WHERE zsel NE 'X'.

  IF  gt_data_03 IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.
  SORT gt_data_03 BY ebeln ebelp .

  CHECK gt_data_03 IS NOT INITIAL .

  REFRESH:gt_konv_z010.
  SELECT knumv kposn stunr zaehk kschl  kbetr kwert waers
       INTO CORRESPONDING FIELDS OF TABLE gt_konv_z010
       FROM konv
       FOR ALL ENTRIES IN gt_data_03
       WHERE knumv = gt_data_03-knumv
       AND kschl EQ 'Z010'
 "     AND   kposn = gt_data_01-ebelp
       AND   kinak EQ space  ."激活状态

  SORT gt_konv_z010 BY knumv kposn .

  SELECT ebeln ebelp zekkn  anln1
    INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
    FROM ekkn
    FOR ALL ENTRIES IN gt_data_03
    WHERE ebeln = gt_data_03-ebeln
    AND ebelp = gt_data_03-ebelp .

  SORT gt_ekkn BY ebeln ebelp .

  "获取交货计划
  REFRESH :gt_eket .
  SELECT * INTO TABLE gt_eket
    FROM eket
    FOR ALL ENTRIES IN gt_data_03
    WHERE ebeln = gt_data_03-ebeln
    AND   ebelp = gt_data_03-ebelp
    AND   etenr EQ '0001'.
  SORT gt_eket BY ebeln ebelp .

*  "查询采购申请数量
*  SELECT * INTO TABLE gt_eban
*    FROM eban
*    FOR ALL ENTRIES IN gt_data_03
*    WHERE banfn = gt_data_03-banfn
*    AND   bnfpo = gt_data_03-bnfpo.
*
*  SORT gt_eban BY banfn bnfpo .

  REFRESH:gt_t001 .
  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs IN  ('2110')."s_bukrs .

  SORT gt_t001 BY bukrs .

  REFRESH:gt_t024.
  SELECT * INTO TABLE gt_t024
    FROM t024
    .
  SORT gt_t024 BY ekgrp .

  REFRESH:gt_header_3 , gt_item_3 .
  LOOP AT gt_data_03 INTO gs_data_03 .
    g_tabix  = sy-tabix .
    CLEAR:gs_item_3 .
    MOVE-CORRESPONDING gs_data_03 TO gs_item_3.
    AT NEW ebeln .
      CLEAR:gs_header_3 .
      gs_header_3-ebeln = gs_data_03-ebeln .
      READ TABLE gt_data_03 INTO gs_data_03 INDEX g_tabix.
      IF sy-subrc EQ 0 .
        "订单类型
        "   gs_header_1-bsart = gs_data_01-bsart .
        "订单日期
        gs_header_3-aedat = gs_data_03-aedat .
        "需方信息
        READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_data_03-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_3-xfdw = gs_t001-butxt .
        ENDIF.

        READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_data_03-ekgrp
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_3-xfdh = gs_t024-tel_number .
          gs_header_3-xfcz = gs_t024-telfx .
          gs_header_3-xfywy = gs_t024-eknam .
        ENDIF.
        gs_header_3-waers = gs_data_03-waers .  "货币码
        "供应商信息
        gs_header_3-lifnr =  gs_data_03-lifnr .
        gs_header_3-name1 = gs_data_03-name1 .
        READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data_03-lifnr
                                        bukrs = gs_data_03-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          "   gs_header_1-stras = gs_lfa1-stras.   "地址
          gs_header_3-telf1 = gs_lfa1-telf1.   "电话
          "   gs_header_1-telfx = gs_lfa1-telfx.   "传真

        ENDIF.
      ENDIF.

      APPEND gs_header_3 TO gt_header_3 .
    ENDAT .
    READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_data_03-knumv
                                             kposn+1(5) = gs_data_03-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_3-kbetr = gs_konv-kbetr.
      gs_item_3-kwert = gs_konv-kwert .
      gs_item_3-waers = gs_konv-waers .

    ENDIF.

    "税率
    READ TABLE gt_konv_z010 INTO gs_konv_z010 WITH KEY knumv = gs_data_03-knumv
                                            kposn+1(5) = gs_data_03-ebelp
                                            BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_3-kbetr_tax = gs_konv_z010-kbetr / 10 .

    ENDIF.

    "交货日期
    READ TABLE gt_eket INTO gs_eket WITH KEY ebeln = gs_data_03-ebeln
                                             ebelp = gs_data_03-ebelp
                                     BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_3-eindt = gs_eket-eindt.
    ENDIF.



    APPEND gs_item_3 TO gt_item_3 .

  ENDLOOP.

  SORT gt_header_3 BY ebeln .

  SORT gt_item_3 BY ebeln ebelp .


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "SMARTFORMS的名字
    IMPORTING
      fm_name            = g_name                "对应的SMARTFORMS的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.





  LOOP AT gt_header_3 INTO gs_header_3 .

    CLEAR:hd_prs_3,g_slhj,g_zjhj.
    MOVE-CORRESPONDING gs_header_3 TO hd_prs_3.

    REFRESH:hd_prt_3,mx_prt_3.
    LOOP AT gt_item_3 INTO gs_item_3 WHERE ebeln = gs_header_3-ebeln .
      CLEAR:mx_prs_3 .
      MOVE-CORRESPONDING gs_item_3 TO mx_prs_3 .
      APPEND mx_prs_3 TO mx_prt_3 .
      g_slhj = g_slhj + gs_item_3-menge .  "数量合计
      g_zjhj = g_zjhj + gs_item_3-kwert.   "总价合计
    ENDLOOP.
*    PERFORM conv_amount USING  hd_prs_1-zjhj
*                  CHANGING  hd_prs_1-zjhjdx.
    APPEND hd_prs_3 TO hd_prt_3 .

    SORT mx_prt_3 BY ebeln ebelp .

    CALL FUNCTION g_name
      EXPORTING
        control_parameters = control
 "      NPAGE_LINE         = NPAGELINE
*       W_HEAD             = LW_PRT
        g_slhj             = g_slhj
        g_zjhj             = g_zjhj
      TABLES
        t_item             = mx_prt_3
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.


    ENDIF.

  ENDLOOP.



  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data_4 .

  REFRESH:gt_data_04,
          gt_ekkn,
          gt_header_4 ,
          gt_item_4,
          hd_prt_4,
          mx_prt_4 .
  l_formname = 'ZSFMM033_4'.  "打印模板名称

  MOVE-CORRESPONDING gt_data TO gt_data_04 .

  DELETE gt_data_04 WHERE zsel NE 'X'.

  IF  gt_data_04 IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.
  SORT gt_data_04 BY ebeln ebelp .

  CHECK gt_data_04 IS NOT INITIAL .

*  SELECT knumv kposn stunr zaehk   kbetr kwert waers
*       INTO CORRESPONDING FIELDS OF TABLE gt_konv
*       FROM konv
*       FOR ALL ENTRIES IN gt_data_04
*       WHERE knumv = gt_data_04-knumv
*       AND kschl EQ 'PBXX'
* "     AND   kposn = gt_data_01-ebelp
*       AND   kinak EQ space  ."激活状态
*
*  SORT gt_konv BY knumv kposn .

  SELECT ebeln ebelp zekkn  anln1
    INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
    FROM ekkn
    FOR ALL ENTRIES IN gt_data_04
    WHERE ebeln = gt_data_04-ebeln
    AND ebelp = gt_data_04-ebelp .

  SORT gt_ekkn BY ebeln ebelp .

  REFRESH:gt_t001 .
  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs IN ('2110'). "s_bukrs .

  SORT gt_t001 BY bukrs .

*  REFRESH:gt_t024.
*  SELECT * INTO TABLE gt_t024
*    FROM t024
*    .
*  SORT gt_t024 BY ekgrp .

  LOOP AT gt_data_04 INTO gs_data_04 .
    g_tabix  = sy-tabix .
    CLEAR:gs_item_4 .
    MOVE-CORRESPONDING gs_data_04 TO gs_item_4.
    AT NEW ebeln .
      CLEAR:gs_header_4 .
      gs_header_4-ebeln = gs_data_04-ebeln .
      READ TABLE gt_data_04 INTO gs_data_04 INDEX g_tabix.
      IF sy-subrc EQ 0 .
        gs_header_4-waers = gs_data_04-waers .  "货币码


        "需方信息
        READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_data_04-bukrs
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_4-xfdw = gs_t001-butxt .
        ENDIF.

*        READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_data_04-ekgrp
*                                        BINARY SEARCH .
*        IF sy-subrc EQ 0 .
*          gs_header_4-xfdh = gs_t024-tel_number .
*          gs_header_4-xfcz = gs_t024-telfx .
*          gs_header_4-xfdz = gs_t024-smtp_addr .
*          "    gs_header_2-xfywy = gs_t024-eknam .
*        ENDIF.

        "供应商信息
        gs_header_4-lifnr =  gs_data_04-lifnr .
        gs_header_4-name1 = gs_data_04-name1 .
        READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_data_04-lifnr
                                        bukrs = gs_data_04-bukrs
                      BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_header_4-stras = gs_lfa1-stras.   "地址
          gs_header_4-telf1 = gs_lfa1-telf1.   "电话
          gs_header_4-telfx = gs_lfa1-telfx.   "传真

        ENDIF.
      ENDIF.

      APPEND gs_header_4 TO gt_header_4 .
    ENDAT .
    READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_data_04-knumv
                                             kposn+1(5) = gs_data_04-ebelp
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_4-kbetr = gs_konv-kbetr.
      gs_item_4-kwert = gs_konv-kwert .
      gs_item_4-waers = gs_konv-waers .

    ENDIF.

    READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data_04-ebeln
                                             ebelp = gs_data_04-ebelp
                                     BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_item_4-anln1 = gs_ekkn-anln1.
    ENDIF.

    APPEND gs_item_4 TO gt_item_4 .

  ENDLOOP.

  SORT gt_header_4 BY ebeln .

  SORT gt_item_4 BY ebeln ebelp .


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "SMARTFORMS的名字
    IMPORTING
      fm_name            = g_name                "对应的SMARTFORMS的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.





  LOOP AT gt_header_4 INTO gs_header_4 .
    CLEAR:hd_prs_4.
    MOVE-CORRESPONDING gs_header_4 TO hd_prs_4.

    REFRESH:hd_prt_4,mx_prt_4.
    LOOP AT gt_item_4 INTO gs_item_4 WHERE ebeln = gs_header_4-ebeln .
      CLEAR:mx_prs_4 .
      MOVE-CORRESPONDING gs_item_4 TO mx_prs_4 .
      APPEND mx_prs_4 TO mx_prt_4 .
      hd_prs_4-zjhj = hd_prs_4-zjhj + gs_item_4-kwert .
    ENDLOOP.
*    PERFORM conv_amount USING  hd_prs_4-zjhj
*                  CHANGING  hd_prs_4-zjhjdx.
    APPEND hd_prs_4 TO hd_prt_4 .

    SORT mx_prt_4 BY ebeln ebelp .

    CALL FUNCTION g_name
      EXPORTING
        control_parameters = control
 "      NPAGE_LINE         = NPAGELINE
*       W_HEAD             = LW_PRT
     "  G_HJ               = G_HJ
      TABLES
        t_item             = mx_prt_4
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.


    ENDIF.

  ENDLOOP.



  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.
ENDFORM.

FORM conv_amount USING VALUE(f_source)
                 CHANGING VALUE(f_result).
  DATA: scr(30) TYPE c, res(60) TYPE c,fen(2) TYPE c .
  DATA: len TYPE i, c1 TYPE i, c2 TYPE i, c3 TYPE i, c4 TYPE i.
  DATA: d1(1) TYPE c, d2(1) TYPE c, d3 TYPE i.
  DATA: digit(2)  TYPE c, weight(2) TYPE c.
  DATA: rule1(20) TYPE c VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: rule2(30) TYPE c VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  scr = f_source * 100.
  CONDENSE scr NO-GAPS.
  IF scr = '0'.
    res = '零元'.
  ELSE.
    len = strlen( scr ).
    c1 = 0.
    d1 = '0'.
    CLEAR res.
    DO len TIMES.
      c1 = c1 + 1.
      c2 = len - c1.
      d2 = scr+c2(1) .
      IF d2 = '0'.
        d3 = 0.
      ELSE.
        d3 = d2.
      ENDIF.
      digit = rule1+d3(1) .
      c3 = ( c1 - 1 ) .
      weight = rule2+c3(1) .
      IF d2 = '0'.
        IF c1 = 3.
          digit = ''.
        ELSEIF c1 = 7.
          digit = ''.
          IF len > 10 .
            c4 = len - 10.
            IF scr+c4(4) = '0000'.
              weight = ''.
            ENDIF.
          ENDIF.
        ELSEIF c1 = 11.
          digit = ''.
        ELSEIF d1 = '0'.
          digit = ''.
          weight = ''.
        ELSE.
          weight = ''.
        ENDIF.
      ENDIF.
      CONCATENATE digit weight res INTO res .
      d1 = d2.
    ENDDO.
  ENDIF.
  len = strlen( res ) - 1.
  fen = res+len(1).
  IF fen <> '分' .
    CONCATENATE res '整' INTO f_result.
  ELSE.
    f_result = res.
  ENDIF.
ENDFORM.                    "conv_amount
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1705   text
*      -->P_L_TDNAME  text
*      -->P_1707   text
*      -->P_1708   text
*      <--P_<FS_DATA>_XMMC  text
*----------------------------------------------------------------------*
FORM read_text  USING    VALUE(id)
                         VALUE(name)
                         VALUE(object)
                         VALUE(ind)"第IND行，若IND=0，则取全部
                CHANGING tdline.
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  DATA tdname TYPE thead-tdname.
  tdname = name.
  CLEAR: tdline, t_tline[].
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = id
      language                = sy-langu
      name                    = tdname
      object                  = object
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
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
*Implement suitable error handling here
  ENDIF.
  IF t_tline[] IS NOT INITIAL.

    LOOP AT t_tline.
      IF tdline IS INITIAL.
        tdline = t_tline-tdline.
      ELSE.
        CONCATENATE tdline t_tline-tdline INTO tdline SEPARATED BY space.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  LISTBOX_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE listbox_9000 OUTPUT.
  TYPE-POOLS:vrm.
  DATA: vid    TYPE vrm_id VALUE 'W_YSFS',
        vlist  TYPE vrm_values,
        values LIKE LINE OF vlist.
  CLEAR vlist.
  CLEAR values.
  MOVE '1' TO values-key.
  MOVE '汽运' TO values-text.
  APPEND values TO vlist.
  MOVE '2' TO values-key.
  MOVE '空运' TO values-text.
  APPEND values TO vlist.
  MOVE '3' TO values-key.
  MOVE '快递' TO values-text.
  APPEND values TO vlist.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = vid
      values          = vlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  vid = 'W_THFS'.
  CLEAR vlist.
  CLEAR values.
  MOVE '1' TO values-key.
  MOVE '送货上门' TO values-text.
  APPEND values TO vlist.
  MOVE '2' TO values-key.
  MOVE '自提' TO values-text.
  APPEND values TO vlist.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = vid
      values          = vlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE '下拉框出错，请联系管理员！' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

ENDMODULE.
