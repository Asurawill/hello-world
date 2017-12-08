*&---------------------------------------------------------------------*
*& 程序名称:ZSD012
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :订单交货明细表
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号        描述
:" 20170216 IT02     ED1K905251   增加“客户物料编码”
*&---------------------------------------------------------------------*

report zsd012.


************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
tables: vbak, vbap, vbkd, vbpa,lips,likp.

************************************************************************
* Type Declaration
************************************************************************
types:begin of ty_output.
        include structure zclv_zsd012.
types:wjhsl      type vbap-kwmeng,
      pjnam(200),
      ptmat      type vbap-matnr,
      ptktx      type vbap-arktx,
      ltext(200),
      vbeln_fp   type vbrp-vbeln,
      bezei type tvagt-bezei,"拒绝原因描述
      kbetr type konv-kbetr,"单价
      jetj type FAGLFLEXT-TSL01,"金额
      vdatu type c length 8, "请求交货日期
      kdmat type vbap-kdmat, "客户物料
      end of ty_output.

types:begin of ty_vbeln,
      vbeln type  vbap-vbeln,
      posnr type vbap-posnr,
     end of ty_vbeln .

************************************************************************
* Internal Table
************************************************************************
data: it_zclv_zsd012 type table of zclv_zsd012,
      wa_zclv_zsd012 type zclv_zsd012,
      wa_zclv        type zclv_zsd012.

data:it_compar like table of zclv_zsd012 with header line.
data:it_konv like table of konv with header line.
data:it_vbkd like table of vbkd with header line.

data: it_vbpa    type table of vbpa,
      wa_vbpa    type vbpa,
      it_vbrp    type table of vbrp,
      wa_vbrp    type vbrp,
      it_vbrk    type table of vbrk,
      wa_vbrk    type vbrk,
      it_vbrk_cx type table of vbrk,
      wa_vbrk_cx type vbrk,
      it_vbak    type table of vbak,
      wa_vbak    type vbak ,
      it_vbap    type table of vbap,
      wa_vbap    type vbap ,
      it_vbeln   type table of ty_vbeln,
      wa_vbeln   type ty_vbeln .

data: it_output type table of ty_output,
      wa_output type ty_output.

field-symbols: <fs_zclv_zsd012> type zclv_zsd012.

data: it_tvko type table of tvko,
      wa_tvko type tvko.

data: it_tvagt like table of tvagt with header line .
************************************************************************
* WorkArea
************************************************************************


************************************************************************
*      DEFINITION
************************************************************************
define init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  if gw_lvc-fieldname = 'VBELN' ."or gw_lvc-fieldname = 'VBELN_LP'.
  gw_lvc-HOTSPOT = &1.
  endif.
  gw_lvc-coltext = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext = &2.
  gw_lvc-outputlen = &3.
*  if &4 = 'x'.
*    gw_lvc-no_zero = 'x'.
*  endif.
*  gw_lvc-icon = &4.
*  gw_lvc-checkbox = &5.
*  gw_lvc-edit = &6.
*  gw_lvc-fix_column =  &5.
  gw_lvc-CFIELDNAME =  &4.
  gw_lvc-QFIELDNAME =  &5.
  gw_lvc-ref_table = &6.
  gw_lvc-ref_field = &7.
  gw_lvc-datatype = &8.
*  gw_lvc-intlen = &9.
  append gw_lvc to gt_lvc.
  clear gw_lvc.
end-of-definition.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
type-pools: slis.

data: gt_lvc           type lvc_t_fcat,
      gt_sort          type lvc_t_sort,
      gw_layout        type lvc_s_layo,   "alv的格式
      gw_variant       type disvariant,
      gw_grid_settings type lvc_s_glay,
      gw_lvc           type lvc_s_fcat,
      gw_sort          type lvc_s_sort,
      gw_grid_setting  type lvc_s_glay,
      g_repid          like sy-repid,  "SY-REPID 指 当前的主程序
      gt_events        type slis_t_event with header line, "保存AVL事件
      gw_events        like line of gt_events.
data: gt_exclude type slis_t_extab,
      gs_exclude type slis_extab.

data: gr_alvgrid type ref to cl_gui_alv_grid.

data: gt_rows type lvc_t_row,
      gt_roid type lvc_t_roid,
      wa_rows type lvc_s_row,
      wa_roid type lvc_s_roid.
data: gs_variant type disvariant.

data: gw_istable type lvc_s_stbl.

************************************************************************
* Global Variant
************************************************************************
*field-symbols: <dyn_table> type standard table,        " 内表结构
*               <dyn_wa>,                               " 表头
*               <dyn_field>.
*
*data: dy_table type ref to data,
*      dy_line  type ref to data.

data: g_objname type thead-tdname.

data: it_lines type table of tline,
      wa_lines type tline.

************************************************************************
* Constant
************************************************************************


************************************************************************
* Selection Screen
************************************************************************
selection-screen begin of block blk1 with frame title text-001.
select-options: s_vbeln for vbak-vbeln matchcode object vmva,
                s_auart for vbak-auart,
                s_kunnr for vbak-kunnr,
                s_erdat for vbak-erdat,
                s_matnr for vbap-matnr,
                s_bstkd for vbkd-bstkd,
                s_ernam for vbak-ernam,
                s_jhpz  for lips-vbeln,"交货凭证 add IT02 150730
                s_budat for likp-wadat_ist."过账日期 add it02 150730
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-002.
select-options: s_vkorg for vbak-vkorg obligatory memory id vko,
                s_vkbur for vbak-vkbur.
selection-screen end of block blk2.

selection-screen begin of block blk3 with frame title text-003.
select-options: s_kunnr1 for vbpa-kunnr,
                s_kunnr2 for vbpa-kunnr.
selection-screen end of block blk3.

selection-screen begin of block blk4 with frame title text-004.
parameter: p_allor type c radiobutton group g1 default 'X',
           p_uncle type c radiobutton group g1.
selection-screen end of block blk4.


************************************************************************
* Initialization
************************************************************************
initialization.


************************************************************************
* At selection screen
************************************************************************
at selection-screen output.


at selection-screen.



************************************************************************
* Event Start of Selection
************************************************************************
start-of-selection.
  perform frm_auth_check.
  perform frm_get_data.
  perform frm_display.


************************************************************************
* Event End-of selection
************************************************************************
end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  select *
    into table it_zclv_zsd012
    from zclv_zsd012
    where vbeln in s_vbeln
      and auart in s_auart
      and kunnr in s_kunnr
      and erdat in s_erdat
      and ernam in s_ernam
      and matnr in s_matnr
      and bstkd in s_bstkd
      and vkorg in s_vkorg
      and vkbur in s_vkbur
      and vbeln_lp in s_jhpz
      and wadat_ist in s_budat
*      and pstyv not in ('Z01', 'Z02', 'Z21', 'Z22', 'Z31', 'Z32', 'Z41', 'Z42')
    .
  select * into corresponding fields of table  it_tvagt
    from  tvagt where spras = sy-langu.  .
  if it_zclv_zsd012 is initial.
    message '无相关数据！' type 'I' display like 'E'.
    stop.
  endif.

 "add 销售订单抬头信息 IT02 20160329
  SELECT *
    INTO TABLE it_vbak
    FROM VBAK
    WHERE vbeln in s_vbeln
      and auart in s_auart
      and kunnr in s_kunnr
      and erdat in s_erdat
      and ernam in s_ernam
      and vkorg in s_vkorg
      and vkbur in s_vkbur
      .
   SORT it_vbak by vbeln.

   "add 销售凭证明细信息 IT02 20170217
   if it_zclv_zsd012 is not initial.
      REFRESH:it_vbap.
      move-corresponding it_zclv_zsd012 to it_vbap.
      sort it_vbap by vbeln posnr .

      SELECT *
      INTO TABLE it_vbap
      from vbap
      for all entries in it_vbap
      where vbeln = it_vbap-vbeln
       and  posnr = it_vbap-posnr.
    sort  it_vbap  by vbeln posnr .
   endif.


  " add konv 单价  it02 150730  begin
 select vbeln posnr bstkd
   from vbkd
   into corresponding fields of table it_vbkd
   for all entries in it_zclv_zsd012
   where vbeln = it_zclv_zsd012-vbeln
  .
 select knumv kposn kbetr
   from konv
   into corresponding fields of table it_konv
   for all entries in it_zclv_zsd012
   where knumv = it_zclv_zsd012-knumv
   and kposn = it_zclv_zsd012-posnr
   and kschl = 'ZR01'.




  "end 150730

  if s_kunnr1 is not initial or s_kunnr2 is not initial.
    select *
      into table it_vbpa
      from vbpa
      for all entries in it_zclv_zsd012
      where vbeln = it_zclv_zsd012-vbeln.

    loop at it_zclv_zsd012 into wa_zclv_zsd012.
      " 判断 项目经理
      loop at it_vbpa into wa_vbpa where vbeln = wa_zclv_zsd012-vbeln
                                     and ( parvw = 'Z1' or parvw = 'Z2' )
                                     and kunnr in s_kunnr1.
        exit.
      endloop.
      if sy-subrc ne 0.
        delete it_zclv_zsd012.
        continue.
      endif.

      " 判断 经办人
      loop at it_vbpa into wa_vbpa where vbeln = wa_zclv_zsd012-vbeln
                                     and ( parvw = 'Z3' or parvw = 'Z4' or parvw = 'Z5' )
                                     and kunnr in s_kunnr2.
        exit.
      endloop.
      if sy-subrc ne 0.
        delete it_zclv_zsd012.
      endif.
    endloop.
  endif.

  select *
    into table it_vbrp
    from vbrp
    for all entries in it_zclv_zsd012
    where vgbel = it_zclv_zsd012-vbeln_lp.

  select *
    appending table it_vbrp
    from vbrp
    for all entries in it_zclv_zsd012
    where vgbel = it_zclv_zsd012-vbeln.

  if it_vbrp is not initial.
    " 排除冲销发票
    select *
      into table it_vbrk
      from vbrk
      for all entries in it_vbrp
      where vbeln = it_vbrp-vbeln.

    select *
      into table it_vbrk_cx
      from vbrk
      for all entries in it_vbrk
      where sfakn = it_vbrk-vbeln.
    sort it_vbrk_cx by sfakn.

    loop at it_vbrk into wa_vbrk.
      if wa_vbrk-sfakn is not initial.
        loop at it_vbrp into wa_vbrp where vbeln = wa_vbrk-vbeln.
          delete it_vbrp.
        endloop.
      else.
        read table it_vbrk_cx into wa_vbrk_cx with key sfakn = wa_vbrk-vbeln binary search.
        if sy-subrc = 0.
          loop at it_vbrp into wa_vbrp where vbeln = wa_vbrk-vbeln.
            delete it_vbrp.
          endloop.
        endif.
      endif.
    endloop.
  endif.

  sort it_vbrp by vgbel vgpos.

  sort it_zclv_zsd012 by vbeln posnr.

  data: l_count type i.

  data: lt_zclv like it_zclv_zsd012.

  lt_zclv = it_zclv_zsd012.
  sort lt_zclv by vbeln posnr.
  delete adjacent duplicates from lt_zclv comparing vbeln posnr.

  loop at lt_zclv into wa_zclv.
    l_count = 0.

    loop at it_zclv_zsd012 into wa_zclv_zsd012 where vbeln = wa_zclv-vbeln
                                                 and posnr = wa_zclv-posnr.
*      if wa_zclv_zsd012-lfimg = 0.
*        continue.
*      endif.
      wa_zclv-kwmeng = wa_zclv-kwmeng - wa_zclv_zsd012-lfimg.
      l_count = l_count + 1.
    endloop.

    if l_count > 1.
      clear: wa_zclv-vbeln_lp,
             wa_zclv-posnr_lp,
             wa_zclv-lfimg,
*             wa_zclv-pstyv,
             wa_zclv-meins.
      modify lt_zclv from wa_zclv.
    else.
      delete lt_zclv.
    endif.
  endloop.

  sort lt_zclv by vbeln posnr.

  append lines of lt_zclv to it_zclv_zsd012.

  sort it_zclv_zsd012 by vbeln posnr.

  loop at it_zclv_zsd012 into wa_zclv_zsd012.
    clear wa_output.

     read table it_vbkd with key vbeln = wa_zclv_zsd012-vbeln posnr = wa_zclv_zsd012-posnr.
     if sy-subrc = 0 .
       wa_zclv_zsd012-bstkd = it_vbkd-bstkd.  "先根据销售订单号、行号读取合同编号150731
       else.
          read table it_vbkd with key vbeln = wa_zclv_zsd012-vbeln  posnr = '000000'. "根据销售订单号、行号读取失败 ，再改为读取行号为000000的合同编号150731
          if sy-subrc = 0 .
             wa_zclv_zsd012-bstkd = it_vbkd-bstkd.
          endif.
     endif.
    move-corresponding wa_zclv_zsd012 to wa_output.
     "请求交货日期
     read table it_vbak into wa_vbak with key vbeln = wa_output-vbeln binary search.
      if sy-subrc = 0.
        wa_output-vdatu = wa_vbak-vdatu.        "add it02 20160329
      endif.
     "客户物料
    read table it_vbap into wa_vbap with key vbeln = wa_output-vbeln
                                             posnr = wa_output-posnr
                                             binary search .
       if sy-subrc eq 0 .
           wa_output-kdmat = wa_vbap-kdmat .
       endif.
    read table it_konv with key knumv = wa_zclv_zsd012-knumv kposn = wa_zclv_zsd012-posnr.
    if sy-subrc = 0.
      wa_output-kbetr = it_konv-kbetr ."读取单价 it02 150731
     endif.

    if wa_output-pstyv = 'Z01'
      or wa_output-pstyv = 'Z02'
      or wa_output-pstyv = 'Z21'
      or wa_output-pstyv = 'Z22'
      or wa_output-pstyv = 'Z31'
      or wa_output-pstyv = 'Z32'
      or wa_output-pstyv = 'Z41'
      or wa_output-pstyv = 'Z42'.
      continue.
    endif.

    " 未交货数量 = 销售订单行数量 - 交货订单数量
*    wa_output-wjhsl = wa_output-kwmeng.
*
*    l_count = 0.
*    loop at it_zclv_zsd012 into wa_zclv where vbeln = wa_zclv_zsd012-vbeln
*                                          and posnr = wa_zclv_zsd012-posnr.
*      wa_output-wjhsl = wa_output-wjhsl - wa_zclv-lfimg.
*    endloop.
    if wa_zclv_zsd012-vbeln_lp is not initial.
      read table lt_zclv into wa_zclv with key vbeln = wa_zclv_zsd012-vbeln
                                               posnr = wa_zclv_zsd012-posnr
                                               binary search.
      if sy-subrc = 0.
        if wa_output-lfimg = 0.
          continue.
        endif.
        wa_output-kwmeng = wa_output-lfimg.
        wa_output-wjhsl = 0.
      else.
        wa_output-wjhsl = wa_output-kwmeng - wa_output-lfimg.
      endif.
    else.
      if wa_output-kwmeng = 0.
        continue.
      endif.
      wa_output-wjhsl = wa_output-kwmeng.
    endif.

    if p_uncle = 'X'. " 只显示未清
      if wa_output-wjhsl = 0.
        continue.
      endif.
    endif.

    if wa_output-auart = 'ZPO' or wa_output-auart = 'ZWV' or wa_output-auart = 'ZF1' or wa_output-auart = 'ZZG'.
      if wa_output-uepos = ''.
        continue.
      else.
        " 取屏体物料及描述
        read table it_zclv_zsd012 into wa_zclv with key vbeln = wa_output-vbeln
                                                        posnr = wa_output-uepos
                                                        binary search.
        if sy-subrc = 0.
          wa_output-ptmat = wa_zclv-matnr.
          wa_output-ptktx = wa_zclv-arktx.
        endif.
      endif.

      read table it_vbrp into wa_vbrp with key vgbel = wa_output-vbeln
                                               vgpos = wa_output-uepos
                                               binary search.
      if sy-subrc = 0.
        wa_output-vbeln_fp = wa_vbrp-vbeln.
      endif.
    else.
      read table it_vbrp into wa_vbrp with key vgbel = wa_output-vbeln_lp
                                               vgpos = wa_output-posnr_lp
                                               binary search.
      if sy-subrc = 0.
        wa_output-vbeln_fp = wa_vbrp-vbeln.
      endif.
    endif.

    if wa_output-bstdk = '00000000'.
      wa_output-bstdk = ''.
    endif.

    if wa_output-wadat_ist = '00000000'.
      wa_output-wadat_ist = ''.
    endif.

    " 取项目名称 - 销售订单抬头文本
    g_objname = wa_output-vbeln.
    refresh it_lines.
    call function 'READ_TEXT'
      exporting
        id                      = 'Z001'
        language                = sy-langu
        name                    = g_objname
        object                  = 'VBBK'
      tables
        lines                   = it_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.
    if sy-subrc <> 0.
* Implement suitable error handling here
*      message '项目名称读取错误!' type 'W'.
    endif.

    read table it_lines into wa_lines index 1.
    if sy-subrc = 0.
      wa_output-pjnam = wa_lines-tdline.
    endif.

    " 取备注 - 行项目文本
    g_objname(10) = wa_output-vbeln.
    g_objname+10 = wa_output-posnr.
    refresh it_lines.
    call function 'READ_TEXT'
      exporting
        id                      = '0006'
        language                = sy-langu
        name                    = g_objname
        object                  = 'VBBP'
      tables
        lines                   = it_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.
    if sy-subrc <> 0.
* Implement suitable error handling here
*      message '项目名称读取错误!' type 'W'.
    endif.

    loop at it_lines into wa_lines.
      concatenate wa_output-ltext wa_lines-tdline into wa_output-ltext.
    endloop.
  "IT02 add 增加拒绝原因描述 150710 begin
     read table it_tvagt with key abgru = wa_output-abgru .
    if sy-subrc = 0.
      wa_output-bezei = it_tvagt-bezei."拒绝原因描述
    endif.
    "IT02 add 增加拒绝原因描述 150710 end
  "金额添加 it02 150731 begin
    wa_output-JETJ = wa_output-KBETR * wa_output-KWMENG .
   "金额添加 it02 150731 end
    append wa_output to it_output.
  endloop.

  sort it_output by vbeln posnr vbeln_lp posnr_lp.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_display .
  perform init_layout.              "设置输出格式
  perform init_sort.                "设置排序、合计
  perform init_variant.             "设置变式控制
  perform frm_init_lvc.             " 初始化内表结构/ALV显示结构
  perform frm_exclude.
  perform frm_build_event.
  perform frm_output tables gt_lvc              "输出
                            gt_sort
                            it_output
                     using ''
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
endform.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       初始化layout参数
*----------------------------------------------------------------------*
form init_layout .
  gw_layout-zebra = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
*  gw_layout-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
endform.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       排序
*----------------------------------------------------------------------*
form init_sort .
*    clear gw_sort.
*    gw_sort-fieldname = 'WERKS'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'FEVOR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'DISPO'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
**    clear gw_sort.
**    gw_sort-fieldname = 'KUNNR'.  "排序字段
**    gw_sort-spos = 1.
**    gw_sort-up = 'X'.             "升序
***  it_sort-subtot = 'X'.         "小计依据
**    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'MATNR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
endform.                    " INIT_SORT
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       初始化变量
*----------------------------------------------------------------------*
form init_variant.
  clear: gw_variant.
  gw_variant-report = sy-repid.
  gw_variant-handle = '0001'.

  clear gw_grid_settings.
  gw_grid_settings-edt_cll_cb = 'X'.

endform.                    " INIT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_exclude .
  refresh gt_exclude.
  clear gs_exclude.
endform.                    " FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
form frm_init_lvc.

*  init_fieldcat 'BOX' text-001 '2' '' 'X' 'X' 'X' '' ''.
*  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
*  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
  init_fieldcat 'VBELN' '' '' '' '' 'VBAP' 'VBELN' '' .
  init_fieldcat 'PJNAM' text-f01 '' '' '' '' '' '' .
  init_fieldcat 'AUART' '' '' '' '' 'VBAK' 'AUART' '' .
  init_fieldcat 'AUTXT' text-f02 '' '' '' 'TVAKT' 'BEZEI' '' .
  init_fieldcat 'BSTKD' text-f03 '' '' '' 'VBKD' 'BSTKD' '' .
  init_fieldcat 'BSTDK' text-f04 '' '' '' 'VBKD' 'BSTDK' '' .
  init_fieldcat 'ERDAT' '' '' '' '' 'VBAK' 'ERDAT' '' .
  init_fieldcat 'VDATU' '请求交货日期' '' '' '' 'VBAK' 'VDATU' '' .
  init_fieldcat 'VKBUR' '' '' '' '' 'VBAK' 'VKBUR' '' .
  init_fieldcat 'VBTXT' text-f05 '' '' '' 'TVKBT' 'BEZEI' '' .
  init_fieldcat 'KUNNR' '' '' '' '' 'VBAK' 'KUNNR' '' .
  init_fieldcat 'NAME1' text-f06 '' '' '' 'KNA1' 'KUNNR' '' .
  init_fieldcat 'VKORG' '' '' '' '' 'VBAK' 'VKORG' '' .
  init_fieldcat 'VKTXT' text-f07 '' '' '' 'TKKOT' 'VTEXT' '' .

  init_fieldcat 'POSNR' '' '' '' '' 'VBAP' 'POSNR' '' .
  init_fieldcat 'PTMAT' text-f08 '' '' '' 'VBAP' 'MATNR' '' .
  init_fieldcat 'PTKTX' text-f09 '' '' '' 'VBAP' 'ARKTX' '' .
  init_fieldcat 'MATNR' '' '' '' '' 'VBAP' 'MATNR' '' .
  init_fieldcat 'ARKTX' '' '' '' '' 'VBAP' 'ARKTX' '' .
  init_fieldcat 'KWMENG' '' '' '' 'VRKME' 'VBAP' 'KWMENG' '' .
  init_fieldcat 'VRKME' '' '' '' '' 'VBAP' 'VRKME' '' .
  init_fieldcat 'KBETR' '单价' '' '' 'KONV' 'KBETR' 'KBETR' '' .
  init_fieldcat 'JETJ' '金额' '' '' '' 'FAGLFLEXT' 'TSL01' '' .
  init_fieldcat 'LTEXT' text-f10 '' '' '' '' '' '' .
  init_fieldcat 'PSTYV' '' '' '' '' 'VBAP' 'PSTYV' '' .
  init_fieldcat 'PSTXT' text-f11 '' '' '' 'TVAPT' 'PSTXT' '' .
  init_fieldcat 'VBELN_LP' '' '' '' '' 'LIPS' 'VBELN' '' .
  init_fieldcat 'POSNR_LP' '' '' '' '' 'LIPS' 'POSNR' '' .
"  init_fieldcat 'LFIMG' '' '' '' '' 'LIPS' 'LFIMG' '' .
   init_fieldcat 'LFIMG' '' '' '' 'MEINS' 'VBAP' 'KWMENG' '' . .
  init_fieldcat 'MEINS' '' '' '' '' 'LIPS' 'MEINS' '' .
  init_fieldcat 'WADAT_IST' '' '' '' '' 'LIKP' 'WADAT_IST' '' .
  init_fieldcat 'WJHSL' text-f12 '' '' 'VRKME' 'VBAP' 'KWMENG' '' .
  init_fieldcat 'VBELN_FP' '' '' '' '' 'VBRP' 'VBELN' '' .
  init_fieldcat 'ABGRU' '决绝原因' '' '' '' 'VBAP' 'ABGRU' '' .
  init_fieldcat 'BEZEI' '决绝原因描述' '' '' '' '' '' '' .
  init_fieldcat 'KDMAT' '客户物料' '' '' '' 'VBAP' 'KDMAT' '' .


endform.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
form frm_output tables pt_lvc type lvc_t_fcat
                       pt_sort type lvc_t_sort
                       pt_data
                using pu_status
                      pu_ucomm
                      pw_layout type lvc_s_layo
                      pw_variant type disvariant
                      pw_grid_settings type lvc_s_glay.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
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
    tables
      t_outtab                 = pt_data
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
form alv_pf_status using rt_extab type slis_t_extab.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  set pf-status 'STANDARD_FULLSCREEN' excluding rt_extab.
endform.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
form alv_user_command using r_ucomm like sy-ucomm
                            rs_selfield type slis_selfield.
  data:l_index type sy-tabix.
data: it_line like line of it_output.

  case r_ucomm.
    when '&IC1'."双击
       IF rs_selfield-fieldname = 'VBELN'.
         read table it_output into it_line  index rs_selfield-tabindex.
          if sy-subrc = 0.
            set parameter id 'AUN' FIELD it_line-vbeln.
            call transaction 'VA03' and skip first screen.
           endif.
        ENDIF.
         IF rs_selfield-fieldname = 'VBELN_LP'.
         read table it_output into it_line  index rs_selfield-tabindex.
          if sy-subrc = 0.
            set parameter id 'VL' FIELD it_line-vbeln_LP.
            call transaction 'VL03N' and skip first screen.
           endif.
        ENDIF.
*      read table it_data into wa_data index rs_selfield-tabindex.
*      if sy-subrc = 0.
*        set parameter id 'XXXX' field wa_data-matnr.   "选择屏字段ID
*        call transaction 'XXXX' and skip first screen."填T-code
*      endif.
*    when 'PRINT'."打印
*      perform frm_print_select.
*    when '&ZALL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = 'X'.
*        modify it_data from wa_data index l_index.
*      endloop.
*    when '&ZSAL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = ''.
*        modify it_data from wa_data index l_index.
*      endloop.
    when others.

  endcase.

  rs_selfield-refresh = 'X'.
endform.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       ALV事件
*----------------------------------------------------------------------*
form frm_build_event .
  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type = 0
    importing
      et_events   = gt_events[].

*  read table gt_events into gw_events with key name = slis_ev_top_of_page.
*  if sy-subrc = 0.
*    gw_events-form = 'ALV_TOP_OF_PAGE'.
*    modify gt_events from gw_events index sy-tabix.
*  endif.
endform.                    " FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_auth_check .
  select *
    into table it_tvko
    from tvko
    where vkorg in s_vkorg.

  loop at it_tvko into wa_tvko.
    authority-check object 'V_VBAK_VKO' id 'ACTVT' field '03'
                                        id 'SPART' dummy
                                        id 'VKORG' field wa_tvko-vkorg
                                        id 'VTWEG' dummy.
    if sy-subrc ne 0.
      message i001(zsd01) with wa_tvko-vkorg display like 'E'.
      stop.
    endif.
  endloop.

endform.
