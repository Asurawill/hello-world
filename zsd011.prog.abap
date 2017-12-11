*&---------------------------------------------------------------------*
*& 程序名称:ZSD011
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :订单汇总表
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

report zsd011.


************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
tables: vbak, likp.

************************************************************************
* Type Declaration
************************************************************************
types:begin of ty_output,
        vbeln      type vbak-vbeln,
        posnr      type vbap-posnr,
        uepos      type vbap-uepos,
        auart      type vbak-auart,
        autxt      type tvakt-bezei,     " 订单类型描述
        erdat      type vbak-erdat,
        zsd0302    type vbak-zsd0302,  " 销售立项号
        pjnam(200),                 " 项目名称
        scwcr      type vbak-vdatu,      " 要求生产完成时间 = 回复发货日期 - 2
        vdatu      type vbak-vdatu,      " 回复发货日期
        xlnam      type kna1-name1,      " 经办人
        ptmat      type vbap-matnr,      " 屏体物料
        ptktx      type vbap-arktx,      " 屏体物料描述
        matnr      type vbap-matnr,
        arktx      type vbap-arktx,
        kwmeng     type vbap-kwmeng,
        vrkme      type vbap-vrkme,
        pstyv      type vbap-pstyv,
        pstxt      type tvapt-vtext,     " 项目类别描述
        breit      type mara-breit,
        hoehe      type mara-hoehe,
        MEABM      type mara-MEABM,
        zarea      type p decimals 3,    " 总面积
        atwrt      type ausp-atwrt,     " 产品类型
        xhwrt      type ausp-atwrt,     " 型号/规格
        vbeln_lp   type lips-vbeln,
        posnr_lp   type lips-posnr,
        lfimg      type lips-lfimg,
        meins      type lips-meins,
        wadat_ist  type likp-wadat_ist,
        yarea      type p decimals 3,    " 已发货面积
        warea      type p decimals 3,    " 未发货面积
        zarea1      type p decimals 3,    " 总面积
        yarea1      type p decimals 3,    " 已发货面积
        warea1      type p decimals 3,    " 未发货面积
      end of ty_output.

types:begin of ty_vbakap,
        vbeln   type vbak-vbeln,
        posnr   type vbap-posnr,
        uepos   type vbap-uepos,
        auart   type vbak-auart,
        erdat   type vbak-erdat,
        zsd0302 type vbak-zsd0302,  " 销售立项号
        vdatu   type vbak-vdatu,      " 回复发货日期
        matnr   type vbap-matnr,
        arktx   type vbap-arktx,
        kwmeng  type vbap-kwmeng,
        vrkme   type vbap-vrkme,
        pstyv   type vbap-pstyv,
      end of ty_vbakap.

types:begin of ty_likpps,
        vbeln     type lips-vbeln,
        posnr     type lips-posnr,
        lfimg     type lips-lfimg,
        meins     type lips-meins,
        vgbel     type lips-vgbel,
        vgpos     type lips-vgpos,
        wadat_ist type likp-wadat_ist,
      end of ty_likpps.

************************************************************************
* Internal Table
************************************************************************
data: it_output  type table of ty_output,
      wa_output  type ty_output,
      it_vbakap  type table of ty_vbakap,
      wa_vbakap  type ty_vbakap,
      wa_vbakap2 type ty_vbakap,
      it_likpps  type table of ty_likpps,
      wa_likpps  type ty_likpps.

data: it_tvakt type table of tvakt,
      wa_tvakt type tvakt,
      it_tvapt type table of tvapt,
      wa_tvapt type tvapt.

data: it_kna1 type table of kna1,
      wa_kna1 type kna1,
      it_vbpa type table of vbpa,
      wa_vbpa type vbpa,
      it_mara type table of mara,
      wa_mara type mara,
      it_auvb type table of ausp,
      wa_auvb type ausp,
      it_ausp type table of ausp,
      wa_ausp type ausp.

data: it_tvko type table of tvko,
      wa_tvko type tvko.

************************************************************************
* WorkArea
************************************************************************


************************************************************************
*      DEFINITION
************************************************************************
define init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
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
  gw_lvc-cfieldname =  &4.
  gw_lvc-qfieldname =  &5.
  gw_lvc-ref_table = &6.
  gw_lvc-ref_field = &7.
*  gw_lvc-datatype = &8.
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
                s_vkorg for vbak-vkorg obligatory memory id vko,
                s_erdat for vbak-erdat,
                s_wadat for likp-wadat_ist.
selection-screen end of block blk1.


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
  select p~vbeln
         p~posnr
         p~uepos
         k~auart
         k~erdat
         k~zsd0302
         k~vdatu
         p~matnr
         p~arktx
         p~kwmeng
         p~vrkme
         p~pstyv
    into table it_vbakap
    from vbak as k
    join vbap as p
      on k~vbeln = p~vbeln
    where p~vbeln in s_vbeln
      and k~auart in s_auart
      and k~vkorg in s_vkorg
      and k~erdat in s_erdat.

  if it_vbakap is initial.
    message '无相关数据！' type 'I' display like 'E'.
    stop.
  endif.

  select p~vbeln
         p~posnr
         p~lfimg
         p~meins
         p~vgbel
         p~vgpos
         k~wadat_ist
    into table it_likpps
    from likp as k
    join lips as p
      on k~vbeln = p~vbeln
    for all entries in it_vbakap
    where vgbel = it_vbakap-vbeln
      and vgpos = it_vbakap-posnr
      and wadat_ist in s_wadat
      and lfimg gt 0.

  select *
    into table it_tvakt
    from tvakt
    for all entries in it_vbakap
    where auart = it_vbakap-auart
      and spras = sy-langu.

  select *
    into table it_tvapt
    from tvapt
    for all entries in it_vbakap
    where pstyv = it_vbakap-pstyv
      and spras = sy-langu.

  select *
    into table it_vbpa
    from vbpa
    for all entries in it_vbakap
    where vbeln = it_vbakap-vbeln
      and posnr = ''
      and parvw = 'Z3'.
  if it_vbpa is not initial.
    select *
      into table it_kna1
      from kna1
      for all entries in it_vbpa
      where kunnr = it_vbpa-kunnr.
  endif.

  select *
    into table it_mara
    from mara
    for all entries in it_vbakap
    where matnr = it_vbakap-matnr.

  loop at it_mara into wa_mara.
    wa_auvb-objek = wa_mara-matnr.
    append wa_auvb to it_auvb.
  endloop.

  if it_auvb is not initial.
    sort it_auvb by objek.
    delete adjacent duplicates from it_auvb comparing objek.
    select *
      into table it_ausp
      from ausp
      for all entries in it_auvb
      where objek = it_auvb-objek.
  endif.

  sort it_vbakap by vbeln posnr.
  sort it_likpps by vgbel vgpos.
  sort it_tvakt by auart.
  sort it_tvapt by pstyv.
  sort it_vbpa by vbeln.
  sort it_kna1 by kunnr.
  sort it_mara by matnr.
  sort it_ausp by objek atinn.

  data: l_objek type ausp-objek.

  data: l_count  type i,
        l_zarea  type p decimals 3,
        l_warea  type p decimals 3,
        l_kwmeng type vbap-kwmeng.

  data: l_atinn1 type atinn,
        l_atinn2 type atinn.

  call function 'CONVERSION_EXIT_ATINN_INPUT'
    exporting
      input  = 'CPLX'
    importing
      output = l_atinn1.

  call function 'CONVERSION_EXIT_ATINN_INPUT'
    exporting
      input  = 'GGXH'
    importing
      output = l_atinn2.

  loop at it_vbakap into wa_vbakap.
    clear wa_output.

    move-corresponding wa_vbakap to wa_output.

    read table it_mara into wa_mara with key matnr = wa_output-matnr binary search.
    if sy-subrc = 0.
      if wa_mara-matkl ne '1001' and wa_mara-matkl ne '1002'.   " 只取箱体物料。
        continue.
      endif.
      wa_output-breit = wa_mara-breit.    " 宽
      wa_output-hoehe = wa_mara-hoehe.    " 高
      wa_output-MEABM = wa_mara-MEABM.    " 单位
    endif.

    wa_output-scwcr = wa_output-vdatu - 2.  " 要求生产完成时间 = 回复发货日期 - 2

    if wa_output-auart = 'ZPO' or wa_output-auart = 'ZWV' or wa_output-auart = 'ZF1' or wa_output-auart = 'ZZG'.
      if wa_output-uepos = ''.  " 屏体物料行不显示
        continue.
      else.
        " 取屏体物料及描述
        read table it_vbakap into wa_vbakap2 with key vbeln = wa_output-vbeln
                                                      posnr = wa_output-uepos
                                                      binary search.
        if sy-subrc = 0.
          wa_output-ptmat = wa_vbakap2-matnr.
          wa_output-ptktx = wa_vbakap2-arktx.
        endif.
      endif.
    endif.

    read table it_tvakt into wa_tvakt with key auart = wa_output-auart binary search.
    if sy-subrc = 0.
      wa_output-autxt = wa_tvakt-bezei.   " 订单类型描述
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

    read table it_vbpa into wa_vbpa with key vbeln = wa_output-vbeln binary search.
    if sy-subrc = 0.
      read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
      if sy-subrc = 0.
        wa_output-xlnam = wa_kna1-name1.  " 经办人
      endif.
    endif.

    read table it_tvapt into wa_tvapt with key pstyv = wa_output-pstyv binary search.
    if sy-subrc = 0.
      wa_output-pstxt = wa_tvapt-vtext.   " 项目类别描述
    endif.

    wa_output-zarea = wa_output-breit * wa_output-hoehe * wa_output-kwmeng.   " 总面积

    l_objek = wa_output-matnr.

    read table it_ausp into wa_ausp with key objek = l_objek
                                             atinn = l_atinn1   " 'CPLX'
                                             binary search.
    if sy-subrc = 0.
      wa_output-atwrt = wa_ausp-atwrt.    " 产品类型
    endif.

    read table it_ausp into wa_ausp with key objek = l_objek
                                             atinn = l_atinn2   " 'GGXH'
                                             binary search.
    if sy-subrc = 0.
      wa_output-xhwrt = wa_ausp-atwrt.    " 规格型号
    endif.

    l_count = 0.

    loop at it_likpps into wa_likpps where vgbel = wa_output-vbeln
                                       and vgpos = wa_output-posnr.
      l_count = l_count + 1.
    endloop.

    if l_count = 0.
      append wa_output to it_output.
    elseif l_count = 1.
      read table it_likpps into wa_likpps with key vgbel = wa_output-vbeln
                                                   vgpos = wa_output-posnr
                                                   binary search.
      if sy-subrc = 0.
        wa_output-vbeln_lp = wa_likpps-vbeln.
        wa_output-posnr_lp = wa_likpps-posnr.
        wa_output-lfimg = wa_likpps-lfimg.
        wa_output-meins = wa_likpps-meins.
        wa_output-wadat_ist = wa_likpps-wadat_ist.

        wa_output-yarea = wa_output-breit * wa_output-hoehe * wa_output-lfimg.   " 已发货面积

        wa_output-warea = wa_output-zarea - wa_output-yarea.   " 未发货面积

        append wa_output to it_output.
      endif.
    else.
      l_warea = wa_output-zarea.
      l_zarea = wa_output-zarea.
      l_kwmeng = wa_output-kwmeng.

      loop at it_likpps into wa_likpps where vgbel = wa_output-vbeln
                                         and vgpos = wa_output-posnr.
        wa_output-vbeln_lp = wa_likpps-vbeln.
        wa_output-posnr_lp = wa_likpps-posnr.
        wa_output-lfimg = wa_likpps-lfimg.
        wa_output-meins = wa_likpps-meins.
        wa_output-wadat_ist = wa_likpps-wadat_ist.

        wa_output-kwmeng = wa_output-lfimg.

        l_kwmeng = l_kwmeng - wa_output-lfimg.

        wa_output-yarea = wa_output-breit * wa_output-hoehe * wa_output-lfimg.   " 已发货面积

        wa_output-zarea = wa_output-yarea.

        wa_output-warea = 0.   " 未发货面积

        l_warea = l_warea - wa_output-yarea.

        append wa_output to it_output.
      endloop.

      if l_kwmeng > 0.
        " 剩余未清
        clear: wa_output-vbeln_lp,
               wa_output-posnr_lp,
               wa_output-lfimg,
               wa_output-meins,
               wa_output-wadat_ist.

        wa_output-kwmeng = l_kwmeng.

        wa_output-zarea = l_warea.

        wa_output-yarea = 0.   " 已发货面积

        wa_output-warea = l_warea.   " 未发货面积

        append wa_output to it_output.
      endif.
    endif.
  endloop.

  delete it_output where wadat_ist not in s_wadat.
  LOOP AT it_output INTO wa_output.
    wa_output-ZAREA = wa_output-ZAREA / 1000000.
    wa_output-YAREA = wa_output-YAREA / 1000000.
    wa_output-WAREA = wa_output-WAREA / 1000000.
    MODIFY it_output FROM wa_output.
  ENDLOOP.

endform.

FORM frm_unit_conv USING input UNIT_IN   CHANGING output.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input                      = input
*     NO_TYPE_CHECK              = 'X'
*     ROUND_SIGN                 = ' '
      UNIT_IN                    = UNIT_IN
      UNIT_OUT                   = 'M'
    IMPORTING
*     ADD_CONST                  =
*     DECIMALS                   =
*     DENOMINATOR                =
*     NUMERATOR                  =
      OUTPUT                     = output
    EXCEPTIONS
      CONVERSION_NOT_FOUND       = 1
      DIVISION_BY_ZERO           = 2
      INPUT_INVALID              = 3
      OUTPUT_INVALID             = 4
      OVERFLOW                   = 5
      TYPE_INVALID               = 6
      UNITS_MISSING              = 7
      UNIT_IN_NOT_FOUND          = 8
      UNIT_OUT_NOT_FOUND         = 9
      OTHERS                     = 10
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

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

  init_fieldcat 'VBELN' '' '' '' '' 'VBAP' 'VBELN'.
  init_fieldcat 'POSNR' '' '' '' '' 'VBAP' 'POSNR'.
  init_fieldcat 'AUART' '' '' '' '' 'VBAK' 'AUART'.
  init_fieldcat 'AUTXT' text-f01 '' '' '' 'TVAKT' 'BEZEI'.
  init_fieldcat 'ERDAT' '' '' '' '' 'VBAK' 'ERDAT'.
  init_fieldcat 'ZSD0302' '' '' '' '' 'VBAK' 'ZSD0302'.
  init_fieldcat 'PJNAM' text-f02 '' '' '' '' ''.
  init_fieldcat 'SCWCR' text-f03 '' '' '' 'VBAK' 'VDATU'.
  init_fieldcat 'VDATU' '' '' '' '' 'VBAK' 'VDATU'.
  init_fieldcat 'XLNAM' text-f04 '' '' '' 'KNA1' 'KANME'.
  init_fieldcat 'PTMAT' text-f05 '' '' '' 'VBAP' 'MATNR'.
  init_fieldcat 'PTKTX' text-f06 '' '' '' 'VBAP' 'ARKTX'.
  init_fieldcat 'MATNR' '' '' '' '' 'VBAP' 'MATNR'.
  init_fieldcat 'ARKTX' '' '' '' '' 'VBAP' 'ARKTX'.
  init_fieldcat 'KWMENG' '' '' '' 'VRKME' 'VBAP' 'KWMENG'.
  init_fieldcat 'VRKME' '' '' '' '' 'VBAP' 'VRKME'.
  init_fieldcat 'PSTYV' '' '' '' '' 'VBAP' 'PSTYV'.
  init_fieldcat 'PSTXT' text-f07 '' '' '' 'TVAPT' 'PSTXT'.
  init_fieldcat 'ZAREA' text-f08 '' '' '' '' ''.
  init_fieldcat 'ATWRT' text-f09 '' '' '' 'AUSP' 'ATWRT'.
  init_fieldcat 'XHWRT' text-f10 '' '' '' 'AUSP' 'ATWRT'.
  init_fieldcat 'VBELN_LP' '' '' '' '' 'LIPS' 'VBELN'.
  init_fieldcat 'POSNR_LP' '' '' '' '' 'LIPS' 'POSNR'.
  init_fieldcat 'LFIMG' '' '' '' 'MEINS' 'LIPS' 'LFIMG'.
  init_fieldcat 'MEINS' '' '' '' '' 'LIPS' 'MEINS'.
  init_fieldcat 'WADAT_IST' '' '' '' '' 'LIKP' 'WADAT_IST'.
  init_fieldcat 'YAREA'   text-f11 '' '' '' '' ''.
  init_fieldcat 'WAREA'   text-f12 '' '' '' '' ''.
  init_fieldcat 'MEABM'   text-f13 '' '' '' '' ''.
  init_fieldcat 'ZAREA1'  text-f14 '' '' '' '' ''.
  init_fieldcat 'YAREA1'  text-f15 '' '' '' '' ''.
  init_fieldcat 'WAREA1'  text-f16 '' '' '' '' ''.
  init_fieldcat 'MEABM1'   text-f17 '' '' '' '' ''.

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

  case r_ucomm.
    when '&IC1'."双击
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
