*&---------------------------------------------------------------------*
*&  Include           ZFI008_TOP
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
tables: vbap, bkpf.

************************************************************************
* Type Declaration
************************************************************************
types:begin of ty_head,
        bldat      type bkpf-bldat,
        blart      type bkpf-blart,
        bukrs      type bkpf-bukrs,
        budat      type bkpf-budat,
        waers      type bkpf-waers,
        bktxt      type bkpf-bktxt,
        vbeln      type vbak-vbeln,
        belnr      type bkpf-belnr,
        gjahr      type bkpf-gjahr,
        belnr2     type bkpf-belnr,
        gjahr2     type bkpf-gjahr,
        bstyp      type c,
        pjnam(200),
      end of ty_head.

types:begin of ty_item,
        buzei     type bseg-buzei,
        bschl     type bseg-bschl,
        hkont     type bseg-hkont,
        hktxt     type skat-txt20,
        kunnr     type bseg-kunnr,
        kname     type kna1-name1,
        wrbtr     type bseg-wrbtr,
        waers     type bkpf-waers,
        zterm     type bseg-zterm,
        zfbdt     type bseg-zfbdt,
        vbeln     type vbak-vbeln,
        posnr     type vbap-posnr,
        mwskz     type bseg-mwskz,
        sgtxt     type bseg-sgtxt,
        cellstyle type lvc_t_styl,
        matnr     type vbap-matnr,
        netwr     type vbap-netwr,
      end of ty_item.



types:begin of ty_kpjl.
        include structure zfi008.
types:pjnam(200),
      bktxt      type bkpf-bktxt,
      KUNNR TYPE VBAK-KUNNR,"客户编码10702
      NAME1 TYPE KNA1-NAME1,"客户名称
      box,
      end of ty_kpjl.

types:begin of ty_qkcx,
        bukrs type vbak-bukrs_vf,
        vbeln type vbap-vbeln,
        posnr type vbap-posnr,
        netwr type vbap-netwr,
        waerk type vbap-waerk,
        wrbtr type zfi008-wrbtr,
        srdkp type zfi008-wrbtr,

*        fkstk type vbuk-fkstk,
        fksak type vbuk-fksak,
        PSWBT type BSEG-PSWBT, "add IT02 150623 收入-调整字段
        pjnam(300),
      end of ty_qkcx.

types:begin of ty_vbakap,
        vbeln    type vbap-vbeln,
        posnr    type vbap-posnr,
        bukrs_vf type vbak-bukrs_vf,
        pstyv    type vbap-pstyv,
        netwr    type vbap-netwr,
        waerk    type vbap-waerk,
      end of ty_vbakap.

types:begin of ty_srys,
        hkont type bseg-hkont,    "原过账科目     "add it02 20170713 映射科目调整
        hkont_tz type bseg-hkont, "自动凭证科目
      end of ty_srys.

data: it_skat type table of skat,
      wa_skat type skat.

data:gt_srys type table of ty_srys,
     gs_srys type ty_srys.

************************************************************************
* Internal Table & WorkArea
************************************************************************
data: wa_head type ty_head.

data: it_item type table of ty_item,
      wa_item type ty_item.

data: it_vbak type table of vbak,
      wa_vbak type vbak,
      it_vbap type table of vbap,
      wa_vbap type vbap,
      it_vbkd type table of vbkd,
      wa_vbkd type vbkd.

data: it_zfi008 type table of zfi008,
      wa_zfi008 type zfi008.

data: g_objname type thead-tdname.

data: it_lines type table of tline,
      wa_lines type tline.

data: g_vrmname   type vrm_id,
      it_vrmlist  type vrm_values,
      wa_vrmvalue like line of it_vrmlist.

field-symbols: <fs_item> type ty_item.

data: it_kpjl type table of ty_kpjl,
      wa_kpjl type ty_kpjl.

data: it_bkpf type table of bkpf,
      wa_bkpf type bkpf.

data: it_qkcx type table of ty_qkcx,
      wa_qkcx type ty_qkcx.

data: it_vbuk type table of vbuk,
      wa_vbuk type vbuk.

data: it_vbakap type table of ty_vbakap,
      wa_vbakap type ty_vbakap.

data: it_vbpa type table of vbpa,
      wa_vbpa type vbpa.

************************************************************************
*      BAPI Variant
************************************************************************


************************************************************************
*      DEFINITION
************************************************************************
define init_fieldcat.      "  ALV Fieldcat Setting
  clear gw_lvc.
  gw_lvc-fieldname = &1.
  gw_lvc-coltext = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext = &2.
  gw_lvc-edit = &3.
  gw_lvc-cfieldname = &4.
  gw_lvc-qfieldname =  &5.
  gw_lvc-ref_table = &6.
  gw_lvc-ref_field = &7.
  gw_lvc-outputlen = &8.
*  gw_lvc-intlen = &9.
  IF &1 = 'ZTERM' or &1 = 'HKONT' or &1 = 'MWSKZ'.
    gw_lvc-f4availabl = 'X'.
  ENDIF.
  IF &1 = 'SFNCX'.
    gw_lvc-checkbox = 'X'.
  ENDIF.
  append gw_lvc to gt_lvc.
end-of-definition.
define init_fieldcat2.      "  ALV Fieldcat Setting
  clear gw_lvc.
  gw_lvc-fieldname = &1.
  gw_lvc-coltext = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext = &2.
  gw_lvc-edit = &3.
  gw_lvc-cfieldname = &4.
  gw_lvc-qfieldname =  &5.
  gw_lvc-ref_table = &6.
  gw_lvc-ref_field = &7.
  gw_lvc-checkbox = &8.
*  gw_lvc-intlen = &9.
  append gw_lvc to gt_lvc.
end-of-definition.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
type-pools: slis.

data: gt_lvc           type lvc_t_fcat,
      gt_sort          type lvc_t_sort,
      gw_lvc           type lvc_s_fcat,
      gw_sort          type lvc_s_sort,
      gw_layout        type lvc_s_layo,   "alv的格式
      gw_variant       type disvariant,
      gw_grid_settings type lvc_s_glay,
      gt_events        type slis_t_event with header line, "保存AVL事件
      gw_events        like line of gt_events.

data: gt_exclude type slis_t_extab,
      gs_exclude type slis_extab.

data: gt_oo_exclude type ui_functions.

data: gr_container type ref to cl_gui_custom_container,
      gr_alvgrid   type ref to cl_gui_alv_grid.

data: gt_rows type lvc_t_row,
      gt_roid type lvc_t_roid,
      wa_rows type lvc_s_row,
      wa_roid type lvc_s_roid.

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

data: ok_code type sy-ucomm,
      save_ok type sy-ucomm.

data: g_edit_mod type c,
      g_changed  type c.

data: g_msg(400).

data: g_land1 type t001-land1,
      g_kalsm type t005-kalsm.

data: g_stgrd type bkpf-stgrd.

data: g_suc.

************************************************************************
* Constant
************************************************************************
constants: gc_editable type c value 'X',
           gc_readonly type c value ''.

constants: abap_true  type c value 'X',
           abap_false type c value ''.

************************************************************************
* BAPI
************************************************************************
data: wa_documentheader    type bapiache09,
      it_accountgl         type table of bapiacgl09,
      wa_accountgl         type bapiacgl09,
      it_accountreceivable type table of bapiacar09,
      wa_accountreceivable type bapiacar09,
      it_currencyamount    type table of bapiaccr09,
      wa_currencyamount    type bapiaccr09,
      it_criteria          type table of bapiackec9,
      wa_criteria          type bapiackec9,
      it_valuefield        type table of bapiackev9,
      wa_valuefield        type bapiackev9,
      it_extension2        type table of bapiparex,
      wa_extension2        type bapiparex,
      it_return            type table of bapiret2,
      wa_return            type bapiret2.

data: wa_obj type bapiache09.

data: wa_zaccdocuext type zaccdocuext.

" BAPI_ACC_DOCUMENT_REV_POST
data: wa_reversal type bapiacrev,
      wa_bus      type bapiache09.


************************************************************************
* Selection Screen
************************************************************************
selection-screen begin of block blk1 with frame title text-001.
parameter: p_kppt type c radiobutton group g1 default 'X' user-command ucomm,
           p_kpjl type c radiobutton group g1,
           p_qkcx type c radiobutton group g1.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-002.
parameter: p_bukrs type bkpf-bukrs modif id cx memory id buk.
select-options: s_vbeln for vbap-vbeln modif id cx,
                s_posnr for vbap-posnr modif id cx,
                s_budat for bkpf-budat modif id jl.
selection-screen end of block blk2.


************************************************************************
* At selection screen
************************************************************************
at selection-screen output.
  loop at screen.
    if screen-group1 = 'CX'.
      if p_kpjl = 'X' or p_qkcx = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    elseif screen-group1 = 'JL'.
      if p_kpjl = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    endif.
  endloop.

at selection-screen.



************************************************************************
* Initialization
************************************************************************
initialization.


************************************************************************
* Event Start of Selection
************************************************************************
start-of-selection.
  if p_kppt = 'X'.      " 项目开票
    perform frm_data_clear.
    perform frm_data_init.
    call screen 9001.
  elseif p_kpjl = 'X'.  " 项目开票记录
    perform frm_auth_check using '03' p_bukrs.
    if sy-subrc ne 0.
      message i011(zfico01) with p_bukrs display like 'E'.
      stop.
    endif.
    perform frm_input_check.
    perform frm_kpjl_report.
  elseif p_qkcx = 'X'.  " 项目开票情况查询
    perform frm_auth_check using '03' p_bukrs.
    if sy-subrc ne 0.
      message i011(zfico01) with p_bukrs display like 'E'.
      stop.
    endif.
    perform frm_input_check.
    perform frm_input_check.
    perform frm_qkcx_report.
  endif.

************************************************************************
* Event End-of selection
************************************************************************
end-of-selection.
