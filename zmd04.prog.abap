*&---------------------------------------------------------------------*
*& 程序名称:ZMD04
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :库存需求清单
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

report zmd04.

************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
tables: vbak, ekko.

************************************************************************
* Type Declaration
************************************************************************
types:begin of ty_show,
        ebeln     type ekpo-ebeln,
        ebelp     type ekpo-ebelp,
*        txz01  type ekpo-txz01,
        menge     type ekpo-menge,
        meins     type ekpo-meins,

        vbeln     type vbap-vbeln,
        posnr     type vbap-posnr,
*        arktx  type vbap-arktx,
        kwmeng    type vbap-kwmeng,
        vrkme     type vbap-vrkme,

        bmein     type mara-meins,
        matnr     type ekpo-matnr,
        maktx     type makt-maktx,
        werks     type ekpo-werks,

        yjhsl     type ekpo-menge,      " 已交货数量
        wjhsl     type ekpo-menge,      " 未交货数量
        labst     type mard-labst,      " 非限制库存

        cstuf(20),
        stufe     type stpox-stufe,     " 层级
        idnrk     type stpox-idnrk,
        ojtxp     type stpox-ojtxp,
        mtart     type stpox-mtart,
        mnglg     type stpox-mnglg,
        beskz     type stpox-beskz,     " 采购类型
        sobsl     type stpox-sobsl,     " 特殊采购类
        idnst     type mard-labst,      " 组件库存
      end of ty_show.

************************************************************************
* Internal Table
************************************************************************
data: it_ekko type table of ekko,
      wa_ekko type ekko,
      it_ekpo type table of ekpo,
      wa_ekpo type ekpo,
      it_vbak type table of vbak,
      wa_vbak type vbak,
      it_vbap type table of vbap,
      wa_vbap type vbap.

field-symbols: <fs_ekko> type ekko,
               <fs_ekpo> type ekpo,
               <fs_vbak> type vbak,
               <fs_vbap> type vbap.

data: it_data type table of ty_show,
      wa_data type ty_show,
      it_show type table of ty_show,
      wa_show type ty_show.

field-symbols: <fs_data> type ty_show,
               <fs_show> type ty_show.

data: it_mara    type table of mara,
      wa_mara    type mara,
      it_mard    type table of mard,
      wa_mard    type mard,
      it_stbmard type table of mard,
      wa_stbmard type mard,
      it_makt    type table of makt,
      wa_makt    type makt.

data: it_stb type table of stpox,
      wa_stb type stpox.

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
*  gw_lvc-outputlen = &3.
*  if &4 = 'x'.
*    gw_lvc-no_zero = 'x'.
*  endif.
*  gw_lvc-icon = &4.
  gw_lvc-checkbox = &3.
  gw_lvc-edit = &4.
*  gw_lvc-fix_column =  &5.
  gw_lvc-cfieldname =  &5.
  gw_lvc-qfieldname =  &6.
  gw_lvc-ref_table = &7.
  gw_lvc-ref_field = &8.
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



************************************************************************
* Constant
************************************************************************
constants: gc_dot(20) value '....................'.


************************************************************************
* Selection Screen
************************************************************************
selection-screen begin of block blk1 with frame title text-001.
parameters: p_po type c radiobutton group g1 default 'X' user-command ucomm,
            p_so type c radiobutton group g1.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-002.
select-options: s_ebeln for ekko-ebeln matchcode object mekk memory id bes modif id po.
selection-screen end of block blk2.

selection-screen begin of block blk3 with frame title text-003.
select-options: s_vbeln for vbak-vbeln matchcode object vmva memory id aun modif id so.
selection-screen end of block blk3.

selection-screen begin of block blk4 with frame title text-004.
parameters: p_expbom type c as checkbox default 'X'.
selection-screen end of block blk4.


************************************************************************
* Initialization
************************************************************************
initialization.


************************************************************************
* At selection screen
************************************************************************
at selection-screen output.
  loop at screen.
    if screen-group1 = 'PO'.
      if p_po = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    elseif screen-group1 = 'SO'.
      if p_so = 'X'.
        screen-active = 1.
      else.
        screen-active = 0.
      endif.
      modify screen.
    endif.
  endloop.

at selection-screen.



************************************************************************
* Event Start of Selection
************************************************************************
start-of-selection.
  perform frm_get_orders.
  perform frm_auth_check.
  perform frm_get_data.


************************************************************************
* Event End-of selection
************************************************************************
end-of-selection.
  perform frm_display.

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_orders .
  if p_po = 'X'.
    if s_ebeln[] is initial.
      message text-m01 type 'I' display like 'E'.
      stop.
    endif.

    select *
      into table it_ekko
      from ekko
      where ebeln in s_ebeln.
    sort it_ekko by ebeln.

    select *
      into table it_ekpo
      from ekpo
      where ebeln in s_ebeln.
    sort it_ekpo by ebeln ebelp.
  elseif p_so = 'X'.
    if s_vbeln[] is initial.
      message text-m02 type 'I' display like 'E'.
      stop.
    endif.

    select *
      into table it_vbak
      from vbak
      where vbeln in s_vbeln.
    sort it_vbak by vbeln.

    select *
      into table it_vbap
      from vbap
      where vbeln in s_vbeln.
    sort it_vbap by vbeln posnr.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_auth_check .
  if p_po = 'X'.
    loop at it_ekko assigning <fs_ekko>.
      authority-check object 'M_BEST_EKO' id 'ACTVT' field '02'
                                          id 'EKORG' field <fs_ekko>-ekorg.
      if sy-subrc ne 0.
        message i018(zmm01) with <fs_ekko>-ekorg display like 'E'.
        stop.
      endif.
    endloop.
  elseif p_so = 'X'.
    loop at it_vbak assigning <fs_vbak>.
      authority-check object 'V_VBAK_VKO' id 'ACTVT' field '03'
                                          id 'SPART' dummy
                                          id 'VKORG' field <fs_vbak>-vkorg
                                          id 'VTWEG' dummy.
      if sy-subrc ne 0.
        message i001(zsd01) with <fs_vbak>-vkorg display like 'E'.
        stop.
      endif.
    endloop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  if p_po = 'X'.
    sort it_ekpo by ebeln ebelp.
    move-corresponding it_ekpo to it_data.
  elseif p_so = 'X'.
    sort it_vbap by vbeln posnr.
    move-corresponding it_vbap to it_data.
  endif.

  select *
    into table it_mara
    from mara
    for all entries in it_data
    where matnr = it_data-matnr.
  sort it_mara by matnr.

  select *
    into table it_mard
    from mard
    for all entries in it_data
    where matnr = it_data-matnr
      and werks = it_data-werks.
  sort it_mard by matnr werks.

  select *
    into table it_makt
    from makt
    for all entries in it_data
    where matnr = it_data-matnr
      and spras = sy-langu.
  sort it_makt by matnr.

  loop at it_data assigning <fs_data>.
    <fs_data>-yjhsl = 0.  " 考虑到此字段每个项目会有变化，先默认等于0

    if p_po = 'X'.
      <fs_data>-wjhsl = <fs_data>-menge - <fs_data>-yjhsl.  " 未交货数量=订单行数量-已交货数量；
    elseif p_so = 'X'.
      <fs_data>-wjhsl = <fs_data>-kwmeng - <fs_data>-yjhsl.  " 未交货数量=订单行数量-已交货数量；
    endif.

    read table it_mara into wa_mara with key matnr = <fs_data>-matnr binary search.
    if sy-subrc = 0.
      <fs_data>-bmein = wa_mara-meins.  " 基本计量单位
      <fs_data>-mtart = wa_mara-mtart.  " 物料类型
    endif.

    if p_po = 'X'.
      read table it_ekko into wa_ekko with key ebeln = <fs_data>-ebeln binary search.
      if sy-subrc = 0.
        <fs_data>-werks = wa_ekko-lifnr+6.
      endif.
    endif.

    loop at it_mard into wa_mard where matnr = <fs_data>-matnr
                                   and werks = <fs_data>-werks
                                   and diskz <> '1'.    " MRP标识: 仓储地点
      <fs_data>-labst = <fs_data>-labst + wa_mard-labst.
    endloop.

    read table it_makt into wa_makt with key matnr = <fs_data>-matnr binary search.
    if sy-subrc = 0.
      <fs_data>-maktx = wa_makt-maktx.
    endif.

    <fs_data>-stufe = 0.
    <fs_data>-cstuf = '0'.

    append <fs_data> to it_show.

    if p_expbom = 'X'.  " 展开BOM
      perform frm_explode_bom tables it_stb
                              using <fs_data>-matnr <fs_data>-werks <fs_data>-wjhsl.
      if sy-subrc ne 0.
        " 无BOM
        continue.
      endif.

      clear wa_show.

      wa_show-ebeln = <fs_data>-ebeln.
      wa_show-ebelp = <fs_data>-ebelp.
      wa_show-vbeln = <fs_data>-vbeln.
      wa_show-posnr = <fs_data>-posnr.
      wa_show-matnr = <fs_data>-matnr.
      wa_show-maktx = <fs_data>-maktx.
      wa_show-werks = <fs_data>-werks.
      wa_show-bmein = <fs_data>-bmein.

      perform frm_add_component.
    endif.
  endloop.
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
  if it_show is initial.
    stop.
  endif.

  perform init_layout.              "设置输出格式
  perform init_sort.                "设置排序、合计
  perform init_variant.             "设置变式控制
  perform frm_exclude.
  perform frm_build_event.

  if p_po = 'X'.
    perform frm_init_lvc_po.             " 初始化内表结构/ALV显示结构
  elseif p_so = 'X'.
    perform frm_init_lvc_so.             " 初始化内表结构/ALV显示结构
  endif.

  perform frm_output tables gt_lvc              "输出
                            gt_sort
                            it_show
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
*  init_fieldcat 'VBELN' '' '' '' '' '' 'VBAP' 'VBELN'.


endform.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
form frm_init_lvc_po.

  init_fieldcat 'EBELN' '' '' '' '' '' 'EKPO' 'EBELN'.
  init_fieldcat 'EBELP' '' '' '' '' '' 'EKPO' 'EBELP'.
  init_fieldcat 'WERKS' '' '' '' '' '' 'EKPO' 'WERKS'.
  init_fieldcat 'MATNR' '' '' '' '' '' 'EKPO' 'MATNR'.
*  init_fieldcat 'TXZ01' '' '' '' '' '' 'EKPO' 'TXZ01'.
  init_fieldcat 'MAKTX' '' '' '' '' '' 'MAKT' 'MAKTX'.

  if p_expbom = 'X'.
*    init_fieldcat 'STUFE' '' '' '' '' '' 'STPOX' 'STUFE'.
    init_fieldcat 'CSTUF' text-f03 '' '' '' '' '' ''.
    init_fieldcat 'IDNRK' '' '' '' '' '' 'STPOX' 'IDNRK'.
    init_fieldcat 'OJTXP' '' '' '' '' '' 'STPOX' 'OJTXP'.
    init_fieldcat 'MTART' '' '' '' '' '' 'STPOX' 'MTART'.
    init_fieldcat 'MNGLG' '' '' '' '' 'BMEIN' 'STPOX' 'MNGLG'.
    init_fieldcat 'BESKZ' '' '' '' '' '' 'STPOX' 'BESKZ'.
    init_fieldcat 'SOBSL' '' '' '' '' '' 'STPOX' 'SOBSL'.
*    init_fieldcat 'IDNST' '' '' '' '' '' 'MARD' 'LABST'.
  endif.

  init_fieldcat 'MEINS' '' '' '' '' '' 'EKPO' 'MEINS'.
  init_fieldcat 'MENGE' '' '' '' '' 'MEINS' 'EKPO' 'MENGE'.
  init_fieldcat 'YJHSL' text-f01 '' '' '' 'MEINS' 'EKPO' 'MENGE'.
  init_fieldcat 'WJHSL' text-f02 '' '' '' 'MEINS' 'EKPO' 'MENGE'.
  init_fieldcat 'BMEIN' '' '' '' '' '' 'MARA' 'MEINS'.
  init_fieldcat 'LABST' '' '' '' '' 'BMEIN' 'MARD' 'LABST'.
endform.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
form frm_init_lvc_so.

  init_fieldcat 'VBELN' '' '' '' '' '' 'VBAP' 'VBELN'.
  init_fieldcat 'POSNR' '' '' '' '' '' 'VBAP' 'POSNR'.
  init_fieldcat 'WERKS' '' '' '' '' '' 'VBAP' 'WERKS'.
  init_fieldcat 'MATNR' '' '' '' '' '' 'VBAP' 'MATNR'.
*  init_fieldcat 'ARKTX' '' '' '' '' '' 'VBAP' 'ARKTX'.
  init_fieldcat 'MAKTX' '' '' '' '' '' 'MAKT' 'MAKTX'.

  if p_expbom = 'X'.
*    init_fieldcat 'STUFE' '' '' '' '' '' 'STPOX' 'STUFE'.
    init_fieldcat 'CSTUF' text-f03 '' '' '' '' '' ''.
    init_fieldcat 'IDNRK' '' '' '' '' '' 'STPOX' 'IDNRK'.
    init_fieldcat 'OJTXP' '' '' '' '' '' 'STPOX' 'OJTXP'.
    init_fieldcat 'MTART' '' '' '' '' '' 'STPOX' 'MTART'.
    init_fieldcat 'MNGLG' '' '' '' '' 'BMEIN' 'STPOX' 'MNGLG'.
    init_fieldcat 'BESKZ' '' '' '' '' '' 'STPOX' 'BESKZ'.
    init_fieldcat 'SOBSL' '' '' '' '' '' 'STPOX' 'SOBSL'.
*    init_fieldcat 'IDNST' '' '' '' '' '' 'MARD' 'LABST'.
  endif.

  init_fieldcat 'VRKME' '' '' '' '' '' 'VBAP' 'VRKME'.
  init_fieldcat 'KWMENG' '' '' '' '' 'VRKME' 'VBAP' 'KWMENG'.
  init_fieldcat 'YJHSL' text-f01 '' '' '' 'VRKME' 'VBAP' 'KWMENG'.
  init_fieldcat 'WJHSL' text-f02 '' '' '' 'VRKME' 'VBAP' 'KWMENG'.
  init_fieldcat 'BMEIN' '' '' '' '' '' 'MARA' 'MEINS'.
  init_fieldcat 'LABST' '' '' '' '' 'BMEIN' 'MARD' 'LABST'.
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
    when '&DATA_SAVE'.
*      message 'Save data.' type 'I' display like 'S'.

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
*&      Form  FRM_EXPLODE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_explode_bom  tables pt_stb structure stpox
                      using value(p_mtnrv) type mara-matnr
                            value(p_werks) type marc-werks
                            value(p_emeng) type stko-bmeng.

  call function 'CS_BOM_EXPL_MAT_V2'
    exporting
*     FTREL                 = ' '
*     ALEKZ                 = ' '
*     ALTVO                 = ' '
*     AUFSW                 = ' '
      aumgb                 = 'X'
*     AUMNG                 = 0
*     AUSKZ                 = ' '
*     AMIND                 = ' '
*     BAGRP                 = ' '
*     BEIKZ                 = ' '
*     BESSL                 = ' '
*     BGIXO                 = ' '
*     BREMS                 = ' '
      capid                 = 'PP01'
*     CHLST                 = ' '
*     COSPR                 = ' '
*     CUOBJ                 = 000000000000000
*     CUOVS                 = 0
*     CUOLS                 = ' '
      datuv                 = sy-datum
*     DELNL                 = ' '
*     DRLDT                 = ' '
      ehndl                 = '1'
      emeng                 = p_emeng
*     ERSKZ                 = ' '
*     ERSSL                 = ' '
*     FBSTP                 = ' '
*     KNFBA                 = ' '
*     KSBVO                 = ' '
*     MBWLS                 = ' '
*     MKTLS                 = 'X'
*     MDMPS                 = ' '
      mehrs                 = 'X'   " 一展到底
*     MKMAT                 = ' '
*     MMAPS                 = ' '
*     SALWW                 = ' '
*     SPLWW                 = ' '
      mmory                 = '1'
      mtnrv                 = p_mtnrv
*     NLINK                 = ' '
*     POSTP                 = ' '
*     RNDKZ                 = ' '
*     RVREL                 = ' '
*     SANFR                 = ' '
*     SANIN                 = ' '
*     SANKA                 = ' '
*     SANKO                 = ' '
*     SANVS                 = ' '
*     SCHGT                 = ' '
*     STKKZ                 = ' '
*     STLAL                 = ' '
*     STLAN                 = ' '
*     STPST                 = 0
*     SVWVO                 = 'X'
      werks                 = p_werks
*     NORVL                 = ' '
*     MDNOT                 = ' '
*     PANOT                 = ' '
*     QVERW                 = ' '
*     VERID                 = ' '
*     VRSVO                 = 'X'
*   IMPORTING
*     TOPMAT                =
*     DSTST                 =
    tables
      stb                   = pt_stb
*     MATCAT                =
    exceptions
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      others                = 9.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_ADD_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_add_component .
  refresh it_stbmard.

  select *
    into table it_stbmard
    from mard
    for all entries in it_stb
    where matnr = it_stb-idnrk
      and werks = it_stb-werks.
  sort it_stbmard by matnr werks.

  loop at it_stb into wa_stb.
    wa_show-stufe = wa_stb-stufe.

    wa_show-cstuf = wa_show-stufe.
    condense wa_show-cstuf.
    concatenate gc_dot(wa_show-stufe) wa_show-cstuf into wa_show-cstuf.

    wa_show-idnrk = wa_stb-idnrk.
    wa_show-ojtxp = wa_stb-ojtxp.
    wa_show-mtart = wa_stb-mtart.
    wa_show-mnglg = wa_stb-mnglg.
    wa_show-beskz = wa_stb-beskz.
    wa_show-sobsl = wa_stb-sobsl.

*    clear wa_show-idnst.    " 取组件库存
    clear wa_show-labst.    " 取组件库存
    read table it_stbmard into wa_stbmard with key matnr = wa_show-idnrk
                                                   werks = wa_show-werks
                                                   binary search.
    if sy-subrc = 0.
*      wa_show-idnst = wa_show-idnst + wa_stbmard-labst.
      wa_show-labst = wa_show-labst + wa_stbmard-labst.
    endif.

    append wa_show to it_show.
  endloop.
endform.
