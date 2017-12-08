*----------------------------------------------------------------------*
*  程序名称         : ZHRPA02
*  创建者           : 吴丽娟
*  创建日期         : 2015-08-21
*----------------------------------------------------------------------*
*  概要说明
* 上岗资格证
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-08-21  HANDYWLJ        ED1K902517    创建
*
*----------------------------------------------------------------------*
report zhrpa02.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
type-pools:slis.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
tables:pa9024,pa0000.
tables:pernr.
nodes:person,group,peras.

infotypes:0000 name p0000,
          0001 name p9001,
          9024 name p9024.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
types: begin of ty_tab,
         xh      type int_4,
         stext   type hrp1000-stext,   "职位
         plans   type pa0001-plans,
         pernr   type pa0000-pernr,    "工号
         ename   type pa0001-ename,    "姓名
         orgtx   type t527x-orgtx,     "最小部门
         zzzjmc  type p9024-zzzjmc,    "证件名称
         zzzjmc2 type p9024-zzzjmc,    "证件名称
         zzzjjb  type p9024-zzzjjb,    "证件级别
         begda   type p9024-begda,    "证件生效日期
         endda   type p9024-endda,    "证件失效日期
         zzhrbz  type p9024-zzhrbz,    "备注
         orgeh   type pa0001-orgeh,
         massn   type pa0000-massn,
       end of ty_tab.
types:begin of ty_pa9024,
        pernr  type pa9024-pernr,
        zzzjjb type p9024-zzzjjb,    "证件级别
        begda  type p9024-begda,    "证件生效日期
        endda  type p9024-endda,    "证件失效日期
        zzhrbz type p9024-zzhrbz,    "备注
        zzzjmc type p9024-zzzjmc,
        zzqtmc type pa9024-zzqtmc,
      end of ty_pa9024.

types: begin of ty_orgeh,
         orgeh type pa0001-orgeh,
       end of ty_orgeh.

types:begin of ty_orgtx,
        orgeh type t527x-orgeh,
        orgtx type t527x-orgtx,
      end of ty_orgtx.

types:begin of ty_plans,
        pernr type pa0000-pernr,
        plans type pa0001-plans,
      end of ty_plans.

types:begin of ty_stext,
        objid type hrp1000-objid,
        stext type hrp1000-stext,
      end of ty_stext.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
data:it_tab type table of ty_tab,
     wa_tab type ty_tab.

data:it_pa9024 type table of ty_pa9024,
     wa_pa9024 type ty_pa9024.

data:it_orgeh type table of ty_orgeh,
     wa_orgeh type ty_orgeh.

data:it_orgtx type table of ty_orgtx,
     wa_orgtx type ty_orgtx.

data:it_plans type table of ty_plans,
     wa_plans type ty_plans.

data:it_stext type table of ty_stext,
     wa_stext type ty_stext.

data:it_dd07t type table of dd07t,
     wa_dd07t type dd07t.

data:it_pa0001 type table of pa0001,
     wa_pa0001 type pa0001.

data:it_pa0000 type table of pa0000,
     wa_pa0000 type pa0000.

data:l_xh type int_4 value 1.
*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
data:it_fieldcat type lvc_t_fcat,
     wa_fieldcat like line of it_fieldcat,

     it_layout   type table of lvc_s_layo,
     wa_layout   type lvc_s_layo.
*----------------------------------------------------------------------*
*                  定义宏
*----------------------------------------------------------------------*
define init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
end-of-definition.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
selection-screen begin of block text with frame title text-001.
parameters:p_datum  type syst-datum obligatory.
select-options:s_zjlx for pa9024-zzzjmc no-extension no intervals.
*              s_pernr for pa0000-pernr.
selection-screen end of block text.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
initialization.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
start-of-selection.
  get peras .

  perform frm_getdata.
  perform frm_dealdata.

end-of-selection.
  perform frm_check.
  perform frm_layout.
  perform frm_fieldcat.
  perform frm_output.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_getdata .
*取证件名称
  select *
  into corresponding fields of table it_pa9024
  from pa9024
  where pernr = peras-pernr
  and pa9024~begda <= p_datum
  and pa9024~endda >= p_datum.

  sort it_pa9024 by zzzjmc.

  delete it_pa9024 where  zzzjmc  not in s_zjlx.

  if it_pa9024 is not initial.

*当发现离职状态删除员工编号
    select *
    from pa0000
    into corresponding fields of table it_pa0000
    for all entries in it_pa9024
    where pernr = it_pa9024-pernr.

    select * from pa0001
    into corresponding fields of table it_pa0001
    for all entries in it_pa9024
    where pernr = it_pa9024-pernr.

    if it_pa0001 is not initial.
      select objid stext
        into corresponding fields of table it_stext
        from hrp1000
        for all entries in it_pa0001
        where objid = it_pa0001-plans
      and stext <> '离职'.

*职位描述
      select orgeh orgtx
      into corresponding fields of table it_orgtx
      from t527x
      for all entries in it_orgeh
      where orgeh = it_orgeh-orgeh
      and   sprsl = sy-langu.
    endif.

*取出证件名称值对应描述
    select * from dd07t
    into corresponding fields of table it_dd07t
    where domname    = 'ZDOZJMC'
    and   ddlanguage = sy-langu.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_dealdata .

  sort it_pa9024  by pernr.
  sort it_pa0001  by pernr.
  sort it_pa0000  by pernr.
  loop at it_pa9024 into wa_pa9024.

    read table it_pa0000 into wa_pa0000
    with key pernr  = wa_pa9024-pernr
             massn  = 'Z6'
             binary search .
    if  sy-subrc = 0.
      continue.
    endif.

    if wa_pa9024-zzzjmc is initial.
      wa_tab-zzzjmc = wa_pa9024-zzqtmc.
    else.
      wa_tab-zzzjmc2 = wa_pa9024-zzzjmc.

      read table it_dd07t into wa_dd07t
      with key valpos = wa_tab-zzzjmc2.
      if sy-subrc = 0.
        wa_tab-zzzjmc = wa_dd07t-ddtext.
      endif.
    endif.

    wa_tab-pernr  = wa_pa9024-pernr.
    wa_tab-zzzjjb = wa_pa9024-zzzjjb.
    wa_tab-begda  = wa_pa9024-begda.
    wa_tab-endda  = wa_pa9024-endda.
    wa_tab-zzhrbz = wa_pa9024-zzhrbz.


*最小部门，姓名
    read table it_pa0001 into wa_pa0001
    with key pernr = wa_pa9024-pernr
             binary search.
    if  sy-subrc = 0.
      wa_tab-ename = wa_pa0001-ename.
      wa_tab-orgeh = wa_pa0001-orgeh.
      wa_tab-plans = wa_pa0001-plans.
      condense wa_tab-ename no-gaps.
    endif.

*最小部门描述
    read table it_orgtx into wa_orgtx
    with key  orgeh = wa_tab-orgeh.
    if sy-subrc = 0.
      wa_tab-orgtx = wa_orgtx-orgtx.
    endif.

    sort it_stext by objid.
    loop at it_stext into wa_stext.
      at end of objid.
        read table it_stext into wa_stext with key objid = wa_tab-plans.
        if sy-subrc = 0.
          if wa_stext-stext <> '离职' and wa_stext-objid = wa_tab-plans.
            wa_tab-stext = wa_stext-stext.
          endif.
        endif.
      endat.
    endloop.

    wa_tab-xh = l_xh.

    l_xh = l_xh + 1.
    append  wa_tab to it_tab.
    clear   wa_tab.
  endloop.
  sort it_tab by pernr.

  "取总条数
*  describe table it_tab lines g_select_num.
*  select_num_str = g_select_num.
*  condense select_num_str.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_layout .
  wa_layout-cwidth_opt = 'X'.
*  WA_LAYOUT-TOTALS_ONLY  = 'X'.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_fieldcat .
  init_fieldcat 'XH' '序号' '' ''.
  init_fieldcat 'PERNR' '工号' 'PA0000' 'PERNR'.
  init_fieldcat 'ENAME' '姓名' 'PA0000' 'ENAME'.
  init_fieldcat 'ORGTX' '最小部门' 'T527X' 'ORGTX'.
  init_fieldcat 'STEXT' '职位' 'HRP1000' 'STEXT'.
  init_fieldcat 'ZZZJMC' '证件名称' 'P9024' 'ZZZJMC'.
  init_fieldcat 'ZZZJJB' '证件级别' 'P9024' 'ZZZJJB'.
  init_fieldcat 'BEGDA' '证件生效日期' 'P9024' 'BEGDA'.
  init_fieldcat 'ENDDA' '证件失效日期' 'P9024' 'ENDDA'.
  init_fieldcat 'ZZHRBZ' '备注' 'P9024' 'ZZHRBZ'.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_output .
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
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
    tables
      t_outtab                 = it_tab
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
form alv_pf_status using rt_extab type slis_t_extab.
*  DATA lw_extab TYPE slis_extab.
*  lw_extab-fcode = '&ALL'.
*  APPEND lw_extab TO rt_extab.
*  lw_extab-fcode = '&SAL'.
*  APPEND lw_extab TO rt_extab.
  delete rt_extab where fcode = '&ALL'.
  delete rt_extab where fcode = '&SAL'.
*  set titlebar  'STANDARD_FULLSCREEN' with  '上岗资格证  共:'  select_num_str '条' .   "GUI标题.
  set pf-status 'STANDARD_FULLSCREEN' excluding rt_extab .
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

  data g_ref_grid type ref to cl_gui_alv_grid. "刷新行到内表


  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = g_ref_grid.

  case r_ucomm.
  endcase.



endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_check .
  if it_tab is initial.
    message '没有数据！' type 'I' display like 'E'.
    stop.
  endif.
endform.
