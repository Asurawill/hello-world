*----------------------------------------------------------------------*
* 程序名称：物料主数据验证报表
* 程序名： ZPP004
* 开发日期：2012/11/29
* 创建者：Jose Zhang
*----------------------------------------------------------------------*
*　概要: 物料主数据验证报表从验证物料的采购类型出发，判断在不同类型下
*       成本估算的要求，并以红绿灯显示。
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

report  zpp641
       no standard page heading
       line-size 255
       line-count 65.

*----------------------------------------------------------------------*
*       Include                                                        *
*----------------------------------------------------------------------*
*INCLUDE:zpp004_common.

*----------------------------------------------------------------------*
*       Type-pools                                                     *
*----------------------------------------------------------------------*
type-pools: slis.

*-----------------------------------------------------------------------
*&      Tables
*-----------------------------------------------------------------------
tables: marc, mara.

*----------------------------------------------------------------------*
*       Types
*----------------------------------------------------------------------*
types: begin of ty_output,
         status,
         werks   type marc-werks,
         matnr   type marc-matnr,
         maktx   type makt-maktx,
         kzkup   type marc-kzkup, "联产品
         bwtty   type mbew-bwtty, "评估类别
         stprs   type mbew-stprs, "标准价格
         beskz   type marc-beskz, "采购类型
         mmsta   type marc-mmsta, "物料状态
         sobsl   type marc-sobsl, "特殊采购类
         stlnr   type mast-stlnr, "物料单
         stlan   type t416-stlan, "BOM用途
         stlst   type stko-stlst,
         plnnr   type mapl-plnnr, "配方组
         verwe   type plko-verwe, "用途
         statu   type plko-statu, "状态
         message type string,
         bwkey   type mbew-bwkey, "评估范围
         mtart   type mara-mtart, "物料类型
         strgr   type marc-strgr,
         dismm   type marc-dismm,
         vprsv   type mbew-vprsv,   "价格控制符
         verpr   type mbew-verpr,   "移动平均价格
         ersda   type mara-ersda,   "物料创建日期
         matkl   type mara-matkl,
         mtpos   type mvke-mtpos,
         sbdkz   type marc-sbdkz,
         mstae   type mara-mstae,
       end of ty_output.

*----------------------------------------------------------------------*
*       Internal Tables and Work Areas
*----------------------------------------------------------------------*
data: it_output   type table of ty_output.
data: wa_output   type ty_output.


data: begin of it_mast_stko occurs 0,
        matnr type mast-matnr,
        werks type mast-werks,
        stlan type mast-stlan,
        stlnr type mast-stlnr,
        stlst type stko-stlst,
      end of it_mast_stko.
data: begin of it_mapl_plko occurs 0,
        matnr type mapl-matnr,
        werks type mapl-werks,
        plnnr type mapl-plnnr,
        verwe type plko-verwe,
        statu type plko-statu,
      end of it_mapl_plko.
data: begin of it_eine occurs 0,
        matnr type mapl-matnr,
        effpr type eine-effpr,
        infnr type eine-infnr,
        esokz type eine-esokz,
      end of it_eine.

data: it_mvke type table of mvke,
      wa_mvke type mvke.

*----------------------------------------------------------------------*
*       ALV层级关系定义
*----------------------------------------------------------------------*
data: it_fieldcat type slis_t_fieldcat_alv,
      wa_layout   type slis_layout_alv,
      g_repid     type sy-repid,
      g_step5     type c,
      g_step9     type c.
data: wa_fieldcat   like line of it_fieldcat.

*&---------------------------------------------------------------------*
*&      Define marco
*&---------------------------------------------------------------------*
define macro_fill_fcat.
  clear wa_fieldcat.
  &1 = &1 + 1.
  wa_fieldcat-col_pos   = &1.
  wa_fieldcat-fieldname = &2.
  wa_fieldcat-seltext_l = &3.
  wa_fieldcat-no_zero   = &4.
  wa_fieldcat-key       = &5.
  wa_fieldcat-ref_tabname = &6.
  wa_fieldcat-ref_fieldname = &7.   " 内表中数量参照字段
  append wa_fieldcat to it_fieldcat.
end-of-definition.

*----------------------------------------------------------------------*
*       Selection-screen                                               *
*----------------------------------------------------------------------*
selection-screen begin of block blk with frame title text-001.
select-options s_werks for marc-werks obligatory
                                      no-extension
                                      no intervals.
select-options s_matnr for marc-matnr.
select-options s_beskz for marc-beskz no-extension
                                      no intervals.
select-options: s_mtart for mara-mtart no intervals,
                s_ersda for mara-ersda.
selection-screen end of block blk.

*&---------------------------------------------------------------------*
*&      INITIALIZATION.
*&---------------------------------------------------------------------*
initialization.
*通常在此事件块中设定输入屏幕字段的初始值
  g_repid = sy-repid.

*&---------------------------------------------------------------------*
*&      AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
at selection-screen.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
start-of-selection.
*取数
  perform frm_get_data.
  perform frm_check_bom.

*&---------------------------------------------------------------------*
*&      END-OF-SELECTION
*&---------------------------------------------------------------------*
end-of-selection.
*报表输出
  perform frm_init_layout.
  perform frm_init_fieldcat.
  perform frm_alv_output.

*&---------------------------------------------------------------------*
*&      Form  frm_get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_get_data.

  select marc~werks
         marc~matnr
         makt~maktx
         marc~beskz "采购类型
         marc~sobsl "特殊采购类型
         marc~kzkup "联产品
         marc~strgr
         marc~dismm
         t001w~bwkey "评估范围
         mara~mtart "物料类型
         mara~ersda
         mara~matkl
         marc~sbdkz
         marc~mmsta
         mara~mstae
    from marc
    join makt on marc~matnr = makt~matnr and makt~spras = sy-langu
    join t001w on marc~werks = t001w~werks
    join mara on marc~matnr = mara~matnr
    into corresponding fields of table it_output
   where marc~werks in s_werks
     and marc~matnr in s_matnr
     and marc~beskz in s_beskz
     and mara~mtart in s_mtart
     and mara~ersda in s_ersda.

  if sy-subrc <> 0.
    message i001(00) with '没有符合查询条件的数据!' display like 'E'.
    stop.
  endif.

*判断BOM
  select mast~matnr
         mast~werks
         mast~stlan "BOM 用途
         mast~stlnr "物料单
         stko~stlst "有效起始日期
    from mast
    join stko on mast~stlnr = stko~stlnr and mast~stlal = stko~stlal
    into corresponding fields of table it_mast_stko
     for all entries in it_output
   where mast~werks = it_output-werks
     and mast~matnr = it_output-matnr
     and mast~stlan = '1'.

*--工艺路线读取
  select mapl~matnr
         mapl~werks
         mapl~plnnr "任务清单组码
*         mapl~datuv "有效起始日期
         plko~verwe "用途
         plko~statu "状态
    from mapl
    join plko on mapl~plnty = plko~plnty and mapl~plnnr = plko~plnnr and mapl~plnal = plko~plnal
    into corresponding fields of table it_mapl_plko
     for all entries in it_output
   where mapl~werks = it_output-werks
     and mapl~matnr = it_output-matnr
     and mapl~loekz = space                       "删除标识 为空
     and plko~verwe = '1'                                   "用途 为1
     and ( plko~statu = '3' or plko~statu = '4' )           "状态 为4或3
       .

  data:lt_mbew type table of mbew with header line.
  select *
    from mbew
    into table lt_mbew
    for all entries in it_output
    where matnr = it_output-matnr
      and bwkey = it_output-bwkey.

  "-- 增加判断是否有采购报价 add by cmh 20140328  ------
  select *
  from eina inner join eine on eina~infnr = eine~infnr
  into corresponding fields of table it_eine
    for all entries in it_output
  where eine~loekz = space
    and eine~effpr > 0
    and matnr = it_output-matnr
    .

  sort it_eine by infnr matnr.
  delete adjacent duplicates from it_eine comparing infnr matnr.

  select *
    into table it_mvke
    from mvke
    for all entries in it_output
    where matnr = it_output-matnr
      and vkorg = it_output-werks
      and vtweg = '00'.
  sort it_mvke by matnr vkorg vtweg.

  loop at it_output into wa_output.
    read table it_mast_stko with key matnr = wa_output-matnr werks = wa_output-werks.
    if sy-subrc = 0.
      wa_output-stlan = it_mast_stko-stlan.
      wa_output-stlnr = it_mast_stko-stlnr.
      wa_output-stlst = it_mast_stko-stlst.
    endif.

    read table it_mapl_plko with key matnr = wa_output-matnr werks = wa_output-werks.
    if sy-subrc = 0.
      move-corresponding it_mapl_plko to wa_output.
    endif.

    read table lt_mbew with key matnr = wa_output-matnr bwkey = wa_output-bwkey.
    if sy-subrc = 0.
      wa_output-bwtty = lt_mbew-bwtty.
      wa_output-stprs = lt_mbew-stprs.   "标准价格
      wa_output-verpr = lt_mbew-verpr.   "移动平均价格
      wa_output-vprsv = lt_mbew-vprsv.   "价格控制符
    endif.

    read table it_mvke into wa_mvke with key matnr = wa_output-matnr
                                             vkorg = wa_output-werks
                                             binary search.
    if sy-subrc = 0.
      wa_output-mtpos = wa_mvke-mtpos.
    endif.

    modify it_output from wa_output.
  endloop.
endform.                    "frm_get_data

*&---------------------------------------------------------------------*
*&      Form  frm_check_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_check_bom. "
*错1：如果物料类型为ZFRT或者ZHLB,检查BOM:逻辑：MRP类型为PD时，检查若没有用途为1的物料单，则报错红灯：BOM出错。
*错2：如果物料类型为ZFRT或者ZHLB,检查工艺路线:逻辑：MRP类型为PD时，检查若没有工艺路线，则报错红灯：工艺路线出错。
*错3：检查策略组为40，标准价有值，且物料状态为Z1的时候，报错：请激活物料状态
*错4：检查策略组为40，标准价无值，且物料状态为Z2的时候，报错：请关闭物料状态
*错5: MRP类型为PD时,原材料没有价格 ,主数据里没有价格 ，再判断信息记录

  loop at it_output into wa_output.
*    if ( wa_output-mtart = 'ZFRT' or wa_output-mtart = 'ZHLB' ) and wa_output-dismm = 'PD' and wa_output-stlst <> '1'.
    if ( wa_output-mtart = 'ZFRT' or wa_output-mtart = 'ZHLB' ) and wa_output-stlst <> '1' and wa_output-stlst <> '2'.
      wa_output-status = 1.
      wa_output-message = 'BOM出错'.

      modify it_output from wa_output.
      continue.
    endif.

*    if ( wa_output-mtart = 'ZFRT' or wa_output-mtart = 'ZHLB' ) and wa_output-dismm = 'PD' and wa_output-plnnr is initial.
    if ( wa_output-mtart = 'ZFRT' or wa_output-mtart = 'ZHLB' )  and wa_output-sobsl ne '50' and wa_output-sobsl ne '30' and wa_output-plnnr is initial.
      wa_output-status = 1.
      wa_output-message = '工艺路线出错'.
      modify it_output from wa_output.
      continue.
    endif.

    if wa_output-strgr = '40' and wa_output-stprs is not initial and wa_output-mmsta = 'Z1'.
      wa_output-status = 1.
      wa_output-message = '请激活物料状态'.
      modify it_output from wa_output.
      continue.
    endif.

    if wa_output-strgr = '40' and wa_output-stprs is initial and wa_output-mmsta = 'Z2'.
      wa_output-status = 1.
      wa_output-message = '请关闭物料状态'.
      modify it_output from wa_output.
      continue.
    endif.

*    if wa_output-mtart = 'ZROH' and wa_output-dismm = 'PD'.  "-- 主数据里没有价格 ，再判断信息记录
    if wa_output-mtart = 'ZROH'.  "-- 主数据里没有价格 ，再判断信息记录
      if ( wa_output-vprsv = 'V' and wa_output-verpr = 0 ) or ( wa_output-vprsv = 'S' and wa_output-stprs = 0 ).
        read table it_eine with key matnr = wa_output-matnr.
        if sy-subrc <> 0.
          wa_output-status = 1.
          wa_output-message = '原材料没有价格'.
          modify it_output from wa_output.
          continue.
        endif.
      endif.
    endif.

    wa_output-status = 3.
    wa_output-message = 'ok'.
    modify it_output from wa_output.
  endloop.
endform.                    "frm_check_bom

*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_init_layout .

  wa_layout-zebra             = 'X'.
  wa_layout-f2code            = '&ETA'.     "双击显示详细信息
  wa_layout-detail_popup      = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  wa_layout-lights_fieldname  = 'STATUS'.   "红绿灯

endform.                    " FRM_INIT_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_init_fieldcat .

  data: l_colpos type lvc_s_fcat-col_pos value 0.

  check it_fieldcat is initial.
  macro_fill_fcat:
    l_colpos 'MESSAGE' '错误信息' ' ' 'X' ' ' ' ',
    l_colpos 'WERKS'  '工厂' ' ' 'X' ' ' ' ',
    l_colpos 'MATNR'  '物料编码' 'X' 'X' ' ' ' ',
    l_colpos 'MAKTX'  '物料描述' ' ' 'X' ' ' ' ',
    l_colpos 'MTART'  '物料类型' ' ' ' ' ' ' ' ',
    l_colpos 'BESKZ'  '采购类型' ' ' ' ' ' ' ' ',
    l_colpos 'SOBSL'  '特殊采购类' ' ' ' ' ' ' ' ',
*    l_colpos 'KZKUP'  text-010 text-010 text-010 ' ' ' ' ' ' ' ',
    l_colpos 'STLNR'  '物料单' ' ' ' ' ' ' ' ',
    l_colpos 'STLAN'  'BOM用途' ' ' ' ' ' ' ' ',
    l_colpos 'STLST'  'BOM状态' ' ' ' ' ' ' ' ',
    l_colpos 'STPRS'  '标准价' ' ' ' ' ' ' ' ',
    l_colpos 'VERPR'  '移动平均价格' ' ' ' ' ' ' ' ',
    l_colpos 'VPRSV'  '价格类型' ' ' ' ' ' ' ' ',
    l_colpos 'MMSTA'  '物料状态' ' ' ' ' ' ' ' ',
*    l_colpos 'MSTAE'  '物料状态' ' ' ' ' ' ' ' ',
    l_colpos 'PLNNR'  '工艺路线' ' ' ' ' ' ' ' ' ,
    l_colpos 'STATU'  '状态' ' ' ' ' ' ' ' ',
    l_colpos 'VERWE'  '用途' ' ' ' ' ' ' ' ',
*    l_colpos 'DATV1'  text-019 text-019 text-019 ' ' ' ' ' ' ' ',
*    l_colpos 'DATB1'  text-020 text-020 text-020 ' ' ' ' ' ' ' ',
    l_colpos 'STRGR'  '策略组' ' ' ' ' ' ' ' ',
    l_colpos 'ERSDA'  '物料创建日期' ' ' ' ' ' ' ' ',
    l_colpos 'DISMM'  'MRP类型' ' ' ' ' ' ' ' ',
    l_colpos 'MATKL'  '物料组' ' ' ' ' 'MARA' 'MATKL',
    l_colpos 'MTPOS'  '项目类别组' ' ' ' ' 'MVKE' 'MTPOS',
    l_colpos 'SBDKZ'  '独立/集中' ' ' ' ' ' ' ' '.

endform.                    " FRM_INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_alv_output .
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = g_repid
      is_layout          = wa_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    tables
      t_outtab           = it_output
    exceptions
      program_error      = 1
      others             = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " FRM_ALV_OUTPUT

end-of-page.

*Text elements
*----------------------------------------------------------
* 001 查询条件
* 002 联产品BOM有问题
* 003 主产品分配结构有错误
* 004 BOM出错
* 005 主配方出错
* 006 价格维护有错误
* 007 工厂
* 008 物料编码
* 009 物料描述
* 010 联产品
* 011 评估类别
* 012 采购类型
* 013 特殊采购类
* 014 税价1
* 015 税价2
* 016 税价3
* 017 物料单
* 018 BOM用途
* 019 有效起始日
* 020 截至日
* 021 配方组
* 022 用途
* 023 错误信息
* 024 请扩充财务视图
* 025 状态
* 026 物料类型
