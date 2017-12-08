*&---------------------------------------------------------------------*
*& Report  ZPP010
*&
*&---------------------------------------------------------------------*
*&
*&  展开式物料供需平衡表
*&---------------------------------------------------------------------*

report zpp010bk.

type-pools:slis,vrm,icon.

tables: mara,marc,syst,vbap,afko.

types:begin of ty_tab,
        werks        type marc-werks,
        matnr        type marc-matnr,
        plifz        type marc-plifz,
        maktx        type makt-maktx,
        zgxlx(2)     type c, " 供需类型
        zgxlx_ms(10) type c,
        mngkc        type mdez-mng01, " 库存数量
        plumi        type mdez-plumi, " 收货/发货标识
        dat00        type mdez-dat00,
        mng01        type mdez-mng01,
        delkz        type mdez-delkz,

      end of ty_tab.

data:gt_tab type standard table of ty_tab with header line.
data:gt_tab_b type standard table of ty_tab with header line.
*DATA:gt_tab_tmp TYPE STANDARD TABLE OF ty_tab WITH HEADER LINE.

data:gt_tab02 type standard table of ty_tab with header line.

data:gt_marc type standard table of marc with header line.

data: it_table     type ref to data,
      it_structure type        lvc_t_fcat,
      wa_structure type        lvc_s_fcat.

data: wa_fieldcat type slis_fieldcat_alv,
      it_fieldcat type slis_t_fieldcat_alv,
      wa_layout   type lvc_s_layo.

field-symbols:<dyn_table> type standard table,
              <dyn_wa>,
              <dyn_field>.

selection-screen begin of block blk with frame title text-001.
select-options:
s_werks for marc-werks no-extension no intervals obligatory ,  "工厂
s_matnr for mara-matnr,
s_vbeln for vbap-vbeln,
s_posnr for vbap-posnr,
s_aufnr for afko-aufnr," 生产订单
s_budat for syst-datum obligatory no-extension no intervals default sy-datum.
selection-screen end of block blk.

at selection-screen .
  if s_matnr-low = '*'.
    message '请输入合适的选择条件,查询物料太多，将超过系统负荷！' type 'E'.
  endif.

  if s_matnr[] is initial and  s_vbeln[] is initial and s_aufnr[] is initial .
    message '请输入合适的选择条件,查询物料太多，将超过系统负荷！' type 'E'.
  endif.

start-of-selection.
  perform frm_get_data.
  perform frm_create_structure.  " 定义内表的结构
  perform create_dynamic_table.  " 按照定义的内表结构，产生一个内表
  perform write_data_to_dyntable.  " 向动态内表中写数

end-of-selection.

  perform frm_init_layout .
  perform frm_show_alv.   " 从动态内表中取数，并写到屏幕


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_get_data .

  data:lt_mdezx type standard table of mdez with header line.
  data:lt_marc type standard table of marc with header line.
  clear:lt_marc,lt_marc[].

  data:lt_marc_bom type standard table of marc with header line.
  clear:lt_marc_bom,lt_marc_bom[].

  data: i_bom like stpox occurs 0 with header line.
  data:lw_cstmat like cstmat.

*按工厂和物料取得数据

  " 销售订单和入生产订单同时为空时
  if s_aufnr[] is initial and s_vbeln[] is initial.
    select werks
      matnr
      plifz
      from marc
      into corresponding fields of table lt_marc
      where matnr in s_matnr
      and werks in s_werks.

    loop at lt_marc.
      call function 'CS_BOM_EXPL_MAT_V2' " 展开BOM
        exporting
          capid                 = 'PP01'
          datuv                 = sy-datum " 有效起始时间
          emeng                 = 1 " Required quantity
          mktls                 = 'X'
          mehrs                 = 'X' "展开至最底层的参数
          mtnrv                 = lt_marc-matnr " 物料号
          stlal                 = '01'  "Alternative BOM
          stlan                 = '1'  " BOM用途
          stpst                 = 0
          svwvo                 = 'X'
          werks                 = lt_marc-werks " 工厂
          vrsvo                 = 'X'
        importing
          topmat                = lw_cstmat
        tables
          stb                   = i_bom
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

      loop at i_bom.
        lt_marc_bom-matnr = i_bom-idnrk.
        lt_marc_bom-werks = i_bom-werks.
        append lt_marc_bom.
        clear lt_marc_bom.
      endloop.

    endloop.

  endif.

*删除重复项目
  sort lt_marc_bom by werks matnr.
  delete adjacent duplicates from lt_marc_bom comparing werks matnr.

  append lines of lt_marc_bom to lt_marc.

*按生产订单取得数据
  data:lt_resb type standard table of resb with header line.
  data:lt_afpo type standard table of afpo with header line.
  data:lt_aufk type standard table of aufk with header line.

  " 输入生产订单时
  if s_aufnr[] is not initial.

    select rsnum
      rspos
      matnr
      werks
      aufnr
      from resb
      into corresponding fields of table lt_resb
      where aufnr in s_aufnr
      and xloek <> 'X' " 已删除项目
      and dumps <> 'X'. " 虚拟项目标识

    select aufnr
        posnr
        matnr
      from afpo
      into corresponding fields of table lt_afpo
      where aufnr in s_aufnr.

    select aufnr
       werks
      from aufk
      into corresponding fields of table lt_aufk
      where  aufnr in s_aufnr.

    " 生产订单抬头物料
    loop at lt_afpo.
      if lt_afpo-matnr is not initial.
        lt_marc-matnr = lt_afpo-matnr. " 物料
      endif.

      read table lt_aufk with key aufnr = lt_afpo-aufnr.
      if sy-subrc = 0.
        lt_marc-werks = lt_aufk-werks. " 工厂
      endif.
      append lt_marc.
      clear lt_marc.

    endloop.

    " 生产订单组件物料
    loop at lt_resb.
      lt_marc-matnr = lt_resb-matnr. " 物料
      lt_marc-werks = lt_resb-werks. " 工厂
      append lt_marc.
      clear lt_marc.
    endloop.

  endif.

*按销售订单取得数据
  data:lt_stb like standard table of stpox with header line .
  data:lt_kdst type standard table of kdst with header line.

  " 输入销售订单时
  if s_vbeln[] is not initial.
    select
      vbeln
      vbpos
      matnr
      werks
      from kdst
      into corresponding fields of table lt_kdst
      where matnr in s_matnr
      and vbeln in s_vbeln
      and vbpos in s_posnr
      and werks in s_werks.

*取得KDST销售订单抬头物料
    loop at  lt_kdst.
      lt_marc-matnr = lt_kdst-matnr. " 物料
      lt_marc-werks = lt_kdst-werks. " 工厂
      append lt_marc.
      clear lt_marc.
    endloop.

    " 取得销售订单
    loop at lt_kdst.
      clear:lt_stb,lt_stb[].
      call function 'CS_BOM_EXPL_KND_V1'
        exporting
          capid                 = 'PP01'
          stlan                 = '1'
          cospr                 = 'X' " Internal: (CO) order-spec. MatPreRead
*         cuobj                 = l_cuobj
          cuols                 = 'X'  " Merkmalsbewertung zur Position einlesen
          datuv                 = sy-datum " 生效日期
          ehndl                 = '2'
          emeng                 = '1' " Required quantity
          mehrs                 = 'X' " 是否全部展开，X表示全部展开
          mtnrv                 = lt_kdst-matnr " 物料
          mmory                 = '0'
          rndkz                 = '1'
          werks                 = s_werks-low " 工厂
          vbeln                 = lt_kdst-vbeln " 销售订单
          vbpos                 = lt_kdst-vbpos " 销售行项目
          stpst                 = 99
*MPORTING
*         topmat                = gs_topmat
        tables
          stb                   = lt_stb
*         MATCAT                = STR_MATCAT_OUT[]
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

      loop at lt_stb where bmtyp = 'K'.
        lt_marc-matnr = lt_stb-idnrk. " 物料
        lt_marc-werks = lt_stb-werks. " 工厂
        append lt_marc.
        clear lt_marc.
      endloop.

    endloop.

  endif.

* MD04，取得库存数量
  loop at lt_marc.

    clear:lt_mdezx,lt_mdezx[].
    call function 'MD_STOCK_REQUIREMENTS_LIST_API'
      exporting
*       PLSCN                    =
        matnr                    = lt_marc-matnr
        werks                    = lt_marc-werks
*       BERID                    =
*       ERGBZ                    =
*       AFIBZ                    =
*       INPER                    =
*       DISPLAY_LIST_MDPSX       =
*       DISPLAY_LIST_MDEZX       =
*       DISPLAY_LIST_MDSUX       =
*       NOBUF                    =
*       PLAUF                    =
*       I_VRFWE                  =
*       IS_SFILT                 =
*       IS_AFILT                 =
*   IMPORTING
*       E_MT61D                  =
*       E_MDKP                   =
*       E_CM61M                  =
*       E_MDSTA                  =
*       E_ERGBZ                  =
      tables
*       MDPSX                    =
        mdezx                    = lt_mdezx[]
*       MDSUX                    =
      exceptions
        material_plant_not_found = 1
        plant_not_found          = 2
        others                   = 3.

    clear:gt_tab.

    loop at lt_mdezx.
      gt_tab-matnr  = lt_marc-matnr.
      gt_tab-werks  = lt_marc-werks.
      gt_tab-plifz  = lt_marc-plifz.
      gt_tab-plumi = lt_mdezx-plumi. " 收货/发货标识
      gt_tab-dat00 = lt_mdezx-dat00. " 收货/需求日期
      gt_tab-mng01 = lt_mdezx-mng01. " 收货数量或需求数量
      collect gt_tab .
      clear gt_tab.
    endloop.

    " 取得工厂库存
    loop at gt_tab where matnr  = lt_marc-matnr and werks  = lt_marc-werks..
      loop at lt_mdezx.
        if lt_mdezx-plumi = 'B' .
          ""  排除不参与MRP运算库存地点库存 ，排除安全库存
          if  lt_mdezx-delkz <> 'LB' and  lt_mdezx-delkz <> 'SH' .
            gt_tab-mngkc = lt_mdezx-mng01 + gt_tab-mngkc.
          endif.
        endif.
      endloop.

      modify gt_tab.
      clear gt_tab.
    endloop.

  endloop.

  gt_marc[] = lt_marc[]. " 全局

endform.                    " FRM_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_create_structure .
  data:l_low  type mkpf-budat,
       l_high type mkpf-budat.

  data: l_result type string,
        l_line   type i,
        l_mid    type string.

  data: l_str1 type string,
        l_str2 type string,
        l_str3 type string.


  data: l_lines type i.

  read table s_budat index 1.
  l_low = s_budat-low.
  l_high = s_budat-low + 14. " 查询日期起，自动加14天

  if l_high is not initial.
    l_line = l_high - l_low + 1.
  else.
    l_line = 1.
  endif.


  l_lines = 7.
* 创建静态结构部分
  perform frm_alv_fcat_set using:  1   'WERKS' text-010 'WERKS'  'MARC'  space   '' space   space,
                                   2   'MATNR' text-011 'MATNR'  'MAKT'  space   '' space   space,
                                   3   'MAKTX' text-012 'MAKTX'  'MAKT'  space   '' space   space,
                                   4   'ZGXLX_MS'  '供需类型' ''  ''  space   '' space   space,
                                   6   'MNGKC'  '工厂库存' ''  ''  space   '18' 'QUAN'    'X',
                                    5   'PLIFZ'  '采购L/T' ''  ''  space   space space   space.

  " 按天显示列
  do l_line times.
    clear wa_structure.
    " 字段名称
    concatenate 'DYNAMIC_MENGE' l_low  into wa_structure-fieldname.
    " 列名称
    l_str1 = l_low+0(4).
    l_str2 = l_low+4(2).
    l_str3 = l_low+6(2).
    concatenate l_str1 l_str2 l_str3 into wa_structure-scrtext_l
    separated by '/'.
    wa_structure-scrtext_m = wa_structure-scrtext_l.
    wa_structure-scrtext_s = wa_structure-scrtext_l.
*    wa_structure-ref_field  =  'MNG01'.
*    wa_structure-ref_table  =  'MDEZ'.
    wa_structure-outputlen = 18.

    wa_structure-datatype = 'QUAN'.
    wa_structure-no_zero = 'X'.

    wa_structure-col_pos    = l_lines.
    append wa_structure to it_structure.

    add 1 to l_low.
    clear:l_str1,l_str2,l_str3.
    l_lines = l_lines + 1.
  enddo.

  " 按周显示列
  l_lines = 21.
  do 16 times.
    clear wa_structure.
    concatenate 'DYNAMIC_MENGE' l_low into wa_structure-fieldname.
    " 列名称
    l_str1 = l_low+0(4).
    l_str2 = l_low+4(2).
    l_str3 = l_low+6(2).
    concatenate l_str1 l_str2 l_str3 into wa_structure-scrtext_l
    separated by '/'.
    wa_structure-scrtext_m = wa_structure-scrtext_l.
    wa_structure-scrtext_s = wa_structure-scrtext_l.
*    wa_structure-ref_field  =  'MNG01'.
*    wa_structure-ref_table  =  'MDEZ'.
    wa_structure-outputlen = 18.

    wa_structure-datatype = 'QUAN'.
    wa_structure-no_zero = 'X'.

    wa_structure-col_pos    = l_lines.
    append wa_structure to it_structure.
    add 7 to l_low.
    clear:l_str1,l_str2,l_str3.
    l_lines = l_lines + 1.
  enddo.

endform.                    " FRM_CREATE_STRUCTURE

*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_dynamic_table .
  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog = it_structure
    importing
      ep_table        = it_table.

  assign it_table->* to <dyn_table>.
endform.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA_TO_DYNTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_data_to_dyntable .


  data:wa_new_line type ref to data.
  create data wa_new_line like line of <dyn_table>.
  assign wa_new_line->* to <dyn_wa>.

  data:l_fieldname type lvc_s_fcat-fieldname.
  data:date1 type syst-datum.
  data:date2 type syst-datum.
  data:date3 type syst-datum.

  data:lt_makt type standard table of makt with header line.
  clear:lt_makt,lt_makt[].

  " 取得B库存数据
  loop at gt_tab.
    if gt_tab-plumi = 'B'.
      move-corresponding gt_tab to gt_tab_b.
      append gt_tab_b.
      clear gt_tab_b.
    endif.
  endloop.

*删除库存数据，只保留需求数量和供给数量
  delete gt_tab where  plumi = 'B'.

*日期出来
  " 列开始日期
  date1 = s_budat-low.
  " 按日期最后一天
  date2 = s_budat-low + 14.
  " 按周最后一周
  date3 = date2 + 112. " 16周*7天

*处理数据，取得查询日期之间的数据
  delete gt_tab where dat00 < date1.
  delete gt_tab where dat00 > date3.

  data:l_mng_1 type mdez-mng01. "　第一周累计数量
  data:l_mng_2 type mdez-mng01."　第二周累计数量
  data:l_mng_3 type mdez-mng01."　第三周累计数量
  data:l_mng_4 type mdez-mng01."　第四周累计数量
  data:l_mng_5 type mdez-mng01."　第五周累计数量
  data:l_mng_6 type mdez-mng01. " 第6周累计数量
  data:l_mng_7 type mdez-mng01. "　第7周累计数量
  data:l_mng_8 type mdez-mng01. "　第8周累计数量
  data:l_mng_9 type mdez-mng01. "　第9周累计数量
  data:l_mng_10 type mdez-mng01. "　第10周累计数量
  data:l_mng_11 type mdez-mng01. "　第11周累计数量
  data:l_mng_12 type mdez-mng01."　第12周累计数量
  data:l_mng_13 type mdez-mng01."　第13周累计数量
  data:l_mng_14 type mdez-mng01."　第14周累计数量
  data:l_mng_15 type mdez-mng01."　第15周累计数量
  data:l_mng_16 type mdez-mng01."　第16周累计数量

  data:date_1 type syst-datum. " 第一周
  data:date_2 type syst-datum."　第二周
  data:date_3 type syst-datum." 第三周
  data:date_4 type syst-datum. " 第四周
  data:date_5 type syst-datum. " 第五周
  data:date_6 type syst-datum. " 第6周
  data:date_7 type syst-datum. " 第7周
  data:date_8 type syst-datum. " 第8周
  data:date_9 type syst-datum. " 第9周
  data:date_10 type syst-datum. " 第10周
  data:date_11 type syst-datum. " 第11周
  data:date_12 type syst-datum. " 第12周
  data:date_13 type syst-datum. " 第13周
  data:date_14 type syst-datum. " 第14周
  data:date_15 type syst-datum. " 第15周
  data:date_16 type syst-datum. " 第16周

  date_1  = date2 + 1.
  date_2 = date_1 + 7.
  date_3 = date_2 + 7.
  date_4 = date_3 + 7.
  date_5 = date_4 + 7.
  date_6 = date_5 + 7.
  date_7 = date_6 + 7.
  date_8 = date_7 + 7.
  date_9 = date_8 + 7.
  date_10 = date_9 + 7.
  date_11 = date_10 + 7.
  date_12 = date_11 + 7.
  date_13 = date_12 + 7.
  date_14 = date_13 + 7.
  date_15 = date_14 + 7.
  date_16 = date_15 + 7.

  if gt_marc[] is not initial.
    select *
      from makt
      into corresponding fields of table lt_makt
      for all entries in gt_marc
      where matnr = gt_marc-matnr
      and spras = sy-langu.
  endif.

  sort gt_marc by matnr.
  loop at gt_marc.
*需求数量
    clear:
    l_mng_1,
    l_mng_2,
    l_mng_3,
    l_mng_4,
    l_mng_5,
    l_mng_6,
    l_mng_7,
    l_mng_8,
    l_mng_9,
    l_mng_10,
    l_mng_11,
    l_mng_12,
    l_mng_13,
    l_mng_14,
    l_mng_15,
    l_mng_16.

    clear l_fieldname.
    loop at gt_tab where plumi = '-' and matnr = gt_marc-matnr and werks = gt_marc-werks.

      if gt_tab-dat00 between date1 and date2." 按天
        concatenate 'DYNAMIC_MENGE' gt_tab-dat00 into l_fieldname.
        assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
        if gt_tab-mng01 is not initial.
          <dyn_field> = gt_tab-mng01.
        endif.
      endif.

      if gt_tab-dat00 > date2 and gt_tab-dat00 <= date_1. " 按1周
        l_mng_1 = l_mng_1 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_1 and gt_tab-dat00 <= date_2 . " 按2周
        l_mng_2 = l_mng_2 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_2 and gt_tab-dat00 <= date_3 . " 按3周
        l_mng_3 = l_mng_3 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_3 and gt_tab-dat00 <= date_4 . " 按4周
        l_mng_4 = l_mng_4 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_4 and gt_tab-dat00 <= date_5 . " 按5周
        l_mng_5 = l_mng_5 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_5 and gt_tab-dat00 <= date_6 . " 按6周
        l_mng_6 = l_mng_6 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_6 and gt_tab-dat00 <= date_7 . " 按7周
        l_mng_7 = l_mng_7 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_7 and gt_tab-dat00 <= date_8 . " 按8周
        l_mng_8 = l_mng_8 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_8 and gt_tab-dat00 <= date_9 . " 按9周
        l_mng_9 = l_mng_9 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_9 and gt_tab-dat00 <= date_10 . " 按10周
        l_mng_10 = l_mng_10 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_10 and gt_tab-dat00 <= date_11 . " 按11周
        l_mng_11 = l_mng_11 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_11 and gt_tab-dat00 <= date_12 . " 按12周
        l_mng_12 = l_mng_12 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_12 and gt_tab-dat00 <= date_13 . " 按13周
        l_mng_13 = l_mng_13 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_13 and gt_tab-dat00 <= date_14 . " 按14周
        l_mng_14 = l_mng_14 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_14 and gt_tab-dat00 <= date_15 . " 按15周
        l_mng_15 = l_mng_15 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_15 and gt_tab-dat00 <= date_16 . " 按16周
        l_mng_16 = l_mng_16 + gt_tab-mng01.
      endif.

    endloop.

    concatenate 'DYNAMIC_MENGE' date_1 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_1 is not initial.
      <dyn_field> = l_mng_1.
    endif.

    concatenate 'DYNAMIC_MENGE' date_2 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_2 is not initial.
      <dyn_field> = l_mng_2.
    endif.

    concatenate 'DYNAMIC_MENGE' date_3 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_3 is not initial.
      <dyn_field> = l_mng_3.
    endif.

    concatenate 'DYNAMIC_MENGE' date_4 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_4 is not initial.
      <dyn_field> = l_mng_4.
    endif.

    concatenate 'DYNAMIC_MENGE' date_5 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_5 is not initial.
      <dyn_field> = l_mng_5.
    endif.

    concatenate 'DYNAMIC_MENGE' date_6 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_6 is not initial.
      <dyn_field> = l_mng_6.
    endif.

    concatenate 'DYNAMIC_MENGE' date_7 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_7 is not initial.
      <dyn_field> = l_mng_7.
    endif.

    concatenate 'DYNAMIC_MENGE' date_8 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_8 is not initial.
      <dyn_field> = l_mng_8.
    endif.

    concatenate 'DYNAMIC_MENGE' date_9 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_9 is not initial.
      <dyn_field> = l_mng_9.
    endif.

    concatenate 'DYNAMIC_MENGE' date_10 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_10 is not initial.
      <dyn_field> = l_mng_10.
    endif.

    concatenate 'DYNAMIC_MENGE' date_11 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_11 is not initial.
      <dyn_field> = l_mng_11.
    endif.

    concatenate 'DYNAMIC_MENGE' date_12 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_12 is not initial.
      <dyn_field> = l_mng_12.
    endif.

    concatenate 'DYNAMIC_MENGE' date_13 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_13 is not initial.
      <dyn_field> = l_mng_13.
    endif.

    concatenate 'DYNAMIC_MENGE' date_14 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_14 is not initial.
      <dyn_field> = l_mng_14.
    endif.

    concatenate 'DYNAMIC_MENGE' date_15 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_15 is not initial.
      <dyn_field> = l_mng_15.
    endif.

    concatenate 'DYNAMIC_MENGE' date_16 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_16 is not initial.
      <dyn_field> = l_mng_16.
    endif.

    assign component 'MATNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-matnr.

    assign component 'WERKS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-werks.

    assign component 'PLIFZ' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-plifz.

    assign component 'ZGXLX_MS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = '需求数量'.

    assign component 'MNGKC' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> =  0.

    read table lt_makt with key matnr = gt_marc-matnr.
    if sy-subrc = 0.
      assign component 'MAKTX' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  lt_makt-maktx.
    endif.

    append <dyn_wa> to <dyn_table>.
    clear <dyn_wa>.

*供给数量
*需求数量
    clear:
    l_mng_1,
    l_mng_2,
    l_mng_3,
    l_mng_4,
    l_mng_5,
    l_mng_6,
    l_mng_7,
    l_mng_8,
    l_mng_9,
    l_mng_10,
    l_mng_11,
    l_mng_12,
    l_mng_13,
    l_mng_14,
    l_mng_15,
    l_mng_16.

    clear l_fieldname.
    loop at gt_tab where plumi = '+' and matnr = gt_marc-matnr and werks = gt_marc-werks.
      if gt_tab-dat00 between date1 and date2." 按天
        concatenate 'DYNAMIC_MENGE' gt_tab-dat00 into l_fieldname.
        assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
        if gt_tab-mng01 is not initial.
          <dyn_field> = gt_tab-mng01.
        endif.
      endif.


      if gt_tab-dat00 > date2 and gt_tab-dat00 <= date_1. " 按1周
        l_mng_1 = l_mng_1 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_1 and gt_tab-dat00 <= date_2 . " 按2周
        l_mng_2 = l_mng_2 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_2 and gt_tab-dat00 <= date_3 . " 按3周
        l_mng_3 = l_mng_3 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_3 and gt_tab-dat00 <= date_4 . " 按4周
        l_mng_4 = l_mng_4 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_4 and gt_tab-dat00 <= date_5 . " 按5周
        l_mng_5 = l_mng_5 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_5 and gt_tab-dat00 <= date_6 . " 按6周
        l_mng_6 = l_mng_6 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_6 and gt_tab-dat00 <= date_7 . " 按7周
        l_mng_7 = l_mng_7 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_7 and gt_tab-dat00 <= date_8 . " 按8周
        l_mng_8 = l_mng_8 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_8 and gt_tab-dat00 <= date_9 . " 按9周
        l_mng_9 = l_mng_9 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_9 and gt_tab-dat00 <= date_10 . " 按10周
        l_mng_10 = l_mng_10 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_10 and gt_tab-dat00 <= date_11 . " 按11周
        l_mng_11 = l_mng_11 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_11 and gt_tab-dat00 <= date_12 . " 按12周
        l_mng_12 = l_mng_12 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_12 and gt_tab-dat00 <= date_13 . " 按13周
        l_mng_13 = l_mng_13 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_13 and gt_tab-dat00 <= date_14 . " 按14周
        l_mng_14 = l_mng_14 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_14 and gt_tab-dat00 <= date_15 . " 按15周
        l_mng_15 = l_mng_15 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_15 and gt_tab-dat00 <= date_16 . " 按16周
        l_mng_16 = l_mng_16 + gt_tab-mng01.
      endif.

    endloop.

    concatenate 'DYNAMIC_MENGE' date_1 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_1 is not initial.
      <dyn_field> = l_mng_1.
    endif.

    concatenate 'DYNAMIC_MENGE' date_2 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_2 is not initial.
      <dyn_field> = l_mng_2.
    endif.

    concatenate 'DYNAMIC_MENGE' date_3 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_3 is not initial.
      <dyn_field> = l_mng_3.
    endif.

    concatenate 'DYNAMIC_MENGE' date_4 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_4 is not initial.
      <dyn_field> = l_mng_4.
    endif.

    concatenate 'DYNAMIC_MENGE' date_5 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_5 is not initial.
      <dyn_field> = l_mng_5.
    endif.

    concatenate 'DYNAMIC_MENGE' date_6 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_6 is not initial.
      <dyn_field> = l_mng_6.
    endif.

    concatenate 'DYNAMIC_MENGE' date_7 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_7 is not initial.
      <dyn_field> = l_mng_7.
    endif.

    concatenate 'DYNAMIC_MENGE' date_8 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_8 is not initial.
      <dyn_field> = l_mng_8.
    endif.

    concatenate 'DYNAMIC_MENGE' date_9 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_9 is not initial.
      <dyn_field> = l_mng_9.
    endif.

    concatenate 'DYNAMIC_MENGE' date_10 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_10 is not initial.
      <dyn_field> = l_mng_10.
    endif.

    concatenate 'DYNAMIC_MENGE' date_11 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_11 is not initial.
      <dyn_field> = l_mng_11.
    endif.

    concatenate 'DYNAMIC_MENGE' date_12 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_12 is not initial.
      <dyn_field> = l_mng_12.
    endif.

    concatenate 'DYNAMIC_MENGE' date_13 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_13 is not initial.
      <dyn_field> = l_mng_13.
    endif.

    concatenate 'DYNAMIC_MENGE' date_14 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_14 is not initial.
      <dyn_field> = l_mng_14.
    endif.

    concatenate 'DYNAMIC_MENGE' date_15 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_15 is not initial.
      <dyn_field> = l_mng_15.
    endif.

    concatenate 'DYNAMIC_MENGE' date_16 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_16 is not initial.
      <dyn_field> = l_mng_16.
    endif.


    assign component 'MATNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-matnr.

    assign component 'WERKS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-werks.

    assign component 'ZGXLX_MS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = '供给数量'.

    assign component 'PLIFZ' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-plifz.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGKC' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  gt_tab_b-mngkc.
    endif.

    read table lt_makt with key matnr = gt_marc-matnr.
    if sy-subrc = 0.
      assign component 'MAKTX' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  lt_makt-maktx.
    endif.

    append <dyn_wa> to <dyn_table>.
    clear <dyn_wa>.


*可用数量
    clear:
    l_mng_1,
    l_mng_2,
    l_mng_3,
    l_mng_4,
    l_mng_5,
    l_mng_6,
    l_mng_7,
    l_mng_8,
    l_mng_9,
    l_mng_10,
    l_mng_11,
    l_mng_12,
    l_mng_13,
    l_mng_14,
    l_mng_15,
    l_mng_16.

    loop at gt_tab where  matnr = gt_marc-matnr and werks = gt_marc-werks.
      if gt_tab-dat00 between date1 and date2." 按天
        concatenate 'DYNAMIC_MENGE' gt_tab-dat00 into l_fieldname.
        assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
        if gt_tab-mng01 is not initial.
          <dyn_field> = gt_tab-mng01.
        endif.
      endif.

      if gt_tab-dat00 > date2 and gt_tab-dat00 <= date_1. " 按1周
        l_mng_1 = l_mng_1 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_1 and gt_tab-dat00 <= date_2 . " 按2周
        l_mng_2 = l_mng_2 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_2 and gt_tab-dat00 <= date_3 . " 按3周
        l_mng_3 = l_mng_3 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_3 and gt_tab-dat00 <= date_4 . " 按4周
        l_mng_4 = l_mng_4 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_4 and gt_tab-dat00 <= date_5 . " 按5周
        l_mng_5 = l_mng_5 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_5 and gt_tab-dat00 <= date_6 . " 按6周
        l_mng_6 = l_mng_6 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_6 and gt_tab-dat00 <= date_7 . " 按7周
        l_mng_7 = l_mng_7 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_7 and gt_tab-dat00 <= date_8 . " 按8周
        l_mng_8 = l_mng_8 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_8 and gt_tab-dat00 <= date_9 . " 按9周
        l_mng_9 = l_mng_9 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_9 and gt_tab-dat00 <= date_10 . " 按10周
        l_mng_10 = l_mng_10 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_10 and gt_tab-dat00 <= date_11 . " 按11周
        l_mng_11 = l_mng_11 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_11 and gt_tab-dat00 <= date_12 . " 按12周
        l_mng_12 = l_mng_12 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_12 and gt_tab-dat00 <= date_13 . " 按13周
        l_mng_13 = l_mng_13 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_13 and gt_tab-dat00 <= date_14 . " 按14周
        l_mng_14 = l_mng_14 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_14 and gt_tab-dat00 <= date_15 . " 按15周
        l_mng_15 = l_mng_15 + gt_tab-mng01.
      endif.

      if gt_tab-dat00 > date_15 and gt_tab-dat00 <= date_16 . " 按16周
        l_mng_16 = l_mng_16 + gt_tab-mng01.
      endif.

    endloop.

    concatenate 'DYNAMIC_MENGE' date_1 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_1 is not initial.
      <dyn_field> = l_mng_1.
    endif.

    concatenate 'DYNAMIC_MENGE' date_2 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_2 is not initial.
      <dyn_field> = l_mng_2.
    endif.

    concatenate 'DYNAMIC_MENGE' date_3 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_3 is not initial.
      <dyn_field> = l_mng_3.
    endif.

    concatenate 'DYNAMIC_MENGE' date_4 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_4 is not initial.
      <dyn_field> = l_mng_4.
    endif.

    concatenate 'DYNAMIC_MENGE' date_5 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_5 is not initial.
      <dyn_field> = l_mng_5.
    endif.

    concatenate 'DYNAMIC_MENGE' date_6 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_6 is not initial.
      <dyn_field> = l_mng_6.
    endif.

    concatenate 'DYNAMIC_MENGE' date_7 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_7 is not initial.
      <dyn_field> = l_mng_7.
    endif.

    concatenate 'DYNAMIC_MENGE' date_8 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_8 is not initial.
      <dyn_field> = l_mng_8.
    endif.

    concatenate 'DYNAMIC_MENGE' date_9 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_9 is not initial.
      <dyn_field> = l_mng_9.
    endif.

    concatenate 'DYNAMIC_MENGE' date_10 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_10 is not initial.
      <dyn_field> = l_mng_10.
    endif.

    concatenate 'DYNAMIC_MENGE' date_11 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_11 is not initial.
      <dyn_field> = l_mng_11.
    endif.

    concatenate 'DYNAMIC_MENGE' date_12 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_12 is not initial.
      <dyn_field> = l_mng_12.
    endif.

    concatenate 'DYNAMIC_MENGE' date_13 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_13 is not initial.
      <dyn_field> = l_mng_13.
    endif.

    concatenate 'DYNAMIC_MENGE' date_14 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_14 is not initial.
      <dyn_field> = l_mng_14.
    endif.

    concatenate 'DYNAMIC_MENGE' date_15 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_15 is not initial.
      <dyn_field> = l_mng_15.
    endif.

    concatenate 'DYNAMIC_MENGE' date_16 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_16 is not initial.
      <dyn_field> = l_mng_16.
    endif.

    assign component 'MATNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-matnr.

    assign component 'WERKS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-werks.

    assign component 'ZGXLX_MS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = '可用数量'.

    assign component 'PLIFZ' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-plifz.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGKC' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  gt_tab_b-mngkc.
    endif.

    read table lt_makt with key matnr = gt_marc-matnr.
    if sy-subrc = 0.
      assign component 'MAKTX' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  lt_makt-maktx.
    endif.

    append <dyn_wa> to <dyn_table>.
    clear <dyn_wa>.

  endloop.

* 可用数量 = 前一列可用数量 + 本列可用数量
  data:before_mng01 type mdez-mng01.
  data:mngkc type mdez-mng01.
  data:date_count type syst-datum.

  field-symbols:<fs> type ty_tab.
  data: l_count type i.
*
  l_count = date2 - date1 + 1.


  loop at <dyn_table> assigning <dyn_wa> .

    assign component 'ZGXLX_MS' of structure <dyn_wa> to <dyn_field>.

    if <dyn_field> = '可用数量'.
      before_mng01 = 0.
      date_count = date1. " 列开始日期

      do l_count times. " 循环按天计算，加上前一列数量

        if date_count = date1. " 如果是开始列日期
          clear mngkc.
          assign component  'MNGKC' of structure <dyn_wa> to <dyn_field>.
          before_mng01 = <dyn_field>.

          concatenate 'DYNAMIC_MENGE' date1 into l_fieldname.
          assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
          before_mng01 =  <dyn_field> + before_mng01.
          <dyn_field>  = before_mng01.
        else.

          concatenate 'DYNAMIC_MENGE' date_count into l_fieldname.
          assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
          before_mng01 =  <dyn_field> + before_mng01.
          <dyn_field> =  before_mng01.
        endif.

        date_count = date_count + 1." 日期累计
      enddo.

      " 按周计算，加上前一列数量
      concatenate 'DYNAMIC_MENGE' date_1 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_2 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_3 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_4 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_5 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_6 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_7 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_8 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_9 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_10 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_11 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_12 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_13 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_14 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_15 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

      concatenate 'DYNAMIC_MENGE' date_16 into l_fieldname.
      assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
      before_mng01 =  <dyn_field> + before_mng01.
      <dyn_field> = before_mng01  .

    endif.

  endloop.





endform.                    " WRITE_DATA_TO_DYNTABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_init_layout .
  wa_layout-cwidth_opt = 'X'.
endform.                    " FRM_INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_show_alv .
* ALV显示函数
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = it_structure
*     i_grid_title       = g_title
*     IT_EXCLUDING       =       "系统自带STATUS图标控制内表
*     I_SAVE             = CNS_X
*     i_callback_pf_status_set = 'PF_STATUS_SET'
*     i_callback_user_command  = 'FRM_USER_COMMAND'
    tables
      t_outtab           = <dyn_table>
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
endform.                    " FRM_SHOW_ALV

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_FCAT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form frm_alv_fcat_set  using    pu_pos    type i
                                pu_fname  type c
                                pu_ftext  type c
                                pu_rfield type c
                                pu_rtable type c
                                pu_qname  type c
                                pu_outputlen   type c
                                pu_datatype type c
                                pu_no_zero type c.
  clear wa_structure.
  wa_structure-col_pos    = pu_pos.
  wa_structure-fieldname  = pu_fname.
  wa_structure-scrtext_l  = pu_ftext.
  wa_structure-scrtext_m  = pu_ftext.
  wa_structure-scrtext_s  = pu_ftext.
  wa_structure-ref_field  = pu_rfield.
  wa_structure-ref_table  = pu_rtable.
  wa_structure-qfieldname = pu_qname.
  wa_structure-outputlen       = pu_outputlen.
  wa_structure-key = 'X'.
  wa_structure-datatype = pu_datatype.
  wa_structure-no_zero = pu_no_zero.
  append wa_structure to it_structure.


endform.                    " FRM_ALV_FCAT_SET
