*&---------------------------------------------------------------------*
*& Report  ZPP011
*&
*&---------------------------------------------------------------------*
*&
*& 欠料表
*&---------------------------------------------------------------------*

report zpp011bk.

type-pools:slis,vrm,icon.

tables: mara,marc,syst,vbap,afko.

types:begin of ty_tab,
        werks  type marc-werks, " 工厂
        matnr  type marc-matnr, " 物料
        maktx  type makt-maktx, " 物料描述
        mngpo  type mdez-mng01, " 在外PO数量
        mngkc  type mdez-mng01, " 库存数量
        mngpr  type mdez-mng01, " 在外PR数量
        mngall type mdez-mng01, " 总需求
        mngph  type mdez-mng01, " 最终平衡数量
        mng_01 type mdez-mng01, " 良品仓
        mng_02 type mdez-mng01, " 待检仓
        mng_03 type mdez-mng01, " 线边仓
        mng_04 type mdez-mng01, " 不良品退货仓
        menge  type stpo-menge, " 单位用量
        meins  type stpo-meins, " 单位
        plumi  type mdez-plumi, " 收货/发货标识
        dat00  type mdez-dat00,
        mng01  type mdez-mng01,
        delkz  type mdez-delkz,

      end of ty_tab.

types:begin of ty_marc,
        werks        type marc-werks, " 工厂
        matnr        type marc-matnr, " 物料
        menge        type stpo-menge, " 单位用量
        meins        type stpo-meins, " 单位
        stlnr        type stko-stlnr, " 物料单
        aufnr        type afpo-aufnr, " 订单号
        matnr_header type marc-matnr, " 上层组件物料
        maktx_header type makt-maktx,
        stlan        type mast-stlan, " BOM 用途
        stlal        type mast-stlal, " 可选的 BOM
        dispo        type marc-dispo, " 组件MRP控制者
      end of ty_marc.

data:gt_tab type standard table of ty_tab with header line.
data:gt_tab_b type standard table of ty_tab with header line.
data:gt_tab_tmp type standard table of ty_tab with header line.
data:gt_tab02 type standard table of ty_tab with header line.
data:gt_marc type standard table of ty_marc with header line.

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
s_dispo for marc-dispo, " MRP控制者
s_vbeln for vbap-vbeln,
s_posnr for vbap-posnr,
s_aufnr for afko-aufnr," 生产订单
s_budat for syst-datum obligatory no-extension no intervals default sy-datum,
s_dispo2 for marc-dispo. " 组件MRP控制者

parameters:
p_gckc as checkbox default 'X',
p_cgdd as checkbox,
p_cgsq as checkbox.


selection-screen end of block blk.

at selection-screen .

  if s_matnr-low = '*'.
    message '请输入合适的选择条件,查询物料太多，将超过系统负荷！' type 'E'.
  endif.

start-of-selection.
  if s_matnr[] is initial and  s_vbeln[] is initial and s_aufnr[] is initial and s_dispo[] is initial  .
    message '请输入合适的选择条件,查询物料太多，将超过系统负荷！' type 'E'.
  endif.
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  data:lt_mdezx type standard table of mdez with header line.
  data:lt_marc type standard table of ty_marc with header line.
  data:lt_marc_tmp type standard table of ty_marc with header line.

  clear:lt_marc,lt_marc[].
  clear:lt_marc_tmp,lt_marc_tmp[].

  data:lt_marc_bom type standard table of ty_marc with header line.
  clear:lt_marc_bom,lt_marc_bom[].

  data: i_bom like stpox occurs 0 with header line.
  data:lw_topmat type cstmat.

*按工厂和物料取得数据

  " 销售订单和入生产订单同时为空时
  if s_aufnr[] is initial and s_vbeln[] is initial.
    select werks
    matnr
    from marc
    into corresponding fields of table lt_marc
    where matnr in s_matnr
    and dispo in s_dispo
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
          topmat                = lw_topmat
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
      " 成品物料
      lt_marc_bom-matnr = lt_marc-matnr.  " 物料
      lt_marc_bom-werks = lt_marc-werks.  " 工厂
      lt_marc_bom-menge = lw_topmat-bmeng.  " 单位用量
      lt_marc_bom-meins = lw_topmat-bmein.  " 单位
      lt_marc_bom-stlnr = lw_topmat-stlnr. " 物料单
      lt_marc_bom-matnr_header = lt_marc-matnr. " 上层组件物料
      append lt_marc_bom.
      clear lt_marc_bom.

      " 取得展开的物料bom
      loop at i_bom.
        lt_marc_bom-matnr = i_bom-idnrk.  " 物料
        lt_marc_bom-werks = i_bom-werks.  " 工厂
        lt_marc_bom-menge = i_bom-mnglg.  " 单位用量
        lt_marc_bom-meins = i_bom-mmein.  " 单位
        lt_marc_bom-stlnr = i_bom-stlnr. " 物料单
        lt_marc_bom-stlan = i_bom-stlan. "
        lt_marc_bom-stlal = i_bom-stlal.
        append lt_marc_bom.
        clear lt_marc_bom.
      endloop.

    endloop.

  endif.

*  SORT lt_marc_bom BY werks matnr.
*  DELETE ADJACENT DUPLICATES FROM lt_marc_bom COMPARING werks matnr.
*  APPEND LINES OF lt_marc_bom TO lt_marc.
  lt_marc[] = lt_marc_bom[].

*按生产订单取得数据
  data:lt_resb type standard table of resb with header line.
  data:lt_afpo type standard table of afpo with header line.
  data:lt_afko type standard table of afko with header line.

  " 输入生产订单时
  if s_aufnr[] is not initial.

    select rsnum
      rspos
      matnr
      werks
      aufnr
      bdmng
      meins
      aufnr
      from resb
      into corresponding fields of table lt_resb
      where aufnr in s_aufnr
      and xloek <> 'X' " 已删除项目
      and dumps <> 'X'. " 虚拟项目标识

    select aufnr
        posnr
        matnr
        meins
      dwerk
      from afpo
      into corresponding fields of table lt_afpo
      where aufnr in s_aufnr.

    select aufnr
      gamng
      from afko
      into corresponding fields of table lt_afko
      where     aufnr in s_aufnr.

    " 生产订单抬头物料
    loop at lt_afpo.
      if lt_afpo-matnr is not initial.
        lt_marc-matnr = lt_afpo-matnr. " 物料
        lt_marc-werks = lt_afpo-dwerk. " 工厂
        lt_marc-meins = lt_afpo-meins. " 单位
        read table lt_afko with key aufnr = lt_afpo-aufnr.
        if sy-subrc = 0.
          lt_marc-menge = lt_afko-gamng." 单位用量
        endif.
        lt_marc-aufnr = lt_afpo-aufnr. " 生产订单
        append lt_marc.
        clear lt_marc.
      endif.

    endloop.

    " 生产订单组件物料
    loop at lt_resb.
      lt_marc-matnr = lt_resb-matnr. " 物料
      lt_marc-werks = lt_resb-werks. " 工厂
      lt_marc-menge = lt_resb-bdmng.  " 单位用量
      lt_marc-meins = lt_resb-meins.  " 单位
      lt_marc-aufnr = lt_resb-aufnr." 生产订单
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
      *
      from kdst
      into corresponding fields of table lt_kdst
      where matnr in s_matnr
      and vbeln in s_vbeln
      and vbpos in s_posnr
      and werks in s_werks.

*取得KDST销售订单抬头物料
*    LOOP AT  lt_kdst.
*      lt_marc-matnr = lt_kdst-matnr. " 物料
*      lt_marc-werks = lt_kdst-werks. " 工厂
*      APPEND lt_marc.
*      CLEAR lt_marc.
*    ENDLOOP.

    " 取得销售订单bom
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
        importing
          topmat                = lw_topmat
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
*取得KDST销售订单抬头物料
      lt_marc-matnr = lt_kdst-matnr. " 物料
      lt_marc-werks = lt_kdst-werks. " 工厂
      lt_marc-menge = lw_topmat-bmeng.  " 单位用量
      lt_marc-meins = lw_topmat-bmein.  " 单位
      lt_marc-stlnr = lw_topmat-stlnr. " 物料单
      lt_marc-matnr_header = lt_kdst-matnr. " 上层组件物料
      append lt_marc.
      clear lt_marc.

      " 取得销售订单bom
      loop at lt_stb where bmtyp = 'K'.
        lt_marc-matnr = lt_stb-idnrk. " 物料
        lt_marc-werks = lt_stb-werks. " 工厂
        lt_marc-menge = lt_stb-mnglg.  " 单位用量
        lt_marc-meins = lt_stb-mmein.  " 单位
        lt_marc-stlnr = lt_stb-stlnr. " 物料单
        lt_marc-stlan = lt_stb-stlan. "
        lt_marc-stlal = lt_stb-stlal.
        append lt_marc.
        clear lt_marc.
      endloop.

    endloop.

  endif.

*按工厂，物料号，排除相同物料
  lt_marc_tmp[] = lt_marc[].
  sort lt_marc by  werks matnr.
  delete adjacent duplicates from lt_marc comparing werks matnr.

* MD04，取得库存数量,汇总
  loop at lt_marc.

    clear:lt_mdezx,lt_mdezx[].
    call function 'MD_STOCK_REQUIREMENTS_LIST_API'
      exporting
*       PLSCN                    =
        matnr                    = lt_marc-matnr
        werks                    = lt_marc-werks
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
      if lt_mdezx-plumi = '+'.
        " 取得类型为BE/E1/LA/LE/RU/U1/U4，BA/U2
        if lt_mdezx-delkz = 'BE' or lt_mdezx-delkz = 'E1'
          or lt_mdezx-delkz = 'LA' or lt_mdezx-delkz = 'LE'
          or lt_mdezx-delkz = 'RU' or lt_mdezx-delkz = 'U1'
          or lt_mdezx-delkz = 'U4' or lt_mdezx-delkz = 'BA'
          or lt_mdezx-delkz = 'U2' or lt_mdezx-delkz = 'QM'.
          gt_tab-matnr  = lt_marc-matnr.
          gt_tab-werks  = lt_marc-werks.
          gt_tab-menge =  lt_marc-menge.  " 单位用量
          gt_tab-meins =  lt_marc-meins.  " 单位
          gt_tab-plumi =  lt_mdezx-plumi. " 收货/发货标识
          gt_tab-dat00 =  lt_mdezx-dat00. " 收货/需求日期
          gt_tab-mng01 =  lt_mdezx-mng01. " 收货数量或需求数量
          gt_tab-delkz =  lt_mdezx-delkz.

          collect gt_tab .
          clear gt_tab.
        endif.

      else.
        gt_tab-matnr  = lt_marc-matnr.
        gt_tab-werks  = lt_marc-werks.
        gt_tab-menge =  lt_marc-menge.  " 单位用量
        gt_tab-meins =  lt_marc-meins.  " 单位
        gt_tab-plumi = lt_mdezx-plumi. " 收货/发货标识
        gt_tab-dat00 = lt_mdezx-dat00. " 收货/需求日期
        gt_tab-mng01 = lt_mdezx-mng01. " 收货数量或需求数量
        gt_tab-delkz = lt_mdezx-delkz.

        collect gt_tab .
        clear gt_tab.
      endif.
    endloop.

    " 计算库存数量
    data:lt_mard type standard table of mard with header line.
    data:lt_mska type standard table of mska with header line.
    data:lt_mcsd type standard table of mcsd with header line.
    data:lt_mslb type standard table of mslb with header line.
*    DATA:lt_mchb TYPE STANDARD TABLE OF mchb WITH HEADER LINE.

    clear:lt_mard,lt_mard[].
    clear:lt_mska,lt_mska[].
    clear:lt_mcsd,lt_mcsd[].
    clear:lt_mslb,lt_mslb[].
*    CLEAR:lt_mchb,lt_mchb[].

    if gt_tab[] is not initial.
      " 库存
      select *
        from mard
        into corresponding fields of table lt_mard
        for all entries in gt_tab
        where matnr = gt_tab-matnr
        and werks = gt_tab-werks
        and lgort in ('1002','R002','1001','R001','1003','R003','2001','2002','2004','2005','2006').

      " 销售订单库存
      select *
        from mska
        into corresponding fields of table lt_mska
        for all entries in gt_tab
        where matnr = gt_tab-matnr
        and werks = gt_tab-werks
        and lgort in ('1002','R002','1001','R001','1003','R003','2001','2002','2004','2005','2006').
      " 客户库存
      select *
        from mcsd
        into corresponding fields of table lt_mcsd
        for all entries in gt_tab
        where matnr = gt_tab-matnr
        and werks = gt_tab-werks
        and lgort in ('1002','R002','1001','R001','1003','R003','2001','2002','2004','2005','2006').
      " 供应商特殊库存
      select *
        from mslb
        into corresponding fields of table lt_mslb
        for all entries in gt_tab
        where matnr = gt_tab-matnr
        and werks = gt_tab-werks.
*      " 批次库存
*      SELECT *
*        FROM mchb
*        INTO CORRESPONDING FIELDS OF TABLE lt_mchb
*       FOR ALL ENTRIES IN gt_tab
*      WHERE matnr = gt_tab-matnr
*      AND werks = gt_tab-werks
*      AND lgort IN ('1002','R002','1001','R001','1003','R003','2001','2002','2004','2005','2006').
    endif.

    " 取得工厂库存和取得未清PO,未清PR,
    loop at gt_tab where matnr  = lt_marc-matnr and werks  = lt_marc-werks.

      loop at lt_mdezx .
        if lt_mdezx-plumi = 'B' .
          ""  排除不参与MRP运算库存地点库存 ，排除安全库存
          if lt_mdezx-delkz <> 'LB' and  lt_mdezx-delkz <> 'SH' .
            gt_tab-mngkc = lt_mdezx-mng01 + gt_tab-mngkc.
          endif.
        endif.

        " 在外PO = 未清PO
        " 取得未清PO
        if lt_mdezx-plumi = '+' .
          if lt_mdezx-delkz = 'BE' or lt_mdezx-delkz = 'E1'
          or lt_mdezx-delkz = 'LA' or lt_mdezx-delkz = 'LE'
          or lt_mdezx-delkz = 'RU' or lt_mdezx-delkz = 'U1'
          or lt_mdezx-delkz = 'U4'. " OR lt_mdezx-delkz = 'QM'.
            gt_tab-mngpo =  lt_mdezx-mng01 + gt_tab-mngpo.
          endif.
        endif.

        " 取得未清PR
*        " 采购申请数量=相应日期MD04采购申请元素（MDEZX-DELKZ=BA/U2）且(MDEZX-PLUMI=+)的值之和
        if lt_mdezx-plumi = '+' .
          if lt_mdezx-delkz = 'BA' or lt_mdezx-delkz = 'U2'.
            gt_tab-mngpr =  gt_tab-mngpr + lt_mdezx-mng01 .
          endif.
        endif.

        " 总需求，取MD04中各负值之和(MDEZX-PLUMI=-)，带符号，加上安全库存（MDEZX-DELKZ=SH）
        if lt_mdezx-plumi = '-' or lt_mdezx-delkz = 'SH'.
          gt_tab-mngall = gt_tab-mngall + lt_mdezx-mng01.
        endif.

        " 取MD04中各正，负值，库存之和
        if lt_mdezx-delkz <> 'PA'. " 排除计划订单，PA
          gt_tab-mngph =   gt_tab-mngph  + lt_mdezx-mng01.
        endif.
      endloop.

      " 良品仓库存,待检仓库存,线边仓库存,不良品退货仓库存
      loop at  lt_mard where matnr = gt_tab-matnr
                        and werks = gt_tab-werks.
        if lt_mard-lgort = '1002' or lt_mard-lgort = 'R002'.
          gt_tab-mng_01 =  lt_mard-labst  +  gt_tab-mng_01.
        endif.

        "待检验仓的库存没有带上检验批的数量。需在原基础上加上检验批数量（MARD-INSME)。
        if lt_mard-lgort = '1001' or lt_mard-lgort = 'R001'.
          gt_tab-mng_02 =  lt_mard-labst  +  gt_tab-mng_02 + lt_mard-insme.
        endif.

        if lt_mard-lgort = '2001' or lt_mard-lgort = '2002' or lt_mard-lgort = '2003'
          or lt_mard-lgort = '2005' or lt_mard-lgort = '2006'.
          gt_tab-mng_03 =  lt_mard-labst +  gt_tab-mng_03.
        endif.

        if lt_mard-lgort = '1003' or lt_mard-lgort = 'R003'.
          gt_tab-mng_04 =  lt_mard-labst  +  gt_tab-mng_04.
        endif.
      endloop.

      loop at lt_mska where matnr = gt_tab-matnr
                      and werks = gt_tab-werks.
        if lt_mska-lgort = '1002' or lt_mska-lgort = 'R002'.
          gt_tab-mng_01 =  lt_mska-kalab  +  gt_tab-mng_01.
        endif.

        if lt_mska-lgort = '1001' or lt_mska-lgort = 'R001'.
          gt_tab-mng_02 =   lt_mska-kalab + lt_mska-kains +  gt_tab-mng_02.
        endif.

        if lt_mska-lgort = '2001' or lt_mska-lgort = '2002' or lt_mska-lgort = '2003'
          or lt_mska-lgort = '2005' or lt_mska-lgort = '2006'.
          gt_tab-mng_03 =   lt_mska-kalab  +  gt_tab-mng_03.
        endif.

        if lt_mska-lgort = '1003' or lt_mska-lgort = 'R003'.
          gt_tab-mng_04 =   lt_mska-kalab  +  gt_tab-mng_04.
        endif.
      endloop.

      loop at lt_mcsd where matnr = gt_tab-matnr
                      and werks =  gt_tab-werks.

        if lt_mcsd-lgort = '1002' or lt_mcsd-lgort = 'R002'.
          gt_tab-mng_01 =  lt_mcsd-sdlab   +  gt_tab-mng_01.
        endif.

        if lt_mcsd-lgort = '1001' or lt_mcsd-lgort = 'R001'.
          gt_tab-mng_02 =   lt_mcsd-sdlab + lt_mcsd-sdins +  gt_tab-mng_02.
        endif.

        if lt_mcsd-lgort = '2001' or lt_mcsd-lgort = '2002' or lt_mcsd-lgort = '2003'
          or lt_mcsd-lgort = '2005' or lt_mcsd-lgort = '2006'.
          gt_tab-mng_03 =   lt_mcsd-sdlab  +  gt_tab-mng_03.
        endif.

        if lt_mcsd-lgort = '1003' or lt_mcsd-lgort = 'R003'.
          gt_tab-mng_04 =  lt_mcsd-sdlab  +  gt_tab-mng_04.
        endif.

      endloop.

*      LOOP AT lt_mchb  WHERE matnr = gt_tab-matnr
*                        AND werks =  gt_tab-werks.
*        IF lt_mchb-lgort = '1002' OR lt_mchb-lgort = 'R002'.
*          gt_tab-mng_01 =  lt_mchb-clabs  +  gt_tab-mng_01.
*        ENDIF.
*
*        IF lt_mchb-lgort = '1001' OR lt_mchb-lgort = 'R001'.
*          gt_tab-mng_02 =  lt_mchb-clabs  +  gt_tab-mng_02.
*        ENDIF.
*
*        IF lt_mchb-lgort = '2001' OR lt_mchb-lgort = '2002' OR lt_mchb-lgort = '2003'
*          OR lt_mchb-lgort = '2005' OR lt_mchb-lgort = '2006'.
*          gt_tab-mng_03 = lt_mchb-clabs + lt_mchb-cinsm +  gt_tab-mng_03.
*        ENDIF.
*
*         IF lt_mchb-lgort = '1003' OR lt_mchb-lgort = 'R003'.
*          gt_tab-mng_04 =  lt_mchb-clabs +  gt_tab-mng_04.
*         ENDIF.
*      ENDLOOP.

      modify gt_tab.
      clear gt_tab.
    endloop.

*数量=上期数量+本期需求数量+本期供给数量，
*本期需求数量=相应日期MD04各负值之和(MDEZX-PLUMI=-)，带符号；
*本期供给数量=工厂库存（勾选考虑工厂库存）+采购订单数量（勾选考虑采购订单）+采购申请数量（勾选考虑采购申请）；
*采购订单数量=相应日期MD04采购订单元素（MDEZX-DELKZ=BE/E1/LA/LE/RU/U1/U4）且(MDEZX-PLUMI=+)的值之和；
*采购申请数量=相应日期MD04采购申请元素（MDEZX-DELKZ=BA/U2）且(MDEZX-PLUMI=+)的值之和；
*工厂库存 = 取MD04各MRP段库存之和(MDEZX-PLUMI=B)，排除不参与MRP运算库存地点库存（MDEZX-DELKZ=LB）

    " 不勾选考虑工厂库存
    if p_gckc = ''.
      delete gt_tab where plumi = 'B'.
    endif.

    " 不勾选考虑采购订单
    if p_cgdd = ''.
      loop at gt_tab   where plumi = '+'.
        if gt_tab-delkz = 'BE' or gt_tab-delkz = 'E1' or gt_tab-delkz = 'LA'
         and gt_tab-delkz = 'LE' or gt_tab-delkz = 'RU' or gt_tab-delkz = 'U1'
         and gt_tab-delkz = 'U4' or lt_mdezx-delkz = 'QM'.
          delete gt_tab.
        endif.
      endloop.
    endif.

    " 不勾选考虑采购申请
    if p_cgsq = ''.
      loop at gt_tab   where plumi = '+'.
        if gt_tab-delkz = 'BA' or gt_tab-delkz = 'U2'.
          delete gt_tab.
        endif.
      endloop.
    endif.

  endloop.

  gt_marc[] = lt_marc_tmp[].

*取得上层物料和物料描述，组件MRP控制者
  data:lt_mast type standard table of mast with header line.
  data:lt_makt_header type standard table of makt with header line.
  data:lt_marc2 type standard table of marc with header line.
  clear:lt_mast,lt_mast[].
  clear:lt_makt_header,lt_makt_header[].
  clear:lt_marc2,lt_marc2[].

  if gt_marc[] is not initial.
    select
      *
    from mast
    into corresponding fields of table lt_mast
    for all entries in  gt_marc
    where werks = gt_marc-werks
    and stlan = gt_marc-stlan
    and stlnr = gt_marc-stlnr
    and stlal = gt_marc-stlal. " 可选的 BOM

    if  lt_mast[] is not initial.
      select
        matnr
        maktx " 组件物料描述
      from makt
      into corresponding fields of table  lt_makt_header
      for all entries in lt_mast
      where matnr = lt_mast-matnr
      and spras = sy-langu.
    endif.

    select
      werks
      matnr
      dispo " MRP 控制者
     from marc
     into corresponding fields of table lt_marc2
     for all entries in  gt_marc
     where matnr = gt_marc-matnr
     and werks = gt_marc-werks.

  endif.

  loop at gt_marc.
    read table  lt_mast with key werks = gt_marc-werks
                                 stlan = gt_marc-stlan
                                 stlnr = gt_marc-stlnr
                                 stlal = gt_marc-stlal.
    if sy-subrc = 0.
      gt_marc-matnr_header = lt_mast-matnr.
      read table lt_makt_header with key matnr = lt_mast-matnr.
      if sy-subrc = 0.
        gt_marc-maktx_header = lt_makt_header-maktx.
      endif.
    endif.

    read table lt_marc2 with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      gt_marc-dispo = lt_marc2-dispo. " MRP控制者
    endif.

    modify gt_marc.

  endloop.

*组件MRP控制者
  if s_dispo2[] is not initial.
    delete gt_marc where dispo not in s_dispo2.
  endif.


endform.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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


  l_lines = 20.
* 创建静态结构部分
  perform frm_alv_fcat_set using:  1   'STLNR'  '物料单' 'STLNR'  'STKO'  space   space '' '',
                                   2   'MATNR_HEADER'  '上层组件料号' ''  ''  space    '18' '' '',
                                   3   'MAKTX_HEADER'  '上层物料描述' ''  ''  space    '' '' '',
                                   4   'AUFNR'  '生产订单' 'AUFNR'  'AFPO'  space   space '' '',
                                   5   'WERKS' text-010 'WERKS'  'MARC'  space   space '' '',
                                   6   'MATNR' text-011 'MATNR'  'MAKT'  space   space '' '',
                                   7   'MAKTX' text-012 'MAKTX'  'MAKT'  space   space '' '',
                                   8   'MENGE'  '单位用量' ''  ''  space   space '' '',
                                   9   'MEINS'  '单位' ''  ''  space   space 'CUNIT' '',
                                   10   'DISPO'  '组件MRP控制者' ''  ''  space   space ' ' '',
                                   11   'MNGPH'  '最终平衡数量' ''  ''  space   '18' '' '',
                                   12   'MNG_01'  '良品仓库存' ''  ''  space    '18' '' '',
                                   13   'MNG_02'  '待检仓库存' ''  ''  space   '18' '' '',
                                   14   'MNG_03'  '线边仓库存' ''  ''  space   '18' '' '',
                                   15  'MNG_04'  '不良品退货仓库存' ''  ''  space   '18' '' '',
                                   16   'MNGPO'  '在外PO' ''  ''  space   '18' '' '',
                                   17   'MNGPR'  '在外PR' ''  ''  space   '18' '' '',
                                   18   'MNGALL'  '总需求' ''  ''  space   '18' '' '',
                                   19   'MNGKC'  '工厂库存' ''  ''  space    '18' '' ''.

  do l_line times.
    clear wa_structure.
    " 字段名称
    concatenate 'MENGE' l_low into wa_structure-fieldname.
    " 列名称
    l_str1 = l_low+0(4).
    l_str2 = l_low+4(2).
    l_str3 = l_low+6(2).
    concatenate l_str1 l_str2 l_str3 into wa_structure-scrtext_l
    separated by '/'.
    wa_structure-scrtext_m = wa_structure-scrtext_l.
    wa_structure-scrtext_s = wa_structure-scrtext_l.
    wa_structure-col_pos    = l_lines.
    wa_structure-outputlen = 18.
    wa_structure-datatype = 'QUAN'.
    wa_structure-no_zero = 'X'.


*    wa_structure-ref_field  =  'MNG01'.
*     wa_structure-ref_table  =  'MDEZ'.
*     wa_structure-qfieldname =  'MEINH'.

    append wa_structure to it_structure.

    add 1 to l_low.
    clear:l_str1,l_str2,l_str3.
    l_lines = l_lines + 1.
  enddo.

  " 按周显示列
  l_lines = 34.
  do 16 times.
    clear wa_structure.
    concatenate 'MENGE' l_low into wa_structure-fieldname.
    " 列名称
    l_str1 = l_low+0(4).
    l_str2 = l_low+4(2).
    l_str3 = l_low+6(2).
    concatenate l_str1 l_str2 l_str3 into wa_structure-scrtext_l
    separated by '/'.
    wa_structure-scrtext_m = wa_structure-scrtext_l.
    wa_structure-scrtext_s = wa_structure-scrtext_l.
    wa_structure-col_pos    = l_lines.
    wa_structure-outputlen = 18.
*    wa_structure-DECIMALS = 0.
    wa_structure-datatype = 'QUAN'.
    wa_structure-no_zero = 'X'.
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

  " 按bom号，订单，工厂，物料排序
  sort gt_marc by stlnr aufnr werks matnr.


  loop at gt_marc.
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
    loop at gt_tab where matnr = gt_marc-matnr and werks = gt_marc-werks.

      if gt_tab-dat00 between date1 and date2." 按天
        concatenate 'MENGE' gt_tab-dat00 into l_fieldname.
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


    concatenate 'MENGE' date_1 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_1 is not initial.
      <dyn_field> = l_mng_1.
    endif.

    concatenate 'MENGE' date_2 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_2 is not initial.
      <dyn_field> = l_mng_2.
    endif.

    concatenate 'MENGE' date_3 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_3 is not initial.
      <dyn_field> = l_mng_3.
    endif.

    concatenate 'MENGE' date_4 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_4 is not initial.
      <dyn_field> = l_mng_4.
    endif.

    concatenate 'MENGE' date_5 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_5 is not initial.
      <dyn_field> = l_mng_5.
    endif.

    concatenate 'MENGE' date_6 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_6 is not initial.
      <dyn_field> = l_mng_6.
    endif.

    concatenate 'MENGE' date_7 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_7 is not initial.
      <dyn_field> = l_mng_7.
    endif.

    concatenate 'MENGE' date_8 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_8 is not initial.
      <dyn_field> = l_mng_8.
    endif.

    concatenate 'MENGE' date_9 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_9 is not initial.
      <dyn_field> = l_mng_9.
    endif.

    concatenate 'MENGE' date_10 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_10 is not initial.
      <dyn_field> = l_mng_10.
    endif.

    concatenate 'MENGE' date_11 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_11 is not initial.
      <dyn_field> = l_mng_11.
    endif.

    concatenate 'MENGE' date_12 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_12 is not initial.
      <dyn_field> = l_mng_12.
    endif.

    concatenate 'MENGE' date_13 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_13 is not initial.
      <dyn_field> = l_mng_13.
    endif.

    concatenate 'MENGE' date_14 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_14 is not initial.
      <dyn_field> = l_mng_14.
    endif.

    concatenate 'MENGE' date_15 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_15 is not initial.
      <dyn_field> = l_mng_15.
    endif.

    concatenate 'MENGE' date_16 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    if l_mng_16 is not initial.
      <dyn_field> = l_mng_16.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGKC' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  gt_tab_b-mngkc.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGPO' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mngpo.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGPR' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mngpr.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGPH' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mngph.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNGALL' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mngall.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNG_01' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mng_01.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNG_02' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mng_02.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNG_03' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mng_03.
    endif.

    read table gt_tab_b with key matnr = gt_marc-matnr
                                 werks = gt_marc-werks.
    if sy-subrc = 0.
      assign component 'MNG_04' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> = gt_tab_b-mng_04.
    endif.

    assign component 'MATNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-matnr.

    assign component 'MATNR_HEADER' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-matnr_header.

    assign component 'MAKTX_HEADER' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-maktx_header.

    assign component 'DISPO' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-dispo.

    assign component 'WERKS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-werks.

    read table lt_makt with key matnr = gt_marc-matnr.
    if sy-subrc = 0.
      assign component 'MAKTX' of structure <dyn_wa> to <dyn_field>.
      <dyn_field> =  lt_makt-maktx.
    endif.

    assign component 'MENGE' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-menge.

    assign component 'MEINS' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-meins.

    assign component 'AUFNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-aufnr.

    assign component 'STLNR' of structure <dyn_wa> to <dyn_field>.
    <dyn_field> = gt_marc-stlnr.

    append <dyn_wa> to <dyn_table>.
    clear <dyn_wa>.

  endloop.

* 数量 = 前一列数量 + 本列数量
  data:before_mng01 type mdez-mng01.
  data:mngkc type mdez-mng01.
  data:date_count type syst-datum.

  field-symbols:<fs> type ty_tab.
  data: l_count type i.
*
  l_count = date2 - date1 + 1.

  loop at <dyn_table> assigning <dyn_wa> .

    before_mng01 = 0.
    date_count = date1. " 列开始日期

    do l_count times. " 循环按天计算，加上前一列数量

      if date_count = date1. " 如果是开始列日期
        clear mngkc.
        assign component  'MNGKC' of structure <dyn_wa> to <dyn_field>.
        before_mng01 = <dyn_field>.

        concatenate 'MENGE' date1 into l_fieldname.
        assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
        before_mng01 =  <dyn_field> + before_mng01.
        <dyn_field>  = before_mng01.
      else.

        concatenate 'MENGE' date_count into l_fieldname.
        assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
        before_mng01 =  <dyn_field> + before_mng01.
        <dyn_field> =  before_mng01.
      endif.

      date_count = date_count + 1." 日期累计
    enddo.

    " 按周计算，加上前一列数量
    concatenate 'MENGE' date_1 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .


    concatenate 'MENGE' date_2 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .


    concatenate 'MENGE' date_3 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .


    concatenate 'MENGE' date_4 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .


    concatenate 'MENGE' date_5 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_6 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_7 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_8 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_9 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_10 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_11 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_12 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_13 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_14 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_15 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .

    concatenate 'MENGE' date_16 into l_fieldname.
    assign component l_fieldname of structure <dyn_wa> to <dyn_field>.
    before_mng01 =  <dyn_field> + before_mng01.
    <dyn_field> = before_mng01  .


  endloop.


endform.                    " WRITE_DATA_TO_DYNTABLE


form frm_alv_fcat_set  using    pu_pos    type i
                                pu_fname  type c
                                pu_ftext  type c
                                pu_rfield type c
                                pu_rtable type c
                                pu_qname  type c
                                pu_outputlen   type c
                                pu_convexit type c
                                pu_no_zero.
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
  wa_structure-key = ''.
  wa_structure-convexit =   pu_convexit.
  wa_structure-no_zero = pu_no_zero.
  append wa_structure to it_structure.


endform.                    " FRM_ALV_FCAT_SET
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
      i_callback_program      = sy-repid
      is_layout_lvc           = wa_layout
      it_fieldcat_lvc         = it_structure
*     i_grid_title            = g_title
*     IT_EXCLUDING            =       "系统自带STATUS图标控制内表
*     I_SAVE                  = CNS_X
*     i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command = 'FRM_USER_COMMAND'
    tables
      t_outtab                = <dyn_table>
    exceptions
      program_error           = 1
      others                  = 2.
  if sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
endform.                    " FRM_SHOW_ALV

form frm_user_command using p_ucomm type sy-ucomm
                        p_rs_selfield type slis_selfield .

  data: l_werks type marc-werks,
        l_matnr type marc-matnr.

  case p_ucomm.
    when '&IC1' .   " 判断用户的动作
      read table <dyn_table> assigning <dyn_wa> index p_rs_selfield-tabindex .
      if sy-subrc = 0.
        if p_rs_selfield-fieldname eq 'MATNR' or p_rs_selfield-fieldname eq 'WERKS'.   " 判断用户点击的是哪一列
          assign component  'MATNR' of structure <dyn_wa> to <dyn_field>.
          l_matnr = <dyn_field>.

          assign component  'WERKS' of structure <dyn_wa> to <dyn_field>.
          l_werks = <dyn_field>.

          set parameter id: 'MAT' field  l_matnr,
                            'WRK' field  l_werks.
          call  transaction 'MD04'   and skip first screen.
        endif.
      endif.

  endcase.
endform.                    " f_user_command
