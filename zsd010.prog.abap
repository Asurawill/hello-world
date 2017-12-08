*&-----------------------------------------------------------------------------*
*& Report  ZSD010
*&
*& 合同汇总表
*&
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
*& Date Created : 2015/03/06                                                  *
*& Created By   : LEYARD-WEIYUN                                                  *
*& Description  :合同明细表                                                   *
*&----------------------------------------------------------------------------*
** 修改日期   开发人员  请求号                    描述
" 20170726   IT02      ED1K906567              增加：其他收费：ZE09+ZRB2
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
REPORT zsd010.
TYPE-POOLS: slis,vrm.
DATA is_layout TYPE slis_layout_alv.
DATA it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
TABLES: vbap ,vbak ,vbkd,kna1,ebkn,t005u,t171,tvv1t,tvv2,t685t,tcurr.
DATA: BEGIN  OF w_out,
        vbeln        LIKE vbak-vbeln, "订单编号
        auart        LIKE vbak-auart, "订单类型
        xmmc(100)    TYPE c, "项目名称
        bstkd        LIKE vbkd-bstkd, "合同编号
        zsd0302      LIKE vbak-zsd0302, "销售立项号
        erdat        LIKE vbak-erdat, "创建时间
        ernam        LIKE vbak-ernam, "创建人
        kunnr        LIKE vbak-kunnr, "客户编号
        name1        LIKE kna1-name1, "客户名称
        erdat2       LIKE ebkn-erdat, "审核日期
        bezei1       LIKE tvv1t-bezei, "行业
        bezei2       LIKE tvv1t-bezei, "经济性质
        bztxt        LIKE t171-bzirk, "行政区域
        landx        LIKE t005t-landx , "项目实施国家
        bztxt2       LIKE t171-bzirk, "项目实施地区
        bezei3       LIKE t005u-bezei, "项目实施省份
        xmdd(100)    TYPE c, "项目地点
        xmjl1        LIKE kna1-name1, "项目经理1
        jl1zb(30)    TYPE c, "经理1占比
        jl1je        TYPE p LENGTH 14 DECIMALS 2, "经理1金额
        xmjl2        LIKE kna1-name1, "项目经理2
        jl2zb(30)    TYPE c, "经理2占比
        jl2je        TYPE p LENGTH 14 DECIMALS 2, "经理2金额
        jbr1         LIKE kna1-name1, "经办人1
        jbr1zb(30)   TYPE c, "经办1占比
        jbr1je       TYPE p LENGTH 14 DECIMALS 2, "经办人1金额
        jbr2         LIKE kna1-name1, "经办人2
        jbr2zb(30)   TYPE c, "经办人2占比
        jbr2je       TYPE p LENGTH 14 DECIMALS 2, "经办人2金额
        jbr3         LIKE kna1-name1, "经办人3
        jbr3zb(30)   TYPE c, "经办人3占比
        jbr3je       TYPE p LENGTH 14 DECIMALS 2, "经办人3金额
        zgfu         LIKE tvbur-vkbur, "主管副总
        bm           LIKE  tvgrt-bezei, "部门
        xslx         LIKE zsd005-bezei_9, "销售类型
        zhbl(30)     TYPE c, "置换比例
        mrpzt(30)    TYPE c, "MRP状态
        vdatu        LIKE vbak-vdatu, "期望发货日期
        zsd0301      LIKE vbak-zsd0301, "完工日期
        vtext        LIKE t685t-vtext, "产品类型
        ptje         TYPE p LENGTH 14 DECIMALS 2, "屏体金额
        tvje         TYPE p LENGTH 14 DECIMALS 2, "电视TV金额
        zmsb         TYPE p LENGTH 14 DECIMALS 2, "照明设备金额
        kzbf         TYPE p LENGTH 14 DECIMALS 2, "控制部分
        ptsb         TYPE p LENGTH 14 DECIMALS 2, "配套设备
        bpbj         TYPE p LENGTH 14 DECIMALS 2, "备品备件
        gccl         TYPE p LENGTH 14 DECIMALS 2, "工程材料金额
        yjbf         TYPE  p LENGTH 14 DECIMALS 2, "硬件部分总金额
        rjje         TYPE p LENGTH 14 DECIMALS 2, "软件金额
        rjbftotal    TYPE p LENGTH 14 DECIMALS 2, "软件部分总金额
        azts         TYPE p LENGTH 14 DECIMALS 2, "安装调试
        ysbx         TYPE p LENGTH 14 DECIMALS 2, "运输保险费
        gjgfy        TYPE p LENGTH 14 DECIMALS 2, "刚结构费用
        zsfy         TYPE p LENGTH 14 DECIMALS 2, "装饰费用
        bxgc         TYPE p LENGTH 14 DECIMALS 2, "布线工程费用
        gcbf         TYPE p LENGTH 14 DECIMALS 2, "工程部分总金额
        bzsf         TYPE p LENGTH 14 DECIMALS 2, "包装收费
        pxsf         TYPE p LENGTH 14 DECIMALS 2, "培训收费
        sjsf         TYPE p LENGTH 14 DECIMALS 2, "税金收费
        tscpyf       TYPE p LENGTH 14 DECIMALS 2, "特殊产品研发收费
        tsypzz       TYPE p LENGTH 14 DECIMALS 2, "特殊样品制作收费
        zbgl         TYPE p LENGTH 14 DECIMALS 2, "总包管理收费
        xmcb         TYPE p LENGTH 14 DECIMALS 2, "项目承包收费
        zbfw         TYPE p LENGTH 14 DECIMALS 2, "中标服务收费
        qtsf         TYPE p LENGTH 14 DECIMALS 2, "其他收费
        qtbftotal    TYPE p LENGTH 14 DECIMALS 2, "其他部分总金额
        zjzk         TYPE p LENGTH 14 DECIMALS 2, "总价折扣金额
        xmhftotal    TYPE p LENGTH 14 DECIMALS 2 , "项目含税总金额
        xmjztotal    TYPE p LENGTH 14 DECIMALS 2, "项目净值总金额
        hblx1        LIKE konv-waers, "货币类型1
        hl1          LIKE tcurr-ukurs, "汇率1
        xmjztotalrmb TYPE p LENGTH 14 DECIMALS 2, "项目净值总金额-REM
        orbx         TYPE p LENGTH 14 DECIMALS 2, "欧洲、日本、巴西销售合同额
        hblx2        LIKE konv-waers, "货币类型2
        huilv2       LIKE tcurr-ukurs, "汇率2
        orbxrmb      TYPE p LENGTH 14 DECIMALS 2, "欧洲、日本、巴西销售合同额——RMB
        sel(1),
        kvgr1        LIKE vbak-kvgr1,
        kvgr2        LIKE vbak-kvgr2,
        bzirk        LIKE vbkd-bzirk,
        vkbur        LIKE vbak-vkbur,
        vkgrp        LIKE vbak-vkgrp,
        augru        LIKE vbak-augru,
        knumv        LIKE vbak-knumv,
        country      LIKE t005t-land1, "国家代码
        zsd0303      LIKE vbak-zsd0303, "项目实施地区
        region       LIKE vbak-region, "项目实施省份
        vsnmr_v      LIKE vbak-vsnmr_v, "质保年限
        vbegdat      LIKE veda-vbegdat,
        venddat      LIKE veda-venddat,
      END OF w_out.
DATA:BEGIN OF w_vkorg ,
       vkorg LIKE tvko-vkorg,
       vtext LIKE tvkot-vtext,
     END OF w_vkorg.
DATA:BEGIN OF w_auart ,
       auart LIKE tvakt-auart,
       bezei LIKE tvakt-bezei,
     END OF w_auart.
DATA:BEGIN OF w_vbkd,
       vbeln LIKE vbkd-vbeln,
       posnr LIKE vbkd-posnr,
       bstkd LIKE vbkd-bstkd,
       bzirk LIKE vbkd-bzirk,
       inco1 LIKE vbkd-inco1,
     END OF w_vbkd.
DATA: BEGIN OF w_vbpa,
        vbeln LIKE vbpa-vbeln,
        posnr LIKE vbpa-posnr,
        parvw LIKE vbpa-parvw,
        kunnr LIKE vbpa-kunnr,
        name1 LIKE kna1-name1,
        adrnr LIKE vbpa-adrnr,
      END OF w_vbpa.
DATA: BEGIN OF w2_vbpa,
        vbeln LIKE vbpa-vbeln,
        parvw LIKE vbpa-parvw,
        adrnr LIKE vbpa-adrnr,

      END OF w2_vbpa.

DATA:BEGIN OF w_ebkn ,
       vbeln LIKE vbak-vbeln,
       vbelp LIKE ebkn-vbelp,
       erdat LIKE ebkn-erdat,
     END OF w_ebkn.
DATA: t2_vbpa LIKE TABLE OF w2_vbpa WITH HEADER LINE.
DATA: t_adrc LIKE TABLE OF adrc WITH HEADER LINE.
DATA: t_ebkn LIKE TABLE OF ebkn WITH HEADER LINE.
DATA: t_vbpa LIKE TABLE OF w_vbpa WITH HEADER LINE.
DATA: t_vbkd LIKE TABLE OF w_vbkd WITH HEADER LINE.
RANGES: t_kunnr FOR vbak-kunnr .
RANGES:t_kvgr1 FOR vbak-kvgr1,
       t_kvgr2 FOR vbak-kvgr2.
RANGES:t1_kunnr FOR vbpa-kunnr.
RANGES:t2_kunnr FOR vbpa-kunnr.
RANGES:t3_kunnr FOR vbpa-kunnr.
DATA: t_vkorg LIKE TABLE OF w_vkorg WITH HEADER  LINE.
DATA: t_auart LIKE TABLE OF w_auart WITH HEADER  LINE.
DATA: t_out LIKE  TABLE OF w_out WITH HEADER LINE.
DATA: t_kna1 LIKE TABLE OF kna1 WITH HEADER LINE.
DATA: t_tvv1t LIKE TABLE OF tvv1t WITH HEADER LINE.
DATA: t_tvv2t LIKE TABLE OF tvv2t WITH HEADER LINE.
DATA: t_t171t LIKE TABLE OF t171t WITH HEADER LINE.
DATA: t_tvkbt LIKE TABLE OF tvkbt WITH HEADER LINE.
DATA: t_tvgrt LIKE TABLE OF tvgrt WITH HEADER LINE.
DATA: t_tvaut LIKE TABLE OF tvaut WITH HEADER LINE.
DATA: t1_konv LIKE TABLE OF konv WITH HEADER LINE.
DATA: t2_konv LIKE TABLE OF konv WITH HEADER LINE.
DATA: t3_konv LIKE TABLE OF konv WITH HEADER LINE.
DATA: t4_konv LIKE TABLE OF konv WITH HEADER LINE.
DATA: t_vbap LIKE TABLE OF vbap WITH HEADER LINE.
DATA: t1_vbap LIKE TABLE OF vbap WITH HEADER LINE.
DATA: z1 TYPE p LENGTH  14  DECIMALS 2.
DATA: z2 TYPE p LENGTH  14  DECIMALS 2.
DATA: z3 TYPE p LENGTH  14  DECIMALS 2.
DATA: z4 TYPE p LENGTH  14  DECIMALS 2.
DATA: z5 TYPE p LENGTH  14  DECIMALS 2.
DATA: t1_posnr LIKE vbap-posnr,
      t2_posnr LIKE vbap-posnr,
      t3_posnr LIKE vbap-posnr.
DATA:kschltmp LIKE konv-kschl.
DATA: g_back(1).
DATA: gt_zsd010 TYPE TABLE OF zsd010 WITH HEADER LINE.
SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE text-001.
"销售组织
SELECT-OPTIONS: s_vkorg  FOR vbak-vkorg OBLIGATORY .
"销售订单
SELECT-OPTIONS: s_vbeln FOR vbak-vbeln.
"订单类型
SELECT-OPTIONS: s_auart FOR vbak-auart ."MATCHCODE OBJECT Z_TVAK_SD005.
"创建日期
SELECT-OPTIONS: s_erdat FOR vbak-erdat.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  CLEAR g_back.
  IMPORT g_back = g_back FROM MEMORY ID 'ZSD010'.
  FREE MEMORY ID 'ZSD010'.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_vkorg-low.
  PERFORM getvkorg.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_vkorg-high.
  PERFORM getvkorg.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_auart-low.
  PERFORM getauart.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_auart-high.
  PERFORM getauart.

START-OF-SELECTION.
  DATA l_vkorg TYPE vbak-vkorg.
  IF g_back NE 'X'.
    SELECT vkorg FROM tvko INTO l_vkorg WHERE vkorg IN s_vkorg.
      AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
               ID 'VKORG' FIELD l_vkorg.
*             ID 'VTWEG' FIELD 'DUMMY'
*             ID 'SPART' FIELD 'DUMMY'
*             ID 'ACTVT' FIELD 'DUMMY'
      IF sy-subrc <> 0.
        MESSAGE s001(zsd01) WITH l_vkorg DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
*    您没有销售组织&的权限！
      ENDIF.
    ENDSELECT.
  ENDIF.
  PERFORM frm_data.

END-OF-SELECTION.
  IF g_back = 'X'.
    IF gt_zsd010[] IS NOT INITIAL.
      MODIFY zsd010 FROM TABLE gt_zsd010.
      COMMIT WORK.
    ENDIF.
  ELSE.
    PERFORM  inint_layout.
    PERFORM  init_fieldcat.
    PERFORM  frm_display_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  ININT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inint_layout .
  is_layout-zebra = 'X'.
  is_layout-detail_popup = 'X'.
  is_layout-colwidth_optimize = 'X'.
  is_layout-box_fieldname = 'SEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_fieldcat .
  PERFORM append_fieldcat USING:
         'VBELN' '订单编号' '' '' '',
          'AUART' '订单类型' '' '' '',
            'XMMC' '项目名称' '' '' '',
           'BSTKD' '合同编号' '' '' '',
           'ZSD0302' '销售立项号' '' '' '',
            'ERDAT' '创建时间' '' '' '',
           'ERNAM' '创建人' '' '' '',
           'KUNNR' '客户编号' '' '' '',
           'NAME1' '客户名称' '' '' '',
           'ERDAT2' '审核日期' '' '' '',
           'BEZEI1' '行业' '' '' '',
           'BEZEI2' '经济性质' '' '' '',
           'BZTXT' '行政区域' '' '' '',
           'LANDX' '项目实施国家' '' '' '',
           'BZTXT2' '项目实施地区' '' '' '',
           'BEZEI3' '项目实施省份' '' '' '',
           'XMDD' '项目地点' '' '' '',
           'XMJL1' '项目经理1' '' '' '',
           'JL1ZB' '经理1占比' '' '' '',
           'JL1JE' '经理1金额' '' '' '',
           'XMJL2' '项目经理2' '' '' '',
           'JL2ZB' '经理2占比' '' '' '',
           'JL2JE' '经理2金额' '' '' '',
           'JBR1' '经办人1' '' '' '',
           'JBR1ZB' '经办1占比' '' '' '',
           'JBR1JE' '经办人1金额' '' '' '',
           'JBR2' '经办人2' '' '' '',
           'JBR2ZB' '经办人2占比' '' '' '',
           'JBR2JE' '经办人2金额' '' '' '',
           'JBR3' '经办人3' '' '' '',
           'JBR3' '经办人3占比' '' '' '',
           'JBR3JE' '经办人3金额' '' '' '',
           'ZGFU' '主管副总' '' '' '',
           'BM' '部门' '' '' '',
           'XSLX' '销售类型' '' '' '',
           'ZHBL' '置换比例' '' '' '',
           'MRPZT' 'MRP状态' '' '' '',
           'VDATU' '期望发货日期' '' '' '',
           'ZSD0301' '完工日期' '' '' '',
           'VTEXT' '产品类型' '' '' '',
           'PTJE' '屏体金额' '' '' 'HBLX1',
           'TVJE' '电视TV金额' '' '' 'HBLX1',
           'ZMSB' '照明设备金额' '' '' 'HBLX1',
           'KZBF' '控制部分金额' '' '' 'HBLX1',
           'PTSB' '配套设备金额' '' '' 'HBLX1',
           'BPBJ' '备品备件金额' '' '' 'HBLX1',
           'GCCL' '工程材料金额' '' '' 'HBLX1',
           'YJBF' '硬件部分总金额' '' '' '',
           'RJJE' '软件金额' '' '' 'HBLX1',
           'RJBFTOTAL' '软件部分总金额' '' '' '',
           'AZTS' '安装调试' '' '' 'HBLX1',
           'YSBX' '运输保险费' '' '' 'HBLX1',
           'GJGFY' '钢结构费用' '' '' 'HBLX1',
           'ZSFY' '装饰费用' '' '' 'HBLX1',
           'BXGC' '布线工程费用' '' '' 'HBLX1',
           'GCBF' '工程部分总金额' '' '' '',
           'BZSF' '包装收费' '' '' 'HBLX1',
           'PXSF' '培训收费' '' '' 'HBLX1',
           'SJSF' '税金收费' '' '' 'HBLX1',
           'TSCPYF' '特殊产品研发收费' '' '' 'HBLX1',
           'TSYPZZ' '特殊样品制作收费' '' '' 'HBLX1',
           'ZBGL' '总包管理收费' '' '' 'HBLX1',
           'XMCB' '项目承包收费' '' '' 'HBLX1',
           'ZBFW' '中标服务收费' '' '' 'HBLX1',
           'QTSF' '其他收费' '' '' 'HBLX1',
           'QTBFTOTAL' '其他部分总金额' '' '' '',
           'ZJZK' '总价折扣金额' '' '' '',
           'XMHFTOTAL' '项目含税总金额' '' '' '',
           'XMJZTOTAL' '项目净值总金额' '' '' '',
           'HBLX1' '货币类型1' '' '' '',
           'HL1' '汇率1' '' '' '',
           'XMJZTOTALRMB' '项目净值总金额-RMB' '' '' '',
          " 'ORBX' '欧洲、日本、巴西销售合同额' '' '' 'HBLX1',
           'ORBX' '分公司销售合同额' '' '' 'HBLX2',
           'HBLX2' '货币类型2' '' '' '',
           'HUILV2' '汇率2' '' '' 'HBLX2',
           'ORBXRMB' '分公司销售合同额——RMB' '' '' 'HBLX2',
           'VSNMR_V' '质保年限' '' '' '',
           'VBEGDAT' '合同开始日期' '' '' '',
           'VENDDAT' '合同结束日期' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'MENU_SET'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout                = is_layout
      it_fieldcat              = it_fieldcat[]
 "    IT_EVENTS                = GIT_EVENTS[]
    TABLES
      t_outtab                 = t_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_data .
  SELECT vbeln auart zsd0302 erdat ernam kunnr kvgr1 kvgr2 vkbur vkgrp augru knumv country zsd0303 region vdatu zsd0301 vsnmr_v
  INTO CORRESPONDING FIELDS OF TABLE t_out
  FROM vbak
  WHERE vbeln IN s_vbeln AND vkorg IN s_vkorg AND auart IN s_auart AND erdat IN s_erdat AND vkorg <> '0001'.
  SORT t_out BY  vbeln erdat .
  DELETE t_out WHERE auart <>'ZPO' AND auart <> 'ZWV' AND auart <> 'ZZG' AND auart <>'ZF1' AND auart <>'ZSO' .

  IF t_out[] IS NOT INITIAL.
    SELECT vbeln parvw adrnr INTO CORRESPONDING FIELDS OF TABLE t2_vbpa
    FROM vbpa
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln AND posnr = '000000' AND parvw IN ('Z1', 'Z2','Z3','Z4','Z5').


    SELECT * INTO CORRESPONDING FIELDS OF TABLE t1_vbap
    FROM  vbap
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln.
    SORT t1_vbap BY vbeln posnr.
    DELETE ADJACENT DUPLICATES FROM t1_vbap COMPARING vbeln posnr.
    SELECT vbeln vbelp erdat FROM ebkn
    INTO CORRESPONDING FIELDS OF TABLE t_ebkn
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln AND ernam = '北京利亚德 计划员'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t1_konv
    FROM konv
    FOR ALL ENTRIES IN t_out
    WHERE knumv = t_out-knumv AND kschl = 'ZRA1'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t2_konv
    FROM konv
    FOR ALL ENTRIES IN t_out
    WHERE knumv = t_out-knumv AND kschl IN ('ZP01','ZP02','ZP03','ZR03','ZR04','ZR05').
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t3_konv
    FROM konv
    FOR ALL ENTRIES IN t_out
    WHERE knumv = t_out-knumv AND kschl IN ('ZP01','ZR03','ZP02','ZR04','ZP03','ZR05','ZP04','ZP05','ZP06','ZP07','ZP08','ZP09','ZP10','ZP11','ZP12','ZP13','ZE01','ZE02','ZE03','ZE04','ZE05','ZE06','ZE07','ZE08','ZE09','ZNE1',
    'ZA01','ZRA2','ZR02','ZRB2').
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_tvkbt
    FROM tvkbt
    FOR ALL ENTRIES IN t_out
    WHERE vkbur = t_out-vkbur.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_tvgrt
    FROM tvgrt
    FOR ALL ENTRIES IN t_out
    WHERE vkgrp = t_out-vkgrp.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_tvaut
    FROM tvaut
    FOR ALL ENTRIES IN t_out
    WHERE augru = t_out-augru.
    SELECT vbeln posnr bstkd bzirk inco1
    FROM vbkd
    INTO  CORRESPONDING FIELDS OF TABLE t_vbkd
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln  AND posnr = '000000'.
    SORT  t_vbkd BY vbeln ASCENDING  posnr ASCENDING .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_vbap
    FROM vbap
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln
    AND pstyv IN ('Z01','Z02','Z31','Z32','Z41','Z42').
    IF t_vbkd[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_t171t
      FROM  t171t
      FOR ALL ENTRIES IN t_vbkd
      WHERE bzirk = t_vbkd-bzirk.
    ENDIF.
    SELECT vbeln posnr parvw kunnr adrnr
    INTO CORRESPONDING FIELDS OF TABLE t_vbpa
    FROM vbpa
    FOR ALL ENTRIES IN t_out
    WHERE vbeln = t_out-vbeln AND posnr = '000000' AND parvw IN ('Z1', 'Z2','Z3','Z4','Z5').


  ENDIF.
  IF t_vbpa[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_adrc
    FROM adrc
    FOR ALL ENTRIES IN t_vbpa
    WHERE addrnumber = t_vbpa-adrnr.

  ENDIF.
  LOOP AT t_out.
    t_kunnr-sign = 'I'.
    t_kunnr-option = 'E'.
    t_kunnr-low = t_out-kunnr.
    APPEND t_kunnr.
    t_kvgr1-sign = 'I'.
    t_kvgr1-option = 'E'.
    t_kvgr1-low = t_out-kvgr1.
    APPEND t_kvgr1.
    t_kvgr2-sign = 'I'.
    t_kvgr2-option = 'E'.
    t_kvgr2-low = t_out-kvgr2.
    APPEND t_kunnr.
  ENDLOOP.
  LOOP AT t_vbpa .
    t1_kunnr-sign = 'I'.
    t1_kunnr-option = 'E'.
    t1_kunnr-low = t_vbpa-kunnr.
    APPEND t1_kunnr.
  ENDLOOP.
  IF t_out[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_kna1
    FROM kna1
    FOR ALL ENTRIES IN t_out
    WHERE  kunnr = t_out-kunnr.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_kna1
    FROM kna1
    FOR ALL ENTRIES IN t_vbpa
    WHERE kunnr = t_vbpa-kunnr.
    DELETE ADJACENT DUPLICATES FROM t_kna1 COMPARING kunnr.
  ENDIF.
  LOOP AT t_vbpa .
    READ TABLE t_kna1 WITH KEY kunnr = t_vbpa-kunnr.
    IF sy-subrc = 0.
      t_vbpa-name1 = t_kna1-name1.
    ENDIF.
    MODIFY t_vbpa.
  ENDLOOP.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_tvv1t
  FROM tvv1t
  FOR ALL ENTRIES IN t_out
  WHERE spras = sy-langu AND   kvgr1 = t_out-kvgr1.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_tvv2t
  FROM tvv2t
  FOR ALL ENTRIES IN t_out
  WHERE spras = sy-langu AND kvgr2 = t_out-kvgr2.
*增加合同开始日期和结束日期 20171121 IT02 郭雷
  DATA lt_veda TYPE TABLE OF veda WITH HEADER LINE.
  IF t_out[] IS NOT INITIAL.
    SELECT * FROM veda
      INTO TABLE lt_veda
      FOR ALL ENTRIES IN t_out
      WHERE vbeln = t_out-vbeln
      AND   vposn = ''.
  ENDIF.
  DATA:zhbl2 TYPE p LENGTH 14 DECIMALS 2.
  DATA:zhbl3(14) TYPE c.
  LOOP AT t_out . "具体向T_OUT赋值
    z1 = 0 .
    z2 = 0.
    z3 = 0.
    z4 = 0.
    z5 = 0.
    SELECT SINGLE landx INTO t_out-landx FROM t005t WHERE spras = sy-langu AND  land1 = t_out-country.
    PERFORM get_text USING 'Z001' CHANGING t_out-xmmc. "项目名称
    PERFORM get_text USING 'Z002' CHANGING t_out-xmdd. ""项目地点
    "项目实施地区
    SELECT SINGLE bztxt INTO t_out-bztxt2 FROM t171t WHERE spras = sy-langu AND bzirk = t_out-zsd0303.
    "项目实施省份
    SELECT SINGLE bezei INTO t_out-bezei3 FROM t005u  WHERE spras = sy-langu AND land1 = t_out-country AND bland = t_out-region.
    READ TABLE t_vbkd WITH KEY vbeln = t_out-vbeln .
    IF sy-subrc = 0.
      t_out-bstkd = t_vbkd-bstkd. "合同编号
      t_out-bzirk = t_vbkd-bzirk.
    ENDIF.
    READ TABLE t_kna1 WITH KEY kunnr = t_out-kunnr.
    IF sy-subrc = 0.
      t_out-name1 = t_kna1-name1.
    ENDIF.
    READ TABLE t_tvv1t WITH KEY kvgr1 = t_out-kvgr1.
    IF sy-subrc = 0.
      t_out-bezei1 = t_tvv1t-bezei.
    ENDIF.
    READ TABLE t_tvv2t WITH KEY kvgr2 = t_out-kvgr2.
    IF sy-subrc = 0.
      t_out-bezei2 = t_tvv2t-bezei.
    ENDIF.
    READ TABLE t_t171t WITH KEY bzirk = t_out-bzirk.
    IF sy-subrc = 0.
      t_out-bztxt = t_t171t-bztxt.
    ENDIF.

    LOOP AT t_vbpa WHERE vbeln = t_out-vbeln.
      CASE t_vbpa-parvw.
        WHEN 'Z1'.
          PERFORM  computzb  USING t_vbpa-adrnr CHANGING t_out-jl1zb  z1.
          t_out-xmjl1 = t_vbpa-name1.
        WHEN 'Z2'.
          PERFORM  computzb  USING t_vbpa-adrnr CHANGING t_out-jl2zb  z2.
          t_out-xmjl2 = t_vbpa-name1   .
        WHEN 'Z3'.
          PERFORM  computzb  USING t_vbpa-adrnr CHANGING t_out-jbr1zb  z3.
          t_out-jbr1 = t_vbpa-name1    .
        WHEN 'Z4'.
          PERFORM  computzb  USING t_vbpa-adrnr CHANGING t_out-jbr2zb  z4.
          t_out-jbr2 = t_vbpa-name1    .
        WHEN 'Z5'.
          PERFORM  computzb  USING t_vbpa-adrnr CHANGING t_out-jbr3zb  z5.
          t_out-jbr3 = t_vbpa-name1.
      ENDCASE.
    ENDLOOP.
    IF t_out-xmjl1 NE '' AND t_out-xmjl2 EQ  ''.
      t_out-jl1zb = '100%'.

    ENDIF.
    IF t_out-xmjl1 EQ '' AND t_out-xmjl2 NE  ''.
      t_out-jl2zb = '100%'.

    ENDIF.
    IF t_out-jbr1 NE '' AND  t_out-jbr2 EQ  '' AND t_out-jbr3 EQ ''.
      t_out-jbr1zb = '100%'.

    ENDIF.
    IF t_out-jbr2 NE '' AND  t_out-jbr1 EQ  '' AND t_out-jbr3 EQ ''.
      t_out-jbr2zb = '100%'.

    ENDIF.
    IF t_out-jbr3 NE '' AND  t_out-jbr1 EQ  '' AND t_out-jbr2 EQ ''.
      t_out-jbr3zb = '100%'.

    ENDIF.
    CLEAR t1_posnr .
    CLEAR t2_posnr.
    CLEAR t3_posnr.
    READ TABLE t1_vbap WITH KEY vbeln = t_out-vbeln. "取审核日期开始
    IF sy-subrc = 0.
      t1_posnr = t1_vbap-posnr + 1.
      t2_posnr = t1_vbap-posnr + 2.
      t3_posnr = t1_vbap-posnr + 3.
      LOOP AT t_ebkn.
        CASE t_ebkn-vbelp .
          WHEN t1_posnr .
            t_out-erdat2 = t_ebkn-erdat.
          WHEN t2_posnr.
            IF t_out-erdat2 > t_ebkn-erdat.
              t_out-erdat2 = t_ebkn-erdat.
            ENDIF.
          WHEN t3_posnr.
            IF t_out-erdat2 > t_ebkn-erdat.
              t_out-erdat2 = t_ebkn-erdat.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF."取审核日期结束
    READ TABLE t_tvkbt WITH KEY vkbur = t_out-vkbur.
    IF sy-subrc = 0.
      t_out-zgfu = t_tvkbt-bezei.
    ENDIF.
    READ TABLE t_tvgrt WITH KEY vkgrp = t_out-vkgrp.
    IF sy-subrc = 0.

      t_out-bm = t_tvgrt-bezei.
    ENDIF.
    READ TABLE t_tvaut WITH KEY augru = t_out-augru.
    IF sy-subrc = 0.
      t_out-xslx = t_tvaut-bezei.
    ENDIF.
    zhbl2 = 0.
    LOOP AT t1_konv WHERE knumv = t_out-knumv AND  kschl = 'ZRA1' AND kposn = '000000' .
      zhbl2 = zhbl2 + t1_konv-kbetr / 1000.
    ENDLOOP.
    zhbl2 = abs( zhbl2 * 100 ).
    zhbl3 = zhbl2.
    CONCATENATE zhbl3 '%' INTO t_out-zhbl.
    IF t_out-erdat2 = '00000000'.
      t_out-mrpzt = 'N'.
    ELSE.
      t_out-mrpzt = 'Y'.
    ENDIF.
    DATA: t_t685t LIKE TABLE OF t685t WITH HEADER LINE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_t685t
    FROM t685t
    FOR ALL ENTRIES IN t2_konv
    WHERE spras = sy-langu  AND kvewe = 'A' AND kappl = 'V'AND
    kschl = t2_konv-kschl.
    READ TABLE t2_konv WITH KEY knumv = t_out-knumv.
    IF sy-subrc = 0.
      READ TABLE t_t685t WITH KEY  kschl = t2_konv-kschl.
      IF sy-subrc = 0.
        t_out-vtext = t_t685t-vtext. "产品类型
      ENDIF.
    ENDIF.
    "计算屏体、电视TV金额、照明设备金额、控制部分金额
    DATA: kbetrtmp TYPE p LENGTH 14 DECIMALS 2.
    DATA: tzmeng TYPE p LENGTH 16 DECIMALS 2.
*           LOOP AT T_VBAP WHERE VBELN = T_OUT-VBELN.
*             TZMENG = TZMENG + T_VBAP-ZMENG.
*             ENDLOOP.
    PERFORM comput USING t_out-knumv t_out-vbeln  'ZP01' CHANGING  kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-ptje = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZR03' CHANGING  kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-ptje = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZP02' CHANGING kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-tvje = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZR04' CHANGING kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-tvje = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZP03' CHANGING kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-zmsb = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv   'ZR05' CHANGING kbetrtmp.
    IF kbetrtmp <> 0 .
      t_out-zmsb = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv   'ZP04' CHANGING kbetrtmp.
    t_out-kzbf = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv   'ZP05' CHANGING kbetrtmp.
    t_out-ptsb = kbetrtmp.
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZP06' CHANGING kbetrtmp.
    t_out-bpbj = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZP07' CHANGING kbetrtmp.
    t_out-gccl = kbetrtmp .
    t_out-yjbf = ( t_out-ptje + t_out-tvje + t_out-zmsb + t_out-kzbf + t_out-ptsb + t_out-bpbj + t_out-gccl ) * ( 1 - zhbl2 / 100 ).
    IF t_out-auart = 'ZF1'.
      t_out-yjbf = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZP08' CHANGING kbetrtmp.
    t_out-rjje = kbetrtmp .
    kbetrtmp = 0.
    t_out-rjbftotal =  t_out-rjje  * ( 1 - zhbl2 / 100 ).
    IF t_out-auart = 'ZF1'.
      t_out-rjbftotal = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZP09' CHANGING kbetrtmp.
    t_out-azts = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZP10' CHANGING kbetrtmp.
    t_out-ysbx = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv   'ZP11' CHANGING kbetrtmp.
    t_out-gjgfy = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv   'ZP12' CHANGING kbetrtmp.
    t_out-zsfy = kbetrtmp.
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZP13' CHANGING kbetrtmp.
    t_out-bxgc = kbetrtmp .
    kbetrtmp = 0.
    t_out-gcbf = ( t_out-azts + t_out-ysbx + t_out-gjgfy + t_out-zsfy + t_out-bxgc  ) * ( 1 - zhbl2 / 100 ).
    IF t_out-auart = 'ZF1'.
      t_out-gcbf = 0.
    ENDIF.
    PERFORM comput1 USING t_out-knumv  'ZE01' CHANGING kbetrtmp.
    t_out-bzsf = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE02' CHANGING kbetrtmp.
    t_out-pxsf = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE03' CHANGING kbetrtmp.
    t_out-sjsf = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE04' CHANGING kbetrtmp.
    t_out-tscpyf = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE05' CHANGING kbetrtmp.
    t_out-tsypzz = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE06' CHANGING kbetrtmp.
    t_out-zbgl = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE07' CHANGING kbetrtmp.
    t_out-xmcb = kbetrtmp.
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE08' CHANGING kbetrtmp.
    t_out-zbfw = kbetrtmp .
    kbetrtmp = 0.
    PERFORM comput1 USING t_out-knumv  'ZE09' CHANGING kbetrtmp.
    t_out-qtsf = kbetrtmp .
    kbetrtmp = 0.
    "add :其他收费统计条件类型ZE09 + ZRB2 . by it02&魏云 20170726 begin
    PERFORM comput1 USING t_out-knumv  'ZRB2' CHANGING kbetrtmp.
    t_out-qtsf =  t_out-qtsf +  kbetrtmp .
    kbetrtmp = 0.
    "add :其他收费统计条件类型ZE09 + ZRB2 . by it02&魏云 20170726 end
    t_out-qtbftotal = ( t_out-bzsf + t_out-pxsf + t_out-sjsf +  t_out-tscpyf + t_out-tsypzz + t_out-zbgl + t_out-xmcb + t_out-zbfw + t_out-qtsf ) * ( 1 - zhbl2 / 100 ).
    IF t_out-auart = 'ZF1'.
      t_out-qtbftotal = 0.
    ENDIF.
    " PERFORM COMPUT2 USING T_OUT-KNUMV  'ZRA2' CHANGING KBETRTMP.
    PERFORM comput3 USING t_out-knumv  'ZRA2' CHANGING kbetrtmp. "MODIYF BY  IT02  根据 KNUMV + KSCHL + KHEKR（C) 条件
    t_out-zjzk = kbetrtmp .
    kbetrtmp = 0.
    t_out-xmhftotal = t_out-yjbf +  t_out-rjbftotal +  t_out-gcbf  +   t_out-qtbftotal + t_out-zjzk.
    IF t_out-auart = 'ZSO'.
      PERFORM comput1 USING t_out-knumv  'ZR02' CHANGING kbetrtmp.
      t_out-xmhftotal = kbetrtmp .
      kbetrtmp = 0.
    ENDIF.
    DATA: p1 TYPE p LENGTH 5 DECIMALS 2 VALUE '1.17'.
    CLEAR kschltmp.
    SELECT SINGLE kschl INTO kschltmp FROM konv WHERE knumv = t_out-knumv AND ( kschl ='ZWS1' OR kschl = 'ZWSI').
    IF kschltmp <> ''.
      t_out-xmjztotal  = t_out-xmhftotal /  p1 .
    ELSE.
      READ TABLE t_vbkd WITH KEY vbeln = t_out-vbeln . .
      IF  t_vbkd-inco1 = 'EXW' .
        t_out-xmjztotal  = t_out-xmhftotal / p1 .
      ELSE.
        t_out-xmjztotal  = t_out-xmhftotal.
      ENDIF.
    ENDIF.

    SELECT SINGLE waers INTO t_out-hblx1 FROM konv WHERE knumv = t_out-knumv AND kschl IN ('ZNE1', 'ZNET' ) AND waers NE ''.
    IF t_out-hblx1 = 'CNY'.
      t_out-hl1 = 1.
    ELSE.
      SELECT SINGLE ukurs INTO t_out-hl1 FROM tcurr WHERE kurst = 'PEND' AND fcurr = t_out-hblx1.
    ENDIF.

    t_out-xmjztotalrmb = t_out-xmjztotal * t_out-hl1.
    PERFORM comput2 USING t_out-knumv  'ZA01' CHANGING kbetrtmp.
    t_out-orbx = kbetrtmp .
    kbetrtmp = 0.
    SELECT SINGLE waers INTO t_out-hblx2 FROM konv WHERE knumv = t_out-knumv AND kschl = 'ZA01' AND waers NE ''.
    IF t_out-hblx2 = 'CNY'.
      t_out-huilv2 = 1.
    ELSE.
      SELECT SINGLE ukurs INTO t_out-huilv2 FROM tcurr WHERE kurst = 'PEND' AND fcurr = t_out-hblx2.
    ENDIF.

    t_out-orbxrmb = t_out-orbx * t_out-huilv2.
    t_out-jl1je = z1 / 100  * t_out-xmhftotal.
    t_out-jl2je = z2 / 100  * t_out-xmhftotal.
    t_out-jbr1je = z3 / 100 * t_out-xmhftotal.
    t_out-jbr2je = z4 / 100  * t_out-xmhftotal.
    t_out-jbr3je = z5 / 100  * t_out-xmhftotal.
*增加合同开始日期和结束日期 20171121 IT02 郭雷
    READ TABLE lt_veda WITH KEY vbeln = t_out-vbeln.
    IF sy-subrc = 0.
      t_out-vbegdat = lt_veda-vbegdat.
      t_out-venddat = lt_veda-venddat.
    ENDIF.
    MODIFY t_out.
  ENDLOOP.
  IF g_back NE 'X'.
    LOOP AT t_out.
      "判断查看创建人的权限
      AUTHORITY-CHECK OBJECT 'Z_SD_USER'
*            ID 'VKORG' FIELD 'DUMMY'
               ID 'USR20_1' FIELD t_out-ernam.
      IF sy-subrc <> 0.
        IF t_out-ernam NE sy-uname.
          DELETE t_out.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT t_out.
      "填写数据到数据表
      MOVE-CORRESPONDING t_out TO gt_zsd010.
      APPEND gt_zsd010.
    ENDLOOP.
  ENDIF.


ENDFORM.
FORM menu_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' .
ENDFORM.
FORM frm_user_command USING l_ucomm LIKE sy-ucomm
                            l_selfield TYPE slis_selfield.

  DATA: l_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  READ TABLE t_out INDEX l_selfield-tabindex.
  CHECK sy-subrc = 0.
  CASE l_ucomm.
    WHEN '&IC1'.
      SET PARAMETER ID  'AUN' FIELD t_out-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
  ENDCASE.
  l_selfield-refresh = 'X'.     "刷新ALV的数据
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0473   text
*      -->P_0474   text
*      -->P_0475   text
*      -->P_0476   text
*      -->P_0477   text
FORM append_fieldcat  USING name
                            text
                            ref_tabname
                            ref_fieldname
                            cfieldname.
  "     PLEN.
  it_fieldcat-fieldname = name.
  it_fieldcat-seltext_l    =
  it_fieldcat-seltext_m    =
  it_fieldcat-seltext_s    =
  it_fieldcat-reptext_ddic = text.
  it_fieldcat-ref_tabname = ref_tabname.
  it_fieldcat-ref_fieldname = ref_fieldname.
  it_fieldcat-cfieldname = cfieldname.
  " IT_FIELDCAT-OUTPUTLEN = PLEN.
  APPEND it_fieldcat.
  CLEAR it_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GETVKORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getvkorg .
  SELECT vkorg vtext FROM tvkot
  INTO CORRESPONDING FIELDS OF  TABLE t_vkorg
  WHERE spras = sy-langu AND  vkorg NE '0001'.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VKORG' "大写,可选值内表的字段名
      value_org       = 'S' "就写'S'
      dynpprog        = sy-repid "返回的输入框所在的MAIN PROGRAM
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'P_ZZB' "返回的输入框名
      window_title    = '销售组织'
    TABLES
      value_tab       = t_vkorg "可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM getauart .
  SELECT auart bezei FROM tvakt
  INTO CORRESPONDING FIELDS OF  TABLE t_auart
  WHERE spras = sy-langu AND  auart IN ('ZPO','ZZG','ZWV','ZF1','ZSO').
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUART' "大写,可选值内表的字段名
      value_org       = 'S' "就写'S'
      dynpprog        = sy-repid "返回的输入框所在的MAIN PROGRAM
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'P_AUART' "返回的输入框名
      window_title    = '订单类型'
    TABLES
      value_tab       = t_auart "可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COMPUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_OUT_KNUMV  text
*      -->P_2175   text
*      <--P_KBETRTMP  text
*----------------------------------------------------------------------*
FORM comput  USING    p_knumv LIKE konv-knumv p_vbeln LIKE vbak-vbeln  p_kschl LIKE konv-kschl CHANGING p_kbetr LIKE t_out-qtsf.
  CLEAR p_kbetr.
  LOOP AT t3_konv WHERE knumv = p_knumv AND kschl = p_kschl.
    READ TABLE t_vbap WITH KEY vbeln = p_vbeln posnr = t3_konv-kposn.
    IF sy-subrc  = 0.
      p_kbetr = p_kbetr + t3_konv-kbetr * t_vbap-zmeng.
    ELSE.
      p_kbetr = p_kbetr + t3_konv-kbetr.
    ENDIF.


  ENDLOOP.
ENDFORM.
FORM comput1  USING    p_knumv LIKE konv-knumv   p_kschl LIKE konv-kschl CHANGING p_kbetr LIKE t_out-qtsf.
  CLEAR p_kbetr.
  LOOP AT t3_konv WHERE knumv = p_knumv AND kschl = p_kschl.

    p_kbetr = p_kbetr + t3_konv-kbetr.



  ENDLOOP.
ENDFORM.
FORM comput2  USING    p_knumv LIKE konv-knumv   p_kschl LIKE konv-kschl CHANGING p_kbetr LIKE t_out-qtsf.
  CLEAR p_kbetr.
  LOOP AT t3_konv WHERE knumv = p_knumv AND kschl = p_kschl AND kposn = '000000'.

    p_kbetr = p_kbetr + t3_konv-kbetr.



  ENDLOOP.
ENDFORM.
FORM comput3  USING    p_knumv LIKE konv-knumv   p_kschl LIKE konv-kschl CHANGING p_kbetr LIKE t_out-qtsf.
  CLEAR p_kbetr.
  LOOP AT t3_konv WHERE knumv = p_knumv AND kschl = p_kschl AND (  kherk = 'C' OR kherk = 'G' ).

    p_kbetr = p_kbetr + t3_konv-kbetr.



  ENDLOOP.
ENDFORM.
FORM get_text  USING    VALUE(p_id)
               CHANGING p_text.
  DATA l_tdname TYPE thead-tdname.
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  l_tdname = t_out-vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = p_id
      language                = sy-langu
      name                    = l_tdname
      object                  = 'VBBK'
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
* Implement suitable error handling here
  ENDIF.
  IF t_tline[] IS NOT INITIAL.
    READ TABLE t_tline INDEX 1.
    p_text = t_tline-tdline.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COMPUTZB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_VBPA_ADRNR  text
*      <--P_T_OUT_JL1ZB  text
*      <--P_Z1  text
*----------------------------------------------------------------------*
FORM computzb  USING    p_adrnr LIKE adrc-addrnumber
               CHANGING p_jlzb LIKE t_out-jl1zb
                        p_z LIKE z1.
  DATA: mlen TYPE n.
  DATA:slx TYPE dd01v-datatype.
  READ TABLE t_adrc WITH KEY addrnumber = p_adrnr.
  IF sy-subrc = 0.
    IF t_adrc-city1 NE ''.
      CALL FUNCTION 'NUMERIC_CHECK'  "检查L_STR是否为全数字组合
        EXPORTING
          string_in = t_adrc-city1
        IMPORTING
          htype     = slx.
      .
      IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
      ENDIF.
      IF slx = 'NUMC'.
        p_z = t_adrc-city1.
        CONCATENATE  t_adrc-city1  '%' INTO p_jlzb.
      ELSE.
        mlen = strlen( t_adrc-city1 ) - 1 .
        IF t_adrc-city1+mlen(1) = '%'.
          CALL FUNCTION 'NUMERIC_CHECK'  "检查L_STR是否为全数字组合
            EXPORTING
              string_in = t_adrc-city1+0(mlen)
            IMPORTING
              htype     = slx.
          IF slx = 'NUMC'.
            p_z =  t_adrc-city1+0(mlen).
            IF t_adrc-city1 NE ''.
              p_jlzb = t_adrc-city1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
