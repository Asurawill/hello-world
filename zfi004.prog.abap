*&---------------------------------------------------------------------*
*& Report  ZFI004
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/11
*& Request       :
*& Descriptions  : 会计凭证查询和打印
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT zfi004.

************************************************************************
* Tables
************************************************************************
TABLES:bkpf,bseg,/bev1/rbvbak,rbkp .
*&--代码添加 BY HANDYBY 19.06.2017 17:45:47  BEGIN
TABLES coas .
*&--代码添加 BY HANDYBY 19.06.2017 17:45:47  END
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data.

TYPES:zbox   TYPE c.
        INCLUDE TYPE zfi004.
TYPES:name   TYPE adrp-name_last,
      name_1 TYPE adrp-name_last,
      kmms   TYPE char100, "打印科目描述
      name_b TYPE char100, "公司代码描述
      post1  TYPE ps_post1, "WBS描述
      "  XNEGP  TYPE XNEGP,    "反记账
      zpost1 TYPE proj-post1, "项目名称
      END OF ty_data.

*人员信息
TYPES:BEGIN OF ty_name,
        bname      TYPE usr21-bname,      "帐号
        persnumber TYPE usr21-persnumber, "人员编号
        name_last  TYPE adrp-name_last,   "姓
      END OF ty_name.
************************************************************************
* Internal Table * WorkArea
************************************************************************

DATA gt_zfi004 TYPE TABLE OF zfi004.
DATA gs_zfi004 TYPE zfi004.

DATA gt_data   TYPE TABLE OF ty_data.
DATA gs_data   TYPE ty_data.

DATA lt_data  TYPE TABLE OF ty_data.
DATA ls_data  TYPE ty_data.

DATA gt_name TYPE TABLE OF ty_name.
DATA gs_name TYPE ty_name.

DATA: gt_rbkp TYPE TABLE OF rbkp,
      gs_rbkp TYPE rbkp.

DATA gt_bkpf_re  TYPE TABLE OF bkpf.
DATA gs_bkpf_re  TYPE bkpf.


*data : BEGIN OF ty_pro,
*  projk(8) TYPE n,
*  END OF ty_pro.
*  data : gt_pro LIKE TABLE OF ty_pro,
*         gs_pro LIKE LINE OF gt_pro.

DATA : BEGIN OF ty_prps,
         pspnr TYPE prps-pspnr,
         psphi TYPE prps-psphi,
*         pspid TYPE proj-pspid,      "项目定义
       END OF ty_prps.
DATA : gt_prps LIKE TABLE OF ty_prps,
       gs_prps LIKE LINE OF gt_prps.



DATA : BEGIN OF ty_proj,
         pspnr TYPE proj-pspnr,
         post1 TYPE proj-post1,
*         pspid TYPE proj-pspid,      "项目定义
       END OF ty_proj.
DATA : gt_proj LIKE TABLE OF ty_proj,
       gs_proj LIKE LINE OF gt_proj.


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

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
   elseif gw_lvc-fieldname = 'WRBTR'..
   gw_lvc-CFIELDNAME = 'WAERS_1'.
   elseif gw_lvc-fieldname = 'DMBTR'.
   gw_lvc-CFIELDNAME = 'WAERS'.
ENDIF.



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
************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                s_gjahr FOR bkpf-gjahr OBLIGATORY NO INTERVALS NO-EXTENSION,"会计年度
                s_monat FOR bkpf-monat,"记账期间
                s_belnr FOR bkpf-belnr,"凭证编码
                s_bldat FOR bkpf-bldat,"凭证日期
                s_budat FOR bkpf-budat,"过账日期
                s_blart FOR bkpf-blart,"凭证类型
                s_ppnam FOR bkpf-ppnam,"制单人
                s_usnam FOR bkpf-usnam,"记账人
                s_hkont FOR bseg-hkont,"科目
                s_lifnr FOR bseg-lifnr,"供应商
                s_kunnr FOR bseg-kunnr,"客户
                s_kostl FOR bseg-kostl,"成本中心
*&--代码注释 BY HANDYBY 19.06.2017 17:44:16  BEGIN
*    s_aufnr FOR bseg-aufnr,"内部订单
*&--代码注释 BY HANDYBY 19.06.2017 17:44:16  END
*&--代码添加 BY HANDYBY 19.06.2017 17:44:22  BEGIN
  s_aufnr FOR COAS-aufnr MATCHCODE OBJECT orde ,"内部订单
*&--代码添加 BY HANDYBY 19.06.2017 17:44:22  END

                s_vbel2 FOR bseg-vbel2,"销售订单号
                s_dmbtr FOR bseg-dmbtr,"行项目本位币金额
                s_sgtxt FOR bseg-sgtxt,"文本
                s_zuonr FOR bseg-zuonr,"分配
                s_matnr FOR bseg-matnr,"物料号
                s_projk FOR /bev1/rbvbak-ps_psp_pnr."WBS元素

PARAMETERS:      p_yzpz AS CHECKBOX  TYPE char1, "预制凭证
                 p_cxpz AS CHECKBOX  TYPE char1 , "冲销凭证
                 p_dypz AS CHECKBOX  TYPE char1 , "需要打印的凭证
                 p_pzqd AS CHECKBOX  TYPE char1 . "凭证清单

SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_usnam-low.
  PERFORM getusnam.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_usnam-high.
  PERFORM getusnam.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*权限检查检查公司代码
  " PERFORM FRM_AUTH_CHECK USING '03'.
  PERFORM frm_auth_check.
  IF sy-subrc NE 0.
    MESSAGE i011(zfico01) WITH s_bukrs-low DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
**----------------对公司进行判断-------
*IF S_BUKRS-LOW EQ '1700' OR S_BUKRS-LOW EQ '1710' OR S_BUKRS-LOW EQ '1720'.
*    PERFORM frm_get_data. "取数逻辑
*  ELSE.
*    MESSAGE '公司不存在' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*ENDIF.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示


*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
  "FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
FORM frm_auth_check .
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD s_bukrs-low.
*  IF SY-SUBRC <> 0.
*   MESSAGE s899(mm) WITH '您没有公司代码' S_BUKRS '的权限' DISPLAY LIKE 'E'.
*  LEAVE LIST-PROCESSING.
*   ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  frm_get_data .
  RANGES:r_yzpz FOR bkpf-bstat,
         r_dypz FOR bkpf-belnr,
         r_pzqd FOR bkpf-belnr.

  REFRESH:r_yzpz ,
          r_dypz ,
          r_pzqd.

*根据CHECKBOX勾选内容进行排除

*预制凭证
  IF p_yzpz = 'X'.
    r_yzpz-sign = 'I'.
    r_yzpz-option = 'EQ'.
    r_yzpz-low = 'V'.
    r_yzpz-high = ''.
    APPEND r_yzpz.
  ENDIF.

*需要打印的凭证
  IF p_dypz = 'X'.
    r_dypz-sign = 'I'.
    r_dypz-option = 'BT'.
    r_dypz-low = '0010000000'.
    r_dypz-high = '0041999999'.
    APPEND r_dypz.

    r_dypz-sign = 'I'.
    r_dypz-option = 'BT'.
    r_dypz-low = '0070000000'.
    r_dypz-high = '0089999999'.
    APPEND r_dypz.

    r_dypz-sign = 'I'.
    r_dypz-option = 'BT'.
    r_dypz-low = '0096000000'.
    r_dypz-high = '0096999999'.
    APPEND r_dypz.
  ENDIF.

*凭证清单
  IF p_pzqd = 'X'.
    r_pzqd-sign = 'I'.
    r_pzqd-option = 'BT'.
    r_pzqd-low = '0048000000'.
    r_pzqd-high = '0059999999'.
    APPEND r_pzqd.

    r_pzqd-sign = 'I'.
    r_pzqd-option = 'BT'.
    r_pzqd-low = '0097000000'.
    r_pzqd-high = '0098999999'.
    APPEND r_pzqd.
  ENDIF.

*冲销凭证
  IF p_cxpz = 'X'.

    SELECT * FROM zfi004
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      WHERE bukrs IN s_bukrs
      AND   gjahr IN s_gjahr
      AND   monat IN s_monat
      AND   belnr IN s_belnr
      AND   bldat IN s_bldat
      AND   budat IN s_budat
      AND   blart IN s_blart
      AND   blart NE 'RE'
      AND   ppnam IN s_ppnam
      AND   usnam IN s_usnam
      AND   hkont IN s_hkont
      AND   lifnr IN s_lifnr
      AND   kunnr IN s_kunnr
      AND   kostl IN s_kostl
      AND   aufnr IN s_aufnr
      AND   vbel2 IN s_vbel2
      AND   dmbtr IN s_dmbtr
      AND   sgtxt IN s_sgtxt
      AND   bktxt IN s_sgtxt
      AND   bstat IN r_yzpz
      AND   belnr IN r_dypz
      AND   belnr IN r_pzqd
      AND   zuonr IN s_zuonr
      AND   matnr IN s_matnr
      AND   projk IN s_projk
      AND   stblg <> ''.
    "先追加RE凭证类型
    IF 'RE' IN s_blart .
      SELECT * FROM zfi004
      APPENDING CORRESPONDING FIELDS OF TABLE gt_data
      WHERE bukrs IN s_bukrs
      AND   gjahr IN s_gjahr
      AND   monat IN s_monat
      AND   belnr IN s_belnr
      AND   bldat IN s_bldat
      AND   budat IN s_budat
      AND   blart EQ 'RE'
      AND   usnam IN s_usnam
      AND   ppnam IN s_ppnam
      AND   hkont IN s_hkont
      AND   lifnr IN s_lifnr
      AND   kunnr IN s_kunnr
      AND   kostl IN s_kostl
      AND   aufnr IN s_aufnr
      AND   vbel2 IN s_vbel2
      AND   dmbtr IN s_dmbtr
      AND   sgtxt IN s_sgtxt
      AND   bktxt IN s_sgtxt
      AND   bstat IN r_yzpz
      AND   belnr IN r_dypz
      AND   belnr IN r_pzqd
      AND   zuonr IN s_zuonr
      AND   matnr IN s_matnr
      AND   projk IN s_projk .
    ENDIF.
  ELSE.
    SELECT * FROM zfi004
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      WHERE bukrs IN s_bukrs
      AND   gjahr IN s_gjahr
      AND   monat IN s_monat
      AND   belnr IN s_belnr
      AND   bldat IN s_bldat
      AND   budat IN s_budat
      AND   blart IN s_blart
      AND   ppnam IN s_ppnam
      AND   usnam IN s_usnam
      AND   hkont IN s_hkont
      AND   lifnr IN s_lifnr
      AND   kunnr IN s_kunnr
      AND   kostl IN s_kostl
      AND   aufnr IN s_aufnr
      AND   vbel2 IN s_vbel2
      AND   dmbtr IN s_dmbtr
      AND   sgtxt IN s_sgtxt
      AND   bktxt IN s_sgtxt
      AND   zuonr IN s_zuonr
      AND   bstat IN r_yzpz
      AND   belnr IN r_dypz
      AND   belnr IN r_pzqd
      AND   matnr IN s_matnr
      AND   projk IN s_projk.
  ENDIF.

  "排除 “仅查询需要打印的凭证”的类型为“Z”的明细
  IF p_dypz = 'X'.

    DELETE gt_data WHERE bstat = 'Z' .

  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_bkpf_re
       FROM bkpf
       WHERE bukrs = s_bukrs-low AND gjahr = s_gjahr-low  AND blart = 'RE' AND awtyp = 'RMRP'.
  SORT gt_bkpf_re BY awkey .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_rbkp
    FROM rbkp
    WHERE gjahr = s_gjahr-low AND bukrs = s_bukrs-low AND  stblg NE '' AND stjah NE ''.
  SORT gt_rbkp BY  belnr gjahr .
*获取制单人姓名
  SELECT  * FROM usr21
   INNER JOIN adrp
   ON usr21~persnumber = adrp~persnumber
  INTO CORRESPONDING FIELDS OF TABLE gt_name.








*ADD BY HANDWY 2015 7-20 排除重复项
  SORT gt_data BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_data COMPARING bukrs belnr gjahr buzei.
*ENDADD.

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
  DATA:re_awkey LIKE bkpf-awkey.
  DATA:sindex TYPE i.

  LOOP AT gt_data INTO gs_data.


    CLEAR:sindex.
    sindex = sy-tabix. "记录当前循环到的行数
    "判断只打印冲销凭证是否选中并类型为RE  add IT02 151211  begin
    IF p_cxpz EQ 'X' AND gs_data-blart = 'RE'.
      READ TABLE gt_rbkp INTO gs_rbkp WITH KEY belnr = gs_data-awkey+0(10)  gjahr = gs_data-awkey+10(4) BINARY SEARCH..
      IF sy-subrc NE 0.
        DELETE gt_data INDEX sindex.
        CONTINUE.

      ENDIF.
    ENDIF.
    IF gs_data-blart = 'RE'.
      CLEAR:re_awkey .
      READ TABLE gt_rbkp INTO gs_rbkp WITH KEY belnr = gs_data-awkey+0(10)  gjahr = gs_data-awkey+10(4) BINARY SEARCH..
      IF sy-subrc EQ 0.
        CONCATENATE gs_rbkp-stblg gs_rbkp-stjah INTO re_awkey .
        "冲销凭证读取
        READ TABLE gt_bkpf_re INTO gs_bkpf_re WITH KEY awkey = re_awkey ..
        IF sy-subrc = 0.
          gs_data-stblg = gs_bkpf_re-belnr.
        ENDIF.
      ENDIF.
    ENDIF.
    "add  it02 151201 end
*公司名
    gs_data-name_b = gs_data-butxt.

*记账人,审核人
    READ TABLE gt_name INTO gs_name
    WITH KEY bname = gs_data-usnam.
    IF sy-subrc = 0.
      gs_data-name = gs_name-name_last.
    ENDIF.

*制单人
    READ TABLE gt_name INTO gs_name
    WITH KEY bname = gs_data-ppnam.
    IF sy-subrc = 0.
      gs_data-name_1 = gs_name-name_last.
    ENDIF.

*如果制单人为空就取记账人
    IF gs_data-name_1 IS INITIAL.
      gs_data-name_1 = gs_data-name.
    ENDIF.

*打印科目描述
    gs_data-kmms = gs_data-txt20."科目描述

*科目描述除了输出科目描述外，还需输出 客户/供应商/成本中心/销售订单/内部订单
    IF  gs_data-kunnr  IS NOT INITIAL.
      CONCATENATE gs_data-kmms '/' gs_data-kunnr gs_data-name1 INTO gs_data-kmms.

    ELSEIF gs_data-lifnr IS NOT INITIAL.
      CONCATENATE gs_data-kmms '/' gs_data-lifnr gs_data-name1_1 INTO gs_data-kmms.

    ELSEIF gs_data-kostl IS NOT INITIAL.
      CONCATENATE gs_data-kmms '/' gs_data-kostl gs_data-ktext INTO gs_data-kmms.

    ELSEIF gs_data-vbel2 IS NOT INITIAL.
      CONCATENATE gs_data-kmms '/' gs_data-vbel2  INTO gs_data-kmms.

    ELSEIF gs_data-aufnr IS NOT INITIAL.
      CONCATENATE gs_data-kmms '/' gs_data-aufnr '/' gs_data-ktext_1 INTO gs_data-kmms.
    ENDIF.
    IF gs_data-anln1 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gs_data-anln1
        IMPORTING
          output = gs_data-anln1.
      CONCATENATE gs_data-kmms '/' gs_data-anln1 '/' gs_data-txt50 INTO gs_data-kmms.

    ENDIF.

*WBS内码转换WBS元素
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = gs_data-projk
      IMPORTING
        output = gs_data-post1.
    "本币金额 和 凭证货币 金额，要根据 借贷方（BSEG-SHKZG）判断：若借贷方=S，则不变；若借贷方 = H ，则乘以 -1
    IF gs_data-shkzg = 'H'.
      gs_data-dmbtr = gs_data-dmbtr * -1 .
      gs_data-wrbtr = gs_data-wrbtr * -1.
    ENDIF.
*-----------------项目名称-----------------------------------------------------------------
    DATA:
          projk_a TYPE prps-pspnr.
    projk_a = gs_data-projk.
    SELECT
     pspnr
     psphi
    FROM prps
   INTO CORRESPONDING FIELDS OF TABLE gt_prps
    WHERE pspnr = projk_a.

*   LOOP AT gt_prps INTO gs_prps.
*      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
*        EXPORTING
*          input  = gs_prps-psphi
*        IMPORTING
*          output = gs_prps-pspid
**         PSELT  =
*        .
*      MODIFY gt_prps FROM gs_prps.
*    ENDLOOP.

    IF gt_prps IS NOT INITIAL.
      SELECT
        pspnr
        post1
*        pspid
        FROM proj
        INTO CORRESPONDING FIELDS OF TABLE gt_proj
        FOR ALL ENTRIES IN gt_prps
        WHERE pspnr = gt_prps-psphi.
    ENDIF.

    READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = projk_a.
    IF  sy-subrc = 0.
      READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-psphi.
      IF sy-subrc = 0.
        gs_data-zpost1 = gs_proj-post1.
      ENDIF.
    ENDIF.


    MODIFY gt_data FROM gs_data.
    CLEAR gs_data.
    CLEAR projk_a.
  ENDLOOP.

  SORT gt_data BY gjahr belnr .
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
  gw_layout-box_fname     = 'ZBOX'.
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
  init_fieldcat 'BUKRS'   text-001         '' '' '' '' '' '' ''.
  init_fieldcat 'BELNR'   text-002         '' '' '' '' 'X' '' ''.
  init_fieldcat 'GJAHR'   text-003         '' '' '' '' '' '' ''.
  init_fieldcat 'BLART'   text-004         '' '' '' '' '' '' ''.
  init_fieldcat 'BSTAT'   text-005         '' '' '' '' '' '' ''.
  init_fieldcat 'BLDAT'   text-006         '' '' '' '' '' '' ''.
  init_fieldcat 'BUDAT'   text-007         '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'   text-008        '' '' '' '' '' '' ''.
  init_fieldcat 'XBLNR'   text-009         '' '' '' '' '' '' ''.
  init_fieldcat 'PPNAM'   text-010         '' '' '' '' '' '' ''.
  init_fieldcat 'USNAM'   text-011         '' '' '' '' '' '' ''.
  init_fieldcat 'BKTXT'   text-012         '' '' '' '' '' '' ''.
  init_fieldcat 'STBLG'   text-013         '' '' '' '' '' '' ''.
  init_fieldcat 'BUZEI'   text-014         '' '' '' '' '' '' ''.
  init_fieldcat 'SHKZG'   text-015         '' '' '' '' '' '' ''.
  init_fieldcat 'XNEGP'   text-048       '' '' '' '' '' '' ''."反记账
  init_fieldcat 'HKONT'   text-016         '' '' '' '' '' '' ''.
  init_fieldcat 'TXT20'   text-017         '' '' '' '' '' '' ''.
  init_fieldcat 'DMBTR'   text-018         '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'   text-019         '' '' '' '' '' '' ''.
  init_fieldcat 'WRBTR'   text-020         '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS_1' text-021         '' '' '' '' '' '' ''.
  init_fieldcat 'KUNNR'   text-022         '' '' '' '' '' '' ''.
  init_fieldcat 'NAME1'   text-023         '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR'   text-024         '' '' '' '' '' '' ''.
  init_fieldcat 'NAME1_1' text-025         '' '' '' '' '' '' ''.
  init_fieldcat 'ANLN1'   text-026         '' '' '' '' '' '' ''.
  init_fieldcat 'TXT50'   text-027         '' '' '' '' '' '' ''.
  init_fieldcat 'KOSTL'   text-028         '' '' '' '' '' '' ''.
  init_fieldcat 'KTEXT'   text-029         '' '' '' '' '' '' ''.
  init_fieldcat 'AUFNR'   text-030         '' '' '' '' '' '' ''.
  init_fieldcat 'KTEXT_1' text-031         '' '' '' '' '' '' ''.
  init_fieldcat 'VBEL2'   text-032         '' '' '' '' '' '' ''.
  init_fieldcat 'POSN2'   text-033         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'   text-034         '' '' '' '' '' 'MARA' 'MATNR'.
  init_fieldcat 'MAKTX'   text-035         '' '' '' '' '' '' ''.
  init_fieldcat 'MATKL'   text-036         '' '' '' '' '' '' ''.
  init_fieldcat 'WGBEZ'   text-037         '' '' '' '' '' '' ''.
  init_fieldcat 'RSTGR'   text-038         '' '' '' '' '' 'BSEG' 'RSTGR'.
  init_fieldcat 'TXT40'   text-047         '' '' '' '' '' '' ''. "150730 add by it02
  init_fieldcat 'SGTXT'   text-039         '' '' '' '' '' '' ''.
  init_fieldcat 'ZUONR'   text-040         '' '' '' '' '' '' ''.
  init_fieldcat 'AWKEY'   text-041         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE'   text-042         '' '' '' '' '' '' ''. "数量
  init_fieldcat 'MEINS'   text-043         '' '' '' '' '' '' ''. "单位
  init_fieldcat 'POST1'   text-044        '' '' '' '' '' '' ''."工作分解结构元素
  init_fieldcat 'ZPOST1'  text-051        '' '' '' '' '' '' ''."项目名称
  init_fieldcat 'EBELN'   text-045         '' '' '' '' '' '' ''. "采购订单
  init_fieldcat 'EBELP'   text-046        '' '' '' '' '' '' ''."采购订单项目
  init_fieldcat 'FKBER'   text-049        '' '' '' '' '' '' ''."功能范围
  init_fieldcat 'XREF3'   text-050        '' '' '' '' '' '' ''."参考码3

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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
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
  REFRESH gt_exclude.
  CLEAR gs_exclude.
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
  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.
  APPEND gw_events TO gt_events.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS

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


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
      READ TABLE gt_data INTO gs_data INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'BELNR'
        AND gs_data-bstat <> 'V'
        AND gs_data-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD gs_data-belnr.
        SET PARAMETER ID 'BUK' FIELD gs_data-bukrs.
        SET PARAMETER ID 'GJR' FIELD gs_data-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

      READ TABLE gt_data INTO gs_data INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'BELNR'
        AND gs_data-bstat = 'V'
        AND gs_data-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLP' FIELD gs_data-belnr.
        SET PARAMETER ID 'BUK' FIELD gs_data-bukrs.
        SET PARAMETER ID 'GJR' FIELD gs_data-gjahr.
        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
      ENDIF.
*打印
    WHEN '&PRNT'.
*需进行保存操作
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = g_ref_grid.

      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0.

*        -------------- 13074 ------------------ new change --------
        IF gs_data-bukrs = '1700' or gs_data-bukrs = '1710' or gs_data-bukrs = '1720'.
          PERFORM frm_print_data1.
        ELSE.
          PERFORM frm_print_data.
        ENDIF.
*        -----------------------------------------------------------
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM frm_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  PERFORM frm_data_enter USING er_data_changed..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_data_enter USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDFORM.                    "frm_data_enter
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 9,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFFI004'.
  DATA l_line TYPE i. "统计打印的行进行补行
  DATA g_line TYPE i. "设定换页行数
  DATA g_all_s TYPE dmbtr.
  DATA g_all_h TYPE dmbtr.
  DATA:
        Z_BUKRS TYPE BKPF-BUKRS.
        Z_BUKRS = S_BUKRS-LOW.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "smartforms的名字
    IMPORTING
      fm_name            = g_name                "对应的smartforms的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open  = 'X'.
  control-no_close = 'X'.
* Start Printing

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
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

*根据凭证编码进行排序

*对于任凭证,选中一行打印整张凭证.
  LOOP AT  gt_data INTO gs_data WHERE zbox = 'X'.
    LOOP AT  gt_data INTO gs_data
      WHERE belnr = gs_data-belnr
      AND   gjahr = gs_data-gjahr
      AND   bukrs = gs_data-bukrs.
*      IF GS_DATA-ZPOST1 IS INITIAL.
*           GS_DATA-ZPOST1 = gs_data-KTEXT_1.
*          ELSE.
*
*      ENDIF.
      gs_data-zbox = 'X'.
      MODIFY gt_data FROM gs_data.
    ENDLOOP.

  ENDLOOP.

  LOOP AT gt_data INTO gs_data WHERE zbox = 'X'.
    AT NEW belnr.
      REFRESH lt_data.
      CLEAR l_line.
      CLEAR g_all_s.
      CLEAR g_all_h.
    ENDAT.

*汇总借贷金额
    IF gs_data-shkzg = 'S'.
      g_all_s = g_all_s + gs_data-dmbtr.
    ELSEIF gs_data-shkzg = 'H'.
      g_all_h = g_all_h + gs_data-dmbtr.
    ENDIF.

    APPEND gs_data TO lt_data.
    CLEAR gs_data.

    AT END OF belnr.
      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
          g_line             = g_line
          g_all_s            = g_all_s
          g_all_h            = g_all_h
          G_BUKRS            = Z_BUKRS
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.
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
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GETPPNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getusnam .
  DATA:BEGIN OF ws_usnam,
         bname      LIKE user_addr-bname,
         name_textc LIKE user_addr-name_textc,
       END OF ws_usnam.
  DATA:ts_usnam LIKE TABLE OF ws_usnam.
  DATA:l_bukrs LIKE t001-bukrs.
  DATA:bukrs1_name TYPE  char30.
  DATA:bukrs2_name TYPE  char30 .
  PERFORM frm_get_field_value USING 'S_BUKRS-LOW' CHANGING l_bukrs.
  CONCATENATE l_bukrs 'FI%' INTO bukrs1_name .
  CONCATENATE l_bukrs 'CO%' INTO bukrs2_name .
  SELECT bname name_textc  INTO CORRESPONDING FIELDS OF TABLE ts_usnam
     FROM user_addr
  WHERE bname LIKE bukrs1_name  OR bname LIKE bukrs2_name.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BNAME' "大写,可选值内表的字段名
      value_org       = 'S' "就写'S'
      dynpprog        = sy-repid "返回的输入框所在的main program
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'S_USNAM' "返回的输入框名
      window_title    = '记账人'
    TABLES
      value_tab       = ts_usnam "可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM frm_get_field_value USING VALUE(f_fieldname) CHANGING VALUE(f_fieldvalue).
  DATA: dynpro_values TYPE TABLE OF dynpread WITH HEADER LINE,
        field_value   LIKE LINE OF dynpro_values.

  CLEAR: field_value, dynpro_values[].
  field_value-fieldname = f_fieldname.
  APPEND field_value TO dynpro_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpro_values.

  CHECK NOT dynpro_values[]  IS INITIAL.
  READ TABLE dynpro_values INDEX 1.
  CHECK sy-subrc = 0.
  f_fieldvalue =  dynpro_values-fieldvalue.
ENDFORM.                    "frm_get_field_value
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA1          ----  13074  insert --
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data1 .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 9,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFFI004_2'.
  DATA l_line TYPE i. "统计打印的行进行补行
  DATA g_line TYPE i. "设定换页行数
  DATA g_all_s TYPE dmbtr.
  DATA g_all_h TYPE dmbtr.
  DATA:
        Z_BUKRS TYPE BKPF-BUKRS.
        Z_BUKRS = S_BUKRS-LOW.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "smartforms的名字
    IMPORTING
      fm_name            = g_name                "对应的smartforms的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open  = 'X'.
  control-no_close = 'X'.
* Start Printing

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
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

*根据凭证编码进行排序

*对于任凭证,选中一行打印整张凭证.
  LOOP AT  gt_data INTO gs_data WHERE zbox = 'X'.
    LOOP AT  gt_data INTO gs_data
      WHERE belnr = gs_data-belnr
      AND   gjahr = gs_data-gjahr
      AND   bukrs = gs_data-bukrs.
*      IF GS_DATA-ZPOST1 IS INITIAL.
*           GS_DATA-ZPOST1 = gs_data-KTEXT_1.
*          ELSE.
*
*      ENDIF.
      gs_data-zbox = 'X'.
      MODIFY gt_data FROM gs_data.
    ENDLOOP.

  ENDLOOP.

  LOOP AT gt_data INTO gs_data WHERE zbox = 'X'.
    AT NEW belnr.
      REFRESH lt_data.
      CLEAR l_line.
      CLEAR g_all_s.
      CLEAR g_all_h.
    ENDAT.

*汇总借贷金额
    IF gs_data-shkzg = 'S'.
      g_all_s = g_all_s + gs_data-dmbtr.
    ELSEIF gs_data-shkzg = 'H'.
      g_all_h = g_all_h + gs_data-dmbtr.
    ENDIF.

    APPEND gs_data TO lt_data.
    CLEAR gs_data.

    AT END OF belnr.
      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
          g_line             = g_line
          g_all_s            = g_all_s
          g_all_h            = g_all_h
          G_BUKRS            = Z_BUKRS
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.
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
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
