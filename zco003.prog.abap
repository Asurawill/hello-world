REPORT zco003.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160902
*& Request       :
*& Descriptions  : 生产订单用料表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:aufk,afpo,afko,mara,likp .
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_main,
        zbox ,
        aufnr     TYPE afpo-aufnr,  "生产订单
        matnr     TYPE afpo-matnr,  "物料
        maktx     TYPE makt-maktx,  "物料描述
        werks     TYPE afpo-dwerk,  "工厂
        ddgb      TYPE tj02t-txt04,  "订单关闭
        ddsh      TYPE tj02t-txt04,   "订单收货
        ftrmi     TYPE afko-ftrmi,  "订单下达日期
        getri     TYPE afko-getri,  "订单完成日期
        gamng     TYPE afko-gamng,   "订单按数量
        wemng     TYPE afpo-wemng,    "入库数量
        igmng     TYPE afko-igmng,    "报工数量
        gmein     TYPE afko-gmein,    "基本计量单位
        wadat_ist TYPE likp-wadat_ist, "发货日期
        kdauf     TYPE afpo-kdauf,   "销售订单号
        kdpos     TYPE afpo-kdpos,  "销售订单行号
        xmmc      TYPE string,      "项目名称
        dispo     TYPE afko-dispo,  "MRP控制者
        dsnam     TYPE t024d-dsnam,  "控制着名称
        fevor     TYPE afko-fevor,   "生产管理员
        fevor_txt TYPE t024f-txt,  "生产管理院内文本
        stprs     TYPE ckmlcr-stprs, "标准价
        pvprs     TYPE ckmlcr-pvprs,  "实际价
        peinh     TYPE ckmlcr-peinh,   "价格单位
        bzclcb    TYPE cosp-wog016,    "标准材料成本（完工时）
        jhclcb    TYPE cosp-wog016,  "计划材料成本
        tlcb      TYPE cosp-wog016,  "投料成本
        sjclcb    TYPE cosp-wog016,  "实际材料成本
        bzjhcy    TYPE cosp-wog016,  "标准&计划差异
        jhtlcy    TYPE cosp-wog016,  "计划&投料差异
        bzsjcy    TYPE cosp-wog016,  "标准&实际差异
        kalnr     TYPE ckmlprkeph-kalnr, "成本估算号
        objnr     TYPE aufk-objnr,
        zsel,
      END OF  ty_main .


TYPES:BEGIN OF ty_sub,
        aufnr     TYPE afpo-aufnr,  "生产订单
        objnr     TYPE objnr,       "对象号
        werks     TYPE afpo-dwerk,  "工厂
        ddgb      TYPE tj02t-txt04,  "订单关闭
        ddsh      TYPE tj02t-txt04,   "订单收货
        matnr     TYPE afpo-matnr,  "母件物料
        maktx     TYPE makt-maktx,  "母件描述
        gamng     TYPE afko-gamng,   "订单数量
        wemng     TYPE afpo-wemng,   "入库数量
        gmein     TYPE afko-gmein,    "母件单位
        matnr_sub TYPE afpo-matnr,  "组件
        zjms      TYPE makt-maktx,   "组件描述
        zjdw      TYPE mara-meins ,  "组件单位
        bzbom     TYPE afko-gamng,   "标准BOM数量
        ddjhsl    TYPE afko-gamng,   "订单计划数量
        sjtlsl    TYPE afko-gamng,   "实际投料数量
        llhys     TYPE afko-gamng,   "按入库—理论耗用数
        zxsl      TYPE afko-gamng,   "在线数量
        ftrmi     TYPE afko-ftrmi,  "订单下达日期
        ql_cl     TYPE  afko-gamng,   "欠料/超领数
        zjbom     TYPE  c ,          "组件有BOM
        zjaufnr   TYPE  c,           "组件有生产订单
        bz_jh_c   TYPE afko-gamng,   "标准&计划数量差
        jh_sj_c   TYPE afko-gamng,   "计划&实际数量差
        ll_sj_c   TYPE afko-gamng,   "理论&实际数量差
        bz_cl_cb  TYPE cosp-wog016,  "标准材料成本（投料时）\
        jhcb      TYPE cosp-wog016,    "计划成本
        tlcb      TYPE cosp-wog016,  "投料成本（标准价X实际数量）
        sjclcb    TYPE cosp-wog016,   "实际材料成本（投料时）
        bz_jh_cy  TYPE cosp-wog016,    "标准&计划差异
        jh_tl_cy  TYPE cosp-wog016,     "计划&投料差异
        zxbzj     TYPE cosp-wog016,     "最新标准价
        zxsjj     TYPE cosp-wog016,    "最新实际价
        peinh     TYPE ckmlcr-peinh,   "价格单位
        kalnr     TYPE ckmlprkeph-kalnr, "成本估算号
        kdauf     TYPE resb-kdauf,   "销售订单
        kdpos     TYPE resb-kdpos,    "销售订单行项目
        zsel,
      END OF  ty_sub .

TYPES:BEGIN OF ty_data,
        aufnr     TYPE afpo-aufnr,  "生产订单
        matnr     TYPE afpo-matnr,  "物料
        maktx     TYPE makt-maktx,  "物料描述
        werks     TYPE afpo-dwerk,  "工厂
        ddgb      TYPE tj02t-txt04,  "订单关闭
        ddsh      TYPE tj02t-txt04,   "订单收货
        ftrmi     TYPE afko-ftrmi,  "订单下达日期
        getri     TYPE afko-getri,  "订单完成日期
        gamng     TYPE afko-gamng,   "订单按数量
        wemng     TYPE afpo-wemng,    "入库数量
        igmng     TYPE afko-igmng,    "报工数量
        gmein     TYPE afko-gmein,    "基本计量单位
        wadat_ist TYPE likp-wadat_ist, "发货日期
        kdauf     TYPE afpo-kdauf,   "销售订单号
        kdpos     TYPE afpo-kdpos,  "销售订单行号
        xmmc      TYPE string,      "项目名称
        dispo     TYPE afko-dispo,  "MRP控制者
        fevor     TYPE afko-fevor,   "生产管理员
        bzclcb    TYPE cosp-wog016,    "标准材料成本（完工时）
        kalnr     TYPE ckmlprkeph-kalnr, "成本估算号
        objnr     TYPE aufk-objnr,
      END OF ty_data.

TYPES:BEGIN OF ty_matnr,
        werks TYPE werks_d,   "工厂
        matnr TYPE matnr,     "物料号
        maktx TYPE maktx,     "物料描述
        matkl TYPE matkl,     "物料组
      END OF ty_matnr .

TYPES:BEGIN OF ty_kdauf,
        kdauf     TYPE kdauf,
        xmmc      TYPE string,
        wadat_ist TYPE likp-wadat_ist,  "交货日期
      END OF ty_kdauf .

TYPES:BEGIN OF ty_likp,
        vgbel     TYPE lips-vgbel,
        vbeln     TYPE likp-vbeln,
        wadat_ist TYPE likp-wadat_ist,
      END OF ty_likp .

TYPES:BEGIN OF ty_wlflz,
        werks TYPE afpo-dwerk,  "工厂
        matnr TYPE afpo-matnr,  "物料
        kdauf TYPE afpo-kdauf,   "销售订单号
        kdpos TYPE afpo-kdpos,  "销售订单行号
        kalnr TYPE ckmlhd-kalnr, "成本估算号
        "     getri     TYPE afko-getri,  "订单完成日期
        bdatj TYPE ckmlcr-bdatj, "会计年度
        poper TYPE ckmlcr-poper,  "会计期间
      END OF ty_wlflz .

TYPES:BEGIN OF ty_aufnr_sel,
        aufnr TYPE afko-aufnr,
        matnr TYPE mast-matnr,
        werks TYPE werks_d,
      END OF ty_aufnr_sel .

TYPES:BEGIN OF ty_matnr_sel,
        matnr TYPE mast-matnr,
        werks TYPE werks_d,
      END OF ty_matnr_sel .



DATA:gt_matnr_sel TYPE TABLE OF ty_matnr_sel,
     gs_matnr_sel TYPE ty_matnr_sel.


DATA:gt_aufnr_sel TYPE TABLE OF ty_aufnr_sel,
     gs_aufnr_sel TYPE ty_aufnr_sel.


DATA:gt_wlflz TYPE TABLE OF ty_wlflz,
     gs_wlflz TYPE ty_wlflz.

DATA:gt_ckmlhd TYPE TABLE OF ckmlhd,
     gs_ckmlhd TYPE ckmlhd.

DATA:gt_ckmlprkeph_s TYPE TABLE OF ckmlprkeph,
     gs_ckmlprkeph_s TYPE ckmlprkeph.

DATA:gt_ckmlprkeph_v TYPE TABLE OF ckmlprkeph,
     gs_ckmlprkeph_v TYPE ckmlprkeph.

DATA:gt_ckmlcr TYPE TABLE OF ckmlcr,
     gs_ckmlcr TYPE ckmlcr.

DATA:gt_likp TYPE TABLE OF ty_likp,
     gs_likp TYPE ty_likp.

DATA:gt_main TYPE TABLE OF ty_main,
     gs_main TYPE ty_main.

DATA:gt_main_sel TYPE TABLE OF ty_main,
     gs_main_sel TYPE ty_main.

DATA:gt_t024d TYPE TABLE OF t024d,
     gs_t024d TYPE t024d.

DATA:gt_t024f TYPE TABLE OF t024f,
     gs_t024f TYPE t024f.

DATA:gt_tj02t TYPE TABLE OF tj02t,
     gs_tj02t TYPE tj02t.

DATA:gt_jest_gb TYPE TABLE OF jest,
     gs_jest_gb TYPE jest.

DATA:gt_jest_sh TYPE TABLE OF jest,
     gs_jest_sh TYPE jest.

DATA:gt_jest TYPE TABLE OF jest,
     gs_jest TYPE jest.

DATA:gt_kdauf TYPE TABLE OF ty_kdauf,
     gs_kdauf TYPE ty_kdauf.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_data_2 TYPE TABLE OF ty_data,
     gs_data_2 TYPE ty_data.

DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.

TYPES:BEGIN OF ty_cosp_hj,
        objnr  TYPE cosp-objnr,
        jhclcb TYPE cosp-wog016,  "计划材料成本
        tlcb   TYPE cosp-wog016,  "投料成本
      END OF ty_cosp_hj .

DATA:gt_cosp_hj_1 TYPE TABLE OF ty_cosp_hj,
     gs_cosp_hj_1 TYPE ty_cosp_hj.

DATA:gt_cosp_hj_4 TYPE TABLE OF ty_cosp_hj,
     gs_cosp_hj_4 TYPE ty_cosp_hj.

DATA:gt_cosp_1 TYPE TABLE OF cosp,
     gs_cosp_1 TYPE cosp.

DATA:gt_cosp_4 TYPE TABLE OF cosp,
     gs_cosp_4 TYPE cosp.

DATA:gt_cosp_1_sub TYPE TABLE OF cosp,
     gs_cosp_1_sub TYPE cosp.

DATA:gt_cosp_4_sub TYPE TABLE OF cosp,
     gs_cosp_4_sub TYPE cosp.

DATA:gt_sub TYPE TABLE OF ty_sub,
     gs_sub TYPE ty_sub.


"*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.


DATA: g_objname TYPE thead-tdname.

*ALV 变量定义
DATA:wcl_container TYPE REF TO cl_gui_docking_container, " 自适应窗口
     gcl_alv       TYPE REF TO cl_gui_alv_grid, " ALV类
     gs_layout     TYPE        lvc_s_layo, " ALV样式
     gs_exclude    TYPE        ui_functions, " ALV排除按钮
     gt_fieldcat   TYPE        lvc_t_fcat, " ALV字段目录
     gt_sort       TYPE        lvc_t_sort. " ALV排序

DATA:wcl_container2 TYPE REF TO cl_gui_docking_container, " 自适应窗口
     gcl_alv2       TYPE REF TO cl_gui_alv_grid, " ALV类
     gs_layout2     TYPE        lvc_s_layo, " ALV样式
*     GS_EXCLUDE2 TYPE UI_FUNCTIONS, " ALV排除按钮
     gt_fieldcat2   TYPE        lvc_t_fcat, " ALV字段目录
     gt_sort2       TYPE        lvc_t_sort. " ALV排序

DATA:gt_bdcdata TYPE TABLE OF bdcdata,
     gw_bdcdata TYPE          bdcdata.

DATA:gt_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
*       CLASS GCL_EVENT_HANDLER_create DEFINITION
*----------------------------------------------------------------------*
* 处理ALV事件类声明 create
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
*双击事件
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
            e_column
            es_row_no,
*ALV工具栏增加新按钮
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive,
*实现用户命令
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm.
ENDCLASS.                    "gcl_event_handler_cu DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.

  ENDMETHOD.                    "handle_double_click

  METHOD handle_toolbar.
    PERFORM frm_handle_toolbar USING e_object e_interactive.
  ENDMETHOD.                    "HANDLE_TOOLBAR


  METHOD handle_user_command.
    PERFORM frm_handle_user_command USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


************************************************************************
*      DEFINITION
************************************************************************
DATA:gt_tabix TYPE sy-tabix .



************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS:
                s_werks FOR aufk-werks, "工厂
                s_aufnr FOR aufk-aufnr ,"OBLIGATORY ,
                s_matnr FOR afpo-matnr .
*PARAMETERS:p_1 RADIOBUTTON GROUP g1  DEFAULT 'X' USER-COMMAND UC,
*           p_2 RADIOBUTTON GROUP g1.


SELECT-OPTIONS:
           s_ftrmi FOR afko-ftrmi MODIF ID mi1,
           s_getri FOR afko-getri MODIF ID mi1.
PARAMETERS:p_fx AS CHECKBOX  MODIF ID mi1..

*SELECT-OPTIONS:
*           s_fhrq FOR likp-wadat_ist MODIF ID MI2, "发货日期
*           s_kdauf FOR afpo-kdauf MODIF ID MI2.    "销售订单号




SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.

*  LOOP AT SCREEN.
*      DEFINE SET_SCREEN.
*      if screen-group1 = &1.
*        screen-active = '1'.
*      elseif screen-group1 cp 'MI*'.
*        screen-active = '0'.
*      endif.
*    END-OF-DEFINITION.
*    case 'X'.
*       WHEN P_1.
*          SET_SCREEN 'MI1'.
*          CLEAR:s_fhrq[],s_kdauf[].
*      WHEN P_2.
*        SET_SCREEN 'MI2'.
*          CLEAR:s_ftrmi[],s_getri[].
*      ENDCASE.
*    MODIFY SCREEN.
*
*  ENDLOOP.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_auth_check.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑

END-OF-SELECTION.
  CALL SCREEN 9001.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  "  IF p_1 EQ 'X'.
  SELECT a~aufnr a~werks a~objnr
   b~ftrmi b~getri b~gamng b~igmng b~gmein b~dispo b~fevor
   c~matnr  c~wemng c~kdauf c~kdpos
   INTO CORRESPONDING FIELDS OF TABLE gt_data
   FROM  aufk AS a
   INNER JOIN afko AS b
   ON a~aufnr = b~aufnr
    INNER JOIN afpo AS c
    ON a~aufnr = c~aufnr
    WHERE a~werks IN s_werks
    AND a~aufnr IN s_aufnr
  AND c~matnr IN s_matnr
   AND b~ftrmi IN s_ftrmi
   AND b~getri IN s_getri.
  SORT gt_data BY aufnr werks .

  "仅查询已完工未结算订单
  IF p_fx EQ 'X'.
    SELECT * INTO TABLE gt_jest
   FROM jest
   FOR ALL ENTRIES IN gt_data
   WHERE objnr = gt_data-objnr
   AND (  ( stat =  'I0045' AND  inact EQ 'X' )
      OR  ( ( stat =  'I0012' AND  inact EQ 'X' )  AND  ( stat =  'I0046' AND  inact EQ '' ) )

    ) .
    SORT gt_jest BY objnr .
    IF gt_jest IS NOT INITIAL .
      LOOP AT gt_data INTO gs_data.
        gt_tabix = sy-tabix .
        READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_data-objnr
                                        BINARY SEARCH .
        IF sy-subrc NE 0 .
          DELETE gt_jest INDEX gt_tabix .
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CHECK gt_data IS NOT INITIAL .
  MOVE-CORRESPONDING gt_data TO gt_kdauf .
  DELETE gt_kdauf WHERE kdauf IS INITIAL .
  SORT gt_kdauf BY kdauf .

  SELECT * INTO TABLE gt_cosp_1
    FROM cosp
    FOR ALL ENTRIES IN gt_data
    WHERE objnr = gt_data-objnr
    AND wrttp = '01'
  AND kstar BETWEEN '5001010000' AND '5001019999'.

  SORT gt_cosp_1 BY objnr .

  LOOP AT gt_cosp_1 INTO gs_cosp_1 .
    CLEAR:gs_cosp_hj_1 .
    gs_cosp_hj_1 = gs_cosp_1-objnr .
    gs_cosp_hj_1-jhclcb = gs_cosp_1-wog001 + gs_cosp_1-wog002 + gs_cosp_1-wog003
                     + gs_cosp_1-wog004 + gs_cosp_1-wog005 + gs_cosp_1-wog006
                     + gs_cosp_1-wog007 + gs_cosp_1-wog008 + gs_cosp_1-wog009
                     + gs_cosp_1-wog010 + gs_cosp_1-wog011 + gs_cosp_1-wog012
                     + gs_cosp_1-wog013 + gs_cosp_1-wog014 + gs_cosp_1-wog015
                     + gs_cosp_1-wog016.
    COLLECT gs_cosp_hj_1 INTO gt_cosp_hj_1 .

  ENDLOOP.
  SORT gt_cosp_hj_1 BY objnr .

  SELECT * INTO TABLE gt_cosp_4
  FROM cosp
  FOR ALL ENTRIES IN gt_data
  WHERE objnr = gt_data-objnr
  AND wrttp = '04'
  AND kstar BETWEEN '5001010000' AND '5001019999'.

  SORT gt_cosp_4 BY objnr .

  LOOP AT gt_cosp_4 INTO gs_cosp_4 .

    gs_main-tlcb =   gs_cosp_4-wog001 + gs_cosp_4-wog002 + gs_cosp_4-wog003
                    + gs_cosp_4-wog004 + gs_cosp_4-wog005 + gs_cosp_4-wog006
                    + gs_cosp_4-wog007 + gs_cosp_4-wog008 + gs_cosp_4-wog009
                    + gs_cosp_4-wog010 + gs_cosp_4-wog011 + gs_cosp_4-wog012
                    + gs_cosp_4-wog013 + gs_cosp_4-wog014 + gs_cosp_4-wog015
                    + gs_cosp_4-wog016.


  ENDLOOP.

  LOOP AT gt_cosp_4 INTO gs_cosp_4.
    CLEAR:gs_cosp_hj_4 .
    gs_cosp_hj_4 = gs_cosp_4-objnr .
    gs_cosp_hj_4-jhclcb = gs_cosp_4-wog001 + gs_cosp_4-wog002 + gs_cosp_4-wog003
                     + gs_cosp_4-wog004 + gs_cosp_4-wog005 + gs_cosp_4-wog006
                     + gs_cosp_4-wog007 + gs_cosp_4-wog008 + gs_cosp_4-wog009
                     + gs_cosp_4-wog010 + gs_cosp_4-wog011 + gs_cosp_4-wog012
                     + gs_cosp_4-wog013 + gs_cosp_4-wog014 + gs_cosp_4-wog015
                     + gs_cosp_4-wog016.
    COLLECT gs_cosp_hj_4 INTO gt_cosp_hj_4 .

  ENDLOOP.

  SORT gt_cosp_hj_4 BY objnr .


  DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf .
  SELECT * INTO TABLE gt_vbak FROM vbak .
  SORT gt_vbak BY vbeln .
  LOOP AT gt_data INTO gs_data WHERE  kdauf IS INITIAL.
    CLEAR:gs_kdauf.
    READ TABLE gt_kdauf INTO gs_kdauf WITH KEY  gs_data-aufnr+0(7)  BINARY SEARCH .
    IF sy-subrc EQ 0 .
      CONTINUE .
    ELSE.
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-aufnr+0(7) BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_kdauf-kdauf = gs_vbak-vbeln .
        APPEND gs_kdauf TO gt_kdauf .
        SORT gt_kdauf BY kdauf .
      ELSE.
        CONTINUE .
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE gt_kdauf WHERE kdauf IS INITIAL .
  SORT gt_kdauf BY kdauf .

  "读取物料描述

  SELECT a~matnr a~werks b~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
    FROM marc  AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    WHERE a~werks IN s_werks
  AND b~spras = sy-langu.

  SORT gt_matnr BY matnr werks .

  SELECT * INTO TABLE gt_t024d
    FROM t024d
  .
  SORT gt_t024d BY dispo  .

  SELECT * INTO TABLE  gt_t024f
  FROM t024f .

  SORT gt_t024f BY fevor .


  SELECT * INTO TABLE gt_jest_gb
    FROM jest
    FOR ALL ENTRIES IN gt_data
    WHERE objnr = gt_data-objnr
    AND inact EQ ''
  AND stat IN ('I0046','I0045') .
  SORT gt_jest_gb BY objnr .

  SELECT * INTO TABLE  gt_jest_sh
   FROM jest
   FOR ALL ENTRIES IN gt_data
   WHERE objnr = gt_data-objnr
   AND inact EQ ''
  AND stat IN ('I0012','I0074') .
  SORT gt_jest_sh BY objnr .

  SELECT * INTO TABLE gt_tj02t
    FROM tj02t
    WHERE spras = sy-langu
  .
  SORT gt_tj02t BY istat .

  SELECT a~vbeln a~wadat_ist
         b~vgbel
         INTO CORRESPONDING FIELDS OF TABLE gt_likp
         FROM likp AS a
         INNER JOIN lips AS b
         ON a~vbeln = b~vbeln
         FOR ALL ENTRIES IN gt_kdauf
  WHERE vgbel = gt_kdauf-kdauf .
  SORT gt_likp BY vgbel .

  "ELSE.



  " ENDIF.



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
  "查询销售订单对应的项目名称、交货日期
  LOOP AT gt_kdauf INTO gs_kdauf .
    PERFORM selxmmc USING gs_kdauf-kdauf sy-langu CHANGING gs_kdauf-xmmc .
    "交货日期
    READ TABLE gt_likp INTO gs_likp WITH KEY vgbel = gs_kdauf-kdauf
                                     BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_kdauf-wadat_ist = gs_likp-wadat_ist .
    ENDIF.
    MODIFY gt_kdauf FROM gs_kdauf.
  ENDLOOP.

  SORT gt_kdauf BY kdauf .

  MOVE-CORRESPONDING gt_data TO gt_wlflz .
  DELETE gt_wlflz WHERE matnr IS INITIAL .
  SORT gt_wlflz BY matnr werks kdauf kdpos .
  DELETE ADJACENT DUPLICATES FROM gt_wlflz COMPARING ALL FIELDS .

  SELECT * INTO TABLE  gt_ckmlhd
    FROM ckmlhd
    FOR ALL ENTRIES IN gt_wlflz
    WHERE bwkey = gt_wlflz-werks
    AND   matnr = gt_wlflz-matnr
    AND   vbeln = gt_wlflz-kdauf
  AND   posnr = gt_wlflz-kdpos .
  SORT gt_ckmlhd BY kalnr .

  SELECT * INTO TABLE gt_ckmlprkeph_s
    FROM ckmlprkeph
    FOR ALL ENTRIES IN gt_ckmlhd
    WHERE kalnr = gt_ckmlhd-kalnr
    AND kkzst = ''
    AND   curtp = '10'
    AND   prtyp = 'S'
  .

  SORT gt_ckmlprkeph_s BY bdatj poper .


  SELECT * INTO TABLE gt_ckmlprkeph_v
    FROM ckmlprkeph
    FOR ALL ENTRIES IN gt_ckmlhd
    WHERE kalnr = gt_ckmlhd-kalnr
    AND kkzst = ''
    AND   curtp = '10'
    AND   prtyp = 'V'
  .

  SORT gt_ckmlprkeph_v BY kalnr bdatj poper .

  SELECT * INTO TABLE gt_ckmlcr
    FROM ckmlcr
    FOR ALL ENTRIES IN gt_ckmlhd
    WHERE kalnr = gt_ckmlhd-kalnr
    AND   curtp = '10'
  AND   vprsv = 'V'.

  SORT gt_ckmlcr BY kalnr bdatj poper .

  LOOP AT gt_data INTO gs_data .
    "生产订单、物料、工厂、订单下达日期、订单完成日期、
    "订单数量、入库数量、报工数量、基本计量单位、销售订单行号、项目名称
    "MRP控制者、生产管理员
    CLEAR:gs_main.
    MOVE-CORRESPONDING gs_data TO gs_main .

    "订单关闭
    READ TABLE gt_jest_gb INTO gs_jest_gb WITH KEY objnr = gs_data-objnr
                                          BINARY SEARCH .
    IF sy-subrc EQ 0 .
      READ TABLE gt_tj02t INTO gs_tj02t WITH KEY istat = gs_jest_gb-stat
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-ddgb = gs_tj02t-txt04 .
      ENDIF.

    ENDIF.

    "订单收货

    READ TABLE gt_jest_sh INTO gs_jest_sh WITH KEY objnr = gs_data-objnr
                                   BINARY SEARCH .
    IF sy-subrc EQ 0 .
      READ TABLE gt_tj02t INTO gs_tj02t WITH KEY istat = gs_jest_sh-stat
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-ddsh = gs_tj02t-txt04 .
      ENDIF.

    ENDIF.

    "有料号的物料处理
    IF gs_main-matnr IS NOT INITIAL .
      READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_main-matnr
                                                 werks = gs_main-werks
                                                 BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-maktx = gs_matnr-maktx .
      ENDIF.



      "物料分类账
      READ TABLE gt_ckmlhd INTO gs_ckmlhd WITH KEY matnr = gs_main-matnr
                                                   bwkey = gs_main-werks
                                                   vbeln = gs_main-kdauf
                                                   posnr = gs_main-kdpos.
      IF sy-subrc EQ 0 .

        "成本估算号
        gs_main-kalnr = gs_ckmlhd-kalnr .

        "标准价 、实际价 、价格单位
        READ TABLE gt_ckmlcr INTO gs_ckmlcr WITH KEY kalnr = gs_ckmlhd-kalnr
                                                      bdatj = gs_data-getri+0(4)
                                                      poper = gs_data-getri+4(2)
                                                      BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_main-stprs = gs_ckmlcr-stprs .
          gs_main-pvprs = gs_ckmlcr-pvprs .
          gs_main-peinh = gs_ckmlcr-peinh .
        ENDIF.

        "标准材料成本（完工时）A
        READ TABLE gt_ckmlprkeph_s INTO gs_ckmlprkeph_s WITH KEY kalnr = gs_ckmlhd-kalnr
                                                      bdatj = gs_data-getri+0(4)
                                                      poper = gs_data-getri+4(2)
                                                      BINARY SEARCH .
        IF sy-subrc EQ 0 AND gs_main-peinh  NE 0 .
          gs_main-bzclcb = gs_ckmlprkeph_s-kst001 * gs_main-gamng /  gs_main-peinh .
        ENDIF.

        "实际材料成本（完工时）D
        READ TABLE gt_ckmlprkeph_v INTO gs_ckmlprkeph_v WITH KEY kalnr = gs_ckmlhd-kalnr
                                                      bdatj = gs_data-getri+0(4)
                                                      poper = gs_data-getri+4(2)
                                                      BINARY SEARCH .
        IF sy-subrc EQ 0  AND gs_main-peinh  NE 0 .
          gs_main-sjclcb = gs_ckmlprkeph_v-kst001 * gs_main-gamng /  gs_main-peinh .
        ENDIF.


      ENDIF.


    ENDIF.

    "取销售订单
    IF gs_main-kdauf IS INITIAL .
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_main-aufnr+0(7)
                                             BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-kdauf = gs_vbak-vbeln .
      ENDIF.
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_main-kdauf
                                             BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-xmmc = gs_kdauf-xmmc .
        gs_main-wadat_ist = gs_kdauf-wadat_ist .
      ENDIF.
    ELSE.
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_main-kdauf
                                          BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_main-xmmc = gs_kdauf-xmmc .
        gs_main-wadat_ist = gs_kdauf-wadat_ist .
      ENDIF.
    ENDIF.

    "计划材料成本B

    READ TABLE gt_cosp_hj_1 INTO gs_cosp_hj_1 WITH KEY objnr = gs_data-objnr
                                               BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_main-jhclcb = gs_cosp_hj_1-jhclcb .

    ENDIF.

    "投料成本（标准价X实际数量）C

    READ TABLE gt_cosp_hj_4 INTO gs_cosp_hj_4 WITH KEY objnr = gs_data-objnr
                                              BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_main-tlcb  = gs_cosp_hj_4-jhclcb .

    ENDIF.

    "标准&计划差异 A-B
    gs_main-bzjhcy = gs_main-bzclcb - gs_main-jhclcb .

    "计划&投料差异 B-C
    gs_main-jhtlcy = gs_main-jhclcb -  gs_main-tlcb .

    "标准&实际差异 A-D

    gs_main-bzsjcy = gs_main-bzclcb - gs_main-sjclcb .


    APPEND gs_main TO gt_main .



  ENDLOOP.
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
  DATA ls_exclude TYPE ui_func.
  CLEAR gs_exclude.
*排除打印按钮
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_exclude TO gs_exclude.
*排除添加、删除、插入、复制按钮
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row .
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row .
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row .
  APPEND ls_exclude TO gs_exclude.
*排除剪切、粘贴按钮、刷新
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo. " 撤销
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row." 插入新行
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gs_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_pc_file.
  APPEND ls_exclude TO gs_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text

FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = p_yy
      name                    = g_objname
      object                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    READ TABLE it_lines INTO wa_lines INDEX 1.
    IF sy-subrc = 0.
      p_xmmc = wa_lines-tdline.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SEL_MX_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_9002_data .


  TYPES:BEGIN OF ty_matnr_bom,
          aufnr     TYPE aufnr,  "生产订单
          matnr     TYPE matnr,
          werks     TYPE werks_d,
          stlnr     TYPE mast-stlnr,
          matnr_sub TYPE matnr,

          menge     TYPE stpo-menge,
          bmeng     TYPE stko-bmeng,

        END OF ty_matnr_bom .

  TYPES:BEGIN OF ty_matnr_aufnr,
          aufnr     TYPE afko-aufnr,  "生产订单
          matnr_sub TYPE resb-matnr,  "物料号
          werks     TYPE werks_d ,    "工厂
          rsnum     TYPE afko-rsnum,   "预留号
          kalnr     TYPE kalnr,        "成本估算号
          kdauf     TYPE resb-kdauf,   "销售订单
          kdpos     TYPE resb-kdpos,    "销售订单行项目
          bdmng     TYPE resb-bdmng,   "订单计划数量

        END OF ty_matnr_aufnr .

  TYPES:BEGIN OF ty_matnr_cosp,
          aufnr     TYPE afko-aufnr,  "生产订单
          werks     TYPE werks_d,      "工厂
          matnr_sub TYPE afpo-matnr,  "组件
          kalnr     TYPE kalnr,        "成本估算号
          sjtlsl    TYPE afko-gamng,   "实际投料数量
          sjclcb    TYPE cosp-wog016,   "实际材料成本（投料时）
        END OF ty_matnr_cosp .

  TYPES:BEGIN OF ty_aufnr_zj ,
          aufnr     TYPE afko-aufnr,  "生产订单
          werks     TYPE werks_d,      "工厂
          "   matnr     type matnr ,
          matnr_sub TYPE afpo-matnr,  "组件
*          bdmng     TYPE resb-bdmng,   "订单计划数量
*          sjtlsl    TYPE afko-gamng,   "实际投料数量

        END OF ty_aufnr_zj .

  "取成本逻辑
  TYPES:BEGIN OF ty_matnr_xm,
          matnr_sub TYPE matnr,  "物料号
          werks     TYPE werks_d,  "工厂
          kdauf     TYPE resb-kdauf,   "销售订单
          kdpos     TYPE resb-kdpos,    "销售订单行项目
          kalnr     TYPE kalnr,  "估算号
        END OF ty_matnr_xm .

  DATA:gt_matnr_xm TYPE TABLE OF ty_matnr_xm,
       gs_matnr_xm TYPE ty_matnr_xm.

  DATA:gt_aufnr_zj_1 TYPE TABLE OF ty_aufnr_zj,
       gt_aufnr_zj_2 TYPE TABLE OF ty_aufnr_zj,
       gt_aufnr_zj_3 TYPE TABLE OF ty_aufnr_zj,
       gt_aufnr_zj   TYPE TABLE OF ty_aufnr_zj,
       gs_aufnr_zj   TYPE ty_aufnr_zj.

  DATA:gt_matnr_cosp TYPE TABLE OF ty_matnr_cosp,
       gs_matnr_cosp TYPE ty_matnr_cosp.

  DATA:gt_matnr_aufnr TYPE TABLE OF ty_matnr_aufnr,
       gs_matnr_aufnr TYPE ty_matnr_aufnr.

  DATA:gt_matnr_bom TYPE TABLE OF ty_matnr_bom,
       gs_matnr_bom TYPE ty_matnr_bom.


  "初始化
  REFRESH:gt_sub.
  "取成本逻辑
  TYPES:BEGIN OF ty_matnr,
          matnr_sub TYPE matnr,  "物料号
          werks     TYPE werks_d,  "工厂

          kalnr     TYPE kalnr,  "估算号
        END OF ty_matnr .

  DATA:gt_matnr_sub   TYPE TABLE OF ty_matnr,
       gt_matnr_sub_2 TYPE TABLE OF ty_matnr,
       gs_matnr_sub   TYPE ty_matnr.

  DATA:gt_matnr_sub_xm TYPE TABLE OF ty_matnr_xm,
       gs_matnr_sub_xm TYPE ty_matnr_xm.

  DATA:gs_mast TYPE mast,
       gs_afpo TYPE afpo.

  DATA:gt_ckmlhd_1 TYPE TABLE OF ckmlhd,
       gs_ckmlhd_1 TYPE ckmlhd.

  DATA:gt_ckmlcr_1 TYPE TABLE OF ckmlcr,
       gs_ckmlcr_1 TYPE ckmlcr.

  DATA:gt_ckmlcr_2 TYPE TABLE OF ckmlcr,
       gs_ckmlcr_2 TYPE ckmlcr.

  REFRESH:gt_main_sel .
  MOVE-CORRESPONDING gt_main TO gt_main_sel .
  DELETE gt_main_sel WHERE zbox NE 'X'.

  SORT gt_main_sel BY aufnr matnr werks .

  IF gt_main_sel IS NOT INITIAL.
    SELECT * INTO TABLE gt_cosp_1_sub
      FROM cosp
      FOR ALL ENTRIES IN gt_main_sel
      WHERE objnr = gt_main_sel-objnr
      AND wrttp = '01'
      AND kstar BETWEEN '5001010000' AND '5001019999'.

    SORT gt_cosp_1_sub BY objnr .

    SELECT * INTO TABLE gt_cosp_4_sub
       FROM cosp
       FOR ALL ENTRIES IN gt_main_sel
       WHERE objnr = gt_main_sel-objnr
       AND wrttp = '04'
       AND kstar BETWEEN '5001010000' AND '5001019999'.

    SORT gt_cosp_4_sub BY objnr .

  ENDIF.


  REFRESH:gt_matnr_sel .
  MOVE-CORRESPONDING gt_main_sel TO gt_matnr_sel.

  DELETE gt_matnr_sel WHERE matnr IS INITIAL .

  DELETE ADJACENT DUPLICATES FROM gt_matnr_sel  COMPARING matnr .
  REFRESH:gt_aufnr_sel.
  MOVE-CORRESPONDING gt_main_sel TO gt_aufnr_sel.

  DELETE gt_aufnr_sel WHERE aufnr IS INITIAL .

  DELETE ADJACENT DUPLICATES FROM gt_aufnr_sel  COMPARING aufnr .

  IF gt_matnr_sel IS NOT INITIAL .

    REFRESH:gt_matnr_bom .

    SELECT a~matnr a~werks a~stlnr
           b~idnrk AS matnr_sub b~menge
           c~bmeng
     INTO CORRESPONDING FIELDS OF TABLE gt_matnr_bom
          FROM mast AS a
          INNER JOIN stko AS c
          ON a~stlnr = c~stlnr
          INNER JOIN stpo AS b
          ON a~stlnr = b~stlnr
          FOR ALL ENTRIES IN  gt_matnr_sel
          WHERE a~matnr = gt_matnr_sel-matnr
          AND   a~werks = gt_matnr_sel-werks
          AND  a~stlan = '1'
    AND a~stlal = '01' .
    SORT  gt_matnr_bom BY matnr werks .
  ENDIF .


  REFRESH:gt_aufnr_zj.
  IF gt_aufnr_sel IS NOT INITIAL .

    REFRESH:gt_matnr_aufnr .
    SELECT a~aufnr a~rsnum
           b~matnr AS matnr_sub b~werks b~bdmng
           b~kdauf b~kdpos
      INTO CORRESPONDING FIELDS OF TABLE gt_matnr_aufnr
      FROM afko AS a
      INNER JOIN resb AS b
      ON a~rsnum = b~rsnum
      FOR ALL ENTRIES IN gt_aufnr_sel
    WHERE a~aufnr = gt_aufnr_sel-aufnr .

    SORT gt_matnr_aufnr BY aufnr   werks  matnr_sub.

    REFRESH:gt_aufnr_zj_1.
    MOVE-CORRESPONDING gt_matnr_aufnr TO gt_aufnr_zj_1 .

    APPEND LINES OF gt_aufnr_zj_1  TO gt_aufnr_zj .

    REFRESH:gt_matnr_sub_xm .
    MOVE-CORRESPONDING gt_matnr_aufnr TO gt_matnr_sub_xm .

    DELETE  gt_matnr_sub_xm  WHERE kdauf IS INITIAL AND  kdpos IS INITIAL.

    SORT gt_matnr_sub_xm BY matnr_sub werks kdauf kdpos.

    DELETE ADJACENT DUPLICATES FROM gt_matnr_sub_xm COMPARING matnr_sub werks kdauf kdpos.

    IF gt_matnr_sub_xm IS NOT INITIAL.
      REFRESH:gt_ckmlhd_1.
      SELECT * INTO TABLE gt_ckmlhd_1
        FROM ckmlhd
        FOR ALL ENTRIES IN gt_matnr_sub_xm
        WHERE bwkey = gt_matnr_sub_xm-werks
        AND   matnr = gt_matnr_sub_xm-matnr_sub
        AND   vbeln = gt_matnr_sub_xm-kdauf
      AND    posnr = gt_matnr_sub_xm-kdpos.
      SORT gt_ckmlhd_1 BY kalnr .
    ENDIF.

    IF gt_ckmlhd_1 IS NOT INITIAL .
      REFRESH:gt_ckmlcr_1.
      SELECT * INTO  TABLE gt_ckmlcr_1
        FROM ckmlcr
        FOR ALL ENTRIES IN gt_ckmlhd_1
      WHERE kalnr = gt_ckmlhd_1-kalnr .
      SORT gt_ckmlcr_1 BY kalnr bdatj poper .

    ENDIF.
  ENDIF.

  DATA:gt_cokey TYPE TABLE OF cokey,
       gs_cokey TYPE cokey.

  IF gt_cosp_4_sub IS NOT INITIAL .
    REFRESH:gt_cokey .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_cokey
         FROM cokey
      FOR ALL ENTRIES IN  gt_cosp_4_sub
    WHERE hrkft = gt_cosp_4_sub-hrkft .
    SORT  gt_cokey BY hrkft .

    REFRESH:gt_matnr_cosp .
    DATA:l_i     TYPE i VALUE 1,
         p_poper TYPE poper VALUE 001 . "期间

    REFRESH:gt_matnr_cosp.

    LOOP AT  gt_cosp_4_sub INTO gs_cosp_4_sub .
      "生产订单
      CLEAR:gs_matnr_cosp .
      gs_matnr_cosp-aufnr = gs_cosp_4_sub-objnr+2(10).
      "投料成本
      gs_matnr_cosp-sjtlsl = gs_cosp_4_sub-meg001 + gs_cosp_4_sub-meg002 +  gs_cosp_4_sub-meg003 + gs_cosp_4_sub-meg004
                           + gs_cosp_4_sub-meg005 + gs_cosp_4_sub-meg006 + gs_cosp_4_sub-meg007 + gs_cosp_4_sub-meg008
                           + gs_cosp_4_sub-meg009 + gs_cosp_4_sub-meg010 + gs_cosp_4_sub-meg011 + gs_cosp_4_sub-meg012
                           + gs_cosp_4_sub-meg013 + gs_cosp_4_sub-meg014 + gs_cosp_4_sub-meg015 + gs_cosp_4_sub-meg016  .

      READ TABLE gt_cokey INTO gs_cokey WITH KEY hrkft = gs_cosp_4_sub-hrkft
                                        BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_matnr_cosp-werks = gs_cokey-werks .
        gs_matnr_cosp-matnr_sub = gs_cokey-matnr .

        READ TABLE gt_matnr_aufnr INTO gs_matnr_aufnr WITH KEY aufnr = gs_matnr_cosp-aufnr
                                                               werks = gs_matnr_cosp-werks
                                                               matnr_sub = gs_matnr_cosp-matnr_sub
                                                               BINARY SEARCH .
        IF sy-subrc EQ 0 .
          "计算实际材料成本
          READ TABLE gt_ckmlhd_1 INTO gs_ckmlhd_1 WITH KEY matnr = gs_matnr_cosp-matnr_sub
                                                           bwkey = gs_matnr_cosp-werks
                                                           vbeln = gs_matnr_aufnr-kdauf
                                                           posnr = gs_matnr_aufnr-kdpos
                                                           BINARY SEARCH.
          IF sy-subrc EQ 0 .
            DO 16 TIMES .
              READ TABLE gt_ckmlcr_1 INTO gs_ckmlcr_1 WITH KEY kalnr = gs_ckmlhd_1-kalnr
                                                               bdatj = gs_cosp_4_sub-gjahr
                                                               poper = p_poper
                                                               BINARY SEARCH .
              IF sy-subrc EQ 0 .
                CASE p_poper.
                  WHEN '001' .
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg001 .
                  WHEN '002'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg002 .
                  WHEN '003'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg003.
                  WHEN '004'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg004.
                  WHEN '005'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg005.
                  WHEN '006'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg006.
                  WHEN '007'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg007.
                  WHEN '008'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg008.
                  WHEN '009'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg009.
                  WHEN '010'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg010.
                  WHEN '011'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg011.
                  WHEN '012'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg012.
                  WHEN '013'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg013.
                  WHEN '014'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg014.
                  WHEN '015'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg015.
                  WHEN '016'.
                    gs_matnr_cosp-sjclcb  =  gs_matnr_cosp-sjclcb + gs_ckmlcr_1-pvprs * gs_cosp_4_sub-meg016.
                ENDCASE.
              ENDIF.
              p_poper =  p_poper + 1 .
            ENDDO.
          ENDIF.
        ENDIF.
      ENDIF.
      COLLECT gs_matnr_cosp  INTO gt_matnr_cosp  .
    ENDLOOP.

    SORT  gt_matnr_cosp BY aufnr werks  matnr_sub .

    MOVE-CORRESPONDING gt_matnr_cosp TO gt_aufnr_zj_2 .

    APPEND LINES OF gt_aufnr_zj_2 TO gt_aufnr_zj .

    SORT gt_aufnr_zj BY aufnr  werks matnr_sub.

    DELETE ADJACENT DUPLICATES FROM gt_aufnr_zj COMPARING aufnr werks matnr_sub  .

  ENDIF.

  DATA:l_tabix TYPE sy-tabix .

  IF gt_matnr_bom IS NOT INITIAL .
    LOOP AT gt_main_sel INTO gs_main_sel .
      CLEAR:gs_aufnr_zj  .
      gs_aufnr_zj-aufnr = gs_main_sel-aufnr. "生产订单
      gs_aufnr_zj-werks = gs_main_sel-werks.

      LOOP AT gt_matnr_bom INTO gs_matnr_bom WHERE matnr = gs_main_sel-matnr
                                                 AND werks = gs_main_sel-werks .
        gs_aufnr_zj-matnr_sub = gs_matnr_bom-matnr_sub ."组件

        APPEND gs_aufnr_zj TO gt_aufnr_zj_3 .
      ENDLOOP.
    ENDLOOP.

  ENDIF.

  SORT gt_aufnr_zj_3 BY aufnr werks  matnr_sub .

  APPEND LINES OF gt_aufnr_zj_3 TO gt_aufnr_zj .

  SORT gt_aufnr_zj BY aufnr werks   matnr_sub .

  DELETE ADJACENT DUPLICATES FROM gt_aufnr_zj COMPARING aufnr  werks  matnr_sub.

  LOOP AT gt_aufnr_zj INTO gs_aufnr_zj .
    CLEAR:gs_sub .
    "生产订单、物料、工厂
    MOVE-CORRESPONDING gs_aufnr_zj TO gs_sub.
    READ TABLE gt_matnr_aufnr INTO gs_matnr_aufnr  WITH KEY  aufnr = gs_sub-aufnr
                                        matnr_sub = gs_sub-matnr_sub
                                        werks = gs_sub-werks
                               BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_sub-ddjhsl = gs_matnr_aufnr-bdmng.
      gs_sub-kdauf = gs_matnr_aufnr-kdauf.
      gs_sub-kdpos = gs_matnr_aufnr-kdpos .
      "按物料+工厂 + 销售订单 + 行项目 取成本估算号
      READ TABLE gt_ckmlhd_1 INTO gs_ckmlhd_1 WITH KEY matnr = gs_sub-matnr_sub
                                                           bwkey = gs_sub-werks
                                                           vbeln = gs_sub-kdauf
                                                           posnr = gs_sub-kdpos
                                                           BINARY SEARCH.
      IF sy-subrc EQ 0 .
        gs_sub-kalnr = gs_ckmlhd_1-kalnr .

      ENDIF.
    ENDIF.

    READ TABLE gt_matnr_cosp INTO gs_matnr_cosp WITH KEY aufnr = gs_sub-aufnr
                                        matnr_sub = gs_sub-matnr_sub
                                        werks = gs_sub-werks
                               BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_sub-sjtlsl = gs_matnr_cosp-sjtlsl .
      "gs_sub-kalnr = gs_matnr_cosp-kalnr .
      gs_sub-sjclcb = gs_matnr_cosp-sjclcb .
    ENDIF.

    "找生产订单的主物料

    READ TABLE gt_main_sel INTO gs_main_sel WITH KEY aufnr = gs_sub-aufnr
                                            BINARY SEARCH .
    IF sy-subrc EQ 0 .
      "对象号
      gs_sub-objnr = gs_main_sel-objnr .
      "主物料
      gs_sub-matnr = gs_main_sel-matnr .
    ENDIF.

    "BOM组件检查
    IF gs_sub-matnr IS NOT INITIAL.
      READ TABLE gt_matnr_bom INTO gs_matnr_bom WITH KEY matnr = gs_sub-matnr
                                                 matnr_sub = gs_sub-matnr_sub
                                                 BINARY SEARCH .
      IF sy-subrc EQ 0.
        IF gs_matnr_bom-bmeng NE 0 .
          gs_sub-bzbom = gs_matnr_bom-menge * gs_main_sel-gamng / gs_matnr_bom-bmeng .
          "按入库-理论耗用数
          IF gs_main_sel-gamng  NE 0 .
            gs_sub-llhys = gs_main_sel-wemng / gs_main_sel-gamng  * gs_sub-bzbom .
          ENDIF.

        ENDIF.

      ENDIF.
      "组件描述
      READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_sub-matnr_sub
                                                 werks = gs_sub-werks
                                         BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_sub-zjms = gs_matnr-maktx.
      ENDIF.
    ENDIF.

    "欠料/超领数
    gs_sub-ql_cl = gs_sub-ddjhsl  - gs_sub-sjtlsl .

    APPEND  gs_sub TO gt_sub .

  ENDLOOP.

  SORT gt_sub BY aufnr matnr matnr_sub .


  MOVE-CORRESPONDING gt_sub TO gt_matnr_sub .

  SORT gt_matnr_sub BY matnr_sub werks .

  DELETE ADJACENT DUPLICATES FROM gt_matnr_sub COMPARING matnr_sub werks .


  IF gt_matnr_sub IS NOT INITIAL.
    SELECT * INTO TABLE gt_ckmlhd_1
      FROM ckmlhd
      FOR ALL ENTRIES IN gt_matnr_sub
      WHERE bwkey = gt_matnr_sub-werks
    AND  matnr = gt_matnr_sub-matnr_sub .

    SORT gt_ckmlhd_1 BY kalnr .

    SELECT * INTO TABLE gt_ckmlcr_1
      FROM ckmlcr
      FOR ALL ENTRIES IN gt_ckmlhd_1
    WHERE kalnr = gt_ckmlhd_1-kalnr  .

    SORT gt_ckmlcr_1 BY kalnr bdatj poper .

    MOVE-CORRESPONDING gt_ckmlcr_1 TO gt_ckmlcr_2.

    SORT gt_ckmlcr_2 BY kalnr ASCENDING bdatj DESCENDING poper DESCENDING .

    DELETE ADJACENT DUPLICATES FROM gt_ckmlcr_2 COMPARING kalnr .

    "重新查找COKEY的值
    REFRESH:gt_cokey .
    SELECT * INTO TABLE gt_cokey
      FROM cokey
      FOR ALL ENTRIES IN gt_matnr_sub
      WHERE werks = gt_matnr_sub-werks
    AND   matnr = gt_matnr_sub-matnr_sub .

    SORT gt_cokey BY hrkft .

  ENDIF.

  LOOP AT gt_sub INTO gs_sub .
    "找生产订单的主物料

    READ TABLE gt_main_sel INTO gs_main_sel WITH KEY aufnr = gs_sub-aufnr
                                            BINARY SEARCH .
    IF sy-subrc EQ 0 .

      "订单关闭
      gs_sub-ddgb = gs_main_sel-ddgb.
      "订单收货
      gs_sub-ddsh = gs_main_sel-ddsh .
      "订单下达日期
      gs_sub-ftrmi = gs_main_sel-ftrmi .
      "订单数量
      gs_sub-gamng = gs_main_sel-gamng.
      "入库数量
      gs_sub-wemng = gs_main_sel-wemng .

      "母件单位
      gs_sub-gmein = gs_main_sel-gmein.

      "母件描述
      gs_sub-maktx = gs_main_sel-maktx .
    ENDIF.
    "在线数量
    IF gs_sub-ddgb EQ '' AND gs_sub-ddsh NE 'DLV'.
      gs_sub-zxsl = gs_sub-sjtlsl - gs_sub-llhys .
    ELSE.
      gs_sub-zxsl =  0.
    ENDIF.
    "标准&计划数量差
    gs_sub-bz_jh_c = gs_sub-bzbom - gs_sub-ddjhsl .
    "计划&实际数量差
    gs_sub-jh_sj_c = gs_sub-ddjhsl  - gs_sub-sjtlsl .
    "理论&实际数量差
    gs_sub-ll_sj_c = gs_sub-bzbom - gs_sub-sjtlsl .
    "组件有BOM
    SELECT SINGLE * INTO gs_mast FROM mast
                    WHERE matnr = gs_sub-matnr_sub
                    AND werks = gs_sub-werks
    .
    IF sy-subrc EQ 0 .
      gs_sub-zjbom  = 'X'.
    ENDIF.
    "组件有生产订单
    SELECT SINGLE * INTO gs_afpo FROM afpo
                    WHERE aufnr = gs_sub-aufnr
                    AND matnr = gs_sub-matnr_sub
    AND dwerk = gs_sub-werks.
    IF sy-subrc EQ 0.

      gs_sub-zjaufnr = 'X'.
    ENDIF.

    "标准材料成本A

    IF gs_sub-bzbom  NE 0 .
      READ TABLE gt_ckmlhd_1 INTO gs_ckmlhd_1 WITH KEY matnr = gs_sub-matnr_sub
                                                 bwkey = gs_sub-werks
                                                 BINARY SEARCH.
      IF sy-subrc EQ 0 .
        READ TABLE gt_ckmlcr_1 INTO gs_ckmlcr_1 WITH KEY kalnr = gs_sub-kalnr
                                                         bdatj = gs_sub-ftrmi+0(4)
                                                         poper = gs_sub-ftrmi+4(2)
                                                         BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_sub-bz_cl_cb = gs_ckmlcr_1-stprs * gs_sub-bzbom / gs_ckmlcr_1-peinh .

          "价格单位
          gs_sub-peinh =  gs_ckmlcr_1-peinh .
        ENDIF.

      ENDIF.
    ENDIF.

    "计划成本B
    READ TABLE gt_cokey INTO gs_cokey  WITH KEY matnr = gs_sub-matnr_sub
                                                werks = gs_sub-werks
                                       BINARY SEARCH .
    IF sy-subrc EQ 0.

      LOOP AT  gt_cosp_1_sub INTO gs_cosp_1_sub WHERE objnr = gs_sub-objnr
                                                  AND  hrkft = gs_cokey-hrkft
                                               .
        gs_sub-jhcb =  gs_cosp_1_sub-wog001 + gs_cosp_1_sub-wog002 + gs_cosp_1_sub-wog003 + gs_cosp_1_sub-wog004
                    +  gs_cosp_1_sub-wog005 + gs_cosp_1_sub-wog006 + gs_cosp_1_sub-wog007 + gs_cosp_1_sub-wog008
                    + gs_cosp_1_sub-wog010 + gs_cosp_1_sub-wog011 + gs_cosp_1_sub-wog012 + gs_cosp_1_sub-wog013
                    + gs_cosp_1_sub-wog014 + gs_cosp_1_sub-wog015 + gs_cosp_1_sub-wog016 .



      ENDLOOP.

      "投料成本（标准价X实际数量）C
      LOOP AT  gt_cosp_4_sub INTO gs_cosp_4_sub WHERE objnr = gs_sub-objnr
                                         AND  hrkft = gs_cokey-hrkft .


        gs_sub-tlcb =  gs_cosp_4_sub-wog001 + gs_cosp_4_sub-wog002 + gs_cosp_4_sub-wog003 + gs_cosp_4_sub-wog004
                    +  gs_cosp_4_sub-wog005 + gs_cosp_4_sub-wog006 + gs_cosp_4_sub-wog007 + gs_cosp_4_sub-wog008
                    +  gs_cosp_4_sub-wog010 + gs_cosp_4_sub-wog011 + gs_cosp_4_sub-wog012 + gs_cosp_4_sub-wog013
                    +  gs_cosp_4_sub-wog014 + gs_cosp_4_sub-wog015 + gs_cosp_4_sub-wog016 .

      ENDLOOP.
      "标准&计划差异A-B
      gs_sub-bz_jh_cy = gs_sub-bz_cl_cb - gs_sub-jhcb  .

      "计划&投料差异
      gs_sub-jh_tl_cy = gs_sub-jhcb - gs_sub-tlcb .
      "最新标准价 "最新实际价
      READ TABLE gt_ckmlcr_2 INTO gs_ckmlcr_2 WITH KEY kalnr = gs_sub-kalnr
                                              BINARY SEARCH .
      IF sy-subrc EQ 0 .

        gs_sub-zxbzj = gs_ckmlcr_2-stprs .
        gs_sub-zxsjj = gs_ckmlcr_2-pvprs .
      ENDIF.
    ENDIF.

    MODIFY gt_sub FROM gs_sub .

  ENDLOOP.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'STA_9001'.
  SET TITLEBAR 'TIT_9001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_9001 OUTPUT.
  IF wcl_container IS INITIAL.
    PERFORM frm_create_container USING 'CONT_9001' '9001' CHANGING wcl_container gcl_alv.
    PERFORM frm_exclude.
    PERFORM frm_pre_layout USING '' space CHANGING gs_layout.
    PERFORM frm_pre_fieldcat.
    PERFORM frm_upload_event CHANGING gcl_alv. " 注册事件
    PERFORM frm_set_tab_display.
  ELSE.
    PERFORM frm_refresh_alv CHANGING gcl_alv.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4959   text
*      -->P_4960   text
*      <--P_WCL_CONTAINER  text
*      <--P_GCL_ALV  text
*----------------------------------------------------------------------*
FORM frm_create_container  USING    p_container_name
                                    p_dyngr
                           CHANGING pcl_container TYPE REF TO cl_gui_docking_container
                                    pcl_alv TYPE REF TO cl_gui_alv_grid.

  " 自适应窗口容器
  CREATE OBJECT pcl_container
    EXPORTING
      repid     = sy-repid
      dynnr     = p_dyngr   " '9001' 屏幕号
      extension = 2050
      side      = cl_gui_docking_container=>property_floating.

  CREATE OBJECT pcl_alv
    EXPORTING
      i_parent = pcl_container.


ENDFORM.                    " FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5059   text
*      -->P_SPACE  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM frm_pre_layout  USING    p_title
                              p_tool
                     CHANGING ps_layout TYPE lvc_s_layo.

  ps_layout-zebra = 'X'.
  ps_layout-no_toolbar = p_tool.
  ps_layout-cwidth_opt = 'X'.
  ps_layout-grid_title = p_title.
*   PS_LAYOUT-BOX_FNAME = 'BOX'. " 选择框字段

ENDFORM.                    " FRM_PRE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_pre_fieldcat .
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'ZBOX'  '选择框' '' 'X' '' 'X' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'AUFNR' '生产订单' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'MATNR' '物料' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'MAKTX' '物料描述' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'WERKS' '工厂' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'DDGB' '订单关闭' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'DDSH' '订单收货' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'FTRMI' '订单下达日期' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'GETRI' '订单完成日期' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'GAMNG' '订单数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'WEMNG' '入库数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'IGMNG' '报工数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'GMEIN' '基本计量单位' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'WADAT_IST' '发货日期' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'KDAUF' '销售订单号' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'KDPOS' '销售订单行号' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'XMMC' '项目名称' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'DISPO' 'MRP控制者' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'DSNAM' '控制者名称' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'FEVOR' '生产管理员' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'FEVOR_TXT' '生产管理员文本' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'BZCLCB' '标准材料成本（完工时）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'JHCLCB' '计划材料成本' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'TLCB' '投料成本' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'SJCLCB' '实际材料成本' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'BZJHCY' '标准&计划差异' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'JHTLCY' '计划&投料差异' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'BZSJCY' '标准&实际差异' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat USING 'KALNR' '成本估算号' '' '' '' '' '' '' ''  ''.

ENDFORM.

FORM frm_fieldcat  TABLES   pt_fieldcat
                   USING    p_fieldname
                            p_coltext
                            p_key
                            p_edit
                            p_nozero
                            p_checkbox
                            p_ref_table
                            p_ref_field
                            p_outputlen
                            p_hotspot.
  DATA w_fcat TYPE lvc_s_fcat.
  w_fcat-fieldname = p_fieldname. " 字段
  w_fcat-coltext = p_coltext. " 字段描述
  w_fcat-key = p_key.
  w_fcat-edit = p_edit.
  w_fcat-no_zero = p_nozero.
  w_fcat-checkbox = p_checkbox.
  w_fcat-ref_table = p_ref_table.
  w_fcat-ref_field = p_ref_field.
  w_fcat-outputlen = p_outputlen.
  w_fcat-hotspot = p_hotspot.
  APPEND w_fcat TO pt_fieldcat.

ENDFORM.                    " FRM_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GCL_ALV  text
*----------------------------------------------------------------------*
FORM frm_upload_event  CHANGING  pcl_alv TYPE REF TO cl_gui_alv_grid.

  DATA lr_event_handler TYPE REF TO lcl_event_receiver.

  CREATE OBJECT lr_event_handler. " 注册双击事件

  SET HANDLER lr_event_handler->handle_double_click FOR pcl_alv.

  SET HANDLER lr_event_handler->handle_toolbar FOR pcl_alv.

  SET HANDLER lr_event_handler->handle_user_command FOR pcl_alv.

ENDFORM.                    " FRM_UPLOAD_EVENT
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_TAB_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_tab_display .
  DATA:gv_alv_variant LIKE disvariant.

  gv_alv_variant-report = sy-repid.

  CALL METHOD gcl_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'X'
      i_default                     = 'X'
      is_layout                     = gs_layout
      it_toolbar_excluding          = gs_exclude
      is_variant                    = gv_alv_variant " 保存变式
    CHANGING
      it_outtab                     = gt_main
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GCL_ALV  text
*----------------------------------------------------------------------*
FORM frm_refresh_alv  CHANGING pcl_alv TYPE REF TO cl_gui_alv_grid.
*刷新稳定性
  DATA: lw_stbl TYPE lvc_s_stbl.
  lw_stbl-row = 'X'.
  lw_stbl-col = 'X'.
  CALL METHOD pcl_alv->check_changed_data.
  CALL METHOD pcl_alv->refresh_table_display
    EXPORTING
      is_stable = lw_stbl
*     I_SOFT_REFRESH =
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM frm_handle_toolbar  USING    p_e_object TYPE REF TO cl_alv_event_toolbar_set
                                  p_e_interactive.

  DATA:ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE icon_select_all TO ls_toolbar-icon.
  MOVE 'ALL' TO ls_toolbar-function.
  MOVE '全选' TO ls_toolbar-quickinfo.
  MOVE '全选' TO ls_toolbar-text.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE icon_deselect_all TO ls_toolbar-icon.
  MOVE 'DEALL' TO ls_toolbar-function.
  MOVE '反选' TO ls_toolbar-quickinfo.
  MOVE '反选' TO ls_toolbar-text.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

*  CLEAR LS_TOOLBAR.
*  MOVE ICON_EXECUTE_OBJECT TO LS_TOOLBAR-ICON.
*  MOVE 'COM_TECH' TO LS_TOOLBAR-FUNCTION.
*  MOVE '批量关闭' TO LS_TOOLBAR-QUICKINFO.
*  MOVE '批量关闭' TO LS_TOOLBAR-TEXT.
*  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

  CLEAR ls_toolbar.
  MOVE icon_select_detail TO ls_toolbar-icon.
  MOVE 'DETAIL' TO ls_toolbar-function.
  MOVE '查看组件' TO ls_toolbar-quickinfo.
  MOVE '查看组件' TO ls_toolbar-text.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.


ENDFORM.                    " FRM_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM frm_handle_user_command  USING   p_e_ucomm.
  CASE p_e_ucomm.
    WHEN 'ALL'.
      LOOP AT gt_main INTO gs_main.
        gs_main-zbox = 'X'.
        MODIFY gt_main FROM gs_main.
      ENDLOOP.
      PERFORM frm_refresh_alv CHANGING gcl_alv." 刷新ALV

    WHEN 'DEALL'. " 反选
      LOOP AT gt_main INTO gs_main.
        gs_main-zbox = ''.
        MODIFY gt_main FROM gs_main.
      ENDLOOP.
      PERFORM frm_refresh_alv CHANGING gcl_alv." 刷新ALV

*    WHEN 'COM_TECH'. " 技术完成
*      PERFORM FRM_CHECK_BEFORE_BDC. " 检查数据
*      PERFORM FRM_BDC_CO02. " 调用BDC,技术性完成
*      PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV." 刷新ALV

    WHEN 'DETAIL'.
      PERFORM frm_get_9002_data.
      CALL SCREEN 9002.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'STA_9002'.
*  SET TITLEBAR 'xxx'.
  SET TITLEBAR 'TIT_9002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_9002 OUTPUT.
  IF wcl_container2 IS INITIAL.
    PERFORM frm_create_container USING 'CONT_9002' '9002' CHANGING wcl_container2 gcl_alv2.
    PERFORM frm_exclude.
    PERFORM frm_pre_layout USING '' space CHANGING gs_layout2.
    PERFORM frm_pre_fieldcat2.
*    PERFORM frm_upload_event CHANGING gcl_alv2. " 注册事件
    PERFORM frm_set_tab_display2.

  ELSE.
    PERFORM frm_refresh_alv CHANGING gcl_alv2.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_pre_fieldcat2 .
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'AUFNR' '生产订单' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'WERKS' '工厂' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'DDGB' '订单关闭' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'DDSH' '订单收货' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'MATNR' '母件物料' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'MAKTX' '母件描述' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'GAMNG' '订单数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'WEMNG' '入库数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'GMEIN' '母件单位' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'MATNR_SUB' '组件' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZJMS' '组件描述' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZJDW' '组件单位' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'BZBOM' '标准BOM数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'DDJHSL' '订单计划数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'SJTLSL' '实际投料数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'LLHYS' '按入库—理论耗用数' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZXSL' '在线数量' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'QL_CL' '欠料/超领数' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZJBOM' '组件有BOM' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZJAUFNR' '组件有生产订单' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'BZ_JH_C' '标准&计划数量差' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'JH_SJ_C' '计划&实际数量差' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'LL_SJ_C' '理论&实际数量差' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'BZ_CL_CB' '标准材料成本（投料时）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'JHCB' '计划成本' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'TLCB' '投料成本（标准价X实际数量）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'SJCLCB' '实际材料成本（投料时）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'BZ_JH_CY' '标准&计划差异' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'JH_TL_CY' '计划&投料差异' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZXBZJ' '最新标准价' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'ZXSJJ' '最新实际价' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'TLCB' '投料成本（标准价X实际数量）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'PEINH' '实际材料成本（投料时）' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'BZ_JH_CY' '价格单位' '' '' '' '' '' '' ''  ''.
  PERFORM frm_fieldcat TABLES gt_fieldcat2 USING 'KALNR' '成本估算号' '' '' '' '' '' '' ''  ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_9002_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_TAB_DISPLAY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_tab_display2 .
  CALL METHOD gcl_alv2->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = gs_layout2
      it_toolbar_excluding          = gs_exclude
    CHANGING
      it_outtab                     = gt_sub
      it_fieldcatalog               = gt_fieldcat2
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
