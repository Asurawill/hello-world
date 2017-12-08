*&---------------------------------------------------------------------*
*& Report  ZPP003
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/5
*& Request       :
*& Descriptions  : 生产计划内领料单
*&
*& Modify by     : it02
*& Modify date   :20161019
*& Request       :
*& Descriptions  :排除2110 打印检查
"  Modify by : IT02 新增2110工厂打印  by 20161107 排除打印检查
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2017-02-10  it02&魏云           ED1K905243  增加2110工厂打印模板的打印次数
*& 2017-03-22  it02&魏云           ED1K905317  增加打印CHECK查询排除未删除的组件信息
*& 2017-08-30  it02&魏云           ED1K907014  增加打印CHECK查询MCSD客户库存数量
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPP003.
************************************************************************
* Tables
************************************************************************
TABLES:AFPO,AFKO,RESB.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        ZBOX    TYPE C,
        LGORT   TYPE RESB-LGORT, "发货地点
        AUFNR   TYPE AFPO-AUFNR, "工单号
        MATKL_Z TYPE MARA-MATKL, "子件物料组
        AUART   TYPE AUFK-AUART, "订单类型
        MATNR   TYPE AFPO-MATNR, "抬头物料类型
        KTEXT   TYPE AUFK-KTEXT, "描述
        MATNR_Z TYPE RESB-MATNR, "组建物料号
        MAKTX   TYPE MAKT-MAKTX, "成品料号
        MAKTX_Z TYPE MAKT-MAKTX, "组建物料描述
        MATKL   TYPE MARA-MATKL, "物料组
        BDMNG   TYPE RESB-BDMNG, "需求数量
        ENMNG   TYPE RESB-ENMNG, "已领料数量
        CHARG   TYPE RESB-CHARG, "批次
        MEINS_Z TYPE RESB-MEINS, "单位
        RSNUM   TYPE AFKO-RSNUM, "订单需求号
        LYTS    TYPE AFKO-GAMNG, "领用套数
        BCLLS   TYPE AFKO-GAMNG, "本次领料数
        OBJNR   TYPE AUFK-OBJNR, "对象号
        GAMNG   TYPE AFKO-GAMNG, "订单数量
        WEMNG   TYPE AFPO-WEMNG, "已经提货数量
        KDAUF   TYPE AFPO-KDAUF, "销售订单号
        XMMC    TYPE CHAR255,    "项目名称
        MEINS   TYPE RESB-MEINS, "单位
        NAME    TYPE CHAR20,     "制单人
        STATU   TYPE ICONNAME,    "状态栏
        LOG     TYPE CHAR200,      "日志
      END OF TY_DATA.
TYPES:BEGIN OF TY_DATA1100,
        ZBOX    TYPE C,
        AUFNR   TYPE AFPO-AUFNR, "工单号
        LGORT   TYPE RESB-LGORT, "发货地点
        MATKL_Z TYPE MARA-MATKL, "子件物料组
        AUART   TYPE AUFK-AUART, "订单类型
        MATNR   TYPE AFPO-MATNR, "抬头物料类型
        KTEXT   TYPE AUFK-KTEXT, "描述
        MATNR_Z TYPE RESB-MATNR, "组建物料号
        MAKTX   TYPE MAKT-MAKTX, "成品料号
        MAKTX_Z TYPE MAKT-MAKTX, "组建物料描述
        MATKL   TYPE MARA-MATKL, "物料组
        BDMNG   TYPE RESB-BDMNG, "需求数量
        ENMNG   TYPE RESB-ENMNG, "已领料数量
        CHARG   TYPE RESB-CHARG, "批次
        MEINS_Z TYPE RESB-MEINS, "单位
        RSNUM   TYPE AFKO-RSNUM, "订单需求号
        LYTS    TYPE AFKO-GAMNG, "领用套数
        BCLLS   TYPE AFKO-GAMNG, "本次领料数
        OBJNR   TYPE AUFK-OBJNR, "对象号
        GAMNG   TYPE AFKO-GAMNG, "订单数量
        WEMNG   TYPE AFPO-WEMNG, "已经提货数量
        KDAUF   TYPE AFPO-KDAUF, "销售订单号
        XMMC    TYPE CHAR255,    "项目名称
        MEINS   TYPE RESB-MEINS, "单位
        NAME    TYPE CHAR20,     "制单人
        STATU   TYPE ICONNAME,   "状态栏
        LOG     TYPE CHAR200,       "日志
      END OF TY_DATA1100.
TYPES:BEGIN OF TY_DATA2110,
        ZBOX    TYPE C,
        AUFNR   TYPE AFPO-AUFNR, "工单号
        LGORT   TYPE RESB-LGORT, "发货地点
        MATKL_Z TYPE MARA-MATKL, "子件物料组
        AUART   TYPE AUFK-AUART, "订单类型
        MATNR   TYPE AFPO-MATNR, "抬头物料类型
        KTEXT   TYPE AUFK-KTEXT, "描述
        MATNR_Z TYPE RESB-MATNR, "组建物料号
        MAKTX   TYPE MAKT-MAKTX, "成品料号
        MAKTX_Z TYPE MAKT-MAKTX, "组建物料描述
        MATKL   TYPE MARA-MATKL, "物料组
        BDMNG   TYPE RESB-BDMNG, "需求数量
        ENMNG   TYPE RESB-ENMNG, "已领料数量
        CHARG   TYPE RESB-CHARG, "批次
        MEINS_Z TYPE RESB-MEINS, "单位
        RSNUM   TYPE AFKO-RSNUM, "订单需求号
        LYTS    TYPE AFKO-GAMNG, "领用套数
        BCLLS   TYPE AFKO-GAMNG, "本次领料数
        OBJNR   TYPE AUFK-OBJNR, "对象号
        GAMNG   TYPE AFKO-GAMNG, "订单数量
        WEMNG   TYPE AFPO-WEMNG, "已经提货数量
        KDAUF   TYPE AFPO-KDAUF, "销售订单号
        XMMC    TYPE CHAR255,    "项目名称
        MEINS   TYPE RESB-MEINS, "单位
        NAME    TYPE CHAR20,     "制单人
        STATU   TYPE ICONNAME,    "状态栏
        LOG     TYPE CHAR200,      "日志
      END OF TY_DATA2110.
TYPES:BEGIN OF TY_HD,
        WERKS TYPE WERKS_D,  "工厂
        NAME1 TYPE NAME1,   "工厂描述
        AUFNR TYPE AUFNR,   "工单单号
        XMMC  TYPE STRING,  "项目名称
        MATNR TYPE MATNR,   "生产料号
        SCSL  TYPE MENGE_D,  "生产数量
        MEINS TYPE MEINS,    "单位
        MAKTX TYPE MAKTX,    "品名
        ZDYCS TYPE I, "打印次数
      END OF TY_HD .

TYPES:BEGIN OF TY_ITEM,
        MATNR TYPE MATNR, "发料料号
        MAKTX TYPE MAKTX,  "品名/规格
        FLSL  TYPE MENGE_D, "发料数量
        LGORT TYPE LGORT_D,  "仓库
        CHARG TYPE RESB-CHARG, "批次
        SFSL  TYPE MENGE_D,  "实发数量
        MEINS TYPE MEINS,  "单位
      END OF TY_ITEM .
DATA:LT_ITEM TYPE TABLE OF TY_ITEM,
     LS_ITEM TYPE TY_ITEM.

DATA:LT_HD TYPE TABLE OF TY_HD,
     LS_HD TYPE TY_HD.

DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

*人员信息
TYPES:BEGIN OF TY_NAME,
        BNAME      TYPE USR21-BNAME,      "帐号
        PERSNUMBER TYPE USR21-PERSNUMBER, "人员编号
        NAME_LAST  TYPE ADRP-NAME_LAST,  "姓
      END OF TY_NAME.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.


DATA GT_DATA1100 TYPE TABLE OF TY_DATA1100.
DATA GS_DATA1100 TYPE TY_DATA1100.

DATA GT_DATA2110 TYPE TABLE OF TY_DATA2110.
DATA GS_DATA2110 TYPE TY_DATA2110.

DATA GT_AFPO TYPE TABLE OF TY_DATA.
DATA GS_AFPO TYPE TY_DATA.

DATA GT_RESB TYPE TABLE OF RESB.
DATA GS_RESB TYPE RESB.

*订单物料
DATA GT_MARA TYPE TABLE OF MARA.
DATA GS_MARA TYPE MARA.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

*组件
DATA GT_MARA1 TYPE TABLE OF MARA.
DATA GS_MARA1 TYPE MARA.

DATA GT_MAKT1 TYPE TABLE OF MAKT.
DATA GS_MAKT1 TYPE MAKT.

*获取状态长文本
DATA L_STTXT LIKE	BSVX-STTXT.

*点击打印按钮时检查，已领料数量+本次领料数量 不能大于 需求数量
DATA L_CHECK TYPE C.

*制单人姓名
DATA GS_NAME TYPE TY_NAME.

*打印
DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

*1100公司打印150522
DATA LT_DATA1100 TYPE TABLE OF TY_DATA1100.
DATA LS_DATA1100 TYPE TY_DATA1100.

*获取销售长文本
DATA LT_LINE TYPE TABLE OF TLINE.
DATA LS_LINE TYPE TLINE.
DATA L_NAME TYPE THEAD-TDNAME.

*增加生产计划内领料单打印次数统计
DATA:GT_ZPP003 TYPE TABLE OF ZPP003,
     GS_ZPP003 TYPE ZPP003.

*打印参数变量
DATA: CONTROL    TYPE SSFCTRLOP,
      NTOTALLINE TYPE I,
      NPAGELINE  TYPE I VALUE 9,
      P_INDEX    LIKE SY-TABIX.
DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
      NCURRLINE       TYPE I,      "中间变量
      JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA: G_NAME TYPE RS38L_FNAM.
DATA:L_FORMNAME TYPE TDSFNAME  VALUE 'ZSFPP003'.
DATA L_LINE TYPE I. "统计打印的行进行补行
DATA G_LINE TYPE I. "设定换页行数
DATA NAME   TYPE CHAR20. "打印人

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
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

  IF gw_lvc-fieldname = 'LYTS'
  OR gw_lvc-fieldname = 'BCLLS'.
     gw_lvc-NO_ZERO = 'X'.
  ENDIF.

  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,                      "SY-REPID 指 当前的主程序
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GW_EVENTS        LIKE LINE OF GT_EVENTS.
DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.

DATA: GW_ISTABLE TYPE LVC_S_STBL.

DATA G_EDIT TYPE C VALUE 'X'. "控制不可编辑

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_WERKS   FOR AFPO-DWERK OBLIGATORY NO-EXTENSION NO INTERVALS MEMORY ID ZPP003,"工厂
                S_AUFNR   FOR AFPO-AUFNR,"生产订单号
                S_MATNR   FOR AFPO-MATNR,"订单物料号
                S_DISPO   FOR AFKO-DISPO,"MRP控制者
                S_FEVOR   FOR AFKO-FEVOR,"生产管理员
                S_GSTRP   FOR AFKO-GSTRP,"基本开始日期
                S_GLTRP   FOR AFKO-GLTRP,"基本完成日期
                S_MATKLZ   FOR RESB-MATKL,"组建物料组
                S_MATNRZ  FOR AFPO-MATNR."组建物料号
SELECTION-SCREEN END OF BLOCK BLK1.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
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
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  S_WERKS-LOW.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH S_WERKS-LOW.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  SELECT * FROM AFKO
  INNER JOIN AFPO
  ON AFKO~AUFNR = AFPO~AUFNR
  INNER JOIN AUFK
  ON AFKO~AUFNR = AUFK~AUFNR
  INTO CORRESPONDING FIELDS OF TABLE GT_AFPO
  WHERE DWERK        IN S_WERKS
  AND   AFKO~AUFNR   IN S_AUFNR
  AND   MATNR        IN S_MATNR
  AND   DISPO        IN S_DISPO
  AND   FEVOR        IN S_FEVOR
  AND   GSTRP        IN S_GSTRP
  AND   GLTRP        IN S_GLTRP.

*排除处理状态为TECO,删除标记，LKD未激活的的生产订单
  PERFORM CHECK_WO_STATU TABLES GT_AFPO.

  CHECK GT_AFPO IS NOT INITIAL.

  SELECT * FROM MARA
  INTO CORRESPONDING FIELDS OF TABLE GT_MARA
  FOR ALL ENTRIES IN GT_AFPO
  WHERE MATNR = GT_AFPO-MATNR.

  SELECT * FROM MAKT
   INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
   FOR ALL ENTRIES IN GT_MARA
   WHERE MATNR = GT_MARA-MATNR.

*已经标记删除的预留行项目RESB-XLOEK
*已经标记最终发货的预留行项目RESB-KZEAR
*被标记为反冲的预留行项目RESB-RGEKZ
*允许的货物移动不勾选的行项目-虚拟件 RESB-XWAOK
  SELECT * FROM RESB
  INTO CORRESPONDING FIELDS OF TABLE GT_RESB
  FOR ALL ENTRIES IN GT_AFPO
  WHERE RSNUM = GT_AFPO-RSNUM
  AND   XLOEK <> 'X'
  AND   KZEAR <> 'X'
  AND   RGEKZ <> 'X'
  AND   XWAOK  = 'X'
  AND   MATNR IN S_MATNRZ
  AND   SHKZG <> 'S'.   "ADD 2015-3-11
*  AND   MATKL IN S_MATKLZ.

*删除需求数为0的行
  DELETE GT_RESB WHERE BDMNG  = 0.

  CHECK GT_RESB IS NOT INITIAL.

*查询子件的物料组，物料描述
  SELECT * FROM MARA
   INTO CORRESPONDING FIELDS OF TABLE GT_MARA1
   FOR ALL ENTRIES IN GT_RESB
   WHERE MATNR = GT_RESB-MATNR
   AND   MATKL IN S_MATKLZ.

  SELECT * FROM MAKT
 INTO CORRESPONDING FIELDS OF TABLE GT_MAKT1
 FOR ALL ENTRIES IN GT_MARA1
 WHERE MATNR = GT_MARA1-MATNR.

*获取制单人姓名
  SELECT SINGLE * FROM USR21
   INNER JOIN ADRP
   ON USR21~PERSNUMBER = ADRP~PERSNUMBER
   INTO CORRESPONDING FIELDS OF GS_NAME
   WHERE BNAME = SY-UNAME.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
  LOOP AT GT_RESB INTO GS_RESB.

*订单组件
    GS_DATA-MATNR_Z = GS_RESB-MATNR."组件物料号
    GS_DATA-BDMNG   = GS_RESB-BDMNG."需求数量
    GS_DATA-ENMNG   = GS_RESB-ENMNG."已领料数量
    GS_DATA-CHARG   = GS_RESB-CHARG."批次
    GS_DATA-MEINS_Z = GS_RESB-MEINS."单位
    GS_DATA-LGORT   = GS_RESB-LGORT."库存地点
*&--代码添加 BY HANDYBY 03.07.2017 12:34:46  BEGIN
    GS_DATA-MEINS = GS_RESB-MEINS."组件单位
*&--代码添加 BY HANDYBY 03.07.2017 12:34:46  END

*制单人
    GS_DATA-NAME    = GS_NAME-NAME_LAST.

*订单信息
    READ TABLE GT_AFPO INTO GS_AFPO
    WITH KEY RSNUM = GS_RESB-RSNUM.
    IF SY-SUBRC = 0.
      GS_DATA-AUFNR = GS_AFPO-AUFNR."工单号
      GS_DATA-AUART = GS_AFPO-AUART."订单类型
      GS_DATA-MATNR = GS_AFPO-MATNR."订单物料
      GS_DATA-KTEXT = GS_AFPO-KTEXT."描述
      GS_DATA-GAMNG = GS_AFPO-GAMNG."订单数量
      GS_DATA-WEMNG = GS_AFPO-WEMNG."提货数量
      GS_DATA-KDAUF = GS_AFPO-KDAUF."销售订单号

*&--代码注释 BY HANDYBY 03.07.2017 12:07:01  BEGIN
*    gs_data-meins = gs_afpo-meins."单位
*&--代码注释 BY HANDYBY 03.07.2017 12:07:01  END

    ENDIF.

*物料组
    READ TABLE GT_MARA INTO GS_MARA
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-MATKL = GS_MARA-MATKL.
    ENDIF.

*子件物料组
    READ TABLE GT_MARA1 INTO GS_MARA1
    WITH KEY MATNR = GS_DATA-MATNR_Z.
    IF SY-SUBRC = 0.
      GS_DATA-MATKL_Z = GS_MARA1-MATKL.
    ELSE.
      CONTINUE.
    ENDIF.

*订单物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

*组件物料描述
    READ TABLE GT_MAKT1 INTO GS_MAKT1
    WITH KEY MATNR = GS_DATA-MATNR_Z
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX_Z = GS_MAKT1-MAKTX.
    ENDIF.

*计算套数
    GS_DATA-LYTS = GS_DATA-GAMNG - GS_DATA-WEMNG.

*计算本次领料数量
    GS_DATA-BCLLS = GS_DATA-BDMNG - GS_DATA-ENMNG.

*获取销售订单上的文本，项目名称
    REFRESH LT_LINE.
    IF GS_DATA-KDAUF IS NOT INITIAL.
      L_NAME = GS_DATA-KDAUF.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          CLIENT                  = SY-MANDT
          ID                      = 'Z001'
          LANGUAGE                = SY-LANGU
          NAME                    = L_NAME
          OBJECT                  = 'VBBK'
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      LOOP AT LT_LINE INTO LS_LINE  .
        CONCATENATE GS_DATA-XMMC LS_LINE-TDLINE INTO GS_DATA-XMMC.
      ENDLOOP.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

  SORT GT_DATA BY AUFNR LGORT .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW .
  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA         = 'X'.
  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'STATU'         TEXT-016       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR'         TEXT-001       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'         TEXT-002       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'         TEXT-003       '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'MAKTX'         TEXT-004       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'         TEXT-008       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR_Z'       TEXT-005       '' '' '' '' 'X' 'MCHB' 'MATNR'.
  INIT_FIELDCAT 'MAKTX_Z'       TEXT-007       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL_Z'       TEXT-008       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BDMNG'         TEXT-009       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ENMNG'         TEXT-010       '' '' ''  '' '' '' ''.
  INIT_FIELDCAT 'LYTS'          TEXT-014       '' '' ''  'X' '' 'AFPO' 'WEMNG'.
  INIT_FIELDCAT 'BCLLS'         TEXT-015       '' '' ''  'X' '' 'AFPO' 'WEMNG'.
  INIT_FIELDCAT 'CHARG'         TEXT-011       '' '' ''  'X' '' 'MCHB' 'CHARG'.
  INIT_FIELDCAT 'MEINS'         TEXT-012       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LGORT'         TEXT-013       '' '' '' 'X' '' 'T001L' 'LGORT'.
  INIT_FIELDCAT 'LOG'           TEXT-017       '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
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
FORM FRM_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
  DATA L_SUBRC TYPE SY-SUBRC.
  CLEAR L_SUBRC.

  CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
*计算领料数量
    WHEN '&COMPUT'.
      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
          GS_DATA-BCLLS = GS_DATA-BDMNG / GS_DATA-GAMNG  * GS_DATA-LYTS.
          MODIFY GT_DATA FROM GS_DATA.
          CLEAR GS_DATA.
        ENDLOOP.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

*打印
    WHEN '&PRNT'.
*点击打印按钮时检查，已领料数量+本次领料数量 不能大于 需求数量
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        IF GS_DATA-ENMNG + GS_DATA-BCLLS > GS_DATA-BDMNG.
          MESSAGE S001(ZPP01) DISPLAY LIKE 'E'.
          L_CHECK = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

*需进行保存操作
      IF  L_CHECK <> 'X'.
        READ TABLE GT_DATA INTO GS_DATA
        WITH KEY ZBOX = 'X'.
        IF SY-SUBRC = 0.

          "排除2110 打印检查 By it02 20161019
          IF S_WERKS-LOW  NE '2110'.
            "  *检查库存数量
            PERFORM CHECK_INPUT CHANGING L_SUBRC.
          ELSE.

          ENDIF.

*          CHECK L_SUBRC <> 4.
*          PERFORM FRM_PRINT_DATA.
        ELSE.
          CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
          MESSAGE S003(Z001) DISPLAY LIKE 'E'.
        ENDIF.

*检查库存数量
        IF L_SUBRC <> 4.
          PERFORM FRM_PRINT_DATA.
        ENDIF.

      ENDIF.

* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'MATNR'
        AND GS_DATA-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD GS_DATA-MATNR.
        SET PARAMETER ID 'WRK' FIELD S_WERKS-LOW.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
      ENDIF.

      IF RS_SELFIELD-FIELDNAME = 'MATNR_Z'
        AND GS_DATA-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD GS_DATA-MATNR_Z.
        SET PARAMETER ID 'WRK' FIELD S_WERKS-LOW.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  PERFORM FRM_DATA_ENTER USING ER_DATA_CHANGED..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
  DATA WA_MOD_CELL TYPE LVC_S_MODI.
  DATA STBL       TYPE LVC_S_STBL.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  READ TABLE ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL INDEX 1.

  CHECK WA_MOD_CELL IS NOT INITIAL.

  READ TABLE GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
  IF SY-SUBRC = 0.
    CASE WA_MOD_CELL-FIELDNAME.
      WHEN 'LGORT'.
        GS_DATA-LGORT = WA_MOD_CELL-VALUE.
      WHEN 'LYTS'.
        GS_DATA-LYTS  = WA_MOD_CELL-VALUE.
      WHEN 'BCLLS'.
        GS_DATA-BCLLS = WA_MOD_CELL-VALUE.
      WHEN 'CHARG'.
        GS_DATA-CHARG = WA_MOD_CELL-VALUE.
    ENDCASE.

    MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
    CLEAR GS_DATA.
  ENDIF.

*自动定位光标
  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.

ENDFORM.                    "frm_data_enter
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .

  IF S_WERKS-LOW = '1100'.
    L_FORMNAME = 'ZSFPP0031100' .
  ELSEIF S_WERKS-LOW = '2110'.
    L_FORMNAME = 'ZSFPP0032110' .
*&--代码添加 BY HANDYBY 10.07.2017 16:14:53  BEGIN
  ELSEIF S_WERKS-LOW EQ '1610' .
    L_FORMNAME = 'ZSFPP003_1' .
*&--代码添加 BY HANDYBY 10.07.2017 16:14:53  END
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的smartforms的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN  = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

*根据工单号以及物料组进行排序
  SORT GT_DATA BY AUFNR LGORT .
  IF S_WERKS-LOW = '1100'.
    CLEAR GS_DATA1100 .
    CLEAR GT_DATA1100[].

    LOOP AT GT_DATA INTO GS_DATA  WHERE ZBOX = 'X' .
      CLEAR GS_DATA1100.
      MOVE-CORRESPONDING GS_DATA TO GS_DATA1100 .
      GS_DATA1100-AUFNR   = GS_DATA-AUFNR.
      GS_DATA1100-LGORT   = GS_DATA-LGORT .
      APPEND GS_DATA1100 TO GT_DATA1100.
    ENDLOOP.
  ELSEIF S_WERKS-LOW = '2110'.
    CLEAR GS_DATA2110 .
    CLEAR GT_DATA2110[].
    LOOP AT GT_DATA INTO GS_DATA  WHERE ZBOX = 'X' .
      MOVE-CORRESPONDING GS_DATA TO GS_DATA2110.
      APPEND GS_DATA2110 TO GT_DATA2110.
    ENDLOOP.
    SORT GT_DATA2110 BY AUFNR .
  ENDIF.

  IF S_WERKS-LOW = '1100'.
    PERFORM  PRTINTITEM1100 .
  ELSEIF S_WERKS-LOW = '2110'.
    PERFORM PRINT_2110. "新增2110工厂打印  by 20161107
  ELSE.
    PERFORM  PRTINTITEM .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_WO_STATU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_WO_STATU TABLES LT_AFPO.
  DATA LW_AFPO TYPE TY_DATA.

  LOOP AT LT_AFPO INTO LW_AFPO.
*统计当前行

    CALL FUNCTION 'STATUS_TEXT_EDIT'               "获取生产订单的状态
      EXPORTING
        OBJNR            = LW_AFPO-OBJNR
        SPRAS            = SY-LANGU
      IMPORTING
        LINE             = L_STTXT
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    FIND  'REL' IN L_STTXT.   "ALL OCCURRENCES OF
    IF SY-SUBRC <> 0 .
      DELETE  TABLE LT_AFPO FROM LW_AFPO.
      CONTINUE.
    ENDIF.

    FIND  'TECO' IN L_STTXT.   "ALL OCCURRENCES OF
    IF SY-SUBRC = 0 .
      DELETE  TABLE LT_AFPO FROM LW_AFPO.
      CONTINUE.
    ENDIF.

    FIND  'LKD' IN L_STTXT.
    IF SY-SUBRC = 0 .
      DELETE  TABLE LT_AFPO FROM LW_AFPO.
      CONTINUE.
    ENDIF.

*    FIND '锁定' IN L_STTXT.
*    IF SY-SUBRC = 0 .
*    DELETE  LT_AFPO WHERE AUFNR = LW_AFPO-AUFNR.
*    CONTINUE.
*    ENDIF.

    FIND  'DLFL' IN L_STTXT.
    IF SY-SUBRC = 0 .
      DELETE  TABLE LT_AFPO FROM LW_AFPO.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT  CHANGING L_SUBRC TYPE SY-SUBRC.
  DATA LT_MARD TYPE TABLE OF MARD.
  DATA LS_MARD TYPE MARD.
  DATA LS_RESB TYPE RESB.
  DATA LS_MSKA TYPE MSKA.
  DATA L_ANS.
  DATA L_STRING TYPE STRING.
  DATA L_MENGE TYPE MENGE_D.
  DATA LT_MCSD TYPE TABLE OF MCSD.
  DATA LS_MCSD TYPE MCSD.


  CLEAR L_ANS.

  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
    CLEAR LS_MARD.
    CLEAR LS_MSKA.
    CLEAR LS_RESB.

    CLEAR:LS_RESB.
    SELECT SINGLE * FROM RESB
      INTO CORRESPONDING FIELDS OF LS_RESB
      WHERE AUFNR = GS_DATA-AUFNR
      AND   MATNR = GS_DATA-MATNR_Z
      AND   XLOEK NE 'X'.

*表示按单生产
    IF LS_RESB-SOBKZ = 'E' AND SY-SUBRC = 0.
      SELECT SINGLE * FROM MSKA
        INTO CORRESPONDING FIELDS OF LS_MSKA
        WHERE VBELN = LS_RESB-KDAUF
        AND   POSNR = LS_RESB-KDPOS
        AND   WERKS = LS_RESB-WERKS
        AND   LGORT = GS_DATA-LGORT
        AND   MATNR = LS_RESB-MATNR.

      IF LS_MSKA-KALAB < GS_DATA-BCLLS.
        CLEAR L_STRING.
        CONCATENATE GS_DATA-MATNR_Z '物料在' LS_RESB-KDAUF '行项目' LS_RESB-KDPOS '销售订单下数量不足，不允许打印!'  INTO L_STRING.

        MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        GS_DATA-STATU = ICON_RED_LIGHT.
        GS_DATA-LOG   = L_STRING.
      ELSE.
        GS_DATA-STATU = ICON_GREEN_LIGHT.
      ENDIF.
    ELSE.

*表示按库生产
       CLEAR:L_MENGE.
      SELECT SINGLE * FROM MARD
     INTO CORRESPONDING FIELDS OF LS_MARD
     WHERE MATNR = GS_DATA-MATNR_Z
     AND   LGORT = GS_DATA-LGORT
     AND   WERKS = S_WERKS-LOW.
        if sy-subrc eq 0 .
            L_MENGE = ls_mard-labst.
        endif.

    "add by it02 客户库存数量 begin
     SELECT SINGLE * FROM MCSD
       INTO CORRESPONDING FIELDS OF ls_mcsd
       where MATNR = GS_DATA-MATNR_Z
     AND   LGORT = GS_DATA-LGORT
     AND   WERKS = S_WERKS-LOW.
        if sy-subrc eq 0 .
            L_MENGE = L_MENGE  + ls_MCSD-SDLAB.
        endif.

       IF L_MENGE < GS_DATA-BCLLS.
      "IF LS_MARD-LABST < GS_DATA-BCLLS.
      "add by it02 客户库存数量 end
        CLEAR L_STRING.
        CONCATENATE GS_DATA-MATNR_Z '物料在' GS_DATA-LGORT '库存地点下数量不足，不允许打印!'  INTO L_STRING.

        MESSAGE L_STRING TYPE 'S' DISPLAY LIKE 'E'.
        L_SUBRC = 4.
        GS_DATA-STATU = ICON_RED_LIGHT.
        GS_DATA-LOG   = L_STRING.
      ELSE.
        GS_DATA-STATU = ICON_GREEN_LIGHT.
      ENDIF.
    ENDIF.

*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          TITLEBAR              = '确认打印'
**         DIAGNOSE_OBJECT       = ' '
*          TEXT_QUESTION         = L_STRING
*          TEXT_BUTTON_1         = '是'(B01)
**         ICON_BUTTON_1         = ' '
*          TEXT_BUTTON_2         = '否'(B02)
**         ICON_BUTTON_2         = ' '
**         DEFAULT_BUTTON        = '1'
*          DISPLAY_CANCEL_BUTTON = ''
**         USERDEFINED_F1_HELP   = ' '
**         START_COLUMN          = 25
**         START_ROW             = 6
**         POPUP_TYPE            =
**         IV_QUICKINFO_BUTTON_1 = ' '
**         IV_QUICKINFO_BUTTON_2 = ' '
*        IMPORTING
*          ANSWER                = L_ANS
**   TABLES
**         PARAMETER             =
*        EXCEPTIONS
*          TEXT_NOT_FOUND        = 1
*          OTHERS                = 2.
*      IF SY-SUBRC <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      IF L_ANS EQ '1'.
*        L_SUBRC = 0.
*        CONTINUE.
*      ELSE.
*        L_SUBRC = 4.
*        EXIT.
*      ENDIF.

    MODIFY GT_DATA FROM GS_DATA.
    CLEAR GS_DATA.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRTINTITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA  text
*----------------------------------------------------------------------*
FORM PRTINTITEM. .


*根据订单号，行项目号进行排序
  LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
*根据生产订单前7位不同和物料组不同进行分页

    AT NEW AUFNR+0(7) .
      REFRESH LT_DATA.
      CLEAR L_LINE.
    ENDAT.
    AT NEW LGORT.
      REFRESH LT_DATA.
      CLEAR L_LINE.
    ENDAT.

    APPEND GS_DATA TO LT_DATA .
    SORT LT_DATA BY MATNR_Z.  " 打印时按照组件物料号大小排序 BY handgxl 20151020
    CLEAR GS_DATA.

    AT END OF LGORT.
*每页补齐10行
      DESCRIBE TABLE LT_DATA LINES L_LINE.
      WHILE L_LINE MOD 8 <> 0.
        L_LINE = L_LINE + 1.
        APPEND INITIAL LINE TO LT_DATA.
      ENDWHILE.
*设定10行自动换页
      G_LINE = 8.

*取打印人
      CLEAR NAME.
      READ TABLE LT_DATA INTO LS_DATA INDEX 1.
      NAME =  LS_DATA-NAME.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
          NAME               = NAME
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CONTINUE.
    ENDAT.

    AT END OF AUFNR+0(7) .
*每页补齐10行

      DESCRIBE TABLE LT_DATA LINES L_LINE.
      WHILE L_LINE MOD 8 <> 0.
        L_LINE = L_LINE + 1.
        APPEND INITIAL LINE TO LT_DATA.
      ENDWHILE.

      "*设定10行自动换页
      G_LINE = 8.

      " *取打印人
      CLEAR NAME.
      READ TABLE LT_DATA INTO LS_DATA INDEX 1.
      NAME =  LS_DATA-NAME.
      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
          NAME               = NAME
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
FORM PRTINTITEM1100. .
  "*根据订单号，行项目号进行排序

  LOOP AT GT_DATA1100 INTO GS_DATA1100 WHERE ZBOX = 'X'.
*根据生产订单前7位不同和物料组不同进行分页

    AT NEW AUFNR+0(7) .

      REFRESH LT_DATA1100.
      CLEAR L_LINE.
    ENDAT.

    APPEND GS_DATA1100 TO LT_DATA1100 .

*    CLEAR GS_DATA.

    AT END OF AUFNR+0(7) .
*每页补齐10行

      DESCRIBE TABLE LT_DATA1100 LINES L_LINE.
      WHILE L_LINE MOD 8 <> 0.
        L_LINE = L_LINE + 1.
        APPEND INITIAL LINE TO LT_DATA1100.
      ENDWHILE.

*设定10行自动换页
      G_LINE = 8.

*取打印人
      CLEAR NAME.
      READ TABLE LT_DATA1100 INTO LS_DATA1100 INDEX 1.
      NAME =  LS_DATA1100-NAME.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
          NAME               = NAME
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_2110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_2110 .
  "*根据工单号换页打印
  DATA:L_TABIX TYPE I .
  DATA:P_NAME1 TYPE NAME1.
  SELECT SINGLE NAME1 INTO P_NAME1 FROM T001W
     WHERE WERKS = S_WERKS-LOW .
  REFRESH:LT_HD,LT_ITEM.
  LOOP AT GT_DATA2110 INTO GS_DATA2110.
    L_TABIX = SY-TABIX .
    AT NEW AUFNR+0(7) .
      REFRESH:LT_HD,LT_ITEM .
      "工厂
      LS_HD-WERKS = S_WERKS.
      "工厂描述
      LS_HD-NAME1 = P_NAME1.
      READ TABLE GT_DATA2110 INTO GS_DATA2110 INDEX L_TABIX.
      IF SY-SUBRC EQ 0 .
        "工单单号
        LS_HD-AUFNR = GS_DATA2110-AUFNR.
        "项目名称
        PERFORM SELXMMC USING LS_HD-AUFNR SY-LANGU CHANGING LS_HD-XMMC.
        "生产料号
        LS_HD-MATNR = GS_DATA2110-MATNR.
        "生产数量
        LS_HD-SCSL = GS_DATA2110-LYTS .
        "单位
        LS_HD-MEINS = GS_DATA2110-MEINS.
        "品名
        LS_HD-MAKTX = GS_DATA2110-MAKTX.
      ENDIF.
      SELECT SINGLE * INTO GS_ZPP003 FROM ZPP003 WHERE AUFNR =  LS_HD-AUFNR .
      IF SY-SUBRC EQ 0 .
        "打印次数增加1
        LS_HD-ZDYCS = GS_ZPP003-ZDYCS + 1.
      ELSE.
        LS_HD-ZDYCS  = 1. "初次打印默认为1
      ENDIF.
      APPEND LS_HD TO LT_HD .
    ENDAT.
    CLEAR:LS_ITEM .
    "发料料号
    LS_ITEM-MATNR = GS_DATA2110-MATNR_Z.
    "品名/规格
    LS_ITEM-MAKTX = GS_DATA2110-MAKTX_Z.
    "发料数量
    LS_ITEM-FLSL = GS_DATA2110-BCLLS.

    "仓库
    LS_ITEM-LGORT = GS_DATA2110-LGORT.
    "批次
    LS_ITEM-CHARG = GS_DATA2110-CHARG .
    "实发数量
    "单位
    LS_ITEM-MEINS = GS_DATA2110-MEINS_Z .
    APPEND LS_ITEM TO LT_ITEM .
    AT END OF AUFNR+0(7).
      SORT LT_ITEM BY MATNR .
      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          G_LINE             = G_LINE
          NAME               = NAME
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        CLEAR:GS_ZPP003.
        "增加工单号维度的打印次数
        SELECT SINGLE * INTO GS_ZPP003 FROM ZPP003 WHERE AUFNR =  LS_HD-AUFNR .
        IF SY-SUBRC EQ 0 .
          "打印次数增加1
          GS_ZPP003-ZDYCS = GS_ZPP003-ZDYCS + 1.
        ELSE.
          GS_ZPP003-AUFNR =  LS_HD-AUFNR .
          GS_ZPP003-ZDYCS  = 1. "初次打印默认为1
        ENDIF.
        APPEND GS_ZPP003 TO GT_ZPP003 .
      ENDIF.
    ENDAT.

  ENDLOOP.


  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   error handling
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF GT_ZPP003 IS NOT INITIAL.

    MODIFY ZPP003 FROM GS_ZPP003 .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_HD_AUFNR  text
*      -->P_SY_LANGU  text
*      <--P_GS_HD_XMMC  text
*----------------------------------------------------------------------*
FORM SELXMMC  USING    P_VBELN TYPE AUFNR
                       P_YY     TYPE SPRAS
              CHANGING P_XMMC TYPE STRING.


  " 取项目名称 - 销售订单抬头文本
  REFRESH:IT_LINES.
  G_OBJNAME = P_VBELN+0(7).
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      ID                      = 'Z001'
      LANGUAGE                = P_YY
      NAME                    = G_OBJNAME
      OBJECT                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC = 0.
    READ TABLE IT_LINES INTO WA_LINES INDEX 1.
    IF SY-SUBRC = 0.
      P_XMMC = WA_LINES-TDLINE.
    ENDIF.
  ENDIF.
ENDFORM.
