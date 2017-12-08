REPORT ZFI034.
TABLES:PROJ ,PRPS  ,ZFI031 ,ZFI005,COSP,MSEG.
TYPE-POOLS:SLIS.
DATA:BEGIN OF GS_DATA,
   " BUKRS  LIKE ZFI031-BUKRS,  "公司代码
    PSPID  LIKE PROJ-PSPID,  "项目定义
    POST1  LIKE PROJ-POST1,    "项目名称
    VERNA LIKE PROJ-VERNA ,    "项目经理
    GJAHR  LIKE ZFI031-GJAHR,   "年度
    MONAT  TYPE MONAT ,       "期间
    JHSK_DY TYPE TSLVT12,     "计划收款-当月
    JHSK_LJ TYPE TSLVT12 ,    "计划收款-累计
    SJSK_DY TYPE TSLVT12,      "实际收款-当月
    SJSK_LJ TYPE TSLVT12,      "实际收款-累计
    JHCZ_DY TYPE TSLVT12,      "计划产值-当月
    JHCZ_LJ TYPE TSLVT12,      "计划产值-累计
    SJCZ_DY TYPE TSLVT12,      "实际产值-当月
    SJCZ_LJ TYPE TSLVT12,      "实际产值-累计
    SJKP_DY TYPE TSLVT12,      "实际开票-当月
    SJKP_LJ TYPE TSLVT12,      "实际开票-累计
    YKPWSK_DY TYPE TSLVT12,       "已开票未收款-当月
    YKPWSK_LJ TYPE TSLVT12,     "已开票未收款-累计
    NDJHCZ    TYPE TSLVT12,     "年度计划产值
    NDJHSK     TYPE TSLVT12,    "年度计划收款
    SEL(1),


  END OF GS_DATA.

DATA:BEGIN OF  GS_ZFI031_SUM,
     PSPID LIKE ZFI031-PSPID,  "项目定义
     YSLX  LIKE ZFI031-YSLX,    "预算类型
     NDZ   LIKE ZFI031-ZNDZE,     "年度值
     YDZ   LIKE ZFI031-Y01,     "月度值
     YDLJZ  TYPE TSLVT12,        "月度累计值

  END OF GS_ZFI031_SUM .

DATA:BEGIN OF GS_ZFI005_SUM,
     PSPID LIKE ZFI031-PSPID, "项目定义
     SJSK  TYPE  TSLVT12,       "实际收款值
     SJSKLJZ TYPE TSLVT12,


  END OF GS_ZFI005_SUM.

DATA: BEGIN OF GS_COSP_SUM,
    PSPID LIKE PROJ-PSPID,
    KPZ   TYPE TSLVT12, "开票值
    KPLJZ TYPE TSLVT12,"
  END OF GS_COSP_SUM.

DATA: BEGIN OF GS_MSEG_SUM,
      PSPID LIKE PROJ-PSPID,  "项目定义

     " PSPID_WBS LIKE PROJ-PSPID ,  "WBS号
    "  CKSL   LIKE MSEG-MENGE,     "出库数量
      DYCZ    TYPE TSLVT12,   "当月产值
      LJCZ    TYPE TSLVT12 ,   "累计产值
   END OF GS_MSEG_SUM.

DATA: GT_MSEG_SUM LIKE TABLE OF GS_MSEG_SUM WITH HEADER LINE.


DATA:GT_COSP_SUM LIKE TABLE OF GS_COSP_SUM WITH HEADER LINE.

"DATA: GT_COSP_SUM_LJ LIKE TABLE OF GS_COSP_SUM WITH HEADER LINE.

DATA:GT_ZFI005_SUM LIKE TABLE OF GS_ZFI005_SUM WITH HEADER LINE.

*DATA:GT_ZFI005_SUM_LJ LIKE TABLE OF GS_ZFI005_SUM WITH HEADER LINE.

DATA:GT_ZFI031_SUM LIKE TABLE OF GS_ZFI031_SUM WITH HEADER LINE.

DATA: GT_DATA LIKE TABLE OF GS_DATA WITH HEADER LINE.

DATA: GT_PROJ LIKE TABLE OF PROJ WITH HEADER LINE .

DATA: GT_PRPS LIKE TABLE OF PRPS  WITH HEADER LINE.

DATA: GT_ZFI005 LIKE TABLE OF ZFI005 WITH HEADER LINE.


DATA: GT_ZFI031 LIKE TABLE OF ZFI031 WITH HEADER LINE.

DATA: GT_COSP_PRPS   LIKE TABLE OF COSP WITH HEADER LINE.

DATA: GT_COSP_AUFK  LIKE TABLE OF COSP WITH HEADER LINE.

"DATA: GT_COSP_LJ  LIKE TABLE OF COSP WITH HEADER LINE.

DATA: GT_AUFK LIKE TABLE OF AUFK WITH HEADER LINE.

"DATA:GT_MSEG_DY LIKE TABLE OF MSEG WITH HEADER LINE.

DATA:GT_MSEG LIKE TABLE OF MSEG WITH HEADER LINE.

DATA:GT_ZMM024 LIKE TABLE OF ZMM024 WITH HEADER LINE.

DATA:GT_ZPSPROD LIKE TABLE OF ZPSPROD WITH HEADER LINE.

DATA:GT_ZPS007A LIKE TABLE OF ZPS007A  WITH HEADER LINE.
DATA: INMEG TYPE STRING.


  DATA E_WBS_ECP        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES    TYPE TABLE OF KIS1.
  DATA LS_COST_LINES    TYPE KIS1.


 DATA:F_DATE LIKE SY-DATUM.
  DATA:E_DATE LIKE SY-DATUM.

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
 " gw_lvc-checkbox = &5.
  gw_lvc-cfieldname = &5.
  IF &6 = 'X'.
      gw_lvc-edit = 'X'.
  ENDIF.

*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.

  gw_lvc-ref_table = &8.
  gw_lvc-ref_field = &9.

IF gw_lvc-fieldname = 'PROJK'.
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

RANGES: R_YSLX FOR ZFI031-YSLX.

  FIELD-SYMBOLS: <LFS_ZFI031>.

 DATA: L_FIELD  TYPE STRING.

 DATA:P_LJ TYPE I  .

  DATA:P_MONAT_01 TYPE BKPF-MONAT .

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 .
PARAMETERS:   P_BUKRS LIKE BKPF-BUKRS OBLIGATORY DEFAULT '1800',   "公司代码

              P_GJAHR LIKE BKPF-GJAHR  OBLIGATORY DEFAULT SY-DATUM+0(4),  "年度
              P_MONAT LIKE BKPF-MONAT . "期间


SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID .     "项目定义


SELECTION-SCREEN END OF BLOCK B1.
AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  "*权限检查检查公司代码

  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    CONCATENATE '无权查询' P_BUKRS '产值收款实际计划对比报表'  INTO  INMEG .
    MESSAGE INMEG   TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  CONCATENATE P_GJAHR P_MONAT '01' INTO F_DATE.

  "取最后一天日期
   CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN                  =  F_DATE
      IMPORTING
       LAST_DAY_OF_MONTH       = E_DATE
     EXCEPTIONS
      DAY_IN_NO_DATE          = 1
      OTHERS                  = 2 .

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE GT_PROJ
    FROM PROJ
    WHERE PSPID IN S_PSPID.
  SORT GT_PROJ BY PSPID.
  CHECK GT_PROJ[] IS NOT INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZFI031
    FROM ZFI031
    WHERE  BUKRS = P_BUKRS AND PSPID IN S_PSPID AND GJAHR = P_GJAHR .

   SORT GT_ZFI031  BY BUKRS ASCENDING PSPID ASCENDING GJAHR ASCENDING  YSLX  ASCENDING  XH DESCENDING .
   DELETE ADJACENT DUPLICATES FROM GT_ZFI031 COMPARING PSPID GJAHR YSLX   .

 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZFI005
   FROM ZFI005
   WHERE BUKRS = P_BUKRS AND STEP = '@08@'   AND POSTDAT <= E_DATE AND XMBH IN S_PSPID AND ( YWZL = '项目回款' OR  YWZL = '项目预收' ).

SORT GT_ZFI005 BY XMBH POSTDAT.



 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
   FROM PRPS
   FOR ALL ENTRIES IN GT_PROJ
   WHERE PSPHI = GT_PROJ-PSPNR.

  SORT GT_PRPS BY PSPNR POSID .

 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AUFK
   FROM AUFK
   FOR ALL ENTRIES IN GT_PRPS
   WHERE BUKRS = P_BUKRS AND PSPEL = GT_PRPS-PSPNR.

 SORT GT_AUFK  BY AUFNR .

CHECK GT_PRPS[] IS NOT INITIAL.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_COSP_PRPS
   FROM COSP
   FOR ALL ENTRIES IN GT_PRPS
   WHERE OBJNR = GT_PRPS-OBJNR AND GJAHR <= P_GJAHR  AND VERSN = '000' AND WRTTP = '04' AND  ( KSTAR LIKE '6001%' OR KSTAR LIKE '6051%' OR  KSTAR LIKE '6301%'  ) .

SORT GT_COSP_PRPS BY OBJNR .

 CHECK GT_AUFK[] IS NOT INITIAL.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_COSP_AUFK
   FROM COSP
   FOR ALL ENTRIES IN GT_AUFK
   WHERE OBJNR = GT_AUFK-OBJNR AND GJAHR <= P_GJAHR  AND VERSN = '000' AND WRTTP = '04' AND  ( KSTAR LIKE '6001%' OR KSTAR LIKE '6051%' OR  KSTAR LIKE '6301%'  ) .

SORT GT_COSP_AUFK BY OBJNR .

*SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MSEG_DY
*  FROM MSEG
*  FOR ALL ENTRIES IN GT_PRPS
*  WHERE  MAT_PSPNR = GT_PRPS-PSPNR
*  AND  BUDAT_MKPF >= F_DATE AND BUDAT_MKPF <= E_DATE AND BWART IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .
CHECK GT_PRPS[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
   FROM MSEG
  FOR ALL ENTRIES IN GT_PRPS
  WHERE  WERKS = P_BUKRS
  AND MAT_PSPNR = GT_PRPS-PSPNR
  AND   BUDAT_MKPF <= E_DATE AND BWART IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .

SORT GT_MSEG BY MAT_PSPNR MATNR   BUDAT_MKPF .

CHECK GT_PROJ[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
  FROM ZMM024
  FOR ALL ENTRIES IN GT_PROJ
  WHERE POSID = GT_PROJ-PSPID.

SORT GT_ZMM024 BY POSID.

 SELECT * FROM ZPSPROD
    INTO CORRESPONDING FIELDS OF TABLE GT_ZPSPROD    "期初项目产值

    WHERE PSPID  IN S_PSPID.

 SORT GT_ZPSPROD BY PSPID.

SELECT * FROM ZPS007A
  INTO CORRESPONDING FIELDS OF TABLE GT_ZPS007A
  WHERE PSPID IN S_PSPID.


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

 P_LJ = P_MONAT - 1.
 "统计计划值 、收款值
 PERFORM  JH_SH_TOTAL.

"统计实际收款 当月、累计汇总值
PERFORM SJSK_DY_LJ_TOTOAL.


"统计 实际开票 当月 、累计汇总值

 PERFORM SJKP_DY_LJ_TOTOAL.


"统计实际产值 当月 累计 汇总值

PERFORM SJCZ_DY_LJ_TOTAL.


" 把以上统计项目值汇总 每个项目 中
LOOP  AT GT_PROJ.
  CLEAR :GS_DATA,GT_DATA.
  GS_DATA-PSPID = GT_PROJ-PSPID. "项目定义
  GS_DATA-POST1 = GT_PROJ-POST1. "项目名称
  GS_DATA-VERNA = GT_PROJ-VERNA."项目经理
  GS_DATA-GJAHR = P_GJAHR."年度
  GS_DATA-MONAT = P_MONAT ."期间
  READ TABLE GT_ZFI031_SUM WITH KEY  PSPID = GS_DATA-PSPID  YSLX = '2'.
  IF SY-SUBRC = 0.
    GS_DATA-JHSK_DY = GT_ZFI031_SUM-YDZ.               "计划收款-当月
    GS_DATA-JHSK_LJ = GT_ZFI031_SUM-YDLJZ.               "计划收款-累计
    GS_DATA-NDJHSK = GT_ZFI031_SUM-NDZ.               "计划收款-累计
   ENDIF.
   READ TABLE GT_ZFI031_SUM WITH KEY  PSPID = GS_DATA-PSPID  YSLX = '1'.
  IF SY-SUBRC = 0.
    GS_DATA-JHCZ_DY = GT_ZFI031_SUM-YDZ.               "计划产值-当月
    GS_DATA-JHCZ_LJ = GT_ZFI031_SUM-YDLJZ.               "计划产值-累计
    GS_DATA-NDJHCZ  = GT_ZFI031_SUM-NDZ.               "年度计划产值
   ENDIF.

  READ TABLE GT_ZFI005_SUM  WITH KEY PSPID = GS_DATA-PSPID .
  IF SY-SUBRC = 0.
    GS_DATA-SJSK_DY = GT_ZFI005_SUM-SJSK ."实际收款-当月
    GS_DATA-SJSK_LJ = GT_ZFI005_SUM-SJSKLJZ .""实际收款-累计

   ENDIF.

   READ TABLE GT_ZPS007A WITH KEY PSPID = GS_DATA-PSPID.
    IF SY-SUBRC = 0 .

      GS_DATA-SJSK_LJ =  GS_DATA-SJSK_LJ +  GT_ZPS007A-LJSKJE.  "实际收款累计 - 需加上期初实际收款累计
     ENDIF.

   READ TABLE GT_COSP_SUM WITH KEY PSPID = GS_DATA-PSPID.
   IF SY-SUBRC = 0.
       IF GS_DATA-PSPID+4(2) = '22' OR GS_DATA-PSPID+4(2) = '23' .

          GS_DATA-SJKP_DY = GT_COSP_SUM-KPZ  *  ( 117 / 100 ) .  "实际开票-当月
          GS_DATA-SJKP_LJ = GT_COSP_SUM-KPZ  *  ( 117 / 100 ) .  "实际开票-当月
       ELSE.

          GS_DATA-SJKP_DY = GT_COSP_SUM-KPZ .
          GS_DATA-SJKP_LJ = GT_COSP_SUM-KPZ .
        ENDIF.


   ENDIF.

 READ TABLE GT_MSEG_SUM WITH KEY  PSPID = GS_DATA-PSPID.

 IF SY-SUBRC = 0.

   GS_DATA-SJCZ_DY = GT_MSEG_SUM-DYCZ .    " "实际产值-当月
   GS_DATA-SJCZ_LJ = GT_MSEG_SUM-LJCZ .    "实际产值-累计

  ENDIF.

   GS_DATA-YKPWSK_DY = GS_DATA-SJKP_DY  - GS_DATA-SJSK_DY .

   GS_DATA-YKPWSK_LJ = GS_DATA-SJKP_LJ  - GS_DATA-SJSK_LJ .

"累计期初产值
READ TABLE GT_ZPSPROD WITH KEY PSPID = GS_DATA-PSPID  BINARY SEARCH .
IF SY-SUBRC = 0.
  GS_DATA-SJCZ_LJ = GS_DATA-SJCZ_LJ  +  GT_ZPSPROD-ZCZ.  "累加 项目期初产值
 ENDIF.

APPEND GS_DATA TO GT_DATA.

ENDLOOP.

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
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_BUILD_EVENT.
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
    GW_LAYOUT-ZEBRA        = 'X'.
  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  GW_LAYOUT-BOX_FNAME = 'SEL'.
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
     INIT_FIELDCAT 'PSPID'       '项目定义'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'POST1'       '项目名称'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'VERNA'       '项目经理'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'GJAHR'       '年度'                 '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'MONAT'       '期间'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'JHSK_DY'       '计划收款-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'JHSK_LJ'       '计划收款-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJSK_DY'       '实际收款-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJSK_LJ'       '实际收款-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'JHCZ_DY'       '计划产值-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'JHCZ_LJ'       '计划产值-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJCZ_DY'       '实际产值-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJCZ_LJ'       '实际产值-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJKP_DY'       '实际开票-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SJKP_LJ'       '实际开票-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'YKPWSK_DY'       '已开票未收款-当月'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'YKPWSK_LJ'       '已开票未收款-累计'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NDJHCZ'       '年度计划产值'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'NDJHSK'       '年度计划收款'                 '' '' '' '' '' '' ''.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0262   text
*      -->P_0263   text
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
*      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
    "  IT_EVENTS                = GT_EVENTS[]
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
      T_OUTTAB                 = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_M_PSPID  text
*      -->P_GT_ZMM024_MATNR  text
*      <--P_M_GPREIS  text
*----------------------------------------------------------------------*
FORM WLZHDJ_JS  USING    P_PSPID  LIKE PROJ-PSPID
                         P_MATNR  LIKE MSEG-MATNR
                CHANGING P_GPREIS LIKE CKIS-GPREIS.
CLEAR:E_WBS_ECP,
          LS_E_WBS_ECP,
          LS_COST_LINES,
          P_GPREIS.

    REFRESH:LT_E_WBS_ECP,
            LT_COST_LINES.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        I_PROJ_DEF    = P_PSPID
        I_VERSION     = '100'
      IMPORTING
        E_WBS_ECP     = E_WBS_ECP
      EXCEPTIONS
        ERROR_MESSAGE = 1.

    LT_E_WBS_ECP = E_WBS_ECP.

        IF  LT_E_WBS_ECP IS NOT INITIAL.
      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
      INDEX 1.
      IF SY-SUBRC = 0.
        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

        READ TABLE   LT_COST_LINES INTO LS_COST_LINES
        WITH KEY  TYPPS = 'M'
                  MATNR = P_MATNR.
        IF SY-SUBRC = 0.
          P_GPREIS = LS_COST_LINES-GPREIS.
        ENDIF.
      ENDIF.
    ENDIF.
ENDFORM.

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  JH_SH_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM JH_SH_TOTAL .
    LOOP AT GT_ZFI031 .
     CLEAR: GS_ZFI031_SUM ,GT_ZFI031_SUM.
     GT_ZFI031_SUM-PSPID = GT_ZFI031-PSPID.
     GT_ZFI031_SUM-YSLX = GT_ZFI031-YSLX.
     GT_ZFI031_SUM-NDZ = GT_ZFI031-ZNDZE .
     CONCATENATE 'Y' P_MONAT INTO L_FIELD .

     ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_ZFI031 TO <LFS_ZFI031> .
     IF SY-SUBRC EQ 0 .
       GT_ZFI031_SUM-YDZ = <LFS_ZFI031>.
       GT_ZFI031_SUM-YDLJZ =  GS_ZFI031_SUM-YDLJZ   + <LFS_ZFI031> .

     ENDIF.
     P_MONAT_01 = '01'.
     DO P_LJ TIMES .

      CONCATENATE 'Y' P_MONAT_01 INTO L_FIELD .
      ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_ZFI031 TO <LFS_ZFI031> .
     IF SY-SUBRC EQ 0 .

       GT_ZFI031_SUM-YDLJZ =  GT_ZFI031_SUM-YDLJZ   + <LFS_ZFI031> .

     ENDIF.

     P_MONAT_01 = P_MONAT_01 + 1.

      ENDDO.

     COLLECT GT_ZFI031_SUM .
   ENDLOOP.
  SORT GT_ZFI031_SUM BY PSPID .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SJSK_DY_LJ_TOTOAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SJSK_DY_LJ_TOTOAL .

LOOP AT GT_ZFI005 .
   CLEAR GT_ZFI005_SUM.
   GT_ZFI005_SUM-PSPID  = GT_ZFI005-XMBH.
   IF GT_ZFI005-POSTDAT >= F_DATE AND GT_ZFI005-POSTDAT <= E_DATE.

     GT_ZFI005_SUM-SJSK = GT_ZFI005-HSL.

   ENDIF.
     IF GT_ZFI005-POSTDAT <= E_DATE.

     GT_ZFI005_SUM-SJSKLJZ = GT_ZFI005-HSL.

   ENDIF.

   COLLECT GT_ZFI005_SUM.


ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SJKP_DY_LJ_TOTOAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SJKP_DY_LJ_TOTOAL .
 FIELD-SYMBOLS: <LFS_COSP>.

LOOP AT GT_COSP_PRPS.
   CLEAR GT_COSP_SUM.
   READ TABLE GT_PRPS WITH KEY OBJNR = GT_COSP_PRPS-OBJNR.
   IF SY-SUBRC = 0.
       READ TABLE GT_PROJ WITH KEY PSPNR = GT_PRPS-PSPHI.
         IF SY-SUBRC = 0.
           GT_COSP_SUM-PSPID = GT_PROJ-PSPID.

           ENDIF.
    ENDIF.
       CONCATENATE 'WTG0' P_MONAT INTO L_FIELD .

    IF GT_COSP_PRPS-GJAHR = P_GJAHR .
      ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_COSP_PRPS TO <LFS_COSP> .
     IF SY-SUBRC EQ 0 .

         IF  <LFS_COSP> < 0 .
              <LFS_COSP> =  <LFS_COSP> * -1 .

          ENDIF.
         GT_COSP_SUM-KPZ = <LFS_COSP>.
         GT_COSP_SUM-KPLJZ =  GT_COSP_SUM-KPLJZ   + <LFS_COSP> .

       ENDIF.


          P_MONAT_01 = '01'.
     DO P_LJ TIMES .

      CONCATENATE 'WTG0' P_MONAT_01 INTO L_FIELD .
      ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_COSP_SUM TO <LFS_COSP> .
     IF SY-SUBRC EQ 0 .
         IF  <LFS_COSP> < 0 .
              <LFS_COSP> =  <LFS_COSP> * -1 .

          ENDIF.

       GT_COSP_SUM-KPLJZ =  GT_COSP_SUM-KPLJZ   + <LFS_COSP> .

     ENDIF.

     P_MONAT_01 = P_MONAT_01 + 1.

      ENDDO.


       ELSE.

         GT_COSP_SUM-KPLJZ =    GT_COSP_PRPS-WTG001 + GT_COSP_PRPS-WTG002 + GT_COSP_PRPS-WTG003 + GT_COSP_PRPS-WTG004 + GT_COSP_PRPS-WTG005 + GT_COSP_PRPS-WTG006 + GT_COSP_PRPS-WTG007 + GT_COSP_PRPS-WTG008
                  + GT_COSP_PRPS-WTG009 + GT_COSP_PRPS-WTG010 + GT_COSP_PRPS-WTG011 + GT_COSP_PRPS-WTG012 + GT_COSP_PRPS-WTG013 + GT_COSP_PRPS-WTG014 + GT_COSP_PRPS-WTG015 + GT_COSP_PRPS-WTG016.
    ENDIF.
   COLLECT GT_COSP_SUM.

ENDLOOP.

SORT GT_COSP_SUM BY PSPID.

LOOP AT GT_COSP_AUFK.
   CLEAR GT_COSP_SUM.

   READ TABLE GT_AUFK WITH KEY OBJNR = GT_COSP_AUFK-OBJNR .
   IF SY-SUBRC = 0.
        READ TABLE GT_PRPS WITH KEY PSPNR = GT_AUFK-PSPEL.
         IF SY-SUBRC = 0.
             READ TABLE GT_PROJ WITH KEY PSPNR = GT_PRPS-PSPHI.
               IF SY-SUBRC = 0.
                   GT_COSP_SUM-PSPID = GT_PROJ-PSPID.

               ENDIF.
          ENDIF.
    ENDIF.

       CONCATENATE 'Y' P_MONAT INTO L_FIELD .

    IF GT_COSP_PRPS-GJAHR = P_GJAHR .
      ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_COSP_AUFK TO <LFS_COSP> .
     IF SY-SUBRC EQ 0 .

        GT_COSP_SUM-KPZ = <LFS_COSP>.

       ENDIF.


          P_MONAT_01 = '01'.
     DO P_LJ TIMES .

      CONCATENATE 'WTG0' P_MONAT_01 INTO L_FIELD .
      ASSIGN COMPONENT L_FIELD OF STRUCTURE GT_COSP_AUFK TO <LFS_COSP> .
     IF SY-SUBRC EQ 0 .

       GT_COSP_SUM-KPLJZ =  GT_COSP_SUM-KPLJZ   + <LFS_COSP> .

     ENDIF.

     P_MONAT_01 = P_MONAT_01 + 1.

      ENDDO.


       ELSE.

         GT_COSP_SUM-KPLJZ =    GT_COSP_AUFK-WTG001 + GT_COSP_AUFK-WTG002 + GT_COSP_AUFK-WTG003 + GT_COSP_AUFK-WTG004 + GT_COSP_AUFK-WTG005 + GT_COSP_AUFK-WTG006 + GT_COSP_AUFK-WTG007 + GT_COSP_AUFK-WTG008
                  + GT_COSP_AUFK-WTG009 + GT_COSP_AUFK-WTG010 + GT_COSP_AUFK-WTG011 + GT_COSP_AUFK-WTG012 + GT_COSP_AUFK-WTG013 + GT_COSP_AUFK-WTG014 + GT_COSP_AUFK-WTG015 + GT_COSP_AUFK-WTG016.
    ENDIF.
   COLLECT GT_COSP_SUM.

ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SJCZ_DY_LJ_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SJCZ_DY_LJ_TOTAL .
  DATA:M_GPREIS LIKE CKIS-GPREIS. "单价
DATA:M_PSPID LIKE PROJ-PSPID.
DATA:M_CKL  TYPE P LENGTH 16 DECIMALS 3.
DATA:M_CZ  TYPE TSLVT12 .
DATA:M_LJCZ TYPE TSLVT12.
DATA:M_LJCKL TYPE P LENGTH 16 DECIMALS 3.."累计出库量
DATA:M_SJDM LIKE ZMM024-SJDM.
LOOP AT GT_MSEG.
   "项目号
  CLEAR :M_CKL ,M_LJCKL ,M_CZ ,M_LJCZ.
   AT NEW MAT_PSPNR .

     CLEAR : M_PSPID .
     READ TABLE GT_PRPS WITH KEY PSPNR = GT_MSEG-MAT_PSPNR .
       IF SY-SUBRC = 0.
           READ TABLE GT_PROJ WITH KEY PSPNR = GT_PRPS-PSPHI.
             IF SY-SUBRC = 0 .
               M_PSPID = GT_PROJ-PSPID.

              ENDIF.

        ENDIF.

    ENDAT.
   IF GT_MSEG-BUDAT_MKPF >= F_DATE AND GT_MSEG-BUDAT_MKPF <= E_DATE.
      "  IF GT_MSEG-SHKZG = 'H'.

         IF GT_MSEG-SHKZG = 'S'.

            M_CKL =   GT_MSEG-MENGE * -1.
           ELSE.
              M_CKL =  GT_MSEG-MENGE.
          ENDIF.



    ENDIF.

    IF GT_MSEG-BUDAT_MKPF <= E_DATE.
      "IF GT_MSEG-SHKZG = 'H'.


         IF  GT_MSEG-SHKZG = 'S'.

            M_LJCKL =  GT_MSEG-MENGE * -1.
          ELSE.
            M_LJCKL =  GT_MSEG-MENGE.

         ENDIF.

     " ELSE.
       "  M_LJCKL = M_LJCKL - GT_MSEG-MENGE.
    "  ENDIF.


    ENDIF.


  "计算物料单价
   " AT NEW MATNR .
       "物料综合单价
     CLEAR :M_GPREIS,M_SJDM.
     READ TABLE GT_ZMM024 WITH KEY MATNR = GT_MSEG-MATNR POSID = M_PSPID.
     IF SY-SUBRC = 0.
         M_SJDM = GT_ZMM024-SJDM.
     ENDIF.
     PERFORM WLZHDJ_JS  USING   M_PSPID GT_MSEG-MATNR CHANGING  M_GPREIS .
       IF   M_GPREIS = 0 .
          LOOP AT GT_ZMM024 WHERE SJDM = M_SJDM  AND POSID = M_PSPID AND  MATNR NE  GT_MSEG-MATNR.
                PERFORM WLZHDJ_JS  USING  M_PSPID GT_ZMM024-MATNR CHANGING  M_GPREIS  .
                   IF M_GPREIS  NE 0 .
                      EXIT.
                  ENDIF.
           ENDLOOP.

         ENDIF.
     "ENDAT.

    "产值累计
     M_CZ = M_CKL * M_GPREIS.
     M_LJCZ = M_LJCKL * M_GPREIS.

   " AT END OF MAT_PSPNR .
      GT_MSEG_SUM-PSPID =  M_PSPID .

      GT_MSEG_SUM-DYCZ  = M_CZ .
      GT_MSEG_SUM-LJCZ = M_LJCZ.

      COLLECT GT_MSEG_SUM.
   " ENDAT.
 ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0589   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
   AUTHORITY-CHECK OBJECT 'ZFI_BUK_CK' ID 'ACTVT' FIELD P_ACTVT
                                     ID 'BUKRS' FIELD P_BUKRS.
                                .
ENDFORM.


FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*  CASE R_UCOMM.
** 双击
*    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*      IF RS_SELFIELD-FIELDNAME = 'VBELN'
*        AND GS_DATA-VBELN IS NOT INITIAL.
*        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*      ENDIF.
*
*  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
