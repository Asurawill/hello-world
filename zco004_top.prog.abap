*&---------------------------------------------------------------------*
*&  包含                ZCO004_TOP
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************

TABLES: BKPF.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA_ABOVE,
        ZBOX    TYPE C,
        BUKRS   TYPE BKPF-BUKRS,
        GJAHR   TYPE BKPF-GJAHR,
        MONAT   TYPE BKPF-MONAT,
        BELNR   TYPE BKPF-BELNR,
        BUZEI   TYPE BSEG-BUZEI,
        MATNR   TYPE BSEG-MATNR,
        MAKTX   TYPE MAKT-MAKTX,
        DMBTR   TYPE BSEG-DMBTR,
        WERKS   TYPE BSEG-WERKS,
        STPRS   TYPE MBEW-STPRS, "S价
        VERPR   TYPE MBEW-VERPR, "V价
        BKLAS   TYPE MBEW-BKLAS, "评估类
        GJAHR_1 TYPE BSEG-GJAHR,
        BELNR_1 TYPE BSEG-BELNR,
      END OF TY_DATA_ABOVE.

TYPES:BEGIN OF TY_DATA_BELOW,
        WERKS     TYPE BSEG-WERKS,   "工厂
        MATNR     TYPE BSEG-MATNR,   "物料
        VBEL2     TYPE BSEG-VBEL2,   "销售订单号
        POSN2     TYPE BSEG-POSN2,   "销售订单行项目
        MENGE     TYPE BSEG-MENGE,   "数量
        MENGE_ALL TYPE BSEG-MENGE,   "物料总数量
        MEINS     TYPE BSEG-MEINS,   "单位
        CYFT      TYPE P DECIMALS 2, "差异分摊
        CYFT_ALL  TYPE P DECIMALS 2, "差异分摊
        POSN3     TYPE BSEG-POSN2,  "交货单行项目
        SFAD      TYPE CHAR1,       "是否按单
        ADCY      TYPE MLCD-ESTPRD, "按单差异
        AKCY      TYPE MLCD-ESTPRD, "按库差异
      END OF TY_DATA_BELOW.

************************************************************************
* Internal Table & WorkArea
************************************************************************
DATA GT_DATA_ABOVE   TYPE TABLE OF TY_DATA_ABOVE.
DATA GS_DATA_ABOVE   TYPE TY_DATA_ABOVE.

DATA GT_DATA_BELOW   TYPE TABLE OF TY_DATA_BELOW.
DATA GS_DATA_BELOW   TYPE TY_DATA_BELOW.
DATA GS_DATA_BELOW_D   TYPE TY_DATA_BELOW.

DATA GT_DATA_BELOW_1   TYPE TABLE OF TY_DATA_BELOW.
DATA GS_DATA_BELOW_1   TYPE TY_DATA_BELOW.

DATA GT_BKPF_ABOVE   TYPE TABLE OF BKPF.
DATA GS_BKPF_ABOVE   TYPE BKPF.

DATA GT_BSEG_ABOVE   TYPE TABLE OF BSEG.
DATA GS_BSEG_ABOVE   TYPE BSEG.

DATA GT_BSEG_ABOVE_1 TYPE TABLE OF BSEG.
DATA GS_BSEG_ABOVE_1 TYPE BSEG.

DATA GT_MAKT_ABOVE   TYPE TABLE OF MAKT.
DATA GS_MAKT_ABOVE   TYPE MAKT.

DATA GT_MBEW_ABOVE   TYPE TABLE OF MBEW.
DATA GS_MBEW_ABOVE   TYPE MBEW.

DATA GT_BKPF_BELOW   TYPE TABLE OF BKPF.
DATA GS_BKPF_BELOW   TYPE BKPF.

DATA GT_BSEG_BELOW   TYPE TABLE OF BSEG.
DATA GS_BSEG_BELOW   TYPE BSEG.

*取出按库的会计凭证
DATA GT_BSEG_BELOW_1 TYPE TABLE OF BSEG.
DATA GS_BSEG_BELOW_1 TYPE BSEG.

*取出按单的会计凭证
DATA GT_BSEG_BELOW_2 TYPE TABLE OF BSEG.
DATA GS_BSEG_BELOW_2 TYPE BSEG.

*取出交货号
DATA GT_BSEG_BELOW_3 TYPE TABLE OF BSEG.
DATA GS_BSEG_BELOW_3 TYPE BSEG.

DATA GT_BSEG_1 TYPE TABLE OF BSEG.
DATA GS_BSEG_1 TYPE BSEG.

DATA GT_ZCO004       TYPE  TABLE OF ZCO004.
DATA GS_ZCO004       TYPE ZCO004.

DATA GT_CKMLHD       TYPE TABLE OF CKMLHD.
DATA GS_CKMLHD       TYPE CKMLHD.

DATA GT_MLCD         TYPE TABLE OF MLCD.
DATA GS_MLCD         TYPE MLCD.

*全局变量
DATA: L_FRSTD        TYPE D,   "当月第一天
      L_LASTD        TYPE D,   "当月最后一天
      L_FIRST_DAY(8).
*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS:    SLIS.
DATA: WA_FIELDCAT            TYPE LVC_S_FCAT,
      G_ABOVE_DOCK_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER, "ALV容器(还有cl_gui_custom_container类)
      G_ABOVE_GRID           TYPE REF TO CL_GUI_ALV_GRID, "ALV容器的实例
      IT_ABOVE_FIELDCAT      TYPE LVC_T_FCAT,  "ALV 控制: 字段目录
      WA_ABOVE_VARIANT       TYPE DISVARIANT,  "格式 (外部使用)
      WA_ABOVE_LAYOUT        TYPE LVC_S_LAYO,  "ALV 控制: 布局结构
      IT_ABOVE_EXCLUDE       TYPE UI_FUNCTIONS, "按钮筛选内表(可以筛掉不想要的功能按钮)

      G_BELOW_DOCK_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER, "ALV容器(还有cl_gui_custom_container类)
      G_BELOW_GRID           TYPE REF TO CL_GUI_ALV_GRID, "ALV容器的实例
      IT_BELOW_FIELDCAT      TYPE LVC_T_FCAT,  "ALV 控制: 字段目录
      WA_BELOW_VARIANT       TYPE DISVARIANT,  "格式 (外部使用)
      WA_BELOW_LAYOUT        TYPE LVC_S_LAYO,  "ALV 控制: 布局结构
      IT_BELOW_EXCLUDE       TYPE UI_FUNCTIONS. "按钮筛选内表(可以筛掉不想要的功能按钮)


DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.

DATA: GW_ISTABLE TYPE LVC_S_STBL.

************************************************************************
* Global Variant
************************************************************************
DATA: OK_CODE TYPE SY-UCOMM,
      SAVE_OK TYPE SY-UCOMM.

DATA: G_EDIT_MOD TYPE C,
      G_CHANGED  TYPE C.

DATA: G_MSG(400).

DATA: G_LAND1 TYPE T001-LAND1,
      G_KALSM TYPE T005-KALSM.

DATA: G_STGRD TYPE BKPF-STGRD.

DATA: G_SUC.

************************************************************************
* Constant
************************************************************************
CONSTANTS: GC_EDITABLE TYPE C VALUE 'X',
           GC_READONLY TYPE C VALUE ''.

CONSTANTS: ABAP_TRUE  TYPE C VALUE 'X',
           ABAP_FALSE TYPE C VALUE ''.

************************************************************************
* BAPI
************************************************************************
DATA: WA_DOCUMENTHEADER    TYPE BAPIACHE09,
      IT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,
      WA_ACCOUNTGL         TYPE BAPIACGL09,
      IT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,
      WA_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      IT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,
      WA_CURRENCYAMOUNT    TYPE BAPIACCR09,
      IT_CRITERIA          TYPE TABLE OF BAPIACKEC9,
      WA_CRITERIA          TYPE BAPIACKEC9,
      IT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      WA_VALUEFIELD        TYPE BAPIACKEV9,
      IT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      WA_EXTENSION2        TYPE BAPIPAREX,
      IT_RETURN            TYPE TABLE OF BAPIRET2,
      WA_RETURN            TYPE BAPIRET2.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: WA_ZACCDOCUEXT TYPE ZACCDOCUEXT.

" BAPI_ACC_DOCUMENT_REV_POST
DATA: WA_REVERSAL TYPE BAPIACREV,
      WA_BUS      TYPE BAPIACHE09.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  WA_FIELDCAT-fieldname = &1.
  WA_FIELDCAT-coltext   = &2.
  WA_FIELDCAT-scrtext_l = &2.
  WA_FIELDCAT-scrtext_m = &2.
  WA_FIELDCAT-scrtext_s = &2.
  WA_FIELDCAT-reptext   = &2.
  WA_FIELDCAT-outputlen = &3.
  IF &4 = 'X'.
    WA_FIELDCAT-key = 'X'.
  ENDIF.
  WA_FIELDCAT-checkbox = &5.
  WA_FIELDCAT-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  WA_FIELDCAT-hotspot   = &7.
  WA_FIELDCAT-ref_field = &9.
  WA_FIELDCAT-ref_table = &8.

  APPEND WA_FIELDCAT TO IT_ABOVE_FIELDCAT.
  CLEAR WA_FIELDCAT.
END-OF-DEFINITION.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT1.      "  ALV Fieldcat Setting
  WA_FIELDCAT-fieldname = &1.
  WA_FIELDCAT-coltext   = &2.
  WA_FIELDCAT-scrtext_l = &2.
  WA_FIELDCAT-scrtext_m = &2.
  WA_FIELDCAT-scrtext_s = &2.
  WA_FIELDCAT-reptext   = &2.
  WA_FIELDCAT-outputlen = &3.
  IF &4 = 'X'.
    WA_FIELDCAT-key = 'X'.
  ENDIF.
  WA_FIELDCAT-checkbox = &5.
  WA_FIELDCAT-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  WA_FIELDCAT-hotspot   = &7.
  WA_FIELDCAT-ref_field = &9.
  WA_FIELDCAT-ref_table = &8.

  APPEND WA_FIELDCAT TO IT_BELOW_FIELDCAT.
  CLEAR WA_FIELDCAT.
END-OF-DEFINITION.
************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_BUKRS TYPE BKPF-BUKRS OBLIGATORY,
           P_GJAHR TYPE BKPF-GJAHR OBLIGATORY,
           P_MONAT TYPE BKPF-MONAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLK1.


************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.



************************************************************************
* Initialization
************************************************************************
INITIALIZATION.


************************************************************************
* Event Start of Selection
************************************************************************
START-OF-SELECTION.

*  PERFORM FRM_DATA_CLEAR.
*权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03' P_BUKRS .
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
*获取ABOVE ALV的取值
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA.

*获取BELOW ALV的取值
  PERFORM FRM_GET_DATA_1.
  PERFORM FRM_DEAL_DATA_1.

  CALL SCREEN 9001.
************************************************************************
* Event End-of selection
************************************************************************
END-OF-SELECTION.
