*&---------------------------------------------------------------------*
*&  包含                ZFI043_TOP
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES: T001,PROJ,BKPF .

************************************************************************
* Type Declaration
************************************************************************
* 项目开票
TYPES:BEGIN OF TY_HEAD,
        BLDAT     TYPE BKPF-BLDAT,
        BLART     TYPE BKPF-BLART,
        ZYWLX(10) TYPE C,
        BUKRS     TYPE BKPF-BUKRS,
        BUDAT     TYPE BKPF-BUDAT,
        WAERS     TYPE BKPF-WAERS,

        PSPID     TYPE PROJ-PSPID,
        POST1     TYPE PROJ-POST1,

        BKTXT     TYPE BKPF-BKTXT,
        BELNR     TYPE BKPF-BELNR,
        GJAHR     TYPE BKPF-GJAHR,
        OPREIS    TYPE CKIS-OPREIS,
*        PJNAM(200),
      END OF TY_HEAD.

TYPES:BEGIN OF TY_ITEM,
        BUZEI     TYPE BSEG-BUZEI,
        BSCHL     TYPE BSEG-BSCHL,
        HKONT     TYPE BSEG-HKONT,
        HKTXT     TYPE SKAT-TXT20,
        KUNNR     TYPE BSEG-KUNNR,
        KNAME     TYPE KNA1-NAME1,

        MATNR     TYPE BSEG-MATNR,
        MAKTX     TYPE MAKT-MAKTX,
        MENGE     TYPE BSEG-MENGE,
        MENGE2    TYPE BSEG-MENGE,
        MENGE3    TYPE BSEG-MENGE,
        MEINS     TYPE MSEG-MEINS,
        OPREIS    TYPE CKIS-OPREIS,
        OPREIS2   TYPE CKIS-OPREIS,

        WAERS     TYPE BKPF-WAERS,
        ZFBDT     TYPE BSEG-ZFBDT,
        POSID     TYPE PRPS-POSID,
        POST1     TYPE PRPS-POST1,

        SGTXT     TYPE BSEG-SGTXT,
        CELLSTYLE TYPE LVC_T_STYL,
      END OF TY_ITEM.

* 开票记录查询
TYPES: BEGIN OF TY_KPJL .
        INCLUDE TYPE ZFI043 .
TYPES: POST1 TYPE PROJ-POST1,
       DMBTR TYPE BSEG-DMBTR,
       BKTXT TYPE BKPF-BKTXT,
       STBLG TYPE BKPF-STBLG,
       STJAH TYPE BKPF-STJAH,
       SEL   TYPE C,
       END OF TY_KPJL .
************************************************************************
* Internal Table & WorkArea
************************************************************************
DATA: GS_HEAD TYPE TY_HEAD.

DATA: GT_ITEM TYPE TABLE OF TY_ITEM,
      GS_ITEM TYPE TY_ITEM.

DATA: G_VRMNAME   TYPE VRM_ID,
      GT_VRMLIST  TYPE VRM_VALUES,
      GS_VRMVALUE LIKE LINE OF GT_VRMLIST.

FIELD-SYMBOLS: <FS_ITEM> TYPE TY_ITEM.

DATA GS_PROJ TYPE PROJ .

DATA: GT_ZFI043 TYPE TABLE OF ZFI043,
      GS_ZFI043 TYPE ZFI043.

DATA: GT_KPJL TYPE TABLE OF TY_KPJL,
      GS_KPJL TYPE TY_KPJL.

DATA GS_BKPF TYPE BKPF .

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  clear gS_lvc.
  gS_lvc-fieldname = &1.
  gS_lvc-coltext = &2.
  gS_lvc-scrtext_l = &2.
  gS_lvc-scrtext_m = &2.
  gS_lvc-scrtext_s = &2.
  gS_lvc-reptext = &2.
  gS_lvc-edit = &3.
  gS_lvc-cfieldname = &4.
  gS_lvc-qfieldname =  &5.
  gS_lvc-ref_table = &6.
  gS_lvc-ref_field = &7.
  gS_lvc-outputlen = &8.
*  gw_lvc-intlen = &9.
  IF &1 = 'HKONT'.
    gS_lvc-f4availabl = 'X'.
  ENDIF.
*  IF &1 = 'SFNCX'.
*    gS_lvc-checkbox = 'X'.
*  ENDIF.
  append gS_lvc to gt_lvc.
END-OF-DEFINITION.
DEFINE INIT_FIELDCAT2.      "  ALV Fieldcat Setting
  clear gS_lvc.
  gS_lvc-fieldname = &1.
  gS_lvc-coltext = &2.
  gS_lvc-scrtext_l = &2.
  gS_lvc-scrtext_m = &2.
  gS_lvc-scrtext_s = &2.
  gS_lvc-reptext = &2.
  gS_lvc-edit = &3.
  gS_lvc-cfieldname = &4.
  gS_lvc-qfieldname =  &5.
  gS_lvc-ref_table = &6.
  gS_lvc-ref_field = &7.
  gS_lvc-checkbox = &8.
*  gw_lvc-intlen = &9.
  append gS_lvc to gt_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GS_LVC           TYPE LVC_S_FCAT,
      GS_SORT          TYPE LVC_S_SORT,
      GS_LAYOUT        TYPE LVC_S_LAYO,   "alv的格式
      GS_VARIANT       TYPE DISVARIANT,
      GS_GRID_SETTINGS TYPE LVC_S_GLAY,
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GS_EVENTS        LIKE LINE OF GT_EVENTS.

DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GT_OO_EXCLUDE TYPE UI_FUNCTIONS.

DATA: GR_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GR_ALVGRID   TYPE REF TO CL_GUI_ALV_GRID.

************************************************************************
* Global Variant
************************************************************************
DATA: OK_CODE TYPE SY-UCOMM,
      SAVE_OK TYPE SY-UCOMM.

DATA: G_EDIT_MOD TYPE C,
      G_CHANGED  TYPE C.

DATA: G_LAND1 TYPE T001-LAND1,
      G_KALSM TYPE T005-KALSM.

DATA: G_SUC.

DATA: G_STGRD TYPE BKPF-STGRD.

************************************************************************
* Constant
************************************************************************
CONSTANTS: GC_EDITABLE TYPE C VALUE 'X',
           GC_READONLY TYPE C VALUE ''.

************************************************************************
* BAPI
************************************************************************
DATA: GS_DOCUMENTHEADER    TYPE BAPIACHE09,
      GT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,
      GS_ACCOUNTGL         TYPE BAPIACGL09,
      GT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,
      GS_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      GT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,
      GS_CURRENCYAMOUNT    TYPE BAPIACCR09,
      GT_CRITERIA          TYPE TABLE OF BAPIACKEC9,
      GS_CRITERIA          TYPE BAPIACKEC9,
      GT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      GS_VALUEFIELD        TYPE BAPIACKEV9,
      GT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      GS_EXTENSION2        TYPE BAPIPAREX,
      GT_RETURN            TYPE TABLE OF BAPIRET2,
      GS_RETURN            TYPE BAPIRET2.

DATA: GS_ZACCDOCUEXT TYPE ZACCDOCUEXT.
DATA: GS_OBJ TYPE BAPIACHE09.

" BAPI_ACC_DOCUMENT_REV_POST
DATA: GS_REVERSAL TYPE BAPIACREV,
      GS_BUS      TYPE BAPIACHE09.
************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_XMKP TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND UCOMM,
           P_KPJL TYPE C RADIOBUTTON GROUP G1 .
SELECTION-SCREEN END OF BLOCK B1 .

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS P_BUKRS TYPE T001-BUKRS MODIF ID KP .
SELECT-OPTIONS S_PSPID FOR PROJ-PSPID MODIF ID KP .
SELECT-OPTIONS S_BLDAT FOR BKPF-BLDAT MODIF ID KP .
SELECTION-SCREEN END OF BLOCK B2 .

************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'KP'.
      IF P_XMKP = 'X' .
        SCREEN-ACTIVE = 0.
      ELSE.
        SCREEN-ACTIVE = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.

************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* Event Start of Selection
************************************************************************
START-OF-SELECTION.
  IF P_XMKP = 'X'.      " 项目开票
    PERFORM FRM_DATA_CLEAR.
    PERFORM FRM_DATA_INIT.
    CALL SCREEN 9001.
  ELSEIF P_KPJL = 'X'.  " 项目开票记录
    PERFORM FRM_BUKRS_EXIST USING P_BUKRS .
    PERFORM FRM_AUTH_CHECK USING '03' P_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    PERFORM FRM_KPJL_REPORT.
  ENDIF.
