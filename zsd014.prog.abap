*&---------------------------------------------------------------------*
*& 程序名称:ZSD013
*& 作者    :张超
*& 开发日期:
*& 请求号  :
*& 描述    :PO与SO需求行处理
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

REPORT ZSD014.


************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
TABLES: VBAK, VBAP, EKKO, EKPO.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_PO,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE EKPO-MATNR,
        TXZ01 TYPE EKPO-TXZ01,
        MENGE TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
        EGLKZ TYPE EKPO-EGLKZ,
        BUKRS TYPE EKKO-BUKRS,
        FRGKE TYPE EKKO-FRGKE,
      END OF TY_PO.

TYPES:BEGIN OF TY_SO,
        VBELN  TYPE VBAP-VBELN,
        POSNR  TYPE VBAP-POSNR,
        MATNR  TYPE VBAP-MATNR,
        ARKTX  TYPE VBAP-ARKTX,
        KWMENG TYPE VBAP-KWMENG,
        VRKME  TYPE VBAP-VRKME,
        ABGRU  TYPE VBAP-ABGRU,
      END OF TY_SO.

TYPES:BEGIN OF TY_EBELN,
        EBELN TYPE EKPO-EBELN, "采购订单
        EBELP TYPE EKPO-EBELP, "采购订单行项目
        EGLKZ TYPE EKPO-EGLKZ, "最后交货
        VBELN TYPE EKKN-VBELN,
        VBELP TYPE EKKN-VBELP,
      END OF TY_EBELN.

TYPES:BEGIN OF TY_VBELN,
        VBELN TYPE VBAP-VBELN,
        POSNR TYPE VBAP-POSNR,
        ABGRU TYPE VBAP-ABGRU,
      END OF TY_VBELN.
************************************************************************
* Internal Table
************************************************************************
DATA: IT_PO     TYPE TABLE OF TY_PO,
      WA_PO     TYPE TY_PO,
      IT_POSHOW TYPE TABLE OF TY_PO,
      WA_POSHOW TYPE TY_PO.

DATA: IT_SO     TYPE TABLE OF TY_SO,
      WA_SO     TYPE TY_SO,
      IT_SOSHOW TYPE TABLE OF TY_SO,
      WA_SOSHOW TYPE TY_SO.

************************************************************************
* WorkArea
************************************************************************

************************************************************************
* BAPI Variables
************************************************************************
DATA: G_PURCHASEORDER TYPE BAPIMEPOHEADER-PO_NUMBER.

DATA: IT_POITEM  TYPE TABLE OF BAPIMEPOITEM,
      IT_POITEMX TYPE TABLE OF BAPIMEPOITEMX.

DATA: WA_POITEM  TYPE BAPIMEPOITEM,
      WA_POITEMX TYPE BAPIMEPOITEMX.

DATA: G_SALESDOCUMENT TYPE BAPIVBELN-VBELN.

DATA: IT_ORDER_ITEM_IN  TYPE TABLE OF BAPISDITM,
      IT_ORDER_ITEM_INX TYPE TABLE OF BAPISDITMX.

DATA: WA_ORDER_HEADER_IN  TYPE BAPISDH1,
      WA_ORDER_HEADER_INX TYPE BAPISDH1X,
      WA_ORDER_ITEM_IN    TYPE BAPISDITM,
      WA_ORDER_ITEM_INX   TYPE BAPISDITMX.

DATA: IT_RETURN TYPE TABLE OF BAPIRET2,
      WA_RETURN TYPE BAPIRET2.

DATA IT_EBELN   TYPE TABLE OF  TY_EBELN.
DATA WA_EBELN   TYPE TY_EBELN.
DATA IT_EBELN_1 TYPE TABLE OF  TY_EBELN.

DATA IT_VBELN   TYPE TABLE OF  TY_VBELN.
DATA WA_VBELN   TYPE TY_VBELN.
DATA IT_VBELN_1 TYPE TABLE OF  TY_VBELN.

DATA IT_EKKN    TYPE TABLE OF EKKN.
DATA WA_EKKN    TYPE EKKN.

DATA IT_VBAK    TYPE TABLE OF VBAK.
DATA WA_VBAK    TYPE VBAK.
************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext = &2.
*  gw_lvc-outputlen = &3.
*  if &4 = 'x'.
*    gw_lvc-no_zero = 'x'.
*  endif.
*  gw_lvc-icon = &4.
  gw_lvc-checkbox = &3.
  gw_lvc-edit = &4.
*  gw_lvc-fix_column =  &5.
  gw_lvc-cfieldname =  &5.
  gw_lvc-qfieldname =  &6.
  gw_lvc-ref_table = &7.
  gw_lvc-ref_field = &8.
*  gw_lvc-datatype = &8.
*  gw_lvc-intlen = &9.
  append gw_lvc to gt_lvc.
  clear gw_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,   "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,  "SY-REPID 指 当前的主程序
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

************************************************************************
* Global Variant
************************************************************************
*field-symbols: <dyn_table> type standard table,        " 内表结构
*               <dyn_wa>,                               " 表头
*               <dyn_field>.
*
*data: dy_table type ref to data,
*      dy_line  type ref to data.

*data: g_objname type thead-tdname.
*
*data: it_lines type table of tline,
*      wa_lines type tline.

************************************************************************
* Constant
************************************************************************


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_PO TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND UCOMM,
           P_SO TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS:
*               S_BSART FOR EKKO-BSART MODIF ID PO NO-EXTENSION NO INTERVALS,
                S_EKORG  FOR EKKO-EKORG MODIF ID PO NO-EXTENSION NO INTERVALS,
                S_EBELN  FOR EKPO-EBELN MODIF ID PO MATCHCODE OBJECT MEKK MEMORY ID BES,
                S_VBELN1 FOR VBAP-VBELN MODIF ID PO MATCHCODE OBJECT VMVA,
                S_BEDAT  FOR EKKO-BEDAT MODIF ID PO .



*           p_bedat type ekpo-bedat modif id po
SELECTION-SCREEN END OF BLOCK BLK2.

SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS:
*                S_AUART FOR  VBAK-AUART MODIF ID SO NO-EXTENSION NO INTERVALS,
                S_VKORG FOR  VBAK-VKORG MODIF ID SO NO-EXTENSION NO INTERVALS,
                S_VBELN FOR  VBAP-VBELN MODIF ID SO MATCHCODE OBJECT VMVA MEMORY ID AUN,
                S_VDATU FOR  VBAK-VDATU MODIF ID SO .

*           p_erdat type vbak-erdat modif id so

SELECTION-SCREEN END OF BLOCK BLK3.


************************************************************************
* Initialization
************************************************************************
INITIALIZATION.


************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'PO'.
      IF P_PO = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'SO'.
      IF P_SO = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN.



************************************************************************
* Event Start of Selection
************************************************************************
START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DISPLAY.


************************************************************************
* Event End-of selection
************************************************************************
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  IF P_PO = 'X'.

    SELECT P~EBELN
           P~EBELP
           MATNR
           TXZ01
           MENGE
           MEINS
           EGLKZ
           K~FRGKE
           K~BUKRS
      INTO CORRESPONDING FIELDS OF TABLE IT_PO
      FROM EKPO AS P
      JOIN EKKO AS K
        ON P~EBELN = K~EBELN
      WHERE K~EBELN IN S_EBELN
*&--代码注释 BY HANDYBY 15.06.2017 19:56:56  BEGIN
*    AND K~BSART = 'Z08'
*&--代码注释 BY HANDYBY 15.06.2017 19:56:56  END
*&--代码添加 BY HANDYBY 15.06.2017 19:57:01  BEGIN
      AND K~BSART IN ('Z08')
*&--代码添加 BY HANDYBY 15.06.2017 19:57:01  END
        AND P~LOEKZ <> 'L'
*        AND K~FRGKE = 'R'
        AND K~EKORG IN S_EKORG.

*&--代码注释 BY HANDYBY 15.06.2017 20:03:13  BEGIN
*    DELETE IT_PO WHERE BUKRS <> '1200' AND FRGKE <> 'R'
*&--代码注释 BY HANDYBY 15.06.2017 20:03:13  END
*&--代码添加 BY HANDYBY 15.06.2017 20:03:19  BEGIN
    DELETE IT_PO WHERE BUKRS = '1200' .
*&--代码添加 BY HANDYBY 15.06.2017 20:03:19  END

    .
    IF IT_PO IS NOT INITIAL.
      SELECT * FROM EKKN
        INTO CORRESPONDING FIELDS OF TABLE IT_EKKN
        FOR ALL ENTRIES IN IT_PO
        WHERE EBELN = IT_PO-EBELN
        AND   EBELP = IT_PO-EBELP
        AND   VBELN IN S_VBELN1.

      IF IT_EKKN IS NOT INITIAL .
        SELECT * FROM VBAK
         INTO CORRESPONDING FIELDS OF TABLE IT_VBAK
         FOR ALL ENTRIES IN IT_EKKN
         WHERE VBELN  = IT_EKKN-VBELN.
      ENDIF.

      SORT IT_PO BY EBELN EBELP.

*筛选 ZFI ZF2的销售定的那类型
*&--代码注释 BY HANDYBY 15.06.2017 20:09:38  BEGIN
*      LOOP AT IT_EKKN INTO WA_EKKN.
*        READ TABLE IT_VBAK INTO WA_VBAK
*        WITH KEY VBELN = WA_EKKN-VBELN.
*        IF SY-SUBRC = 0.
*          IF WA_VBAK-AUART <> 'ZF1' AND WA_VBAK <> 'ZF2'.
*            DELETE TABLE IT_EKKN FROM  WA_EKKN.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*&--代码注释 BY HANDYBY 15.06.2017 20:09:38  END


      IF S_VBELN1 IS NOT INITIAL.
        LOOP AT IT_PO INTO WA_PO.
          READ TABLE IT_EKKN INTO WA_EKKN
          WITH KEY EBELN = WA_PO-EBELN
                   EBELP = WA_PO-EBELP.
          IF SY-SUBRC <> 0.
            DELETE TABLE IT_PO FROM  WA_PO.
          ENDIF.
        ENDLOOP.
        IT_POSHOW = IT_PO.
      ELSE.
        IT_POSHOW = IT_PO.
      ENDIF.
    ELSE.
*&--代码注释 BY HANDYBY 15.06.2017 19:56:01  BEGIN
*    MESSAGE '只能选择订单类型Z08且审批通过且无删除标识的订单！' TYPE 'I' DISPLAY LIKE 'E'.
*&--代码注释 BY HANDYBY 15.06.2017 19:56:01  END
*&--代码添加 BY HANDYBY 15.06.2017 19:56:08  BEGIN
      MESSAGE '只能选择订单类型Z08无删除标识的订单！' TYPE 'I' DISPLAY LIKE 'E'.
*&--代码添加 BY HANDYBY 15.06.2017 19:56:08  END
      STOP.
    ENDIF.

  ELSEIF P_SO = 'X'.
    SELECT P~VBELN
           P~POSNR
           MATNR
           ARKTX
           KWMENG
           VRKME
           ABGRU
      INTO CORRESPONDING FIELDS OF TABLE  IT_SO
      FROM VBAP AS P
      JOIN VBAK AS K
        ON P~VBELN = K~VBELN
      WHERE K~VBELN IN S_VBELN
*&--代码注释 BY HANDYBY 15.06.2017 20:11:59  BEGIN
*      AND K~AUART IN ('ZF1', 'ZF2')
*&--代码注释 BY HANDYBY 15.06.2017 20:11:59  END
*&--代码添加 BY HANDYBY 15.06.2017 20:12:08  BEGIN
      AND K~AUART IN ('ZF1', 'ZF2' , 'ZPO')
*&--代码添加 BY HANDYBY 15.06.2017 20:12:08  END
        AND K~VKORG IN S_VKORG.

    IF IT_SO IS INITIAL.
*&--代码注释 BY HANDYBY 15.06.2017 20:12:26  BEGIN
*      MESSAGE '只能选择类型''ZF1''或''ZF2''的订单！' TYPE 'I' DISPLAY LIKE 'E'.
*&--代码注释 BY HANDYBY 15.06.2017 20:12:26  END
*&--代码添加 BY HANDYBY 15.06.2017 20:12:35  BEGIN
      MESSAGE '只能选择类型''ZF1''或''ZF2''或''ZPO''的订单！' TYPE 'I' DISPLAY LIKE 'E'.
*&--代码添加 BY HANDYBY 15.06.2017 20:12:35  END
      STOP.
    ENDIF.

    SORT IT_SO BY VBELN POSNR.

    IT_SOSHOW = IT_SO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DISPLAY .
  PERFORM INIT_LAYOUT.              "设置输出格式
  PERFORM INIT_SORT.                "设置排序、合计
  PERFORM INIT_VARIANT.             "设置变式控制
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  IF P_PO = 'X'.
    PERFORM FRM_INIT_LVC_PO.             " 初始化内表结构/ALV显示结构
    PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                              GT_SORT
                              IT_POSHOW
                       USING ''
                             'ALV_USER_COMMAND'
                             GW_LAYOUT
                             GW_VARIANT
                             GW_GRID_SETTINGS.
  ELSEIF P_SO = 'X'.
    PERFORM FRM_INIT_LVC_SO.             " 初始化内表结构/ALV显示结构
    PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                              GT_SORT
                              IT_SOSHOW
                       USING ''
                             'ALV_USER_COMMAND'
                             GW_LAYOUT
                             GW_VARIANT
                             GW_GRID_SETTINGS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       初始化layout参数
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA = 'X'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
  GW_LAYOUT-SEL_MODE = 'A'.
*  gw_layout-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       排序
*----------------------------------------------------------------------*
FORM INIT_SORT .
*    clear gw_sort.
*    gw_sort-fieldname = 'WERKS'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'FEVOR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'DISPO'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
*
**    clear gw_sort.
**    gw_sort-fieldname = 'KUNNR'.  "排序字段
**    gw_sort-spos = 1.
**    gw_sort-up = 'X'.             "升序
***  it_sort-subtot = 'X'.         "小计依据
**    append gw_sort to gt_sort.
*
*    clear gw_sort.
*    gw_sort-fieldname = 'MATNR'.  "排序字段
*    gw_sort-spos = 1.
*    gw_sort-up = 'X'.             "升序
**  it_sort-subtot = 'X'.         "小计依据
*    append gw_sort to gt_sort.
ENDFORM.                    " INIT_SORT
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       初始化变量
*----------------------------------------------------------------------*
FORM INIT_VARIANT.
  CLEAR: GW_VARIANT.
  GW_VARIANT-REPORT = SY-REPID.
  GW_VARIANT-HANDLE = '0001'.

  CLEAR GW_GRID_SETTINGS.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.

ENDFORM.                    " INIT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.                    " FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC.

*  init_fieldcat 'BOX' text-001 '2' '' 'X' 'X' 'X' '' ''.
*  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
*  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
  INIT_FIELDCAT 'VBELN' '' '' '' '' '' 'VBAP' 'VBELN'.


ENDFORM.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC_PO.

*  init_fieldcat 'BOX' text-001 '2' '' 'X' 'X' 'X' '' ''.
*  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
*  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
  INIT_FIELDCAT 'EBELN' '' '' '' '' '' 'EKPO' 'EBELN'.
  INIT_FIELDCAT 'EBELP' '' '' '' '' '' 'EKPO' 'EBELP'.
  INIT_FIELDCAT 'MATNR' '' '' '' '' '' 'EKPO' 'MATNR'.
  INIT_FIELDCAT 'TXZ01' '' '' '' '' '' 'EKPO' 'TXZ01'.
  INIT_FIELDCAT 'MENGE' '' '' '' '' 'MEINS' 'EKPO' 'MENGE'.
  INIT_FIELDCAT 'MEINS' '' '' '' '' '' 'EKPO' 'MEINS'.
  INIT_FIELDCAT 'EGLKZ' '' 'X' 'X' '' '' 'EKPO' 'ELIKZ'.


ENDFORM.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       列参数
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC_SO.

*  init_fieldcat 'BOX' text-001 '2' '' 'X' 'X' 'X' '' ''.
*  init_fieldcat 'STATUS' '状态' '' 'X' '' '' '' ''.
*  init_fieldcat 'MSG' '日志' '' '' '' '' '' '' .
  INIT_FIELDCAT 'VBELN' '' '' '' '' '' 'VBAP' 'VBELN'.
  INIT_FIELDCAT 'POSNR' '' '' '' '' '' 'VBAP' 'POSNR'.
  INIT_FIELDCAT 'MATNR' '' '' '' '' '' 'VBAP' 'MATNR'.
  INIT_FIELDCAT 'ARKTX' '' '' '' '' '' 'VBAP' 'ARKTX'.
  INIT_FIELDCAT 'KWMENG' '' '' '' '' 'VRKME' 'VBAP' 'KWMENG'.
  INIT_FIELDCAT 'VRKME' '' '' '' '' '' 'VBAP' 'VRKME'.
  INIT_FIELDCAT 'ABGRU' '' '' 'X' '' '' 'VBAP' 'ABGRU'.


ENDFORM.                    "frm_init_lvc
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
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
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
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
  DATA:L_INDEX TYPE SY-TABIX.

  CASE R_UCOMM.
    WHEN '&IC1'."双击
*      read table it_data into wa_data index rs_selfield-tabindex.
*      if sy-subrc = 0.
*        set parameter id 'XXXX' field wa_data-matnr.   "选择屏字段ID
*        call transaction 'XXXX' and skip first screen."填T-code
*      endif.
*    when 'PRINT'."打印
*      perform frm_print_select.
*    when '&ZALL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = 'X'.
*        modify it_data from wa_data index l_index.
*      endloop.
*    when '&ZSAL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = ''.
*        modify it_data from wa_data index l_index.
*      endloop.
    WHEN '&DATA_SAVE'.
      IF P_PO = 'X'.
        PERFORM FRM_PO_CHANGE.
      ELSEIF P_SO = 'X'.
        PERFORM FRM_SO_CHANGE.
      ENDIF.
*      message 'Save data.' type 'I' display like 'S'.

    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       ALV事件
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = GT_EVENTS[].

*  read table gt_events into gw_events with key name = slis_ev_top_of_page.
*  if sy-subrc = 0.
*    gw_events-form = 'ALV_TOP_OF_PAGE'.
*    modify gt_events from gw_events index sy-tabix.
*  endif.
ENDFORM.                    " FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  DATA: L_EKORG TYPE EKKO-EKORG,
        L_VKORG TYPE VBAK-VKORG.

*  IF P_PO = 'X'.
*    SELECT SINGLE EKORG
*      INTO L_EKORG
*      FROM EKKO
*      WHERE EBELN = S_EBELN.
*    IF SY-SUBRC = 0.
*      AUTHORITY-CHECK OBJECT 'M_BEST_EKO' ID 'ACTVT' FIELD '02'
*                                          ID 'EKORG' FIELD L_EKORG.
*      IF SY-SUBRC NE 0.
*        MESSAGE I018(ZMM01) WITH L_EKORG DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSE.
*      MESSAGE I017(ZMM01) WITH S_EBELN DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*  ELSEIF P_SO = 'X'.
*    SELECT SINGLE VKORG
*      INTO L_VKORG
*      FROM VBAK
*      WHERE VBELN = S_VBELN.
*    IF SY-SUBRC = 0.
*      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO' ID 'ACTVT' FIELD '02'
*                                          ID 'SPART' DUMMY
*                                          ID 'VKORG' FIELD L_VKORG
*                                          ID 'VTWEG' DUMMY.
*      IF SY-SUBRC NE 0.
*        MESSAGE I001(ZSD01) WITH L_VKORG DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSE.
*      MESSAGE I002(ZSD01) WITH S_VBELN DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PO_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PO_CHANGE .

  PERFORM FRM_PO_BAPI_CLEAR.

  PERFORM FRM_PO_BAPI_PRE.

  IF IT_EBELN IS INITIAL.
    MESSAGE '没有变更的数据！' TYPE 'I' DISPLAY LIKE 'W'.
  ELSE.
    LOOP AT IT_EBELN INTO WA_EBELN.
      PERFORM FRM_PO_BAPI_CALL CHANGING WA_EBELN.
    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SO_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SO_CHANGE .
  PERFORM FRM_SO_BAPI_CLEAR.

  PERFORM FRM_SO_BAPI_PRE.

  IF IT_VBELN IS INITIAL.
    MESSAGE '没有变更的数据！' TYPE 'I' DISPLAY LIKE 'W'.
  ELSE.
    LOOP AT IT_VBELN INTO WA_VBELN.
      PERFORM FRM_SO_BAPI_CALL CHANGING WA_VBELN.
    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PO_BAPI_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PO_BAPI_PRE .
  REFRESH IT_EBELN.
  LOOP AT IT_PO INTO WA_PO.
    READ TABLE IT_POSHOW INTO WA_POSHOW WITH KEY EBELN = WA_PO-EBELN
                                                 EBELP = WA_PO-EBELP
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF WA_PO-EGLKZ <> WA_POSHOW-EGLKZ.
        CLEAR WA_EBELN.

        WA_EBELN-EBELN = WA_POSHOW-EBELN.
        WA_EBELN-EBELP = WA_POSHOW-EBELP.
        WA_EBELN-EGLKZ = WA_POSHOW-EGLKZ.

        READ TABLE IT_EKKN INTO WA_EKKN
        WITH KEY EBELN = WA_EBELN-EBELN
                 EBELP = WA_EBELN-EBELP.
        IF SY-SUBRC = 0.
          WA_EBELN-VBELN = WA_EKKN-VBELN.
          WA_EBELN-VBELP = WA_EKKN-VBELP.
        ENDIF.

        APPEND WA_EBELN TO IT_EBELN.
        CLEAR WA_EBELN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IT_EBELN_1 = IT_EBELN.

  DELETE ADJACENT DUPLICATES FROM IT_EBELN COMPARING EBELN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PO_BAPI_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PO_BAPI_CLEAR .
  REFRESH: IT_POITEM, IT_POITEMX, IT_RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PO_BAPI_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PO_BAPI_CALL CHANGING LW_EBELN TYPE TY_EBELN.

  DATA LT_POITEM TYPE TABLE OF  BAPIMEPOITEM.
  DATA LS_POITEM TYPE BAPIMEPOITEM.

  DATA LT_POITEMX TYPE TABLE OF  BAPIMEPOITEMX.
  DATA LS_POITEMX TYPE BAPIMEPOITEMX.

  REFRESH:LT_POITEM,
          LT_POITEMX.

  LOOP AT IT_EBELN_1 INTO WA_EBELN
  WHERE EBELN = LW_EBELN-EBELN.

    CLEAR: LS_POITEM, LS_POITEMX.

    WA_POITEM-PO_ITEM    = WA_EBELN-EBELP.
    WA_POITEM-DELIV_COMPL = WA_EBELN-EGLKZ.
    APPEND WA_POITEM TO LT_POITEM.

    WA_POITEMX-PO_ITEM = WA_EBELN-EBELP.
    WA_POITEMX-DELIV_COMPL = 'X'.
    APPEND WA_POITEMX TO LT_POITEMX.

  ENDLOOP.


  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      PURCHASEORDER = LW_EBELN-EBELN
*     POHEADER      =
*     POHEADERX     =
*     POADDRVENDOR  =
*     TESTRUN       =
*     MEMORY_UNCOMPLETE            =
*     MEMORY_COMPLETE              =
*     POEXPIMPHEADER               =
*     POEXPIMPHEADERX              =
*     VERSIONS      =
*     NO_MESSAGING  =
*     NO_MESSAGE_REQ               =
*     NO_AUTHORITY  =
*     NO_PRICE_FROM_PO             =
*     PARK_UNCOMPLETE              =
*     PARK_COMPLETE =
*   IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      RETURN        = IT_RETURN
      POITEM        = LT_POITEM
      POITEMX       = LT_POITEMX
*     POADDRDELIVERY               =
*     POSCHEDULE    =
*     POSCHEDULEX   =
*     POACCOUNT     =
*     POACCOUNTPROFITSEGMENT       =
*     POACCOUNTX    =
*     POCONDHEADER  =
*     POCONDHEADERX =
*     POCOND        =
*     POCONDX       =
*     POLIMITS      =
*     POCONTRACTLIMITS             =
*     POSERVICES    =
*     POSRVACCESSVALUES            =
*     POSERVICESTEXT               =
*     EXTENSIONIN   =
*     EXTENSIONOUT  =
*     POEXPIMPITEM  =
*     POEXPIMPITEMX =
*     POTEXTHEADER  =
*     POTEXTITEM    =
*     ALLVERSIONS   =
*     POPARTNER     =
*     POCOMPONENTS  =
*     POCOMPONENTSX =
*     POSHIPPING    =
*     POSHIPPINGX   =
*     POSHIPPINGEXP =
*     POHISTORY     =
*     POHISTORY_TOTALS             =
*     POCONFIRMATION               =
*     SERIALNUMBER  =
*     SERIALNUMBERX =
*     INVPLANHEADER =
*     INVPLANHEADERX               =
*     INVPLANITEM   =
*     INVPLANITEMX  =
*     POHISTORY_MA  =
*     NFMETALLITMS  =
    .
  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    IT_PO = IT_POSHOW.

    LOOP AT IT_EBELN_1 INTO  WA_EBELN
    WHERE EBELN = LW_EBELN-EBELN
    AND   VBELN <> ''
    AND   VBELP <> ''.

      PERFORM FRM_SO_BAPI_CALL_1 CHANGING WA_EBELN.
    ENDLOOP.

    MESSAGE '修改成功！' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_DISPLAY .
  PERFORM FRM_MESSAGE_INITIAL.

  LOOP AT IT_RETURN INTO WA_RETURN.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.
*
*    write: / g_msg.

    "append all error warning messages to below function module
    PERFORM STORE_MESSAGES USING WA_RETURN-ID
                                 WA_RETURN-TYPE
                                 WA_RETURN-MESSAGE_V1
                                 WA_RETURN-MESSAGE_V2
                                 WA_RETURN-MESSAGE_V3
                                 WA_RETURN-MESSAGE_V4
                                 WA_RETURN-NUMBER.
  ENDLOOP.

  PERFORM FRM_MESSAGE_SHOW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_INITIAL .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      LOG_NOT_ACTIVE       = 1
      WRONG_IDENTIFICATION = 2
      OTHERS               = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_SHOW .
  "at lsat call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA L_EXIT_COMMAND TYPE BAL_S_EXCM.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO         = SPACE
    IMPORTING
      E_EXIT_COMMAND     = L_EXIT_COMMAND
    EXCEPTIONS
      INCONSISTENT_RANGE = 1
      NO_MESSAGES        = 2
      OTHERS             = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETURN_ID  text
*      -->P_WA_RETURN_TYPE  text
*      -->P_WA_RETURN_MESSAGE_V1  text
*      -->P_WA_RETURN_MESSAGE_V2  text
*      -->P_WA_RETURN_MESSAGE_V3  text
*      -->P_WA_RETURN_MESSAGE_V4  text
*      -->P_WA_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM STORE_MESSAGES USING P_MSGID
                          P_MSGTY
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                          P_TXTNR.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      ARBGB                  = P_MSGID
      MSGTY                  = P_MSGTY
      MSGV1                  = P_MSGV1
      MSGV2                  = P_MSGV2
      MSGV3                  = P_MSGV3
      MSGV4                  = P_MSGV4
      TXTNR                  = P_TXTNR
    EXCEPTIONS
      MESSAGE_TYPE_NOT_VALID = 1
      NOT_ACTIVE             = 2
      OTHERS                 = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SO_BAPI_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SO_BAPI_CLEAR .
  REFRESH: IT_RETURN, IT_ORDER_ITEM_IN, IT_ORDER_ITEM_INX.
  CLEAR WA_ORDER_HEADER_INX.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SO_BAPI_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SO_BAPI_PRE .
  G_SALESDOCUMENT = S_VBELN.
  WA_ORDER_HEADER_INX-UPDATEFLAG = 'U'.   " 设置抬头更新标识

  LOOP AT IT_SO INTO WA_SO.
    READ TABLE IT_SOSHOW INTO WA_SOSHOW WITH KEY VBELN = WA_SO-VBELN
                                                 POSNR = WA_SO-POSNR
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF WA_SO-ABGRU <> WA_SOSHOW-ABGRU.
*        CLEAR: WA_ORDER_ITEM_IN, WA_ORDER_ITEM_INX.
*
*        WA_ORDER_ITEM_IN-ITM_NUMBER = WA_SOSHOW-POSNR.
*        WA_ORDER_ITEM_IN-REASON_REJ = WA_SOSHOW-ABGRU.
*        APPEND WA_ORDER_ITEM_IN TO IT_ORDER_ITEM_IN.
*
*        WA_ORDER_ITEM_INX-ITM_NUMBER = WA_SOSHOW-POSNR.
*        WA_ORDER_ITEM_INX-UPDATEFLAG = 'U'.
*        WA_ORDER_ITEM_INX-REASON_REJ = 'X'.
*        APPEND WA_ORDER_ITEM_INX TO IT_ORDER_ITEM_INX.
        CLEAR WA_VBELN.

        WA_VBELN-VBELN = WA_SOSHOW-VBELN.
        WA_VBELN-POSNR = WA_SOSHOW-POSNR.
        WA_VBELN-ABGRU = WA_SOSHOW-ABGRU.
        APPEND WA_VBELN TO IT_VBELN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IT_VBELN_1 = IT_VBELN.

  DELETE ADJACENT DUPLICATES FROM IT_VBELN COMPARING VBELN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SO_BAPI_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SO_BAPI_CALL CHANGING LW_VBELN TYPE TY_VBELN.

  DATA LT_ORDER_ITEM_IN TYPE TABLE OF  BAPISDITM.
  DATA LS_ORDER_ITEM_IN TYPE BAPISDITM.

  DATA LT_ORDER_ITEM_INX TYPE TABLE OF  BAPISDITMX.
  DATA LS_ORDER_ITEM_INX TYPE BAPISDITMX.

  REFRESH:LT_ORDER_ITEM_IN,
          LT_ORDER_ITEM_INX.

  WA_ORDER_HEADER_INX-UPDATEFLAG = 'U'.   " 设置抬头更新标识

  LOOP AT IT_VBELN_1 INTO WA_VBELN
  WHERE VBELN = LW_VBELN-VBELN.

    CLEAR: LS_ORDER_ITEM_IN, LS_ORDER_ITEM_INX.

    LS_ORDER_ITEM_IN-ITM_NUMBER = WA_VBELN-POSNR.
    LS_ORDER_ITEM_IN-REASON_REJ = WA_VBELN-ABGRU.
    APPEND LS_ORDER_ITEM_IN TO LT_ORDER_ITEM_IN.

    LS_ORDER_ITEM_INX-ITM_NUMBER = WA_VBELN-POSNR.
    LS_ORDER_ITEM_INX-UPDATEFLAG = 'U'.
    LS_ORDER_ITEM_INX-REASON_REJ = 'X'.
    APPEND LS_ORDER_ITEM_INX TO LT_ORDER_ITEM_INX.

  ENDLOOP.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      SALESDOCUMENT    = WA_VBELN-VBELN
*     ORDER_HEADER_IN  =
      ORDER_HEADER_INX = WA_ORDER_HEADER_INX
*     SIMULATION       =
*     BEHAVE_WHEN_ERROR           = ' '
*     INT_NUMBER_ASSIGNMENT       = ' '
*     LOGIC_SWITCH     =
*     NO_STATUS_BUF_INIT          = ' '
    TABLES
      RETURN           = IT_RETURN
      ORDER_ITEM_IN    = LT_ORDER_ITEM_IN
      ORDER_ITEM_INX   = LT_ORDER_ITEM_INX
*     PARTNERS         =
*     PARTNERCHANGES   =
*     PARTNERADDRESSES =
*     ORDER_CFGS_REF   =
*     ORDER_CFGS_INST  =
*     ORDER_CFGS_PART_OF          =
*     ORDER_CFGS_VALUE =
*     ORDER_CFGS_BLOB  =
*     ORDER_CFGS_VK    =
*     ORDER_CFGS_REFINST          =
*     SCHEDULE_LINES   =
*     SCHEDULE_LINESX  =
*     ORDER_TEXT       =
*     ORDER_KEYS       =
*     CONDITIONS_IN    =
*     CONDITIONS_INX   =
*     EXTENSIONIN      =
*     EXTENSIONEX      =
*     NFMETALLITMS     =
    .
  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    IT_SO = IT_SOSHOW.

    MESSAGE '修改成功！' TYPE 'S'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_SO_BAPI_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SO_BAPI_CALL_1 CHANGING LW_EBELN TYPE TY_EBELN.

  DATA LT_ORDER_ITEM_IN TYPE TABLE OF  BAPISDITM.
  DATA LS_ORDER_ITEM_IN TYPE BAPISDITM.

  DATA LT_ORDER_ITEM_INX TYPE TABLE OF  BAPISDITMX.
  DATA LS_ORDER_ITEM_INX TYPE BAPISDITMX.

  REFRESH:LT_ORDER_ITEM_IN,
          LT_ORDER_ITEM_INX.

  WA_ORDER_HEADER_INX-UPDATEFLAG = 'U'.   " 设置抬头更新标识

  CLEAR: LS_ORDER_ITEM_IN, LS_ORDER_ITEM_INX.

  LS_ORDER_ITEM_IN-ITM_NUMBER = LW_EBELN-VBELP.
  IF LW_EBELN-EGLKZ = 'X'.
    LS_ORDER_ITEM_IN-REASON_REJ = '50'.
  ELSE.
    LS_ORDER_ITEM_IN-REASON_REJ = ''.
  ENDIF.

  APPEND LS_ORDER_ITEM_IN TO LT_ORDER_ITEM_IN.

  LS_ORDER_ITEM_INX-ITM_NUMBER = LW_EBELN-VBELP.
  LS_ORDER_ITEM_INX-UPDATEFLAG = 'U'.
  LS_ORDER_ITEM_INX-REASON_REJ = 'X'.
  APPEND LS_ORDER_ITEM_INX TO LT_ORDER_ITEM_INX.


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      SALESDOCUMENT    = LW_EBELN-VBELN
*     ORDER_HEADER_IN  =
      ORDER_HEADER_INX = WA_ORDER_HEADER_INX
*     SIMULATION       =
*     BEHAVE_WHEN_ERROR           = ' '
*     INT_NUMBER_ASSIGNMENT       = ' '
*     LOGIC_SWITCH     =
*     NO_STATUS_BUF_INIT          = ' '
    TABLES
      RETURN           = IT_RETURN
      ORDER_ITEM_IN    = LT_ORDER_ITEM_IN
      ORDER_ITEM_INX   = LT_ORDER_ITEM_INX
*     PARTNERS         =
*     PARTNERCHANGES   =
*     PARTNERADDRESSES =
*     ORDER_CFGS_REF   =
*     ORDER_CFGS_INST  =
*     ORDER_CFGS_PART_OF          =
*     ORDER_CFGS_VALUE =
*     ORDER_CFGS_BLOB  =
*     ORDER_CFGS_VK    =
*     ORDER_CFGS_REFINST          =
*     SCHEDULE_LINES   =
*     SCHEDULE_LINESX  =
*     ORDER_TEXT       =
*     ORDER_KEYS       =
*     CONDITIONS_IN    =
*     CONDITIONS_INX   =
*     EXTENSIONIN      =
*     EXTENSIONEX      =
*     NFMETALLITMS     =
    .
  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    MESSAGE '修改成功！' TYPE 'S'.
  ENDIF.

ENDFORM.
