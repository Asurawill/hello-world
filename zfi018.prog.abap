REPORT ZFI018.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/16
*& Request       :
*& Descriptions  : 项目采购质保金
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
TABLES:EKKO,PROJ.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        EKGRP TYPE   ZFI017_1-EKGRP,   "采购组
        EKNAM TYPE   T024-EKNAM,       "采购组描述
        EBELN TYPE   ZFI017_1-EBELN,   "采购订单号
        AEDAT TYPE   ZFI017_1-AEDAT,   "订单创建日期
        POSID TYPE   ZFI017_1-POSID,   "项目WBS
        POST1 TYPE   ZFI017_1-POST1,   "项目描述
        NETWR TYPE   ZFI017_1-NETWR,   "采购订单含税金额
        RATPZ TYPE   ZFI017_1-RATPZ,   "应付比例
        YFJE1 TYPE   ZFI017_1-YFJE1,   "应付金额
        ZTERM TYPE   ZFI017_1-ZTERM,   "子付款条件
        TEXT1 TYPE   ZFI017_1-TEXT1,   "子付款条件描述
        WRBTR TYPE   ZFI017_1-WRBTR,   "已开票金额
        ZBDQR TYPE   ZFI017_1-ZBDQR,   "质保到期日
        ZCH   TYPE   ZFI017_1-ZCH,     "拆行
      END OF TY_DATA.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_EKKO TYPE TABLE OF EKKO.
DATA GS_EKKO TYPE EKKO.

DATA GT_EKPO TYPE TABLE OF EKPO.
DATA GS_EKPO TYPE EKPO.

DATA GT_EKKN TYPE TABLE OF EKKN.
DATA GS_EKKN TYPE EKKN.

DATA GT_KONV TYPE TABLE OF KONV.
DATA GS_KONV TYPE KONV.

DATA GT_RSEG TYPE TABLE OF RSEG.
DATA GS_RSEG TYPE RSEG.

DATA GS_AUFK  TYPE AUFK.

DATA GT_T052  TYPE TABLE OF T052.
DATA GS_T052  TYPE T052.

DATA GT_T052S TYPE TABLE OF T052S.
DATA GS_T052S TYPE T052S.

DATA GT_T052U TYPE TABLE OF T052U.
DATA GS_T052U TYPE T052U.

DATA GT_ZFI017 TYPE TABLE OF ZFI017_1.
DATA GS_ZFI017 TYPE ZFI017_1.

DATA GT_ZFI017_1 TYPE TABLE OF ZFI017_1.
DATA GS_ZFI017_1 TYPE ZFI017_1.

DATA GT_T024   TYPE TABLE OF T024.
DATA GS_T024   TYPE T024.
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

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECT-OPTIONS:S_BUKRS FOR EKKO-BUKRS,
               S_EBELN FOR EKKO-EBELN,
               S_PSPID FOR PROJ-PSPID,
               S_EKGRP FOR EKKO-EKGRP.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.
*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**权限检查检查公司代码
*  " PERFORM FRM_AUTH_CHECK USING '03'.
*  PERFORM FRM_AUTH_CHECK.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA.
  PERFORM FRM_ALV_SHOW. "ALV显示

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

  SELECT * FROM EKKO
  INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
  WHERE BUKRS IN S_BUKRS
  AND   EBELN IN S_EBELN
  AND   EKGRP IN S_EKGRP.

  CHECK GT_EKKO IS NOT INITIAL.

*采购组描述
  SELECT * FROM T024
  INTO CORRESPONDING FIELDS OF TABLE GT_T024
  FOR ALL ENTRIES IN GT_EKKO
  WHERE EKGRP = GT_EKKO-EKGRP.

*查询采购订单PB00或者PBXX的金额
  SELECT * FROM KONV
    INTO CORRESPONDING FIELDS OF TABLE GT_KONV
    FOR ALL ENTRIES IN GT_EKKO
    WHERE KNUMV = GT_EKKO-KNUMV.

  SELECT * FROM EKPO
  INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
  FOR ALL ENTRIES IN GT_EKKO
  WHERE EBELN = GT_EKKO-EBELN
  AND  LOEKZ <> 'L'.

  CHECK GT_EKPO IS NOT INITIAL.

  SELECT * FROM EKKN
  INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
  FOR ALL ENTRIES IN GT_EKPO
  WHERE EBELN = GT_EKPO-EBELN
  AND   EBELP = GT_EKPO-EBELP.

*查询发票金额
  SELECT * FROM RSEG
  INTO CORRESPONDING FIELDS OF TABLE GT_RSEG
  FOR ALL ENTRIES IN GT_EKKO
  WHERE EBELN = GT_EKKO-EBELN.

*查询子付款条件
  SELECT * FROM T052
    INTO CORRESPONDING FIELDS OF TABLE GT_T052
    FOR ALL ENTRIES IN GT_EKKO
    WHERE ZTERM = GT_EKKO-ZTERM.

  IF GT_T052 IS NOT INITIAL.
    SELECT * FROM T052S
      INTO CORRESPONDING FIELDS OF TABLE GT_T052S
      FOR ALL ENTRIES IN GT_T052
      WHERE ZTERM = GT_T052-ZTERM.

    SELECT * FROM T052U
      INTO CORRESPONDING FIELDS OF TABLE GT_T052U
      FOR ALL ENTRIES IN GT_T052S
      WHERE ZTERM = GT_T052S-RATZT.
  ENDIF.

  SELECT * FROM ZFI017_1
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFI017_1
    FOR ALL ENTRIES IN GT_EKKO
    WHERE EBELN = GT_EKKO-EBELN.
*

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

  DATA L_NETWR    TYPE EKPO-NETWR.
  DATA L_WRBTR    TYPE RSEG-WRBTR.

  LOOP AT GT_EKKO INTO GS_EKKO.

*采购组
    GS_DATA-EKGRP = GS_EKKO-EKGRP.

*采购组描述
    READ TABLE GT_T024 INTO GS_T024
    WITH KEY EKGRP = GS_DATA-EKGRP.
    IF SY-SUBRC = 0.
      GS_DATA-EKNAM = GS_T024-EKNAM.
    ENDIF.

*合同订单号
    GS_DATA-EBELN = GS_EKKO-EBELN.

*订单创建日期
    GS_DATA-AEDAT = GS_EKKO-AEDAT.

**项目编号
    READ TABLE GT_EKKN INTO GS_EKKN
    WITH KEY EBELN = GS_DATA-EBELN.
    IF SY-SUBRC = 0.
      IF GS_EKKN-PS_PSP_PNR IS  NOT INITIAL.

*WBS内码转换WBS元素
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = GS_EKKN-PS_PSP_PNR
          IMPORTING
            OUTPUT = GS_DATA-POSID+0(12).

      ELSEIF GS_EKKN-NPLNR IS NOT INITIAL.
        CLEAR GS_AUFK.
        SELECT SINGLE * FROM AUFK
        INTO CORRESPONDING FIELDS OF GS_AUFK
        WHERE AUFNR = GS_EKKN-NPLNR.

        IF SY-SUBRC = 0.
*WBS内码转换WBS元素
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_AUFK-PSPEL
            IMPORTING
              OUTPUT = GS_DATA-POSID+0(12).
        ENDIF.
      ENDIF.
    ENDIF.

*项目名称
    IF GS_DATA-POSID IS NOT INITIAL.
      SELECT SINGLE POST1 FROM PRPS
        INTO  GS_DATA-POST1
        WHERE POSID = GS_DATA-POSID.
    ENDIF.

*合同金额
    CLEAR L_NETWR.
    LOOP AT GT_EKPO INTO GS_EKPO
     WHERE EBELN = GS_DATA-EBELN.

*读取条件类型PB00,以及PBXX对应的价格
      LOOP AT GT_KONV INTO GS_KONV
      WHERE  KNUMV = GS_EKKO-KNUMV
      AND    KPOSN = GS_EKPO-EBELP
      AND    KSCHL = 'PBXX'.
        L_NETWR = L_NETWR + GS_KONV-KWERT.
      ENDLOOP.

      LOOP AT GT_KONV INTO GS_KONV
      WHERE  KNUMV = GS_EKKO-KNUMV
      AND    KPOSN = GS_EKPO-EBELP
      AND    KSCHL = 'PB00'.
        L_NETWR = L_NETWR + GS_KONV-KWERT.
      ENDLOOP.
    ENDLOOP.

    GS_DATA-NETWR = L_NETWR.

*已开票金额
    CLEAR L_WRBTR.
    LOOP AT GT_RSEG INTO GS_RSEG
    WHERE EBELN = GS_DATA-EBELN.
      IF GS_RSEG-SHKZG = 'S'.
        L_WRBTR  = L_WRBTR + GS_RSEG-WRBTR.
      ELSE.
        L_WRBTR  = L_WRBTR - GS_RSEG-WRBTR.
      ENDIF.
    ENDLOOP.

    GS_DATA-WRBTR = L_WRBTR.

**整张采购订单金额（本币） * 汇率
*    GS_DATA-NETWR   = L_NETWR * GS_EKKO-WKURS.

*子付款条件 *应付比例 *应付金额
    READ TABLE GT_T052 INTO GS_T052
    WITH KEY ZTERM = GS_EKKO-ZTERM.
    IF SY-SUBRC = 0.
      IF GS_T052-XSPLT IS INITIAL.
*        GS_DATA-RATPZ     = 100.
*
**整张采购订单金额（本币） * 汇率
*        GS_DATA-NETWR   = L_NETWR * GS_EKKO-WKURS.
*        APPEND GS_DATA TO GT_DATA.
*        CLEAR GS_DATA.
      ELSE.

        LOOP AT GT_T052S INTO GS_T052S
        WHERE ZTERM      = GS_EKKO-ZTERM
        AND   RATZT+0(2) = 'Y9'.
          GS_DATA-YFJE1 = GS_DATA-NETWR * GS_T052S-RATPZ / 100.
          GS_DATA-RATPZ = GS_T052S-RATPZ.
          GS_DATA-ZTERM = GS_T052S-RATZT.
          GS_DATA-ZCH   = SY-TABIX.

*子付款条件描述
          READ TABLE GT_T052U INTO GS_T052U
          WITH KEY ZTERM = GS_T052S-RATZT.
          IF SY-SUBRC = 0.
            GS_DATA-TEXT1 = GS_T052U-TEXT1.
          ENDIF.

          GS_DATA-ZCH = SY-TABIX.

*质保到期日
          READ TABLE GT_ZFI017_1 INTO GS_ZFI017_1
          WITH KEY EBELN = GS_DATA-EBELN
                   ZCH   = GS_DATA-ZCH.
          IF SY-SUBRC = 0.
            GS_DATA-ZBDQR = GS_ZFI017_1-ZBDQR.
          ENDIF.

          APPEND GS_DATA TO GT_DATA.
        ENDLOOP.
      ENDIF.
    ENDIF.


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
*  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'EKGRP'        '采购组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKNAM'        '采购组描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'        '采购订单号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AEDAT'        '订单创建日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'        '项目WBS'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NETWR'        '采购订单含税金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RATPZ'        '应付比例'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE1'        '应付金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM '       '子付款条件'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT1'        '子付款条件描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'        '已开票金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZBDQR'        '质保到期日'         '' '' '' 'X' '' 'ZFI017_1' 'ZBDQR'.

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
*  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
*  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
*  APPEND GW_EVENTS TO GT_EVENTS.
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
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "

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


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'PSPID'
*        AND GS_DATA-PSPID IS NOT INITIAL.
*        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
*        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
*      ENDIF.

*保存
    WHEN '&DATA_SAVE'.
      REFRESH GT_ZFI017.
      LOOP AT GT_DATA INTO GS_DATA.
        MOVE-CORRESPONDING GS_DATA TO GS_ZFI017.
        APPEND GS_ZFI017 TO GT_ZFI017.
        CLEAR GS_ZFI017.
      ENDLOOP.

      MODIFY ZFI017_1 FROM TABLE GT_ZFI017.

      MESSAGE '保存成功' TYPE 'S'.

  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND
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
