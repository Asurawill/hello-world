REPORT ZMM023.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/11
*& Request       :
*& Descriptions  : 采购申请与采购订单执行对比报表
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
TABLES:PROJ,PRPS,EBAN,EKPO,EKKO,ZMM024.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        PSPID  TYPE PROJ-PSPID, "项目定义
        POST1  TYPE PROJ-POST1, "项目描述
        POSID  TYPE PRPS-POSID, "WBS元素
        WERKS  TYPE EKPO-WERKS, "工厂
        AFNAM  TYPE EKPO-AFNAM, "申请者
        BANFN  TYPE EBAN-BANFN, "采购申请编号
        BNFPO  TYPE EBAN-BNFPO, "采购申请项目
        BEDNR  TYPE EKPO-BEDNR, "需求跟踪号
        EKGRP  TYPE EBAN-EKGRP, "采购申请采购组
        BADAT  TYPE EBAN-BADAT, "采购申请求日期
        MATNR  TYPE EBAN-MATNR, "采购申请物料编码
        TXZ01  TYPE EBAN-TXZ01, "采购申请物料描述
        MENGE  TYPE EBAN-MENGE, "采购申请数量
        MATKL  TYPE EBAN-MATKL, "采购申请物料组
        EBELN  TYPE EKPO-EBELN, "采购订单
        EBELP  TYPE EKPO-EBELP, "采购订单行号
        LOEKZ  TYPE EKPO-LOEKZ, "采购订单删除标识
        EKGRP1 TYPE EKKO-EKGRP, "采购订单采购组
        BEDAT  TYPE EKKO-BEDAT, "采购订单创建日期
        LIFNR  TYPE EKKO-LIFNR, "供应商
        NAME1  TYPE LFA1-NAME1, "供应商名称
        MATNR1 TYPE EKPO-MATNR, "采购订单物料编码
        TXZ011 TYPE EKPO-TXZ01, "采购订单物料编码
        MENGE1 TYPE EKPO-MENGE, "采购订单数量
        MATKL1 TYPE EKPO-MATKL, "采购订单物料组
        SJDM   TYPE ZMM024-SJDM, "设计代码
      END OF TY_DATA.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_EKPO TYPE TABLE OF EKPO.
DATA GS_EKPO TYPE EKPO.

DATA GT_EKKO TYPE TABLE OF EKKO.
DATA GS_EKKO TYPE EKKO.

DATA GT_EKKN TYPE TABLE OF EKKN.
DATA GS_EKKN TYPE EKKN.

DATA GT_PRPS TYPE TABLE OF PRPS.
DATA GS_PRPS TYPE PRPS.

DATA GT_PROJ TYPE TABLE OF PROJ.
DATA GS_PROJ TYPE PROJ.

DATA GT_EBAN TYPE TABLE OF EBAN.
DATA GS_EBAN TYPE EBAN.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.

DATA GT_ZMM024 TYPE TABLE OF ZMM024.
DATA GS_ZMM024 TYPE ZMM024.
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

INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
  SELECT-OPTIONS:S_PSPID    FOR PROJ-PSPID, "项目定义
                 S_POSID    FOR PRPS-POSID, "WBS元素
                 S_WERKS    FOR EKPO-WERKS, "工厂
                 S_AFNAM    FOR EKPO-AFNAM, "申请者
                 S_BANFN    FOR EBAN-BANFN, "采购申请编码
                 S_EKGRP    FOR EBAN-EKGRP, "采购申请采购组
                 S_BADAT    FOR EBAN-BADAT, "采购申请创建日期
                 S_MATNR    FOR EBAN-MATNR, "采购申请物料编码
                 S_MATKL    FOR EBAN-MATKL, "采购申请物料组
                 S_EBELN    FOR EKPO-EBELN, "采购订单
                 S_LOEKZ    FOR EKPO-LOEKZ, "采购订单删除标识
                 S_EKGRP1   FOR EKKO-EKGRP, "采购订单采购组
                 S_BEDAT    FOR EKKO-BEDAT, "采购订单创建日期
                 S_MATNR1   FOR EKPO-MATNR, "采购订单物料编码
                 S_MATKL1   FOR EKPO-MATKL, "采购订单物料组
                 S_SJDM     FOR ZMM024-SJDM.   "设计代码
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

  SELECT * FROM EKPO
  INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
  WHERE KNTTP = 'Q'
*&--代码注释 BY HANDYBY 12.06.2017 11:02:58  BEGIN
*    AND   WERKS = '1800'
*&--代码注释 BY HANDYBY 12.06.2017 11:02:58  END

  AND   EBELN IN S_EBELN
  AND   WERKS IN S_WERKS
  AND   AFNAM IN S_AFNAM
  AND   LOEKZ IN S_LOEKZ
  AND   MATNR IN S_MATNR1
  AND   MATKL IN S_MATKL1
  AND   AFNAM IN S_AFNAM
  AND   LOEKZ <> 'L'.

  CHECK GT_EKPO IS NOT INITIAL.

  SELECT * FROM EKKO
  INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
  FOR ALL ENTRIES IN GT_EKPO
  WHERE EBELN = GT_EKPO-EBELN
  AND   EKGRP IN S_EKGRP1
  AND   BEDAT IN S_BEDAT.

  SELECT * FROM LFA1
  INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
  FOR ALL ENTRIES IN GT_EKKO
  WHERE LIFNR = GT_EKKO-LIFNR.

  SELECT * FROM EKKN
  INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
  FOR ALL ENTRIES IN GT_EKPO
  WHERE EBELN = GT_EKPO-EBELN
  AND   EBELP = GT_EKPO-EBELP.

  SELECT * FROM PRPS
  INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
  FOR ALL ENTRIES IN GT_EKKN
  WHERE PSPNR = GT_EKKN-PS_PSP_PNR
  AND   POSID IN  S_POSID.

  SELECT * FROM PROJ
  INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
  FOR ALL ENTRIES IN GT_PRPS
  WHERE PSPNR = GT_PRPS-PSPHI
  AND   PSPID IN S_PSPID.

  SELECT * FROM EBAN
  INTO CORRESPONDING FIELDS OF TABLE GT_EBAN
*  FOR ALL ENTRIES IN GT_EKPO
*  WHERE ( BANFN = GT_EKPO-BANFN
*  OR    BANFN = GT_EKPO-BEDNR+0(8) )
  WHERE BANFN IN S_BANFN
  AND   EKGRP IN S_EKGRP
  AND   BADAT IN S_BADAT
  AND   MATNR IN S_MATNR
  AND   MATKL IN S_MATKL    .

  SELECT * FROM ZMM024
  INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024.

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
  LOOP AT GT_EKPO INTO GS_EKPO.

    READ TABLE GT_EKKN INTO GS_EKKN
    WITH KEY EBELN = GS_EKPO-EBELN
             EBELP = GS_EKPO-EBELP.
    IF SY-SUBRC = 0.
*WBS元素
      READ TABLE GT_PRPS INTO GS_PRPS
      WITH KEY PSPNR = GS_EKKN-PS_PSP_PNR.
      IF SY-SUBRC = 0.
        GS_DATA-POSID = GS_PRPS-POSID.
      ELSE.
        IF S_POSID IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

*项目定义*项目描述
      READ TABLE GT_PROJ INTO GS_PROJ
      WITH KEY PSPNR = GS_PRPS-PSPHI.
      IF SY-SUBRC = 0.
        GS_DATA-PSPID = GS_PROJ-PSPID.
        GS_DATA-POST1 = GS_PROJ-POST1.
      ELSE.
        IF S_PSPID IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

*采购订单创建日期 采购订单采购组
    READ TABLE GT_EKKO INTO GS_EKKO
    WITH KEY  EBELN = GS_EKPO-EBELN.
    IF SY-SUBRC = 0.
      GS_DATA-BEDAT  = GS_EKKO-BEDAT.
      GS_DATA-EKGRP1 = GS_EKKO-EKGRP.
      GS_DATA-LIFNR  = GS_EKKO-LIFNR.
    ELSE.
      IF S_EKGRP1 IS NOT INITIAL
       OR S_BEDAT IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*供应商描述
    READ TABLE GT_LFA1 INTO GS_LFA1
    WITH KEY LIFNR = GS_EKKO-LIFNR.
    IF SY-SUBRC = 0.
      GS_DATA-NAME1 = GS_LFA1-NAME1.
    ENDIF.
*工厂
    GS_DATA-WERKS = GS_EKPO-WERKS.

*采购申请
    IF GS_EKPO-BANFN IS NOT INITIAL.
      GS_DATA-BANFN = GS_EKPO-BANFN.
    ELSE.

      GS_DATA-BANFN = GS_EKPO-BEDNR+0(8).

*增加前导0
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_DATA-BANFN
        IMPORTING
          OUTPUT = GS_DATA-BANFN.
    ENDIF.

*采购申请行项目
    IF GS_EKPO-BNFPO IS NOT INITIAL.
      GS_DATA-BNFPO = GS_EKPO-BNFPO.
    ELSE.
      GS_DATA-BNFPO = GS_EKPO-BEDNR+8(2).

*增加前导0
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_DATA-BNFPO
        IMPORTING
          OUTPUT = GS_DATA-BNFPO.
    ENDIF.

*需求跟踪号
    GS_DATA-BEDNR  = GS_EKPO-BEDNR.

*采购申请采购组,*采购申请物料编码,请求日期
    READ TABLE GT_EBAN INTO GS_EBAN
    WITH KEY BANFN = GS_DATA-BANFN
             BNFPO = GS_DATA-BNFPO.
    IF SY-SUBRC = 0.
      GS_DATA-EKGRP  = GS_EBAN-EKGRP.
      GS_DATA-BADAT  = GS_EBAN-BADAT.
      GS_DATA-MATNR  = GS_EBAN-MATNR.
      GS_DATA-TXZ01  = GS_EBAN-TXZ01.
      GS_DATA-MENGE  = GS_EBAN-MENGE.
      GS_DATA-MATKL  = GS_EBAN-MATKL.

*申请者  CHANGE BY HANDWY 2015-10-22
      GS_DATA-AFNAM = GS_EBAN-AFNAM.
    ELSE.
      IF S_BANFN IS NOT INITIAL
       OR S_EKGRP IS NOT INITIAL
       OR S_BADAT IS NOT INITIAL
       OR S_MATNR IS NOT INITIAL
       OR S_MATKL IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*设计代码
    READ TABLE GT_ZMM024 INTO GS_ZMM024
    WITH KEY MATNR = GS_EBAN-MATNR
             POSID = GS_PROJ-PSPID.
    IF SY-SUBRC = 0.
      GS_DATA-SJDM = GS_ZMM024-SJDM.
    ENDIF.

*采购订单号
    GS_DATA-EBELN = GS_EKPO-EBELN.

*采购订单行号
    GS_DATA-EBELP = GS_EKPO-EBELP.

*采购订单删除标识
    GS_DATA-LOEKZ = GS_EKPO-LOEKZ.

*采购订单物料编码
    GS_DATA-MATNR1 = GS_EKPO-MATNR.

*采购订单物理描述
    GS_DATA-TXZ011 = GS_EKPO-TXZ01.

*采购订单数量
    GS_DATA-MENGE1 = GS_EKPO-MENGE.

*采购订单物料组
    GS_DATA-MATKL1  = GS_EKPO-MATKL.


    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

*设计代码
  IF S_SJDM IS NOT INITIAL .
    DELETE GT_DATA WHERE SJDM NOT IN S_SJDM.
  ENDIF.

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
  INIT_FIELDCAT 'PSPID'       '项目定义'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'POST1'       '项目描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'       'WBS元素'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'       '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AFNAM'       '申请者'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANFN'       '采购申请编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BNFPO'       '采购申请项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEDNR'       '需求跟踪号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'       '采购申请采购组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BADAT'       '采购申请求日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'       '采购申请物料编码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXZ01'       '采购申请物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'       '采购申请数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'       '采购申请物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'       '采购订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELP'       '采购订单行号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LOEKZ'       '采购订单删除标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP1'      '采购订单采购组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEDAT'       '采购订单创建日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR1'      '采购订单物料编码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXZ011'      '采购订单物料编码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE1'      '采购订单数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL1'      '采购订单物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
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
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
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
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'PSPID'
        AND GS_DATA-PSPID IS NOT INITIAL.
        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
      ENDIF.

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
