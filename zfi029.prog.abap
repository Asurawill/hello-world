*&---------------------------------------------------------------------*
*& Report  ZFI029
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/07/28
*& Request       :
*& Descriptions  : 进销存报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZFI029.

************************************************************************
* Tables
************************************************************************
TABLES:BKPF,MARC,BSEG.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA.
        INCLUDE STRUCTURE ZFI029_H.
TYPES: END OF TY_DATA.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

*抬头
DATA GT_ZFI029_H TYPE TABLE OF ZFI029_H.
DATA GS_ZFI029_H TYPE  ZFI029_H.

*行项目
DATA GT_ZFI029 TYPE TABLE OF ZFI029.
DATA GS_ZFI029 TYPE ZFI029.

DATA GT_BSEG   TYPE TABLE OF BSEG.
DATA GS_BSEG   TYPE BSEG.

DATA GT_T156   TYPE TABLE OF T156.
DATA GS_T156   TYPE T156.
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
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME .
SELECT-OPTIONS: S_BUKRS FOR BKPF-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_GJAHR FOR BKPF-GJAHR OBLIGATORY NO INTERVALS NO-EXTENSION DEFAULT SY-DATUM+0(4),
                S_WERKS FOR MARC-WERKS OBLIGATORY,
                S_MATNR FOR MARC-MATNR ,
                S_MONAT FOR BKPF-MONAT OBLIGATORY,
                S_LGORT FOR MARC-LGPRO,
                S_XSAKNR FOR BSEG-SAKNR.
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
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
  PERFORM FRM_AUTH_CHECK.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

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
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD S_BUKRS-LOW.
  IF SY-SUBRC <> 0.
    MESSAGE S899(MM) WITH '您没有公司代码' S_BUKRS '的权限' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
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

*处理根据期间，取出对应的日期.
  DATA L_START_DATA TYPE SY-DATUM.
  DATA E_START_DATA TYPE SY-DATUM.

  RANGES: R_BUDAT FOR BKPF-BUDAT.
  IF S_MONAT-HIGH IS NOT INITIAL.

    CONCATENATE S_GJAHR-LOW S_MONAT-HIGH '01' INTO L_START_DATA.
    CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = L_START_DATA
      IMPORTING
        E_DATE = E_START_DATA
      EXCEPTIONS
        OTHERS = 1.

    CLEAR L_START_DATA.
    CONCATENATE S_GJAHR-LOW S_MONAT-LOW '01' INTO L_START_DATA.
    R_BUDAT-SIGN   = 'I'.
    R_BUDAT-OPTION = 'BT'.
    R_BUDAT-LOW    = L_START_DATA.
    R_BUDAT-HIGH   = E_START_DATA.
    APPEND R_BUDAT.
  ELSE.

    CONCATENATE S_GJAHR-LOW S_MONAT-LOW '01' INTO L_START_DATA.
    R_BUDAT-SIGN   = 'I'.
    R_BUDAT-OPTION = 'BT'.
    R_BUDAT-LOW    = L_START_DATA.
    R_BUDAT-HIGH   = '99999999'.
    APPEND R_BUDAT.

  ENDIF.

  SELECT * FROM ZFI029_H INTO
    CORRESPONDING FIELDS OF TABLE GT_ZFI029_H
    WHERE YBUKRS IN S_BUKRS
    AND   WERK   IN S_WERKS
    AND   XMATNR IN S_MATNR
    AND   LGORT  IN S_LGORT
    AND   XSAKNR IN S_XSAKNR.

  CHECK GT_ZFI029_H IS NOT INITIAL.

  SELECT * FROM ZFI029 INTO
    CORRESPONDING FIELDS OF TABLE GT_ZFI029
    FOR ALL ENTRIES IN GT_ZFI029_H
    WHERE FIELD_LINK = GT_ZFI029_H-FIELD_LINK
    AND   XGJAHR IN S_GJAHR
    AND   BUDAT  IN R_BUDAT
    AND   XBELNR <> '##########'
    AND   XBELNR <> ''.

*取出移动类型
  SELECT * FROM T156 INTO
    CORRESPONDING FIELDS OF TABLE GT_T156.

  CHECK GT_ZFI029 IS NOT INITIAL.

*取出会计凭证
  SELECT * FROM BSEG INTO
    CORRESPONDING FIELDS OF TABLE GT_BSEG
    FOR ALL ENTRIES IN GT_ZFI029
    WHERE BUKRS = GT_ZFI029-XBUKRS
    AND   GJAHR = GT_ZFI029-XGJAHR
    AND   BELNR = GT_ZFI029-XBELNR
    AND   BUZEI = GT_ZFI029-BUZEI.


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

  DATA L_DELTLBKUMD TYPE ZFI029_H-DELTLBKUMD.
  DATA L_DELTLBKUMK TYPE ZFI029_H-DELTLBKUMK.
  DATA L_DELTSALKD  TYPE ZFI029_H-DELTSALKD.
  DATA L_DELTSALKK  TYPE ZFI029_H-DELTSALKK.


  LOOP AT GT_ZFI029_H INTO GS_ZFI029_H.
    MOVE-CORRESPONDING GS_ZFI029_H TO GS_DATA.

    CLEAR:L_DELTLBKUMD,
          L_DELTLBKUMK,
          L_DELTSALKD ,
          L_DELTSALKK .

    LOOP AT GT_ZFI029 INTO GS_ZFI029
    WHERE FIELD_LINK = GS_ZFI029_H-FIELD_LINK.

*处理差异凭证
      READ TABLE GT_BSEG INTO GS_BSEG
      WITH KEY GJAHR = GS_ZFI029-XGJAHR
               BELNR = GS_ZFI029-XBELNR
               BUZEI = GS_ZFI029-BUZEI
               BUKRS = GS_ZFI029-XBUKRS.
      IF SY-SUBRC = 0.
        IF GS_BSEG-XNEGP <> 'X'.
          L_DELTLBKUMD = L_DELTLBKUMD + GS_ZFI029-XMENSH_IN. "入库数量
          L_DELTLBKUMK = L_DELTLBKUMK + GS_ZFI029-XMENSH_OUT."出库数量
          L_DELTSALKD  = L_DELTSALKD  + GS_ZFI029-XDMSHB_IN.  "入库金额
          L_DELTSALKK  = L_DELTSALKK  + GS_ZFI029-XDMSHB_OUT. "出库金额.
        ELSE.
          L_DELTLBKUMD = L_DELTLBKUMD + GS_ZFI029-XMENSH_OUT. "入库数量
          L_DELTLBKUMK = L_DELTLBKUMK + GS_ZFI029-XMENSH_IN. "出库数量
          L_DELTSALKD  = L_DELTSALKD  + GS_ZFI029-XDMSHB_OUT.  "入库金额
          L_DELTSALKK  = L_DELTSALKK  + GS_ZFI029-XDMSHB_IN. "出库金额.
        ENDIF.
      ELSE.
*根据移动类型判断进出
        READ TABLE GT_T156 INTO GS_T156
        WITH KEY  BWART = GS_ZFI029-BWART.
        IF SY-SUBRC = 0.
          IF GS_T156-XSTBW <> 'X' AND GS_T156-SHKZG = 'S'.
            L_DELTLBKUMD = L_DELTLBKUMD + GS_ZFI029-XMENSH_IN. "入库数量
            L_DELTLBKUMK = L_DELTLBKUMK + GS_ZFI029-XMENSH_OUT."出库数量
            L_DELTSALKD  = L_DELTSALKD  + GS_ZFI029-XDMSHB_IN. "入库金额
            L_DELTSALKK  = L_DELTSALKK  + GS_ZFI029-XDMSHB_OUT."出库金额.
          ELSE.
            L_DELTLBKUMD = L_DELTLBKUMD + GS_ZFI029-XMENSH_OUT. "入库数量
            L_DELTLBKUMK = L_DELTLBKUMK + GS_ZFI029-XMENSH_IN. "出库数量
            L_DELTSALKD  = L_DELTSALKD  + GS_ZFI029-XDMSHB_OUT."入库金额
            L_DELTSALKK  = L_DELTSALKK  + GS_ZFI029-XDMSHB_IN. "出库金额.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*      入库数量
    GS_DATA-DELTLBKUMD   = L_DELTLBKUMD.

*      出库数量
    GS_DATA-DELTLBKUMK   = L_DELTLBKUMK.

*      入库金额
    GS_DATA-DELTSALKD    = L_DELTSALKD.

*      出库金额
    GS_DATA-DELTSALKK    = L_DELTSALKK .

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
    CLEAR GS_ZFI029_H.
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
*  PERFORM FRM_BUILD_EVENT.
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
  INIT_FIELDCAT 'XMATNR'   '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWTAR'   '评估类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERK'   '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LGORT'   '库存地点'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CHARG'   '批号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SOBKZ'   '特殊库存标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LINKK'   'Order/WBS/Vendor/Customer'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'   '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEGPRICE1'   'Price unit storage material'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'   '货币码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YMEINS'   '基本计量单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEGREMN'   '期初数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ENDREMN'   '期末数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DELTLBKUMD'    '期间入库数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DELTLBKUMK'    '期间出库数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DELTSALKD'    '期间入库金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DELTSALKK'    '期间出库金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEGSUM'    '期初金额'         '' '' '' '' '' '' '' .
  INIT_FIELDCAT 'ENDSUM'    '期末金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKLAS'      '评估类'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XSAKNR'    '总帐科目编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP'    '采购组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MTART'    '物料类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'    '物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YBUKRS'   '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BISMT'    '旧物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STPRS'    '标准价格'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PEINH'    '价格单位'         '' '' '' '' '' '' ''.
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
*     IT_EVENTS                = GT_EVENTS[]
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


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BSTAT <> 'V'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BSTAT = 'V'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLP' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
*      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
