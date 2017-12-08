*&---------------------------------------------------------------------*
*& Report  ZPP003
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/6
*& Request       :
*& Descriptions  : 生产计划外料单
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZPP004_1.
************************************************************************
* Tables
************************************************************************
TABLES:AFPO,AFKO.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        ZBOX    TYPE C,
        AUFNR   TYPE AFPO-AUFNR, "生产订单
        WERKS   TYPE AFPO-DWERK, "工厂
        MATNR   TYPE AFPO-MATNR, "物料号
        MAKTX   TYPE MAKT-MAKTX, "描述
        MATNR_Z TYPE AFPO-MATNR, "组建物料号
        MAKTX_Z TYPE MAKT-MAKTX, "描述
        LLSL    TYPE AFPO-WEMNG, "领料数量
        CHARG   TYPE AFPO-CHARG, "批次
        MEINS   TYPE AFPO-MEINS, "单位
        LGORT   TYPE AFPO-LGORT, "库存地点
        NAME    TYPE ADRP-NAME_LAST,  "制单人
      END OF TY_DATA.

*人员信息
TYPES:BEGIN OF TY_NAME,
        BNAME      TYPE USR21-BNAME,      "帐号
        PERSNUMBER TYPE USR21-PERSNUMBER, "人员编号
        NAME_LAST  TYPE ADRP-NAME_LAST,  "姓
      END OF TY_NAME.
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

  IF gw_lvc-fieldname = 'LLSL'.
     gw_lvc-NO_ZERO = 'X'.
     gw_lvc-just    = 'L'."水平对齐方式，L左对齐，R右对齐。
  ENDIF.

  IF gw_lvc-fieldname = 'AUFNR'.
  gw_lvc-F4AVAILABL = 'X'.
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
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

*传入打印的值
DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LS_DATA TYPE TY_DATA.

*制单人姓名
DATA GS_NAME TYPE TY_NAME.

*设置工单搜索帮助
DATA GT_AFKO TYPE TABLE OF AFKO.
DATA GS_AFKO TYPE AFKO.

**更新数据库表
*DATA GT_ZPP004 TYPE TABLE OF ZPP004.
*DATA GS_ZPP004 TYPE ZPP004.

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
*  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
**           ID 'ACTVT' FIELD '03'
*           ID 'WERKS' FIELD  S_WERKS-LOW.
*  IF SY-SUBRC <> 0.
*    MESSAGE E603(FCO) WITH S_WERKS-LOW.
*  ENDIF.
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
*初始给10空行
  DO 50 TIMES.
    APPEND INITIAL LINE TO GT_DATA.
  ENDDO.

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
*  GW_LAYOUT-CWIDTH_OPT    = 'X'.
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
FORM FRM_INIT_LVC.

  INIT_FIELDCAT 'AUFNR'         TEXT-001      '' '' '' 'X' '' 'COCF_S_DYNF_SN' 'BO_PPORDER'.
  INIT_FIELDCAT 'MATNR'         TEXT-002      '' '' '' '' '' 'AFPO' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'         TEXT-003      '18' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR_Z'       TEXT-004       '' '' '' 'X' '' 'MCHA' 'MATNR'.
  INIT_FIELDCAT 'MAKTX_Z'       TEXT-005       '18' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LLSL'          TEXT-006       '' '' '' 'X' '' 'AFPO' 'WEMNG'.
  INIT_FIELDCAT 'CHARG'         TEXT-007       '' '' '' 'X' '' 'MCHA' 'CHARG'.
  INIT_FIELDCAT 'MEINS'         TEXT-008       '' '' '' '' '' '' ''.
*  INIT_FIELDCAT 'WERKS'         TEXT-008       '' '' '' '' ''  'MCHB' 'WERKS'.
  INIT_FIELDCAT 'LGORT'         TEXT-009       '8' '' '' 'X' '' 'T001L' 'LGORT'.
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
  DATA L_CHECK TYPE C."检查数据

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
    WHEN '&ADD'.
      APPEND INITIAL LINE TO GT_DATA.
      CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

    WHEN '&DELE'.
      DELETE GT_DATA WHERE ZBOX = 'X'.
      CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
*
*    WHEN '&DATA_SAVE'.
*
**更新数据库表
*      REFRESH GT_ZPP004.
*      READ TABLE GT_DATA INTO GS_DATA
*      WITH KEY ZBOX = 'X'.
*
**判断是否选中行项目
*      IF SY-SUBRC = 0.
*        LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
*          MOVE-CORRESPONDING GS_DATA TO GS_ZPP004.
*          APPEND GS_ZPP004 TO GT_ZPP004.
*          CLEAR GS_ZPP004.
*        ENDLOOP.
*
*        MODIFY ZPP004 FROM TABLE GT_ZPP004.
*        IF SY-SUBRC = 0.
*          MESSAGE S002(Z001).
*        ENDIF.

*更改到查看状态
*        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
*          EXPORTING
*            I_READY_FOR_INPUT = 0.
*
*        CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
*
*      ELSE.
*        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
*      ENDIF.

*编辑
*    WHEN '&EDIT'.
*
*      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 0.
*
*        CALL METHOD G_REF_GRID->SET_READY_FOR_INPUT
*          EXPORTING
*            I_READY_FOR_INPUT = 1.

*      ENDIF.
*      CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.

*打印
    WHEN '&PRNT'.
*需进行保存操作
*      et_fieldcat_lvc = gt_fieldcat
*      IF G_REF_GRID->IS_READY_FOR_INPUT( ) = 1.
*        MESSAGE S001(ZMM01)  DISPLAY LIKE 'E'.
*      ELSE.
      CLEAR L_CHECK.

*点击打印时检查选中行的“生产订单号”“组件物料号”“领料数量”，如果这3个字段中有1个有值，而其余2个字段存在空值，则报错“选中数据存在空值!
      LOOP AT GT_DATA INTO GS_DATA
       WHERE ZBOX = 'X'.
        IF  GS_DATA-AUFNR  IS INITIAL
         OR GS_DATA-MATNR_Z IS INITIAL
         OR GS_DATA-LLSL    IS INITIAL.
          L_CHECK = 'X'.
          MESSAGE S006(ZPP01) DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK L_CHECK <> 'X'.

*删除工单号为空的数据
      DELETE  GT_DATA WHERE AUFNR IS INITIAL.

      READ TABLE GT_DATA INTO GS_DATA
      WITH KEY ZBOX = 'X'.
      IF SY-SUBRC = 0.
        PERFORM FRM_PRINT_DATA.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.
*      ENDIF.
  ENDCASE.

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


  DATA: G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL       TYPE LVC_S_STBL.
  DATA LT_OUT TYPE  TABLE OF TY_DATA.
  DATA LS_OUT TYPE TY_DATA.
  DATA LS_MAKT TYPE MAKT.
  DATA LS_AFPO TYPE AFPO.
  DATA LS_MARA TYPE MARA.
  DATA LS_RESB TYPE RESB.
  DATA LS_MARC TYPE MARC.
  DATA: WA_MOD_CELL TYPE LVC_S_MODI.

  FIELD-SYMBOLS:<L_MATNR> TYPE ANY.

  CLEAR:LS_AFPO,
        LS_MAKT,
        LS_MARA,
        LS_RESB,
        LS_MARC.
  CLEAR WA_MOD_CELL.
  LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO WA_MOD_CELL .
    CHECK WA_MOD_CELL IS NOT INITIAL.

    TRANSLATE WA_MOD_CELL-VALUE TO UPPER CASE.

*根据输入的工单自动带出工单物料
    IF  WA_MOD_CELL-FIELDNAME = 'AUFNR'.
      SELECT SINGLE * FROM AFPO
        INTO CORRESPONDING FIELDS OF LS_AFPO
        WHERE AUFNR = WA_MOD_CELL-VALUE.

      IF SY-SUBRC = 0.
        SELECT SINGLE * FROM MAKT
          INTO CORRESPONDING FIELDS OF LS_MAKT
          WHERE MATNR = LS_AFPO-MATNR
          AND   SPRAS = SY-LANGU.
      ENDIF.

*带出物料描述
      READ TABLE  GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
      IF SY-SUBRC = 0.
        GS_DATA-MAKTX = LS_MAKT-MAKTX.
        GS_DATA-MATNR = LS_AFPO-MATNR.
        GS_DATA-NAME  = GS_NAME-NAME_LAST.
        GS_DATA-WERKS = LS_AFPO-DWERK.
*&--代码注释 BY HANDYBY 10.07.2017 19:04:13  BEGIN
*        MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
*        CLEAR GS_DATA.
*&--代码注释 BY HANDYBY 10.07.2017 19:04:13  END
      ENDIF.

*&--代码添加 BY HANDYBY 10.07.2017 18:44:01  BEGIN

* 取出组件物料号
      DATA: BEGIN OF LS_MAST,
              MATNR TYPE MAST-MATNR,
              WERKS TYPE MAST-WERKS,
              STLAN TYPE MAST-STLAN,
              STLNR TYPE MAST-STLNR,
              STLAL TYPE MAST-STLAL,
            END OF LS_MAST .
      DATA: BEGIN OF LS_STKO,
              STLTY TYPE STKO-STLTY,
              STLNR TYPE STKO-STLNR,
              STLAL TYPE STKO-STLAL,
              STKOZ TYPE STKO-STKOZ,
            END OF LS_STKO .
      DATA: BEGIN OF LS_STPO,
              STLTY TYPE STPO-STLTY,
              STLNR TYPE STPO-STLNR,
              STLKN TYPE STPO-STLKN,
              STPOZ TYPE STPO-STPOZ,
              IDNRK TYPE STPO-IDNRK,
              MEINS TYPE STPO-MEINS,
            END OF LS_STPO .
      DATA LT_STPO LIKE TABLE OF LS_STPO .
      DATA: BEGIN OF LS_MAKT2 ,
              MATNR TYPE MAKT-MATNR,
              MAKTX TYPE MAKT-MAKTX,
            END OF LS_MAKT2  .
      DATA LT_MAKT2 LIKE TABLE OF LS_MAKT2 .

      SELECT SINGLE
             MATNR
             WERKS
             STLAN
             STLNR
             STLAL
        INTO CORRESPONDING FIELDS OF LS_MAST
        FROM MAST
       WHERE MATNR = LS_AFPO-MATNR
         AND WERKS = LS_AFPO-DWERK
         AND STLAN = '1' .
      IF LS_MAST IS NOT INITIAL .
        SELECT SINGLE
               STLTY
               STLNR
               STLAL
               STKOZ
          INTO CORRESPONDING FIELDS OF LS_STKO
          FROM STKO
         WHERE STLNR = LS_MAST-STLNR
           AND STLAL = LS_MAST-STLAL
           AND STLTY = 'M' .
        IF LS_STKO IS NOT INITIAL .
          SELECT STLTY
                 STLNR
                 STLKN
                 STPOZ
                 IDNRK
                 MEINS
            INTO CORRESPONDING FIELDS OF TABLE LT_STPO
            FROM STPO
           WHERE STLNR = LS_STKO-STLNR
             AND STLTY = 'M' .
          IF LT_STPO IS NOT INITIAL .
            SELECT MATNR
                   MAKTX
              INTO CORRESPONDING FIELDS OF TABLE LT_MAKT2
              FROM MAKT
               FOR ALL ENTRIES IN LT_STPO
             WHERE MATNR = LT_STPO-IDNRK .
          ENDIF.

          DATA L_INDEX TYPE I VALUE IS INITIAL .
          L_INDEX = WA_MOD_CELL-ROW_ID - 1 .
* 填充物料号，描述，单位
          LOOP AT LT_STPO INTO LS_STPO .
            L_INDEX = L_INDEX + 1 .

            GS_DATA-AUFNR = WA_MOD_CELL-VALUE .
            GS_DATA-MATNR_Z = LS_STPO-IDNRK .
            READ TABLE LT_MAKT2 INTO LS_MAKT2 WITH KEY MATNR = LS_STPO-IDNRK .
            IF SY-SUBRC = 0 .
              GS_DATA-MAKTX_Z = LS_MAKT2-MAKTX .
            ENDIF.
            GS_DATA-MEINS = LS_STPO-MEINS .

            MODIFY GT_DATA FROM GS_DATA INDEX L_INDEX .

            CLEAR LS_STPO .
            CLEAR LS_MAKT2 .
            CLEAR: GS_DATA-MATNR_Z ,
                   GS_DATA-MAKTX_Z ,
                   GS_DATA-MEINS .
          ENDLOOP.
        ENDIF.
      ENDIF.

*&--代码添加 BY HANDYBY 10.07.2017 18:44:01  END

    ENDIF.

*根据输入的子件物料自动带出描述和单位
    IF WA_MOD_CELL-FIELDNAME = 'MATNR_Z'.

      CLEAR LS_MAKT.
      CLEAR LS_MARA.
      CLEAR LS_RESB.

      SELECT SINGLE * FROM MAKT
        INTO CORRESPONDING FIELDS OF LS_MAKT
        WHERE MATNR = WA_MOD_CELL-VALUE
            AND   SPRAS = SY-LANGU.

      SELECT SINGLE * FROM MARA
        INTO CORRESPONDING FIELDS OF LS_MARA
        WHERE MATNR = WA_MOD_CELL-VALUE.

      READ TABLE GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
      IF SY-SUBRC = 0.
        GS_DATA-MAKTX_Z = LS_MAKT-MAKTX.
        GS_DATA-MEINS   = LS_MARA-MEINS.

**自动带出库存地点
*        SELECT SINGLE * FROM RESB
*        INTO CORRESPONDING FIELDS OF LS_RESB
*        WHERE MATNR = WA_MOD_CELL-VALUE
*        AND   AUFNR = GS_DATA-AUFNR.

*自动带出库存地点
        IF GS_DATA-WERKS IS  INITIAL.
          SELECT SINGLE * FROM MARC
             INTO CORRESPONDING FIELDS OF LS_MARC
             WHERE MATNR = WA_MOD_CELL-VALUE
             AND   WERKS = '1100'.
        ELSE.
          SELECT SINGLE * FROM MARC
           INTO CORRESPONDING FIELDS OF LS_MARC
           WHERE MATNR = WA_MOD_CELL-VALUE
           AND   WERKS = GS_DATA-WERKS.
        ENDIF.



*        GS_DATA-LGORT = LS_RESB-LGORT.
        GS_DATA-LGORT = LS_MARC-LGPRO.

        MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
        CLEAR GS_DATA.
      ENDIF.
    ENDIF.

*领用数量，批次，库存地点
    READ TABLE GT_DATA INTO GS_DATA INDEX WA_MOD_CELL-ROW_ID.
    IF SY-SUBRC = 0.
      CASE WA_MOD_CELL-FIELDNAME.
        WHEN 'LLSL'.
          REPLACE ',' IN WA_MOD_CELL-VALUE WITH ''.
          GS_DATA-LLSL   = WA_MOD_CELL-VALUE.
        WHEN 'CHARG'.
          GS_DATA-CHARG  = WA_MOD_CELL-VALUE.
        WHEN 'LGORT'.
          GS_DATA-LGORT  = WA_MOD_CELL-VALUE.
      ENDCASE.
      MODIFY GT_DATA FROM GS_DATA INDEX WA_MOD_CELL-ROW_ID.
      CLEAR GS_DATA.
    ENDIF.
  ENDLOOP.

*刷新屏幕
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

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
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 9,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFPP004_1'.
  DATA L_LINE TYPE I. "统计打印的行进行补行
  DATA G_LINE TYPE I. "设定换页行数
  REFRESH LT_DATA.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "smartforms的名字
    IMPORTING
      FM_NAME            = G_NAME             "对应的smartforms的函数
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

*根据选中的值进行打印

  LT_DATA = GT_DATA.
  DELETE LT_DATA  WHERE AUFNR = ''.
  DELETE LT_DATA  WHERE  ZBOX <> 'X'.

*每页补齐10行
  DESCRIBE TABLE LT_DATA LINES L_LINE.
  WHILE L_LINE MOD 10 <> 0.
    L_LINE = L_LINE + 1.
    APPEND INITIAL LINE TO LT_DATA.
  ENDWHILE.

*设定10行自动换页
  G_LINE = 10.

  CALL FUNCTION G_NAME
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
      G_LINE             = G_LINE
*     w_head             = lw_prt
*         TABLES
*     t_item             = lt_prt[]
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
