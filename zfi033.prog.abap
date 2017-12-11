REPORT ZFI033.
TABLES:PROJ,ZPMLOGINFO,ZFI031.
TYPE-POOLS:SLIS.
DATA:BEGIN OF GS_DATA,
    BUKRS  LIKE ZFI031-BUKRS,  "公司代码
    PSPID  LIKE PROJ-PSPID,  "项目定义
    POST1  LIKE PROJ-POST1,    "项目名称
    GJAHR  LIKE ZFI031-GJAHR,   "年度
    YSLX   LIKE ZFI031-YSLX,  "预算类型
    XH     LIKE ZFI031-XH,    "序号
    YSLX_TXT TYPE STRING ,    "预算类型描述
    ZNDZE   LIKE  ZFI031-ZNDZE,  "年度总额
    Y01      LIKE  ZFI031-Y01,    "1月
    Y02      LIKE  ZFI031-Y02,     "2月
    Y03      LIKE  ZFI031-Y03,     "3月
    Y04      LIKE  ZFI031-Y04,     "4月
    Y05      LIKE  ZFI031-Y05,      "5月
    Y06      LIKE  ZFI031-Y06,     "6月
    Y07      LIKE  ZFI031-Y07,     "7月
    Y08      LIKE  ZFI031-Y08,     "8月
    Y09      LIKE  ZFI031-Y09,     "9月
    Y10     LIKE  ZFI031-Y10,    "10月
    Y11     LIKE  ZFI031-Y11,     "11月
    Y12     LIKE  ZFI031-Y12,     "12月
    WAERS    LIKE ZFI031-WAERS,   "货币吗
    BNAME   LIKE  ZFI031-BNAME,   "登陆SAP账号
    WHRQ    LIKE  ZFI031-WHRQ,    "维护日期
    WHSJ    LIKE  ZFI031-WHSJ,     "维护时间
    VERNR   LIKE  ZFI031-VERNR,    "项目经理账号
    SEL(1),
END  OF GS_DATA.
DATA: GT_DATA  LIKE TABLE OF GS_DATA WITH HEADER LINE.
DATA: GT_PROJ  LIKE TABLE OF PROJ WITH HEADER LINE.
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
DATA: INMEG TYPE STRING.
***********************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 .
PARAMETERS:   P_BUKRS LIKE BKPF-BUKRS OBLIGATORY,   "公司代码

              P_GJAHR LIKE BKPF-GJAHR  OBLIGATORY.  "年度

SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID .     "项目定义


SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.


  START-OF-SELECTION.
  "*权限检查检查公司代码

  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    CONCATENATE '无权查询' P_BUKRS '项目成本计划实际明细表'  INTO  INMEG .
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
  SELECT  * INTO CORRESPONDING FIELDS OF TABLE GT_DATA
     FROM ZFI031
     WHERE BUKRS = P_BUKRS AND GJAHR = P_GJAHR AND PSPID IN S_PSPID .
   SORT GT_DATA BY BUKRS ASCENDING PSPID ASCENDING GJAHR ASCENDING  YSLX  ASCENDING  XH DESCENDING .
   DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING PSPID GJAHR YSLX   .

   CHECK GT_DATA[]  IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
   FROM PROJ
   WHERE VBUKR = P_BUKRS  AND PSPID in S_PSPID.
 SORT GT_PROJ BY PSPNR.

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
     LOOP AT GT_DATA.
     READ TABLE GT_PROJ WITH KEY PSPID = GT_DATA-PSPID.
       IF SY-SUBRC = 0.
          GT_DATA-POST1 = GT_PROJ-POST1.
       ENDIF.
       IF GT_DATA-YSLX = '1' .
         GT_DATA-YSLX_TXT = '产值计划'.
       ENDIF.
          IF GT_DATA-YSLX = '2' .
         GT_DATA-YSLX_TXT = '收款计划'.
       ENDIF.
     MODIFY GT_DATA.
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
     INIT_FIELDCAT 'GJAHR'       '年度'                 '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'YSLX'        '预算类型'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'YSLX_TXT'    '预算类型描述'         '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'ZNDZE'       '年度总额'             '' '' 'WAERS' '' '' 'ZFI031' 'ZNDZE'.
     INIT_FIELDCAT 'Y01'          '1月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y01'.
     INIT_FIELDCAT 'Y02'          '2月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y02'.
     INIT_FIELDCAT 'Y03'          '3月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y03'.
     INIT_FIELDCAT 'Y04'          '4月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y04'.
     INIT_FIELDCAT 'Y05'          '5月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y05'.
     INIT_FIELDCAT 'Y06'          '6月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y06'.
     INIT_FIELDCAT 'Y07'          '7月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y07'.
     INIT_FIELDCAT 'Y08'          '8月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y08'.
     INIT_FIELDCAT 'Y09'          '9月'                  '' '' 'WAERS' '' '' 'ZFI031' 'Y09'.
     INIT_FIELDCAT 'Y10'         '10月'                 '' '' 'WAERS' '' '' 'ZFI031' 'Y10'.
     INIT_FIELDCAT 'Y11'         '11月'                 '' '' 'WAERS' '' '' 'ZFI031' 'Y11'.
     INIT_FIELDCAT 'Y12'         '12月'                 '' '' 'WAERS' '' '' 'ZFI031' 'Y12'.
     INIT_FIELDCAT 'WAERS'       '货币码'               '' '' '' '' '' 'BKPF' 'WAERS'.
     INIT_FIELDCAT 'BNAME'       'SAP账号'              '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'WHRQ'        '维护日期'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'WHSJ'        '维护时间'             '' '' '' '' '' '' ''.
     INIT_FIELDCAT 'VERNR'       '项目经理账号'         '' '' '' '' '' '' ''.
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
*      -->P_0454   text
*      -->P_0455   text
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
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.

FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表

  DATA: SELLEN TYPE I .
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*  CASE R_UCOMM.
*      WHEN '&DATA_SAVE'.
*        IF G_WH EQ 'X'.
*             GT_DATA_SEL[] = GT_DATA[].
*             DELETE GT_DATA_SEL[] WHERE SEL NE 'X'.
*             DESCRIBE TABLE GT_DATA_SEL LINES SELLEN.
*             SORT GT_DATA_SEL BY BUKRS PSPID GJAHR YSLX.
*             IF SELLEN > 0 .
*                PERFORM  SAVEDATA.   "执行保存操作
*             ELSE.
*                MESSAGE '请选择要保存的行数' TYPE  'S'  DISPLAY LIKE 'E'.
*             ENDIF.
*        ENDIF.
*
*        IF G_XS EQ 'X'.
*          MESSAGE '显示模式不能保存视图数据！'  TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*
*  ENDCASE.
**
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
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0335   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
   AUTHORITY-CHECK OBJECT 'ZFI_BUK_CK' ID 'ACTVT' FIELD P_ACTVT
                                     ID 'BUKRS' FIELD P_BUKRS.
                                .
ENDFORM.
