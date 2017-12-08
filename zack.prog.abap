REPORT ZACK.
*&---------------------------------------------------------------------*
*& Report  ZACK
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/10/23
*& Request       :
*& Descriptions  : OA信息记录报表
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
TABLES:ZZACKNOW.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_OUTPUT,
        STAU   TYPE ICONNAME,       "状态栏
        DDTEXT TYPE VAL_TEXT,      "接口类型域值描述
        DYTEXT TYPE VAL_TEXT.      "调用系统名称
        INCLUDE  TYPE ZZACKNOW.
TYPES:END OF TY_OUTPUT.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_OUTPUT TYPE TABLE OF TY_OUTPUT.
DATA GS_OUTPUT TYPE TY_OUTPUT.

DATA GT_DD07T_DYXT  TYPE TABLE OF DD07T.
DATA GS_DD07T_DYXT  TYPE DD07T.

DATA GT_DD07T_JKLX  TYPE TABLE OF DD07T.
DATA GS_DD07T_JKLX  TYPE DD07T.
************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  IF &4 = 'X'.
    GW_LVC-KEY = 'X'.
  ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
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
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
                S_ZZDYXT FOR ZZACKNOW-ZZDYXTM  OBLIGATORY,
                S_ZZINT FOR ZZACKNOW-ZZINTTY NO-EXTENSION NO INTERVALS OBLIGATORY,
                S_ZZCDA FOR ZZACKNOW-ZZCDATE,
                S_ZZCTI FOR ZZACKNOW-ZZCTIME,
                S_ZZPRF FOR ZZACKNOW-ZZPROFG NO-EXTENSION NO INTERVALS.
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
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
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
  SELECT * FROM ZZACKNOW
  INTO CORRESPONDING FIELDS OF TABLE GT_OUTPUT
  WHERE ZZDYXTM IN S_ZZDYXT
  AND   ZZINTTY IN S_ZZINT
  AND   ZZCDATE IN S_ZZCDA
  AND   ZZCTIME IN S_ZZCTI
  AND   ZZPROFG IN S_ZZPRF.

SORT GT_OUTPUT BY ZZDYXTM ZZINTTY ZBUKRS USNAM ZZCDATE ZZCTIME .
*查询域值描述
  SELECT * FROM DD07T
   INTO CORRESPONDING FIELDS OF TABLE GT_DD07T_JKLX
   WHERE DOMNAME  = 'ZINTERFACE_TYPE'
   AND DDLANGUAGE = SY-LANGU.

   SELECT * FROM DD07T
   INTO CORRESPONDING FIELDS OF TABLE GT_DD07T_DYXT
   WHERE DOMNAME  = 'ZZDYXTM'
   AND DDLANGUAGE = SY-LANGU.

ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
  LOOP AT  GT_OUTPUT INTO GS_OUTPUT.
    IF GS_OUTPUT-ZZPROFG  = 'S'.
      GS_OUTPUT-STAU = ICON_GREEN_LIGHT.
    ELSEIF GS_OUTPUT-ZZPROFG = 'E'.
      GS_OUTPUT-STAU = ICON_RED_LIGHT.
    ELSE.
      GS_OUTPUT-STAU = ICON_YELLOW_LIGHT.
    ENDIF.

    READ TABLE GT_DD07T_JKLX INTO GS_DD07T_JKLX
    WITH KEY DOMVALUE_L = GS_OUTPUT-ZZINTTY.
    IF SY-SUBRC = 0.
      GS_OUTPUT-DDTEXT = GS_DD07T_JKLX-DDTEXT.
    ENDIF.

   READ TABLE GT_DD07T_DYXT INTO GS_DD07T_DYXT
    WITH KEY DOMVALUE_L = GS_OUTPUT-ZZDYXTM.
    IF SY-SUBRC = 0.
      GS_OUTPUT-DYTEXT = GS_DD07T_DYXT-DDTEXT.
    ENDIF.

    MODIFY GT_OUTPUT FROM GS_OUTPUT.
    CLEAR GS_OUTPUT.
    CLEAR GS_DD07T_DYXT.
    CLEAR GS_DD07T_JKLX.
  ENDLOOP.
ENDFORM.                    " FRM_DEAL_DATA
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
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_OUTPUT
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.                    " FRM_ALV_SHOW
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
*  GW_LAYOUT-CWIDTH_OPT   = 'X'.
ENDFORM.                    "INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.                    "INIT_SORT
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.                    "INIT_VARIANT
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
ENDFORM.                    "FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'STAU'           '状态栏'                 '10' '' '' '' '' '' '' .
  INIT_FIELDCAT 'ZZDYXTM'        '调用系统名'            '25' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYTEXT'         '调用系统描述'          '25' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZINTTY'        '接口类型'              '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DDTEXT'         '接口类型描述'          '25' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZBUKRS'         '公司代码'              '4' '' '' '' '' '' ''.
  INIT_FIELDCAT 'USNAM'          '用户名'                '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DDTEXT'         '接口类型描述'          '25' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZPROFG'        '处理标记'              '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZCDATE'        '创建日期'              '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZCTIME'        '创建时间'              '20' '' '' '' '' '' ''.
  INIT_FIELDCAT 'USNAM'          '创建人'                '10' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZKEYF1'        '键区1'                 '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZKEYF2'        '键区2'                 '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZKEYF3'        '键区3'                 '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZKEYF4'        '键区4'                 '15' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZCOMEN'        '日志记录'              '50' '' '' '' '' '' ''.
ENDFORM.                    "FRM_INIT_LVC
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
                USING  PU_STATUS
                       PU_UCOMM
                       PW_LAYOUT TYPE LVC_S_LAYO
                       PW_VARIANT TYPE DISVARIANT
                       PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
      IT_SORT_LVC              = PT_SORT[]
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
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
ENDFORM.                    "ALV_USER_COMMAND
