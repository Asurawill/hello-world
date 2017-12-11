*&---------------------------------------------------------------------*
*& Report  ZPP008
*&程序名称： 生产订单修改记录日志表
*&开发日期：2014-08-15
*&创建者：  李权
*&
*&---------------------------------------------------------------------*
*&概要说明
*&
*&---------------------------------------------------------------------*
*&变更记录
*&
*&---------------------------------------------------------------------*
REPORT ZPP007.
TYPE-POOLS: SLIS.
TABLES:ZCO02_LOG,AFPO,USR02.

TYPES:BEGIN OF TY_ALV.
        INCLUDE STRUCTURE ZCO02_LOG.
TYPES:END OF TY_ALV.

DATA:GT_ALV TYPE STANDARD TABLE OF ZCO02_LOG WITH HEADER LINE.

DATA:GT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
     GS_LAYOUT   TYPE LVC_S_LAYO,
*     gs_print  TYPE lvc_s_prnt ," "ALV打印格式
     G_REPID     LIKE SY-REPID.
*     v_stru_disvar TYPE disvariant,    "ALV 显示格式
*     it_sort TYPE lvc_t_sort WITH HEADER LINE, "ALV 排序内表
*     git_events TYPE slis_t_event,     "ALV 事件
*     git_listheader LIKE slis_t_listheader. "ALV 表头

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:S_AUFNR FOR AFPO-AUFNR,
S_OPDAT FOR ZCO02_LOG-OPDAT,
S_OPUSR FOR USR02-BNAME.
SELECTION-SCREEN END OF BLOCK B1.


INITIALIZATION.

AT SELECTION-SCREEN .
  IF S_AUFNR[] IS INITIAL AND S_OPDAT[] IS INITIAL AND S_OPUSR[] IS INITIAL.
    MESSAGE '请输入合适的查询条件' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  PERFORM FRM_DATA.

END-OF-SELECTION.

  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_FIELDCAT.           "设置输出字段
  PERFORM FRM_OUTPUT_ALV.


*&---------------------------------------------------------------------*
*&      Form  FRM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DATA .
  SELECT *
    FROM ZCO02_LOG
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    WHERE AUFNR IN S_AUFNR
    AND OPDAT IN S_OPDAT
    AND OPUSR IN S_OPUSR.

ENDFORM.                    " FRM_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_FIELDCAT .
  DEFINE ADD_FIELDCAT.
    gt_fieldcat-fieldname = &1.
    gt_fieldcat-reptext = &2.
    gt_fieldcat-outputlen = &3.
    gt_fieldcat-col_pos = &4.
    gt_fieldcat-just = &5.
    append gt_fieldcat.
    clear:gt_fieldcat.
  END-OF-DEFINITION.

*  add_fieldcat 'ZCPLB' '产品类型' '18' '' ''.
  ADD_FIELDCAT 'AUFNR' '生产订单' '' '' ''.
  ADD_FIELDCAT 'OPDAT' '操作日期(服务器)' '' '' ''.
  ADD_FIELDCAT 'OPTIM' '操作时间(服务器)' '' '' ''.
  ADD_FIELDCAT 'OPNUM' '操作流水号(本次)' '' '' ''.
  ADD_FIELDCAT 'OPTYP' '更新标志' '' '' ''.
  ADD_FIELDCAT 'OBJTP' '操作对象类别描述' '' '' ''.
  ADD_FIELDCAT 'OBJNM' '操作对象描述' '' '' ''.
  ADD_FIELDCAT 'OBJLU' '操作对象行标识（唯一）' '' '' ''.
  ADD_FIELDCAT 'OBJLA' '操作对象行标识（辅助）' '' '' ''.
  ADD_FIELDCAT 'VLOLD' '操作对象原值' '' '' ''.
  ADD_FIELDCAT 'VLNEW' '作对象新值' '' '' ''.
  ADD_FIELDCAT 'VLNE2' '操作对象新增（补充）' '' '' ''.
  ADD_FIELDCAT 'TCODE' '事务代码' '' '' ''.
  ADD_FIELDCAT 'OPUSR' '操作SAP用户名' '' '' ''.
  ADD_FIELDCAT 'OPIPA' '操作客户端IP地址' '' '' ''.
  ADD_FIELDCAT 'OPHOS' '操作客户端主机名' '' '' ''.
ENDFORM.                    " INIT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT_ALV .
*  ALV输出
  G_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT[]
      I_SAVE                   = 'X'
      I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
*     i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*     is_variant               = v_stru_disvar
*     it_events                = git_events
*     it_sort_lvc              = it_sort[]
*     is_print_lvc             = gs_print
    TABLES
      T_OUTTAB                 = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_OUTPUT_ALV

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD' .
ENDFORM.                    "ALV_PF_STATUS
