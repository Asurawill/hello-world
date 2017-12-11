*----------------------------------------------------------------------*
*  程序名称         : ZHRPA03
*  创建者           : 吴丽娟
*  创建日期         : 2015-08-21
*----------------------------------------------------------------------*
*  概要说明
* 技术职称表
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-08-21  HANDYWLJ        ED1K902509   创建
*
*----------------------------------------------------------------------*
REPORT zhrpa03.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
TABLES:pa9024,pa0000,pa9025.
TABLES:pernr.
NODES:person,group,peras.

INFOTYPES:0000 NAME p0000,
          0001 NAME p9001,
          9024 NAME p9024.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tab,
         xh      TYPE int_4,
         pernr   TYPE pa0000-pernr,   "工号
         ename   TYPE pa0001-ename,   "姓名
         orgtx   TYPE t527x-orgtx,   "最小部门
         stext   TYPE hrp1000-stext,   "职位
         zzzcmc  TYPE p9025-zzzcmc,   "职位名称
         zzzcmc2 TYPE p9025-zzzcmc,   "职位名称
         zzzcjb  TYPE p9025-zzzcjb,   "职位级别
         begda   TYPE p9025-begda,    "证件生效日期
         endda   TYPE p9025-endda,    "证件失效日期
         zzpsjg  TYPE p9025-zzpsjg,   "评审机构
         zzzcss  TYPE p9025-zzzcss,   "职称所属
         zzzcss2 TYPE p9025-zzzcss,   "职称所属
         zzhrbz  TYPE pa9024-zzhrbz,   "备注
         plans   TYPE pa0001-plans,
         orgeh   TYPE pa0001-orgeh,
         massn   TYPE pa0000-massn,
       END OF ty_tab.
TYPES: BEGIN OF ty_orgeh,
         orgeh TYPE pa0001-orgeh,
       END OF ty_orgeh.
TYPES:BEGIN OF ty_plans,
        pernr TYPE pa0000-pernr,
        plans TYPE pa0001-plans,
      END OF ty_plans.
TYPES:BEGIN OF ty_pa9025,
        pernr  TYPE pa9024-pernr,
        zzzcjb TYPE pa9025-zzzcjb,
        begda  TYPE pa9025-begda,
        endda  TYPE pa9025-endda,
        zzpsjg TYPE pa9025-zzpsjg,
        zzzcss TYPE pa9025-zzzcss,
        zzhrbz TYPE pa9025-zzhrbz,
      END OF ty_pa9025.
TYPES:BEGIN OF ty_pa9024,
        pernr  TYPE pa9025-pernr,
        zzzcmc TYPE pa9025-zzzcmc,
      END OF ty_pa9024.
TYPES:BEGIN OF ty_orgtx,
        orgeh TYPE t527x-orgtx,
        orgtx TYPE t527x-orgtx,
      END OF ty_orgtx.
TYPES:BEGIN OF ty_stext,
        objid TYPE hrp1000-objid,
        stext TYPE hrp1000-stext,
      END OF ty_stext.
TYPES:BEGIN OF ty_zzhrbz,
        pernr  TYPE pa9024-pernr,
        zzhrbz TYPE pa9024-zzhrbz,
      END OF ty_zzhrbz.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_tab TYPE TABLE OF ty_tab,
     wa_tab TYPE ty_tab.

DATA:it_orgeh TYPE TABLE OF ty_orgeh,
     wa_orgeh TYPE ty_orgeh.

DATA:it_plans TYPE TABLE OF ty_plans,
     wa_plans TYPE ty_plans.

DATA:it_orgtx TYPE TABLE OF ty_orgtx,
     wa_orgtx TYPE ty_orgtx.

DATA:it_stext TYPE TABLE OF ty_stext,
     wa_stext TYPE ty_stext.

DATA:it_zzhrbz TYPE TABLE OF ty_zzhrbz,
     wa_zzhrbz TYPE ty_zzhrbz.

DATA:it_pa9025 TYPE TABLE OF ty_pa9025,
     wa_pa9025 TYPE ty_pa9025.

DATA:it_pa9024 TYPE TABLE OF ty_pa9024,
     wa_pa9024 TYPE ty_pa9024.

DATA:it_dd07t TYPE TABLE OF dd07t,
     wa_dd07t TYPE dd07t.

DATA:it_dd07t2 TYPE TABLE OF dd07t,
     wa_dd07t2 TYPE dd07t.

DATA:it_pa0001 TYPE TABLE OF pa0001,
     wa_pa0001 TYPE pa0001.

DATA:it_pa0000 TYPE TABLE OF pa0000,
     wa_pa0000 TYPE pa0000.

DATA:l_xh TYPE int_4 VALUE 1.
*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo.
*----------------------------------------------------------------------*
*                  定义宏
*----------------------------------------------------------------------*
DEFINE init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
PARAMETERS:p_datum  TYPE syst-datum OBLIGATORY,
           p_zzzcss TYPE p9025-zzzcss.
SELECT-OPTIONS:s_zzzcmc FOR pa9025-zzzcmc NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK text.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  GET peras .
  PERFORM frm_getdata.
  PERFORM frm_dealdata.

END-OF-SELECTION.
  PERFORM frm_check.
  PERFORM frm_layout.
  PERFORM frm_fieldcat.
  PERFORM frm_output.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_getdata .
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_pa9024
  FROM pa9025
  WHERE pernr = peras-pernr
  AND pa9025~begda <= p_datum
  AND pa9025~endda >= p_datum.

  SORT it_pa9024 BY zzzcmc.

  DELETE it_pa9024 WHERE  zzzcmc  NOT IN s_zzzcmc.

  IF it_pa9024 IS NOT INITIAL.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_pa9025
    FROM pa9025
    FOR ALL ENTRIES IN it_pa9024
    WHERE pernr = it_pa9024-pernr.
    IF it_pa9025 IS NOT INITIAL.
*当发现离职状态删除员工编号
      SELECT *
      FROM pa0000
      INTO CORRESPONDING FIELDS OF TABLE it_pa0000
      FOR ALL ENTRIES IN it_pa9025
      WHERE pernr = it_pa9025-pernr.

      IF it_pa0000 IS NOT INITIAL.

        SELECT * FROM pa0001
        INTO CORRESPONDING FIELDS OF TABLE it_pa0001
        FOR ALL ENTRIES IN it_pa9025
        WHERE pernr = it_pa9025-pernr.

      ENDIF.

      IF it_pa0001 IS NOT INITIAL.
        SELECT objid stext
          INTO CORRESPONDING FIELDS OF TABLE it_stext
          FROM hrp1000
          FOR ALL ENTRIES IN it_pa0001
          WHERE objid = it_pa0001-plans
          AND stext <> '离职'.
*职位描述
        SELECT orgeh orgtx
        INTO CORRESPONDING FIELDS OF TABLE it_orgtx
        FROM t527x
        FOR ALL ENTRIES IN it_orgeh
        WHERE orgeh = it_orgeh-orgeh
        AND   sprsl = sy-langu.
      ENDIF.

*取出证件名称值对应描述
      SELECT * FROM dd07t
      INTO CORRESPONDING FIELDS OF TABLE it_dd07t
      WHERE domname    = 'ZDOZJMC'
      AND   ddlanguage = sy-langu.

      SELECT * FROM dd07t
      INTO CORRESPONDING FIELDS OF TABLE it_dd07t2
      WHERE domname    = 'ZDOZCSS'
      AND   ddlanguage = sy-langu.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_dealdata .
  SORT it_pa9025  BY pernr.
  SORT it_pa0001  BY pernr.
  SORT it_pa0000  BY pernr.
  LOOP AT it_pa9024 INTO wa_pa9024.

    READ TABLE it_pa0000 INTO wa_pa0000
    WITH KEY pernr  = wa_pa9024-pernr
             massn  = 'Z6'
             BINARY SEARCH .
    IF  sy-subrc = 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_pa9025 INTO wa_pa9025 WITH KEY pernr = wa_pa9024-pernr.
    IF sy-subrc = 0.
      wa_tab-pernr  = wa_pa9025-pernr.
      wa_tab-zzzcjb = wa_pa9025-zzzcjb.
      wa_tab-begda  = wa_pa9025-begda.
      wa_tab-endda  = wa_pa9025-endda.
      wa_tab-zzpsjg = wa_pa9025-zzpsjg.
      wa_tab-zzhrbz = wa_pa9025-zzhrbz.
      wa_tab-zzzcss2 = wa_pa9025-zzzcss.
    ELSE.
      CONTINUE.
    ENDIF.

*    IF wa_pa9024-zzzjmc IS INITIAL.
*      wa_tab-zzzcmc = wa_pa9024-zzqtmc.
*    ELSE.
*      wa_tab-zzzcmc2 = wa_pa9024-zzzjmc.
*      READ TABLE it_dd07t INTO wa_dd07t WITH KEY valpos = wa_tab-zzzcmc2.
*      IF sy-subrc = 0.
*        wa_tab-zzzcmc = wa_dd07t-ddtext.
*      ENDIF.
*    ENDIF.

    wa_tab-zzzcmc = wa_pa9024-zzzcmc.

    IF wa_tab-zzzcss2 IS NOT INITIAL.
      READ TABLE it_dd07t2 INTO wa_dd07t2 WITH KEY valpos = wa_tab-zzzcss2.
      IF sy-subrc = 0.
        wa_tab-zzzcss = wa_dd07t2-ddtext.
      ENDIF.
    ENDIF.
*最小部门，姓名
    READ TABLE it_pa0001 INTO wa_pa0001
    WITH KEY pernr = wa_pa9025-pernr
             BINARY SEARCH.
    IF  sy-subrc = 0.
      wa_tab-ename = wa_pa0001-ename.
      wa_tab-orgeh = wa_pa0001-orgeh.
      wa_tab-plans = wa_pa0001-plans.
      CONDENSE wa_tab-ename NO-GAPS.
    ENDIF.

*最小部门描述
    READ TABLE it_orgtx INTO wa_orgtx
    WITH KEY  orgeh = wa_tab-orgeh.
    IF sy-subrc = 0.
      wa_tab-orgtx = wa_orgtx-orgtx.
    ENDIF.

    SORT it_stext BY objid.
    LOOP AT it_stext INTO wa_stext.
      AT END OF objid.
        READ TABLE it_stext INTO wa_stext WITH KEY objid = wa_tab-plans.
        IF sy-subrc = 0.
          IF wa_stext-stext <> '离职' AND wa_stext-objid = wa_tab-plans.
            wa_tab-stext = wa_stext-stext.
          ENDIF.
        ENDIF.
      ENDAT.
    ENDLOOP.
    wa_tab-xh = l_xh.

    l_xh = l_xh + 1.
    APPEND  wa_tab TO it_tab.
    CLEAR   wa_tab.
  ENDLOOP.
  SORT it_tab BY pernr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_layout .
  wa_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  init_fieldcat 'XH' '序号' '' ''.
  init_fieldcat 'PERNR' '工号' 'PA0000' 'PERNR'.
  init_fieldcat 'ENAME' '姓名' 'PA0000' 'ENAME'.
  init_fieldcat 'ORGTX' '最小部门' 'T527X' 'ORGTX'.
  init_fieldcat 'STEXT' '职位' 'HRP1000' 'STEXT'.
  init_fieldcat 'ZZZCMC' '' 'P9025' 'ZZZCMC'.
  init_fieldcat 'ZZZCJB' '' 'P9025' 'ZZZCJB'.
  init_fieldcat 'BEGDA' '' 'P9025' 'BEGDA'.
  init_fieldcat 'ENDDA' '' 'P9025' 'ENDDA'.
  init_fieldcat 'ZZPSJG' '' 'P9025' 'ZZPSJG'.
  init_fieldcat 'ZZZCSS' '' 'P9025' 'ZZZCSS'.
  init_fieldcat 'ZZHRBZ' '' 'PA9024' 'ZZHRBZ'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_output .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_tab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
*  DATA lw_extab TYPE slis_extab.
*  lw_extab-fcode = '&ALL'.
*  APPEND lw_extab TO rt_extab.
*  lw_extab-fcode = '&SAL'.
*  APPEND lw_extab TO rt_extab.
  DELETE rt_extab WHERE fcode = '&ALL'.
  DELETE rt_extab WHERE fcode = '&SAL'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_check .
  IF it_tab IS INITIAL.
    MESSAGE '没有数据！' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
