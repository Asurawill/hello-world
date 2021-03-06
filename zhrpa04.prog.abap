*----------------------------------------------------------------------*
*  程序名称         : ZHRPA04
*  创建者           : 吴丽娟
*  创建日期         : 2015-08-24
*----------------------------------------------------------------------*
*  概要说明
* 上岗资格证
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-08-24  HANDYWLJ       ED1K902517  创建
*
*----------------------------------------------------------------------*
REPORT zhrpa04.
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
TABLES:ps9021,pa0000.
TABLES:pernr.
NODES:person,group,peras.
INFOTYPES:0000 NAME p0000,
          0001 NAME p9001,
          9024 NAME p9024.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tab,
         xh     TYPE int_4,
         pernr  TYPE pa0000-pernr,   "工号
         ename  TYPE pa0001-ename,   "姓名
         orgtx  TYPE t527x-orgtx,   "最小部门
         stext  TYPE hrp1000-stext,   "职位
         zzjsgx TYPE ps9021-zzjsgx,   "家属关系
         zzgxsm TYPE ps9021-zzgxsm,   "具体关系备注
         zzjsxm TYPE ps9021-zzjsxm,   "家属姓名
         zzsfh  TYPE ps9021-zzsfh,   "身份证号
         zzcsrq TYPE ps9021-zzcsrq,   "出生日期
         zzxb   TYPE ps9021-zzxb,    "性别
         zzhksd TYPE ps9021-zzhksd,   "户口所在地
         zzlxfs TYPE ps9021-zzlxfs,   "联系方式
         zzgzdw TYPE ps9021-zzgzdw,   "工作单位
         zzgzbm TYPE ps9021-zzgzbm,   "工作部门
         zzzw   TYPE ps9021-zzzw,   "职位
         zzsyjt TYPE ps9021-zzsyjt,   "是否属于本集团
         zzsygs TYPE ps9021-zzsygs,   "是否属于本公司
         zzhrbz TYPE ps9021-zzhrbz,   "备注
         plans  TYPE pa0001-plans,
         orgeh  TYPE pa0001-orgeh,
       END OF ty_tab.
TYPES: BEGIN OF ty_orgeh,
         orgeh TYPE pa0001-orgeh,
       END OF ty_orgeh.
TYPES:BEGIN OF ty_orgtx,
        orgtx TYPE t527x-orgtx,
        orgeh TYPE t527x-orgeh,
      END OF ty_orgtx.
TYPES:BEGIN OF ty_plans,
        pernr TYPE pa0000-pernr,
        plans TYPE pa0001-plans,
      END OF ty_plans.
TYPES:BEGIN OF ty_stext,
        objid TYPE hrp1000-objid,
        stext TYPE hrp1000-stext,
      END OF ty_stext.
TYPES:BEGIN OF ty_pa9021,
        pernr  TYPE pa9021-pernr,
        zzjsgx TYPE pa9021-zzjsgx,   "家属关系
        zzgxsm TYPE pa9021-zzgxsm,   "具体关系备注
        zzjsxm TYPE pa9021-zzjsxm,   "家属姓名
        zzsfh  TYPE pa9021-zzsfh,   "身份证号
        zzcsrq TYPE pa9021-zzcsrq,   "出生日期
        zzxb   TYPE pa9021-zzxb,    "性别
        zzhksd TYPE pa9021-zzhksd,   "户口所在地
        zzlxfs TYPE pa9021-zzlxfs,   "联系方式
        zzgzdw TYPE pa9021-zzgzdw,   "工作单位
        zzgzbm TYPE pa9021-zzgzbm,   "工作部门
        zzzw   TYPE pa9021-zzzw,   "职位
        zzsyjt TYPE pa9021-zzsyjt,   "是否属于本集团
        zzsygs TYPE pa9021-zzsygs,   "是否属于本公司
        zzhrbz TYPE pa9021-zzhrbz,   "备注
      END OF ty_pa9021.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_tab TYPE TABLE OF ty_tab,
     wa_tab TYPE ty_tab.
DATA:it_orgeh TYPE TABLE OF ty_orgeh,
     wa_orgeh TYPE ty_orgeh.
DATA:it_orgtx TYPE TABLE OF ty_orgtx,
     wa_orgtx TYPE ty_orgtx.
DATA:it_plans TYPE TABLE OF ty_plans,
     wa_plans TYPE ty_plans.
DATA:it_stext TYPE TABLE OF ty_stext,
     wa_stext TYPE ty_stext.
DATA:it_pa9021 TYPE TABLE OF ty_pa9021,
     wa_pa9021 TYPE ty_pa9021.
DATA:it_dd07t TYPE TABLE OF dd07t,
     wa_dd07t TYPE dd07t.
DATA:it_dd07t2 TYPE TABLE OF dd07t,
     wa_dd07t2 TYPE dd07t.
DATA:it_dd07t3 TYPE TABLE OF dd07t,
     wa_dd07t3 TYPE dd07t.
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
PARAMETERS:p_datum  TYPE syst-datum OBLIGATORY.
SELECT-OPTIONS:s_zzjsgx FOR ps9021-zzjsgx NO-EXTENSION NO INTERVALS.
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
*取证件名称
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_pa9021
  FROM pa9021
  WHERE pernr = peras-pernr
  AND pa9021~begda <= p_datum
  AND pa9021~endda >= p_datum.

  SORT it_pa9021 BY zzjsgx.

  DELETE it_pa9021 WHERE zzjsgx NOT IN s_zzjsgx.

  IF it_pa9021 IS NOT INITIAL.
*当发现离职状态删除员工编号
    SELECT *
    FROM pa0000
    INTO CORRESPONDING FIELDS OF TABLE it_pa0000
    FOR ALL ENTRIES IN it_pa9021
    WHERE pernr = it_pa9021-pernr.

    SELECT * FROM pa0001
    INTO CORRESPONDING FIELDS OF TABLE it_pa0001
    FOR ALL ENTRIES IN it_pa9021
    WHERE pernr = it_pa9021-pernr
      AND begda <= p_datum
      AND endda >= p_datum.

    IF it_pa0001 IS NOT INITIAL.
      SELECT objid stext
        INTO CORRESPONDING FIELDS OF TABLE it_stext
        FROM hrp1000
        FOR ALL ENTRIES IN it_pa0001
        WHERE objid = it_pa0001-plans
        AND   otype = 'S'
        AND   begda <= p_datum
        AND   endda >= p_datum
      AND stext <> '离职'.
      LOOP AT it_pa0001 INTO wa_pa0001.
        wa_orgeh-orgeh = wa_pa0001-orgeh.
        APPEND wa_orgeh TO it_orgeh.
      ENDLOOP.
*职位描述
      SELECT orgeh orgtx
      INTO CORRESPONDING FIELDS OF TABLE it_orgtx
      FROM t527x
      FOR ALL ENTRIES IN it_orgeh
      WHERE orgeh = it_orgeh-orgeh
      AND   begda <= p_datum
      AND   endda >= p_datum
      AND   sprsl = sy-langu.
    ENDIF.
*取出证件名称值对应描述
    SELECT * FROM dd07t
    INTO CORRESPONDING FIELDS OF TABLE it_dd07t
    WHERE domname    = 'ZDOJSGX'
    AND   ddlanguage = sy-langu.

    SELECT * FROM dd07t
    INTO CORRESPONDING FIELDS OF TABLE it_dd07t2
    WHERE domname    = 'ZDOXB'
    AND   ddlanguage = sy-langu.

*    select * from dd07t
*    into corresponding fields of table it_dd07t3
*    where domname    = 'ZDOHKSD'
*    and   ddlanguage = sy-langu.
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

  SORT it_pa9021  BY pernr.
  SORT it_pa0001  BY pernr.
  SORT it_pa0000  BY pernr.
  DELETE ADJACENT DUPLICATES FROM it_pa9021 COMPARING ALL FIELDS.
  LOOP AT it_pa9021 INTO wa_pa9021.

    READ TABLE it_pa0000 INTO wa_pa0000
    WITH KEY pernr  = wa_pa9021-pernr
             massn  = 'Z6'
             BINARY SEARCH .
    IF  sy-subrc = 0.
      CONTINUE.
    ENDIF.

    IF wa_pa9021-zzjsgx IS NOT INITIAL.
      READ TABLE it_dd07t INTO wa_dd07t
     WITH KEY valpos = wa_pa9021-zzjsgx.
      IF sy-subrc = 0.
        wa_tab-zzjsgx = wa_dd07t-ddtext.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
    wa_tab-pernr = wa_pa9021-pernr.
    wa_tab-zzgxsm = wa_pa9021-zzgxsm.
    wa_tab-zzjsxm = wa_pa9021-zzjsxm.
    wa_tab-zzsfh = wa_pa9021-zzsfh.
    wa_tab-zzcsrq = wa_pa9021-zzcsrq.
    wa_tab-zzhksd = wa_pa9021-zzhksd.
    wa_tab-zzlxfs = wa_pa9021-zzlxfs.
    wa_tab-zzgzdw = wa_pa9021-zzgzdw.
    wa_tab-zzgzbm = wa_pa9021-zzgzbm.
    wa_tab-zzzw = wa_pa9021-zzzw.
    wa_tab-zzhrbz = wa_pa9021-zzhrbz.
    IF wa_pa9021-zzxb IS NOT INITIAL.
      READ TABLE it_dd07t2 INTO wa_dd07t2
     WITH KEY valpos = wa_pa9021-zzxb.
      IF sy-subrc = 0.
        wa_tab-zzxb = wa_dd07t2-ddtext.
      ENDIF.
    ENDIF.
*    if wa_pa9021-zzhksd is not initial.
*      read table it_dd07t3 into wa_dd07t3
*     with key valpos = wa_pa9021-zzhksd.
*      if sy-subrc = 0.
*        wa_tab-zzhksd = wa_dd07t3-ddtext.
*      endif.
*    endif.
    IF wa_pa9021-zzsyjt = 'X'.
      wa_tab-zzsyjt = '是'.
    ELSE.
      wa_tab-zzsyjt = ''.
    ENDIF.
    IF wa_pa9021-zzsygs = 'X'.
      wa_tab-zzsygs = '是'.
    ELSE.
      wa_tab-zzsygs = ''.
    ENDIF.

*最小部门，姓名
    READ TABLE it_pa0001 INTO wa_pa0001
    WITH KEY pernr = wa_pa9021-pernr
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
  init_fieldcat 'ZZJSGX' '家属关系' 'PS9021' 'ZZJSGX'.
  init_fieldcat 'ZZGXSM' '具体关系说明' 'PS9021' 'ZZGXSM'.
  init_fieldcat 'ZZJSXM' '家属姓名' 'PS9021' 'ZZJSXM'.
  init_fieldcat 'ZZSFH' '身份证号' 'PS9021' 'ZZSFH'.
  init_fieldcat 'ZZCSRQ' '出生日期' 'PS9021' 'ZZCSRQ'.
  init_fieldcat 'ZZXB' '性别' 'PS9021' 'ZZXB'.
  init_fieldcat 'ZZHKSD' '户口所在地' 'PS9021' 'ZZHKSD'.
  init_fieldcat 'ZZLXFS' '联系方式' 'PS9021' 'ZZLXFS'.
  init_fieldcat 'ZZGZDW' '工作单位' 'PS9021' 'ZZGZDW'.
  init_fieldcat 'ZZGZBM' '工作部门' 'PS9021' 'ZZGZBM'.
  init_fieldcat 'ZZZW' '职位' 'PS9021' 'ZZZW'.
  init_fieldcat 'ZZSYJT' '是否属于本集团' 'PS9021' 'ZZSYJT'.
  init_fieldcat 'ZZSYGS' '是否属于本公司' 'PS9021' 'ZZSYGS'.
  init_fieldcat 'ZZHRBZ' '备注' 'PS9021' 'ZZHRBZ'.
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
