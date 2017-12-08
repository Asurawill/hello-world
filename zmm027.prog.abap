*----------------------------------------------------------------------*
*  程序名称         :ZMM027
*  创建者           : 吴丽娟
*  创建日期         :20151027
*----------------------------------------------------------------------*
*  概要说明
* 整备库项目库存报表
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
* 20151027   HANDYWLJ        ED1K903365     创建
*
*----------------------------------------------------------------------*
"add 3012库存地 20160121

REPORT zmm027.
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
TABLES:mseg,vbak .
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*

TYPES:BEGIN OF ty_data,
        mblnr     TYPE zmm026-mblnr,
        zeile     TYPE zmm026-zeile,
        werks     TYPE mseg-werks,
        lgort     TYPE mseg-lgort,
        matnr     TYPE mseg-matnr,
        maktx     TYPE makt-maktx,
        mat_kdauf TYPE mseg-mat_kdauf,
        xmms(100) TYPE c,
        menge     TYPE mseg-menge,
        meins     TYPE mara-meins,
        ablad     TYPE mseg-ablad,
        vbeln_im  TYPE mseg-vbeln_im,
        vbelp_im  TYPE mseg-vbelp_im,
        shkzg     TYPE mseg-shkzg,
        statu(1)  TYPE c,
      END OF ty_data.
TYPES:BEGIN OF ty_maktx,         "取物料描述的表
        maktx TYPE makt-maktx,
        matnr TYPE mseg-matnr,
      END OF ty_maktx.
TYPES:BEGIN OF ty_vbeln,
        vgbel TYPE lips-vgbel,
        vgpos TYPE lips-vgpos,
      END OF ty_vbeln.
TYPES:BEGIN OF ty_vgbel,
        vgbel TYPE lips-vgbel,
        vgpos TYPE lips-vgpos,
      END OF ty_vgbel.
TYPES:BEGIN OF ty_vbeln1,
        vbeln TYPE ekkn-vbeln,
        vbelp TYPE ekkn-vbelp,
      END OF ty_vbeln1.
TYPES:BEGIN OF ty_meins,
        matnr TYPE mara-matnr,
        meins TYPE mara-meins,
      END OF ty_meins.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_data TYPE TABLE OF ty_data,
     wa_data TYPE ty_data.

DATA:it_data1 TYPE TABLE OF zmm027,
     wa_data1 TYPE zmm027.

DATA:it_data3 TYPE TABLE OF zmm027,
     wa_data3 TYPE zmm027.

DATA:it_data2 TYPE TABLE OF ty_data,
     wa_data2 TYPE ty_data.

DATA:it_maktx TYPE TABLE OF ty_maktx,
     wa_maktx TYPE ty_maktx.

DATA:it_vbeln TYPE TABLE OF ty_vbeln,
     wa_vbeln TYPE ty_vbeln.

DATA:it_vgbel TYPE TABLE OF ty_vgbel,
     wa_vgbel TYPE ty_vgbel.

DATA:it_vbeln1 TYPE TABLE OF ty_vbeln1,
     wa_vbeln1 TYPE ty_vbeln1.

DATA:it_meins TYPE TABLE OF ty_meins,
     wa_meins TYPE ty_meins.

DATA:it_zmm027 TYPE TABLE OF zmm027,
     wa_zmm027 TYPE zmm027.

DATA:it_zmm027a TYPE TABLE OF zmm027,
     wa_zmm027a TYPE zmm027.
*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo,

     it_events   TYPE slis_t_event,
     wa_events   LIKE LINE OF it_events.
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
SELECT-OPTIONS:s_werks      FOR mseg-werks,"工厂
               s_lgort      FOR mseg-lgort,"库存地点
               s_matnr      FOR mseg-matnr,"物料编码
               s_kdauf      FOR vbak-vbeln."销售订单
PARAMETERS:p_fxk AS CHECKBOX DEFAULT 'X'."仅显示非零库存行
SELECTION-SCREEN END OF BLOCK text.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.
MESSAGE '整备库台账维护及查询请执行新事务码：ZMM035' TYPE 'S' DISPLAY LIKE 'E'..

*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_getdata.
  PERFORM frm_dealdata.
  PERFORM frm_layout.
  PERFORM frm_fieldcat.
  PERFORM frm_output.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_getdata .
*取MSEG表中的所有数据
  SELECT *
    FROM mseg
    INTO CORRESPONDING FIELDS OF TABLE it_data
    WHERE werks      IN s_werks
    AND matnr      IN s_matnr
    AND mat_kdauf  IN s_kdauf
    AND ( werks = 1500 OR werks = 1100 )
    AND budat_mkpf >= '20151101'
    AND ( lgort = '3002'
     OR lgort = '3004'
     OR lgort = '3006'
     OR lgort = '3003'
     OR lgort = '3012')
    AND sobkz <> 'F'.

  DELETE it_data WHERE  lgort NOT IN s_lgort .

  IF it_data IS NOT INITIAL.
*物料描述
    SELECT makt~maktx
           mseg~matnr
      FROM makt
      JOIN mseg ON makt~matnr = mseg~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_maktx
      FOR ALL ENTRIES IN it_data
      WHERE mseg~matnr = it_data-matnr
      AND makt~spras = sy-langu.

*取单位
    SELECT matnr meins
      FROM mara
      INTO CORRESPONDING FIELDS OF TABLE it_meins
      FOR ALL ENTRIES IN it_data
      WHERE matnr = it_data-matnr.

  ENDIF.
  SELECT *
    FROM zmm027
    INTO CORRESPONDING FIELDS OF TABLE it_zmm027
    WHERE werks IN s_werks
    AND  lgort IN s_lgort
    AND matnr IN s_matnr
    AND mat_kdauf IN s_kdauf
    AND  ( werks = 1500 OR werks = 1100 )
    AND ( lgort = '3002'
     OR lgort = '3004'
     OR lgort = '3006'
     OR lgort = '3003'
     OR lgort = '3012' ).
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

  SELECT *
    FROM zmm026 INTO CORRESPONDING FIELDS OF TABLE it_data2.

  LOOP AT it_data INTO wa_data.

*数据筛选逻辑
*    IF wa_data-werks = '1500' AND wa_data-lgort <> '3002'.
*      CONTINUE.
*    ENDIF.

    IF wa_data-werks = '1500' AND ( wa_data-lgort <> '3002' AND wa_data-lgort <> '3003'  ) .
      CONTINUE.
    ENDIF.

    IF wa_data-werks = '1100' AND ( wa_data-lgort <> '3002' AND wa_data-lgort <> '3004'  AND wa_data-lgort <> '3006' AND wa_data-lgort <> '3012') .
      CONTINUE.
    ENDIF.

*状态标识
    wa_data-statu = 'X'.

**物料描述
*    READ TABLE it_maktx INTO wa_maktx WITH KEY matnr = wa_data-matnr.
*    IF sy-subrc = 0.
*      wa_data-maktx = wa_maktx-maktx.
*    ENDIF.

*单位
    READ TABLE it_meins INTO wa_meins WITH KEY matnr = wa_data-matnr.
    IF sy-subrc = 0.
      wa_data-meins = wa_meins-meins.
    ENDIF.

*销售订单和销售订单行号
    REFRESH:it_vbeln,it_vgbel,it_vbeln1.
    IF wa_data-mat_kdauf IS INITIAL. "如果MAT_KDAUF不是空，那么销售订单就等于MAT_KDAUF的值，行号就是MAT_KAPOS。
      IF wa_data-ablad IS NOT INITIAL."如果MAT_KDAUF是空，ABLAD不是空，那么销售订单就等于ABLAD。
        CONDENSE wa_data-ablad NO-GAPS.
        TRANSLATE wa_data-ablad TO UPPER CASE.
        wa_data-mat_kdauf = wa_data-ablad.
      ELSE.                             "如果ABLAD是空的话，
        IF wa_data-vbeln_im IS NOT INITIAL.    "如果VBELN_IM不是空，那么在表LIPS中取值
          SELECT vgbel
                 vgpos
            FROM lips
            INTO CORRESPONDING FIELDS OF TABLE it_vbeln
            WHERE vbeln = wa_data-vbeln_im
            AND posnr = wa_data-vbelp_im
            AND ( vgtyp = 'C' OR vgtyp = 'I' OR vgtyp = 'H' ).

          IF it_vbeln IS NOT INITIAL."如果取出的值不是空的，那么销售订单和行号就在LIPS取。
            READ TABLE it_vbeln INTO wa_vbeln INDEX 1.
            IF sy-subrc = 0.
              wa_data-mat_kdauf = wa_vbeln-vgbel.
            ENDIF.
          ELSE.
            SELECT vgbel                "如果取出的值是空的，那么取VGTYP为V的销售订单和行号。
                   vgpos
              FROM lips
              INTO CORRESPONDING FIELDS OF TABLE it_vgbel
              WHERE vbeln = wa_data-vbeln_im
              AND posnr = wa_data-vbelp_im
              AND vgtyp = 'V'.
            IF it_vgbel IS NOT INITIAL.
              READ TABLE it_vgbel INTO wa_vgbel INDEX 1.  "如果V类型的不为空，那么根据取出的销售订单和行号在表EKKN中取出相应的销售订单和行号。
              IF sy-subrc = 0.
                SELECT vbeln
                       vbelp
                  FROM ekkn
                  INTO CORRESPONDING FIELDS OF TABLE it_vbeln1
                  WHERE ebeln = wa_vgbel-vgbel
                  AND ebelp = wa_vgbel-vgpos.
                IF it_vbeln1 IS NOT INITIAL.
                  READ TABLE it_vbeln1 INTO wa_vbeln1 INDEX 1.
                  IF sy-subrc = 0.
                    wa_data-mat_kdauf = wa_vbeln1-vbeln.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**取项目描述
*    DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
*    CLEAR t_tline[].
*    DATA:tname TYPE thead-tdname.
*    CONDENSE wa_data-mat_kdauf NO-GAPS.
*    TRANSLATE wa_data-mat_kdauf TO UPPER CASE.
*    tname = wa_data-mat_kdauf.
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
**       CLIENT                  = SY-MANDT
*        id                      = 'Z001'
*        language                = sy-langu
*        name                    = tname
*        object                  = 'VBBK'
**       ARCHIVE_HANDLE          = 0
**       LOCAL_CAT               = ' '
**   IMPORTING
**       HEADER                  =
**       OLD_LINE_COUNTER        =
*      TABLES
*        lines                   = t_tline[]
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*    IF t_tline[] IS NOT INITIAL.
*      LOOP AT t_tline.
*        CONCATENATE wa_data-xmms t_tline-tdline INTO wa_data-xmms.
*        CLEAR t_tline.
*      ENDLOOP.
*    ENDIF.

*数量
    IF wa_data-shkzg = 'H'.
      wa_data-menge = - wa_data-menge.
    ENDIF.

    LOOP AT it_data2 INTO wa_data2 WHERE mblnr = wa_data-mblnr
                                    AND zeile = wa_data-zeile
                                    AND mat_kdauf <> wa_data-mat_kdauf.


      wa_data-mat_kdauf = wa_data2-mat_kdauf.
      CONDENSE wa_data-mat_kdauf NO-GAPS.
      TRANSLATE wa_data-mat_kdauf TO UPPER CASE.
      wa_data-xmms = wa_data2-xmms.

    ENDLOOP.

    MODIFY it_data FROM wa_data.

    CLEAR wa_data.

  ENDLOOP.

  DELETE it_data WHERE statu = ''.

  SORT it_data BY werks lgort matnr mat_kdauf.

  IF s_kdauf IS NOT INITIAL.
    IF s_kdauf-option = 'EQ'.
      DELETE it_data WHERE mat_kdauf <> s_kdauf-low.
    ELSEIF s_kdauf-option = 'BT'.
      DELETE it_data WHERE mat_kdauf NOT BETWEEN s_kdauf-low AND s_kdauf-high.
    ENDIF.
  ENDIF.

  SORT it_data BY werks lgort matnr mat_kdauf.

  LOOP AT it_data INTO wa_data.
    CLEAR wa_data1.
    wa_data1-werks = wa_data-werks.
    wa_data1-lgort = wa_data-lgort.
    wa_data1-matnr = wa_data-matnr.
    "   wa_data1-maktx = wa_data-maktx.
    wa_data1-mat_kdauf = wa_data-mat_kdauf.
    CONDENSE wa_data1-mat_kdauf NO-GAPS.
    TRANSLATE wa_data1-mat_kdauf TO UPPER CASE.
    "   wa_data1-xmms = wa_data-xmms.
    wa_data1-menge = wa_data-menge.
    wa_data1-meins = wa_data-meins.

    COLLECT wa_data1 INTO it_data1.
  ENDLOOP.

  SORT it_zmm027 BY werks lgort matnr mat_kdauf.

  LOOP AT it_zmm027 INTO wa_zmm027.
    CLEAR wa_zmm027a.
    wa_zmm027a-werks = wa_zmm027-werks.
    wa_zmm027a-lgort = wa_zmm027-lgort.
    wa_zmm027a-matnr = wa_zmm027-matnr.
    "   wa_zmm027a-maktx = wa_zmm027-maktx.
    wa_zmm027a-mat_kdauf = wa_zmm027-mat_kdauf.
    CONDENSE wa_zmm027a-mat_kdauf NO-GAPS.
    TRANSLATE wa_zmm027a-mat_kdauf TO UPPER CASE.
    "  wa_zmm027a-xmms = wa_zmm027-xmms.
    wa_zmm027a-menge = wa_zmm027-menge.
    wa_zmm027a-meins = wa_zmm027-meins.
    COLLECT wa_zmm027a INTO it_zmm027a.
  ENDLOOP.

  SORT it_zmm027a BY werks lgort matnr mat_kdauf.

  LOOP AT it_zmm027a INTO wa_zmm027a.
    APPEND wa_zmm027a TO it_data1.
  ENDLOOP.


  SORT it_data1 BY werks lgort matnr mat_kdauf.

  LOOP AT it_data1 INTO wa_data1.
    CLEAR wa_data3.
    wa_data3-werks = wa_data1-werks.
    wa_data3-lgort = wa_data1-lgort.
    wa_data3-matnr = wa_data1-matnr.
    " wa_data3-maktx = wa_data1-maktx.
    wa_data3-mat_kdauf = wa_data1-mat_kdauf.
    CONDENSE wa_data3-mat_kdauf NO-GAPS.
    TRANSLATE wa_data3-mat_kdauf TO UPPER CASE.
    "   wa_data3-xmms = wa_data1-xmms.
    wa_data3-menge = wa_data1-menge.
    wa_data3-meins = wa_data1-meins.

    COLLECT wa_data3 INTO it_data3.
  ENDLOOP.

  "取项目描述
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.

  DATA:tname TYPE thead-tdname.
  LOOP AT it_data3 INTO wa_data3.

    CLEAR t_tline[].
    CONDENSE wa_data3-mat_kdauf NO-GAPS.
    TRANSLATE wa_data3-mat_kdauf TO UPPER CASE.
    tname = wa_data3-mat_kdauf.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = sy-langu
        name                    = tname
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        CONCATENATE wa_data-xmms t_tline-tdline INTO wa_data3-xmms.
        CLEAR t_tline.
      ENDLOOP.
    ENDIF.

**物料描述
    READ TABLE it_maktx INTO wa_maktx WITH KEY matnr = wa_data3-matnr.
    IF sy-subrc = 0.
      wa_data3-maktx = wa_maktx-maktx.
    ENDIF.

    MODIFY it_data3 FROM wa_data3.
  ENDLOOP.

  IF it_data3 IS INITIAL.
    MESSAGE 'No data!' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF p_fxk = 'X'.
    DELETE it_data3 WHERE menge = 0.
  ENDIF.

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
  init_fieldcat 'WERKS' '工厂' 'MSEG' 'WERKS'.
  init_fieldcat 'LGORT' '库存地点' 'MSEG' 'LGORT' .
  init_fieldcat 'MATNR' '物料编码' 'MSEG' 'MATNR' .
  init_fieldcat 'MAKTX' '物料描述' 'MAKT' 'MAKTX' .
  init_fieldcat 'MAT_KDAUF' '销售订单' 'KOMG' 'VBELN'.
  init_fieldcat 'XMMS' '项目描述' '' ''.
  init_fieldcat 'MENGE' '数量' 'MSEG' 'MENGE' .
  init_fieldcat 'MEINS' '单位' 'MARA' 'MEINS'.
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
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = it_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        =
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = it_data3
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
