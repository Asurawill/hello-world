*----------------------------------------------------------------------*
*  程序名称         :ZMM026
*  创建者           : 吴丽娟
*  创建日期         :20151023
*----------------------------------------------------------------------*
*  概要说明
* 整备库台账维护报表
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
* 20151023     HANDYWLJ     ED1K903294           创建
*
*----------------------------------------------------------------------*
"增加调整凭证号  ADD it02 20160118
"增加整备库3012 IT02 20160121

REPORT zmm026.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
TYPE-POOLS:slis,icon.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
TABLES: mseg,vbak.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES:BEGIN OF ty_maktx,         "取物料描述的表
        maktx TYPE makt-maktx,
        matnr TYPE mseg-matnr,
      END OF ty_maktx.
TYPES:BEGIN OF ty_matkl,         "取物料组的表
        matnr TYPE mseg-matnr,
        matkl TYPE mara-matkl,
      END OF ty_matkl.
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
TYPES:BEGIN OF ty_data.
        INCLUDE STRUCTURE zmm026.
TYPES: cellstyle TYPE lvc_t_styl,
       END OF ty_data.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_data TYPE TABLE OF ty_data,
     wa_data TYPE ty_data.

DATA:it_data1 TYPE TABLE OF zmm026,
     wa_data1 TYPE zmm026.

DATA:it_data2 TYPE TABLE OF zmm026,
     wa_data2 TYPE zmm026.

DATA:it_data3 TYPE TABLE OF zmm026,
     wa_data3 TYPE zmm026.

DATA:it_data4 TYPE TABLE OF zmm026,
     wa_data4 TYPE zmm026.

DATA:it_maktx TYPE TABLE OF ty_maktx,
     wa_maktx TYPE ty_maktx.

DATA:it_matkl TYPE TABLE OF ty_matkl,
     wa_matkl TYPE ty_matkl.

DATA:it_vbeln TYPE TABLE OF ty_vbeln,
     wa_vbeln TYPE ty_vbeln.

DATA:it_vgbel TYPE TABLE OF ty_vgbel,
     wa_vgbel TYPE ty_vgbel.

DATA:it_vbeln1 TYPE TABLE OF ty_vbeln1,
     wa_vbeln1 TYPE ty_vbeln1.

DATA gt_t001w  TYPE TABLE OF t001w WITH HEADER LINE.

DATA:it_zmm002I TYPE TABLE OF zmm002I,
     wa_zmm002I TYPE zmm002I.
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
  wa_fieldcat-edit = &5.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_mblnr      FOR mseg-mblnr,"物料凭证
               s_werks      FOR mseg-werks,"工厂
               s_lgort      FOR mseg-lgort,"库存地点
               s_matnr      FOR mseg-matnr,"物料编码
               s_kdauf      FOR vbak-vbeln,"销售订单
               s_bwart      FOR mseg-bwart,"移动类型
               s_budat      FOR mseg-budat_mkpf,"过账日期
               s_usnam      FOR mseg-usnam_mkpf."过账人
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

*查询工厂
  SELECT * FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE gt_t001w.

  PERFORM frm_auth_check.

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
*    JOIN lips ON mseg~vbeln_im = lips~vbeln
*    AND mseg~vbelp_im = lips~posnr
*    JOIN ekkn ON lips~vgbel = ekkn~ebeln
*    AND lips~vgpos = ekkn~ebelp
    INTO CORRESPONDING FIELDS OF TABLE it_data
    WHERE mblnr    IN s_mblnr
    AND mseg~werks      IN s_werks
    AND mseg~lgort      IN s_lgort
    AND mseg~matnr      IN s_matnr
*    AND ( mseg~mat_kdauf  IN s_kdauf
*    OR mseg~ablad IN s_kdauf )
*    OR  lips~vbeln IN s_kdauf
*    OR ekkn~vbeln IN s_kdauf )
    AND mseg~bwart      IN s_bwart
    AND mseg~budat_mkpf IN s_budat
    AND mseg~usnam_mkpf IN s_usnam
    AND ( mseg~werks = 1500 OR mseg~werks = 1100 )
    AND mseg~budat_mkpf >= '20151101'
    AND ( mseg~lgort = '3002'
     OR mseg~lgort = '3004'
     OR mseg~lgort = '3006'
     OR mseg~lgort = '3003'
     OR mseg~lgort = '3012')
    AND mseg~sobkz <> 'F'.

  IF it_data IS NOT INITIAL.
*物料描述
    SELECT makt~maktx
           mseg~matnr
      FROM makt
      JOIN mseg ON makt~matnr = mseg~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_maktx
      FOR ALL ENTRIES IN it_data
      WHERE mseg~matnr = it_data-matnr
      AND spras = sy-langu.
*取物料组
    SELECT mseg~matnr
           mara~matkl
      FROM mseg
      JOIN mara ON mseg~matnr = mara~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_matkl
      FOR ALL ENTRIES IN it_data
      WHERE mseg~matnr = it_data-matnr.

   "取ZMM002I的已调整凭证  ADD  IT02 160118
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZMM002I
      FROM ZMM002I
      WHERE WERKS IN s_werks
      AND   TZBS  EQ 'X'.
    SORT IT_ZMM002I BY WERKS  TZPZH ZEILE3 .
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

  SELECT * FROM zmm026 INTO CORRESPONDING FIELDS OF TABLE it_data1.
  "DELETE zmm026 FROM TABLE it_data1.

  LOOP AT it_data INTO wa_data.

*数据筛选逻辑
    IF wa_data-werks = '1500' and ( wa_data-lgort <> '3002' and wa_data-lgort <> '3003'  ) .

        CONTINUE.

      ENDIF.


*    IF wa_data-werks = '1100' AND  wa_data-lgort <> '3002'.
*      IF wa_data-lgort <> '3004'.
*        IF wa_data-lgort <> '3006'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*    ENDIF.

   IF wa_data-werks = '1100' and ( wa_data-lgort <> '3002' and wa_data-lgort <> '3004'  and wa_data-lgort <> '3006' and wa_data-lgort <> '3012') .

        CONTINUE.

      ENDIF.


*是否冲销凭证
    IF wa_data-smbln IS INITIAL.
      wa_data-sfcxpz = '否'.
    ELSE.
      wa_data-sfcxpz = '是'.
    ENDIF.

"是否调整凭证
   READ TABLE IT_ZMM002I INTO WA_ZMM002I WITH KEY TZPZH = WA_DATA-MBLNR ZEILE3 = WA_DATA-ZEILE BINARY SEARCH .
     IF SY-SUBRC = 0.
       WA_DATA-IS_TZ = '是'.
       WA_DATA-TZPZ_YSPZ =  WA_ZMM002I-MBLNR.
       WA_DATA-TZPZ_YSPZH = WA_ZMM002I-ZEILE.
     ENDIF.
*物料描述
    READ TABLE it_maktx INTO wa_maktx WITH KEY matnr = wa_data-matnr.
    IF sy-subrc = 0.
      wa_data-maktx = wa_maktx-maktx.
    ENDIF.

*物料组
    READ TABLE it_matkl INTO wa_matkl WITH KEY matnr = wa_data-matnr.
    IF sy-subrc = 0.
      wa_data-matkl = wa_matkl-matkl.
    ENDIF.

*销售订单和销售订单行号
    REFRESH:it_vbeln,it_vgbel,it_vbeln1.
    IF wa_data-mat_kdauf IS INITIAL. "如果MAT_KDAUF不是空，那么销售订单就等于MAT_KDAUF的值，行号就是MAT_KAPOS。
      IF wa_data-ablad IS NOT INITIAL."如果MAT_KDAUF是空，ABLAD不是空，那么销售订单就等于ABLAD。
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
              wa_data-mat_kdpos = wa_vbeln-vgpos.
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
                    wa_data-mat_kdpos = wa_vbeln1-vbelp.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*判断有销售订单的话，该字段就为X，否则为空
    IF wa_data-mat_kdauf = ''.
    ELSE.
      wa_data-pdsfwk = 'X'.
    ENDIF.
*取项目描述
    DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
    CLEAR t_tline[].
    DATA:tname TYPE thead-tdname.
    tname = wa_data-mat_kdauf.
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
        CONCATENATE wa_data-xmms t_tline-tdline INTO wa_data-xmms.
        CLEAR t_tline.
      ENDLOOP.
    ENDIF.

*状态标识
    IF wa_data-mat_kdauf IS INITIAL.
      wa_data-statu = icon_red_light.
    ELSE.
      wa_data-statu = icon_green_light.
    ENDIF.

*数量
    IF wa_data-shkzg = 'H'.
      wa_data-menge = - wa_data-menge.
    ENDIF.

    LOOP AT it_data1 INTO wa_data1 WHERE
       mblnr = wa_data-mblnr AND
      zeile = wa_data-zeile.
      wa_data-statu = wa_data1-statu.         "状态标识
      wa_data-mat_kdauf = wa_data1-mat_kdauf. "销售订单号
      wa_data-xmms = wa_data1-xmms.            "项目描述
      wa_data-tzzhggr = wa_data1-tzzhggr.     "台账最后更改人
      wa_data-zhggrq = wa_data1-zhggrq.         "最后更改日期
    ENDLOOP.

    IF wa_data-pdsfwk = 'X'.
      PERFORM frm_cell_style USING 'MAT_KDAUF'
                                   'X'
                             CHANGING wa_data-cellstyle.
    ENDIF.

    MODIFY it_data FROM wa_data.

    CLEAR wa_data.

  ENDLOOP.

  DELETE it_data WHERE statu = ''.
  SORT it_data BY mblnr zeile.

  IF s_kdauf IS NOT INITIAL.
    IF s_kdauf-option = 'EQ'.
      DELETE it_data WHERE mat_kdauf <> s_kdauf-low.
    ELSEIF s_kdauf-option = 'BT'.
      DELETE it_data WHERE mat_kdauf NOT BETWEEN s_kdauf-low AND s_kdauf-high.
    ENDIF.
  ENDIF.

  IF it_data IS INITIAL.
    MESSAGE 'No data!' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  MOVE-CORRESPONDING it_data TO it_data2.
  MODIFY zmm026 FROM TABLE it_data2.

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
  wa_layout-sel_mode = 'A'.
  wa_layout-stylefname = 'CELLSTYLE'.
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
  init_fieldcat 'STATU' '状态标识' '' '' ''.
  init_fieldcat 'MBLNR' '物料凭证' 'MSEG' 'MBLNR' ''.
  init_fieldcat 'ZEILE' '物料凭证行' 'MSEG' 'ZEILE' ''.
  init_fieldcat 'MJAHR' '物料凭证年度' 'MSEG' 'MJAHR' ''.
  init_fieldcat 'SFCXPZ' '是否冲销凭证' '' '' ''.
  init_fieldcat 'SMBLN' '冲销凭证对应原始凭证' 'MSEG' 'SMBLN' ''.
  init_fieldcat 'SMBLP' '冲销凭证对应原始凭证行项目' 'MSEG' 'SMBLP' ''.
  init_fieldcat 'IS_TZ' '是否调整凭证' '' '' ''.
  init_fieldcat 'TZPZ_YSPZ' '调整凭证对应原始凭证' 'ZMM002I' 'TZPZH' ''.
  init_fieldcat 'TZPZ_YSPZH' '调整凭证对应原始凭证行项目' '' '' ''.
  init_fieldcat 'WERKS' '工厂' 'MSEG' 'WERKS' ''.
  init_fieldcat 'LGORT' '库存地点' 'MSEG' 'LGORT' ''.
  init_fieldcat 'MATNR' '物料编码' 'MSEG' 'MATNR' ''.
  init_fieldcat 'MAKTX' '物料描述' 'MAKT' 'MAKTX' ''.
  init_fieldcat 'MATKL' '物料组' 'MARA' 'MATKL' ''.
  init_fieldcat 'BWART' '移动类型' 'MSEG' 'BWART' ''.
  init_fieldcat 'MENGE' '数量' 'MSEG' 'MENGE' ''.
  init_fieldcat 'MEINS' '单位' 'MSEG' 'MEINS' ''.
  init_fieldcat 'BUDAT_MKPF' '过账日期' 'MSEG' 'BUDAT_MKPF' ''.
  init_fieldcat 'USNAM_MKPF' '过账人' 'MSEG' 'USNAM_MKPF' ''.
  init_fieldcat 'MAT_KDAUF' '销售订单' 'KOMG' 'VBELN' 'X'.
  init_fieldcat 'XMMS' '项目描述' '' '' ''.
  init_fieldcat 'MAT_KDPOS' '销售订单行号' 'MSEG' 'MAT_KDPOS' ''.
  init_fieldcat 'TZZHGGR' '台账最后更改人' '' '' ''.
  init_fieldcat 'ZHGGRQ' '最后更改日期' '' '' ''.
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
      i_callback_user_command  = 'ALV_USER_COMMAND'
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
      it_events                = it_events
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
      t_outtab                 = it_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                     rs_selfield TYPE slis_selfield.


  CASE r_ucomm.

    WHEN '&DATA_SAVE'.
      LOOP AT it_data INTO wa_data.
        READ TABLE it_data2 INTO wa_data2   "INDEX sy-tabix.
                 WITH KEY mblnr = wa_data-mblnr
                          zeile = wa_data-zeile.
        IF sy-subrc = 0.
          IF wa_data-mat_kdauf <> wa_data2-mat_kdauf.
*状态标识
            IF wa_data-mat_kdauf IS NOT INITIAL.
              wa_data-statu = icon_green_light.
            ELSE.
              wa_data-statu = icon_red_light.
            ENDIF.
*台账最后更改人
            wa_data-tzzhggr = sy-uname.
*最后更改日期
            wa_data-zhggrq = sy-datum.
*项目描述
            CLEAR wa_data-xmms.
            DATA t_tline1 TYPE TABLE OF tline WITH HEADER LINE.
            CLEAR t_tline1[].
            DATA:tname1 TYPE thead-tdname.
            tname1 = wa_data-mat_kdauf.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
*               CLIENT                  = SY-MANDT
                id                      = 'Z001'
                language                = sy-langu
                name                    = tname1
                object                  = 'VBBK'
*               ARCHIVE_HANDLE          = 0
*               LOCAL_CAT               = ' '
*   IMPORTING
*               HEADER                  =
*               OLD_LINE_COUNTER        =
              TABLES
                lines                   = t_tline1[]
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
            IF t_tline1[] IS NOT INITIAL.
              LOOP AT t_tline1.
                CONCATENATE wa_data-xmms t_tline1-tdline INTO wa_data-xmms.
                CLEAR t_tline1.
              ENDLOOP.
            ENDIF.

            MODIFY it_data FROM wa_data.

            CLEAR wa_data.
          ENDIF.
        ENDIF.

      ENDLOOP.

      SELECT *
        FROM zmm026 INTO CORRESPONDING FIELDS OF TABLE it_data3.
      "DELETE zmm026 FROM TABLE it_data3.

      MOVE-CORRESPONDING it_data TO it_data4.
      MODIFY zmm026 FROM TABLE it_data4.

      rs_selfield-refresh = 'X'.
      SET USER-COMMAND '&OPT'.


    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  LOOP AT gt_t001w WHERE werks IN s_werks.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
             ID 'WERKS' FIELD gt_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gt_t001w-werks.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CELL_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATA_CELLSTYLE  text
*      -->P_0867   text
*----------------------------------------------------------------------*
FORM frm_cell_style  USING    p_fieldname
                              p_editable
                     CHANGING pt_cellstyle TYPE lvc_t_styl.
  DATA: lw_cellstyle TYPE lvc_s_styl.

  lw_cellstyle-fieldname = p_fieldname.

  lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.


  INSERT lw_cellstyle INTO TABLE pt_cellstyle.

ENDFORM.
