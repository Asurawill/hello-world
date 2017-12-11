*&---------------------------------------------------------------------*
*&  包含                ZMM039_FRM
*&---------------------------------------------------------------------*

FORM temp_excel_get USING p_objid TYPE wwwdata-objid.
  DATA: l_objdata     LIKE wwwdatatab,
        l_destination LIKE rlgrap-filename,
        l_rc          LIKE sy-subrc,
        l_errtxt      TYPE string.
  DATA: l_fullpath  TYPE string,
        l_extension TYPE string,
        l_fname     LIKE rlgrap-filename,
        l_formkey   LIKE  wwwdatatab.

  l_extension = p_objid.
  PERFORM get_file_name USING '.xls'
                              'Excel|*.xls;'
                     CHANGING l_fullpath.
  IF l_fullpath = space.
    MESSAGE '请选择下载文件名' TYPE 'E'.
  ELSE.
    CONCATENATE l_fullpath '' INTO l_fname.
    SELECT SINGLE relid objid
      FROM wwwdata
      INTO CORRESPONDING FIELDS OF l_objdata
      WHERE relid = 'MI'
        AND objid = p_objid .

    IF sy-subrc NE 0 OR l_objdata-objid = space.
      MESSAGE e001(00) WITH '文件不存在！'.
    ELSE.
      CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
        EXPORTING
          key         = l_objdata
          destination = l_fname
        IMPORTING
          rc          = l_rc
        CHANGING
          temp        = l_fname.
      IF l_rc NE 0.
        MESSAGE e001(00) WITH '文件下载失败！'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.



FORM get_file_name USING p_extension
                         p_file_filter
                CHANGING p_fullpath.
  DATA: l_filename TYPE string,
        l_path     TYPE string,
        l_fullpath TYPE string,
        l_titile   TYPE string,
        l_init_dir TYPE string.

  CLEAR p_fullpath.
  l_titile    = TEXT-t04.
  l_init_dir  = TEXT-t03.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = l_titile
      default_extension    = '.xls'
      initial_directory    = l_init_dir
      prompt_on_overwrite  = 'X'
      file_filter          = p_file_filter
    CHANGING
      filename             = l_filename
      path                 = l_path
      fullpath             = l_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  p_fullpath = l_fullpath.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_NEW  text
*----------------------------------------------------------------------*
FORM frm_xls_to_sap   TABLES p_i_data .
  CLEAR : gt_new , gt_modify.
  DATA: l_raw TYPE truxs_t_text_data.
* 从EXCEL上传到内表
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*     i_line_header        = 'X'
      i_tab_raw_data       = l_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = p_i_data
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    gd_count = lines( p_i_data ) - 2.
  ENDIF.
ENDFORM.


FORM select_path .
  DATA v_material LIKE rlgrap-filename.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.* ,*.*.'
      mode             = '0'
      title            = '请选择要上传的信息文件'
    IMPORTING
      filename         = v_material
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.

  ENDIF.
  p_file = v_material.
ENDFORM.

FORM frm_save_data .
  "SORT gt_new BY lifnr matnr ekorg.
  DATA: lv_idx     TYPE string,
        l_index    TYPE sy-index,

        l_text(50) TYPE c. "拼接字符串
  l_index = 1.
  LOOP AT gt_new ASSIGNING  <fls_new>.
*&--------------创建采购信息记录
    "    DATA : l_infnr, l_meins.
    "    ls_new = gs_new.
    "  AT NEW ekorg.
**&-------------- 判断记录是否已经存在,不存在则新建数据
*    SELECT SINGLE eine~infnr eina~meins
*         INTO (l_infnr,l_meins)
*         FROM eine
*      INNER JOIN eina
*      ON eine~infnr = eina~infnr
*         WHERE
*      matnr = ls_new-matnr AND
*      lifnr =  ls_new-lifnr AND
*      ekorg =  ls_new-ekorg AND
*      esokz =  ls_new-esokz.
*    IF sy-subrc <> 0.
    IF <fls_new>-type  IS INITIAL.
      PERFORM frm_bdc_me11 .
      "进度条
      l_text = l_index.
      CONDENSE l_text.
      CONCATENATE '处理第' l_text '条数据/' gd_count INTO l_text.
      PERFORM set_indicator USING '' l_text.
      l_index = l_index + 1.
    ENDIF.
*    ELSE.
*      ls_new-type = 'E'.
*      ls_new-message = '采购订单信息记录已存在'.
*      ls_new-light = '@5C@'.    "红灯
*      APPEND ls_new TO gt_new_output.
*    ENDIF.
    "  ENDAT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_COMFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_NEW  text
*----------------------------------------------------------------------*
FORM frm_data_comform  .
  IF p_new = 'X'.
    DELETE gt_new INDEX 1.
    DELETE gt_new INDEX 1.
    LOOP AT gt_new ASSIGNING <fls_new>.
      "    PERFORM frm_add_qdl_matnr CHANGING <fls_new>-matnr.
      "   PERFORM frm_add_qdl_lifnr CHANGING <fls_new>-lifnr.
*&----------单位转换成大写  金额单位
      IF <fls_new>-waers IS NOT INITIAL.
        TRANSLATE <fls_new>-waers TO UPPER CASE.
      ENDIF.
    ENDLOOP.
  ELSEIF p_modify = 'X'.
    DELETE gt_modify INDEX 1.
    DELETE gt_modify INDEX 1.
    LOOP AT gt_modify ASSIGNING <fls_modify>.
      PERFORM frm_add_qdl_matnr CHANGING <fls_modify>-matnr.
      PERFORM frm_add_qdl_lifnr CHANGING <fls_modify>-lifnr.
*&----------单位转换成大写  金额单位
      IF <fls_modify>-waers IS NOT INITIAL.
        TRANSLATE <fls_modify>-waers TO UPPER CASE.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM frm_add_qdl_matnr CHANGING matnr.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = matnr
*   EXCEPTIONS
*     LENGTH_ERROR       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_ADD_QDL_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FLS_NEW>_LIFNR  text
*----------------------------------------------------------------------*
FORM frm_add_qdl_lifnr  CHANGING lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lifnr
    IMPORTING
      output = lifnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_ME11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bdc_me11 .
  DATA:
    l_number TYPE sy-msgno,
    l_msgv1  TYPE sy-msgv1,
    l_msgv2  TYPE sy-msgv2,
    l_msgv3  TYPE sy-msgv3,
    l_msgv4  TYPE sy-msgv4.



*&-------------第一个屏幕输入表头数据
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                               <fls_new>-lifnr.       "供应商
  PERFORM bdc_field       USING 'EINA-MATNR'
                                <fls_new>-matnr.       "物料
  PERFORM bdc_field       USING 'EINE-EKORG'
                                <fls_new>-ekorg.       "采购组织

*&-----------------------------------------------------------------------&*
***22.01.2017 14:11:45    HANDLR********************注释开始*
*  IF <fls_new>-esokz = '3'.
*    PERFORM bdc_field       USING 'EINE-WERKS'
*                             <fls_new>-werks.       "工厂
*  ELSE.
*    PERFORM bdc_field       USING 'EINE-WERKS'
*                            ' '.       "工厂
*  ENDIF.
***22.01.2017 14:11:45    HANDLR********************注释结束*
*&-----------------------------------------------------------------------&*
*&-----------------------------------------------------------------------&*
***22.01.2017 14:11:53    HANDLR********************新增开始*
PERFORM bdc_field       USING 'EINE-WERKS'
                             <fls_new>-werks.       "工厂
***22.01.2017 14:11:53    HANDLR********************新增结束*
*&-----------------------------------------------------------------------&*
  PERFORM bdc_field       USING 'EINA-INFNR'
                             ' '.       "记录号
  IF <fls_new>-esokz = '0'.
*    PERFORM bdc_field       USING 'RM06I-NORMB'
*                             'X'.       "标准为空
    PERFORM bdc_field       USING 'RM06I-LOHNB'
                               ''.       "外协
    PERFORM bdc_field       USING 'RM06I-PIPEL'
                           ''.       "管道 为空
    PERFORM bdc_field       USING 'RM06I-KONSI'
                       ''.       "寄售 为空
  ELSEIF <fls_new>-esokz = '3'.
*    PERFORM bdc_field       USING 'RM06I-NORMB'
*                             ''.       "标准为空
    PERFORM bdc_field       USING 'RM06I-LOHNB'
                               'X'.       "外协
    PERFORM bdc_field       USING 'RM06I-PIPEL'
                           ''.       "管道 为空
    PERFORM bdc_field       USING 'RM06I-KONSI'
                          ''.       "寄售 为空
  ENDIF.


*&-----------------第二个屏幕不输入
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.           "因为此处按回车后会跳出一个警告消息,故需要多回车一次

*&-----------------第三个屏幕输入采购组织数据

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                              'EINA-MAHN1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.
  PERFORM bdc_field       USING 'EINE-APLFZ'
                                <fls_new>-aplfz.       "计划交货时间
  PERFORM bdc_field       USING 'EINE-EKGRP'
                                <fls_new>-ekgrp.       "采购组
*  PERFORM bdc_field       USING 'EINE-NORMB'
*                                <fls_new>-NORMB.       "标准数量
  PERFORM bdc_field       USING 'EINE-WEBRE'
                                'X'.       "基于收货的发票校验
  PERFORM bdc_field       USING 'EINE-MWSKZ'
                                <fls_new>-mwskz.       "税代码
*  PERFORM bdc_field       USING 'EINE-KZABS'
*                              <fls_new>-kzabs.       "税代码

  IF <fls_new>-esokz = '3'.
*    PERFORM bdc_field       USING 'EINE-VERID'
*                                <fls_new>-verid.       "生产版本 外协需要加生产版
  ENDIF.

  PERFORM bdc_field       USING 'EINE-NETPR'
                              <fls_new>-netpr.       "含税价
  PERFORM bdc_field       USING 'EINE-PEINH'
                              <fls_new>-peinh.       "价格单位(数量)
  IF  ls_new-waers IS NOT INITIAL.
    PERFORM bdc_field       USING 'EINE-WAERS'
                                <fls_new>-waers.       "货币
  ENDIF.
*&--------------------保存
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                              <fls_new>-datab.        "时间2
  IF <fls_new>-datbi IS NOT INITIAL.
    PERFORM bdc_field       USING 'RV13A-DATBI'
                              <fls_new>-datbi.        "时间2
  ENDIF.
  CLEAR messtab[].
  CALL TRANSACTION  'ME11' USING bdcdata  MODE bdcmode
            MESSAGES INTO  messtab .
  CLEAR : bdcdata[].
  LOOP AT messtab WHERE msgtyp = 'E' OR msgtyp = 'A' .
    CLEAR:l_number,l_msgv1,l_msgv2,l_msgv3,l_msgv4.
    l_number = messtab-msgnr.
    l_msgv1 = messtab-msgv1.
    l_msgv2 = messtab-msgv2.
    l_msgv3 = messtab-msgv3.
    l_msgv4 = messtab-msgv4.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = messtab-msgid
        msgnr               = l_number
        msgv1               = l_msgv1
        msgv2               = l_msgv2
        msgv3               = l_msgv3
        msgv4               = l_msgv4
      IMPORTING
        message_text_output = <fls_new>-message.
    <fls_new>-light = '@5C@'.    "红灯
    <fls_new>-type = 'E'.
  ENDLOOP.

  IF sy-subrc <> 0.
    READ TABLE messtab WITH KEY msgtyp = 'S' .
    IF sy-subrc = 0.
      CLEAR: l_number,l_msgv1,l_msgv2,l_msgv3,l_msgv4.
      l_number = messtab-msgnr.
      l_msgv1 = messtab-msgv1.
      l_msgv2 = messtab-msgv2.
      l_msgv3 = messtab-msgv3.
      l_msgv4 = messtab-msgv4.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = messtab-msgid
          msgnr               = l_number
          msgv1               = l_msgv1
          msgv2               = l_msgv2
          msgv3               = l_msgv3
          msgv4               = l_msgv4
        IMPORTING
          message_text_output = <fls_new>-message.

      <fls_new>-type = 'S'.
      <fls_new>-light = '@5B@'.    "绿灯
      <fls_new>-message = l_msgv1.  "信息记录号
    ELSE.
      <fls_new>-type = 'E'.
      <fls_new>-light = '@5C@'.    "失败
      <fls_new>-message = '失败原因未知'.
    ENDIF.
  ENDIF.
*  APPEND ls_new TO gt_new.
ENDFORM.


FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  CLEAR: fieldcatalog.
  DEFINE build_fieldcat.
    fieldcatalog-fieldname = &1.
    fieldcatalog-seltext_m = &2.
    fieldcatalog-outputlen = &3.
    IF fieldcatalog-fieldname = 'LIGHT'.
      fieldcatalog-key  = 'X'.
    ENDIF.
    IF fieldcatalog-fieldname = 'MESSAGE'.
      fieldcatalog-emphasize = 'X'.
      fieldcatalog-key  = 'X'.
    ENDIF.

    APPEND fieldcatalog TO fieldcatalog.
    CLEAR fieldcatalog.

  END-OF-DEFINITION.

  build_fieldcat 'LIFNR' '供应商' 1.
  build_fieldcat 'MATNR' '物料号' 1.
  build_fieldcat 'EKORG' '采购组织' 1.
  build_fieldcat 'INFNR' '信息记录号' 1.

  build_fieldcat 'LIGHT' '执行结果' 1.
  build_fieldcat 'MESSAGE' '消息' 1.
  build_fieldcat 'TYPE' '结果' 1.
ENDFORM.

FORM build_layout.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-zebra             = 'X'.
  gd_layout-header_text      = '导入结果查询'.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_MODIFY_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_modify_condition .
  LOOP AT gt_modify ASSIGNING <fls_modify>.
    CLEAR : gw_keys, gt_komv.
    DATA : l_message TYPE string.
    gw_keys-lifnr =   <fls_modify>-lifnr.
    gw_keys-matnr =   <fls_modify>-matnr.
    gw_keys-ekorg =   <fls_modify>-ekorg.
    gw_keys-esokz =   <fls_modify>-esokz.

    gw_komv-kappl = 'M'.        " APPLICATION V = SALES
    gw_komv-kschl = 'PB00'.    " CONDITION TYPE
    gw_komv-kbetr = <fls_modify>-netpr.  "价格
    gw_komv-waers =  <fls_modify>-waers.  "货比码
    IF <fls_modify>-waers IS INITIAL.
      gw_komv-waers = 'CNY'.  "货比码
    ENDIF.
    gw_komv-kmein = <fls_modify>-peinh."凭证中的条件单位
    gw_komv-kpein = <fls_modify>-kmein."条件定价单位

    APPEND gw_komv TO gt_komv.
    CLEAR gw_komv.
    CALL FUNCTION 'RV_CONDITION_RESET'.
    CALL FUNCTION 'RV_CONDITION_COPY'
      EXPORTING
        application                 = 'M'     "
        condition_table             = '018'   "
        condition_type              = 'PB00'
        date_from                   = <fls_modify>-datab
        date_to                     = <fls_modify>-datbi
        enqueue                     = 'X'   "
        key_fields                  = gw_keys
        maintain_mode               = 'A'
        overlap_confirmed           = 'X'
      TABLES
        copy_records                = gt_komv
      EXCEPTIONS
        enqueue_on_record           = 1   " CONDITION RECORD IS LOCKED
        invalid_application         = 2
        invalid_condition_number    = 3
        invalid_condition_type      = 4
        no_authority_ekorg          = 5
        no_authority_kschl          = 6
        no_authority_vkorg          = 7
        no_selection                = 8
        table_not_valid             = 9
        no_material_for_settlement  = 10
        no_unit_for_period_cond     = 11
        no_unit_reference_magnitude = 12
        invalid_condition_table     = 13
        OTHERS                      = 14.
    IF sy-subrc = 0.
      CALL FUNCTION 'RV_CONDITION_SAVE'.
      COMMIT WORK.
      <fls_modify>-message = '修改成功'.
      <fls_modify>-type = 'S'.
      <fls_modify>-light = '@5B@'.    "绿灯.
    ELSE.
      CALL FUNCTION 'RV_CONDITION_RESET'.
      COMMIT WORK.
      <fls_modify>-type = sy-msgty.
      <fls_modify>-light = '@5C@'.    "红灯
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
INTO <fls_modify>-message.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_BEFORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_before .

  build_fieldcat 'LIFNR' '供应商' 1.
  build_fieldcat 'MATNR' '物料号' 1.
  build_fieldcat 'EKORG' '采购组织' 1.
  build_fieldcat 'WERKS' '工厂' 1.
  build_fieldcat 'INFNR' '信息记录号' 1.
  build_fieldcat 'ESOKZ' '信息类别' 1.

  build_fieldcat 'APLFZ' '计划交货时间' 1.
  build_fieldcat 'EKGRP' '采购组' 1.
  build_fieldcat 'NORMB' '标准数量' 1.
  build_fieldcat 'KZABS' '需求确定' 1.
  build_fieldcat 'MWSKZ' '税代码' 1.
  build_fieldcat 'VERID' '生产版本' 1.
  build_fieldcat 'NETPR' '净价' 1.

  build_fieldcat 'WAERS' '货币码' 1.
  build_fieldcat 'PEINH' '价格单位' 1.
  build_fieldcat 'DATAB' '有效期' 1.
  build_fieldcat 'DATBI' '有效期至' 1.

  build_fieldcat 'LIGHT' '执行结果' 1.
  build_fieldcat 'MESSAGE' '消息' 1.
  build_fieldcat 'TYPE' '结果' 1.
ENDFORM.


FORM frm_set_status USING pt_extab TYPE slis_t_extab .
  SET PF-STATUS 'STANDARD'.
ENDFORM. "Frm_SET_STATUS




FORM frm_user_command USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  DATA: l_grid  TYPE REF TO cl_gui_alv_grid,
        l_subrc TYPE sy-subrc.
  CASE r_ucomm.
    WHEN 'SAVE1'.
*      CLEAR :  fieldcatalog[] ,gt_new_output .
      PERFORM frm_save_data.
*&----------------ALV展示结果
*      PERFORM build_fieldcatalog.
*      PERFORM build_layout.
*      PERFORM display_alv_report.
*&-----------刷新
      rs_selfield-refresh = 'X'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. "user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT_BEFORE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report_before .
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gd_repid
      is_layout                = gd_layout
      it_fieldcat              = fieldcatalog[]  "输出的列信息
      i_save                   = 'A'          "变式可保存
      i_callback_pf_status_set = 'FRM_SET_STATUS'  "状态栏
      i_callback_user_command  = 'FRM_USER_COMMAND' "按钮处理
    TABLES
      t_outtab                 = gt_new    "输出内容内表
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM set_indicator USING p_par
                         p_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_par
      text       = p_text.
ENDFORM.
