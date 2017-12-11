*&---------------------------------------------------------------------*
*&  包含                ZMM008_1_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  frm_read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_TDNAME   text
*      -->T_TDSPRAS  text
*      -->T_TDID     text
*      -->T_TDOBJECT text
*      -->T_TEXT     text
*----------------------------------------------------------------------*
FORM frm_read_text USING t_tdname t_tdspras t_tdid t_tdobject CHANGING t_text.
  IF t_tdname <> ''.
    DATA:lt_tline TYPE tline OCCURS 0 WITH HEADER LINE.
*  DATA:stxl LIKE stxl OCCURS 0 WITH HEADER LINE."抬头备注
    DATA l_stxl TYPE stxl.
    l_stxl-tdid     = t_tdid     .
    l_stxl-tdspras  = t_tdspras  .
    l_stxl-tdname   = t_tdname   .
    l_stxl-tdobject = t_tdobject .

*  SELECT SINGLE * FROM STXL INTO STXL
*    WHERE TDNAME = T_TDNAME AND TDID = T_TDID AND TDSPRAS = T_TDSPRAS AND TDOBJECT = T_TDOBJECT.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = l_stxl-tdid    "读取文本的id
        language                = l_stxl-tdspras "读取文本的语言
        name                    = l_stxl-tdname    "读取文本的名字
        object                  = l_stxl-tdobject
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

*  DATA: itemp LIKE thead-tdname."itemp为变量无值

    LOOP AT lt_tline .
      CONCATENATE t_text lt_tline-tdline INTO t_text SEPARATED BY space.  "解决回车事件
    ENDLOOP.
  ENDIF.


ENDFORM. "readitemtext


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data TABLES t_zmm002_1 STRUCTURE zmm002_1.
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 8,  "默认每页行数
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i ,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.

  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFMM0081'.
* DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
* DATA:lw_select LIKE LINE OF lt_select.
  DATA:lt_prt TYPE zmm002_1 OCCURS 0 WITH HEADER LINE.
  DATA:lw_prt LIKE LINE OF lt_prt.
  FIELD-SYMBOLS <lw_prt> LIKE LINE OF lt_prt.

  LOOP AT t_zmm002_1 ASSIGNING <lw_prt>.
    <lw_prt>-timestamp = ''.
  ENDLOOP.

  IF t_zmm002_1[] IS INITIAL.
    MESSAGE s007(zmm01) DISPLAY LIKE 'W'.
  ENDIF.
  CHECK t_zmm002_1[] IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "smartforms的名字
    IMPORTING
      fm_name            = g_name                "对应的smartforms的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* Start Printing
  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  SORT t_zmm002_1[] BY dbdh bwart werks.


  LOOP AT t_zmm002_1 ASSIGNING <lw_prt>.
    AT NEW werks.
      CLEAR : lt_prt[],
              ncurrline,
              emptycount.
*      npageline = 5.
    ENDAT.

    APPEND <lw_prt> TO lt_prt.

    AT END OF werks.
      DESCRIBE TABLE lt_prt LINES ntotalline.
      ncurrline = ntotalline MOD npageline.
      IF  ncurrline > 0.
        emptycount = npageline - ncurrline.
        DO emptycount TIMES.
          APPEND INITIAL LINE TO lt_prt.
        ENDDO.
      ENDIF.

      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
          w_head             = <lw_prt>
          NPAGE_LINE         = npageline
        TABLES
          t_item             = lt_prt[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.
  ENDLOOP.
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.

ENDFORM. "frm_print_data
