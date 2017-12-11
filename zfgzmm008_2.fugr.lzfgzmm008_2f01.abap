*&---------------------------------------------------------------------*
*&  包含                LZFGZMM008_2F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATUM_TO_TIMESTAMPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ZMM002_2_TIMESTAMP  text
*----------------------------------------------------------------------*
FORM convert_datum_to_timestamps  CHANGING p_timestamp.
   data: p_date TYPE d ,
         p_time TYPE t,
         l_timezone  LIKE ttzz-tzone,
         l_timestamp TYPE timestamp.
  p_date = sy-datum.
  p_time = sy-uzeit.
  p_time = p_time + EXIND.
  CONVERT DATE p_date
          TIME p_time
          INTO
          TIME STAMP l_timestamp  TIME ZONE l_timezone.
  p_timestamp = l_timestamp.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  frm_print_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 8, "打印默认行数
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i ,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.

  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFMM0083'.
* DATA:lt_select LIKE  it_out OCCURS 0 WITH HEADER LINE.
* DATA:lw_select LIKE LINE OF lt_select.
  DATA:lt_prt TYPE zmm002_2 OCCURS 0 WITH HEADER LINE.
  DATA:lw_prt LIKE LINE OF lt_prt.
  FIELD-SYMBOLS <lw_prt> LIKE LINE OF lt_prt.

  LOOP AT gt_zmm002_2 ASSIGNING <lw_prt>.
    <lw_prt>-timestamp = ''.
  ENDLOOP.

  IF gt_zmm002_2[] IS INITIAL.
    MESSAGE s007(zmm01) DISPLAY LIKE 'W'.
  ENDIF.
  CHECK gt_zmm002_2[] IS NOT INITIAL.


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

  SORT gt_zmm002_2[] BY dbdh werks.


  LOOP AT gt_zmm002_2 ASSIGNING <lw_prt>.
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
