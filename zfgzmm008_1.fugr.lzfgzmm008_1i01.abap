*----------------------------------------------------------------------*
***INCLUDE LZFGZMM008_1I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_IT_ZMM002_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_max_dbdh INPUT.


  CLEAR:g_maxdbdh.
  SELECT SINGLE MAX( dbdh ) FROM zmm002_1
    INTO  g_maxdbdh.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_TIMESTAMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_timestamp INPUT.
  IF ( status = 'EALX' OR  status = 'ECLX' ) AND zmm002_1-timestamp = ''.
    PERFORM convert_datum_to_timestamps CHANGING zmm002_1-timestamp.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DBDH_CREAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dbdh_creat INPUT.
  IF status = 'EALX' OR  status = 'ECLX' .
    zmm002_1-dbdh = g_maxdbdh + 1.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PRINT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE print_data INPUT.

  IF sy-ucomm = '&PRT'.
    break handlj.
    SELECT * FROM zmm002_1 INTO TABLE gt_zmm002_1
      WHERE dbdh = g_dbdh.
    PERFORM frm_print_data.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_AUTO_OUT  INPUT
*&---------------------------------------------------------------------*
*       自动带出字段值
*----------------------------------------------------------------------*
MODULE get_auto_out INPUT.
  SELECT SINGLE maktx INTO zmm002_1-maktx
    FROM makt
    WHERE spras = sy-langu
    AND matnr = zmm002_1-matnr
    .
  SELECT SINGLE meins INTO zmm002_1-meins
    FROM mara
    WHERE matnr = zmm002_1-matnr.

  zmm002_1-xchpf = ''.
  SELECT SINGLE xchpf INTO zmm002_1-xchpf
    FROM marc
    WHERE matnr = zmm002_1-matnr
    AND   werks = zmm002_1-werks.

  SELECT COUNT( * ) FROM mvke
    WHERE matnr = zmm002_1-matnr
    AND   vkorg = zmm002_1-werks
    AND   mtpos = 'Z003'.
  IF sy-subrc = 0.
    zmm002_1-spcid = 'E'.
  ELSE.
    zmm002_1-spcid = ''.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_SELECT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE get_select_data INPUT.
*    BREAK handlj.
*  IF VIM_MARKED = 'X' AND sy-ucomm = '&PRT'.
*
*    CLEAR gr_timestamp.
*    gr_timestamp-sign = 'I'.
*    gr_timestamp-option = 'EQ'.
*    gr_timestamp-low = TOTAL+3(15).
*    APPEND gr_timestamp.
*    CLEAR  gr_timestamp.
*  ENDIF.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREAT_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE creat_date INPUT.
  IF status = 'EALX' OR  status = 'ECLX' .
    zmm002_1-zuser = sy-uname.
    zmm002_1-zdatum = sy-datum.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZJWUNAME_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zjwuname_check INPUT.
  data l_uname TYPE ZJWYG.
  IF status = 'EALX' OR  status = 'ECLX' or status =  'EULG' .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input         = zmm002_1-zjwuname
   IMPORTING
     OUTPUT        = l_uname    .
    IF l_uname NP '3*'.
      MESSAGE e000 WITH '借物员工编号必须为3开头'.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_VBDESCRIB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_vbdescrib INPUT.
   PERFORM frm_read_text USING ZMM002_1-VBELN sy-langu 'Z001' 'VBBK'
                         CHANGING ZMM002_1-vbdescrib.
ENDMODULE.
