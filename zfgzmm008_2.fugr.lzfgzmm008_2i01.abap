*&---------------------------------------------------------------------*
*&  包含                LZFGZMM008_2I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_MAX_DBDH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_max_dbdh INPUT.
  CLEAR:g_maxdbdh.
  SELECT SINGLE MAX( DBDH ) FROM ZMM002_2
    INTO  g_maxdbdh.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DBDH_CREAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dbdh_creat INPUT.
IF STATUS = 'EALX' or  STATUS = 'ECLX' .
  zmm002_2-dbdh = g_maxdbdh + 1.
ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_TIMESTAMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_timestamp INPUT.
  IF ( STATUS = 'EALX' or  STATUS = 'ECLX' ) AND ZMM002_2-TIMESTAMP = ''.
  PERFORM convert_datum_to_timestamps CHANGING ZMM002_2-TIMESTAMP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_AUTO_OUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_auto_out INPUT.
    SELECT SINGLE MAKTX INTO ZMM002_2-MAKTX
      FROM makt
      WHERE SPRAS = sy-langu
      AND MATNR = ZMM002_2-MATNR
      .
    SELECT SINGLE MEINS INTO ZMM002_2-MEINS
      FROM mara
      WHERE MATNR = ZMM002_2-MATNR.

    ZMM002_2-XCHPF = ''.
    SELECT SINGLE XCHPF INTO ZMM002_2-XCHPF
      FROM marc
      WHERE MATNR = ZMM002_2-MATNR
      AND   WERKS = ZMM002_2-WERKS.

    SELECT COUNT( * ) FROM mvke
      WHERE MATNR = ZMM002_2-MATNR
      AND   VKORG = ZMM002_2-WERKS
      AND   MTPOS = 'Z003'.
      IF sy-subrc = 0.
        ZMM002_2-SPCID = 'E'.
       ELSE.
         ZMM002_2-SPCID = ''.
      ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PRINT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE print_data INPUT.
  IF sy-ucomm = '&PRT'.
    BREAK handlj.
    SELECT * FROM ZMM002_2 INTO TABLE gt_zmm002_2
      WHERE dbdh = g_dbdh.
    PERFORM frm_print_data.
  ENDIF.
*  CLEAR gr_timestamp[].
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_SELECT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE get_select_data INPUT.
*  IF VIM_MARKED = 'X' AND sy-ucomm = '&PRT'.
*    CLEAR gr_timestamp.
*    gr_timestamp-sign = 'I'.
*    gr_timestamp-option = 'EQ'.
*    gr_timestamp-low = TOTAL+3(15).
*    APPEND gr_timestamp.
*    CLEAR gr_timestamp.
*  ENDIF.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREAT_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE creat_date INPUT.
  IF STATUS = 'EALX' or  STATUS = 'ECLX' .
    zmm002_2-BWART = '311'. "2015.4.8修改为默认311 调库
    ZMM002_2-zuser = sy-uname.
    ZMM002_2-zdatum = sy-datum.
  endif.
ENDMODULE.
