REPORT ZMM999.

TYPES:BEGIN OF TY_ZMM009,
        MATNR    TYPE ZMM009-MATNR,
        PSPHI    TYPE ZMM009-PSPHI,
*        MAKTX    TYPE ZMM009-MAKTX,
*        POST1    TYPE ZMM009-POST1,
        WQRLJSGS TYPE ZMM009-WQRLJSGS,
        YQRLJSGS TYPE ZMM009-YQRLJSGS,
        LJCGXDS  TYPE ZMM009-LJCGXDS,
      END OF TY_ZMM009.

DATA GT_DATA TYPE TABLE OF TY_ZMM009.
DATA GS_DATA TYPE TY_ZMM009.

DATA GT_ZMM009 TYPE TABLE OF ZMM009.
DATA GS_ZMM009 TYPE ZMM009.

DATA GT_PROJ TYPE TABLE OF PROJ.
DATA GS_PROJ TYPE PROJ.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.


PARAMETERS: PO TYPE RLGRAP-FILENAME. "导入摸版

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PO.
  PERFORM SELECT_PATH."选择路径

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  REFRESH  GT_DATA.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = PO
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = GT_DATA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
*    CASE SY-SUBRC.
*      WHEN  '1'.
*        MESSAGE '存在错误的传入参数' TYPE 'E'.
*      WHEN  '2'.
*        MESSAGE 'OLE控件错误，请检查EXCEL插件及系统' TYPE 'E'.
*      WHEN  '3'.
*        MESSAGE '数据上载出错' TYPE 'E'.
*      WHEN OTHERS.
*    ENDCASE.

*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF GT_DATA IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.

  SELECT * FROM PROJ
   INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
   FOR ALL ENTRIES IN GT_DATA
   WHERE PSPID = GT_DATA-PSPHI.

  SELECT * FROM MAKT
  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR
  AND   SPRAS = SY-LANGU.

*  MOVE-CORRESPONDING GT_DATA TO GT_ZMM009.

  LOOP AT GT_DATA INTO GS_DATA.

    MOVE-CORRESPONDING GS_DATA TO GS_ZMM009.

    READ TABLE GT_PROJ INTO GS_PROJ
    WITH  KEY  PSPID = GS_DATA-PSPHI.
    IF SY-SUBRC = 0.
      GS_ZMM009-POST1 = GS_PROJ-POST1.
    ENDIF.

    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_ZMM009-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

    APPEND GS_ZMM009 TO GT_ZMM009.
    CLEAR GS_ZMM009.
  ENDLOOP.

  MODIFY ZMM009 FROM TABLE GT_ZMM009.

  MESSAGE '导入成功' TYPE 'I'.


*&---------------------------------------------------------------------*
*& Form SELECT_PATH
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM SELECT_PATH .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.* ,*.*.'
      MODE             = '0'
      TITLE            = '请选择要上传的信息文件'
    IMPORTING
      FILENAME         = PO
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " SELECT_PATH
