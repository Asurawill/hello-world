REPORT ZMM029.

TYPES:BEGIN OF TY_ZMM029,
        MATNR  TYPE ZMM024-MATNR,
        POSID  TYPE ZMM024-POSID,
        SJDM   TYPE ZMM024-SJDM,
        BEIZHU TYPE ZMM024-BEIZHU,
        MAKTX  TYPE ZMM024-MAKTX,
        POST1  TYPE ZMM024-POST1,
        NAME   TYPE ICONNAME, "图标
        MES    TYPE STRING,  "消息
      END OF TY_ZMM029.

DATA GT_DATA  TYPE TABLE OF TY_ZMM029.
DATA GT_DATA_1 TYPE TABLE OF TY_ZMM029.
DATA GS_DATA TYPE TY_ZMM029.

DATA GT_ZMM024 TYPE TABLE OF ZMM024.
DATA GS_ZMM024 TYPE ZMM024.

DATA GT_PROJ TYPE TABLE OF PROJ.
DATA GS_PROJ TYPE PROJ.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

************************************************************************
*     ALV                                                              *
************************************************************************
TYPE-POOLS SLIS.
DATA: GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GS_REPID    TYPE SY-REPID.
DATA  GT_EXCLU     TYPE SLIS_T_EXTAB.
DATA  GT_SORT      TYPE SLIS_T_SORTINFO_ALV.
DATA  GS_SORT      TYPE SLIS_SORTINFO_ALV.

DEFINE APPEND_FIELDCAT.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = &1.
  gs_fieldcat-seltext_m = &2.
  gs_fieldcat-hotspot = &3.
  gs_fieldcat-do_sum = &4.
  append gs_fieldcat to gt_fieldcat.
END-OF-DEFINITION.
************************************************************************


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
      I_BEGIN_ROW             = 3
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
   WHERE PSPID = GT_DATA-POSID.

  SELECT * FROM MAKT
  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR
  AND   SPRAS = SY-LANGU.

*  MOVE-CORRESPONDING GT_DATA TO GT_ZMM009.

  LOOP AT GT_DATA INTO GS_DATA.

    MOVE-CORRESPONDING GS_DATA TO GS_ZMM024.

    READ TABLE GT_PROJ INTO GS_PROJ
    WITH  KEY  PSPID = GS_DATA-POSID.
    IF SY-SUBRC = 0.
      GS_DATA-POST1 = GS_PROJ-POST1.
    ELSE.
      GS_DATA-NAME = ICON_RED_LIGHT.
      GS_DATA-MES  = '项目编号不存在请核对'.
    ENDIF.

    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ELSE.
      GS_DATA-NAME = ICON_RED_LIGHT.
      GS_DATA-MES  = '物料编码不存在请核对'.
    ENDIF.

    IF GS_DATA-NAME IS INITIAL.
      GS_DATA-NAME = ICON_GREEN_LIGHT.
      GS_DATA-MES  = '数据更新成功'.
    ENDIF.

    MODIFY GT_DATA FROM GS_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

  GT_DATA_1 = GT_DATA .

  DELETE GT_DATA_1 WHERE NAME = ICON_RED_LIGHT.
  MOVE-CORRESPONDING GT_DATA_1 TO GT_ZMM024.

  MODIFY ZMM024 FROM TABLE GT_ZMM024.

*  MESSAGE '导入成功' TYPE 'I'.
  PERFORM DISPLAY_MSG.

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

*&---------------------------------------------------------------------*
*&      Form  display_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MSG .
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  REFRESH GT_FIELDCAT.
  APPEND_FIELDCAT 'NAME' '状态栏' '' ''.
  APPEND_FIELDCAT 'MES' '消息' '' ''.
  APPEND_FIELDCAT 'MATNR' '物料编号' '' ''.
  APPEND_FIELDCAT 'MAKTX' '物料描述' '' ''.
  APPEND_FIELDCAT 'POSID' '项目编号' '' ''.
  APPEND_FIELDCAT 'POST1' '项目描述' '' ''.
  APPEND_FIELDCAT 'SJDM' '设计代码' '' ''.
  APPEND_FIELDCAT 'BEIZHU' '备注' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GS_REPID
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GT_FIELDCAT
      I_SAVE             = 'A'
    TABLES
      T_OUTTAB           = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
ENDFORM.                    " display_msg
