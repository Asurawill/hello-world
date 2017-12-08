REPORT ZFI006_55.
TABLES:ZFI006 .
TYPE-POOLS: SLIS,TRUXS.
*-->上传接口定义
TYPES: BEGIN OF TY_UPLOAD,
        ZSQD(40) TYPE C,     "申请单
        BUKRS(40) TYPE C,     "公司代码
        GJAHR(40) TYPE C,     "年度
        EBELN(40) TYPE C,     "采购订单
        EBELP(40) TYPE C,     "采购订单行号
        BELNR(40) TYPE C,
        LIFNR(40) TYPE C,
        ZFKXZ(40) TYPE C,
        ZSQFKJE(40) TYPE C,
        WAERS_2(40) TYPE C,
        WAERS(40) TYPE C,
        ZSQRQ(40) TYPE C ,
        ZSQFKRQ(40) TYPE C ,
        ZZY(40) TYPE C,
        ZCLJD(40) TYPE C,
        BELNR_F(40) TYPE C,
        GJAHR_F(40) TYPE C,
        STATU(40) TYPE C,
        NAME1(40) TYPE C,
        NETWR(40) TYPE C,
        NETWR_1(40) TYPE C,
        YJHJE(40) TYPE C,
        YJHJE_1(40) TYPE C,
        FPPZJE(40) TYPE C,
        FPPZJE_1(40) TYPE C,
        YFJE(40) TYPE C,
        WAERS_1(40) TYPE C,
        ZNAME(40) TYPE C,
        ZDATE(40) TYPE C ,
        ZTIME(40) TYPE C,
        ZNAME1(40) TYPE C,
        YSQJE(40) TYPE C,

       END OF TY_UPLOAD.
DATA: WYT_UPLOAD TYPE STANDARD TABLE OF TY_UPLOAD,
      WYS_UPLOAD TYPE TY_UPLOAD.
DATA: IT_DOWNLOAD TYPE STANDARD TABLE OF TY_UPLOAD,
      WA_DOWNLOAD TYPE TY_UPLOAD.
DATA T_RAW_DATA TYPE TRUXS_T_TEXT_DATA.

DATA:  GS_ZFI006 TYPE ZFI006,
       GT_ZFI006 TYPE TABLE OF ZFI006.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS P_FILE LIKE RLGRAP-FILENAME OBLIGATORY.
PARAMETERS: R_ADD RADIOBUTTON GROUP RAD1 DEFAULT 'X',
            R_DEL RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN END OF BLOCK B1.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_FILEPATH.
*&---------------------------------------------------------------------*
** Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
   IF P_FILE IS NOT INITIAL.
       PERFORM FRM_INPUT.
    ENDIF.
  IF R_ADD = 'X'.

    PERFORM ADD_DB_DATA..

  ENDIF.
  IF R_DEL = 'X'.
    PERFORM DEL_DB_DATA.

  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_FILEPATH .
 CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    EXPORTING
*     DEF_FILENAME     = ' '
*     DEF_PATH         = ' '
      MASK             = 'Excel Files,*.xls,All Files,*.*. '
      MODE             = 'O'
*     TITLE            = ' '
    IMPORTING
      FILENAME         = P_FILE
*     PATH             =
*     FILE             =
    EXCEPTIONS
      SELECTION_CANCEL = 1
      SELECTION_ERROR  = 2
      OTHERS           = 3.
  CASE SY-SUBRC.
    WHEN 0.
    WHEN 2.
      MESSAGE 'Cancel.' TYPE 'S'.
    WHEN OTHERS.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INPUT .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR          =
     I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             = T_RAW_DATA
      I_FILENAME                 = P_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = WYT_UPLOAD
*   EXCEPTIONS
*     CONVERSION_FAILED          = 1
*     OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_DB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_DB_DATA .
  LOOP AT WYT_UPLOAD INTO WYS_UPLOAD .
    CLEAR:GS_ZFI006.
    MOVE-CORRESPONDING WYS_UPLOAD TO  GS_ZFI006.
     MODIFY ZFI006 FROM GS_ZFI006 .


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEL_DB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_DB_DATA .
  LOOP AT WYT_UPLOAD INTO WYS_UPLOAD .
    CLEAR :GS_ZFI006.
    MOVE-CORRESPONDING WYS_UPLOAD TO  GS_ZFI006.
     DELETE ZFI006 FROM GS_ZFI006 .
  ENDLOOP.

 ENDFORM.
