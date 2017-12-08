*&---------------------------------------------------------------------*
*&  包含                ZSD001_SCREEN_INCLUDE
*&---------------------------------------------------------------------*
************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:P_UP TYPE CHAR1   RADIOBUTTON  GROUP  G1 DEFAULT 'X' USER-COMMAND G_UCMD,
          P_DOWN TYPE CHAR1 RADIOBUTTON  GROUP  G1 .
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_FN TYPE RLGRAP-FILENAME MODIF ID ZUP MEMORY ID ZSD001. "主数据文件路径
SELECTION-SCREEN END OF BLOCK BLK2.

SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_DO TYPE RLGRAP-FILENAME MODIF ID  ZDO. "模版下载
SELECTION-SCREEN END OF BLOCK BLK3.


************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'ZUP'.
      IF P_UP = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'ZDO'.
      IF P_DOWN = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FN.
  PERFORM FRM_GET_FN.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DO.
  PERFORM FRM_GET_DO.
************************************************************************
* Event top of page
************************************************************************
TOP-OF-PAGE.


AT SELECTION-SCREEN.


START-OF-SELECTION.
  IF P_UP = 'X'.
    PERFORM FRM_CHECH_FILENAME.             "检测文件名
*客户主数据
    PERFORM FRM_UPLOAD_DATA TABLES T_DATA."上传文件到内表
    PERFORM FRM_GET_DATA.
    PERFORM FRM_ALV_SHOW.
  ELSE.
    PERFORM FRM_DOWNLOAD  .
  ENDIF.

*----------------------------------------------------------------------------------
*Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2012. Sap Release 702
