*&---------------------------------------------------------------------*
*&  包含                ZPP035_SCREEN
*&---------------------------------------------------------------------*
*选择屏幕的屏幕字段限制
PARAMETERS: P_FILE TYPE RLGRAP-FILENAME   OBLIGATORY."文件目录

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  "选择上传文件
  PERFORM CHOOSE_FILE.

************************************************************************
* event Start of Selection
************************************************************************
START-OF-SELECTION.
  PERFORM ENSURE_CHOOSE.
