
*----------------------------------------------------------------------*
* 程序名称：员工照片批量导入                                           *
* 程序名  ：ZHRD_002                                                 *
* 开发日期：2015.06.23                                               *
* 创建者  ：曹磊                                                    *
* 申请者  ：杨超雄                                                     *
* 模  块  : HR                                                         *
*----------------------------------------------------------------------*
* 概　要  ：                                                           *
*                                                                      *
*----------------------------------------------------------------------*
* 变更记录:                                                            *
* 2015.06.23   By  曹磊(初始开发)                   *
*----------------------------------------------------------------------*


REPORT ZHR001.

*----------------------------------------------------------------------*
*                       类 型 池、数 据 表 引 用
*----------------------------------------------------------------------*

INCLUDE: <ICON>.

TABLES: PA0001.

*----------------------------------------------------------------------*
*                         内表、工作区 定 义
*----------------------------------------------------------------------*

DATA: FILETAB TYPE TABLE OF FILE_INFO WITH HEADER LINE.

*----------------------------------------------------------------------*
*                           常量、变量 定 义
*----------------------------------------------------------------------*

DATA: SAPOBJID LIKE SAPB-SAPOBJID,
      SAPPFAD  LIKE SAPB-SAPPFAD.

DATA: GD_PATH TYPE STRING,
      COUNT   TYPE I.

DATA: FILENAME(40) TYPE C,
      FILEEXT(10)  TYPE C,
      LEN          TYPE I,
      FLAG         TYPE C,
      MESS         TYPE STRING.

*----------------------------------------------------------------------*
*                           SELECTION-SCREEN
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-T01.

*重点图片名称要等于系统员工编号
PARAMETERS: FILEPATH LIKE RLGRAP-FILENAME."上传文件夹路径

SELECTION-SCREEN END OF BLOCK BLK1.

*----------------------------------------------------------------------*
*                         AT SELECTION-SCREEN
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILEPATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE    = 'File Directory'
      INITIAL_FOLDER  = 'C:\'
    CHANGING
      SELECTED_FOLDER = GD_PATH.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  CONCATENATE GD_PATH '' INTO FILEPATH.

*----------------------------------------------------------------------*
*                         START-OF-SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.

  " 设置新的Gui抬头
  SY-TITLE = TEXT-H01.

  GD_PATH = FILEPATH .
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = GD_PATH
      FILTER                      = '*.jpg'
    CHANGING
      FILE_TABLE                  = FILETAB[]
      COUNT                       = COUNT
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      NOT_SUPPORTED_BY_GUI        = 5
      OTHERS                      = 6.

  CLEAR: FLAG.

*判断图片编号是否等于8位
  LOOP AT FILETAB.
    SPLIT FILETAB-FILENAME AT '.' INTO FILENAME FILEEXT.
    LEN = STRLEN( FILENAME ) .
    IF LEN <> 8 .
      FLAG = 'X'.
      CONCATENATE '@5C@' '文件' FILETAB-FILENAME  '的文件名' FILENAME '长度必须等于8位' INTO MESS.
      CONDENSE MESS.
      WRITE: / MESS.
    ENDIF.
  ENDLOOP.

  IF FLAG = 'X'.
    EXIT.
  ENDIF.

  LOOP AT FILETAB.

    CONCATENATE GD_PATH '\' FILETAB-FILENAME INTO SAPPFAD.

*判断员工编号是否存在
    SELECT SINGLE *
      FROM PA0001
     WHERE PERNR = FILETAB-FILENAME+0(8).
    IF SY-SUBRC = 0.
*判断员工编号是否已过期
      SELECT SINGLE *
       FROM PA0001
       WHERE PERNR = FILETAB-FILENAME+0(8)
         AND BEGDA =< SY-DATUM
         AND ENDDA >= SY-DATUM.
      IF SY-SUBRC = 0.
        CONCATENATE FILETAB-FILENAME+0(8) '0002' INTO SAPOBJID.
        CALL FUNCTION 'ARCHIV_CREATE_FILE'
          EXPORTING
            AR_OBJECT               = 'HRICOLFOTO'
            OBJECT_ID               = SAPOBJID
            SAP_OBJECT              = 'PREL'
            DOC_TYPE                = 'JPG'
            PATH                    = SAPPFAD
          EXCEPTIONS
            ERROR_CONECTIONTABLE    = 1
            ERROR_PARAMETER         = 2
            ERROR_ARCHIV            = 3
            ERROR_UPLOAD            = 4
            ERROR_KERNEL            = 5
            NO_ENTRY_POSSIBLE       = 6
            ERROR_COMUNICATIONTABLE = 7
            OTHERS                  = 8.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          WRITE : / '@5B@', 'Upload ',SAPPFAD,'To pernr ',FILETAB-FILENAME+0(8),'Sccuess!'.
        ENDIF.
      ELSE.
        WRITE : / '@5C@', 'ERROR  ',SAPPFAD, '此员工编号 ', FILETAB-FILENAME+0(8),'已过期!'.
      ENDIF.
    ELSE.
      WRITE : / '@5C@', 'ERROR  ',SAPPFAD, '此员工编号 ', FILETAB-FILENAME+0(8),'不存在!'.
    ENDIF.

  ENDLOOP.
