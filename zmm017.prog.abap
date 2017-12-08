REPORT ZMM017.
************************************************************************
* SELECTION SCREEN
************************************************************************
*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,   "ALV的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,  "SY-REPID 指 当前的主程序
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GW_EVENTS        LIKE LINE OF GT_EVENTS.
DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.

DATA: GW_ISTABLE TYPE LVC_S_STBL.
TYPES:BEGIN OF TY_FILE,
        EBELN      TYPE EKPO-EBELN,"采购订单
        EBELP     TYPE EKPO-EBELP, "行项目号
        KBETR      TYPE KONV-KBETR, "价格
      END OF TY_FILE.

TYPES:BEGIN OF TY_DATA,
        EBELN      TYPE EKPO-EBELN,"采购订单
        EBELP     TYPE EKPO-EBELP, "行项目号
        KBETR      TYPE KONV-KBETR, "价格
       KNUMV TYPE EKKO-KNUMV,"条件类型
       KSCHL TYPE KONV-KSCHL,"条件类型
        STATUS(20), "状态
        MSG(400),"日志
      END OF TY_DATA.
TYPES:BEGIN OF TY_EKKO,
      EBELN TYPE EKKO-EBELN,"采购订单
      BSART TYPE EKKO-BSART,"采购类型
      KNUMV TYPE EKKO-KNUMV,"条件类型
      END OF TY_EKKO.
DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT = &2.
  GW_LVC-OUTPUTLEN = &3.
*  IF &4 = 'X'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
  GW_LVC-ICON = &4.
*  GW_LVC-CHECKBOX = &5.
*  GW_LVC-EDIT = &6.
  GW_LVC-QFIELDNAME =  &5.
  GW_LVC-REF_TABLE = &6.
  GW_LVC-REF_FIELD = &7.
  GW_LVC-DATATYPE = &8.
*  GW_LVC-INTLEN = &9.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.
DATA: IT_FILE TYPE TABLE OF TY_FILE WITH HEADER LINE ,
      IT_DATA TYPE TABLE OF TY_DATA WITH HEADER LINE.
DATA:IT_EKKO TYPE TABLE OF  TY_EKKO WITH HEADER LINE.
DATA:IT_KONV LIKE TABLE OF KONV WITH HEADER LINE.
DATA: T_POITEM LIKE BAPIMEPOITEM OCCURS 0 WITH HEADER LINE.
DATA: T_POITEMX LIKE BAPIMEPOITEMX OCCURS 0 WITH HEADER LINE.
DATA: T_RETURN LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
DATA: T_COND LIKE BAPIMEPOCOND OCCURS 0 WITH HEADER LINE.
DATA: T_CONDX LIKE BAPIMEPOCONDX OCCURS 0 WITH HEADER LINE.
DATA:P_DOFN TYPE STRING.
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_UP TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND UCOMM,
           P_DOWN TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETER: P_UPFN TYPE STRING MODIF ID UP MEMORY ID ZMM017.
SELECTION-SCREEN END OF BLOCK BLK2.

*SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
*PARAMETER: P_DOFN TYPE STRING MODIF ID DO ."default 'ZMM017_TEMPLATE.XLS'.
*SELECTION-SCREEN END OF BLOCK BLK3.
************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.


************************************************************************
* AT SELECTION SCREEN
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'UP'.
      IF P_UP = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
*    ELSEIF SCREEN-GROUP1 = 'DO'.
*      IF P_DOWN = 'X'.
*        SCREEN-ACTIVE = 1.
*      ELSE.
*        SCREEN-ACTIVE = 0.
*      ENDIF.
*      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_UPFN.
  PERFORM FRM_GET_PATH CHANGING P_UPFN. "上传的搜索帮助

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DOFN.
*  PERFORM FRM_GET_DO_FILE CHANGING P_DOFN . "下载的搜索帮助

AT SELECTION-SCREEN.
************************************************************************
* EVENT TOP OF PAGE
************************************************************************
TOP-OF-PAGE.

************************************************************************
* EVENT START OF SELECTION
************************************************************************
START-OF-SELECTION.
  IF P_UP = 'X'.
    PERFORM FRM_CHECH_FILENAME.             "检查文件名
    PERFORM FRM_UPLOAD.  "上传EXCEL数据到内表
    PERFORM FRM_GET_DATA. "处理数逻辑
    PERFORM FRM_DISPLAY. "ALV展示
  ELSE.
     "if P_DOFN  is initial.
      PERFORM FRM_GET_DO_FILE  CHANGING P_DOFN . "下载的搜索帮助

  "  endif.
     PERFORM FRM_DOWNLOAD.
   "  PERFORM FRM_DOWNLOAD2."下载模板
  ENDIF.

************************************************************************
* EVENT END-OF SELECTION
************************************************************************
END-OF-SELECTION.

************************************************************************
* EVENT  END-OF PAGE
************************************************************************
END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      FORM  FRM_CHECH_FILENAME
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_CHECH_FILENAME .
  IF P_UPFN IS INITIAL.
*    MESSAGE I010."主数据文件，路径和文件名，不能为空！
    MESSAGE '主数据文件，路径和文件名，不能为空！' TYPE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_UPLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_UPLOAD .
DATA OREF TYPE REF TO CX_ROOT.
  DATA LV_ERROR TYPE STRING.
  " GET FILE DATA
*  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
*    EXPORTING
*      FILENAME                = P_UPFN
*      FILETYPE                = 'ASC'
*      HAS_FIELD_SEPARATOR     = 'X'
**     HEADER_LENGTH           = 0
**     READ_BY_LINE            = 'X'
**     DAT_MODE                = SPACE
*      CODEPAGE                = '8400'
**     IGNORE_CERR             = ABAP_TRUE
**     REPLACEMENT             = '#'
**     VIRUS_SCAN_PROFILE      =
**    IMPORTING
**     FILELENGTH              = G_FILELENGTH
**     HEADER                  =
*    CHANGING
*      DATA_TAB                = IT_FILE
**     ISSCANPERFORMED         = SPACE
*    EXCEPTIONS
*      FILE_OPEN_ERROR         = 1
*      FILE_READ_ERROR         = 2
*      NO_BATCH                = 3
*      GUI_REFUSE_FILETRANSFER = 4
*      INVALID_TYPE            = 5
*      NO_AUTHORITY            = 6
*      UNKNOWN_ERROR           = 7
*      BAD_DATA_FORMAT         = 8
*      HEADER_NOT_ALLOWED      = 9
*      SEPARATOR_NOT_ALLOWED   = 10
*      HEADER_TOO_LONG         = 11
*      UNKNOWN_DP_ERROR        = 12
*      ACCESS_DENIED           = 13
*      DP_OUT_OF_MEMORY        = 14
*      DISK_FULL               = 15
*      DP_TIMEOUT              = 16
*      NOT_SUPPORTED_BY_GUI    = 17
*      ERROR_NO_GUI            = 18
*      OTHERS                  = 19.
*  IF SY-SUBRC <> 0.
**   IMPLEMENT SUITABLE ERROR HANDLING HERE
*    MESSAGE 'READ FILE ERROR!' TYPE 'E'.
*  ENDIF.

  DATA L_FILENAME TYPE RLGRAP-FILENAME.
  L_FILENAME = P_UPFN.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = L_FILENAME
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = IT_FILE
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE '文件导入错误！' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  TRY .
    LOOP AT IT_FILE.
     MOVE-CORRESPONDING IT_FILE TO IT_DATA.
     IT_DATA-STATUS = ICON_YELLOW_LIGHT.
     APPEND IT_DATA.
    ENDLOOP.
    CATCH CX_SY_CONVERSION_NO_NUMBER INTO OREF.
      CLEAR LV_ERROR.
      LV_ERROR = OREF->GET_TEXT( ) .
      MESSAGE LV_ERROR  TYPE 'E' .

  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
 IF IT_DATA[] IS NOT INITIAL.
  SELECT EBELN BSART KNUMV
     INTO CORRESPONDING FIELDS OF TABLE IT_EKKO  "根据已导入数据的采购订单号，再读取出采购订单、采购类型、单据条件数
     FROM EKKO
     FOR ALL ENTRIES IN IT_DATA
    WHERE EBELN = IT_DATA-EBELN .
 IF IT_EKKO[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_KONV
  FROM KONV
  FOR ALL ENTRIES IN IT_EKKO    "根据IT_EKKO读取到条件类型值
  WHERE KNUMV = IT_EKKO-KNUMV.
 ENDIF.
 ENDIF.
DATA:F1 TYPE C LENGTH 2 .
LOOP AT IT_DATA.

  F1 = IT_DATA-EBELN+0(2)....
*   IF F1 <>  '46'.
*      IT_DATA-STATUS = ICON_RED_LIGHT.     "判断以46开头采购订单才可批量修改
*      IT_DATA-MSG = '只能46开头采购订单可批量修改价格'.
*     ENDIF.
  READ TABLE IT_EKKO WITH KEY EBELN = IT_DATA-EBELN.
  IF SY-SUBRC = 0.
    IT_DATA-KNUMV = IT_EKKO-KNUMV.
     IF IT_EKKO-BSART = 'Z06' OR IT_EKKO-BSART = 'Z08'.
       else.
         IT_DATA-STATUS = ICON_RED_LIGHT.
         IT_DATA-MSG = '只能Z06或Z08采购订单类型可批量修改价格'. "判断Z06订单类型可批量修改
       ENDIF.
   ENDIF.
 READ TABLE IT_KONV WITH KEY KNUMV = IT_DATA-KNUMV KPOSN+1(5) = IT_DATA-EBELP   KSCHL = 'PB00'.
  IF SY-SUBRC = 0.
    IT_DATA-KSCHL = IT_KONV-KSCHL. "判断条件类型是'PB00 ,PBXX的条件类型
    ENDIF.
  READ TABLE IT_KONV WITH KEY KNUMV = IT_DATA-KNUMV KPOSN+1(5) = IT_DATA-EBELP KSCHL = 'PBXX'.
  IF SY-SUBRC = 0.
    IT_DATA-KSCHL = IT_KONV-KSCHL.
    ENDIF.
 MODIFY IT_DATA.
ENDLOOP.
 SORT IT_DATA BY EBELN EBELP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DISPLAY
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_DISPLAY .
  PERFORM INIT_LAYOUT.              "设置输出格式
  PERFORM INIT_SORT.                "设置排序、合计
  PERFORM INIT_VARIANT.             "设置变式控制
  PERFORM FRM_INIT_LVC.             " 初始化内表结构/ALV显示结构
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            IT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD .
 DATA: L_OBJDATA     LIKE WWWDATATAB,
        L_MIME        LIKE W3MIME,
        L_DESTINATION LIKE RLGRAP-FILENAME,
        L_OBJNAM      TYPE STRING,
        L_RC          LIKE SY-SUBRC,
        L_ERRTXT      TYPE STRING.

  DATA: L_FILENAME TYPE STRING,
        L_RESULT,
        L_SUBRC    TYPE SY-SUBRC.

  DATA: L_OBJID TYPE WWWDATATAB-OBJID .


  L_OBJID = 'ZMM017'.  "上传的模版名称

  "查找文件是否存在。
  SELECT SINGLE RELID OBJID
    FROM WWWDATA
    INTO CORRESPONDING FIELDS OF L_OBJDATA
    WHERE SRTF2    = 0
    AND   RELID    = 'MI'
    AND   OBJID    = L_OBJID.

  "判断模版不存在则报错
  IF SY-SUBRC NE 0 OR L_OBJDATA-OBJID EQ SPACE.
    CONCATENATE '模板文件：' L_OBJID '不存在，请用TCODE：SMW0进行加载'
    INTO L_ERRTXT.
    MESSAGE E000(SU) WITH L_ERRTXT.
  ENDIF.

  L_FILENAME = P_DOFN.

  "判断本地地址是否已经存在此文件。
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = L_FILENAME
    RECEIVING
      RESULT               = L_RESULT
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.
  IF L_RESULT EQ 'X'.  "如果存在则删除原始文件，重新覆盖
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
      EXPORTING
        FILENAME             = L_FILENAME
      CHANGING
        RC                   = L_SUBRC
      EXCEPTIONS
        FILE_DELETE_FAILED   = 1
        CNTL_ERROR           = 2
        ERROR_NO_GUI         = 3
        FILE_NOT_FOUND       = 4
        ACCESS_DENIED        = 5
        UNKNOWN_ERROR        = 6
        NOT_SUPPORTED_BY_GUI = 7
        WRONG_PARAMETER      = 8
        OTHERS               = 9.
    IF L_SUBRC <> 0. "如果删除失败，则报错。
      CONCATENATE '同名EXCEL文件已打开' '请关闭该EXCEL后重试。'
      INTO L_ERRTXT.
      MESSAGE E000(SU) WITH L_ERRTXT.
    ENDIF.
  ENDIF.

  L_DESTINATION   = P_DOFN.
  if  L_DESTINATION is not initial.
  "下载模版。
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = L_OBJDATA
      DESTINATION = L_DESTINATION
    IMPORTING
      RC          = L_RC.
  IF L_RC NE 0.
    CONCATENATE '模板文件' '下载失败' INTO L_ERRTXT.
    MESSAGE E000(SU) WITH L_ERRTXT.
  ENDIF.
 endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_PATH
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      <--P_P_UPFN  TEXT
*----------------------------------------------------------------------*
FORM FRM_GET_PATH  CHANGING P_FILENAME.
  " GET PATH FOR LOCAL FILE

  DATA: LT_FILE_TABLE TYPE FILETABLE,
        LW_FILE_TABLE TYPE FILE_TABLE,
        L_RC          TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = 'FILE PATH'
*     DEFAULT_EXTENSION       =
*     DEFAULT_FILENAME        =
*     FILE_FILTER             =
*     WITH_ENCODING           =
*     INITIAL_DIRECTORY       = 'E:\'
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = LT_FILE_TABLE
      RC                      = L_RC
*     USER_ACTION             =
*     FILE_ENCODING           =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC = 0.
    READ TABLE LT_FILE_TABLE INTO LW_FILE_TABLE INDEX 1.
    IF SY-SUBRC = 0.
      P_FILENAME = LW_FILE_TABLE-FILENAME.
    ENDIF.
  ELSE.
*   IMPLEMENT SUITABLE ERROR HANDLING HERE
  ENDIF.
ENDFORM.                    " FRM_GET_PATH
FORM FRM_GET_DO_FILE  CHANGING P_FULLPATH TYPE STRING.

  DATA: L_INIT_PATH  TYPE STRING,
        L_INIT_FNAME TYPE STRING,
        L_PATH       TYPE STRING,
        L_FILENAME   TYPE STRING,
        L_FULLPATH   TYPE STRING.

* 初始名称(输出的文件名称)
  L_INIT_FNAME = 'ZMM017_TEMPLATE.XLS'.

* 获取桌面路径
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY    = L_INIT_PATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

* 用户选择名称、路径
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
*     WINDOW_TITLE         = '指定保存文件名'
*     DEFAULT_EXTENSION    = 'DOC'
      DEFAULT_FILE_NAME    = L_INIT_FNAME
*     FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
*     FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_WORD
      INITIAL_DIRECTORY    = L_INIT_PATH
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = L_FILENAME
      PATH                 = L_PATH
      FULLPATH             = L_FULLPATH
*     USER_ACTION          =
*     FILE_ENCODING        =
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC = 0.
    P_FULLPATH = L_FULLPATH.
  ENDIF.
ENDFORM.
*       初始化LAYOUT参数
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
  GW_LAYOUT-SEL_MODE = 'A'.
*  GW_LAYOUT-EDIT_MODE = 'X'.
*  GW_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.
*  GW_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
*  GW_LAYOUT-INFO_FNAME = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  INIT_SORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  INIT_VARIANT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM INIT_VARIANT .
  CLEAR: GW_VARIANT.
  GW_VARIANT-REPORT = SY-REPID.
  GW_VARIANT-HANDLE = '0001'.

  CLEAR GW_GRID_SETTINGS.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
*  INIT_FIELDCAT 'BOX' TEXT-001 '2' '' 'X' 'X' 'X' '' ''.
  INIT_FIELDCAT 'STATUS' '状态' '' 'X' '' '' '' ''.
  INIT_FIELDCAT 'MSG' '日志' '' '' '' '' '' '' .
  INIT_FIELDCAT 'EBELN' '' '' '' '' 'EKPO' 'EBELN' '' .
  INIT_FIELDCAT 'EBELP' '' '' '' '' 'EKPO' 'EBELP' '' .
  INIT_FIELDCAT 'KBETR' '' '' '' '' 'KONV' 'KBETR' '' .

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
 CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = GT_EVENTS[].

ENDFORM.
*&      FORM  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
FORM FRM_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DELETE RT_EXTAB WHERE FCODE = '&ALL'.
  DELETE RT_EXTAB WHERE FCODE = '&SAL'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA:L_INDEX TYPE SY-TABIX.

  CASE R_UCOMM.
    WHEN '&IC1'."双击
      READ TABLE IT_DATA  INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        SET PARAMETER ID 'BES' FIELD IT_DATA-EBELN.   "选择屏字段ID
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN."填T-CODE
      ENDIF.
*    WHEN 'PRINT'."打印
*      PERFORM FRM_PRINT_SELECT.
*    WHEN '&ZALL'.
*      LOOP AT IT_DATA INTO WA_DATA.
*        L_INDEX = SY-TABIX.
*        WA_DATA-BOX = 'X'.
*        MODIFY IT_DATA FROM WA_DATA INDEX L_INDEX.
*      ENDLOOP.
*    WHEN '&ZSAL'.
*      LOOP AT IT_DATA INTO WA_DATA.
*        L_INDEX = SY-TABIX.
*        WA_DATA-BOX = ''.
*        MODIFY IT_DATA FROM WA_DATA INDEX L_INDEX.
*      ENDLOOP.
    WHEN 'IMPORT'.
      READ TABLE IT_DATA WITH KEY  STATUS = ICON_RED_LIGHT .
      IF SY-SUBRC = 0.
        MESSAGE '请根据行项目日志信息调整红灯状态的数据，再重新导入' TYPE 'E'.
       ELSE.
        PERFORM FRM_IMPORT_DATA. "批量修改me22n采购订单价格
       ENDIF.

    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      FORM  FRM_IMPORT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_IMPORT_DATA .

LOOP AT IT_DATA where status <> ICON_GREEN_LIGHT.

  CLEAR T_POITEM.
  CLEAR T_POITEMX.
  CLEAR T_CONDX.
  CLEAR T_COND.
  CLEAR T_RETURN.
  CLEAR T_POITEM[].
  CLEAR T_POITEMX[].
  CLEAR T_CONDX[].
  CLEAR T_COND[].
  CLEAR T_RETURN[].
T_POITEM-PO_ITEM = IT_DATA-EBELP.
T_POITEM-NET_PRICE = IT_DATA-KBETR / 10 .
APPEND T_POITEM.
 T_POITEMX-PO_ITEM = IT_DATA-EBELP.
T_POITEMX-NET_PRICE = 'X'.
T_POITEMX-PO_ITEMX = 'X'.
APPEND T_POITEMX.
T_COND-ITM_NUMBER = IT_DATA-EBELP.
T_COND-COND_TYPE = IT_DATA-KSCHL.    " GIVE THE PRICING CONDITION TYPE THAT DERIVES NETPRICE
T_COND-COND_VALUE = IT_DATA-KBETR / 10  .

T_COND-CHANGE_ID = 'U'.
"T_COND-COND_UPDAT = 'X'.
APPEND T_COND.
T_CONDX-ITM_NUMBER = IT_DATA-EBELP.
T_CONDX-ITM_NUMBERX = 'X'.
T_CONDX-COND_TYPE = 'X'.
T_CONDX-COND_VALUE = 'X'.
T_CONDX-CHANGE_ID = 'X'.
APPEND T_CONDX.
CALL FUNCTION 'BAPI_PO_CHANGE'
EXPORTING
PURCHASEORDER = IT_DATA-EBELN
TABLES
RETURN = T_RETURN
POITEM = T_POITEM
POITEMX = T_POITEMX
POCOND = T_COND
POCONDX = T_CONDX.
READ TABLE T_RETURN WITH KEY TYPE = 'E'.
 IF SY-SUBRC = 0.
   IT_DATA-MSG = T_RETURN-MESSAGE.
   IT_DATA-STATUS = ICON_RED_LIGHT.
 ELSE.
    IT_DATA-STATUS = ICON_GREEN_LIGHT.
    IT_DATA-MSG = '该行项目价格已修改'.
     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
       WAIT = 'X'.
   wait up to 5 seconds.
  ENDIF.

MODIFY IT_DATA.
 ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD2 .
DATA:row type i ,
        col type i .
  DATA:
           LV_SHEET_NUMBER TYPE I, "打开的EXCEL的SHEET的名字
           LV_NAME      LIKE WWWDATATAB,
           LV_MIME      LIKE W3MIME OCCURS 10,
           LV_FILENAME  TYPE STRING,    "文件名
           LV_PATH      TYPE STRING,    "路径
           LV_FULLPATH  TYPE STRING,    "全路径
           LV_TITLE     TYPE STRING.

  LV_NAME-RELID = 'MI'.


  "pv_mname.       "***************这里给定模板的名字*******************\

  row = 2.
  col = 1.
  LV_NAME-OBJID = 'ZMM017'.
  LV_NAME-TEXT = '采购订单批量修改价格模板'.
  LV_TITLE =  '采购订单批量修改价格模板'.

  "LV"_NAME-TEXT = SY-TITLE.

  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      KEY               = LV_NAME
    TABLES
      MIME              = LV_MIME
    EXCEPTIONS
      WRONG_OBJECT_TYPE = 1
      IMPORT_ERROR      = 2
      OTHERS            = 3.
  IF SY-SUBRC <> 0.
    STOP.
  ENDIF.
  " LV_TITLE = SY-TITLE.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = LV_TITLE
      DEFAULT_EXTENSION    = 'xls'
      DEFAULT_FILE_NAME    = LV_TITLE
      FILE_FILTER          = '(电子表格EXCEL)'
    CHANGING
      FILENAME             = LV_FILENAME
      PATH                 = LV_PATH
      FULLPATH             = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    STOP.
  ENDIF.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME = LV_FULLPATH
      FILETYPE = 'BIN'
    TABLES
      DATA_TAB = LV_MIME.
  IF SY-SUBRC = 0.
    MESSAGE '导出模板成功.' TYPE 'I'.
  ENDIF.
ENDFORM.
