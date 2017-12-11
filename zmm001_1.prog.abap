*&---------------------------------------------------------------------*
*& 程序名称:ZMM001
*& 作者    :汪昱
*& 开发日期:
*& 请求号  :
*& 描述    :物料主数据导入程序
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*
REPORT ZMM001_1.


TYPE-POOLS: SLIS.

* COL.
TABLES: MLGN,
        T001L,
        T100. "MESSAGE
*----------------------------------------------------------------------*
*ALV data declarations
*----------------------------------------------------------------------*

DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
      GD_REPID     LIKE SY-REPID.

DATA: TIND(4) TYPE N.
DATA: ZWFELD(19).
FIELD-SYMBOLS: <FS1>.

*分类视图
DATA:
  IT_NUM  LIKE BAPI1003_ALLOC_VALUES_NUM  OCCURS 0 WITH HEADER LINE,
  IT_CHAR LIKE BAPI1003_ALLOC_VALUES_CHAR OCCURS 0 WITH HEADER LINE,
  IT_CURR LIKE BAPI1003_ALLOC_VALUES_CURR OCCURS 0 WITH HEADER LINE,
  IT_RET  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF LOG_RETURN OCCURS 0,
        MATNR   LIKE MARA-MATNR,
        MESSAGE TYPE BAPI_MSG,
      END OF LOG_RETURN.

DATA  RETURN   TYPE BAPIRET2.

*基本视图
DATA: BEGIN OF I_DATA1 OCCURS 0,
        MATNR(18)       TYPE C, "物料号MARA-MATNR
        GGXH(30)        TYPE C, "规格型号
        XSJJ(30)        TYPE C, "相素间距
        ROHS(30)        TYPE C, "ROHS认证
        SFYGD(30)       TYPE C, "是否亿光灯
        CHANGJIA(30)    TYPE C, "产家
        DHH1(30)        TYPE C, "订货单
        TZZ(30)         TYPE C, "特征值
        CPLX(30)        TYPE C, "产品类型
        SHENQINGREN(30) TYPE C, "申请人
        TYPE(10)        TYPE C,
        MESSAGE(300)    TYPE C,
      END OF I_DATA1.

DATA T_DATA1 LIKE I_DATA1 OCCURS 0 WITH HEADER LINE. "用于ALV显示

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS: MATERIAL LIKE RLGRAP-FILENAME OBLIGATORY MEMORY ID ZMM001_1. "物料导入摸版
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR MATERIAL.
  PERFORM SELECT_PATH."选择路径
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.
  IF NOT MATERIAL IS INITIAL.
    PERFORM UPLOAD_MATERIAL_DATA."数据导入

    PERFORM BUILD_FIELDCATALOG.

    PERFORM BUILD_LAYOUT.

    PERFORM DISPLAY_ALV_REPORT TABLES T_DATA1.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form SELECT_PATH
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*

FORM SELECT_PATH .
  DATA V_MATERIAL LIKE RLGRAP-FILENAME.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.* ,*.*.'
      MODE             = '0'
      TITLE            = '请选择要上传的信息文件'
    IMPORTING
      FILENAME         = V_MATERIAL
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MATERIAL  =    V_MATERIAL.

ENDFORM. " SELECT_PATH
FORM UPLOAD_MATERIAL_DATA.
*分类视图
  PERFORM FRM_XLS_TO_SAP TABLES I_DATA1.
  PERFORM FRM_FILL_DATA1.
ENDFORM. " UPLOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG.
  PERFORM FIELD_CAT USING :
    'TYPE'    '消息类型' '',
    'MESSAGE' '消息内容' '',
    'MATNR'   '物料号'   '',
    'GGXH'    '规格型号' '',
    'XSJJ'    '相素间距' '',
    'ROHS'     'ROHS认证' '',
    'SFYGD'    '是否亿光灯' '',
    'CHANGJIA' '产家' '',
    'DHH1'     '订货单' '',
    'TZZ'      '特征值' '',
    'CPLX'      '产品类型' '',
    'SHENQINGREN' '申请人' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_LAYOUT
*&---------------------------------------------------------------------*
* Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT.

* GD_LAYOUT-NO_INPUT = ''.<

  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GD_LAYOUT-ZEBRA = 'X'.

* GD_LAYOUT-GROUP_CHANGE_EDIT = 'X'.

  GD_LAYOUT-HEADER_TEXT = '导入结果查询'.
ENDFORM. " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
* Display report using ALV grid
*----------------------------------------------------------------------*

FORM DISPLAY_ALV_REPORT TABLES TDATA.
  GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GD_REPID
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_grid_title       = outtext
      IS_LAYOUT          = GD_LAYOUT
      IT_FIELDCAT        = FIELDCATALOG[]
*     it_special_groups  = gd_tabgroup
*     IT_EVENTS          = GT_XEVENTS
      I_SAVE             = 'X'
*     is_variant         = z_template
    TABLES
      T_OUTTAB           = TDATA
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " DISPLAY_ALV_REPORT

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* -->UCOMM text
* -->SELFIELD text
*----------------------------------------------------------------------*

FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM.


ENDFORM. "USER_COMMAND

FORM INPUT USING PIN CHANGING POUT.

  IF PIN <> ' '.
    IF PIN = '#'.
      POUT = ' '.
    ELSE.
      POUT = PIN.
    ENDIF.
  ENDIF.

ENDFORM.

FORM INPUT1 USING PIN CHANGING POUT.
* 针对更新标识的勾选
  IF PIN IS NOT INITIAL.
    POUT = 'X'.
  ELSE.
    CLEAR POUT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3775   text
*      -->P_3776   text
*      -->P_3777   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    PNAME
                         PTEXT
                         PKEY.
  CLEAR FIELDCATALOG.
  FIELDCATALOG-FIELDNAME = PNAME.
  FIELDCATALOG-SELTEXT_M = PTEXT.
  FIELDCATALOG-KEY       = PKEY.
  APPEND FIELDCATALOG TO FIELDCATALOG.

ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_DATA1  text
*----------------------------------------------------------------------*
FORM FRM_XLS_TO_SAP  TABLES P_I_DATA .

  REFRESH  P_I_DATA[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = MATERIAL
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 3
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = P_I_DATA[]
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN  '1'.
        MESSAGE '存在错误的传入参数' TYPE 'E'.
      WHEN  '2'.
        MESSAGE 'OLE控件错误，请检查EXCEL插件及系统' TYPE 'E'.
      WHEN  '3'.
        MESSAGE '数据上载出错' TYPE 'E'.
      WHEN OTHERS.
    ENDCASE.

*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF P_I_DATA[] IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.
ENDFORM.                    " FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA1 .
*当创建成功的时候，扩建分类视图
  DATA   L_OBJECT LIKE BAPI1003_KEY-OBJECT.

  LOOP AT I_DATA1.
    L_OBJECT = I_DATA1-MATNR.
*读取该物料有没扩充分类视图
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        OBJECTKEY       = L_OBJECT
        OBJECTTABLE     = 'MARA'
        CLASSNUM        = 'LYD001'
        CLASSTYPE       = '001'
      TABLES
        ALLOCVALUESNUM  = IT_NUM
        ALLOCVALUESCHAR = IT_CHAR
        ALLOCVALUESCURR = IT_CURR
        RETURN          = IT_RET.

    REFRESH:IT_NUM[],
            IT_CHAR[],
            IT_CURR[].
    CLEAR:IT_NUM,
          IT_CHAR,
          IT_CURR.
*分类特性
    IT_CHAR-CHARACT = 'GGXH'.
    IT_CHAR-VALUE_CHAR = I_DATA1-GGXH.
    APPEND IT_CHAR.

    IT_CHAR-CHARACT    = 'XSJJ'.
    IT_CHAR-VALUE_CHAR = I_DATA1-XSJJ.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT    = 'ROHS'.
    IT_CHAR-VALUE_CHAR = I_DATA1-ROHS.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT    = 'SFYGD'.
    IT_CHAR-VALUE_CHAR = I_DATA1-SFYGD.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT    = 'CHANGJIA'.
    IT_CHAR-VALUE_CHAR = I_DATA1-CHANGJIA.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT    = 'DHH'.
    IT_CHAR-VALUE_CHAR = I_DATA1-DHH1.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT    = 'TEZZ'.
    IT_CHAR-VALUE_CHAR = I_DATA1-TZZ.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT = 'CPLX'.
    IT_CHAR-VALUE_CHAR = I_DATA1-CPLX.
    APPEND IT_CHAR .

    IT_CHAR-CHARACT = 'SHENQINGREN'.
    IT_CHAR-VALUE_CHAR = I_DATA1-SHENQINGREN.
    APPEND IT_CHAR .


*      IT_CHAR-CHARACT = 'SQR'.
*      IT_CHAR-VALUE_CHAR = I_DATA1-CPLX.
*      APPEND IT_CHAR .

    READ TABLE IT_RET WITH KEY TYPE = 'S'
                               ID = 'CL'
                               NUMBER = 731.
*当没有创建的，进入修改模式。
    IF SY-SUBRC <> 0.
      REFRESH IT_RET[].
      CALL FUNCTION 'BAPI_OBJCL_CREATE'
        EXPORTING
          OBJECTKEYNEW      = L_OBJECT
          OBJECTTABLENEW    = 'MARA'
          CLASSNUMNEW       = 'LYD001'
          CLASSTYPENEW      = '001'
          STATUS            = '1'
*         STANDARDCLASS     =
*         CHANGENUMBER      = 'LYD001'
*         KEYDATE           = SY-DATUM
          NO_DEFAULT_VALUES = 'X'
*         CLASSIF_STATUS    =
        TABLES
          ALLOCVALUESNUM    = IT_NUM
          ALLOCVALUESCHAR   = IT_CHAR
          ALLOCVALUESCURR   = IT_CURR
          RETURN            = IT_RET.

      READ TABLE IT_RET WITH KEY TYPE = 'E'.
      IF SY-SUBRC  <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

*执行成功表示物料创建成功
        I_DATA1-TYPE    = RETURN-TYPE.
        I_DATA1-MESSAGE = RETURN-MESSAGE.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        I_DATA1-TYPE    = IT_RET-TYPE.
        I_DATA1-MESSAGE = IT_RET-MESSAGE.
      ENDIF.
    ELSE.

*修改模式
      REFRESH IT_RET[].
      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
        EXPORTING
          OBJECTKEY          = L_OBJECT
          OBJECTTABLE        = 'MARA'
          CLASSNUM           = 'LYD001'
          CLASSTYPE          = '001'
*         STATUS             = '1'
*         STANDARDCLASS      =
*         CHANGENUMBER       =
*         KEYDATE            = SY-DATUM
          NO_DEFAULT_VALUES  = 'X'
*         KEEP_SAME_DEFAULTS = ' '
*                IMPORTING
*         CLASSIF_STATUS     =
        TABLES
          ALLOCVALUESNUMNEW  = IT_NUM
          ALLOCVALUESCHARNEW = IT_CHAR
          ALLOCVALUESCURRNEW = IT_CURR
          RETURN             = IT_RET.

      READ TABLE IT_RET WITH KEY TYPE = 'E'.
      IF SY-SUBRC  <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

*执行成功表示物料创建成功
        I_DATA1-TYPE    = IT_RET-TYPE.
        I_DATA1-MESSAGE = IT_RET-MESSAGE.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        I_DATA1-TYPE    = IT_RET-TYPE.
        I_DATA1-MESSAGE = IT_RET-MESSAGE.
      ENDIF.
    ENDIF.
**********************************************************************
    LOG_RETURN-MATNR   = I_DATA1-MATNR."日志增加对应得物料号
    LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
    APPEND LOG_RETURN.

    APPEND I_DATA1 TO T_DATA1.
  ENDLOOP.

**********************************************************************
  LOG_RETURN-MATNR   = I_DATA1-MATNR."日志增加对应得物料号
  LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
  APPEND LOG_RETURN.
ENDFORM.
