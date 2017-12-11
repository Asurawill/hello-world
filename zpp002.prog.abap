*&---------------------------------------------------------------------*
*& Report  ZPP002
*&程序名称：工艺路线批导
*&开发日期：2014-08-15
*&创建者：  李权
*&
*&---------------------------------------------------------------------*
*&概要说明
*&
*&---------------------------------------------------------------------*
*&变更记录
*&
*&---------------------------------------------------------------------*

REPORT ZPP002.

TYPES:BEGIN OF TY_UPLOAD_DATA,
        TASK_LIST_GROUP        TYPE BAPI1012_TSK_C-TASK_LIST_GROUP, " 任务清单组码
        GROUP_COUNTER          TYPE BAPI1012_TSK_C-GROUP_COUNTER,   "组计数器
        PLANT                  TYPE BAPI1012_TSK_C-PLANT,           "工艺路线所属工厂
        DESCRIPTION            TYPE BAPI1012_TSK_C-DESCRIPTION,     "任务清单描述
        MATNR                  TYPE RC27M-MATNR, " 物料
        WERKS                  TYPE RC27M-WERKS, " 物料工厂
        MAKTX                  TYPE MAKT-MAKTX,  "物料描述
        STTAG                  TYPE RC271-STTAG, "关键日期
        VERWE                  TYPE PLKOD-VERWE, " 用途(任务清单使用)
        STATU                  TYPE PLKOD-STATU, " 状态
        VORNR                  TYPE OPR_CLASS_DATA_INTERFACE-VORNR, " 工序号
        ARBPL                  TYPE OPR_CLASS_DATA_INTERFACE-ARBPL, " 工作中心
        PLANT02                TYPE BAPI1012_OPR_C-PLANT,           " 工作中心工厂
        STEUS                  TYPE OPR_CLASS_DATA_INTERFACE-STEUS, " 控制码
*        KTSCH                  TYPE BAPI1012_OPR_C-DESCRIPTION,     "工序描述
        LTXA1                  TYPE OPR_CLASS_DATA_INTERFACE-LTXA1, " 工序短文本
*        STANDARD_TEXT_KEY      TYPE BAPI1012_OPR_C-STANDARD_TEXT_KEY, "标准文本码
        BASE_QUANTITY          TYPE BAPI1012_OPR_C-BASE_QUANTITY,   " 基本数量
        OPERATION_MEASURE_UNIT TYPE BAPI1012_OPR_C-OPERATION_MEASURE_UNIT, "工序单位
        VGW01                  TYPE OPR_CLASS_DATA_INTERFACE-VGW01, " 标准值1
        VGE01                  TYPE OPR_CLASS_DATA_INTERFACE-VGE01, " 单位
        VGW02                  TYPE OPR_CLASS_DATA_INTERFACE-VGW02, " " 标准值2
        VGE02                  TYPE OPR_CLASS_DATA_INTERFACE-VGE02, " 单位
        VGW03                  TYPE OPR_CLASS_DATA_INTERFACE-VGW03, " " 标准值3
        VGE03                  TYPE OPR_CLASS_DATA_INTERFACE-VGE03, " 单位
        VGW04                  TYPE OPR_CLASS_DATA_INTERFACE-VGW04, " " 标准值4
        VGE04                  TYPE OPR_CLASS_DATA_INTERFACE-VGE04, " 单位
        VGW05                  TYPE OPR_CLASS_DATA_INTERFACE-VGW05, " " 标准值5
        VGE05                  TYPE OPR_CLASS_DATA_INTERFACE-VGE05, " 单位
*        userfields_keyword_id type bapi1012_opr_c-userfields_keyword_id, " 自定义字段码
*        slwid                 TYPE opr_class_data_interface-slwid,
*        usr06                 type opr_class_data_interface-usr06,
*        usr07                 type opr_class_data_interface-usr07,


      END OF TY_UPLOAD_DATA.

DATA:GT_UPLOAD_DATA TYPE STANDARD TABLE OF TY_UPLOAD_DATA,
     GW_UPLOAD_DATA TYPE                   TY_UPLOAD_DATA.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:P_FILE TYPE RLGRAP-FILENAME  ."导入文件

SELECTION-SCREEN END OF BLOCK B1.

IF P_FILE IS INITIAL .
  MESSAGE '请选择导入文件' TYPE 'E'.
ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_BROWSER_FILE.

START-OF-SELECTION.
  PERFORM FRM_UPLOAD_DATA.
  PERFORM FRM_BAPI.

*&---------------------------------------------------------------------*
*&      Form  FRM_BROWSER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_BROWSER_FILE .
  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
*   EXPORTING
*     DEF_FILENAME           = ' '
*     DEF_PATH               = ' '
*     MASK                   = ' '
*     MODE                   = ' '
*     TITLE                  = ' '
    IMPORTING
      FILENAME         = P_FILE
*     PATH             =
*     FILE             =
    EXCEPTIONS
      SELECTION_CANCEL = 1
      SELECTION_ERROR  = 2
      OTHERS           = 3.
  IF SY-SUBRC <> 0.
  ENDIF.
ENDFORM.                    " FRM_BROWSER_FILE

*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_UPLOAD_DATA .
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = GT_UPLOAD_DATA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_BAPI .
  TYPES:BEGIN OF TY_MATNR,
          TASK_LIST_GROUP TYPE BAPI1012_TSK_C-TASK_LIST_GROUP, " 任务清单组码
          GROUP_COUNTER   TYPE BAPI1012_TSK_C-GROUP_COUNTER, " 组计数器
          DESCRIPTION     TYPE BAPI1012_TSK_C-DESCRIPTION, " 任务清单描述
          PLANT           TYPE BAPI1012_TSK_C-PLANT, " 工艺路线所属工厂
          MATNR           TYPE RC27M-MATNR,  " 物料
          WERKS           TYPE RC27M-WERKS, "  物料工厂
          VERWE           TYPE PLKOD-VERWE,  " 用途
          STATU           TYPE PLKOD-STATU, " 状态
          STTAG           TYPE RC271-STTAG, "　关键日期
        END OF TY_MATNR.

  DATA:LT_MATNR TYPE STANDARD TABLE OF TY_MATNR WITH HEADER LINE.
  DATA:LW_MATNR TYPE TY_MATNR.

  DATA:LT_MATNR2 TYPE STANDARD TABLE OF TY_MATNR WITH HEADER LINE.
  DATA:LW_MATNR2 TYPE TY_MATNR.

  DATA:L_MEINS TYPE MARA-MEINS.

  DATA:LT_RETURN TYPE STANDARD TABLE OF BAPIRET2  WITH HEADER LINE.
  DATA:L_MSG TYPE STRING.
  CLEAR L_MSG.
  LOOP AT  GT_UPLOAD_DATA INTO GW_UPLOAD_DATA. "   ，按物料号、工厂、用途、状态汇总记录条目
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-MATNR
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-MATNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-TASK_LIST_GROUP
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-TASK_LIST_GROUP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-GROUP_COUNTER
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-GROUP_COUNTER.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-VORNR
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-VORNR.

    " 检查规则
    SELECT COUNT(*)
      FROM MAPL
      WHERE MATNR = GW_UPLOAD_DATA-MATNR
      AND WERKS = GW_UPLOAD_DATA-WERKS
      AND PLNTY = 'N'
      AND LOEKZ <> 'X'.
*      AND plnnr = gw_upload_data-task_list_group. " 任务清单组码
*    BREAK-POINT.
*      AND plnal =  gw_upload_data-group_counter." 组计数器
    IF SY-SUBRC = 0.
      CLEAR L_MSG.
      CONCATENATE '物料' GW_UPLOAD_DATA-MATNR '/' '工厂' GW_UPLOAD_DATA-WERKS '/'
       '已经存在工艺路线!'  INTO L_MSG.
      MESSAGE L_MSG TYPE 'E'.
    ENDIF.

    MOVE-CORRESPONDING GW_UPLOAD_DATA TO LT_MATNR.
    APPEND LT_MATNR.
    CLEAR LT_MATNR.
    MODIFY GT_UPLOAD_DATA FROM GW_UPLOAD_DATA.
  ENDLOOP.



  LT_MATNR2[] = LT_MATNR[].

*  DATA:lt_result LIKE STANDARD TABLE OF bapiret2 WITH HEADER LINE.

  DATA:LT_OPR TYPE STANDARD TABLE OF OPR_CLASS_DATA_INTERFACE WITH HEADER LINE.
  " 按组,组计数器
  SORT LT_MATNR BY  WERKS MATNR . " 删除重复数据
  DELETE ADJACENT DUPLICATES FROM LT_MATNR COMPARING  WERKS MATNR.

*  " 按组,组计数器,工厂,物料编码
*  SORT LT_MATNR2 BY TASK_LIST_GROUP GROUP_COUNTER WERKS MATNR  .
*  DELETE ADJACENT DUPLICATES FROM LT_MATNR2 COMPARING TASK_LIST_GROUP GROUP_COUNTER WERKS MATNR.
*

*调用BAPI 物料号、工厂、用途、状态 分组导入
  LOOP AT LT_MATNR INTO LW_MATNR.

    DATA:IT_TASK LIKE TABLE OF BAPI1012_TSK_C WITH HEADER LINE.
    DATA:IT_MATASK LIKE TABLE OF BAPI1012_MTK_C WITH HEADER LINE.
    DATA:IT_OPERATION LIKE TABLE OF BAPI1012_OPR_C WITH HEADER LINE.
    DATA:IT_SEQUENCE LIKE TABLE OF BAPI1012_SEQ_C WITH HEADER LINE.

    CLEAR:IT_TASK,IT_TASK[].
    IT_TASK-TASK_LIST_GROUP  = LW_MATNR-TASK_LIST_GROUP. " 任务清单组码
    IT_TASK-GROUP_COUNTER    = LW_MATNR-GROUP_COUNTER. " 组计数器
    IT_TASK-DESCRIPTION      = LW_MATNR-DESCRIPTION." 任务清单描述
    IT_TASK-TASK_LIST_USAGE  = LW_MATNR-VERWE. " 任务清单使用
    IT_TASK-TASK_LIST_STATUS = LW_MATNR-STATU. " 状态
    IT_TASK-PLANT            = LW_MATNR-PLANT. " 工艺路线所属工厂

    SELECT SINGLE MEINS
      INTO IT_TASK-TASK_MEASURE_UNIT   " 取得物料单位
      FROM MARA
      WHERE MATNR = LW_MATNR-MATNR.

    IT_TASK-DESCRIPTION   =  LW_MATNR-DESCRIPTION.
    IT_TASK-VALID_FROM    = LW_MATNR-STTAG ." 关键日期
    IT_TASK-VALID_TO_DATE = '99991231'.

    APPEND IT_TASK.

    CLEAR:IT_MATASK,IT_MATASK[].
    CLEAR:IT_MATASK.
    IT_MATASK-MATERIAL        = LW_MATNR-MATNR.
    IT_MATASK-PLANT           = LW_MATNR-WERKS.           " 物料工厂
    IT_MATASK-VALID_FROM      = LW_MATNR-STTAG.           " 关键日期
    IT_MATASK-TASK_LIST_GROUP = LW_MATNR-TASK_LIST_GROUP. " 组计数器
    IT_MATASK-GROUP_COUNTER   = LW_MATNR-GROUP_COUNTER.   " 组计数器
    IT_MATASK-VALID_TO_DATE   = '99991231'.
    APPEND IT_MATASK.

    CLEAR:L_MEINS.
    " 取得到单位
    SELECT SINGLE MEINS
      INTO L_MEINS
      FROM MARA
      WHERE MATNR = LW_MATNR-MATNR.

    CLEAR:IT_OPERATION,IT_OPERATION[].

*    LOOP AT GT_UPLOAD_DATA INTO GW_UPLOAD_DATA WHERE TASK_LIST_GROUP = LW_MATNR-TASK_LIST_GROUP
*                                                 AND GROUP_COUNTER = LW_MATNR-GROUP_COUNTER.
    LOOP AT GT_UPLOAD_DATA INTO GW_UPLOAD_DATA WHERE MATNR = LW_MATNR-MATNR
                                               AND   WERKS = LW_MATNR-WERKS..
      CLEAR:IT_OPERATION.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = gw_upload_data-vornr
*        IMPORTING
*          output = gw_upload_data-vornr.

      IT_OPERATION-TASK_LIST_GROUP = GW_UPLOAD_DATA-TASK_LIST_GROUP. " 任务清单组码
      IT_OPERATION-GROUP_COUNTER   = GW_UPLOAD_DATA-GROUP_COUNTER.     " 组计数器
      IT_OPERATION-SEQUENCE_NO     = '000000'.
      IT_OPERATION-VALID_FROM      = GW_UPLOAD_DATA-STTAG.
      IT_OPERATION-VALID_TO_DATE   = '99991231'.
      IT_OPERATION-ACTIVITY        = GW_UPLOAD_DATA-VORNR.             " 操作/活动编号
*       it_operation-operation_id = gw_upload_data-vornr. " ''. " 工序 ID
      IT_OPERATION-WORK_CNTR = GW_UPLOAD_DATA-ARBPL.            " 工作中心
      IT_OPERATION-PLANT = GW_UPLOAD_DATA-PLANT02.              " 工作中心工厂
      IT_OPERATION-CONTROL_KEY = GW_UPLOAD_DATA-STEUS..         " 控制码
*      IT_OPERATION-STANDARD_TEXT_KEY = GW_UPLOAD_DATA-STANDARD_TEXT_KEY. " 标准文本码

**      SELECT SINGLE TXT
**        INTO GW_UPLOAD_DATA-LTXA1
**        FROM T435T
**        WHERE VLSCH = GW_UPLOAD_DATA-STANDARD_TEXT_KEY.

      IT_OPERATION-DESCRIPTION   = GW_UPLOAD_DATA-LTXA1.          " 工序短文本
      IT_OPERATION-BASE_QUANTITY = GW_UPLOAD_DATA-BASE_QUANTITY. " 基本数量

      IT_OPERATION-COST_RELEVANT = 'X'. " 与成本核算有关的标识
      IT_OPERATION-OPERATION_MEASURE_UNIT =  L_MEINS. " 单位

      IT_OPERATION-DENOMINATOR   = '1'. " 用于转换工艺路线和工序单位的分母
      IT_OPERATION-NOMINATOR     = '1'. " 用于转换任务清单和工序计量单位的计数器
      IT_OPERATION-BASE_QUANTITY =   GW_UPLOAD_DATA-BASE_QUANTITY . " 基本数量

      IF GW_UPLOAD_DATA-VGE01 IS NOT INITIAL
        AND GW_UPLOAD_DATA-VGW01 IS NOT INITIAL.
        IT_OPERATION-ACTTYPE_01   = 'HR00'. " 活动类型
        IT_OPERATION-STD_UNIT_01  = GW_UPLOAD_DATA-VGE01. " 标准值计量单位
        IT_OPERATION-STD_VALUE_01 = GW_UPLOAD_DATA-VGW01. " 标准值
      ENDIF.

      IF GW_UPLOAD_DATA-VGE02 IS NOT INITIAL
       AND GW_UPLOAD_DATA-VGW02 IS NOT INITIAL.
        IT_OPERATION-ACTTYPE_02   = 'MC00'. " 活动类型
        IT_OPERATION-STD_UNIT_02  = GW_UPLOAD_DATA-VGE02. " 标准值计量单位
        IT_OPERATION-STD_VALUE_02 = GW_UPLOAD_DATA-VGW02. " 标准值
      ENDIF.

      IF GW_UPLOAD_DATA-VGE03 IS NOT INITIAL
           AND GW_UPLOAD_DATA-VGW03 IS NOT INITIAL.
        IT_OPERATION-ACTTYPE_03   = 'EY00'. " 活动类型
        IT_OPERATION-STD_UNIT_03  = GW_UPLOAD_DATA-VGE03. " 标准值计量单位
        IT_OPERATION-STD_VALUE_03 = GW_UPLOAD_DATA-VGW03. " 标准值
      ENDIF.

      IF GW_UPLOAD_DATA-VGE04 IS NOT INITIAL
           AND GW_UPLOAD_DATA-VGW04 IS NOT INITIAL.
        IT_OPERATION-ACTTYPE_04   = 'ZZ00'. " 活动类型
        IT_OPERATION-STD_UNIT_04  = GW_UPLOAD_DATA-VGE04. " 标准值计量单位
        IT_OPERATION-STD_VALUE_04 = GW_UPLOAD_DATA-VGW04. " 标准值
      ENDIF.


      IF GW_UPLOAD_DATA-VGE05 IS NOT INITIAL.
        IT_OPERATION-ACTTYPE_05 = ''. " 活动类型
        IT_OPERATION-STD_UNIT_05 = GW_UPLOAD_DATA-VGE05. " 标准值计量单位
        IT_OPERATION-STD_VALUE_05 = GW_UPLOAD_DATA-VGW05. " 标准值
      ENDIF.

*      it_operation-userfields_keyword_id  = gw_upload_data-userfields_keyword_id. "
*      it_operation-userfield_curr_06  =    gw_upload_data-usr06.  "
*      it_operation-userfield_curr_07  =    gw_upload_data-usr07.  "
      APPEND IT_OPERATION.

    ENDLOOP.

    CALL FUNCTION 'BAPI_ROUTING_CREATE'
*      EXPORTING
*        TESTRUN                      = 'X'
*       PROFILE                      =
*       BOMUSAGE                     =
*       APPLICATION                  =
*     IMPORTING
*       GROUP                        =
*       GROUPCOUNTER                 =
      TABLES
        TASK                   = IT_TASK
        MATERIALTASKALLOCATION = IT_MATASK
*       sequence               = it_sequence
        OPERATION              = IT_OPERATION[]
*       SUBOPERATION           =
*       REFERENCEOPERATION     =
*       WORKCENTERREFERENCE    =
*       COMPONENTALLOCATION    =
*       PRODUCTIONRESOURCE     =
*       INSPCHARACTERISTIC     =
*       TEXTALLOCATION         =
*       TEXT                   =
        RETURN                 = LT_RETURN[].

    LOOP AT LT_RETURN. " 打印反馈信息
      IF LT_RETURN-TYPE = 'E' OR LT_RETURN-TYPE = 'A'.
        " 如果报序列号无效，请重新分配组和组计数器,记得补零哦
        ROLLBACK WORK.
        WRITE:/
        LT_RETURN-TYPE,
        LW_MATNR-MATNR,
        LT_RETURN-ID,
        LT_RETURN-NUMBER,
        LT_RETURN-MESSAGE,
        LT_RETURN-MESSAGE_V1 ,
        LT_RETURN-MESSAGE_V2 ,
        LT_RETURN-MESSAGE_V3 ,
        LT_RETURN-MESSAGE_V4 ,
          '调用工艺路线 BAPI 出错'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = SPACE.         " dont pass X

        WRITE:/ '导入完毕'  .
      ENDIF.
    ENDLOOP.

    REFRESH:IT_OPERATION[],
            LT_RETURN[],
            IT_TASK,
            IT_MATASK.
  ENDLOOP.

ENDFORM.                    " FRM_BAPI
