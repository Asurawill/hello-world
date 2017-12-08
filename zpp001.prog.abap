*&---------------------------------------------------------------------*
*& Report  ZPP001
*&程序名称：标准BOM批导
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

REPORT ZPP001.

TYPES:BEGIN OF TY_UPLOAD_DATA,
        " 抬头
        PLANT      LIKE CSAP_MBOM-WERKS, " 工厂
*        material   LIKE csap_mbom-matnr, " 物料号
        MATERIAL   LIKE MARA-MATNR,
        BOM_USAGE  LIKE CSAP_MBOM-STLAN , " 用途
        STLAL      LIKE CSAP_MBOM-STLAL , "可选BOM
        BOM_STATUS LIKE STKO_API01-BOM_STATUS, " BOM 状态
*        LABORATORY LIKE STKO_API01-LABORATORY, " 办公室（阶段）
        VALID_FROM LIKE CSAP_MBOM-DATUV,         " 生效日期
        BASE_QUAN  LIKE STKO_API01-BASE_QUAN,   " 基准数量(BTCI)
        BASE_UNIT  LIKE STKO_API01-BASE_UNIT,   " BOM 基本单位
        ITEM_NO    LIKE STPO_API03-ITEM_NO,     " 项目
        ITEM_CATEG LIKE STPO_API03-ITEM_CATEG,  " 项目类别
*        component  LIKE stpo_api03-component,  " 组件编码
        COMPONENT  LIKE MARA-MATNR,
        COMP_QTY   LIKE STPO-MENGE,             " 组件数量
        COMP_UNIT  LIKE STPO_API03-COMP_UNIT,   "计量单位
*        REL_COST   LIKE STPO_API03-REL_COST,   "成本核算相关标志
*        item_text1 LIKE stpo_api03-item_text1, " BOM 项目文本（行1）
        STR(1000)  TYPE C,
        COMP_SCRAP LIKE STPO_API03-COMP_SCRAP, " 部件废品用百分比表示(BTCI)
        AI_GROUP   LIKE STPO_API03-AI_GROUP,   "替代项目：组
        AI_PRIO    LIKE STPO_API03-AI_PRIO,    "优先级
        AI_STRATEG LIKE STPO_API03-AI_STRATEG, " 替代项目：策略
        USAGE_PROB LIKE STPO_API03-USAGE_PROB, " 使用可能性按 % (BTCI)
        SPPROCTYPE LIKE STPO_API03-SPPROCTYPE, " 特殊获取
        ISSUE_LOC  LIKE STPO_API03-ISSUE_LOC , "生产仓储地点
        FIXED_QTY  LIKE STPO_API03-FIXED_QTY,  " 固定数量

      END OF TY_UPLOAD_DATA.


TYPES:BEGIN OF TY_STPO.
        INCLUDE STRUCTURE STPO_API03.
TYPES:STR(2112) TYPE C,
      END OF TY_STPO.

DATA:GT_UPLOAD_DATA TYPE STANDARD TABLE OF TY_UPLOAD_DATA,
     GW_UPLOAD_DATA TYPE                   TY_UPLOAD_DATA.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
            P_FILE TYPE RLGRAP-FILENAME  ."导入文件


*PARAMETERS:P1 RADIOBUTTON GROUP G1." 批导新BOM


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
*  -->  p1        text
*  <--  p2        text
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

*&--代码注释 BY HANDYBY 16.05.2017 17:31:42  BEGIN
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*&--代码注释 BY HANDYBY 16.05.2017 17:31:42  END
*&--代码添加 BY HANDYBY 16.05.2017 17:32:02  BEGIN
    MESSAGE 'EXCEL导入失败！' TYPE 'E' .
*&--代码添加 BY HANDYBY 16.05.2017 17:32:02  END

  ENDIF.

ENDFORM.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI .

  DATA:BEGIN OF LT_MATNR OCCURS 0 ,
         PLANT      LIKE CSAP_MBOM-WERKS, " 工厂
         MATERIAL   LIKE CSAP_MBOM-MATNR, " 物料
         BOM_USAGE  LIKE CSAP_MBOM-STLAN, " 用途
         STLAL      LIKE CSAP_MBOM-STLAL, "可选BOM
         BOM_STATUS LIKE STKO_API01-BOM_STATUS, " BOM 状态
         LABORATORY LIKE STKO_API01-LABORATORY, " 办公室（阶段）
         VALID_FROM LIKE CSAP_MBOM-DATUV, " " 生效日期
         BASE_QUAN  LIKE STKO_API01-BASE_QUAN, " 基准数量(BTCI)
         BASE_UNIT  LIKE STKO_API01-BASE_UNIT, " BOM 基本单位
       END OF  LT_MATNR.

  DATA:LW_MATNR LIKE LT_MATNR.
  DATA:O_STKO LIKE STKO_API02.
  DATA:L_MSG TYPE STRING.
  DATA:RETURN LIKE STANDARD TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA:MATERIAL LIKE BAPI1080_MBM_C-MATERIAL.
  DATA:PLANT LIKE BAPI1080_MBM_C-PLANT.
  DATA:BOMUSAGE LIKE BAPI1080_BGR_C-BOM_USAGE.
  DATA:STLAL LIKE BAPI1080_MBM_C-ALTERNATIVE_BOM.
  DATA:VALID_FROM_DATE LIKE BAPI1080_BOM_C-VALID_FROM_DATE.

  DATA:LT_BAPIRETURN1 TYPE BAPIRETURN1 .
  DATA:DELETION_FLAG TYPE BAPIMATALL-DEL_FLAG.

*导入前，做数据检查
  LOOP AT  GT_UPLOAD_DATA INTO GW_UPLOAD_DATA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-MATERIAL
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-MATERIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-COMPONENT
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-COMPONENT.

    CLEAR: LT_BAPIRETURN1,DELETION_FLAG.

*抬头物料号
    CALL FUNCTION 'BAPI_MATERIAL_EXISTENCECHECK' " 判断物料是否存在
      EXPORTING
        MATERIAL      = GW_UPLOAD_DATA-MATERIAL
*       MATERIAL_EVG  =
      IMPORTING
        DELETION_FLAG = DELETION_FLAG
        RETURN        = LT_BAPIRETURN1.

    IF LT_BAPIRETURN1-TYPE =  'E' OR LT_BAPIRETURN1-TYPE =  'A' OR DELETION_FLAG = 'X'.
      CLEAR L_MSG.
      CONCATENATE 'BOM抬头物料号' GW_UPLOAD_DATA-MATERIAL '物料号不存在或删除' INTO L_MSG.
      MESSAGE L_MSG TYPE 'E'.
      EXIT.
    ENDIF.

    CLEAR: LT_BAPIRETURN1,DELETION_FLAG.
    " 组件物料号
    CALL FUNCTION 'BAPI_MATERIAL_EXISTENCECHECK' " 判断物料是否存在
      EXPORTING
        MATERIAL      = GW_UPLOAD_DATA-COMPONENT
*       MATERIAL_EVG  =
      IMPORTING
        DELETION_FLAG = DELETION_FLAG
        RETURN        = LT_BAPIRETURN1.

    IF LT_BAPIRETURN1-TYPE =  'E' OR LT_BAPIRETURN1-TYPE =  'A' OR DELETION_FLAG = 'X'.
      CLEAR L_MSG.
      CONCATENATE 'BOM抬头' GW_UPLOAD_DATA-MATERIAL '组件' GW_UPLOAD_DATA-COMPONENT '物料号不存在或删除' INTO L_MSG.
      MESSAGE L_MSG TYPE 'E'.
      EXIT.
    ENDIF.

    " 组件数量不为0
    IF GW_UPLOAD_DATA-COMP_QTY = 0 OR GW_UPLOAD_DATA-COMP_QTY IS INITIAL.
      BREAK-POINT.
      CLEAR L_MSG.
      CONCATENATE '父物料' GW_UPLOAD_DATA-MATERIAL '组件' GW_UPLOAD_DATA-COMPONENT '组件数量不能为零！' INTO L_MSG.
      MESSAGE L_MSG TYPE 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

  LOOP AT  GT_UPLOAD_DATA INTO GW_UPLOAD_DATA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-MATERIAL
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-MATERIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' " 补零
      EXPORTING
        INPUT  = GW_UPLOAD_DATA-COMPONENT
      IMPORTING
        OUTPUT = GW_UPLOAD_DATA-COMPONENT.


    MOVE-CORRESPONDING GW_UPLOAD_DATA TO LT_MATNR.
    APPEND LT_MATNR.
    CLEAR LT_MATNR.
    MODIFY GT_UPLOAD_DATA FROM GW_UPLOAD_DATA.
  ENDLOOP.

  SORT LT_MATNR BY MATERIAL  PLANT BOM_USAGE  .
  DELETE ADJACENT DUPLICATES FROM LT_MATNR COMPARING MATERIAL  PLANT BOM_USAGE VALID_FROM.

*  DELETE lt_matnr WHERE  material IS INITIAL.

  DATA:L_STKO LIKE STKO_API01 .
  DATA:LT_STPO TYPE STANDARD TABLE OF TY_STPO WITH HEADER LINE.

*批导新BOM,检查BOM是否存在
*  IF  P1 = 'X'.
  LOOP AT LT_MATNR INTO LW_MATNR.
    MATERIAL = LW_MATNR-MATERIAL .
    PLANT    = LW_MATNR-PLANT.
    BOMUSAGE = LW_MATNR-BOM_USAGE.
    STLAL    = LW_MATNR-STLAL.
    VALID_FROM_DATE = LW_MATNR-VALID_FROM.

    CALL FUNCTION 'BAPI_MAT_BOM_EXISTENCE_CHECK'
      EXPORTING
        MATERIAL        = MATERIAL " 物料
        PLANT           = PLANT    " 工厂
        BOMUSAGE        = BOMUSAGE " 用途
        VALID_FROM_DATE = VALID_FROM_DATE " 生效日期
*       VALID_TO_DATE   =
*       MATERIAL_EVG    =
      TABLES
        RETURN          = RETURN[].

    IF RETURN[] IS  INITIAL. " 如果检查bom已经存在
      CLEAR L_MSG.
      CONCATENATE MATERIAL ',' PLANT ',' BOMUSAGE ','  VALID_FROM_DATE '已经存在' INTO L_MSG.
      MESSAGE L_MSG TYPE 'E'.
    ENDIF.
  ENDLOOP.
*  ENDIF.

  " 导入BOM
  LOOP AT LT_MATNR INTO LW_MATNR.
    CLEAR L_STKO.
    L_STKO-BASE_QUAN =  LW_MATNR-BASE_QUAN. " 基准数量(BTCI)
    "BOM 基本单位
*    SELECT SINGLE meins INTO l_stko-base_unit FROM mara  WHERE matnr = lw_matnr-material.
    L_STKO-BASE_UNIT  = LW_MATNR-BASE_UNIT.
    L_STKO-BOM_STATUS = LW_MATNR-BOM_STATUS. "" BOM 状态
*    L_STKO-LABORATORY = LW_MATNR-LABORATORY. " 办公室（阶段）

    CLEAR:LT_STPO,LT_STPO[].
    LOOP AT GT_UPLOAD_DATA INTO GW_UPLOAD_DATA WHERE MATERIAL = LW_MATNR-MATERIAL
                                                AND PLANT = LW_MATNR-PLANT
                                                AND BOM_USAGE = LW_MATNR-BOM_USAGE.
      CLEAR LT_STPO.
      LT_STPO-ITEM_NO    =  GW_UPLOAD_DATA-ITEM_NO.   " 项目
      LT_STPO-ITEM_CATEG =  GW_UPLOAD_DATA-ITEM_CATEG." 项目类别
      LT_STPO-COMPONENT  =  GW_UPLOAD_DATA-COMPONENT. " 组件编码
      LT_STPO-COMP_QTY   =  GW_UPLOAD_DATA-COMP_QTY.  " 组件数量
      LT_STPO-COMP_UNIT  =  GW_UPLOAD_DATA-COMP_UNIT. " 计量单位
*      LT_STPO-REL_COST  =   GW_UPLOAD_DATA-REL_COST. "　成本核算相关标志
*      lt_stpo-item_text1 = gw_upload_data-item_text1. " " BOM 项目文本（行1
      LT_STPO-ITEM_TEXT1 = GW_UPLOAD_DATA-STR+0(40). " " BOM 项目文本（行1) ,或者长文本
      LT_STPO-ITEM_TEXT2 = GW_UPLOAD_DATA-STR+40(40). " " BOM 项目文本（行1) ,或者长文本
      LT_STPO-STR        = GW_UPLOAD_DATA-STR. "长文本
      LT_STPO-COMP_SCRAP = GW_UPLOAD_DATA-COMP_SCRAP. " 部件废品(%)
      LT_STPO-AI_GROUP   = GW_UPLOAD_DATA-AI_GROUP. " " 替代项目：组
      LT_STPO-AI_PRIO    = GW_UPLOAD_DATA-AI_PRIO. " " 优先级
      LT_STPO-AI_STRATEG = GW_UPLOAD_DATA-AI_STRATEG. " " 替代项目：策略
      LT_STPO-USAGE_PROB = GW_UPLOAD_DATA-USAGE_PROB. " 使用可能性按 % (BTCI)
      LT_STPO-SPPROCTYPE = GW_UPLOAD_DATA-SPPROCTYPE. " 特殊获取
      LT_STPO-REC_ALLOWD = ''. " 递归性允许
      LT_STPO-ISSUE_LOC  = GW_UPLOAD_DATA-ISSUE_LOC."生产库存地点
      LT_STPO-FIXED_QTY  = GW_UPLOAD_DATA-FIXED_QTY. " 固定数量
      APPEND LT_STPO  .
    ENDLOOP.

**判断组件是否有重复数据
*    DATA:lt_stpo02 TYPE STANDARD TABLE OF stpo_api03 WITH HEADER LINE.
*    CLEAR:lt_stpo02,lt_stpo02[].
*    DATA:lt_msg TYPE string.
*
*    lt_stpo02[] = lt_stpo[].
*
*    SORT lt_stpo02 BY component.
*    DELETE ADJACENT DUPLICATES FROM lt_stpo02 COMPARING component.
*    IF sy-subrc = 0.
*      MESSAGE '存在重复组件，请重新检查数据' TYPE 'E'.
*    ENDIF.

    MATERIAL = LW_MATNR-MATERIAL .
    PLANT    = LW_MATNR-PLANT.
    BOMUSAGE = LW_MATNR-BOM_USAGE.
    VALID_FROM_DATE = LW_MATNR-VALID_FROM.

*检查判断替代项目：组和固定数量，同时存在时报错
*    LOOP AT LT_STPO.
*      IF LT_STPO-AI_GROUP IS NOT INITIAL AND LT_STPO-FIXED_QTY IS NOT INITIAL.
*        CLEAR L_MSG.
*        CONCATENATE LT_STPO-ITEM_NO '   物料号:' LT_STPO-COMPONENT '不支持替代项目的固定数量' INTO L_MSG.
*        MESSAGE L_MSG TYPE 'E'.
*      ENDIF.
*    ENDLOOP.

    DATA:T_RESULT LIKE STANDARD TABLE OF BAPIRETURN1 WITH HEADER LINE.
    DATA:LW_RESULT TYPE BAPIRETURN1.
*SAP标准消息
    DATA:LT_T100 TYPE STANDARD TABLE OF T100 WITH HEADER LINE.

    CLEAR O_STKO.
*创建bom
    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        MATERIAL           = LW_MATNR-MATERIAL  " 物料
        PLANT              = LW_MATNR-PLANT     " 工厂
        BOM_USAGE          = LW_MATNR-BOM_USAGE " 用途
        ALTERNATIVE        = LW_MATNR-STLAL     "可选BOM
*       ALTERNATIVE        =
        VALID_FROM         = LW_MATNR-VALID_FROM " 生效日期
*       CHANGE_NO          =
*       REVISION_LEVEL     =
        I_STKO             = L_STKO
*       FL_NO_CHANGE_DOC   = ''
        FL_COMMIT_AND_WAIT = 'X'
*       FL_CAD             = ''
        FL_BOM_CREATE      = 'X'  " 创建标识
*       FL_NEW_ITEM        = 'X'
*       fl_complete        = 'X' " 不知道是否需要
*       FL_DEFAULT_VALUES  = 'X'
*       FL_IDENTIFY_BY_GUID       = ' '
      IMPORTING
*       FL_WARNING         =
        O_STKO             = O_STKO
      TABLES
        T_STPO             = LT_STPO
      EXCEPTIONS
        ERROR              = 1
        OTHERS             = 2.


    IF SY-SUBRC <> 0. " 创建失败

      CLEAR:LT_T100,LT_T100[]. " 读取标准系统信息
      SELECT *
        FROM T100
        INTO CORRESPONDING FIELDS OF TABLE LT_T100
        WHERE ARBGB = SY-MSGID
        AND SPRSL = 1.

      CLEAR LW_RESULT.
      LW_RESULT-TYPE = SY-MSGTY.
      LW_RESULT-ID = SY-MSGID.
      LW_RESULT-NUMBER = SY-MSGNO.
      LW_RESULT-MESSAGE_V1 = SY-MSGV1.
      LW_RESULT-MESSAGE_V2 = SY-MSGV2.
      LW_RESULT-MESSAGE_V3 = SY-MSGV3.
      LW_RESULT-MESSAGE_V4 = LW_MATNR-MATERIAL.

      READ TABLE LT_T100 WITH KEY ARBGB =  SY-MSGID MSGNR = SY-MSGNO.
      IF SY-SUBRC = 0.
        LW_RESULT-MESSAGE = LT_T100-TEXT. " 获取文本消息
      ENDIF.
      APPEND LW_RESULT TO T_RESULT.

    ELSE." 创建成功

* 写入长文本，--------------------------------begin
      " 按item no，组件物料编码,进行匹配,更新长文本
      DATA:STLKN LIKE STPO-STLKN,
           STLNR LIKE STPO-STLNR,
           STLTY LIKE STPO-STLTY,
           STPOZ LIKE STPO-STPOZ.
      DATA:IDNRK LIKE STPO-IDNRK,
           POSNR LIKE STPO-POSNR.
      DATA:LTSCH LIKE STXH-TDNAME.

      DATA:TXHEAD LIKE THEAD.

      DATA: IT_TLINES LIKE TLINE OCCURS  0 WITH HEADER LINE.

      LOOP AT LT_STPO. "

        POSNR = LT_STPO-ITEM_NO. " item no
        IDNRK = LT_STPO-COMPONENT. " 组件

        " 第一次创建，取得bom物料单号
        IF O_STKO-BOM_NO IS INITIAL.
          SELECT SINGLE STLNR
            INTO O_STKO-BOM_NO
            FROM MAST
            WHERE MATNR = MATERIAL
            AND WERKS = PLANT
            AND STLAN = BOMUSAGE.
        ENDIF.

        SELECT SINGLE STLTY " BOM 类别
          STLNR " 物料单
          STLKN " BOM 项目节点号
          STPOZ " 内部计数器
          INTO (STLTY,STLNR,STLKN,STPOZ)
          FROM STPO
          WHERE STLNR = O_STKO-BOM_NO " 物料单
          AND POSNR = POSNR
          AND IDNRK = IDNRK.

        IF SY-SUBRC = 0.

          DATA: LEN TYPE I .
          DATA OFF TYPE I VALUE '0'.
          LEN = STRLEN( LT_STPO-STR ) .  " Finding String Length using STRLEN
          LEN =  LEN / 40 + 1.

          REFRESH IT_TLINES.
          CLEAR IT_TLINES.

          DO LEN TIMES .
            MOVE '*' TO IT_TLINES-TDFORMAT.
            MOVE   LT_STPO-STR+OFF(40) TO IT_TLINES-TDLINE.
            SHIFT IT_TLINES-TDLINE LEFT DELETING LEADING ' '.
            OFF = OFF + 40 .
            APPEND IT_TLINES.
            CLEAR  IT_TLINES.
          ENDDO.

          CLEAR: LEN , OFF.
          "长文本
*          REFRESH it_tlines.
*          it_tlines-tdformat = '*'.
*          it_tlines-tdline =  'test long text rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrryyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyytttttttttttttttttiiiiiiiiiiiiiiiiiiiiiiiiiiiiii'.
*          APPEND it_tlines.
*          CLEAR  it_tlines.
*          it_tlines-tdformat = '*'.
*          it_tlines-tdline =  'test long text rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrryyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyytttttttttttttttttiiiiiiiiiiiiiiiiiiiiiiiiiiiiii'.
*          APPEND it_tlines.
*          CLEAR  it_tlines.
*          it_tlines-tdformat = '*'.
*          it_tlines-tdline =  'test long text rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrryyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyytttttttttttttttttiiiiiiiiiiiiiiiiiiiiiiiiiiiiii'.
*          APPEND it_tlines.
*          CLEAR  it_tlines.

* se SAVE_TEXT and also update STPO-LTXSP to SY-LANGU directly.
* If you don't update this field, you will not see the texts in BOM display transaction,
*even though they are there. You can retrieve them using READ_TEXT,
* but you will see them during BOM display only if you update this field
          UPDATE STPO
          SET LTXSP = SY-LANGU
          WHERE STLTY = STLTY
          AND STLKN = STLKN
          AND STLNR = STLNR
          AND STPOZ = STPOZ.

          " 文本名
          CALL FUNCTION 'CO_ZK_TEXTKEY_STPO'
            EXPORTING
              STLKN = STLKN
              STLNR = STLNR
              STLTY = STLTY
              STPOZ = STPOZ
            IMPORTING
              LTSCH = LTSCH.

          TXHEAD-TDNAME = LTSCH. " 文本名
          TXHEAD-TDID = 'MPO'. " text id
          TXHEAD-TDSPRAS = SY-LANGU. " 语言
          TXHEAD-TDOBJECT = 'BOM'. " 文本对象

*          txhead-tdlinesize = '40'.
*                    .

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              CLIENT          = SY-MANDT
              HEADER          = TXHEAD
              INSERT          = ' '
              SAVEMODE_DIRECT = 'X'
*             owner_specified = 'X'
*             local_cat       = ' '
*     IMPORTING
*             FUNCTION        =
*             NEWHEADER       =
            TABLES
              LINES           = IT_TLINES[]
            EXCEPTIONS
              ID              = 1
              LANGUAGE        = 2
              NAME            = 3
              OBJECT          = 4
              OTHERS          = 5.

          IF SY-SUBRC = 0.
            CALL FUNCTION 'COMMIT_TEXT'.
          ENDIF.

        ENDIF.

      ENDLOOP.

*写入长文本，--------------------------------end

      SELECT *  " 读取标准系统信息
        FROM T100
        INTO CORRESPONDING FIELDS OF TABLE LT_T100
        WHERE ARBGB = SY-MSGID
        AND SPRSL = 1.

      CLEAR LW_RESULT.
      LW_RESULT-TYPE = SY-MSGTY.
      LW_RESULT-ID = SY-MSGID.
      LW_RESULT-NUMBER = SY-MSGNO.
      LW_RESULT-MESSAGE_V1 = SY-MSGV1.
      LW_RESULT-MESSAGE_V2 = SY-MSGV2.
      LW_RESULT-MESSAGE_V3 = SY-MSGV3.
      LW_RESULT-MESSAGE_V4 =  SY-MSGV4.
      LW_RESULT-MESSAGE =  '物料bom操作成功'. " 获取文本消息
      APPEND LW_RESULT TO T_RESULT.
    ENDIF.

  ENDLOOP.

*打印信息
  LOOP AT T_RESULT INTO LW_RESULT.
    IF LW_RESULT-TYPE = 'E' OR LW_RESULT-TYPE = 'A' OR LW_RESULT-TYPE = 'W' .
      WRITE:/
      LW_RESULT-TYPE,
      LW_RESULT-MESSAGE_V1 ,
      LW_RESULT-MESSAGE_V2 ,
      LW_RESULT-MESSAGE_V3 ,
      LW_RESULT-MESSAGE_V4 ,
      LW_RESULT-MESSAGE.
      ULINE.
    ELSE.
      WRITE:/ '导入完毕,bom导入成功'  .
    ENDIF.
  ENDLOOP.


ENDFORM.                    " FRM_BAPI
