FUNCTION ZPS002.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_PROJ) TYPE  ZPS002
*"     VALUE(I_NETWORK) TYPE  ZPS002_02
*"     VALUE(I_ACTIV) TYPE  ZPS002_03
*"     VALUE(I_DYXTM) TYPE  ZDYXTM
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  ZPS002_05
*"  TABLES
*"      I_WBS STRUCTURE  ZPS002_01
*"      E_MSG STRUCTURE  BAPI_METH_MESSAGE OPTIONAL
*"----------------------------------------------------------------------

* 数据校验
  IF I_PROJ-PROJECT_DEFINITION IS INITIAL .
    E_RETURN-TYPE = 'E' .
    E_RETURN-MESSAGE = '项目结构创建失败!' .
    E_MSG-MESSAGE_TYPE = 'E'.
    E_MSG-MESSAGE_TEXT = 'I_PROJ参数的项目定义字段不能为空！'.
    APPEND E_MSG .
    RETURN .
  ENDIF.
  IF I_WBS[] IS INITIAL .
    E_RETURN-TYPE = 'E' .
    E_RETURN-MESSAGE = '项目结构创建失败!' .
    E_MSG-MESSAGE_TYPE = 'E'.
    E_MSG-MESSAGE_TEXT = 'I_WBS参数不能为空！'.
    APPEND E_MSG .
    RETURN .
  ENDIF.
  IF I_WBS-WBS_ELEMENT IS INITIAL .
    E_RETURN-TYPE = 'E' .
    E_RETURN-MESSAGE = '项目结构创建失败!' .
    E_MSG-MESSAGE_TYPE = 'E'.
    E_MSG-MESSAGE_TEXT = 'I_WBS参数的WBS元素字段不能为空！'.
    APPEND E_MSG .
    RETURN .
  ENDIF.
  IF I_NETWORK-PROFILE IS INITIAL .
    E_RETURN-TYPE = 'E' .
    E_RETURN-MESSAGE = '项目结构创建失败!' .
    E_MSG-MESSAGE_TYPE = 'E'.
    E_MSG-MESSAGE_TEXT = 'I_NETWORK参数的网络参数文件字段不能为空！'.
    APPEND E_MSG .
    RETURN .
  ENDIF.
  IF I_ACTIV-ACTIVITY IS INITIAL .
    E_RETURN-TYPE = 'E' .
    E_RETURN-MESSAGE = '项目结构创建失败!' .
    E_MSG-MESSAGE_TYPE = 'E'.
    E_MSG-MESSAGE_TEXT = 'I_ACTIV参数的作业编号字段不能为空！'.
    APPEND E_MSG .
    RETURN .
  ENDIF.

  DATA LS_PROJ TYPE BAPI_PROJECT_DEFINITION .
  DATA LS_RETURN1 TYPE BAPIRETURN1 .
  DATA LT_MSG1 TYPE TABLE OF BAPI_METH_MESSAGE WITH HEADER LINE .

  MOVE-CORRESPONDING I_PROJ TO LS_PROJ .
  CALL FUNCTION 'BAPI_PROJECTDEF_CREATE'
    EXPORTING
      PROJECT_DEFINITION_STRU = LS_PROJ
    IMPORTING
      RETURN                  = LS_RETURN1
    TABLES
      E_MESSAGE_TABLE         = LT_MSG1[].

  IF LS_RETURN1-TYPE = 'E' OR LS_RETURN1-TYPE = 'A'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    IF LS_RETURN1-ID = 'CJ' AND LS_RETURN1-NUMBER = 010 .
      E_RETURN-TYPE = 'E' .
      E_RETURN-MESSAGE = LS_RETURN1-MESSAGE .
    ELSE .
      E_RETURN-TYPE = 'E' .
      E_RETURN-MESSAGE = '项目结构创建失败!'.
    ENDIF.
    PERFORM FRM_MSG1 TABLES E_MSG[] LT_MSG1[] .
    RETURN .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    E_RETURN-TYPE = 'S'.
    E_RETURN-MESSAGE = '项目结构创建成功！'.
    E_RETURN-PROJ = LS_PROJ-PROJECT_DEFINITION .

* 创建WBS元素
    DATA LT_WBS TYPE TABLE OF BAPI_BUS2054_NEW WITH HEADER LINE .
    DATA LT_RETURN2 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .
    DATA LT_RETURN3_1 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .

    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
    LOOP AT I_WBS.
      MOVE-CORRESPONDING I_WBS TO LT_WBS .
      LT_WBS-WBS_BILLING_ELEMENT = 'X'.
      APPEND LT_WBS .
      CLEAR LT_WBS .
    ENDLOOP.
    CALL FUNCTION 'BAPI_BUS2054_CREATE_MULTI'
      EXPORTING
        I_PROJECT_DEFINITION = LS_PROJ-PROJECT_DEFINITION
      TABLES
        IT_WBS_ELEMENT       = LT_WBS[]
        ET_RETURN            = LT_RETURN2[].

    READ TABLE LT_RETURN2 WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      E_RETURN-TYPE = 'E'.
      E_RETURN-MESSAGE = '项目结构创建失败！'.
      PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN2[] .
      RETURN .
    ELSEIF SY-SUBRC <> 0 .
      READ TABLE LT_RETURN2 WITH KEY TYPE = 'A'.
      IF SY-SUBRC = 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '项目结构创建失败！'.
        PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN2[] .
        RETURN .
        RETURN .
      ELSE .
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = LT_RETURN3_1[].
        READ TABLE LT_RETURN3_1 WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '项目结构创建失败！'.
          PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_1[] .
          RETURN .
          RETURN .
        ELSEIF SY-SUBRC <> 0 .
          READ TABLE LT_RETURN3_1 WITH KEY TYPE = 'A'.
          IF SY-SUBRC = 0 .
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
            E_RETURN-TYPE = 'E'.
            E_RETURN-MESSAGE = '项目结构创建失败！'.
            PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_1[] .
            RETURN .
          ELSEIF SY-SUBRC <> 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
            E_RETURN-TYPE = 'S'.
            E_RETURN-MESSAGE = '项目结构创建成功！'.
            LOOP AT LT_WBS.
              CONCATENATE E_RETURN-WBS LT_WBS-WBS_ELEMENT ';' INTO E_RETURN-WBS .
            ENDLOOP.

* 网络表头创建
            DATA LS_NETWORK TYPE BAPI_BUS2002_NEW .
            DATA LT_RETURN4 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .
            DATA LT_RETURN3_2 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .

            MOVE-CORRESPONDING I_NETWORK TO LS_NETWORK .
            CLEAR LS_NETWORK-NETWORK .
*            CLEAR LS_NETWORK-SHORT_TEXT .
            LS_NETWORK-SCHED_TYPE = '1'.

            CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
            CALL FUNCTION 'BAPI_BUS2002_CREATE'
              EXPORTING
                I_NETWORK = LS_NETWORK
              TABLES
                ET_RETURN = LT_RETURN4[].
            READ TABLE LT_RETURN4 WITH KEY TYPE = 'E'.
            IF SY-SUBRC = 0 .
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
              E_RETURN-TYPE = 'E'.
              E_RETURN-MESSAGE = '项目结构创建失败！'.
              PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN4[] .
              RETURN .
            ELSEIF SY-SUBRC <> 0 .
              READ TABLE LT_RETURN4 WITH KEY TYPE = 'A'.
              IF SY-SUBRC = 0 .
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                E_RETURN-TYPE = 'E'.
                E_RETURN-MESSAGE = '项目结构创建失败！'.
                PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN4[] .
                RETURN .
              ELSEIF SY-SUBRC <> 0 .
                CALL FUNCTION 'BAPI_PS_PRECOMMIT'
                  TABLES
                    ET_RETURN = LT_RETURN3_2.
                READ TABLE LT_RETURN3_2 WITH KEY TYPE = 'E'.
                IF SY-SUBRC = 0 .
                  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                  E_RETURN-TYPE = 'E'.
                  E_RETURN-MESSAGE = '项目结构创建失败！'.
                  PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_2[] .
                  RETURN .
                ELSEIF SY-SUBRC <> 0 .
                  READ TABLE LT_RETURN3_2 WITH KEY TYPE = 'A'.
                  IF SY-SUBRC = 0 .
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                    E_RETURN-TYPE = 'E'.
                    E_RETURN-MESSAGE = '项目结构创建失败！'.
                    PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_2[] .
                    RETURN .
                  ELSEIF SY-SUBRC <> 0 .
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        WAIT = 'X'.
                    E_RETURN-TYPE = 'S'.
                    E_RETURN-MESSAGE = '项目结构创建成功！'.
                    READ TABLE LT_RETURN3_2 WITH KEY TYPE = 'S'
                                                     ID = 'CNIF_PI'
                                                     NUMBER = '003' .
                    IF SY-SUBRC = 0 .
                      E_RETURN-NETWORK = LT_RETURN3_2-MESSAGE_V2 .
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

* 网络作业创建
            DATA:LS_PROJ2  TYPE BAPI_PROJECT_DEFINITION,
                 LS_PROJ2X TYPE BAPI_PROJECT_DEFINITION_UP.
            DATA LS_RETURN2 TYPE BAPIRETURN1 .
            DATA LT_METHOD TYPE TABLE OF BAPI_METHOD_PROJECT WITH HEADER LINE .
            DATA:LT_ACTIV  TYPE TABLE OF BAPI_NETWORK_ACTIVITY WITH HEADER LINE,
                 LT_ACTIVX TYPE TABLE OF BAPI_NETWORK_ACTIVITY_UP WITH HEADER LINE.
            DATA E_MSG2 TYPE TABLE OF BAPI_METH_MESSAGE WITH HEADER LINE .

            LS_PROJ2-PROJECT_DEFINITION = I_PROJ-PROJECT_DEFINITION .
            IF I_PROJ-PROJECT_DEFINITION IS NOT INITIAL .
              LS_PROJ2X-PROJECT_DEFINITION = 'X'.
            ENDIF.
            LT_METHOD-REFNUMBER = 000001 .
            LT_METHOD-OBJECTTYPE = 'NETWORK-ACTIVITY' .
            LT_METHOD-METHOD = 'Create' .
            CONCATENATE E_RETURN-NETWORK I_ACTIV-ACTIVITY INTO LT_METHOD-OBJECTKEY .
            APPEND LT_METHOD .
            CLEAR LT_METHOD .
*                    LT_METHOD-REFNUMBER =  .
*                    LT_METHOD-OBJECTTYPE = 'NETWORK-ACTIVITY' .
            LT_METHOD-METHOD = 'Save' .
*                    LT_METHOD-OBJECTKEY =  .
            APPEND LT_METHOD .
            CLEAR LT_METHOD .
            MOVE-CORRESPONDING I_ACTIV TO LT_ACTIV .
            LT_ACTIV-NETWORK = E_RETURN-NETWORK .
*            LT_ACTIV-CONTROL_KEY = 'PS01'.
            APPEND LT_ACTIV .
            CLEAR LT_ACTIV .
            IF I_ACTIV-NETWORK IS NOT INITIAL .
              LT_ACTIVX-NETWORK = 'X'.
            ENDIF.
            IF I_ACTIV-ACTIVITY IS NOT INITIAL .
              LT_ACTIVX-ACTIVITY = 'X'.
            ENDIF.
            IF I_ACTIV-CONTROL_KEY IS NOT INITIAL .
              LT_ACTIVX-CONTROL_KEY = 'X'.
            ENDIF.
            IF I_ACTIV-DESCRIPTION IS NOT INITIAL .
              LT_ACTIVX-DESCRIPTION = 'X'.
            ENDIF.
            APPEND LT_ACTIVX .
            CLEAR LT_ACTIVX .
            CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
              EXPORTING
                I_PROJECT_DEFINITION     = LS_PROJ2
                I_PROJECT_DEFINITION_UPD = LS_PROJ2X
              IMPORTING
                RETURN                   = LS_RETURN2
              TABLES
                I_METHOD_PROJECT         = LT_METHOD[]
                I_ACTIVITY               = LT_ACTIV[]
                I_ACTIVITY_UPDATE        = LT_ACTIVX[]
                E_MESSAGE_TABLE          = E_MSG2[].
            IF LS_RETURN2-TYPE = 'E' OR LS_RETURN2-TYPE = 'A'.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
              E_RETURN-TYPE = LS_RETURN2-TYPE .
              E_RETURN-MESSAGE = LS_RETURN2-MESSAGE .
              PERFORM FRM_MSG1 TABLES E_MSG[] E_MSG2[] .
              RETURN .
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.
              E_RETURN-TYPE = 'S'.
              E_RETURN-MESSAGE = '项目结构创建成功！'.
              E_RETURN-NETWORK = I_ACTIV-ACTIVITY .

* 下达项目
              DATA LS_RETURN3 TYPE BAPIRETURN1 .
              DATA LT_MSG2 TYPE TABLE OF BAPI_STATUS_RESULT WITH HEADER LINE .
              DATA LT_RETURN3_3 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .

              WAIT UP TO 1 SECONDS .
              CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
              CALL FUNCTION 'BAPI_BUS2001_SET_STATUS'
                EXPORTING
                  PROJECT_DEFINITION = I_PROJ-PROJECT_DEFINITION
                  SET_SYSTEM_STATUS  = 'REL'
                IMPORTING
                  RETURN             = LS_RETURN3
                TABLES
                  E_RESULT           = LT_MSG2[].
              IF LS_RETURN3-TYPE = 'E' OR LS_RETURN3-TYPE = 'A'.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                E_RETURN-TYPE = LS_RETURN3-TYPE .
                E_RETURN-MESSAGE = LS_RETURN3-MESSAGE .
                PERFORM FRM_MSG3 TABLES E_MSG[] LT_MSG2[] .
                RETURN .
              ELSE.
                CALL FUNCTION 'BAPI_PS_PRECOMMIT'
                  TABLES
                    ET_RETURN = LT_RETURN3_3.
                READ TABLE LT_RETURN3_3 WITH KEY TYPE = 'E'.
                IF SY-SUBRC = 0 .
                  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                  E_RETURN-TYPE = 'E'.
                  E_RETURN-MESSAGE = '项目结构创建失败！'.
                  PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_3[] .
                  RETURN .
                ELSEIF SY-SUBRC <> 0 .
                  READ TABLE LT_RETURN3_3 WITH KEY TYPE = 'A'.
                  IF SY-SUBRC = 0 .
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
                    E_RETURN-TYPE = 'E'.
                    E_RETURN-MESSAGE = '项目结构创建失败！'.
                    PERFORM FRM_MSG2 TABLES E_MSG[] LT_RETURN3_3[] .
                    RETURN .
                  ELSEIF SY-SUBRC <> 0 .
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        WAIT = 'X'.
                    E_RETURN-TYPE = 'S'.
                    E_RETURN-MESSAGE = '项目结构创建成功！'.
                    E_RETURN-STATUS = 'I0002' .
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

* 写入调用日志
  DATA: LT_ZZACKNOW TYPE TABLE OF ZZACKNOW,
        LS_ZZACKNOW TYPE ZZACKNOW.
  DATA L_BUKRS TYPE BUKRS .

  SELECT SINGLE
         VBUKR
    INTO L_BUKRS
    FROM PROJ
   WHERE PSPID = I_PROJ-PROJECT_DEFINITION .

  LS_ZZACKNOW-ZZDYXTM  = I_DYXTM.     "调用系统名
  LS_ZZACKNOW-ZZINTTY  = 'PS'.     "接口类型
  LS_ZZACKNOW-ZZCDATE  = SY-DATUM.   "日期
  LS_ZZACKNOW-ZZCTIME  = SY-UZEIT.   "时间
  LS_ZZACKNOW-ZBUKRS   = L_BUKRS.    "公司代码
  LS_ZZACKNOW-USNAM    = SY-UNAME .   "用户名
  LS_ZZACKNOW-ZZKEYF1  = 'ZPS002' .   "函数模块名
  IF E_RETURN-TYPE = 'E'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用失败！' .
    LS_ZZACKNOW-ZZPROFG = 'E'."S"表成功 “E"表失败
  ELSEIF E_RETURN-TYPE = 'S'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用成功！' .
    LS_ZZACKNOW-ZZPROFG = 'S'."S"表成功 “E"表失败
  ENDIF.

  APPEND LS_ZZACKNOW TO LT_ZZACKNOW.
  MODIFY ZZACKNOW FROM TABLE LT_ZZACKNOW.
  CLEAR LS_ZZACKNOW .
  REFRESH LT_ZZACKNOW .

ENDFUNCTION.
