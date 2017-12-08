FUNCTION ZPS001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_NETWORK) TYPE  BAPI_NETWORK_LIST-NETWORK OPTIONAL
*"     VALUE(I_WBS) TYPE  PS_POSID
*"     VALUE(I_DYXTM) TYPE  ZDYXTM
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  ZPS001_01
*"  TABLES
*"      I_COMPS STRUCTURE  ZPS001
*"      E_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA: BEGIN OF LS_NETWORK,
          PSPNR TYPE PRPS-PSPNR,
          POSID TYPE PRPS-POSID,
          AUFNR TYPE AUFK-AUFNR,
          PSPEL TYPE AUFK-PSPEL,
        END OF LS_NETWORK .
  DATA LT_NETWORK LIKE TABLE OF LS_NETWORK .

* 根据自建表判断是否需要新增网络
  DATA: L_BUKRS TYPE BUKRS .
  SELECT SINGLE BUKRS
    INTO L_BUKRS
    FROM ZPST002
   WHERE BUKRS = I_WBS+0(4) .
  IF L_BUKRS IS NOT INITIAL .

* 网络表头创建
    DATA LS_NETWORK_BAPI TYPE BAPI_BUS2002_NEW .
    DATA LT_RETURN_BAPI TYPE TABLE OF BAPIRET2 WITH HEADER LINE .
    DATA LT_RETURN2_BAPI TYPE TABLE OF BAPIRET2 WITH HEADER LINE .

    LS_NETWORK_BAPI-WBS_ELEMENT = I_WBS .
    LS_NETWORK_BAPI-SCHED_TYPE = '1'.

    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
    CALL FUNCTION 'BAPI_BUS2002_CREATE'
      EXPORTING
        I_NETWORK = LS_NETWORK_BAPI
      TABLES
        ET_RETURN = LT_RETURN_BAPI[].
    READ TABLE LT_RETURN_BAPI WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      E_MSG-TYPE = 'E'.
      E_MSG-MESSAGE = '网络创建失败！'.
      PERFORM FRM_MSG TABLES E_RETURN[] LT_RETURN_BAPI[] .
      RETURN .
    ELSEIF SY-SUBRC <> 0 .
      READ TABLE LT_RETURN_BAPI WITH KEY TYPE = 'A'.
      IF SY-SUBRC = 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = '网络创建失败！'.
        PERFORM FRM_MSG TABLES E_RETURN[] LT_RETURN_BAPI[] .
        RETURN .
      ELSEIF SY-SUBRC <> 0 .
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = LT_RETURN2_BAPI.
        READ TABLE LT_RETURN2_BAPI WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
          E_MSG-TYPE = 'E'.
          E_MSG-MESSAGE = '网络创建失败！'.
          PERFORM FRM_MSG TABLES E_RETURN[] LT_RETURN2_BAPI[] .
          RETURN .
        ELSEIF SY-SUBRC <> 0 .
          READ TABLE LT_RETURN2_BAPI WITH KEY TYPE = 'A'.
          IF SY-SUBRC = 0 .
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
            E_MSG-TYPE = 'E'.
            E_MSG-MESSAGE = '网络创建失败！'.
            PERFORM FRM_MSG TABLES E_RETURN[] LT_RETURN2_BAPI[] .
            RETURN .
          ELSEIF SY-SUBRC <> 0 .
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
            E_MSG-TYPE = 'S'.
            E_MSG-MESSAGE = '网络创建成功！'.
            READ TABLE LT_RETURN2_BAPI WITH KEY TYPE = 'S'
                                                  ID = 'CNIF_PI'
                                              NUMBER = '003' .
            IF SY-SUBRC = 0 .
              LS_NETWORK-AUFNR = LT_RETURN2_BAPI-MESSAGE_V2 .
              APPEND LS_NETWORK TO LT_NETWORK .
              CLEAR LS_NETWORK .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

* 网络作业创建
    DATA:LS_PROJ  TYPE BAPI_PROJECT_DEFINITION,
         LS_PROJX TYPE BAPI_PROJECT_DEFINITION_UP.
    DATA LS_RETURN_BAPI TYPE BAPIRETURN1 .
    DATA LT_METHOD TYPE TABLE OF BAPI_METHOD_PROJECT WITH HEADER LINE .
    DATA:LT_ACTIV  TYPE TABLE OF BAPI_NETWORK_ACTIVITY WITH HEADER LINE,
         LT_ACTIVX TYPE TABLE OF BAPI_NETWORK_ACTIVITY_UP WITH HEADER LINE.
    DATA E_MSG2 TYPE TABLE OF BAPI_METH_MESSAGE WITH HEADER LINE .

    SELECT SINGLE PSPID
      INTO LS_PROJ-PROJECT_DEFINITION
      FROM PROJ AS A
     INNER JOIN PRPS AS B
        ON A~PSPNR = B~PSPHI
     WHERE B~POSID = I_WBS .
    IF LS_PROJ-PROJECT_DEFINITION IS NOT INITIAL .
      LS_PROJX-PROJECT_DEFINITION = 'X'.
    ENDIF.
    LT_METHOD-REFNUMBER = 000001 .
    LT_METHOD-OBJECTTYPE = 'NETWORK-ACTIVITY' .
    LT_METHOD-METHOD = 'Create' .
    READ TABLE LT_NETWORK INTO LS_NETWORK INDEX 1 .
    CONCATENATE LS_NETWORK-AUFNR '0010' INTO LT_METHOD-OBJECTKEY .
    APPEND LT_METHOD .
    CLEAR LT_METHOD .
*                    LT_METHOD-REFNUMBER =  .
*                    LT_METHOD-OBJECTTYPE = 'NETWORK-ACTIVITY' .
    LT_METHOD-METHOD = 'Save' .
*                    LT_METHOD-OBJECTKEY =  .
    APPEND LT_METHOD .
    CLEAR LT_METHOD .
    LT_ACTIV-PROJECT_DEFINITION = LS_PROJ-PROJECT_DEFINITION .
    LT_ACTIV-NETWORK = LS_NETWORK-AUFNR .
    LT_ACTIV-ACTIVITY = '0010' .
    LT_ACTIV-CONTROL_KEY = 'PS01'.
    APPEND LT_ACTIV .
    CLEAR LT_ACTIV .
    IF LS_PROJ-PROJECT_DEFINITION IS NOT INITIAL .
      LT_ACTIVX-PROJECT_DEFINITION = 'X'.
    ENDIF.
    IF LS_NETWORK-AUFNR IS NOT INITIAL .
      LT_ACTIVX-NETWORK = 'X'.
    ENDIF.
    LT_ACTIVX-ACTIVITY = 'X'.
    LT_ACTIVX-CONTROL_KEY = 'X'.
*    IF I_ACTIV-DESCRIPTION IS NOT INITIAL .
*      LT_ACTIVX-DESCRIPTION = 'X'.
*    ENDIF.
    APPEND LT_ACTIVX .
    CLEAR LT_ACTIVX .
    CLEAR LS_NETWORK .
    CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
      EXPORTING
        I_PROJECT_DEFINITION     = LS_PROJ
        I_PROJECT_DEFINITION_UPD = LS_PROJX
      IMPORTING
        RETURN                   = LS_RETURN_BAPI
      TABLES
        I_METHOD_PROJECT         = LT_METHOD[]
        I_ACTIVITY               = LT_ACTIV[]
        I_ACTIVITY_UPDATE        = LT_ACTIVX[]
        E_MESSAGE_TABLE          = E_MSG2[].
    IF LS_RETURN_BAPI-TYPE = 'E' OR LS_RETURN_BAPI-TYPE = 'A'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      E_MSG-TYPE = LS_RETURN_BAPI-TYPE .
      E_MSG-MESSAGE = LS_RETURN_BAPI-MESSAGE .
      PERFORM FRM_MSG4 TABLES E_RETURN[] E_MSG2[] .
      RETURN .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
      E_MSG-TYPE = 'S'.
      E_MSG-MESSAGE = '网络作业创建成功！'.
    ENDIF.

  ELSE .

    IF I_WBS IS NOT INITIAL .
* 通过WBS号查网络号
      SELECT A~PSPNR
             A~POSID
             B~AUFNR
             B~PSPEL
        INTO CORRESPONDING FIELDS OF TABLE LT_NETWORK
        FROM PRPS AS A
       INNER JOIN AUFK AS B
          ON A~PSPNR = B~PSPEL
       WHERE A~POSID = I_WBS .
      IF LT_NETWORK IS INITIAL .
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '该WBS找不到对应的网络号'.
        APPEND E_RETURN .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = '挂接失败，具体报错看E_RETURN参数！'.
        RETURN .
      ENDIF.
    ENDIF.
  ENDIF .

* 调用BAPI挂物料
  DATA LS_RETURN TYPE BAPIRET2 .
  DATA LT_RETURN2 TYPE TABLE OF BAPIRET2 WITH HEADER LINE .
  DATA LT_COMPS TYPE TABLE OF BAPI_NETWORK_COMP_ADD WITH HEADER LINE .
  DATA LT_MSG TYPE TABLE OF BAPI_METH_MESSAGE WITH HEADER LINE .
  DATA: BEGIN OF LS_MARC,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
          BESKZ TYPE MARC-BESKZ,
        END OF LS_MARC .
  DATA LT_MARC LIKE TABLE OF LS_MARC .

  SELECT MATNR
         WERKS
         BESKZ
    INTO CORRESPONDING FIELDS OF TABLE LT_MARC
    FROM MARC
     FOR ALL ENTRIES IN I_COMPS[]
   WHERE MATNR = I_COMPS-MATNR .
  SORT LT_MARC BY MATNR .

  LOOP AT I_COMPS.
    LT_COMPS-ACTIVITY = '0010' .
    LT_COMPS-MATERIAL = I_COMPS-MATNR.
    LT_COMPS-ENTRY_QUANTITY = I_COMPS-MENGE.
    LT_COMPS-ITEM_CAT = 'L' .
    LT_COMPS-REQ_DATE = I_COMPS-REQ_DATE.
    LT_COMPS-PREQ_NAME = I_COMPS-DISGR.
    LT_COMPS-ITEM_TEXT = I_COMPS-TXT.
    LT_COMPS-TRACKINGNO = I_COMPS-BEDNR.

    LT_COMPS-TYPE_OF_PUR_RESV = '5'.
    READ TABLE LT_MARC INTO LS_MARC WITH KEY MATNR = I_COMPS-MATNR BINARY SEARCH .
    IF SY-SUBRC = 0 .
      IF LS_MARC-BESKZ = 'E'.
        LT_COMPS-TYPE_OF_PUR_RESV = '8'.
      ENDIF.
      CLEAR LS_MARC .
    ENDIF.
*    IF I_COMPS-ITEM_CAT = 'L'.
    LT_COMPS-MRP_RELEVANT = '3'.
*    ENDIF.
    LT_COMPS-PUR_GROUP = I_COMPS-PUR_GROUP.
    APPEND LT_COMPS .
    CLEAR LT_COMPS .
  ENDLOOP.

  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
  WAIT UP TO 1 SECONDS .
  IF LT_NETWORK IS NOT INITIAL .
    READ TABLE LT_NETWORK INTO LS_NETWORK INDEX 1 .
    CALL FUNCTION 'BAPI_NETWORK_COMP_ADD'
      EXPORTING
        NUMBER           = LS_NETWORK-AUFNR
      IMPORTING
        RETURN           = LS_RETURN
      TABLES
        I_COMPONENTS_ADD = LT_COMPS[]
        E_MESSAGE_TABLE  = LT_MSG[].
  ELSE.
    CALL FUNCTION 'BAPI_NETWORK_COMP_ADD'
      EXPORTING
        NUMBER           = I_NETWORK
      IMPORTING
        RETURN           = LS_RETURN
      TABLES
        I_COMPONENTS_ADD = LT_COMPS[]
        E_MESSAGE_TABLE  = LT_MSG[].
  ENDIF.

  IF LS_RETURN-TYPE <> 'E'.

    CALL FUNCTION 'BAPI_PS_PRECOMMIT'
      TABLES
        ET_RETURN = LT_RETURN2[].

    READ TABLE LT_RETURN2 WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
*      CONCATENATE S_STR '物料挂接成功；' INTO S_STR.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      E_MSG-TYPE = 'S'.
      E_MSG-MESSAGE = '挂接成功！'.

      DATA L_AUFPL TYPE AFKO-AUFPL .
      SELECT SINGLE AUFPL
        INTO L_AUFPL
        FROM AFKO
       WHERE AUFNR = LS_NETWORK-AUFNR .
      IF L_AUFPL IS NOT INITIAL .
        DATA: BEGIN OF LS_AFVC ,
                AUFPL TYPE AFVC-AUFPL,
                APLZL TYPE AFVC-APLZL,
                BANFN TYPE AFVC-BANFN,
                BNFPO TYPE AFVC-BNFPO,
              END OF LS_AFVC .
        DATA LT_AFVC LIKE TABLE OF LS_AFVC .
        SELECT AUFPL
               APLZL
               BANFN
               BNFPO
          INTO CORRESPONDING FIELDS OF TABLE LT_AFVC
          FROM AFVC
         WHERE AUFPL = L_AUFPL .
        SORT LT_AFVC BY BNFPO DESCENDING .
        READ TABLE LT_AFVC INTO LS_AFVC INDEX 1 .

        E_MSG-BANFN = LS_AFVC-BANFN .
        E_MSG-BNFPO = LS_AFVC-BNFPO .
      ENDIF.

*      DATA LS_RETURN3 TYPE BAPIRET2 .
*      DATA LT_LIST TYPE TABLE OF BAPI_NETWORK_COMP_LIST WITH HEADER LINE .
*      DATA LT_RANGES TYPE TABLE OF BAPI_NETWORK_COMP_ACT_RNG WITH HEADER LINE .
*      IF LT_NETWORK IS NOT INITIAL .
*        CALL FUNCTION 'BAPI_NETWORK_COMP_GETLIST'
*          EXPORTING
*            NUMBER            = LS_NETWORK-AUFNR
**           MAX_ROWS          = 0
*          IMPORTING
*            RETURN            = LS_RETURN3
*          TABLES
*            I_ACTIVITY_RANGE  = LT_RANGES[]
*            E_COMPONENTS_LIST = LT_LIST[].
*      ELSE.
*        CALL FUNCTION 'BAPI_NETWORK_COMP_GETLIST'
*          EXPORTING
*            NUMBER            = I_NETWORK-AUFNR
**           MAX_ROWS          = 0
*          IMPORTING
*            RETURN            = LS_RETURN3
*          TABLES
*            I_ACTIVITY_RANGE  = LT_RANGES[]
*            E_COMPONENTS_LIST = LT_LIST[].
*      ENDIF.


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      E_RETURN[] = LT_RETURN2[].
      E_MSG-TYPE = 'E'.
      E_MSG-MESSAGE = '挂接失败，具体报错看E_RETURN参数！'.
    ENDIF .

  ELSE .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    LOOP AT LT_MSG.
      E_RETURN-TYPE = LT_MSG-MESSAGE_TYPE .
      E_RETURN-ID = LT_MSG-MESSAGE_ID .
      E_RETURN-NUMBER = LT_MSG-MESSAGE_NUMBER .
      E_RETURN-MESSAGE = LT_MSG-MESSAGE_TEXT .
      APPEND E_RETURN .
      CLEAR E_RETURN .
    ENDLOOP.
    E_MSG-TYPE = 'E'.
    E_MSG-MESSAGE = '挂接失败，具体报错看E_RETURN参数！'.
  ENDIF.

* 写入调用日志
  DATA: LT_ZZACKNOW TYPE TABLE OF ZZACKNOW,
        LS_ZZACKNOW TYPE ZZACKNOW.

  LS_ZZACKNOW-ZZDYXTM  = I_DYXTM.     "调用系统名
  LS_ZZACKNOW-ZZINTTY  = 'PS'.     "接口类型
  LS_ZZACKNOW-ZZCDATE  = SY-DATUM.   "日期
  LS_ZZACKNOW-ZZCTIME  = SY-UZEIT.   "时间
  LS_ZZACKNOW-ZBUKRS   = L_BUKRS.    "公司代码
  LS_ZZACKNOW-USNAM    = SY-UNAME .   "用户名
  LS_ZZACKNOW-ZZKEYF1  = 'ZPS001' .   "函数模块名
  IF E_MSG-TYPE = 'E'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用失败！' .
    LS_ZZACKNOW-ZZPROFG = 'E'."S"表成功 “E"表失败
  ELSEIF E_MSG-TYPE = 'S'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用成功！' .
    LS_ZZACKNOW-ZZPROFG = 'S'."S"表成功 “E"表失败
  ENDIF.

  APPEND LS_ZZACKNOW TO LT_ZZACKNOW.
  MODIFY ZZACKNOW FROM TABLE LT_ZZACKNOW.
  CLEAR LS_ZZACKNOW .
  REFRESH LT_ZZACKNOW .

ENDFUNCTION.
