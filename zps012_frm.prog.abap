*&  包含                ZPS012_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DEAL_DATA .

  DATA:BEGIN OF LS_PROJ,
         PSPNR TYPE PROJ-PSPNR, "项目定义内码
         PSPID TYPE PROJ-PSPID, "项目定义外码
         POST1 TYPE PROJ-POST1, "项目定义名称
       END OF LS_PROJ .
*  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         PSPHI TYPE PRPS-PSPHI,
       END OF LS_PRPS.

  IF P_POSID IS NOT INITIAL .
    SELECT
    SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS
      FROM PRPS
     WHERE POSID = P_POSID .
    IF LS_PRPS IS NOT INITIAL .
      SELECT SINGLE
             PSPNR  "项目定义内码
             PSPID  "项目定义外码
             POST1  "项目定义名称
        INTO CORRESPONDING FIELDS OF LS_PROJ
        FROM PROJ
       WHERE PSPNR = LS_PRPS-PSPHI .
    ENDIF.
  ELSE .
    SELECT SINGLE
           PSPNR  "项目定义内码
           PSPID  "项目定义外码
           POST1  "项目定义名称
      INTO CORRESPONDING FIELDS OF LS_PROJ
      FROM PROJ .
  ENDIF.

  DATA LT_NETWORK TYPE TTY_NETWORK_CK_ITEMS_RDEXP .
  DATA LT_WBS TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX .
  DATA LS_WBS TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP .
  DATA LT_RETURN TYPE BAPIRET2_T .
  DATA LT_COSTLINES TYPE CKF_STANDARD_KIS1_TABLE .
  DATA LS_KIS1 TYPE KIS1 .

  DATA:BEGIN OF LS_DATA,
         PSPID TYPE PROJ-PSPID, "项目定义外码
         POST1 TYPE PROJ-POST1, "项目定义名称
         PSPNR TYPE PRPS-PSPNR, "WBS元素
         MATNR TYPE PRPS-MATNR, "物料号
         WERKS TYPE PRPS-WERKS , "工厂
         TYPPS TYPE KIS1-TYPPS, "项目类别
         MENGE TYPE MENGE_POS,  "预算数量
       END OF LS_DATA .
  DATA LT_DATA LIKE HASHED TABLE OF LS_DATA WITH UNIQUE KEY PSPID POST1 PSPNR MATNR WERKS TYPPS .
  DATA LT_DATA2 LIKE TABLE OF LS_DATA .
  DATA LS_DATA2 LIKE LS_DATA .
  DATA:BEGIN OF LS_DATA3,
         PSPNR TYPE PROJ-PSPNR, "项目定义内码
         PSPID TYPE PROJ-PSPID,
         MATNR TYPE PRPS-MATNR,
       END OF LS_DATA3.
  DATA LT_DATA3 LIKE TABLE OF LS_DATA3.
  FIELD-SYMBOLS <FS_DATA3> LIKE LS_DATA3 .

  DATA:BEGIN OF LS_MAKT,
         MATNR TYPE MAKT-MATNR,
         SPRAS TYPE MAKT-SPRAS,
         MAKTX TYPE MAKT-MAKTX,
       END OF LS_MAKT .
  DATA LT_MAKT LIKE TABLE OF LS_MAKT .

  DATA:BEGIN OF LS_EBAN,
         BANFN TYPE EBAN-BANFN,
         BNFPO TYPE EBAN-BNFPO,
         MATNR TYPE EBAN-MATNR,
       END OF LS_EBAN .
  DATA LT_EBAN LIKE TABLE OF LS_EBAN .
  DATA:BEGIN OF LS_EBKN,
         BANFN      TYPE EBKN-BANFN,
         BNFPO      TYPE EBKN-BNFPO,
         ZEBKN      TYPE EBKN-ZEBKN,
         MENGE      TYPE EBKN-MENGE,
         PS_PSP_PNR TYPE EBKN-PS_PSP_PNR,
       END OF LS_EBKN .
  DATA LT_EBKN LIKE TABLE OF LS_EBKN .
  DATA:BEGIN OF LS_EBKN2 ,
         PS_PSP_PNR TYPE EBKN-PS_PSP_PNR,
         MATNR      TYPE EBAN-MATNR,
         MENGE      TYPE EBKN-MENGE,
       END OF LS_EBKN2 .
  DATA LT_EBKN2 LIKE HASHED TABLE OF LS_EBKN2 WITH UNIQUE KEY PS_PSP_PNR MATNR .

  DATA L_NUM TYPE I VALUE IS INITIAL .

*  LOOP AT LT_PROJ INTO LS_PROJ .
  CALL FUNCTION 'CNECP_READ'
    EXPORTING
      I_PROJ_DEF    = LS_PROJ-PSPID
      I_VERSION     = '000'
*     I_VSNMR       =
    IMPORTING
      E_NETWORK_ECP = LT_NETWORK
      E_WBS_ECP     = LT_WBS
      ET_MESG       = LT_RETURN.
  IF LT_WBS IS NOT INITIAL .
    LOOP AT LT_WBS INTO LS_WBS .

*    READ TABLE LT_WBS INTO LS_WBS INDEX 1.
      LT_COSTLINES = LS_WBS-COST_LINES .

      SORT LT_COSTLINES BY TYPPS .
      READ TABLE LT_COSTLINES WITH KEY TYPPS = 'M' BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_COSTLINES INTO LS_KIS1 FROM SY-TABIX .
          IF LS_KIS1-TYPPS = 'M'.
            LS_DATA-PSPID = LS_PROJ-PSPID .
            LS_DATA-POST1 = LS_PROJ-POST1 .
            LS_DATA-PSPNR = LS_KIS1-PSPNR ."WBS元素内码
            LS_DATA-MATNR = LS_KIS1-MATNR .
            LS_DATA-WERKS = LS_KIS1-WERKS .
            LS_DATA-TYPPS = LS_KIS1-TYPPS .
            LS_DATA-MENGE = LS_KIS1-MENGE ."预算数量
            COLLECT LS_DATA INTO LT_DATA .
            CLEAR LS_KIS1 .
          ELSE.
            EXIT .
          ENDIF.
        ENDLOOP.

      ENDIF.
      CLEAR LS_WBS .
      REFRESH LT_COSTLINES .
    ENDLOOP.
  ENDIF.
*  ENDLOOP.

* 取物料描述
  LT_DATA2 = LT_DATA .
  SORT LT_DATA2 BY MATNR .
  DELETE ADJACENT DUPLICATES FROM LT_DATA2 COMPARING MATNR .
  IF LT_DATA2 IS NOT INITIAL .
    SELECT MATNR
           SPRAS
           MAKTX
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FROM MAKT
       FOR ALL ENTRIES IN LT_DATA2
     WHERE MATNR = LT_DATA2-MATNR
       AND SPRAS = SY-LANGU .
    IF LT_MAKT IS NOT INITIAL .
      SORT LT_MAKT BY MATNR .
    ENDIF.
  ENDIF.

  IF LT_DATA IS NOT INITIAL .
    SELECT BANFN
           BNFPO
           MATNR
      INTO CORRESPONDING FIELDS OF TABLE LT_EBAN  "取已经存在的采购申请里的数量
      FROM EBAN
       FOR ALL ENTRIES IN LT_DATA
     WHERE MATNR = LT_DATA-MATNR
       AND LOEKZ <> 'X' .
    SORT LT_EBAN BY BANFN BNFPO .
    IF LT_EBAN IS NOT INITIAL .
      SELECT BANFN
             BNFPO
             ZEBKN
             MENGE
             PS_PSP_PNR
        INTO CORRESPONDING FIELDS OF TABLE LT_EBKN
        FROM EBKN
         FOR ALL ENTRIES IN LT_EBAN
       WHERE BANFN = LT_EBAN-BANFN
         AND BNFPO = LT_EBAN-BNFPO .
      IF LT_EBKN IS NOT INITIAL .
        LOOP AT LT_EBKN INTO LS_EBKN .
          MOVE-CORRESPONDING LS_EBKN TO LS_EBKN2 .
          READ TABLE LT_EBAN INTO LS_EBAN WITH KEY BANFN = LS_EBKN-BANFN
                                                   BNFPO = LS_EBKN-BNFPO
                                                   BINARY SEARCH .
          IF SY-SUBRC = 0 .
            LS_EBKN2-MATNR = LS_EBAN-MATNR .
            CLEAR LS_EBAN .
          ENDIF.
          COLLECT LS_EBKN2 INTO LT_EBKN2 .
          CLEAR LS_EBKN .
          CLEAR LS_EBKN2 .
        ENDLOOP.
      ENDIF.
    ENDIF.

*&--代码添加 BY HANDYBY 03.08.2017 22:14:54  BEGIN
* 取物料的采购类型
    DATA: BEGIN OF LS_MARC ,
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
       FOR ALL ENTRIES IN LT_DATA
     WHERE MATNR = LT_DATA-MATNR
       AND WERKS = LT_DATA-WERKS .
    SORT LT_MARC BY MATNR WERKS .
* 取自制采购件
    DATA: BEGIN OF LS_RESB ,
            RSNUM TYPE RESB-RSNUM,
            RSPOS TYPE RESB-RSPOS,
            MATNR TYPE RESB-MATNR,
            AUFNR TYPE RESB-AUFNR,
            PSPEL TYPE RESB-PSPEL,
            BDMNG TYPE RESB-BDMNG,
          END OF LS_RESB .
    DATA LT_RESB LIKE TABLE OF LS_RESB .
    SELECT RSNUM
           RSPOS
           MATNR
           AUFNR
           PSPEL
           BDMNG
      INTO CORRESPONDING FIELDS OF TABLE LT_RESB
      FROM RESB
       FOR ALL ENTRIES IN LT_DATA
     WHERE PSPEL = LT_DATA-PSPNR
       AND WERKS = LT_DATA-WERKS  .
    SORT LT_RESB BY PSPEL MATNR .
*&--代码添加 BY HANDYBY 03.08.2017 22:14:54  END
  ENDIF.

  DATA: BEGIN OF LS_PRPS2,
          PSPNR TYPE PRPS-PSPNR,
          POSID TYPE PRPS-POSID,
        END OF LS_PRPS2 .
  DATA LT_PRPS2 LIKE TABLE OF LS_PRPS2 .
  IF LT_DATA IS NOT INITIAL .
    SELECT PSPNR
           POSID
      INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
      FROM PRPS
       FOR ALL ENTRIES IN LT_DATA
     WHERE PSPNR = LT_DATA-PSPNR .
    SORT LT_PRPS2 BY PSPNR  .
  ENDIF.

* 放进ALV
  SORT LT_DATA BY PSPID MATNR .
  LOOP AT LT_DATA INTO LS_DATA .
    READ TABLE LT_PRPS2 INTO LS_PRPS2 WITH KEY PSPNR = LS_DATA-PSPNR BINARY SEARCH .
    IF LS_PRPS2-POSID <> P_POSID .
      CONTINUE .
    ENDIF.
    L_NUM = L_NUM + 1 .
    GS_ALV-NUM = L_NUM .
    GS_ALV-POSID = LS_PRPS2-POSID .
    GS_ALV-POST1 = LS_DATA-POST1 .
    GS_ALV-MATNR = LS_DATA-MATNR .
    GS_ALV-TYPPS = LS_DATA-TYPPS .
    READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = LS_DATA-MATNR BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV-MAKTX = LS_MAKT-MAKTX .
      CLEAR LS_MAKT .
    ENDIF.
    GS_ALV-MENGE = LS_DATA-MENGE .
    " 先判断物料的采购类型，根据采购类型来决定已申请数要从哪里取
    READ TABLE LT_MARC INTO LS_MARC WITH KEY MATNR = LS_DATA-MATNR
                                             WERKS = LS_DATA-WERKS BINARY SEARCH .
    IF LS_MARC-BESKZ = 'E'.
      READ TABLE LT_RESB WITH KEY PSPEL = LS_DATA-PSPNR
                                  MATNR = LS_DATA-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_RESB INTO LS_RESB FROM SY-TABIX .
          IF LS_RESB-PSPEL = LS_DATA-PSPNR AND
               LS_RESB-MATNR = LS_DATA-MATNR .
            GS_ALV-MENGE2 = GS_ALV-MENGE2 + LS_RESB-BDMNG .
            CLEAR LS_RESB .
          ELSE .
            CLEAR LS_RESB .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE .
      READ TABLE LT_EBKN2 INTO LS_EBKN2 WITH TABLE KEY PS_PSP_PNR = LS_DATA-PSPNR
                                                            MATNR = LS_DATA-MATNR .
      IF SY-SUBRC = 0 .
        GS_ALV-MENGE2 = LS_EBKN2-MENGE .
        CLEAR LS_EBKN2 .
      ENDIF.
    ENDIF.

    GS_ALV-MENGE3 = GS_ALV-MENGE - GS_ALV-MENGE2 .
    IF GS_ALV-MENGE3 LT 0 .
      GS_ALV-MENGE3 = 0 .
    ENDIF.
    GS_ALV-BUDAT = P_BUDAT .
    GS_ALV-MENGE4 = GS_ALV-MENGE - GS_ALV-MENGE2 - GS_ALV-MENGE3 .
    IF GS_ALV-MENGE4 >= 0 .
      CLEAR GS_ALV-MENGE4 .
    ELSEIF GS_ALV-MENGE4 < 0 .
      GS_ALV-MENGE4 = ABS( GS_ALV-MENGE4 ) .
    ENDIF.
    GS_ALV-USRNAM = P_DISGR .
    APPEND GS_ALV TO GT_ALV .
    CLEAR GS_ALV .
    CLEAR LS_PRPS2 .
  ENDLOOP.

  IF GT_ALV IS INITIAL .
    MESSAGE '没有符合条件的数据！' TYPE 'E'  .
    RETURN .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .
  PERFORM INIT_LAYOUT.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.            "字段列定义
*  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
*                            GT_SORT
                            GT_ALV
                     USING
                           'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GS_LAYOUT
*                           GS_VARIANT
                           GS_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GS_LAYOUT-ZEBRA        = 'X'.
  GS_LAYOUT-CWIDTH_OPT   = 'X'.
  GS_LAYOUT-BOX_FNAME = 'SEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'NUM'        '序号'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'        '项目定义'         '' 'X' '' '' '' 'PROJ' 'PSPID'.
  INIT_FIELDCAT 'POST1'        '项目描述'         '' '' '' '' '' 'PROJ' 'POST1'.
  INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' 'MARA' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' 'MAKT' 'MAKTX'.
  INIT_FIELDCAT 'MENGE'        '预算数量'         '' '' '' '' '' 'EBKN' 'MENGE'.
  INIT_FIELDCAT 'MENGE2'        '已申请数量'         '' '' '' '' '' 'EBKN' 'MENGE'.
  INIT_FIELDCAT 'MENGE3'        '本次申购数'         '' '' '' 'X' '' 'EBKN' 'MENGE'.
  INIT_FIELDCAT 'BUDAT'        '需求日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE4'        '超预算数'         '' '' '' '' '' 'EBKN' 'MENGE'.
  INIT_FIELDCAT 'USRNAM'        '申请人'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'EKGRP'        '采购组'         '' '' '' 'X' '' 'T024' 'EKGRP'.
  INIT_FIELDCAT 'TXT'        'BOM项目文本'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'BEDNR'        '需求跟踪号'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'TYPE'        '消息类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MSG'        '返回消息'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIGHT'        '显示灯'         '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0574   text
*      -->P_0575   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT  TABLES   PT_LVC
*                          PT_SORT
                          PT_DATA
                 USING
                          PU_STATUS
                          PU_UCOMM
                          PW_LAYOUT
*                          PW_VARIANT
                          PW_GRID_SETTINGS.


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
*     IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = 'A'
*     IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS
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

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

*  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
*
*  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*    IMPORTING
*      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&SAVE'.
*      READ TABLE GT_ALV INTO GS_ALV INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
      READ TABLE GT_ALV INTO GS_ALV WITH KEY SEL = 'X'.
      IF SY-SUBRC <> 0 .
        MESSAGE '没有选中的数据！' TYPE 'E' DISPLAY LIKE 'S' .
      ENDIF.
      PERFORM FRM_CRT_PR USING GT_ALV .

*      IF RS_SELFIELD-FIELDNAME = 'PSPID'
*        AND GS_DATA-PSPID IS NOT INITIAL.
*        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
*        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
*      ENDIF.

  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.

ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  PERFORM FRM_SET_ENTER .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_ENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_ENTER .
  GS_EVENT-NAME = 'CALLER_EXIT'.
  GS_EVENT-FORM = 'FRM_ENTER'.
  APPEND GS_EVENT TO GT_EVENTS .
  CLEAR GS_EVENT .
ENDFORM.
FORM FRM_ENTER USING PS_DATA TYPE SLIS_DATA_CALLER_EXIT.
  DATA LT_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER .
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GR_GRID.
* 设置ENTER事件
  CALL METHOD GR_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
  CREATE OBJECT LT_EVENT_RECEIVER .
  SET HANDLER LT_EVENT_RECEIVER->HANDLE_MODIFY FOR GR_GRID .

ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  FRM_CRT_PR
*&---------------------------------------------------------------------*
*       创建PR
*----------------------------------------------------------------------*
*      -->P_GT_ALV  text
*----------------------------------------------------------------------*
FORM FRM_CRT_PR  USING    P_GT_ALV.
  DATA: LT_ALV TYPE TABLE OF TY_ALV,
        LS_ALV TYPE TY_ALV.
  FIELD-SYMBOLS <FS_ALV> TYPE TY_ALV .
  ASSIGN LS_ALV TO <FS_ALV> .
  TYPES:BEGIN OF TY_ALV2,
          POSID      TYPE PRPS-POSID, "项目定义或WBS元素外码
          SEL        TYPE C,
          NUM        TYPE I,
          POST1      TYPE PROJ-POST1, "项目定义名称
          MATNR      TYPE MARA-MATNR, "物料号
          MAKTX      TYPE MAKT-MAKTX, "物料描述
          MENGE(20)  TYPE C, "预算数量
          MENGE2(17) TYPE C, "已申请数量
          MENGE3(17) TYPE C, "本次申购数
          BUDAT      TYPE SY-DATUM,  "需求日期
          MENGE4(17) TYPE C, "超预算数
          USRNAM     TYPE STRING,    "申请人
          EKGRP      TYPE T024-EKGRP,    "采购组
          TYPPS      TYPE KIS1-TYPPS , "项目类型
          TXT        TYPE STRING,   " BOM 项目文本
          BEDNR      TYPE EKPO-BEDNR , " 需求跟踪号
* 返回消息
          TYPE       TYPE C,
          MSG        TYPE STRING,
          LIGHT(10)  TYPE C , "灯
        END OF TY_ALV2 .
  DATA LS_ALV2 TYPE TY_ALV2 .
  FIELD-SYMBOLS <FS_ALV2> TYPE TY_ALV2  .
  ASSIGN LS_ALV2 TO <FS_ALV2> .

  DATA L_MSG TYPE STRING .
  DATA L_LINE TYPE STRING .

* 从自建表取数判断表里的公司代码是否超预算
  DATA: LT_ZPST001 TYPE TABLE OF ZPST001,
        LS_ZPST001 TYPE ZPST001.
  DATA L_BUKRS(4) TYPE C .
  DATA L_NUM2 TYPE C .
  SELECT *
    INTO TABLE LT_ZPST001
    FROM ZPST001 .
  IF LT_ZPST001 IS NOT INITIAL .
    LOOP AT GT_ALV INTO GS_ALV WHERE SEL = 'X'.
      L_BUKRS = GS_ALV-POSID+0(4) .
      READ TABLE LT_ZPST001 INTO LS_ZPST001 WITH KEY BUKRS = L_BUKRS .
      IF SY-SUBRC = 0 .
        IF GS_ALV-MENGE4 IS NOT INITIAL .
          L_NUM2 = GS_ALV-NUM .
          CONCATENATE '第' L_NUM2 '行超预算，不能挂接！' INTO L_MSG .
          MESSAGE L_MSG TYPE 'E' DISPLAY LIKE 'S'.
          RETURN .
        ELSEIF GS_ALV-MENGE2 + GS_ALV-MENGE3 GT GS_ALV-MENGE .
          L_NUM2 = GS_ALV-NUM .
          CONCATENATE '第' L_NUM2 '行"本次申购数"加"已申请数"超预算，不能挂接！' INTO L_MSG .
          MESSAGE L_MSG TYPE 'E' DISPLAY LIKE 'S'.
          RETURN .
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT GT_ALV INTO GS_ALV WHERE SEL = 'X'.
    IF GS_ALV-MENGE3 IS INITIAL .
      L_LINE = GS_ALV-NUM .
      CONCATENATE '第' L_LINE '行的 "本次申购数量" 字段不能为空！' INTO L_MSG .
      MESSAGE L_MSG TYPE 'E' DISPLAY LIKE 'S'.
      RETURN .
    ELSEIF GS_ALV-USRNAM IS INITIAL .
      L_LINE = GS_ALV-NUM .
      CONCATENATE '第' L_LINE '行的 "申请人" 字段不能为空！' INTO L_MSG .
      MESSAGE L_MSG TYPE 'E' DISPLAY LIKE 'S'.
      RETURN .
    ELSEIF GS_ALV-EKGRP IS INITIAL .
      L_LINE = GS_ALV-NUM .
      CONCATENATE '第' L_LINE '行的 "采购组" 字段不能为空！' INTO L_MSG .
      MESSAGE L_MSG TYPE 'E' DISPLAY LIKE 'S'.
      RETURN .
    ENDIF.
    MOVE-CORRESPONDING GS_ALV TO LS_ALV .
    APPEND LS_ALV TO LT_ALV .
    CLEAR: LS_ALV ,GS_ALV .
  ENDLOOP.

  SORT LT_ALV BY POSID .

* 取网络号
  DATA: BEGIN OF LS_PROJ,
          PSPNR TYPE PROJ-PSPNR,
          PSPID TYPE PROJ-PSPID,
        END OF LS_PROJ .
  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  DATA: BEGIN OF LS_PRPS,
          PSPNR TYPE PRPS-PSPNR,
          POSID TYPE PRPS-POSID,
          PSPHI TYPE PRPS-PSPHI,
        END OF LS_PRPS .
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .
  DATA: BEGIN OF LS_AUFK,
          AUFNR TYPE AUFK-AUFNR,
          PSPEL TYPE AUFK-PSPEL,
        END OF LS_AUFK.
  DATA LT_AUFK LIKE TABLE OF LS_AUFK .


*  IF LT_PROJ IS NOT INITIAL .
  SELECT PSPNR
         POSID
         PSPHI
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN LT_ALV
   WHERE POSID = LT_ALV-POSID .
  IF LT_PRPS IS NOT INITIAL  .
    SELECT AUFNR
           PSPEL
      INTO CORRESPONDING FIELDS OF TABLE LT_AUFK
      FROM AUFK
       FOR ALL ENTRIES IN LT_PRPS
     WHERE PSPEL = LT_PRPS-PSPNR .

    SELECT PSPNR
           PSPID
      INTO CORRESPONDING FIELDS OF TABLE LT_PROJ
      FROM PROJ
       FOR ALL ENTRIES IN LT_PRPS
     WHERE PSPNR = LT_PRPS-PSPHI .
  ENDIF.
*  ENDIF.

  SORT LT_PROJ BY PSPNR .
  SORT LT_PRPS BY POSID .
  SORT LT_AUFK BY PSPEL .

* 取作业号
  DATA: BEGIN OF LS_AFKO,
          AUFNR TYPE AFKO-AUFNR,
          PRONR TYPE AFKO-PRONR,
          AUFPL TYPE AFKO-AUFPL,
        END OF LS_AFKO.
  DATA LT_AFKO LIKE TABLE OF LS_AFKO .
  DATA: BEGIN OF LS_AFVC,
          AUFPL TYPE AFVC-AUFPL,
          APLZL TYPE AFVC-APLZL,
          VORNR TYPE AFVC-VORNR,
        END OF LS_AFVC.
  DATA LT_AFVC LIKE TABLE OF LS_AFVC .

  IF LT_PROJ IS NOT INITIAL .
    SELECT AUFNR
           PRONR
           AUFPL
      INTO CORRESPONDING FIELDS OF TABLE LT_AFKO
      FROM AFKO
       FOR ALL ENTRIES IN LT_PROJ
     WHERE PRONR = LT_PROJ-PSPNR .
    IF LT_AFKO IS NOT INITIAL  .
      SELECT AUFPL
             APLZL
             VORNR
        INTO CORRESPONDING FIELDS OF TABLE LT_AFVC
        FROM AFVC
         FOR ALL ENTRIES IN LT_AFKO
       WHERE AUFPL = LT_AFKO-AUFPL .
    ENDIF.
  ENDIF.

  SORT LT_AFKO BY PRONR .
  SORT LT_AFVC BY AUFPL .

* 调用函数创建PR
  DATA L_NETWORK TYPE AUFNR .
  DATA L_WBS TYPE PRPS-POSID .
  DATA L_ACTIV TYPE CN_VORNR .
  DATA LS_MSG TYPE ZPS001_01 .
  DATA LT_COMPS TYPE TABLE OF ZPS001 WITH HEADER LINE .
  DATA LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE .

  LOOP AT LT_ALV INTO LS_ALV .

    MOVE-CORRESPONDING LS_ALV TO LS_ALV2 .

*    AT NEW POSID .
    "取网络号
*      READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPID = LS_ALV2-PSPID BINARY SEARCH .
*      IF SY-SUBRC = 0 .
*    READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_ALV2-POSID BINARY SEARCH .
*    IF SY-SUBRC = 0 .
*      READ TABLE LT_AUFK INTO LS_AUFK WITH KEY PSPEL = LS_PRPS-PSPNR BINARY SEARCH .
*      IF SY-SUBRC = 0 .
*        L_NETWORK = LS_AUFK-AUFNR .
*        CLEAR LS_AUFK .
*      ENDIF.
*      CLEAR LS_PRPS .
*    ENDIF.
*        CLEAR LS_PROJ .
*      ENDIF.
*    ENDAT .

    "取作业号
    READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_ALV2-POSID BINARY SEARCH .
    IF SY-SUBRC = 0 .
      L_WBS = LS_PRPS-POSID .
*      READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPNR = LS_PRPS-PSPHI BINARY SEARCH .
*      IF SY-SUBRC = 0 .
*        READ TABLE LT_AFKO INTO LS_AFKO WITH KEY PRONR = LS_PROJ-PSPNR BINARY SEARCH .
*        IF SY-SUBRC = 0 .
*          READ TABLE LT_AFVC INTO LS_AFVC WITH KEY AUFPL = LS_AFKO-AUFPL BINARY SEARCH .
*          IF SY-SUBRC = 0 .
*            L_ACTIV = LS_AFVC-VORNR .
*            CLEAR LS_AFVC .
*          ENDIF.
*          CLEAR LS_AFKO .
*        ENDIF.
*        CLEAR LS_PROJ .
*      ENDIF.
      CLEAR LS_PRPS .
    ENDIF.

*    IF L_NETWORK IS INITIAL .
*      MESSAGE '项目定义对应的网络号为空！' TYPE 'E' DISPLAY LIKE 'S' .
*    ENDIF.
*    IF L_ACTIV IS INITIAL .
*      MESSAGE '项目定义对应的作业号为空！' TYPE 'E' DISPLAY LIKE 'S' .
*    ENDIF.

*    LT_COMPS-ACTIVITY = L_ACTIV.
    LT_COMPS-MATNR = LS_ALV2-MATNR.
    LT_COMPS-MENGE = LS_ALV2-MENGE3.
    LT_COMPS-ITEM_CAT = LS_ALV2-TYPPS .
    LT_COMPS-REQ_DATE = P_BUDAT.
    LT_COMPS-DISGR = LS_ALV2-USRNAM .
    LT_COMPS-TXT = LS_ALV2-TXT .
*    LT_COMPS-TYPE_OF_PUR_RESV = '5'.
*    IF I_COMPS-ITEM_CAT = 'L'.
*    LT_COMPS-MRP_RELEVANT = '3'.
*    ENDIF.
    LT_COMPS-PUR_GROUP = LS_ALV2-EKGRP.
    LT_COMPS-BEDNR = LS_ALV2-BEDNR.
    APPEND LT_COMPS .

*    AT END OF POSID .
    CALL FUNCTION 'ZPS001'
      EXPORTING
*       I_NETWORK = L_NETWORK
        I_WBS    = L_WBS
        I_DYXTM  = 'ZPS012'
      IMPORTING
        E_MSG    = LS_MSG
      TABLES
        I_COMPS  = LT_COMPS[]
        E_RETURN = LT_RETURN[].

    IF LS_MSG-TYPE = 'E'.
      LS_ALV2-TYPE = 'E'.
      LOOP AT LT_RETURN WHERE TYPE = 'E' .
        CONCATENATE LS_ALV2-MSG LT_RETURN-MESSAGE ';' INTO LS_ALV2-MSG .
      ENDLOOP.
      CONCATENATE '挂接失败：' LS_ALV2-MSG INTO LS_ALV2-MSG .
*      LS_ALV2-MSG = LT_RETURN-MESSAGE .
      LS_ALV2-LIGHT = ICON_RED_LIGHT .
    ELSEIF LS_MSG-TYPE = 'S' OR LS_MSG-TYPE IS INITIAL .
      LS_ALV2-TYPE = 'S'.
      LS_ALV2-MSG =  LS_MSG-MESSAGE.
      LS_ALV2-LIGHT = ICON_GREEN_LIGHT .
      " 更新已申请数量、超预算数
      LS_ALV2-MENGE2 = LS_ALV2-MENGE2 + LS_ALV2-MENGE3 .
      LS_ALV2-MENGE4 = LS_ALV2-MENGE - LS_ALV2-MENGE2 - LS_ALV2-MENGE3 .
      IF LS_ALV2-MENGE4 GE 0 .
        CLEAR LS_ALV2-MENGE4 .
      ELSEIF LS_ALV2-MENGE4 LT 0 .
        LS_ALV2-MENGE4 = ABS( LS_ALV2-MENGE4 ) .
      ENDIF.
    ENDIF.

    CLEAR LS_ALV .
    MOVE-CORRESPONDING LS_ALV2 TO LS_ALV .
    MODIFY LT_ALV FROM LS_ALV  .

    CLEAR: L_NETWORK,L_WBS,L_ACTIV .
    REFRESH: LT_COMPS[],LT_RETURN[].
    CLEAR: LS_MSG,LT_COMPS,LT_RETURN .
*    ENDAT .
    CLEAR LS_ALV2.

  ENDLOOP.

* 将LT_ALV的值返回给GT_ALV
  SORT GT_ALV BY NUM .
  LOOP AT LT_ALV INTO LS_ALV.
    READ TABLE GT_ALV INTO GS_ALV WITH KEY NUM = LS_ALV-NUM BINARY SEARCH .
    IF SY-SUBRC = 0 .
*      MOVE-CORRESPONDING LS_ALV TO GS_ALV .
      GS_ALV-TYPE = LS_ALV-TYPE .
      GS_ALV-MSG = LS_ALV-MSG .
      GS_ALV-LIGHT = LS_ALV-LIGHT .
      GS_ALV-MENGE2 = LS_ALV-MENGE2 .
      GS_ALV-MENGE4 = LS_ALV-MENGE4 .
      MODIFY GT_ALV FROM GS_ALV TRANSPORTING TYPE MSG LIGHT MENGE2 MENGE4 WHERE NUM = GS_ALV-NUM .
      CLEAR LS_ALV .
      CLEAR GS_ALV .
    ENDIF.
  ENDLOOP.

  REFRESH LT_ALV .

ENDFORM.
