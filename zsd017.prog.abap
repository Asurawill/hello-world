*----------------------------------------------------------------------------
*模块	SD
*
*请求类型           PROG:ZSD017
*内容描述           交货单领料打印
*版本       V1.0
*姓名       LEYARDIT02
*日期       20.04.2015 14:26:23
*-----------------------------------------------------------------------------
REPORT ZSD017.
TABLES:LIPS,LIKP,ZEV_SD017.
TYPE-POOLS:SLIS.
**INTERNAL TABLE DECLARTION
DATA :
  GR_ALV     TYPE REF TO CL_SALV_TABLE,
  GR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV WITH HEADER LINE,

      G_SAVE      TYPE C VALUE 'X',
      G_VARIANT   TYPE DISVARIANT,
      GX_VARIANT  TYPE DISVARIANT,
      G_EXIT      TYPE C,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.
TYPES:BEGIN OF TY_OUT,
     VBELN LIKE LIKP-VBELN,"交货单编号
      VGBEL LIKE LIPS-VGBEL, "订单编号
      VGPOS LIKE ZEV_SD017-VGPOS,
      VGSTRING(255) TYPE  C               ,    "项目名称
      ERDAT LIKE ZEV_SD017-ERDAT,"创建日期
      LFART LIKE LIKP-LFART ,"交货单类型
      LFART_VTEXT LIKE ZEV_SD017-LFART_VTEXT,"交货单类型描述
      VTEXT LIKE TVLKT-VTEXT,"交货单类型描述
      VSTEL LIKE LIKP-VSTEL,"装运点
      VSTEL_VTEXT LIKE TVSTT-VTEXT,"装运点描述
      VKORG LIKE TVKOT-VKORG,"销售组织
      VKORG_VTEXT LIKE TVKOT-VTEXT,"销售组织描述
      POSNR LIKE ZEV_SD017-POSNR,"行项目编号
      MATNR LIKE LIPS-MATNR,"物料编码
      ARKTX LIKE LIPS-ARKTX,"物料名称
      LFIMG LIKE LIPS-LFIMG,"数量
      MEINS LIKE LIPS-MEINS ,"单位
      CHARG LIKE LIPS-CHARG ,"批次
      WERKS LIKE ZEV_SD017-WERKS,"工厂
      NAME1 LIKE T001W-NAME1,"工厂描述
      LGORT LIKE LIPS-LGORT,"库位
      LGOBE LIKE T001L-LGOBE,"库位描述
      ERNAM LIKE ZEV_SD017-ERNAM,"创建人
      ZBOX TYPE C,
     END OF TY_OUT.
DATA: IT_OUT TYPE TY_OUT OCCURS 0 WITH HEADER LINE.
DATA:IT_OUT2 TYPE TY_OUT OCCURS 0 WITH HEADER LINE.
DATA: IT_PRT TYPE TABLE OF TY_OUT.
DATA:WA_PRT TYPE TY_OUT.
DATA:IT_VBAK LIKE TABLE OF VBAK WITH HEADER LINE.
DATA:IT_EKKN LIKE TABLE OF EKKN WITH HEADER LINE.
  DATA:NT_PRT TYPE TABLE OF TY_OUT .

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002 .
SELECT-OPTIONS:P_VSTEL FOR LIKP-VSTEL ,
               P_VKORG FOR LIKP-VKORG OBLIGATORY,
               P_VBELN FOR LIKP-VBELN MATCHCODE OBJECT VMVL,
               P_WADAT FOR LIKP-WADAT,
               P_LFART FOR LIKP-LFART,
               P_VGBEL FOR LIPS-VGBEL.

SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.

  GX_VARIANT-REPORT = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    G_VARIANT = GX_VARIANT-VARIANT.
  ENDIF.
  AT SELECTION-SCREEN.
  PERFORM FRM_AUTH_CHECK.

**PERFORM DECLARATIONS
START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  IF P_VSTEL[] is INITIAL AND P_VKORG[] IS INITIAL AND P_VBELN[] IS INITIAL AND P_WADAT[] IS INITIAL AND P_LFART[] IS INITIAL AND P_VGBEL[] IS INITIAL.
    MESSAGE '请至少选择一个查询条件' TYPE 'E'.
   ENDIF.
DATA:LT_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
  SELECT VKORG FROM TVKO
    INTO CORRESPONDING FIELDS OF TABLE LT_TVKO
    WHERE VKORG IN P_VKORG
    .
 LOOP AT LT_TVKO WHERE VKORG IN P_VKORG.
   AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
            ID 'VKORG' FIELD LT_TVKO-VKORG
            .
   IF SY-SUBRC <> 0.
      MESSAGE E430(VELO) WITH LT_TVKO-VKORG.
   ENDIF.
 ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_DATA .
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUT
  FROM ZEV_SD017                                    "从外部取出相应的基础数据LIKP&LIPS&TVLKT&TVSTT&TVKOT&T001W&T001L
  WHERE VSTEL IN P_VSTEL AND VKORG IN P_VKORG AND VBELN IN P_VBELN AND WADAT IN P_WADAT
  AND LFART IN P_LFART AND VGBEL IN P_VGBEL.
  IF IT_OUT[] IS NOT INITIAL.
    SORT IT_OUT BY VBELN POSNR VGBEL.
    IT_OUT2[] = IT_OUT[].
    DELETE ADJACENT DUPLICATES FROM IT_OUT2 COMPARING VGBEL.
   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_VBAK
     FROM VBAK
     FOR ALL ENTRIES IN IT_OUT2     "根据交货单号取出的订单编号从VBAK匹配相应的销售订单编号
     WHERE VBELN = IT_OUT2-VGBEL.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_EKKN
     FROM EKKN
     FOR ALL ENTRIES IN IT_OUT    "根据交货单号中的采购订单号、项目号从EKKN取出相应的销售订单号、项目号
     WHERE EBELN = IT_OUT-VGBEL .
     DATA: EBELPTMP LIKE EKKN-EBELP.
     LOOP AT IT_OUT.
       " CONCATENATE '0' IT_OUT-VGPOS INTO EBELPTMP.

       READ TABLE IT_VBAK WITH KEY VBELN = IT_OUT-VGBEL."根据从IT_OUT取出的订单号检查VBAK表是否存在，若存在原号保留。否就从EKKN表根据采购订单号取出相应的销售订单号
       IF SY-SUBRC <> 0.
        "  CLEAR IT_OUT-VGBEL.
          READ TABLE IT_EKKN WITH KEY EBELN = IT_OUT-VGBEL  EBELP = IT_OUT-VGPOS+1(5).
            IF SY-SUBRC = 0.
              IT_OUT-VGBEL = IT_EKKN-VBELN.
              ELSE.
                CLEAR IT_OUT-VGBEL.
              ENDIF.
           ENDIF.
            PERFORM FRM_READ_TEXT USING IT_OUT-VGBEL SY-LANGU 'Z001' 'VBBK'  "取出相应的项目名称
                  CHANGING IT_OUT-VGSTRING.
         MODIFY IT_OUT.
      ENDLOOP.
   ENDIF.
   IF IT_OUT[] is initial.
   MESSAGE '未查询到任何数据' TYPE 'E'.

    endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
 CLEAR IT_FIELDCAT[].
  PERFORM FRM_FILL_CAT USING :
           1  '' 'VGBEL' '订单编号'          , "订单编号
           2  '' 'VGSTRING' '项目名称'          , "项目名称
           3  '' 'VBELN' '交货单编号'          , "交货单编号
           4  '' 'ERDAT' '创建日期'           , "创建日期
           5  '' 'LFART' '交货单类型'          , "交货单类型
           6  '' 'LFART_VTEXT' '交货单类型描述'          , "交货单类型描述
           7  '' 'VSTEL' '装运点'             , "装运点
           8  '' 'VSTEL_VTEXT' '装运点描述'             , "装运点描述
           9  '' 'VKORG' '销售组织'             , "销售组织
           10  '' 'VKORG_VTEXT' '销售组织描述'             , "销售组织描
           11 '' 'POSNR' '行项目编号'          , "行项目编号
           12 '' 'MATNR' '物料编码'          , "物料编码
           13 '' 'ARKTX' '物料名称'          , "物料名称
           14 '' 'LFIMG' '数量'          , "数量
           15 '' 'MEINS' '单位'          , "单位
           16 '' 'CHARG' '批次'          , "批次
           17 '' 'WERKS' '工厂'          , "工厂
           18 '' 'NAME1' '工厂名称'          , "工厂
           19 '' 'LGORT' '库位'          , "库位
           20 '' 'LGOBE' '库位描述'          . "库位描述 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM FRM_READ_TEXT USING T_TDNAME T_TDSPRAS T_TDID T_TDOBJECT CHANGING T_TEXT.
  DATA:LT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.
*  DATA:STXL LIKE STXL OCCURS 0 WITH HEADER LINE."抬头备注
  DATA L_STXL TYPE STXL.
  L_STXL-TDID     = T_TDID     .
  L_STXL-TDSPRAS  = T_TDSPRAS  .
  L_STXL-TDNAME   = T_TDNAME   .
  L_STXL-TDOBJECT = T_TDOBJECT .

*  SELECT SINGLE * FROM STXL INTO STXL
*    WHERE TDNAME = T_TDNAME AND TDID = T_TDID AND TDSPRAS = T_TDSPRAS AND TDOBJECT = T_TDOBJECT.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = L_STXL-TDID    "读取文本的ID
      LANGUAGE                = L_STXL-TDSPRAS "读取文本的语言
      NAME                    = L_STXL-TDNAME    "读取文本的名字
      OBJECT                  = L_STXL-TDOBJECT
    TABLES
      LINES                   = LT_TLINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

*  DATA: ITEMP LIKE THEAD-TDNAME."ITEMP为变量无值

  LOOP AT LT_TLINE .
    CONCATENATE T_TEXT LT_TLINE-TDLINE INTO T_TEXT SEPARATED BY SPACE.  "解决回车事件
  ENDLOOP.

ENDFORM. "READITEMTEXT
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  LW_FIELDCAT-NO_ZERO   = 'X'.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "FRM_FILL_CAT
FORM DISPLAY_ALV_REPORT .

  DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.

* L_LAYOUT-CWIDTH_OPT = 'X'.
  L_LAYOUT-BOX_FIELDNAME = 'ZBOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_SAVE                   = 'X'
      I_GRID_SETTINGS          = L_GRID_SETTINGS
      IS_LAYOUT                = L_LAYOUT
      IS_VARIANT               = G_VARIANT
    TABLES
      T_OUTTAB                 = IT_OUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_REPORT" DISPLAY_ALV_REPORT
*-------------------------------------------------------------------*
* FORM  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV REPORT HEADER                                                 *
*-------------------------------------------------------------------*
FORM TOP-OF-PAGE.
*ALV HEADER DECLARATIONS
  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.
* TITLE
  WA_HEADER-TYP  = 'H'.
  WA_HEADER-INFO =  SY-TITLE."'
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* DATE
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'DATE: '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "TODAYS DATE
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING R_UCOMM LIKE SY-UCOMM
                                   RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE SY-UCOMM.
    WHEN '&ENTER'.
    WHEN '&PRNT'.
      PERFORM FRM_PRINT_DATA.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "USER_COMMAND


*&---------------------------------------------------------------------*
*&      FORM  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM FRM_SAVE_DATA.

ENDFORM.                    "FRM_SAVE_DATA

*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZSD017_STATUS'.
ENDFORM.                    "SET_PF_STATUS


*&---------------------------------------------------------------------*
*&      FORM  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 6,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFSD017'.
  DATA:LT_SELECT LIKE  IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA:LW_SELECT LIKE LINE OF LT_SELECT.
  DATA:LT_PRT LIKE TABLE OF IT_OUT WITH HEADER LINE.
  FIELD-SYMBOLS <LW_PRT> LIKE LINE OF IT_OUT.
  LT_SELECT[] = IT_OUT[].
  DELETE LT_SELECT WHERE ZBOX <>'X'.
  DELETE ADJACENT DUPLICATES FROM LT_SELECT  COMPARING VBELN.
  SORT LT_SELECT BY VBELN.
  CLEAR:LT_PRT[].
  LOOP AT LT_SELECT.
      LOOP AT IT_OUT WHERE VBELN = LT_SELECT-VBELN.
          MOVE-CORRESPONDING IT_OUT TO LT_PRT.
          APPEND LT_PRT.
        ENDLOOP.
   ENDLOOP.


  IF LT_PRT[] IS INITIAL.
    MESSAGE S001(Z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK LT_PRT[] IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "SMARTFORMS的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的SMARTFORMS的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN = 'X'.
  CONTROL-NO_CLOSE = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.
  SORT LT_PRT BY VBELN POSNR VGBEL.
  LOOP AT LT_PRT ASSIGNING <LW_PRT>.
    AT NEW VBELN.
      CLEAR WA_PRT.
      CLEAR NT_PRT[].
      WA_PRT = <LW_PRT>.
    ENDAT.
    APPEND <LW_PRT> TO NT_PRT[].
    AT END OF VBELN.
*      DESCRIBE TABLE IT_PRT LINES NTOTALLINE.
*      NCURRLINE = NTOTALLINE MOD NPAGELINE.
*      IF  NCURRLINE > 0.
*        EMPTYCOUNT = NPAGELINE - NCURRLINE.
*        DO EMPTYCOUNT TIMES.
*          APPEND INITIAL LINE TO IT_PRT.
*        ENDDO.
*      ENDIF.

      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
    "      NPAGE_LINE         = NPAGELINE
*         W_HEAD             = LW_PRT
         TABLES
        T_ITEM             = NT_PRT[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.

  ENDIF.

ENDFORM. "FRM_PRINT_DATA
