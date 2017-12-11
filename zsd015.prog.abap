*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZSD015
*内容描述                   销售订单对应采购订单跟踪报表
*版本       V1.0
*姓名       销售订单对应采购订单跟踪报表
*日期       14.04.2015 14:11:06
*-----------------------------------------------------------------------------
REPORT ZSD015.
TABLES : VBAK,VBAP,EKKN,EKKO,EKPO.
TYPE-POOLS : SLIS.

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


DATA:BEGIN OF IT_OUT OCCURS 0.
        INCLUDE TYPE ZVBAKAP.
        INCLUDE TYPE ZEKKOPN.
DATA BELNR TYPE EKBE-BELNR.
DATA BUZEI  TYPE EKBE-BUZEI.
DATA VBDSCR(200) TYPE C.
*       vbeln    TYPE vbak-vbeln,
*       auart    TYPE vbak-auart,
*       bezei    TYPE tvakt-bezei,
*       erdat    TYPE vbak-erdat,
*       vbdscr  (200)   TYPE c,
*       vkorg    TYPE vbak-vkorg,
*       vtext    TYPE tvkot-vtext,
*       posnr    TYPE vbap-posnr,
*       matnr    TYPE vbap-matnr,
*       arktx    TYPE vbap-arktx,
*       kwmeng   TYPE vbap-kwmeng,
*       vrkme    TYPE vbap-vrkme,
*       bedae    TYPE vbap-bedae,
*       bdtxt    TYPE T459W-bdtxt,
*       ebeln    TYPE ekkn-ebeln,
*       ebelp    TYPE ekkn-ebelp,
*       loekz    TYPE ekpo-loekz,
*       sobkz    TYPE ekpo-sobkz,
*       frgke    TYPE ekko-frgke,
*       vbeln_st TYPE ekbe-vbeln_st,
*       vbelp_st TYPE ekbe-vbelp_st,
DATA:   END OF IT_OUT.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002 .

SELECT-OPTIONS: S_VBELN       FOR VBAK-VBELN   MATCHCODE OBJECT VMVA ,
                S_AUART       FOR VBAK-AUART    ,
                S_VKORG       FOR VBAK-VKORG  ,
                S_KUNNR       FOR VBAK-KUNNR  ,
                S_MATNR       FOR VBAP-MATNR ,
                S_EBELN       FOR EKKN-EBELN MATCHCODE OBJECT MEKK,
                S_LOEKZ       FOR EKPO-LOEKZ,
                S_FRGKE       FOR EKKO-FRGKE.


*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
*  s_loekz-sign = 'I'.
*  s_loekz-option = 'NE'.
*  s_loekz-low = 'L'.
*  APPEND s_loekz.
*  s_frgke-sign = 'I'.
*  s_frgke-option = 'NE'.
*  s_frgke-low = 'R'.
*  APPEND s_frgke.

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
  PERFORM DATA_RETRIVEL.
  PERFORM BUILD_FIELDCATALOG.
  PERFORM DISPLAY_ALV_REPORT.



FORM FRM_AUTH_CHECK.
*  DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.
*  SELECT werks
*    FROM t001w
*    INTO CORRESPONDING FIELDS OF TABLE lt_t001w
*    WHERE werks IN s_werk.
*  LOOP AT lt_t001w WHERE werks IN s_werk.
*    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
**            ID 'ACTVT' FIELD '__________'
*             ID 'WERKS' FIELD lt_t001w-werks.
*    IF sy-subrc <> 0.
*      MESSAGE e603(fco) WITH lt_t001w-werks.
*    ENDIF.
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVEL .
  DATA LT_ZVBAKAP TYPE TABLE OF ZVBAKAP WITH HEADER LINE.
  DATA LT_ZEKKOPN TYPE TABLE OF ZEKKOPN WITH HEADER LINE.
  DATA LT_EKBE    TYPE TABLE OF EKBE WITH HEADER LINE.

  SELECT * FROM ZVBAKAP
    INTO CORRESPONDING FIELDS OF TABLE LT_ZVBAKAP
    WHERE VBELN IN S_VBELN
    AND AUART IN S_AUART
    AND VKORG IN S_VKORG
    AND KUNNR IN S_KUNNR
    AND MATNR IN S_MATNR
    .
  IF LT_ZVBAKAP[] IS NOT INITIAL .

    SELECT * FROM ZEKKOPN
      INTO TABLE LT_ZEKKOPN
      FOR ALL ENTRIES IN LT_ZVBAKAP
      WHERE VBELN_EK = LT_ZVBAKAP-VBELN
      AND   VBELP_EK = LT_ZVBAKAP-POSNR
      .

    IF LT_ZEKKOPN[] IS  NOT INITIAL.
      SELECT EBELN
             EBELP
             BELNR
             BUZEI
        FROM EKBE
        INTO CORRESPONDING FIELDS OF TABLE LT_EKBE
        FOR ALL ENTRIES IN LT_ZEKKOPN
        WHERE EBELN = LT_ZEKKOPN-EBELN
        AND   EBELP = LT_ZEKKOPN-EBELP
        AND   BEWTP = 'L'.
    ENDIF.
  ENDIF.


*&--代码添加 BY HANDYBY 26.04.2017 17:02:07  BEGIN
  SORT LT_ZEKKOPN BY VBELN_EK VBELP_EK .
  SORT LT_EKBE BY EBELN EBELP .

  LOOP AT LT_ZVBAKAP.
    READ TABLE LT_ZEKKOPN WITH KEY VBELN_EK = LT_ZVBAKAP-VBELN
                                   VBELP_EK = LT_ZVBAKAP-POSNR
                                   BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT LT_ZEKKOPN FROM SY-TABIX .
        IF LT_ZEKKOPN-VBELN_EK = LT_ZVBAKAP-VBELN AND
            LT_ZEKKOPN-VBELP_EK = LT_ZVBAKAP-POSNR .
          READ TABLE LT_EKBE WITH KEY EBELN = LT_ZEKKOPN-EBELN
                                      EBELP = LT_ZEKKOPN-EBELP
                                      BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT LT_EKBE FROM SY-TABIX.
              IF LT_EKBE-EBELN = LT_ZEKKOPN-EBELN AND
                   LT_EKBE-EBELP = LT_ZEKKOPN-EBELP .
                MOVE-CORRESPONDING LT_ZVBAKAP TO IT_OUT.
                MOVE-CORRESPONDING LT_ZEKKOPN TO IT_OUT.
                PERFORM FRM_READ_TEXT USING IT_OUT-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-VBDSCR.
                IT_OUT-BELNR = LT_EKBE-BELNR.
                IT_OUT-BUZEI = LT_EKBE-BUZEI.
                APPEND IT_OUT.
                CLEAR IT_OUT.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            IF SY-SUBRC <> 0.
              MOVE-CORRESPONDING LT_ZVBAKAP TO IT_OUT.
              MOVE-CORRESPONDING LT_ZEKKOPN TO IT_OUT.
              PERFORM FRM_READ_TEXT USING IT_OUT-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-VBDSCR.
              APPEND IT_OUT.
              CLEAR IT_OUT.
            ENDIF.
          ENDIF.
        ELSE.
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
*&--代码添加 BY HANDYBY 26.04.2017 17:02:07  END


*&--代码注释 BY HANDYBY 26.04.2017 17:02:29  BEGIN
*  LOOP AT LT_ZVBAKAP .
*    LOOP AT LT_ZEKKOPN WHERE  VBELN_EK    =  LT_ZVBAKAP-VBELN
*                             AND  VBELP_EK = LT_ZVBAKAP-POSNR.
*      LOOP AT LT_EKBE WHERE EBELN = LT_ZEKKOPN-EBELN
*                            AND EBELP = LT_ZEKKOPN-EBELP.
*        MOVE-CORRESPONDING LT_ZVBAKAP TO IT_OUT.
*        MOVE-CORRESPONDING LT_ZEKKOPN TO IT_OUT.
*        PERFORM FRM_READ_TEXT USING IT_OUT-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-VBDSCR.
*        IT_OUT-BELNR = LT_EKBE-BELNR.
*        IT_OUT-BUZEI = LT_EKBE-BUZEI.
*        APPEND IT_OUT.
*        CLEAR IT_OUT.
*      ENDLOOP.
*      IF SY-SUBRC <> 0.
*        MOVE-CORRESPONDING LT_ZVBAKAP TO IT_OUT.
*        MOVE-CORRESPONDING LT_ZEKKOPN TO IT_OUT.
*        PERFORM FRM_READ_TEXT USING IT_OUT-VBELN SY-LANGU 'Z001' 'VBBK' CHANGING IT_OUT-VBDSCR.
*        APPEND IT_OUT.
*        CLEAR IT_OUT.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*&--代码注释 BY HANDYBY 26.04.2017 17:02:29  END

*    DELETE it_out WHERE  ebeln not in s_ebeln or loekz not in s_loekz or FRGKE not in s_FRGKE.

  DELETE IT_OUT WHERE  EBELN NOT IN S_EBELN .
  DELETE IT_OUT WHERE  LOEKZ NOT IN S_LOEKZ .
  DELETE IT_OUT WHERE   FRGKE NOT IN S_FRGKE.

ENDFORM.                    " DATA_RETRIVEL


*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
  CLEAR IT_FIELDCAT[].
  PERFORM FRM_FILL_CAT USING :
           1  '' 'VBELN'   '销售订单' , "销售订单
           2  '' 'AUART'   '订单类型' , "订单类型
           3  '' 'BEZEI'   '订单类型名称' , "订单类型名称
           4  '' 'ERDAT'   '凭证日期' , "凭证日期
           5  '' 'VBDSCR'   '项目名称' , "项目名称
           6  '' 'VKORG'   '销售组织' , "销售组织
           7  '' 'VTEXT'   '销售组织名称' , "销售组织名称
           8  '' 'POSNR'   '销售订单行项目' , "销售订单行项目
           9  '' 'MATNR'   '物料编号' , "物料编号
           10 '' 'ARKTX'   '物料名称' , "物料名称
           11 '' 'KWMENG'   '订单数量' , "订单数量
           12 '' 'VRKME'   '单位' , "单位
           13 '' 'BEDAE'   '需求类型' , "需求类型
           14 '' 'BDTXT'   '需求类型描述' , "需求类型描述
           15 '' 'EBELN'   '采购订单' , "采购订单
           16 '' 'EBELP'   '采购订单行项目' , "采购订单行项目
           17 '' 'LOEKZ'   '采购订单删除标识' , "采购订单删除标识
           18 '' 'SOBKZ'   '特殊库存' , "特殊库存
           19 '' 'FRGKE'   '采购订单审批标识' , "采购订单审批标识
           20 '' 'BELNR'   '交货单编号' , "交货单编号
           21 '' 'BUZEI'   '交货单行项目' , "交货单行项目
           22 '' 'VDATU'   '请求交货日期'.     "请求交货日期
ENDFORM.                    " BUILD_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  frm_fill_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_POS      text
*      -->U_EDIT     text
*      -->U_FNAME    text
*      -->U_NAME     text
*----------------------------------------------------------------------*
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "frm_fill_cat

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_REPORT .

  DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.

* l_layout-CWIDTH_OPT = 'X'.
*  l_layout-box_fieldname = 'ZBOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'TOP-OF-PAGE'  "see FORM
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      IT_FIELDCAT            = IT_FIELDCAT[]
      I_SAVE                 = 'X'
      I_GRID_SETTINGS        = L_GRID_SETTINGS
      IS_LAYOUT              = L_LAYOUT
      IS_VARIANT             = G_VARIANT
    TABLES
      T_OUTTAB               = IT_OUT
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_REPORT" DISPLAY_ALV_REPORT

*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM TOP-OF-PAGE.
*ALV Header declarations
  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.
* Title
  WA_HEADER-TYP  = 'H'.
  WA_HEADER-INFO =  SY-TITLE."'进料检验记录表打印'.
  APPEND WA_HEADER TO T_HEADER.
  CLEAR WA_HEADER.
* Date
  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Date: '.
  CONCATENATE  SY-DATUM+6(2) '.'
               SY-DATUM+4(2) '.'
               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
  APPEND WA_HEADER TO T_HEADER.
  CLEAR: WA_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.
ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING R_UCOMM LIKE SY-UCOMM
                                   RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE SY-UCOMM.
    WHEN '&ENTER'.
    WHEN '&DATA_SAVE'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_SAVE_DATA.

ENDFORM.                    "frm_save_data

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  SET PF-STATUS 'ZSD015_STATUS'.
ENDFORM.                    "set_pf_status



*&---------------------------------------------------------------------*
*&      Form  frm_read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_TDNAME   text
*      -->T_TDSPRAS  text
*      -->T_TDID     text
*      -->T_TDOBJECT text
*      -->T_TEXT     text
*----------------------------------------------------------------------*
FORM FRM_READ_TEXT USING T_TDNAME T_TDSPRAS T_TDID T_TDOBJECT CHANGING T_TEXT.
  DATA:LT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.
*  DATA:stxl LIKE stxl OCCURS 0 WITH HEADER LINE."抬头备注
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
      ID                      = L_STXL-TDID    "读取文本的id
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

*  DATA: itemp LIKE thead-tdname."itemp为变量无值

  LOOP AT LT_TLINE .
    CONCATENATE T_TEXT LT_TLINE-TDLINE INTO T_TEXT SEPARATED BY SPACE.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext
