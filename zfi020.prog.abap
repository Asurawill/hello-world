REPORT ZFI020.
TABLES:T001 ,LFA1,EKKO ,MSEG,MKPF,EKKN ,T052U  ,T052,PRPS,MAKT.

DATA:BEGIN OF GS_DATA,
      LIFNR   LIKE MSEG-LIFNR,                   "供应商编号
     LIFNR_NAME1 LIKE LFA1-NAME1,           "供应商名称
     EBELN LIKE MSEG-EBELN,                "采购订单编号
     EBELP LIKE MSEG-EBELP,                "订单行项目
     ZTERM LIKE EKKO-ZTERM,               "付款条件
     FK_TEXT1 LIKE  T052U-TEXT1,          "付款条件描述
     XBLNR LIKE MKPF-XBLNR,               "交货单编号
     DMBTR LIKE MSEG-DMBTR,                "交货金额
     SHKZG LIKE MSEG-SHKZG,                "借贷标记
     MWSKZ LIKE EKKN-MWSKZ,                "税码
     HSJE  LIKE  MSEG-DMBTR ,           "含税金额
     ERFMG LIKE MSEG-ERFMG,   "交货数量
     ERFME LIKE MSEG-ERFME,   "单位
     BUDAT_MKPF LIKE MSEG-BUDAT_MKPF  , "交货过账日期,
     ZTAG1 LIKE T052-ZTAG1,       "账期天数
     DQR   TYPE D ,               "到期日
     QQTS  TYPE I,                 "逾期天数
     QQTY  TYPE I,                "逾期月
     QQTN TYPE I,               "逾期年
     PS_PSP_PNR LIKE MSEG-PS_PSP_PNR,  "项目编号
     POST1 LIKE PRPS-POST1,           "项目名称
     MJAHR  LIKE MKPF-MJAHR  ,"会计年度
     MBLNR LIKE MSEG-MBLNR,      "物料凭证号
     ZEILE LIKE MSEG-ZEILE,    "物料凭证行项目
     MAKTX LIKE MAKT-MAKTX,      "物理描述
     MATNR  LIKE  MSEG-MATNR ,   "物料号
     SEL(1),
  END OF GS_DATA.

  DATA:GT_DATA  LIKE TABLE OF GS_DATA WITH HEADER LINE.
  DATA:GT_MAKT  LIKE TABLE OF MAKT   WITH  HEADER LINE.
  DATA: GT_PRPS LIKE TABLE OF PRPS  WITH HEADER LINE.
  DATA: GT_EKKO  LIKE  TABLE OF EKKO WITH HEADER LINE.
  DATA: GT_EKPO  LIKE  TABLE OF EKPO WITH  HEADER LINE.
  DATA: GT_T052U LIKE TABLE OF T052U WITH HEADER LINE.
 " DATA: GT_EKKN LIKE TABLE OF T052U WITH HEADER LINE.
  DATA: GT_LFA1 LIKE TABLE OF LFA1 WITH HEADER LINE.
  DATA: GT_FTAXP LIKE  TABLE OF FTAXP .
  DATA: GS_FTAXP LIKE FTAXP.
  DATA: GT_ZFIFKTJ LIKE TABLE OF ZFIFKTJ  WITH HEADER LINE.
  DATA: GT_T052 LIKE TABLE OF T052 WITH HEADER LINE .
  DATA: GT_T052S LIKE  TABLE OF T052S WITH HEADER LINE.
    DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  IF &4 = 'X'.
    GW_LVC-KEY = 'X'.
  ENDIF.
*    IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
*   IF &1 = 'WRBTR' .
*   GW_LVC-CFIELDNAME = 'WAERS_1'.
*  ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "ALV的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,                      "SY-REPID 指 当前的主程序
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
DATA: INMEG TYPE STRING.

************************************************************************
* GLOBAL VARIANT
************************************************************************
************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
SELECT-OPTIONS: S_LIFNR  FOR LFA1-LIFNR ,    "供应商
                S_EBELN FOR EKKO-EBELN ."采购订单号
SELECTION-SCREEN END OF BLOCK BLK1.
*&---------------------------------------------------------------------*
*& 初始化处理
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.
"&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

**权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    CONCATENATE '无权查询' P_BUKRS '采购收货到期日报表'  INTO  INMEG .
    MESSAGE INMEG   TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示*&---------------------------------------------------------------------*
*& 程序结束处理
  END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_0558   TEXT
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'ZFI_BUK_CK' ID 'ACTVT' FIELD P_ACTVT
                                     ID 'BUKRS' FIELD P_BUKRS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  SELECT A~MBLNR A~MJAHR A~XBLNR  B~ZEILE B~MATNR B~LIFNR B~EBELN B~EBELP
    B~DMBTR B~SHKZG B~ERFMG B~ERFME B~BUDAT_MKPF B~PS_PSP_PNR
    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM MKPF AS A INNER JOIN MSEG AS B
    ON A~MBLNR = B~MBLNR AND A~MJAHR = B~MJAHR
    WHERE B~WERKS = P_BUKRS AND B~LIFNR IN S_LIFNR  AND B~EBELN IN S_EBELN AND B~SMBLN = '' AND B~BWART = '101'.

    SORT GT_DATA BY  LIFNR EBELN EBELP.

 "查询自定义维护的付款条件
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZFIFKTJ
       FROM ZFIFKTJ
       WHERE BUKRS  = P_BUKRS .


  CHECK GT_DATA[] IS NOT INITIAL.
  SELECT PSPNR POST1
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FROM PRPS
    FOR ALL ENTRIES IN GT_DATA
    WHERE PSPNR = GT_DATA-PS_PSP_PNR .
 SORT GT_PRPS BY PSPNR .

  SELECT MATNR MAKTX
    INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FROM MAKT
    FOR ALL ENTRIES IN GT_DATA
    WHERE MATNR = GT_DATA-MATNR AND SPRAS = '1' .
  SORT GT_MAKT BY MATNR.

 SELECT LIFNR NAME1
   INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
   FROM LFA1
   FOR ALL ENTRIES IN GT_DATA
   WHERE LIFNR = GT_DATA-LIFNR .

  SORT GT_LFA1 BY LIFNR .

  SELECT BUKRS EBELN ZTERM
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
    FROM EKKO
    FOR ALL ENTRIES IN GT_DATA
    WHERE BUKRS = P_BUKRS AND EBELN = GT_DATA-EBELN .
    SORT GT_EKKO BY BUKRS EBELN.

  CHECK GT_EKKO[]  IS NOT INITIAL.

 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T052U
   FROM T052U WHERE SPRAS = '1'.

 SORT GT_T052U BY ZTERM .

  SELECT EBELN EBELP  MWSKZ
    INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
   FROM EKPO
   FOR ALL ENTRIES IN GT_EKKO
   WHERE EBELN = GT_EKKO-EBELN .
 SORT GT_EKPO BY EBELN EBELP.

 CHECK GT_ZFIFKTJ[]  IS NOT INITIAL.
   SELECT *  INTO CORRESPONDING FIELDS OF TABLE GT_T052
     FROM  T052 .
  SORT  GT_T052 BY  ZTERM .
 CHECK GT_T052[] IS NOT INITIAL.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T052S
     FROM T052S
     FOR  ALL ENTRIES IN GT_T052
     WHERE ZTERM = GT_T052-ZTERM
          AND
     RATZT NOT IN ( SELECT ZTERM  FROM ZFIFKTJ WHERE BUKRS  = P_BUKRS ) .

SORT GT_T052S BY ZTERM  .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
 LOOP AT GT_DATA.
    IF GT_DATA-SHKZG = 'H'.
        GT_DATA-DMBTR = GT_DATA-DMBTR * -1.
    ENDIF.
    "物料描述
    READ TABLE GT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH .
     IF SY-SUBRC = 0.
         GT_DATA-MAKTX  = GT_MAKT-MAKTX .
     ENDIF.
    "供应商名称
     READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DATA-LIFNR BINARY SEARCH .
      IF SY-SUBRC = 0.
         GT_DATA-LIFNR_NAME1 = GT_LFA1-NAME1.
      ENDIF.
      "项目名称
      READ TABLE GT_PRPS WITH KEY PSPNR = GT_DATA-PS_PSP_PNR BINARY SEARCH.
       IF SY-SUBRC = 0.
          GT_DATA-POST1 = GT_PRPS-POST1.
       ENDIF.
      "付款条件描述

        READ TABLE GT_EKKO  WITH  KEY BUKRS = P_BUKRS EBELN = GT_DATA-EBELN BINARY SEARCH .
        IF SY-SUBRC = 0 .
           GT_DATA-ZTERM = GT_EKKO-ZTERM.
              "计算付款条件对应的到期日
*     DATA: G_JZRQ TYPE D .
*     CLEAR :G_JZRQ.
           READ TABLE GT_T052 WITH KEY ZTERM = GT_DATA-ZTERM  BINARY SEARCH .
           IF SY-SUBRC  = 0 .
              GT_DATA-ZTAG1 =  GT_T052-ZTAG1.  "账期天数
              IF GT_T052-XSPLT NE 'X'.
                READ TABLE GT_ZFIFKTJ WITH KEY ZTERM  = GT_DATA-ZTERM BINARY SEARCH .
                 IF SY-SUBRC = 0.
                     MODIFY GT_DATA.
                   "  CONTINUE.
                   ELSE.
                      PERFORM JSDQR USING GT_DATA-BUDAT_MKPF GT_T052-ZMONA GT_T052-ZFAEL  GT_DATA-ZTAG1 CHANGING GT_DATA-DQR .

                   ENDIF.
             ELSE.
                READ TABLE GT_T052S WITH KEY ZTERM  = GT_DATA-ZTERM  BINARY SEARCH .
                  IF SY-SUBRC = 0.
                      READ TABLE GT_T052 WITH KEY ZTERM = GT_T052S-RATZT BINARY SEARCH.
                       IF SY-SUBRC = 0.
                          GT_DATA-ZTAG1 =  GT_T052-ZTAG1.  "账期天数
                           PERFORM JSDQR USING GT_DATA-BUDAT_MKPF GT_T052-ZMONA GT_T052-ZFAEL GT_DATA-ZTAG1 CHANGING GT_DATA-DQR .
                        ENDIF.

                  ELSE.
                "    CONTINUE.
                  ENDIF.
              ENDIF.
          ENDIF.
         ENDIF.
      READ TABLE GT_T052U WITH KEY  ZTERM = GT_DATA-ZTERM BINARY SEARCH .
        IF SY-SUBRC = 0.
          GT_DATA-FK_TEXT1 = GT_T052U-TEXT1.

        ENDIF.
       "税吗
       READ TABLE GT_EKPO WITH KEY EBELN = GT_DATA-EBELN EBELP = GT_DATA-EBELP BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_DATA-MWSKZ = GT_EKPO-MWSKZ.
           "含税金额
           "含税金额
         CLEAR: GT_FTAXP[] ,GS_FTAXP.
        CALL FUNCTION 'GET_TAX_PERCENTAGE'
            EXPORTING
              ALAND         = 'CN'
              DATAB         = '20000101'
              MWSKZ         = GT_DATA-MWSKZ
              TXJCD         =  'TAXCN'
         "*     EXPORT        = ' '
          TABLES
              T_FTAXP       = GT_FTAXP
               .
         READ TABLE GT_FTAXP INTO GS_FTAXP INDEX 1 .
         IF SY-SUBRC = 0.
              GT_DATA-HSJE = GT_DATA-DMBTR  * ( 1 + GS_FTAXP-KBETR / 1000 ) .
          ENDIF.
       ENDIF.

     IF GT_DATA-DQR  NE '00000000' .

         CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
           EXPORTING
             I_DATE_FROM          =  GT_DATA-DQR
*            I_KEY_DAY_FROM       =
             I_DATE_TO            =  SY-DATUM
*            I_KEY_DAY_TO         =
*            I_FLG_SEPARATE       = ' '
           IMPORTING
             E_DAYS               =  GT_DATA-QQTS
            E_MONTHS             =  GT_DATA-QQTY
           E_YEARS              =   GT_DATA-QQTN
                 .

      ENDIF.

   MODIFY GT_DATA.
 ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW .
 PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_BUILD_EVENT.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
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
GW_LAYOUT-ZEBRA        = 'X'.
  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  GW_LAYOUT-BOX_FNAME = 'SEL'.
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
    INIT_FIELDCAT 'LIFNR'  '供应商编号'  '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LIFNR_NAME1'          '供应商名称'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELN'          '采购订单编号'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELP'          '订单行项目'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'ZTERM'          '付款条件'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FK_TEXT1'          '付款条件描述'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'XBLNR'          '交货单编号'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'DMBTR'          '交货金额'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MWSKZ'          '税码'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'HSJE'          '含税金额'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'ERFMG'          '交货数量'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'ERFME'          '单位'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'BUDAT_MKPF'          '交货过账日期'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'ZTAG1'          '账期天数'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'DQR'          '到期日'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'QQTS'          '逾期天数'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'PS_PSP_PNR'          '项目编号'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'POST1'          '项目名称'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'MBLNR'          '物料凭证号'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'MATNR'          '物料号'             '' '' '' '' '' '' ''.
   INIT_FIELDCAT 'MAKTX'          '物料描述'             '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0306   text
*      -->P_0307   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
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
*      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
      T_OUTTAB                 = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.

FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*  CASE R_UCOMM.
** 双击
**    WHEN '&IC1'.
**      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
**      CHECK SY-SUBRC = 0.
**      IF RS_SELFIELD-FIELDNAME = 'BELNR'
**        AND GS_DATA-BELNR IS NOT INITIAL.
**        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
**        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
**        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
**        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
**      ENDIF.
**      IF RS_SELFIELD-FIELDNAME = 'VBELN'
**        AND GS_DATA-VBELN IS NOT INITIAL.
**        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
**        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
**      ENDIF.
*
*  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  JSDQR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA_BUDAT_MKPF  text
*      -->P_GT_T052_ZMONA  text
*      -->P_T052_ZFAEL  text
*      <--P_GT_DATA_DQR  text
*----------------------------------------------------------------------*
FORM JSDQR  USING    P_GZRQ TYPE D  "TYPE D  GT_T052-ZMONA T052-ZFAEL GT_DATA-ZTAG1
                      P_M  LIKE T052-ZMONA
                     P_D  LIKE T052-ZFAEL
                    ZTAG1  LIKE T052-ZTAG1
            CHANGING P_DQR  TYPE D.
  DATA:LAST_DATE  TYPE D ,"当月最后一天
        LAST_DAY   TYPE N LENGTH 2,"最后一天
        P_YEAR  TYPE  N LENGTH 4,
        P_MONTH  TYPE N LENGTH 2,
        P_DAY   TYPE N LENGTH 2.
        P_YEAR  = P_GZRQ+0(4)."年
        P_MONTH  = P_GZRQ+4(2)."月
        P_DAY    = P_GZRQ+6(2)."日
       CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
           DATE            = P_GZRQ
           DAYS            = '00'
           MONTHS          = P_M
           SIGNUM          = '+'
           YEARS           = '00'
       IMPORTING
         CALC_DATE       = P_GZRQ  .

     P_YEAR  = P_GZRQ+0(4)."年
     P_MONTH  = P_GZRQ+4(2)."月            .
    IF  P_D NE '00'.
        P_DAY = P_D .
    ENDIF.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN                  =  P_GZRQ
      IMPORTING
       LAST_DAY_OF_MONTH       = LAST_DATE
     EXCEPTIONS
      DAY_IN_NO_DATE          = 1
      OTHERS                  = 2
              .
    IF SY-SUBRC <> 0.

* Implement suitable error handling here
    ENDIF.
    IF P_DAY < LAST_DATE+6(2)  .
         " P_DAY = LAST_DATE+6(2).
          LAST_DATE+6(2) =  P_DAY .

       ENDIF.

 P_DQR  = LAST_DATE  +  ZTAG1 ."基准日

ENDFORM.
