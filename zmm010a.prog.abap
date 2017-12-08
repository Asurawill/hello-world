REPORT ZMM010A.
"add by it02 20160121
TABLES:MSEG,MKPF.
TYPES:BEGIN OF TY_DATA,
        BUDAT   TYPE MKPF-BUDAT, "借物日期
        WERKS   TYPE MSEG-WERKS, "工厂
        BWART   TYPE MSEG-BWART, "移动类型
        MJAHR   TYPE MSEG-MJAHR, "物料凭证年度
        MBLNR   TYPE MSEG-MBLNR, "物料凭证编号
        ZEILE   TYPE MSEG-ZEILE, "物料凭证行号
        LGORT   TYPE MSEG-LGORT, "领料出库
        LIFNR   TYPE MSEG-LIFNR, "借物联系人
        NAME1   TYPE LFA1-NAME1, "供应商名称
        KDAUF   TYPE MSEG-KDAUF, "销售订单号
        KDPOS   TYPE MSEG-KDPOS, "销售订单行项目
        MATNR   TYPE MSEG-MATNR, "物料号
        MAKTX   TYPE MAKT-MAKTX, "物料描述
        MEINS   TYPE MSEG-MEINS, "单位
        MENGE   TYPE MSEG-MENGE, "数量
        WEMPF   TYPE MSEG-WEMPF, "实际借物人
        SGTXT   TYPE MSEG-SGTXT, "借物原因& 抵消凭证号
        YJGHRQ  TYPE MSEG-SGTXT, "预计归还日期
        SJAHR   TYPE MSEG-SJAHR, "物料凭证年度
        SMBLN   TYPE MSEG-SMBLN, "物料凭证号
        SMBLP   TYPE MSEG-SMBLP, "物料凭证行号
        ABLAD   TYPE MSEG-ABLAD, "项目订单号
        XBLNR   TYPE MKPF-XBLNR , "预计归还日期
        SHKZG   TYPE MSEG-SHKZG, "借贷
        XMMC    TYPE STRING ,     "项目名称
        DXPZ    TYPE STRING, "抵消凭证编号
        DXPZHH  TYPE STRING, "抵消凭证编号 行项目
        DXSLHJ  TYPE MSEG-MENGE, "抵消凭证数量
        ZSEL(1) TYPE C ,


      END OF TY_DATA.

TYPES:BEGIN OF TY_UPLOAD,
     MJAHR(4) TYPE C,
     MBLNR(10) TYPE C,
     ZEILE(4) TYPE C,
     DXPZ(10) TYPE C,
     DXPZHH(4) TYPE C,
     ZSEL(1) TYPE C,
 END OF TY_UPLOAD.
" ************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_DATA_02 TYPE TABLE OF TY_DATA.
DATA GS_DATA_02 TYPE TY_DATA.

DATA:GT_DATA_SMBLN TYPE TABLE OF TY_DATA.
DATA:GS_DATA_SMBLN TYPE TY_DATA.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.

DATA:GT_VBAK TYPE TABLE OF VBAK,
     GS_VBAK  TYPE VBAK.

*DATA GT_MAKT TYPE TABLE OF MAKT.
*DATA GS_MAKT TYPE MAKT.

DATA GT_T001W TYPE TABLE OF T001W WITH HEADER LINE.

DATA GT_MBEW TYPE TABLE OF MBEW.
DATA GS_MBEW TYPE MBEW.

DATA GT_EBEW TYPE TABLE OF EBEW.
*DATA GS_EBEW TYPE EBEW.
*
*DATA GT_LFA1 TYPE TABLE OF LFA1.
*DATA GS_LFA1 TYPE LFA1.
*
*DATA:GT_VBAK TYPE TABLE OF VBAK,
*     GS_VBAK  TYPE VBAK.
DATA: GS_MKPF TYPE MKPF .

DATA:GT_ZMM010A TYPE TABLE OF ZMM010A ,
     GS_ZMM010A TYPE ZMM010A .

DATA:GT_ZMM010A_SAVE TYPE TABLE OF ZMM010A,
     GS_ZMM010A_SAVE TYPE ZMM010A.

DATA:GT_UPLOAD TYPE TABLE OF TY_UPLOAD,
     GS_UPLOAD TYPE TY_UPLOAD.
DATA T_RAW_DATA TYPE TRUXS_T_TEXT_DATA.

************************************************************************
*      DEFINITION
************************************************************************

DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_table = &8.
  gw_lvc-ref_field = &9.
  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "alv的格式
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
 DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_VBELN FOR MSEG-KDAUF,
                S_MATNR FOR MSEG-MATNR,
                S_WERKS FOR MSEG-WERKS,
                S_LIFNR FOR MSEG-LIFNR,
                S_BUDAT FOR MKPF-BUDAT.
SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_D AS CHECKBOX  USER-COMMAND UCOMM.
PARAMETERS P_FILE LIKE RLGRAP-FILENAME MODIF ID MP1 .
SELECTION-SCREEN END OF BLOCK BLK2.
SELECTION-SCREEN PUSHBUTTON 2(13) but1 USER-COMMAND cmd2 MODIF ID 11.    "模板下载
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
  CALL FUNCTION 'ICON_CREATE'                                            "调用模板下载按钮的图片
    EXPORTING
      name   = icon_export
      text   = '模板下载'
    IMPORTING
      result = but1
    EXCEPTIONS
      OTHERS = 0.
*&---------------------------------------------------------------------*
*& EVENT AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
 DATA:p_objid(20)       TYPE c.

  CASE sy-ucomm.
    WHEN 'CMD2'.                                                         "模板下载
      PERFORM download_tpt USING text-001
                                 p_objid
                           CHANGING P_FILE.
    WHEN OTHERS.
  ENDCASE.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_FILEPATH.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT  SCREEN .
     IF SCREEN-GROUP1 = 'MP1'.
        IF P_D EQ 'X'.
          SCREEN-ACTIVE = 1 .
          SCREEN-INPUT = 1.
         ELSE.
          SCREEN-ACTIVE = 0 .
          SCREEN-INPUT = 0.
         ENDIF.
     ENDIF.
     MODIFY SCREEN.
   ENDLOOP.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
"AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*查询工厂
  SELECT * FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE GT_T001W.

  PERFORM FRM_AUTH_CHECK.
  IF P_D = 'X'.
   PERFORM FRM_INPUT.

  ELSE.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  ENDIF.
  PERFORM FRM_ALV_SHOW. "ALV显示



 " *&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  LOOP AT GT_T001W WHERE WERKS IN S_WERKS.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
             ID 'WERKS' FIELD GT_T001W-WERKS
             .
    IF SY-SUBRC <> 0.
      MESSAGE E603(FCO) WITH GT_T001W-WERKS.
    ENDIF.
  ENDLOOP.

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
*取出移动类型是Z08的物料凭证   (增加561，562移动类型 for 期初的员工借物信息查看，限制供应商编码)
"先取出移动类型Z08凭证且未参照任何凭证的行项目数据  MODIFIED BY IT02 160120
  SELECT * FROM MKPF
  INNER JOIN MSEG ON
      MKPF~MBLNR = MSEG~MBLNR
  AND MKPF~MJAHR = MSEG~MJAHR
  INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  WHERE                         "KDAUF IN S_VBELN  AND
  MATNR IN S_MATNR
  AND   WERKS IN S_WERKS
  AND   LIFNR IN S_LIFNR
  AND   BUDAT IN S_BUDAT
  AND   BWART = 'Z08'
  AND   LIFNR BETWEEN '0000300000' AND '0000399999'
  AND   SMBLN EQ  '' .
 "再追加 Z08参照其他凭证为依据生成的过账凭证数据   ADDED  BY IT02 20160120
  SELECT * FROM MKPF
  INNER JOIN MSEG ON
      MKPF~MBLNR = MSEG~MBLNR
  AND MKPF~MJAHR = MSEG~MJAHR
  APPENDING CORRESPONDING FIELDS OF TABLE GT_DATA
  WHERE                         "KDAUF IN S_VBELN  AND
  MATNR IN S_MATNR
  AND   WERKS IN S_WERKS
  AND   LIFNR IN S_LIFNR
  AND   BUDAT IN S_BUDAT
  AND   BWART = 'Z08'
  AND   LIFNR BETWEEN '0000300000' AND '0000399999'
  AND   SMBLN NE  ''
    .
  CHECK GT_DATA IS NOT INITIAL.


  SELECT * FROM LFA1 INTO
   CORRESPONDING FIELDS OF TABLE GT_LFA1
   FOR ALL ENTRIES IN GT_DATA
   WHERE LIFNR = GT_DATA-LIFNR.

  DELETE GT_DATA WHERE SHKZG = 'S' AND  BWART = 'Z08' .

  SORT GT_DATA BY MJAHR MBLNR ZEILE .
  DELETE  ADJACENT DUPLICATES FROM GT_DATA COMPARING MJAHR MBLNR ZEILE .

  IF GT_DATA[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_DATA_SMBLN
      FROM  MKPF
     INNER JOIN MSEG ON
      MKPF~MBLNR = MSEG~MBLNR
     AND MKPF~MJAHR = MSEG~MJAHR
      FOR ALL ENTRIES IN GT_DATA
      WHERE  SJAHR = GT_DATA-MJAHR
         AND SMBLN = GT_DATA-MBLNR   "追加GT_DATA 参照 Z07、561的 类型为542 、562冲销的凭证数据
         AND SMBLP = GT_DATA-ZEILE
      ."   AND ( BWART = 'Z07' OR BWART = 'Z08' OR  BWART = '542' OR  BWART = '562') .

 "   APPEND LINES OF GT_APPEND  TO GT_DATA.
 SORT GT_DATA_SMBLN BY SJAHR SMBLN SMBLP.

  ENDIF.

  SELECT * FROM MAKT
  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR
    AND SPRAS = SY-LANGU.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
   FROM  VBAK .
 SORT GT_VBAK BY VBELN.

 SELECT * INTO CORRESPONDING FIELDS OF TABLE  GT_ZMM010A
   FROM ZMM010A .
 SORT GT_ZMM010A BY MJAHR MBLNR ZEILE .

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
  DATA :S_TABIX TYPE i.
  DATA:WLPZHH TYPE STRING .
  DATA:JSLEN TYPE i.
 LOOP AT  GT_DATA INTO GS_DATA.
    S_TABIX = SY-TABIX.
   IF GS_DATA-BWART = 'Z08'.
     READ TABLE GT_DATA_SMBLN INTO GS_DATA_SMBLN WITH KEY SJAHR = GS_DATA-MJAHR   SMBLN = GS_DATA-MBLNR  SMBLP = GS_DATA-ZEILE BINARY SEARCH .
      IF SY-SUBRC = 0.
          DELETE GT_DATA INDEX S_TABIX .          "排除Z08 后又作为参考凭证冲销 的凭证
          CONTINUE.
      ENDIF.
    ENDIF.
       "项目名称
   CONDENSE GS_DATA-ABLAD NO-GAPS.  "改为ABLAD 为销售订单号
   TRANSLATE GS_DATA-ABLAD TO UPPER CASE.
   IF GS_DATA-ABLAD NOT IN S_VBELN .
     CONTINUE.
   ENDIF.
   G_OBJNAME = GS_DATA-ABLAD .
   READ TABLE GT_VBAK INTO GS_VBAK WITH KEY VBELN = G_OBJNAME BINARY SEARCH .
   IF SY-SUBRC = 0.
      CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'Z001'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        LINES                   = IT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC = 0.
      READ TABLE IT_LINES INTO WA_LINES INDEX 1.
      IF SY-SUBRC = 0.
     GS_DATA-XMMC = WA_LINES-TDLINE.
      ENDIF.
    ENDIF.
   ENDIF.

*读取物料描述
    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

*读取供应商描述
    READ TABLE GT_LFA1 INTO GS_LFA1
    WITH KEY LIFNR = GS_DATA-LIFNR
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-NAME1 = GS_LFA1-NAME1.
    ENDIF.
*   增加抵销凭证
    IF GS_DATA-SMBLN NE ''.  "先判断本行Z08取取的是否为参照Z07做的完全取消凭证
            GS_DATA-DXPZ = GS_DATA-SMBLN.   "抵消凭证号为参照凭证
            GS_DATA-DXPZHH = GS_DATA-SMBLP.
       ELSE.
             CLEAR :WLPZHH.
             SHIFT GS_DATA-SGTXT LEFT DELETING LEADING ''.
             WLPZHH = GS_DATA-SGTXT+0(15).
             SPLIT WLPZHH AT '-' INTO GS_DATA-DXPZ GS_DATA-DXPZHH.
             CLEAR :GS_MKPF.
             SELECT SINGLE * INTO GS_MKPF FROM MKPF WHERE MBLNR = GS_DATA-DXPZ .
               IF SY-SUBRC NE 0 .
                  CLEAR : GS_DATA-DXPZ .
                 ENDIF.
       ENDIF.
    READ TABLE GT_ZMM010A INTO GS_ZMM010A WITH KEY MJAHR = GS_DATA-MJAHR MBLNR = GS_DATA-MBLNR ZEILE = GS_DATA-ZEILE BINARY SEARCH.
     IF SY-SUBRC = 0.
       GS_DATA-DXPZ = GS_ZMM010A-DXPZ.
       GS_DATA-DXPZHH = GS_ZMM010A-DXPZHH.
     ENDIF.
    GS_DATA-MENGE = GS_DATA-MENGE * ( -1 ).
    MODIFY GT_DATA FROM GS_DATA.
    CLEAR GS_DATA.
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
  PERFORM FRM_BUILD_EVENT.
  IF P_D EQ 'X'.
    PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_ZMM010A
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
  ELSE.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
  ENDIF.

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
  GW_LAYOUT-ZEBRA = 'X'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
  IF P_D NE 'X'.
   GW_LAYOUT-BOX_FNAME = 'ZSEL'.
  ENDIF.
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
  IF P_D NE 'X'.
  INIT_FIELDCAT 'BUDAT'          '过账日期'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'          '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'          '移动类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MJAHR'          '物料凭证年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MBLNR'          '物料凭证编码'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'ZEILE'          '行号'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'LGORT'          '领料出库'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'          '借物联系人'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '借物联系人描述'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ABLAD'          '销售订单号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'          '项目名称'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料编号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '描述'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '单位'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WEMPF'          '实际借物人'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '借物原因'                 '' '' '' '' '' '' ''.
 " INIT_FIELDCAT 'YJGHRQ'         '预计归还日期'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DXPZ'           '抵消凭证'                 '' '' '' 'X' '' 'MSEG' 'MBLNR'.
  INIT_FIELDCAT 'DXPZHH'         '抵消凭证行号'                 '' '' '' 'X' '' 'MSEG' 'ZEILE'.
   ELSE.
  INIT_FIELDCAT 'MJAHR'          '物料凭证年度'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'MBLNR'          '物料凭证编码'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'ZEILE'          '行号'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'DXPZ'           '抵消凭证'                 '' '' '' 'X' '' 'MSEG' 'MBLNR'.
  INIT_FIELDCAT 'DXPZHH'         '抵消凭证行号'                 '' '' '' 'X' '' 'MSEG' 'ZEILE'.
  ENDIF.



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
 REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
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
*      -->P_0415   text
*      -->P_0416   text
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
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.

* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'MBLNR'
        AND GS_DATA-MJAHR IS NOT INITIAL
        AND GS_DATA-MBLNR IS NOT INITIAL.
        SET PARAMETER ID 'MBN' FIELD GS_DATA-MBLNR.
        SET PARAMETER ID 'MJA' FIELD GS_DATA-MJAHR.
        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&DATA_SAVE'.
     "保存列表数据到
      IF P_D NE 'X'.

         LOOP AT GT_DATA INTO GS_DATA WHERE ZSEL EQ 'X'.
             MOVE-CORRESPONDING GS_DATA TO  GS_ZMM010A_SAVE.
              GS_ZMM010A_SAVE-UNAME = SY-UNAME.  "修改名
              GS_ZMM010A_SAVE-UDATE = SY-DATUM.  "修改日期
             APPEND  GS_ZMM010A_SAVE TO  GT_ZMM010A_SAVE.
         ENDLOOP.
        SORT  GT_ZMM010A_SAVE BY MJAHR MBLNR ZEILE.

        IF GT_ZMM010A_SAVE IS INITIAL.
           MESSAGE '请选中要保存的行数' TYPE 'E'.
        ELSE.
          LOOP AT GT_ZMM010A_SAVE  INTO GS_ZMM010A_SAVE WHERE DXPZ = '' OR DXPZHH = '0000'.
            MESSAGE '选中要保存行数的抵销凭证或抵销行号不能为空，请核对检查!' TYPE 'E'.
          ENDLOOP.

          "保存数据库表数据
          MODIFY ZMM010A FROM TABLE GT_ZMM010A_SAVE.
          IF SY-SUBRC EQ 0.
             MESSAGE '选中行数已保存成功' TYPE 'S'.
            ENDIF.
        ENDIF.
       ELSE.


          LOOP AT GT_ZMM010A  INTO GS_ZMM010A WHERE MJAHR = '' OR  DXPZ = '' OR DXPZHH = '0000'.
            MESSAGE '要保存行数的物料凭证年度、抵销凭证或抵销行号不能为空，请核对检查!' TYPE 'E'.
          ENDLOOP.
           LOOP AT GT_ZMM010A INTO GS_ZMM010A.
             GS_ZMM010A-UNAME = SY-UNAME.  "修改名
             GS_ZMM010A-UDATE = SY-DATUM.  "修改日期
             MODIFY GT_ZMM010A FROM GS_ZMM010A.
           ENDLOOP.
          "保存数据库表数据
           SORT GT_ZMM010A BY MJAHR MBLNR ZEILE.
          MODIFY ZMM010A FROM TABLE GT_ZMM010A.
           IF SY-SUBRC EQ 0.
             MESSAGE '导入文件数据已保存成功' TYPE 'S'.
            ENDIF.

      ENDIF.




  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_FILEPATH .
 CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    EXPORTING
*     DEF_FILENAME     = ' '
*     DEF_PATH         = ' '
      MASK             = 'Excel Files,*.xls,All Files,*.*. '
      MODE             = 'O'
*     TITLE            = ' '
    IMPORTING
      FILENAME         = P_FILE
*     PATH             =
*     FILE             =
    EXCEPTIONS
      SELECTION_CANCEL = 1
      SELECTION_ERROR  = 2
      OTHERS           = 3.
  CASE SY-SUBRC.
    WHEN 0.
    WHEN 2.
      MESSAGE 'Cancel.' TYPE 'S'.
    WHEN OTHERS.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INPUT .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR          =
     I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             = T_RAW_DATA
      I_FILENAME                 = P_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = GT_UPLOAD
*   EXCEPTIONS
*     CONVERSION_FAILED          = 1
*     OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    MOVE-CORRESPONDING GT_UPLOAD TO GT_ZMM010A.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_001  text
*      -->P_P_OBJID  text
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM download_tpt   USING    p_text
                   VALUE(p_objid)
                   CHANGING pv_file.
                                                "下载ZMM010A批导模板
    p_objid = 'ZMM010A'.


  DATA: lv_fname TYPE string,
        lv_title TYPE string,
        lv_path  TYPE string VALUE 'D:/',
        lv_fpath TYPE string VALUE 'D:/'.

  DATA: ls_wdatb   LIKE wwwdatatab.
  DATA: lv_subrc   TYPE sy-subrc.
  DATA: gv_msg TYPE string .

  lv_fname = p_text.

  CONCATENATE p_text '下载' INTO lv_title.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = lv_title
      default_extension    = 'XLS'
      default_file_name    = lv_fname
      initial_directory    = 'D:\'
      file_filter          = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
      prompt_on_overwrite  = 'X'
    CHANGING
      filename             = lv_fname
      path                 = lv_path
      fullpath             = lv_fpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    SELECT SINGLE relid
                  objid
      FROM wwwdata
      INTO CORRESPONDING FIELDS OF ls_wdatb
      WHERE srtf2 = 0
      AND relid = 'MI'
      AND objid = p_objid.                        "p_objid就是传入模板的参数
    IF ls_wdatb IS INITIAL.
      MESSAGE '模板文件不存在！' TYPE 'E'.
    ELSE.
      pv_file = lv_fpath.
      IF pv_file IS NOT INITIAL.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            key         = ls_wdatb
            destination = pv_file
          IMPORTING
            rc          = lv_subrc.
        IF lv_subrc NE 0.
          MESSAGE '模板下载失败！' TYPE 'E'.
        ELSE.
          CLEAR gv_msg.
          CONCATENATE '模板下载到本地文件' pv_file INTO gv_msg.
          MESSAGE gv_msg TYPE 'S' .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
