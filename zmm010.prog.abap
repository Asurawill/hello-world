*&---------------------------------------------------------------------*
*& Report  ZMM010
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/01/20
*& Request       :
*& Descriptions  : 个人借物汇总查询报表
*&
*& Modify by     : IT02
*& Modify date   : 20160120
*& Request       :
*& Descriptions  :列表增加抵消凭证、号、抵消数量、剩余数量、是否完全归还
                  "根据Z07和Z08对应参照关系及Z08行文本输入的Z07的抵销凭证
*&
*&---------------------------------------------------------------------*
REPORT ZMM010.

************************************************************************
* Tables
************************************************************************
TABLES:MSEG,MKPF.

************************************************************************
* Type Declaration
************************************************************************
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
        CHARG   TYPE MSEG-CHARG, "批次
        WEMPF   TYPE MSEG-WEMPF, "实际借物人
        SGTXT   TYPE MSEG-SGTXT, "借物原因& 抵消凭证号
        YJGHRQ  TYPE MSEG-SGTXT, "预计归还日期
        VERPR   TYPE EBEW-VERPR, "期间单价
        PEINH   TYPE EBEW-PEINH, "价格单位
        SJAHR   TYPE MSEG-SJAHR, "物料凭证年度
        SMBLN   TYPE MSEG-SMBLN, "物料凭证号
        SMBLP   TYPE MSEG-SMBLP, "物料凭证行号
        ABLAD   TYPE MSEG-ABLAD, "项目订单号
        XBLNR   TYPE MKPF-XBLNR , "预计归还日期
        XMMC    TYPE STRING ,     "项目名称
        DXPZ    TYPE STRING, "抵消凭证编号
        DXPZHH  TYPE STRING, "抵消凭证编号 行项目
        DXSLHJ  TYPE MSEG-MENGE, "抵消凭证数量
        SYSL    TYPE MSEG-MENGE, "剩余数量
        IS_WQGH TYPE CHAR01 , "是否完全归还
        SHKZG   TYPE MSEG-SHKZG, "借贷
        GHFLAG  TYPE ZMM010M-GHFLAG, "归还标识
      END OF TY_DATA.

TYPES:BEGIN OF TY_DXPZ ,
    DXPZ    TYPE STRING, "抵消凭证编号
    DXPZHH  TYPE STRING, "抵消凭证编号 行项目
    MENGE   TYPE MSEG-MENGE ,"数量
    END OF TY_DXPZ .
DATA:GT_DXPZ TYPE TABLE OF TY_DXPZ .
DATA:GS_DXPZ TYPE TY_DXPZ .

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.
DATA GS_DATA02 TYPE TY_DATA.
DATA:GT_APPEND TYPE TABLE OF TY_DATA.
DATA:GS_APPEND TYPE  TY_DATA.

DATA:GT_Z07 TYPE TABLE OF TY_DATA,
     GS_Z07 TYPE TY_DATA.

DATA:GT_Z08 TYPE TABLE OF TY_DATA,
     GS_Z08 TYPE TY_DATA.

DATA:GT_DATA_SMBLN TYPE TABLE OF TY_DATA.
DATA:GS_DATA_SMBLN TYPE TY_DATA.
DATA:GT_DATA_542 TYPE TABLE OF TY_DATA.
DATA GT_DATA02 TYPE TABLE OF TY_DATA.

DATA GT_MAKT TYPE TABLE OF MAKT.
DATA GS_MAKT TYPE MAKT.

DATA GT_T001W TYPE TABLE OF T001W WITH HEADER LINE.

DATA GT_MBEW TYPE TABLE OF MBEW.
DATA GS_MBEW TYPE MBEW.

DATA GT_EBEW TYPE TABLE OF EBEW.
DATA GS_EBEW TYPE EBEW.

DATA GT_LFA1 TYPE TABLE OF LFA1.
DATA GS_LFA1 TYPE LFA1.

DATA:GT_VBAK TYPE TABLE OF VBAK,
     GS_VBAK  TYPE VBAK.

DATA:GT_ZMM010A TYPE TABLE OF ZMM010A,
     GS_ZMM010A TYPE ZMM010A.

DATA:GT_ZMM010M TYPE TABLE OF  ZMM010M,
     GS_ZMM010M TYPE ZMM010M.


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
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.
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

DATA: GS_MKPF TYPE MKPF .


************************************************************************
* Global Variant
************************************************************************


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

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*查询工厂
  SELECT * FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE GT_T001W.

  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

*&---------------------------------------------------------------------*
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

*取出移动类型是Z07,Z08的物料凭证   (增加561，562移动类型 for 期初的员工借物信息查看，限制供应商编码)
"先取出移动类型Z07、Z08、561、562、541、542凭证且未参照任何凭证的行项目数据  MODIFIED BY IT02 160120
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
  AND ( BWART = 'Z07'
  OR    BWART = 'Z08'
  OR    BWART = '561'
  OR    BWART = '541'
  OR    BWART = '542'
  OR    BWART = '562')
  AND   LIFNR BETWEEN '0000300000' AND '0000399999'
  AND   SMBLN EQ  ''
    .
"再追加 Z08、542参照其他凭证为依据生成的过账凭证数据   ADDED  BY IT02 20160120
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
  AND (   BWART = 'Z08'
    OR    BWART = '542'
    )
  AND   LIFNR BETWEEN '0000300000' AND '0000399999'
  AND   SMBLN NE  ''
    .
  CHECK GT_DATA IS NOT INITIAL.

  SELECT * FROM LFA1 INTO
   CORRESPONDING FIELDS OF TABLE GT_LFA1
   FOR ALL ENTRIES IN GT_DATA
   WHERE LIFNR = GT_DATA-LIFNR.

*查询物料(按库)的期间单价
  SELECT * FROM MBEW
    INTO CORRESPONDING FIELDS OF TABLE GT_MBEW
    FOR ALL ENTRIES IN GT_DATA
    WHERE MATNR = GT_DATA-MATNR
    AND   BWKEY = GT_DATA-WERKS.

*查询物料按单的库存的期间单价
  SELECT * FROM EBEW
    INTO CORRESPONDING FIELDS OF TABLE GT_EBEW
    FOR ALL ENTRIES IN GT_DATA
    WHERE MATNR = GT_DATA-MATNR
    AND   BWKEY = GT_DATA-WERKS
    AND   SOBKZ = 'E'
    AND   VBELN = GT_DATA-KDAUF
    AND   POSNR = GT_DATA-KDPOS.


*删除库存地点为空的，只显示发出的行项目(期初导入的561不删除)
*  DELETE GT_DATA WHERE LGORT IS INITIAL AND ( BWART = 'Z07' OR BWART = 'Z08' ).


  " GT_DATA_SMBLN 只取保留 Z07 、561的数据 IT02 151019
 " GT_DATA_SMBLN[] = GT_DATA[].
 " DELETE GT_DATA_SMBLN WHERE (  BWART NE 'Z07' AND BWART NE '561' ).

  DELETE GT_DATA WHERE SHKZG = 'S' AND ( BWART = 'Z07' OR BWART = 'Z08'
                                   OR    BWART = '541' OR BWART = '542'
                                 ).
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
*  GT_Z08 只保留 Z08 类型的数据
  MOVE-CORRESPONDING GT_DATA TO GT_Z08.
  DELETE GT_Z08 WHERE BWART NE 'Z08'.
  SORT GT_Z08 BY MJAHR MBLNR ZEILE.
 "查询自建表维护抵消凭证关系
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZMM010A
    FROM ZMM010A
    FOR ALL ENTRIES IN GT_Z08
    WHERE MJAHR = GT_Z08-MJAHR
     AND  MBLNR = GT_Z08-MBLNR
     AND  ZEILE = GT_Z08-ZEILE.
   SORT GT_ZMM010A BY MJAHR MBLNR ZEILE.

   "查询自建表的ZMM010M：个人借物归还标记维护
  SELECT * INTO  CORRESPONDING FIELDS OF TABLE GT_ZMM010M
    FROM  ZMM010M
    .
  SORT  GT_ZMM010M BY MJAHR MBLNR ZEILE .
  ENDIF.





  GT_DATA02[] = GT_DATA[].
  SELECT * FROM MAKT
  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR
    AND SPRAS = SY-LANGU.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
   FROM  VBAK .
 SORT GT_VBAK BY VBELN.

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
  "抵消凭证数量汇总
  DELETE GT_DATA02 WHERE BWART NE 'Z08'.
  DATA:WLPZHH TYPE STRING .
  DATA:JSLEN TYPE i.
  LOOP AT  GT_DATA02 INTO GS_DATA02 .
    READ TABLE GT_DATA_SMBLN INTO GS_DATA_SMBLN WITH KEY SJAHR = GS_DATA02-MJAHR   SMBLN = GS_DATA02-MBLNR  SMBLP = GS_DATA02-ZEILE BINARY SEARCH .
    IF SY-SUBRC = 0.
       CONTINUE.          "排除Z08 后又作为参考凭证冲销 的凭证数量
    ENDIF.

    CLEAR :WLPZHH,GS_DXPZ.
    SHIFT GS_DATA02-SGTXT LEFT DELETING LEADING ''.
    WLPZHH = GS_DATA02-SGTXT+0(15).
    SPLIT WLPZHH AT '-' INTO GS_DXPZ-DXPZ GS_DXPZ-DXPZHH.
    READ TABLE GT_DATA INTO GS_DATA WITH KEY MBLNR = GS_DXPZ-DXPZ ." ZEILE =  GS_DXPZ-DXPZHH.
    IF SY-SUBRC = 0.
    CASE STRLEN( GS_DXPZ-DXPZHH )  .
      WHEN '0'.
        CONCATENATE '0000' GS_DXPZ-DXPZHH INTO GS_DXPZ-DXPZHH.
      WHEN '1'.
        CONCATENATE '000' GS_DXPZ-DXPZHH INTO GS_DXPZ-DXPZHH.
      WHEN '2'.
        CONCATENATE '00' GS_DXPZ-DXPZHH INTO GS_DXPZ-DXPZHH.
      WHEN '3'.
        CONCATENATE '0' GS_DXPZ-DXPZHH INTO GS_DXPZ-DXPZHH.
    ENDCASE.
    GS_DXPZ-MENGE = GS_DATA02-MENGE.
    COLLECT GS_DXPZ INTO GT_DXPZ.   "汇总抵消凭证总计
    ENDIF.
  ENDLOOP.

  SORT GT_DXPZ BY DXPZ DXPZHH.
  DATA :S_TABIX TYPE i.
  LOOP AT GT_DATA INTO GS_DATA.
       S_TABIX = SY-TABIX.
     IF GS_DATA-BWART = 'Z08'.
     READ TABLE GT_DATA_SMBLN INTO GS_DATA_SMBLN WITH KEY SJAHR = GS_DATA-MJAHR   SMBLN = GS_DATA-MBLNR  SMBLP = GS_DATA-ZEILE BINARY SEARCH .
      IF SY-SUBRC = 0.
          DELETE GT_DATA INDEX S_TABIX .          "排除Z08 后又作为参考凭证冲销 的凭证
          CONTINUE.
      ENDIF.
    ENDIF.
    "IF GS_DATA
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




    " BEGIN  ADD BY IT02 151016   ADD 抵消凭证 、抵消凭证数量 、是否完全归还
    DATA:DXSLHJ TYPE MSEG-MENGE.
      CLEAR :DXSLHJ.
    IF GS_DATA-BWART = 'Z07' OR GS_DATA-BWART = '561'.
        GS_DATA-YJGHRQ = GS_DATA-XBLNR ."预计归还日期
      READ TABLE  GT_DXPZ INTO GS_DXPZ  WITH KEY DXPZ = GS_DATA-MBLNR  DXPZHH = GS_DATA-ZEILE  .
       IF SY-SUBRC = 0.
          DXSLHJ =  DXSLHJ + GS_DXPZ-MENGE.  "汇总的是Z08归还的数量 到 抵消凭证上
       ENDIF.
      LOOP AT GT_DATA_SMBLN INTO GS_DATA_SMBLN WHERE SJAHR = GS_DATA-MJAHR AND  SMBLN = GS_DATA-MBLNR AND SMBLP = GS_DATA-ZEILE.
        DXSLHJ =  DXSLHJ + GS_DATA_SMBLN-MENGE.  "汇总的是 参照Z07、561冲销的凭证
      ENDLOOP.
      LOOP AT  GT_ZMM010A INTO GS_ZMM010A WHERE DXPZ = GS_DATA-MBLNR AND DXPZHH = GS_DATA-ZEILE .

           READ TABLE GT_Z08 INTO GS_Z08 WITH KEY MJAHR = GS_ZMM010A-MJAHR MBLNR = GS_ZMM010A-MBLNR ZEILE = GS_ZMM010A-ZEILE BINARY SEARCH .
             IF SY-SUBRC = 0 .
                DXSLHJ =  DXSLHJ +  GS_Z08-MENGE.  "汇总参照自建表维护抵消关系的数量
            ENDIF.

        ENDLOOP.
      GS_DATA-DXSLHJ = DXSLHJ.
      GS_DATA-SYSL = GS_DATA-MENGE - GS_DATA-DXSLHJ ."剩余数量
      IF GS_DATA-MENGE = GS_DATA-DXSLHJ .
        GS_DATA-IS_WQGH = '是'.
      ELSE.
        GS_DATA-IS_WQGH = '否'.
      ENDIF.
      "补充后续人为选择的归还标识
      READ TABLE  GT_ZMM010M INTO  GS_ZMM010M WITH KEY MJAHR = GS_DATA-MJAHR
                 MBLNR = GS_DATA-MBLNR ZEILE = GS_DATA-ZEILE BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
          GS_DATA-GHFLAG = GS_ZMM010M-GHFLAG .

      ENDIF.
  "  ENDIF.
     CLEAR:DXSLHJ.
   ELSEIF GS_DATA-BWART = '542' OR GS_DATA-BWART = '562' . "读取 542、562 冲销时对应的参考凭证号作为抵消凭证
      READ TABLE GT_DATA_SMBLN INTO GS_DATA_SMBLN WITH KEY MJAHR = GS_DATA-MJAHR MBLNR = GS_DATA-MBLNR ZEILE = GS_DATA-ZEILE.
      IF SY-SUBRC = 0.
        GS_DATA-DXPZ = GS_DATA_SMBLN-SMBLN.
        GS_DATA-DXPZHH = GS_DATA_SMBLN-SMBLP.
      ENDIF.

      "   READ TABLE GT_APPEND  WITH KEY
 "   ENDIF.

   ELSEIF GS_DATA-BWART = 'Z08'. "Z08 列表需填充 过账填充的抵消凭证号及行号 \Z08 冲销时对应的参考凭证号作为抵消凭证
       IF GS_DATA-SMBLN NE ''.  "先判断本行Z08取取的是否为参照Z07做的完全取消凭证
            GS_DATA-DXPZ = GS_DATA-SMBLN.   "抵消凭证号为参照凭证
            GS_DATA-DXPZHH = GS_DATA-SMBLP.
       ELSE.
             CLEAR :WLPZHH,GS_DXPZ.
             SHIFT GS_DATA-SGTXT LEFT DELETING LEADING ''.
             WLPZHH = GS_DATA-SGTXT+0(15).
             SPLIT WLPZHH AT '-' INTO GS_DATA-DXPZ GS_DATA-DXPZHH.
             CLEAR :GS_MKPF.
             SELECT SINGLE * INTO GS_MKPF FROM MKPF WHERE MBLNR = GS_DATA-DXPZ .
               IF SY-SUBRC NE 0 .
                  CLEAR : GS_DATA-DXPZ .
                 ENDIF.
       ENDIF.
       READ TABLE GT_ZMM010A INTO GS_ZMM010A WITH KEY MJAHR = GS_DATA-MJAHR MBLNR = GS_DATA-MBLNR ZEILE = GS_DATA-ZEILE BINARY SEARCH .
         IF SY-SUBRC = 0.
           GS_DATA-DXPZ = GS_ZMM010A-DXPZ.     "最后查询维护自建表ZMM010A是否有抵销凭证关系
           GS_DATA-DXPZHH = GS_ZMM010A-DXPZHH.
          ENDIF.

    ENDIF.


    " END ADD BY IT02 151016   ADD 抵消凭证 、抵消凭证数量 、是否完全归还

*借物退库的为负数
    IF GS_DATA-BWART  = 'Z08' OR GS_DATA-BWART = '562' OR GS_DATA-BWART = '542'.
      GS_DATA-MENGE = GS_DATA-MENGE * ( -1 ).
    ENDIF.

   " GS_DATA-YJGHRQ = GS_DATA-SGTXT. "预计归还日期



*周期单价(有销售订单的按单，无销售订单的按库)
*    IF GS_DATA-KDAUF IS NOT INITIAL
*    AND GS_DATA-KDPOS IS NOT INITIAL .
*      READ TABLE GT_EBEW INTO GS_EBEW
*      WITH KEY MATNR = GS_DATA-MATNR
*               BWKEY = GS_DATA-WERKS
*               SOBKZ = 'E'
*               VBELN = GS_DATA-KDAUF
*               POSNR = GS_DATA-KDPOS.
*      IF SY-SUBRC = 0.
**有V价取最新期间的V价
*        IF GS_EBEW-VERPR = 0.
*          GS_DATA-VERPR = GS_EBEW-STPRS.
*        ELSE.
*          GS_DATA-VERPR = GS_EBEW-VERPR.
*        ENDIF.
*      ENDIF.
*
**普通库存的调拨
*    ELSE.
    READ TABLE GT_MBEW INTO GS_MBEW
        WITH KEY MATNR = GS_DATA-MATNR
                 BWKEY = GS_DATA-WERKS.
    IF SY-SUBRC = 0.
*有V价取最新期间的V价
      IF GS_EBEW-VERPR = 0.
        GS_DATA-VERPR = GS_MBEW-STPRS.
        GS_DATA-PEINH = GS_MBEW-PEINH.
      ELSE.
        GS_DATA-VERPR = GS_MBEW-VERPR.
        GS_DATA-PEINH = GS_MBEW-PEINH.
      ENDIF.
    ENDIF.


*    ENDIF.

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
  GW_LAYOUT-ZEBRA = 'X'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
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
  INIT_FIELDCAT 'BUDAT'          '借物日期'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'          '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'          '移动类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MJAHR'          '物料凭证年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MBLNR'          '物料凭证编码'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'ZEILE'          '行号'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'LGORT'          '领料出库'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'          '借物联系人'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '借物联系人描述'               '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'KDAUF'          '项目名称（销售订单）'           '' '' '' '' '' '' ''.
 " INIT_FIELDCAT 'KDPOS'          '销售订单行项目'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ABLAD'          '销售订单号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XMMC'          '项目名称'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料编号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '描述'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '单位'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '数量'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CHARG'          '批次'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WEMPF'          '实际借物人'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SGTXT'          '借物原因'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YJGHRQ'         '预计归还日期'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DXPZ'           '抵消凭证'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DXPZHH'         '抵消凭证行号'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DXSLHJ'         '抵消凭证数量'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SYSL'            '剩余数量'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'IS_WQGH'         '是否完全归还'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GHFLAG'         '人为选择是否归还'                 '' '' '' '' '' '' ''.


*价格权限控制
  AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
  IF SY-SUBRC = 0.
    INIT_FIELDCAT 'VERPR'          '期间单价'                 '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'PEINH'          '价格单位'                 '' '' '' '' '' '' ''.
  ENDIF.
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
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

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
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
