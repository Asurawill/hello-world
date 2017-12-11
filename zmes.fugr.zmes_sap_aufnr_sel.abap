FUNCTION zmes_sap_aufnr_sel.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D
*"     VALUE(P_CX_L) TYPE  DATS DEFAULT '00000000'
*"     VALUE(P_CX_H) TYPE  DATS DEFAULT '99991231'
*"     VALUE(P_SPRAS) TYPE  SPRAS DEFAULT '1'
*"     VALUE(P_DYXTM) TYPE  CHAR50
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_MES_AUFNR_IN STRUCTURE  ZMES_AUFNR_IN
*"      T_MES_AUFNR_OUT STRUCTURE  ZMES_AUFNR_SEL
*"      T_MES_AUFNR_SUB STRUCTURE  ZMES_AUFNR_SEL_SUB
*"----------------------------------------------------------------------
  " *&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160729
*& Request       :
*& Descriptions  : MES查询生产订单信息
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
  "*记录日志数据
  DATA gt_zzacknow TYPE TABLE OF zzacknow.
  DATA gs_zzacknow TYPE zzacknow.

  DATA:gt_jest TYPE TABLE OF jest,
       gs_jest TYPE jest.



  TYPES:BEGIN OF ty_main,
          aufnr TYPE aufnr,       "订单号
          ktext TYPE auftext,      "订单描述
          objnr TYPE  j_objnr,       "对象号
          rsnum TYPE rsnum,       "预留号
          kdauf TYPE kdauf,       "销售订单号
          kdpos TYPE kdpos,        "销售订单行号
          xmmc  TYPE c LENGTH 150,   "项目名称
          matnr TYPE matnr,         "物料号
          maktx TYPE maktx,         "物料描述
          gamng TYPE gamng,         "订单数量
          gmein TYPE meins,         "订单单位
          erdat TYPE auferfdat,     "创建日期
          aedat TYPE aufaedat,      "更改日期
          loekz TYPE aufloekz,      "删除标记
          gstrp TYPE pm_ordgstrp,   "订单开始日期
          gltrp TYPE co_gltrp,      "订单完成日期
          werks TYPE werks_d,       "工厂
        END OF ty_main .

  TYPES:BEGIN OF  ty_sub,
          "  objid     TYPE objid,
          rsnum     TYPE rsnum,       "预留号
          rspos     TYPE rspos,       "预留行号
          aufnr     TYPE  aufnr,
          posnr     TYPE aposn,
          matnr_sub TYPE matnr,
          maktx_sub TYPE maktx,
          bdmng     TYPE bdmng,
          meins     TYPE meins,
          charg     TYPE charg_d,
          xloek     TYPE xloek,
          werks     TYPE werks_d,
        END OF ty_sub .

  TYPES:BEGIN OF ty_kdauf,
          kdauf TYPE kdauf,
          xmmc  TYPE string,

        END OF ty_kdauf .

  TYPES:BEGIN OF ty_matnr,
          matnr TYPE matnr,
          maktx TYPE maktx,
          mtart TYPE mtart,
        END OF ty_matnr.

  TYPES:BEGIN OF ty_vbap,
          vbeln  TYPE vbeln_va,
          posnr  TYPE posnr_va,
          matnr  TYPE matnr,
          arktx  TYPE arktx,
          kdmat  TYPE kdmat,
          kwmeng TYPE kwmeng,
          vrkme  TYPE vrkme,
        END OF ty_vbap .

  DATA:gt_vbap TYPE TABLE OF vbap,
       gs_vbap TYPE vbap.

  DATA:gt_kdauf TYPE TABLE OF ty_kdauf,
       gs_kdauf TYPE ty_kdauf.

  DATA:gt_matnr TYPE TABLE OF ty_matnr,
       gs_matnr TYPE ty_matnr.

  DATA:gt_main TYPE TABLE OF ty_main,
       gs_main TYPE ty_main.

  DATA:gt_main_xm TYPE TABLE OF ty_main,
       gs_main_xm TYPE ty_main.

  DATA:gt_sub TYPE TABLE OF ty_sub,
       gs_sub TYPE ty_sub.



  CLEAR:t_mes_aufnr_out,t_mes_aufnr_out[] ,
        t_mes_aufnr_sub,t_mes_aufnr_sub[],
        gt_zzacknow,gs_zzacknow,
        gt_jest,gs_jest ,
        gt_kdauf,gs_kdauf,
        gt_matnr,gs_matnr,
        gt_main,gs_main,
        gt_sub,gs_sub,gt_vbap,gs_vbap .

  REFRESH:gt_kdauf,gt_matnr,gt_main,gt_sub ,gt_vbap.



  IF t_mes_aufnr_in[] IS NOT INITIAL.
    "根据输入订单号查找一段期间的创建生产订单抬头信息

    SELECT a~aufnr a~objnr  a~werks a~erdat  a~aedat a~loekz
      a~kdauf a~kdpos
      b~gltrp b~gstrp  b~gamng b~gmein b~rsnum
      c~matnr
      INTO CORRESPONDING FIELDS OF TABLE gt_main
      FROM aufk AS a
      INNER JOIN afko AS b
      ON a~aufnr = b~aufnr
      INNER JOIN afpo AS c
      ON  a~aufnr = c~aufnr
      FOR ALL ENTRIES IN t_mes_aufnr_in
      WHERE a~aufnr = t_mes_aufnr_in-aufnr
      AND   a~werks = p_werks
      AND   a~erdat BETWEEN p_cx_l AND p_cx_h
      AND   C~MATNR NE ''.

    SORT gt_main BY aufnr .


    "根据输入订单号追加一段期间的修改生产订单抬头信息
    SELECT a~aufnr a~objnr  a~werks a~aedat a~erdat a~loekz
  a~kdauf a~kdpos
  b~gltrp b~gstrp  b~gamng b~gmein b~rsnum
  c~matnr
  APPENDING CORRESPONDING FIELDS OF TABLE gt_main
  FROM aufk AS a
  INNER JOIN afko AS b
  ON a~aufnr = b~aufnr
  INNER JOIN afpo AS c
  ON  a~aufnr = c~aufnr
  FOR ALL ENTRIES IN t_mes_aufnr_in
  WHERE a~aufnr = t_mes_aufnr_in-aufnr
  AND   a~werks = p_werks
  AND   a~aedat BETWEEN p_cx_l AND p_cx_h
  AND   C~MATNR NE ''..
    SORT gt_main BY aufnr .
    DELETE ADJACENT DUPLICATES  FROM gt_main COMPARING aufnr .


  ELSE.
    "查找一段期间的创建生产订单抬头信息

    SELECT a~aufnr a~objnr  a~werks  a~aedat a~erdat a~loekz
      a~kdauf a~kdpos
      b~gltrp b~gstrp  b~gamng b~gmein b~rsnum
      c~matnr
      INTO CORRESPONDING FIELDS OF TABLE gt_main
      FROM aufk AS a
      INNER JOIN afko AS b
      ON a~aufnr = b~aufnr
      INNER JOIN afpo AS c
      ON  a~aufnr = c~aufnr
      WHERE   a~werks = p_werks
      AND   a~loekz NE 'X'
      AND   a~erdat BETWEEN p_cx_l AND p_cx_h
      AND   C~MATNR NE ''.
    SORT gt_main BY aufnr .


    "追加查找一段期间的修改生产订单抬头信息
    SELECT a~aufnr a~objnr  a~werks  a~aedat a~erdat a~loekz
  a~kdauf a~kdpos
  b~gltrp b~gstrp  b~gamng b~gmein b~rsnum
  c~matnr
  APPENDING CORRESPONDING FIELDS OF TABLE gt_main
  FROM aufk AS a
  INNER JOIN afko AS b
  ON a~aufnr = b~aufnr
  INNER JOIN afpo AS c
  ON  a~aufnr = c~aufnr
  WHERE   a~werks = p_werks
  AND   a~aedat BETWEEN p_cx_l AND p_cx_h
  AND   C~MATNR NE ''. .
    SORT gt_main BY aufnr .
    DELETE ADJACENT DUPLICATES  FROM gt_main COMPARING aufnr .

  ENDIF.
  DATA:l_tabix TYPE sy-tabix .
  IF gt_main IS NOT INITIAL.

    "查找已关闭的对象
    SELECT * INTO TABLE gt_jest
      FROM jest
      FOR ALL ENTRIES IN gt_main
      WHERE objnr  = gt_main-objnr
      AND    stat = 'I0046'
      AND    inact = ''
      .
    SORT  gt_jest BY objnr .

    "过滤已关闭的生产订单信息
    LOOP AT gt_main  INTO  gs_main .
      l_tabix = sy-tabix .
      READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_main-objnr .
      IF sy-subrc EQ 0 .
        DELETE gt_main INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING gt_main TO gt_main_xm .
    DELETE gt_main_xm WHERE kdauf IS INITIAL .
    IF gt_main_xm IS NOT INITIAL.
      SELECT vbeln posnr  matnr
             arktx kdmat  kwmeng
             vrkme
        INTO  CORRESPONDING  FIELDS OF TABLE gt_vbap
        FROM vbap
        FOR ALL ENTRIES IN gt_main_xm
        WHERE vbeln = gt_main_xm-kdauf
        AND   posnr = gt_main_xm-kdpos.
      SORT gt_vbap  BY vbeln posnr .

    ENDIF.

    IF gt_main IS NOT INITIAL .

      MOVE-CORRESPONDING  gt_main TO gt_kdauf .

      DELETE gt_kdauf  WHERE kdauf IS INITIAL .

      SORT gt_kdauf BY kdauf .

      DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf .

      "先读取项目名称
      LOOP AT gt_kdauf INTO gs_kdauf .

        PERFORM selxmmc USING gs_kdauf-kdauf p_spras CHANGING gs_kdauf-xmmc .
        MODIFY gt_kdauf FROM gs_kdauf.

      ENDLOOP.

      "读取物料描述

      SELECT a~matnr b~maktx c~mtart
        INTO CORRESPONDING FIELDS OF TABLE gt_matnr
        FROM marc  AS a
        INNER JOIN makt AS b
        ON a~matnr = b~matnr
        INNER JOIN mara AS c
        ON a~matnr = c~matnr
        WHERE a~werks EQ p_werks
         AND b~spras = p_spras.

      SORT gt_matnr BY matnr .


      "根据生产订单号读取 对应现有效地组件信息
      SELECT rsnum rspos aufnr matnr AS matnr_sub
             bdmng meins charg xloek werks posnr
        INTO CORRESPONDING FIELDS OF TABLE gt_sub
        FROM  resb
        FOR ALL ENTRIES IN gt_main
        WHERE rsnum = gt_main-rsnum .
 "       AND   xloek NE 'X'.

      SORT gt_sub  BY rsnum  rspos.


      "主表数据赋值
      LOOP AT gt_main INTO gs_main .
        CLEAR:t_mes_aufnr_out .
        MOVE-CORRESPONDING gs_main TO t_mes_aufnr_out .
        IF gs_main-kdauf IS NOT INITIAL .
          READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf = gs_main-kdauf
                                            BINARY SEARCH .
          IF sy-subrc EQ 0 .
            t_mes_aufnr_out-xmmc = gs_kdauf-xmmc .
          ENDIF.

        ENDIF.

        "读取物料描述
        READ  TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_main-matnr BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_mes_aufnr_out-maktx = gs_matnr-maktx .
        ENDIF.
        "查询销售订单、行对应的信息
        IF gs_main-kdauf IS NOT INITIAL.
          READ TABLE gt_vbap INTO gs_vbap WITH KEY vbeln = gs_main-kdauf posnr = gs_main-kdpos BINARY SEARCH .
          IF sy-subrc EQ 0 .
            t_mes_aufnr_out-xm_matnr = gs_vbap-matnr .
            t_mes_aufnr_out-arktx = gs_vbap-arktx.
            t_mes_aufnr_out-kdmat = gs_vbap-kdmat.
            t_mes_aufnr_out-kwmeng = gs_vbap-kwmeng.
            t_mes_aufnr_out-vrkme = gs_vbap-vrkme .
          ENDIF.
        ELSE.

        ENDIF.

        APPEND t_mes_aufnr_out .
      ENDLOOP.

      "子表数据
      LOOP AT gt_sub INTO gs_sub .
        CLEAR:t_mes_aufnr_sub .
        MOVE-CORRESPONDING gs_sub TO t_mes_aufnr_sub .
        READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_sub-matnr_sub BINARY SEARCH .
        IF sy-subrc EQ 0 .
          t_mes_aufnr_sub-maktx_sub = gs_matnr-maktx .
        ENDIF.
        APPEND t_mes_aufnr_sub .
      ENDLOOP.
    ENDIF.
  ENDIF.

  DATA:t1_lines TYPE i,
       t2_lines TYPE i.

  DATA:len1 TYPE string,
       len2 TYPE string.

  "根据执行结果 写入 日志表  及接口返回变量信息
  IF t_mes_aufnr_out[] IS NOT INITIAL.
    r_msg = '数据读取成功,请查看T_MES_AUFNR_OUT、T_MES_AUFNR_SUB表'.
    CLEAR:gs_zzacknow .
    gs_zzacknow-zzprofg = 'S'.
    DESCRIBE TABLE t_mes_aufnr_out[]  LINES t1_lines .
    DESCRIBE TABLE t_mes_aufnr_sub[]  LINES t2_lines .
    MOVE t1_lines TO len1.
    MOVE t2_lines TO len2.
    CONCATENATE p_dyxtm '系统已成功读取' len1 '条主记录' INTO gs_zzacknow-zzcomen .
    CONCATENATE  gs_zzacknow-zzcomen  '&' len2 '条子记录' INTO gs_zzacknow-zzcomen .

  ELSE.
    r_msg =  '数据读取失败,请重新更新检索条件' .
    gs_zzacknow-zzprofg = 'E'.
    CONCATENATE p_dyxtm '系统读取失败！'  INTO gs_zzacknow-zzcomen .
  ENDIF.
  gs_zzacknow-zzdyxtm  = p_dyxtm.     "调用系统名
  gs_zzacknow-zzintty  = 'AUFNR'.     "接口类型
  gs_zzacknow-zzcdate  = sy-datum.   "日期
  gs_zzacknow-zzctime  = sy-uzeit.   "时间
  gs_zzacknow-zbukrs   = p_werks.    "公司代码
  gs_zzacknow-usnam    = sy-uname .   "用户名
  gs_zzacknow-zzkeyf1  = 'ZMES_SAP_AUFNR_SEL' .   "函数模块名

  APPEND gs_zzacknow TO gt_zzacknow.
  MODIFY zzacknow FROM TABLE gt_zzacknow.




ENDFUNCTION.

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

FORM selxmmc  USING    p_vbeln  TYPE vbeln
                       p_yy      TYPE spras
              CHANGING p_xmmc    TYPE string .


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = p_yy
      name                    = g_objname
      object                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    LOOP AT  it_lines INTO wa_lines .

      IF P_XMMC IS INITIAL .

        p_xmmc = wa_lines-tdline.
      ELSE.

        concatenate wa_lines-tdline p_xmmc into p_xmmc .

      ENDIF.


    ENDLOOP.
  ENDIF.
ENDFORM.
