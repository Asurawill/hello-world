REPORT zmm035_1.

"Created by :IT02
"Request:    整备库报表:刷新台账
"Modify by:
"Modify date:
"
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2017-06-16  IT02&WEIYUN    ED1K905743     优化：查询zzbksj台账表明细

TABLES:mseg,mkpf,zzbksj.

TYPES:BEGIN OF ty_data,
        mblnr      TYPE mseg-mblnr, "物料凭证号
        zeile      TYPE mseg-zeile,  "物料凭证行项目
        mjahr      TYPE mseg-mjahr,  "物料凭证年度
        clm    ,                       "处理吗
        clzt   ,                      "处理状态
        werks      TYPE mseg-werks,  "工厂
        lgort      TYPE mseg-lgort,  "库存地点
        matnr      TYPE mseg-matnr,  "物料编码
        menge      TYPE mseg-menge,  "数量
        meins      TYPE mseg-meins,  "单位
        shkzg      TYPE mseg-shkzg,   "借/贷方标识
        vbeln      TYPE vbak-vbeln,  "项目订单
        mat_kdauf  TYPE mseg-kdauf,  "销售订单号
        mat_kdpos  TYPE mseg-kdpos,  "销售订单行项目
        vbeln_im   TYPE mseg-vbeln_im,  "交货单号
        vbelp_im   TYPE mseg-vbelp_im,   "交货单行号
        ebeln      TYPE mseg-ebeln,    "采购订单号
        ebelp      TYPE mseg-ebelp,    "采购订单行号
        ablad      TYPE mseg-ablad,     "卸货点
        bwart      TYPE mseg-bwart,      "移动类型
        budat_mkpf TYPE mseg-budat_mkpf,  "过账日期
        usnam_mkpf TYPE mseg-usnam_mkpf, "过账人
        smbln      TYPE mseg-smbln,   "冲销凭证对应原始凭证
        smblp      TYPE mseg-smblp,   "冲销凭证对应原始凭证行项目

      END OF ty_data .

TYPES:BEGIN OF ty_save,
        mblnr      TYPE mseg-mblnr,      "物料凭证号
        mjahr      TYPE mseg-mjahr,      "物料凭证年度
        zeile      TYPE mseg-zeile,      "物料凭证行项目
        vbeln      TYPE vbak-vbeln,      "项目订单
        werks      TYPE mseg-werks ,     "工厂
        lgort      TYPE mseg-lgort,      "库存地点
        matnr      TYPE mseg-matnr,      "物料号
        menge      TYPE mseg-menge,       "数量
        meins      TYPE mseg-meins,       "单位
        shkzg      TYPE mseg-shkzg,       "借方/贷方标识
        mat_kdauf  TYPE mseg-mat_kdauf,   "销售订单
        mat_kdpos  TYPE mseg-mat_kdpos,   "销售订单行号
        vbeln_im   TYPE mseg-vbeln_im,    "交货
        vbelp_im   TYPE mseg-vbelp_im,    "交货项目
        ablad      TYPE mseg-ablad,       "卸货点
        bwart      TYPE mseg-bwart,       "移动类型
        budat_mkpf TYPE mseg-budat_mkpf,  "过账日期
        usnam_mkpf TYPE mseg-usnam_mkpf,  "过账人
        smbln      TYPE mseg-smbln,       "物料凭证编号
        smblp      TYPE mseg-smblp,       "物料凭证中的项目
      END OF ty_save .

TYPES:BEGIN OF ty_cg,
        ebeln TYPE ekko-ebeln,
        ebelp TYPE ekpo-ebelp,
      END OF ty_cg .

TYPES:BEGIN OF ty_jh,
        vbeln_im TYPE mseg-vbeln_im,
        vbelp_im TYPE mseg-vbelp_im,
      END OF ty_jh .

TYPES:BEGIN OF ty_mblnr,
        werks TYPE mseg-werks,
*&--代码添加 BY HANDYBY 31.05.2017 18:20:08  BEGIN
        mjahr TYPE mseg-mjahr,
        zeile TYPE mseg-zeile,
*&--代码添加 BY HANDYBY 31.05.2017 18:20:08  END
        mblnr TYPE mseg-mblnr,
      END OF  ty_mblnr .

DATA:gt_mblnr TYPE TABLE OF ty_mblnr,
     gs_mblnr TYPE ty_mblnr.

*&--代码添加 BY HANDYBY 08.06.2017 15:24:41  BEGIN
DATA:gt_mblnr1 LIKE gt_mblnr,
     gs_mblnr1 LIKE LINE OF gt_mblnr1.
DATA:gt_mblnr2 LIKE gt_mblnr,
     gs_mblnr2 LIKE LINE OF gt_mblnr2.
*&--代码添加 BY HANDYBY 08.06.2017 15:24:41  END


DATA:gt_save TYPE TABLE OF ty_save,
     gs_save TYPE ty_save.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_data_1 TYPE TABLE OF ty_data,
     gs_data_1 TYPE ty_data.

DATA:gt_cg TYPE TABLE OF ty_cg,
     gs_cg TYPE ty_cg.

DATA:gt_jh TYPE TABLE OF ty_jh,
     gs_jh TYPE ty_jh.

DATA:gt_data_e TYPE TABLE OF ty_data,
     gs_data_e TYPE ty_data.

DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.

DATA:gt_ekkn TYPE TABLE OF ekkn,
     gs_ekkn TYPE ekkn.

DATA:gt_lips TYPE TABLE OF lips,
     gs_lips TYPE lips.

DATA:gt_mseg TYPE TABLE OF mseg,
     gs_mseg TYPE mseg.

DATA:gs_zzbksj TYPE zzbksj,
     gt_zzbksj TYPE TABLE OF zzbksj.

DATA:l_4 TYPE mblnr,
     l_5 TYPE mblnr.

RANGES:r_lgort FOR mseg-lgort .

FIELD-SYMBOLS:<fs_data> TYPE ty_data .

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS:p_werks TYPE mseg-werks .  "工厂


"SELECT-OPTIONS: s_budat FOR mkpf-budat.  "过账日期
SELECTION-SCREEN END OF BLOCK blk1.

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

*权限检查检查工厂代码
  PERFORM frm_auth_check USING '03'.
  IF sy-subrc NE 0.
    MESSAGE e603(fco) WITH p_werks DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
*  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
FORM frm_auth_check USING VALUE(p_actvt).
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'ACTVT' FIELD p_actvt
                                      ID 'WERKS' FIELD p_werks.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .

  "根据工厂：默认查询的库存地点
  IF p_werks EQ '1500' .
    CLEAR:r_lgort .
    r_lgort-sign = 'I'.
    r_lgort-option = 'EQ'.
    r_lgort-low = '3002'.
    APPEND r_lgort .
    r_lgort-low = '3003'.
    APPEND r_lgort .

  ELSEIF p_werks EQ '1100'.
    CLEAR:r_lgort .
    r_lgort-sign = 'I'.
    r_lgort-option = 'EQ'.
    r_lgort-low = '3002'.
    APPEND r_lgort .
    r_lgort-low = '3004'.
    APPEND r_lgort .
    r_lgort-low = '3006'.
    APPEND r_lgort .
    r_lgort-low = '3012'.
    APPEND r_lgort .


  ENDIF.

  "取存储整备库表的最大值

  "取出49最大的物料凭证号
  SELECT  mblnr werks INTO CORRESPONDING FIELDS OF TABLE gt_mblnr
  FROM zzbksj
  WHERE werks EQ p_werks
  AND mblnr LIKE '49%'
  .
  SORT gt_mblnr BY mblnr DESCENDING .

*&--代码添加 BY HANDYBY 08.06.2017 15:08:51  BEGIN
  DELETE ADJACENT DUPLICATES FROM gt_mblnr COMPARING mblnr .
  READ TABLE gt_mblnr INTO gs_mblnr INDEX 20 .
*&--代码添加 BY HANDYBY 08.06.2017 15:08:51  END
*&--代码注释 BY HANDYBY 08.06.2017 15:09:30  BEGIN
*    READ TABLE GT_MBLNR INTO GS_MBLNR INDEX 1.
*&--代码注释 BY HANDYBY 08.06.2017 15:09:30  END

  IF sy-subrc EQ 0.
    l_4  = gs_mblnr-mblnr  + 1.
  ELSE.
    l_4 = '4900000000'.
  ENDIF.

**&--代码添加 BY HANDYBY 08.06.2017 15:33:51  BEGIN
*  MOVE-CORRESPONDING GT_MBLNR TO GT_MBLNR1 .
*  SORT GT_MBLNR1 BY MBLNR MJAHR ZEILE .
*  REFRESH GT_MBLNR .
**&--代码添加 BY HANDYBY 08.06.2017 15:33:51  END

  "//取出已存储zzbksj表的现明细账 by it02 2017.06.16 begin
  SELECT werks mblnr mjahr zeile INTO CORRESPONDING FIELDS OF TABLE gt_mblnr1
      FROM zzbksj
    WHERE werks EQ p_werks
    AND  mblnr BETWEEN l_4 AND '4999999999' .
  .
  SORT gt_mblnr1 BY mblnr mjahr zeile .
  "//取出已存储zzbksj表的现明细账 by it02 2017.06.16 end

  "取出5最大的物料凭证号
  SELECT  mblnr werks INTO CORRESPONDING FIELDS OF TABLE gt_mblnr
  FROM zzbksj
  WHERE werks EQ p_werks
  AND mblnr LIKE '5%'
  .
  SORT gt_mblnr BY mblnr DESCENDING .

*&--代码添加 BY HANDYBY 08.06.2017 15:09:59  BEGIN
  DELETE ADJACENT DUPLICATES FROM gt_mblnr COMPARING mblnr .
  READ TABLE gt_mblnr INTO gs_mblnr INDEX 20 .
*&--代码添加 BY HANDYBY 08.06.2017 15:09:59  END
*&--代码注释 BY HANDYBY 08.06.2017 15:10:13  BEGIN
*  READ TABLE GT_MBLNR INTO GS_MBLNR INDEX 1.
*&--代码注释 BY HANDYBY 08.06.2017 15:10:13  END

  IF sy-subrc EQ 0.
    l_5  = gs_mblnr-mblnr  + 1.
  ELSE.
    l_5 = '5000000000'.
  ENDIF.

*&--代码添加 BY HANDYBY 08.06.2017 15:33:58  BEGIN
*  MOVE-CORRESPONDING gt_mblnr TO gt_mblnr2 .
*  SORT gt_mblnr2 BY mblnr mjahr zeile .
*  REFRESH gt_mblnr .
*&--代码添加 BY HANDYBY 08.06.2017 15:33:58  END

 "//取出已存储zzbksj表的现明细账 by it02 2017.06.16 begin
  SELECT werks mblnr mjahr zeile INTO CORRESPONDING FIELDS OF TABLE gt_mblnr2
      FROM zzbksj
    WHERE werks EQ p_werks
    AND mblnr BETWEEN l_5 AND '5999999999'
    .
  SORT gt_mblnr2 BY mblnr mjahr zeile .
 "//取出已存储zzbksj表的现明细账 by it02 2017.06.16 end.

  "取出按单的物料凭证
  SELECT a~mblnr a~mjahr
         b~zeile b~werks  b~mat_kdauf  AS vbeln
         b~lgort b~matnr b~menge b~meins b~shkzg
         b~mat_kdauf b~mat_kdpos b~bwart
         b~budat_mkpf b~usnam_mkpf b~smbln b~smblp
  INTO CORRESPONDING FIELDS OF TABLE gt_save
  FROM mkpf AS a
  INNER JOIN mseg AS b
  ON a~mblnr = b~mblnr
  AND a~mjahr  = b~mjahr
  WHERE b~werks EQ p_werks
  AND a~budat > '20151101'
 " AND a~budat IN s_budat
  AND (    a~mblnr BETWEEN l_4 AND '4999999999'
        OR a~mblnr BETWEEN l_5 AND '5999999999' )
  AND b~sobkz NE 'F'
  AND b~sobkz EQ 'E'
  AND b~lgort IN r_lgort .

  SORT gt_save BY mblnr mjahr zeile .

  "取出非按单的物料凭证明细
  SELECT a~mblnr a~mjahr
        b~zeile b~werks  b~lgort  b~matnr b~menge b~meins b~shkzg b~vbeln_im
        b~vbelp_im b~ebeln b~ebelp b~ablad b~bwart b~budat_mkpf b~usnam_mkpf b~smbln b~smblp
   INTO CORRESPONDING FIELDS OF TABLE gt_data
   FROM mkpf AS a
   INNER JOIN mseg AS b
   ON a~mblnr = b~mblnr
   AND a~mjahr  = b~mjahr
   WHERE b~werks EQ p_werks
   AND a~budat > '20151101'
  " AND a~budat IN s_budat
   AND (    a~mblnr BETWEEN l_4 AND '4999999999'
         OR a~mblnr BETWEEN l_5 AND '5999999999' )
   AND b~sobkz NOT IN ('F','E')
   AND b~lgort IN r_lgort .


  SORT gt_data BY mblnr mjahr zeile .

  IF gt_data IS NOT INITIAL .
    gt_data_1 = gt_data.
    "gt_data_1保留无匹配项目订单关系的物料凭证明细
    DELETE gt_data_1 WHERE ablad NE '' OR  ebeln NE '' OR vbeln_im NE '' .
    LOOP AT gt_data_1 INTO gs_data_1.
      CLEAR:gs_save.
      "传送：GS_DATA_1的物料凭证明细到GS_SAVE结构
      MOVE-CORRESPONDING gs_data_1 TO gs_save.
*      gs_save-mblnr = gs_data_1-mblnr.
*      gs_save-mjahr = gs_data_1-mjahr.
*      gs_save-zeile = gs_data_1-zeile.
*      gs_save-werks = gs_data_1-werks.
*      gs_save-budat = gs_data_1-budat_mkpf.
      APPEND gs_save TO gt_save .

    ENDLOOP.
    SORT gt_save BY mblnr mjahr zeile .

    "gt_data只保留可匹配项目订单关系的物料凭证明细
    DELETE gt_data WHERE ablad EQ ''  AND ebeln  EQ '' AND vbeln_im EQ '' .

    "gt_cg只保留不重复的采购订单
    MOVE-CORRESPONDING gt_data TO gt_cg .
    DELETE gt_cg WHERE ebeln EQ '' .
    SORT gt_cg BY ebeln .
    DELETE ADJACENT DUPLICATES FROM gt_cg COMPARING ebeln ebelp.
    IF gt_cg IS NOT INITIAL.

      SELECT * INTO TABLE gt_ekkn
        FROM ekkn
        FOR  ALL ENTRIES IN gt_cg
        WHERE ebeln = gt_cg-ebeln
        AND   ebelp = gt_cg-ebelp
        .
      SORT gt_ekkn BY ebeln ebelp.
    ENDIF.

    "gt_jh只保留不重复的交货单号
    MOVE-CORRESPONDING gt_data TO gt_jh .
    DELETE gt_jh WHERE vbeln_im EQ '' .
    SORT gt_jh  BY vbeln_im vbelp_im .
    DELETE ADJACENT DUPLICATES FROM gt_jh COMPARING vbeln_im vbelp_im.

    IF  gt_jh IS NOT INITIAL.
      SELECT * INTO TABLE gt_lips
      FROM lips
      FOR ALL ENTRIES IN gt_jh
      WHERE  vbeln = gt_jh-vbeln_im
      AND  posnr = gt_jh-vbelp_im.

      SORT gt_lips BY vbeln posnr .

    ENDIF.
    "查询销售订单明细
    SELECT * INTO TABLE gt_vbak
      FROM vbak .
    SORT gt_vbak BY vbeln .



  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .
  LOOP AT gt_data INTO gs_data.
    "先判断卸货点再采购订单再交货单号
    IF gs_data-ablad NE '' .
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-ablad BINARY SEARCH .
      IF sy-subrc EQ  0 .
        CLEAR:gs_save .
        MOVE-CORRESPONDING gs_data TO gs_save.
        gs_save-vbeln = gs_data-ablad ."验证过的项目订单号写到VBELN
        APPEND gs_save TO gt_save .
        CONTINUE .
*        gs_save-mblnr = gs_data-mblnr .
*        gs_save-mjahr = gs_data-mjahr.
*        gs_save-zeile = gs_data-zeile.
*        gs_save-vbeln = gs_data-ablad .
*        gs_save-werks = gs_data-werks .
*        gs_save-budat = gs_data_1-budat_mkpf.
      ELSE.
        CLEAR:gs_save.
        MOVE-CORRESPONDING gs_data TO gs_save.
        APPEND gs_save TO gt_save .
        CONTINUE .

      ENDIF.

    ELSEIF gs_data-ebeln NE ''.
      READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_data-ebeln
                                               ebelp = gs_data-ebelp
                                      BINARY SEARCH .
      IF sy-subrc EQ 0.
        CLEAR:gs_save .
        MOVE-CORRESPONDING gs_data TO gs_save.
        gs_save-vbeln = gs_ekkn-vbeln ."验证过的项目订单号写到VBELN
        APPEND gs_save TO gt_save .
        CONTINUE .
*        gs_save-mblnr = gs_data-mblnr .
*        gs_save-mjahr = gs_data-mjahr.
*        gs_save-zeile = gs_data-zeile.
*        gs_save-vbeln = gs_data-ebeln .
*        gs_save-werks = gs_data-werks .
*        gs_save-budat = gs_data_1-budat_mkpf.
*        APPEND gs_save TO gt_save .
*        CONTINUE .
      ELSE.
        CLEAR:gs_save.
        MOVE-CORRESPONDING gs_data TO gs_save.
        APPEND gs_save TO gt_save .
        CONTINUE .
      ENDIF.

    ELSEIF gs_data-vbeln_im NE ''.
      READ TABLE gt_lips INTO gs_lips WITH KEY vbeln = gs_data-vbeln_im
                                               posnr = gs_data-vbelp_im
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        CLEAR:gs_save.
        MOVE-CORRESPONDING gs_data TO gs_save.
        gs_save-vbeln = gs_lips-vgbel ."验证过的项目订单号写到VBELN
        APPEND gs_save TO gt_save .
        CONTINUE .
*        gs_save-mblnr = gs_data-mblnr .
*        gs_save-mjahr = gs_data-mjahr.
*        gs_save-zeile = gs_data-zeile.
*        gs_save-vbeln = gs_data-vbeln_im .
*        gs_save-werks = gs_data-werks .
*        gs_save-budat = gs_data_1-budat_mkpf.
*        APPEND gs_save TO gt_save .
      ELSE.
        CLEAR:gs_save.
        MOVE-CORRESPONDING gs_data TO gs_save.
        APPEND gs_save TO gt_save .
        CONTINUE .
*        gs_save-mblnr = gs_data-mblnr .
*        gs_save-mjahr = gs_data-mjahr.
*        gs_save-zeile = gs_data-zeile.
*        gs_save-werks = gs_data-werks .
*        gs_save-budat = gs_data_1-budat_mkpf.
*        APPEND gs_save TO gt_save .
      ENDIF.
    ELSE.
      CLEAR:gs_save.
      MOVE-CORRESPONDING gs_data TO gs_save.
      APPEND gs_save TO gt_save .
      CONTINUE .
    ENDIF.
    .
  ENDLOOP.
  SORT gt_save BY mblnr mjahr zeile .

  DELETE ADJACENT DUPLICATES FROM gt_save COMPARING mblnr mjahr zeile .

  IF gt_save IS NOT INITIAL.
    LOOP AT gt_save INTO gs_save .

      CLEAR:gs_zzbksj .

*&--代码添加 BY HANDYBY 08.06.2017 15:36:50  BEGIN
      READ TABLE gt_mblnr1 WITH KEY mblnr = gs_save-mblnr
                                    mjahr = gs_save-mjahr
                                    zeile = gs_save-zeile
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
      IF sy-subrc = 0 .
        CONTINUE .
      ELSE .
        READ TABLE gt_mblnr2 WITH KEY mblnr = gs_save-mblnr
                                      mjahr = gs_save-mjahr
                                      zeile = gs_save-zeile
                                      BINARY SEARCH TRANSPORTING NO FIELDS .
        IF sy-subrc = 0 .
          CONTINUE .
        ENDIF.
      ENDIF.
*&--代码添加 BY HANDYBY 08.06.2017 15:36:50  END
      MOVE-CORRESPONDING gs_save TO gs_zzbksj .

      IF gs_save-vbeln IS NOT INITIAL.
        gs_zzbksj-zclm = '2'.
      ELSE.
        gs_zzbksj-zclm = '3'.
      ENDIF.
      "维护账户
      gs_zzbksj-whnam = sy-uname.
      "维护日期
      gs_zzbksj-whdat = sy-datum .
      "维护时间
      gs_zzbksj-whtim = sy-uzeit.
      APPEND gs_zzbksj TO gt_zzbksj .
    ENDLOOP.

    SORT gt_zzbksj  BY mblnr mjahr zeile .
  ENDIF.

  IF  gt_zzbksj IS NOT INITIAL.
    "更新数据表ZZBKSJ 。
    MODIFY zzbksj FROM TABLE gt_zzbksj .

    MESSAGE '数据更新成功！'  TYPE 'I'.
  ELSE.
    MESSAGE '无数据需更新!' TYPE 'I'.

  ENDIF.

*    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_mseg
*      FROM mseg
*      FOR ALL ENTRIES IN gt_save
*      WHERE mblnr = gt_save-mblnr
*      AND   mjahr = gt_save-mjahr
*      AND   zeile = gt_save-zeile .
*
*    SORT gt_mseg BY mblnr mjahr zeile .
ENDFORM.
