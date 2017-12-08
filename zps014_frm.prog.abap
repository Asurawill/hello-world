*&---------------------------------------------------------------------*
*&  包含                ZPS014_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_and_process_data .

  DATA: BEGIN OF ls_prps ,
          pspnr TYPE prps-pspnr,
          posid TYPE prps-posid,
          post1 TYPE prps-post1,
          psphi TYPE prps-psphi,
          werks TYPE prps-werks,
        END OF ls_prps .
  DATA lt_prps LIKE TABLE OF ls_prps .
  DATA: BEGIN OF ls_proj ,
          pspnr TYPE proj-pspnr,
          pspid TYPE proj-pspid,
        END OF ls_proj .
  DATA lt_proj LIKE TABLE OF ls_proj .
  DATA: BEGIN OF ls_ckis ,
          pspnr  TYPE prps-pspnr,
          matnr  TYPE ckis-matnr,
          versn  TYPE precp2-versn,
          gpreis TYPE ckis-gpreis,
          menge  TYPE ckis-menge,
        END OF ls_ckis .
  DATA lt_ckis LIKE TABLE OF ls_ckis .
  DATA: BEGIN OF ls_ekpo ,
          pspnr TYPE prps-pspnr,
          matnr TYPE ekpo-matnr,
        END OF ls_ekpo .
  DATA lt_ekpo LIKE TABLE OF ls_ekpo .
  DATA: BEGIN OF ls_resb ,
          pspnr TYPE prps-pspnr,
          matnr TYPE resb-matnr,
          bdmng TYPE resb-bdmng,
        END OF ls_resb .
  DATA lt_resb LIKE TABLE OF ls_resb .
  DATA: BEGIN OF ls_m ,
          pspnr TYPE prps-pspnr,
          matnr TYPE mara-matnr,
        END OF ls_m .
  DATA lt_m LIKE TABLE OF ls_m .
  DATA: BEGIN OF ls_mara ,
          matnr TYPE mara-matnr,
          matkl TYPE mara-matkl,
          meins TYPE mara-meins,
        END OF ls_mara .
  DATA lt_mara LIKE TABLE OF ls_mara .
  DATA: BEGIN OF ls_makt ,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF ls_makt .
  DATA lt_makt LIKE TABLE OF ls_makt .
  DATA: BEGIN OF ls_marc ,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
          beskz TYPE marc-beskz,
        END OF ls_marc .
  DATA lt_marc LIKE TABLE OF ls_marc .
  DATA: lt_t023t TYPE TABLE OF t023t,
        ls_t023t TYPE t023t.
  DATA: BEGIN OF ls_ekan ,
          ps_psp_pnr TYPE ebkn-ps_psp_pnr,
          matnr      TYPE eban-matnr,
          menge      TYPE eban-menge,
        END OF ls_ekan .
  DATA lt_ekan LIKE TABLE OF ls_ekan .
  DATA: BEGIN OF ls_ekpo2 ,
          ebeln      TYPE ekpo-ebeln,
          ebelp      TYPE ekpo-ebelp,
          ps_psp_pnr TYPE ekkn-ps_psp_pnr,
          matnr      TYPE ekpo-matnr,
          menge      TYPE ekpo-menge,
        END OF ls_ekpo2 .
  DATA lt_ekpo2 LIKE TABLE OF ls_ekpo2 .
  DATA: BEGIN OF ls_ekbe ,
          ebeln TYPE ekbe-ebeln,
          ebelp TYPE ekbe-ebelp,
          zekkn TYPE ekbe-zekkn,
          vgabe TYPE ekbe-vgabe,
          gjahr TYPE ekbe-gjahr,
          belnr TYPE ekbe-belnr,
          buzei TYPE ekbe-buzei,
          menge TYPE ekbe-menge,
          shkzg TYPE ekbe-shkzg,
          matnr TYPE ekbe-matnr,
        END OF ls_ekbe .
  DATA lt_ekbe LIKE TABLE OF ls_ekbe .
  DATA: BEGIN OF ls_mseg ,
          mblnr     TYPE mseg-mblnr,
          mjahr     TYPE mseg-mjahr,
          zeile     TYPE mseg-zeile,
          mat_pspnr TYPE mseg-mat_pspnr,
          matnr     TYPE mseg-matnr,
          shkzg     TYPE mseg-shkzg,
          menge     TYPE mseg-menge,
        END OF ls_mseg .
  DATA lt_mseg LIKE TABLE OF ls_mseg .
  DATA: BEGIN OF ls_ekpo3 ,
          ebeln      TYPE ekpo-ebeln,
          ebelp      TYPE ekpo-ebelp,
          ps_psp_pnr TYPE ekkn-ps_psp_pnr,
          matkl      TYPE ekpo-matkl,
          txz01      TYPE ekpo-txz01,
          netpr      TYPE ekpo-netpr,
          menge      TYPE ekpo-menge,
        END OF ls_ekpo3 .
  DATA lt_ekpo3 LIKE TABLE OF ls_ekpo3 .
  DATA: BEGIN OF ls_ekbe2 ,
          ebeln TYPE ekbe-ebeln,
          ebelp TYPE ekbe-ebelp,
          menge TYPE ekbe-menge,
        END OF ls_ekbe2 .
  DATA lt_ekbe2 LIKE TABLE OF ls_ekbe2 .

  DATA ls_alv TYPE ty_alv .

  DATA l_num TYPE i VALUE IS INITIAL .
  DATA l_ebeln TYPE ekko-ebeln .

  SELECT pspnr
         posid
         post1
         psphi
         werks
    INTO CORRESPONDING FIELDS OF TABLE lt_prps
    FROM prps
   WHERE posid IN s_posid
     AND werks IN s_werks .
  IF lt_prps IS NOT INITIAL .
    " 项目定义
    SELECT pspnr
           pspid
      INTO CORRESPONDING FIELDS OF TABLE lt_proj
      FROM proj
       FOR ALL ENTRIES IN lt_prps
     WHERE pspnr = lt_prps-psphi .
    SORT lt_proj BY pspnr .
    DELETE ADJACENT DUPLICATES FROM lt_proj COMPARING pspnr .

    " 第一种类型的物料
    SELECT a~pspnr
           c~matnr
           b~versn
           c~gpreis
           c~menge
      INTO CORRESPONDING FIELDS OF TABLE lt_ckis
      FROM prps AS a
     INNER JOIN precp2 AS b
        ON a~objnr = b~subnr
     INNER JOIN ckis AS c
        ON b~kalnr = c~kalnr
     WHERE a~posid IN s_posid
       AND a~werks IN s_werks
*       AND B~VERSN = '000'
       AND c~typps = 'M'
       AND c~matnr IN s_matnr
       AND c~matkl IN s_matkl .
    SORT lt_ckis BY pspnr matnr versn .
    " 第二种类型的物料
    SELECT a~pspnr
           c~matnr
      INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
      FROM prps AS a
     INNER JOIN ekkn AS b
        ON a~pspnr = b~ps_psp_pnr
     INNER JOIN ekpo AS c
        ON b~ebeln = c~ebeln
       AND b~ebelp = c~ebelp
     INNER JOIN ekko AS d
        ON b~ebeln = d~ebeln
     WHERE a~posid IN s_posid
       AND a~werks IN s_werks
       AND d~bsart = 'Z01'
       AND c~matnr IN s_matnr
       AND c~matkl IN s_matkl .
    SORT lt_ekpo BY pspnr matnr .
    " 第三种类型的物料
    SELECT a~pspnr
           c~matnr
           c~bdmng
      INTO CORRESPONDING FIELDS OF TABLE lt_resb
      FROM prps AS a
     INNER JOIN afpo AS b
        ON a~pspnr = b~projn
     INNER JOIN resb AS c
        ON b~aufnr = c~aufnr
     WHERE a~posid IN s_posid
       AND a~werks IN s_werks
       AND c~matnr IN s_matnr
       AND c~matkl IN s_matkl .
    SORT lt_resb BY pspnr matnr .

    " 合并3种物料
    LOOP AT lt_ckis INTO ls_ckis .
      ls_m-pspnr = ls_ckis-pspnr .
      ls_m-matnr = ls_ckis-matnr .
      APPEND ls_m TO lt_m .
      CLEAR ls_m .
      CLEAR ls_ckis .
    ENDLOOP.
    LOOP AT lt_ekpo INTO ls_ekpo .
      ls_m-pspnr = ls_ekpo-pspnr .
      ls_m-matnr = ls_ekpo-matnr .
      APPEND ls_m TO lt_m .
      CLEAR ls_m .
      CLEAR ls_ekpo .
    ENDLOOP.
    LOOP AT lt_resb INTO ls_resb .
      ls_m-pspnr = ls_resb-pspnr .
      ls_m-matnr = ls_resb-matnr .
      APPEND ls_m TO lt_m .
      CLEAR ls_m .
      CLEAR ls_resb .
    ENDLOOP.
    SORT lt_m BY pspnr matnr .
    DELETE ADJACENT DUPLICATES FROM lt_m COMPARING pspnr matnr .

    IF lt_m IS NOT INITIAL .
      " 单位、物料组
      SELECT matnr
             matkl
             meins
        INTO CORRESPONDING FIELDS OF TABLE lt_mara
        FROM mara
         FOR ALL ENTRIES IN lt_m
       WHERE matnr = lt_m-matnr .
      SORT lt_mara BY matnr .
      " 物料描述
      SELECT matnr
             maktx
        INTO CORRESPONDING FIELDS OF TABLE lt_makt
        FROM makt
         FOR ALL ENTRIES IN lt_m
       WHERE matnr = lt_m-matnr
         AND spras = sy-langu .
      SORT lt_makt BY matnr .
      " 采购类型
      SELECT matnr
             werks
             beskz
        INTO CORRESPONDING FIELDS OF TABLE lt_marc
        FROM marc
         FOR ALL ENTRIES IN lt_m
       WHERE matnr = lt_m-matnr
         AND werks IN s_werks .
      SORT lt_marc BY matnr werks .
      " 物料组描述
      SELECT *
        INTO TABLE lt_t023t
        FROM t023t
       WHERE spras = sy-langu .
      SORT lt_t023t BY matkl .
      " 采购申请数量
      SELECT a~ps_psp_pnr
             b~matnr
             b~menge
        INTO CORRESPONDING FIELDS OF TABLE lt_ekan
        FROM ebkn AS a
       INNER JOIN eban AS b
          ON a~banfn = b~banfn
         AND a~bnfpo = b~bnfpo
         FOR ALL ENTRIES IN lt_m
       WHERE a~ps_psp_pnr = lt_m-pspnr
         AND b~matnr = lt_m-matnr
         AND b~loekz = '' .
      SORT lt_ekan BY ps_psp_pnr matnr .
      " 采购订单数
      SELECT b~ps_psp_pnr
             a~matnr
             a~menge
             a~ebeln
             a~ebelp
        INTO CORRESPONDING FIELDS OF TABLE lt_ekpo2
        FROM ekpo AS a
       INNER JOIN ekkn AS b
          ON a~ebeln = b~ebeln
         AND a~ebelp = b~ebelp
         FOR ALL ENTRIES IN lt_m
       WHERE a~matnr = lt_m-matnr
         AND b~ps_psp_pnr = lt_m-pspnr
         AND a~loekz = '' .
      SORT lt_ekpo2 BY ps_psp_pnr matnr .
      " 采购已入库数
      IF lt_ekpo2 IS NOT INITIAL .
        SELECT ebeln
               ebelp
               zekkn
               vgabe
               gjahr
               belnr
               buzei
               menge
               shkzg
               matnr
          INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
          FROM ekbe
           FOR ALL ENTRIES IN lt_ekpo2
         WHERE ebeln = lt_ekpo2-ebeln
           AND ebelp = lt_ekpo2-ebelp
           AND vgabe = '1' .
        SORT lt_ekbe BY ebeln ebelp matnr .
      ENDIF.
      " 项目发货数
      SELECT
             mblnr
             mjahr
             zeile
             mat_pspnr
             matnr
             shkzg
             menge
      INTO CORRESPONDING FIELDS OF TABLE lt_mseg
      FROM mseg
       FOR ALL ENTRIES IN lt_m
     WHERE mat_pspnr = lt_m-pspnr
       AND matnr = lt_m-matnr
       AND bwart IN ('281','282','Z19','Z20','Z21','Z22','Z23','Z24','Z27','Z28','Z31','Z32').
      SORT lt_mseg BY mat_pspnr matnr .

    ELSE .
      MESSAGE '没物料数据' TYPE 'S' DISPLAY LIKE 'E' .
      LEAVE LIST-PROCESSING .
    ENDIF.

    " 劳务采购信息
    SELECT b~ebeln
           b~ebelp
           a~ps_psp_pnr
           b~matkl
           b~txz01
           b~netpr
           b~menge
      INTO CORRESPONDING FIELDS OF TABLE lt_ekpo3
      FROM ekkn AS a
     INNER JOIN ekpo AS b
        ON a~ebeln = b~ebeln
       AND a~ebelp = b~ebelp
     INNER JOIN ekko AS c
        ON a~ebeln = c~ebeln
       FOR ALL ENTRIES IN lt_prps
     WHERE a~ps_psp_pnr = lt_prps-pspnr
       AND c~bsart = 'Z09' .
    SORT lt_ekpo3 BY ps_psp_pnr .
    IF lt_ekpo3 IS NOT INITIAL .
      SELECT ebeln
             ebelp
             menge
        INTO CORRESPONDING FIELDS OF TABLE lt_ekbe2
        FROM ekbe
         FOR ALL ENTRIES IN lt_ekpo3
       WHERE ebeln = lt_ekpo3-ebeln
         AND ebelp = lt_ekpo3-ebelp .
      SORT lt_ekbe2 BY ebeln ebelp .
    ENDIF.

  ELSE .
    MESSAGE '没项目数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF.

* 将查询结果塞进ALV
  LOOP AT lt_prps INTO ls_prps .
*    L_NUM = L_NUM + 1 .
*    GS_ALV-NUM = L_NUM .
    READ TABLE lt_proj INTO ls_proj WITH KEY pspnr = ls_prps-psphi BINARY SEARCH .
    ls_alv-pspid = ls_proj-pspid .
    ls_alv-posid = ls_prps-posid .
    ls_alv-post1 = ls_prps-post1 .
    ls_alv-werks = ls_prps-werks .
    READ TABLE lt_m WITH KEY pspnr = ls_prps-pspnr BINARY SEARCH TRANSPORTING NO FIELDS .
    IF sy-subrc = 0 .
      LOOP AT lt_m INTO ls_m FROM sy-tabix .
        IF ls_m-pspnr = ls_prps-pspnr .
          MOVE-CORRESPONDING ls_alv TO gs_alv .
          l_num = l_num + 1 .
          gs_alv-num = l_num .
          gs_alv-matnr = ls_m-matnr .
          "增加库存量 BY 黄健杭 执行人：夏俊
          CLEAR gs_alv-lbkum.
          SELECT SINGLE lbkum
            INTO gs_alv-lbkum
            FROM qbew
            WHERE pspnr = ls_prps-pspnr
            AND   matnr = gs_alv-matnr.
          READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_m-matnr BINARY SEARCH .
          gs_alv-maktx = ls_makt-maktx .
          READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_m-matnr
                                                   werks = ls_prps-werks BINARY SEARCH .
          gs_alv-beskz = ls_marc-beskz .
          READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_m-matnr BINARY SEARCH .
          gs_alv-matkl = ls_mara-matkl .
          gs_alv-meins = ls_mara-meins .
          READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_mara-matkl BINARY SEARCH .
          gs_alv-wgbez60 = ls_t023t-wgbez .
          READ TABLE lt_ckis INTO ls_ckis WITH KEY pspnr = ls_prps-pspnr
                                                   matnr = ls_m-matnr
                                                   versn = '100' BINARY SEARCH .
          gs_alv-gpreis_ht = ls_ckis-gpreis .
          gs_alv-menge_ht = ls_ckis-menge .
          CLEAR ls_ckis .
          READ TABLE lt_ckis INTO ls_ckis WITH KEY pspnr = ls_prps-pspnr
                                                   matnr = ls_m-matnr
                                                   versn = '000' BINARY SEARCH .
          gs_alv-gpreis_ys = ls_ckis-gpreis .
          gs_alv-menge_ys = ls_ckis-menge .
          READ TABLE lt_resb WITH KEY pspnr = ls_prps-pspnr
                                      matnr = ls_m-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
          IF sy-subrc = 0 .
            LOOP AT lt_resb INTO ls_resb FROM sy-tabix .
              IF ls_resb-pspnr = ls_prps-pspnr AND
                   ls_resb-matnr = ls_m-matnr .
                gs_alv-bdmng = gs_alv-bdmng + ls_resb-bdmng .
                CLEAR ls_resb .
              ELSE .
                CLEAR ls_resb .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.
          READ TABLE lt_ekan INTO ls_ekan WITH KEY ps_psp_pnr = ls_prps-pspnr
                                                        matnr = ls_m-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
          IF sy-subrc = 0 .
            LOOP AT lt_ekan INTO ls_ekan FROM sy-tabix .
              IF ls_ekan-ps_psp_pnr = ls_prps-pspnr AND
                  ls_ekan-matnr = ls_m-matnr .
                gs_alv-menge_pr = gs_alv-menge_pr + ls_ekan-menge .
                CLEAR ls_ekan .
              ELSE .
                CLEAR ls_ekan .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.
          READ TABLE lt_ekpo2 WITH KEY ps_psp_pnr = ls_prps-pspnr
                                            matnr = ls_m-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
          IF sy-subrc = 0 .
            LOOP AT lt_ekpo2 INTO ls_ekpo2 FROM sy-tabix .
              IF ls_ekpo2-ps_psp_pnr = ls_prps-pspnr AND
                    ls_ekpo2-matnr = ls_m-matnr .
                gs_alv-menge_po = gs_alv-menge_po + ls_ekpo2-menge .
                l_ebeln = ls_ekpo2-ebeln .
                "收货数量需要考虑所有的采购订单以及采购订单行项目
                READ TABLE lt_ekbe WITH KEY ebeln = l_ebeln
                                            ebelp = ls_ekpo2-ebelp
                                            matnr = ls_m-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
                IF sy-subrc = 0 .
                  LOOP AT lt_ekbe INTO ls_ekbe FROM sy-tabix .
                    IF ls_ekbe-ebeln = l_ebeln AND
                        ls_ekbe-ebelp = ls_ekpo2-ebelp AND
                          ls_ekbe-matnr = ls_m-matnr .
                      IF ls_ekbe-shkzg = 'S'.
                        gs_alv-menge_rk = gs_alv-menge_rk + ls_ekbe-menge .
                      ELSEIF ls_ekbe-shkzg = 'H'.
                        gs_alv-menge_rk = gs_alv-menge_rk - ls_ekbe-menge .
                      ENDIF.
                      CLEAR ls_ekbe .
                    ELSE .
                      CLEAR ls_ekbe .
                      EXIT .
                    ENDIF.
                  ENDLOOP.
                ENDIF.
                CLEAR ls_ekpo2 .
              ELSE .
                CLEAR ls_ekpo2 .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.
          READ TABLE lt_mseg WITH KEY mat_pspnr = ls_prps-pspnr
                                          matnr = ls_m-matnr BINARY SEARCH TRANSPORTING NO FIELDS .
          IF sy-subrc = 0 .
            LOOP AT lt_mseg INTO ls_mseg FROM sy-tabix .
              IF ls_mseg-mat_pspnr = ls_prps-pspnr AND
                    ls_mseg-matnr = ls_m-matnr .
                IF ls_mseg-shkzg = 'S'.
                  gs_alv-menge_fh = gs_alv-menge_fh - ls_mseg-menge .
                ELSEIF ls_mseg-shkzg = 'H'.
                  gs_alv-menge_fh = gs_alv-menge_fh + ls_mseg-menge .
                ENDIF.
                CLEAR ls_mseg .
              ELSE .
                CLEAR ls_mseg .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.

          APPEND gs_alv TO gt_alv .
          CLEAR gs_alv .

          CLEAR l_ebeln .
          CLEAR ls_ckis .
          CLEAR ls_t023t .
          CLEAR ls_mara .
          CLEAR ls_marc .
          CLEAR ls_makt .
          CLEAR ls_m .
        ELSE .
          CLEAR ls_m .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.

    " 劳务行
    READ TABLE lt_ekpo3 WITH KEY ps_psp_pnr = ls_prps-pspnr BINARY SEARCH TRANSPORTING NO FIELDS .
    IF sy-subrc = 0 .
      LOOP AT lt_ekpo3 INTO ls_ekpo3 FROM sy-tabix .
        IF ls_ekpo3-ps_psp_pnr = ls_prps-pspnr .
          MOVE-CORRESPONDING ls_alv TO gs_alv .
          l_num = l_num + 1 .
          gs_alv-num = l_num .
          gs_alv-matkl = ls_ekpo3-matkl .
          READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_ekpo3-matkl BINARY SEARCH .
          gs_alv-wgbez60 = ls_t023t-wgbez60 .
          gs_alv-ebeln = ls_ekpo3-ebeln .
          gs_alv-ebelp = ls_ekpo3-ebelp .
          gs_alv-txz01 = ls_ekpo3-txz01 .
          gs_alv-netpr = ls_ekpo3-netpr .
          gs_alv-menge_po = ls_ekpo3-menge .
          READ TABLE lt_ekbe2 WITH KEY ebeln = ls_ekpo3-ebeln
                                       ebelp = ls_ekpo3-ebelp BINARY SEARCH TRANSPORTING NO FIELDS .
          IF sy-subrc = 0 .
            LOOP AT lt_ekbe2 INTO ls_ekbe2 FROM sy-tabix .
              IF ls_ekbe2-ebeln = ls_ekpo3-ebeln AND
                  ls_ekbe2-ebelp = ls_ekpo3-ebelp .
                gs_alv-menge_rk = gs_alv-menge_rk + ls_ekbe2-menge .
                CLEAR ls_ekbe2 .
              ELSE .
                CLEAR ls_ekbe2 .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.
          " 颜色
          gs_alv-clr = 'C310' .

          APPEND gs_alv TO gt_alv .
          CLEAR gs_alv .

          CLEAR ls_t023t .
          CLEAR ls_ekpo3 .
        ELSE .
          CLEAR ls_ekpo3 .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR ls_proj .
    CLEAR ls_prps .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_alv .
  PERFORM init_layout.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM frm_init_lvc.            "字段列定义
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
*  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM frm_output  .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_layout .
  gs_layout-zebra        = 'X'.
  gs_layout-cwidth_opt   = 'X'.
  gs_layout-box_fname = 'SEL'.
  gs_layout-info_fname = 'CLR'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'NUM'          '序号'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'PSPID'        '项目定义'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'POSID'        'WBS编号'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'POST1'        '项目描述'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'WERKS'        '工厂'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' 'X' '' '' '' '' ''.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'BESKZ'        '采购类型'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATKL'        '物料组'         '' '' '' '' '' '' ''.
  init_fieldcat 'WGBEZ60'        '物料组描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'        '计量单位'         '' '' '' '' '' '' ''.

  AUTHORITY-CHECK OBJECT 'Z_CNY'
          ID 'Z_CNY' FIELD 'X' .
  IF sy-subrc = 0 .
    init_fieldcat 'GPREIS_HT'        '合同单价'         '' '' '' '' '' '' ''.
  ENDIF.

  init_fieldcat 'MENGE_HT'        '合同数'         '' '' '' '' '' '' ''.

  AUTHORITY-CHECK OBJECT 'Z_CNY'
          ID 'Z_CNY' FIELD 'X' .
  IF sy-subrc = 0 .
    init_fieldcat 'GPREIS_YS'        '预算单价'         '' '' '' '' '' '' ''.
  ENDIF.

  init_fieldcat 'MENGE_YS'        '预算数'         '' '' '' '' '' '' ''.
  init_fieldcat 'BDMNG'        '预留数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE_PR'        '采购申请数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'EBELN'        '劳务采购订单'         '' '' '' '' '' '' ''.
  init_fieldcat 'EBELP'        '订单项目行号'         '' '' '' '' '' '' ''.
  init_fieldcat 'TXZ01'        '劳务采购描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'NETPR'        '劳务采购订单价格'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE_PO'        '采购订单数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE_RK'        '采购已入库数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE_FH'        '项目发货数量'         '' '' '' '' '' '' ''.
  init_fieldcat 'LBKUM'        '库存数量'         '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_output .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET = PU_STATUS
*     I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   = ''
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    = PW_GRID_SETTINGS
      is_layout_lvc      = gs_layout
      it_fieldcat_lvc    = gt_lvc
*     IT_EXCLUDING       = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        = PT_SORT[]
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         = PW_VARIANT
*     IT_EVENTS          = GT_EVENTS
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
