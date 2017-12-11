*&---------------------------------------------------------------------*
*&  包含                ZMM045_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_SET_DATA_FOR_POSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_SET_DATA_FOR_POSO .
******************************************
* PO+SO 部分
******************************************
  " POSO 主表取值
  IF P_EKORG1 IS NOT INITIAL .
    " PO取值
    IF S_PSPNR1[] IS NOT INITIAL .
      SELECT A~EBELN
             A~BUKRS
             A~EKGRP
             A~RLWRT
        INTO CORRESPONDING FIELDS OF TABLE GT_EKON
        FROM EKKO AS A
       INNER JOIN EKKN AS B
          ON A~EBELN = B~EBELN
       WHERE A~EKORG = P_EKORG1
         AND A~LIFNR IN S_LIFNR1
         AND A~EBELN IN S_EBELN1
         AND A~BEDAT IN S_BEDAT1
         AND A~EKGRP IN S_EKGRP1
         AND A~ERNAM IN S_ERNAM1
         AND B~PS_PSP_PNR IN S_PSPNR1 .
    ELSE .
      SELECT EBELN
             BUKRS
             EKGRP
             RLWRT
        INTO CORRESPONDING FIELDS OF TABLE GT_EKON
        FROM EKKO
       WHERE EKORG = P_EKORG1
         AND LIFNR IN S_LIFNR1
         AND EBELN IN S_EBELN1
         AND BEDAT IN S_BEDAT1
         AND EKGRP IN S_EKGRP1
         AND ERNAM IN S_ERNAM1 .
    ENDIF.
    " SO取值
    DATA: BEGIN OF LS_EKKO ,
            BSTNK TYPE VBAK-BSTNK,
          END OF LS_EKKO .
    DATA LT_EKKO LIKE TABLE OF LS_EKKO .
    LOOP AT GT_EKON INTO GS_EKON .
      LS_EKKO-BSTNK = GS_EKON-EBELN .
      APPEND LS_EKKO TO LT_EKKO .
      CLEAR LS_EKKO .
      CLEAR GS_EKON .
    ENDLOOP.
    SORT LT_EKKO BY BSTNK .
    DELETE ADJACENT DUPLICATES FROM LT_EKKO COMPARING BSTNK .
    IF LT_EKKO IS NOT INITIAL .
      IF P_VKORG2 IS NOT INITIAL .
        SELECT VBELN
               VKORG
               NETWR
               BSTNK
          INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
          FROM VBAK
           FOR ALL ENTRIES IN LT_EKKO
         WHERE VKORG = P_VKORG2
           AND KUNNR IN S_KUNNR2
           AND VBELN IN S_VBELN2
           AND BSTNK IN S_BSTNK2
           AND ERNAM IN S_ERNAM2
           AND AUDAT IN S_AUDAT2
           AND BSTNK = LT_EKKO-BSTNK .
      ELSE .
        SELECT VBELN
               VKORG
               NETWR
               BSTNK
          INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
          FROM VBAK
           FOR ALL ENTRIES IN LT_EKKO
         WHERE KUNNR IN S_KUNNR2
           AND VBELN IN S_VBELN2
           AND BSTNK IN S_BSTNK2
           AND ERNAM IN S_ERNAM2
           AND AUDAT IN S_AUDAT2
           AND BSTNK = LT_EKKO-BSTNK .
      ENDIF.
    ENDIF.
    REFRESH LT_EKKO .
  ELSEIF P_VKORG2 IS NOT INITIAL .
    " SO取值
    SELECT VBELN
           VKORG
           NETWR
           BSTNK
      INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
      FROM VBAK
     WHERE VKORG = P_VKORG2
       AND KUNNR IN S_KUNNR2
       AND VBELN IN S_VBELN2
       AND BSTNK IN S_BSTNK2
       AND ERNAM IN S_ERNAM2
       AND AUDAT IN S_AUDAT2 .
    " PO取值
    IF GT_VBAK IS NOT INITIAL .
      IF S_PSPNR1[] IS NOT INITIAL .
        SELECT A~EBELN
               A~BUKRS
               A~EKGRP
               A~RLWRT
          INTO CORRESPONDING FIELDS OF TABLE GT_EKON
          FROM EKKO AS A
         INNER JOIN EKKN AS B
            ON A~EBELN = B~EBELN
           FOR ALL ENTRIES IN GT_VBAK
         WHERE A~LIFNR IN S_LIFNR1
           AND A~EBELN IN S_EBELN1
           AND A~BEDAT IN S_BEDAT1
           AND A~EKGRP IN S_EKGRP1
           AND A~ERNAM IN S_ERNAM1
           AND B~PS_PSP_PNR IN S_PSPNR1
           AND A~EBELN = GT_VBAK-BSTNK+0(10) .
      ELSE .
        SELECT EBELN
               BUKRS
               EKGRP
               RLWRT
          INTO CORRESPONDING FIELDS OF TABLE GT_EKON
          FROM EKKO
           FOR ALL ENTRIES IN GT_VBAK
         WHERE LIFNR IN S_LIFNR1
           AND EBELN IN S_EBELN1
           AND BEDAT IN S_BEDAT1
           AND EKGRP IN S_EKGRP1
           AND ERNAM IN S_ERNAM1
           AND EBELN = GT_VBAK-BSTNK+0(10) .
      ENDIF .
    ENDIF.
  ENDIF.

  " PO 其它表取值
  SORT GT_EKON BY EBELN .
  DELETE ADJACENT DUPLICATES FROM GT_EKON COMPARING EBELN .
  IF GT_EKON IS NOT INITIAL .
    SELECT EBELN
           EBELP
           TXZ01
           MENGE
           MATNR
           MWSKZ
           BRTWR
           NETWR
      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
      FROM EKPO
       FOR ALL ENTRIES IN GT_EKON
     WHERE EBELN = GT_EKON-EBELN .
*       AND EBELP = GT_EKON-EBELP .
    SORT GT_EKPO BY EBELN EBELP .

    SELECT EBELN
           EBELP
           ZEKKN
           MENGE
           DMBTR
           AREWR
           BEWTP
           SHKZG
      INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
      FROM EKBE
       FOR ALL ENTRIES IN GT_EKON
     WHERE EBELN = GT_EKON-EBELN .
*       AND EBELP = GT_EKON-EBELP
*       AND ZEKKN = GT_EKON-ZEKKN .
    SORT GT_EKBE BY EBELN EBELP ZEKKN .
  ELSE .
    MESSAGE '没数据' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE TO SCREEN 9001 .
  ENDIF.

  " SO 其它表取值
  SORT GT_VBAK BY BSTNK .
  IF GT_VBAK IS NOT INITIAL .
    SELECT B~VBELN AS VBELN_VBRP
           B~POSNR AS POSNR_VBRP
           A~VBELN AS VBELN_VBAP
           A~POSNR AS POSNR_VBAP
           A~MATNR
           A~ARKTX
           A~KWMENG
           A~NETWR AS NETWR_VBAP
           A~MWSBP AS MWSBP_VBAP
           B~NETWR AS NETWR_VBRP
           B~MWSBP AS MWSBP_VBRP
      INTO CORRESPONDING FIELDS OF TABLE GT_VRP
      FROM VBAP AS A
      LEFT JOIN VBRP AS B
        ON A~VBELN = B~AUBEL
       AND A~POSNR = B~AUPOS
       FOR ALL ENTRIES IN GT_VBAK
     WHERE A~VBELN = GT_VBAK-VBELN .
    SORT GT_VRP BY VBELN_VBAP .
*    IF GT_VRP IS NOT INITIAL .
    SELECT A~VBELN AS VBELN_VBUP,
           A~POSNR AS POSNR_VBUP,
           B~VBELN AS VBELN_LIPS,
           B~POSNR AS POSNR_LIPS,
           B~VGBEL,
           B~VGPOS,
           B~LFIMG
      INTO CORRESPONDING FIELDS OF TABLE @GT_LIUP
      FROM VBUP AS A
      LEFT JOIN LIPS AS B
        ON A~VBELN = B~VBELN
       AND A~POSNR = B~POSNR
       FOR ALL ENTRIES IN @GT_VBAK
     WHERE B~VGBEL = @GT_VBAK-VBELN
       AND A~WBSTA = 'C' .
    SORT GT_LIUP BY VGBEL VGPOS .
*    ENDIF.
  ELSE .
    MESSAGE '没数据' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE TO SCREEN 9001 .
  ENDIF.

* 将值放进PO+SO抬头
  DATA L_MWSKZ TYPE EKPO-MWSKZ .
  LOOP AT GT_EKON INTO GS_EKON .

* PO 赋值
    GS_POSO1-BUKRS = GS_EKON-BUKRS .
    GS_POSO1-EKGRP = GS_EKON-EKGRP .
    GS_POSO1-EBELN = GS_EKON-EBELN .
    READ TABLE GT_EKPO WITH KEY EBELN = GS_EKON-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_EKPO INTO GS_EKPO FROM SY-TABIX .
        IF GS_EKPO-EBELN = GS_EKON-EBELN .
          GS_POSO1-MENGE_CG = GS_EKPO-MENGE + GS_POSO1-MENGE_CG .
          GS_POSO1-BRTWR = GS_EKPO-BRTWR + GS_POSO1-BRTWR .
          GS_POSO1-RLWRT = GS_POSO1-RLWRT + GS_EKPO-NETWR .
          L_MWSKZ = GS_EKPO-MWSKZ .
          CLEAR GS_EKPO .
        ELSE.
          CLEAR GS_EKPO .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.
*    GS_POSO1-RLWRT = GS_EKON-RLWRT .
    READ TABLE GT_EKBE WITH KEY EBELN = GS_EKON-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_EKBE INTO GS_EKBE FROM SY-TABIX .
        IF GS_EKBE-EBELN = GS_EKON-EBELN .
          IF GS_EKBE-BEWTP = 'E' .
            IF GS_EKBE-SHKZG = 'S'.
              GS_POSO1-DMBTR = GS_EKBE-DMBTR + GS_POSO1-DMBTR .
              GS_POSO1-MENGE_RK = GS_EKBE-MENGE + GS_POSO1-MENGE_RK .
              GS_POSO1-AREWR = GS_EKBE-AREWR + GS_POSO1-AREWR .
            ELSEIF GS_EKBE-SHKZG = 'H'.
              GS_POSO1-DMBTR = GS_POSO1-DMBTR - GS_EKBE-DMBTR .
              GS_POSO1-MENGE_RK = GS_POSO1-MENGE_RK - GS_EKBE-MENGE .
              GS_POSO1-AREWR = GS_POSO1-AREWR - GS_EKBE-AREWR .
            ENDIF.
          ENDIF.
          IF GS_EKBE-SHKZG = 'S'.
            GS_POSO1-AREWR = GS_EKBE-AREWR + GS_POSO1-AREWR .
          ELSEIF GS_EKBE-SHKZG = 'H'.
            GS_POSO1-AREWR = GS_POSO1-AREWR - GS_EKBE-AREWR .
          ENDIF.
          CLEAR GS_EKBE .
        ELSE .
          CLEAR GS_EKBE .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.
    PERFORM FRM_GET_MWSKZ USING L_MWSKZ GS_POSO1-MWSKZ_E .
    GS_POSO1-DMBTR_TAX = GS_POSO1-DMBTR * ( 1 + GS_POSO1-MWSKZ_E ) .
    GS_POSO1-AREWR_TAX = GS_POSO1-AREWR * ( 1 + GS_POSO1-MWSKZ_E ) .
    GS_POSO1-WFPJYJE_TAX = GS_POSO1-BRTWR - GS_POSO1-AREWR_TAX .
    GS_POSO1-WFPJYJE = GS_POSO1-RLWRT - GS_POSO1-AREWR .

* SO 赋值
    READ TABLE GT_VBAK INTO GS_VBAK WITH KEY BSTNK = GS_EKON-EBELN BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_POSO1-VKORG = GS_VBAK-VKORG .
      GS_POSO1-VBELN = GS_VBAK-VBELN .
      GS_POSO1-NETWR = GS_VBAK-NETWR .

      READ TABLE GT_LIUP WITH KEY VGBEL = GS_VBAK-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_LIUP INTO GS_LIUP FROM SY-TABIX .
          IF GS_LIUP-VGBEL = GS_VBAK-VBELN .
            GS_POSO1-LFIMG = GS_POSO1-LFIMG + GS_LIUP-LFIMG .
            CLEAR GS_LIUP .
          ELSE .
            CLEAR GS_LIUP .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.

      READ TABLE GT_VRP WITH KEY VBELN_VBAP = GS_VBAK-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_VRP INTO GS_VRP FROM SY-TABIX .
          IF GS_VRP-VBELN_VBAP = GS_VBAK-VBELN .
            GS_POSO1-KWMENG = GS_POSO1-KWMENG + GS_VRP-KWMENG .
            GS_POSO1-DDZE = GS_POSO1-DDZE + GS_VRP-NETWR_VBAP + GS_VRP-MWSBP_VBAP .
            GS_POSO1-FHJE_TAX = GS_POSO1-DDZE / GS_VRP-KWMENG * GS_POSO1-LFIMG .
            GS_POSO1-FHJE = GS_VRP-NETWR_VBAP / GS_VRP-KWMENG * GS_POSO1-LFIMG .
            GS_POSO1-KPJE_TAX = GS_POSO1-KPJE_TAX + GS_VRP-NETWR_VBRP + GS_VRP-MWSBP_VBRP .
            GS_POSO1-KPJE = GS_POSO1-KPJE + GS_VRP-NETWR_VBRP .
            CLEAR GS_VRP .
          ELSE .
            CLEAR GS_VRP .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.

      GS_POSO1-WKPJE_TAX = GS_POSO1-DDZE - GS_POSO1-KPJE_TAX .
      GS_POSO1-WKPJE = GS_POSO1-NETWR - GS_POSO1-KPJE .

      CLEAR GS_VBAK .
    ELSE .
      CLEAR L_MWSKZ .
      CLEAR GS_POSO1 .
      CLEAR GS_EKON .
      CONTINUE .
    ENDIF.

    APPEND GS_POSO1 TO GT_POSO1 .
    CLEAR GS_POSO1 .

    CLEAR L_MWSKZ .
    CLEAR GS_EKON .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER1 .
  CREATE OBJECT GR_CONTAINER1
    EXPORTING
      CONTAINER_NAME = 'CONTAINER1'.
*      LIFETIME       = CL_GUI_CUSTOM_CONTAINER=>LIFETIME_IMODE.

  CREATE OBJECT GR_ALVGRID1
    EXPORTING
      I_APPL_EVENTS = 'X'
      I_PARENT      = GR_CONTAINER1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REFRESH_ALV1 .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GR_ALVGRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
*     i_soft_refresh =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER2 .
  CREATE OBJECT GR_CONTAINER2
    EXPORTING
      CONTAINER_NAME = 'CONTAINER2'.
*      LIFETIME       = CL_GUI_CUSTOM_CONTAINER=>LIFETIME_IMODE.

  CREATE OBJECT GR_ALVGRID2
    EXPORTING
      I_APPL_EVENTS = 'X'
      I_PARENT      = GR_CONTAINER2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REFRESH_ALV2 .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GR_ALVGRID2->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
*     i_soft_refresh =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER3 .
  CREATE OBJECT GR_CONTAINER3
    EXPORTING
      CONTAINER_NAME = 'CONTAINER3'.
*      LIFETIME       = CL_GUI_CUSTOM_CONTAINER=>LIFETIME_IMODE.

  CREATE OBJECT GR_ALVGRID3
    EXPORTING
      I_APPL_EVENTS = 'X'
      I_PARENT      = GR_CONTAINER3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REFRESH_ALV3 .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GR_ALVGRID3->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
*     i_soft_refresh =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY1 .
  PERFORM INIT_OO_LAYOUT1.              "设置输出格式
*  PERFORM INIT_SORT.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'.             "设置变式控制
  PERFORM FRM_INIT_LVC1.             " 初始化内表结构/ALV显示结构
*  PERFORM EXCLUDE_TB_FUNCTIONS1 TABLES GT_OO_EXCLUDE1.
  PERFORM FRM_OO_BUILD_EVENT1.

  IF MYTAB-ACTIVETAB = 'BUT1'.
    PERFORM FRM_OO_OUTPUT1 TABLES GT_LVC1              "输出
                                 GT_SORT1
                                 GT_OO_EXCLUDE1
                                 GT_POSO1
                          USING GS_LAYOUT1
                                GS_VARIANT1.

  ELSEIF MYTAB-ACTIVETAB = 'BUT2'.
    PERFORM FRM_OO_OUTPUT1 TABLES GT_LVC1              "输出
                                     GT_SORT1
                                     GT_OO_EXCLUDE1
                                     GT_POPS2
                              USING GS_LAYOUT1
                                    GS_VARIANT1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY2 .
  PERFORM INIT_OO_LAYOUT2.              "设置输出格式
*  PERFORM INIT_SORT.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'.             "设置变式控制
  PERFORM FRM_INIT_LVC2.             " 初始化内表结构/ALV显示结构
*  PERFORM EXCLUDE_TB_FUNCTIONS1 TABLES GT_OO_EXCLUDE1.
*  PERFORM FRM_OO_BUILD_EVENT1.
  IF MYTAB-ACTIVETAB = 'BUT1'.
    PERFORM FRM_OO_OUTPUT2 TABLES GT_LVC2              "输出
                                 GT_SORT2
                                 GT_OO_EXCLUDE2
                                 GT_PO1
                          USING GS_LAYOUT2
                                GS_VARIANT2 .
  ELSEIF MYTAB-ACTIVETAB = 'BUT2'.
    PERFORM FRM_OO_OUTPUT2 TABLES GT_LVC2              "输出
                                   GT_SORT2
                                   GT_OO_EXCLUDE2
                                   GT_PO2
                            USING GS_LAYOUT2
                                  GS_VARIANT2 .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY3 .
  PERFORM INIT_OO_LAYOUT3.              "设置输出格式
*  PERFORM INIT_SORT.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'.             "设置变式控制
  PERFORM FRM_INIT_LVC3.             " 初始化内表结构/ALV显示结构
*  PERFORM EXCLUDE_TB_FUNCTIONS1 TABLES GT_OO_EXCLUDE1.
*  PERFORM FRM_OO_BUILD_EVENT1.
  IF MYTAB-ACTIVETAB = 'BUT1' .
    PERFORM FRM_OO_OUTPUT3 TABLES GT_LVC3              "输出
                                 GT_SORT3
                                 GT_OO_EXCLUDE3
                                 GT_SO1
                          USING GS_LAYOUT3
                                GS_VARIANT3 .
  ELSEIF MYTAB-ACTIVETAB = 'BUT2' .
    PERFORM FRM_OO_OUTPUT3 TABLES GT_LVC3              "输出
                                     GT_SORT3
                                     GT_OO_EXCLUDE3
                                     GT_PS2
                              USING GS_LAYOUT3
                                    GS_VARIANT3 .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  SAVE_OK = OK_CODE .
  CLEAR OK_CODE .

  CASE SAVE_OK .
    WHEN 'BACK' OR 'CANCEL'.
      PERFORM FRM_GLOBAL_REFRESH .  "全局参数刷新
      PERFORM FRM_ALV_REFRESH .     "ALV参数刷新
      LEAVE TO SCREEN 0 .
*    WHEN '%_GC 189 3' . "双击抬头某行
*      PERFORM FRM_SET_DATA_INTO_POSO .
*      PERFORM FRM_REFRESH_ALV2 .
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_OO_LAYOUT1 .
  GS_LAYOUT1-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  GS_LAYOUT1-SEL_MODE = 'A'.
  GS_LAYOUT1-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  GS_LAYOUT1-STYLEFNAME = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC1 .
  IF MYTAB-ACTIVETAB = 'BUT1'.
    " PO
    INIT_FIELDCAT1 'BUKRS' '采购方' '' '' '' 'EKKO' 'BUKRS' 15.
    INIT_FIELDCAT1 'EKGRP' '采购组' '' '' '' 'EKKO' 'EKGRP' 15.
    INIT_FIELDCAT1 'EBELN' '采购订单' '' '' '' 'EKKO' 'EBELN' 15.
    INIT_FIELDCAT1 'MENGE_CG' '采购数量' '' '' '' 'EKPO' 'MENGE' 15.
    INIT_FIELDCAT1 'BRTWR' '订单总价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'RLWRT' '订单总净价' '' '' '' 'EKKO' 'RLWRT' 15.
    INIT_FIELDCAT1 'MENGE_RK' '入库数量' '' '' '' 'EKBE' 'MENGE' 15.
    INIT_FIELDCAT1 'DMBTR' '入库金额-净价' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT1 'DMBTR_TAX' '入库金额-含税' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT1 'AREWR' '已发票校验金额-净价' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT1 'AREWR_TAX' '已发票校验金额-含税' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT1 'WFPJYJE' '未发票校验金额-净价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'WFPJYJE_TAX' '未发票校验金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'MWSKZ_E' '税码' '' '' '' 'EKPO' 'MWSKZ' 10.
    " SO
    INIT_FIELDCAT1 'VKORG' '销售方' '' '' '' 'VBAK' 'VKORG' 15.
    INIT_FIELDCAT1 'VBELN' '销售订单' '' '' '' 'VBAK' 'VBELN' 15.
    INIT_FIELDCAT1 'KWMENG' '销售数量' '' '' '' 'VBAP' 'KWMENG' 15.
    INIT_FIELDCAT1 'NETWR' '订单总额净价' '' '' '' 'VBAK' 'NETWR' 15.
    INIT_FIELDCAT1 'DDZE' '订单总额' '' '' '' 'VBAK' 'DDZE' 15.
    INIT_FIELDCAT1 'LFIMG' '发货数量' '' '' '' 'LIPS' 'LFIMG' 15.
    INIT_FIELDCAT1 'FHJE_TAX' '发货金额-含税' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT1 'FHJE' '发货金额-净价' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT1 'KPJE_TAX' '开票金额-含税' '' '' '' 'VBRP' 'NETWR' 15.
    INIT_FIELDCAT1 'KPJE' '开票金额-净价' '' '' '' 'VBRP' 'NETWR' 15.
    INIT_FIELDCAT1 'WKPJE_TAX' '未开票金额-含税' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT1 'WKPJE' '未开票金额-净价' '' '' '' 'VBAP' 'NETWR' 15.
*    INIT_FIELDCAT1 'MWSKZ_V' '税码' '' '' '' 'VBAP' 'MWSKZ' 15.

  ELSEIF MYTAB-ACTIVETAB = 'BUT2'.
    " PO
    INIT_FIELDCAT1 'BUKRS' '采购方' '' '' '' 'EKKO' 'BUKRS' 15.
    INIT_FIELDCAT1 'EKGRP' '采购组' '' '' '' 'EKKO' 'EKGRP' 15.
    INIT_FIELDCAT1 'EBELN' '采购订单' '' '' '' 'EKKO' 'EBELN' 15.
    INIT_FIELDCAT1 'MENGE_CG' '采购数量' '' '' '' 'EKPO' 'MENGE' 15.
    INIT_FIELDCAT1 'BRTWR' '订单总价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'RLWRT' '订单总净价' '' '' '' 'EKKO' 'RLWRT' 15.
    INIT_FIELDCAT1 'MENGE_RK' '入库数量' '' '' '' 'EKBE' 'MENGE' 15.
    INIT_FIELDCAT1 'DMBTR' '入库金额-净价' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT1 'DMBTR_TAX' '入库金额-含税' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT1 'AREWR' '已发票校验金额-净价' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT1 'AREWR_TAX' '已发票校验金额-含税' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT1 'WFPJYJE' '未发票校验金额-净价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'WFPJYJE_TAX' '未发票校验金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT1 'MWSKZ' '税码' '' '' '' 'EKPO' 'MWSKZ' 15.
    " PS
    INIT_FIELDCAT1 'PBUKR' '销售方' '' '' '' 'PRPS' 'PBUKR' 15.
    INIT_FIELDCAT1 'POSID' '销售订单' '' '' '' 'PRPS' 'POSID' 15.
    INIT_FIELDCAT1 'BDMNG' '销售数量' '' '' '' 'RESB' 'BDMNG' 15.
    INIT_FIELDCAT1 'DDZE' '订单总额净价' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT1 'DDZE_TAX' '订单总额' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT1 'MENGE' '发货数量' '' '' '' 'MSEG' 'MENGE' 15.
    INIT_FIELDCAT1 'FHJE_TAX' '发货金额-含税' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT1 'FHJE' '发货金额-净价' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT1 'KPJE_TAX' '开票金额-含税' '' '' '' 'BSEG' 'DMBTR' 15.
    INIT_FIELDCAT1 'KPJE' '开票金额-净价' '' '' '' 'BSEG' 'DMBTR' 15.
    INIT_FIELDCAT1 'WKPJE_TAX' '未开票金额-含税' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT1 'WKPJE' '未开票金额-净价' '' '' '' 'CKIS' 'GPREIS' 15.
*    INIT_FIELDCAT1 'MWSKZ_V' '税码' '' '' '' 'VBAP' 'MWSKZ' 20.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC1  text
*      -->P_GT_SORT1  text
*      -->P_GT_OO_EXCLUDE1  text
*      -->P_GT_ITEM1  text
*      -->P_GS_LAYOUT1  text
*      -->P_GS_VARIANT1  text
*----------------------------------------------------------------------*
FORM FRM_OO_OUTPUT1  TABLES   PT_LVC TYPE LVC_T_FCAT
                             PT_SORT TYPE LVC_T_SORT
                             PT_EXCLUDE TYPE UI_FUNCTIONS
                             PT_DATA
                      USING  PS_LAYOUT TYPE LVC_S_LAYO
                              PS_VARIANT TYPE DISVARIANT.

  CALL METHOD GR_ALVGRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
*     IS_VARIANT                    = PS_VARIANT
      I_SAVE                        = 'A'
*     i_default                     = 'X'
      IS_LAYOUT                     = PS_LAYOUT
*     is_print                      =
*     it_special_groups             =
      IT_TOOLBAR_EXCLUDING          = PT_EXCLUDE[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      IT_OUTTAB                     = PT_DATA[]
      IT_FIELDCATALOG               = PT_LVC[]
      IT_SORT                       = PT_SORT[]
*     it_filter                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_EMPTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_EMPTY .
  IF MYTAB-ACTIVETAB = 'BUT1' .
    IF P_EKORG1 IS INITIAL AND  P_VKORG2 IS INITIAL .
      MESSAGE '采购组织和销售组织必须填一个' TYPE 'E' .
    ENDIF.
  ENDIF.

  IF MYTAB-ACTIVETAB = 'BUT2' .
    IF P_EKORG3 IS INITIAL AND  P_BUKRS4 IS INITIAL .
      MESSAGE '采购组织和销售组织必须填一个' TYPE 'E' .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_OO_LAYOUT2 .
  GS_LAYOUT2-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  GS_LAYOUT2-SEL_MODE = 'A'.
  GS_LAYOUT2-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  GS_LAYOUT2-STYLEFNAME = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC2 .
  IF MYTAB-ACTIVETAB = 'BUT1' .
    " PO
    INIT_FIELDCAT2 'EBELN' '采购订单' '' '' '' 'EKPO' 'EBELN' 15.
    INIT_FIELDCAT2 'EBELP' '行项目' '' '' '' 'EKPO' 'EBELP' 15.
    INIT_FIELDCAT2 'MATNR' '物料' '' '' '' 'EKPO' 'MATNR' 15.
    INIT_FIELDCAT2 'TXZ01' '物料描述' '' '' '' 'EKPO' 'TXZ01' 15.
    INIT_FIELDCAT2 'MENGE_CG' '数量' '' '' '' 'EKPO' 'MENGE' 15.
    INIT_FIELDCAT2 'BRTWR' '金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'RLWRT' '金额-净价值' '' '' '' 'EKKO' 'RLWRT' 15.
    INIT_FIELDCAT2 'MENGE_RK' '入库数量' '' '' '' 'EKBE' 'MENGE' 15.
    INIT_FIELDCAT2 'DMBTR_TAX' '入库金额-含税' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT2 'DMBTR' '入库金额-净价值' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT2 'AREWR' '已发票校验金额-净价' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT2 'AREWR_TAX' '已发票校验金额-含税' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT2 'WFPJYJE' '未发票校验金额-净价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'WFPJYJE_TAX' '未发票校验金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'MWSKZ' '税码' '' '' '' 'EKPO' 'MWSKZ' 15.

  ELSEIF MYTAB-ACTIVETAB = 'BUT2' .
    " PO
    INIT_FIELDCAT2 'EBELN' '采购订单' '' '' '' 'EKPO' 'EBELN' 15.
    INIT_FIELDCAT2 'EBELP' '行项目' '' '' '' 'EKPO' 'EBELP' 15.
    INIT_FIELDCAT2 'MATNR' '物料' '' '' '' 'EKPO' 'MATNR' 15.
    INIT_FIELDCAT2 'TXZ01' '物料描述' '' '' '' 'EKPO' 'TXZ01' 15.
    INIT_FIELDCAT2 'MENGE_CG' '数量' '' '' '' 'EKPO' 'MENGE' 15.
    INIT_FIELDCAT2 'BRTWR' '金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'RLWRT' '金额-净价值' '' '' '' 'EKKO' 'RLWRT' 15.
    INIT_FIELDCAT2 'MENGE_RK' '入库数量' '' '' '' 'EKBE' 'MENGE' 15.
    INIT_FIELDCAT2 'DMBTR_TAX' '入库金额-含税' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT2 'DMBTR' '入库金额-净价值' '' '' '' 'EKBE' 'DMBTR' 15.
    INIT_FIELDCAT2 'AREWR' '已发票校验金额-净价' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT2 'AREWR_TAX' '已发票校验金额-含税' '' '' '' 'EKBE' 'AREWR' 15.
    INIT_FIELDCAT2 'WFPJYJE' '未发票校验金额-净价' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'WFPJYJE_TAX' '未发票校验金额-含税' '' '' '' 'EKPO' 'BRTWR' 15.
    INIT_FIELDCAT2 'MWSKZ' '税码' '' '' '' 'EKPO' 'MWSKZ' 15.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC2  text
*      -->P_GT_SORT2  text
*      -->P_GT_OO_EXCLUDE2  text
*      -->P_GT_HEAD2  text
*      -->P_GS_LAYOUT2  text
*      -->P_GS_VARIANT2  text
*----------------------------------------------------------------------*
FORM FRM_OO_OUTPUT2  TABLES   PT_LVC TYPE LVC_T_FCAT
                             PT_SORT TYPE LVC_T_SORT
                             PT_EXCLUDE TYPE UI_FUNCTIONS
                             PT_DATA
                      USING  PS_LAYOUT TYPE LVC_S_LAYO
                              PS_VARIANT TYPE DISVARIANT.

  CALL METHOD GR_ALVGRID2->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
*     IS_VARIANT                    = PS_VARIANT
      I_SAVE                        = 'A'
*     i_default                     = 'X'
      IS_LAYOUT                     = PS_LAYOUT
*     is_print                      =
*     it_special_groups             =
      IT_TOOLBAR_EXCLUDING          = PT_EXCLUDE[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      IT_OUTTAB                     = PT_DATA[]
      IT_FIELDCATALOG               = PT_LVC[]
      IT_SORT                       = PT_SORT[]
*     it_filter                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_OO_LAYOUT3 .
  GS_LAYOUT3-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  GS_LAYOUT3-SEL_MODE = 'A'.
  GS_LAYOUT3-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  GS_LAYOUT3-STYLEFNAME = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC3 .
  IF MYTAB-ACTIVETAB = 'BUT1' .
    " SO
    INIT_FIELDCAT3 'VBELN' '销售订单' '' '' '' 'VBAP' 'VBELN' 15.
    INIT_FIELDCAT3 'POSNR' '行项目' '' '' '' 'VBAP' 'POSNR' 15.
    INIT_FIELDCAT3 'MATNR' '物料' '' '' '' 'VBAP' 'MATNR' 15.
    INIT_FIELDCAT3 'ARKTX' '物料描述' '' '' '' 'VBAP' 'ARKTX' 15.
    INIT_FIELDCAT3 'KWMENG' '销售数量' '' '' '' 'VBAP' 'KWMENG' 15.
    INIT_FIELDCAT3 'NETWR' '订单总额净价' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT3 'DDZE' '订单总额' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT3 'LFIMG' '发货数量' '' '' '' 'LIPS' 'LFIMG' 15.
    INIT_FIELDCAT3 'FHJE_TAX' '发货金额-含税价' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT3 'FHJE' '发货金额-净价' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT3 'KPJE_TAX' '开票金额-含税价' '' '' '' 'VBRP' 'NETWR' 15.
    INIT_FIELDCAT3 'KPJE' '开票金额-净价' '' '' '' 'VBRP' 'NETWR' 15.
    INIT_FIELDCAT3 'WKPJE_TAX' '未开票金额-含税价' '' '' '' 'VBAP' 'NETWR' 15.
    INIT_FIELDCAT3 'WKPJE' '未开票金额-净价' '' '' '' 'VBAP' 'NETWR' 15.
*    INIT_FIELDCAT3 'MWSKZ' '税码' '' '' '' 'EKPO' 'MWSKZ' 15.

  ELSEIF MYTAB-ACTIVETAB = 'BUT2' .
    " PS
    INIT_FIELDCAT3 'POSID' 'WBS元素' '' '' '' 'PRPS' 'POSID' 15.
    INIT_FIELDCAT3 'POSNR' '行项目' '' '' '' '' '' 15.
    INIT_FIELDCAT3 'MATNR' '物料' '' '' '' 'RESB' 'MATNR' 15.
    INIT_FIELDCAT3 'MAKTX' '物料描述' '' '' '' 'MAKT' 'MAKTX' 15.
    INIT_FIELDCAT3 'BDMNG' '销售数量' '' '' '' 'RESB' 'BDMNG' 15.
    INIT_FIELDCAT3 'XXJE_TAX' '销售金额-含税' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT3 'XXJE' '销售金额-净价' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT3 'MENGE' '发货数量' '' '' '' 'MSEG' 'MENGE' 15.
    INIT_FIELDCAT3 'FHJE' '发货金额-净价' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT3 'FHJE_TAX' '发货金额-含税' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT3 'DMBTR_TAX' '开票金额-含税' '' '' '' 'BSEG' 'DMBTR' 15.
    INIT_FIELDCAT3 'DMBTR' '开票金额-净价' '' '' '' 'BSEG' 'DMBTR' 15.
    INIT_FIELDCAT3 'WKPJE_TAX' '未发票金额-含税价' '' '' '' 'CKIS' 'GPREIS' 15.
    INIT_FIELDCAT3 'WKPJE' '未发票金额-净价' '' '' '' 'CKIS' 'GPREIS' 15.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC3  text
*      -->P_GT_SORT3  text
*      -->P_GT_OO_EXCLUDE3  text
*      -->P_GT_SO1  text
*      -->P_GS_LAYOUT3  text
*      -->P_GS_VARIANT3  text
*----------------------------------------------------------------------*
FORM FRM_OO_OUTPUT3  TABLES   PT_LVC TYPE LVC_T_FCAT
                             PT_SORT TYPE LVC_T_SORT
                             PT_EXCLUDE TYPE UI_FUNCTIONS
                             PT_DATA
                      USING  PS_LAYOUT TYPE LVC_S_LAYO
                              PS_VARIANT TYPE DISVARIANT.

  CALL METHOD GR_ALVGRID3->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
*     IS_VARIANT                    = PS_VARIANT
      I_SAVE                        = 'A'
*     i_default                     = 'X'
      IS_LAYOUT                     = PS_LAYOUT
*     is_print                      =
*     it_special_groups             =
      IT_TOOLBAR_EXCLUDING          = PT_EXCLUDE[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      IT_OUTTAB                     = PT_DATA[]
      IT_FIELDCATALOG               = PT_LVC[]
      IT_SORT                       = PT_SORT[]
*     it_filter                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_MWSKZ
*&---------------------------------------------------------------------*
*       税码转换
*----------------------------------------------------------------------*
*      -->P_MWSKZ_IN  text
*      -->P_MWSKZ_OUT  text
*----------------------------------------------------------------------*
FORM FRM_GET_MWSKZ  USING    P_MWSKZ_IN
                             P_MWSKZ_OUT.
  CASE P_MWSKZ_IN .
    WHEN 'J0' .
      P_MWSKZ_OUT = '0.00' .
    WHEN 'J1' .
      P_MWSKZ_OUT = '0.17' .
    WHEN 'J2' .
      P_MWSKZ_OUT = '0.10' .
    WHEN 'J3' .
      P_MWSKZ_OUT = '0.11' .
    WHEN 'J5' .
      P_MWSKZ_OUT = '0.06' .
    WHEN 'J6' .
      P_MWSKZ_OUT = '0.03' .
    WHEN 'J7' .
      P_MWSKZ_OUT = '0.00' .
    WHEN 'J8' .
      P_MWSKZ_OUT = '0.17' .
    WHEN 'J9' .
      P_MWSKZ_OUT = '0.13' .
    WHEN 'JA' .
      P_MWSKZ_OUT = '0.00' .
    WHEN 'JB' .
      P_MWSKZ_OUT = '0.17' .

    WHEN OTHERS.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_DATA_INTO_POSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_DATA_INTO_POSO USING P_ROWID .
  REFRESH: GT_PO1,GT_SO1 .
  READ TABLE GT_POSO1 INTO GS_POSO1 INDEX P_ROWID .

* PO 赋值
  READ TABLE GT_EKPO WITH KEY EBELN = GS_POSO1-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_EKPO INTO GS_EKPO FROM SY-TABIX .
      IF GS_EKPO-EBELN = GS_POSO1-EBELN .
        GS_PO1-EBELN = GS_EKPO-EBELN .
        GS_PO1-EBELP = GS_EKPO-EBELP .
        GS_PO1-MATNR = GS_EKPO-MATNR .
        GS_PO1-TXZ01 = GS_EKPO-TXZ01 .
        GS_PO1-MENGE_CG = GS_EKPO-MENGE .
        GS_PO1-BRTWR = GS_EKPO-BRTWR .
        GS_PO1-RLWRT = GS_EKPO-NETWR .
        READ TABLE GT_EKBE WITH KEY EBELN = GS_EKPO-EBELN
                                    EBELP = GS_EKPO-EBELP BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_EKBE INTO GS_EKBE FROM SY-TABIX .
            IF GS_EKBE-EBELN = GS_EKPO-EBELN AND
                GS_EKBE-EBELP = GS_EKPO-EBELP .
              IF GS_EKBE-BEWTP = 'E'.
                IF GS_EKBE-SHKZG = 'S'.
                  GS_PO1-MENGE_RK = GS_PO1-MENGE_RK + GS_EKBE-MENGE .
                  GS_PO1-DMBTR = GS_PO1-DMBTR + GS_EKBE-DMBTR .

                ELSEIF GS_EKBE-SHKZG = 'H'.
                  GS_PO1-MENGE_RK = GS_PO1-MENGE_RK - GS_EKBE-MENGE .
                  GS_PO1-DMBTR = GS_PO1-DMBTR - GS_EKBE-DMBTR .
                  GS_PO1-AREWR = GS_PO1-AREWR - GS_EKBE-AREWR .
                ENDIF.
              ENDIF.
              IF GS_EKBE-SHKZG = 'S'.
                GS_PO1-AREWR = GS_PO1-AREWR + GS_EKBE-AREWR .
              ELSEIF GS_EKBE-SHKZG = 'H'.
                GS_PO1-AREWR = GS_PO1-AREWR - GS_EKBE-AREWR .
              ENDIF.
              CLEAR GS_EKBE .
            ELSE .
              CLEAR GS_EKBE .
              EXIT .
            ENDIF.
          ENDLOOP.
        ENDIF.

        GS_PO1-DMBTR_TAX = GS_PO1-DMBTR * ( 1 + GS_POSO1-MWSKZ_E ).
        GS_PO1-AREWR_TAX = GS_PO1-AREWR * ( 1 + GS_POSO1-MWSKZ_E ) .
        GS_PO1-WFPJYJE_TAX = GS_PO1-BRTWR - GS_PO1-AREWR_TAX .
        GS_PO1-WFPJYJE = GS_PO1-RLWRT - GS_PO1-AREWR .
        GS_PO1-MWSKZ = GS_POSO1-MWSKZ_E .

        APPEND GS_PO1 TO GT_PO1 .
        CLEAR GS_PO1 .

        CLEAR GS_EKPO .
      ELSE.
        CLEAR GS_EKPO .
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.

* SO 赋值
  READ TABLE GT_VRP WITH KEY VBELN_VBAP = GS_POSO1-VBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_VRP INTO GS_VRP FROM SY-TABIX .
      IF GS_VRP-VBELN_VBAP = GS_POSO1-VBELN .
        GS_SO1-VBELN = GS_VRP-VBELN_VBAP .
        GS_SO1-POSNR = GS_VRP-POSNR_VBAP .
        GS_SO1-MATNR = GS_VRP-MATNR .
        GS_SO1-ARKTX = GS_VRP-ARKTX .
        GS_SO1-KWMENG = GS_VRP-KWMENG .
        GS_SO1-NETWR = GS_VRP-NETWR_VBAP .
        GS_SO1-DDZE = GS_VRP-NETWR_VBAP + GS_VRP-MWSBP_VBAP .
        READ TABLE GT_LIUP INTO GS_LIUP WITH KEY VBELN_VBUP = GS_VRP-VBELN_VBAP
                                                 POSNR_VBUP = GS_VRP-POSNR_VBAP BINARY SEARCH .
        IF SY-SUBRC = 0 .
          GS_SO1-LFIMG = GS_LIUP-LFIMG .
        ENDIF.
        GS_SO1-FHJE_TAX = ( GS_VRP-NETWR_VBAP + GS_VRP-MWSBP_VBAP ) / GS_VRP-KWMENG * GS_LIUP-LFIMG .
        GS_SO1-FHJE = GS_VRP-NETWR_VBAP / GS_VRP-KWMENG * GS_LIUP-LFIMG .
        GS_SO1-KPJE_TAX = GS_VRP-NETWR_VBRP + GS_VRP-MWSBP_VBRP .
        GS_SO1-KPJE = GS_VRP-NETWR_VBRP .
        GS_SO1-WKPJE_TAX = GS_SO1-DDZE - GS_SO1-KPJE_TAX .
        GS_SO1-WKPJE = GS_SO1-DDZE - GS_SO1-KPJE .

        APPEND GS_SO1 TO GT_SO1 .
        CLEAR GS_SO1 .

        CLEAR GS_LIUP .
        CLEAR GS_VRP .
      ELSE.
        CLEAR GS_VRP .
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    P_E_ROW TYPE LVC_S_ROW
                                   P_E_COLUMN TYPE LVC_S_COL
                                   P_ES_ROW_NO TYPE LVC_S_ROID .
  IF MYTAB-ACTIVETAB = 'BUT1' .
    PERFORM FRM_SET_DATA_INTO_POSO USING P_ES_ROW_NO-ROW_ID .
  ELSEIF MYTAB-ACTIVETAB = 'BUT2' .
    PERFORM FRM_SET_DATA_INTO_POPS USING P_ES_ROW_NO-ROW_ID .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_BUILD_EVENT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OO_BUILD_EVENT1 .
  DATA LR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.
  CREATE OBJECT LR_EVENT_HANDLER.
  SET HANDLER : "LR_EVENT_HANDLER->HANDLE_TOOLBAR              FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_BEFORE_USER_COMMAND  FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_USER_COMMAND         FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_AFTER_USER_COMMAND   FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_ONF4                 FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED         FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED_FINISHED FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK         FOR GR_ALVGRID1 .
*                LR_EVENT_HANDLER->HANDLE_BUTTON_CLICK         FOR GR_ALVGRID.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_SET_DATA_FOR_POPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_SET_DATA_FOR_POPS .
******************************************
* PO+PS 部分
******************************************
  " POPS 主表取值
  IF P_EKORG3 IS NOT INITIAL .
    " PO 取值
    IF S_PSPNR3[] IS NOT INITIAL .
      SELECT A~EBELN
             A~BUKRS
             A~EKGRP
             A~RLWRT
        INTO CORRESPONDING FIELDS OF TABLE GT_EKON
        FROM EKKO AS A
       INNER JOIN EKKN AS B
          ON A~EBELN = B~EBELN
       WHERE A~EKORG = P_EKORG3
         AND A~LIFNR IN S_LIFNR3
         AND A~EBELN IN S_EBELN3
         AND A~BEDAT IN S_BEDAT3
         AND A~EKGRP IN S_EKGRP3
         AND A~ERNAM IN S_ERNAM3
         AND B~PS_PSP_PNR IN S_PSPNR3 .
    ELSE .
      SELECT EBELN
             BUKRS
             EKGRP
             RLWRT
        INTO CORRESPONDING FIELDS OF TABLE GT_EKON
        FROM EKKO
       WHERE EKORG = P_EKORG3
         AND LIFNR IN S_LIFNR3
         AND EBELN IN S_EBELN3
         AND BEDAT IN S_BEDAT3
         AND EKGRP IN S_EKGRP3
         AND ERNAM IN S_ERNAM3 .
    ENDIF.
    " PS 取值
    DATA: BEGIN OF LS_EKKO ,
            ZCGDDH TYPE PRPS-ZCGDDH,
          END OF LS_EKKO .
    DATA LT_EKKO LIKE TABLE OF LS_EKKO .
    LOOP AT GT_EKON INTO GS_EKON .
      LS_EKKO-ZCGDDH = GS_EKON-EBELN .
      APPEND LS_EKKO TO LT_EKKO .
      CLEAR LS_EKKO .
      CLEAR GS_EKON .
    ENDLOOP.
    SORT LT_EKKO BY ZCGDDH .
    DELETE ADJACENT DUPLICATES FROM LT_EKKO COMPARING ZCGDDH .
    IF LT_EKKO IS NOT INITIAL .
      IF P_BUKRS4 IS INITIAL .
        SELECT PSPNR
               POSID
               OBJNR
               PBUKR
               ZCGDDH
          INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
          FROM PRPS
           FOR ALL ENTRIES IN LT_EKKO
         WHERE ZKHBM IN S_KUNNR4
           AND POSID IN S_POSID4
           AND ZCGDDH IN S_EBELN4
           AND ERNAM IN S_ERNAM4
           AND ERDAT IN S_ERDAT4
           AND ZCGDDH = LT_EKKO-ZCGDDH .
      ELSE .
        SELECT PSPNR
               POSID
               OBJNR
               PBUKR
               ZCGDDH
          INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
          FROM PRPS
           FOR ALL ENTRIES IN LT_EKKO
         WHERE PBUKR = P_BUKRS4
           AND ZKHBM IN S_KUNNR4
           AND POSID IN S_POSID4
           AND ZCGDDH IN S_EBELN4
           AND ERNAM IN S_ERNAM4
           AND ERDAT IN S_ERDAT4
           AND ZCGDDH = LT_EKKO-ZCGDDH .
      ENDIF.
    ENDIF.
    REFRESH LT_EKKO .
  ELSEIF P_BUKRS4 IS NOT INITIAL .
    " PS 取值
    SELECT PSPNR
           POSID
           OBJNR
           PBUKR
           ZCGDDH
      INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
      FROM PRPS
     WHERE PBUKR = P_BUKRS4
       AND ZKHBM IN S_KUNNR4
       AND POSID IN S_POSID4
       AND ZCGDDH IN S_EBELN4
       AND ERNAM IN S_ERNAM4
       AND ERDAT IN S_ERDAT4 .
    " PO 取值
    IF GT_PRPS IS NOT INITIAL .
      IF S_PSPNR3[] IS NOT INITIAL .
        SELECT A~EBELN
               A~BUKRS
               A~EKGRP
               A~RLWRT
          INTO CORRESPONDING FIELDS OF TABLE GT_EKON
          FROM EKKO AS A
         INNER JOIN EKKN AS B
            ON A~EBELN = B~EBELN
           FOR ALL ENTRIES IN GT_PRPS
         WHERE A~LIFNR IN S_LIFNR1
           AND A~EBELN IN S_EBELN1
           AND A~BEDAT IN S_BEDAT1
           AND A~EKGRP IN S_EKGRP1
           AND A~ERNAM IN S_ERNAM1
           AND B~PS_PSP_PNR IN S_PSPNR1
           AND A~EBELN = GT_PRPS-ZCGDDH+0(10) .
      ELSE .
        SELECT EBELN
               BUKRS
               EKGRP
               RLWRT
          INTO CORRESPONDING FIELDS OF TABLE GT_EKON
          FROM EKKO
           FOR ALL ENTRIES IN GT_PRPS
         WHERE LIFNR IN S_LIFNR1
           AND EBELN IN S_EBELN1
           AND BEDAT IN S_BEDAT1
           AND EKGRP IN S_EKGRP1
           AND ERNAM IN S_ERNAM1
           AND EBELN = GT_PRPS-ZCGDDH+0(10) .
      ENDIF .
    ENDIF.
  ENDIF.

  " PO 其它表取值
  SORT GT_EKON BY EBELN  .
  DELETE ADJACENT DUPLICATES FROM GT_EKON COMPARING EBELN .
  IF GT_EKON IS NOT INITIAL .
    SELECT EBELN
           EBELP
           TXZ01
           MENGE
           MATNR
           MWSKZ
           BRTWR
           NETWR
      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
      FROM EKPO
       FOR ALL ENTRIES IN GT_EKON
     WHERE EBELN = GT_EKON-EBELN .
*       AND EBELP = GT_EKON-EBELP .
    SORT GT_EKPO BY EBELN EBELP .

    SELECT EBELN
           EBELP
           ZEKKN
           MENGE
           DMBTR
           AREWR
           BEWTP
           SHKZG
      INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
      FROM EKBE
       FOR ALL ENTRIES IN GT_EKON
     WHERE EBELN = GT_EKON-EBELN .
*       AND EBELP = GT_EKON-EBELP
*       AND ZEKKN = GT_EKON-ZEKKN .
    SORT GT_EKBE BY EBELN EBELP ZEKKN .
  ELSE .
    MESSAGE '没数据' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE TO SCREEN 9001 .
  ENDIF.

  " PS 其它表取值
  SORT GT_PRPS BY ZCGDDH .
  IF GT_PRPS IS NOT INITIAL .
    SELECT A~SUBNR
           A~KALNR
           B~MATNR
           B~GPREIS
      INTO CORRESPONDING FIELDS OF TABLE GT_PRCKS
      FROM PRECP2 AS A
     INNER JOIN CKIS AS B
        ON A~KALNR = B~KALNR
       FOR ALL ENTRIES IN GT_PRPS
     WHERE A~SUBNR = GT_PRPS-OBJNR
       AND A~VERSN = '100' .
    SORT GT_PRCKS BY SUBNR MATNR .

    SELECT RSNUM
           RSPOS
           BWART
           MATNR
           BDMNG
           PSPEL
      INTO CORRESPONDING FIELDS OF TABLE GT_RESB
      FROM RESB
       FOR ALL ENTRIES IN GT_PRPS
     WHERE PSPEL = GT_PRPS-PSPNR .
    SORT GT_RESB BY PSPEL MATNR .
    IF GT_RESB IS NOT INITIAL .
      SELECT *
        INTO TABLE GT_MAKT
        FROM MAKT
         FOR ALL ENTRIES IN GT_RESB
       WHERE MATNR = GT_RESB-MATNR
         AND SPRAS = SY-LANGU .
    ENDIF.

    SELECT MBLNR
           MJAHR
           ZEILE
           BWART
           MATNR
           MENGE
           PS_PSP_PNR
      INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
      FROM MSEG
       FOR ALL ENTRIES IN GT_PRPS
     WHERE PS_PSP_PNR = GT_PRPS-PSPNR
       AND BWART IN ('Z27','Z28') .
    SORT GT_MSEG BY PS_PSP_PNR MATNR .

    SELECT BUKRS
           BELNR
           GJAHR
           BUZEI
           MATNR
           MENGE
           PROJK
           SHKZG
           DMBTR
      INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
      FROM BSEG
       FOR ALL ENTRIES IN GT_PRPS
     WHERE PROJK = GT_PRPS-PSPNR
       AND HKONT LIKE '6001%' .
    SORT GT_BSEG BY PROJK MATNR .
  ELSE .
    MESSAGE '没数据' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE TO SCREEN 9001 .
  ENDIF.

* 将值放进PO+PS抬头
  DATA L_MWSKZ TYPE EKPO-MWSKZ .
  LOOP AT GT_EKON INTO GS_EKON .

* PO 赋值
    GS_POPS2-BUKRS = GS_EKON-BUKRS .
    GS_POPS2-EKGRP = GS_EKON-EKGRP .
    GS_POPS2-EBELN = GS_EKON-EBELN .
    READ TABLE GT_EKPO WITH KEY EBELN = GS_EKON-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_EKPO INTO GS_EKPO FROM SY-TABIX .
        IF GS_EKPO-EBELN = GS_EKON-EBELN .
          GS_POPS2-MENGE_CG = GS_EKPO-MENGE + GS_POPS2-MENGE_CG .
          GS_POPS2-BRTWR = GS_EKPO-BRTWR + GS_POPS2-BRTWR .
          GS_POPS2-RLWRT = GS_POPS2-RLWRT + GS_EKPO-NETWR .
          L_MWSKZ = GS_EKPO-MWSKZ .
          CLEAR GS_EKPO .
        ELSE.
          CLEAR GS_EKPO .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.
*    GS_POPS2-RLWRT = GS_EKON-RLWRT .
    READ TABLE GT_EKBE WITH KEY EBELN = GS_EKON-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_EKBE INTO GS_EKBE FROM SY-TABIX .
        IF GS_EKBE-EBELN = GS_EKON-EBELN .
          IF GS_EKBE-BEWTP = 'E' .
            IF GS_EKBE-SHKZG = 'S'.
              GS_POPS2-DMBTR = GS_EKBE-DMBTR + GS_POPS2-DMBTR .
              GS_POPS2-MENGE_RK = GS_EKBE-MENGE + GS_POPS2-MENGE_RK .
              GS_POPS2-AREWR = GS_EKBE-AREWR + GS_POPS2-AREWR .
            ELSEIF GS_EKBE-SHKZG = 'H'.
              GS_POPS2-DMBTR = GS_POPS2-DMBTR - GS_EKBE-DMBTR .
              GS_POPS2-MENGE_RK = GS_POPS2-MENGE_RK - GS_EKBE-MENGE .
              GS_POPS2-AREWR = GS_POPS2-AREWR - GS_EKBE-AREWR .
            ENDIF.
          ENDIF.
          IF GS_EKBE-SHKZG = 'S'.
            GS_POPS2-AREWR = GS_EKBE-AREWR + GS_POPS2-AREWR .
          ELSEIF GS_EKBE-SHKZG = 'H'.
            GS_POPS2-AREWR = GS_POPS2-AREWR - GS_EKBE-AREWR .
          ENDIF.
          CLEAR GS_EKBE .
        ELSE .
          CLEAR GS_EKBE .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.
    PERFORM FRM_GET_MWSKZ USING L_MWSKZ GS_POPS2-MWSKZ .
    GS_POPS2-DMBTR_TAX = GS_POPS2-DMBTR * ( 1 + GS_POPS2-MWSKZ ) .
    GS_POPS2-AREWR_TAX = GS_POPS2-AREWR * ( 1 + GS_POPS2-MWSKZ ) .
    GS_POPS2-WFPJYJE_TAX = GS_POPS2-BRTWR - GS_POPS2-AREWR_TAX .
    GS_POPS2-WFPJYJE = GS_POPS2-RLWRT - GS_POPS2-AREWR .

* PS 赋值
    READ TABLE GT_PRPS INTO GS_PRPS WITH KEY ZCGDDH = GS_EKON-EBELN BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_POPS2-PBUKR = GS_PRPS-PBUKR .  "销售方
      GS_POPS2-POSID = GS_PRPS-POSID .  "WBS元素

      READ TABLE GT_RESB WITH KEY PSPEL = GS_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_RESB INTO GS_RESB FROM SY-TABIX .
          IF GS_RESB-PSPEL = GS_PRPS-PSPNR .
            GS_POPS2-BDMNG = GS_POPS2-BDMNG + GS_RESB-BDMNG . "销售数量
            " 订单总额-净价
            READ TABLE GT_PRCKS INTO GS_PRCKS WITH KEY SUBNR = GS_PRPS-OBJNR
                                                       MATNR = GS_RESB-MATNR BINARY SEARCH .
            IF SY-SUBRC = 0	 .
              GS_POPS2-DDZE = GS_POPS2-DDZE + GS_PRCKS-GPREIS * GS_RESB-BDMNG .
            ENDIF.
            " **&
            IF GS_PRPS-PBUKR = '1700' .
              GS_POPS2-MENGE = GS_POPS2-MENGE + GS_RESB-BDMNG . "发货数量 1700
              GS_POPS2-FHJE = GS_POPS2-FHJE + GS_RESB-BDMNG * GS_PRCKS-GPREIS . "发货金额-净价
            ENDIF.
            CLEAR GS_RESB .
            CLEAR GS_PRCKS .
          ELSE .
            CLEAR GS_RESB .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.

      GS_POPS2-DDZE_TAX = GS_POPS2-DDZE * ( 1 + GS_POPS2-MWSKZ ) .  "订单总额-含税

      READ TABLE GT_MSEG WITH KEY PS_PSP_PNR = GS_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_MSEG INTO GS_MSEG FROM SY-TABIX .
          IF GS_MSEG-PS_PSP_PNR = GS_PRPS-PSPNR .
            "发货数量，非1700
            IF GS_PRPS-PBUKR NE '1700' .
              IF GS_MSEG-BWART = 'Z27'.
                GS_POPS2-MENGE = GS_POPS2-MENGE + GS_MSEG-MENGE .
              ELSEIF GS_MSEG-BWART = 'Z28' .
                GS_POPS2-MENGE = GS_POPS2-MENGE - GS_MSEG-MENGE .
              ENDIF.
            ENDIF.
            " 发货金额-净价
            READ TABLE GT_PRCKS INTO GS_PRCKS WITH KEY SUBNR = GS_PRPS-OBJNR
                                                       MATNR = GS_MSEG-MATNR BINARY SEARCH .
            IF SY-SUBRC = 0	 .
              GS_POPS2-FHJE = GS_POPS2-FHJE + GS_MSEG-MENGE * GS_PRCKS-GPREIS .
            ENDIF.
            CLEAR GS_PRCKS .
            CLEAR GS_MSEG .
          ELSE .
            CLEAR GS_MSEG .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.

      GS_POPS2-FHJE_TAX = GS_POPS2-FHJE * ( 1 + GS_POPS2-MWSKZ ) .  "发货金额-含税

      READ TABLE GT_BSEG WITH KEY PROJK = GS_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT GT_BSEG INTO GS_BSEG FROM SY-TABIX .
          IF GS_BSEG-PROJK = GS_PRPS-PSPNR .
            " 开票金额-净价
            IF GS_BSEG-SHKZG = 'S' .
              GS_POPS2-DMBTR = GS_POPS2-DMBTR + GS_BSEG-DMBTR .
            ELSEIF GS_BSEG-SHKZG = 'H' .
              GS_POPS2-DMBTR = GS_POPS2-DMBTR - GS_BSEG-DMBTR .
            ENDIF.
            CLEAR GS_BSEG .
          ELSE .
            CLEAR GS_BSEG .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.

      GS_POPS2-DMBTR_TAX = GS_POPS2-DMBTR * ( 1 + GS_POPS2-MWSKZ ) .  "开票金额-含税
      GS_POPS2-WKPJE = GS_POPS2-DDZE - GS_POPS2-KPJE .   " 未开票金额-净价
      GS_POPS2-WKPJE_TAX = GS_POPS2-DDZE_TAX - GS_POPS2-KPJE_TAX .  " 未开票金额-含税

      CLEAR GS_PRPS .
    ELSE .
      CLEAR L_MWSKZ .
      CLEAR GS_POPS2 .
      CLEAR GS_EKON .
      CONTINUE .
    ENDIF.

    APPEND GS_POPS2 TO GT_POPS2 .
    CLEAR GS_POPS2 .

    CLEAR L_MWSKZ .
    CLEAR GS_EKON .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_REFRESH .
  REFRESH: GT_LVC1,GT_SORT1,GT_OO_EXCLUDE1,GT_POSO1,GT_POPS2 ,
           GT_LVC2,GT_SORT2,GT_OO_EXCLUDE2,GT_PO1,GT_PO2 ,
           GT_LVC3,GT_SORT3,GT_OO_EXCLUDE3,GT_SO1,GT_PS2 .
  CLEAR: GS_LAYOUT1,GS_VARIANT1 ,
         GS_LAYOUT2,GS_VARIANT2 ,
         GS_LAYOUT3,GS_VARIANT3 .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GLOBAL_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GLOBAL_REFRESH .
  REFRESH: GT_EKON,GT_EKPO,GT_EKBE,GT_VBAK,GT_VRP,GT_LIUP ,
           GT_PRPS,GT_PRCKS,GT_RESB,GT_MSEG,GT_BSEG .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_DATA_INTO_POPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROWID  text
*----------------------------------------------------------------------*
FORM FRM_SET_DATA_INTO_POPS  USING    P_ROWID.
  REFRESH: GT_PO2,GT_PS2 .
  READ TABLE GT_POPS2 INTO GS_POPS2 INDEX P_ROWID .

  SORT GT_PRPS BY POSID .

* PO 赋值
  READ TABLE GT_EKPO WITH KEY EBELN = GS_POPS2-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_EKPO INTO GS_EKPO FROM SY-TABIX .
      IF GS_EKPO-EBELN = GS_POPS2-EBELN .
        GS_PO2-EBELN = GS_EKPO-EBELN .
        GS_PO2-EBELP = GS_EKPO-EBELP .
        GS_PO2-MATNR = GS_EKPO-MATNR .
        GS_PO2-TXZ01 = GS_EKPO-TXZ01 .
        GS_PO2-MENGE_CG = GS_EKPO-MENGE .
        GS_PO2-BRTWR = GS_EKPO-BRTWR .
        GS_PO2-RLWRT = GS_EKPO-NETWR .
        READ TABLE GT_EKBE WITH KEY EBELN = GS_EKPO-EBELN
                                    EBELP = GS_EKPO-EBELP BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_EKBE INTO GS_EKBE FROM SY-TABIX .
            IF GS_EKBE-EBELN = GS_EKPO-EBELN AND
                GS_EKBE-EBELP = GS_EKPO-EBELP .
              IF GS_EKBE-BEWTP = 'E'.
                IF GS_EKBE-SHKZG = 'S'.
                  GS_PO2-MENGE_RK = GS_PO2-MENGE_RK + GS_EKBE-MENGE .
                  GS_PO2-DMBTR = GS_PO2-DMBTR + GS_EKBE-DMBTR .
                  GS_PO2-AREWR = GS_PO2-AREWR + GS_EKBE-AREWR .
                ELSEIF GS_EKBE-SHKZG = 'H'.
                  GS_PO2-MENGE_RK = GS_PO2-MENGE_RK - GS_EKBE-MENGE .
                  GS_PO2-DMBTR = GS_PO2-DMBTR - GS_EKBE-DMBTR .
                  GS_PO2-AREWR = GS_PO2-AREWR - GS_EKBE-AREWR .
                ENDIF.
              ENDIF.
              IF GS_EKBE-SHKZG = 'S'.
                GS_PO2-AREWR = GS_PO2-AREWR + GS_EKBE-AREWR .
              ELSEIF GS_EKBE-SHKZG = 'H'.
                GS_PO2-AREWR = GS_PO2-AREWR - GS_EKBE-AREWR .
              ENDIF.
              CLEAR GS_EKBE .
            ELSE .
              CLEAR GS_EKBE .
              EXIT .
            ENDIF.
          ENDLOOP.
        ENDIF.
        GS_PO2-DMBTR_TAX = GS_PO2-DMBTR * ( 1 + GS_POPS2-MWSKZ ).
        GS_PO2-AREWR_TAX = GS_PO2-AREWR * ( 1 + GS_POPS2-MWSKZ ) .
        GS_PO2-WFPJYJE_TAX = GS_PO2-BRTWR - GS_PO2-AREWR_TAX .
        GS_PO2-WFPJYJE = GS_PO2-RLWRT - GS_PO2-AREWR .
        GS_PO2-MWSKZ = GS_POPS2-MWSKZ .

        APPEND GS_PO2 TO GT_PO2 .
        CLEAR GS_PO2 .

        CLEAR GS_EKPO .
      ELSE.
        CLEAR GS_EKPO .
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.

* PS 赋值
  READ TABLE GT_PRPS WITH KEY POSID = GS_POPS2-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT GT_PRPS INTO GS_PRPS FROM SY-TABIX .
      IF GS_PRPS-POSID = GS_POPS2-POSID .
        READ TABLE GT_RESB WITH KEY PSPEL = GS_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LOOP AT GT_RESB INTO GS_RESB FROM SY-TABIX .
            IF GS_RESB-PSPEL = GS_PRPS-PSPNR .
              GS_PS2-POSID = GS_PRPS-POSID .
              GS_PS2-MATNR = GS_RESB-MATNR .
              READ TABLE GT_MAKT INTO GS_MAKT WITH KEY MATNR = GS_RESB-MATNR BINARY SEARCH .
              IF SY-SUBRC = 0 .
                GS_PS2-MAKTX = GS_MAKT-MAKTX .
                CLEAR GS_MAKT .
              ENDIF.
              GS_PS2-BDMNG = GS_RESB-BDMNG .
              IF GS_PRPS-PBUKR = '1700' .
                GS_PS2-MENGE = GS_RESB-BDMNG ."发货数量
              ELSE .
                READ TABLE GT_MSEG WITH KEY PS_PSP_PNR = GS_PRPS-PSPNR
                                                 MATNR = GS_RESB-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
                IF SY-SUBRC = 0 .
                  LOOP AT GT_MSEG INTO GS_MSEG FROM SY-TABIX .
                    IF GS_MSEG-PS_PSP_PNR = GS_PRPS-PSPNR AND
                       GS_MSEG-MATNR = GS_RESB-MATNR .
                      IF GS_MSEG-BWART = 'S' .
                        GS_PS2-MENGE = GS_PS2-MENGE + GS_MSEG-MENGE .
                      ELSEIF GS_MSEG-BWART = 'H' .
                        GS_PS2-MENGE = GS_PS2-MENGE - GS_MSEG-MENGE .
                      ENDIF.

                      CLEAR GS_MSEG .
                    ELSE .
                      CLEAR GS_MSEG .
                      EXIT .
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
              READ TABLE GT_PRCKS INTO GS_PRCKS WITH KEY SUBNR = GS_PRPS-OBJNR
                                                         MATNR = GS_RESB-MATNR BINARY SEARCH .
              IF SY-SUBRC = 0 .
                GS_PS2-XXJE = GS_PRCKS-GPREIS .
                GS_PS2-XXJE_TAX = GS_PS2-XXJE * ( 1 + GS_POPS2-MWSKZ ) .
              ENDIF.
              GS_PS2-FHJE = GS_PS2-MENGE * GS_PRCKS-GPREIS .
              GS_PS2-FHJE_TAX = GS_PS2-FHJE * ( 1 + GS_POPS2-MWSKZ ) .
              READ TABLE GT_BSEG WITH KEY PROJK = GS_PRPS-PSPNR
                                          MATNR = GS_RESB-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
              IF SY-SUBRC = 0 .
                LOOP AT GT_BSEG INTO GS_BSEG FROM SY-TABIX .
                  IF GS_BSEG-PROJK = GS_PRPS-PSPNR AND
                     GS_BSEG-MATNR = GS_RESB-MATNR .
                    IF GS_BSEG-SHKZG = 'S' .
                      GS_PS2-DMBTR = GS_PS2-DMBTR + GS_BSEG-DMBTR .
                    ELSEIF GS_BSEG-SHKZG = 'H' .
                      GS_PS2-DMBTR = GS_PS2-DMBTR - GS_BSEG-DMBTR .
                    ENDIF.

                    CLEAR GS_BSEG .
                  ELSE .
                    CLEAR GS_BSEG .
                    EXIT .
                  ENDIF.
                ENDLOOP.
              ENDIF.
              GS_PS2-DMBTR_TAX = GS_PS2-DMBTR * ( 1 + GS_POPS2-MWSKZ ) .
              GS_PS2-WKPJE = GS_PS2-XXJE - GS_PS2-DMBTR .
              GS_PS2-WKPJE_TAX   = GS_PS2-WKPJE * ( 1 + GS_POPS2-MWSKZ ) .

              APPEND GS_PS2 TO GT_PS2 .
              CLEAR GS_PS2 .

              CLEAR GS_PRCKS .
              CLEAR GS_RESB .
            ELSE.
              CLEAR GS_RESB .
              EXIT .
            ENDIF.
          ENDLOOP.
        ENDIF.

        CLEAR GS_PRPS .
      ELSE.
        CLEAR GS_PRPS .
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
