*&---------------------------------------------------------------------*
*& Report  ZPS006
*&
*&---------------------------------------------------------------------*
*& Create by     : 吴丽娟 （hand)
*& Create date   : 2015/09/14
*& Descriptions  : 项目财务信息表
*&---------------------------------------------------------------------*
REPORT zps007_copy.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*  TABLES                                                               *
*----------------------------------------------------------------------*
TABLES:proj,prps,t001,ckhs.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tab,
         pspnr      TYPE proj-pspnr, "项目编号
         pspid      TYPE proj-pspid,
         post1      TYPE proj-post1, "项目名称
         nd         TYPE int4, "年度
         qj         TYPE int2, "期间
         zkhbm      TYPE proj-zkhbm, "客户编号
         name1      TYPE kna1-name1, "客户描述
         zhtje      TYPE p LENGTH 16 DECIMALS 2, "合同金额
         objnr      TYPE j_objnr,                  "项目号
         xmyscbgc   TYPE p LENGTH 16 DECIMALS 2, "项目预算成本-工程成本
         xmyscbgc2  TYPE p LENGTH 16 DECIMALS 14, "项目预算成本-工程成本
         xmyscbqj   TYPE p LENGTH 16 DECIMALS 2, "项目预算成本-期间费用
         xmyscbqj2  TYPE p LENGTH 16 DECIMALS 14, "项目预算成本-期间费用2
         xmcbgc     TYPE p LENGTH 16 DECIMALS 14, "项目成本-工程成本
         xmcbgc2    TYPE p LENGTH 16 DECIMALS 2, "项目成本-工程成本
         xmcbqj     TYPE p LENGTH 16 DECIMALS 2, "项目成本-期间费用
         xmsr       TYPE p LENGTH 16 DECIMALS 2, "项目收入
         xmsr3      TYPE p LENGTH 16 DECIMALS 14, "项目收入
         ljkps      TYPE p LENGTH 16 DECIMALS 2, "累计开票数（应收）
         xmwgbfb    TYPE p LENGTH 16 DECIMALS 2, "项目完工百分比
         xmwgbfb2   TYPE p LENGTH 16 DECIMALS 14, "项目完工百分比
         ljskje     TYPE p LENGTH 16 DECIMALS 2, "累计收款金额
         ykpwskje   TYPE p LENGTH 16 DECIMALS 2, "已开票未收款金额
         zjgjsje    TYPE p LENGTH 16 DECIMALS 2, "竣工决算金额
         lwhtje     TYPE p LENGTH 16 DECIMALS 2, "劳务合同金额
         lwrgfzc    TYPE p LENGTH 16 DECIMALS 2, "劳务人工费支出
         htmll      TYPE p LENGTH 16 DECIMALS 2, "合同毛利率
         mle        TYPE p LENGTH 16 DECIMALS 2, "毛利额
         mle2       TYPE p LENGTH 16 DECIMALS 14, "毛利额
         xmyscbbggc TYPE p LENGTH 16 DECIMALS 2, "项目预算成本（变更）-工程成本
         xmyscbbgqj TYPE p LENGTH 16 DECIMALS 2, "项目预算成本（变更）-期间费用
         xmcb2gc    TYPE p LENGTH 16 DECIMALS 14, "项目成本-工程成本
         xmcb2gc2   TYPE p LENGTH 16 DECIMALS 2, "项目成本-工程成本
         xmcb2qj    TYPE p LENGTH 16 DECIMALS 2, "项目成本-期间费用
         xmsr2      TYPE p LENGTH 16 DECIMALS 2, "项目收入
         dqkps      TYPE p LENGTH 16 DECIMALS 2, "当期开票数（应收）
         dqskje     TYPE p LENGTH 16 DECIMALS 2, "当期收款金额
         zzhtje     TYPE p LENGTH 16 DECIMALS 2,
         wskje      TYPE p LENGTH 16 DECIMALS 2,
         zhtje30    TYPE p LENGTH 16 DECIMALS 2, "取30的合同金额  added by  it02 151127
       END OF ty_tab.
TYPES:BEGIN OF ty_name1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
      END OF ty_name1.
TYPES:BEGIN OF ty_posid,
        posid TYPE prps-posid,
        ebeln TYPE ekpo-ebeln,
        knumv TYPE ekko-knumv,
        kwert TYPE konv-kwert,
        waers TYPE konv-waers,
        kkurs TYPE konv-kkurs,
      END OF ty_posid.
TYPES:BEGIN OF ty_kalnr,
        pspnr TYPE ckis-pspnr,
        kalnr TYPE ckis-kalnr,
        erfdt TYPE ckhs-erfdt,
      END OF ty_kalnr.
TYPES:BEGIN OF ty_hxx,
        pspnr     TYPE ckis-pspnr,
        kalnr     TYPE ckis-kalnr,
        tvers     TYPE ckis-tvers,
        kstar     TYPE ckis-kstar,
        wrtfw_pos TYPE ckis-wrtfw_pos,
      END OF ty_hxx.
TYPES:BEGIN OF ty_cbgsh,
        pspnr TYPE ckis-pspnr,
        kalnr TYPE ckhs-kalnr,
        erfdt TYPE ckhs-erfdt,
      END OF ty_cbgsh.
TYPES:BEGIN OF ty_objnr,
        objnr1 TYPE prps-objnr,
        objnr2 TYPE aufk-objnr,
      END OF ty_objnr.
TYPES:BEGIN OF ty_objnr1,
        objnr TYPE aufk-objnr,
      END OF ty_objnr1.
TYPES:BEGIN OF ty_hsl,
        xmbh    TYPE zfi005-xmbh,
        hsl     TYPE zfi005-hsl,
        postdat TYPE zfi005-postdat,
      END OF ty_hsl.
TYPES:BEGIN OF ty_ljje,
        54011 TYPE ckis-wrtfw_pos,
        81    TYPE ckis-wrtfw_pos,
        54012 TYPE ckis-wrtfw_pos,
        82    TYPE ckis-wrtfw_pos,
      END OF ty_ljje.
TYPES:BEGIN OF ty_sz,
        posid  TYPE prps-posid,
        pspnr  TYPE prps-pspnr,
        objnr  TYPE cosp-objnr,
        gjahr  TYPE cosp-gjahr,
        kstar  TYPE cosp-kstar,
        wrttp  TYPE cosp-wrttp,
        wtg001 TYPE cosp-wtg001,
        wtg002 TYPE cosp-wtg002,
        wtg003 TYPE cosp-wtg003,
        wtg004 TYPE cosp-wtg004,
        wtg005 TYPE cosp-wtg005,
        wtg006 TYPE cosp-wtg006,
        wtg007 TYPE cosp-wtg007,
        wtg008 TYPE cosp-wtg008,
        wtg009 TYPE cosp-wtg009,
        wtg010 TYPE cosp-wtg010,
        wtg011 TYPE cosp-wtg011,
        wtg012 TYPE cosp-wtg012,
        wtg013 TYPE cosp-wtg013,
        wtg014 TYPE cosp-wtg014,
        wtg015 TYPE cosp-wtg015,
        wtg016 TYPE cosp-wtg016,
      END OF ty_sz.
TYPES:BEGIN OF ty_pspnr,
        pspnr TYPE prps-pspnr,
      END OF ty_pspnr.
TYPES:BEGIN OF ty_kalnr2,
        pspnr TYPE ckis-pspnr,
        kalnr TYPE ckis-kalnr,
        versn TYPE precp2-versn,
      END OF ty_kalnr2.
TYPES:BEGIN OF ty_dxbh,
        objnr TYPE prps-objnr,
        pspnr TYPE prps-pspnr,
      END OF ty_dxbh.
TYPES:BEGIN OF ty_posid1,
        posid TYPE prps-posid,
        objnr TYPE prps-objnr,
      END OF ty_posid1.
TYPES:BEGIN OF ty_aufk,
        pspel TYPE aufk-pspel,
        objnr TYPE aufk-objnr,
      END OF ty_aufk.
TYPES:BEGIN OF ty_zfi017,
        ebeln TYPE zfi017-ebeln,
      END OF ty_zfi017.
TYPES:BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
      END OF ty_ekpo.
TYPES:BEGIN OF ty_zfi0172,
        posid   TYPE zfi017-posid,
        belnr_f TYPE zfi017-belnr_f,
        zsqfkje TYPE zfi017-zsqfkje,
      END OF ty_zfi0172.
TYPES:BEGIN OF ty_bkpf,
        posid   TYPE zfi017-posid,
        zsqfkje TYPE zfi017-zsqfkje,
      END OF ty_bkpf.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_tab TYPE TABLE OF ty_tab,
     wa_tab TYPE ty_tab.
DATA:it_name1 TYPE TABLE OF ty_name1,
     wa_name1 TYPE ty_name1.
DATA:it_posid TYPE TABLE OF ty_posid,
     wa_posid TYPE ty_posid.
DATA:p_date TYPE sy-datum.
DATA:p_date2 TYPE sy-datum.
DATA:p_date1 TYPE sy-datum.
DATA:p_day TYPE n LENGTH 2 VALUE '01'.
DATA:it_kalnr TYPE TABLE OF ty_kalnr,
     wa_kalnr TYPE ty_kalnr.
DATA:it_kalnr1 TYPE TABLE OF ty_kalnr,
     wa_kalnr1 TYPE ty_kalnr.
DATA:it_hxx TYPE TABLE OF ty_hxx,
     wa_hxx TYPE ty_hxx.
DATA:it_hxx1 TYPE TABLE OF ty_hxx,
     wa_hxx1 TYPE ty_hxx.
DATA:it_hxx2 TYPE TABLE OF ty_hxx,
     wa_hxx2 TYPE ty_hxx.
DATA:it_cbgsh TYPE TABLE OF ty_cbgsh,
     wa_cbgsh TYPE ty_cbgsh.
DATA:it_cbgsh1 TYPE TABLE OF ty_cbgsh,
     wa_cbgsh1 TYPE ty_cbgsh.
DATA:it_cbgsh2 TYPE TABLE OF ty_cbgsh,
     wa_cbgsh2 TYPE ty_cbgsh.
DATA:it_objnr TYPE TABLE OF ty_objnr,
     wa_objnr TYPE ty_objnr.
DATA:it_objnr1 TYPE TABLE OF ty_objnr1,
     wa_objnr1 TYPE ty_objnr1.
DATA:it_cosp TYPE TABLE OF zps007,
     wa_cosp TYPE zps007.
DATA:it_ckis1 TYPE TABLE OF ckis,
     wa_ckis1 TYPE ckis.
DATA:it_ckis2 TYPE TABLE OF ckis,
     wa_ckis2 TYPE ckis.
DATA:it_ckis3 TYPE TABLE OF ckis,
     wa_ckis3 TYPE ckis.
DATA:it_ckis4 TYPE TABLE OF ckis,
     wa_ckis4 TYPE ckis.
DATA:it_hsl TYPE TABLE OF ty_hsl,
     wa_hsl TYPE ty_hsl.
DATA:it_ljje TYPE TABLE OF ty_ljje,
     wa_ljje TYPE ty_ljje.
DATA:it_5401 TYPE TABLE OF ty_sz,
     wa_5401 TYPE ty_sz.
DATA:it_8 TYPE TABLE OF ty_sz,
     wa_8 TYPE ty_sz.
DATA:it_6 TYPE TABLE OF ty_sz,
     wa_6 TYPE ty_sz.
DATA:g_psphi(2).
CONSTANTS a TYPE p LENGTH 16 DECIMALS 2 VALUE '1.17'.
DATA:it_zps007 TYPE TABLE OF zps007.
DATA:it_pspnr TYPE TABLE OF ty_pspnr.
DATA:it_kalnr2 TYPE TABLE OF ty_kalnr2,
     wa_kalnr2 TYPE ty_kalnr2.
DATA:it_kalnr3 TYPE TABLE OF ty_kalnr2,
     wa_kalnr3 TYPE ty_kalnr2.
DATA:it_dxbh TYPE TABLE OF ty_dxbh,
     wa_dxbh TYPE ty_dxbh.
DATA:it_posid1 TYPE TABLE OF ty_posid1,
     wa_posid1 TYPE ty_posid1.
DATA:it_aufk TYPE TABLE OF ty_aufk.
DATA:wa_aufk TYPE ty_aufk.
DATA:it_zfi017  TYPE TABLE OF ty_zfi017,
     it_ekpo    TYPE TABLE OF ty_ekpo,
     it_zfi0172 TYPE TABLE OF ty_zfi0172,
     it_bkpf    TYPE TABLE OF ty_bkpf,
     wa_bkpf    TYPE ty_bkpf.
DATA:it_zps007a TYPE TABLE OF ty_tab,
     wa_zps007a TYPE ty_tab.
DATA:it_tab1 TYPE TABLE OF zps007a,
     wa_tab1 TYPE zps007a.

DATA:gt_jcds TYPE TABLE OF jcds,
     gs_jcds TYPE jcds.

DATA: lv_fs_date TYPE sy-datum,    "期间月初
      lv_ls_date TYPE sy-datum.    "期间月末

*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo.
*----------------------------------------------------------------------*
*                  定义宏
*----------------------------------------------------------------------*
DEFINE init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.
DATA:gt_trdir LIKE trdir OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF gt_src OCCURS 1,
        txt(255) TYPE c,
      END OF gt_src.
DATA:g_tname TYPE se16n_tab,
     g_rname TYPE trdir-name.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_xmdy FOR proj-pspid.
PARAMETERS:p_year LIKE sy-datum(4) ,"OBLIGATORY,
           p_mnth LIKE sy-datum+4(2)." OBLIGATORY.
SELECT-OPTIONS:s_gsdm FOR ckhs-bukrs ,"OBLIGATORY,
               s_zkhbm FOR proj-zkhbm .        "客户编码 add by it02 151225
PARAMETERS:p_ex  TYPE c AS CHECKBOX  DEFAULT 'X'.
PARAMETERS:p_name(40).
SELECTION-SCREEN END OF BLOCK text.
DATA:p_mnth4(2) TYPE c.
p_mnth4 = p_mnth + 1.
IF p_mnth4 < 10.
  CONCATENATE '0' p_mnth4 INTO p_mnth4.
ENDIF.
CONCATENATE p_year p_mnth4 p_day INTO p_date." SEPARATED BY '.'.
CONCATENATE p_year p_mnth p_day INTO p_date2." SEPARATED BY '.'.
DATA:p_mnth1(2) TYPE c.
DATA:p_mnth3(2) TYPE c.
p_mnth1 = p_mnth - 1.
DATA:p_mnth2(1) TYPE c VALUE '0'.
CONCATENATE p_mnth2 p_mnth1 INTO p_mnth3.
CONCATENATE p_year p_mnth3 p_day INTO p_date1." SEPARATED BY '.'.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  IF sy-uname NE 'IT02'.
    LOOP AT SCREEN.
      IF screen-name CS 'P_NAME'.
*        screen-output = '0'.
        screen-active = '0'.
*        screen-INVISIBLE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF sy-uname = 'IT02'.
    g_rname = p_name.
    SELECT SINGLE * INTO  gt_trdir FROM trdir WHERE name = g_rname.
    IF sy-subrc = 0.
      READ REPORT g_rname INTO gt_src.

      EDITOR-CALL FOR gt_src.

      IF sy-subrc = 0.

        INSERT REPORT g_rname FROM gt_src.

      ENDIF.

      UPDATE progdir SET unam = gt_trdir-unam "LAST CHANGED
                       udat = gt_trdir-udat "更改日期
                       sdate = gt_trdir-sdate
                       stime = gt_trdir-stime
                       idate = gt_trdir-idate
                       itime = gt_trdir-itime
*                       CNAM = GT_TRDIR-CNAM
                       WHERE name = g_rname.
      IF sy-subrc = 0.
        MESSAGE s001(00) WITH 'Update succeed!' '' '' ''.
      ELSE.
        MESSAGE w001(00) WITH 'Update failed,try it again' '' '' ''.
      ENDIF.
    ELSE.
      MESSAGE i001(00) WITH 'Error Report,please check your input data' '' '' ''.
    ENDIF.
  ELSE.
    PERFORM frm_getdata.
    PERFORM frm_dealdata.
    PERFORM frm_layout.
    PERFORM frm_fieldcat.
    PERFORM frm_output.
  ENDIF.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_getdata .

  TYPES:BEGIN OF ty_bukrs,
          bukrs TYPE ckhs-bukrs,
        END OF ty_bukrs.

  DATA:it_bukrs TYPE TABLE OF ty_bukrs,
       wa_bukrs TYPE ty_bukrs.

  DATA:l_tabix TYPE sy-tabix .
*modify by 夏俊 20170907 --->
*  SELECT bukrs
*    FROM ckhs
*    INTO TABLE it_bukrs.
*
*  DATA:l_gsdm TYPE int4.
*
*  LOOP AT it_bukrs INTO wa_bukrs WHERE bukrs = s_gsdm-low OR bukrs = s_gsdm-high.
*    l_gsdm = l_gsdm + 1.
*  ENDLOOP.
*  IF l_gsdm = 0.
  SELECT COUNT(*)
    FROM ckhs
    WHERE bukrs IN s_gsdm.
  IF sy-subrc NE 0.
*modify by 夏俊 20170907 <---
    MESSAGE: '请输入正确的公司代码！' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
*  CLEAR l_gsdm.
  "取项目编号、项目名称、客户编号、竣工决算金额、合同金额
  SELECT pspnr
         pspid
         post1
         zkhbm
         zjgjsje
         zhtje    AS  zhtje30
         objnr
*         zhtje
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_tab
    WHERE pspid IN s_xmdy
       AND zkhbm IN s_zkhbm.

  IF it_tab IS NOT INITIAL.
    IF p_ex EQ 'X'.
      "查询期间的月初
      CLEAR:lv_fs_date,lv_ls_date.
      CONCATENATE  p_year p_mnth '01' INTO lv_fs_date.
      CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = lv_fs_date
        IMPORTING
          e_date = lv_ls_date.
      "add 增加“排除TECO的项目”  by it02 20160630
      SELECT * INTO TABLE gt_jcds
        FROM jcds
        FOR ALL ENTRIES IN it_tab
        WHERE objnr = it_tab-objnr
        AND   stat = 'I0045'
        AND   inact = ''
        AND   udate <=  lv_ls_date .
      SORT gt_jcds BY  objnr ASCENDING stat  ASCENDING chgnr  DESCENDING .

      DELETE ADJACENT DUPLICATES FROM gt_jcds  COMPARING  objnr  stat.
      IF gt_jcds  IS NOT INITIAL.
        LOOP AT  it_tab INTO wa_tab .
          l_tabix = sy-tabix.
          READ TABLE  gt_jcds INTO gs_jcds WITH KEY objnr = wa_tab-objnr .
          IF sy-subrc EQ 0 .
            DELETE it_tab INDEX l_tabix .
          ELSE.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
    "end

    "取客户描述
    SELECT kunnr name1
      FROM kna1
      INTO CORRESPONDING FIELDS OF TABLE it_name1
      FOR ALL ENTRIES IN it_tab
      WHERE kunnr = it_tab-zkhbm.

    "劳务合同金额
    SELECT posid
           ekpo~ebeln
           ekko~knumv
           konv~kwert
           konv~waers
           konv~kkurs
      FROM prps
      JOIN ekkn ON prps~posid = ekkn~ps_psp_pnr
      JOIN ekpo ON ekkn~ebeln = ekpo~ebeln
      JOIN ekko ON ekpo~ebeln = ekko~ebeln
      JOIN konv ON ekko~knumv = konv~knumv
      INTO CORRESPONDING FIELDS OF TABLE it_posid
      FOR ALL ENTRIES IN it_tab
      WHERE prps~psphi = it_tab-pspnr
      AND ( ekpo~knttp = 'Y' OR ekpo~knttp = 'H' OR ekpo~knttp = 'J')
    AND konv~stunr = 1.

    "取数1
    SELECT pspnr
      FROM prps
      INTO CORRESPONDING FIELDS OF TABLE it_pspnr
      "WHERE prps~psphi IN s_xmdy.
      FOR ALL ENTRIES IN it_tab
      WHERE psphi = it_tab-pspnr.

    IF it_pspnr IS NOT INITIAL.
      SELECT ckis~pspnr ckis~kalnr precp2~versn
        FROM ckis
        JOIN precp2 ON ckis~kalnr = precp2~kalnr
        INTO CORRESPONDING FIELDS OF TABLE it_kalnr2
        FOR ALL ENTRIES IN it_pspnr
        WHERE ckis~pspnr = it_pspnr-pspnr
        AND versn BETWEEN '000' AND '099'.

      SELECT ckis~pspnr ckis~kalnr precp2~versn
        FROM ckis
        JOIN precp2 ON ckis~kalnr = precp2~kalnr
        INTO CORRESPONDING FIELDS OF TABLE it_kalnr3
        FOR ALL ENTRIES IN it_pspnr
        WHERE ckis~pspnr = it_pspnr-pspnr
        AND versn = '000'.

    ENDIF.

    IF it_kalnr2 IS NOT INITIAL.

      IF it_kalnr3 IS NOT INITIAL.

        SELECT ckis~pspnr ckhs~kalnr ckhs~erfdt
          FROM ckhs
          JOIN ckis ON ckhs~kalnr = ckis~kalnr
          INTO CORRESPONDING FIELDS OF TABLE it_kalnr
          FOR ALL ENTRIES IN it_kalnr3
          WHERE ckhs~kalnr = it_kalnr3-kalnr
          AND ckhs~bukrs IN s_gsdm
          AND ckhs~kokrs = 1000
          AND ckhs~erfdt <= p_date.
      ENDIF.

      SORT it_kalnr BY pspnr erfdt kalnr.

      LOOP AT it_kalnr INTO wa_kalnr.
        AT END OF pspnr.
          READ TABLE it_kalnr INTO wa_kalnr INDEX sy-tabix.
          IF sy-subrc = 0.
            wa_kalnr1-pspnr = wa_kalnr-pspnr.
            wa_kalnr1-kalnr = wa_kalnr-kalnr.
            wa_kalnr1-erfdt = wa_kalnr-erfdt.
          ENDIF.
        ENDAT.
        APPEND wa_kalnr1 TO it_kalnr1.
      ENDLOOP.
    ENDIF.

    IF it_kalnr1 IS NOT INITIAL.
      SELECT *
        FROM ckis
        INTO CORRESPONDING FIELDS OF TABLE it_hxx
        FOR ALL ENTRIES IN it_kalnr1
        WHERE kalnr = it_kalnr1-kalnr
        AND ( kstar LIKE '6001%'
        OR kstar LIKE '6051%'
        OR kstar LIKE '6301%').

      SELECT *
        FROM ckis
        INTO CORRESPONDING FIELDS OF TABLE it_hxx1
        FOR ALL ENTRIES IN it_kalnr1
        WHERE kalnr = it_kalnr1-kalnr
        AND ( kstar LIKE '5401%').

      SELECT *
        FROM ckis
        INTO CORRESPONDING FIELDS OF TABLE it_hxx2
        FOR ALL ENTRIES IN it_kalnr1
        WHERE kalnr = it_kalnr1-kalnr
        AND ( kstar LIKE '8%'
        OR kstar LIKE '6603%').

    ENDIF.


    IF it_kalnr IS NOT INITIAL.
      SELECT ckis~pspnr ckhs~erfdt ckhs~kalnr
        FROM ckhs
        JOIN ckis ON ckhs~kalnr = ckis~kalnr
        INTO CORRESPONDING FIELDS OF TABLE it_cbgsh
        FOR ALL ENTRIES IN it_kalnr
        WHERE ckhs~kalnr = it_kalnr-kalnr
        AND ckhs~bukrs IN s_gsdm
        AND ckhs~kokrs = 1000.
    ENDIF.

    "(1)
    SORT it_cbgsh BY pspnr erfdt kalnr.
    LOOP AT it_cbgsh INTO wa_cbgsh WHERE erfdt BETWEEN p_date2 AND p_date.
      AT END OF pspnr.
        READ TABLE it_cbgsh INTO wa_cbgsh INDEX sy-tabix.
        IF sy-subrc = 0.
          wa_cbgsh1-pspnr = wa_cbgsh-pspnr.
          wa_cbgsh1-erfdt = wa_cbgsh-erfdt.
          wa_cbgsh1-kalnr = wa_cbgsh-kalnr.
        ENDIF.
      ENDAT.
      APPEND wa_cbgsh1 TO it_cbgsh1.
    ENDLOOP.

    "(2)
    LOOP AT it_cbgsh INTO wa_cbgsh WHERE erfdt <= p_date2.
      AT END OF pspnr.
        READ TABLE it_cbgsh INTO wa_cbgsh INDEX sy-tabix.
        IF sy-subrc = 0.
          wa_cbgsh2-pspnr = wa_cbgsh-pspnr.
          wa_cbgsh2-erfdt = wa_cbgsh-erfdt.
          wa_cbgsh2-kalnr = wa_cbgsh-kalnr.
        ENDIF.
      ENDAT.
      APPEND wa_cbgsh2 TO it_cbgsh2.
    ENDLOOP.

    SELECT *
      FROM zps007
      INTO CORRESPONDING FIELDS OF TABLE it_zps007.

    "删除自建表中之前运行存储的数据
    DELETE zps007 FROM TABLE it_zps007.

    "取数2

    SELECT objnr pspnr
      FROM prps
      INTO CORRESPONDING FIELDS OF TABLE it_dxbh
      FOR ALL ENTRIES IN it_tab
      WHERE psphi = it_tab-pspnr.

    IF it_dxbh IS NOT INITIAL.
      SELECT objnr
        FROM aufk
        INTO CORRESPONDING FIELDS OF TABLE it_objnr1
        FOR ALL ENTRIES IN it_dxbh
        WHERE pspel = it_dxbh-pspnr
        AND bukrs IN s_gsdm.
    ENDIF.

    LOOP AT it_dxbh INTO wa_dxbh.
      wa_objnr-objnr1 = wa_dxbh-objnr.

      APPEND wa_objnr TO it_objnr.
    ENDLOOP.
    LOOP AT it_objnr1 INTO wa_objnr1.
      wa_objnr-objnr2 = wa_objnr1-objnr.

      APPEND wa_objnr TO it_objnr.
    ENDLOOP.

*    SELECT prps~objnr AS objnr1 aufk~objnr AS objnr2
*      FROM prps
*      JOIN aufk ON prps~pspnr = aufk~pspel
*      INTO CORRESPONDING FIELDS OF TABLE it_objnr
*      FOR ALL ENTRIES IN it_tab
*      WHERE psphi = it_tab-pspnr
*      AND bukrs IN s_gsdm.

    IF it_objnr IS NOT INITIAL.
      SELECT *
        FROM cosp
        INTO CORRESPONDING FIELDS OF TABLE it_cosp
        FOR ALL ENTRIES IN it_objnr
        WHERE ( objnr = it_objnr-objnr1
        OR objnr = it_objnr-objnr2 )
        AND versn = '000'
        AND ( wrttp = '04'
        OR wrttp = '01').
    ENDIF.

    "将数据存入自建表
    MODIFY zps007 FROM TABLE it_cosp.

    IF it_cosp IS NOT INITIAL.
      SELECT *
        FROM cosp
        INTO CORRESPONDING FIELDS OF TABLE it_5401
        FOR ALL ENTRIES IN it_cosp
        WHERE cosp~objnr = it_cosp-objnr
        AND cosp~kstar LIKE '5401%'
        AND versn = '000'
        AND wrttp = '04'.

      SELECT posid objnr
        FROM prps
        INTO CORRESPONDING FIELDS OF TABLE it_posid1.

      SELECT pspel objnr
        FROM aufk
        INTO CORRESPONDING FIELDS OF TABLE it_aufk
        FOR ALL ENTRIES IN it_5401
        WHERE objnr = it_5401-objnr.

      DATA:p_pspel(20) TYPE c.
      LOOP AT it_aufk INTO wa_aufk.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            input  = wa_aufk-pspel
          IMPORTING
            output = p_pspel.

        wa_posid1-posid = p_pspel.
        wa_posid1-objnr = wa_aufk-objnr.
        APPEND wa_posid1 TO it_posid1.

      ENDLOOP.

      SELECT *
        FROM cosp
        INTO CORRESPONDING FIELDS OF TABLE it_8
        FOR ALL ENTRIES IN it_cosp
        WHERE cosp~objnr = it_cosp-objnr
        AND ( cosp~kstar LIKE '8%'
        OR cosp~kstar LIKE '6603%')
        AND versn = '000'
        AND wrttp = '04'.
      SELECT *
        FROM cosp
        INTO CORRESPONDING FIELDS OF TABLE it_6
        FOR ALL ENTRIES IN it_cosp
        WHERE cosp~objnr = it_cosp-objnr
        AND ( kstar LIKE '6001%' OR kstar LIKE '6051%' OR kstar LIKE '6301%')
        AND versn = '000'
        AND ( wrttp = '01'
        OR wrttp = '04').

      LOOP AT it_5401 INTO wa_5401.
        READ TABLE it_posid1 INTO wa_posid1 WITH KEY objnr = wa_5401-objnr.
        IF sy-subrc = 0.
          wa_5401-posid = wa_posid1-posid.
          MODIFY it_5401 FROM wa_5401.
        ENDIF.
      ENDLOOP.

      LOOP AT it_8 INTO wa_8.
        READ TABLE it_posid1 INTO wa_posid1 WITH KEY objnr = wa_8-objnr.
        IF sy-subrc = 0.
          wa_8-posid = wa_posid1-posid.
          MODIFY it_8 FROM wa_8.
        ENDIF.
      ENDLOOP.

      LOOP AT it_6 INTO wa_6.
        READ TABLE it_posid1 INTO wa_posid1 WITH KEY objnr = wa_6-objnr.
        IF sy-subrc = 0.
          wa_6-posid = wa_posid1-posid.
          MODIFY it_6 FROM wa_6.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

  "取累计收款金额
  SELECT xmbh hsl postdat
    FROM zfi005
    INTO CORRESPONDING FIELDS OF TABLE it_hsl
    WHERE bukrs IN s_gsdm
    AND xmbh IN s_xmdy
    AND step = icon_green_light
    AND postdat < p_date
    AND  ( ywzl = '项目回款'  OR ywzl = '项目预收' ).  "add it02 增加业务类型限制项目回款和项目预收

  LOOP AT it_hxx INTO wa_hxx.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_hxx-pspnr
      IMPORTING
        output = wa_hxx-pspnr.
    MODIFY it_hxx FROM wa_hxx.
  ENDLOOP.

  LOOP AT it_hxx1 INTO wa_hxx1.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_hxx1-pspnr
      IMPORTING
        output = wa_hxx1-pspnr.
    MODIFY it_hxx1 FROM wa_hxx1.
  ENDLOOP.

  LOOP AT it_hxx2 INTO wa_hxx2.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_hxx2-pspnr
      IMPORTING
        output = wa_hxx2-pspnr.
    MODIFY it_hxx2 FROM wa_hxx2.
  ENDLOOP.

  "劳务人工费支出
  IF it_tab IS NOT INITIAL.
    SELECT ebeln
      FROM zfi017
      INTO CORRESPONDING FIELDS OF TABLE it_zfi017
      FOR ALL ENTRIES IN it_tab
      WHERE posid = it_tab-pspid
      AND ebeln LIKE '42%'
      AND zcljd = '3'.

    IF it_zfi017 IS NOT INITIAL.
      SELECT ebeln
        FROM ekpo
        INTO CORRESPONDING FIELDS OF TABLE it_ekpo
        FOR ALL ENTRIES IN it_zfi017
        WHERE ebeln = it_zfi017-ebeln
        AND ( knttp = 'Y'
        OR knttp = 'H'
        OR knttp = 'J').
    ENDIF.
    IF it_ekpo IS NOT INITIAL.
      SELECT posid belnr_f zsqfkje
        FROM zfi017
        INTO CORRESPONDING FIELDS OF TABLE it_zfi0172
        FOR ALL ENTRIES IN it_ekpo
        WHERE ebeln = it_ekpo-ebeln
        AND zcljd = '3'.
    ENDIF.

    IF it_zfi0172 IS NOT INITIAL.
      SELECT zfi017~posid zfi017~zsqfkje
        FROM bkpf
        JOIN zfi017 ON bkpf~belnr = zfi017~belnr_f
        INTO CORRESPONDING FIELDS OF TABLE it_bkpf
        FOR ALL ENTRIES IN it_zfi0172
        WHERE bkpf~belnr = it_zfi0172-belnr_f
        AND bkpf~gjahr <= p_year
        AND bkpf~monat <= p_mnth.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zps007a
    INTO CORRESPONDING FIELDS OF TABLE it_zps007a.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_dealdata .
*
*  LOOP AT it_tab INTO wa_tab.
*
*    IF wa_tab-post1 = '项目定义 TEST'.
*
*      CONTINUE.
*
*    ELSE.
*
*      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
*        EXPORTING
*          input  = wa_tab-pspnr
*        IMPORTING
*          output = wa_tab-pspnr.
*    ENDIF.
*    MODIFY it_tab FROM wa_tab.
*  ENDLOOP.
  "20170907将数据库取数逻辑挪至循环之外 by 夏俊
  SELECT *
    FROM ckis
    INTO CORRESPONDING FIELDS OF TABLE it_ckis1
    FOR ALL ENTRIES IN it_cbgsh1
    WHERE kalnr = it_cbgsh1-kalnr
    AND kstar LIKE '5401%'.

  SELECT *
    FROM ckis
    INTO CORRESPONDING FIELDS OF TABLE it_ckis2
    FOR ALL ENTRIES IN it_cbgsh1
    WHERE kalnr = it_cbgsh1-kalnr
    AND ( kstar LIKE '8%'
    OR kstar LIKE '6603%' ).

  IF it_cbgsh2 IS NOT INITIAL.
    SELECT *
      FROM ckis
      INTO CORRESPONDING FIELDS OF TABLE it_ckis3
      FOR ALL ENTRIES IN it_cbgsh2
      WHERE kalnr = it_cbgsh2-kalnr
      AND kstar LIKE '5401%'.

    SELECT *
      FROM ckis
      INTO CORRESPONDING FIELDS OF TABLE it_ckis4
      FOR ALL ENTRIES IN it_cbgsh2
      WHERE kalnr = it_cbgsh2-kalnr
      AND ( kstar LIKE '8%'
      OR kstar LIKE '6603%').
  ENDIF.

  LOOP AT it_ckis1 INTO wa_ckis1.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_ckis1-pspnr
      IMPORTING
        output = wa_ckis1-pspnr.
    MODIFY it_ckis1 FROM wa_ckis1.
  ENDLOOP.

  LOOP AT it_ckis2 INTO wa_ckis2.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_ckis2-pspnr
      IMPORTING
        output = wa_ckis2-pspnr.
    MODIFY it_ckis2 FROM wa_ckis2.
  ENDLOOP.

  LOOP AT it_ckis3 INTO wa_ckis3.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_ckis3-pspnr
      IMPORTING
        output = wa_ckis3-pspnr.
    MODIFY it_ckis3 FROM wa_ckis3.
  ENDLOOP.

  LOOP AT it_ckis4 INTO wa_ckis4.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_ckis4-pspnr
      IMPORTING
        output = wa_ckis4-pspnr.
    MODIFY it_ckis4 FROM wa_ckis4.
  ENDLOOP.
  "20170907将数据库取数逻辑挪至循环之外 by 夏俊
  DATA:p_pspidd(8) TYPE c.
  LOOP AT it_tab INTO wa_tab.

    "年度
    wa_tab-nd = p_year.

    "期间
    wa_tab-qj = p_mnth.

    "客户描述
    READ TABLE it_name1 INTO wa_name1 WITH KEY kunnr = wa_tab-zkhbm.
    IF sy-subrc = 0.
      wa_tab-name1 = wa_name1-name1.
    ENDIF.

    IF wa_tab-pspid = '18001015000*' OR wa_tab-pspid = '18001015*'.
      wa_tab-nd = p_year.
      wa_tab-qj = p_mnth.
      MODIFY it_tab FROM wa_tab.
      CLEAR wa_tab.
      CONTINUE.
    ELSE.
      p_pspidd = wa_tab-pspid+4(8).
      "合同金额
      LOOP AT it_hxx INTO wa_hxx WHERE pspnr = p_pspidd.
        wa_tab-zhtje = wa_tab-zhtje + wa_hxx-wrtfw_pos.
        CLEAR wa_hxx.
      ENDLOOP.
      "项目预算成本-工程成本
      LOOP AT it_hxx1 INTO wa_hxx1 WHERE pspnr = p_pspidd.
        wa_tab-xmyscbgc2 = wa_tab-xmyscbgc2 + wa_hxx1-wrtfw_pos.
        CLEAR wa_hxx1.
      ENDLOOP.
      wa_tab-xmyscbgc = wa_tab-xmyscbgc2.
      "项目预算成本-期间费用
      LOOP AT it_hxx2 INTO wa_hxx2 WHERE pspnr = p_pspidd.
        wa_tab-xmyscbqj = wa_tab-xmyscbqj + wa_hxx2-wrtfw_pos.
        CLEAR wa_hxx2.
      ENDLOOP.
    ENDIF.
    "项目预算成本（变更）-工程成本、项目预算成本（变更）-期间费用
    IF it_cbgsh1 IS INITIAL.
      wa_tab-xmyscbbggc = 0.
      wa_tab-xmyscbbgqj = 0.
    ELSE.
      "数据库取数逻辑挪至LOOP循环之外

      LOOP AT it_ckis1 INTO wa_ckis1 WHERE pspnr = p_pspidd.
        wa_ljje-54011 = wa_ljje-54011 + wa_ckis1-wrtfw_pos.
        CLEAR wa_ckis1.
      ENDLOOP.

      LOOP AT it_ckis2 INTO wa_ckis2 WHERE pspnr = p_pspidd.
        wa_ljje-81 = wa_ljje-81 + wa_ckis2-wrtfw_pos.
        CLEAR wa_ckis2.
      ENDLOOP.

      LOOP AT it_ckis3 INTO wa_ckis3 WHERE pspnr = p_pspidd.
        wa_ljje-54012 = wa_ljje-54012 + wa_ckis3-wrtfw_pos.
        CLEAR wa_ckis3.
      ENDLOOP.

      LOOP AT it_ckis4 INTO wa_ckis4 WHERE pspnr = p_pspidd.
        wa_ljje-82 = wa_ljje-82 + wa_ckis4-wrtfw_pos.
        CLEAR wa_ckis4.
      ENDLOOP.

      wa_tab-xmyscbbggc = wa_ljje-54011 - wa_ljje-54012.
      wa_tab-xmyscbbgqj = wa_ljje-81 - wa_ljje-82.

      CLEAR wa_ljje.
    ENDIF.

    DATA:p_wtg(4) TYPE c VALUE 'WTG0'.
    DATA:p_wtgaa TYPE string.
    CONCATENATE p_wtg p_mnth INTO p_wtgaa.

*    DATA:p_pspid1(4) TYPE c VALUE '1800'.
*    DATA:p_pspid2(12) TYPE c.
*    CONCATENATE p_pspid1 wa_tab-pspnr INTO p_pspid2.
    DATA:p_p1(12) TYPE c.
    DATA:p_p2(12) TYPE c.
    p_p2 = wa_tab-pspid(12).
    "项目成本-工程成本
    LOOP AT it_5401 INTO wa_5401.
      p_p1 = wa_5401-posid(12).

      IF p_p1 = p_p2 AND wa_5401-gjahr < p_year.
        wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                       wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012 + wa_5401-wtg013 +
                       wa_5401-wtg014 + wa_5401-wtg015 + wa_5401-wtg016.
      ENDIF.
      IF p_p1 = p_p2 AND wa_5401-gjahr = p_year.
        IF p_wtgaa = 'WTG001'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001.
        ELSEIF p_wtgaa = 'WTG002'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002.
        ELSEIF p_wtgaa = 'WTG003'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003.
        ELSEIF p_wtgaa = 'WTG004'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004.
        ELSEIF p_wtgaa = 'WTG005'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005.
        ELSEIF p_wtgaa = 'WTG006'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006.
        ELSEIF p_wtgaa = 'WTG007'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007.
        ELSEIF p_wtgaa = 'WTG008'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008.
        ELSEIF p_wtgaa = 'WTG009'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009.
        ELSEIF p_wtgaa = 'WTG010'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010.
        ELSEIF p_wtgaa = 'WTG011'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011.
        ELSEIF p_wtgaa = 'WTG012'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012.
        ELSEIF p_wtgaa = 'WTG013'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012 + wa_5401-wtg013.
        ELSEIF p_wtgaa = 'WTG014'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012 + wa_5401-wtg013 +
                          wa_5401-wtg014.
        ELSEIF p_wtgaa = 'WTG015'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012 + wa_5401-wtg013 +
                          wa_5401-wtg014 + wa_5401-wtg015.
        ELSEIF p_wtgaa = 'WTG016'.
          wa_tab-xmcbgc = wa_tab-xmcbgc + wa_5401-wtg001 + wa_5401-wtg002 + wa_5401-wtg003 + wa_5401-wtg004 + wa_5401-wtg005 + wa_5401-wtg006 +
                          wa_5401-wtg007 + wa_5401-wtg008 + wa_5401-wtg009 + wa_5401-wtg010 + wa_5401-wtg011 + wa_5401-wtg012 + wa_5401-wtg013 +
                          wa_5401-wtg014 + wa_5401-wtg015 + wa_5401-wtg016.
        ENDIF.
      ENDIF.
      IF p_p1 = p_p2 AND wa_5401-gjahr = p_year.
        IF p_wtgaa = 'WTG001'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg001.
        ELSEIF p_wtgaa = 'WTG002'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg002.
        ELSEIF p_wtgaa = 'WTG003'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg003.
        ELSEIF p_wtgaa = 'WTG004'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg004.
        ELSEIF p_wtgaa = 'WTG005'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg005.
        ELSEIF p_wtgaa = 'WTG006'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg006.
        ELSEIF p_wtgaa = 'WTG007'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg007.
        ELSEIF p_wtgaa = 'WTG008'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg008.
        ELSEIF p_wtgaa = 'WTG009'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg009.
        ELSEIF p_wtgaa = 'WTG010'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg010.
        ELSEIF p_wtgaa = 'WTG011'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg011.
        ELSEIF p_wtgaa = 'WTG012'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg012.
        ELSEIF p_wtgaa = 'WTG013'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg013.
        ELSEIF p_wtgaa = 'WTG014'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg014.
        ELSEIF p_wtgaa = 'WTG015'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg015.
        ELSEIF p_wtgaa = 'WTG016'.
          wa_tab-xmcb2gc = wa_tab-xmcb2gc + wa_5401-wtg016.
        ENDIF.
      ENDIF.
      CLEAR wa_5401.
    ENDLOOP.
    CLEAR p_p1.
    "项目成本-期间费用
    LOOP AT it_8 INTO wa_8.
      p_p1 = wa_8-posid(12).
      IF p_p1 = p_p2 AND wa_8-gjahr < p_year.
        wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                      + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012 + wa_8-wtg013 + wa_8-wtg014 + wa_8-wtg015 + wa_8-wtg016.
      ENDIF.
      IF p_p1 = p_p2 AND wa_8-gjahr = p_year.
        IF p_wtgaa = 'WTG001'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001.
        ELSEIF p_wtgaa = 'WTG002'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002.
        ELSEIF p_wtgaa = 'WTG003'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003.
        ELSEIF p_wtgaa = 'WTG004'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004.
        ELSEIF p_wtgaa = 'WTG005'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005.
        ELSEIF p_wtgaa = 'WTG006'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006.
        ELSEIF p_wtgaa = 'WTG007'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007.
        ELSEIF p_wtgaa = 'WTG008'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008.
        ELSEIF p_wtgaa = 'WTG009'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009.
        ELSEIF p_wtgaa = 'WTG010'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010.
        ELSEIF p_wtgaa = 'WTG011'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011.
        ELSEIF p_wtgaa = 'WTG012'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012.
        ELSEIF p_wtgaa = 'WTG013'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012 + wa_8-wtg013.
        ELSEIF p_wtgaa = 'WTG014'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012 + wa_8-wtg013 + wa_8-wtg014.
        ELSEIF p_wtgaa = 'WTG015'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012 + wa_8-wtg013 + wa_8-wtg014 + wa_8-wtg015.
        ELSEIF p_wtgaa = 'WTG016'.
          wa_tab-xmcbqj = wa_tab-xmcbqj + wa_8-wtg001 + wa_8-wtg002 + wa_8-wtg003 + wa_8-wtg004 + wa_8-wtg005 + wa_8-wtg006 + wa_8-wtg007
                         + wa_8-wtg008 + wa_8-wtg009 + wa_8-wtg010 + wa_8-wtg011 + wa_8-wtg012 + wa_8-wtg013 + wa_8-wtg014 + wa_8-wtg015 + wa_8-wtg016.
        ENDIF.
      ENDIF.
      IF p_p1 = p_p2 AND wa_8-gjahr = p_year.
        IF p_wtgaa = 'WTG001'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg001.
        ELSEIF p_wtgaa = 'WTG002'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg002.
        ELSEIF p_wtgaa = 'WTG003'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg003.
        ELSEIF p_wtgaa = 'WTG004'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg004.
        ELSEIF p_wtgaa = 'WTG005'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg005.
        ELSEIF p_wtgaa = 'WTG006'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg006.
        ELSEIF p_wtgaa = 'WTG007'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg007.
        ELSEIF p_wtgaa = 'WTG008'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg008.
        ELSEIF p_wtgaa = 'WTG009'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg009.
        ELSEIF p_wtgaa = 'WTG010'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg010.
        ELSEIF p_wtgaa = 'WTG011'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg011.
        ELSEIF p_wtgaa = 'WTG012'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg012.
        ELSEIF p_wtgaa = 'WTG013'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg013.
        ELSEIF p_wtgaa = 'WTG014'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg014.
        ELSEIF p_wtgaa = 'WTG015'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg015.
        ELSEIF p_wtgaa = 'WTG016'.
          wa_tab-xmcb2qj = wa_tab-xmcb2qj + wa_8-wtg016.
        ENDIF.
      ENDIF.
      CLEAR wa_8.
    ENDLOOP.
    CLEAR p_p1.
    DATA:p_1 TYPE p LENGTH 16 DECIMALS 14."累计开票数
    DATA:p_2 TYPE p LENGTH 16 DECIMALS 14."当期开票数（应收）
    DATA:p_3 TYPE p LENGTH 16 DECIMALS 14."项目收入
    DATA:p_4 TYPE p LENGTH 16 DECIMALS 14."2222项目收入
    LOOP AT it_6 INTO wa_6.
      p_p1 = wa_6-posid(12).
      IF wa_6-wrttp = '04'.
        IF p_p1 = p_p2 AND wa_6-gjahr < p_year.
          p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
          + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015 + wa_6-wtg016.
        ENDIF.
        IF p_p1 = p_p2 AND wa_6-gjahr = p_year.
          IF p_wtgaa = 'WTG001'.
            p_1 = p_1 + wa_6-wtg001.
          ELSEIF p_wtgaa = 'WTG002'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002.                              "累计开票数
          ELSEIF p_wtgaa = 'WTG003'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003.
          ELSEIF p_wtgaa = 'WTG004'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004.
          ELSEIF p_wtgaa = 'WTG005'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005.
          ELSEIF p_wtgaa = 'WTG006'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006.
          ELSEIF p_wtgaa = 'WTG007'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007.
          ELSEIF p_wtgaa = 'WTG008'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008.
          ELSEIF p_wtgaa = 'WTG009'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009.
          ELSEIF p_wtgaa = 'WTG010'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010.
          ELSEIF p_wtgaa = 'WTG011'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011.
          ELSEIF p_wtgaa = 'WTG012'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012.
          ELSEIF p_wtgaa = 'WTG013'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013.
          ELSEIF p_wtgaa = 'WTG014'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014.
          ELSEIF p_wtgaa = 'WTG015'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015.
          ELSEIF p_wtgaa = 'WTG016'.
            p_1 = p_1 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
            + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015 + wa_6-wtg016.
          ENDIF.
        ENDIF.
      ELSEIF wa_6-wrttp = '01'.
        IF p_p1 = p_p2 AND wa_6-gjahr = p_year.
          p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
       + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015 + wa_6-wtg016.
        ENDIF.
        IF p_p1 = p_p2 AND wa_6-gjahr < p_year.
          IF p_wtgaa = 'WTG001'.
            p_3 = p_3 + wa_6-wtg001.
          ELSEIF p_wtgaa = 'WTG002'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002.
          ELSEIF p_wtgaa = 'WTG003'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003.
          ELSEIF p_wtgaa = 'WTG004'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004.                  "项目收入
          ELSEIF p_wtgaa = 'WTG005'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005.
          ELSEIF p_wtgaa = 'WTG006'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006.
          ELSEIF p_wtgaa = 'WTG007'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007.
          ELSEIF p_wtgaa = 'WTG008'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008.
          ELSEIF p_wtgaa = 'WTG009'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009.
          ELSEIF p_wtgaa = 'WTG010'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010.
          ELSEIF p_wtgaa = 'WTG011'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011.
          ELSEIF p_wtgaa = 'WTG012'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012.
          ELSEIF p_wtgaa = 'WTG013'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013.
          ELSEIF p_wtgaa = 'WTG014'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014.
          ELSEIF p_wtgaa = 'WTG015'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015.
          ELSEIF p_wtgaa = 'WTG016'.
            p_3 = p_3 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009
             + wa_6-wtg010 + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015 + + wa_6-wtg016.
          ENDIF.
        ENDIF.
      ENDIF.

      IF wa_6-wrttp = '04'.
        IF p_p1 = p_p2 AND wa_6-gjahr = p_year.
          IF p_wtgaa = 'WTG001'.
            p_2 = p_2 + wa_6-wtg001.
          ELSEIF p_wtgaa = 'WTG002'.
            p_2 = p_2 + wa_6-wtg002.
          ELSEIF p_wtgaa = 'WTG003'.                  "当期开票数（应收）
            p_2 = p_2 + wa_6-wtg003.
          ELSEIF p_wtgaa = 'WTG004'.
            p_2 = p_2 + wa_6-wtg004.
          ELSEIF p_wtgaa = 'WTG005'.
            p_2 = p_2 + wa_6-wtg005.
          ELSEIF p_wtgaa = 'WTG006'.
            p_2 = p_2 + wa_6-wtg006.
          ELSEIF p_wtgaa = 'WTG007'.
            p_2 = p_2 + wa_6-wtg007.
          ELSEIF p_wtgaa = 'WTG008'.
            p_2 = p_2 + wa_6-wtg008.
          ELSEIF p_wtgaa = 'WTG009'.
            p_2 = p_2 + wa_6-wtg009.
          ELSEIF p_wtgaa = 'WTG010'.
            p_2 = p_2 + wa_6-wtg010.
          ELSEIF p_wtgaa = 'WTG011'.
            p_2 = p_2 + wa_6-wtg011.
          ELSEIF p_wtgaa = 'WTG012'.
            p_2 = p_2 + wa_6-wtg012.
          ELSEIF p_wtgaa = 'WTG013'.
            p_2 = p_2 + wa_6-wtg013.
          ELSEIF p_wtgaa = 'WTG014'.
            p_2 = p_2 + wa_6-wtg014.
          ELSEIF p_wtgaa = 'WTG015'.
            p_2 = p_2 + wa_6-wtg015.
          ELSEIF p_wtgaa = 'WTG016'.
            p_2 = p_2 + wa_6-wtg016.
          ENDIF.
        ENDIF.
      ELSEIF wa_6-wrttp = '01'.
        IF p_p1 = p_p2 AND wa_6-gjahr < p_year.
          p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015 + wa_6-wtg016.
        ENDIF.
        IF p_p1 = p_p2 AND wa_6-gjahr = p_year.
          IF p_wtgaa = 'WTG001'.
            p_4 = p_4 + wa_6-wtg001 .
          ELSEIF p_wtgaa = 'WTG002'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002.
          ELSEIF p_wtgaa = 'WTG003'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 .
          ELSEIF p_wtgaa = 'WTG004'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 .
          ELSEIF p_wtgaa = 'WTG005'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005 .
          ELSEIF p_wtgaa = 'WTG006'.                        "项目收入22222
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 .
          ELSEIF p_wtgaa = 'WTG007'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 .
          ELSEIF p_wtgaa = 'WTG008'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 .
          ELSEIF p_wtgaa = 'WTG009'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 .
          ELSEIF p_wtgaa = 'WTG010'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010.
          ELSEIF p_wtgaa = 'WTG011'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                     + wa_6-wtg011 .
          ELSEIF p_wtgaa = 'WTG012'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                     + wa_6-wtg011 + wa_6-wtg012.
          ELSEIF p_wtgaa = 'WTG013'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                     + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 .
          ELSEIF p_wtgaa = 'WTG014'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                     + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 .
          ELSEIF p_wtgaa = 'WTG015'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                     + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                     + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015.
          ELSEIF p_wtgaa = 'WTG016'.
            p_4 = p_4 + wa_6-wtg001 + wa_6-wtg002 + wa_6-wtg003 + wa_6-wtg004 + wa_6-wtg005
                      + wa_6-wtg006 + wa_6-wtg007 + wa_6-wtg008 + wa_6-wtg009 + wa_6-wtg010
                      + wa_6-wtg011 + wa_6-wtg012 + wa_6-wtg013 + wa_6-wtg014 + wa_6-wtg015
                      + wa_6-wtg016.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR wa_6.
    ENDLOOP.
    wa_tab-xmcbgc2 = wa_tab-xmcbgc.
    wa_tab-xmcb2gc2 = wa_tab-xmcb2gc.
    IF wa_tab-xmyscbgc2 IS NOT INITIAL.
      "项目完工百分比
      wa_tab-xmwgbfb2 = wa_tab-xmcbgc / wa_tab-xmyscbgc2.
      wa_tab-xmwgbfb = wa_tab-xmwgbfb2.
      "项目收入

      wa_tab-xmsr2 = p_4 * wa_tab-xmcb2gc / wa_tab-xmyscbgc2.
    ENDIF.
    wa_tab-xmsr = p_3 * wa_tab-xmwgbfb2.
    wa_tab-xmsr3 = p_3 * wa_tab-xmwgbfb2.
    "累计收款金额
    LOOP AT it_hsl INTO wa_hsl WHERE xmbh = wa_tab-pspid.
      wa_tab-ljskje = wa_tab-ljskje + wa_hsl-hsl.
      CLEAR wa_hsl.
    ENDLOOP.
    "当前收款金额
    LOOP AT it_hsl INTO wa_hsl WHERE xmbh = wa_tab-pspid AND postdat < p_date AND postdat >= p_date2.
      wa_tab-dqskje = wa_tab-dqskje + wa_hsl-hsl.
      CLEAR wa_hsl.
    ENDLOOP.
*    DATA:pspnr TYPE proj-pspnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
*      EXPORTING
*        input  = wa_tab-pspid
*      IMPORTING
*        output = pspnr.
**        projwa    =
**      EXCEPTIONS
**        not_found = 1
**        OTHERS    = 2.
*
*    g_psphi = pspnr(2).
    CLEAR g_psphi.
    g_psphi = wa_tab-pspid+4(2).

    "累计开票数（应收）、当期开票数（应收）
    IF g_psphi = 22 OR g_psphi = 23.
      wa_tab-ljkps = p_1 * a.
      wa_tab-dqkps = p_2 * a.
    ELSE.
      wa_tab-ljkps = p_1.
      wa_tab-dqkps = p_2.
    ENDIF.
    CLEAR:p_1,p_2,p_3,p_4.
    "已开票未收款金额
    wa_tab-ykpwskje = 0 - ( wa_tab-ljkps + wa_tab-ljskje ).

    "毛利额
    wa_tab-mle = -1 * ( wa_tab-xmsr3 + wa_tab-xmcbgc ).
    wa_tab-mle2 = wa_tab-mle.
    "合同毛利率
    IF wa_tab-xmsr3 IS NOT INITIAL.
      wa_tab-htmll = wa_tab-mle2 / wa_tab-xmsr3.
    ENDIF.

    LOOP AT it_bkpf INTO wa_bkpf WHERE posid = wa_tab-pspid.
      wa_tab-lwrgfzc = wa_tab-lwrgfzc + wa_bkpf-zsqfkje.
      CLEAR wa_bkpf.
    ENDLOOP.

    "总合同金额 更改为：总合同金额为 PROJ-ZHTJE  by it02 20160630
*    IF g_psphi = 22 OR g_psphi = 23.
*      wa_tab-zzhtje = - wa_tab-zhtje * a.
*    ELSE.
*      wa_tab-zzhtje = - wa_tab-zhtje.
*    ENDIF.
*    "
*    IF g_psphi = '30'.
*      wa_tab-zzhtje = wa_tab-zhtje30  .  "30项目合同金额直接取CJ20N的自定义合同金额
*    ENDIF.
    wa_tab-zzhtje = wa_tab-zhtje30  .
    "未收款金额
    IF wa_tab-zjgjsje = 0.
      wa_tab-wskje = wa_tab-zzhtje - wa_tab-ljskje.
    ELSE.
      wa_tab-wskje = wa_tab-zjgjsje - wa_tab-ljskje.
    ENDIF.

    MODIFY it_tab FROM wa_tab.
    CLEAR wa_tab.
  ENDLOOP.

  SORT it_zps007a BY pspid.

  LOOP AT it_zps007a INTO wa_zps007a.
    CLEAR wa_tab1.
    wa_tab1-pspid = wa_zps007a-pspid.
    wa_tab1-ljskje = wa_zps007a-ljskje.
    wa_tab1-lwrgfzc = wa_zps007a-lwrgfzc.
    COLLECT wa_tab1 INTO it_tab1.
  ENDLOOP.

  LOOP AT it_tab INTO wa_tab.

    READ TABLE it_tab1 INTO wa_tab1 WITH KEY pspid = wa_tab-pspid.
    IF sy-subrc = 0.
      wa_tab-ljskje = wa_tab1-ljskje + wa_tab-ljskje.
      wa_tab-lwrgfzc = wa_tab1-lwrgfzc + wa_tab-lwrgfzc.
    ENDIF.
    "已开票未收款金额 add it02 151202
    wa_tab-ykpwskje = 0 - ( wa_tab-ljkps + wa_tab-ljskje ).


    "未收款金额
    IF wa_tab-zjgjsje = 0.
      wa_tab-wskje = wa_tab-zzhtje - wa_tab-ljskje.
    ELSE.
      wa_tab-wskje = wa_tab-zjgjsje - wa_tab-ljskje.
    ENDIF.

    MODIFY it_tab FROM wa_tab.
    CLEAR wa_tab.
  ENDLOOP.

  IF it_tab IS INITIAL.
    MESSAGE 'No data!' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_layout .
  wa_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  init_fieldcat 'PSPNR' '项目编号' 'PROJ' 'PSPNR'.
  init_fieldcat 'POST1' '项目名称' 'PROJ' 'POST1'.
  init_fieldcat 'ND' '年度' '' ''.
  init_fieldcat 'QJ' '期间' '' ''.
  init_fieldcat 'ZKHBM' '客户编号' 'PROJ' 'ZKHBM'.
  init_fieldcat 'NAME1' '客户描述' 'KNA1' 'NAME1'.
  " *成本权限控制 add by it02_weiyun   20161115
  AUTHORITY-CHECK OBJECT 'Z_CO_XMCB' ID 'ACTVT' FIELD '3'.
  IF sy-subrc = 0.
    init_fieldcat 'ZHTJE' '计划收入金额' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMYSCBGC' '项目预算成本-工程成本' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMYSCBQJ' '项目预算成本-期间费用' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMCBGC2' '项目成本-工程成本' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMCBQJ' '项目成本-期间费用' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMSR' '项目收入' 'PROJ' 'ZHTJE'.
  ENDIF.
  init_fieldcat 'LJKPS' '累计开票数（应收）' 'PROJ' 'ZHTJE'.
  init_fieldcat 'XMWGBFB' '项目完工百分比' 'PROJ' 'ZHTJE'.
  init_fieldcat 'LJSKJE' '累计收款金额' 'PROJ' 'ZHTJE'.
  init_fieldcat 'YKPWSKJE' '已开票未收款金额' 'PROJ' 'ZHTJE'.
  init_fieldcat 'ZJGJSJE' '竣工决算金额' 'PROJ' 'ZHTJE'.
  init_fieldcat 'LWHTJE' '劳务合同金额' 'PROJ' 'ZHTJE'.
  " *成本权限控制 add by it02_weiyun   20161115
  AUTHORITY-CHECK OBJECT 'Z_CO_XMCB' ID 'ACTVT' FIELD '3'.
  IF sy-subrc = 0.
    init_fieldcat 'LWRGFZC' '劳务人工费支出' 'PROJ' 'ZHTJE'.
    init_fieldcat 'HTMLL' '合同毛利率' 'PROJ' 'ZHTJE'.
    init_fieldcat 'MLE' '毛利额' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMYSCBBGGC' '项目预算成本（变更）-工程成本(当期)' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMYSCBBGQJ' '项目预算成本（变更）-期间费用(当期)' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMCB2GC2'   '项目成本-工程成本(当期)' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMCB2QJ'    '项目成本-期间费用(当期)' 'PROJ' 'ZHTJE'.
    init_fieldcat 'XMSR2'      '项目收入(当期)' 'PROJ' 'ZHTJE'.
  ENDIF.
  init_fieldcat 'DQKPS' '当期开票数（应收）' 'PROJ' 'ZHTJE'.
  init_fieldcat 'DQSKJE' '当期收款金额' 'PROJ' 'ZHTJE'.
  init_fieldcat 'ZZHTJE' '总合同金额' '' ''.
  init_fieldcat 'WSKJE' '未收款金额' '' ''.
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
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = it_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        =
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = it_tab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
