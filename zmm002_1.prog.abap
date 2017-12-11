
*&---------------------------------------------------------------------*
*& 程序名称:ZMM002_1
*& 作者    :IT02
*& 开发日期:20161229
*& 请求号  : ED1K905135
*& 描述    :工厂级物料主数据导出：数据逻辑FROMZMM002
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*
REPORT zmm002_1.


TYPE-POOLS: slis,vrm.
TABLES:mara,marc,makt,mard,mlan,mvke,mbew,mvgd,qmat.


TYPES: BEGIN OF t2_out,
         bismt LIKE mara-bismt,
         matnr LIKE mara-matnr,
         lvorm LIKE marc-lvorm,
         mtart LIKE mara-mtart,
         mbrsh LIKE mara-mbrsh,
         werks LIKE marc-werks,
         maktx LIKE makt-maktx,
         meins LIKE mara-meins,
         matkl LIKE mara-matkl,
         spart LIKE mara-spart,
         maabc LIKE marc-maabc,
         dismm LIKE marc-dismm,
         dispo LIKE marc-dispo,
         disls LIKE marc-disls,
         minbe LIKE marc-minbe,
         bstfe LIKE marc-bstfe,
         bstmi LIKE marc-bstmi,
         bstrf LIKE marc-bstrf,
         bstma LIKE marc-bstma,
         beskz LIKE marc-beskz,
         sobsl LIKE marc-sobsl,
         lgpro LIKE marc-lgpro,
         lgfsb LIKE marc-lgfsb,
         schgt LIKE marc-schgt,
         dzeit LIKE marc-dzeit,
         plifz LIKE marc-plifz,
         fhori LIKE marc-fhori,
         eisbe LIKE marc-eisbe,
         strgr LIKE marc-strgr,
         mtvfp LIKE marc-mtvfp,
         sbdkz LIKE marc-sbdkz,
         kzaus LIKE marc-kzaus,
         ausdt LIKE marc-ausdt,
         nfmat LIKE marc-nfmat,
         fevor LIKE marc-fevor,
         ersda LIKE mara-ersda,
         disgr LIKE marc-disgr,
         rgekz LIKE marc-rgekz,
         kausf LIKE marc-kausf,
         bearz LIKE marc-bearz,
         basmg LIKE marc-basmg,
         sfcpf LIKE marc-sfcpf,
         webaz LIKE marc-webaz,
         ausss LIKE marc-ausss,
*ADD BY HANDWY 2015,3,5.
         mabst LIKE marc-mabst, "最大库存水平
         vrmod LIKE marc-vrmod, "消耗模式
         vint1 LIKE marc-vint1, "逆向消耗期间
         vint2 LIKE marc-vint2, "向前消耗期间
         ruezt LIKE marc-ruezt, "准备时间
       END OF t2_out.


DATA: it_gcst TYPE table of t2_out,
      is_gcst type t2_out.

DATA: it_marc TYPE STANDARD TABLE OF marc WITH HEADER LINE,
      it_mara TYPE STANDARD TABLE OF mara WITH HEADER LINE,
      it_mard TYPE STANDARD TABLE OF mard WITH HEADER LINE,
      it_makt TYPE STANDARD TABLE OF makt WITH HEADER LINE,
      it_mlan TYPE STANDARD TABLE OF mlan WITH HEADER LINE,
      it_mvke TYPE STANDARD TABLE OF mvke WITH HEADER LINE,
      it_mbew TYPE STANDARD TABLE OF mbew WITH HEADER LINE,
      it_mvgd TYPE STANDARD TABLE OF mvgd WITH HEADER LINE.

DATA: ls_fieldcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gs_layout      TYPE slis_layout_alv,
      git_listheader TYPE slis_t_listheader,        "ALV 表头
      git_events     TYPE slis_t_event,             "ALV 事件
      v_stru_disvar  TYPE disvariant,
      g_repid        LIKE sy-repid.

DATA gt_t001w  TYPE TABLE OF t001w WITH HEADER LINE.

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

*分类视图
DATA:
  it_num  LIKE bapi1003_alloc_values_num  OCCURS 0 WITH HEADER LINE,
  it_char LIKE bapi1003_alloc_values_char OCCURS 0 WITH HEADER LINE,
  it_curr LIKE bapi1003_alloc_values_curr OCCURS 0 WITH HEADER LINE,
  it_ret  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:s_matnr2 FOR mara-matnr MODIF ID mi2.
SELECT-OPTIONS:s_bismt2 FOR mara-bismt MODIF ID mi2.    "旧物料号
SELECT-OPTIONS:s_mtart2 FOR mara-mtart MODIF ID mi2.
SELECT-OPTIONS:s_matkl2 FOR mara-matkl MODIF ID mi2.   "物料组"
SELECT-OPTIONS:s_extwg2 FOR mara-extwg MODIF ID mi2.   "外部物料组
SELECT-OPTIONS:s_werks2 FOR marc-werks MODIF ID mi2 obligatory.
SELECT-OPTIONS:s_dispo  FOR marc-dispo MODIF ID mi2.   "MRP控制者"
SELECT-OPTIONS:s_beskz2 FOR marc-beskz MODIF ID mi2.
SELECT-OPTIONS:s_ersda2 FOR mara-ersda MODIF ID mi2.
SELECT-OPTIONS:s_laeda2 FOR mara-laeda MODIF ID mi2.    "上次更改
SELECTION-SCREEN END OF BLOCK b2 .

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

*查询工厂
  SELECT * FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE gt_t001w.

  PERFORM frm_auth_check.

  PERFORM frm_data.

END-OF-SELECTION.

FORM frm_data.

      PERFORM frm_data02.
      PERFORM frm_zh_xml.   "转换XML格式
      PERFORM frm_cc_sapfwq."生成SAP服务器文件


ENDFORM.

FORM frm_data02.
  CLEAR: it_makt,it_makt[].

  SELECT a~bismt
  a~matnr a~mtart a~mbrsh a~meins a~matkl a~spart a~ersda b~werks b~maabc
  b~dispo b~disls b~minbe b~bstfe b~bstmi b~bstrf b~bstma b~beskz b~sobsl b~lgpro
  b~lgfsb b~schgt b~dzeit b~plifz b~fhori b~eisbe b~strgr b~mtvfp b~sbdkz b~kzaus
  b~ausdt b~nfmat b~fevor b~dismm
  b~disgr
  b~rgekz
  b~kausf
  b~bearz
  b~basmg
  b~sfcpf
  b~webaz
  b~ausss
*ADD BY HANDWY 2015,3,5
  b~mabst
  b~vrmod
  b~vint1
  b~vint2
  b~ruezt
  b~lvorm
  INTO CORRESPONDING FIELDS OF TABLE it_gcst
  FROM mara AS a
  INNER JOIN marc AS b ON a~matnr = b~matnr
  WHERE a~matnr IN s_matnr2
  AND a~mtart IN s_mtart2
  AND a~ersda IN s_ersda2
  AND matkl   IN s_matkl2
  AND extwg   IN s_extwg2
  AND laeda   IN s_laeda2
  AND b~werks IN s_werks2
  AND b~beskz IN s_beskz2
  AND a~bismt IN s_bismt2
  AND b~dispo IN s_dispo.



  IF it_gcst[] IS NOT INITIAL.
    SELECT matnr maktx
    FROM makt
    INTO CORRESPONDING FIELDS OF TABLE it_makt
    FOR ALL ENTRIES IN it_gcst
    WHERE matnr = it_gcst-matnr
    AND   spras = sy-langu..
  ENDIF.

  LOOP AT it_gcst into is_gcst ..
    READ TABLE it_makt WITH KEY matnr = is_gcst-matnr.
    IF sy-subrc = 0.
      is_gcst-maktx = it_makt-maktx.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = is_gcst-meins
*       LANGUAGE       = SY-LANGU
      IMPORTING
*       LONG_TEXT      =
        output         = is_gcst-meins
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    MODIFY it_gcst from is_gcst.
  ENDLOOP.
ENDFORM.

FORM frm_auth_check .
  LOOP AT gt_t001w WHERE werks IN s_werks2.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
             ID 'WERKS' FIELD gt_t001w-werks
             .
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gt_t001w-werks.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ZH_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_zh_xml .

 CALL TRANSFORMATION zmm002_1_zh
     SOURCE table = it_gcst
     RESULT XML xmlstr.
  REPLACE FIRST OCCURRENCE OF 'encoding="utf-16"' IN xmlstr WITH 'encoding="gbk"'.
  APPEND xmlstr TO xml_table.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CC_SAPFWQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_cc_sapfwq .

  dname = 'ZMM002物料主数据工厂级明细表.XLS'.
  CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
  DELETE DATASET dname .
  OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0. EXIT. ENDIF.
  LOOP AT xml_table INTO wa_xml.
    TRANSFER wa_xml TO dname.
  ENDLOOP.
  CLOSE DATASET dname.

ENDFORM.
