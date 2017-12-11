*&---------------------------------------------------------------------*
*&  包含                ZMM005A_GSJDY
*&---------------------------------------------------------------------*
DATA: BEGIN OF gt_alv_b OCCURS 0,
        checkbox TYPE c.
        INCLUDE STRUCTURE zmm005a_b.
DATA:htjj   TYPE ekpo-netwr,   "合同净价
     bzjg   TYPE mbew-stprs,   "标准价格
     bzjgdw TYPE mbew-peinh,    "标准价格单位
     zjcgjj TYPE ekpo-netwr,    "最近采购净价
     jjbl   TYPE ekpo-netwr,    "降价比例
     jjbl_% TYPE string,
     bhhtje TYPE tslvt12,    "本行合同金额
     xmmc   TYPE string,        "项目名称
     ppv    TYPE mbew-stprs,    "PPV
     gsmng  TYPE gsmng,        "计划数量
     sqsl   TYPE ekpo-menge,   "申请数量
     cgcsg  TYPE eban-menge,   "采购超申购数量
     cjhsl  TYPE ekpo-menge,   "超计划数量
     cjhje  TYPE  tslvt12,  "超计划金额
     cjhl   TYPE  konv-kbetr,  "超级率
     cjhl_1 TYPE  string,

     aedat2 TYPE dats,
     END OF gt_alv_b.
*****   项目名称长文本   *****
DATA:gt_text TYPE TABLE OF tline,
     gs_text LIKE LINE OF gt_text.

DATA:gt_text_b TYPE TABLE OF tline,
     gs_text_b LIKE LINE OF gt_text_b.
DATA l_name TYPE thead-tdrefname.
DATA l_name_b TYPE thead-tdrefname.
*------------对ALV里面的进行重新赋值------------
LOOP AT gt_alv.
  MOVE-CORRESPONDING gt_alv TO gt_alv_b.
  APPEND gt_alv_b.

ENDLOOP.

*-----------------EKPO结构------------
TYPES:
  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    kunnr TYPE ekpo-kunnr,
    matnr TYPE ekpo-matnr,
    txz01 TYPE ekpo-txz01,
    menge TYPE ekpo-menge,
    meins TYPE ekpo-meins,
    prdat TYPE ekpo-prdat,
  END OF ty_ekpo.
DATA:
      gt_ekpo TYPE TABLE OF ty_ekpo.
DATA:
     gt_ekpo_b TYPE TABLE OF ty_ekpo.

DATA:
   gt_ekpo_c TYPE TABLE OF ty_ekpo.
TYPES:
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1.
DATA:
      gt_kna1 TYPE TABLE OF ty_kna1.

DATA:
    gt_kna1_b TYPE TABLE OF ty_kna1.
TYPES:
  BEGIN OF ty_ekpv,
    ebeln TYPE ekpv-ebeln,
    kunnr TYPE ekpv-kunnr,
  END OF ty_ekpv.
DATA:
      gt_ekpv TYPE TABLE OF ty_ekpv.
DATA:
     gt_ekpv_b TYPE TABLE OF ty_ekpv.

FIELD-SYMBOLS:
  <gs_ekpo>   TYPE ty_ekpo,
  <gs_ekpo_b> TYPE ty_ekpo,
  <gs_ekpo_c> TYPE ty_ekpo,
  <gs_kna1>   TYPE ty_kna1,
  <gs_kna1_b> TYPE ty_kna1,
  <gs_ekpv>   TYPE ty_ekpv,
  <gs_ekpv_b> TYPE ty_ekpv.


*定义打印属性
DATA: g_fm_name            TYPE rs38l_fnam,
      g_output             TYPE ssfcompop,
      g_control_parameters TYPE ssfctrlop,
      g_lw_ssfcrescl       TYPE ssfcrescl,
      g_option             TYPE ssfcrescl.
*分页页数
DATA gc_num TYPE i VALUE 25.
*****  业务控制打印参数   *****
DATA gc_type TYPE i VALUE 1.

DATA: gt_tab  LIKE TABLE OF zmm005_item, " 定义传入SMARTFORMS表中的内表
      gs_tab  LIKE LINE OF gt_tab,
      zzs_tab LIKE zmm005_head.         " 表抬头数据

*&---------------------------------------------------------------------*
*&      Form  FRM_SQL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
"从EKPO表里面查询出数据
LOOP AT gt_alv_b WHERE checkbox = 'X'.
  AT NEW ebeln.
    SELECT
       ebeln
       ebelp
       kunnr
       matnr
       txz01
       menge
       meins
       prdat
    FROM ekpo INTO CORRESPONDING FIELDS OF TABLE gt_ekpo
      WHERE
      ebeln = gt_alv_b-ebeln AND
      loekz = ''.
    IF gt_ekpo IS NOT INITIAL.

      SELECT
        ebeln
        kunnr
       FROM ekpv INTO CORRESPONDING FIELDS OF TABLE gt_ekpv
        FOR ALL ENTRIES IN gt_ekpo
       WHERE
        ebeln = gt_ekpo-ebeln.

    ENDIF.
    IF gt_ekpv IS NOT INITIAL.
      SELECT
          kunnr
          name1
       FROM kna1 INTO CORRESPONDING FIELDS OF TABLE gt_kna1
        FOR ALL ENTRIES IN gt_ekpv
       WHERE
        kunnr = gt_ekpv-kunnr.
    ENDIF.
    APPEND LINES OF gt_ekpo TO gt_ekpo_b.
    APPEND LINES OF gt_ekpv TO gt_ekpv_b.
    APPEND LINES OF gt_kna1 TO gt_kna1_b.
  ENDAT.

ENDLOOP.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'ZMM005_SF'
*   VARIANT            = ' '
*   DIRECT_CALL        = ' '
  IMPORTING
    fm_name            = g_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
"打印设置
g_control_parameters-no_open   = 'X'.
g_control_parameters-no_close  = 'X'.
*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
g_output-tddest = 'LP01'.
*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
g_output-rqposname = ''.
g_output-tddataset = ''.
g_output-tdsuffix1 = ''.
g_output-tdsuffix2 = ''.
g_output-tdimmed   = 'X'.
g_output-tddelete  = 'X'.

CALL FUNCTION 'SSF_OPEN'
  EXPORTING
    control_parameters = g_control_parameters
    output_options     = g_output
*    IMPORTING
*   JOB_OUTPUT_OPTIONS = OPTION
  EXCEPTIONS
    formatting_error   = 1
    internal_error     = 2
    send_error         = 3
    user_canceled      = 4
    OTHERS             = 5.
APPEND LINES OF gt_ekpo_b TO gt_ekpo_c.

LOOP AT gt_alv_b WHERE checkbox = 'X'.
  "读取表头的数据
  AT NEW ebeln.
    READ TABLE gt_ekpo_b ASSIGNING <gs_ekpo_b> WITH  KEY ebeln = gt_alv_b-ebeln.
    IF sy-subrc EQ 0.
      zzs_tab-ebeln = <gs_ekpo_b>-ebeln.

      zzs_tab-ebeln = <gs_ekpo_b>-ebeln.    "合同编号

    ENDIF.
*****  采购订单号表头长文本   *****
    l_name = <gs_ekpo_b>-ebeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'F01'
        language                = '1'
        name                    = l_name
        object                  = 'EKKO'
      TABLES
        lines                   = gt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text IS NOT INITIAL.
      LOOP AT gt_text INTO gs_text.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
        zzs_tab-ztext = gs_text-tdline.
        CLEAR gs_text.
      ENDLOOP.
    ENDIF.
*----------------SUB数据---------------
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'F00'
        language                = '1'
        name                    = l_name
        object                  = 'EKKO'
      TABLES
        lines                   = gt_text_b
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF gt_text_b IS NOT INITIAL.
      LOOP AT gt_text_b INTO gs_text_b.
*            CONCATENATE LINES OF gt_text INTO gs_tab_print-zstan." SEPARATED BY '&'.
        zzs_tab-ztext_b = gs_text_b-tdline.
        CLEAR gs_text_b.

      ENDLOOP.
    ENDIF.
  ENDAT.
  LOOP AT gt_ekpo_c ASSIGNING <gs_ekpo_c>  WHERE ebeln = gt_alv_b-ebeln
                                            AND  ebelp = gt_alv_b-ebelp.
    IF <gs_ekpo_c>-ebelp = 10.
      zzs_tab-prdat = <gs_ekpo_c>-prdat.   "表头计划出货时间
      READ TABLE gt_ekpv_b ASSIGNING <gs_ekpv_b> WITH  KEY ebeln = <gs_ekpo_c>-ebeln.
      IF sy-subrc EQ 0.
        READ TABLE gt_kna1_b ASSIGNING <gs_kna1_b> WITH  KEY kunnr = <gs_ekpv_b>-kunnr.
        zzs_tab-name1 = <gs_kna1_b>-name1.   "表头客户名称
      ENDIF.
    ENDIF.

    gs_tab-matnr = <gs_ekpo_c>-matnr.
    gs_tab-txz01 = <gs_ekpo_c>-txz01.
    gs_tab-menge = <gs_ekpo_c>-menge.
    gs_tab-meins = <gs_ekpo_c>-meins.
    APPEND gs_tab TO gt_tab.
    CLEAR gs_tab.
  ENDLOOP.
  AT END OF ebeln.
**判断空行
*    gc_count = gc_count MOD gc_num.
*    IF gc_count NE 0.
*      gc_count = gc_num - gc_count.   " 空行数
*      CLEAR gs_tab_print.
*      DO gc_count TIMES.
*        APPEND gs_tab_print TO gt_tab_print.
*      ENDDO.
*    ENDIF.

*调用Smartforms的Function Module打印
    CALL FUNCTION g_fm_name
      EXPORTING
        control_parameters = g_control_parameters
        output_options     = g_output
        gs_head            = zzs_tab
        gc_num             = gc_num
        gc_type            = gc_type
      TABLES
        gt_item            = gt_tab
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4.
    CLEAR gt_tab.
  ENDAT.
ENDLOOP.
"#  关闭打印机设置
CALL FUNCTION 'SSF_CLOSE'
  IMPORTING
    job_output_info  = g_lw_ssfcrescl
  EXCEPTIONS
    formatting_error = 1
    internal_error   = 2
    send_error       = 3
    OTHERS           = 4.
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
