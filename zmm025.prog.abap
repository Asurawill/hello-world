*----------------------------------------------------------------------*
*  程序名称         : ZMM025
*  创建者           : 吴丽娟
*  创建日期         : 2015-09-18
*----------------------------------------------------------------------*
*  概要说明
* 金达采购合同
* 增加打印对话框长文本保存事项
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-09-18  HANDYWLJ                      创建
*2017-02-07  IT02&WEIYUN    ED1K905235     APPEND:打印对话框长文本保存事项
*2017-03-21  IT02&WEIYUN    ED1K905313     APPEND:类型4打印选项和格式1、2、3
"2017-04-14  IT02&WEIYUN    ED1K905344     APPEND：格式1、格式追加条款内容
"2017-04-25  IT02&WEIYUN   ED1K905432     优化：格式4模板大调整
"2017-05-03  IT02&WEIYUN   ED1K905449    优化：格式4表头换页
"及追加格式4尾注银行信息
"条款内容的变更
*----------------------------------------------------------------------*
REPORT zmm025.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
TABLES:ekko.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES:BEGIN OF ty_alvoutput,
        checkbox TYPE c,
        bsart    TYPE ekko-bsart, "采购订单类型
        ebeln    TYPE ekko-ebeln, "采购订单
        lifnr    TYPE ekko-lifnr, "供应商
        ekorg    TYPE ekko-ekorg, "采购组织
        ekgrp    TYPE ekko-ekgrp, "采购组
        aedat    TYPE ekko-aedat, "采购订单日期
        submi    TYPE ekko-submi,  "税率
        bukrs    TYPE ekko-bukrs,  "公司代码
        zterm    TYPE ekko-zterm,  "付款方式
        lands    TYPE ekko-lands, "国家
        ebelp    TYPE ekpo-ebelp, "项目
        banfn    TYPE ekpo-banfn, "采购申请
        matnr    TYPE ekpo-matnr, "物料编码
        txz01    TYPE ekpo-txz01, "物料描述
        menge    TYPE ekpo-menge, "数量
        meins    TYPE ekpo-meins, "单位
        adrnr    TYPE ekpo-adrnr,  "地址号
        name1    TYPE lfa1-name1, "供应商姓名
        knumv    TYPE ekko-knumv, "单据条件号
      END OF ty_alvoutput.
TYPES:BEGIN OF ty_name1,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1, "供应商姓名
      END OF ty_name1.
TYPES:BEGIN OF ty_yifang,
        lifnr      TYPE lfa1-lifnr,
        name1      TYPE lfa1-name1,
        stras      TYPE lfa1-stras,
        telf1      TYPE lfa1-telf1,
        telfx      TYPE lfa1-telfx,
        j_1kfrepre TYPE lfa1-j_1kfrepre,
      END OF ty_yifang.
TYPES:BEGIN OF ty_konv,
        kbetr TYPE konv-kbetr,
        kwert TYPE konv-kwert,
        kposn TYPE konv-kposn,
        ebeln TYPE ekpo-ebeln,
      END OF ty_konv.
TYPES:BEGIN OF ty_matnr,
        matnr TYPE mara-matnr,
        meins TYPE mara-meins,
      END OF ty_matnr.
TYPES:BEGIN OF ty_pspid,
        matnr TYPE ekpo-matnr,
        pspid TYPE proj-pspid,
      END OF ty_pspid.
TYPES:BEGIN OF ty_sjdm,
        matnr TYPE zmm024-matnr,
        posid TYPE zmm024-posid,
        sjdm  TYPE zmm024-sjdm,
      END OF ty_sjdm.
TYPES:BEGIN OF ty_table1,
        aedat      TYPE ekko-aedat,
        lifnr      TYPE ekko-lifnr,
        waers      TYPE ekko-waers,
        eknam      TYPE t024-eknam,
        tel_number TYPE t024-tel_number,
        ektel      TYPE t024-ektel,
      END OF ty_table1.
TYPES:BEGIN OF ty_jhdd,
        ebeln TYPE ekkn-ebeln,
        zxmdz TYPE proj-zxmdz,
        verna TYPE tcj04-verna,
      END OF ty_jhdd.
TYPES:BEGIN OF ty_s1,
        ps_psp_pnr TYPE ekkn-ps_psp_pnr,
        ebeln      TYPE ekkn-ebeln,
      END OF ty_s1.
TYPES:BEGIN OF ty_s2,
        psphi TYPE prps-psphi,
      END OF ty_s2.
TYPES:BEGIN OF ty_s3,
        vernr TYPE proj-vernr,
        zxmdz TYPE proj-zxmdz,
      END OF ty_s3.
TYPES:BEGIN OF ty_s4,
        vernr TYPE tcj04-vernr,
      END OF ty_s4.
TYPES:BEGIN OF ty_fkfs,
        text1 TYPE t052u-text1,
        ebeln TYPE ekko-ebeln,
      END OF ty_fkfs.
TYPES:BEGIN OF ty_zxmjldh,
        zxmjldh TYPE proj-zxmjldh,
        ebeln   TYPE ekkn-ebeln,
        ebelp   TYPE ekkn-ebelp,
      END OF ty_zxmjldh.
TYPES:BEGIN OF ty_banka,
        ebeln TYPE ekko-ebeln,
        banka TYPE bnka-banka,
      END OF ty_banka.

TYPES:BEGIN OF ty_bank,
        lifnr LIKE lfbk-lifnr,
        banks LIKE lfbk-banks,
        bankl LIKE lfbk-bankl,
        koinh LIKE lfbk-koinh,
        banka LIKE bnka-banka,
      END OF ty_bank.
DATA:gt_bank TYPE TABLE OF ty_bank,
     gs_bank TYPE ty_bank.
TYPES:BEGIN OF ty_knvk,
        lifnr LIKE knvk-lifnr,
        parnr LIKE knvk-parnr,
        name1 LIKE knvk-name1,
        telf1 LIKE knvk-telf1,
      END OF ty_knvk.

TYPES:BEGIN OF ty_hd_4,
        ebeln     TYPE ekko-ebeln,                   "SAP采购订单
        ddbh      TYPE string,                       "订单编号
        aedat     TYPE ekko-aedat,                   "创建日期
        dyrq      TYPE ekko-aedat,                   "打印日期
        post1     TYPE proj-post1,                       "项目名称
        lands     TYPE ekko-lands,                     "国家
        ekgrp     TYPE ekko-ekgrp,                   "采购组
        bukrs     TYPE ekko-bukrs,                   "甲方-编号
        butxt     TYPE t001-butxt,                   "甲方-名称
        submi     TYPE ekko-submi,                   "税率
        lifnr     TYPE ekko-lifnr,                   "乙方-编号
        name1     TYPE lfa1-name1,                   "乙方-名称
        cghtbh    TYPE string,                       "采购合同编号
        sjhj      TYPE ekpo-brtwr,                   "税价合计
        sjhj_dx   TYPE string,                       "税价合计-大写
        waers     TYPE konv-waers,                   "货币码
        zterm     TYPE ekko-zterm,                  "付款方式
        zterm_txt TYPE t052u-text1,                "付款方式-文本
        yfcdf     TYPE c LENGTH 20,                   "运费承担方
        zbqnx     TYPE c LENGTH 20,                    "质保期
        bctk      TYPE string,                        "补充条款
        jhdz      TYPE string,                        "交货地址
        shlxr     TYPE lfa1-name1,                   "收货联系人
        shlxdh    TYPE c LENGTH 20,                  "收货联系电话
        fjwb      TYPE string,                        "附件文本
        banka     TYPE bnka-banka,                    "开户行
        bankn     TYPE lfbk-koinh,                    "账号
        stceg     TYPE lfa1-stceg,                    "税号
        eknam     TYPE t024-eknam,                    "联系人-甲方
        ektel     TYPE t024-ektel,                    "电话-甲方
        lxr_yf    TYPE t024-eknam,                     "联系人-乙方
        dh_yf     TYPE t024-ektel,                     "电话-乙方
      END OF ty_hd_4 .

TYPES:BEGIN OF ty_mx_4,
        ebeln  TYPE ekko-ebeln,                     "采购订单
        ebelp  TYPE ekpo-ebelp,                     "采购项目
        banfn  TYPE ekpo-banfn,                     "采购申请
        adrnr  TYPE ekpo-adrnr,                     "地址号
        knumv  TYPE ekko-knumv,                      "单据条件号
        sjdm   TYPE zsjdm,                           "设计代码
        matnr  TYPE matnr,                          "物料编码
        txz01  TYPE ekpo-txz01,                     "物料描述
        meins  TYPE meins,                          "单位
        menge  TYPE menge_d,                        "数量
        kbetr  TYPE konv-kbetr,                     "含税单价
        kwert  TYPE konv-kwert,                     "税价合计
        pinpai TYPE string,                          "品牌
        eindt  TYPE eket-eindt,                     "交货日期
        waers  TYPE konv-waers,                     "货币码
        bz     TYPE string,                         "备注
      END OF ty_mx_4.

TYPES:BEGIN OF ty_hj_4,
        ebeln   TYPE ekpo-ebeln,                 "采购订单
        sjhj    TYPE ekpo-brtwr,                 "税价合计
        waers   TYPE konv-waers,                 "货币码
        sjhj_dx TYPE string,                     "税价合计-大写
      END OF ty_hj_4.



DATA:gt_hd_4 TYPE TABLE OF ty_hd_4,
     gs_hd_4 TYPE ty_hd_4.

DATA:gt_mx_4 TYPE TABLE OF ty_mx_4,
     gs_mx_4 TYPE ty_mx_4.

DATA:pt_hd_4 TYPE TABLE OF ty_hd_4,
     ps_hd_4 TYPE ty_hd_4.

DATA:pt_mx_4 TYPE TABLE OF ty_mx_4,
     ps_mx_4 TYPE ty_mx_4.

DATA:gt_knvk TYPE TABLE OF ty_knvk,
     gs_knvk TYPE ty_knvk.

DATA:gt_hj_4 TYPE TABLE OF ty_hj_4,
     gs_hj_4 TYPE ty_hj_4.

DATA:gt_eket TYPE TABLE OF eket,
     gs_eket TYPE eket.

DATA:gt_konv TYPE TABLE OF konv,
     gs_konv TYPE konv.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.

DATA:gt_t052u TYPE TABLE OF t052u,
     gs_t052u TYPE t052u.

DATA:gt_t024 TYPE TABLE OF t024,
     gs_t024 TYPE t024.

DATA:gt_ekpo TYPE TABLE OF ekpo,
     gs_ekpo TYPE ekpo.

DATA:gt_lfa1 TYPE TABLE OF lfa1,
     gs_lfa1 TYPE lfa1.

*DATA:gt_t007s type table of t007s,
*     gs_t007s type t007s.

DATA t_ftaxp TYPE TABLE OF ftaxp WITH HEADER LINE.

*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_head1       TYPE TABLE OF zmm025_head,
     wa_head1       TYPE zmm025_head,
     it_main1       TYPE TABLE OF zmm025_tab,
     wa_main1       TYPE zmm025_tab,
     it_end1        TYPE TABLE OF zmm025_end,
     wa_end1        TYPE zmm025_end,
     it_end2        TYPE TABLE OF zmm025_end2,
     wa_end2        TYPE zmm025_end2,
     wa_alvoutput   TYPE ty_alvoutput,
     it_alvoutput   TYPE TABLE OF ty_alvoutput,
     it_alvoutput_4 TYPE TABLE OF ty_alvoutput,
     sel_alvoutput  TYPE TABLE OF ty_alvoutput,
     it_name1       TYPE TABLE OF ty_name1,
     wa_name1       TYPE ty_name1,
     it_yifang      TYPE TABLE OF ty_yifang,
     wa_yifang      TYPE ty_yifang,
     it_konv        TYPE TABLE OF ty_konv,
     wa_konv        TYPE ty_konv,
     it_matnr       TYPE TABLE OF ty_matnr,
     wa_matnr       TYPE ty_matnr,
     it_pspid       TYPE TABLE OF ty_pspid,
     it_sjdm        TYPE TABLE OF ty_sjdm,
     wa_sjdm        TYPE ty_sjdm,
     it_table1      TYPE TABLE OF ty_table1,
     wa_table1      TYPE ty_table1,
     it_head2       TYPE TABLE OF zmm025_head,
     wa_head2       TYPE zmm025_head,
     it_jhdd        TYPE TABLE OF ty_jhdd,
     wa_jhdd        TYPE ty_jhdd,
     it_s1          TYPE TABLE OF ty_s1,
     wa_s1          TYPE ty_s1,
     it_s2          TYPE TABLE OF ty_s2,
     wa_s2          TYPE ty_s2,
     it_s3          TYPE TABLE OF ty_s3,
     wa_s3          TYPE ty_s3,
     it_s4          TYPE TABLE OF ty_s4,
     wa_s4          TYPE ty_s4,
     it_fkfs        TYPE TABLE OF ty_fkfs,
     wa_fkfs        TYPE ty_fkfs,
     it_zxmjldh     TYPE TABLE OF ty_zxmjldh,
     wa_zxmjldh     TYPE ty_zxmjldh,
     it_banka       TYPE TABLE OF ty_banka,
     wa_banka       TYPE ty_banka.

DATA:x_style1 TYPE c.
DATA:x_style2 TYPE c VALUE 'X'.
DATA:x_style3 TYPE c.
DATA:x_style4 TYPE c.
DATA:w(10) TYPE c VALUE '。'.
DATA:ok_code TYPE sy-ucomm,
     save_ok TYPE sy-ucomm.

DATA:control TYPE ssfctrlop.
DATA:job_output_info TYPE ssfcrescl.
DATA:l_tdname TYPE thead-tdname.
DATA:gt_ekkn LIKE TABLE OF ekkn WITH HEADER LINE.
DATA:gt_prps LIKE TABLE OF prps WITH HEADER LINE.
DATA:gt_proj LIKE TABLE OF proj WITH HEADER LINE.
DATA:gt_ekko LIKE TABLE OF ekko WITH HEADER LINE.

DATA:gt_adrc TYPE TABLE OF adrc,
     gs_adrc TYPE adrc.

DATA:gt_tcj04 TYPE TABLE OF tcj04,
     gs_tcj04 TYPE tcj04.

***---------------------------------------------------------------
* 定制控制 编辑长文本对象定义
DATA:container TYPE REF TO cl_gui_custom_container,
     editor    TYPE REF TO cl_gui_textedit,
     wa_thead  LIKE thead .  "用于存放save_text表头数据



DATA:init.
*     fjwb(256)      TYPE c OCCURS 0,
*     fjwb_line(256) TYPE c.

" 900 自定义屏幕供应商联系人、账号、开户行名定义
DATA:lxr         LIKE  knvk-name1,       "联系人
     zh          LIKE  lfbk-koinh,        "账号
     khh         LIKE  bnka-banka ,       "开户行
     lxdh        LIKE knvk-telf1,        "联系电话
     yfcdf       LIKE ekko-yfcdf,       "运费承担方
     fptgzhkzfqh LIKE ekko-fptg,  "发票提供在货款支付前后
     yfmfzbqnx   LIKE  ekko-zbnx,   "质保年限
     zlysjf      LIKE    ekko-bjzl,   "报检资料原件一式几份
     qhts        LIKE    ekko-fptg.   "发票提供在货款支付前后
*----------------------------------------------------------------*

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

*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln."采购订单
SELECT-OPTIONS: s_bsart FOR ekko-bsart."采购订单类型
SELECT-OPTIONS: s_aedat FOR ekko-aedat."采购订单日期
SELECT-OPTIONS: s_bukrs FOR ekko-bukrs."公司代码
SELECT-OPTIONS: s_ekorg FOR ekko-ekorg."采购组织
SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp."采购组
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr."供应商
SELECTION-SCREEN END OF BLOCK text.

*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.
  INCLUDE zinclude_cwb .  "存放处理长文本的代码

*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_getdata.
  PERFORM frm_dealdata.
  PERFORM frm_layout.
  PERFORM frm_fieldcat.
  PERFORM frm_output.

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
*****************************ALVOUTPUT********************************
  SELECT ekko~bsart"采购订单类型
         ekko~ebeln"采购订单
         ekko~lifnr"供应商
         ekko~ekorg"采购组织
         ekko~ekgrp"采购组
         ekko~bukrs "公司代码
         ekko~aedat"采购订单日期
         ekko~knumv "单据条件号
         ekko~zterm "付款条件
         ekko~lands "国家
         ekko~submi
         ekpo~ebelp"项目
         ekpo~matnr"物料编码
         ekpo~txz01"物料描述
         ekpo~menge"数量
         ekpo~meins"单位
         ekpo~banfn"采购申请
         ekpo~adrnr "地址号
   FROM  ekko
   JOIN  ekpo ON ekko~ebeln = ekpo~ebeln
   INTO CORRESPONDING FIELDS OF TABLE it_alvoutput
   WHERE ekko~ebeln IN s_ebeln
     AND ekko~bsart IN s_bsart
     AND ekko~aedat IN s_aedat
     AND ekko~bukrs IN s_bukrs
     AND ekko~ekorg IN s_ekorg
     AND ekko~ekgrp IN s_ekgrp
     AND ekko~lifnr IN s_lifnr
     AND ekpo~loekz <> 'L'.

  IF it_alvoutput IS INITIAL.
    MESSAGE 'No data!' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT lifnr"供应商
         name1"供应商姓名
    FROM lfa1
    INTO CORRESPONDING FIELDS OF TABLE it_name1
    FOR ALL ENTRIES IN it_alvoutput
    WHERE lifnr = it_alvoutput-lifnr.

  SELECT * INTO TABLE gt_t001
    FROM t001
    WHERE bukrs IN s_bukrs.
  SORT gt_t001 BY bukrs.
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
*****************************ALVOUTPUT********************************
  LOOP AT it_alvoutput INTO wa_alvoutput.
    READ TABLE it_name1 INTO wa_name1 WITH KEY lifnr = wa_alvoutput-lifnr.
    IF sy-subrc = 0.
      wa_alvoutput-name1 = wa_name1-name1.
    ENDIF.
    MODIFY it_alvoutput FROM wa_alvoutput.
    CLEAR wa_alvoutput.
  ENDLOOP.
  SORT it_alvoutput BY ebeln ebelp.
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
  wa_layout-box_fname = 'CHECKBOX'.
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
  init_fieldcat 'BSART' '采购订单类型' 'EKKO' 'BSART'.
  init_fieldcat 'EBELN' '采购订单' 'EKKO' 'EBELN'.
  init_fieldcat 'LIFNR' '供应商' 'EKKO' 'LIFNR'.
  init_fieldcat 'EKORG' '采购组织' 'EKKO' 'EKORG'.
  init_fieldcat 'EKGRP' '采购组' 'EKKO' 'EKGRP'.
  init_fieldcat 'AEDAT' '采购订单日期' 'EKKO' 'AEDAT'.
  init_fieldcat 'EBELP' '项目' 'EKPO' 'EBELP'.
  init_fieldcat 'MATNR' '物料编码' 'EKPO' 'MATNR'.
  init_fieldcat 'TXZ01' '物料描述' 'EKPO' 'TXZ01'.
  init_fieldcat 'MENGE' '数量' 'EKPO' 'MENGE'.
  init_fieldcat 'NAME1' '供应商姓名' 'LFA1' 'NAME1'.
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
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_alvoutput
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM user_command USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&PRINT'.
      CALL SCREEN 9000 STARTING AT 11 3 ENDING AT 80 15.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS_SET
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->FIELD1   TEXT
*      -->FIELD2   TEXT
*----------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATU9000'.
  "  CLEAR:FJWB,FJWB[].
* IF INIT IS INITIAL.
*    INIT = 'X'.
*     CREATE OBJECT :CONTAINER
*     EXPORTING CONTAINER_NAME = 'FJ'.
*
*     CREATE OBJECT EDITOR
*     EXPORTING
*       PARENT = CONTAINER
*       WORDWRAP_MODE = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
*       WORDWRAP_POSITION = 256
*       WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.
*
*  ENDIF.
*
*  CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
*      EXPORTING
*        TABLE = FJWB .
  "初始化容器对象
  PERFORM create_container_object USING container 'FJ'.
  "初始化文本编辑器
  PERFORM create_editor_object USING editor container .

*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.

    WHEN 'CANCEL'.
      CLEAR: x_style1,x_style2.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      " add the checking by it02 20160321 begin
      IF yfcdf EQ ''.
        MESSAGE '运费承担方为空不能打印，请往ME22N维护！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF fptgzhkzfqh EQ ''.
        MESSAGE '发票提供在货款支付前后为空不能打印，请往ME22N维护！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF yfmfzbqnx EQ 0.
        MESSAGE '乙方免费质保期年限为空不能打印，请往ME22N维护！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF zlysjf EQ ''.
        MESSAGE '报检资料原件一式几份为空不能打印，请往ME22N维护！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF fptgzhkzfqh EQ '后' AND qhts EQ 0.
        MESSAGE '甲方在支付货款后天数为空不能打印，请往ME22N维护！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF LXR EQ ''.
         MESSAGE '联系人不能为空，请补充！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

        IF LXDH EQ ''.
         MESSAGE '联系电话不能为空，请补充！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

         IF ZH EQ ''.
         MESSAGE '账号不能为空，请补充！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

         IF KHH EQ ''.
         MESSAGE  '开户行不能为空，请补充！' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      " add the checking by it02 20160321 end

      CASE 'X'.
        WHEN x_style1.
          LOOP AT it_alvoutput INTO wa_alvoutput WHERE checkbox EQ 'X'."将选中的EBELN告诉要打印的抬头的EBELN，可以用来进行判断需要打印多少次
            wa_head1-ebeln = wa_alvoutput-ebeln.
            wa_head1-ebelp = wa_alvoutput-ebelp.
            wa_head1-lifnr = wa_alvoutput-lifnr.
            APPEND wa_head1 TO it_head1.
            CLEAR:wa_head1,wa_alvoutput.
          ENDLOOP.
          "         PERFORM SELEKKO ."读取采购订单信息  IT02 15119
          IF it_head1 IS INITIAL.
            MESSAGE '请至少选中一行数据！' TYPE 'I' DISPLAY LIKE 'W'.
            LEAVE TO SCREEN 9000.
          ENDIF.

          SORT it_head1 BY ebeln ebelp lifnr.
          DELETE ADJACENT DUPLICATES FROM it_head1 COMPARING ebeln."删除重复的行，来保证每个采购凭证只打印一张


          PERFORM frm_head1."抬头数据的获取
          PERFORM frm_main1."表内容的获取
          SORT it_main1 BY ebeln ebelp.
          PERFORM frm_end1."表尾内容的获取
          PERFORM savevalue.
          PERFORM frm_print1."调用SMARTFORM进行打印


        WHEN x_style2.
          LOOP AT it_alvoutput INTO wa_alvoutput WHERE checkbox EQ 'X'."将选中的EBELN告诉要打印的抬头的EBELN，可以用来进行判断需要打印多少次
            wa_head1-ebeln = wa_alvoutput-ebeln.
            wa_head1-ebelp = wa_alvoutput-ebelp.
            wa_head1-lifnr = wa_alvoutput-lifnr.
            APPEND wa_head1 TO it_head1.
            CLEAR:wa_head1,wa_alvoutput.
          ENDLOOP.
          "    PERFORM SELEKKO ."读取采购订单信息  IT02 15119
          IF it_head1 IS INITIAL.
            MESSAGE '请至少选中一行数据！' TYPE 'I' DISPLAY LIKE 'W'.
            LEAVE TO SCREEN 9000.
          ENDIF.

          SORT it_head1 BY ebeln ebelp lifnr.
          DELETE ADJACENT DUPLICATES FROM it_head1 COMPARING ebeln."删除重复的行，来保证每个采购凭证只打印一张


          PERFORM frm_head1."抬头数据的获取
          PERFORM frm_main1."表内容的获取
          SORT it_main1 BY ebeln ebelp.
          PERFORM frm_end2."表尾内容的获取
          PERFORM savevalue2.
          PERFORM frm_print2."调用SMARTFORM进行打印

        WHEN x_style3.
          LOOP AT it_alvoutput INTO wa_alvoutput WHERE checkbox EQ 'X'."将选中的EBELN告诉要打印的抬头的EBELN，可以用来进行判断需要打印多少次
            wa_head1-ebeln = wa_alvoutput-ebeln.
            wa_head1-ebelp = wa_alvoutput-ebelp.
            wa_head1-lifnr = wa_alvoutput-lifnr.
            APPEND wa_head1 TO it_head1.
            CLEAR:wa_head1,wa_alvoutput.
          ENDLOOP.
          "   PERFORM SELEKKO ."读取采购订单信息  IT02 15119
          IF it_head1 IS INITIAL.
            MESSAGE '请至少选中一行数据！' TYPE 'I' DISPLAY LIKE 'W'.
            LEAVE TO SCREEN 9000.
          ENDIF.

          SORT it_head1 BY ebeln ebelp lifnr.
          DELETE ADJACENT DUPLICATES FROM it_head1 COMPARING ebeln."删除重复的行，来保证每个采购凭证只打印一张


          PERFORM frm_head1."抬头数据的获取
          PERFORM frm_main1."表内容的获取
          SORT it_main1 BY ebeln ebelp.
          PERFORM frm_end2."表尾内容的获取
          PERFORM savevalue2.
          PERFORM frm_print3."调用SMARTFORM进行打印
        WHEN x_style4.
          PERFORM frm_cl_4. "处理打印数据
          PERFORM frm_prt_4. "打印数据

      ENDCASE.
  ENDCASE.

  REFRESH:it_head1,it_main1,it_end1,it_end2,it_jhdd.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREAT_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE creat_listbox OUTPUT.

  TYPE-POOLS vrm.

  DATA:vid TYPE vrm_id VALUE 'YFCDF'.
  DATA:vlist TYPE vrm_values,
       value LIKE LINE OF vlist.

  CLEAR:value.
  REFRESH vlist.

  CLEAR value.
  MOVE '甲方' TO value-key.
  " MOVE '甲方' TO value-text.
  APPEND value TO vlist.

  CLEAR value.
  MOVE '乙方' TO value-key.
  "  MOVE '乙方' TO value-text.
  APPEND value TO vlist.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = vid
      values = vlist.
  CLEAR value.

  DATA:vid1 TYPE vrm_id VALUE 'FPTGZHKZFQH'.
  DATA:vlist1 TYPE vrm_values,
       value1 LIKE LINE OF vlist.

  CLEAR:value1.
  REFRESH vlist1.

  CLEAR value1.
  " MOVE 'A' TO value1-key.
  MOVE '前' TO value1-key.
  " MOVE '前' TO value1-text.
  APPEND value1 TO vlist1.

  CLEAR value1.
  " MOVE 'B' TO value1-key.
  MOVE '后' TO value1-key.
  "  MOVE '后' TO value1-text.
  APPEND value1 TO vlist1.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = vid1
      values = vlist1.
  CLEAR value1.

  DATA:vid2 TYPE vrm_id VALUE 'YFMFZBQNX'.
  DATA:vlist2 TYPE vrm_values,
       value2 LIKE LINE OF vlist.

  CLEAR:value2.
  REFRESH vlist2.

  CLEAR value2.
  MOVE '1' TO value2-key.
  MOVE '1' TO value2-text.
  APPEND value2 TO vlist2.

  CLEAR value2.
  MOVE '2' TO value2-key.
  MOVE '2' TO value2-text.
  APPEND value2 TO vlist2.

  CLEAR value2.
  MOVE '3' TO value2-key.
  MOVE '3' TO value2-text.
  APPEND value2 TO vlist2.

  CLEAR value2.
  MOVE '4' TO value2-key.
  MOVE '4' TO value2-text.
  APPEND value2 TO vlist2.

  CLEAR value2.
  MOVE '5' TO value2-key.
  MOVE '5' TO value2-text.
  APPEND value2 TO vlist2.

  CLEAR value2.
  MOVE '7' TO value2-key.
  MOVE '7' TO value2-text.
  APPEND value2 TO vlist2.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = vid2
      values = vlist2.
  CLEAR value2.

  DATA:vid3 TYPE vrm_id VALUE 'ZLYSJF'.
  DATA:vlist3 TYPE vrm_values,
       value3 LIKE LINE OF vlist.

  CLEAR:value3.
  REFRESH vlist3.

  CLEAR value3.
*  MOVE '1' TO value3-key.
*  MOVE '两份' TO value3-text.
  MOVE '两' TO value3-key.
  APPEND value3 TO vlist3.

  CLEAR value3.
*  MOVE '2' TO value3-key.
*  MOVE '三份' TO value3-text.
  MOVE '三' TO value3-key.
  APPEND value3 TO vlist3.

  CLEAR value3.
*  MOVE '3' TO value3-key.
*  MOVE '四份' TO value3-text.
  MOVE '四' TO value3-key.
  APPEND value3 TO vlist3.

  CLEAR value3.
*  MOVE '4' TO value3-key.
*  MOVE '八份' TO value3-text.
  MOVE '八' TO value3-key.
  APPEND value3 TO vlist3.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = vid3
      values = vlist3.
  CLEAR value3.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_HEAD1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_head1 .
  SELECT ekko~aedat
         ekko~lifnr
         ekko~waers
         t024~eknam
         t024~ektel
         t024~tel_number
       FROM  ekko
       JOIN  t024 ON ekko~ekgrp = t024~ekgrp
       INTO CORRESPONDING FIELDS OF TABLE it_table1    "根据有EBELN进行对其他数据的取数
       FOR ALL ENTRIES IN it_head1
       WHERE ekko~ebeln = it_head1-ebeln.

  IF it_head1 IS NOT INITIAL.
    SELECT lifnr name1
           stras telf1 telfx j_1kfrepre
      FROM lfa1
      INTO CORRESPONDING FIELDS OF TABLE it_yifang
      FOR ALL ENTRIES IN it_head1
      WHERE lifnr = it_head1-lifnr.
  ENDIF.

  TYPES:BEGIN OF ty_knvk,
          lifnr TYPE knvk-lifnr,
          name1 TYPE knvk-name1,
          telf1 TYPE knvk-telf1,
        END OF ty_knvk.

  DATA:it_knvk  TYPE TABLE OF ty_knvk,
       it_knvk1 TYPE TABLE OF ty_knvk,
       wa_knvk  TYPE ty_knvk,
       wa_knvk1 TYPE ty_knvk.

  IF it_yifang IS NOT INITIAL.

*    SELECT lifnr
*           name1
*           telf1
*      FROM knvk
*      INTO CORRESPONDING FIELDS OF TABLE it_knvk1
*      FOR ALL ENTRIES IN it_yifang
*      WHERE lifnr = it_yifang-lifnr
*      AND namev = 'Y'.
*
*    SELECT lifnr
*           name1
*           telf1
*      FROM knvk
*      INTO CORRESPONDING FIELDS OF TABLE it_knvk
*      FOR ALL ENTRIES IN it_yifang
*      WHERE lifnr = it_yifang-lifnr.

  ENDIF.

  DATA:l_js TYPE int4 VALUE 1.

  LOOP AT it_head1 INTO wa_head1.

    READ TABLE it_table1 INTO wa_table1 WITH KEY lifnr = wa_head1-lifnr.
    IF sy-subrc = 0.
      wa_head1-aedat = wa_table1-aedat.
      wa_head1-waers = wa_table1-waers.
      wa_head1-eknam = wa_table1-eknam.
      wa_head1-tel_number = wa_table1-tel_number.
      wa_head1-ektel = wa_table1-ektel.
    ENDIF.

    READ TABLE it_yifang INTO wa_yifang WITH KEY lifnr = wa_head1-lifnr.
    IF sy-subrc = 0.
      wa_head1-name1 = wa_yifang-name1.
      wa_head1-stras = wa_yifang-stras.
      wa_head1-telf1 = wa_yifang-telf1.
      wa_head1-telfx = wa_yifang-telfx.
      wa_head1-j_1kfrepre = wa_yifang-j_1kfrepre.
    ENDIF.

*    IF it_knvk1 IS NOT INITIAL.
*      LOOP AT it_knvk1 INTO wa_knvk1 WHERE lifnr = wa_head1-lifnr.
*        IF l_js = 1.
*        "  wa_head1-name2 = wa_knvk1-name1.
*       "    wa_head1-telf2 = wa_knvk1-telf1.
*        ELSE.
*          CONCATENATE '/' wa_knvk1-name1 INTO wa_head1-name2.
*          CONCATENATE '/' wa_knvk1-telf1 INTO wa_head1-telf2.
*        ENDIF.
*        CLEAR wa_knvk.
*        l_js = l_js + 1.
*      ENDLOOP.
*    ELSE.
*      READ TABLE it_knvk INTO wa_knvk WITH KEY lifnr = wa_head1-lifnr.
*      IF sy-subrc = 0.
*       " wa_head1-name2 = wa_knvk-name1.
*       " wa_head1-telf2 = wa_knvk-telf1.
*      ENDIF.
*    ENDIF.

    CLEAR l_js.
    l_js = 1.

    DATA lt_tline TYPE TABLE OF tline WITH HEADER LINE.
    DATA tdname TYPE thead-tdname.
    tdname = wa_head1-ebeln.
    CLEAR:lt_tline[].

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F06'
        language                = sy-langu
        name                    = tdname
        object                  = 'EKKO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      LOOP AT lt_tline.
        " wa_head1-htbh = lt_tline-tdline.
        CONCATENATE wa_head1-htbh lt_tline-tdline INTO wa_head1-htbh.
      ENDLOOP.
    ENDIF.
    "乙方联系人:更改为打印窗口的联系人 IT02 20160201
    wa_head1-name2 = lxr .
    wa_head1-telf2 = lxdh.
    MODIFY it_head1 FROM wa_head1.
    CLEAR wa_head1.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MAIN1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_main1 .


  LOOP AT it_alvoutput INTO wa_alvoutput WHERE checkbox EQ 'X'.
    wa_head2-ebeln = wa_alvoutput-ebeln.
    wa_head2-ebelp = wa_alvoutput-ebelp.
    APPEND wa_head2 TO it_head2.
    CLEAR:wa_head2,wa_alvoutput.
  ENDLOOP.

  SORT it_head2 BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM it_head2 COMPARING ebeln ebelp.

  SELECT ekpo~ebeln
         ekpo~ebelp"序号
         ekpo~banfn"采购申请
         ekpo~matnr"物料编码
         ekpo~txz01"物料描述
         ekpo~meins"单位
         ekpo~menge"数量
         "ekpo~mwskz"税码
         eket~eindt"交货日期
         ekko~submi
    FROM ekpo
    JOIN eket ON ekpo~ebeln = eket~ebeln AND ekpo~ebelp = eket~ebelp
    JOIN ekko ON eket~ebeln = ekko~ebeln
    INTO CORRESPONDING FIELDS OF TABLE it_main1
    FOR ALL ENTRIES IN it_head1
    WHERE ekpo~ebeln = it_head1-ebeln
    AND ekpo~ebelp = it_head1-ebelp
    AND eket~etenr = '1'.


  IF it_head2 IS NOT INITIAL.
    SELECT ekpo~ebeln
          ekpo~ebelp"序号
          ekpo~banfn"采购申请
          ekpo~matnr"物料编码
          ekpo~txz01"物料描述
          ekpo~meins"单位
          ekpo~menge"数量
          "ekpo~mwskz"税码
          eket~eindt"交货日期
          ekko~submi
      FROM ekpo
      JOIN eket ON ekpo~ebeln = eket~ebeln AND ekpo~ebelp = eket~ebelp
      JOIN ekko ON eket~ebeln = ekko~ebeln
      INTO CORRESPONDING FIELDS OF TABLE it_main1
      FOR ALL ENTRIES IN it_head2
      WHERE ekpo~ebeln = it_head2-ebeln
      AND ekpo~ebelp = it_head2-ebelp
      AND eket~etenr = '1'.
  ENDIF.

  IF it_main1 IS NOT INITIAL.
    SELECT konv~kbetr"含税单价
           konv~kwert
           konv~kposn
           ekpo~ebeln
      FROM konv
      JOIN ekko ON konv~knumv = ekko~knumv
      JOIN ekpo ON ekko~ebeln = ekpo~ebeln
      INTO CORRESPONDING FIELDS OF TABLE it_konv
      FOR ALL ENTRIES IN it_main1
      WHERE ekpo~ebeln = it_main1-ebeln
      "AND ekpo~ebelp = it_main1-ebelp
      AND ( konv~kschl = 'PB00'
      OR konv~kschl = 'PBXX' ).

*    SELECT ekpo~matnr proj~pspid
*      FROM ekpo
*      JOIN ekkn ON ekpo~ebelp = ekkn~ebelp
*      JOIN prps ON ekkn~ps_psp_pnr = prps~posid
*      JOIN proj ON prps~posid = proj~pspid
*      INTO CORRESPONDING FIELDS OF TABLE it_pspid
*      FOR ALL ENTRIES IN it_main1
*      WHERE ekkn~ebeln = it_main1-ebeln
*      AND ekkn~ebelp = it_main1-ebelp.
*       SELECT sjdm matnr    "设计代码
*      FROM zmm024
*      INTO CORRESPONDING FIELDS OF TABLE it_sjdm
*      FOR ALL ENTRIES IN it_pspid
*      WHERE matnr = it_pspid-matnr
*      AND posid = it_pspid-pspid.
    "MODIFIED IT02  151118 begin
    SELECT ebeln ebelp ps_psp_pnr
      INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
      FROM ekkn
      FOR ALL ENTRIES IN it_main1
      WHERE ebeln = it_main1-ebeln AND ebelp = it_main1-ebelp.
    SORT gt_ekkn BY ebeln ebelp ps_psp_pnr.
    IF gt_ekkn[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_prps
        FROM prps
        FOR ALL ENTRIES IN gt_ekkn
        WHERE pspnr  = gt_ekkn-ps_psp_pnr .
      SORT gt_prps BY pspnr.
      CHECK gt_prps[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_proj
        FROM proj
        FOR ALL ENTRIES IN gt_prps
        WHERE pspnr = gt_prps-psphi.
      SORT gt_proj BY pspnr.
      SELECT *      "r    "设计代码
        FROM zmm024
        INTO CORRESPONDING FIELDS OF TABLE it_sjdm
        FOR ALL ENTRIES IN it_main1
        WHERE matnr = it_main1-matnr .

      SORT it_sjdm BY matnr posid.
    ENDIF.
    "MODIFIED IT02  151118 end
  ENDIF.

  DATA w_ekpo TYPE ekpo.
  DATA w_ekko TYPE ekko.
  DATA t_ftaxp TYPE TABLE OF ftaxp WITH HEADER LINE.

  LOOP AT it_main1 INTO wa_main1.

    CLEAR l_tdname.
    CONCATENATE wa_main1-ebeln wa_main1-ebelp INTO l_tdname."备注

    DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
    DATA:l_sl TYPE int4 VALUE 1.
    CLEAR t_tline[].
    l_sl = 1.      "期初定义默认值1会在循环第二次不赋值  ，此重新循环赋值为 1  IT02 151123
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F01'
        language                = sy-langu
        name                    = l_tdname
        object                  = 'EKPO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        IF l_sl = 1.
          wa_main1-bz = t_tline-tdline.
        ELSE.
          IF t_tline-tdformat = '*'.
            CONCATENATE wa_main1-bz t_tline-tdline INTO wa_main1-bz SEPARATED BY space.
          ELSE.
            CONCATENATE wa_main1-bz t_tline-tdline INTO wa_main1-bz.
          ENDIF.
        ENDIF.
        CLEAR t_tline.
        l_sl = l_sl + 1.
      ENDLOOP.
      CLEAR l_sl.

      l_sl = 1.

    ENDIF.

    IF wa_main1-bz NE ''.
      CONCATENATE '备注：' wa_main1-bz INTO wa_main1-bz.
    ENDIF.

    LOOP AT it_konv INTO wa_konv WHERE ebeln = wa_main1-ebeln AND kposn = wa_main1-ebelp.

      wa_main1-kbetr = wa_konv-kbetr.
      wa_main1-kwert = wa_konv-kwert.

      CLEAR wa_konv.
    ENDLOOP.

*    LOOP AT it_sjdm INTO wa_sjdm WHERE matnr = wa_main1-matnr.
*
*      wa_main1-sjdm = wa_sjdm-sjdm.   "
*
*      CLEAR wa_sjdm.
*    ENDLOOP.
    " ADDED BY IT02 151118 BEGIN
    READ TABLE gt_ekkn WITH KEY ebeln = wa_main1-ebeln ebelp = wa_main1-ebelp BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE gt_prps WITH KEY pspnr = gt_ekkn-ps_psp_pnr   BINARY SEARCH .
      IF sy-subrc = 0 .
        READ TABLE gt_proj WITH KEY pspnr = gt_prps-psphi BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_sjdm INTO wa_sjdm WITH KEY  matnr = wa_main1-matnr posid = gt_proj-pspid.
          IF sy-subrc = 0.
            wa_main1-sjdm = wa_sjdm-sjdm .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
    " ADDED BY IT02 151118 END

    IF wa_main1-submi IS NOT INITIAL.
      wa_main1-mwskz = wa_main1-submi.
    ELSE.

      SELECT SINGLE *
        FROM ekpo
        INTO CORRESPONDING FIELDS OF w_ekpo
        WHERE ebeln EQ wa_main1-ebeln
        AND ebelp EQ wa_main1-ebelp
        AND loekz EQ space.

      SELECT SINGLE *
        FROM ekko
        INTO CORRESPONDING FIELDS OF w_ekko
        WHERE ebeln EQ wa_main1-ebeln.

      IF sy-subrc = 0.

        IF w_ekko-lands IS NOT INITIAL.

          CLEAR t_ftaxp[].
          CALL FUNCTION 'GET_TAX_PERCENTAGE'
            EXPORTING
              aland   = w_ekko-lands
              datab   = sy-datum
              mwskz   = w_ekpo-mwskz
              txjcd   = w_ekpo-txjcd
            TABLES
              t_ftaxp = t_ftaxp[].
          IF t_ftaxp[] IS NOT INITIAL.
            READ TABLE t_ftaxp INDEX 1.
            wa_main1-mwskz = t_ftaxp-kbetr / 10.
          ENDIF.
          REFRESH t_ftaxp[].
        ENDIF.
      ENDIF.

    ENDIF.

    CONDENSE wa_main1-mwskz NO-GAPS.

    CLEAR t_tline[].
    CLEAR l_sl.
    l_sl = 1 ."add it02 151123
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F06'
        language                = sy-langu
        name                    = l_tdname
        object                  = 'EKPO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        IF l_sl = 1.
          wa_main1-pinpai = t_tline-tdline.
        ELSE.
          IF t_tline-tdformat = '*'.
            CONCATENATE wa_main1-pinpai t_tline-tdline INTO wa_main1-pinpai SEPARATED BY space.
          ELSE.
            CONCATENATE wa_main1-pinpai t_tline-tdline INTO wa_main1-pinpai.
          ENDIF.
        ENDIF.
        CLEAR t_tline.
        l_sl = l_sl + 1.
      ENDLOOP.
      CLEAR l_sl.

      l_sl = 1.

    ENDIF.


    MODIFY it_main1 FROM wa_main1.
    CLEAR wa_main1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_END1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_end1 .
  IF it_head1 IS NOT INITIAL.
    SELECT lfa1~name1 AS jwname1
          "bnka~banka
           lfbk~koinh AS bankn
           lfa1~stceg
           ekko~ebeln
      FROM lfa1
      JOIN ekko ON lfa1~lifnr = ekko~lifnr
      LEFT JOIN lfbk ON ekko~lifnr = lfbk~lifnr
*      LEFT JOIN bnka ON lfbk~banks = bnka~banks
*      AND lfbk~bankl = bnka~bankl
      INTO CORRESPONDING FIELDS OF TABLE it_end1
      FOR ALL ENTRIES IN it_head1
      WHERE ekko~lifnr = it_head1-lifnr
      AND ekko~ebeln = it_head1-ebeln.

    SELECT ekko~ebeln
           bnka~banka
      FROM ekko
      JOIN lfbk ON ekko~lifnr = lfbk~lifnr
      JOIN bnka ON lfbk~banks = bnka~banks
      AND lfbk~bankl = bnka~bankl
      INTO CORRESPONDING FIELDS OF TABLE it_banka
      FOR ALL ENTRIES IN it_head1
      WHERE ekko~lifnr = it_head1-lifnr
      AND ekko~ebeln = it_head1-ebeln.


    SELECT proj~zxmdz
           tcj04~verna
           ekkn~ebeln
      FROM ekkn
      JOIN prps ON ekkn~ps_psp_pnr = prps~pspnr
      JOIN proj ON prps~psphi = proj~pspnr
      JOIN tcj04 ON proj~vernr = tcj04~vernr
      INTO CORRESPONDING FIELDS OF TABLE it_jhdd
      FOR ALL ENTRIES IN it_head1
      WHERE ekkn~ebeln = it_head1-ebeln.

    REFRESH it_fkfs.
    CLEAR wa_fkfs.

    SELECT t052u~text1
           ekko~ebeln
      FROM t052u
      JOIN ekko ON t052u~zterm = ekko~zterm
      INTO CORRESPONDING FIELDS OF TABLE it_fkfs
      FOR ALL ENTRIES IN it_head1
      WHERE ekko~lifnr = it_head1-lifnr.

    TYPES:BEGIN OF ty_loekz,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
          END OF ty_loekz.

    DATA:it_loekz  TYPE TABLE OF ty_loekz,
         it_loekz1 TYPE TABLE OF ty_loekz,
         wa_loekz  TYPE ty_loekz,
         wa_loekz1 TYPE ty_loekz.

    SELECT ebeln
           ebelp
      FROM ekpo
      INTO TABLE it_loekz
      FOR ALL ENTRIES IN it_main1
      WHERE ebelp = it_main1-ebelp
      AND ebeln = it_main1-ebeln
      AND loekz <> 'X'.

    SORT it_loekz BY ebeln ebelp.

    IF it_loekz IS NOT INITIAL.
      READ TABLE it_loekz INTO wa_loekz INDEX 1.
      IF sy-subrc = 0.
        wa_loekz1-ebeln = wa_loekz-ebeln.
        wa_loekz1-ebelp = wa_loekz-ebelp.
      ENDIF.
      APPEND wa_loekz1 TO it_loekz1.

      TYPES:BEGIN OF ty_street,
              ebeln  TYPE ekpo-ebeln,
              street TYPE adrc-street,
              city2  TYPE adrc-city2,
            END OF ty_street.

      DATA:it_street TYPE TABLE OF ty_street,
           wa_street TYPE ty_street.

      SELECT ekpo~ebeln
             adrc~street
             adrc~city2
        FROM adrc
        JOIN ekpo ON adrc~addrnumber = ekpo~adrnr
        INTO CORRESPONDING FIELDS OF TABLE it_street
        FOR ALL ENTRIES IN it_loekz1
        WHERE ekpo~ebelp = it_loekz1-ebelp
        AND ekpo~ebeln = it_loekz1-ebeln.

    ENDIF.
    DATA:a TYPE zmm025_end-vernr.
    DATA:b TYPE zmm025_end-dh.
    DATA:c(1) VALUE '-'.
    LOOP AT it_end1 INTO wa_end1.

*      READ TABLE it_banka INTO wa_banka WITH KEY ebeln = wa_end1-ebeln.
*      IF sy-subrc = 0.
*        wa_end1-banka = wa_banka-banka.
*      ENDIF.

      IF it_street IS NOT INITIAL.
        LOOP AT it_street INTO wa_street WHERE ebeln = wa_end1-ebeln.
          wa_end1-zxmdz = wa_street-street.
          SPLIT wa_street-city2 AT c INTO a b.
          wa_end1-vernr = a.
          wa_end1-dh = b.

          IF wa_end1-zxmdz IS INITIAL.
            READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end1-ebeln.
            IF sy-subrc = 0.
              wa_end1-zxmdz = wa_jhdd-zxmdz.
            ENDIF.
          ENDIF.

          IF wa_end1-vernr IS INITIAL.
            READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end1-ebeln.
            IF sy-subrc = 0.
              wa_end1-vernr = wa_jhdd-verna.
            ENDIF.
          ENDIF.
          CLEAR wa_jhdd.
          CLEAR wa_street.
          CLEAR:a,b.
        ENDLOOP.

      ELSE.
        READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end1-ebeln.
        IF sy-subrc = 0.
          wa_end1-zxmdz = wa_jhdd-zxmdz.
          wa_end1-vernr = wa_jhdd-verna.
        ENDIF.
      ENDIF.

      IF wa_end1-dh IS INITIAL.
        SELECT zxmjldh
               ekkn~ebeln
               ekkn~ebelp
          FROM proj
          JOIN prps ON proj~pspnr = prps~psphi
          JOIN ekkn ON prps~pspnr = ekkn~ps_psp_pnr
          INTO CORRESPONDING FIELDS OF TABLE it_zxmjldh
          FOR ALL ENTRIES IN it_main1
          WHERE ekkn~ebeln = it_main1-ebeln.

        READ TABLE it_zxmjldh INTO wa_zxmjldh WITH KEY ebeln = wa_end1-ebeln.
        IF sy-subrc = 0.
          wa_end1-dh = wa_zxmjldh-zxmjldh.
        ENDIF.
        CLEAR wa_zxmjldh.
        REFRESH it_zxmjldh.
      ENDIF.

      READ TABLE it_fkfs INTO wa_fkfs WITH KEY ebeln = wa_end1-ebeln.
      IF sy-subrc = 0.
        wa_end1-text1 = wa_fkfs-text1.
      ENDIF.

      IF wa_end1-text1 IS NOT INITIAL.
        CONCATENATE wa_end1-text1 w INTO wa_end1-text1.
      ENDIF.

      DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
      DATA:l_sl TYPE int4 VALUE 1.
      CLEAR t_tline[].
      DATA:tname1 TYPE thead-tdname.
      tname1 = wa_end1-ebeln.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'F07'
          language                = sy-langu
          name                    = tname1
          object                  = 'EKKO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = t_tline[]
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      IF t_tline[] IS NOT INITIAL.
        LOOP AT t_tline.
          IF l_sl = 1.
            wa_end1-bctk = t_tline-tdline.
          ELSE.
            CONCATENATE wa_end1-bctk t_tline-tdline INTO wa_end1-bctk.
          ENDIF.
          CLEAR t_tline.
          l_sl = l_sl + 1.
        ENDLOOP.
        CLEAR l_sl.

        l_sl = 1.

      ENDIF.

*      "附件文本值添加  IT02 151217 begin
*      REFRESH fjwb .
*      CALL METHOD editor->get_text_as_r3table
*        IMPORTING
*          table = fjwb.
*      LOOP AT fjwb INTO fjwb_line.
*        CONDENSE fjwb_line NO-GAPS.
*        IF wa_end1-fjwb IS INITIAL.
*          wa_end1-fjwb = fjwb_line .
*        ELSE.
*          CONCATENATE wa_end1-fjwb fjwb_line  INTO wa_end1-fjwb.
*        ENDIF.
*
*      ENDLOOP.
      " PERFORM  readtext USING editor wa_thead.

      "保存
      PERFORM savetext USING editor wa_thead.

      LOOP AT it_lines INTO wa_lines.
        IF wa_end1-fjwb IS INITIAL.
          wa_end1-fjwb = wa_lines-tdline .
        ELSE.
          CONCATENATE wa_end1-fjwb wa_lines-tdline  INTO wa_end1-fjwb.
        ENDIF.
      ENDLOOP.
      "附件文本值添加  IT02 151217 end
      "修改乙方开户行、账号 IT02 20160201
      wa_end1-banka = khh.  "
      wa_end1-bankn = zh.
      MODIFY it_end1 FROM wa_end1.
      CLEAR wa_end1.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print1 .


  DATA:lv_fmname TYPE rs38l_fnam,
       lw_output TYPE ssfcompop.


  control-no_open = 'X'.
  control-no_close = 'X'.
  lw_output-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
      output_options     = lw_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*     ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  "  LOOP AT it_head1 INTO wa_head1.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZJDCGHT'
    IMPORTING
      fm_name            = lv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION lv_fmname
    EXPORTING
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     ARCHIVE_PARAMETERS =
      control_parameters = control
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
      output_options     = lw_output
*     USER_SETTINGS      = 'X'
* IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO    =
*     JOB_OUTPUT_OPTIONS =
    TABLES
      it_head1           = it_head1
      it_main1           = it_main1
      it_end1            = it_end1
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.
  " ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*     error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH:it_head1,it_head2,it_main1,it_end1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_END2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_end2 .
  SELECT lfa1~name1 AS jwname1
        " bnka~banka
         lfbk~koinh AS bankn
         lfa1~stceg
         ekko~ebeln
    FROM lfa1
    JOIN ekko ON lfa1~lifnr = ekko~lifnr
    LEFT JOIN lfbk ON ekko~lifnr = lfbk~lifnr
   " JOIN bnka ON lfbk~banks = bnka~banks
    "         AND lfbk~bankl = bnka~bankl
    INTO CORRESPONDING FIELDS OF TABLE it_end2
    FOR ALL ENTRIES IN it_head1
    WHERE ekko~lifnr = it_head1-lifnr
    AND ekko~ebeln = it_head1-ebeln.

  SELECT ekko~ebeln
         bnka~banka
    FROM ekko
    JOIN lfbk ON ekko~lifnr = lfbk~lifnr
    JOIN bnka ON lfbk~banks = bnka~banks
    AND lfbk~bankl = bnka~bankl
    INTO CORRESPONDING FIELDS OF TABLE it_banka
    FOR ALL ENTRIES IN it_head1
    WHERE ekko~lifnr = it_head1-lifnr
    AND ekko~ebeln = it_head1-ebeln.

  SELECT proj~zxmdz
         tcj04~verna
         ekkn~ebeln
    FROM ekkn
    JOIN prps ON ekkn~ps_psp_pnr = prps~pspnr
    JOIN proj ON prps~psphi = proj~pspnr
    JOIN tcj04 ON proj~vernr = tcj04~vernr
    INTO CORRESPONDING FIELDS OF TABLE it_jhdd
    FOR ALL ENTRIES IN it_head1
    WHERE ekkn~ebeln = it_head1-ebeln.

  REFRESH it_fkfs.
  CLEAR wa_fkfs.

  SELECT t052u~text1
         ekko~ebeln
    FROM t052u
    JOIN ekko ON t052u~zterm = ekko~zterm
    INTO CORRESPONDING FIELDS OF TABLE it_fkfs
    FOR ALL ENTRIES IN it_head1
    WHERE ekko~lifnr = it_head1-lifnr.

  TYPES:BEGIN OF ty_loekz,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
        END OF ty_loekz.

  DATA:it_loekz  TYPE TABLE OF ty_loekz,
       it_loekz1 TYPE TABLE OF ty_loekz,
       wa_loekz  TYPE ty_loekz,
       wa_loekz1 TYPE ty_loekz.

  SELECT ebeln
         ebelp
    FROM ekpo
    INTO TABLE it_loekz
    FOR ALL ENTRIES IN it_main1
    WHERE ebelp = it_main1-ebelp
    AND ebeln = it_main1-ebeln
    AND loekz <> 'X'.

  IF it_loekz IS NOT INITIAL.
    READ TABLE it_loekz INTO wa_loekz INDEX 1.
    IF sy-subrc = 0.
      wa_loekz1-ebeln = wa_loekz-ebeln.
      wa_loekz1-ebelp = wa_loekz-ebelp.
    ENDIF.
    APPEND wa_loekz1 TO it_loekz1.

    TYPES:BEGIN OF ty_street,
            ebeln  TYPE ekpo-ebeln,
            street TYPE adrc-street,
            city2  TYPE adrc-city2,
          END OF ty_street.

    DATA:it_street TYPE TABLE OF ty_street,
         wa_street TYPE ty_street.

    SELECT ekpo~ebeln
           adrc~street
           adrc~city2
      FROM adrc
      JOIN ekpo ON adrc~addrnumber = ekpo~adrnr
      INTO CORRESPONDING FIELDS OF TABLE it_street
      FOR ALL ENTRIES IN it_loekz1
      WHERE ekpo~ebelp = it_loekz1-ebelp
      AND ekpo~ebeln = it_loekz1-ebeln.

  ENDIF.

  DATA:a TYPE zmm025_end-vernr.
  DATA:b TYPE zmm025_end-dh.
  DATA:c(1) VALUE '-'.
  LOOP AT it_end2 INTO wa_end2.

*    READ TABLE it_banka INTO wa_banka WITH KEY ebeln = wa_end2-ebeln.
*    IF sy-subrc = 0.
*      wa_end2-banka = wa_banka-banka.
*    ENDIF.

    IF it_street IS NOT INITIAL.
      LOOP AT it_street INTO wa_street WHERE ebeln = wa_end2-ebeln.
        wa_end2-zxmdz = wa_street-street.
        SPLIT wa_street-city2 AT c INTO a b.
        wa_end2-vernr = a.
        wa_end2-dh = b.
        CLEAR wa_street.
        CLEAR:a,b.

        IF wa_end2-zxmdz IS INITIAL.

          READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end2-ebeln.
          IF sy-subrc = 0.
            wa_end2-zxmdz = wa_jhdd-zxmdz.
          ENDIF.
        ENDIF.

        IF wa_end2-vernr IS INITIAL.
          READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end2-ebeln.
          IF sy-subrc = 0.
            wa_end2-vernr = wa_jhdd-verna.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.

      READ TABLE it_jhdd INTO wa_jhdd WITH KEY ebeln = wa_end2-ebeln.
      IF sy-subrc = 0.
        wa_end2-zxmdz = wa_jhdd-zxmdz.
        wa_end2-vernr = wa_jhdd-verna.
      ENDIF.

    ENDIF.

    IF wa_end2-dh IS INITIAL.
      SELECT zxmjldh
             ekkn~ebeln
             ekkn~ebelp
        FROM proj
        JOIN prps ON proj~pspnr = prps~psphi
        JOIN ekkn ON prps~pspnr = ekkn~ps_psp_pnr
        INTO CORRESPONDING FIELDS OF TABLE it_zxmjldh
        FOR ALL ENTRIES IN it_main1
        WHERE ekkn~ebeln = it_main1-ebeln.

      READ TABLE it_zxmjldh INTO wa_zxmjldh WITH KEY ebeln = wa_end2-ebeln.
      IF sy-subrc = 0.
        wa_end2-dh = wa_zxmjldh-zxmjldh.
      ENDIF.
      CLEAR wa_zxmjldh.
      REFRESH it_zxmjldh.
    ENDIF.

    READ TABLE it_fkfs INTO wa_fkfs WITH KEY ebeln = wa_end2-ebeln.
    IF sy-subrc = 0.
      wa_end2-text1 = wa_fkfs-text1.
    ENDIF.

    IF wa_end2-text1 IS NOT INITIAL.
      CONCATENATE wa_end2-text1 w INTO wa_end2-text1.
    ENDIF.

    DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
    DATA:l_sl TYPE int4 VALUE 1.
    CLEAR t_tline[].
    DATA:tname TYPE thead-tdname.
    tname = wa_end2-ebeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F07'
        language                = sy-langu
        name                    = tname
        object                  = 'EKKO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        IF l_sl = 1.
          wa_end2-bctk = t_tline-tdline.
        ELSE.
          CONCATENATE wa_end2-bctk t_tline-tdline INTO wa_end2-bctk.
        ENDIF.
        CLEAR t_tline.
        l_sl = l_sl + 1.
      ENDLOOP.
      CLEAR l_sl.

      l_sl = 1.

    ENDIF.

    "附件文本值添加  IT02 151217 begin
*    REFRESH fjwb .
*    CALL METHOD editor->get_text_as_r3table
*      IMPORTING
*        table = fjwb.
*    LOOP AT fjwb INTO fjwb_line.
*      CONDENSE fjwb_line NO-GAPS.
*      IF wa_end2-fjwb IS INITIAL.
*        wa_end2-fjwb = fjwb_line .
*      ELSE.
*        CONCATENATE wa_end2-fjwb fjwb_line  INTO wa_end2-fjwb.
*      ENDIF.
*
*    ENDLOOP.
    "保存
    PERFORM savetext USING editor wa_thead.
    LOOP AT it_lines INTO wa_lines.
      IF wa_end2-fjwb IS INITIAL.
        wa_end2-fjwb = wa_lines-tdline .
      ELSE.
        CONCATENATE wa_end2-fjwb wa_lines-tdline  INTO wa_end2-fjwb.
      ENDIF.
    ENDLOOP.
    "附件文本值添加  IT02 151217 end
    "修改乙方开户行、账号 IT02 20160201
    wa_end2-banka = khh.  "
    wa_end2-bankn = zh.
    MODIFY it_end2 FROM wa_end2.
    CLEAR wa_end2.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print2.

  DATA:lv_fmname TYPE rs38l_fnam,
       lw_output TYPE ssfcompop.


  control-no_open = 'X'.
  control-no_close = 'X'.
  lw_output-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
      output_options     = lw_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*     ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZJDCGHT2'
    IMPORTING
      fm_name            = lv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION lv_fmname
    EXPORTING
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     ARCHIVE_PARAMETERS =
      control_parameters = control
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
      output_options     = lw_output
*     USER_SETTINGS      = 'X'
*    IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO    =
*     JOB_OUTPUT_OPTIONS =
    TABLES
      it_head1           = it_head1
      it_main1           = it_main1
      it_end2            = it_end2
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*     error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH:it_head1,it_head2,it_main1,it_end2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SETVALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM savevalue.
  LOOP AT it_end1 INTO wa_end1.
    DATA: dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.
    DATA: name1(30) TYPE c.

    CLEAR: dynpfields, dynpfields[].
    dynpfields-fieldname = 'YFCDF'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'YFCDF'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end1-yfcdf =  name1 .
*    IF name1 = 1.
*      wa_end1-yfcdf = '甲方'.
*    ELSEIF name1 = 2.
*      wa_end1-yfcdf = '乙方'.
*    ENDIF.
*    IF WA_END1-YFCDF EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =  wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END1-YFCDF  = GT_EKKO-YFCDF ."  运费承担方
*               ENDIF.
*        ENDIF.
*    ENDIF.
    CONDENSE wa_end1-yfcdf NO-GAPS.
    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'FPTGZHKZFQH'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'FPTGZHKZFQH'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end1-fptgzhkzfqh = name1.

*    IF name1 = 'A'.
*      wa_end1-fptgzhkzfqh = '前'.
*    ELSEIF name1 = 'B'.
*      wa_end1-fptgzhkzfqh = '后'.
*    ENDIF.
*      IF WA_END1-fptgzhkzfqh EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =   wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END1-fptgzhkzfqh  = GT_EKKO-FPTG ."  发票提供在货款支付前后
*               ENDIF.
*        ENDIF.
*    ENDIF.

    CONDENSE  wa_end1-fptgzhkzfqh NO-GAPS.
    "ADD 增加前后天数 IT02 20160107 begin
    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'QHTS'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'QHTS'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    IF wa_end1-fptgzhkzfqh EQ '后'.
      IF name1 NE 0.
        CONCATENATE  name1 '天内'  INTO wa_end1-qhts.
      ENDIF.
    ENDIF.
    CONDENSE  wa_end1-qhts NO-GAPS.
    "ADD 增加前后天数 IT02 20160107 end

    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'YFMFZBQNX'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'YFMFZBQNX'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end1-yfmfzbqnx = name1.


*     IF WA_END1-yfmfzbqnx EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =  wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END1-yfmfzbqnx = GT_EKKO-ZBNX ."  乙方免费质保年限
*               ENDIF.
*        ENDIF.
*    ENDIF.
*    CONDENSE WA_END1-yfmfzbqnx NO-GAPS.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wa_end1-yfmfzbqnx
*    IMPORTING
*      output = wa_end1-yfmfzbqnx.
    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'ZLYSJF'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'ZLYSJF'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.
    wa_end1-zlysjf = name1.
*    IF name1 = 1.
*      wa_end1-zlysjf = '两'.
*    ELSEIF name1 = 2.
*      wa_end1-zlysjf = '三'.
*    ELSEIF name1 = 3.
*      wa_end1-zlysjf = '四'.
*    ELSEIF name1 = 4.
*      wa_end1-zlysjf = '八'.
*    ENDIF.
*   IF WA_END1-zlysjf EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =   wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END1-zlysjf = GT_EKKO-BJZL ."  报检资料一式几份
*               ENDIF.
*        ENDIF.
*    ENDIF.
    CONDENSE wa_end1-zlysjf NO-GAPS.
    MODIFY it_end1 FROM wa_end1.
    CLEAR wa_end1.
    CLEAR:dynpfields, dynpfields[],name1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVEVALUE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM savevalue2 .
  LOOP AT it_end2 INTO wa_end2.
    DATA: dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.
    DATA: name1(30) TYPE c.

    CLEAR: dynpfields, dynpfields[].
    dynpfields-fieldname = 'YFCDF'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'YFCDF'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end2-yfcdf = name1.
*    IF name1 = 1.
*      wa_end2-yfcdf = '甲方'.
*    ELSEIF name1 = 2.
*      wa_end2-yfcdf = '乙方'.
*    ENDIF.

*     IF WA_END2-YFCDF EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =  wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END2-YFCDF  = GT_EKKO-YFCDF ."  运费承担方
*               ENDIF.
*        ENDIF.
*    ENDIF.
    CONDENSE wa_end2-yfcdf NO-GAPS.
    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'FPTGZHKZFQH'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'FPTGZHKZFQH'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end2-fptgzhkzfqh =  name1.
*    IF name1 = 'A'.
*      wa_end2-fptgzhkzfqh = '前'.
*    ELSEIF name1 = 'B'.
*      wa_end2-fptgzhkzfqh = '后'.
*    ENDIF.
*     IF WA_END2-fptgzhkzfqh EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =   wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END2-fptgzhkzfqh  = GT_EKKO-FPTG ."  发票提供在货款支付前后
*               ENDIF.
*        ENDIF.
*    ENDIF.
    CONDENSE  wa_end2-fptgzhkzfqh NO-GAPS.

    "ADD 增加前后天数 IT02 20160107 begin
    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'QHTS'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'QHTS'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    IF wa_end2-fptgzhkzfqh EQ '后'.
      IF name1 NE 0.
        CONCATENATE  name1 '天内'  INTO wa_end2-qhts.
      ENDIF.
    ENDIF.
    CONDENSE  wa_end1-qhts NO-GAPS.
    "ADD 增加前后天数 IT02 20160107 end

    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'YFMFZBQNX'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'YFMFZBQNX'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end2-yfmfzbqnx = name1.
*      IF WA_END2-yfmfzbqnx EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =  wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END2-yfmfzbqnx = GT_EKKO-ZBNX ."  乙方免费质保年限
*               ENDIF.
*        ENDIF.
*    ENDIF.
*    CONDENSE WA_END2-yfmfzbqnx NO-GAPS.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = WA_END2-yfmfzbqnx
*    IMPORTING
*      output = WA_END2-yfmfzbqnx.

    CLEAR:dynpfields, dynpfields[],name1.

    dynpfields-fieldname = 'ZLYSJF'. "填入需要读值的字段名
    APPEND dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = dynpfields
      EXCEPTIONS
        OTHERS             = 9.
    IF sy-subrc = 0.
      READ TABLE dynpfields WITH KEY fieldname = 'ZLYSJF'.
      name1 = dynpfields-fieldvalue. "获取到的值
    ENDIF.

    wa_end2-zlysjf = name1.
*    IF name1 = 1.
*      wa_end2-zlysjf = '两'.
*    ELSEIF name1 = 2.
*      wa_end2-zlysjf = '三'.
*    ELSEIF name1 = 3.
*      wa_end2-zlysjf = '四'.
*    ELSEIF name1 = 4.
*      wa_end2-zlysjf = '八'.
*    ENDIF.
*    IF WA_END2-zlysjf EQ ''.
*       READ TABLE IT_MAIN1 INTO wa_main1 INDEX 1.
*        IF SY-SUBRC = 0.
*             READ TABLE GT_EKKO WITH KEY EBELN =   wa_main1-EBELN .
*              IF SY-SUBRC = 0.
*                  WA_END2-zlysjf = GT_EKKO-BJZL ."  报检资料一式几份
*               ENDIF.
*        ENDIF.
*    ENDIF.
    CONDENSE wa_end2-zlysjf NO-GAPS.

    MODIFY it_end2 FROM wa_end2.
    CLEAR wa_end2.
    CLEAR:dynpfields, dynpfields[],name1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print3 .

  DATA:lv_fmname TYPE rs38l_fnam,
       lw_output TYPE ssfcompop.


  control-no_open = 'X'.
  control-no_close = 'X'.
  lw_output-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
      output_options     = lw_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*     ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZJDCGHT3'
    IMPORTING
      fm_name            = lv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION lv_fmname
    EXPORTING
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     ARCHIVE_PARAMETERS =
      control_parameters = control
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
      output_options     = lw_output
*     USER_SETTINGS      = 'X'
*    IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO    =
*     JOB_OUTPUT_OPTIONS =
    TABLES
      it_head1           = it_head1
      it_main1           = it_main1
      it_end2            = it_end2
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*     error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH:it_head1,it_head2,it_main1,it_end2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELEKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SELEKKO .
* SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
*   FROM EKKO
*   FOR ALL ENTRIES IN IT_HEAD1
*   WHERE EBELN = IT_HEAD1-EBELN.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  LXRF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lxrf4 INPUT.

  DATA:selindex TYPE ddshmarks .
*DATA:RETURN_TAB   TYPE TABLE OF  DDSHRETVAL ,
*
*     FIELD_TAB TYPE TABLE OF DFIES,
*     DYNPFLD_MAPPING TYPE TABLE OF DSELC.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'NAME1'
*     PVALKEY     = ' '
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'LXR'
*     STEPL       = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
 "    MARK_TAB    = SELINDEX
* IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = gt_knvk
*     FIELD_TAB   = FIELD_TAB
*     RETURN_TAB  = RETURN_TAB
*     DYNPFLD_MAPPING        =  DYNPFLD_MAPPING
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZHF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zhf4 INPUT.

  " DATA:GS_SEL TYPE ty_alvoutput .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'KOINH'
*     PVALKEY     = ' '
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ZH'
      "*   STEPL                  = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB    =
* IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = gt_bank
*     FIELD_TAB   =
*     RETURN_TAB  =
*     DYNPFLD_MAPPING        =
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
*  READ TABLE sel_alvoutput into GS_SEL  index 1.
* IF SY-SUBRC = 0 .
*     READ TABLE GT_BANK WITH KEY LIFRN
* ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INI900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ini900 OUTPUT.
*  初始选中900 屏幕字段基础数据值 （供应商列表 账号、开户行、联系人）
  " DATA:INI_900 TYPE TABLE OF dynpread WITH HEADER LINE.
  REFRESH sel_alvoutput.
  CLEAR:zh,khh,lxr.
  sel_alvoutput = it_alvoutput .
  SORT sel_alvoutput BY lifnr.
  DELETE sel_alvoutput WHERE checkbox NE 'X'.
  DELETE ADJACENT DUPLICATES FROM sel_alvoutput COMPARING lifnr .
  "查询附件文本
  "将指定位置的长文本数据读取到文本编辑器中
  READ TABLE sel_alvoutput INTO wa_alvoutput INDEX 1 .
  IF sy-subrc EQ 0 .
    CLEAR:wa_thead .
    wa_thead-tdobject = 'Z001'.  "文本对象
    wa_thead-tdid = 'ZT01'.      "文本对应ID
    wa_thead-tdname  =  wa_alvoutput-ebeln . "文本对象名称 ：以采购订单号存储
    wa_thead-tdspras = sy-langu.
    PERFORM  readtext USING editor wa_thead.
    CLEAR:wa_alvoutput.
  ENDIF.


* 取选中行对应的银行账号信息
  SELECT lfbk~lifnr lfbk~banks  lfbk~bankl lfbk~koinh bnka~banka
    INTO CORRESPONDING FIELDS OF TABLE gt_bank
    FROM lfbk
    LEFT JOIN bnka
    ON lfbk~banks = bnka~banks
    AND lfbk~bankl = bnka~bankl
    FOR ALL ENTRIES IN sel_alvoutput
    WHERE lifnr = sel_alvoutput-lifnr.
  SORT gt_bank BY lifnr banks bankl koinh banka.
  READ TABLE gt_bank INTO gs_bank INDEX 1.
  IF sy-subrc = 0.
    zh = gs_bank-koinh .  "账号
    khh = gs_bank-banka.  "开户行
  ENDIF.
  "取供应商联系人信息
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE gt_knvk
    FROM knvk
    FOR ALL ENTRIES IN sel_alvoutput
    WHERE lifnr = sel_alvoutput-lifnr.
  SORT gt_knvk BY lifnr name1.
  READ TABLE gt_knvk INTO gs_knvk INDEX 1.
  IF sy-subrc = 0 .
    lxr = gs_knvk-name1.   "联系人
    lxdh = gs_knvk-telf1.   "联系电话
  ENDIF.

  "    PERFORM SELEKKO ."读取采购订单信息  IT02 15119
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ekko
    FROM ekko
    FOR ALL ENTRIES IN sel_alvoutput
    WHERE ebeln = sel_alvoutput-ebeln.

  SORT gt_ekko BY ebeln .
  READ TABLE gt_ekko INDEX 1.
  IF sy-subrc = 0 .
    yfcdf       = gt_ekko-yfcdf. "运费承担方
    fptgzhkzfqh = gt_ekko-fptg. "发票提供在货款支付前后
    yfmfzbqnx   = gt_ekko-zbnx. "质保年限
    "去掉前导零
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = yfmfzbqnx
      IMPORTING
        output = yfmfzbqnx.

    zlysjf      = gt_ekko-bjzl. "报检资料原件一式几份

    qhts        = gt_ekko-qhts.  "发票提供在货款支付前后
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = qhts
      IMPORTING
        output = qhts.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  KHHF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE khhf4 INPUT.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'BANKA'
*     PVALKEY     = ' '
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'KHH'
      "*   STEPL                  = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB    =
* IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = gt_bank
*     FIELD_TAB   =
*     RETURN_TAB  =
*     DYNPFLD_MAPPING        =
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
*  READ TABLE sel_alvoutput into GS_SEL  index 1.
* IF SY-SUBRC = 0 .
*     READ TABLE GT_BANK WITH KEY LIFRN
* ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LXDH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lxdh INPUT.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'TELF1'
*     PVALKEY     = ' '
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'LXDH'
*     STEPL       = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
 "    MARK_TAB    = SELINDEX
* IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = gt_knvk
*     FIELD_TAB   =
*     RETURN_TAB  =
*     DYNPFLD_MAPPING        =
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CONTAINER  text
*      -->P_1162   text
*----------------------------------------------------------------------*
FORM create_container_object USING p_container TYPE REF TO cl_gui_custom_container p_container_name.
  CHECK p_container IS INITIAL.
  CREATE OBJECT p_container   "初始化容器对象
    EXPORTING
      container_name              = p_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  CREATE_EDITOR_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EDITOR  text
*      -->P_CONTAINER  text
*----------------------------------------------------------------------*
FORM create_editor_object USING p_editor TYPE REF TO cl_gui_textedit
                                p_container TYPE REF TO cl_gui_custom_container .

  CHECK p_editor IS INITIAL .
  CREATE OBJECT p_editor "初始文本编辑器对象
    EXPORTING
      parent                     = p_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 41  "c_line_length
      wordwrap_to_linebreak_mode = cl_gui_textedit=>false
      max_number_chars           = 100000
    EXCEPTIONS
      OTHERS                     = 1.

ENDFORM . "create_editor_object_cu
*&---------------------------------------------------------------------*
*&      Form  SELBZ_XX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TDNAME  text
*      -->P_1906   text
*      -->P_1907   text
*      <--P_GS_HD_4_DDBH  text
*----------------------------------------------------------------------*
FORM selbz_xx  USING    p_tdname TYPE thead-tdname
                        p_object TYPE thead-tdobject
                        p_id  TYPE thead-tdid
                        CHANGING p_xx TYPE string .

  " 取长文本信息
  CLEAR:p_xx.
  REFRESH:it_lines.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = p_id
      language                = sy-langu
      name                    = p_tdname
      object                  = p_object
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
    LOOP AT it_lines INTO wa_lines .
      IF p_xx IS INITIAL.
        p_xx = wa_lines-tdline.
      ELSE.
        CONCATENATE p_xx   wa_lines-tdline INTO p_xx.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONV_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_HD_4_SJHJ  text
*      <--P_GS_HD_4_SJHJ_DX  text
*----------------------------------------------------------------------*
FORM conv_amount USING VALUE(f_source)
                 CHANGING VALUE(f_result).
  DATA: scr(30) TYPE c, res(60) TYPE c,fen(2) TYPE c .
  DATA: len TYPE i, c1 TYPE i, c2 TYPE i, c3 TYPE i, c4 TYPE i.
  DATA: d1(1) TYPE c, d2(1) TYPE c, d3 TYPE i.
  DATA: digit(2)  TYPE c, weight(2) TYPE c.
  DATA: rule1(20) TYPE c VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: rule2(30) TYPE c VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  scr = f_source * 100.
  CONDENSE scr NO-GAPS.
  IF scr = '0'.
    res = '零元'.
  ELSE.
    len = strlen( scr ).
    c1 = 0.
    d1 = '0'.
    CLEAR res.
    DO len TIMES.
      c1 = c1 + 1.
      c2 = len - c1.
      d2 = scr+c2(1) .
      IF d2 = '0'.
        d3 = 0.
      ELSE.
        d3 = d2.
      ENDIF.
      digit = rule1+d3(1) .
      c3 = ( c1 - 1 ) .
      weight = rule2+c3(1) .
      IF d2 = '0'.
        IF c1 = 3.
          digit = ''.
        ELSEIF c1 = 7.
          digit = ''.
          IF len > 10 .
            c4 = len - 10.
            IF scr+c4(4) = '0000'.
              weight = ''.
            ENDIF.
          ENDIF.
        ELSEIF c1 = 11.
          digit = ''.
        ELSEIF d1 = '0'.
          digit = ''.
          weight = ''.
        ELSE.
          weight = ''.
        ENDIF.
      ENDIF.
      CONCATENATE digit weight res INTO res .
      d1 = d2.
    ENDDO.
  ENDIF.
  len = strlen( res ) - 1.
  fen = res+len(1).
  IF fen <> '分' .
    CONCATENATE res '整' INTO f_result.
  ELSE.
    f_result = res.
  ENDIF.
ENDFORM.                    "conv_amount
*&---------------------------------------------------------------------*
*&      Form  FRM_CL_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_cl_4 .
  DATA: dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.
  DATA: name1(30) TYPE c.
  REFRESH: it_alvoutput_4.
  it_alvoutput_4 = it_alvoutput .
  SORT it_alvoutput_4 BY ebeln.
  DELETE it_alvoutput_4 WHERE checkbox NE 'X'.
  "gt_hd_4:存储打印采购订单抬头相关信息
  REFRESH:gt_hd_4.
  MOVE-CORRESPONDING it_alvoutput_4 TO gt_hd_4 .
  SORT  gt_hd_4 BY ebeln .
  DELETE ADJACENT DUPLICATES FROM gt_hd_4 COMPARING ebeln.

  "查询供应商信息
  REFRESH:gt_lfa1.
  SELECT * INTO TABLE gt_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN gt_hd_4
    WHERE lifnr = gt_hd_4-lifnr.

  SORT gt_lfa1 BY lifnr.

  "查询采购明细数据
  REFRESH:gt_ekpo.
  SELECT * INTO TABLE gt_ekpo
    FROM ekpo
    FOR ALL ENTRIES IN gt_hd_4
    WHERE ebeln = gt_hd_4-ebeln
    AND   loekz EQ ''
    AND   mwskz NE ''.

  SORT gt_ekpo BY ebeln ebelp.
  "gt_mx_4:存储打印采购明细数据
  REFRESH:gt_mx_4.
  MOVE-CORRESPONDING it_alvoutput_4 TO gt_mx_4 .
  SORT  gt_mx_4 BY ebeln ebelp .

  REFRESH:gt_eket.
  IF gt_mx_4 IS NOT INITIAL.
    "查询地址信息
    REFRESH:gt_adrc.
    SELECT * INTO TABLE gt_adrc
      FROM adrc
      FOR ALL ENTRIES IN gt_mx_4
      WHERE addrnumber = gt_mx_4-adrnr..
    SORT gt_adrc BY addrnumber .
    "查询采购账户分配信息
    REFRESH:gt_ekkn.
    SELECT *
    INTO TABLE gt_ekkn
    FROM ekkn
    FOR ALL ENTRIES IN gt_mx_4
    WHERE ebeln = gt_mx_4-ebeln AND ebelp = gt_mx_4-ebelp.
    SORT gt_ekkn BY ebeln ebelp ps_psp_pnr.
    IF gt_ekkn[] IS NOT INITIAL.
      "查询工作分解结构信息WBS信息
      REFRESH:gt_prps.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_prps
        FROM prps
        FOR ALL ENTRIES IN gt_ekkn
        WHERE pspnr  = gt_ekkn-ps_psp_pnr .
      SORT gt_prps BY pspnr.
      IF gt_prps[] IS NOT INITIAL.
        "查询项目信息
        REFRESH:gt_proj.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_proj
         FROM proj
         FOR ALL ENTRIES IN gt_prps
         WHERE pspnr = gt_prps-psphi.
        SORT gt_proj BY pspnr.
        " 查询设计代码信息
        REFRESH:it_sjdm.
        SELECT *      "r
          FROM zmm024
          INTO CORRESPONDING FIELDS OF TABLE it_sjdm
          FOR ALL ENTRIES IN it_main1
          WHERE matnr = it_main1-matnr .

        SORT it_sjdm BY matnr posid.

        "查询联系人信息
        REFRESH:gt_tcj04.
        SELECT * INTO TABLE gt_tcj04
          FROM tcj04  .
        SORT gt_tcj04 BY vernr .
      ENDIF.
    ENDIF.

    "查询计划值
    REFRESH:gt_eket.
    SELECT * INTO TABLE gt_eket
    FROM eket
    FOR ALL ENTRIES IN gt_mx_4
    WHERE ebeln = gt_mx_4-ebeln
      AND ebelp = gt_mx_4-ebelp
      AND etenr = '1'.
    SORT gt_eket BY ebeln ebelp .

    "查询条件价格信息
    REFRESH:gt_konv.
    SELECT * INTO TABLE gt_konv
      FROM konv
      FOR ALL ENTRIES IN gt_mx_4
      WHERE knumv = gt_mx_4-knumv
      AND  kschl IN ( 'PB00', 'PBXX' )
      AND kinak EQ space
      AND stunr EQ '1' .
    SORT gt_konv BY knumv kposn.

    "查询付款条件代码信息
    REFRESH:gt_t052u.
    SELECT * INTO TABLE gt_t052u
      FROM t052u
      WHERE spras = '1' .
    SORT gt_t052u BY zterm .

    REFRESH:gt_hj_4.
    LOOP AT gt_mx_4 INTO gs_mx_4.
      "交货日期
      READ TABLE gt_eket INTO gs_eket WITH KEY ebeln = gs_mx_4-ebeln
                                               ebelp = gs_mx_4-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_mx_4-eindt  = gs_eket-eindt.
      ENDIF.
      "含税单价
      READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_mx_4-knumv
                                               kposn+1(5) = gs_mx_4-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_mx_4-kbetr = gs_konv-kbetr / gs_konv-kpein.  "含税单价
        gs_mx_4-kwert = gs_konv-kwert.  "税价合计
        gs_mx_4-waers = gs_konv-waers.  "货币码
      ENDIF.
      "设计代码
      READ TABLE gt_ekkn WITH KEY ebeln = gs_mx_4-ebeln ebelp = gs_mx_4-ebelp BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE gt_prps WITH KEY pspnr = gt_ekkn-ps_psp_pnr   BINARY SEARCH .
        IF sy-subrc = 0 .
          READ TABLE gt_proj WITH KEY pspnr = gt_prps-psphi BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE it_sjdm INTO wa_sjdm WITH KEY  matnr = gs_mx_4-matnr posid = gt_proj-pspid.
            IF sy-subrc = 0.
              gs_mx_4-sjdm = wa_sjdm-sjdm .

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
      "品牌
      CLEAR:l_tdname.
      CONCATENATE gs_mx_4-ebeln gs_mx_4-ebelp INTO l_tdname .
      PERFORM selbz_xx USING l_tdname 'EKPO' 'F06' CHANGING gs_mx_4-pinpai.
      "备注
      PERFORM selbz_xx USING l_tdname 'EKPO' 'F01' CHANGING gs_mx_4-bz.


      MODIFY gt_mx_4 FROM gs_mx_4.
      CLEAR:gs_hj_4.
      "按采购订单合计
      gs_hj_4-ebeln = gs_mx_4-ebeln. "采购订单
      gs_hj_4-sjhj = gs_mx_4-kwert. "税价合计
      gs_hj_4-waers = gs_mx_4-waers. "货币码
      COLLECT gs_hj_4 INTO gt_hj_4.
    ENDLOOP.
    SORT gt_hj_4 BY ebeln .
    "查询采购组信息
    SELECT * INTO TABLE gt_t024
      FROM t024 .
    SORT gt_t024 BY ekgrp .
    LOOP AT gt_hd_4 INTO gs_hd_4.
      "公司名称
      READ TABLE gt_t001 INTO gs_t001 WITH KEY bukrs = gs_hd_4-bukrs
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd_4-butxt = gs_t001-butxt.
      ENDIF.
      "项目名称
      READ TABLE gt_ekkn WITH KEY ebeln = gs_hd_4-ebeln .
      IF sy-subrc = 0.
        READ TABLE gt_prps WITH KEY pspnr = gt_ekkn-ps_psp_pnr   BINARY SEARCH .
        IF sy-subrc = 0 .
          READ TABLE gt_proj WITH KEY pspnr = gt_prps-psphi BINARY SEARCH.
          IF sy-subrc = 0.
            gs_hd_4-post1 = gt_proj-post1.
          ENDIF.
        ENDIF.
      ENDIF.
      "订单编号
      l_tdname = gs_hd_4-ebeln .
      PERFORM selbz_xx USING l_tdname 'EKKO' 'F06' CHANGING gs_hd_4-ddbh.
      "打印日期
      gs_hd_4-dyrq = sy-datum.
      "采购合同编号
      PERFORM selbz_xx USING l_tdname 'EKKO' 'F08' CHANGING gs_hd_4-cghtbh.
      "税价合计
      READ TABLE gt_hj_4 INTO gs_hj_4 WITH KEY ebeln = gs_hd_4-ebeln
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd_4-sjhj = gs_hj_4-sjhj .
        "大写
        PERFORM conv_amount USING gs_hd_4-sjhj
                     CHANGING gs_hd_4-sjhj_dx.
        gs_hd_4-waers = gs_hj_4-waers.
      ENDIF.
      "税率
      IF gs_hd_4-submi IS INITIAL.
        READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_hd_4-ebeln
                                        BINARY SEARCH.
        IF sy-subrc EQ 0 AND gs_hd_4-lands IS NOT INITIAL. .
          CLEAR t_ftaxp[].
          CALL FUNCTION 'GET_TAX_PERCENTAGE'
            EXPORTING
              aland   = gs_hd_4-lands
              datab   = sy-datum
              mwskz   = gs_ekpo-mwskz
              txjcd   = gs_ekpo-txjcd
            TABLES
              t_ftaxp = t_ftaxp[].
          IF t_ftaxp[] IS NOT INITIAL.
            READ TABLE t_ftaxp INDEX 1.
            gs_hd_4-submi = t_ftaxp-kbetr / 10.
            CONCATENATE '税率，' gs_hd_4-submi '%' INTO gs_hd_4-submi .
          ENDIF.
          REFRESH t_ftaxp[].

        ENDIF.
        ELSE.
          CONCATENATE '税率，' gs_hd_4-submi  INTO gs_hd_4-submi .

      ENDIF.
      "付款方式
      READ TABLE gt_t052u INTO gs_t052u WITH KEY zterm = gs_hd_4-zterm
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd_4-zterm_txt  = gs_t052u-text1.

      ENDIF.

      "运费承担方
      CLEAR: dynpfields, dynpfields[].
      dynpfields-fieldname = 'YFCDF'. "填入需要读值的字段名
      APPEND dynpfields.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname             = sy-repid
          dynumb             = sy-dynnr
          translate_to_upper = 'X'
        TABLES
          dynpfields         = dynpfields
        EXCEPTIONS
          OTHERS             = 9.
      IF sy-subrc = 0.
        READ TABLE dynpfields WITH KEY fieldname = 'YFCDF'.
        name1 = dynpfields-fieldvalue. "获取到的值
      ENDIF.
      gs_hd_4-yfcdf =  name1 .
      CLEAR:dynpfields, dynpfields[],name1.

      dynpfields-fieldname = 'YFMFZBQNX'. "填入需要读值的字段名
      APPEND dynpfields.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname             = sy-repid
          dynumb             = sy-dynnr
          translate_to_upper = 'X'
        TABLES
          dynpfields         = dynpfields
        EXCEPTIONS
          OTHERS             = 9.
      IF sy-subrc = 0.
        READ TABLE dynpfields WITH KEY fieldname = 'YFMFZBQNX'.
        name1 = dynpfields-fieldvalue. "获取到的值
      ENDIF.

      gs_hd_4-zbqnx = name1.
      "补充条款

      PERFORM selbz_xx USING l_tdname 'EKKO' 'F07' CHANGING gs_hd_4-bctk.

      "交货地址
      READ TABLE gt_mx_4 INTO gs_mx_4 WITH KEY ebeln = gs_hd_4-ebeln
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = gs_mx_4-adrnr
                                        BINARY SEARCH.
        IF sy-subrc EQ 0.
          gs_hd_4-jhdz  =  gs_adrc-street.  "交货地址
          ""收货地址、"收货联系电话
          SPLIT gs_adrc-city2 AT '-' INTO gs_hd_4-shlxr gs_hd_4-shlxdh.
        ELSE.
          READ TABLE gt_ekkn WITH KEY ebeln = gs_hd_4-ebeln .
          IF sy-subrc = 0.
            READ TABLE gt_prps WITH KEY pspnr = gt_ekkn-ps_psp_pnr   BINARY SEARCH .
            IF sy-subrc = 0 .
              READ TABLE gt_proj WITH KEY pspnr = gt_prps-psphi BINARY SEARCH.
              IF sy-subrc = 0.
                READ TABLE gt_tcj04 INTO gs_tcj04 WITH KEY vernr = gt_proj-vernr
                                                  BINARY SEARCH.
                IF sy-subrc EQ 0.
                  gs_hd_4-shlxr = gs_tcj04-verna.  "收货联系人
                ENDIF.
                gs_hd_4-jhdz = gt_proj-zxmdz.     "收货地址
                gs_hd_4-shlxdh = gt_proj-zxmjldh. "收货联系电话

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "开户行
      gs_hd_4-banka = khh.
      "账号
      gs_hd_4-bankn = zh.
      "税号
      READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_hd_4-lifnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd_4-stceg =  gs_lfa1-stceg.
      ENDIF.

      "附件文本
      "保存
      PERFORM savetext USING editor wa_thead.
      LOOP AT it_lines INTO wa_lines.
        IF gs_hd_4-fjwb IS INITIAL.
          gs_hd_4-fjwb = wa_lines-tdline .
        ELSE.
          CONCATENATE gs_hd_4-fjwb wa_lines-tdline  INTO gs_hd_4-fjwb.
        ENDIF.
      ENDLOOP.
      "联系人-甲方
      READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_hd_4-ekgrp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_hd_4-eknam = gs_t024-eknam.
        gs_hd_4-ektel = gs_t024-ektel.
      ENDIF.
      "联系人-乙方
      gs_hd_4-lxr_yf = lxr .
      gs_hd_4-dh_yf = lxdh .
      MODIFY gt_hd_4  FROM gs_hd_4.
    ENDLOOP.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRT_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_prt_4 .
  DATA:lv_fmname TYPE rs38l_fnam,
       lw_output TYPE ssfcompop.


  control-no_open = 'X'.
  control-no_close = 'X'.
  lw_output-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
      output_options     = lw_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*     ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZJDCGHT4'
    IMPORTING
      fm_name            = lv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_mx_4 INTO gs_mx_4.
    AT NEW ebeln.
      REFRESH:pt_mx_4,pt_hd_4.
      READ TABLE gt_hd_4 INTO gs_hd_4 WITH KEY ebeln = gs_mx_4-ebeln
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_hd_4 TO ps_hd_4.
        APPEND ps_hd_4 TO pt_hd_4.
      ENDIF.
    ENDAT.
    CLEAR:ps_mx_4.
    MOVE-CORRESPONDING gs_mx_4 TO ps_mx_4.
    APPEND ps_mx_4 TO pt_mx_4.
    AT END OF ebeln .
      SORT pt_mx_4 BY ebeln ebelp .
      CALL FUNCTION lv_fmname
        EXPORTING
*         ARCHIVE_INDEX      =
*         ARCHIVE_INDEX_TAB  =
*         ARCHIVE_PARAMETERS =
          control_parameters = control
*         MAIL_APPL_OBJ      =
*         MAIL_RECIPIENT     =
*         MAIL_SENDER        =
          output_options     = lw_output
*         USER_SETTINGS      = 'X'
*    IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
*         JOB_OUTPUT_INFO    =
*         JOB_OUTPUT_OPTIONS =
        TABLES
          it_hd              = pt_hd_4
          it_mx              = pt_mx_4
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

      ENDIF.
    ENDAT.
  ENDLOOP.



  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*     error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH:it_head1,it_head2,it_main1,it_end2.
ENDFORM.
