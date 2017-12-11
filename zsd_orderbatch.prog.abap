*&---------------------------------------------------------------------*
*& Report  ZSD_ORDERBATCH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsd_orderbatch.
*---data declarations.
DATA it_mesage_tab TYPE esp1_message_wa_type OCCURS 5 WITH HEADER LINE.
"报消息
DATA: l_vbeln            LIKE vbak-vbeln.
TYPE-POOLS vrm.
DATA myvalue TYPE vrm_values WITH HEADER LINE.
*---表头
DATA: order_header_in TYPE bapisdhd1.
DATA: order_header_inx  TYPE  bapisdhd1x.

*---行项目
DATA: BEGIN OF order_items_in OCCURS 1.
        INCLUDE STRUCTURE bapisditm.
DATA: END OF order_items_in.

DATA: BEGIN OF order_items_inx OCCURS 1.
        INCLUDE STRUCTURE bapisditmx.
DATA: END OF order_items_inx.
*---合作伙伴
DATA: BEGIN OF order_partners OCCURS 1.
        INCLUDE STRUCTURE bapiparnr.
DATA: END OF order_partners.
*---add. 文本
DATA: BEGIN OF order_text OCCURS 1.
        INCLUDE STRUCTURE bapisdtext.
DATA: END OF order_text.
*---返回表
DATA: BEGIN OF it_return OCCURS 1.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_return.
*---计划行
DATA: BEGIN OF order_schedules_in OCCURS 1.
        INCLUDE STRUCTURE bapischdl.
DATA: END OF order_schedules_in.
DATA: BEGIN OF order_schedules_inx OCCURS 1.
        INCLUDE STRUCTURE bapischdlx.
DATA: END OF order_schedules_inx.
*---价格条件
DATA: wa_cond  TYPE bapicond,
      wa_condx TYPE bapicondx,
      it_cond  TYPE STANDARD TABLE OF bapicond,
      it_condx TYPE STANDARD TABLE OF bapicondx.

DATA:
  it_extention  TYPE bapiparex OCCURS 0 WITH HEADER LINE,
  it_extentionx TYPE bapiparex OCCURS 0 WITH HEADER LINE.


DATA:
  iw_bape_vbapx TYPE   bape_vbapx,
  iw_bape_vbakx TYPE   bape_vbakx,

  iw_bape_vbap  TYPE   bape_vbap,
  iw_bape_vbak  TYPE   bape_vbak.

*---逻辑设计 更新价格
DATA: ls_logic_switch    TYPE bapisdls.

DATA:BEGIN OF it_head_data OCCURS 0,
       auart   TYPE vbak-auart, "订单类型
       vkorg   TYPE vbak-vkorg, "销售组织
       vtweg   TYPE vbak-vtweg, "分销渠道
       spart   TYPE vbak-spart, "产品组
       vbeln   TYPE vbak-vbeln, "订单编号
       kunnr   TYPE vbak-kunnr, "客户编码
       bname   TYPE vbak-bname, ""旧订单编号 （项目编号）"
       bstkd   TYPE vbkd-bstkd, "合同编号
       bstdk   TYPE vbkd-bstdk, "合同签订日期
       vdatu   TYPE vbak-vdatu, "请求交货日期
       vkbur   TYPE vbak-vkbur, "销售副总编号
       vkgrp   TYPE vbak-vkgrp, "部门代码
       vsnmr_v TYPE vbak-vsnmr_v, "质保年限
       augru   TYPE vbak-augru, "订单原因
       kunnr1  TYPE vbpa-kunnr, ""项目经理1编号"
       kunnr2  TYPE vbpa-kunnr, ""项目经理2编号"
       kunnr3  TYPE vbpa-kunnr, ""经办人1编号"
       kunnr4  TYPE vbpa-kunnr, ""经办人2编号"
       kunnr5  TYPE vbpa-kunnr, ""经办人3编号"
       name    TYPE char255, "项目名称
       kvgr1   TYPE vbak-kvgr1, "行业
       kvgr2   TYPE vbak-kvgr2, "经济性质
       country TYPE vbak-country, "项目实施国家
       region  TYPE vbak-region , "项目实施省份
       zsd0303 TYPE vbak-zsd0303, "项目实施地区
       zsd0301 TYPE vbak-zsd0301, "项目完工日期
       zsd0302 TYPE vbak-zsd0302, "销售立项号
       WAERK   TYPE vbak-WAERK,   "货币
     END OF it_head_data.

DATA:BEGIN OF it_item_data OCCURS 0,
       vbeln   TYPE vbap-vbeln   , "销售订单编号
       posnr   TYPE vbap-posnr   , "项目
       matnr   TYPE vbap-matnr   , "产品编号
       zmeng   TYPE vbap-zmeng   , "目标数量
       kwmeng  TYPE vbap-kwmeng  , "订单数量
       kschl   TYPE konv-kschl   , "条件类型
       kbetr   TYPE konv-kbetr   , "单价金额
       mvgr1   TYPE vbap-mvgr1   , ""规格型号物料组1"
       mvgr2   TYPE vbap-mvgr2   , ""控制设备物料组2"
       mvgr3   TYPE vbap-mvgr3   , ""像素间距物料组3"
       mvgr4   TYPE vbap-mvgr4   , "发光管型号物料组4
       mvgr5   TYPE vbap-mvgr5   , ""安装方式物料组5"
       zsd0105 TYPE vbap-zsd0105 , "屏幕宽度
       zsd0106 TYPE vbap-zsd0106 , "屏幕高度
       zsd0107 TYPE vbap-zsd0107 , "屏体面积
       zsd0108 TYPE vbap-zsd0108 , "屏数
       zsd0109 TYPE vbap-zsd0109 , "总面积
       zsd0110 TYPE vbap-zsd0110 , "用途
     END OF it_item_data.

DATA:init.
************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETER: p_idu TYPE c AS LISTBOX DEFAULT 'I'  USER-COMMAND idu VISIBLE
LENGTH 10.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS: p_head TYPE rlgrap-filename MODIF ID zup MEMORY ID zsd001.
"主数据文件路径
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-003.
PARAMETERS: p_item TYPE rlgrap-filename MODIF ID  zdo. "模版下载
SELECTION-SCREEN END OF BLOCK blk3.

************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM frm_setlist.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_head.
  PERFORM frm_get_head.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_item.
  PERFORM frm_get_item.


************************************************************************
* Event top of page
************************************************************************
TOP-OF-PAGE.


AT SELECTION-SCREEN.


START-OF-SELECTION.
*  IF p_up = 'X'.
  PERFORM frm_chech_filename.             "检测文件名
* 主数据
  IF p_idu <> '' AND p_head <> ''.
    PERFORM frm_upload_head ."上传文件到内表
  ENDIF.

  IF p_idu <> 'D' AND p_item <> ''.
    PERFORM frm_upload_item ."上传文件到内表
  ENDIF.

  PERFORM frm_process_data.
  PERFORM frm_message USING 'X' '' '' '' ''  '' '' ''.
*  ELSE.
*    PERFORM frm_download  .
*  ENDIF.


FORM frm_setlist.
*对内表加载值
  myvalue-key = 'I'. myvalue-text = '增'. APPEND myvalue.
  myvalue-key = 'D'. myvalue-text = '删'. APPEND myvalue.
  myvalue-key = 'U'. myvalue-text = '改'. APPEND myvalue.
  myvalue-key = ''. myvalue-text = '增行项目'. APPEND myvalue.

  IF init IS INITIAL.
    CALL FUNCTION 'VRM_SET_VALUES' "调用函数对下拉框对象传递数据
      EXPORTING
        id              = 'P_IDU' "下拉框对象名
        values          = myvalue[]  "下拉框中加载的数据
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDIF.

  init = 'X'. "记录初始化状态

ENDFORM.


FORM frm_process_data.
  FIELD-SYMBOLS <lw_item> LIKE LINE OF it_item_data.
  DATA: l_posnr TYPE vbap-posnr VALUE '000000'.
  DATA: l_etenr TYPE vbep-etenr VALUE '0000'.
  DATA l_bapisdh1  TYPE bapisdh1.
  DATA l_bapisdh1x TYPE bapisdh1x.

  DATA lt_mara TYPE TABLE OF mara INITIAL SIZE 0 WITH HEADER LINE.
  SELECT matnr
         meins
         bstme
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN it_item_data
    WHERE matnr = it_item_data-matnr.
*---Header data

  IF p_idu <> 'D' AND p_idu <> ''.

    LOOP AT it_head_data.
      CLEAR:
             order_header_in,
             order_header_inx,
             order_items_in,order_items_in[],
             order_items_inx,order_items_inx[],
             order_partners,order_partners[],
             order_text,order_text[],
             iw_bape_vbakx,iw_bape_vbak,
             it_extention[],it_extentionx[].

      l_vbeln = it_head_data-vbeln.
      PERFORM frm_zero_add USING l_vbeln CHANGING l_vbeln.
      order_header_in-doc_type     = it_head_data-auart.
      order_header_in-sales_org    = it_head_data-vkorg.
      order_header_in-distr_chan   = it_head_data-vtweg.
      order_header_in-division     = it_head_data-spart.
      order_header_in-req_date_h   = it_head_data-vdatu.  "请求交货日期
      order_header_in-name         = it_head_data-bname.  "
      order_header_in-purch_no_c   = it_head_data-bstkd.  "
      order_header_in-purch_date   = it_head_data-bstdk.  "
      order_header_in-doc_date     = sy-datum.  "
      order_header_in-sales_off    = it_head_data-vkbur.  "
      order_header_in-sales_grp    = it_head_data-vkgrp.  "
      order_header_in-version      = it_head_data-vsnmr_v.  "
      order_header_in-ord_reason   = it_head_data-augru.  "
      order_header_in-cust_grp1    = it_head_data-kvgr1.  "
      order_header_in-cust_grp2    = it_head_data-kvgr2.  "
      order_header_in-CURRENCY     = it_head_data-WAERK.  "


      order_header_inx-updateflag   = p_idu.
      order_header_inx-doc_type     = 'X'.
      order_header_inx-sales_org    = 'X'.
      order_header_inx-distr_chan   = 'X'.
      order_header_inx-division     = 'X'.
      order_header_inx-req_date_h   = 'X'."请求交货日期
      order_header_inx-name         = 'X'."
      order_header_inx-purch_no_c   = 'X'."
      order_header_inx-purch_date   = 'X'."
      order_header_inx-doc_date     = 'X'."
      order_header_inx-sales_off    = 'X'."
      order_header_inx-sales_grp    = 'X'."
      order_header_inx-version      = 'X'.  "
      order_header_inx-ord_reason   = 'X'."
      order_header_inx-cust_grp1    = 'X'."
      order_header_inx-cust_grp2    = 'X'."
      order_header_inx-CURRENCY     = 'X'."

      MOVE-CORRESPONDING order_header_inx TO l_bapisdh1x.
      MOVE-CORRESPONDING order_header_in  TO l_bapisdh1.

      iw_bape_vbak-vbeln   = l_vbeln.
      iw_bape_vbak-country = it_head_data-country.
      iw_bape_vbak-region  = it_head_data-region.
      iw_bape_vbak-zsd0303 = it_head_data-zsd0303.

      iw_bape_vbak-zsd0301 = it_head_data-zsd0301.
      iw_bape_vbak-zsd0302 = it_head_data-zsd0302.

      it_extention-structure =   'BAPE_VBAK'.
      it_extention+30(960)   = iw_bape_vbak  .
      APPEND it_extention.

      iw_bape_vbakx-vbeln   = l_vbeln.
      iw_bape_vbakx-country = 'X'.
      iw_bape_vbakx-region  = 'X'.
      iw_bape_vbakx-zsd0303 = 'X'.
      iw_bape_vbakx-zsd0301 = 'X'.
      iw_bape_vbakx-zsd0302 = 'X'.

      it_extention-structure =   'BAPE_VBAKX'.
      it_extention+30(960)   = iw_bape_vbakx  .
      APPEND it_extention.

*  order_header_in-war_date    = sy-datum.         "生产日期
*  order_header_in-purch_date  = sy-datum.         "采购订单日期
*order_header_in-purch_no_c  = 'po'.
*"采购订单编号,采购订单信息要带上,否则订单抬头不完整， bapi有警告消息

*---售达方
      order_partners-partn_role = 'AG'.
      order_partners-partn_numb = it_head_data-kunnr.
      APPEND order_partners.
*---送达方
      order_partners-partn_role = 'WE'.
      order_partners-partn_numb = it_head_data-kunnr.
      APPEND order_partners.
      "项目经理1编号"
      "项目经理2编号"
      "经办人1编号"
      "经办人2编号"
      "经办人3编号"
      IF it_head_data-kunnr1 <> ''.
        order_partners-partn_role = 'Z1'.
        order_partners-partn_numb = it_head_data-kunnr1.
        APPEND order_partners.

      ENDIF.

      IF it_head_data-kunnr2 <> ''.
        order_partners-partn_role = 'Z2'.
        order_partners-partn_numb = it_head_data-kunnr2.
        APPEND order_partners.
      ENDIF.

      IF it_head_data-kunnr3 <> ''.
        order_partners-partn_role = 'Z3'.
        order_partners-partn_numb = it_head_data-kunnr3.
        APPEND order_partners.
      ENDIF.

      IF it_head_data-kunnr4 <> ''.
        order_partners-partn_role = 'Z4'.
        order_partners-partn_numb = it_head_data-kunnr4.
        APPEND order_partners.
      ENDIF.

      IF it_head_data-kunnr5 <> ''.
        order_partners-partn_role = 'Z5'.
        order_partners-partn_numb = it_head_data-kunnr5.
        APPEND order_partners.
      ENDIF.
      IF it_head_data-name <> ''.
        "---抬头文本
        order_text-doc_number          = it_head_data-vbeln.
        order_text-itm_number          = space.
        order_text-text_id             = 'Z001'.
        order_text-langu               = sy-langu.
        order_text-format_col          = '*'.
        order_text-text_line           = it_head_data-name.
        APPEND order_text.
      ENDIF.


      LOOP AT it_item_data ASSIGNING <lw_item> WHERE vbeln =
        it_head_data-vbeln.
        AT NEW vbeln.
          CLEAR: l_etenr,it_cond[],it_condx[],order_schedules_in[],
          order_schedules_inx[].
        ENDAT.

        AT NEW posnr.
          iw_bape_vbap-vbeln   = l_vbeln.
          iw_bape_vbap-posnr   = <lw_item>-posnr.
          iw_bape_vbap-zsd0105 = <lw_item>-zsd0105.
          iw_bape_vbap-zsd0106 = <lw_item>-zsd0106.
          iw_bape_vbap-zsd0107 = <lw_item>-zsd0107.
          iw_bape_vbap-zsd0108 = <lw_item>-zsd0108.
          iw_bape_vbap-zsd0109 = <lw_item>-zsd0109.
          iw_bape_vbap-zsd0110 = <lw_item>-zsd0110.

          it_extention-structure =   'BAPE_VBAP'.
          it_extention+30(960)   =   iw_bape_vbap.
          APPEND it_extention.

          iw_bape_vbapx-vbeln   = l_vbeln.
          iw_bape_vbapx-posnr   = <lw_item>-posnr.
          iw_bape_vbapx-zsd0105 = 'X'.
          iw_bape_vbapx-zsd0106 = 'X'.
          iw_bape_vbapx-zsd0107 = 'X'.
          iw_bape_vbapx-zsd0108 = 'X'.
          iw_bape_vbapx-zsd0109 = 'X'.
          iw_bape_vbapx-zsd0110 = 'X'.

          it_extention-structure =   'BAPE_VBAPX'.
          it_extention+30(960)   = iw_bape_vbapx  .
          APPEND it_extention.
          l_etenr = l_etenr + 1.
*---Item data
          order_items_in-itm_number   = <lw_item>-posnr.
          order_items_in-material   = <lw_item>-matnr.

          order_items_in-prc_group1   = <lw_item>-mvgr1.
          order_items_in-prc_group2   = <lw_item>-mvgr2.
          order_items_in-prc_group3   = <lw_item>-mvgr3.
          order_items_in-prc_group4   = <lw_item>-mvgr4.
          order_items_in-prc_group5   = <lw_item>-mvgr5.

          IF <lw_item>-zmeng > 0.
            order_items_in-target_qty   = <lw_item>-zmeng."目标数量
          ENDIF.

          CLEAR lt_mara.
          READ TABLE lt_mara WITH KEY matnr = <lw_item>-matnr.
          IF sy-subrc = 0.

            order_items_in-sales_unit   = lt_mara-meins."销售单位
            order_items_inx-sales_unit  = 'X'.         "销售单位
            IF <lw_item>-zmeng > 0.
              order_items_in-target_qu    = lt_mara-meins."目标数量单位
              order_items_inx-target_qu   = 'X'.
            ENDIF.
          ENDIF.
          APPEND order_items_in.

          order_items_inx-itm_number  = <lw_item>-posnr.
          order_items_inx-updateflag  = p_idu.

          order_items_inx-material    = 'X'.
          order_items_inx-prc_group1  = 'X'.
          order_items_inx-prc_group2  = 'X'.
          order_items_inx-prc_group3  = 'X'.
          order_items_inx-prc_group4  = 'X'.
          order_items_inx-prc_group5  = 'X'.
          order_items_inx-target_qty  = 'X'.

          APPEND order_items_inx.

*   Fill schedule lines
          order_schedules_in-itm_number = <lw_item>-posnr.
          order_schedules_in-sched_line =  l_etenr.
          order_schedules_in-req_qty    = <lw_item>-kwmeng.
          APPEND order_schedules_in.

*   Fill schedule line flags
          order_schedules_inx-updateflag  = p_idu.
          order_schedules_inx-itm_number  = <lw_item>-posnr.
          order_schedules_inx-sched_line  = l_etenr.
          order_schedules_inx-req_qty     = 'X'.
          APPEND order_schedules_inx.
        ENDAT.
*    l_posnr = l_posnr + 10.

* Item Conditions
        "新增价格条件记录
        ls_logic_switch-pricing = 'G'.
        CLEAR: wa_cond,wa_condx.
        wa_cond-itm_number = <lw_item>-posnr.
        wa_cond-cond_type  = <lw_item>-kschl.                  "定价条件
        wa_cond-cond_value = <lw_item>-kbetr.  "价格
        wa_cond-currency   = it_head_data-WAERK.               "货币或%
        IF wa_cond-cond_type = 'ZP01'.
          CLEAR lt_mara.
          READ TABLE lt_mara WITH KEY matnr = <lw_item>-matnr.
          IF sy-subrc = 0.
          wa_cond-COND_UNIT = lt_mara-meins.
          endif.
        ENDIF.
*      wa_cond-cond_unit  = t_import_01-vrkme.       "条件单位
        wa_cond-cond_p_unt = '1'.                     "条件定价单位
        wa_cond-cond_updat = 'X'.
        APPEND wa_cond TO it_cond.

        CLEAR: wa_cond,wa_condx.
        wa_condx-itm_number = <lw_item>-posnr.
        wa_condx-cond_type  = <lw_item>-kschl.       "定价条件
        wa_condx-cond_value = 'X'.  "价格
        wa_condx-currency   = 'X'.                   "货币或%
*       wa_cond-cond_unit  = t_import_01-vrkme.       "条件单位
        wa_condx-cond_p_unt = 'X'.                   "条件定价单位
*       wa_condx-cond_updat = 'X'.
        wa_condx-updateflag = 'X'.
        IF wa_condx-cond_type = 'ZP01'.
          CLEAR lt_mara.
          READ TABLE lt_mara WITH KEY matnr = <lw_item>-matnr.
          IF sy-subrc = 0.
          wa_condx-COND_UNIT = 'X'.
          endif.
        ENDIF.
        APPEND wa_condx TO it_condx.

      ENDLOOP.

      IF p_idu = 'U'.
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = it_head_data-vbeln
            order_header_in  = l_bapisdh1
            order_header_inx = l_bapisdh1x
*           SIMULATION       =
*           BEHAVE_WHEN_ERROR           = ' '
*           INT_NUMBER_ASSIGNMENT       = ' '
*           LOGIC_SWITCH     =
*           NO_STATUS_BUF_INIT          = ' '
          TABLES
            return           = it_return[]
            order_item_in    = order_items_in
            order_item_inx   = order_items_inx
            partners         = order_partners
*           PARTNERCHANGES   =
*           PARTNERADDRESSES =
*           ORDER_CFGS_REF   =
*           ORDER_CFGS_INST  =
*           ORDER_CFGS_PART_OF          =
*           ORDER_CFGS_VALUE =
*           ORDER_CFGS_BLOB  =
*           ORDER_CFGS_VK    =
*           ORDER_CFGS_REFINST          =
            schedule_lines   = order_schedules_in
            schedule_linesx  = order_schedules_inx
            order_text       = order_text
*           ORDER_KEYS       =
            conditions_in    = it_cond
            conditions_inx   = it_condx
            extensionin      = it_extention
*            extensionex      = it_extentionx
*           NFMETALLITMS     =
          .

      ELSEIF p_idu = 'I'.
*---Call the BAPI to create the sales order.
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
          EXPORTING
            salesdocumentin       = l_vbeln
            order_header_in       = order_header_in
            order_header_inx      = order_header_inx
            logic_switch          = ls_logic_switch
            int_number_assignment = ''
*      IMPORTING
*           salesdocument         = l_vbeln
          TABLES
            return                = it_return
            order_items_in        = order_items_in
            order_items_inx       = order_items_inx
            order_schedules_in    = order_schedules_in
            order_schedules_inx   = order_schedules_inx
            order_partners        = order_partners
            extensionin           = it_extention
*            extensionex           = it_extentionx
            order_text            = order_text
            order_conditions_in   = it_cond
            order_conditions_inx  = it_condx.

      ENDIF.

*---Check the return table.
      LOOP AT it_return WHERE type = 'E' OR type = 'A'.
        PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS' '043'
        l_vbeln  it_return-message it_return-parameter ''.
      ENDLOOP.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR it_return[].
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*       IMPORTING
*           RETURN        =
          .
* Commit the work.
        COMMIT WORK AND WAIT.
        WAIT UP TO '0.1' SECONDS.
        LOOP AT it_return.
          PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS'
          '043' l_vbeln  it_return-message it_return-parameter ''.
        ENDLOOP.

        CLEAR it_return[].
        PERFORM frm_message USING '' 'S' 'STC_SC_TASKS' '043' l_vbeln
        '导入成功' '' ''.
      ENDIF.

    ENDLOOP.
  ELSEIF p_idu = 'D'.

    LOOP AT it_head_data.
      l_bapisdh1x-updateflag   = p_idu.
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = it_head_data-vbeln
*         ORDER_HEADER_IN  =
          order_header_inx = l_bapisdh1x
*         SIMULATION       =
*         BEHAVE_WHEN_ERROR           = ' '
*         INT_NUMBER_ASSIGNMENT       = ' '
*         LOGIC_SWITCH     =
*         NO_STATUS_BUF_INIT          = ' '
        TABLES
          return           = it_return[]
*         ORDER_ITEM_IN    =
*         ORDER_ITEM_INX   =
*         PARTNERS         =
*         PARTNERCHANGES   =
*         PARTNERADDRESSES =
*         ORDER_CFGS_REF   =
*         ORDER_CFGS_INST  =
*         ORDER_CFGS_PART_OF          =
*         ORDER_CFGS_VALUE =
*         ORDER_CFGS_BLOB  =
*         ORDER_CFGS_VK    =
*         ORDER_CFGS_REFINST          =
*         SCHEDULE_LINES   =
*         SCHEDULE_LINESX  =
*         ORDER_TEXT       =
*         ORDER_KEYS       =
*         CONDITIONS_IN    =
*         CONDITIONS_INX   =
*         EXTENSIONIN      =
*         EXTENSIONEX      =
*         NFMETALLITMS     =
        .
*---Check the return table.
      LOOP AT it_return WHERE type = 'E' OR type = 'A'.
        PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS' '043'
        l_vbeln  it_return-message it_return-parameter ''.
      ENDLOOP.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR it_return[].
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*       IMPORTING
*           RETURN        =
          .
* Commit the work.
        COMMIT WORK AND WAIT.
        WAIT UP TO '0.1' SECONDS.
        LOOP AT it_return.
          PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS'
          '043' l_vbeln  it_return-message it_return-parameter ''.
        ENDLOOP.
        CLEAR it_return[].
        PERFORM frm_message USING '' 'S' 'STC_SC_TASKS' '043' l_vbeln
        '删除成功' '' ''.
      ENDIF.
    ENDLOOP.
  ELSEIF p_idu = ''.
    data lt_head TYPE vbak OCCURS 0 with HEADER LINE.

    SELECT vbeln WAERK
      INTO CORRESPONDING FIELDS OF TABLE lt_head
      FROM vbak
      FOR ALL ENTRIES IN it_item_data
      WHERE vbeln = it_item_data-vbeln.

    SORT it_item_data BY vbeln posnr.
    LOOP AT it_item_data ASSIGNING <lw_item> .
      AT NEW vbeln.
        CLEAR: l_etenr,it_cond[],it_condx[],order_schedules_in[],it_extention[],it_extentionx[],
        order_schedules_inx[],order_items_in[],order_items_inx[].
      ENDAT.

      AT NEW posnr.
        iw_bape_vbap-vbeln   = <lw_item>-vbeln.
        iw_bape_vbap-posnr   = <lw_item>-posnr.
        iw_bape_vbap-zsd0105 = <lw_item>-zsd0105.
        iw_bape_vbap-zsd0106 = <lw_item>-zsd0106.
        iw_bape_vbap-zsd0107 = <lw_item>-zsd0107.
        iw_bape_vbap-zsd0108 = <lw_item>-zsd0108.
        iw_bape_vbap-zsd0109 = <lw_item>-zsd0109.
        iw_bape_vbap-zsd0110 = <lw_item>-zsd0110.

        it_extention-structure =   'BAPE_VBAP'.
        it_extention+30(960)   =   iw_bape_vbap.
        APPEND it_extention.

        iw_bape_vbapx-vbeln   = <lw_item>-vbeln.
        iw_bape_vbapx-posnr   = <lw_item>-posnr.
        iw_bape_vbapx-zsd0105 = 'X'.
        iw_bape_vbapx-zsd0106 = 'X'.
        iw_bape_vbapx-zsd0107 = 'X'.
        iw_bape_vbapx-zsd0108 = 'X'.
        iw_bape_vbapx-zsd0109 = 'X'.
        iw_bape_vbapx-zsd0110 = 'X'.

        it_extention-structure =   'BAPE_VBAPX'.
        it_extention+30(960)   = iw_bape_vbapx  .
        APPEND it_extention.



        l_etenr = l_etenr + 1.
*---Item data
        order_items_in-itm_number   = <lw_item>-posnr.
        order_items_in-material     = <lw_item>-matnr.

        order_items_in-prc_group1   = <lw_item>-mvgr1.
        order_items_in-prc_group2   = <lw_item>-mvgr2.
        order_items_in-prc_group3   = <lw_item>-mvgr3.
        order_items_in-prc_group4   = <lw_item>-mvgr4.
        order_items_in-prc_group5   = <lw_item>-mvgr5.

        IF <lw_item>-zmeng > 0.
          order_items_in-target_qty   = <lw_item>-zmeng."目标数量
        ENDIF.

        CLEAR lt_mara.
        READ TABLE lt_mara WITH KEY matnr = <lw_item>-matnr.
        IF sy-subrc = 0.
          order_items_inx-sales_unit   = lt_mara-meins."销售单位
          order_items_in-sales_unit    = lt_mara-meins."销售单位
          IF <lw_item>-zmeng > 0.
            order_items_inx-target_qu   = 'X'.
            order_items_in-target_qu    = lt_mara-meins."目标数量单位
          ENDIF.
        ENDIF.
        APPEND order_items_in.

        order_items_inx-itm_number  = <lw_item>-posnr.
        order_items_inx-updateflag  = 'I'.

        order_items_inx-material    = 'X'.
        order_items_inx-prc_group1  = 'X'.
        order_items_inx-prc_group2  = 'X'.
        order_items_inx-prc_group3  = 'X'.
        order_items_inx-prc_group4  = 'X'.
        order_items_inx-prc_group5  = 'X'.
        order_items_inx-target_qty  = 'X'.

        APPEND order_items_inx.

*   Fill schedule lines
        order_schedules_in-itm_number = <lw_item>-posnr.
        order_schedules_in-sched_line = l_etenr.
        order_schedules_in-req_qty    = <lw_item>-kwmeng.
        APPEND order_schedules_in.

*   Fill schedule line flags
        order_schedules_inx-updateflag  = p_idu.
        order_schedules_inx-itm_number  = <lw_item>-posnr.
        order_schedules_inx-sched_line  = l_etenr.
        order_schedules_inx-req_qty     = 'X'.
        APPEND order_schedules_inx.
      ENDAT.
*    l_posnr = l_posnr + 10.

* Item Conditions
      "新增价格条件记录
      ls_logic_switch-pricing = 'G'.
      CLEAR: wa_cond,wa_condx.
      wa_cond-itm_number = <lw_item>-posnr.
      wa_cond-cond_type  = <lw_item>-kschl.                  "定价条件
      wa_cond-cond_value = <lw_item>-kbetr.  "价格
      CLEAR lt_head.
      READ TABLE lt_head with KEY vbeln = <lw_item>-vbeln.
      IF sy-subrc = 0.
        wa_cond-currency   = lt_head-WAERK.                   "货币或%
      ENDIF.


*      wa_cond-cond_unit  = t_import_01-vrkme.       "条件单位
      wa_cond-cond_p_unt = '1'.                     "条件定价单位
      wa_cond-cond_updat = 'X'.
      APPEND wa_cond TO it_cond.

      CLEAR: wa_cond,wa_condx.
      wa_condx-itm_number = <lw_item>-posnr.
      wa_condx-cond_type  = <lw_item>-kschl.                  "定价条件
      wa_condx-cond_value = 'X'.  "价格
      wa_condx-currency   = 'X'.                   "货币或%
*      wa_cond-cond_unit  = t_import_01-vrkme.       "条件单位
      wa_condx-cond_p_unt = 'X'.                     "条件定价单位
*      wa_condx-cond_updat = 'X'.
      wa_condx-updateflag = 'X'.
      APPEND wa_condx TO it_condx.
      AT END OF vbeln.
        l_bapisdh1x-updateflag   = 'U'.
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = <lw_item>-vbeln
*           ORDER_HEADER_IN  =
            order_header_inx = l_bapisdh1x
*           SIMULATION       =
*           BEHAVE_WHEN_ERROR           = ' '
*           INT_NUMBER_ASSIGNMENT       = ' '
            LOGIC_SWITCH     =    ls_logic_switch
*           NO_STATUS_BUF_INIT          = ' '
          TABLES
            return           = it_return[]
            order_item_in    = order_items_in
            order_item_inx   = order_items_inx
*           partners         = order_partners
*           PARTNERCHANGES   =
*           PARTNERADDRESSES =
*           ORDER_CFGS_REF   =
*           ORDER_CFGS_INST  =
*           ORDER_CFGS_PART_OF          =
*           ORDER_CFGS_VALUE =
*           ORDER_CFGS_BLOB  =
*           ORDER_CFGS_VK    =
*           ORDER_CFGS_REFINST          =
            schedule_lines   = order_schedules_in
            schedule_linesx  = order_schedules_inx
*           ORDER_TEXT       =
*           ORDER_KEYS       =
            conditions_in    = it_cond
            conditions_inx   = it_condx
            extensionin      = it_extention
*           extensionex      = it_extentionx
*           NFMETALLITMS     =
          .
*---Check the return table.
        LOOP AT it_return WHERE type = 'E' OR type = 'A'.
          PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS'
          '043' l_vbeln  it_return-message it_return-parameter ''.
        ENDLOOP.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CLEAR it_return[].
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'
            .
*         Commit the work.
          COMMIT WORK.
          WAIT UP TO '1' SECONDS.
          LOOP AT it_return.
            PERFORM frm_message USING '' it_return-type 'STC_SC_TASKS'
            '043' <lw_item>-vbeln  it_return-message it_return-parameter
            ''.
          ENDLOOP.
          CLEAR it_return[].
          PERFORM frm_message USING '' 'S' 'STC_SC_TASKS' '043'
          <lw_item>-vbeln  '行项目插入成功' '' ''.
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  frm_chech_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_chech_filename .
  IF p_head IS INITIAL AND p_item IS INITIAL.
*    MESSAGE i010."主数据文件，路径和文件名，不能为空！
    MESSAGE '头文件 行文件 路径和文件名，不同时为空！' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
*  IF p_item IS INITIAL.
**    MESSAGE i010."主数据文件，路径和文件名，不能为空！
*    MESSAGE '行文件，路径和文件名，不能为空！' TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
ENDFORM.                    "frm_chech_filename


*&---------------------------------------------------------------------*
*&  包含                ZSD001_FILE_INCLUDE
*&---------------------------------------------------------------------*
FORM frm_get_head .
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',Excel Files,*.xls;*.xlsx,All Files,*.*.'(101)
      title            = '选择文件'(100)
    IMPORTING
      filename         = p_head
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc <> 0 AND sy-subrc <> 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF sy-subrc = 3.
    MESSAGE '未选择文件' TYPE 'I'.
  ENDIF.

*  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
**   EXPORTING
**     DEF_FILENAME           = ' '
**     DEF_PATH               = ' '
**     MASK                   = ' '
**     MODE                   = ' '
**     TITLE                  = ' '
*    IMPORTING
*      filename         = p_head
**     PATH             =
**     FILE             =
*    EXCEPTIONS
*      selection_cancel = 1
*      selection_error  = 2
*      OTHERS           = 3.
*  IF sy-subrc <> 0.
*  ENDIF.

ENDFORM.                    "frm_get_fn

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_item .
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',Excel Files,*.xls;*.xlsx,All Files,*.*.'(101)
      title            = '选择文件'(100)
    IMPORTING
      filename         = p_item
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc <> 0 AND sy-subrc <> 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF sy-subrc = 3.
    MESSAGE '未选择文件' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  frm_upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_upload_head .
  DATA:BEGIN OF lt_up OCCURS 0,
         text1(30),
         text2(30),
         text3(35),
         text4(30),
         text5(30),
         text6(30),
         text7(30), "不传
         text8(30),
         text9(30),
         text10(30),
         text11(30),
         text12(30),
         text13(30),"不传
         text14(30),
         text15(30),"不传
         text16(30),
         text17(30),
         text18(30),
         text19(30),"不传
         text20(30),
         text21(30),"不传
         text22(30),
         text23(30),"不传
         text24(30),
         text25(30),"不传
         text26(30),
         text27(30),"不传
         text28(30),
         text29(30),
         text30(30),
         text31(30),
         text32(30),
         text33(30),
         text34(30),
         text35(30),
         text36(30),
       END OF lt_up.

  DATA: lt_raw TYPE truxs_t_text_data.
  REFRESH  it_head_data[].
*  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*    EXPORTING
**     I_FIELD_SEPERATOR    =
*      i_line_header        = 'X'                   "带表头行情况去掉表头
*      i_tab_raw_data       = lt_raw
*      i_filename           = p_head
*    TABLES
*      i_tab_converted_data = lt_up[]
*    EXCEPTIONS
*      conversion_failed    = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
*  ENDIF.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_head
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 36
      i_end_row               = 65000
    TABLES
      intern                  = lt_up[]
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_up.
    it_head_data-auart   = lt_up-text1.
    it_head_data-vkorg   = lt_up-text2.
    it_head_data-vtweg   = lt_up-text3.
    it_head_data-spart   = lt_up-text4.
    it_head_data-vbeln   = lt_up-text5.
    PERFORM frm_zero_add USING it_head_data-vbeln CHANGING
    it_head_data-vbeln.
    it_head_data-kunnr   = lt_up-text6.
    PERFORM frm_zero_add USING it_head_data-kunnr CHANGING
    it_head_data-kunnr.
    it_head_data-bname   = lt_up-text8.
    it_head_data-bstkd   = lt_up-text9.
    PERFORM frm_conver_chartodate USING lt_up-text10 CHANGING
    it_head_data-bstdk .
    PERFORM frm_conver_chartodate USING lt_up-text11 CHANGING
    it_head_data-vdatu .
*    it_head_data-bstdk   = lt_up-text10.
*    it_head_data-vdatu   = lt_up-text11.
    it_head_data-vkbur   = lt_up-text12.
    it_head_data-vkgrp   = lt_up-text14.
    it_head_data-vsnmr_v = lt_up-text16.
    it_head_data-augru   = lt_up-text17.
    it_head_data-kunnr1  = lt_up-text18.
    it_head_data-kunnr2  = lt_up-text20.
    it_head_data-kunnr3  = lt_up-text22.
    it_head_data-kunnr4  = lt_up-text24.
    it_head_data-kunnr5  = lt_up-text26.
    PERFORM frm_zero_add USING it_head_data-kunnr1 CHANGING
    it_head_data-kunnr1.
    PERFORM frm_zero_add USING it_head_data-kunnr2 CHANGING
    it_head_data-kunnr2.
    PERFORM frm_zero_add USING it_head_data-kunnr3 CHANGING
    it_head_data-kunnr3.
    PERFORM frm_zero_add USING it_head_data-kunnr4 CHANGING
    it_head_data-kunnr4.
    PERFORM frm_zero_add USING it_head_data-kunnr5 CHANGING
    it_head_data-kunnr5.
    it_head_data-name    = lt_up-text28.
    it_head_data-kvgr1   = lt_up-text29.
    it_head_data-kvgr2   = lt_up-text30.
    it_head_data-country = lt_up-text31.
    it_head_data-region  = lt_up-text32.
    it_head_data-zsd0303 = lt_up-text33.
    PERFORM frm_conver_chartodate USING lt_up-text34 CHANGING it_head_data-zsd0301.
*    it_head_data-zsd0301 = lt_up-text34.
    it_head_data-zsd0302 = lt_up-text35.
    it_head_data-WAERK   = lt_up-text36.
    APPEND it_head_data.
  ENDLOOP.


  IF it_head_data[] IS INITIAL.
    MESSAGE 'head文件为空！' TYPE 'I'.
    STOP.
  ENDIF.

ENDFORM.
"frm_upload_data
FORM frm_zero_add USING u_in CHANGING u_out.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_in
    IMPORTING
      output = u_out.

ENDFORM.


FORM frm_conver_chartodate USING date1 CHANGING date2.
  IF date1 <> ''.
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external = date1
*       ACCEPT_INITIAL_DATE            =
      IMPORTING
        date_internal = date2
*     EXCEPTIONS
*       DATE_EXTERNAL_IS_INVALID       = 1
*       OTHERS        = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  frm_upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_upload_item .
  DATA:BEGIN OF lt_up OCCURS 0,
         text1(30),
         text2(30),
         text3(35),
         text4(30),"不传
         text5(30),
         text6(30),
         text7(30),
         text8(30),
         text9(30),
         text10(30),
         text11(30),
         text12(30),
         text13(30),
         text14(30),
         text15(30),
         text16(30),
         text17(30),
         text18(30),
         text19(30),
       END OF lt_up.
  DATA: lt_raw TYPE truxs_t_text_data.
  REFRESH  it_item_data[].
*  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*    EXPORTING
**     I_FIELD_SEPERATOR    =
*      i_line_header        = 'X'                   "带表头行情况去掉表头
*      i_tab_raw_data       = lt_raw
*      i_filename           = p_item
*    TABLES
*      i_tab_converted_data = lt_up[]
*    EXCEPTIONS
*      conversion_failed    = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
*  ENDIF.

*
*CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*  EXPORTING
*    filename                      = p_item
*    i_begin_col                   = 1
*    i_begin_row                   = 2
*    i_end_col                     = 256
*    i_end_row                     = 65000
*  TABLES
*    intern                        = lt_up[]
* EXCEPTIONS
*   INCONSISTENT_PARAMETERS       = 1
*   UPLOAD_OLE                    = 2
*   OTHERS                        = 3
*          .
*
*
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_item
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 19
      i_end_row               = 65000
    TABLES
      intern                  = lt_up[]
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_up.
    it_item_data-vbeln   = lt_up-text1.
    PERFORM frm_zero_add USING it_item_data-vbeln CHANGING
    it_item_data-vbeln.
    it_item_data-posnr   = lt_up-text2.
    PERFORM frm_zero_add USING it_item_data-posnr CHANGING
    it_item_data-posnr.
    it_item_data-matnr   = lt_up-text3.
    PERFORM frm_zero_add USING it_item_data-matnr CHANGING
    it_item_data-matnr.
    it_item_data-zmeng   = lt_up-text5.
    it_item_data-kwmeng  = lt_up-text6.
    it_item_data-kschl   = lt_up-text7.
    it_item_data-kbetr   = lt_up-text8.
    it_item_data-mvgr1   = lt_up-text9.
    it_item_data-mvgr2   = lt_up-text10.
    it_item_data-mvgr3   = lt_up-text11.
    it_item_data-mvgr4   = lt_up-text12.
    it_item_data-mvgr5   = lt_up-text13.
    it_item_data-zsd0105 = lt_up-text14.
    it_item_data-zsd0106 = lt_up-text15.
    it_item_data-zsd0107 = lt_up-text16.
    it_item_data-zsd0108 = lt_up-text17.
    it_item_data-zsd0109 = lt_up-text18.
    CONDENSE lt_up-text19 NO-GAPS.
    it_item_data-zsd0110 = lt_up-text19.
    IF p_idu <> ''. "对于非增加行项目的方式 需要检查 抬头和行项目的匹配
      CLEAR it_head_data.
      READ TABLE it_head_data WITH KEY vbeln = it_item_data-vbeln.
      IF sy-subrc = 0.
        APPEND it_item_data.
        CLEAR it_head_data.
      ENDIF.
    ELSE.
      APPEND it_item_data.
    ENDIF.

  ENDLOOP.

  IF it_item_data[] IS INITIAL.
    MESSAGE 'item文件为空！' TYPE 'I'.
    STOP.
  ENDIF.

ENDFORM.                    "frm_upload_data


FORM frm_message USING u_flag "是否报消息：X为报消息 空为添加消息
                       u_type
                       u_id
                       u_msgno
                       u_ms1
                       u_ms2
                       u_ms3
                       u_ms4.

  IF u_flag = ''.
    it_mesage_tab-msgid  =  u_id.
    it_mesage_tab-msgty  =  u_type.
    it_mesage_tab-msgno  =  u_msgno.
    it_mesage_tab-msgv1  =  u_ms1.
    it_mesage_tab-msgv2  =  u_ms2.
    it_mesage_tab-msgv3  =  u_ms3.
    it_mesage_tab-msgv3  =  u_ms4.
    APPEND it_mesage_tab.
  ENDIF.


  IF it_mesage_tab[] IS NOT INITIAL AND u_flag = 'X'.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = it_mesage_tab.
    CLEAR it_mesage_tab[].
  ENDIF.
ENDFORM.

*FORM frm_save_longtext USING u_vbeln u_lines.
*  DATA l_thead TYPE thead.
*  DATA lt_tline TYPE tline OCCURS 0 WITH HEADER LINE.
*  lt_tline-tdline = u_lines.
*  APPEND lt_tline.
*  l_thead-tdobject = 'VBBK'.
*  l_thead-tdname   = u_vbeln.
*  l_thead-tdid     = 'Z001'.
*  l_thead-tdspras  = sy-langu.
*  l_thead-tdfuser  = sy-uname.
*  l_thead-tdfdate  = sy-datum.
*  l_thead-tdftime  = sy-uzeit.
*  l_thead-tdospras = sy-langu.
*
*CALL FUNCTION 'DELETE_TEXT'
*          EXPORTING
*            CLIENT          = SY-MANDT
*            ID              = l_thead-tdid
*            LANGUAGE        = l_thead-tdspras
*            NAME            = l_thead-tdname
*            OBJECT          = l_thead-tdobject
*            SAVEMODE_DIRECT = 'X'
*          EXCEPTIONS
*            NOT_FOUND       = 1
*            OTHERS          = 2.
*
*
*CALL FUNCTION 'CREATE_TEXT'
*  EXPORTING
*    fid               = l_thead-tdid
*    flanguage         = l_thead-tdspras
*    fname             = l_thead-tdname
*    fobject           = l_thead-tdobject
*    SAVE_DIRECT       = 'X'
**   FFORMAT           = '*'
*  TABLES
*    flines            = lt_tline
* EXCEPTIONS
*   NO_INIT           = 1
*   NO_SAVE           = 2
*   OTHERS            = 3
*          .
*
*
**  CALL FUNCTION 'SAVE_TEXT'
**    EXPORTING
***     CLIENT          = SY-MANDT
**      header          = l_thead
***     INSERT          = ' '
**      savemode_direct = 'X'
***     OWNER_SPECIFIED = ' '
***     LOCAL_CAT       = ' '
***   IMPORTING
***     FUNCTION        =
***     NEWHEADER       =
**    TABLES
**      lines           = lt_tline
**    EXCEPTIONS
**      id              = 1
**      language        = 2
**      name            = 3
**      object          = 4
**      OTHERS          = 5.
*  IF sy-subrc <> 0.
*    PERFORM frm_message USING '' 'E' 'STC_SC_TASKS' '043' l_vbeln
*    '长文本更新失败' '' ''.
*  ELSE.
*    PERFORM frm_message USING '' 'S' 'STC_SC_TASKS' '043' l_vbeln
*    '长文本更新成功' '' ''.
** Implement suitable error handling here
*  ENDIF.
*
*ENDFORM.


**&---------------------------------------------------------------------
**
**&      Form  FRM_DOWNLOAD
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*
*FORM frm_download .
*
**&---导入模板
*  DATA: lv_filename TYPE string, "下载文件名
*        lv_wintitle TYPE string, "下载对话框标题名
*        lv_filepath TYPE string, "文件路径
*        lv_fullpath TYPE string, "全文件路径
*        lv_default  TYPE string. "默认下载文件名
*
*
**&---模板类型定义
*
*  TYPES:BEGIN OF ty_down,
*        text0(30),
*        text1(30),
*        text2(30),
*        text6(30),
*        text3(35),
*        text4(30),
*        text5(30),
**        text6(30),
**        text7(30),
**        text8(30),
*        text9(30),
*        text10(30),
*        text11(30),
*        text12(30),
*        text13(30),
**        text14(30),
*        text15(30),
*        text16(30),
*        text17(30),
*        text18(30),
**        text19(30),
**        text20(30),
**        text21(30),
**        text22(30),
*        text23(30),
**        text24(30),
*        text25(30),
**        text26(30),
**        text27(30),
**        text28(30),
**        text29(30),
*       END OF ty_down.
*  DATA:lwa_down TYPE ty_down,
*       lit_down TYPE TABLE OF ty_down.
*  FREE:lwa_down,
*       lit_down.
*
*
**&---EXCEL模板下载赋值
**&---检验计划抬头
*
*  lwa_down-text0 = '物料'.
*  lwa_down-text1 = '工厂'.
*  lwa_down-text2 = '组'.
*  lwa_down-text6 = '关键日期'.
*  lwa_down-text3 = '用途'.
*  lwa_down-text4 = '状态'.
*  lwa_down-text5 = '检验点'.
*
**  lwa_down-text6 = '从批量'.
**  lwa_down-text7 = '到批量'.
**  lwa_down-text8 = '批量单位'.
**&---工序
*
*  lwa_down-text9 = '工序编号'.
*  lwa_down-text10 = '工序描述'.
*  lwa_down-text11 = '工作中心'.
*  lwa_down-text12 = '控制码'.
*  lwa_down-text13 = '基本数量'.
*
**  lwa_down-text14 = '工序计量单位'.
*
*  lwa_down-text15 = '检验点完成'.
*
**&---特性
*
*  lwa_down-text16 = '特性序号'.
*  lwa_down-text17 = '主检验特性'.
*  lwa_down-text18 = '工厂'.
*
**  lwa_down-text19 = '版本'.
**  lwa_down-text20 = '检验特性短文本'.
**  lwa_down-text21 = '检验方法'.
**  lwa_down-text22 = '检验方法工厂'.
*
*  lwa_down-text23 = '采样过程'.
*
**  lwa_down-text24 = '采样单位'.
*
*  lwa_down-text25 = '基本单位'.
*
**  lwa_down-text26 = '选择集'.
**  lwa_down-text27 = '工厂'.
**  lwa_down-text28 = '目录类型'.
**  lwa_down-text29 = '目录'.
*
*
*  APPEND lwa_down TO lit_down.
*  lv_wintitle = '检验计划模板下载'.
*  lv_default  = '检验计划模板.XLS'.
*
*
*
*
**&---保存对话框
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      window_title      = lv_wintitle      "对话框的标题
*      default_extension = 'XLS'            "默认的文件后缀名
*      default_file_name = lv_default       "默认文件名
*      file_filter       = 'XLS'            "文件的filter
*
**     INITIAL_DIRECTORY =                  "初始化的目录
*
*    CHANGING
*      filename          = lv_filename      "保存的文件名
*      path              = lv_filepath      "文件路径
*      fullpath          = lv_fullpath      "全文件路径
*
**     USER_ACTION       =
*
*    EXCEPTIONS
*      cntl_error        = 1
*      error_no_gui      = 2
*      OTHERS            = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*
**&---模板下载
*
*  CALL METHOD cl_gui_frontend_services=>gui_download
*    EXPORTING
*      filename                = lv_fullpath
*      filetype                = 'DAT'
*      codepage                = '8400'
*    CHANGING
*      data_tab                = lit_down[]
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      OTHERS                  = 22.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*ENDFORM. " FRM_DOWNLOAD
