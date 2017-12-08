*&---------------------------------------------------------------------*
*&  包含                ZXCO1U01
*&---------------------------------------------------------------------*
*{   INSERT         IDSK900056                                        1
*&---------------------------------------------------------------------*
*&  包含                ZXCO1U01
*&---------------------------------------------------------------------*
* create by handlq
*增强1：记录修改生产订单中关键字段的日志

  " 当为生产订单时才继续执行
  CHECK HEADER_TABLE-AUTYP = '10'.  "订单类别：10-PP生产订单

*ADD BY HANDWY 2015-01-13
*订单组建中需求数量和提货数量比较
  IF SY-TCODE = 'CO02'.
    LOOP AT COMPONENT_TABLE.
      IF COMPONENT_TABLE-BDMNG < COMPONENT_TABLE-ENMNG.
        MESSAGE I000(ZPP01) DISPLAY LIKE 'E'.
        LEAVE TO TRANSACTION  'CO02'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " 当不是创建时才继续执行
  CHECK HEADER_TABLE-VBKZ <> 'I'.   "抬头更新标识：I-新增

  DATA: IT_LOG LIKE ZCO02_LOG OCCURS 0 WITH HEADER LINE.   "日志内表

  DATA: IPX      LIKE MSXXLIST-HOSTADR,  "IP地址（16进制）
        IP(15)   TYPE C,                 "IP地址（10进制字符串）
        HOST(20) TYPE C.                 "主机名
  DATA: VI_OPNUM(5) TYPE N,              "流水码变量
        VS_STR      TYPE STRING.         "字符串变量
  DATA: V_ARBPL_OLD LIKE CRHD-ARBPL,     "工作中心（旧值）
        V_ARBPL_NEW LIKE CRHD-ARBPL.     "工作中心（新值）


* 定义去除字符串中小数点后无用的0
*（只适用于带小数点的数字字符串）
  DEFINE DEL_ZERO_R.
    SHIFT &1 RIGHT DELETING TRAILING  '0 '. "去掉末尾0
    SHIFT &1 RIGHT DELETING TRAILING  '. '.  "若小数位全是0，则去掉小数点，只保存整数位
    CONDENSE &1 NO-GAPS.
  END-OF-DEFINITION.

*========================================
* 定义记录修改的宏（对单行表）
*========================================
  DEFINE ULOG_LINE.
*  ---------------------------------------------------
*  &1：记录新值的表名称
*  &2：记录旧值的表名称与新表的差异部分（通常为OLD）
*  &3：比较的字段名称
*  &4：操作对象类型
*  &5：操作对象描述
*  ---------------------------------------------------
    IF &1-&3 <> &1_&2-&3.
         VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
         IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
         IT_LOG-OPTYP = 'U'.          "操作类型：U-更新
         IT_LOG-OBJTP = '&4'.         "操作对象类型（抬头、组件、工艺等）
         IT_LOG-OBJNM = '&5'.         "操作对象描述
         IT_LOG-VLOLD = &1_&2-&3.     "操作对象的旧值
         IT_LOG-VLNEW = &1-&3.        "操作对象的新值
         APPEND IT_LOG.               "追加记录
         CLEAR IT_LOG.
     ENDIF.
  END-OF-DEFINITION.

*===========================================
* 定义记录修改的宏（对多行表，需要记录行号）
*===========================================
  DEFINE ULOG_ITAB.
*  ---------------------------------------------------
*  &1：记录新值的内表名称
*  &2：记录旧值的内表名称与新表的差异部分（通常为OLD）
*  &3：比较的字段名称
*  &4：操作对象类型
*  &5：操作对象描述
*  &6：操作对象唯一行标记描述
*  &7：操作对象唯一行标记字段名
*  &8：操作对象辅助行标记描述
*  &9：操作对象辅助行标记字段名
*  ---------------------------------------------------
    IF &1-&3 <> &1_&2-&3.
        VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
        IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
        IT_LOG-OPTYP = 'U'.          "操作类型：U-更新
        IT_LOG-OBJTP = '&4'.         "操作对象类型（抬头、组件、工艺等）
        IT_LOG-OBJNM = '&5'.         "操作对象描述

*-------------------------------------------
*操作对象唯一行标记，例如：预留项目号(1)
*唯一行标记一般前台不可见，但唯一且不可更改
*-------------------------------------------
        IT_LOG-OBJLU = &1-&7.        "操作对象唯一行标记的值（例如：0001）
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
            INPUT  = IT_LOG-OBJLU        IMPORTING
            OUTPUT = IT_LOG-OBJLU.                             "去除前置0
        CONCATENATE '&6' '(' IT_LOG-OBJLU ')' INTO IT_LOG-OBJLU.  "拼写描述

*-------------------------------------------
* 操作对象辅助行标记，例如：BOM项目号(0010)
* 辅助行标记一般不唯一或可修改，但最常用
*-------------------------------------------
        IT_LOG-OBJLA = &1-&9.        "操作对象辅助行标记的值（例如：0010）
        CONCATENATE '&8' '(' IT_LOG-OBJLA ')' INTO IT_LOG-OBJLA.  "拼写描述
        IT_LOG-VLOLD = &1_&2-&3.     "操作对象的旧值
        IT_LOG-VLNEW = &1-&3.        "操作对象的新值
        APPEND IT_LOG.               "追加记录
        CLEAR IT_LOG.
    ENDIF.

  END-OF-DEFINITION.


* 订单抬头修改
  IF HEADER_TABLE-VBKZ = 'U'.

    ULOG_LINE: HEADER_TABLE OLD GAMNG 订单抬头 订单总数量   ,
               HEADER_TABLE OLD GASMG 订单抬头 订单废品数量 ,
               HEADER_TABLE OLD GLTRP 订单抬头 基本完成日期 ,
               HEADER_TABLE OLD GSTRP 订单抬头 基本开始日期 ,
               HEADER_TABLE OLD GLUZP 订单抬头 基本日期-结束时间,
               HEADER_TABLE OLD GSUZP 订单抬头 基本日期-开始时间,
               HEADER_TABLE OLD FHORI 订单抬头 计划边际码   ,
               HEADER_TABLE OLD DISPO 订单抬头 MRP控制者    ,
               HEADER_TABLE OLD FEVOR 订单抬头 生产调度员   ,
               HEADER_TABLE OLD KTEXT 订单抬头 描述   ,
               HEADER_TABLE OLD CY_SEQNR 订单抬头  顺序编号.

  ENDIF.


* 订单项目更改
  IF POSITION_TABLE-VBKZ = 'U'.
    ULOG_LINE: POSITION_TABLE OLD LGORT 订单项目 收货库存地点.
  ENDIF.


* 订单组件更改
  LOOP AT COMPONENT_TABLE WHERE NOT VBKZ IS INITIAL.

    CASE COMPONENT_TABLE-VBKZ.
        " 记录组件的新增日志
      WHEN 'I'.
        VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
        IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
        IT_LOG-OPTYP = 'I'.    "操作类型：I-新增
        IT_LOG-OBJTP = '订单组件'.   "操作对象类型（抬头、组件、工艺等）
        IT_LOG-OBJNM = '订单组件(整行)'.   "操作对象描述

*操作对象唯一行标记，例如：预留项目号(1)
*唯一行标记一般前台不可见，但唯一且不可更改

        IT_LOG-OBJLU = COMPONENT_TABLE-RSPOS.        "预留项目号（例如：0001）
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = IT_LOG-OBJLU
          IMPORTING
            OUTPUT = IT_LOG-OBJLU.                           "去除前置0

        CONCATENATE '预留项目号(' IT_LOG-OBJLU ')' INTO IT_LOG-OBJLU.  "拼写描述


*操作对象辅助行标记，例如：BOM项目号(0010)
*辅助行标记一般不唯一或可修改，但最常用
        IT_LOG-OBJLA = COMPONENT_TABLE-POSNR.        "BOM项目号（例如：0010）
        CONCATENATE 'BOM项目号(' IT_LOG-OBJLA ')' INTO IT_LOG-OBJLA.  "拼写描述

* 将新增组件的所有信息组合为一行存入NEW字段
        VS_STR = COMPONENT_TABLE-ERFMG.
        DEL_ZERO_R VS_STR.
        CONCATENATE '物料号:' COMPONENT_TABLE-MATNR
                    ',需求数量:' VS_STR '(' COMPONENT_TABLE-ERFME ')'
                    ',工厂:' COMPONENT_TABLE-WERKS INTO IT_LOG-VLNEW.
        IF NOT COMPONENT_TABLE-LGORT IS INITIAL.
          CONCATENATE IT_LOG-VLNEW ',库存地点:' COMPONENT_TABLE-LGORT INTO IT_LOG-VLNEW.
        ENDIF.
        IF NOT COMPONENT_TABLE-RGEKZ IS INITIAL.
          CONCATENATE IT_LOG-VLNEW ',反冲:' COMPONENT_TABLE-RGEKZ INTO IT_LOG-VLNEW.
        ENDIF.
        APPEND IT_LOG.               "追加记录
        CLEAR IT_LOG.

*    记录组件的修改日志
      WHEN 'U'.
        "　根据预留及项目查询更改前的信息
        READ TABLE COMPONENT_TABLE_OLD WITH KEY RSNUM = COMPONENT_TABLE-RSNUM
                                                RSPOS = COMPONENT_TABLE-RSPOS.
        CHECK SY-SUBRC = 0.
        ULOG_ITAB: COMPONENT_TABLE OLD POSNR 订单组件 BOM项目号 预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD MATNR 订单组件 物料号    预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD BDMNG 订单组件 需求数量  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD MEINS 订单组件 计量单位  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD WERKS 订单组件 工厂      预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD LGORT 订单组件 库存地点  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD SCHGT 订单组件 散装标识  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD RGEKZ 订单组件 反冲标识  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD SOBKZ 订单组件 特殊库存  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD KZEAR 订单组件 最后发货  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD SANKA 订单组件 成本核算相关  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD ALPGR 订单组件 替代项目组    预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD ALPRF 订单组件 优先级  预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD ALPST 订单组件 策略    预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD EWAHR 订单组件 使用可能性    预留项目号 RSPOS BOM项目号 POSNR,
                   COMPONENT_TABLE OLD XLOEK 订单组件 项目删除  预留项目号 RSPOS BOM项目号 POSNR.
      WHEN 'D'.
        VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
        IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
        IT_LOG-OPTYP = 'D'.    "操作类型：D-删除
        IT_LOG-OBJTP = '订单组件'.   "操作对象类型（抬头、组件、工艺等）
        IT_LOG-OBJNM = '订单组件(整行)'.   "操作对象描述
        CONCATENATE '项目号' COMPONENT_TABLE-POSNR '物料' COMPONENT_TABLE-MATNR  INTO IT_LOG-VLOLD .
        CONCATENATE '项目号' COMPONENT_TABLE-POSNR '已经删除' INTO IT_LOG-VLNEW.
        APPEND IT_LOG.               "追加记录
        CLEAR IT_LOG.

    ENDCASE.
  ENDLOOP.

* 订单工序更改
  LOOP AT OPERATION_TABLE WHERE NOT VBKZ IS INITIAL.
    CASE OPERATION_TABLE-VBKZ.
*---------------------------
* 记录工序的新增日志
*---------------------------


      WHEN 'I'.
        VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
        IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
        IT_LOG-OPTYP = 'I'.          "操作类型：I-新增
        IT_LOG-OBJTP = '订单工序'.   "操作对象类型（抬头、组件、工艺等）
        IT_LOG-OBJNM = '订单工序(整行)'.  "操作对象描述

*-------------------------------------------
*操作对象唯一行标记，例如：预留项目号(1)
*唯一行标记一般前台不可见，但唯一且不可更改
*-------------------------------------------
        IT_LOG-OBJLU = OPERATION_TABLE-APLZL.        "工序计数器（例如：00000001）
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = IT_LOG-OBJLU
          IMPORTING
            OUTPUT = IT_LOG-OBJLU.    "去除前置0

        CONCATENATE '工序计数器(' IT_LOG-OBJLU ')' INTO IT_LOG-OBJLU.  "拼写描述

*-------------------------------------------
* 操作对象辅助行标记，例如：BOM项目号(0010)
* 辅助行标记一般不唯一或可修改，但最常用
*-------------------------------------------
        IT_LOG-OBJLA = OPERATION_TABLE-VORNR.        "工序号（例如：0010）
        CONCATENATE '工序号(' IT_LOG-OBJLA ')' INTO IT_LOG-OBJLA.  "拼写描述

*-------------------------------------------------------------------
* 将新增工序的所有信息组合为一行存入NEW字段
* 由于ALV的列宽最大为128字符，所以将工时部分组合为一行存入NEW2字段
*-------------------------------------------------------------------
        CLEAR V_ARBPL_NEW.
        SELECT SINGLE ARBPL
          FROM CRHD
          INTO V_ARBPL_NEW
          WHERE OBJTY = 'A' AND
                OBJID = OPERATION_TABLE-ARBID.
        IF SY-SUBRC = 0.
          CONCATENATE '工作中心:' V_ARBPL_NEW ',' INTO IT_LOG-VLNEW.
        ENDIF.

        CONCATENATE IT_LOG-VLNEW   '工厂:' OPERATION_TABLE-WERKS  ',控制码:' OPERATION_TABLE-STEUS INTO IT_LOG-VLNEW.

        IF NOT OPERATION_TABLE-KTSCH IS INITIAL.
          CONCATENATE IT_LOG-VLNEW ',标准文本码:' OPERATION_TABLE-KTSCH INTO IT_LOG-VLNEW.
        ENDIF.
        CONCATENATE IT_LOG-VLNEW ',工序短文本:' OPERATION_TABLE-LTXA1 INTO IT_LOG-VLNEW.

*工时部分存入NEW2字段
*------------------------
        IF NOT OPERATION_TABLE-VGW01 IS INITIAL.
          VS_STR = OPERATION_TABLE-VGW01.          DEL_ZERO_R VS_STR.
          CONCATENATE IT_LOG-VLNE2 ',准备时间:' VS_STR '(' OPERATION_TABLE-VGE01 ')' INTO IT_LOG-VLNE2.
        ENDIF.
        IF NOT OPERATION_TABLE-VGW02 IS INITIAL.
          VS_STR = OPERATION_TABLE-VGW02.          DEL_ZERO_R VS_STR.
          CONCATENATE IT_LOG-VLNE2 ',加工时间:' VS_STR '(' OPERATION_TABLE-VGE02 ')' INTO IT_LOG-VLNE2.
        ENDIF.
        IF NOT OPERATION_TABLE-VGW03 IS INITIAL.
          VS_STR = OPERATION_TABLE-VGW03.          DEL_ZERO_R VS_STR.
          CONCATENATE IT_LOG-VLNE2 ',机器时间:' VS_STR '(' OPERATION_TABLE-VGE03 ')' INTO IT_LOG-VLNE2.
        ENDIF.

        IF NOT OPERATION_TABLE-VGW04 IS INITIAL.
          VS_STR = OPERATION_TABLE-VGW04.          DEL_ZERO_R VS_STR.
          CONCATENATE IT_LOG-VLNE2 ',人工时间:' VS_STR '(' OPERATION_TABLE-VGE03 ')' INTO IT_LOG-VLNE2.
        ENDIF.

        IF NOT OPERATION_TABLE-VGW05 IS INITIAL.
          VS_STR = OPERATION_TABLE-VGW05.          DEL_ZERO_R VS_STR.
          CONCATENATE IT_LOG-VLNE2 ',制造费用:' VS_STR '(' OPERATION_TABLE-VGE03 ')' INTO IT_LOG-VLNE2.
        ENDIF.

        IF NOT OPERATION_TABLE-PREIS IS INITIAL.
          VS_STR = OPERATION_TABLE-PREIS.
          CONCATENATE IT_LOG-VLNE2 ',外协价格:' VS_STR '(' OPERATION_TABLE-WAERS ')/' INTO IT_LOG-VLNE2.          VS_STR = OPERATION_TABLE-PEINH.
          CONCATENATE IT_LOG-VLNE2 VS_STR '(' OPERATION_TABLE-MEINH ')' INTO IT_LOG-VLNE2.
        ENDIF.

        IF OPERATION_TABLE-SLWID IS NOT INITIAL.
          CONCATENATE IT_LOG-VLNE2 ',自定义字段码:' VS_STR '(' OPERATION_TABLE-SLWID ')/' INTO IT_LOG-VLNE2.
        ENDIF.
        IF OPERATION_TABLE-USR06 IS NOT INITIAL.
          VS_STR = OPERATION_TABLE-USR06 .
          CONCATENATE IT_LOG-VLNE2 ',标准人力:' VS_STR    INTO IT_LOG-VLNE2.
        ENDIF.

        IF OPERATION_TABLE-USR07 IS NOT INITIAL.
          VS_STR = OPERATION_TABLE-USR07 .
          CONCATENATE IT_LOG-VLNE2 ',标准产能:' VS_STR   INTO IT_LOG-VLNE2.
        ENDIF.

        SHIFT IT_LOG-VLNE2 LEFT DELETING LEADING ','.        CONDENSE IT_LOG-VLNE2 NO-GAPS.
        APPEND IT_LOG.               "追加记录        CLEAR IT_LOG.
        CLEAR IT_LOG.

*  记录工序的修改日志
*---------------------------
      WHEN 'U'.

        ULOG_ITAB: OPERATION_TABLE  OLD_AFVU USR06 订单工序  标准人力   工序计数器 APLZL 工序号 VORNR,
                   OPERATION_TABLE  OLD_AFVU USR07 订单工序  标准产能   工序计数器 APLZL 工序号 VORNR,
                   OPERATION_TABLE  OLD_AFVU SLWID 订单工序  自定义字段码   工序计数器 APLZL 工序号 VORNR.
*----------------------------------------------
* 根据工艺路线号及计数器查询更改前的信息(AFVC)
*----------------------------------------------
        READ TABLE OPERATION_TABLE_OLD_AFVC WITH KEY AUFPL = OPERATION_TABLE-AUFPL
                                                     APLZL =  OPERATION_TABLE-APLZL.

        IF SY-SUBRC = 0.
*-------------------------------------
* 可直接记录更改的参数（工序基本信息）
*-------------------------------------
          ULOG_ITAB: OPERATION_TABLE OLD_AFVC VORNR 订单工序 工序号 工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC WERKS 订单工序 工厂   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC STEUS 订单工序 控制码  工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC KTSCH 订单工序 标准文本码 工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC LTXA1 订单工序 工序短文本 工序计数器 APLZL 工序号 VORNR.
*-------------------------------------
*可直接记录更改的参数（外协价格信息）
* -------------------------------------
          ULOG_ITAB: OPERATION_TABLE OLD_AFVC PREIS 订单工序 外协价格  工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC WAERS 订单工序 外协价格货币 工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVC PEINH 订单工序 外协价格单位(每) 工序计数器 APLZL 工序号 VORNR.

*---------------------------------------------
*比较工作中心是否修改，因未直接记录工作中心，
*只能根据AFVC-ARBID查找工作中心并比较

          IF OPERATION_TABLE-ARBID <> OPERATION_TABLE_OLD_AFVC-ARBID.

* 根据AFVC-ARBID查询工作中心的原值与新值
            CLEAR: V_ARBPL_OLD,V_ARBPL_NEW.

            SELECT SINGLE ARBPL
            FROM CRHD
            INTO V_ARBPL_OLD
            WHERE OBJTY = 'A'
            AND OBJID = OPERATION_TABLE_OLD_AFVC-ARBID.

            SELECT SINGLE ARBPL
            FROM CRHD
            INTO V_ARBPL_NEW
            WHERE OBJTY = 'A'
            AND OBJID = OPERATION_TABLE-ARBID.

*如果工作中心的原值与新值不同则记录
            IF V_ARBPL_OLD <> V_ARBPL_NEW.
              VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
              IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
              IT_LOG-OPTYP = 'U'.          "操作类型：U-更新
              IT_LOG-OBJTP = '订单工序'.   "操作对象类型（抬头、组件、工艺等）
              IT_LOG-OBJNM = '工作中心'.   "操作对象描述

* 操作对象唯一行标记，例如：预留项目号(1)
* 唯一行标记一般前台不可见，但唯一且不可更改
              IT_LOG-OBJLU = OPERATION_TABLE-APLZL.        "工序计数器（例如：00000001）
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = IT_LOG-OBJLU
                IMPORTING
                  OUTPUT = IT_LOG-OBJLU.  "去除前置0

              CONCATENATE '工序计数器(' IT_LOG-OBJLU ')' INTO IT_LOG-OBJLU.  "拼写描述

* 作对象辅助行标记，例如：BOM项目号(0010)
* 辅助行标记一般不唯一或可修改，但最常用
              IT_LOG-OBJLA = OPERATION_TABLE-VORNR.   "工序号（例如：0010）
              CONCATENATE '工序号(' IT_LOG-OBJLA ')' INTO IT_LOG-OBJLA.  "拼写描述
              IT_LOG-VLOLD = V_ARBPL_OLD.             "操作对象的旧值
              IT_LOG-VLNEW = V_ARBPL_NEW.             "操作对象的新值
              APPEND IT_LOG.     "追加记录
              CLEAR IT_LOG.
            ENDIF.
          ENDIF.
        ENDIF.

*----------------------------------------------
*根据工艺路线号及计数器查询更改前的信息(AFVV)
*----------------------------------------------
        READ TABLE OPERATION_TABLE_OLD_AFVV WITH KEY AUFPL = OPERATION_TABLE-AUFPL
                                                     APLZL = OPERATION_TABLE-APLZL.
        IF SY-SUBRC = 0.
*---------------------------------------------
*可直接记录更改的参数（工时信息）
*---------------------------------------------
          ULOG_ITAB: OPERATION_TABLE OLD_AFVV VGW01 订单工序 准备时间(数值)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGE01 订单工序 准备时间(单位)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGW02 订单工序 加工时间(数值)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGE02 订单工序 加工时间(单位)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGW03 订单工序 机器时间(数值)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGE03 订单工序 机器时间(单位)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGW04 订单工序 人工时间(数值)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGE04 订单工序 人工时间(单位)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGW05 订单工序 制造费用(数值)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV VGE05 订单工序 制造费用(单位)   工序计数器 APLZL 工序号 VORNR,
                     OPERATION_TABLE OLD_AFVV MEINH 订单工序 作业的计量单位   工序计数器 APLZL 工序号 VORNR.



        ENDIF.

      WHEN 'D'.
        VI_OPNUM     = VI_OPNUM + 1. "记录本次操作的流水编码（自增1）
        IT_LOG-OPNUM = VI_OPNUM.     "记录本次操作的流水编码
        IT_LOG-OPTYP = 'D'.    "操作类型：D-删除
        IT_LOG-OBJTP = '订单工序'.   "操作对象类型（抬头、组件、工艺等）
        "操作对象描述
        CONCATENATE '序列' OPERATION_TABLE-PLNFL '任务清单节点数' OPERATION_TABLE-PLNKN '组计数器' OPERATION_TABLE-PLNAL
        '任务清单类型' OPERATION_TABLE-PLNTY INTO IT_LOG-VLOLD.
        CONCATENATE '订单工序'  '已经删除' INTO IT_LOG-VLNEW.
        APPEND IT_LOG.               "追加记录
        CLEAR IT_LOG.

    ENDCASE.
  ENDLOOP.



*=======================================
* 如有修改日志产生时才进行记录
*=======================================
  IF NOT IT_LOG[] IS INITIAL.

*  获取客户端IP地址（16进制）、主机名
    CALL FUNCTION 'TH_USER_INFO'
      IMPORTING
        HOSTADDR = IPX    "IP地址（16进制）
        TERMINAL = HOST.  "主机名

* 转换IP地址（16进制转换为10进制字符串）
    CALL FUNCTION 'GWY_IPADR2STRING'
      EXPORTING
        IPADR  = IPX
      IMPORTING
        STRING = IP.   "IP地址（10进制字符串）

*  补充日志内表的公共部分
    GET TIME.   "获取最新的时间
    IT_LOG-AUFNR  = HEADER_TABLE-AUFNR.  "订单编码
    IT_LOG-OPDAT  = SY-DATUM.            "操作日期(服务器)
    IT_LOG-OPTIM  = SY-UZEIT.            "操作时间(服务器)
    IT_LOG-TCODE  = SY-TCODE.            "事务代码
    IT_LOG-OPUSR  = SY-UNAME.            "操作客户端用户名
    IT_LOG-OPIPA  = IP.                  "操作客户端IP地址
    IT_LOG-OPHOS  = HOST.                "操作客户端主机名
    MODIFY IT_LOG TRANSPORTING AUFNR OPDAT OPTIM TCODE OPUSR OPIPA OPHOS                               WHERE AUFNR IS INITIAL.   "全部更新
*  将日志保存至数据库
    INSERT ZCO02_LOG FROM TABLE IT_LOG ACCEPTING DUPLICATE KEYS.  "忽略索引相同的记录

  ENDIF.

**create by handlq 2014/08/22
**增强2:生产订单BOM同一层物料出现多行提醒
*IF sy-uname = 'HANDLQ'.
*
*  DATA:lt_component_table TYPE STANDARD TABLE OF resbb.
*  DATA:lw_component_table TYPE resbb.
*
*  CLEAR:lt_component_table,lw_component_table.
*
*  LOOP AT component_table.
*    IF component_table-xloek <> 'X'. " 删除标识
*      lw_component_table-matnr = component_table-matnr. " 物料号
*      APPEND lw_component_table TO lt_component_table.
*      CLEAR:lw_component_table.
*    ENDIF.
*
*  ENDLOOP.
*
*  SORT lt_component_table BY matnr.
*
*  DELETE ADJACENT DUPLICATES FROM lt_component_table COMPARING matnr.
*
*  IF sy-subrc = 0.
*    MESSAGE 'BOM同一层物料出现多行提醒' TYPE 'W'.
*  ENDIF.
*ENDIF.
*
**
*}   INSERT
