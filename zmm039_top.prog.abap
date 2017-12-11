*&---------------------------------------------------------------------*
*&  包含                ZMM039_TOP
*&---------------------------------------------------------------------*

TABLES: SSCRFIELDS.
DATA: FUNCTXT TYPE SMP_DYNTXT.
*&-------alv类型定义
DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
      GD_REPID     LIKE SY-REPID.
DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
DATA:BDCMODE TYPE C VALUE 'N'.
*     bdcupdate TYPE c VALUE 'L'.
DATA: MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA: GD_COUNT   TYPE STRING. "上传内表的总行数

DATA : P_NEW TYPE C LENGTH 1 VALUE 'X'.
DATA : P_MODIFY TYPE C LENGTH 1 VALUE ''.
*DATA :gt_message  TYPE TABLE OF bdcmsgcoll,
TYPES : BEGIN OF TY_NEW,
          LIFNR(10)   TYPE C,  "供应商代码
          MATNR(40)   TYPE C, "物料号
          EKORG(4)    TYPE C,  "采购组织
          WERKS(4)    TYPE C, "工厂
          INFNR(10)   TYPE C,   "采购信息记录
          ESOKZ(1)    TYPE C,  "信息类别

          APLFZ(3)    TYPE C,     "计划交货时间
          EKGRP(3)    TYPE C,  "采购组
          NORMB(13)   TYPE C, "标准数量
          KZABS(1)    TYPE C, "需求确定
          MWSKZ(2)    TYPE C,  "税代码
          VERID(4)    TYPE C, "成产版本
          NETPR(11)   TYPE C,  "含税价

          WAERS(5)    TYPE C,  "货币码
          PEINH(13)   TYPE C,   "数量 价格单位
          DATAB(8)    TYPE C,   "有效期
          DATBI(8)    TYPE C, "有效期至

          TYPE(1)     TYPE C, "消息类型
          MESSAGE(60) TYPE C,    "消息
          LIGHT(4)    TYPE C,    "执行情况

        END OF TY_NEW.

DATA : GS_NEW TYPE TY_NEW .
DATA : GT_NEW LIKE TABLE OF GS_NEW.
DATA : LS_NEW LIKE LINE OF GT_NEW.
DATA : GT_NEW_OUTPUT LIKE TABLE OF GS_NEW.
FIELD-SYMBOLS : <FLS_NEW> LIKE LINE OF GT_NEW.
DATA GT_A018 TYPE TABLE OF A018.

DATA: WA_EINA TYPE EINA,
      WA_EINE TYPE EINE.

DATA: IT_EINA TYPE STANDARD TABLE OF EINA,
      IT_EINE TYPE STANDARD TABLE OF EINE.



TYPES : BEGIN OF TY_MODIFY ,
          LIFNR(10)   TYPE C,  "供应商代码
          MATNR(18)   TYPE C, "物料号
          EKORG(4)    TYPE C,  "采购组织
          INFNR(10)   TYPE C,   "采购信息记录
          ESOKZ(1)    TYPE C,  "信息类别


          NETPR(11)   TYPE C,  "含税价
          WAERS(5)    TYPE C,  "货币码
          PEINH(13)   TYPE C,   "数量 价格单位
          KMEIN(3)    TYPE C,    "订单定价单位
          DATAB(8)    TYPE C,   "有效期
          DATBI(8)    TYPE C, "有效期至

          TYPE(1)     TYPE C, "消息类型
          MESSAGE(60) TYPE C,    "消息
          LIGHT(4)    TYPE C,    "执行情况

        END OF TY_MODIFY.

DATA : GS_MODIFY TYPE TY_MODIFY.
DATA : GT_MODIFY LIKE TABLE OF GS_MODIFY.
FIELD-SYMBOLS : <FLS_MODIFY> LIKE LINE OF GT_MODIFY.

DATA : GW_KEYS TYPE KOMG.
DATA : GT_KOMV TYPE TABLE OF KOMV.
DATA : GW_KOMV TYPE KOMV.


*TYPES : BEGIN OF ty_eine ,
*          infnr LIKE eine-infnr,
*          ekorg LIKE eine-ekorg,
*          esokz LIKE eine-esokz,
*        END OF ty_eine.
