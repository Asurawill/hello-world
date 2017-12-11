*&---------------------------------------------------------------------*
*&  包含                ZPP035_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*
* 定义结构
*&---------------------------------------------------------------------*
TYPES:BEGIN OF TY_ITEM,
        AUFNR(100)  TYPE C, "工单号
        DATE(100)   TYPE C, "录入日期
        KDAUF(100)  TYPE C, "销售订单号
        KDPOS(100)  TYPE C, "销售订单行项目
        MATNR(100)  TYPE C, "物料
        MAKTX(200)  TYPE C, "物料描述
        WERKS(100)  TYPE C, "生产工厂
        AUART(100)  TYPE C, "订单类型
        GAMNG(100)  TYPE C, "总数量
        MEINS(100)  TYPE C, "单位
        GSTRP(100)  TYPE C, "开工时间
        GLTRP(100)  TYPE C, "完工时间
        WEMPF(100)  TYPE C, "原系统生产订单编号
        ABLAD(100)  TYPE C, "原系统销售订单编号
        POSNR(100)  TYPE C, "项目
        RMATNR(100) TYPE C, "组件
        RMAKTX(200) TYPE C, "组件描述
        MENGE(100)  TYPE C, "需求数量
        RMEINS(100) TYPE C, "计量单位
        POSTP(100)  TYPE C, "项目类型
        VORNR(100)  TYPE C, "工序
        RWERKS(100) TYPE C, "组件工厂
        LGORT(100)  TYPE C, "仓储地点
        RGEKZ(100)  TYPE C, "反冲
        SANKA(100)  TYPE C, "核算相关标识
      END OF TY_ITEM.

TYPES:BEGIN OF TY_ORDERLIST,
        TYPE         TYPE C, "消息类型
        AUFNR        TYPE AFPO-AUFNR, "订单号
        MESSAGE(140) TYPE C,  "消息
        NUM(20)      TYPE C, "编号
        DATE         TYPE DATUM, "录入日期
        KDAUF        TYPE AFPO-KDAUF, "销售订单号
        KDPOS        TYPE AFPO-KDPOS, "销售订单行项目
        MATNR        TYPE AFKO-PLNBEZ, "物料
        WERKS        TYPE AUFK-WERKS, "生产工厂
        AUART        TYPE AUFK-AUART, "订单类型
        GAMNG        TYPE AFKO-GAMNG, "总数量
        MEINS        TYPE AFKO-GMEIN, "单位
        GSTRP        TYPE AFKO-GSTRP, "开工时间
        GLTRP        TYPE AFKO-GLTRP, "完工时间
        WEMPF        TYPE WEMPF, "原系统生产订单编号
        ABLAD        TYPE ABLAD, "原系统销售订单编号
        POSNR        TYPE RESB-RSPOS, "项目
        RMATNR       TYPE RESB-MATNR, "组件
        MENGE        TYPE RESB-BDMNG, "需求数量
        RMEINS       TYPE RESB-MEINS, "计量单位
        POSTP        TYPE RESB-POSTP, "项目类型
        VORNR        TYPE RESB-VORNR, "工序
        RWERKS       TYPE RESB-WERKS, "组件工厂
        LGORT        TYPE RESB-LGORT, "仓储地点
        RGEKZ        TYPE RESB-RGEKZ, "反冲
        SANKA        TYPE RESB-SANKA, "核算相关标识
      END OF TY_ORDERLIST.
*----------------------------------------------------------------------*
*  定义ALV数据对象
*
*----------------------------------------------------------------------*
DATA:       IT_STRUCTURE TYPE LVC_T_FCAT,
            IW_STRUCTURE TYPE LVC_S_FCAT,
            IW_LAYOUT    TYPE LVC_S_LAYO,
            L_TITLE      TYPE LVC_TITLE.

*&---------------------------------------------------------------------*
*
* 定义工作区和内表
*&---------------------------------------------------------------------*
"导入数据需要用到的变量
DATA: TAB_RAW_DATA TYPE TRUXS_T_TEXT_DATA."局部变量——导入excel文件时用到的工作区
DATA: GV_FILENAME TYPE STRING.            "全局变量——文件名
DATA: LV_ANSWER.                          "局部变量——导入文件确认码

DATA:IW_ITEM TYPE TY_ITEM,
     IT_ITEM LIKE IW_ITEM OCCURS 0.

DATA:IW_ORDERDATA TYPE TY_ORDERLIST,
     IT_ORDERDATA LIKE IW_ORDERDATA OCCURS 0.

DATA:IW_ORDERLIST TYPE TY_ORDERLIST,
     IT_ORDERLIST LIKE IW_ORDERLIST OCCURS 0.

DATA:BEGIN OF IT_T399X OCCURS 9,
       WERKS TYPE T399X-WERKS, "工厂
       AUART TYPE T399X-AUART, "订单类型
       CAPID TYPE T399X-CAPID, "应用程序
     END OF IT_T399X,
     IW_T399X LIKE LINE OF IT_T399X.

DATA GT_MARA TYPE TABLE OF MARA.
DATA GS_MARA TYPE MARA.
